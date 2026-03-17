#!/usr/bin/env Rscript
# ==============================================================================
# ownership_model_joe.R
# Splash-format ownership model for NCAA tournament Rounds 1 & 2 (Days 1 & 2)
#
# Trains on Splash pool data (2024 + 2025) — NOT Hodes.
# Rounds 1 & 2 only: full availability (everyone has every team).
# Single model across both days (no day-split).
#
# THREE MODELS (trained on all available data):
#   M1: M3-Splash    — direct port of Hodes M3 log-linear softmax (16 params)
#   M2: Log-Ratio    — log(own / naive_uniform), isolates pure behavioral bias (5 params)
#   M3: Seed-Tier    — compact 7-param model with 4 seed tiers, robust for small N
# ==============================================================================

library(data.table)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

SPLASH_YEARS <- 2024:2025
TARGET_YEAR  <- 2026
LOG_SCALE    <- 0.0917  # KenPom logistic scale factor

# ==============================================================================
# SHARED HELPERS (adapted from ownership_model.R)
# ==============================================================================

team_dict_file <- file.path(script_dir, "team_names.csv")
if (!file.exists(team_dict_file)) stop("team_names.csv not found: ", team_dict_file)
team_dict     <- fread(team_dict_file)
kp_alias      <- setNames(team_dict$kenpom_name,   team_dict$bracket_name)
cl_to_bracket <- setNames(team_dict$bracket_name,  team_dict$closing_lines_name)

resolve_name <- function(name) {
  name <- trimws(name)
  if (name %in% names(kp_alias)) kp_alias[[name]] else name
}

resolve_cl_to_bracket <- function(cl_name, bracket_teams) {
  if (cl_name %in% names(cl_to_bracket)) return(cl_to_bracket[[cl_name]])
  sorted_bt <- bracket_teams[order(-nchar(bracket_teams))]
  for (bt in sorted_bt) {
    if (startsWith(cl_name, bt) &&
        (nchar(cl_name) == nchar(bt) ||
         substr(cl_name, nchar(bt)+1, nchar(bt)+1) == " ")) return(bt)
  }
  NA_character_
}

load_kenpom_yr <- function(year) {
  f <- file.path(script_dir, "kenpom_data", sprintf("kenpom_%d.csv", year))
  if (!file.exists(f)) return(NULL)
  dt <- fread(f)
  if ("adj_em" %in% names(dt)) setnames(dt, "adj_em", "AdjEM")
  if ("NetRtg" %in% names(dt)) { idx <- which(names(dt)=="NetRtg")[1]; names(dt)[idx] <- "AdjEM" }
  if ("team"   %in% names(dt)) setnames(dt, "team", "Team")
  dt[, AdjEM := suppressWarnings(as.numeric(AdjEM))]
  dt <- dt[!is.na(AdjEM) & Team != "" & Team != "Team"]
  dt$Team <- gsub("\\s*\\d+$", "", trimws(dt$Team))
  dt
}

load_cl_yr <- function(year, bracket_teams) {
  f <- file.path(script_dir, "closing_lines", sprintf("ncaat_%d_closing_lines.csv", year))
  if (!file.exists(f)) return(NULL)
  cl <- fread(f)
  cl[, home_bracket := sapply(home_team, resolve_cl_to_bracket, bracket_teams=bracket_teams)]
  cl[, away_bracket := sapply(away_team, resolve_cl_to_bracket, bracket_teams=bracket_teams)]
  cl <- cl[!is.na(home_bracket) & !is.na(away_bracket)]
  if (nrow(cl) == 0) return(NULL)
  cl[, game_date := as.Date(date)]
  setorder(cl, game_date)
  # First 32 R1 games
  seen <- character(0); r1_rows <- integer(0)
  for (i in seq_len(nrow(cl))) {
    h <- cl$home_bracket[i]; a <- cl$away_bracket[i]
    if (!(h %in% seen) && !(a %in% seen)) {
      r1_rows <- c(r1_rows, i); seen <- c(seen, h, a)
    }
    if (length(r1_rows) == 32) break
  }
  r1 <- cl[r1_rows]
  wp <- list()
  for (i in seq_len(nrow(r1))) {
    wp[[r1$home_bracket[i]]] <- r1$home_win_prob[i]
    wp[[r1$away_bracket[i]]] <- 1 - r1$home_win_prob[i]
  }
  # Also attach game_date per team for day assignment
  date_map <- list()
  for (i in seq_len(nrow(r1))) {
    date_map[[r1$home_bracket[i]]] <- r1$game_date[i]
    date_map[[r1$away_bracket[i]]] <- r1$game_date[i]
  }
  list(wp=wp, date_map=date_map)
}

# ==============================================================================
# BUILD FEATURE MATRIX (all 64 bracket teams for a given year)
# ==============================================================================

build_features <- function(year) {
  bf <- file.path(script_dir, "brackets", sprintf("bracket_%d.csv", year))
  if (!file.exists(bf)) { cat(sprintf("  No bracket for %d\n", year)); return(NULL) }
  br <- fread(bf)
  bt <- data.table(name=br$team, seed=br$seed, region=br$region, team_id=seq_len(nrow(br)))
  bt[, kp_name := sapply(name, resolve_name)]

  kp <- load_kenpom_yr(year)
  if (is.null(kp)) { cat(sprintf("  No KenPom for %d\n", year)); return(NULL) }
  bt <- merge(bt, kp[, .(Team, AdjEM)], by.x="kp_name", by.y="Team", all.x=TRUE)
  for (i in which(is.na(bt$AdjEM))) {
    m <- grep(bt$name[i], kp$Team, value=TRUE, ignore.case=TRUE)
    if (length(m) > 0) bt$AdjEM[i] <- kp[Team==m[1]]$AdjEM[1]
  }
  bt[is.na(AdjEM), AdjEM := -15]
  setorder(bt, team_id)

  # R1 win probabilities
  cl_data <- load_cl_yr(year, bt$name)
  cl_wp    <- if (!is.null(cl_data)) cl_data$wp       else NULL
  cl_dates <- if (!is.null(cl_data)) cl_data$date_map else NULL

  bt[, wp := 0.0]
  if (!is.null(cl_wp) && length(cl_wp) >= 50) {
    for (i in seq_len(nrow(bt))) {
      w <- cl_wp[[bt$name[i]]]
      if (!is.null(w)) {
        bt$wp[i] <- w
      } else {
        j <- if (i%%2==1) i+1 else i-1
        bt$wp[i] <- 1/(1+exp(-LOG_SCALE*(bt$AdjEM[i]-bt$AdjEM[j])))
      }
    }
  } else {
    for (g in 1:32) {
      ii <- 2*g-1; jj <- 2*g
      p <- 1/(1+exp(-LOG_SCALE*(bt$AdjEM[ii]-bt$AdjEM[jj])))
      bt$wp[ii] <- p; bt$wp[jj] <- 1-p
    }
  }

  # R2 compound win probabilities
  bt[, wp_r2 := 0.0]
  for (g in 1:16) {
    i1 <- 4*(g-1)+1; i2 <- i1+1; i3 <- i1+2; i4 <- i1+3
    for (ti in c(i1, i2)) {
      p3 <- 1/(1+exp(-LOG_SCALE*(bt$AdjEM[ti]-bt$AdjEM[i3])))
      p4 <- 1/(1+exp(-LOG_SCALE*(bt$AdjEM[ti]-bt$AdjEM[i4])))
      bt$wp_r2[ti] <- bt$wp[ti]*(bt$wp[i3]*p3 + bt$wp[i4]*p4)
    }
    for (ti in c(i3, i4)) {
      p1 <- 1/(1+exp(-LOG_SCALE*(bt$AdjEM[ti]-bt$AdjEM[i1])))
      p2 <- 1/(1+exp(-LOG_SCALE*(bt$AdjEM[ti]-bt$AdjEM[i2])))
      bt$wp_r2[ti] <- bt$wp[ti]*(bt$wp[i1]*p1 + bt$wp[i2]*p2)
    }
  }
  bt[, wp_drop := pmin(wp - wp_r2, 0.45)]

  # Championship probability from sim
  sim_pq2  <- file.path(script_dir, sprintf("sim_results_%d_part2.parquet", year))
  sim_meta <- file.path(script_dir, sprintf("sim_results_%d_meta.rds",     year))
  sim_rds  <- file.path(script_dir, sprintf("sim_results_%d.rds",          year))
  bt[, champ_prob := 0.0]
  if (file.exists(sim_pq2) && file.exists(sim_meta)) {
    suppressPackageStartupMessages(library(arrow))
    sim_yr     <- readRDS(sim_meta)
    champ_game <- as.integer(read_parquet(sim_pq2, col_select="game_63")[[1]])
    counts     <- tabulate(champ_game, nbins=nrow(sim_yr$teams))
    bt[, champ_prob := counts[team_id] / sim_yr$n_sims]
  } else if (file.exists(sim_rds)) {
    sim_yr     <- readRDS(sim_rds)
    champ_game <- sim_yr$all_results[, 63]
    counts     <- tabulate(champ_game, nbins=nrow(sim_yr$teams))
    bt[, champ_prob := counts[team_id] / sim_yr$n_sims]
  } else {
    bt[, champ_prob := pmax(AdjEM/max(AdjEM, na.rm=TRUE), 0)^3]
  }
  max_cp <- max(bt$champ_prob, na.rm=TRUE)
  if (max_cp > 0) bt[, champ_prob := champ_prob / max_cp]

  # Assign game_day from closing lines dates (1 = earliest date, 2 = second, etc.)
  bt[, game_day := NA_integer_]
  if (!is.null(cl_dates) && length(cl_dates) > 0) {
    all_dates <- sort(unique(unlist(lapply(cl_dates, as.character))))
    date_rank <- setNames(seq_along(all_dates), all_dates)
    for (i in seq_len(nrow(bt))) {
      d <- cl_dates[[bt$name[i]]]
      if (!is.null(d)) bt$game_day[i] <- date_rank[as.character(d)]
    }
    # Propagate to R1 partner (same game, same day)
    for (i in which(is.na(bt$game_day))) {
      j <- if (i%%2==1) i+1 else i-1
      if (!is.na(bt$game_day[j])) bt$game_day[i] <- bt$game_day[j]
    }
  }

  bt[, year := year]
  bt
}

# ==============================================================================
# LOAD SPLASH DATA + BUILD TRAINING SET
# ==============================================================================

cat("=== OWNERSHIP MODEL JOE — Splash-Trained Rounds 1 & 2 ===\n\n")
cat("Loading splash ownership data...\n")

splash_raw <- rbindlist(lapply(SPLASH_YEARS, function(yr) {
  f <- file.path(script_dir, "splash_ownership", sprintf("ownership_%d.csv", yr))
  if (!file.exists(f)) { cat(sprintf("  WARNING: not found: %s\n", f)); return(NULL) }
  dt <- fread(f); dt[, year := yr]; dt
}), fill=TRUE)
splash_raw[, day_num := as.integer(sub("^day([0-9]+)_.*$", "\\1", day))]
splash_d12 <- splash_raw[day_num %in% 1:2]

for (yr in SPLASH_YEARS) {
  n1 <- nrow(splash_d12[year==yr & day_num==1])
  n2 <- nrow(splash_d12[year==yr & day_num==2])
  cat(sprintf("  %d: Day1=%d teams listed, Day2=%d teams listed\n", yr, n1, n2))
}

# Build feature matrices for training years
cat("\nBuilding feature matrices...\n")
feat_list <- lapply(setNames(SPLASH_YEARS, SPLASH_YEARS), build_features)

# Match splash team names to bracket names
match_to_bracket <- function(splash_name, bracket_names) {
  if (splash_name %in% bracket_names) return(splash_name)
  # Try case-insensitive exact match
  m <- bracket_names[toupper(bracket_names) == toupper(splash_name)]
  if (length(m) > 0) return(m[1])
  # Try kenpom alias reverse lookup
  for (bn in bracket_names) {
    kpn <- if (bn %in% names(kp_alias)) kp_alias[[bn]] else bn
    if (toupper(splash_name) == toupper(kpn)) return(bn)
  }
  # Partial/fuzzy
  m <- grep(paste0("^", gsub("\\.", "\\\\.", splash_name), "$"),
            bracket_names, value=TRUE, ignore.case=TRUE)
  if (length(m) > 0) return(m[1])
  m <- grep(splash_name, bracket_names, value=TRUE, ignore.case=TRUE)
  if (length(m) > 0) return(m[1])
  NA_character_
}

# Merge splash ownership into feature table; add 0-pick rows for missing teams
build_training_data <- function() {
  rows <- list()
  for (yr in SPLASH_YEARS) {
    feats <- feat_list[[as.character(yr)]]
    if (is.null(feats)) next
    ft <- copy(feats)
    ft[, ownership := 0.0]
    # Fill in observed ownership
    sp <- splash_d12[year==yr]
    unmatched <- character(0)
    for (i in seq_len(nrow(sp))) {
      bn <- match_to_bracket(sp$team[i], ft$name)
      if (!is.na(bn)) {
        # Assign ownership and confirm day_num matches closing-line day
        ft[name==bn, ownership := sp$ownership[i]]
        # If game_day unassigned, use splash day_num as fallback
        if (is.na(ft[name==bn]$game_day)) ft[name==bn, game_day := sp$day_num[i]]
      } else {
        unmatched <- c(unmatched, sp$team[i])
      }
    }
    if (length(unmatched) > 0)
      cat(sprintf("  %d: %d splash teams unmatched: %s\n",
                  yr, length(unmatched), paste(unmatched, collapse=", ")))
    rows[[as.character(yr)]] <- ft
  }
  rbindlist(rows, fill=TRUE)
}

train_data <- build_training_data()
# Only use teams with a valid game_day (days 1 & 2)
train_data <- train_data[game_day %in% 1:2]

cat(sprintf("\nTraining data: %d team-day observations\n", nrow(train_data)))
for (yr in SPLASH_YEARS) {
  for (d in 1:2) {
    g <- train_data[year==yr & game_day==d]
    cat(sprintf("  %d Day%d: %d teams (%d with picks > 0, total own=%.3f)\n",
                yr, d, nrow(g), sum(g$ownership>0), sum(g$ownership)))
  }
}

# ==============================================================================
# SOFTMAX UTILITY
# ==============================================================================

smx <- function(la) { la <- la - max(la); exp(la)/sum(exp(la)) }

group_mse <- function(pred_fn, data) {
  total <- 0; n <- 0
  for (yr in unique(data$year)) {
    for (d in unique(data[year==yr]$game_day)) {
      g <- data[year==yr & game_day==d]
      if (nrow(g) < 4) next
      actual <- g$ownership / sum(g$ownership)
      total  <- total + sum((pred_fn(g) - actual)^2)
      n      <- n + 1
    }
  }
  if (n == 0) 1e6 else total/n
}

# ==============================================================================
# MODEL 1: M3-SPLASH — 16-param log-linear softmax (direct Hodes M3 port)
# log(share) ~ β_wp*wp + β_champ*champ + β_wxs*(wp*seed) + β_drop*wp_drop
#            + seed_FE[1..12]
# ==============================================================================

m1_pred <- function(par, g) {
  cs <- pmin(g$seed, 12)
  la <- par[1]*g$wp + par[2]*g$champ_prob + par[3]*g$wp*cs +
        par[4]*g$wp_drop + par[4+cs]
  smx(la)
}

m1_loss <- function(par, data) group_mse(function(g) m1_pred(par, g), data)

fit_m1 <- function(data) {
  dfe <- c(-3.0,-1.5,-0.5, 0.0, 0.0,-0.3,-0.5, 0.5,-0.3,-0.3,-0.5,-0.8)
  starts <- list(
    c(3.5, 1.0, 0.40, 5.0, dfe),
    c(3.8, 1.1, 0.35, 5.8, -2.5,-1.5,-0.6, 0.0, 0.0,-0.3,-0.5, 0.5,-0.3,-0.3,-0.5,-0.8),
    c(3.0, 0.5, 0.30, 4.5, -3.5,-2.0,-1.0,-0.3, 0.0, 0.0,-0.3, 0.3,-0.5,-0.5,-0.3,-1.0),
    c(4.5, 1.5, 0.50, 6.0, -2.0,-1.0,-0.3, 0.3, 0.2,-0.5,-0.8, 0.8,-0.2,-0.2,-0.6,-1.2),
    c(3.5, 0.0, 0.40, 5.0, -4.0,-2.5,-1.5,-0.5, 0.0, 0.0, 0.0, 0.5, 0.0, 0.0,-0.5,-0.5),
    c(2.5, 2.0, 0.25, 4.0, -3.0,-1.8,-0.8, 0.2, 0.3,-0.2,-0.4, 0.6,-0.4,-0.1,-0.4,-0.9),
    c(5.0, 0.8, 0.45, 7.0, -2.0,-1.2,-0.4, 0.1, 0.1,-0.4,-0.6, 0.4,-0.1,-0.4,-0.7,-1.5),
    c(4.0, 0.3, 0.35, 5.0, -3.5,-2.2,-1.2,-0.2, 0.1,-0.1,-0.5, 0.3,-0.3,-0.3,-0.4,-0.7)
  )
  lo <- c(-10,-10,-2,-10, rep(-8,12)); hi <- c(15,15,3,15, rep(8,12))
  bv <- Inf; bp <- starts[[1]]
  for (s in starts) {
    r <- tryCatch(optim(s, m1_loss, data=data, method="L-BFGS-B",
                        lower=lo, upper=hi, control=list(maxit=5000)),
                  error=function(e) list(value=Inf, par=s))
    if (r$value < bv) { bv <- r$value; bp <- r$par }
  }
  list(par=bp, loss=bv)
}

# ==============================================================================
# MODEL 2: LOG-RATIO — 5-param, models log(own/naive) to isolate behavioral bias
# log_attract ~ β0 + β_wp*wp + β_champ*champ + β_drop*wp_drop + β_seed*(seed/16)
# ==============================================================================

m2_pred <- function(par, g) {
  la <- par[1] + par[2]*g$wp + par[3]*g$champ_prob +
        par[4]*g$wp_drop + par[5]*(g$seed/16)
  smx(la)
}

m2_loss <- function(par, data) group_mse(function(g) m2_pred(par, g), data)

fit_m2 <- function(data) {
  starts <- list(
    c(0.0, 3.5, 1.0, 5.0, 1.0),
    c(0.0, 4.0, 0.5, 4.0, 1.5),
    c(0.0, 2.5, 1.5, 6.0, 0.5),
    c(0.0, 3.0, 2.0, 3.0, 2.0),
    c(0.5, 4.5, 0.0, 5.0, 1.2)
  )
  lo <- c(-5,-10,-10,-10,-5); hi <- c(5,15,15,15,10)
  bv <- Inf; bp <- starts[[1]]
  for (s in starts) {
    r <- tryCatch(optim(s, m2_loss, data=data, method="L-BFGS-B",
                        lower=lo, upper=hi, control=list(maxit=3000)),
                  error=function(e) list(value=Inf, par=s))
    if (r$value < bv) { bv <- r$value; bp <- r$par }
  }
  list(par=bp, loss=bv)
}

# ==============================================================================
# MODEL 3: SEED-TIER COMPACT — 7 params, robust for small N
# log(share) ~ β_wp*wp + β_champ*champ + β_drop*wp_drop + tier_FE[1..4]
# Tiers: save(1-2), moderate(3-4), sweet_spot(5-8), longshot(9-16)
# ==============================================================================

tier_idx <- function(seed) ifelse(seed<=2, 1L, ifelse(seed<=4, 2L, ifelse(seed<=8, 3L, 4L)))

m3_pred <- function(par, g) {
  ti <- tier_idx(g$seed)
  la <- par[1]*g$wp + par[2]*g$champ_prob + par[3]*g$wp_drop + par[3+ti]
  smx(la)
}

m3_loss <- function(par, data) group_mse(function(g) m3_pred(par, g), data)

fit_m3 <- function(data) {
  starts <- list(
    c(3.5, 1.0, 5.0, -2.0,-0.5, 0.5, 0.0),
    c(4.0, 0.5, 4.5, -2.5,-0.8, 0.3, 0.2),
    c(3.0, 1.5, 6.0, -1.5,-0.3, 0.8,-0.2),
    c(2.5, 0.8, 4.0, -3.0,-1.0, 0.0, 0.5),
    c(4.5, 1.2, 5.5, -1.8,-0.4, 0.6, 0.1)
  )
  lo <- c(-10,-10,-10, rep(-8,4)); hi <- c(15,15,15, rep(8,4))
  bv <- Inf; bp <- starts[[1]]
  for (s in starts) {
    r <- tryCatch(optim(s, m3_loss, data=data, method="L-BFGS-B",
                        lower=lo, upper=hi, control=list(maxit=3000)),
                  error=function(e) list(value=Inf, par=s))
    if (r$value < bv) { bv <- r$value; bp <- r$par }
  }
  list(par=bp, loss=bv)
}

# ==============================================================================
# FIT FINAL MODELS ON ALL DATA
# ==============================================================================

cat("==============================================================\n")
cat("  FINAL MODELS — Trained on all splash data\n")
cat("==============================================================\n\n")

cat("Fitting all 3 models on full training set...\n")
fits <- list(
  m1 = fit_m1(train_data),
  m2 = fit_m2(train_data),
  m3 = fit_m3(train_data)
)

# Print coefficients
cat("\nM1 (M3-Splash, 16 params):\n")
p <- fits$m1$par
cat(sprintf("  β_wp=%.3f  β_champ=%.3f  β_wxs=%.3f  β_drop=%.3f\n", p[1],p[2],p[3],p[4]))
cat(sprintf("  Seed FE: %s\n", paste(sprintf("s%d=%+.2f",1:12,p[5:16]), collapse=" ")))
cat(sprintf("  Train MSE: %.6f\n\n", fits$m1$loss))

cat("M2 (Log-Ratio, 5 params):\n")
p <- fits$m2$par
cat(sprintf("  β0=%.3f  β_wp=%.3f  β_champ=%.3f  β_drop=%.3f  β_seed=%.3f\n",
            p[1],p[2],p[3],p[4],p[5]))
cat(sprintf("  Train MSE: %.6f\n\n", fits$m2$loss))

cat("M3 (Seed-Tier, 7 params):\n")
p <- fits$m3$par
cat(sprintf("  β_wp=%.3f  β_champ=%.3f  β_drop=%.3f\n", p[1],p[2],p[3]))
cat(sprintf("  Tiers: save=%+.2f  moderate=%+.2f  sweet_spot=%+.2f  longshot=%+.2f\n",
            p[4],p[5],p[6],p[7]))
cat(sprintf("  Train MSE: %.6f\n\n", fits$m3$loss))

# In-sample summary per year+day
cat("In-sample fit summary:\n")
cat(sprintf("  %-13s  %8s  %8s  %8s\n", "Model", "Train MSE", "Avg MAE", "Avg Corr"))
cat(paste(rep("-", 47), collapse=""), "\n")
for (mn in c("m1","m2","m3")) {
  f <- fits[[mn]]
  total_mae <- 0; total_cor <- 0; ng <- 0
  for (yr in SPLASH_YEARS) {
    for (d in 1:2) {
      g <- train_data[year==yr & game_day==d]
      if (nrow(g) < 4) next
      actual <- g$ownership / sum(g$ownership)
      p <- switch(mn,
        m1 = m1_pred(f$par, g),
        m2 = m2_pred(f$par, g),
        m3 = m3_pred(f$par, g))
      total_mae <- total_mae + mean(abs(p - actual))
      total_cor <- total_cor + tryCatch(cor(p, actual), error=function(e) NA_real_)
      ng <- ng + 1
    }
  }
  label <- c(m1="M1_Splash", m2="M2_LogRatio", m3="M3_SeedTier")[[mn]]
  cat(sprintf("  %-13s  %8.5f  %8.5f  %+8.4f\n",
              label, f$loss, total_mae/ng, total_cor/ng))
}

# ==============================================================================
# SAVE M1 MODEL
# ==============================================================================

m1_save <- list(
  par        = fits$m1$par,
  loss       = fits$m1$loss,
  trained_on = SPLASH_YEARS,
  param_names = c("beta_wp","beta_champ","beta_wxs","beta_drop",
                  paste0("seed_fe_", 1:12))
)
saveRDS(m1_save, file.path(script_dir, "m1_ownership_model.rds"))
cat(sprintf("\nM1 model saved to m1_ownership_model.rds\n"))

# ==============================================================================
# 2026 PREDICTION
# ==============================================================================

cat("\n==============================================================\n")
cat(sprintf("  %d PREDICTIONS\n", TARGET_YEAR))
cat("==============================================================\n")

feats_2026 <- build_features(TARGET_YEAR)

predict_day <- function(day_grp, day_num, year=TARGET_YEAR) {
  g <- day_grp[!is.na(wp)]
  if (nrow(g) == 0) { cat("No teams to predict.\n"); return(invisible(NULL)) }

  p1 <- m1_pred(fits$m1$par, g)
  p2 <- m2_pred(fits$m2$par, g)
  p3 <- m3_pred(fits$m3$par, g)
  avg <- (p1+p2+p3)/3

  dt <- data.table(
    team=g$name, seed=g$seed, wp=round(100*g$wp,1),
    champ=round(100*g$champ_prob,2), wp_drop=round(100*g$wp_drop,1),
    M1=round(100*p1,1), M2=round(100*p2,1),
    M3=round(100*p3,1),
    avg=round(100*avg,1)
  )
  setorder(dt, -avg)

  cat(sprintf("\n%d Day %d — %d teams playing today:\n", year, day_num, nrow(dt)))
  cat(sprintf("%-22s %4s %6s %5s %5s | %6s %6s %6s | %6s\n",
              "Team","Seed","WinP%","Chmp%","Drop%",
              "M1","M2","M3","Avg%"))
  cat(paste(rep("-", 74), collapse=""), "\n")
  for (i in seq_len(nrow(dt))) {
    cat(sprintf("%-22s  %2d  %5.1f%% %4.1f%% %4.1f%% | %5.1f%% %5.1f%% %5.1f%% | %5.1f%%\n",
                dt$team[i], dt$seed[i], dt$wp[i], dt$champ[i], dt$wp_drop[i],
                dt$M1[i], dt$M2[i], dt$M3[i], dt$avg[i]))
  }
  top5 <- sum(sort(dt$avg, decreasing=TRUE)[1:min(5,nrow(dt))])
  cat(sprintf("\nTop-5 concentration: %.0f%%  |  Consensus leader: %s (%s%%)\n",
              top5, dt$team[1], dt$avg[1]))
  invisible(dt)
}

if (!is.null(feats_2026)) {
  days_present <- sort(unique(feats_2026$game_day[feats_2026$game_day %in% 1:2 &
                                                   !is.na(feats_2026$game_day)]))
  if (length(days_present) > 0) {
    for (d in days_present) {
      day_grp <- feats_2026[game_day == d]
      assign(sprintf("day%d_2026", d), predict_day(day_grp, d))
    }
  } else {
    cat(sprintf("\nNo game_day 1/2 assignments found for %d.\n", TARGET_YEAR))
    cat("Ensure ncaat_2026_closing_lines.csv has date column.\n")
    cat("Manual use: predict_day(feats_2026[name %%in%% c(...)], day_num=1)\n")
  }
} else {
  cat(sprintf("Could not build features for %d.\n", TARGET_YEAR))
}

# ==============================================================================
# EXPORTED OBJECTS FOR INTERACTIVE USE
# ==============================================================================
cat("\n--------------------------------------------------------------\n")
cat("  Interactive objects:\n")
cat("    fits          — list(m1, m2, m3), each with $par and $loss\n")
cat("    train_data    — feature+ownership data for all training years\n")
cat("    feats_2026    — 2026 feature matrix (if built)\n")
cat("    predict_day(day_grp, day_num)  — predict any group of teams\n\n")
cat("  Example:\n")
cat("    day1_teams <- feats_2026[game_day == 1]\n")
cat("    predict_day(day1_teams, day_num = 1)\n")
cat("--------------------------------------------------------------\n")
