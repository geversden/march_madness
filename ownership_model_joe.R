#!/usr/bin/env Rscript
# ==============================================================================
# ownership_model_simple.R
# Simple, low-parameter ownership models for NCAA tournament Rounds 1 & 2
#
# Designed to prevent overfitting with limited training data (~128 obs).
#
# FEATURES:
#   wp         — R1 win probability (from closing lines)
#   seed       — tournament seed (1-16)
#   champ_prob — championship probability (from simulation)
#   wp_drop    — probability drop from R1 to R2 (survival risk)
#
# MODELS:
#   M0: Null (Uniform)     — baseline (0 params)
#   M1: WinProb-Only       — single feature (1 param)
#   M2: Seed-Only          — single feature (1 param)  
#   M3: WP + Seed          — two features (2 params)
#   M4: All Linear         — all 4 features (4 params)
#   G1: GAM Additive       — smooth(wp) + smooth(champ) + smooth(seed)
#   G2: GAM + Interaction  — G1 + tensor interaction wp:seed
# ==============================================================================

library(data.table)
library(mgcv)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

SPLASH_YEARS <- 2024:2025
TARGET_YEAR  <- 2026
LOG_SCALE    <- 0.0917

# ==============================================================================
# HELPER FUNCTIONS
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
  seen <- character(0); r1_rows <- integer(0)
  for (i in seq_len(nrow(cl))) {
    h <- cl$home_bracket[i]; a <- cl$away_bracket[i]
    if (!(h %in% seen) && !(a %in% seen)) {
      r1_rows <- c(r1_rows, i); seen <- c(seen, h, a)
    }
    if (length(r1_rows) == 32) break
  }
  r1 <- cl[r1_rows]
  wp <- list(); date_map <- list()
  for (i in seq_len(nrow(r1))) {
    wp[[r1$home_bracket[i]]] <- r1$home_win_prob[i]
    wp[[r1$away_bracket[i]]] <- 1 - r1$home_win_prob[i]
    date_map[[r1$home_bracket[i]]] <- r1$game_date[i]
    date_map[[r1$away_bracket[i]]] <- r1$game_date[i]
  }
  list(wp=wp, date_map=date_map)
}

# ==============================================================================
# BUILD FEATURE MATRIX (includes all features)
# ==============================================================================

build_features <- function(year) {
  bf <- file.path(script_dir, "brackets", sprintf("bracket_%d.csv", year))
  if (!file.exists(bf)) { cat(sprintf("  No bracket for %d\n", year)); return(NULL) }
  
  br <- fread(bf)
  bt <- data.table(
    name = br$team,
    seed = br$seed,
    region = br$region,
    team_id = seq_len(nrow(br))
  )
  bt[, kp_name := sapply(name, resolve_name)]
  
  kp <- load_kenpom_yr(year)
  if (is.null(kp)) { cat(sprintf("  No KenPom for %d\n", year)); return(NULL) }
  
  bt <- merge(bt, kp[, .(Team, AdjEM)], by.x="kp_name", by.y="Team", all.x=TRUE)
  for (i in which(is.na(bt$AdjEM))) {
    m <- grep(bt$name[i], kp$Team, value=TRUE, ignore.case=TRUE)
    if (length(m) > 0) bt$AdjEM[i] <- kp[Team == m[1]]$AdjEM[1]
  }
  bt[is.na(AdjEM), AdjEM := -15]
  setorder(bt, team_id)
  
  # R1 win probabilities
  cl_data <- load_cl_yr(year, bt$name)
  cl_wp <- if (!is.null(cl_data)) cl_data$wp else NULL
  cl_dates <- if (!is.null(cl_data)) cl_data$date_map else NULL
  
  bt[, wp := 0.0]
  if (!is.null(cl_wp) && length(cl_wp) >= 50) {
    for (i in seq_len(nrow(bt))) {
      w <- cl_wp[[bt$name[i]]]
      if (!is.null(w)) {
        bt$wp[i] <- w
      } else {
        j <- if (i %% 2 == 1) i + 1 else i - 1
        bt$wp[i] <- 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[i] - bt$AdjEM[j])))
      }
    }
  } else {
    for (g in 1:32) {
      ii <- 2*g - 1; jj <- 2*g
      p <- 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[ii] - bt$AdjEM[jj])))
      bt$wp[ii] <- p; bt$wp[jj] <- 1 - p
    }
  }
  
  # R2 compound win probabilities → wp_drop
  bt[, wp_r2 := 0.0]
  for (g in 1:16) {
    i1 <- 4*(g-1) + 1; i2 <- i1+1; i3 <- i1+2; i4 <- i1+3
    for (ti in c(i1, i2)) {
      p3 <- 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[ti] - bt$AdjEM[i3])))
      p4 <- 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[ti] - bt$AdjEM[i4])))
      bt$wp_r2[ti] <- bt$wp[ti] * (bt$wp[i3]*p3 + bt$wp[i4]*p4)
    }
    for (ti in c(i3, i4)) {
      p1 <- 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[ti] - bt$AdjEM[i1])))
      p2 <- 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[ti] - bt$AdjEM[i2])))
      bt$wp_r2[ti] <- bt$wp[ti] * (bt$wp[i1]*p1 + bt$wp[i2]*p2)
    }
  }
  bt[, wp_drop := pmin(wp - wp_r2, 0.45)]
  
  # Championship probability
  sim_rds <- file.path(script_dir, sprintf("sim_results_%d.rds", year))
  bt[, champ_prob := 0.0]
  if (file.exists(sim_rds)) {
    sim_yr <- readRDS(sim_rds)
    champ_game <- sim_yr$all_results[, 63]
    counts <- tabulate(champ_game, nbins = nrow(sim_yr$teams))
    bt[, champ_prob := counts[team_id] / sim_yr$n_sims]
  } else {
    # Fallback: use AdjEM-based proxy
    bt[, champ_prob := pmax(AdjEM, 0)^3 / sum(pmax(AdjEM, 0)^3)]
  }
  
  # Game day assignment
  bt[, game_day := NA_integer_]
  if (!is.null(cl_dates) && length(cl_dates) > 0) {
    all_dates <- sort(unique(unlist(lapply(cl_dates, as.character))))
    date_rank <- setNames(seq_along(all_dates), all_dates)
    for (i in seq_len(nrow(bt))) {
      d <- cl_dates[[bt$name[i]]]
      if (!is.null(d)) bt$game_day[i] <- date_rank[as.character(d)]
    }
    for (i in which(is.na(bt$game_day))) {
      j <- if (i %% 2 == 1) i + 1 else i - 1
      if (!is.na(bt$game_day[j])) bt$game_day[i] <- bt$game_day[j]
    }
  }
  
  bt[, year := year]
  bt
}

# ==============================================================================
# LOAD DATA & BUILD TRAINING SET
# ==============================================================================

cat("=== OWNERSHIP MODELS — Softmax + GAM ===\n\n")

splash_raw <- rbindlist(lapply(SPLASH_YEARS, function(yr) {
  f <- file.path(script_dir, "splash_ownership", sprintf("ownership_%d.csv", yr))
  if (!file.exists(f)) { cat(sprintf("  WARNING: not found: %s\n", f)); return(NULL) }
  dt <- fread(f); dt[, year := yr]; dt
}), fill=TRUE)
splash_raw[, day_num := as.integer(sub("^day([0-9]+)_.*$", "\\1", day))]
splash_d12 <- splash_raw[day_num %in% 1:2]

feat_list <- lapply(setNames(SPLASH_YEARS, SPLASH_YEARS), build_features)

match_to_bracket <- function(splash_name, bracket_names) {
  if (splash_name %in% bracket_names) return(splash_name)
  m <- bracket_names[toupper(bracket_names) == toupper(splash_name)]
  if (length(m) > 0) return(m[1])
  for (bn in bracket_names) {
    kpn <- if (bn %in% names(kp_alias)) kp_alias[[bn]] else bn
    if (toupper(splash_name) == toupper(kpn)) return(bn)
  }
  m <- grep(splash_name, bracket_names, value=TRUE, ignore.case=TRUE)
  if (length(m) > 0) return(m[1])
  NA_character_
}

build_training_data <- function() {
  rows <- list()
  for (yr in SPLASH_YEARS) {
    feats <- feat_list[[as.character(yr)]]
    if (is.null(feats)) next
    ft <- copy(feats)
    ft[, ownership := 0.0]
    sp <- splash_d12[year==yr]
    for (i in seq_len(nrow(sp))) {
      bn <- match_to_bracket(sp$team[i], ft$name)
      if (!is.na(bn)) {
        ft[name==bn, ownership := sp$ownership[i]]
        if (is.na(ft[name==bn]$game_day)) ft[name==bn, game_day := sp$day_num[i]]
      }
    }
    rows[[as.character(yr)]] <- ft
  }
  rbindlist(rows, fill=TRUE)
}

train_data <- build_training_data()
train_data <- train_data[game_day %in% 1:2]

# Create group ID for year-day combinations
train_data[, group_id := paste(year, game_day, sep="_")]

# Compute log-share relative to group mean (response for GAMs)
train_data[, group_total := sum(ownership), by=group_id]
train_data[, share := ownership / group_total]
train_data[, log_share := log(share + 1e-6)]  # small constant for zeros

cat(sprintf("Training data: %d observations, %d groups\n\n", 
            nrow(train_data), length(unique(train_data$group_id))))

# ==============================================================================
# SOFTMAX UTILITIES
# ==============================================================================

softmax <- function(x) { x <- x - max(x); exp(x) / sum(exp(x)) }

calc_mse <- function(pred_fn, data) {
  total <- 0; n <- 0
  for (gid in unique(data$group_id)) {
    g <- data[group_id == gid]
    if (nrow(g) < 2) next
    actual <- g$share
    pred <- pred_fn(g)
    total <- total + sum((pred - actual)^2)
    n <- n + 1
  }
  if (n == 0) return(1e6)
  total / n
}

# ==============================================================================
# SOFTMAX MODELS (Parametric)
# ==============================================================================

# M0: Null — uniform
m0_pred <- function(g) rep(1/nrow(g), nrow(g))

# M1: WinProb only (1 param)
m1_pred <- function(par, g) softmax(par[1] * g$wp)

# M2: Seed only (1 param) — lower seed = higher share
m2_pred <- function(par, g) softmax(par[1] * (1 - g$seed/16))

# M3: WP + Seed (2 params)
m3_pred <- function(par, g) softmax(par[1]*g$wp + par[2]*(1 - g$seed/16))

# M4: All 4 features linear (4 params)
# wp, champ_prob, wp_drop (negative = bad), seed
m4_pred <- function(par, g) {
  softmax(par[1]*g$wp + par[2]*g$champ_prob + par[3]*g$wp_drop + par[4]*(1 - g$seed/16))
}

# ==============================================================================
# FIT SOFTMAX MODELS
# ==============================================================================

cat("Fitting softmax models...\n")

# M0: no params
m0_mse <- calc_mse(m0_pred, train_data)

# M1: grid search
m1_grid <- seq(0, 10, by=0.2)
m1_losses <- sapply(m1_grid, function(b) calc_mse(function(g) m1_pred(b, g), train_data))
m1_par <- m1_grid[which.min(m1_losses)]
m1_mse <- min(m1_losses)

# M2: grid search
m2_grid <- seq(0, 10, by=0.2)
m2_losses <- sapply(m2_grid, function(b) calc_mse(function(g) m2_pred(b, g), train_data))
m2_par <- m2_grid[which.min(m2_losses)]
m2_mse <- min(m2_losses)

# M3: 2D grid + refine
m3_best <- list(par=c(0,0), mse=Inf)
for (b1 in seq(0, 8, by=0.5)) {
  for (b2 in seq(0, 8, by=0.5)) {
    mse <- calc_mse(function(g) m3_pred(c(b1, b2), g), train_data)
    if (mse < m3_best$mse) m3_best <- list(par=c(b1, b2), mse=mse)
  }
}
m3_fit <- optim(m3_best$par, function(p) calc_mse(function(g) m3_pred(p, g), train_data),
                method="L-BFGS-B", lower=c(0,0), upper=c(15,15))
m3_par <- m3_fit$par
m3_mse <- m3_fit$value

# M4: 4D optimization
m4_start <- c(2, 5, -2, 2)  # wp, champ, drop (negative), seed
m4_fit <- optim(m4_start, function(p) calc_mse(function(g) m4_pred(p, g), train_data),
                method="L-BFGS-B", lower=c(-5,-10,-10,-5), upper=c(15,20,10,15),
                control=list(maxit=2000))
m4_par <- m4_fit$par
m4_mse <- m4_fit$value

# ==============================================================================
# GAM MODELS
# ==============================================================================

cat("Fitting GAM models...\n")

# For GAMs: predict log-attractiveness, then apply softmax within groups
# Key: use low k (basis dimension) and gamma > 1 (extra smoothing penalty)

# G1: Additive smooth effects only
# s(wp) + s(champ_prob) + s(seed) — k=4 is minimal for smooths
gam_g1 <- gam(log_share ~ s(wp, k=4) + s(champ_prob, k=4) + s(seed, k=4),
              data = train_data, 
              method = "REML",
              gamma = 1.5,  # extra penalty to prevent overfitting
              select = TRUE)  # allows terms to be zeroed out

g1_pred_fn <- function(g) {
  scores <- predict(gam_g1, newdata = g)
  softmax(scores)
}
g1_mse <- calc_mse(g1_pred_fn, train_data)

# G2: Add wp:seed interaction using tensor product (ti = pure interaction)
# ti() with k=3 is very constrained
gam_g2 <- gam(log_share ~ s(wp, k=4) + s(champ_prob, k=4) + s(seed, k=4) + 
                          ti(wp, seed, k=3),
              data = train_data,
              method = "REML", 
              gamma = 1.5,
              select = TRUE)

g2_pred_fn <- function(g) {
  scores <- predict(gam_g2, newdata = g)
  softmax(scores)
}
g2_mse <- calc_mse(g2_pred_fn, train_data)

# G3: Include wp_drop, minimal interaction
gam_g3 <- gam(log_share ~ s(wp, k=4) + s(champ_prob, k=4) + s(wp_drop, k=4) + 
                          s(seed, k=4) + ti(wp, seed, k=3),
              data = train_data,
              method = "REML",
              gamma = 2.0,  # even more penalty with more terms
              select = TRUE)

g3_pred_fn <- function(g) {
  scores <- predict(gam_g3, newdata = g)
  softmax(scores)
}
g3_mse <- calc_mse(g3_pred_fn, train_data)

# ==============================================================================
# COLLECT ALL MODELS
# ==============================================================================

fits <- list(
  m0 = list(par=NULL, mse=m0_mse, name="Null (Uniform)", n_params=0, type="softmax"),
  m1 = list(par=m1_par, mse=m1_mse, name="WinProb-Only", n_params=1, type="softmax"),
  m2 = list(par=m2_par, mse=m2_mse, name="Seed-Only", n_params=1, type="softmax"),
  m3 = list(par=m3_par, mse=m3_mse, name="WP + Seed", n_params=2, type="softmax"),
  m4 = list(par=m4_par, mse=m4_mse, name="All Linear", n_params=4, type="softmax"),
  g1 = list(gam=gam_g1, mse=g1_mse, name="GAM Additive", n_params=sum(gam_g1$edf), type="gam"),
  g2 = list(gam=gam_g2, mse=g2_mse, name="GAM + wp:seed", n_params=sum(gam_g2$edf), type="gam"),
  g3 = list(gam=gam_g3, mse=g3_mse, name="GAM Full", n_params=sum(gam_g3$edf), type="gam")
)

# ==============================================================================
# LEAVE-ONE-OUT CROSS-VALIDATION
# ==============================================================================

cat("Running leave-one-out CV...\n\n")

groups <- unique(train_data$group_id)
loo_results <- list()

for (mn in names(fits)) {
  loo_mse <- numeric(length(groups))
  
  for (i in seq_along(groups)) {
    test_gid <- groups[i]
    train_fold <- train_data[group_id != test_gid]
    test_fold  <- train_data[group_id == test_gid]
    
    if (nrow(test_fold) < 2 || nrow(train_fold) < 10) { loo_mse[i] <- NA; next }
    
    # Refit on training fold
    if (mn == "m0") {
      pred <- m0_pred(test_fold)
    } else if (mn == "m1") {
      losses <- sapply(m1_grid, function(b) calc_mse(function(g) m1_pred(b, g), train_fold))
      par <- m1_grid[which.min(losses)]
      pred <- m1_pred(par, test_fold)
    } else if (mn == "m2") {
      losses <- sapply(m2_grid, function(b) calc_mse(function(g) m2_pred(b, g), train_fold))
      par <- m2_grid[which.min(losses)]
      pred <- m2_pred(par, test_fold)
    } else if (mn == "m3") {
      best <- list(par=c(2,2), mse=Inf)
      for (b1 in seq(0, 8, by=1)) {
        for (b2 in seq(0, 8, by=1)) {
          mse <- calc_mse(function(g) m3_pred(c(b1,b2), g), train_fold)
          if (mse < best$mse) best <- list(par=c(b1,b2), mse=mse)
        }
      }
      pred <- m3_pred(best$par, test_fold)
    } else if (mn == "m4") {
      fit <- tryCatch(
        optim(c(2,5,-2,2), function(p) calc_mse(function(g) m4_pred(p, g), train_fold),
              method="L-BFGS-B", lower=c(-5,-10,-10,-5), upper=c(15,20,10,15)),
        error = function(e) list(par=c(2,5,-2,2)))
      pred <- m4_pred(fit$par, test_fold)
    } else if (mn == "g1") {
      gfit <- tryCatch(
        gam(log_share ~ s(wp, k=4) + s(champ_prob, k=4) + s(seed, k=4),
            data=train_fold, method="REML", gamma=1.5, select=TRUE),
        error = function(e) NULL)
      if (is.null(gfit)) { loo_mse[i] <- NA; next }
      pred <- softmax(predict(gfit, newdata=test_fold))
    } else if (mn == "g2") {
      gfit <- tryCatch(
        gam(log_share ~ s(wp, k=4) + s(champ_prob, k=4) + s(seed, k=4) + ti(wp, seed, k=3),
            data=train_fold, method="REML", gamma=1.5, select=TRUE),
        error = function(e) NULL)
      if (is.null(gfit)) { loo_mse[i] <- NA; next }
      pred <- softmax(predict(gfit, newdata=test_fold))
    } else if (mn == "g3") {
      gfit <- tryCatch(
        gam(log_share ~ s(wp, k=4) + s(champ_prob, k=4) + s(wp_drop, k=4) + 
                        s(seed, k=4) + ti(wp, seed, k=3),
            data=train_fold, method="REML", gamma=2.0, select=TRUE),
        error = function(e) NULL)
      if (is.null(gfit)) { loo_mse[i] <- NA; next }
      pred <- softmax(predict(gfit, newdata=test_fold))
    }
    
    actual <- test_fold$share
    loo_mse[i] <- sum((pred - actual)^2)
  }
  
  loo_results[[mn]] <- mean(loo_mse, na.rm=TRUE)
}

# ==============================================================================
# RESULTS SUMMARY
# ==============================================================================

cat("==============================================================\n")
cat("  MODEL COMPARISON\n")
cat("==============================================================\n\n")

cat(sprintf("%-16s %6s %10s %10s %10s %10s\n", 
            "Model", "EDF", "Train MSE", "LOO-MSE", "Train↓", "LOO↓"))
cat(paste(rep("-", 70), collapse=""), "\n")

for (mn in names(fits)) {
  f <- fits[[mn]]
  edf <- if (f$type == "gam") sprintf("%.1f", f$n_params) else sprintf("%d", f$n_params)
  train_imp <- if (mn == "m0") "—" else sprintf("%.1f%%", 100*(1 - f$mse/m0_mse))
  loo_imp <- if (mn == "m0") "—" else sprintf("%.1f%%", 100*(1 - loo_results[[mn]]/loo_results[["m0"]]))
  cat(sprintf("%-16s %6s %10.6f %10.6f %10s %10s\n", 
              f$name, edf, f$mse, loo_results[[mn]], train_imp, loo_imp))
}

# Overfitting check
cat("\n  Overfitting check (Train MSE / LOO-MSE ratio, <1 = overfit):\n")
for (mn in names(fits)) {
  ratio <- fits[[mn]]$mse / loo_results[[mn]]
  flag <- if (ratio < 0.7) " ⚠️ OVERFIT" else if (ratio < 0.85) " (mild)" else ""
  cat(sprintf("    %-16s: %.3f%s\n", fits[[mn]]$name, ratio, flag))
}

# ==============================================================================
# FITTED PARAMETERS
# ==============================================================================

cat("\n==============================================================\n")
cat("  FITTED PARAMETERS\n")
cat("==============================================================\n\n")

cat("SOFTMAX MODELS:\n")
cat(sprintf("  M1 (WinProb):   β_wp = %.2f\n", m1_par))
cat(sprintf("  M2 (Seed):      β_seed = %.2f\n", m2_par))
cat(sprintf("  M3 (WP+Seed):   β_wp = %.2f, β_seed = %.2f\n", m3_par[1], m3_par[2]))
cat(sprintf("  M4 (All):       β_wp = %.2f, β_champ = %.2f, β_drop = %.2f, β_seed = %.2f\n",
            m4_par[1], m4_par[2], m4_par[3], m4_par[4]))

cat("\nGAM SUMMARIES:\n")
cat("\n  G1 (Additive):\n")
print(summary(gam_g1)$s.table)

cat("\n  G2 (+ wp:seed interaction):\n
")
print(summary(gam_g2)$s.table)

cat("\n  G3 (Full with wp_drop):\n")
print(summary(gam_g3)$s.table)

# ==============================================================================
# 2026 PREDICTIONS
# ==============================================================================

cat("\n==============================================================\n")
cat(sprintf("  %d PREDICTIONS\n", TARGET_YEAR))
cat("==============================================================\n")

feats_2026 <- build_features(TARGET_YEAR)

predict_day <- function(day_grp, day_num, models_to_use = c("m0","m3","m4","g1","g2")) {
  g <- day_grp[!is.na(wp)]
  if (nrow(g) == 0) { cat("No teams to predict.\n"); return(invisible(NULL)) }
  
  # Compute all predictions
  preds <- list(
    m0 = m0_pred(g),
    m1 = m1_pred(m1_par, g),
    m2 = m2_pred(m2_par, g),
    m3 = m3_pred(m3_par, g),
    m4 = m4_pred(m4_par, g),
    g1 = softmax(predict(gam_g1, newdata=g)),
    g2 = softmax(predict(gam_g2, newdata=g)),
    g3 = softmax(predict(gam_g3, newdata=g))
  )
  
  dt <- data.table(
    team = g$name,
    seed = g$seed,
    wp = round(100*g$wp, 1),
    champ = round(100*g$champ_prob, 2),
    drop = round(100*g$wp_drop, 1)
  )
  
  for (mn in models_to_use) {
    dt[, (mn) := round(100*preds[[mn]], 1)]
  }
  
  # Sort by best model (g2 or m4)
  sort_col <- if ("g2" %in% models_to_use) "g2" else if ("m4" %in% models_to_use) "m4" else "m3"
  setorderv(dt, sort_col, order=-1)
  
  cat(sprintf("\n%d Day %d — %d teams:\n", TARGET_YEAR, day_num, nrow(dt)))
  
  # Print header
  hdr <- sprintf("%-20s %4s %5s %5s %5s", "Team", "Seed", "WP%", "Chmp", "Drop")
  for (mn in models_to_use) hdr <- paste0(hdr, sprintf(" %6s", toupper(mn)))
  cat(hdr, "\n")
  cat(paste(rep("-", nchar(hdr)), collapse=""), "\n")
  
  for (i in seq_len(nrow(dt))) {
    row <- sprintf("%-20s  %2d  %4.0f%% %4.1f%% %4.0f%%",
                   dt$team[i], dt$seed[i], dt$wp[i], dt$champ[i], dt$drop[i])
    for (mn in models_to_use) {
      row <- paste0(row, sprintf(" %5.1f%%", dt[[mn]][i]))
    }
    cat(row, "\n")
  }
  
  invisible(dt)
}

if (!is.null(feats_2026)) {
  days_present <- sort(unique(feats_2026$game_day[feats_2026$game_day %in% 1:2 & 
                                                   !is.na(feats_2026$game_day)]))
  if (length(days_present) > 0) {
    predictions_2026 <- list()
    for (d in days_present) {
      predictions_2026[[paste0("day", d)]] <- predict_day(
        feats_2026[game_day == d], d,
        models_to_use = c("m0", "m3", "m4", "g1", "g2")
      )
    }
  } else {
    cat("\nNo game_day assignments. Manual: predict_day(feats_2026[...], day_num=1)\n")
  }
} else {
  cat(sprintf("Could not build features for %d.\n", TARGET_YEAR))
}

# ==============================================================================
# EXPORT
# ==============================================================================

cat("\n--------------------------------------------------------------\n")
cat("Exported objects:\n")
cat("  fits           — all model fits (softmax + GAM)\n
")
cat("  loo_results    — LOO-CV MSE for each model\n")
cat("  train_data     — training data with features\n")
cat("  feats_2026     — 2026 features\n")
cat("  gam_g1/g2/g3   — fitted GAM objects (use plot() or summary())\n")
cat("  predict_day()  — generate predictions for any team group\n")
cat("--------------------------------------------------------------\n")

# ==============================================================================
# G2 PREDICTIONS — 2026
# ==============================================================================

if (!is.null(feats_2026)) {
  
  for (d in 1:2) {
    g <- feats_2026[game_day == d & !is.na(wp)]
    if (nrow(g) == 0) next
    
    scores <- predict(gam_g2b, newdata = g)
    g2_pred <- softmax(scores)
    
    out <- data.table(
      team = g$name,
      seed = g$seed,
      wp = round(100 * g$wp, 1),
      G2 = round(100 * g2_pred, 2)
    )
    setorder(out, -G2)
    
    cat(sprintf("\n=== 2026 Day %d — G2 Predictions ===\n\n", d))
    cat(sprintf("%-22s %4s %6s %8s\n", "Team", "Seed", "WP%", "G2 Own%"))
    cat(paste(rep("-", 44), collapse=""), "\n")
    
    for (i in seq_len(nrow(out))) {
      cat(sprintf("%-22s  %2d  %5.1f%% %7.2f%%\n",
                  out$team[i], out$seed[i], out$wp[i], out$G2[i]))
    }
    
    cat(sprintf("\nTop-5 ownership: %.1f%%\n", sum(sort(out$G2, decreasing=TRUE)[1:5])))
  }
}

# ==============================================================================
# G2 TRAINING ERRORS
# ==============================================================================

cat("\n\n=== G2 TRAINING ERRORS ===\n")

train_data[, g2_score := predict(gam_g2b, newdata = .SD)]
train_data[, g2_pred := softmax(g2_score), by = group_id]
train_data[, error := g2_pred - share]
train_data[, abs_error := abs(error)]

# Largest errors
cat("\nLargest errors (sorted by |error|):\n\n")
cat(sprintf("%-20s %4s %5s %7s %7s %7s  %s\n",
            "Team", "Seed", "WP%", "Actual", "Pred", "Error", "Group"))
cat(paste(rep("-", 75), collapse=""), "\n")

big_errors <- train_data[order(-abs_error)]
for (i in seq_len(min(20, nrow(big_errors)))) {
  row <- big_errors[i]
  cat(sprintf("%-20s  %2d  %4.0f%% %6.2f%% %6.2f%% %+6.2f%%  %s\n",
              row$name, row$seed, 100*row$wp, 
              100*row$share, 100*row$g2_pred, 100*row$error,
              row$group_id))
}

# Error by seed
cat("\n\nError by seed:\n")
cat(sprintf("%4s %4s %8s %8s\n", "Seed", "N", "Mean Err", "MAE"))
cat(paste(rep("-", 28), collapse=""), "\n")
seed_err <- train_data[, .(n=.N, me=mean(error), mae=mean(abs_error)), by=seed][order(seed)]
for (i in seq_len(nrow(seed_err))) {
  cat(sprintf("%4d %4d %+7.2f%% %7.2f%%\n",
              seed_err$seed[i], seed_err$n[i], 100*seed_err$me[i], 100*seed_err$mae[i]))
}

# Cleanup
train_data[, c("g2_score", "g2_pred", "error", "abs_error") := NULL]


# ==============================================================================
# INVESTIGATING 3-SEED ERROR
# ==============================================================================

cat("=== 3-SEED DEEP DIVE ===\n\n")

# Get all 3-seeds from training data
train_data[, g2_score := predict(gam_g2b, newdata = .SD)]
train_data[, g2_pred := softmax(g2_score), by = group_id]

threes <- train_data[seed == 3][order(group_id, -share)]

cat("All 3-seeds in training data:\n\n")
cat(sprintf("%-20s %5s %6s %7s %7s %7s  %s\n",
            "Team", "WP%", "Champ%", "Actual", "Pred", "Error", "Group"))
cat(paste(rep("-", 72), collapse=""), "\n")

for (i in seq_len(nrow(threes))) {
  row <- threes[i]
  cat(sprintf("%-20s %4.0f%% %5.2f%% %6.2f%% %6.2f%% %+6.2f%%  %s\n",
              row$name, 100*row$wp, 100*row$champ_prob,
              100*row$share, 100*row$g2_pred, 100*(row$g2_pred - row$share),
              row$group_id))
}

# Compare 3-seeds to similar WP teams of other seeds
cat("\n\nComparing 3-seeds to teams with similar WP (70-90%):\n\n")

similar_wp <- train_data[wp >= 0.70 & wp <= 0.90][order(seed, -share)]

cat(sprintf("%4s %4s %5s %7s %7s %7s\n", "Seed", "N", "Avg WP", "Avg Act", "Avg Pred", "Avg Err"))
cat(paste(rep("-", 42), collapse=""), "\n")

comp <- similar_wp[, .(
  n = .N,
  avg_wp = mean(wp),
  avg_actual = mean(share),
  avg_pred = mean(g2_pred),
  avg_err = mean(g2_pred - share)
), by = seed][order(seed)]

for (i in seq_len(nrow(comp))) {
  cat(sprintf("%4d %4d %4.0f%% %6.2f%% %6.2f%% %+6.2f%%\n",
              comp$seed[i], comp$n[i], 100*comp$avg_wp[i],
              100*comp$avg_actual[i], 100*comp$avg_pred[i], 100*comp$avg_err[i]))
}

# What's the actual ownership share by seed?
cat("\n\nActual ownership by seed (training data):\n\n")
cat(sprintf("%4s %7s %7s %7s\n", "Seed", "Avg Own", "Min", "Max"))
cat(paste(rep("-", 30), collapse=""), "\n")

by_seed <- train_data[, .(
  avg = mean(share),
  min = min(share),
  max = max(share)
), by = seed][order(seed)]

for (i in seq_len(nrow(by_seed))) {
  cat(sprintf("%4d %6.2f%% %6.2f%% %6.2f%%\n",
              by_seed$seed[i], 100*by_seed$avg[i], 100*by_seed$min[i], 100*by_seed$max[i]))
}

# Cleanup
train_data[, c("g2_score", "g2_pred") := NULL]

# Add seed as a factor (uses ~4-5 df effectively due to sparsity at high seeds)
gam_g2b <- gam(log_share ~ s(wp, k=4) + factor(pmin(seed, 8)),
               data = train_data, method = "REML", gamma = 1.5)



# ==============================================================================
# REFIT WITH SEED FACTORS
# ==============================================================================

cat("=== REFITTING WITH SEED FACTORS ===\n\n")

# Create seed factor — collapse 9+ since they're all near zero
train_data[, seed_f := factor(pmin(seed, 9), levels = 1:9, 
                               labels = c("1","2","3","4","5","6","7","8","9+"))]

# G2f: Replace s(seed) and ti(wp,seed) with seed factor + wp:seed_f interaction
gam_g2f <- gam(log_share ~ s(wp, k=4) + seed_f + s(wp, by=seed_f, k=3, m=1),
               data = train_data, method = "REML", gamma = 1.5, select = TRUE)

cat("G2f Summary (seed factors):\n")
print(summary(gam_g2f))

# Simpler version: just main effects, no smooth interaction
gam_g2f_simple <- gam(log_share ~ s(wp, k=4) + seed_f,
                       data = train_data, method = "REML", gamma = 1.5)

cat("\n\nG2f_simple Summary (seed factors, no interaction):\n")
print(summary(gam_g2f_simple))

# Compare models
cat("\n\n=== MODEL COMPARISON ===\n\n")

models <- list(
  g2_original = gam_g2,
  g2f_interact = gam_g2f,
  g2f_simple = gam_g2f_simple
)

# Calc MSE for each
calc_gam_mse <- function(gam_mod, data) {
  data[, tmp_score := predict(gam_mod, newdata = .SD)]
  data[, tmp_pred := softmax(tmp_score), by = group_id]
  mse <- data[, sum((tmp_pred - share)^2), by = group_id][, mean(V1)]
  data[, c("tmp_score", "tmp_pred") := NULL]
  mse
}

cat(sprintf("%-20s %8s %8s\n", "Model", "EDF", "MSE"))
cat(paste(rep("-", 40), collapse=""), "\n")

for (nm in names(models)) {
  mod <- models[[nm]]
  edf <- sum(mod$edf)
  mse <- calc_gam_mse(mod, train_data)
  cat(sprintf("%-20s %8.1f %8.6f\n", nm, edf, mse))
}

# Check 3-seed errors with new model
cat("\n\n=== 3-SEED ERRORS: BEFORE vs AFTER ===\n\n")

train_data[, g2_old := predict(gam_g2, newdata = .SD)]
train_data[, g2_old_pred := softmax(g2_old), by = group_id]

train_data[, g2f_score := predict(gam_g2f_simple, newdata = .SD)]
train_data[, g2f_pred := softmax(g2f_score), by = group_id]

threes <- train_data[seed == 3][order(group_id)]

cat(sprintf("%-20s %5s %7s %7s %7s %7s %7s\n",
            "Team", "WP%", "Actual", "G2 Orig", "Err", "G2f", "Err"))
cat(paste(rep("-", 78), collapse=""), "\n")

for (i in seq_len(nrow(threes))) {
  row <- threes[i]
  cat(sprintf("%-20s %4.0f%% %6.2f%% %6.2f%% %+6.2f%% %6.2f%% %+6.2f%%\n",
              row$name, 100*row$wp, 100*row$share,
              100*row$g2_old_pred, 100*(row$g2_old_pred - row$share),
              100*row$g2f_pred, 100*(row$g2f_pred - row$share)))
}

# Error by seed comparison
cat("\n\n=== ERROR BY SEED: BEFORE vs AFTER ===\n\n")
cat(sprintf("%4s %4s %10s %10s\n", "Seed", "N", "G2 Orig", "G2f"))
cat(paste(rep("-", 32), collapse=""), "\n")

seed_comp <- train_data[, .(
  n = .N,
  g2_mae = mean(abs(g2_old_pred - share)),
  g2f_mae = mean(abs(g2f_pred - share))
), by = seed][order(seed)]

for (i in seq_len(nrow(seed_comp))) {
  cat(sprintf("%4d %4d %9.2f%% %9.2f%%\n",
              seed_comp$seed[i], seed_comp$n[i], 
              100*seed_comp$g2_mae[i], 100*seed_comp$g2f_mae[i]))
}

# Seed coefficients from the simple model
cat("\n\n=== SEED COEFFICIENTS (G2f_simple) ===\n")
cat("Baseline is seed 1. Positive = more ownership than seed 1.\n\n")

coefs <- coef(gam_g2f_simple)
seed_coefs <- coefs[grepl("seed_f", names(coefs))]
cat(sprintf("Seed 1 (baseline): 0.00\n"))
for (i in seq_along(seed_coefs)) {
  cat(sprintf("Seed %s: %+.2f\n", names(seed_coefs)[i], seed_coefs[i]))
}

# Cleanup
train_data[, c("seed_f", "g2_old", "g2_old_pred", "g2f_score", "g2f_pred") := NULL]

# ==============================================================================
# 2026 PREDICTIONS WITH G2f
# ==============================================================================

cat("\n\n=== 2026 PREDICTIONS (G2f_simple) ===\n")

if (!is.null(feats_2026)) {
  feats_2026[, seed_f := factor(pmin(seed, 9), levels = 1:9,
                                 labels = c("1","2","3","4","5","6","7","8","9+"))]
  
  for (d in 1:2) {
    g <- feats_2026[game_day == d & !is.na(wp)]
    if (nrow(g) == 0) next
    
    scores <- predict(gam_g2f_simple, newdata = g)
    pred <- softmax(scores)
    
    out <- data.table(
      team = g$name,
      seed = g$seed,
      wp = round(100 * g$wp, 1),
      own = round(100 * pred, 2)
    )
    setorder(out, -own)
    
    cat(sprintf("\n--- Day %d ---\n\n", d))
    cat(sprintf("%-22s %4s %6s %8s\n", "Team", "Seed", "WP%", "Own%"))
    cat(paste(rep("-", 44), collapse=""), "\n")
    
    for (i in seq_len(nrow(out))) {
      cat(sprintf("%-22s  %2d  %5.1f%% %7.2f%%\n",
                  out$team[i], out$seed[i], out$wp[i], out$own[i]))
    }
    
    cat(sprintf("\nTop-5: %.1f%%\n", sum(sort(out$own, decreasing=TRUE)[1:5])))
  }
}