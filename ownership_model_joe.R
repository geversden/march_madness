#!/usr/bin/env Rscript
# ==============================================================================
# ownership_model_joe.R
# GAM ownership model: s(wp) + factor(seed) + s(wp, by=seed_f) interaction
# ==============================================================================

library(data.table)
library(mgcv)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

SPLASH_YEARS <- 2024:2025
TARGET_YEAR  <- 2026
LOG_SCALE    <- 0.0917

# ==============================================================================
# HELPERS
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
    if (!is.na(r1$home_win_prob[i])) {
      wp[[r1$home_bracket[i]]] <- r1$home_win_prob[i]
      wp[[r1$away_bracket[i]]] <- 1 - r1$home_win_prob[i]
    }
    date_map[[r1$home_bracket[i]]] <- r1$game_date[i]
    date_map[[r1$away_bracket[i]]] <- r1$game_date[i]
  }
  list(wp=wp, date_map=date_map)
}

# ==============================================================================
# BUILD FEATURES
# ==============================================================================

build_features <- function(year) {
  bf <- file.path(script_dir, "brackets", sprintf("bracket_%d.csv", year))
  if (!file.exists(bf)) { cat(sprintf("  No bracket for %d\n", year)); return(NULL) }

  br <- fread(bf)
  bt <- data.table(
    name    = br$team,
    seed    = br$seed,
    region  = br$region,
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

  # R1 win probabilities from closing lines (fall back to AdjEM logit)
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

  # Championship probability from simulation
  sim_rds <- file.path(script_dir, sprintf("sim_results_%d.rds", year))
  bt[, champ_prob := 0.0]
  if (file.exists(sim_rds)) {
    sim_yr    <- readRDS(sim_rds)
    champ_game <- sim_yr$all_results[, 63]
    counts     <- tabulate(champ_game, nbins = nrow(sim_yr$teams))
    bt[, champ_prob := counts[team_id] / sim_yr$n_sims]
  } else {
    bt[, champ_prob := pmax(AdjEM, 0)^3 / sum(pmax(AdjEM, 0)^3)]
  }

  # Game day assignment
  bt[, game_day := NA_integer_]
  if (!is.null(cl_dates) && length(cl_dates) > 0) {
    all_dates  <- sort(unique(unlist(lapply(cl_dates, as.character))))
    date_rank  <- setNames(seq_along(all_dates), all_dates)
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
# TRAINING DATA
# ==============================================================================

cat("=== GAM Ownership Model (Categorical Seed + wp Interaction) ===\n\n")

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
    sp <- splash_d12[year == yr]
    for (i in seq_len(nrow(sp))) {
      bn <- match_to_bracket(sp$team[i], ft$name)
      if (!is.na(bn)) {
        ft[name == bn, ownership := sp$ownership[i]]
        if (is.na(ft[name == bn]$game_day)) ft[name == bn, game_day := sp$day_num[i]]
      }
    }
    rows[[as.character(yr)]] <- ft
  }
  rbindlist(rows, fill=TRUE)
}

train_data <- build_training_data()
train_data <- train_data[game_day %in% 1:2]
train_data[, group_id    := paste(year, game_day, sep="_")]
train_data[, group_total := sum(ownership), by = group_id]
train_data[, share       := ownership / group_total]
train_data[, log_share   := log(share + 1e-6)]
train_data[, seed_f      := factor(pmin(seed, 9), levels = 1:9,
                                    labels = c("1","2","3","4","5","6","7","8","9+"))]

cat(sprintf("Training data: %d observations, %d groups\n\n",
            nrow(train_data), length(unique(train_data$group_id))))

# ==============================================================================
# FIT MODEL
# ==============================================================================

softmax <- function(x) { x <- x - max(x); exp(x) / sum(exp(x)) }

cat("Fitting GAM...\n")
gam_model <- gam(log_share ~ s(wp, k=4) + seed_f + s(wp, by=seed_f, k=3, m=1),
                 data = train_data, method = "REML", gamma = 1.5, select = TRUE)

cat("\nModel Summary:\n")
print(summary(gam_model))

# ==============================================================================
# PREDICTIONS ON TRAINING YEARS
# ==============================================================================

cat("\n\n=== PREDICTIONS ON TRAINING YEARS ===\n")

train_data[, pred_score := predict(gam_model, newdata = .SD)]
train_data[, pred_share := softmax(pred_score), by = group_id]

for (yr in SPLASH_YEARS) {
  for (d in 1:2) {
    g <- train_data[year == yr & game_day == d]
    if (nrow(g) == 0) next
    setorder(g, -share)

    cat(sprintf("\n--- %d Day %d ---\n\n", yr, d))
    cat(sprintf("%-22s %4s %6s %8s %8s %8s\n",
                "Team", "Seed", "WP%", "Actual%", "Pred%", "Error"))
    cat(paste(rep("-", 62), collapse=""), "\n")

    for (i in seq_len(nrow(g))) {
      row <- g[i]
      cat(sprintf("%-22s  %2d  %5.1f%% %7.2f%% %7.2f%% %+7.2f%%\n",
                  row$name, row$seed, 100*row$wp,
                  100*row$share, 100*row$pred_share,
                  100*(row$pred_share - row$share)))
    }
    cat(sprintf("\nMAE: %.3f%%\n", 100 * g[, mean(abs(pred_share - share))]))
  }
}

train_data[, c("pred_score", "pred_share") := NULL]

# ==============================================================================
# 2026 PREDICTIONS
# ==============================================================================

cat("\n\n=== 2026 PREDICTIONS ===\n")

feats_2026 <- build_features(TARGET_YEAR)

if (!is.null(feats_2026)) {
  feats_2026[, seed_f := factor(pmin(seed, 9), levels = 1:9,
                                 labels = c("1","2","3","4","5","6","7","8","9+"))]

  days_present <- sort(unique(feats_2026$game_day[
    feats_2026$game_day %in% 1:2 & !is.na(feats_2026$game_day)]))

  for (d in days_present) {
    g <- feats_2026[game_day == d & !is.na(wp)]
    if (nrow(g) == 0) next

    pred <- softmax(predict(gam_model, newdata = g))
    
    # --- Post-processing: re-weight ownership by seed priority ---
    seed_multiplier <- function(s, own_pct) {
      # own_pct is the raw ownership (0-1 scale) used to distinguish
      # "good" vs mediocre teams within a seed group
      switch(as.character(s),
             "4"  = 1.50,
             "5"  = 1.50,
             "3"  = 0.6,
             "6"  = 1.2,
             "7"  = 1.2,
             "8"  = 1.2,
             "9"  = 1.2,
             "2"  = 0.5,
             "10" = 1.1,
             "11" = 1.1,
             "1"  = 0.2,
             1  # 12-16 seeds
      )
    }
    
    adj <- mapply(seed_multiplier, g$seed, pred)
    pred <- pred * adj
    pred <- pred / sum(pred)  # re-normalize to sum to 1
    # --- End post-processing ---
    
    out <- data.table(
      team  = g$name,
      seed  = g$seed,
      wp    = round(100 * g$wp,         1),
      champ = round(100 * g$champ_prob, 2),
      own   = round(100 * pred,         2)
    )
    setorder(out, -own)

    cat(sprintf("\n--- Day %d ---\n\n", d))
    cat(sprintf("%-22s %4s %6s %6s %8s\n", "Team", "Seed", "WP%", "Chmp%", "Own%"))
    cat(paste(rep("-", 50), collapse=""), "\n")

    for (i in seq_len(nrow(out))) {
      cat(sprintf("%-22s  %2d  %5.1f%% %5.2f%% %7.2f%%\n",
                  out$team[i], out$seed[i], out$wp[i], out$champ[i], out$own[i]))
    }
    cat(sprintf("\nTop-5: %.1f%%\n", sum(sort(out$own, decreasing=TRUE)[1:5])))
  }
} else {
  cat(sprintf("Could not build features for %d.\n", TARGET_YEAR))
}
