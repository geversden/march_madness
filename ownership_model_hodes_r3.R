#!/usr/bin/env Rscript
# ==============================================================================
# ownership_model_hodes_r3.R
# GAM ownership model for Hodes pool — Round 3 (Sweet 16)
#
# Hodes format: 3 picks in R1, 3 picks in R2, then 1/1/1/1
# R3 = 1 pick per person (Sweet 16)
# ==============================================================================

library(data.table)
library(mgcv)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

HODES_YEARS <- 2023:2025
TARGET_YEAR <- 2026
LOG_SCALE   <- 0.0917

# ==============================================================================
# HELPERS
# ==============================================================================

team_dict_file <- file.path(script_dir, "team_names.csv")
if (!file.exists(team_dict_file)) {
  stop("team_names.csv not found: ", team_dict_file)
}
team_dict     <- fread(team_dict_file)
kp_alias      <- setNames(team_dict$kenpom_name,   team_dict$bracket_name)
cl_to_bracket <- setNames(team_dict$bracket_name,  team_dict$closing_lines_name)

resolve_name <- function(name) {
  name <- trimws(name)
  if (name %in% names(kp_alias)) kp_alias[[name]] else name
}

resolve_cl_to_bracket <- function(cl_name, bracket_teams) {
  if (cl_name %in% names(cl_to_bracket)) {
    return(cl_to_bracket[[cl_name]])
  }
  sorted_bt <- bracket_teams[order(-nchar(bracket_teams))]
  for (bt in sorted_bt) {
    if (
      startsWith(cl_name, bt) &&
        (nchar(cl_name) == nchar(bt) ||
          substr(cl_name, nchar(bt) + 1, nchar(bt) + 1) == " ")
    ) {
      return(bt)
    }
  }
  norm <- function(x) {
    x <- tolower(gsub("[.''`]", "", gsub("\\s+", " ", trimws(x))))
    x <- gsub("\\bst\\b", "state", x)
    x <- gsub("\\bso\\b", "south", x)
    x <- gsub("\\bn\\b",  "north", x)
    x <- gsub("\\bw\\b",  "west",  x)
    x <- gsub("\\be\\b",  "east",  x)
    x
  }
  cn <- norm(cl_name)
  for (bt in sorted_bt) {
    if (norm(bt) == cn) return(bt)
  }
  for (bt in sorted_bt) {
    bn <- norm(bt)
    if (
      startsWith(cn, bn) &&
        (nchar(cn) == nchar(bn) ||
          substr(cn, nchar(bn) + 1, nchar(bn) + 1) == " ")
    ) {
      return(bt)
    }
  }
  NA_character_
}

load_kenpom_yr <- function(year) {
  f <- file.path(script_dir, "kenpom_data", sprintf("kenpom_%d.csv", year))
  if (!file.exists(f)) return(NULL)
  dt <- fread(f)
  if ("adj_em" %in% names(dt)) setnames(dt, "adj_em", "AdjEM")
  if ("NetRtg" %in% names(dt)) { idx <- which(names(dt) == "NetRtg")[1]; names(dt)[idx] <- "AdjEM" }
  if ("team"   %in% names(dt)) setnames(dt, "team", "Team")
  dt[, AdjEM := suppressWarnings(as.numeric(AdjEM))]
  dt <- dt[!is.na(AdjEM) & Team != "" & Team != "Team"]
  dt$Team <- gsub("\\s*\\d+$", "", trimws(dt$Team))
  dt
}

match_to_bracket <- function(hodes_name, bracket_names) {
  if (hodes_name %in% bracket_names) return(hodes_name)
  m <- bracket_names[toupper(bracket_names) == toupper(hodes_name)]
  if (length(m) > 0) return(m[1])
  for (bn in bracket_names) {
    kpn <- if (bn %in% names(kp_alias)) kp_alias[[bn]] else bn
    if (toupper(hodes_name) == toupper(kpn)) return(bn)
  }
  norm <- function(x) tolower(gsub("[.''`]", "", gsub("\\s+", " ", trimws(x))))
  hn <- norm(hodes_name)
  for (bn in bracket_names) {
    if (norm(bn) == hn) return(bn)
    kpn <- if (bn %in% names(kp_alias)) kp_alias[[bn]] else bn
    if (norm(kpn) == hn) return(bn)
  }
  m <- grep(hodes_name, bracket_names, value = TRUE, ignore.case = TRUE, fixed = TRUE)
  if (length(m) > 0) return(m[1])
  m <- grep(hn, sapply(bracket_names, norm), fixed = TRUE)
  if (length(m) > 0) return(bracket_names[m[1]])
  NA_character_
}

# ==============================================================================
# LOAD CLOSING LINES — R1 + R2 + R3
# ==============================================================================

load_cl_yr <- function(year, bracket_teams) {
  f <- file.path(script_dir, "closing_lines", sprintf("ncaat_%d_closing_lines.csv", year))
  if (!file.exists(f)) return(NULL)
  cl <- fread(f)
  cl[, home_bracket := sapply(home_team, resolve_cl_to_bracket, bracket_teams = bracket_teams)]
  cl[, away_bracket := sapply(away_team, resolve_cl_to_bracket, bracket_teams = bracket_teams)]
  unmatched_cl <- unique(c(cl[is.na(home_bracket)]$home_team, cl[is.na(away_bracket)]$away_team))
  if (length(unmatched_cl) > 0)
    cat(sprintf("  CL %d: %d unmatched teams: %s\n", year, length(unmatched_cl), paste(unmatched_cl, collapse = ", ")))
  cl <- cl[!is.na(home_bracket) & !is.na(away_bracket)]
  if (nrow(cl) == 0) return(NULL)
  cl[, game_date := as.Date(date)]
  setorder(cl, game_date)

  # --- R1: first 32 unique-team matchups ---
  seen <- character(0); r1_rows <- integer(0)
  for (i in seq_len(nrow(cl))) {
    h <- cl$home_bracket[i]; a <- cl$away_bracket[i]
    if (!(h %in% seen) && !(a %in% seen)) { r1_rows <- c(r1_rows, i); seen <- c(seen, h, a) }
    if (length(r1_rows) == 32) break
  }
  r1 <- cl[r1_rows]
  r1_teams <- c(r1$home_bracket, r1$away_bracket)

  # --- R2: next 16 unique-team matchups after R1, within +2 days of R2 start ---
  r1_max_date <- max(r1$game_date)
  r2_first_date <- min(cl$game_date[cl$game_date > r1_max_date &
                                      cl$home_bracket %in% r1_teams &
                                      !is.na(cl$home_bracket)])
  r2_cands <- cl[game_date > r1_max_date & game_date <= r2_first_date + 2 &
                   home_bracket %in% r1_teams & away_bracket %in% r1_teams]
  r2_seen <- character(0); r2_rows <- integer(0)
  for (i in seq_len(nrow(r2_cands))) {
    h <- r2_cands$home_bracket[i]; a <- r2_cands$away_bracket[i]
    if (!(h %in% r2_seen) && !(a %in% r2_seen)) { r2_rows <- c(r2_rows, i); r2_seen <- c(r2_seen, h, a) }
    if (length(r2_rows) == 16) break
  }
  cat(sprintf("  CL %d: found %d R2 games (of 16 expected)\n", year, length(r2_rows)))

  # --- R3: next 8 unique-team matchups after R2 ---
  r3_wp <- list(); r3_dates <- list(); r3_matchups <- list()
  if (length(r2_rows) > 0) {
    r2_max_date <- max(r2_cands[r2_rows]$game_date)
    r3_cands <- cl[game_date > r2_max_date & home_bracket %in% r1_teams & away_bracket %in% r1_teams]
    r3_seen <- character(0); r3_rows <- integer(0)
    for (i in seq_len(nrow(r3_cands))) {
      h <- r3_cands$home_bracket[i]; a <- r3_cands$away_bracket[i]
      if (!(h %in% r3_seen) && !(a %in% r3_seen)) { r3_rows <- c(r3_rows, i); r3_seen <- c(r3_seen, h, a) }
      if (length(r3_rows) == 8) break
    }
    if (length(r3_rows) > 0) {
      r3 <- r3_cands[r3_rows]
      for (i in seq_len(nrow(r3))) {
        if (!is.na(r3$home_win_prob[i])) {
          r3_wp[[r3$home_bracket[i]]] <- r3$home_win_prob[i]
          r3_wp[[r3$away_bracket[i]]] <- 1 - r3$home_win_prob[i]
        }
        r3_dates[[r3$home_bracket[i]]] <- r3$game_date[i]
        r3_dates[[r3$away_bracket[i]]] <- r3$game_date[i]
        r3_matchups[[r3$home_bracket[i]]] <- r3$away_bracket[i]
        r3_matchups[[r3$away_bracket[i]]] <- r3$home_bracket[i]
      }
    }
  }

  list(r3_wp = r3_wp, r3_dates = r3_dates, r3_matchups = r3_matchups)
}

# ==============================================================================
# BUILD FEATURES (R3 teams)
# ==============================================================================

build_features <- function(year) {
  bf <- file.path(script_dir, "brackets", sprintf("bracket_%d.csv", year))
  if (!file.exists(bf)) { cat(sprintf("  No bracket for %d\n", year)); return(NULL) }

  br <- fread(bf)
  bt <- data.table(name = br$team, seed = br$seed, region = br$region, team_id = seq_len(nrow(br)))
  bt[, kp_name := sapply(name, resolve_name)]

  kp <- load_kenpom_yr(year)
  if (is.null(kp)) { cat(sprintf("  No KenPom for %d\n", year)); return(NULL) }

  bt <- merge(bt, kp[, .(Team, AdjEM)], by.x = "kp_name", by.y = "Team", all.x = TRUE)
  for (i in which(is.na(bt$AdjEM))) {
    m <- grep(bt$name[i], kp$Team, value = TRUE, ignore.case = TRUE)
    if (length(m) > 0) bt$AdjEM[i] <- kp[Team == m[1]]$AdjEM[1]
  }
  bt[is.na(AdjEM), AdjEM := -15]
  setorder(bt, team_id)

  cl_data     <- load_cl_yr(year, bt$name)
  r3_wp       <- if (!is.null(cl_data)) cl_data$r3_wp       else NULL
  r3_dates    <- if (!is.null(cl_data)) cl_data$r3_dates    else NULL
  r3_matchups <- if (!is.null(cl_data)) cl_data$r3_matchups else NULL

  # R3 win probability
  bt[, wp := 0.0]
  if (!is.null(r3_wp) && length(r3_wp) > 0) {
    for (i in seq_len(nrow(bt))) {
      w <- r3_wp[[bt$name[i]]]
      if (!is.null(w)) bt$wp[i] <- w
    }
  }

  # Championship probability
  sim_rds <- file.path(script_dir, sprintf("sim_results_%d.rds", year))
  bt[, champ_prob := 0.0]
  if (file.exists(sim_rds)) {
    sim_yr     <- readRDS(sim_rds)
    champ_game <- sim_yr$all_results[, 63]
    counts     <- tabulate(champ_game, nbins = nrow(sim_yr$teams))
    bt[, champ_prob := counts[team_id] / sim_yr$n_sims]
  } else {
    bt[, champ_prob := pmax(AdjEM, 0)^3]
  }

  # R3 opponent name
  bt[, r3_opponent := NA_character_]
  if (!is.null(r3_matchups)) {
    for (i in seq_len(nrow(bt))) {
      opp <- r3_matchups[[bt$name[i]]]
      if (!is.null(opp)) bt$r3_opponent[i] <- opp
    }
  }

  # Game day: R3 dates map to days 5-6
  bt[, game_day := NA_integer_]
  if (!is.null(r3_dates) && length(r3_dates) > 0) {
    all_dates <- sort(unique(unlist(lapply(r3_dates, as.character))))
    date_rank <- setNames(seq_along(all_dates) + 4L, all_dates)
    for (i in seq_len(nrow(bt))) {
      d <- r3_dates[[bt$name[i]]]
      if (!is.null(d)) bt$game_day[i] <- date_rank[as.character(d)]
    }
  }

  bt[, year := year]
  bt
}

# ==============================================================================
# HODES OWNERSHIP DATA
# ==============================================================================

cat("=== GAM Ownership Model — Hodes Round 3 (Sweet 16, 1-pick format) ===\n\n")

hodes_raw <- rbindlist(
  lapply(HODES_YEARS, function(yr) {
    f <- file.path(script_dir, "hodes_usage", sprintf("hodes_usage_%d.csv", yr))
    if (!file.exists(f)) { cat(sprintf("  WARNING: not found: %s\n", f)); return(NULL) }
    dt <- fread(f); dt[, year := yr]
    if ("Team"  %in% names(dt)) setnames(dt, "Team",  "team")
    if ("Seed"  %in% names(dt)) setnames(dt, "Seed",  "seed")
    if ("Round" %in% names(dt)) setnames(dt, "Round", "round")
    if (!"ownership" %in% names(dt) && "pick_perc" %in% names(dt))
      dt[, ownership := as.numeric(gsub("%", "", pick_perc)) / 100]
    dt
  }),
  fill = TRUE
)

hodes_r3 <- hodes_raw[round == 3]

for (yr in HODES_YEARS) {
  n <- nrow(hodes_r3[year == yr])
  cat(sprintf("  %d: R3=%d teams listed\n", yr, n))
}

cat("\nBuilding feature matrices...\n")
feat_list <- lapply(setNames(HODES_YEARS, HODES_YEARS), build_features)

# ==============================================================================
# TRAINING DATA
# ==============================================================================

build_training_data <- function() {
  rows <- list()
  for (yr in HODES_YEARS) {
    feats <- feat_list[[as.character(yr)]]
    if (is.null(feats)) next
    ft <- copy(feats)
    ft[, ownership    := 0.0]
    ft[, own_prev     := 0.0]
    sp <- hodes_r3[year == yr]

    # Parse previous_usage_perc once
    if ("previous_usage_perc" %in% names(sp))
      sp[, prev_usage := as.numeric(gsub("%", "", previous_usage_perc)) / 100]
    else
      sp[, prev_usage := 0.0]

    unmatched <- character(0)
    for (i in seq_len(nrow(sp))) {
      bn <- match_to_bracket(sp$team[i], ft$name)
      if (!is.na(bn)) {
        ft[name == bn, ownership := sp$ownership[i]]
        ft[name == bn, own_prev  := sp$prev_usage[i]]
      } else {
        unmatched <- c(unmatched, sp$team[i])
      }
    }
    if (length(unmatched) > 0)
      cat(sprintf("  %d: %d hodes teams unmatched: %s\n", yr, length(unmatched), paste(unmatched, collapse = ", ")))

    # Opponent's previous usage
    ft[, opp_own_prev := 0.0]
    prev_lookup <- setNames(ft$own_prev, ft$name)
    for (i in seq_len(nrow(ft))) {
      opp <- ft$r3_opponent[i]
      if (!is.na(opp) && opp %in% names(prev_lookup))
        ft$opp_own_prev[i] <- prev_lookup[opp]
    }

    ft <- ft[!is.na(r3_opponent) & r3_opponent != ""]
    rows[[as.character(yr)]] <- ft
  }
  rbindlist(rows, fill = TRUE)
}

train_data <- build_training_data()

# Reframe as eligible fraction: teams with high own_prev have fewer available pickers
train_data[, avail     := 1 - own_prev]
train_data[, opp_avail := 1 - opp_own_prev]
train_data[, wp_avail  := wp * avail]

train_data[, group_id    := as.character(year)]
train_data[, group_total := sum(ownership), by = group_id]
train_data[, share       := ownership / group_total]
train_data[, log_share   := log(share + 1e-6)]

cat(sprintf("\nTraining data: %d observations, %d groups\n",
            nrow(train_data), length(unique(train_data$group_id))))
for (yr in HODES_YEARS) {
  g <- train_data[year == yr]
  if (nrow(g) == 0) next
  cat(sprintf("  %d: %d teams (%d with picks > 0, total own=%.3f)\n",
              yr, nrow(g), sum(g$ownership > 0), sum(g$ownership)))
}

# ==============================================================================
# FIT & COMPARE MODELS — avail + opp_avail focus, GAM vs linear
# ==============================================================================

softmax <- function(x) { x <- x - max(x); exp(x) / sum(exp(x)) }

model_specs <- list(
  # Baseline (no avail)
  M03_baseline        = list(f = log_share ~ ti(wp, k=4) + ti(seed, k=4) + ti(wp, seed, k=c(4,4)),              type="gam"),
  # wp_avail product term — extra weight for high-WP + high-avail teams
  G_wpa              = list(f = log_share ~ ti(wp_avail, k=4),                                                   type="gam"),
  G_wpa_seed         = list(f = log_share ~ ti(wp_avail, k=4) + ti(seed, k=4),                                  type="gam"),
  G_wp_wpa           = list(f = log_share ~ ti(wp, k=4) + wp_avail,                                             type="gam"),
  G_wp_seed_wpa      = list(f = log_share ~ ti(wp, k=4) + ti(seed, k=4) + wp_avail,                            type="gam"),
  G_M03_wpa          = list(f = log_share ~ ti(wp, k=4) + ti(seed, k=4) + ti(wp, seed, k=c(4,4)) + wp_avail,   type="gam"),
  L_wpa              = list(f = log_share ~ wp_avail,                                                            type="lm"),
  L_wpa_seed         = list(f = log_share ~ wp_avail + seed,                                                     type="lm"),
  L_wp_wpa           = list(f = log_share ~ wp + wp_avail,                                                      type="lm"),
  L_wp_seed_wpa      = list(f = log_share ~ wp + seed + wp_avail,                                               type="lm")
)

fit_m <- function(spec, data) {
  if (spec$type == "lm") {
    tryCatch(lm(spec$f, data = data), error = function(e) NULL)
  } else {
    tryCatch(gam(spec$f, data = data, method = "REML", gamma = 1.5, select = TRUE), error = function(e) NULL)
  }
}

cv_errors <- function(resids) list(mae = mean(abs(resids)), rmse = sqrt(mean(resids^2)))

loyo_cv <- function(spec, data, years) {
  resids <- c()
  for (yr in years) {
    tr <- data[year != yr]; te <- data[year == yr]
    if (nrow(te) == 0) next
    m  <- fit_m(spec, tr); if (is.null(m)) next
    sc <- tryCatch(predict(m, newdata = te), error = function(e) NULL); if (is.null(sc)) next
    resids <- c(resids, softmax(sc) * te$group_total[1] - te$ownership)
  }
  if (length(resids) == 0) return(list(mae = NA_real_, rmse = NA_real_))
  cv_errors(resids)
}

loo_cv <- function(spec, data, years) {
  resids <- c()
  for (yr in years) {
    yr_data <- data[year == yr]; gt <- yr_data$group_total[1]; n <- nrow(yr_data)
    if (n < 2) next
    for (i in seq_len(n)) {
      tr <- rbind(data[year != yr], yr_data[-i])
      m  <- fit_m(spec, tr); if (is.null(m)) next
      sc <- tryCatch(predict(m, newdata = yr_data), error = function(e) NULL); if (is.null(sc)) next
      resids <- c(resids, softmax(sc)[i] * gt - yr_data$ownership[i])
    }
  }
  cv_errors(resids)
}

cat("\n=== MODEL COMPARISON ===\n\n")
cat(sprintf("%-20s  %5s  %6s %6s  %7s %7s  %6s %6s\n",
            "Model", "R2adj", "IS-MAE", "IS-RMSE", "LOYO-MAE", "LOYO-RMSE", "LOO-MAE", "LOO-RMSE"))
cat(paste(rep("-", 85), collapse = ""), "\n")

fitted_models  <- list()
balance_scores <- list()

for (nm in names(model_specs)) {
  spec <- model_specs[[nm]]
  m    <- fit_m(spec, train_data)
  if (is.null(m)) { cat(sprintf("  %s failed\n", nm)); next }
  fitted_models[[nm]] <- m

  sc  <- predict(m, newdata = train_data)
  pp  <- ave(sc, train_data$group_id, FUN = softmax)
  gt  <- train_data[, group_total[1], by = group_id][match(train_data$group_id, group_id)]$V1
  is  <- cv_errors(pp * gt - train_data$ownership)
  r2  <- if (spec$type == "lm") summary(m)$r.squared else summary(m)$r.sq

  loyo <- loyo_cv(spec, train_data, HODES_YEARS)
  loo  <- loo_cv( spec, train_data, HODES_YEARS)

  balance_scores[[nm]] <- loyo$rmse

  cat(sprintf("%-20s  %5.3f  %5.1f%% %5.1f%%  %7.1f%% %7.1f%%  %5.1f%% %5.1f%%\n",
              nm, r2,
              100*is$mae, 100*is$rmse,
              100*loyo$mae, 100*loyo$rmse,
              100*loo$mae, 100*loo$rmse))
}

best_model <- names(which.min(unlist(balance_scores)))
cat(sprintf("\nSelected: %s\n", best_model))
final_model <- fitted_models[[best_model]]
final_spec  <- model_specs[[best_model]]

# ==============================================================================
# PREDICTIONS ON TRAINING YEARS
# ==============================================================================

sc_is <- predict(final_model, newdata = train_data)
train_data[, pred_own := ave(sc_is, group_id, FUN = softmax) * group_total]

cat("\n\n=== PREDICTIONS ON TRAINING YEARS ===\n")

for (yr in HODES_YEARS) {
  g <- train_data[year == yr]
  if (nrow(g) == 0) next
  setorder(g, -ownership)

  cat(sprintf("\n--- %d Round 3 ---\n\n", yr))
  cat(sprintf("%-22s %4s %6s %7s  %7s\n", "Team", "Seed", "WP%", "Actual%", "Pred%"))
  cat(paste(rep("-", 55), collapse=""), "\n")

  for (i in seq_len(nrow(g))) {
    r <- g[i]
    cat(sprintf("%-22s  %2d  %5.1f%%  %6.1f%%  %6.1f%%\n",
                r$name, r$seed, 100*r$wp, 100*r$ownership, 100*r$pred_own))
  }
  mae <- g[, mean(abs(pred_own - ownership))]
  cat(sprintf("  MAE: %.1f%%\n", 100*mae))
}

train_data[, pred_own := NULL]

# ==============================================================================
# 2026 PREDICTIONS
# ==============================================================================

cat("\n\n=== 2026 PREDICTIONS ===\n")

feats_2026 <- build_features(TARGET_YEAR)

if (!is.null(feats_2026)) {
  # Load 2026 R3 hodes data (has previous_usage_perc for prior rounds)
  own_2026_f <- file.path(script_dir, "hodes_usage", sprintf("hodes_usage_%d.csv", TARGET_YEAR))
  feats_2026[, own_prev     := 0.0]
  feats_2026[, opp_own_prev := 0.0]

  if (file.exists(own_2026_f)) {
    hodes_2026 <- fread(own_2026_f)
    if ("Team"  %in% names(hodes_2026)) setnames(hodes_2026, "Team",  "team")
    if ("Seed"  %in% names(hodes_2026)) setnames(hodes_2026, "Seed",  "seed")
    if ("Round" %in% names(hodes_2026)) setnames(hodes_2026, "Round", "round")
    if (!"ownership" %in% names(hodes_2026) && "pick_perc" %in% names(hodes_2026))
      hodes_2026[, ownership := as.numeric(gsub("%", "", pick_perc)) / 100]
    if ("previous_usage_perc" %in% names(hodes_2026))
      hodes_2026[, prev_usage := as.numeric(gsub("%", "", previous_usage_perc)) / 100]
    else
      hodes_2026[, prev_usage := 0.0]

    sp3 <- hodes_2026[round == 3]
    for (i in seq_len(nrow(sp3))) {
      bn <- match_to_bracket(sp3$team[i], feats_2026$name)
      if (!is.na(bn)) feats_2026[name == bn, own_prev := sp3$prev_usage[i]]
    }
    prev_lookup_2026 <- setNames(feats_2026$own_prev, feats_2026$name)
    for (i in seq_len(nrow(feats_2026))) {
      opp <- feats_2026$r3_opponent[i]
      if (!is.na(opp) && opp %in% names(prev_lookup_2026))
        feats_2026$opp_own_prev[i] <- prev_lookup_2026[opp]
    }
    cat(sprintf("  Loaded R3 previous_usage_perc from %s\n", basename(own_2026_f)))
  } else {
    cat("  No 2026 hodes_usage file found — own_prev will be 0\n")
  }

  feats_2026[, avail     := 1 - own_prev]
  feats_2026[, opp_avail := 1 - opp_own_prev]
  feats_2026[, wp_avail  := wp * avail]

  g <- feats_2026[!is.na(r3_opponent) & wp > 0]
  if (nrow(g) == 0) {
    cat("\nNo R3 matchups found for 2026.\n")
    cat("R3 closing lines may not be available yet.\n")
    cat("Once ncaat_2026_closing_lines.csv includes R3 games, re-run.\n")
  } else {
    pred_shares <- softmax(predict(final_model, newdata = g))
    pred_own    <- pred_shares * 1  # 1 pick per person in R3

    out <- data.table(
      team  = g$name,
      seed  = g$seed,
      wp    = round(100 * g$wp,         1),
      champ = round(100 * g$champ_prob, 2),
      avail = round(100 * g$avail,      1),
      own   = round(100 * pred_own,      1)
    )
    setorder(out, -own)

    cat(sprintf("\n--- 2026 Round 3 (%d teams) ---\n\n", nrow(out)))
    cat(sprintf("%-22s %4s %6s %6s %7s %8s\n",
                "Team", "Seed", "WP%", "Chmp%", "Avail%", "Own%"))
    cat(paste(rep("-", 57), collapse = ""), "\n")

    for (i in seq_len(nrow(out))) {
      cat(sprintf("%-22s  %2d  %5.1f%% %5.2f%% %6.1f%% %7.1f%%\n",
                  out$team[i], out$seed[i], out$wp[i], out$champ[i],
                  out$avail[i], out$own[i]))
    }
    cat(sprintf("\nTop-5: %.1f%%\n", sum(sort(out$own, decreasing = TRUE)[1:min(5, nrow(out))])))
  }
} else {
  cat(sprintf("Could not build features for %d.\n", TARGET_YEAR))
}
