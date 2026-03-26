#!/usr/bin/env Rscript
# ==============================================================================
# ownership_model_r3.R
# GAM ownership model for Days 5 & 6 (Round 3 / Sweet 16)
#
# Key differences from Days 3-4:
#   - 16 teams play (R2 winners)
#   - Win probabilities come from R3 closing lines
#   - Splash ownership from day_num 5-6
#   - Features include own_r1, own_r2, opp_own_r2
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

# ==============================================================================
# LOAD CLOSING LINES — returns R1 + R2 + R3 data
# ==============================================================================

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

  # --- R1: first 32 unique-team matchups ---
  seen <- character(0); r1_rows <- integer(0)
  for (i in seq_len(nrow(cl))) {
    h <- cl$home_bracket[i]; a <- cl$away_bracket[i]
    if (!(h %in% seen) && !(a %in% seen)) {
      r1_rows <- c(r1_rows, i); seen <- c(seen, h, a)
    }
    if (length(r1_rows) == 32) break
  }
  r1 <- cl[r1_rows]
  r1_teams <- c(r1$home_bracket, r1$away_bracket)

  r1_wp <- list()
  for (i in seq_len(nrow(r1))) {
    if (!is.na(r1$home_win_prob[i])) {
      r1_wp[[r1$home_bracket[i]]] <- r1$home_win_prob[i]
      r1_wp[[r1$away_bracket[i]]] <- 1 - r1$home_win_prob[i]
    }
  }

  # --- R2: next 16 unique-team matchups after R1 ---
  r1_max_date <- max(r1$game_date)
  r2_first_date <- min(cl$game_date[cl$game_date > r1_max_date &
                                      cl$home_bracket %in% r1_teams &
                                      !is.na(cl$home_bracket)])
  r2_cands <- cl[game_date > r1_max_date & game_date <= r2_first_date + 2 &
                   home_bracket %in% r1_teams &
                   away_bracket %in% r1_teams]
  r2_seen <- character(0); r2_rows <- integer(0)
  for (i in seq_len(nrow(r2_cands))) {
    h <- r2_cands$home_bracket[i]; a <- r2_cands$away_bracket[i]
    if (!(h %in% r2_seen) && !(a %in% r2_seen)) {
      r2_rows <- c(r2_rows, i); r2_seen <- c(r2_seen, h, a)
    }
    if (length(r2_rows) == 16) break
  }

  r2_wp <- list(); r2_teams <- character(0)
  if (length(r2_rows) > 0) {
    r2 <- r2_cands[r2_rows]
    r2_teams <- c(r2$home_bracket, r2$away_bracket)
    for (i in seq_len(nrow(r2))) {
      if (!is.na(r2$home_win_prob[i])) {
        r2_wp[[r2$home_bracket[i]]] <- r2$home_win_prob[i]
        r2_wp[[r2$away_bracket[i]]] <- 1 - r2$home_win_prob[i]
      }
    }
  }

  # --- R3: next 8 unique-team matchups after R2 ---
  r3_wp <- list(); r3_dates <- list(); r3_matchups <- list()
  if (length(r2_rows) > 0) {
    r2_max_date <- max(r2_cands[r2_rows]$game_date)
    r3_cands <- cl[game_date > r2_max_date & home_bracket %in% r1_teams & away_bracket %in% r1_teams]
    r3_seen <- character(0); r3_rows <- integer(0)
    for (i in seq_len(nrow(r3_cands))) {
      h <- r3_cands$home_bracket[i]; a <- r3_cands$away_bracket[i]
      if (!(h %in% r3_seen) && !(a %in% r3_seen)) {
        r3_rows <- c(r3_rows, i); r3_seen <- c(r3_seen, h, a)
      }
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

  list(
    r1_wp = r1_wp, r2_wp = r2_wp,
    r3_wp = r3_wp, r3_dates = r3_dates, r3_matchups = r3_matchups
  )
}

# ==============================================================================
# BUILD FEATURES (R3 teams only)
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

  cl_data    <- load_cl_yr(year, bt$name)
  r3_wp      <- if (!is.null(cl_data)) cl_data$r3_wp      else NULL
  r3_dates   <- if (!is.null(cl_data)) cl_data$r3_dates   else NULL
  r3_matchups <- if (!is.null(cl_data)) cl_data$r3_matchups else NULL
  r2_wp      <- if (!is.null(cl_data)) cl_data$r2_wp      else NULL
  r1_wp      <- if (!is.null(cl_data)) cl_data$r1_wp      else NULL

  # R3 win probability (the actual game-level probability)
  bt[, wp := 0.0]
  if (!is.null(r3_wp) && length(r3_wp) > 0) {
    for (i in seq_len(nrow(bt))) {
      w <- r3_wp[[bt$name[i]]]
      if (!is.null(w)) bt$wp[i] <- w
    }
  }

  # R2 win probability (for context / compound features if needed)
  bt[, wp_r2 := 0.0]
  if (!is.null(r2_wp) && length(r2_wp) > 0) {
    for (i in seq_len(nrow(bt))) {
      w <- r2_wp[[bt$name[i]]]
      if (!is.null(w)) bt$wp_r2[i] <- w
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

  # Game day: R3 dates map to days 5-6
  bt[, game_day := NA_integer_]
  if (!is.null(r3_dates) && length(r3_dates) > 0) {
    all_dates <- sort(unique(unlist(lapply(r3_dates, as.character))))
    date_rank <- setNames(seq_along(all_dates) + 4L, all_dates)  # +4 so days are 5,6
    for (i in seq_len(nrow(bt))) {
      d <- r3_dates[[bt$name[i]]]
      if (!is.null(d)) bt$game_day[i] <- date_rank[as.character(d)]
    }
  }

  # R3 opponent
  bt[, r3_opponent := NA_character_]
  if (!is.null(r3_matchups)) {
    for (i in seq_len(nrow(bt))) {
      opp <- r3_matchups[[bt$name[i]]]
      if (!is.null(opp)) bt$r3_opponent[i] <- opp
    }
  }

  bt[, year := year]
  bt
}

# ==============================================================================
# TRAINING DATA
# ==============================================================================

cat("=== GAM Ownership Model — Days 5-6 (Round 3 / Sweet 16) ===\n\n")

splash_raw <- rbindlist(lapply(SPLASH_YEARS, function(yr) {
  f <- file.path(script_dir, "splash_ownership", sprintf("ownership_%d.csv", yr))
  if (!file.exists(f)) { cat(sprintf("  WARNING: not found: %s\n", f)); return(NULL) }
  dt <- fread(f); dt[, year := yr]; dt
}), fill=TRUE)
splash_raw[, day_num := as.integer(sub("^day([0-9]+)_.*$", "\\1", day))]

splash_d56 <- splash_raw[day_num %in% 5:6]

for (yr in SPLASH_YEARS) {
  n5 <- nrow(splash_d56[year==yr & day_num==5])
  n6 <- nrow(splash_d56[year==yr & day_num==6])
  cat(sprintf("  %d: Day5=%d teams listed, Day6=%d teams listed\n", yr, n5, n6))
}

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
    sp <- splash_d56[year == yr]
    unmatched <- character(0)
    for (i in seq_len(nrow(sp))) {
      bn <- match_to_bracket(sp$team[i], ft$name)
      if (!is.na(bn)) {
        ft[name == bn, ownership := sp$ownership[i]]
        if (is.na(ft[name == bn]$game_day)) ft[name == bn, game_day := sp$day_num[i]]
      } else {
        unmatched <- c(unmatched, sp$team[i])
      }
    }
    if (length(unmatched) > 0)
      cat(sprintf("  %d: %d splash teams unmatched: %s\n",
                  yr, length(unmatched), paste(unmatched, collapse=", ")))

    ft <- ft[game_day %in% 5:6 | ownership > 0]
    rows[[as.character(yr)]] <- ft
  }
  rbindlist(rows, fill=TRUE)
}

train_data <- build_training_data()
train_data <- train_data[game_day %in% 5:6]

train_data[, group_id    := paste(year, game_day, sep="_")]
train_data[, group_total := sum(ownership), by = group_id]
train_data[, share       := ownership / group_total]
train_data[, log_share   := log(share + 1e-6)]
cat(sprintf("\nTraining data: %d observations, %d groups\n",
            nrow(train_data), length(unique(train_data$group_id))))
for (yr in SPLASH_YEARS) {
  for (d in 5:6) {
    g <- train_data[year==yr & game_day==d]
    if (nrow(g) == 0) next
    cat(sprintf("  %d Day%d: %d teams (%d with picks > 0, total own=%.3f)\n",
                yr, d, nrow(g), sum(g$ownership>0), sum(g$ownership)))
  }
}

# ==============================================================================
# FIT MODEL
# ==============================================================================

softmax <- function(x) { x <- x - max(x); exp(x) / sum(exp(x)) }

cat("\nFitting GAM...\n")

gam_model <- gam(log_share ~ ti(wp, k=4) + ti(seed, k=4) + ti(wp, seed, k=c(4,4)),
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
  for (d in 5:6) {
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
  days_present <- sort(unique(feats_2026$game_day[
    feats_2026$game_day %in% 5:6 & !is.na(feats_2026$game_day)]))

  if (length(days_present) == 0) {
    cat("\nNo Day 5-6 assignments found for 2026.\n")
    cat("R3 closing lines may not be available yet.\n")
    cat("Once ncaat_2026_closing_lines.csv includes R3 games, re-run.\n")
  }


  for (d in days_present) {
    g <- feats_2026[game_day == d & wp > 0]
    if (nrow(g) == 0) next

    pred <- softmax(predict(gam_model, newdata = g))

    out <- data.table(
      team  = g$name,
      seed  = g$seed,
      wp    = round(100 * g$wp,         1),
      champ = round(100 * g$champ_prob, 2),
      own   = round(100 * pred,         2)
    )
    setorder(out, -own)

    cat(sprintf("\n--- Day %d ---\n\n", d))
    cat(sprintf("%-22s %4s %6s %6s %8s\n",
                "Team", "Seed", "WP%", "Chmp%", "Own%"))
    cat(paste(rep("-", 52), collapse=""), "\n")

    for (i in seq_len(nrow(out))) {
      cat(sprintf("%-22s  %2d  %5.1f%% %5.2f%% %7.2f%%\n",
                  out$team[i], out$seed[i], out$wp[i], out$champ[i], out$own[i]))
    }
    cat(sprintf("\nTop-5: %.1f%%\n", sum(sort(out$own, decreasing=TRUE)[1:min(5, nrow(out))])))
  }
} else {
  cat(sprintf("Could not build features for %d.\n", TARGET_YEAR))
}
