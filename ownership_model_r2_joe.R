#!/usr/bin/env Rscript
# ==============================================================================
# ownership_model_joe_d34.R
# GAM ownership model for Days 3 & 4 (Round 2)
#
# Key differences from Days 1-2:
#   - Only 32 teams play (R1 winners)
#   - Win probabilities come from R2 closing lines
#   - Splash ownership from day_num 3-4
# ==============================================================================

library(data.table)
library(mgcv)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

SPLASH_YEARS <- 2024:2025
TARGET_YEAR  <- 2026
LOG_SCALE    <- 0.0917

# ==============================================================================
# HELPERS (shared with d12 model)
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
# LOAD CLOSING LINES — returns R1 + R2 data
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

  r1_wp <- list(); r1_dates <- list()
  for (i in seq_len(nrow(r1))) {
    if (!is.na(r1$home_win_prob[i])) {
      r1_wp[[r1$home_bracket[i]]] <- r1$home_win_prob[i]
      r1_wp[[r1$away_bracket[i]]] <- 1 - r1$home_win_prob[i]
    }
    r1_dates[[r1$home_bracket[i]]] <- r1$game_date[i]
    r1_dates[[r1$away_bracket[i]]] <- r1$game_date[i]
  }

  # --- R2: next 16 games involving R1 teams, after R1 dates ---
  r1_max_date <- max(r1$game_date)
  r2_cands <- cl[game_date > r1_max_date &
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

  r2_wp <- list(); r2_dates <- list(); r2_matchups <- list()
  if (length(r2_rows) > 0) {
    r2 <- r2_cands[r2_rows]
    for (i in seq_len(nrow(r2))) {
      if (!is.na(r2$home_win_prob[i])) {
        r2_wp[[r2$home_bracket[i]]] <- r2$home_win_prob[i]
        r2_wp[[r2$away_bracket[i]]] <- 1 - r2$home_win_prob[i]
      }
      r2_dates[[r2$home_bracket[i]]] <- r2$game_date[i]
      r2_dates[[r2$away_bracket[i]]] <- r2$game_date[i]
      # R2 opponent mapping
      r2_matchups[[r2$home_bracket[i]]] <- r2$away_bracket[i]
      r2_matchups[[r2$away_bracket[i]]] <- r2$home_bracket[i]
    }
  }

  list(
    r1_wp = r1_wp, r1_dates = r1_dates,
    r2_wp = r2_wp, r2_dates = r2_dates,
    r2_matchups = r2_matchups
  )
}

# ==============================================================================
# BUILD FEATURES (R2 teams only)
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

  # Load closing lines (R1 + R2)
  cl_data <- load_cl_yr(year, bt$name)
  r1_wp    <- if (!is.null(cl_data)) cl_data$r1_wp    else NULL
  r2_wp       <- if (!is.null(cl_data)) cl_data$r2_wp       else NULL
  r2_dates    <- if (!is.null(cl_data)) cl_data$r2_dates    else NULL
  r2_matchups <- if (!is.null(cl_data)) cl_data$r2_matchups else NULL

  # R1 win prob (needed for compound features even though R1 is over)
  bt[, wp_r1 := 0.0]
  if (!is.null(r1_wp) && length(r1_wp) >= 50) {
    for (i in seq_len(nrow(bt))) {
      w <- r1_wp[[bt$name[i]]]
      if (!is.null(w)) bt$wp_r1[i] <- w
    }
  } else {
    for (g in 1:32) {
      ii <- 2*g - 1; jj <- 2*g
      p <- 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[ii] - bt$AdjEM[jj])))
      bt$wp_r1[ii] <- p; bt$wp_r1[jj] <- 1 - p
    }
  }

  # R2 win prob — the actual game-level probability for the R2 matchup
  bt[, wp := 0.0]
  if (!is.null(r2_wp) && length(r2_wp) > 0) {
    for (i in seq_len(nrow(bt))) {
      w <- r2_wp[[bt$name[i]]]
      if (!is.null(w)) bt$wp[i] <- w
    }
  }
  # For teams without R2 closing lines, estimate from AdjEM vs likely opponent
  # (pod of 4: team i's R2 opponent is from the other half of the pod)
  for (g in 1:16) {
    i1 <- 4*(g-1)+1; i2 <- i1+1; i3 <- i1+2; i4 <- i1+3
    pod <- c(i1, i2, i3, i4)
    for (ti in pod) {
      if (bt$wp[ti] == 0) {
        # Opponent is the strongest team from the other side of the pod
        if (ti %in% c(i1, i2)) opps <- c(i3, i4) else opps <- c(i1, i2)
        # Weighted average against possible opponents
        opp_wts <- bt$wp_r1[opps] / sum(bt$wp_r1[opps] + 1e-9)
        est_wp  <- sum(opp_wts * 1/(1+exp(-LOG_SCALE*(bt$AdjEM[ti] - bt$AdjEM[opps]))))
        bt$wp[ti] <- est_wp
      }
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

  # Game day: R2 dates map to days 3-4
  bt[, game_day := NA_integer_]
  if (!is.null(r2_dates) && length(r2_dates) > 0) {
    all_dates <- sort(unique(unlist(lapply(r2_dates, as.character))))
    # Day 3 = first R2 date, Day 4 = second R2 date
    date_rank <- setNames(seq_along(all_dates) + 2L, all_dates)  # +2 so days are 3,4
    for (i in seq_len(nrow(bt))) {
      d <- r2_dates[[bt$name[i]]]
      if (!is.null(d)) bt$game_day[i] <- date_rank[as.character(d)]
    }
  }

  # R2 opponent name
  bt[, r2_opponent := NA_character_]
  if (!is.null(r2_matchups)) {
    for (i in seq_len(nrow(bt))) {
      opp <- r2_matchups[[bt$name[i]]]
      if (!is.null(opp)) bt$r2_opponent[i] <- opp
    }
  }

  # Only keep teams that have R2 data (i.e., they played in R2)
  bt[, in_r2 := !is.na(game_day) | (wp > 0)]

  bt[, year := year]
  bt
}

# ==============================================================================
# TRAINING DATA
# ==============================================================================

cat("=== GAM Ownership Model — Days 3-4 (Round 2) ===\n\n")

splash_raw <- rbindlist(lapply(SPLASH_YEARS, function(yr) {
  f <- file.path(script_dir, "splash_ownership", sprintf("ownership_%d.csv", yr))
  if (!file.exists(f)) { cat(sprintf("  WARNING: not found: %s\n", f)); return(NULL) }
  dt <- fread(f); dt[, year := yr]; dt
}), fill=TRUE)
splash_raw[, day_num := as.integer(sub("^day([0-9]+)_.*$", "\\1", day))]
splash_d34 <- splash_raw[day_num %in% 3:4]

for (yr in SPLASH_YEARS) {
  n3 <- nrow(splash_d34[year==yr & day_num==3])
  n4 <- nrow(splash_d34[year==yr & day_num==4])
  cat(sprintf("  %d: Day3=%d teams listed, Day4=%d teams listed\n", yr, n3, n4))
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

# Build a lookup of R1 ownership shares per year-day from the d1-2 splash data
splash_d12 <- splash_raw[day_num %in% 1:2]

build_r1_own_lookup <- function() {
  # Returns list: year -> named vector of bracket_name -> R1 ownership share
  lookup <- list()
  for (yr in SPLASH_YEARS) {
    feats <- feat_list[[as.character(yr)]]
    if (is.null(feats)) next
    sp <- splash_d12[year == yr]
    own_vec <- setNames(rep(0.0, nrow(feats)), feats$name)
    for (i in seq_len(nrow(sp))) {
      bn <- match_to_bracket(sp$team[i], feats$name)
      if (!is.na(bn)) own_vec[bn] <- sp$ownership[i]
    }
    # Normalize per game_day to get shares
    for (d in 1:2) {
      day_teams <- feats[game_day == d]$name
      day_teams <- day_teams[day_teams %in% names(own_vec)]
      total <- sum(own_vec[day_teams])
      if (total > 0) own_vec[day_teams] <- own_vec[day_teams] / total
    }
    lookup[[as.character(yr)]] <- own_vec
  }
  lookup
}

r1_own_lookup <- build_r1_own_lookup()

build_training_data <- function() {
  rows <- list()
  for (yr in SPLASH_YEARS) {
    feats <- feat_list[[as.character(yr)]]
    if (is.null(feats)) next
    ft <- copy(feats)
    ft[, ownership := 0.0]
    sp <- splash_d34[year == yr]
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

    # Add R1 opponent ownership: bracket pairs are (1,2), (3,4), ...
    r1_own <- r1_own_lookup[[as.character(yr)]]
    # R2 opponent's R1 ownership + own R1 ownership
    r1_own <- r1_own_lookup[[as.character(yr)]]
    ft[, opp_own_r1 := 0.0]
    ft[, own_r1 := 0.0]
    if (!is.null(r1_own)) {
      for (i in seq_len(nrow(ft))) {
        # Own R1 ownership
        if (ft$name[i] %in% names(r1_own)) {
          ft$own_r1[i] <- r1_own[ft$name[i]]
        }
        # R2 opponent's R1 ownership
        opp <- ft$r2_opponent[i]
        if (!is.na(opp) && opp %in% names(r1_own)) {
          ft$opp_own_r1[i] <- r1_own[opp]
        }
      }
    }

    # Keep only teams that actually played in R2 (have ownership or R2 closing line)
    ft <- ft[game_day %in% 3:4 | ownership > 0]
    rows[[as.character(yr)]] <- ft
  }
  rbindlist(rows, fill=TRUE)
}

train_data <- build_training_data()
train_data <- train_data[game_day %in% 3:4]

train_data[, group_id    := paste(year, game_day, sep="_")]
train_data[, group_total := sum(ownership), by = group_id]
train_data[, share       := ownership / group_total]
train_data[, log_share   := log(share + 1e-6)]
cat(sprintf("\nTraining data: %d observations, %d groups\n",
            nrow(train_data), length(unique(train_data$group_id))))
for (yr in SPLASH_YEARS) {
  for (d in 3:4) {
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

# Smooth main effects + tensor interaction
gam_model <- gam(log_share ~ ti(wp, k=4) + ti(seed, k=4) + ti(wp, seed, k=c(4,4)) + own_r1 + opp_own_r1,
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
  for (d in 3:4) {
    g <- train_data[year == yr & game_day == d]
    if (nrow(g) == 0) next
    setorder(g, -share)

    cat(sprintf("\n--- %d Day %d ---\n\n", yr, d))
    cat(sprintf("%-22s %4s %6s %7s %7s %8s %8s %8s\n",
                "Team", "Seed", "WP%", "OwnR1%", "OppR1%", "Actual%", "Pred%", "Error"))
    cat(paste(rep("-", 78), collapse=""), "\n")

    for (i in seq_len(nrow(g))) {
      row <- g[i]
      cat(sprintf("%-22s  %2d  %5.1f%% %6.2f%% %6.2f%% %7.2f%% %7.2f%% %+7.2f%%\n",
                  row$name, row$seed, 100*row$wp,
                  100*row$own_r1, 100*row$opp_own_r1,
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
  # Add R1 opponent ownership for 2026
  # Try actual splash d12 data first, fall back to uniform
  sp_2026_d12 <- splash_raw[year == TARGET_YEAR & day_num %in% 1:2]
  own_2026 <- setNames(rep(0.0, nrow(feats_2026)), feats_2026$name)
  if (nrow(sp_2026_d12) > 0) {
    for (i in seq_len(nrow(sp_2026_d12))) {
      bn <- match_to_bracket(sp_2026_d12$team[i], feats_2026$name)
      if (!is.na(bn)) own_2026[bn] <- sp_2026_d12$ownership[i]
    }
    # Normalize per game_day
    for (d in 1:2) {
      day_teams <- feats_2026[game_day == d]$name
      day_teams <- day_teams[day_teams %in% names(own_2026)]
      total <- sum(own_2026[day_teams])
      if (total > 0) own_2026[day_teams] <- own_2026[day_teams] / total
    }
    cat("  Using actual Day 1-2 ownership for opp_own_r1\n")
  } else {
    # No d12 ownership yet — use 1/32 per team as placeholder
    own_2026[] <- 1/32
    cat("  No Day 1-2 ownership available — using uniform 1/32 placeholder\n")
  }

  feats_2026[, opp_own_r1 := 0.0]
  feats_2026[, own_r1 := 0.0]
  for (i in seq_len(nrow(feats_2026))) {
    # Own R1 ownership
    if (feats_2026$name[i] %in% names(own_2026)) {
      feats_2026$own_r1[i] <- own_2026[feats_2026$name[i]]
    }
    # R2 opponent's R1 ownership
    opp <- feats_2026$r2_opponent[i]
    if (!is.na(opp) && opp %in% names(own_2026)) {
      feats_2026$opp_own_r1[i] <- own_2026[opp]
    }
  }

  days_present <- sort(unique(feats_2026$game_day[
    feats_2026$game_day %in% 3:4 & !is.na(feats_2026$game_day)]))

  if (length(days_present) == 0) {
    cat("\nNo Day 3-4 assignments found for 2026.\n")
    cat("R2 closing lines may not be available yet.\n")
    cat("Once ncaat_2026_closing_lines.csv includes R2 games, re-run.\n")
  }

  for (d in days_present) {
    g <- feats_2026[game_day == d & wp > 0]
    if (nrow(g) == 0) next

    pred <- softmax(predict(gam_model, newdata = g))

    out <- data.table(
      team   = g$name,
      seed   = g$seed,
      wp     = round(100 * g$wp,         1),
      champ  = round(100 * g$champ_prob, 2),
      own_r1 = round(100 * g$own_r1,     2),
      opp_r1 = round(100 * g$opp_own_r1, 2),
      own    = round(100 * pred,         2)
    )
    setorder(out, -own)

    cat(sprintf("\n--- Day %d ---\n\n", d))
    cat(sprintf("%-22s %4s %6s %6s %7s %7s %8s\n", "Team", "Seed", "WP%", "Chmp%", "OwnR1%", "OppR1%", "Own%"))
    cat(paste(rep("-", 64), collapse=""), "\n")

    for (i in seq_len(nrow(out))) {
      cat(sprintf("%-22s  %2d  %5.1f%% %5.2f%% %6.2f%% %6.2f%% %7.2f%%\n",
                  out$team[i], out$seed[i], out$wp[i], out$champ[i], out$own_r1[i], out$opp_r1[i], out$own[i]))
    }
    cat(sprintf("\nTop-5: %.1f%%\n", sum(sort(out$own, decreasing=TRUE)[1:5])))
  }
} else {
  cat(sprintf("Could not build features for %d.\n", TARGET_YEAR))
}

#!/usr/bin/env Rscript
# ==============================================================================
# ownership_model_hodes_r2.R
# GAM ownership model for Hodes pool — Round 2
#
# Hodes format: 3 picks in R1, 3 picks in R2, then 1/1/1/1
# ==============================================================================

library(data.table)
library(mgcv)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

HODES_YEARS <- 2021:2025
TARGET_YEAR <- 2026
LOG_SCALE <- 0.0917

# ==============================================================================
# HELPERS
# ==============================================================================

team_dict_file <- file.path(script_dir, "team_names.csv")
if (!file.exists(team_dict_file)) {
  stop("team_names.csv not found: ", team_dict_file)
}
team_dict <- fread(team_dict_file)
kp_alias <- setNames(team_dict$kenpom_name, team_dict$bracket_name)
cl_to_bracket <- setNames(team_dict$bracket_name, team_dict$closing_lines_name)

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
  # Normalize: punctuation, abbreviations
  norm <- function(x) {
    x <- tolower(gsub("[.''`]", "", gsub("\\s+", " ", trimws(x))))
    x <- gsub("\\bst\\b", "state", x) # expand St -> State
    x <- gsub("\\bso\\b", "south", x)
    x <- gsub("\\bn\\b", "north", x)
    x <- gsub("\\bw\\b", "west", x)
    x <- gsub("\\be\\b", "east", x)
    x
  }
  cn <- norm(cl_name)
  for (bt in sorted_bt) {
    if (norm(bt) == cn) return(bt)
  }
  # Partial normalized match (bracket name is prefix of CL name)
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
  if (!file.exists(f)) {
    return(NULL)
  }
  dt <- fread(f)
  if ("adj_em" %in% names(dt)) {
    setnames(dt, "adj_em", "AdjEM")
  }
  if ("NetRtg" %in% names(dt)) {
    idx <- which(names(dt) == "NetRtg")[1]
    names(dt)[idx] <- "AdjEM"
  }
  if ("team" %in% names(dt)) {
    setnames(dt, "team", "Team")
  }
  dt[, AdjEM := suppressWarnings(as.numeric(AdjEM))]
  dt <- dt[!is.na(AdjEM) & Team != "" & Team != "Team"]
  dt$Team <- gsub("\\s*\\d+$", "", trimws(dt$Team))
  dt
}

match_to_bracket <- function(hodes_name, bracket_names) {
  if (hodes_name %in% bracket_names) {
    return(hodes_name)
  }
  m <- bracket_names[toupper(bracket_names) == toupper(hodes_name)]
  if (length(m) > 0) {
    return(m[1])
  }
  # Try kenpom alias reverse lookup
  for (bn in bracket_names) {
    kpn <- if (bn %in% names(kp_alias)) kp_alias[[bn]] else bn
    if (toupper(hodes_name) == toupper(kpn)) return(bn)
  }
  # Normalize punctuation: strip periods, apostrophes, extra spaces
  norm <- function(x) tolower(gsub("[.''`]", "", gsub("\\s+", " ", trimws(x))))
  hn <- norm(hodes_name)
  for (bn in bracket_names) {
    if (norm(bn) == hn) {
      return(bn)
    }
    kpn <- if (bn %in% names(kp_alias)) kp_alias[[bn]] else bn
    if (norm(kpn) == hn) return(bn)
  }
  # Partial match
  m <- grep(
    hodes_name,
    bracket_names,
    value = TRUE,
    ignore.case = TRUE,
    fixed = TRUE
  )
  if (length(m) > 0) {
    return(m[1])
  }
  m <- grep(hn, sapply(bracket_names, norm), fixed = TRUE)
  if (length(m) > 0) {
    return(bracket_names[m[1]])
  }
  NA_character_
}

# ==============================================================================
# LOAD CLOSING LINES — R1 + R2
# ==============================================================================

load_cl_yr <- function(year, bracket_teams) {
  f <- file.path(
    script_dir,
    "closing_lines",
    sprintf("ncaat_%d_closing_lines.csv", year)
  )
  if (!file.exists(f)) {
    return(NULL)
  }
  cl <- fread(f)
  cl[,
    home_bracket := sapply(
      home_team,
      resolve_cl_to_bracket,
      bracket_teams = bracket_teams
    )
  ]
  cl[,
    away_bracket := sapply(
      away_team,
      resolve_cl_to_bracket,
      bracket_teams = bracket_teams
    )
  ]
  unmatched_cl <- unique(c(
    cl[is.na(home_bracket)]$home_team,
    cl[is.na(away_bracket)]$away_team
  ))
  if (length(unmatched_cl) > 0) {
    cat(sprintf(
      "  CL %d: %d unmatched teams: %s\n",
      year,
      length(unmatched_cl),
      paste(unmatched_cl, collapse = ", ")
    ))
  }
  cl <- cl[!is.na(home_bracket) & !is.na(away_bracket)]
  if (nrow(cl) == 0) {
    return(NULL)
  }
  cl[, game_date := as.Date(date)]
  setorder(cl, game_date)

  # --- R1: first 32 unique-team matchups ---
  seen <- character(0)
  r1_rows <- integer(0)
  for (i in seq_len(nrow(cl))) {
    h <- cl$home_bracket[i]
    a <- cl$away_bracket[i]
    if (!(h %in% seen) && !(a %in% seen)) {
      r1_rows <- c(r1_rows, i)
      seen <- c(seen, h, a)
    }
    if (length(r1_rows) == 32) break
  }
  r1 <- cl[r1_rows]
  r1_teams <- c(r1$home_bracket, r1$away_bracket)

  r1_wp <- list()
  r1_dates <- list()
  for (i in seq_len(nrow(r1))) {
    if (!is.na(r1$home_win_prob[i])) {
      r1_wp[[r1$home_bracket[i]]] <- r1$home_win_prob[i]
      r1_wp[[r1$away_bracket[i]]] <- 1 - r1$home_win_prob[i]
    }
    r1_dates[[r1$home_bracket[i]]] <- r1$game_date[i]
    r1_dates[[r1$away_bracket[i]]] <- r1$game_date[i]
  }

  # --- R2: next 16 games after R1 ---
  r1_max_date <- max(r1$game_date)
  r2_cands <- cl[
    game_date > r1_max_date &
      home_bracket %in% r1_teams &
      away_bracket %in% r1_teams
  ]
  r2_seen <- character(0)
  r2_rows <- integer(0)
  for (i in seq_len(nrow(r2_cands))) {
    h <- r2_cands$home_bracket[i]
    a <- r2_cands$away_bracket[i]
    if (!(h %in% r2_seen) && !(a %in% r2_seen)) {
      r2_rows <- c(r2_rows, i)
      r2_seen <- c(r2_seen, h, a)
    }
    if (length(r2_rows) == 16) break
  }

  r2_wp <- list()
  r2_dates <- list()
  r2_matchups <- list()
  if (length(r2_rows) > 0) {
    r2 <- r2_cands[r2_rows]
    for (i in seq_len(nrow(r2))) {
      if (!is.na(r2$home_win_prob[i])) {
        r2_wp[[r2$home_bracket[i]]] <- r2$home_win_prob[i]
        r2_wp[[r2$away_bracket[i]]] <- 1 - r2$home_win_prob[i]
      }
      r2_dates[[r2$home_bracket[i]]] <- r2$game_date[i]
      r2_dates[[r2$away_bracket[i]]] <- r2$game_date[i]
      r2_matchups[[r2$home_bracket[i]]] <- r2$away_bracket[i]
      r2_matchups[[r2$away_bracket[i]]] <- r2$home_bracket[i]
    }
  }

  list(
    r1_wp = r1_wp,
    r1_dates = r1_dates,
    r2_wp = r2_wp,
    r2_dates = r2_dates,
    r2_matchups = r2_matchups
  )
}

# ==============================================================================
# BUILD FEATURES
# ==============================================================================

build_features <- function(year) {
  bf <- file.path(script_dir, "brackets", sprintf("bracket_%d.csv", year))
  if (!file.exists(bf)) {
    cat(sprintf("  No bracket for %d\n", year))
    return(NULL)
  }

  br <- fread(bf)
  bt <- data.table(
    name = br$team,
    seed = br$seed,
    region = br$region,
    team_id = seq_len(nrow(br))
  )
  bt[, kp_name := sapply(name, resolve_name)]

  kp <- load_kenpom_yr(year)
  if (is.null(kp)) {
    cat(sprintf("  No KenPom for %d\n", year))
    return(NULL)
  }

  bt <- merge(
    bt,
    kp[, .(Team, AdjEM)],
    by.x = "kp_name",
    by.y = "Team",
    all.x = TRUE
  )
  for (i in which(is.na(bt$AdjEM))) {
    m <- grep(bt$name[i], kp$Team, value = TRUE, ignore.case = TRUE)
    if (length(m) > 0) bt$AdjEM[i] <- kp[Team == m[1]]$AdjEM[1]
  }
  bt[is.na(AdjEM), AdjEM := -15]
  setorder(bt, team_id)

  # Closing lines
  cl_data <- load_cl_yr(year, bt$name)
  r1_wp <- if (!is.null(cl_data)) cl_data$r1_wp else NULL
  r2_wp <- if (!is.null(cl_data)) cl_data$r2_wp else NULL
  r2_dates <- if (!is.null(cl_data)) cl_data$r2_dates else NULL
  r2_matchups <- if (!is.null(cl_data)) cl_data$r2_matchups else NULL

  # R1 win prob
  bt[, wp_r1 := 0.0]
  if (!is.null(r1_wp) && length(r1_wp) >= 50) {
    for (i in seq_len(nrow(bt))) {
      w <- r1_wp[[bt$name[i]]]
      if (!is.null(w)) bt$wp_r1[i] <- w
    }
  } else {
    for (g in 1:32) {
      ii <- 2 * g - 1
      jj <- 2 * g
      p <- 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[ii] - bt$AdjEM[jj])))
      bt$wp_r1[ii] <- p
      bt$wp_r1[jj] <- 1 - p
    }
  }

  # R2 win prob
  bt[, wp := 0.0]
  if (!is.null(r2_wp) && length(r2_wp) > 0) {
    for (i in seq_len(nrow(bt))) {
      w <- r2_wp[[bt$name[i]]]
      if (!is.null(w)) bt$wp[i] <- w
    }
  }
  # Fallback: estimate from AdjEM vs likely opponent in pod
  for (g in 1:16) {
    i1 <- 4 * (g - 1) + 1
    i2 <- i1 + 1
    i3 <- i1 + 2
    i4 <- i1 + 3
    pod <- c(i1, i2, i3, i4)
    for (ti in pod) {
      if (bt$wp[ti] == 0) {
        if (ti %in% c(i1, i2)) {
          opps <- c(i3, i4)
        } else {
          opps <- c(i1, i2)
        }
        opp_wts <- bt$wp_r1[opps] / sum(bt$wp_r1[opps] + 1e-9)
        est_wp <- sum(
          opp_wts * 1 / (1 + exp(-LOG_SCALE * (bt$AdjEM[ti] - bt$AdjEM[opps])))
        )
        bt$wp[ti] <- est_wp
      }
    }
  }

  # Championship probability
  sim_rds <- file.path(script_dir, sprintf("sim_results_%d.rds", year))
  bt[, champ_prob := 0.0]
  if (file.exists(sim_rds)) {
    sim_yr <- readRDS(sim_rds)
    champ_game <- sim_yr$all_results[, 63]
    counts <- tabulate(champ_game, nbins = nrow(sim_yr$teams))
    bt[, champ_prob := counts[team_id] / sim_yr$n_sims]
  } else {
    bt[, champ_prob := pmax(AdjEM, 0)^3]
  }

  # R2 opponent name
  bt[, r2_opponent := NA_character_]
  if (!is.null(r2_matchups)) {
    for (i in seq_len(nrow(bt))) {
      opp <- r2_matchups[[bt$name[i]]]
      if (!is.null(opp)) bt$r2_opponent[i] <- opp
    }
  }

  # R2 opponent seed and seed difference
  bt[, opp_seed := NA_integer_]
  bt[, seed_diff := NA_real_]
  for (i in seq_len(nrow(bt))) {
    opp <- bt$r2_opponent[i]
    if (!is.na(opp)) {
      opp_row <- bt[name == opp]
      if (nrow(opp_row) > 0) {
        bt$opp_seed[i] <- opp_row$seed[1]
        bt$seed_diff[i] <- bt$seed[i] - opp_row$seed[1] # positive = higher seed (worse)
      }
    }
  }

  # Game day (R2 dates -> days 3-4)
  bt[, game_day := NA_integer_]
  if (!is.null(r2_dates) && length(r2_dates) > 0) {
    all_dates <- sort(unique(unlist(lapply(r2_dates, as.character))))
    date_rank <- setNames(seq_along(all_dates) + 2L, all_dates)
    for (i in seq_len(nrow(bt))) {
      d <- r2_dates[[bt$name[i]]]
      if (!is.null(d)) bt$game_day[i] <- date_rank[as.character(d)]
    }
  }

  bt[, in_r2 := !is.na(game_day) | (wp > 0)]
  bt[, year := year]
  bt
}

# ==============================================================================
# HODES OWNERSHIP DATA
# ==============================================================================

cat("=== GAM Ownership Model — Hodes Round 2 (3-pick format) ===\n\n")

hodes_raw <- rbindlist(
  lapply(HODES_YEARS, function(yr) {
    f <- file.path(script_dir, "hodes_usage", sprintf("hodes_usage_%d.csv", yr))
    if (!file.exists(f)) {
      cat(sprintf("  WARNING: not found: %s\n", f))
      return(NULL)
    }
    dt <- fread(f)
    dt[, year := yr]
    if ("Team" %in% names(dt)) {
      setnames(dt, "Team", "team")
    }
    if ("Seed" %in% names(dt)) {
      setnames(dt, "Seed", "seed")
    }
    if ("Round" %in% names(dt)) {
      setnames(dt, "Round", "round")
    }
    if (!"ownership" %in% names(dt) && "pick_perc" %in% names(dt)) {
      dt[, ownership := as.numeric(gsub("%", "", pick_perc)) / 100]
    }
    dt
  }),
  fill = TRUE
)

hodes_r2 <- hodes_raw[round == 2]

for (yr in HODES_YEARS) {
  n <- nrow(hodes_r2[year == yr])
  cat(sprintf("  %d: R2=%d teams listed\n", yr, n))
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
    if (is.null(feats)) {
      next
    }
    ft <- copy(feats)
    ft[, ownership := 0.0]
    sp <- hodes_r2[year == yr]
    unmatched <- character(0)
    for (i in seq_len(nrow(sp))) {
      bn <- match_to_bracket(sp$team[i], ft$name)
      if (!is.na(bn)) {
        ft[name == bn, ownership := sp$ownership[i]]
      } else {
        unmatched <- c(unmatched, sp$team[i])
      }
    }
    if (length(unmatched) > 0) {
      cat(sprintf(
        "  %d: %d hodes teams unmatched: %s\n",
        yr,
        length(unmatched),
        paste(unmatched, collapse = ", ")
      ))
    }

    ft <- ft[ownership > 0 | (!is.na(r2_opponent) & r2_opponent != "")]
    rows[[as.character(yr)]] <- ft
  }
  rbindlist(rows, fill = TRUE)
}

train_data <- build_training_data()

train_data[, group_id := as.character(year)]
train_data[, group_total := sum(ownership), by = group_id]
train_data[, share := ownership / group_total]
train_data[, log_share := log(share + 1e-6)]

cat(sprintf(
  "\nTraining data: %d observations, %d groups\n",
  nrow(train_data),
  length(unique(train_data$group_id))
))
for (yr in HODES_YEARS) {
  g <- train_data[year == yr]
  if (nrow(g) == 0) {
    next
  }
  cat(sprintf(
    "  %d: %d teams (%d with picks > 0, total own=%.3f)\n",
    yr,
    nrow(g),
    sum(g$ownership > 0),
    sum(g$ownership)
  ))
}

# ==============================================================================
# FIT MODEL
# ==============================================================================

softmax <- function(x) {
  x <- x - max(x)
  exp(x) / sum(exp(x))
}

cat("\nFitting GAM...\n")

gam_model <- gam(
  log_share ~ ti(wp, k = 4) +
    ti(seed, k = 4) +
    ti(opp_seed, k = 4) +
    ti(wp, seed, k = c(4, 4)) +
    ti(seed, opp_seed, k = c(4, 4)),
  data = train_data,
  method = "REML",
  gamma = 1.5,
  select = TRUE
)

cat("\nModel Summary:\n")
print(summary(gam_model))

# ==============================================================================
# PREDICTIONS ON TRAINING YEARS
# ==============================================================================

cat("\n\n=== PREDICTIONS ON TRAINING YEARS ===\n")

all_pred_scores <- predict(gam_model, newdata = train_data)
train_data[, pred_score := all_pred_scores]
train_data[, pred_share := softmax(pred_score), by = group_id]

for (yr in HODES_YEARS) {
  g <- train_data[year == yr]
  if (nrow(g) == 0) {
    next
  }

  gt <- g$group_total[1]
  g[, pred_own := pred_share * gt]
  setorder(g, -ownership)

  cat(sprintf("\n--- %d Round 2 ---\n\n", yr))
  cat(sprintf(
    "%-22s %4s %6s %5s %6s %8s %8s %8s\n",
    "Team",
    "Seed",
    "WP%",
    "OppSd",
    "SDiff",
    "Actual%",
    "Pred%",
    "Error"
  ))
  cat(paste(rep("-", 73), collapse = ""), "\n")

  for (i in seq_len(nrow(g))) {
    r <- g[i]
    cat(sprintf(
      "%-22s  %2d  %5.1f%%  %2d   %+3d   %7.1f%% %7.1f%% %+7.1f%%\n",
      r$name,
      r$seed,
      100 * r$wp,
      r$opp_seed,
      as.integer(r$seed_diff),
      100 * r$ownership,
      100 * r$pred_own,
      100 * (r$pred_own - r$ownership)
    ))
  }
  mae <- g[, mean(abs(pred_own - ownership))]
  cat(sprintf("\nMAE: %.1f%%\n", 100 * mae))
}

train_data[, c("pred_score", "pred_share") := NULL]

# ==============================================================================
# 2026 PREDICTIONS
# ==============================================================================

cat("\n\n=== 2026 PREDICTIONS ===\n")

feats_2026 <- build_features(TARGET_YEAR)

if (!is.null(feats_2026)) {
  g <- feats_2026[!is.na(r2_opponent) & wp > 0]
  if (nrow(g) == 0) {
    cat("\nNo R2 matchups found for 2026.\n")
    cat("R2 closing lines may not be available yet.\n")
  } else {
    pred_shares <- softmax(predict(gam_model, newdata = g))
    pred_own <- pred_shares * 3 # 3 picks per person

    out <- data.table(
      team = g$name,
      seed = g$seed,
      wp = round(100 * g$wp, 1),
      champ = round(100 * g$champ_prob, 2),
      opp_seed = g$opp_seed,
      seed_diff = as.integer(g$seed_diff),
      own = round(100 * pred_own, 1)
    )
    setorder(out, -own)

    cat(sprintf("\n--- 2026 Round 2 (%d teams) ---\n\n", nrow(out)))
    cat(sprintf(
      "%-22s %4s %6s %6s %5s %6s %8s\n",
      "Team",
      "Seed",
      "WP%",
      "Chmp%",
      "OppSd",
      "SDiff",
      "Own%"
    ))
    cat(paste(rep("-", 61), collapse = ""), "\n")

    for (i in seq_len(nrow(out))) {
      cat(sprintf(
        "%-22s  %2d  %5.1f%% %5.2f%%  %2d   %+3d   %7.1f%%\n",
        out$team[i],
        out$seed[i],
        out$wp[i],
        out$champ[i],
        out$opp_seed[i],
        out$seed_diff[i],
        out$own[i]
      ))
    }
    cat(sprintf(
      "\nTop-5: %.1f%%\n",
      sum(sort(out$own, decreasing = TRUE)[1:5])
    ))
  }
} else {
  cat(sprintf("Could not build features for %d.\n", TARGET_YEAR))
}