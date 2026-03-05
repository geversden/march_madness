#!/usr/bin/env Rscript
# ==============================================================================
# ownership_model.R
# Estimate field ownership (pick rates) for Hodes-format NCAA survivor pool
#
# Uses 5 years of historical usage data (2021-2025) to calibrate models,
# then applies them to the current year's bracket + KenPom ratings.
#
# FOUR KEY OWNERSHIP DRIVERS:
#   1. Win probability - how likely is this team to advance?
#   2. Availability - how many entrants still have this team available?
#   3. Future value (save effect) - are people saving this team for later?
#   4. Opponent prior usage - is the opponent a popular prior pick?
#      (People are more likely to pick AGAINST teams they already used,
#       because eliminating those entries is good strategy.)
#
# THREE ESTIMATION METHODS:
#   M1: Seed-Round Historical Baseline (simple, data-driven)
#   M2: Strength-Driven Softmax (KenPom-based, accounts for save effect)
#   M3: Full Dynamic Model (chains all 4 factors round-by-round)
#
# Usage:
#   source("ownership_model.R")
#   print_ownership(1)     # R1 ownership
#   print_leverage(1)      # contrarian value
# ==============================================================================

library(data.table)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

HISTORICAL_YEARS <- 2021:2025
PICKS_PER_ROUND <- c(R1 = 3, R2 = 3, R3 = 1, R4 = 1, R5 = 1, R6 = 1)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading historical data...\n")

load_usage <- function(year) {
  f <- file.path(script_dir, "hodes_usage", sprintf("hodes_usage_%d.csv", year))
  if (!file.exists(f)) return(NULL)
  dt <- fread(f, header = TRUE)
  if (ncol(dt) > 7) dt <- dt[, 1:7]
  setnames(dt, names(dt)[1:7],
           c("Seed", "Team", "previous_usage_raw", "previous_usage_perc",
             "raw_picks", "pick_perc", "Round"))
  dt[, pick_perc := as.numeric(gsub("%", "", pick_perc)) / 100]
  dt[, previous_usage_perc := as.numeric(gsub("%", "", previous_usage_perc)) / 100]
  dt[, year := year]
  dt[!is.na(Seed) & Team != "" & Team != "No Pick"]
}

load_kenpom <- function(year) {
  f <- file.path(script_dir, "kenpom_data", sprintf("kenpom_%d.csv", year))
  if (!file.exists(f)) return(NULL)
  dt <- fread(f)
  if ("adj_em" %in% names(dt)) setnames(dt, "adj_em", "AdjEM")
  if ("NetRtg" %in% names(dt)) setnames(dt, "NetRtg", "AdjEM")
  if ("team" %in% names(dt)) setnames(dt, "team", "Team")
  if ("rank" %in% names(dt)) setnames(dt, "rank", "Rank")
  dt[, AdjEM := as.numeric(AdjEM)]
  dt <- dt[!is.na(AdjEM) & Team != "" & Team != "Team"]
  dt$Team <- gsub("\\s*\\d+$", "", trimws(dt$Team))
  dt[, year := year]
  dt
}

hist_usage <- rbindlist(lapply(HISTORICAL_YEARS, load_usage), fill = TRUE)
hist_kenpom <- rbindlist(lapply(HISTORICAL_YEARS, load_kenpom), fill = TRUE)

entries_by_year <- hist_usage[Round == 1,
  .(total_entries = sum(raw_picks, na.rm = TRUE) / 3), by = year]

cat(sprintf("  %d observations, %d-%d, avg %.0f entries/year\n\n",
            nrow(hist_usage), min(HISTORICAL_YEARS), max(HISTORICAL_YEARS),
            mean(entries_by_year$total_entries)))

# ==============================================================================
# NAME RESOLUTION
# ==============================================================================

# Load team name dictionary (single source of truth for name mappings)
team_dict_file <- file.path(script_dir, "team_names.csv")
if (file.exists(team_dict_file)) {
  team_dict <- fread(team_dict_file)
  # bracket_name -> kenpom_name
  kp_alias <- setNames(team_dict$kenpom_name, team_dict$bracket_name)
  # closing_lines_name -> bracket_name (for loading closing lines)
  cl_to_bracket <- setNames(team_dict$bracket_name, team_dict$closing_lines_name)
  cat(sprintf("Loaded team name dictionary: %d entries\n", nrow(team_dict)))
} else {
  stop("team_names.csv not found at: ", team_dict_file,
       "\nThis file is required for team name resolution.")
}

resolve_name <- function(name) {
  name <- trimws(name)
  if (name %in% names(kp_alias)) kp_alias[[name]] else name
}

# Resolve closing-lines team name (with mascot) to bracket name
resolve_cl_to_bracket <- function(cl_name, bracket_teams) {
  # 1. Dictionary lookup (priority — handles "St" vs "State" ambiguity)
  if (!is.null(cl_to_bracket) && cl_name %in% names(cl_to_bracket)) {
    return(cl_to_bracket[[cl_name]])
  }
  # 2. Prefix match: bracket name is a prefix of closing-lines name
  sorted_bt <- bracket_teams[order(-nchar(bracket_teams))]
  for (bt in sorted_bt) {
    if (startsWith(cl_name, bt) &&
        (nchar(cl_name) == nchar(bt) ||
         substr(cl_name, nchar(bt) + 1, nchar(bt) + 1) == " ")) {
      return(bt)
    }
  }
  return(NA_character_)
}

# ==============================================================================
# METHOD 1: SEED-ROUND HISTORICAL BASELINE
# ==============================================================================

cat("=== METHOD 1: SEED-ROUND HISTORICAL BASELINE ===\n\n")

seed_round_stats <- hist_usage[,
  .(avg_pct = mean(pick_perc, na.rm = TRUE),
    med_pct = median(pick_perc, na.rm = TRUE),
    n_obs = .N),
  by = .(Seed, Round)
][order(Round, Seed)]

# Lookup: use median for small samples, mean for large
m1_predict <- function(seed, round) {
  row <- seed_round_stats[Seed == seed & Round == round]
  if (nrow(row) == 0) return(0.003)
  if (row$n_obs < 5) row$med_pct else row$avg_pct
}

cat("Avg pick% by seed (R1 | R2):\n")
for (s in 1:16) {
  r1 <- seed_round_stats[Seed == s & Round == 1]
  r2 <- seed_round_stats[Seed == s & Round == 2]
  r1v <- if (nrow(r1)) sprintf("%5.1f%%", 100*r1$avg_pct) else "   --"
  r2v <- if (nrow(r2)) sprintf("%5.1f%%", 100*r2$avg_pct) else "   --"
  cat(sprintf("  Seed %2d:  R1=%s  R2=%s\n", s, r1v, r2v))
}

# ==============================================================================
# METHOD 2: STRENGTH + SAVE SOFTMAX
# ==============================================================================

cat("\n=== METHOD 2: STRENGTH + SAVE SOFTMAX ===\n\n")

# Join historical usage with KenPom
hist_joined <- copy(hist_usage)
hist_joined[, kp_name := sapply(Team, resolve_name)]
hist_kenpom[, kp_name := Team]
hist_joined <- merge(hist_joined, hist_kenpom[, .(kp_name, AdjEM, year)],
                     by = c("kp_name", "year"), all.x = TRUE)

# Fuzzy match remainder
for (i in which(is.na(hist_joined$AdjEM))) {
  yr <- hist_joined$year[i]; nm <- hist_joined$Team[i]
  kp_yr <- hist_kenpom[year == yr]
  m <- grep(nm, kp_yr$Team, value = TRUE, ignore.case = TRUE)
  if (length(m) > 0) hist_joined[i, AdjEM := kp_yr[Team == m[1]]$AdjEM[1]]
}

cat(sprintf("KenPom match rate: %.0f%%\n",
            100 * sum(!is.na(hist_joined$AdjEM)) / nrow(hist_joined)))

# Fit: for each round, find softmax temperature and save penalties
# pick_share_i ∝ exp(β * AdjEM + save_pen[seed])
fit_softmax <- function(round_num) {
  rd <- hist_joined[Round == round_num & !is.na(AdjEM)]
  if (nrow(rd) < 10) return(list(beta = 0.05, save_1 = 0, save_2 = 0))

  nll <- function(par) {
    beta <- par[1]; s1 <- par[2]; s2 <- par[3]
    total_err <- 0
    for (yr in unique(rd$year)) {
      yd <- rd[year == yr]
      attract <- beta * yd$AdjEM
      if (round_num <= 2) {
        attract[yd$Seed == 1] <- attract[yd$Seed == 1] + s1
        attract[yd$Seed == 2] <- attract[yd$Seed == 2] + s2
      }
      pred <- exp(attract) / sum(exp(attract))
      actual <- yd$pick_perc / sum(yd$pick_perc)
      total_err <- total_err + sum((pred - actual)^2)
    }
    total_err
  }

  fit <- tryCatch(
    optim(c(0.05, -1.5, -0.8), nll, method = "Nelder-Mead",
          control = list(maxit = 5000)),
    error = function(e) list(par = c(0.05, -1.5, -0.8)))
  list(beta = fit$par[1], save_1 = fit$par[2], save_2 = fit$par[3])
}

m2_params <- lapply(1:4, fit_softmax)
for (rd in 1:4) {
  p <- m2_params[[rd]]
  cat(sprintf("  R%d: β=%.4f, save_1=%.2f, save_2=%.2f\n",
              rd, p$beta, p$save_1, p$save_2))
}

m2_predict <- function(adjems, seeds, round_num) {
  p <- if (round_num <= 4) m2_params[[round_num]] else list(beta = 0.05, save_1 = 0, save_2 = 0)
  attract <- p$beta * adjems
  if (round_num <= 2) {
    attract[seeds == 1] <- attract[seeds == 1] + p$save_1
    attract[seeds == 2] <- attract[seeds == 2] + p$save_2
  }
  if (round_num >= 3) {
    # Save reversal: 1-seeds become MORE attractive in later rounds
    attract[seeds == 1] <- attract[seeds == 1] + 0.5 * (round_num - 2)
    attract[seeds == 2] <- attract[seeds == 2] + 0.3 * (round_num - 2)
  }
  # Seed tiebreaker: higher seeds get a significant boost in attractiveness
  # (people prefer higher-numbered seeds for the seed sum tiebreaker)
  # 0.06 * 12 = 0.72, equivalent to ~14 AdjEM points for a 12-seed
  # This matches historical patterns: 8-seeds get 17-27% R1 picks
  attract <- attract + 0.06 * seeds
  soft <- exp(attract)
  soft / sum(soft)
}

# ==============================================================================
# METHOD 3: FULL DYNAMIC MODEL (5 FACTORS)
#
# For each team in round R:
#   attractiveness = f(win_prob) * availability * save_disc * opp_boost * seed_boost
#
# Factor 1 - Win probability: from sim results or KenPom logistic model
# Factor 2 - Availability: 1 - cumulative prior usage
# Factor 3 - Future value: discount for teams people want to save
#   save_discount = 1 - (future_value / max_future_value) * save_strength
#   where future_value = sum of win_probs in rounds R+1..R+6
# Factor 4 - Opponent prior usage boost: if my R2 opponent was heavily
#   picked in R1, people are incentivized to pick me (to knock them out).
#   opp_boost = 1 + α * opponent_R1_usage
# Factor 5 - Seed tiebreaker value: higher seed numbers (lower-seeded teams)
#   add more to the seed sum tiebreaker. A 10-seed adds 10 vs a 1-seed
#   adding 1. In a winner-take-all format this matters: people prefer
#   higher-numbered seeds when win probability is comparable.
#   seed_boost = 1 + γ * (seed - avg_seed) / max_seed
# ==============================================================================

cat("\n=== METHOD 3: FULL DYNAMIC MODEL (5 factors) ===\n\n")

# ==============================================================================
# LOAD TARGET BRACKET + RATINGS
# ==============================================================================

# Find latest bracket
bracket_files <- list.files(file.path(script_dir, "brackets"),
                            pattern = "bracket_\\d+\\.csv", full.names = TRUE)
years <- as.integer(gsub(".*bracket_(\\d+)\\.csv", "\\1", bracket_files))
target_year <- max(years)
if (exists("OWNERSHIP_YEAR")) target_year <- OWNERSHIP_YEAR

cat(sprintf("Target year: %d\n", target_year))

bracket <- fread(file.path(script_dir, "brackets",
                           sprintf("bracket_%d.csv", target_year)))
teams <- data.table(
  name = bracket$team, seed = bracket$seed,
  region = bracket$region, team_id = 1:nrow(bracket)
)

# Load KenPom: prefer year-matched file first, then dated file
kp_year_file <- file.path(script_dir, "kenpom_data",
                          sprintf("kenpom_%d.csv", target_year))
kp_dated <- list.files(file.path(script_dir, "kenpom_data"),
                       pattern = "kenpom_ratings_.*\\.csv", full.names = TRUE)
kp_file <- if (file.exists(kp_year_file)) kp_year_file else
  if (length(kp_dated) > 0) sort(kp_dated, decreasing = TRUE)[1] else kp_year_file

kp <- fread(kp_file)
if ("adj_em" %in% names(kp)) setnames(kp, "adj_em", "AdjEM")
if ("NetRtg" %in% names(kp)) setnames(kp, "NetRtg", "AdjEM")
if ("team" %in% names(kp)) setnames(kp, "team", "Team")
kp[, AdjEM := as.numeric(AdjEM)]
kp <- kp[!is.na(AdjEM)]
kp$Team <- gsub("\\s*\\d+$", "", trimws(kp$Team))

teams[, kp_name := sapply(name, resolve_name)]
kp[, kp_name := Team]
teams <- merge(teams, kp[, .(kp_name, AdjEM)], by = "kp_name", all.x = TRUE)
for (i in which(is.na(teams$AdjEM))) {
  m <- grep(teams$name[i], kp$Team, value = TRUE, ignore.case = TRUE)
  if (length(m) > 0) teams$AdjEM[i] <- kp[Team == m[1]]$AdjEM[1]
}
teams[is.na(AdjEM), AdjEM := -15]  # assume worst for unmatched (16-seeds etc)
setorder(teams, team_id)

cat(sprintf("Loaded %d teams, KenPom from %s\n", nrow(teams), basename(kp_file)))

# ==============================================================================
# WIN PROBABILITIES (from sim, closing lines, or KenPom approximation)
# ==============================================================================

# Load closing lines for a given year and return R1 win probabilities
# Returns a named list: bracket_name -> R1 win probability, or NULL
load_closing_lines <- function(year, bracket_teams) {
  f <- file.path(script_dir, "closing_lines",
                 sprintf("ncaat_%d_closing_lines.csv", year))
  if (!file.exists(f)) return(NULL)

  cl <- fread(f)

  # Resolve team names to bracket names
  cl[, home_bracket := sapply(home_team, resolve_cl_to_bracket,
                              bracket_teams = bracket_teams)]
  cl[, away_bracket := sapply(away_team, resolve_cl_to_bracket,
                              bracket_teams = bracket_teams)]

  # Keep only games where both teams resolved
  cl <- cl[!is.na(home_bracket) & !is.na(away_bracket)]
  if (nrow(cl) == 0) return(NULL)

  # Identify R1 games: first 32 games (by date) where each team appears once
  cl[, game_date := as.Date(date)]
  setorder(cl, game_date)

  r1_teams_seen <- character(0)
  r1_rows <- integer(0)
  for (i in seq_len(nrow(cl))) {
    h <- cl$home_bracket[i]; a <- cl$away_bracket[i]
    if (!(h %in% r1_teams_seen) && !(a %in% r1_teams_seen)) {
      r1_rows <- c(r1_rows, i)
      r1_teams_seen <- c(r1_teams_seen, h, a)
    }
    if (length(r1_rows) == 32) break
  }

  r1 <- cl[r1_rows]

  # Build lookup: bracket_name -> R1 win probability
  wp_lookup <- list()
  for (i in seq_len(nrow(r1))) {
    wp_lookup[[r1$home_bracket[i]]] <- r1$home_win_prob[i]
    wp_lookup[[r1$away_bracket[i]]] <- 1 - r1$home_win_prob[i]
  }

  wp_lookup
}

LOG_SCALE <- 0.0917

sim_file <- file.path(script_dir, sprintf("sim_results_%d.rds", target_year))
if (file.exists(sim_file)) {
  # Priority 1: Simulation results (all rounds)
  cat(sprintf("Loading sim results: %s\n", basename(sim_file)))
  sim <- readRDS(sim_file)
  ri <- sim$round_info
  trp <- matrix(0, nrow = nrow(sim$teams), ncol = 6)
  for (rd in 1:6) {
    cols <- which(ri$round_num == rd)
    rr <- sim$all_results[, cols, drop = FALSE]
    for (j in seq_len(ncol(rr))) {
      tab <- tabulate(rr[, j], nbins = nrow(sim$teams))
      trp[, rd] <- trp[, rd] + tab
    }
  }
  trp <- trp / sim$n_sims
  for (rd in 1:6) teams[, paste0("wp_R", rd) := trp[, rd]]
  has_sim <- TRUE
} else {
  has_sim <- FALSE

  # Priority 2: Closing lines for R1
  cl_wp <- load_closing_lines(target_year, teams$name)

  if (!is.null(cl_wp) && length(cl_wp) >= 60) {
    n_matched <- 0
    for (i in seq_len(nrow(teams))) {
      wp <- cl_wp[[teams$name[i]]]
      if (!is.null(wp)) {
        teams[i, wp_R1 := wp]
        n_matched <- n_matched + 1
      } else {
        # Fallback for unmatched: use KenPom logistic
        j <- if (i %% 2 == 1) i + 1 else i - 1
        teams[i, wp_R1 := 1 / (1 + exp(-LOG_SCALE * (teams$AdjEM[i] - teams$AdjEM[j])))]
        cat(sprintf("  WARNING: No closing line for %s, using KenPom fallback\n",
                    teams$name[i]))
      }
    }
    cat(sprintf("Using closing lines for R1 win probabilities (%d/%d teams matched)\n",
                n_matched, nrow(teams)))
  } else {
    # Priority 3: KenPom logistic approximation
    cat("No closing lines found. Approximating win probs from KenPom.\n")
    for (g in 1:32) {
      i <- 2*g - 1; j <- 2*g
      p <- 1 / (1 + exp(-LOG_SCALE * (teams$AdjEM[i] - teams$AdjEM[j])))
      teams[i, wp_R1 := p]
      teams[j, wp_R1 := 1 - p]
    }
  }

  # R2+: rough approximation (compound probabilities)
  for (rd in 2:6) {
    prev <- paste0("wp_R", rd - 1)
    decay <- c(0.65, 0.60, 0.55, 0.50, 0.45)[rd - 1]
    teams[, paste0("wp_R", rd) := get(prev) * decay]
  }
}

# ==============================================================================
# BRACKET STRUCTURE: WHO PLAYS WHOM
# ==============================================================================

# Determine R1 matchups (positions 1v2, 3v4, etc. in bracket order)
# and potential R2 matchups (winner of game 1 vs winner of game 2, etc.)
get_opponent_idx <- function(team_idx, round_num) {
  # In the bracket, R1 matchups are: (1,2), (3,4), (5,6), ...
  # R2 matchups: winner of (1,2) vs winner of (3,4), etc.
  # General: in round R, groups of 2^R teams play against each other
  group_size <- 2^round_num
  group_start <- ((team_idx - 1) %/% group_size) * group_size + 1
  group_end <- group_start + group_size - 1
  # Return all teams in the opposing half of the group
  mid <- group_start + group_size / 2
  if (team_idx < mid) {
    return(mid:group_end)
  } else {
    return(group_start:(mid - 1))
  }
}

cat("\n")

# ==============================================================================
# DYNAMIC MODEL: COMPUTE OWNERSHIP ROUND BY ROUND
# ==============================================================================

# --- FACTOR 3: FUTURE VALUE ---
# For each team, compute "future value" = expected value of saving this team
# = sum of (win_prob_Rk * round_weight) for rounds k > current round
# Higher future value → people save it → lower current ownership

round_importance <- c(1, 2, 4, 8, 16, 32)  # later rounds matter more

for (rd in 1:6) {
  fv <- rep(0, nrow(teams))
  for (future_rd in (rd+1):6) {
    if (future_rd > 6) break
    wp_col <- paste0("wp_R", future_rd)
    fv <- fv + teams[[wp_col]] * round_importance[future_rd]
  }
  teams[, paste0("future_val_R", rd) := fv]
}

# Normalize future value to [0, 1]
for (rd in 1:6) {
  fv_col <- paste0("future_val_R", rd)
  maxfv <- max(teams[[fv_col]], na.rm = TRUE)
  if (maxfv > 0) teams[, (fv_col) := get(fv_col) / maxfv]
}

# Save strength: how much does future value suppress current picks?
# Calibrated from historical data: 1-seeds have ~2% R1 usage despite ~99% win prob
# This implies a strong save effect. save_strength = 0.7 means a team with
# max future value has its R1 attractiveness reduced by 70%.
SAVE_STRENGTH <- c(0.70, 0.50, 0.25, 0.10, 0.0, 0.0)  # by round

# --- FACTOR 4: OPPONENT PRIOR USAGE ---
# In R2+, people prefer to pick AGAINST teams that were popular in prior rounds.
# If your opponent was picked by 20% of entries in R1, picking you eliminates
# those 20% (a strategic advantage). This makes you MORE attractive.
# opp_usage_boost = 1 + OPP_BOOST_COEFF * opponent_cumulative_usage
OPP_BOOST_COEFF <- 0.5  # each 1% of opponent usage adds 0.5% attractiveness

# --- FACTOR 5: SEED TIEBREAKER VALUE ---
# In a winner-take-all pool, the 2nd tiebreaker is highest seed sum.
# Picking a 10-seed adds 10 to your sum; a 1-seed adds only 1.
# So among teams with similar win probability, people prefer higher seeds.
#
# Calibrated from historical R1 data: within the 3-8 seed range
# (where win probs are similar), higher-numbered seeds get disproportionate
# picks. E.g. 8-seeds average 7.9% despite lower win prob than 3-seeds (10.1%),
# suggesting the seed value partially offsets the strength gap.
#
# seed_boost = 1 + SEED_TB_COEFF * (seed / 16)
# This gives a 10-seed a ~31% boost and a 1-seed a ~3% boost.
SEED_TB_COEFF <- 1.2  # strength of seed tiebreaker preference
# Calibrated from 2021-2025 data: seeds 7-12 get a meaningful boost from
# the tiebreaker incentive. A 12-seed gets 1 + 1.2 * 12/12 = 2.2x boost,
# a 8-seed gets 1 + 1.2 * 8/12 = 1.8x, a 3-seed gets 1 + 1.2 * 3/12 = 1.3x.
# Combined with the save effect and concentration, this produces realistic
# ownership: top 8-seed ~20-25%, strong 12-seeds ~10-15%, 3-seeds ~10-15%.

# ==============================================================================
# CALIBRATE M3 ON HISTORICAL R1 DATA (LOG-LINEAR / SOFTMAX MODEL)
# ==============================================================================
# Instead of multiplicative factors with power concentration, use a softmax:
#   log_attract = β_wp * wp + β_seed * seed + β_save * fv + β_interact * wp*seed
#   ownership ∝ exp(log_attract)
#
# This naturally produces the right concentration: softmax creates exponential
# peaks for teams with the best feature combinations.

cat("Calibrating M3 (log-linear softmax) on historical R1 data...\n")

bracket_dir <- file.path(script_dir, "brackets")

# Build historical R1 feature matrix for fitting
build_r1_features <- function(yr) {
  bf <- file.path(bracket_dir, sprintf("bracket_%d.csv", yr))
  kf <- file.path(script_dir, "kenpom_data", sprintf("kenpom_%d.csv", yr))
  if (!file.exists(bf) || !file.exists(kf)) return(NULL)

  br <- fread(bf)
  kpyr <- fread(kf)
  if ("NetRtg" %in% names(kpyr)) setnames(kpyr, names(kpyr)[which(names(kpyr) == "NetRtg")[1]], "AdjEM")
  if ("adj_em" %in% names(kpyr)) setnames(kpyr, "adj_em", "AdjEM")
  if ("team" %in% names(kpyr)) setnames(kpyr, "team", "Team")
  kpyr[, AdjEM := as.numeric(AdjEM)]
  kpyr <- kpyr[!is.na(AdjEM) & Team != "" & Team != "Team"]
  kpyr$Team <- gsub("\\s*\\d+$", "", trimws(kpyr$Team))

  bt <- data.table(name = br$team, seed = br$seed, team_id = 1:nrow(br))
  bt[, kp_name := sapply(name, resolve_name)]
  bt <- merge(bt, kpyr[, .(Team, AdjEM)], by.x = "kp_name", by.y = "Team", all.x = TRUE)
  for (i in which(is.na(bt$AdjEM))) {
    m <- grep(bt$name[i], kpyr$Team, value = TRUE, ignore.case = TRUE)
    if (length(m) > 0) bt$AdjEM[i] <- kpyr[Team == m[1]]$AdjEM[1]
  }
  bt[is.na(AdjEM), AdjEM := -15]
  setorder(bt, team_id)

  # R1 win probabilities: prefer closing lines, fall back to KenPom logistic
  cl_wp <- load_closing_lines(yr, bt$name)
  if (!is.null(cl_wp) && length(cl_wp) >= 50) {
    bt[, wp := 0]
    for (i in seq_len(nrow(bt))) {
      w <- cl_wp[[bt$name[i]]]
      if (!is.null(w)) {
        bt$wp[i] <- w
      } else {
        # Fallback for unmatched teams
        j <- if (i %% 2 == 1) i + 1 else i - 1
        bt$wp[i] <- 1 / (1 + exp(-0.0917 * (bt$AdjEM[i] - bt$AdjEM[j])))
      }
    }
  } else {
    for (g in 1:32) {
      ii <- 2*g - 1; jj <- 2*g
      p <- 1 / (1 + exp(-0.0917 * (bt$AdjEM[ii] - bt$AdjEM[jj])))
      bt[ii, wp := p]
      bt[jj, wp := 1 - p]
    }
  }

  # R2 win probabilities (compound: must win R1, then beat expected R2 opponent)
  # R2 matchups: winners of games (1,2) vs (3,4), (5,6) vs (7,8), etc.
  bt[, wp_r2 := 0]
  for (g in 1:16) {
    # Two R1 games feed into one R2 game
    i1 <- 4*(g-1) + 1; i2 <- i1 + 1; i3 <- i1 + 2; i4 <- i1 + 3
    # For each of the 4 teams, their R2 WP = P(win R1) * E[P(beat R2 opponent)]
    for (ti in c(i1, i2)) {
      # ti's R2 opponents are i3 and i4, weighted by their R1 win probs
      p_vs_i3 <- 1 / (1 + exp(-0.0917 * (bt$AdjEM[ti] - bt$AdjEM[i3])))
      p_vs_i4 <- 1 / (1 + exp(-0.0917 * (bt$AdjEM[ti] - bt$AdjEM[i4])))
      bt$wp_r2[ti] <- bt$wp[ti] * (bt$wp[i3] * p_vs_i3 + bt$wp[i4] * p_vs_i4)
    }
    for (ti in c(i3, i4)) {
      p_vs_i1 <- 1 / (1 + exp(-0.0917 * (bt$AdjEM[ti] - bt$AdjEM[i1])))
      p_vs_i2 <- 1 / (1 + exp(-0.0917 * (bt$AdjEM[ti] - bt$AdjEM[i2])))
      bt$wp_r2[ti] <- bt$wp[ti] * (bt$wp[i1] * p_vs_i1 + bt$wp[i2] * p_vs_i2)
    }
  }

  # WP drop: how much win probability drops from R1 to R2
  # High wp_drop = "use it or lose it" (e.g. 8-seed: 70% R1, 14% R2 → drop = 56%)
  # Low wp_drop = "save for later" (e.g. 1-seed: 99% R1, 85% R2 → drop = 14%)
  bt[, wp_drop := wp - wp_r2]

  # Future value (simplified: higher AdjEM → more future value)
  bt[, fv := AdjEM / max(AdjEM)]
  bt[, fv := pmax(fv, 0)]

  bt[, year := yr]
  bt
}

# Build features for all historical years
hist_features <- rbindlist(lapply(HISTORICAL_YEARS, build_r1_features))

# Match to actual R1 usage
r1_actual <- hist_usage[Round == 1]
r1_actual[, kp_name := sapply(Team, resolve_name)]

hist_features <- merge(hist_features, r1_actual[, .(kp_name, pick_perc, year)],
                       by = c("kp_name", "year"), all.x = TRUE)
# Fuzzy match remainder
for (i in which(is.na(hist_features$pick_perc))) {
  yr <- hist_features$year[i]; nm <- hist_features$name[i]
  yr_actual <- r1_actual[year == yr]
  m <- grep(nm, yr_actual$Team, value = TRUE, ignore.case = TRUE)
  if (length(m) > 0) hist_features$pick_perc[i] <- yr_actual[Team == m[1]]$pick_perc[1]
}
hist_features[is.na(pick_perc), pick_perc := 0]

# Fit log-linear model:
#   log(pick_share) ~ β_wp*wp + β_seed*seed + β_save*fv + β_wxs*wp*seed
#                    + β_seed²*seed² + β_drop*wp_drop
#
# The wp_drop feature captures "use it or lose it" behavior:
#   wp_drop = wp_R1 - wp_R2 (compound). High drop means the team is strong
#   NOW but faces a brutal R2 matchup (e.g. 8-seed vs 1-seed).
#   This is why Gonzaga 8-seed gets 26.5% despite only 70% R1 WP —
#   people know they won't be usable in R2.
#
# Objective: for each year, softmax predictions match actual ownership
m3_loglinear_loss <- function(par) {
  b_wp <- par[1]; b_seed <- par[2]; b_save <- par[3]
  b_wxs <- par[4]; b_seed2 <- par[5]; b_drop <- par[6]; b_drop2 <- par[7]

  total_err <- 0
  n_years <- 0

  for (yr in HISTORICAL_YEARS) {
    yd <- hist_features[year == yr]
    if (nrow(yd) < 30) next

    cs <- pmin(yd$seed, 12)
    log_attract <- b_wp * yd$wp + b_seed * cs + b_save * yd$fv +
                   b_wxs * yd$wp * cs + b_seed2 * cs^2 +
                   b_drop * yd$wp_drop + b_drop2 * yd$wp_drop^2
    # Softmax
    log_attract <- log_attract - max(log_attract)  # numerical stability
    pred <- exp(log_attract) / sum(exp(log_attract))

    actual_norm <- yd$pick_perc / sum(yd$pick_perc)

    # Loss: weighted MSE with extra weight on top teams + peak matching
    wt <- actual_norm + 0.001  # linear weight by actual (heavy emphasis on top teams)
    err <- sum(wt * (pred - actual_norm)^2)
    # Peak penalty: match the max ownership level
    peak_err <- (max(pred) - max(actual_norm))^2 * 5.0
    total_err <- total_err + err + peak_err
    n_years <- n_years + 1
  }

  if (n_years == 0) return(1e6)
  total_err / n_years
}

# Optimize with multiple starting points (7 params now)
best_val <- Inf
best_par <- c(1.0, 0.2, -2.0, 0.1, 0.0, 2.0, 1.0)

starts <- list(
  c(1.0, 0.2, -2.0, 0.1, 0.0, 2.0, 1.0),
  c(2.0, 0.3, -3.0, 0.0, 0.01, 3.0, 2.0),
  c(0.5, 0.1, -1.0, 0.2, -0.01, 1.5, 0.5),
  c(3.0, 0.4, -4.0, -0.1, 0.02, 4.0, 3.0),
  c(-4.4, -0.94, 1.28, 1.30, 0.021, 1.6, 2.0),  # near previous best
  c(-4.4, -0.94, 1.28, 1.30, 0.021, 3.0, 5.0),  # previous best + large drop terms
  c(-8.0, -1.5, 2.0, 2.5, 0.04, 5.0, 8.0),  # scaled-up version
  c(-6.0, -1.2, 1.5, 2.0, 0.03, 4.0, 4.0)
)

for (s in starts) {
  opt_try <- tryCatch(
    optim(s, m3_loglinear_loss, method = "Nelder-Mead",
          control = list(maxit = 10000)),
    error = function(e) list(value = Inf, par = s))
  if (opt_try$value < best_val) {
    best_val <- opt_try$value
    best_par <- opt_try$par
  }
}

M3_BETA <- best_par
cat(sprintf("  Fitted M3 (log-linear): β_wp=%.3f, β_seed=%.3f, β_save=%.3f, β_wp×seed=%.3f, β_seed²=%.4f, β_drop=%.3f, β_drop²=%.3f\n",
            M3_BETA[1], M3_BETA[2], M3_BETA[3], M3_BETA[4], M3_BETA[5], M3_BETA[6], M3_BETA[7]))
cat(sprintf("  Loss: %.6f\n\n", best_val))

# Function to predict R1 ownership using fitted log-linear model
m3_predict_r1 <- function(wp, seed, fv, wp_drop) {
  cs <- pmin(seed, 12)
  log_attract <- M3_BETA[1]*wp + M3_BETA[2]*cs + M3_BETA[3]*fv +
                 M3_BETA[4]*wp*cs + M3_BETA[5]*cs^2 +
                 M3_BETA[6]*wp_drop + M3_BETA[7]*wp_drop^2
  log_attract <- log_attract - max(log_attract)
  exp(log_attract) / sum(exp(log_attract))
}

# Diagnostic: show M3 predictions vs actual for ALL historical years
# Display in RAW PICK PERCENTAGE space (sums to ~300% for R1 with 3 picks/person)
cat("  M3 R1 Calibration vs Actuals — raw pick %% (top 15 per year):\n\n")
all_resid <- data.table()
for (cal_year in HISTORICAL_YEARS) {
  cal_yr <- hist_features[year == cal_year]
  if (nrow(cal_yr) < 30) next
  cs <- pmin(cal_yr$seed, 12)
  la <- M3_BETA[1]*cal_yr$wp + M3_BETA[2]*cs + M3_BETA[3]*cal_yr$fv +
        M3_BETA[4]*cal_yr$wp*cs + M3_BETA[5]*cs^2 +
        M3_BETA[6]*cal_yr$wp_drop + M3_BETA[7]*cal_yr$wp_drop^2
  la <- la - max(la)
  cal_pred_norm <- exp(la) / sum(exp(la))
  # Convert to raw pick%: multiply by PICKS_PER_ROUND for R1 (=3)
  cal_pred_raw <- cal_pred_norm * 3
  cal_actual_raw <- cal_yr$pick_perc  # already in raw pick% space
  cal_dt <- data.table(team = cal_yr$name, seed = cal_yr$seed,
                       actual = cal_actual_raw, pred = cal_pred_raw,
                       diff = cal_pred_raw - cal_actual_raw, year = cal_year)
  all_resid <- rbind(all_resid, cal_dt)
  setorder(cal_dt, -actual)
  cat(sprintf("  --- %d ---\n", cal_year))
  cat(sprintf("  %-20s %4s %7s %7s %7s %6s\n", "Team", "Seed", "Actual", "Pred", "Diff", "WPdrop"))
  for (i in 1:min(15, nrow(cal_dt))) {
    wp_drop_val <- cal_yr[name == cal_dt$team[i]]$wp_drop[1]
    cat(sprintf("  %-20s  %2d   %5.1f%%  %5.1f%%  %+5.1f%%  %.2f\n",
                cal_dt$team[i], cal_dt$seed[i],
                100*cal_dt$actual[i], 100*cal_dt$pred[i],
                100*cal_dt$diff[i], wp_drop_val))
  }
  cat("\n")
}
# Summary: systematic errors by seed (in raw pick% space)
cat("  Residual summary by seed (across all years, raw pick %%):\n")
all_resid[, .(avg_actual = sprintf("%.1f%%", 100*mean(actual)),
              avg_pred = sprintf("%.1f%%", 100*mean(pred)),
              avg_err = sprintf("%+.1f%%", 100*mean(diff)),
              n = .N), by = seed][order(seed)] |>
  (\(x) { for (i in 1:nrow(x)) {
    cat(sprintf("  Seed %2d: actual=%s  pred=%s  err=%s  (n=%d)\n",
                x$seed[i], x$avg_actual[i], x$avg_pred[i], x$avg_err[i], x$n[i]))
  }})()
cat("\n")

# ==============================================================================
# BUILD DYNAMIC OWNERSHIP
# ==============================================================================
# R1: Use fitted log-linear softmax model (calibrated from historical data)
# R2+: Use multiplicative factor model with availability chain

# Initialize availability (everyone has all teams available in R1)
teams[, avail := 1.0]

# --- R1: Log-linear softmax (fitted) ---
{
  fv <- teams$AdjEM / max(teams$AdjEM)
  fv <- pmax(fv, 0)

  # Compute wp_drop for target bracket: how much does WP drop from R1 to R2?
  # This captures "use it or lose it" — teams facing brutal R2 matchups
  teams[, wp_r2_compound := 0]
  for (g in 1:16) {
    i1 <- 4*(g-1) + 1; i2 <- i1 + 1; i3 <- i1 + 2; i4 <- i1 + 3
    for (ti in c(i1, i2)) {
      p_vs_i3 <- 1 / (1 + exp(-0.0917 * (teams$AdjEM[ti] - teams$AdjEM[i3])))
      p_vs_i4 <- 1 / (1 + exp(-0.0917 * (teams$AdjEM[ti] - teams$AdjEM[i4])))
      teams$wp_r2_compound[ti] <- teams$wp_R1[ti] * (teams$wp_R1[i3] * p_vs_i3 + teams$wp_R1[i4] * p_vs_i4)
    }
    for (ti in c(i3, i4)) {
      p_vs_i1 <- 1 / (1 + exp(-0.0917 * (teams$AdjEM[ti] - teams$AdjEM[i1])))
      p_vs_i2 <- 1 / (1 + exp(-0.0917 * (teams$AdjEM[ti] - teams$AdjEM[i2])))
      teams$wp_r2_compound[ti] <- teams$wp_R1[ti] * (teams$wp_R1[i1] * p_vs_i1 + teams$wp_R1[i2] * p_vs_i2)
    }
  }
  teams[, wp_drop := wp_R1 - wp_r2_compound]

  ownership <- m3_predict_r1(teams$wp_R1, teams$seed, fv, teams$wp_drop)
  teams[, m3_R1 := ownership]
  teams[, avail := pmax(0, avail - ownership)]
  cat(sprintf("R1 dynamic model: top=%s (%.1f%%), concentration(top5)=%.1f%%\n",
              teams$name[which.max(ownership)], 100*max(ownership),
              100*sum(sort(ownership, decreasing = TRUE)[1:5])))
}

# --- R2+: Multiplicative factor model ---
for (rd in 2:6) {
  wp_col <- paste0("wp_R", rd)
  fv_col <- paste0("future_val_R", rd)

  win_prob <- teams[[wp_col]]
  availability <- teams$avail

  save_disc <- 1 - SAVE_STRENGTH[rd] * teams[[fv_col]]
  save_disc <- pmax(save_disc, 0.05)

  # Opponent prior usage boost
  opp_boost <- rep(1.0, nrow(teams))
  for (i in 1:nrow(teams)) {
    opp_indices <- get_opponent_idx(i, rd)
    opp_usage <- 1 - teams$avail[opp_indices]
    opp_wp <- teams[[paste0("wp_R", rd - 1)]][opp_indices]
    weighted_opp_usage <- sum(opp_usage * opp_wp) / max(sum(opp_wp), 0.01)
    opp_boost[i] <- 1 + OPP_BOOST_COEFF * weighted_opp_usage
  }

  capped_seed <- pmin(teams$seed, 12)
  seed_boost <- 1 + SEED_TB_COEFF * (capped_seed / 12)

  attract <- win_prob * availability * save_disc * opp_boost * seed_boost
  min_wp <- c(0.01, 0.005, 0.003, 0.002, 0.001, 0.001)[rd]
  attract[win_prob < min_wp] <- 0
  attract <- attract^2.5

  if (sum(attract) > 0) {
    ownership <- attract / sum(attract)
  } else {
    ownership <- rep(1/nrow(teams), nrow(teams))
  }

  teams[, paste0("m3_R", rd) := ownership]
  teams[, avail := pmax(0, avail - ownership)]

  cat(sprintf("R%d dynamic model: top=%s (%.1f%%), concentration(top5)=%.1f%%\n",
              rd, teams$name[which.max(ownership)], 100*max(ownership),
              100*sum(sort(ownership, decreasing = TRUE)[1:5])))
}

# ==============================================================================
# APPLY ALL THREE METHODS TO CURRENT BRACKET
# ==============================================================================

cat("\nApplying all three methods...\n")

# Method 1: normalize to sum to 1 (same scale as M2, M3)
for (rd in 1:6) {
  raw <- sapply(teams$seed, function(s) m1_predict(s, rd))
  if (sum(raw) > 0) raw <- raw / sum(raw)
  teams[, paste0("m1_R", rd) := raw]
}

# Method 2: already sums to 1
for (rd in 1:6) {
  preds <- m2_predict(teams$AdjEM, teams$seed, rd)
  teams[, paste0("m2_R", rd) := preds]
}

# Method 3 already computed above (sums to 1)

# Blend (Method 3 gets highest weight because it models the most factors)
# All three methods are on the same scale (normalized shares summing to 1)
# M3 (dynamic/log-linear) gets highest weight since it's calibrated from
# historical data with all 5 factors. M1 (seed baseline) gets modest weight
# as a prior. M2 (strength softmax) fills in 3-seed / strength gaps.
BLEND_W <- c(m1 = 0.15, m2 = 0.35, m3 = 0.50)

for (rd in 1:6) {
  blend_raw <- BLEND_W["m1"] * teams[[paste0("m1_R", rd)]] +
               BLEND_W["m2"] * teams[[paste0("m2_R", rd)]] +
               BLEND_W["m3"] * teams[[paste0("m3_R", rd)]]
  # Renormalize blend to sum to 1 (shares)
  blend_raw <- blend_raw / sum(blend_raw)
  teams[, paste0("blend_R", rd) := blend_raw]

  # Convert to "entry fraction" = fraction of entries picking this team
  # This matches the format of historical data (sums to ~PICKS_PER_ROUND)
  picks <- c(3, 3, 1, 1, 1, 1)[rd]
  teams[, paste0("own_R", rd) := blend_raw * picks]
}

# ==============================================================================
# DISPLAY FUNCTIONS
# ==============================================================================

print_ownership <- function(round_num = 1, min_pct = 0.3) {
  rd <- round_num
  oc <- paste0("own_R", rd)   # entry fraction (sums to PICKS_PER_ROUND)
  m1c <- paste0("m1_R", rd); m2c <- paste0("m2_R", rd)
  m3c <- paste0("m3_R", rd)
  wpc <- paste0("wp_R", rd)
  picks <- c(3, 3, 1, 1, 1, 1)[rd]

  dt <- copy(teams)[order(-get(oc))]

  round_names <- c("Round of 64", "Round of 32", "Sweet 16",
                    "Elite 8", "Final Four", "Championship")

  cat(sprintf("\n============== OWNERSHIP ESTIMATES: %s (%d picks/person) ==============\n",
              round_names[rd], picks))
  cat(sprintf("%-20s %4s %-8s %6s | %7s %7s %7s | %7s\n",
              "Team", "Seed", "Region", "WinP%",
              "SeedHx", "Strngth", "Dynamc", "OWN%"))
  cat(paste(rep("-", 85), collapse = ""), "\n")

  for (i in 1:nrow(dt)) {
    ov <- 100 * dt[[oc]][i]
    if (ov < min_pct && i > 15) break
    cat(sprintf("%-20s  %2d   %-8s %5.1f%% | %5.1f%%  %5.1f%%  %5.1f%% | %5.1f%%\n",
                dt$name[i], dt$seed[i], dt$region[i],
                100 * dt[[wpc]][i],
                100 * dt[[m1c]][i] * picks,
                100 * dt[[m2c]][i] * picks,
                100 * dt[[m3c]][i] * picks, ov))
  }
  top5 <- 100 * sum(sort(dt[[oc]], decreasing = TRUE)[1:5])
  total <- 100 * sum(dt[[oc]])
  cat(sprintf("\nTop-5 concentration: %.1f%%  |  Total: %.0f%%  |  Blend: SeedHx=%.0f%% + Strength=%.0f%% + Dynamic=%.0f%%\n\n",
              top5, total, 100*BLEND_W["m1"], 100*BLEND_W["m2"], 100*BLEND_W["m3"]))
}

print_leverage <- function(round_num = 1, min_own_pct = 0.3) {
  rd <- round_num
  oc <- paste0("own_R", rd)
  wpc <- paste0("wp_R", rd)

  dt <- copy(teams)
  # Normalize win probability to shares for fair comparison
  dt[, wp_share := get(wpc) / sum(get(wpc))]
  dt[, own_share := get(oc) / sum(get(oc))]  # normalize own to shares too
  dt[, leverage := wp_share / pmax(own_share, 0.0001)]
  dt <- dt[get(oc) >= min_own_pct / 100]

  # Split into underowned (contrarian) and overowned (chalk)
  dt_contra <- dt[leverage > 1.0][order(-leverage)]
  dt_chalk <- dt[leverage <= 1.0][order(leverage)]

  round_names <- c("Round of 64", "Round of 32", "Sweet 16",
                    "Elite 8", "Final Four", "Championship")

  cat(sprintf("\n============== LEVERAGE: %s ==============\n", round_names[rd]))

  cat("\n  UNDEROWNED (contrarian value) — leverage > 1.0:\n")
  cat(sprintf("  %-20s %4s %6s %6s %8s\n",
              "Team", "Seed", "WinP%", "Own%", "Leverage"))
  cat(paste0("  ", paste(rep("-", 55), collapse = ""), "\n"))
  for (i in 1:min(nrow(dt_contra), 15)) {
    cat(sprintf("  %-20s  %2d   %5.1f%% %5.1f%%   %5.2fx\n",
                dt_contra$name[i], dt_contra$seed[i],
                100*dt_contra[[wpc]][i], 100*dt_contra[[oc]][i],
                dt_contra$leverage[i]))
  }

  cat("\n  OVEROWNED (chalk traps) — leverage < 1.0:\n")
  cat(sprintf("  %-20s %4s %6s %6s %8s\n",
              "Team", "Seed", "WinP%", "Own%", "Leverage"))
  cat(paste0("  ", paste(rep("-", 55), collapse = ""), "\n"))
  for (i in 1:min(nrow(dt_chalk), 10)) {
    cat(sprintf("  %-20s  %2d   %5.1f%% %5.1f%%   %5.2fx\n",
                dt_chalk$name[i], dt_chalk$seed[i],
                100*dt_chalk[[wpc]][i], 100*dt_chalk[[oc]][i],
                dt_chalk$leverage[i]))
  }
  cat("\n")
}

# ==============================================================================
# OUTPUT
# ==============================================================================

cat("\n")
for (rd in 1:3) print_ownership(rd)
for (rd in 1:2) print_leverage(rd)

cat("\n========================================================\n")
cat("  KEY OWNERSHIP INSIGHTS\n")
cat("========================================================\n")
cat("
  FIVE FACTORS DRIVING OWNERSHIP:

  1. WIN PROBABILITY: The base driver. People pick teams they think will win.

  2. AVAILABILITY: Each prior pick removes a team from your pool. A team
     picked by 15% in R1 has 15% less supply in R2. This naturally creates
     contrarian spots for teams that are strong but weren't popular earlier.

  3. FUTURE VALUE (SAVE EFFECT): The strongest pattern in the data.
     1-seeds get only ~2% of R1 picks despite ~97-99% win probability.
     People save them for later rounds where the games are harder.
     This effect reverses in R3+: saved teams become the chalk plays.

  4. OPPONENT PRIOR USAGE: A subtle but real effect. If your R2 opponent
     was a popular R1 pick, many entries already used that opponent —
     meaning those entries NEED the opponent to lose (to thin the field).
     Picking against a heavily-used opponent is strategically attractive.

  5. SEED TIEBREAKER VALUE: The 2nd tiebreaker is highest seed sum.
     A 10-seed adds 10 to your sum; a 1-seed adds only 1. So among
     teams with comparable win probability, people prefer higher-numbered
     seeds. This partly explains why 8-seeds (7.9% avg R1) get almost
     as many picks as 3-seeds (10.1%) despite lower win probability.
     Winner-take-all format amplifies this: being risky with higher
     seeds is rational because the upside (tiebreaker edge) only
     matters if you survive — and if you survive, it's decisive.

  USAGE:
    print_ownership(1)     # Round 1 estimates
    print_ownership(2)     # Round 2 estimates
    print_leverage(1)      # Contrarian value, Round 1
    teams[order(-blend_R1)]  # Full data sorted by R1 blend
")
