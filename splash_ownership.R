#!/usr/bin/env Rscript
# ==============================================================================
# splash_ownership.R
# Field ownership estimation for Splash NCAA survivor pools.
#
# Key differences from Hodes (ownership_model.R):
#   - 1 pick per day (not 3 per round) — higher chalk concentration
#   - No seed-sum tiebreaker — seed boost = 0
#   - Candidate pool is only teams playing TODAY
#   - Day-level granularity (R1 Thu vs R1 Fri are separate decisions)
#
# Calibration priority:
#   1. Live scrape from Splash API (actual ownership, post-hoc)
#   2. Splash historical data (when available)
#   3. Simple win-prob-based softmax as starting point
#
# Usage:
#   source("splash_config.R")
#   source("splash_ownership.R")
# ==============================================================================

library(data.table)

# ==============================================================================
# CORE OWNERSHIP MODEL
# ==============================================================================

#' Estimate field ownership for a given slot
#'
#' For each team playing today, compute the fraction of field entries expected
#' to pick that team. Shares sum to n_picks for the slot (1 for most, 2 for E8).
#'
#' @param slot_id Character slot ID (e.g., "R1_d1")
#' @param teams_dt Data frame with columns: team_id, name, seed, region, rating/AdjEM
#' @param sim_matrix The all_results matrix (n_sims x 63)
#' @param round_info Data frame from sim output
#' @param prior_ownership Optional named list of actual ownership from prior slots
#'   (team_name -> fraction). Used to estimate availability for later slots.
#' @param field_used_dist Optional: estimated distribution of teams the field has
#'   already used. A named numeric vector: team_name -> fraction of field that
#'   has used this team.
#' @return Named numeric vector: team_name -> ownership fraction.
#'   Sums to n_picks for the slot.
estimate_ownership <- function(slot_id, teams_dt, sim_matrix, round_info,
                                prior_ownership = NULL, field_used_dist = NULL) {
  slot <- get_slot(slot_id)
  game_idxs <- slot$game_indices
  n_picks <- slot$n_picks
  round_num <- slot$round_num

  # Identify teams playing in this slot
  # For R64, we know the exact matchups from the bracket
  if (round_num == 1) {
    candidate_ids <- integer(0)
    for (g in game_idxs) {
      candidate_ids <- c(candidate_ids,
                         teams_dt$team_id[2 * g - 1],
                         teams_dt$team_id[2 * g])
    }
    candidates <- teams_dt[teams_dt$team_id %in% candidate_ids, ]
  } else {
    # For later rounds, candidates are teams that won their prior game
    # in at least some sims. Use sim data to identify them.
    candidates <- get_round_candidates(game_idxs, teams_dt, sim_matrix)
  }

  if (nrow(candidates) == 0) {
    warning("No candidates found for slot ", slot_id)
    return(numeric(0))
  }

  # --- Factor 1: Win probability in this round's game ---
  wp <- compute_slot_win_probs(candidates, game_idxs, round_num,
                                teams_dt, sim_matrix)

  # --- Factor 2: Future value (save effect) ---
  # Teams with high championship equity get saved for later rounds
  # Stronger effect in early rounds, weaker later
  fv <- compute_future_value(candidates, sim_matrix, round_num)

  # Save strength: how much future value suppresses current picks
  # With 1 pick per day (vs 3 in Hodes), save effect is even stronger
  # because you have fewer "slots" to use a team
  save_strength <- c(0.60, 0.50, 0.35, 0.15, 0.0, 0.0)[round_num]

  # --- Factor 3: Availability ---
  # Estimate what fraction of the field has already used each candidate
  avail <- rep(1.0, nrow(candidates))
  if (!is.null(field_used_dist)) {
    for (i in seq_len(nrow(candidates))) {
      used_frac <- field_used_dist[[candidates$name[i]]]
      if (!is.null(used_frac)) {
        avail[i] <- max(1.0 - used_frac, 0.05)
      }
    }
  }

  # --- Combine into attractiveness score ---
  # Softmax over: log(attract) = β_wp * wp - save_strength * fv + log(avail)
  # No seed tiebreaker boost (Splash has no seed-sum TB)

  # Concentration parameter: with only 1 pick, people concentrate more on chalk
  # Higher beta = more concentrated on high-wp teams
  beta_wp <- 4.0  # starting estimate; calibrate when Splash data arrives

  log_attract <- beta_wp * wp - save_strength * fv + log(avail)
  log_attract <- log_attract - max(log_attract)  # numerical stability

  raw_shares <- exp(log_attract)
  ownership <- n_picks * raw_shares / sum(raw_shares)

  names(ownership) <- candidates$name
  ownership
}

# ==============================================================================
# HELPER: WIN PROBABILITIES FOR CANDIDATES IN A SLOT
# ==============================================================================

#' Compute win probability for each candidate team in the current slot
compute_slot_win_probs <- function(candidates, game_idxs, round_num,
                                    teams_dt, sim_matrix) {
  n_sims <- nrow(sim_matrix)
  wp <- numeric(nrow(candidates))

  for (i in seq_len(nrow(candidates))) {
    tid <- candidates$team_id[i]
    # Count how many sims this team wins a game in this slot
    wins <- 0
    for (g in game_idxs) {
      wins <- wins + sum(sim_matrix[, g] == tid)
    }
    wp[i] <- wins / n_sims
  }

  # For R64, wp is just the team's R1 win probability
  # For later rounds, wp is conditional on reaching this round
  # (the sim matrix already handles this -- a team only appears as
  # a winner if it won all prior rounds in that sim)
  wp
}

# ==============================================================================
# HELPER: FUTURE VALUE (CHAMPIONSHIP EQUITY)
# ==============================================================================

#' Compute future value for each candidate
#' Future value = championship probability, normalized to [0, 1]
compute_future_value <- function(candidates, sim_matrix, round_num) {
  n_sims <- nrow(sim_matrix)
  champ_col <- 63  # championship game column

  fv <- numeric(nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    tid <- candidates$team_id[i]
    fv[i] <- sum(sim_matrix[, champ_col] == tid) / n_sims
  }

  # Normalize to [0, 1]
  max_fv <- max(fv, na.rm = TRUE)
  if (max_fv > 0) fv <- fv / max_fv
  fv
}

# ==============================================================================
# HELPER: GET CANDIDATES FOR LATER ROUNDS
# ==============================================================================

#' For rounds 2+, identify which teams could be playing in these games
get_round_candidates <- function(game_idxs, teams_dt, sim_matrix) {
  # Find all unique team_ids that appear as winners in these game columns
  all_winners <- integer(0)
  for (g in game_idxs) {
    all_winners <- c(all_winners, unique(sim_matrix[, g]))
  }
  candidate_ids <- unique(all_winners)
  teams_dt[teams_dt$team_id %in% candidate_ids, ]
}

# ==============================================================================
# LIVE OWNERSHIP OVERRIDE
# ==============================================================================

#' Compute actual ownership from scraped Splash data
#'
#' After a slot is complete, compute the actual pick distribution from
#' scraped entry data. This overrides the model for future availability
#' calculations.
#'
#' @param picks_df Data frame of all entries' picks for a slot.
#'   Must have columns: entry_id, team_name (or team_id)
#' @param teams_dt Teams data frame for name resolution
#' @return Named numeric vector: team_name -> actual ownership fraction
compute_actual_ownership <- function(picks_df, teams_dt) {
  picks_df <- as.data.table(picks_df)

  if ("team_name" %in% names(picks_df)) {
    counts <- picks_df[, .N, by = team_name]
    total <- nrow(picks_df)
    result <- setNames(counts$N / total, counts$team_name)
  } else if ("team_id" %in% names(picks_df)) {
    counts <- picks_df[, .N, by = team_id]
    total <- nrow(picks_df)
    # Map back to names
    id_to_name <- setNames(teams_dt$name, teams_dt$team_id)
    result <- setNames(counts$N / total, id_to_name[as.character(counts$team_id)])
  } else {
    stop("picks_df must have 'team_name' or 'team_id' column")
  }

  result
}

#' Estimate field's cumulative team usage from prior slot ownership
#'
#' Given actual ownership for completed slots, estimate what fraction of
#' the field has used each team across all prior slots.
#'
#' @param prior_ownerships List of named vectors from compute_actual_ownership()
#'   or estimate_ownership(), one per completed slot.
#' @return Named numeric vector: team_name -> estimated fraction of field that
#'   has used this team in any prior slot.
estimate_field_usage <- function(prior_ownerships) {
  all_teams <- unique(unlist(lapply(prior_ownerships, names)))
  usage <- setNames(rep(0, length(all_teams)), all_teams)

  for (own in prior_ownerships) {
    for (tm in names(own)) {
      # P(field entry used team across slots) ≈ 1 - prod(1 - slot_ownership)
      # Simplified: just sum, capped at 1
      usage[tm] <- min(usage[tm] + own[tm], 1.0)
    }
  }

  usage
}

# ==============================================================================
# LEVERAGE CALCULATION
# ==============================================================================

#' Compute leverage (contrarian value) for each candidate
#'
#' Leverage = win_probability / ownership.
#' Higher leverage = team is underowned relative to their win probability.
#' Good contrarian picks have high leverage.
#'
#' @param win_probs Named numeric vector: team_name -> win probability
#' @param ownership Named numeric vector: team_name -> ownership fraction
#' @return Named numeric vector: team_name -> leverage ratio
compute_leverage <- function(win_probs, ownership) {
  common_teams <- intersect(names(win_probs), names(ownership))
  leverage <- setNames(numeric(length(common_teams)), common_teams)

  for (tm in common_teams) {
    if (ownership[tm] > 0) {
      leverage[tm] <- win_probs[tm] / ownership[tm]
    } else {
      leverage[tm] <- Inf
    }
  }

  leverage
}

#' Print a formatted ownership + leverage table for a slot
print_ownership <- function(slot_id, ownership, teams_dt, sim_matrix) {
  slot <- get_slot(slot_id)
  game_idxs <- slot$game_indices

  # Compute win probs for display
  n_sims <- nrow(sim_matrix)
  wp <- numeric(length(ownership))
  names(wp) <- names(ownership)

  for (tm in names(ownership)) {
    tid <- teams_dt$team_id[teams_dt$name == tm]
    wins <- 0
    for (g in game_idxs) {
      wins <- wins + sum(sim_matrix[, g] == tid)
    }
    wp[tm] <- wins / n_sims
  }

  leverage <- compute_leverage(wp, ownership)

  # Build display table
  display <- data.table(
    Team = names(ownership),
    Seed = teams_dt$seed[match(names(ownership), teams_dt$name)],
    Region = teams_dt$region[match(names(ownership), teams_dt$name)],
    WinProb = wp[names(ownership)],
    Ownership = ownership,
    Leverage = leverage[names(ownership)]
  )
  setorder(display, -Leverage)

  cat(sprintf("\n=== OWNERSHIP: %s ===\n", slot$label))
  cat(sprintf("%-20s %4s %-8s %7s %7s %8s\n",
              "Team", "Seed", "Region", "WinP%", "Own%", "Leverage"))
  cat(paste(rep("-", 58), collapse = ""), "\n")

  for (i in seq_len(nrow(display))) {
    r <- display[i]
    cat(sprintf("%-20s  %2d   %-8s %6.1f%% %6.1f%% %7.2fx\n",
                r$Team, r$Seed, r$Region,
                100 * r$WinProb, 100 * r$Ownership, r$Leverage))
  }
  cat("\n")

  invisible(display)
}

cat("Splash ownership module loaded\n")
