#!/usr/bin/env Rscript
# ==============================================================================
# contest_mc_sim.R
#
# Forward-knowledge Monte Carlo scoring for Splash NCAA survivor pools.
#
# Replaces the beam search EV computation with exact per-sim optimal paths:
# for each entry group × each sim, find the longest survivable path through
# remaining slots using full knowledge of that sim's tournament outcomes
# (all_results). Resolve payouts against entry-level field survival data
# (n_alive_matrix). Aggregate per-candidate EVs across all sims.
#
# The key insight: this is NOT cheating — we're computing
#   E[payout] = mean over sims of (best possible payout given this outcome)
# which correctly captures optionality without separate scoring.
#
# Usage:
#   source("R/contest_mc_sim.R")
#   scores <- mc_score_candidates(group, ...)
# ==============================================================================

library(data.table)

# ==============================================================================
# HELPERS
# ==============================================================================

#' Build the game column (0-indexed) for a team in a given round
#' @param team_id Integer team_id (1-64 bracket position)
#' @param round_num Integer round number (1-6)
#' @return Integer 0-indexed column in all_results
game_col_for_team <- function(team_id, round_num) {
  ROUND_BASE <- c(0L, 32L, 48L, 56L, 60L, 62L)
  ROUND_BASE[round_num] + as.integer(ceiling(team_id / 2^round_num)) - 1L
}

#' Get candidate teams for a slot that actually survived to this round.
#' Uses locked results (row 1 of all_results) to determine feeder winners.
#'
#' @param slot_id Character slot ID
#' @param sim Sim object with all_results
#' @param teams_dt Teams data frame
#' @return Integer vector of team_ids that reached this round
get_slot_candidates <- function(slot_id, sim, teams_dt, current_round = NULL) {
  slot_def <- get_slot(slot_id)
  round_num <- slot_def$round_num

  if (round_num <= 1) {
    # R64: all teams in these games
    team_ids <- integer(0)
    for (g in slot_def$game_indices) {
      team_ids <- c(team_ids, teams_dt$team_id[2 * g - 1],
                    teams_dt$team_id[2 * g])
    }
    return(unique(team_ids))
  }

  # For rounds whose feeders are locked (already played), use sim 1 winners.
  # For rounds whose feeders are sim-dependent (not yet played), include ALL
  # teams that could bracket into each game. The DFS's
  # all_results(sim, gcol) == tid check will filter to actual winners per sim.
  #
  # Feeders are locked if feeder_round < current_round (the round being decided today).
  # E.g., if current_round=3 (S16), then R64/R32 feeders are locked, but S16+ feeders are not.
  feeder_round <- round_num - 1L
  feeders_locked <- !is.null(current_round) && feeder_round < current_round

  if (feeders_locked) {
    # Feeders already played — use sim 1 winners (all sims agree)
    actual_winners <- as.integer(sim$all_results[1, ])
    candidates <- integer(0)
    for (g in slot_def$game_indices) {
      feeders <- get_feeder_games(g)
      for (fg in feeders) {
        w <- actual_winners[fg]
        if (!is.na(w) && w > 0) {
          candidates <- c(candidates, w)
        }
      }
    }
  } else {
    # Feeders NOT locked — include all teams that could bracket into each game.
    # For a game at round R, 0-indexed position p within the round,
    # the team_ids are: (p * 2^R + 1) through ((p+1) * 2^R).
    ROUND_BASE <- c(0L, 32L, 48L, 56L, 60L, 62L)
    candidates <- integer(0)
    for (g in slot_def$game_indices) {
      p <- g - ROUND_BASE[round_num] - 1L  # 0-indexed position within round
      first_tid <- p * (2L^round_num) + 1L
      last_tid <- (p + 1L) * (2L^round_num)
      candidates <- c(candidates, seq.int(first_tid, last_tid))
    }
  }
  unique(candidates)
}

# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

#' Score all candidates for an entry group using forward-knowledge MC.
#'
#' For each candidate team in today's slot, runs the C++ DFS across all
#' subsampled sims to find the optimal survivable path with full knowledge
#' of sim outcomes. Computes exact payouts against n_field_alive_matrix.
#'
#' @param group A row from group_entries() — has used_teams, contest_id, etc.
#' @param current_slot_id Current slot being decided
#' @param sim Sim object with all_results (n_sims x 63)
#' @param teams_dt Teams data frame (64 rows)
#' @param sample_idx Integer vector of sim indices to use
#' @param n_field_alive_matrix Subsampled matrix (length(sample_idx) x n_remaining_slots)
#' @param viable_cids Integer vector of candidate team_ids to score
#' @param format Contest format ("A", "B", or "C")
#' @return data.table with columns: team_id, team_name, ev, p_survive_today,
#'         p_win_contest, mean_death_rd, p_survive_all
mc_score_candidates <- function(group, current_slot_id, sim, teams_dt,
                                 sample_idx, n_field_alive_matrix,
                                 viable_cids, format = "A") {

  slot_order <- get_slot_order(format)
  current_idx <- match(current_slot_id, slot_order)
  remaining_slots <- slot_order[current_idx:length(slot_order)]
  n_remaining <- length(remaining_slots)

  used_teams <- group$used_teams[[1]]
  prize_pool <- group$prize_pool

  # Build slot metadata for C++
  slot_team_ids_list <- vector("list", n_remaining)
  slot_game_cols_list <- vector("list", n_remaining)
  slot_n_picks <- integer(n_remaining)
  slot_round_nums <- integer(n_remaining)

  for (si in seq_along(remaining_slots)) {
    sid <- remaining_slots[si]
    slot_def <- get_slot(sid)
    round_num <- slot_def$round_num
    n_picks <- get_n_picks(sid, format)

    # Get candidates for this slot (pass current_round so future slots
    # enumerate all bracket-possible teams instead of using sim-1 winners)
    current_round <- get_slot(current_slot_id)$round_num
    cands <- get_slot_candidates(sid, sim, teams_dt, current_round = current_round)

    # Build game columns (0-indexed for C++)
    gcols <- vapply(cands, function(tid) {
      game_col_for_team(tid, round_num)
    }, integer(1))

    slot_team_ids_list[[si]] <- as.integer(cands)
    slot_game_cols_list[[si]] <- as.integer(gcols)
    slot_n_picks[si] <- as.integer(n_picks)
    slot_round_nums[si] <- as.integer(round_num)
  }

  # First slot candidates: filter to viable_cids that aren't in used_teams
  first_cands <- viable_cids[!viable_cids %in% used_teams]
  if (length(first_cands) == 0) {
    return(data.table(
      team_id         = integer(0),
      team_name       = character(0),
      ev              = numeric(0),
      p_survive_today = numeric(0),
      p_win_contest   = numeric(0),
      mean_death_rd   = numeric(0),
      p_survive_all   = numeric(0)
    ))
  }

  first_gcols <- vapply(first_cands, function(tid) {
    game_col_for_team(tid, slot_round_nums[1])
  }, integer(1))

  # Subsample all_results to sample_idx
  ar_sub <- sim$all_results[sample_idx, , drop = FALSE]

  # Call C++
  cpp_result <- solve_optimal_paths_cpp(
    all_results       = ar_sub,
    n_field_alive     = n_field_alive_matrix,
    slot_team_ids_list = slot_team_ids_list,
    slot_game_cols_list = slot_game_cols_list,
    slot_n_picks      = as.integer(slot_n_picks),
    slot_round_nums   = as.integer(slot_round_nums),
    used_teams_vec    = as.integer(used_teams),
    first_slot_cands  = as.integer(first_cands),
    first_slot_gcols  = as.integer(first_gcols),
    prize_pool        = as.double(prize_pool),
    n_slots           = as.integer(n_remaining)
  )

  # Build result data.table
  # Compute p_survive_today from sims (fraction where candidate won slot 0)
  p_today <- vapply(seq_along(first_cands), function(i) {
    tid <- first_cands[i]
    gcol <- first_gcols[i] + 1L  # 1-indexed for R
    mean(ar_sub[, gcol] == tid)
  }, numeric(1))

  result_dt <- data.table(
    team_id         = first_cands,
    team_name       = teams_dt$name[first_cands],
    ev              = cpp_result$ev,
    p_survive_today = p_today,
    p_win_contest   = cpp_result$p_survive,
    mean_death_rd   = cpp_result$mean_death,
    p_survive_all   = cpp_result$p_survive,
    mean_n_alive_surv = if (!is.null(cpp_result$mean_n_alive_when_survived))
                          cpp_result$mean_n_alive_when_survived else NA_real_,
    mean_payout_surv  = if (!is.null(cpp_result$mean_payout_when_survived))
                          cpp_result$mean_payout_when_survived else NA_real_,
    max_payout        = if (!is.null(cpp_result$max_payout))
                          cpp_result$max_payout else NA_real_,
    die_zero_alive    = if (!is.null(cpp_result$die_with_zero_alive))
                          cpp_result$die_with_zero_alive else NA_integer_
  )
  # Attach per-candidate x per-sim death rounds matrix for portfolio EV
  # Rows = candidates (same order as first_cands), Cols = sims
  attr(result_dt, "death_rounds") <- cpp_result$death_rounds
  result_dt
}

cat("Forward-knowledge MC scoring loaded\n")
