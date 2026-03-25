#!/usr/bin/env Rscript
# ==============================================================================
# entry_ownership_model.R
#
# Entry-level ownership model for S16 and E8 picks in NCAA survivor pools.
# Predicts P(entry picks team t | used_teams, bracket state) using a softmax
# model calibrated to 2024+2025 historical data.
#
# Model: P(pick t | U) ∝ exp(β_wp * wp(t) - β_save * ce(t) + β_path * path(t, U))
#   where path(t, U) = I(opponent(t) ∈ U_C) * f(constraint_pressure)
#
# Usage:
#   source("R/entry_ownership_model.R")
#   params <- calibrate_entry_model(historical_analysis)
#   probs <- predict_entry_pick_s16(used_teams, candidates, wp, ce, aw, params)
# ==============================================================================

library(data.table)

# Source helpers from analyze_historical_paths.R (game_for_team, get_opponent, etc.)
# These should already be loaded if running after analyze_historical_paths.R
# If not, we define them here as fallbacks

if (!exists("ROUND_BASE")) ROUND_BASE <- c(0L, 32L, 48L, 56L, 60L, 62L)

if (!exists("game_for_team")) {
  game_for_team <- function(team_id, round_num) {
    ROUND_BASE[round_num] + as.integer(ceiling(team_id / 2^round_num))
  }
}

if (!exists("get_feeder_games")) {
  get_feeder_games <- function(game_idx) {
    if (game_idx <= 32L) return(NULL)
    if (game_idx <= 48L) { base_cur <- 32L; base_prev <- 0L }
    else if (game_idx <= 56L) { base_cur <- 48L; base_prev <- 32L }
    else if (game_idx <= 60L) { base_cur <- 56L; base_prev <- 48L }
    else if (game_idx <= 62L) { base_cur <- 60L; base_prev <- 56L }
    else { base_cur <- 62L; base_prev <- 60L }
    offset <- game_idx - base_cur
    c(base_prev + 2L * offset - 1L, base_prev + 2L * offset)
  }
}

if (!exists("get_opponent")) {
  get_opponent <- function(team_id, game_idx, actual_winners) {
    if (game_idx <= 32L) {
      t1 <- 2L * game_idx - 1L; t2 <- 2L * game_idx
      if (team_id == t1) return(t2)
      if (team_id == t2) return(t1)
      return(NA_integer_)
    }
    feeders <- get_feeder_games(game_idx)
    participants <- actual_winners[feeders]
    if (team_id == participants[1]) return(participants[2])
    if (team_id == participants[2]) return(participants[1])
    NA_integer_
  }
}

# ==============================================================================
# SEED MAPPING
# ==============================================================================

# Standard NCAA bracket seed order within each 16-team region
.SEED_ORDER <- c(1L,16L,8L,9L,5L,12L,4L,13L,6L,11L,3L,14L,7L,10L,2L,15L)

#' Get seed from team_id (deterministic mapping)
seed_from_tid <- function(tid) {
  region_pos <- ((tid - 1L) %% 16L) + 1L
  .SEED_ORDER[region_pos]
}

#' Seed prestige: higher for low seeds (1-seed = 1.0, 16-seed ≈ 0.06)
seed_prestige <- function(tid) {
  (17L - seed_from_tid(tid)) / 16
}

# ==============================================================================
# DEFAULT PARAMETERS
# ==============================================================================

default_params <- function(format = "A") {
  list(
    beta_wp_S16      = 4.0,    # log(wp) coefficient for S16
    beta_save_S16    = 2.0,    # seed_prestige save penalty for S16
    beta_path_S16    = 0.5,    # pick-against-self boost for S16
    beta_oneseed_S16 = 1.0,    # additional 1-seed avoidance in S16
    epsilon_S16      = 0.15,   # noise: fraction of field picking "randomly"
    beta_wp_E8       = 4.0,    # wp coefficient for E8 (CE-based save)
    beta_save_E8     = 0.15,   # fv_fraction save penalty for E8
    beta_path_E8     = 2.0,    # pick-against-self boost for E8
    epsilon_E8       = 0.10,   # noise for E8
    contest_size     = NULL,
    format           = format
  )
}

# ==============================================================================
# S16 PREDICTION
# ==============================================================================

#' Predict entry's S16 pick probability for one day
#'
#' @param used_teams Integer vector of team_ids already picked by this entry
#' @param day_candidates Integer vector of team_ids playing on this S16 day
#' @param all_s16_candidates Integer vector of ALL teams in S16 (both days)
#' @param win_probs Named numeric: team_id -> win probability this game
#' @param champ_equities Named numeric: team_id -> championship probability
#' @param actual_winners Integer[63]: who won each game (for opponent detection)
#' @param params Parameter list from calibrate_entry_model() or default_params()
#' @return Named numeric vector: team_id -> pick probability (sums to 1)
predict_entry_pick_s16 <- function(used_teams, day_candidates,
                                    all_s16_candidates, win_probs,
                                    champ_equities, actual_winners, params) {
  n_cand <- length(day_candidates)
  if (n_cand == 0) return(numeric(0))

  beta_wp <- params$beta_wp_S16
  beta_save <- params$beta_save_S16
  beta_path <- params$beta_path_S16
  beta_oneseed <- if (!is.null(params$beta_oneseed_S16)) params$beta_oneseed_S16 else 0

  # Constraint pressure: how many of ALL S16 candidates are used?
  used_in_s16 <- intersect(used_teams, all_s16_candidates)
  constraint_pressure <- length(used_in_s16) / length(all_s16_candidates)

  scores <- numeric(n_cand)
  names(scores) <- as.character(day_candidates)

  for (i in seq_len(n_cand)) {
    tid <- day_candidates[i]

    # Hard constraint: can't pick a team already used
    if (tid %in% used_teams) {
      scores[i] <- -Inf
      next
    }

    wp <- win_probs[as.character(tid)]
    if (is.na(wp)) wp <- 0.5

    # Seed-based save: field saves by seed prestige, not CE
    sp <- seed_prestige(tid)
    is_oneseed <- as.numeric(seed_from_tid(tid) == 1L)

    # Path feature: is this team's opponent a used team?
    game_idx <- game_for_team(tid, 3L)
    opponent <- get_opponent(tid, game_idx, actual_winners)
    opp_is_used <- !is.na(opponent) && opponent %in% used_in_s16

    path_boost <- if (opp_is_used) beta_path * constraint_pressure else 0

    scores[i] <- beta_wp * log(max(wp, 0.01)) - beta_save * sp -
                 beta_oneseed * is_oneseed + path_boost
  }

  # Softmax
  finite_mask <- is.finite(scores)
  if (!any(finite_mask)) {
    # All candidates used — shouldn't happen but handle gracefully
    probs <- rep(1 / n_cand, n_cand)
    names(probs) <- as.character(day_candidates)
    return(probs)
  }

  scores[!finite_mask] <- -Inf
  scores <- scores - max(scores[finite_mask])
  raw <- exp(scores)
  probs <- raw / sum(raw)

  # Mix with uniform over available candidates (noise parameter)
  epsilon <- if (!is.null(params$epsilon_S16)) params$epsilon_S16 else 0
  if (epsilon > 0) {
    n_avail <- sum(finite_mask)
    uniform <- ifelse(finite_mask, 1 / n_avail, 0)
    probs <- (1 - epsilon) * probs + epsilon * uniform
  }

  names(probs) <- as.character(day_candidates)
  probs
}

# ==============================================================================
# E8 PREDICTION (PAIR SELECTION)
# ==============================================================================

#' Predict entry's E8 pick probabilities (marginal per team, sums to 2)
#'
#' Models E8 as pair selection: P(pair) ∝ exp(score(t1) + score(t2|t1))
#' Valid pairs must be from opposite bracket sides (regions 1+2 vs 3+4).
#' For Format B, additionally from different days.
#'
#' @param used_teams Integer vector of team_ids already picked
#' @param e8_candidates Integer vector of team_ids in E8
#' @param win_probs Named numeric: team_id -> win probability
#' @param champ_equities Named numeric: team_id -> championship probability
#' @param actual_winners Integer[63]: who won each game
#' @param params Parameter list
#' @param e8_day_games Optional list for Format B: list(sat = game_indices, sun = game_indices)
#' @return Named numeric vector: team_id -> marginal pick probability (sums to 2)
predict_entry_pick_e8 <- function(used_teams, e8_candidates, win_probs,
                                   champ_equities, actual_winners, params,
                                   e8_day_games = NULL) {
  n_cand <- length(e8_candidates)
  if (n_cand == 0) return(numeric(0))

  beta_wp <- params$beta_wp_E8
  beta_save <- params$beta_save_E8
  beta_path <- params$beta_path_E8
  format <- if (!is.null(params$format)) params$format else "A"

  # Constraint pressure
  used_in_e8 <- intersect(used_teams, e8_candidates)
  constraint_pressure <- length(used_in_e8) / length(e8_candidates)

  # Remaining FV
  unused_e8 <- setdiff(e8_candidates, used_teams)
  remaining_fv <- sum(champ_equities[as.character(unused_e8)], na.rm = TRUE)

  # Compute individual scores
  ind_scores <- numeric(n_cand)
  names(ind_scores) <- as.character(e8_candidates)
  team_regions <- integer(n_cand)
  team_games <- integer(n_cand)

  for (i in seq_len(n_cand)) {
    tid <- e8_candidates[i]
    team_regions[i] <- as.integer(ceiling(tid / 16))  # get_team_region
    team_games[i] <- game_for_team(tid, 4L)

    if (tid %in% used_teams) {
      ind_scores[i] <- -Inf
      next
    }

    wp <- win_probs[as.character(tid)]
    ce <- champ_equities[as.character(tid)]
    if (is.na(wp)) wp <- 0.5
    if (is.na(ce)) ce <- 0

    fv_frac <- if (remaining_fv > 0) ce / remaining_fv else 0

    game_idx <- game_for_team(tid, 4L)
    opponent <- get_opponent(tid, game_idx, actual_winners)
    opp_is_used <- !is.na(opponent) && opponent %in% used_in_e8

    path_boost <- if (opp_is_used) beta_path * constraint_pressure else 0

    ind_scores[i] <- beta_wp * wp - beta_save * fv_frac + path_boost
  }

  # Enumerate all valid pairs
  available <- which(is.finite(ind_scores))
  if (length(available) < 2) {
    # Can't form a valid pair — return uniform over available
    probs <- rep(0, n_cand)
    names(probs) <- as.character(e8_candidates)
    if (length(available) > 0) probs[available] <- 2 / length(available)
    return(probs)
  }

  pair_scores <- list()
  pair_indices <- list()

  for (a in seq_along(available)[-length(available)]) {
    for (b in (a + 1):length(available)) {
      i <- available[a]
      j <- available[b]

      # Must be from different games
      if (team_games[i] == team_games[j]) next

      # Must be from opposite bracket sides (regions 1+2 vs 3+4)
      side_i <- if (team_regions[i] <= 2) 1L else 2L
      side_j <- if (team_regions[j] <= 2) 1L else 2L
      if (side_i == side_j) next

      # Format B: must be from different days
      if (format == "B" && !is.null(e8_day_games)) {
        day_i <- if (team_games[i] %in% e8_day_games$sat) "sat" else "sun"
        day_j <- if (team_games[j] %in% e8_day_games$sat) "sat" else "sun"
        if (day_i == day_j) next
      }

      # Score for this pair: score(t1, U) + score(t2, U ∪ {t1})
      # For t2, recompute path boost with t1 added to used
      tid_j <- e8_candidates[j]
      game_j <- team_games[j]
      opp_j <- get_opponent(tid_j, game_j, actual_winners)
      used_plus_i <- c(used_in_e8, e8_candidates[i])
      opp_j_used <- !is.na(opp_j) && opp_j %in% used_plus_i
      cp_j <- (length(used_plus_i)) / length(e8_candidates)

      wp_j <- win_probs[as.character(tid_j)]
      ce_j <- champ_equities[as.character(tid_j)]
      if (is.na(wp_j)) wp_j <- 0.5
      if (is.na(ce_j)) ce_j <- 0
      # After picking i, remaining_fv decreases
      rfv_after_i <- remaining_fv - champ_equities[as.character(e8_candidates[i])]
      if (is.na(rfv_after_i) || rfv_after_i <= 0) rfv_after_i <- 1e-6
      fv_frac_j <- ce_j / rfv_after_i
      score_j_given_i <- beta_wp * wp_j - beta_save * fv_frac_j +
        (if (opp_j_used) beta_path * cp_j else 0)

      # Symmetric: also compute score(t1, U ∪ {t2})
      tid_i <- e8_candidates[i]
      game_i <- team_games[i]
      opp_i <- get_opponent(tid_i, game_i, actual_winners)
      used_plus_j <- c(used_in_e8, e8_candidates[j])
      opp_i_used <- !is.na(opp_i) && opp_i %in% used_plus_j
      cp_i <- (length(used_plus_j)) / length(e8_candidates)

      wp_i <- win_probs[as.character(tid_i)]
      ce_i <- champ_equities[as.character(tid_i)]
      if (is.na(wp_i)) wp_i <- 0.5
      if (is.na(ce_i)) ce_i <- 0
      rfv_after_j <- remaining_fv - champ_equities[as.character(e8_candidates[j])]
      if (is.na(rfv_after_j) || rfv_after_j <= 0) rfv_after_j <- 1e-6
      fv_frac_i <- ce_i / rfv_after_j
      score_i_given_j <- beta_wp * wp_i - beta_save * fv_frac_i +
        (if (opp_i_used) beta_path * cp_i else 0)

      # Pair score: average of both orderings
      pair_score <- (ind_scores[i] + score_j_given_i +
                       ind_scores[j] + score_i_given_j) / 2

      pair_scores[[length(pair_scores) + 1]] <- pair_score
      pair_indices[[length(pair_indices) + 1]] <- c(i, j)
    }
  }

  if (length(pair_scores) == 0) {
    probs <- rep(0, n_cand)
    names(probs) <- as.character(e8_candidates)
    if (length(available) > 0) probs[available] <- 2 / length(available)
    return(probs)
  }

  # Softmax over pairs
  ps <- unlist(pair_scores)
  ps <- ps - max(ps)
  pair_probs <- exp(ps) / sum(exp(ps))

  # Marginals: sum pair probabilities containing each team
  marginals <- rep(0, n_cand)
  for (k in seq_along(pair_indices)) {
    marginals[pair_indices[[k]][1]] <- marginals[pair_indices[[k]][1]] + pair_probs[k]
    marginals[pair_indices[[k]][2]] <- marginals[pair_indices[[k]][2]] + pair_probs[k]
  }

  # Scale to sum to 2
  marginals <- 2 * marginals / sum(marginals)
  names(marginals) <- as.character(e8_candidates)
  marginals
}


# ==============================================================================
# E8 PAIR PROBABILITIES (for entry field sim)
# ==============================================================================

#' Predict entry's E8 pair probabilities (returns list of pairs + probs)
#'
#' Same model as predict_entry_pick_e8() but returns the pair-level probabilities
#' instead of marginals. Used by entry_field_sim.R for exact E8 survival and
#' sub-group expansion.
#'
#' @param used_teams Integer vector of team_ids already picked
#' @param e8_candidates Integer vector of team_ids in E8
#' @param win_probs Named numeric: team_id -> win probability
#' @param champ_equities Named numeric: team_id -> championship probability
#' @param actual_winners Integer[63]: who won each game
#' @param params Parameter list
#' @param e8_day_games Optional list for Format B
#' @return List with:
#'   pairs: list of integer(2) vectors (team_id pairs)
#'   probs: numeric vector of pair probabilities (sums to 1)
#'   candidates: the original e8_candidates
predict_entry_pick_e8_pairs <- function(used_teams, e8_candidates, win_probs,
                                         champ_equities, actual_winners, params,
                                         e8_day_games = NULL) {
  n_cand <- length(e8_candidates)
  if (n_cand < 2) {
    return(list(pairs = list(), probs = numeric(0), candidates = e8_candidates))
  }

  beta_wp <- params$beta_wp_E8
  beta_save <- params$beta_save_E8
  beta_path <- params$beta_path_E8
  format <- if (!is.null(params$format)) params$format else "A"

  used_in_e8 <- intersect(used_teams, e8_candidates)
  constraint_pressure <- length(used_in_e8) / length(e8_candidates)

  unused_e8 <- setdiff(e8_candidates, used_teams)
  remaining_fv <- sum(champ_equities[as.character(unused_e8)], na.rm = TRUE)

  # Individual scores
  ind_scores <- numeric(n_cand)
  names(ind_scores) <- as.character(e8_candidates)
  team_regions <- integer(n_cand)
  team_games <- integer(n_cand)

  for (i in seq_len(n_cand)) {
    tid <- e8_candidates[i]
    team_regions[i] <- as.integer(ceiling(tid / 16))
    team_games[i] <- game_for_team(tid, 4L)

    if (tid %in% used_teams) {
      ind_scores[i] <- -Inf
      next
    }

    wp <- win_probs[as.character(tid)]
    ce <- champ_equities[as.character(tid)]
    if (is.na(wp)) wp <- 0.5
    if (is.na(ce)) ce <- 0

    fv_frac <- if (remaining_fv > 0) ce / remaining_fv else 0

    game_idx <- game_for_team(tid, 4L)
    opponent <- get_opponent(tid, game_idx, actual_winners)
    opp_is_used <- !is.na(opponent) && opponent %in% used_in_e8

    path_boost <- if (opp_is_used) beta_path * constraint_pressure else 0

    ind_scores[i] <- beta_wp * wp - beta_save * fv_frac + path_boost
  }

  available <- which(is.finite(ind_scores))
  if (length(available) < 2) {
    return(list(pairs = list(), probs = numeric(0), candidates = e8_candidates))
  }

  pair_scores_vec <- numeric(0)
  pair_list <- list()

  for (a in seq_along(available)[-length(available)]) {
    for (b in (a + 1):length(available)) {
      i <- available[a]
      j <- available[b]

      if (team_games[i] == team_games[j]) next

      # NOTE: No bracket-side constraint here. Unlike our optimizer (which
      # enforces opposite sides for strategic correctness), the field model
      # allows same-side pairs. The calibrated betas naturally capture the
      # tendency to pick opposite sides from historical data, while letting
      # some probability mass flow to same-side pairs for field entries
      # that make this mistake.

      # Pair score (same logic as predict_entry_pick_e8)
      tid_j <- e8_candidates[j]
      game_j <- team_games[j]
      opp_j <- get_opponent(tid_j, game_j, actual_winners)
      used_plus_i <- c(used_in_e8, e8_candidates[i])
      opp_j_used <- !is.na(opp_j) && opp_j %in% used_plus_i
      cp_j <- (length(used_plus_i)) / length(e8_candidates)

      wp_j <- win_probs[as.character(tid_j)]
      ce_j <- champ_equities[as.character(tid_j)]
      if (is.na(wp_j)) wp_j <- 0.5
      if (is.na(ce_j)) ce_j <- 0
      rfv_after_i <- remaining_fv - champ_equities[as.character(e8_candidates[i])]
      if (is.na(rfv_after_i) || rfv_after_i <= 0) rfv_after_i <- 1e-6
      fv_frac_j <- ce_j / rfv_after_i
      score_j_given_i <- beta_wp * wp_j - beta_save * fv_frac_j +
        (if (opp_j_used) beta_path * cp_j else 0)

      tid_i <- e8_candidates[i]
      game_i <- team_games[i]
      opp_i <- get_opponent(tid_i, game_i, actual_winners)
      used_plus_j <- c(used_in_e8, e8_candidates[j])
      opp_i_used <- !is.na(opp_i) && opp_i %in% used_plus_j
      cp_i <- (length(used_plus_j)) / length(e8_candidates)

      wp_i <- win_probs[as.character(tid_i)]
      ce_i <- champ_equities[as.character(tid_i)]
      if (is.na(wp_i)) wp_i <- 0.5
      if (is.na(ce_i)) ce_i <- 0
      rfv_after_j <- remaining_fv - champ_equities[as.character(e8_candidates[j])]
      if (is.na(rfv_after_j) || rfv_after_j <= 0) rfv_after_j <- 1e-6
      fv_frac_i <- ce_i / rfv_after_j
      score_i_given_j <- beta_wp * wp_i - beta_save * fv_frac_i +
        (if (opp_i_used) beta_path * cp_i else 0)

      pair_score <- (ind_scores[i] + score_j_given_i +
                       ind_scores[j] + score_i_given_j) / 2

      pair_scores_vec <- c(pair_scores_vec, pair_score)
      pair_list[[length(pair_list) + 1]] <- c(e8_candidates[i], e8_candidates[j])
    }
  }

  if (length(pair_scores_vec) == 0) {
    return(list(pairs = list(), probs = numeric(0), candidates = e8_candidates))
  }

  # Softmax over pairs
  ps <- pair_scores_vec - max(pair_scores_vec)
  pair_probs <- exp(ps) / sum(exp(ps))

  list(
    pairs      = pair_list,
    probs      = pair_probs,
    candidates = e8_candidates
  )
}


# ==============================================================================
# CALIBRATION
# ==============================================================================

# NOTE: prepare_calibration_data has been moved into analyze_year() in
# analyze_historical_paths.R so calibration data is computed while the sim
# matrix is still in memory (avoids double-loading 38MB+ files).

.REMOVED_prepare_calibration_data <- function(year, entry_wide, actual_winners,
                                      sim_matrix, teams_dt, picks_long) {
  n_sims <- nrow(sim_matrix)

  # --- Identify S16 candidates (teams that made S16) ---
  s16_winners <- actual_winners[49:56]  # these are E8 participants but...
  # Actually, S16 participants = winners of R32 games 33-48
  s16_participants <- actual_winners[33:48]
  s16_participants <- s16_participants[s16_participants > 0L]

  # Determine S16 day split from picks
  s16_d1_teams <- picks_long[slot_id == "S16_d1" & !is.na(team_id), unique(team_id)]
  s16_d2_teams <- picks_long[slot_id == "S16_d2" & !is.na(team_id), unique(team_id)]

  # S16 games per day
  s16_d1_games <- unique(vapply(s16_d1_teams, function(t) game_for_team(t, 3L), integer(1)))
  s16_d2_games <- unique(vapply(s16_d2_teams, function(t) game_for_team(t, 3L), integer(1)))

  # Compute win probs and champ equity for all S16 participants
  wp_s16 <- setNames(numeric(length(s16_participants)), as.character(s16_participants))
  ce_s16 <- setNames(numeric(length(s16_participants)), as.character(s16_participants))
  for (tid in s16_participants) {
    g <- game_for_team(tid, 3L)
    # Win prob: fraction of sims where this team wins this S16 game,
    # among sims where this team reached S16
    feeder_g <- game_for_team(tid, 2L)
    reached <- sum(sim_matrix[, feeder_g] == tid)
    won <- sum(sim_matrix[, g] == tid)
    wp_s16[as.character(tid)] <- if (reached > 0) won / reached else 0.5
    ce_s16[as.character(tid)] <- sum(sim_matrix[, 63] == tid) / n_sims
  }

  # --- Build S16 calibration rows ---
  prior_cols <- c("pick_R1_d1", "pick_R1_d2", "pick_R2_d1", "pick_R2_d2")
  prior_cols <- intersect(prior_cols, names(entry_wide))

  s16_rows <- list()

  for (s16_slot in c("S16_d1", "S16_d2")) {
    pick_col <- paste0("pick_", s16_slot)
    if (!pick_col %in% names(entry_wide)) next

    day_teams <- if (s16_slot == "S16_d1") s16_d1_teams else s16_d2_teams
    day_teams <- day_teams[day_teams > 0]

    entries_with_pick <- entry_wide[!is.na(get(pick_col))]
    if (nrow(entries_with_pick) == 0) next

    for (row_i in seq_len(nrow(entries_with_pick))) {
      entry <- entries_with_pick[row_i]
      actual_pick <- entry[[pick_col]]
      used <- na.omit(as.integer(unlist(entry[, ..prior_cols])))

      # Constraint pressure (across both S16 days)
      used_in_s16 <- intersect(used, s16_participants)
      cp <- length(used_in_s16) / length(s16_participants)

      s16_rows[[length(s16_rows) + 1]] <- data.table(
        year = year,
        entry_id = entry$entry_id,
        slot_id = s16_slot,
        actual_pick = actual_pick,
        candidates = list(as.integer(day_teams)),
        used_teams = list(as.integer(used)),
        constraint_pressure = cp,
        win_probs = list(wp_s16[as.character(day_teams)]),
        champ_equities = list(ce_s16[as.character(day_teams)])
      )
    }
  }

  s16_calib <- if (length(s16_rows) > 0) rbindlist(s16_rows) else data.table()

  # --- Build E8 calibration rows ---
  e8_participants <- actual_winners[49:56]
  e8_participants <- e8_participants[e8_participants > 0L]

  wp_e8 <- setNames(numeric(length(e8_participants)), as.character(e8_participants))
  ce_e8 <- setNames(numeric(length(e8_participants)), as.character(e8_participants))
  for (tid in e8_participants) {
    g <- game_for_team(tid, 4L)
    feeder_g <- game_for_team(tid, 3L)
    reached <- sum(sim_matrix[, feeder_g] == tid)
    won <- sum(sim_matrix[, g] == tid)
    wp_e8[as.character(tid)] <- if (reached > 0) won / reached else 0.5
    ce_e8[as.character(tid)] <- sum(sim_matrix[, 63] == tid) / n_sims
  }

  prior_to_e8_cols <- c("pick_R1_d1", "pick_R1_d2", "pick_R2_d1", "pick_R2_d2",
                          "pick_S16_d1", "pick_S16_d2")
  prior_to_e8_cols <- intersect(prior_to_e8_cols, names(entry_wide))

  e8_rows <- list()
  e8_entries <- entry_wide[!is.na(entry_wide[["pick_E8_1"]])]

  if (nrow(e8_entries) > 0) {
    for (row_i in seq_len(nrow(e8_entries))) {
      entry <- e8_entries[row_i]
      pick1 <- entry$pick_E8_1
      pick2 <- if ("pick_E8_2" %in% names(entry)) entry$pick_E8_2 else NA_integer_
      used <- na.omit(as.integer(unlist(entry[, ..prior_to_e8_cols])))

      used_in_e8 <- intersect(used, e8_participants)
      cp <- length(used_in_e8) / length(e8_participants)

      e8_rows[[length(e8_rows) + 1]] <- data.table(
        year = year,
        entry_id = entry$entry_id,
        pick_1 = pick1,
        pick_2 = pick2,
        candidates = list(as.integer(e8_participants)),
        used_teams = list(as.integer(used)),
        constraint_pressure = cp,
        win_probs = list(wp_e8[as.character(e8_participants)]),
        champ_equities = list(ce_e8[as.character(e8_participants)])
      )
    }
  }

  e8_calib <- if (length(e8_rows) > 0) rbindlist(e8_rows) else data.table()

  list(
    s16_calib = s16_calib,
    e8_calib = e8_calib,
    actual_winners = actual_winners,
    wp_s16 = wp_s16, ce_s16 = ce_s16,
    wp_e8 = wp_e8, ce_e8 = ce_e8,
    s16_participants = s16_participants,
    e8_participants = e8_participants
  )
}


#' Compute negative log-likelihood for S16 model
#'
#' @param par Numeric vector: c(beta_wp, beta_save, beta_path, beta_oneseed)
#' @param calib_dt S16 calibration data.table
#' @param actual_winners The results vector
#' @return Scalar NLL
s16_nll <- function(par, calib_dt, actual_winners) {
  beta_wp <- par[1]
  beta_save <- par[2]
  beta_path <- par[3]
  beta_oneseed <- if (length(par) >= 4) par[4] else 0

  total_nll <- 0

  for (row_i in seq_len(nrow(calib_dt))) {
    row <- calib_dt[row_i]
    candidates <- row$candidates[[1]]
    used <- row$used_teams[[1]]
    wp <- row$win_probs[[1]]
    cp <- row$constraint_pressure
    actual <- row$actual_pick

    # Compute scores
    scores <- numeric(length(candidates))
    for (j in seq_along(candidates)) {
      tid <- candidates[j]
      if (tid %in% used) {
        scores[j] <- -Inf
        next
      }

      # Seed-based save (same as Hodes finding: field saves by seed, not CE)
      sp <- seed_prestige(tid)
      is_oneseed <- as.numeric(seed_from_tid(tid) == 1L)

      # Path feature
      g <- game_for_team(tid, 3L)
      opp <- get_opponent(tid, g, actual_winners)
      used_in_s16 <- intersect(used, candidates)
      opp_used <- !is.na(opp) && opp %in% used_in_s16
      path <- if (opp_used) beta_path * cp else 0

      scores[j] <- beta_wp * log(max(wp[j], 0.01)) - beta_save * sp -
                   beta_oneseed * is_oneseed + path
    }

    # Softmax
    finite <- is.finite(scores)
    if (!any(finite)) next

    scores[!finite] <- -Inf
    scores <- scores - max(scores[finite])
    probs <- exp(scores) / sum(exp(scores[finite]))

    # Find actual pick index
    pick_idx <- match(actual, candidates)
    if (is.na(pick_idx) || !is.finite(scores[pick_idx])) next

    p <- max(probs[pick_idx], 1e-10)
    total_nll <- total_nll - log(p)
  }

  total_nll
}


#' Compute negative log-likelihood for E8 pair model
#'
#' @param par Numeric vector: c(beta_wp, beta_save, beta_path)
#' @param calib_dt E8 calibration data.table
#' @param actual_winners The results vector
#' @return Scalar NLL
e8_nll <- function(par, calib_dt, actual_winners) {
  beta_wp <- par[1]
  beta_save <- par[2]
  beta_path <- par[3]

  total_nll <- 0

  for (row_i in seq_len(nrow(calib_dt))) {
    row <- calib_dt[row_i]
    candidates <- row$candidates[[1]]
    used <- row$used_teams[[1]]
    wp <- row$win_probs[[1]]
    fv_frac <- if ("fv_fractions" %in% names(calib_dt)) row$fv_fractions[[1]] else row$champ_equities[[1]]
    ce <- row$champ_equities[[1]]  # keep raw CE for remaining_fv adjustments in pair scoring
    cp <- row$constraint_pressure
    remaining_fv <- if ("remaining_fv" %in% names(calib_dt)) row$remaining_fv else sum(ce, na.rm = TRUE)
    pick1 <- row$pick_1
    pick2 <- row$pick_2

    if (is.na(pick1) || is.na(pick2)) next

    n_cand <- length(candidates)
    used_in_e8 <- intersect(used, candidates)

    # Compute individual scores
    ind_scores <- numeric(n_cand)
    team_regions <- integer(n_cand)
    team_games <- integer(n_cand)

    for (j in seq_len(n_cand)) {
      tid <- candidates[j]
      team_regions[j] <- as.integer(ceiling(tid / 16))
      team_games[j] <- game_for_team(tid, 4L)

      if (tid %in% used) {
        ind_scores[j] <- -Inf
        next
      }

      g <- game_for_team(tid, 4L)
      opp <- get_opponent(tid, g, actual_winners)
      opp_used <- !is.na(opp) && opp %in% used_in_e8
      path <- if (opp_used) beta_path * cp else 0

      ind_scores[j] <- beta_wp * wp[j] - beta_save * fv_frac[j] + path
    }

    # Enumerate valid pairs and compute pair scores
    available <- which(is.finite(ind_scores))
    pair_scores <- numeric(0)
    pair_is_actual <- logical(0)

    for (a in seq_along(available)[-length(available)]) {
      for (b in (a + 1):length(available)) {
        i <- available[a]; j <- available[b]

        # Different games
        if (team_games[i] == team_games[j]) next

        # Opposite bracket sides
        side_i <- if (team_regions[i] <= 2) 1L else 2L
        side_j <- if (team_regions[j] <= 2) 1L else 2L
        if (side_i == side_j) next

        # Pair score with path adjustment for second pick
        tid_j <- candidates[j]
        g_j <- team_games[j]
        opp_j <- get_opponent(tid_j, g_j, actual_winners)
        used_plus_i <- c(used_in_e8, candidates[i])
        opp_j_used_2 <- !is.na(opp_j) && opp_j %in% used_plus_i
        cp_j <- length(used_plus_i) / n_cand
        # After picking i, remaining_fv decreases
        rfv_j <- remaining_fv - ce[i]
        if (is.na(rfv_j) || rfv_j <= 0) rfv_j <- 1e-6
        fvf_j2 <- ce[j] / rfv_j
        score_j2 <- beta_wp * wp[j] - beta_save * fvf_j2 +
          (if (opp_j_used_2) beta_path * cp_j else 0)

        tid_i <- candidates[i]
        g_i <- team_games[i]
        opp_i <- get_opponent(tid_i, g_i, actual_winners)
        used_plus_j <- c(used_in_e8, candidates[j])
        opp_i_used_2 <- !is.na(opp_i) && opp_i %in% used_plus_j
        cp_i <- length(used_plus_j) / n_cand
        rfv_i <- remaining_fv - ce[j]
        if (is.na(rfv_i) || rfv_i <= 0) rfv_i <- 1e-6
        fvf_i2 <- ce[i] / rfv_i
        score_i2 <- beta_wp * wp[i] - beta_save * fvf_i2 +
          (if (opp_i_used_2) beta_path * cp_i else 0)

        ps <- (ind_scores[i] + score_j2 + ind_scores[j] + score_i2) / 2

        pair_scores <- c(pair_scores, ps)

        # Check if this is the actual pair picked
        ci <- candidates[i]; cj <- candidates[j]
        is_actual <- (ci == pick1 && cj == pick2) || (ci == pick2 && cj == pick1)
        pair_is_actual <- c(pair_is_actual, is_actual)
      }
    }

    if (length(pair_scores) == 0 || !any(pair_is_actual)) next

    # Softmax over pairs
    ps <- pair_scores - max(pair_scores)
    pair_probs <- exp(ps) / sum(exp(ps))

    actual_prob <- sum(pair_probs[pair_is_actual])
    actual_prob <- max(actual_prob, 1e-10)
    total_nll <- total_nll - log(actual_prob)
  }

  total_nll
}


#' Calibrate the entry-level ownership model
#'
#' @param historical_analysis Output from analyze_historical_paths.R
#'   (contains s16_calib and e8_calib pre-computed during analysis)
#' @param n_restarts Number of random restarts for optimization
#' @return Calibrated params list
calibrate_entry_model <- function(historical_analysis, n_restarts = 5) {
  cat("Calibrating entry-level ownership model...\n")

  # Collect pre-computed calibration data from analysis output
  all_s16 <- list()
  all_e8 <- list()
  all_actual_winners <- list()

  for (yr_name in c("results_2024", "results_2025")) {
    yr <- historical_analysis[[yr_name]]
    if (is.null(yr)) next

    year_str <- as.character(yr$year)
    if (!is.null(yr$s16_calib) && nrow(yr$s16_calib) > 0) {
      all_s16[[year_str]] <- yr$s16_calib
    }
    if (!is.null(yr$e8_calib) && nrow(yr$e8_calib) > 0) {
      all_e8[[year_str]] <- yr$e8_calib
    }
    all_actual_winners[[year_str]] <- yr$actual_winners
  }

  s16_calib <- rbindlist(all_s16, fill = TRUE)
  e8_calib <- rbindlist(all_e8, fill = TRUE)

  cat(sprintf("  S16 calibration rows: %d\n", nrow(s16_calib)))
  cat(sprintf("  E8 calibration rows: %d\n", nrow(e8_calib)))

  # --- Fit S16 parameters ---
  cat("\n  Fitting S16 model...\n")
  best_s16 <- NULL
  best_s16_nll <- Inf

  # Combine actual_winners for NLL computation (handle multi-year)
  # The NLL function uses actual_winners per-row, but our current implementation
  # uses a single actual_winners. Split by year.
  s16_nll_combined <- function(par) {
    total <- 0
    for (yr_str in names(all_actual_winners)) {
      yr_data <- all_s16[[yr_str]]
      if (is.null(yr_data) || nrow(yr_data) == 0) next
      total <- total + s16_nll(par, yr_data, all_actual_winners[[yr_str]])
    }
    total
  }

  for (restart in seq_len(n_restarts)) {
    init <- c(
      runif(1, 1, 8),   # beta_wp (on log(wp) scale)
      runif(1, 0, 4),   # beta_save (seed_prestige)
      runif(1, 0, 3),   # beta_path
      runif(1, 0, 3)    # beta_oneseed
    )

    result <- tryCatch(
      optim(init, s16_nll_combined,
            method = "L-BFGS-B",
            lower = c(0.01, 0, 0, 0),
            upper = c(30, 15, 15, 10)),
      error = function(e) NULL
    )

    if (!is.null(result) && result$value < best_s16_nll) {
      best_s16_nll <- result$value
      best_s16 <- result$par
    }
  }

  cat(sprintf("  S16 fitted: beta_wp=%.3f, beta_save=%.3f, beta_path=%.3f, beta_oneseed=%.3f (NLL=%.1f)\n",
              best_s16[1], best_s16[2], best_s16[3], best_s16[4], best_s16_nll))

  # --- Fit E8 parameters ---
  cat("\n  Fitting E8 model...\n")
  best_e8 <- NULL
  best_e8_nll <- Inf

  e8_nll_combined <- function(par) {
    total <- 0
    for (yr_str in names(all_actual_winners)) {
      yr_data <- all_e8[[yr_str]]
      if (is.null(yr_data) || nrow(yr_data) == 0) next
      total <- total + e8_nll(par, yr_data, all_actual_winners[[yr_str]])
    }
    total
  }

  for (restart in seq_len(n_restarts)) {
    init <- c(
      runif(1, 1, 8),
      runif(1, 0, 1),
      runif(1, 0, 5)
    )

    result <- tryCatch(
      optim(init, e8_nll_combined,
            method = "L-BFGS-B",
            lower = c(0.01, 0, 0),
            upper = c(30, 30, 15)),
      error = function(e) NULL
    )

    if (!is.null(result) && result$value < best_e8_nll) {
      best_e8_nll <- result$value
      best_e8 <- result$par
    }
  }

  cat(sprintf("  E8 fitted: beta_wp=%.3f, beta_save=%.3f, beta_path=%.3f (NLL=%.1f)\n",
              best_e8[1], best_e8[2], best_e8[3], best_e8_nll))

  # --- Assemble params ---
  params <- list(
    beta_wp_S16      = best_s16[1],
    beta_save_S16    = best_s16[2],
    beta_path_S16    = best_s16[3],
    beta_oneseed_S16 = best_s16[4],
    beta_wp_E8       = best_e8[1],
    beta_save_E8     = best_e8[2],
    beta_path_E8     = best_e8[3],
    contest_size     = NULL,
    format           = "A",
    s16_nll          = best_s16_nll,
    e8_nll           = best_e8_nll,
    n_s16_obs        = nrow(s16_calib),
    n_e8_obs         = nrow(e8_calib)
  )

  cat("\nCalibration complete.\n")
  cat(sprintf("  S16: NLL/obs = %.4f (%.0f obs)\n",
              best_s16_nll / nrow(s16_calib), nrow(s16_calib)))
  cat(sprintf("  E8:  NLL/obs = %.4f (%.0f obs)\n",
              best_e8_nll / nrow(e8_calib), nrow(e8_calib)))

  params
}

cat("Entry ownership model loaded.\n")
