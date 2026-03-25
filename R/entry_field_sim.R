#!/usr/bin/env Rscript
# ==============================================================================
# entry_field_sim.R
#
# Entry-level field simulation for S16+ rounds.
# Instead of group-level field behavior models, this uses the calibrated
# entry ownership model to predict each entry's picks based on their
# used_teams history. Produces per-sim exact opponent survival counts.
#
# Memory-efficient: uses the identity (A * B) * c = A * (B * c) to avoid
# materializing the full (n_sims x n_groups) survival matrix. Only the
# n_alive vector (n_sims x 1 per slot) is kept.
#
# Usage:
#   source("R/entry_ownership_model.R")
#   source("R/entry_field_sim.R")
#   result <- simulate_entry_level_field(field_avail, sim, "S16_d1", params, teams_dt)
# ==============================================================================

library(data.table)

# ==============================================================================
# HELPERS
# ==============================================================================

#' Compute conditional win probabilities from sim matrix
#'
#' @param sim Sim object with all_results
#' @param round_num Integer round number (3=S16, 4=E8, 5=FF, 6=CHAMP)
#' @param team_ids Integer vector of team_ids to compute for
#' @param sample_idx Integer vector of sim indices to use (for speed)
#' @return Named numeric: as.character(team_id) -> win probability
compute_sim_win_probs <- function(sim, round_num, team_ids, sample_idx = NULL) {
  if (is.null(sample_idx)) sample_idx <- seq_len(nrow(sim$all_results))
  results <- sim$all_results[sample_idx, , drop = FALSE]
  n_sims <- nrow(results)
  wp <- setNames(numeric(length(team_ids)), as.character(team_ids))

  for (tid in team_ids) {
    game_col <- game_for_team(tid, round_num)
    if (round_num > 1) {
      feeder_col <- game_for_team(tid, round_num - 1L)
      reached <- results[, feeder_col] == tid
      n_reached <- sum(reached)
      if (n_reached > 0) {
        wp[as.character(tid)] <- sum(results[reached, game_col] == tid) / n_reached
      } else {
        wp[as.character(tid)] <- 0
      }
    } else {
      wp[as.character(tid)] <- sum(results[, game_col] == tid) / n_sims
    }
  }
  wp
}

#' Compute championship equity from sim matrix
#'
#' @param sim Sim object with all_results
#' @param team_ids Integer vector of team_ids
#' @param sample_idx Integer vector of sim indices (for speed)
#' @return Named numeric: as.character(team_id) -> P(championship)
compute_champ_equities <- function(sim, team_ids, sample_idx = NULL) {
  if (is.null(sample_idx)) sample_idx <- seq_len(nrow(sim$all_results))
  results <- sim$all_results[sample_idx, , drop = FALSE]
  n_sims <- nrow(results)
  ce <- setNames(numeric(length(team_ids)), as.character(team_ids))
  for (tid in team_ids) {
    ce[as.character(tid)] <- sum(results[, 63L] == tid) / n_sims
  }
  ce
}

#' Build actual_winners vector from locked sim results
#'
#' For locked games, all sim rows agree. Extract row 1 as the canonical winners.
#'
#' @param sim Sim object with all_results and locked_results
#' @return Integer vector of length 63
build_actual_winners <- function(sim) {
  if (!is.null(sim$locked_results)) {
    aw <- sim$locked_results
  } else {
    aw <- as.integer(sim$all_results[1, ])
  }
  aw
}

#' Simple softmax pick prediction for FF/CHAMP slots
#'
#' @param used_teams Integer vector of team_ids already picked
#' @param candidates Integer vector of candidate team_ids
#' @param win_probs Named numeric: team_id -> win probability
#' @param beta_wp Softmax temperature (default 4.0)
#' @return Named numeric: team_id -> pick probability
predict_entry_pick_simple <- function(used_teams, candidates, win_probs, beta_wp = 4.0) {
  n_cand <- length(candidates)
  if (n_cand == 0) return(numeric(0))

  scores <- numeric(n_cand)
  names(scores) <- as.character(candidates)

  for (i in seq_len(n_cand)) {
    tid <- candidates[i]
    if (tid %in% used_teams) {
      scores[i] <- -Inf
      next
    }
    wp <- win_probs[as.character(tid)]
    if (is.na(wp)) wp <- 0.5
    scores[i] <- beta_wp * wp
  }

  finite_mask <- is.finite(scores)
  if (!any(finite_mask)) {
    probs <- rep(1 / n_cand, n_cand)
    names(probs) <- as.character(candidates)
    return(probs)
  }

  scores[!finite_mask] <- -Inf
  scores <- scores - max(scores[finite_mask])
  raw <- exp(scores)
  probs <- raw / sum(raw)
  names(probs) <- as.character(candidates)
  probs
}

# ==============================================================================
# GROUP ENTRIES BY USED_TEAMS
# ==============================================================================

#' Group field entries by identical RELEVANT used_teams (same future behavior)
#'
#' Key optimization: only used_teams that are still alive in the tournament
#' matter for future picks. A team used in R64 that lost in R32 is irrelevant —
#' it can't be a candidate and doesn't affect path viability. By filtering to
#' only surviving teams before fingerprinting, entries that differ only in
#' which eliminated teams they used get collapsed into the same group.
#'
#' @param field_avail List with used_teams_by_entry and alive_count
#' @param surviving_teams Integer vector of team_ids still alive in the tournament.
#'   If NULL, all used_teams are considered relevant (no filtering).
#' @return data.table with group_id, used_teams (list col), n_entries
group_entries_by_history <- function(field_avail, surviving_teams = NULL) {
  used_list <- field_avail$used_teams_by_entry
  n <- length(used_list)
  if (n == 0) {
    return(data.table(group_id = integer(0),
                      used_teams = list(),
                      n_entries = numeric(0)))
  }

  # Filter used_teams to only surviving teams (if provided)
  if (!is.null(surviving_teams)) {
    used_list_filtered <- lapply(used_list, function(ut) {
      intersect(ut, surviving_teams)
    })
  } else {
    used_list_filtered <- used_list
  }

  fps <- vapply(used_list_filtered, function(ut) {
    paste(sort(ut), collapse = ",")
  }, character(1))

  dt <- data.table(fp = fps, idx = seq_len(n))
  groups <- dt[, .(
    used_teams = list(used_list_filtered[[idx[1]]]),
    n_entries  = .N
  ), by = fp]
  groups[, group_id := .I]
  groups[, fp := NULL]
  setcolorder(groups, c("group_id", "used_teams", "n_entries"))
  groups
}

# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

#' Simulate entry-level field survival across remaining slots
#'
#' Produces per-sim exact counts of surviving opponents using the calibrated
#' entry ownership model. Memory-efficient: avoids materializing the full
#' (n_sims x n_groups) matrix by using the identity A*(B*c) = (A*B)*c.
#'
#' @param field_avail List with used_teams_by_entry and alive_count
#' @param sim Sim object with all_results (n_sims x 63)
#' @param current_slot_id Current slot (e.g. "S16_d1")
#' @param entry_model_params Calibrated params from entry_model_params.rds
#' @param teams_dt Data frame with 64 rows: team_id, name, seed, region, rating
#' @param format Contest format "A", "B", or "C" (default "A")
#' @return List with:
#'   n_alive_matrix: matrix (n_sims x n_remaining_slots)
#'   remaining_slots: character vector
#'   entry_groups: data.table of initial groups
#'   alive_count: total alive field entries
#'   implied_ownership: list of named vectors per slot
simulate_entry_level_field <- function(field_avail, sim, current_slot_id,
                                        entry_model_params, teams_dt,
                                        format = "A") {
  n_all_sims <- nrow(sim$all_results)
  cat(sprintf("  Entry-level field sim: %d entries, %s sims\n",
              field_avail$alive_count, format(n_all_sims, big.mark = ",")))
  start_time <- proc.time()[["elapsed"]]

  sim_results <- sim$all_results
  actual_winners <- build_actual_winners(sim)

  slot_order <- get_slot_order(format)
  current_idx <- match(current_slot_id, slot_order)
  remaining_slots <- slot_order[current_idx:length(slot_order)]
  n_remaining <- length(remaining_slots)

  # Use a sample for computing win probs / champ equity (fast and sufficient)
  wp_sample <- if (n_all_sims > 200000) sample.int(n_all_sims, 200000) else seq_len(n_all_sims)

  # Identify all S16 candidates (both days) for the entry model
  all_s16_candidates <- integer(0)
  for (sid in c("S16_d1", "S16_d2")) {
    slot_def <- tryCatch(get_slot(sid), error = function(e) NULL)
    if (!is.null(slot_def)) {
      for (g in slot_def$game_indices) {
        feeders <- get_feeder_games(g)
        for (fg in feeders) {
          w <- actual_winners[fg]
          if (!is.na(w) && w > 0) all_s16_candidates <- c(all_s16_candidates, w)
        }
      }
    }
  }
  all_s16_candidates <- unique(all_s16_candidates)
  cat(sprintf("  %d teams still alive (S16 candidates)\n", length(all_s16_candidates)))

  # Group entries — only fingerprint on teams still alive in the tournament.
  # Entries that differ only in which eliminated teams they used are functionally
  # identical: same available picks, same path viability, same constraints.
  entry_groups <- group_entries_by_history(field_avail, surviving_teams = all_s16_candidates)
  n_groups <- nrow(entry_groups)
  cat(sprintf("  %d groups from %d entries (grouped by relevant used_teams only)\n",
              n_groups, field_avail$alive_count))

  if (n_groups == 0) {
    return(list(
      n_alive_matrix  = matrix(0, nrow = n_all_sims, ncol = n_remaining),
      remaining_slots = remaining_slots,
      entry_groups    = entry_groups,
      alive_count     = 0L,
      implied_ownership = list()
    ))
  }

  # ---- Forward simulation slot by slot ----
  current_groups <- copy(entry_groups)
  n_alive_matrix <- matrix(0, nrow = n_all_sims, ncol = n_remaining)
  implied_ownership <- list()

  # Track cumulative expected survival per group (scalar, not per-sim)
  # Used for group expansion only. Per-sim counts come from the efficient path.
  cum_expected_survive <- rep(1, n_groups)

  for (si in seq_along(remaining_slots)) {
    slot_start <- proc.time()[["elapsed"]]
    sid <- remaining_slots[si]
    slot_def <- get_slot(sid)
    round_num <- slot_def$round_num
    n_picks <- get_n_picks(sid, format)

    cat(sprintf("  [%d/%d] %s (round %d, %d pick%s): %d groups...",
                si, n_remaining, sid, round_num, n_picks,
                if (n_picks > 1) "s" else "", nrow(current_groups)))
    flush.console()

    # Get candidates that survived to this round
    candidate_ids <- get_surviving_candidates(sid, round_num, sim_results,
                                               wp_sample, teams_dt, actual_winners)

    if (length(candidate_ids) == 0) {
      n_alive_matrix[, si:n_remaining] <- 0
      break
    }

    # Compute win probabilities and championship equity
    wp <- compute_sim_win_probs(sim, round_num, candidate_ids, wp_sample)
    ce <- compute_champ_equities(sim, candidate_ids, wp_sample)

    # No pruning — all candidates matter. Even a team that rarely wins
    # contributes to n_alive in the sims where it does. The is_winner matrix
    # handles per-sim correctness (0 for teams that didn't reach this round).

    # ---- Determine if this is a 2-pick E8 slot (Format A/C combined E8) ----
    is_e8_combined <- (n_picks == 2 && round_num == 4L)

    # Predict picks for each group
    n_cur_groups <- nrow(current_groups)

    if (is_e8_combined) {
      # ========== E8 COMBINED (2-pick) PATH ==========
      # Check if E8 feeder games (S16) are locked
      e8_feeders_locked <- all(vapply(slot_def$game_indices, function(g) {
        fds <- get_feeder_games(g)
        all(vapply(fds, function(fg) {
          !is.na(actual_winners[fg]) && actual_winners[fg] > 0
        }, logical(1)))
      }, logical(1)))

      if (e8_feeders_locked) {
        # ---- E8 LOCKED: use full pair model (candidates known) ----
        pair_data_per_group <- vector("list", n_cur_groups)
        pred_start <- proc.time()[["elapsed"]]
        last_print <- pred_start

        for (gi in seq_len(n_cur_groups)) {
          used <- current_groups$used_teams[[gi]]
          pair_data_per_group[[gi]] <- predict_entry_pick_e8_pairs(
            used_teams = used, e8_candidates = candidate_ids, win_probs = wp,
            champ_equities = ce, actual_winners = actual_winners,
            params = entry_model_params, e8_day_games = NULL
          )
          now <- proc.time()[["elapsed"]]
          if (now - last_print >= 2 || gi == n_cur_groups) {
            cat(sprintf("\r    Predicting E8 pairs: %d/%d groups (%.0f%%)   ",
                        gi, n_cur_groups, 100 * gi / n_cur_groups))
            flush.console()
            last_print <- now
          }
        }
        cat(sprintf("\r    Predicting E8 pairs: %d groups done (%.1fs)          \n",
                    n_cur_groups, proc.time()[["elapsed"]] - pred_start))

        # Implied ownership marginals
        marginals <- setNames(numeric(length(candidate_ids)), as.character(candidate_ids))
        total_entries <- sum(current_groups$n_entries)
        for (gi in seq_len(n_cur_groups)) {
          pd <- pair_data_per_group[[gi]]
          w <- current_groups$n_entries[gi] / total_entries
          for (k in seq_along(pd$pairs)) {
            p1 <- as.character(pd$pairs[[k]][1])
            p2 <- as.character(pd$pairs[[k]][2])
            marginals[p1] <- marginals[p1] + w * pd$probs[k]
            marginals[p2] <- marginals[p2] + w * pd$probs[k]
          }
        }
        names(marginals) <- teams_dt$name[candidate_ids]
        implied_ownership[[sid]] <- marginals

        # n_alive via streaming pair computation (same as before)
        game_cols <- vapply(candidate_ids, function(tid) game_for_team(tid, round_num), integer(1))
        is_winner <- matrix(0L, nrow = n_all_sims, ncol = length(candidate_ids))
        for (c_idx in seq_along(candidate_ids)) {
          is_winner[, c_idx] <- as.integer(
            sim_results[, game_cols[c_idx]] == candidate_ids[c_idx]
          )
        }
        cand_idx_map <- setNames(seq_along(candidate_ids), as.character(candidate_ids))
        expected_win_probs <- wp[as.character(candidate_ids)]
        expected_win_probs[is.na(expected_win_probs)] <- 0

        pair_env <- new.env(hash = TRUE, parent = emptyenv())
        all_unique_pairs <- list()
        n_unique_pairs <- 0L
        for (gi in seq_len(n_cur_groups)) {
          pd <- pair_data_per_group[[gi]]
          for (pk in seq_along(pd$pairs)) {
            key <- paste(sort(pd$pairs[[pk]]), collapse = ",")
            if (is.null(pair_env[[key]])) {
              n_unique_pairs <- n_unique_pairs + 1L
              pair_env[[key]] <- n_unique_pairs
              all_unique_pairs[[n_unique_pairs]] <- pd$pairs[[pk]]
            }
          }
        }

        pair_expected_win <- numeric(n_unique_pairs)
        weighted_pair_totals <- numeric(n_unique_pairs)
        for (gi in seq_len(n_cur_groups)) {
          pd <- pair_data_per_group[[gi]]
          if (length(pd$pairs) == 0) next
          eff_count <- current_groups$n_entries[gi] * cum_expected_survive[gi]
          for (pk in seq_along(pd$pairs)) {
            key <- paste(sort(pd$pairs[[pk]]), collapse = ",")
            pk_idx <- pair_env[[key]]
            weighted_pair_totals[pk_idx] <- weighted_pair_totals[pk_idx] +
              pd$probs[pk] * eff_count
          }
        }
        for (pk in seq_len(n_unique_pairs)) {
          t1 <- all_unique_pairs[[pk]][1]; t2 <- all_unique_pairs[[pk]][2]
          idx1 <- cand_idx_map[as.character(t1)]; idx2 <- cand_idx_map[as.character(t2)]
          pair_expected_win[pk] <- expected_win_probs[idx1] * expected_win_probs[idx2]
        }

        n_alive_vec <- numeric(n_all_sims)
        for (pk in seq_len(n_unique_pairs)) {
          if (abs(weighted_pair_totals[pk]) < 1e-6) next
          t1 <- all_unique_pairs[[pk]][1]; t2 <- all_unique_pairs[[pk]][2]
          idx1 <- cand_idx_map[as.character(t1)]; idx2 <- cand_idx_map[as.character(t2)]
          n_alive_vec <- n_alive_vec + weighted_pair_totals[pk] * is_winner[, idx1] * is_winner[, idx2]
        }
        n_alive_matrix[, si] <- pmax(n_alive_vec, 0)

        for (gi in seq_len(n_cur_groups)) {
          pd <- pair_data_per_group[[gi]]
          if (length(pd$pairs) == 0) next
          p_expected <- 0
          for (pk in seq_along(pd$pairs)) {
            key <- paste(sort(pd$pairs[[pk]]), collapse = ",")
            pk_idx <- pair_env[[key]]
            p_expected <- p_expected + pd$probs[pk] * pair_expected_win[pk_idx]
          }
          cum_expected_survive[gi] <- cum_expected_survive[gi] * p_expected
        }

      } else {
        # ---- E8 VARIABLE: candidates differ per sim ----
        # Decompose into two bracket halves (each half = one pick).
        # P(survive E8) = P(half1 pick wins) × P(half2 pick wins)
        # Process via per-participant-set approach on each half independently,
        # then combine the n_alive contributions.
        cat(" variable E8 (half-bracket decomposition)...")
        flush.console()
        pred_start <- proc.time()[["elapsed"]]

        # E8 games: 57 (East), 58 (South), 59 (West), 60 (Midwest)
        # Half 1: games 57, 58 (regions 1+2). Half 2: games 59, 60 (regions 3+4)
        e8_games <- slot_def$game_indices  # 57:60
        half1_games <- e8_games[1:2]  # 57, 58
        half2_games <- e8_games[3:4]  # 59, 60

        # For each half, compute per-sim n_alive using the variable-candidate approach
        # Then multiply: n_alive_e8 = n_alive_half1 * n_alive_half2 / effective_total
        # (since both halves draw from the same pool of entries)
        # Actually: n_alive_e8[i] = sum_g eff_g * P_half1_g[i] * P_half2_g[i]
        # We can't easily decompose this into independent products.
        # Instead, compute P_survive_e8[i, g] = P_half1_g[i] * P_half2_g[i]
        # Then n_alive[i] = sum_g eff_g * P_survive_e8[i, g]
        # Process in chunks to limit memory.

        effective_counts <- current_groups$n_entries * cum_expected_survive[seq_len(n_cur_groups)]
        beta_wp_e8 <- entry_model_params$beta_wp_E8

        # Get feeder games for each half
        half1_feeders <- sort(unique(unlist(lapply(half1_games, get_feeder_games))))
        half2_feeders <- sort(unique(unlist(lapply(half2_games, get_feeder_games))))

        # Build per-sim participant vectors for each half
        # Half 1 participants = winners of feeder games for half1_games
        h1_parts_mat <- matrix(0L, nrow = n_all_sims, ncol = length(half1_feeders))
        for (fi in seq_along(half1_feeders)) {
          h1_parts_mat[, fi] <- sim_results[, half1_feeders[fi]]
        }
        h2_parts_mat <- matrix(0L, nrow = n_all_sims, ncol = length(half2_feeders))
        for (fi in seq_along(half2_feeders)) {
          h2_parts_mat[, fi] <- sim_results[, half2_feeders[fi]]
        }

        # Find unique participant sets for each half
        h1_keys <- apply(h1_parts_mat, 1, paste, collapse = ",")
        h2_keys <- apply(h2_parts_mat, 1, paste, collapse = ",")
        combo_keys <- paste(h1_keys, h2_keys, sep = "|")
        unique_combos <- unique(combo_keys)
        n_combos <- length(unique_combos)
        cat(sprintf(" %d unique E8 configs...", n_combos))
        flush.console()

        # For implied ownership
        implied_own_accum <- setNames(numeric(length(candidate_ids)), as.character(candidate_ids))
        implied_own_total_sims <- 0L

        # Accumulate cum_survive updates
        cum_survive_accum <- numeric(n_cur_groups)
        cum_survive_weight <- numeric(n_cur_groups)

        # Pre-compute blocked matrix for all candidates once (used_teams membership)
        # blocked_map[team_id] -> logical vector of length n_cur_groups
        all_e8_teams <- unique(c(as.integer(h1_parts_mat), as.integer(h2_parts_mat)))
        all_e8_teams <- all_e8_teams[all_e8_teams > 0]
        blocked_map <- list()
        for (tid in all_e8_teams) {
          blocked_map[[as.character(tid)]] <- vapply(
            current_groups$used_teams, function(ut) tid %in% ut, logical(1)
          )
        }
        total_eff <- sum(effective_counts)

        for (ci in seq_len(n_combos)) {
          sim_mask <- which(combo_keys == unique_combos[ci])
          n_sims_combo <- length(sim_mask)
          parts_str <- strsplit(unique_combos[ci], "\\|")[[1]]
          h1_parts <- as.integer(strsplit(parts_str[1], ",")[[1]])
          h2_parts <- as.integer(strsplit(parts_str[2], ",")[[1]])

          wp_h1 <- wp[as.character(h1_parts)]; wp_h1[is.na(wp_h1)] <- 0.3
          wp_h2 <- wp[as.character(h2_parts)]; wp_h2[is.na(wp_h2)] <- 0.3

          # Vectorized softmax for half 1: build scores matrix (n_groups x n_h1)
          n_h1 <- length(h1_parts)
          scores_h1 <- matrix(beta_wp_e8 * wp_h1, nrow = n_cur_groups, ncol = n_h1, byrow = TRUE)
          for (j in seq_len(n_h1)) {
            scores_h1[blocked_map[[as.character(h1_parts[j])]], j] <- -Inf
          }
          max_h1 <- apply(scores_h1, 1, function(x) { f <- x[is.finite(x)]; if (length(f)) max(f) else 0 })
          exp_h1 <- exp(scores_h1 - max_h1)
          exp_h1[!is.finite(scores_h1)] <- 0
          pp_h1 <- exp_h1 / pmax(rowSums(exp_h1), 1e-10)

          # Vectorized softmax for half 2
          n_h2 <- length(h2_parts)
          scores_h2 <- matrix(beta_wp_e8 * wp_h2, nrow = n_cur_groups, ncol = n_h2, byrow = TRUE)
          for (j in seq_len(n_h2)) {
            scores_h2[blocked_map[[as.character(h2_parts[j])]], j] <- -Inf
          }
          max_h2 <- apply(scores_h2, 1, function(x) { f <- x[is.finite(x)]; if (length(f)) max(f) else 0 })
          exp_h2 <- exp(scores_h2 - max_h2)
          exp_h2[!is.finite(scores_h2)] <- 0
          pp_h2 <- exp_h2 / pmax(rowSums(exp_h2), 1e-10)

          # Weighted probs via matrix multiply
          w_probs_h1 <- as.numeric(crossprod(pp_h1, effective_counts))
          w_probs_h2 <- as.numeric(crossprod(pp_h2, effective_counts))

          # Accumulate implied ownership for this config
          total_eff_sum <- sum(effective_counts)
          for (j in seq_len(n_h1)) {
            tid_str <- as.character(h1_parts[j])
            if (tid_str %in% names(implied_own_accum)) {
              implied_own_accum[tid_str] <- implied_own_accum[tid_str] +
                sum(effective_counts * pp_h1[, j]) / total_eff_sum * n_sims_combo
            }
          }
          for (j in seq_len(n_h2)) {
            tid_str <- as.character(h2_parts[j])
            if (tid_str %in% names(implied_own_accum)) {
              implied_own_accum[tid_str] <- implied_own_accum[tid_str] +
                sum(effective_counts * pp_h2[, j]) / total_eff_sum * n_sims_combo
            }
          }
          implied_own_total_sims <- implied_own_total_sims + n_sims_combo

          # cum_survive: P(survive E8) = P(h1 pick wins) * P(h2 pick wins)
          p_surv_h1 <- as.numeric(pp_h1 %*% wp_h1)  # per-group expected survival for half1
          p_surv_h2 <- as.numeric(pp_h2 %*% wp_h2)  # per-group expected survival for half2
          cum_survive_accum <- cum_survive_accum + p_surv_h1 * p_surv_h2 * n_sims_combo
          cum_survive_weight <- cum_survive_weight + n_sims_combo

          # n_alive for this combo's sims
          game_cols_h1 <- vapply(h1_parts, function(t) game_for_team(t, round_num), integer(1))
          game_cols_h2 <- vapply(h2_parts, function(t) game_for_team(t, round_num), integer(1))
          n_alive_h1 <- numeric(n_sims_combo)
          n_alive_h2 <- numeric(n_sims_combo)
          for (j in seq_len(n_h1)) {
            wins <- as.numeric(sim_results[sim_mask, game_cols_h1[j]] == h1_parts[j])
            n_alive_h1 <- n_alive_h1 + w_probs_h1[j] * wins
          }
          for (j in seq_len(n_h2)) {
            wins <- as.numeric(sim_results[sim_mask, game_cols_h2[j]] == h2_parts[j])
            n_alive_h2 <- n_alive_h2 + w_probs_h2[j] * wins
          }
          n_alive_matrix[sim_mask, si] <- n_alive_h1 * n_alive_h2 / pmax(total_eff, 1)

          if (ci %% 50 == 0 || ci == n_combos) {
            cat(sprintf("\r    E8 variable configs: %d/%d (%.0f%%)   ",
                        ci, n_combos, 100 * ci / n_combos))
            flush.console()
          }
        }

        # Update cum_expected_survive
        for (gi in seq_len(n_cur_groups)) {
          avg_p_surv <- cum_survive_accum[gi] / pmax(cum_survive_weight[gi], 1)
          cum_expected_survive[gi] <- cum_expected_survive[gi] * avg_p_surv
        }

        # Normalize implied ownership by total sims
        implied_own_accum <- implied_own_accum / pmax(implied_own_total_sims, 1)
        names(implied_own_accum) <- teams_dt$name[candidate_ids]
        implied_ownership[[sid]] <- implied_own_accum

        cat(sprintf("\r    E8 variable: %d configs done (%.1fs)          \n",
                    n_combos, proc.time()[["elapsed"]] - pred_start))
      }

    } else {
      # ========== SINGLE-PICK PATH (S16, E8_d1/d2, FF, CHAMP) ==========

      # Check if all feeder games are locked (candidates fixed across sims)
      feeders_locked <- all(vapply(slot_def$game_indices, function(g) {
        fds <- get_feeder_games(g)
        all(vapply(fds, function(fg) {
          !is.na(actual_winners[fg]) && actual_winners[fg] > 0
        }, logical(1)))
      }, logical(1)))

      if (feeders_locked) {
        # ---- LOCKED PATH: candidates same in every sim (S16) ----
        pick_probs_matrix <- matrix(0, nrow = n_cur_groups, ncol = length(candidate_ids))
        colnames(pick_probs_matrix) <- as.character(candidate_ids)
        pred_start <- proc.time()[["elapsed"]]
        last_print <- pred_start

        for (gi in seq_len(n_cur_groups)) {
          used <- current_groups$used_teams[[gi]]

          if (round_num == 3L) {
            probs <- predict_entry_pick_s16(
              used_teams = used, day_candidates = candidate_ids,
              all_s16_candidates = all_s16_candidates, win_probs = wp,
              champ_equities = ce, actual_winners = actual_winners,
              params = entry_model_params
            )
          } else if (round_num == 4L) {
            probs <- predict_entry_pick_simple(used, candidate_ids, wp,
                                                beta_wp = entry_model_params$beta_wp_E8)
          } else {
            probs <- predict_entry_pick_simple(used, candidate_ids, wp)
          }

          for (j in seq_along(candidate_ids)) {
            cid_str <- as.character(candidate_ids[j])
            if (cid_str %in% names(probs)) {
              pick_probs_matrix[gi, j] <- probs[cid_str]
            }
          }

          now <- proc.time()[["elapsed"]]
          if (now - last_print >= 2 || gi == n_cur_groups) {
            cat(sprintf("\r    Predicting picks: %d/%d groups (%.0f%%)   ",
                        gi, n_cur_groups, 100 * gi / n_cur_groups))
            flush.console()
            last_print <- now
          }
        }
        cat(sprintf("\r    Predicting picks: %d groups done (%.1fs)          \n",
                    n_cur_groups, proc.time()[["elapsed"]] - pred_start))

        # Implied ownership
        weights <- current_groups$n_entries / sum(current_groups$n_entries)
        avg_own <- as.numeric(crossprod(weights, pick_probs_matrix))
        names(avg_own) <- teams_dt$name[candidate_ids]
        implied_ownership[[sid]] <- avg_own

        # Per-sim n_alive via efficient matrix identity
        effective_counts <- current_groups$n_entries * cum_expected_survive[seq_len(n_cur_groups)]
        weighted_probs <- as.numeric(crossprod(pick_probs_matrix, effective_counts))

        game_cols <- vapply(candidate_ids, function(tid) game_for_team(tid, round_num), integer(1))
        is_winner <- matrix(0, nrow = n_all_sims, ncol = length(candidate_ids))
        for (c_idx in seq_along(candidate_ids)) {
          is_winner[, c_idx] <- as.numeric(
            sim_results[, game_cols[c_idx]] == candidate_ids[c_idx]
          )
        }
        n_alive_matrix[, si] <- as.numeric(is_winner %*% weighted_probs)

        # Update cum_expected_survive
        expected_win_probs <- wp[as.character(candidate_ids)]
        expected_win_probs[is.na(expected_win_probs)] <- 0
        for (gi in seq_len(n_cur_groups)) {
          p_survive_expected <- sum(pick_probs_matrix[gi, ] * expected_win_probs)
          cum_expected_survive[gi] <- cum_expected_survive[gi] * p_survive_expected
        }

      } else {
        # ---- VARIABLE PATH: candidates differ per sim (E8+, FF, CHAMP) ----
        # Enumerate unique participant sets, pre-compute picks for each, batch-assign.
        cat(" variable candidates...")
        flush.console()
        pred_start <- proc.time()[["elapsed"]]

        # Determine per-sim participants from feeder game winners
        feeder_games_flat <- integer(0)
        for (g in slot_def$game_indices) {
          feeder_games_flat <- c(feeder_games_flat, get_feeder_games(g))
        }
        feeder_games_flat <- sort(unique(feeder_games_flat))

        # Build per-sim participant matrix: each row = set of feeder game winners
        n_feeders <- length(feeder_games_flat)
        participant_matrix <- matrix(0L, nrow = n_all_sims, ncol = n_feeders)
        for (fi in seq_len(n_feeders)) {
          participant_matrix[, fi] <- sim_results[, feeder_games_flat[fi]]
        }

        # Find unique participant sets
        pset_keys <- apply(participant_matrix, 1, paste, collapse = ",")
        unique_keys <- unique(pset_keys)
        n_psets <- length(unique_keys)
        cat(sprintf(" %d unique sets...", n_psets))
        flush.console()

        # Determine beta_wp for this round
        beta_wp_round <- if (round_num == 4L) entry_model_params$beta_wp_E8 else 4.0

        # Effective counts (shared across participant sets)
        effective_counts <- current_groups$n_entries * cum_expected_survive[seq_len(n_cur_groups)]

        # For implied ownership: accumulate across participant sets
        implied_own_accum <- setNames(numeric(length(candidate_ids)), as.character(candidate_ids))
        implied_own_total_sims <- 0L

        # For cum_expected_survive update: accumulate P(survive) per group
        cum_survive_accum <- numeric(n_cur_groups)
        cum_survive_weight <- numeric(n_cur_groups)

        # Pre-compute blocked vectors for all possible participants
        all_part_teams <- unique(as.integer(participant_matrix))
        all_part_teams <- all_part_teams[all_part_teams > 0]
        blocked_map <- list()
        for (tid in all_part_teams) {
          blocked_map[[as.character(tid)]] <- vapply(
            current_groups$used_teams, function(ut) tid %in% ut, logical(1)
          )
        }

        for (psi in seq_len(n_psets)) {
          sim_mask <- which(pset_keys == unique_keys[psi])
          n_sims_pset <- length(sim_mask)
          parts <- as.integer(strsplit(unique_keys[psi], ",")[[1]])

          wp_parts <- wp[as.character(parts)]
          wp_parts[is.na(wp_parts)] <- 0.3

          # Vectorized softmax: build scores matrix (n_groups x n_parts)
          n_parts <- length(parts)
          scores_mat <- matrix(beta_wp_round * wp_parts,
                               nrow = n_cur_groups, ncol = n_parts, byrow = TRUE)
          for (j in seq_len(n_parts)) {
            bvec <- blocked_map[[as.character(parts[j])]]
            if (!is.null(bvec)) scores_mat[bvec, j] <- -Inf
          }
          max_s <- apply(scores_mat, 1, function(x) {
            f <- x[is.finite(x)]; if (length(f)) max(f) else 0
          })
          exp_s <- exp(scores_mat - max_s)
          exp_s[!is.finite(scores_mat)] <- 0
          pp_matrix <- exp_s / pmax(rowSums(exp_s), 1e-10)

          # Weighted probs via matrix multiply
          w_probs <- as.numeric(crossprod(pp_matrix, effective_counts))

          # n_alive for sims in this participant set
          game_cols_parts <- vapply(parts, function(tid) game_for_team(tid, round_num), integer(1))
          for (j in seq_len(n_parts)) {
            wins_j <- as.numeric(sim_results[sim_mask, game_cols_parts[j]] == parts[j])
            n_alive_matrix[sim_mask, si] <- n_alive_matrix[sim_mask, si] + w_probs[j] * wins_j
          }

          # Accumulate implied ownership
          for (j in seq_len(n_parts)) {
            cid_str <- as.character(parts[j])
            if (cid_str %in% names(implied_own_accum)) {
              own_j <- sum(effective_counts * pp_matrix[, j]) / sum(effective_counts)
              implied_own_accum[cid_str] <- implied_own_accum[cid_str] + own_j * n_sims_pset
            }
          }
          implied_own_total_sims <- implied_own_total_sims + n_sims_pset

          # Accumulate cum_survive per group (vectorized)
          p_surv_gi <- as.numeric(pp_matrix %*% wp_parts)
          cum_survive_accum <- cum_survive_accum + p_surv_gi * n_sims_pset
          cum_survive_weight <- cum_survive_weight + n_sims_pset

          if (psi %% 50 == 0 || psi == n_psets) {
            cat(sprintf("\r    Variable candidates: %d/%d sets (%.0f%%)   ",
                        psi, n_psets, 100 * psi / n_psets))
            flush.console()
          }
        }

        # Finalize implied ownership
        implied_own_accum <- implied_own_accum / pmax(implied_own_total_sims, 1)
        names(implied_own_accum) <- teams_dt$name[candidate_ids]
        implied_ownership[[sid]] <- implied_own_accum

        # Update cum_expected_survive (weighted average across participant sets)
        for (gi in seq_len(n_cur_groups)) {
          avg_p_surv <- cum_survive_accum[gi] / pmax(cum_survive_weight[gi], 1)
          cum_expected_survive[gi] <- cum_expected_survive[gi] * avg_p_surv
        }

        cat(sprintf("\r    Variable candidates: %d sets done (%.1fs)          \n",
                    n_psets, proc.time()[["elapsed"]] - pred_start))
      }
    }  # end single-pick vs E8-combined

    # ---- Ensure pick_probs_matrix exists for expansion ----
    # Variable-candidate paths don't set pick_probs_matrix, so build it here
    # using average probabilities over the full candidate set (vectorized).
    if (!exists("pick_probs_matrix", inherits = FALSE) ||
        nrow(pick_probs_matrix) != n_cur_groups ||
        ncol(pick_probs_matrix) != length(candidate_ids)) {
      n_cand <- length(candidate_ids)
      beta_wp_exp <- if (round_num == 4L) entry_model_params$beta_wp_E8
                     else if (round_num == 3L) entry_model_params$beta_wp_S16
                     else 4.0
      wp_vals <- wp[as.character(candidate_ids)]
      wp_vals[is.na(wp_vals)] <- 0.3

      blocked <- matrix(FALSE, nrow = n_cur_groups, ncol = n_cand)
      for (j in seq_len(n_cand)) {
        blocked[, j] <- vapply(current_groups$used_teams,
                               function(ut) candidate_ids[j] %in% ut, logical(1))
      }
      scores_mat <- matrix(beta_wp_exp * wp_vals,
                           nrow = n_cur_groups, ncol = n_cand, byrow = TRUE)
      scores_mat[blocked] <- -Inf
      max_s <- apply(scores_mat, 1, function(x) {
        fin <- x[is.finite(x)]
        if (length(fin) > 0) max(fin) else 0
      })
      exp_s <- exp(scores_mat - max_s)
      exp_s[!is.finite(scores_mat)] <- 0
      pick_probs_matrix <- exp_s / pmax(rowSums(exp_s), 1e-10)
      colnames(pick_probs_matrix) <- as.character(candidate_ids)
    }

    # ---- Expand groups for next slot ----
    if (si < n_remaining) {
      # Pre-allocate to avoid O(n^2) vector growth.
      # Upper bound: n_groups × n_candidates (single-pick) or n_groups × n_pairs (E8)
      max_new <- if (is_e8_combined) n_cur_groups * 120L else n_cur_groups * length(candidate_ids)
      new_used_list <- vector("list", max_new)
      new_weight_vec <- numeric(max_new)
      new_cum_vec <- numeric(max_new)
      k <- 0L

      if (is_e8_combined && exists("pair_data_per_group", inherits = FALSE)) {
        # E8 combined LOCKED: expand by PAIR (both teams added to used_teams)
        expected_win_probs <- wp[as.character(candidate_ids)]
        expected_win_probs[is.na(expected_win_probs)] <- 0
        cand_idx_map <- setNames(seq_along(candidate_ids), as.character(candidate_ids))

        for (gi in seq_len(n_cur_groups)) {
          pd <- pair_data_per_group[[gi]]
          p_survive_total <- 0
          for (pk in seq_along(pd$pairs)) {
            idx1 <- cand_idx_map[as.character(pd$pairs[[pk]][1])]
            idx2 <- cand_idx_map[as.character(pd$pairs[[pk]][2])]
            p_survive_total <- p_survive_total +
              pd$probs[pk] * expected_win_probs[idx1] * expected_win_probs[idx2]
          }

          for (pk in seq_along(pd$pairs)) {
            if (pd$probs[pk] < 0.001) next

            t1 <- pd$pairs[[pk]][1]
            t2 <- pd$pairs[[pk]][2]
            idx1 <- cand_idx_map[as.character(t1)]
            idx2 <- cand_idx_map[as.character(t2)]
            p_pair_wins <- expected_win_probs[idx1] * expected_win_probs[idx2]

            k <- k + 1L
            new_used_list[[k]] <- c(current_groups$used_teams[[gi]], t1, t2)
            new_weight_vec[k] <- current_groups$n_entries[gi] * pd$probs[pk]
            new_cum_vec[k] <- cum_expected_survive[gi] / max(p_survive_total, 1e-10) * p_pair_wins
          }
        }
      } else if (is_e8_combined) {
        # E8 combined VARIABLE: expand by per-half picks (no pair_data available).
        # Use average marginal probabilities per half-bracket.
        # Each group gets 2 teams added to used_teams (one from each half).
        expected_win_probs <- wp[as.character(candidate_ids)]
        expected_win_probs[is.na(expected_win_probs)] <- 0
        beta_wp_e8_exp <- entry_model_params$beta_wp_E8

        e8_games_exp <- slot_def$game_indices  # 57:60
        half1_cands <- integer(0)
        half2_cands <- integer(0)
        for (g_idx in e8_games_exp[1:2]) {
          fds <- get_feeder_games(g_idx)
          for (fg in fds) {
            w <- actual_winners[fg]
            if (!is.na(w) && w > 0) half1_cands <- c(half1_cands, w)
            else half1_cands <- c(half1_cands, sim_results[1, fg])
          }
        }
        for (g_idx in e8_games_exp[3:4]) {
          fds <- get_feeder_games(g_idx)
          for (fg in fds) {
            w <- actual_winners[fg]
            if (!is.na(w) && w > 0) half2_cands <- c(half2_cands, w)
            else half2_cands <- c(half2_cands, sim_results[1, fg])
          }
        }
        half1_cands <- unique(half1_cands)
        half2_cands <- unique(half2_cands)

        wp_h1 <- wp[as.character(half1_cands)]; wp_h1[is.na(wp_h1)] <- 0.3
        wp_h2 <- wp[as.character(half2_cands)]; wp_h2[is.na(wp_h2)] <- 0.3

        max_new <- n_cur_groups * length(half1_cands) * length(half2_cands)
        new_used_list <- vector("list", max_new)
        new_weight_vec <- numeric(max_new)
        new_cum_vec <- numeric(max_new)
        k <- 0L

        for (gi in seq_len(n_cur_groups)) {
          used <- current_groups$used_teams[[gi]]
          probs_h1 <- predict_entry_pick_simple(used, half1_cands, wp_h1, beta_wp = beta_wp_e8_exp)
          probs_h2 <- predict_entry_pick_simple(used, half2_cands, wp_h2, beta_wp = beta_wp_e8_exp)
          p_surv_h1 <- sum(probs_h1 * wp_h1[as.character(half1_cands)])
          p_surv_h2 <- sum(probs_h2 * wp_h2[as.character(half2_cands)])
          p_survive_total <- p_surv_h1 * p_surv_h2

          for (ci1 in seq_along(half1_cands)) {
            p1 <- probs_h1[as.character(half1_cands[ci1])]
            if (p1 < 0.001) next
            for (ci2 in seq_along(half2_cands)) {
              p2 <- probs_h2[as.character(half2_cands[ci2])]
              if (p2 < 0.001) next

              pair_prob <- p1 * p2
              t1 <- half1_cands[ci1]
              t2 <- half2_cands[ci2]
              p_pair_wins <- wp_h1[as.character(t1)] * wp_h2[as.character(t2)]

              k <- k + 1L
              new_used_list[[k]] <- c(used, t1, t2)
              new_weight_vec[k] <- current_groups$n_entries[gi] * pair_prob
              new_cum_vec[k] <- cum_expected_survive[gi] / max(p_survive_total, 1e-10) * p_pair_wins
            }
          }
        }

      } else {
        # Single-pick expansion
        expected_win_probs <- wp[as.character(candidate_ids)]
        expected_win_probs[is.na(expected_win_probs)] <- 0

        for (gi in seq_len(n_cur_groups)) {
          p_survive_total <- sum(pick_probs_matrix[gi, ] * expected_win_probs)
          for (ci in seq_along(candidate_ids)) {
            prob <- pick_probs_matrix[gi, ci]
            if (prob < 0.001) next

            tid <- candidate_ids[ci]
            p_tid_wins <- expected_win_probs[ci]

            k <- k + 1L
            new_used_list[[k]] <- c(current_groups$used_teams[[gi]], tid)
            new_weight_vec[k] <- current_groups$n_entries[gi] * prob
            new_cum_vec[k] <- cum_expected_survive[gi] / max(p_survive_total, 1e-10) * p_tid_wins
          }
        }
      }

      if (k > 0) {
        # Trim to actual size
        new_used_list <- new_used_list[1:k]
        new_weight_vec <- new_weight_vec[1:k]
        new_cum_vec <- new_cum_vec[1:k]

        expanded_dt <- data.table(
          used_teams = new_used_list,
          n_entries  = new_weight_vec,
          cum_surv   = new_cum_vec
        )

        # Collapse groups with identical used_teams
        expanded_dt[, fingerprint := vapply(used_teams, function(ut) {
          paste(sort(ut), collapse = ",")
        }, character(1))]

        collapsed <- expanded_dt[, .(
          used_teams = used_teams[1],
          n_entries  = sum(n_entries),
          cum_surv   = sum(n_entries * cum_surv) / sum(n_entries)  # weighted average
        ), by = fingerprint]

        current_groups <- data.table(
          group_id   = seq_len(nrow(collapsed)),
          used_teams = collapsed$used_teams,
          n_entries  = collapsed$n_entries
        )
        cum_expected_survive <- collapsed$cum_surv
      }

      slot_elapsed <- proc.time()[["elapsed"]] - slot_start
      cat(sprintf(" -> %d sub-groups, %.0f alive avg (%.1fs)\n",
                  nrow(current_groups), mean(n_alive_matrix[, si]), slot_elapsed))
    } else {
      slot_elapsed <- proc.time()[["elapsed"]] - slot_start
      cat(sprintf(" %.0f alive avg (%.1fs)\n",
                  mean(n_alive_matrix[, si]), slot_elapsed))
    }
  }

  elapsed <- proc.time()[["elapsed"]] - start_time
  cat(sprintf("  Entry-level field sim complete: %.1fs\n", elapsed))

  list(
    n_alive_matrix    = n_alive_matrix,
    remaining_slots   = remaining_slots,
    entry_groups      = entry_groups,
    alive_count       = field_avail$alive_count,
    implied_ownership = implied_ownership
  )
}


#' Get candidate team_ids that could participate in a given round
#'
#' For locked rounds (feeder games already played), returns the actual winners.
#' For future rounds (feeder games not locked), returns ALL teams that could
#' potentially reach this round — i.e., all teams from the feeder bracket region.
#' This is critical because different sims produce different participants.
#' The is_winner matrix handles per-sim correctness: teams that didn't actually
#' reach this round in a given sim get is_winner = 0 automatically.
#'
#' @param slot_id Slot ID
#' @param round_num Round number
#' @param sim_results Full sim matrix
#' @param sample_idx Sim indices
#' @param teams_dt Teams data frame
#' @param actual_winners Locked results vector
#' @return Integer vector of team_ids that could play in this round
get_surviving_candidates <- function(slot_id, round_num, sim_results,
                                      sample_idx, teams_dt, actual_winners) {
  slot_def <- get_slot(slot_id)
  game_indices <- slot_def$game_indices

  if (round_num <= 1) {
    team_ids <- integer(0)
    for (g in game_indices) {
      team_ids <- c(team_ids, teams_dt$team_id[2 * g - 1], teams_dt$team_id[2 * g])
    }
    return(unique(team_ids))
  }

  candidates <- integer(0)
  for (g in game_indices) {
    feeders <- get_feeder_games(g)
    for (fg in feeders) {
      w <- actual_winners[fg]
      if (!is.na(w) && w > 0) {
        # Locked game: we know exactly who won
        candidates <- c(candidates, w)
      } else {
        # Unlocked game: include ALL teams that could reach this game.
        # Recurse to find all possible participants by tracing feeder chain
        # back to the last locked level.
        possible <- get_all_possible_winners(fg, actual_winners, teams_dt)
        candidates <- c(candidates, possible)
      }
    }
  }
  unique(candidates)
}

#' Recursively find all teams that could win an unlocked game
#'
#' Traces the feeder chain back until we hit locked games, then collects
#' all possible winners.
#'
#' @param game_idx Game index (1-63)
#' @param actual_winners Locked results vector
#' @param teams_dt Teams data frame
#' @return Integer vector of team_ids that could win this game
get_all_possible_winners <- function(game_idx, actual_winners, teams_dt) {
  # Base case: R64 game (always "locked" in the sense that participants are known)
  if (game_idx <= 32L) {
    return(c(teams_dt$team_id[2 * game_idx - 1], teams_dt$team_id[2 * game_idx]))
  }

  # Check if this game is locked
  w <- actual_winners[game_idx]
  if (!is.na(w) && w > 0) {
    return(w)
  }

  # Unlocked: recurse into feeder games
  feeders <- get_feeder_games(game_idx)
  possible <- integer(0)
  for (fg in feeders) {
    possible <- c(possible, get_all_possible_winners(fg, actual_winners, teams_dt))
  }
  unique(possible)
}
