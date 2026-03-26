#!/usr/bin/env Rscript
# ==============================================================================
# splash_optimizer.R
# Day-by-day decision engine for Splash NCAA survivor portfolio optimization.
#
# Core idea: for today's slot, evaluate each candidate team by simulating
# the full contest outcome (not just survival probability). The winner is
# whoever lasts longest — many contests end with everyone eliminated.
#
# Run this script each day of the tournament to get allocation recommendations.
#
# Usage:
#   source("splash_config.R")
#   source("splash_state.R")
#   source("splash_ownership.R")
#   source("splash_optimizer.R")
# ==============================================================================

library(data.table)

# ==============================================================================
# PROGRESS TRACKING UTILITIES
# ==============================================================================

#' Create a progress tracker that prints updates at fixed intervals
#'
#' @param total Total number of items
#' @param label Short description of what's being processed
#' @param update_every Minimum seconds between progress prints (default 2)
#' @return List with tick() and done() methods
make_progress <- function(total, label = "Processing", update_every = 2) {
  env <- new.env(parent = emptyenv())
  env$total <- total
  env$current <- 0L
  env$label <- label
  env$start_time <- proc.time()[["elapsed"]]
  env$last_print <- env$start_time
  env$update_every <- update_every

  tick <- function(n = 1L) {
    env$current <- env$current + n
    now <- proc.time()[["elapsed"]]
    elapsed <- now - env$start_time
    since_print <- now - env$last_print

    if (since_print >= env$update_every || env$current == env$total) {
      pct <- env$current / env$total * 100
      rate <- env$current / max(elapsed, 0.001)
      remaining <- (env$total - env$current) / max(rate, 0.001)

      # Format ETA
      if (remaining < 60) {
        eta <- sprintf("%.0fs", remaining)
      } else if (remaining < 3600) {
        eta <- sprintf("%.1fm", remaining / 60)
      } else {
        eta <- sprintf("%.1fh", remaining / 3600)
      }

      # Bar: 30 chars wide
      filled <- round(pct / 100 * 30)
      bar <- paste0("[", strrep("=", filled),
                    ifelse(filled < 30, ">", ""),
                    strrep(" ", max(0, 30 - filled - 1)), "]")

      cat(sprintf("\r  %s %s %5.1f%% (%s/%s) ETA %s   ",
                  env$label, bar, pct,
                  format(env$current, big.mark = ","),
                  format(env$total, big.mark = ","), eta))
      if (env$current == env$total) cat("\n")
      flush.console()
      env$last_print <- now
    }
  }

  done <- function() {
    elapsed <- proc.time()[["elapsed"]] - env$start_time
    if (elapsed < 60) {
      time_str <- sprintf("%.1fs", elapsed)
    } else if (elapsed < 3600) {
      time_str <- sprintf("%.1fm", elapsed / 60)
    } else {
      time_str <- sprintf("%.1fh", elapsed / 3600)
    }
    cat(sprintf("\r  %s: done (%s/%s in %s)%s\n",
                env$label,
                format(env$total, big.mark = ","),
                format(env$total, big.mark = ","),
                time_str,
                strrep(" ", 30)))
    flush.console()
  }

  list(tick = tick, done = done)
}

# ==============================================================================
# STEP 1: PRECOMPUTE TEAM-ROUND WIN MATRICES
# (Reuses pattern from hodes_pathing.R:60-78)
# ==============================================================================

#' Precompute team-round win matrices from sim results
#'
#' @param sim List with: all_results (n_sims x 63), teams, round_info, n_sims
#' @return List with:
#'   team_round_wins: list of 6 logical matrices (n_sims x n_teams)
#'   team_round_probs: matrix (n_teams x 6) of marginal win probabilities
precompute_team_wins <- function(sim) {
  ar <- sim$all_results
  ri <- sim$round_info
  n_sims <- sim$n_sims
  n_teams <- nrow(sim$teams)

  round_cols <- list(
    R1 = which(ri$round_num == 1),
    R2 = which(ri$round_num == 2),
    R3 = which(ri$round_num == 3),
    R4 = which(ri$round_num == 4),
    R5 = which(ri$round_num == 5),
    R6 = which(ri$round_num == 6)
  )

  team_round_wins <- vector("list", 6)
  team_round_probs <- matrix(0, nrow = n_teams, ncol = 6)

  cat("Precomputing team-round win matrices...\n")
  pg_tw <- make_progress(6, "Rounds", update_every = 1)
  for (rd in 1:6) {
    cols <- round_cols[[rd]]
    round_results <- ar[, cols, drop = FALSE]

    win_mat <- matrix(FALSE, nrow = n_sims, ncol = n_teams)
    for (j in seq_len(ncol(round_results))) {
      idx <- cbind(seq_len(n_sims), round_results[, j])
      win_mat[idx] <- TRUE
    }

    team_round_wins[[rd]] <- win_mat
    team_round_probs[, rd] <- colMeans(win_mat)
    pg_tw$tick()
  }
  pg_tw$done()

  list(
    team_round_wins = team_round_wins,
    team_round_probs = team_round_probs,
    round_cols = round_cols
  )
}

# ==============================================================================
# STEP 1b: FORWARD SIMULATION (for portfolio-level EV)
#
# Simple greedy forward simulation of a single entry. Used by
# evaluate_portfolio_ev() to compute death_round distributions for
# allocated picks. NOT used for candidate scoring (beam search handles that).
# ==============================================================================

#' Forward simulate an entry's survival across all remaining slots
#'
#' @param candidate_id Integer team_id for today's pick
#' @param used_teams Integer vector of team_ids already used
#' @param current_slot_id Current slot
#' @param sim Sim results
#' @param tw Precomputed team wins
#' @param teams_dt Teams data frame
#' @param slot_order Slot order for this format
#' @param format Contest format ("A", "B", or "C")
#' @return List with death_round (integer vector, 0 = survived all)
forward_simulate_entry <- function(candidate_id, used_teams, current_slot_id,
                                    sim, tw, teams_dt,
                                    slot_order = SLOT_ORDER, format = "A") {
  n_sims <- sim$n_sims
  alive <- rep(TRUE, n_sims)
  death_round <- rep(0L, n_sims)

  current_slot_idx <- match(current_slot_id, slot_order)
  all_used <- c(used_teams, candidate_id)

  # Check today's pick
  slot <- get_slot(current_slot_id)
  round_num <- slot$round_num
  today_wins <- tw$team_round_wins[[round_num]][, candidate_id]
  died_today <- alive & !today_wins
  death_round[died_today] <- round_num
  alive <- alive & today_wins

  # Future slots: greedily assign best available team
  for (s in (current_slot_idx + 1):length(slot_order)) {
    if (s > length(slot_order)) break

    sid <- slot_order[s]
    future_slot <- get_slot(sid)
    future_round <- future_slot$round_num
    n_future_picks <- get_n_picks(sid, format)

    slot_team_ids <- get_teams_in_slot(sid, teams_dt)
    slot_team_ids <- setdiff(slot_team_ids, all_used)

    if (length(slot_team_ids) == 0) {
      died_here <- alive
      death_round[died_here] <- future_round
      alive <- rep(FALSE, n_sims)
      break
    }

    # Score by P(win this round) - P(win next round) for future value
    slot_win_probs <- tw$team_round_probs[slot_team_ids, future_round]
    if (future_round < 6) {
      next_round_probs <- tw$team_round_probs[slot_team_ids, future_round + 1]
      score <- slot_win_probs - next_round_probs
    } else {
      score <- slot_win_probs
    }

    n_to_pick <- min(n_future_picks, length(slot_team_ids))
    best_local_idx <- order(score, decreasing = TRUE)[1:n_to_pick]
    best_ids <- slot_team_ids[best_local_idx]
    all_used <- c(all_used, best_ids)

    for (tid in best_ids) {
      round_wins <- tw$team_round_wins[[future_round]][, tid]
      died_here <- alive & !round_wins
      death_round[died_here] <- future_round
      alive <- alive & round_wins
    }
  }

  list(death_round = death_round)
}

# ==============================================================================
# STEP 2: PRECOMPUTE GROUP CONTEXT
# ==============================================================================

#' Precompute all group-level quantities needed for fast candidate evaluation.
#'
#' For each future slot, computes:
#'   1. Field survival (ownership-weighted, same as before)
#'   2. V_die[sim, round] — payout if we die in each round
#'   3. V_survive[sim, slot] — backward-recursed continuation value
#'   4. Our pick distributions (EV-based) and our survival for future slots
#'
#' @param group A row from group_entries()
#' @param current_slot_id Current slot being decided
#' @param tw Precomputed team wins
#' @param teams_dt Teams data frame (64 rows)
#' @param ownership_by_slot Named list: slot_id -> named numeric vector
#' @param sample_idx Integer vector of sim indices to use
#' @param current_slot_team_ids Team IDs playing in today's slot (excluded from future picks)
#' @param field_survival_curves Optional: output from build_field_survival_curves().
#'   When provided, replaces ownership-weighted field modeling with actual
#'   grouped field simulation data. List with field_dies_slot and p_field_survives_all.
#' @param alive_field_count Optional: actual alive field entries (excluding ours).
#'   When provided, replaces contest_size - our_n.
#' @return List (ctx) with precomputed matrices
precompute_group_context <- function(group, current_slot_id, tw, teams_dt,
                                      ownership_by_slot, sample_idx,
                                      field_survival_curves = NULL,
                                      alive_field_count = NULL,
                                      n_field_alive_matrix = NULL) {
  n_teams <- nrow(teams_dt)
  n_sims <- length(sample_idx)
  contest_size <- group$contest_size
  prize_pool <- group$prize_pool
  our_n <- group$n_entries
  # Field = non-our entries. Use alive count from scrape if available.
  full_field <- if (!is.null(alive_field_count)) alive_field_count
                else contest_size - our_n
  used_teams <- group$used_teams[[1]]
  max_round <- 6L

  group_format <- if ("format" %in% names(group)) group$format else "A"
  slot_order <- get_slot_order(group_format)
  current_idx <- match(current_slot_id, slot_order)
  remaining_slots <- slot_order[current_idx:length(slot_order)]
  n_remaining <- length(remaining_slots)

  slot_round_nums <- integer(n_remaining)
  for (si in seq_along(remaining_slots)) {
    slot_round_nums[si] <- get_slot(remaining_slots[si])$round_num
  }

  # ---- 1 & 2. Field survival and death-round distribution ----
  if (!is.null(n_field_alive_matrix)) {
    # ENTRY-LEVEL PATH: per-sim exact opponent survival counts
    # n_field_alive_matrix[i, s] = number of field entries alive after slot s in sim i
    # This is the most accurate path — replaces both field_survival_curves and ownership.

    # Derive field_slot_survive from counts (for backward recursion compatibility)
    field_slot_survive <- matrix(1, nrow = n_sims, ncol = n_remaining)
    n_alive_before <- rep(full_field, n_sims)
    for (si in seq_len(n_remaining)) {
      n_alive_after <- n_field_alive_matrix[, si]
      field_slot_survive[, si] <- ifelse(n_alive_before > 0,
                                          pmin(n_alive_after / n_alive_before, 1),
                                          0)
      n_alive_before <- n_alive_after
    }

    # Cumulative field survival
    field_cum <- field_slot_survive
    if (n_remaining >= 2) {
      for (si in 2:n_remaining) {
        field_cum[, si] <- field_cum[, si - 1] * field_slot_survive[, si]
      }
    }
    p_field_all <- n_field_alive_matrix[, n_remaining] / pmax(full_field, 1)

    # Field death-round distribution (per sim, exact from counts)
    field_dies_round <- matrix(0, nrow = n_sims, ncol = max_round)
    prev_alive <- rep(full_field, n_sims)
    for (si in seq_along(remaining_slots)) {
      rd <- slot_round_nums[si]
      n_die <- pmax(prev_alive - n_field_alive_matrix[, si], 0)
      field_dies_round[, rd] <- field_dies_round[, rd] + n_die / pmax(full_field, 1)
      prev_alive <- n_field_alive_matrix[, si]
    }

  } else if (!is.null(field_survival_curves)) {
    # Use actual grouped field simulation data (from splash_field_sim.R)
    # field_survival_curves has: field_dies_slot (vector of avg fractions),
    #   p_field_survives_all (scalar avg probability)
    fsc <- field_survival_curves

    # Map slot-based death to round-based death for V_die computation
    # field_dies_slot is a vector of averages (uniform across sims)
    field_dies_round <- matrix(0, nrow = n_sims, ncol = max_round)
    n_fsc_slots <- length(fsc$field_dies_slot)
    for (si in seq_along(remaining_slots)) {
      rd <- slot_round_nums[si]
      if (si <= n_fsc_slots) {
        field_dies_round[, rd] <- field_dies_round[, rd] + fsc$field_dies_slot[si]
      }
    }
    p_field_all <- rep(fsc$p_field_survives_all, n_sims)

    # Reconstruct field_slot_survive from death data (for diagnostics/display)
    field_slot_survive <- matrix(1, nrow = n_sims, ncol = n_remaining)
    for (si in seq_along(remaining_slots)) {
      if (si <= n_fsc_slots) {
        field_slot_survive[, si] <- 1 - fsc$field_dies_slot[si]
      }
    }

    # Build field_cum for return list consistency
    field_cum <- field_slot_survive
    if (n_remaining >= 2) {
      for (si in 2:n_remaining) {
        field_cum[, si] <- field_cum[, si - 1] * field_slot_survive[, si]
      }
    }

  } else {
    # Fallback: ownership-weighted field modeling (original approach)
    field_slot_survive <- matrix(1, nrow = n_sims, ncol = n_remaining)

    for (si in seq_along(remaining_slots)) {
      sid <- remaining_slots[si]
      slot <- get_slot(sid)
      n_picks <- get_n_picks(sid, group_format)
      win_mat_sub <- tw$team_round_wins[[slot$round_num]][sample_idx, , drop = FALSE]

      own <- ownership_by_slot[[sid]]
      if (!is.null(own) && length(own) > 0) {
        own_vec <- numeric(n_teams)
        own_team_ids <- teams_dt$team_id[match(names(own), teams_dt$name)]
        valid <- !is.na(own_team_ids)
        own_vec[own_team_ids[valid]] <- own[names(own)[valid]]

        p_field_single <- as.numeric(win_mat_sub %*% own_vec)
        own_sum <- sum(own_vec)
        if (n_picks > 1 && own_sum > 0) {
          field_slot_survive[, si] <- pmin((p_field_single / own_sum) ^ n_picks, 1)
        } else if (own_sum > 0) {
          field_slot_survive[, si] <- pmin(p_field_single, 1)
        }
        # Debug: check for NaN
        if (any(is.nan(field_slot_survive[, si]))) {
          cat(sprintf("  WARNING: NaN in field_slot_survive for slot %s (own_sum=%.4f, n_picks=%d)\n",
                      sid, own_sum, n_picks))
        }
      }
    }

    # Field death-round distribution (ownership-based)
    field_cum <- field_slot_survive
    if (n_remaining >= 2) {
      for (si in 2:n_remaining) {
        field_cum[, si] <- field_cum[, si - 1] * field_slot_survive[, si]
      }
    }
    p_field_all <- field_cum[, n_remaining]

    field_dies_round <- matrix(0, nrow = n_sims, ncol = max_round)
    for (si in seq_along(remaining_slots)) {
      rd <- slot_round_nums[si]
      p_die <- if (si == 1) 1 - field_slot_survive[, si]
               else field_cum[, si - 1] * (1 - field_slot_survive[, si])
      field_dies_round[, rd] <- field_dies_round[, rd] + p_die
    }
  }

  # ---- 3. V_die[sim, round] — payout if we die in each round ----
  V_die <- matrix(0, nrow = n_sims, ncol = max_round)

  if (!is.null(n_field_alive_matrix)) {
    # ENTRY-LEVEL PATH: n_alive values are continuous expected counts per sim.
    # We can't use ifelse(n_alive > 0, 0, ...) because n_alive is never exactly 0.
    # Instead, use the Poisson approximation: P(0 survive) ≈ exp(-n_alive),
    # which is the standard limit of (1 - n_alive/N)^N for large N.
    for (d in 1:max_round) {
      round_d_slots <- which(slot_round_nums == d)
      if (length(round_d_slots) == 0) next
      last_slot_d <- max(round_d_slots)
      first_slot_d <- min(round_d_slots)

      n_alive_after_d <- n_field_alive_matrix[, last_slot_d]
      n_alive_before_d <- if (first_slot_d == 1) rep(full_field, n_sims)
                           else n_field_alive_matrix[, first_slot_d - 1L]
      n_die_d <- pmax(n_alive_before_d - n_alive_after_d, 0)

      # P(nobody survives past round d) ≈ exp(-n_alive_after_d)
      # Matches the ownership path: (1 - p_outlast)^full_field ≈ exp(-p_outlast * full_field)
      p_nobody_outlasts <- exp(-n_alive_after_d)
      V_die[, d] <- p_nobody_outlasts * prize_pool / (1 + n_die_d)
    }
  } else if (!is.null(field_survival_curves)) {
    # FIELD SIM PATH: fractions are deterministic per sim, not probabilities
    for (d in 1:max_round) {
      frac_outlast <- p_field_all
      if (d < max_round) {
        for (rd in (d + 1):max_round) {
          frac_outlast <- frac_outlast + field_dies_round[, rd]
        }
      }
      n_outlast <- round(frac_outlast * full_field)
      n_same <- round(field_dies_round[, d] * full_field)
      # If anyone outlasts us, we get $0; otherwise split with those dying same round
      V_die[, d] <- ifelse(n_outlast == 0, prize_pool / (1 + n_same), 0)
    }
  } else {
    # OWNERSHIP PATH: probabilistic (original)
    for (d in 1:max_round) {
      p_outlast <- p_field_all
      if (d < max_round) {
        for (rd in (d + 1):max_round) {
          p_outlast <- p_outlast + field_dies_round[, rd]
        }
      }
      # Clamp to [0, 1] — floating point arithmetic can produce tiny overflows
      p_outlast <- pmin(pmax(p_outlast, 0), 1)
      p_nobody <- (1 - p_outlast) ^ full_field
      p_same <- pmax(field_dies_round[, d], 0)
      expected_same <- p_same * full_field
      V_die[, d] <- p_nobody * prize_pool / (1 + expected_same)
    }
  }

  # NaN guard on V_die
  if (any(is.nan(V_die))) {
    nan_rounds <- which(apply(V_die, 2, function(x) any(is.nan(x))))
    cat(sprintf("  WARNING: NaN in V_die for rounds: %s (full_field=%d)\n",
                paste(nan_rounds, collapse = ","), full_field))
    V_die[is.nan(V_die)] <- 0
  }

  # ---- 4. V_survive[sim, slot] — backward recursion ----
  # V_survive[, s] = expected value of being alive at slot s, using field survival
  # for all remaining slots from s onward.
  V_survive <- matrix(0, nrow = n_sims, ncol = n_remaining + 1)
  # Terminal: survived everything → Case 1 payout
  if (!is.null(n_field_alive_matrix)) {
    # Entry-level: n_alive at terminal is expected field survivors per sim.
    # Use same formula as ownership path for consistency:
    # prize / (1 + expected_field_survivors)
    V_survive[, n_remaining + 1] <- prize_pool / (1 + n_field_alive_matrix[, n_remaining])
    # Note: n_alive here varies per sim (higher when chalk holds, lower on upsets),
    # which is the key advantage of the entry-level model.
  } else {
    V_survive[, n_remaining + 1] <- prize_pool / (1 + p_field_all * full_field)
  }

  for (s in n_remaining:2) {
    rd <- slot_round_nums[s]
    V_survive[, s] <- field_slot_survive[, s] * V_survive[, s + 1] +
                       (1 - field_slot_survive[, s]) * V_die[, rd]
  }

  # ---- 5. Precompute per-slot team scores for greedy forward sim ----
  # For each future slot, score every team by mini-EV = mean(win[sim,t] * delta_V[sim]).
  # These scores are independent of which candidate we pick today, so precompute once.
  # The actual pick selection happens per-candidate (greedy, respecting no-reuse).
  slot_team_scores <- vector("list", n_remaining)  # slot_team_scores[[si]] = named numeric

  for (si in 2:n_remaining) {
    sid <- remaining_slots[si]
    slot <- get_slot(sid)
    win_mat_sub <- tw$team_round_wins[[slot$round_num]][sample_idx, , drop = FALSE]

    rd <- slot_round_nums[si]
    delta_V <- V_survive[, si + 1] - V_die[, rd]

    slot_team_ids <- get_teams_in_slot(sid, teams_dt)
    if (length(slot_team_ids) == 0) {
      slot_team_scores[[si]] <- setNames(numeric(0), character(0))
      next
    }

    scores <- as.numeric(crossprod(win_mat_sub[, slot_team_ids, drop = FALSE], delta_V)) / n_sims
    slot_team_scores[[si]] <- setNames(scores, as.character(slot_team_ids))
  }

  list(
    remaining_slots    = remaining_slots,
    slot_round_nums    = slot_round_nums,
    n_remaining        = n_remaining,
    field_slot_survive = field_slot_survive,
    field_cum          = field_cum,
    field_dies_round   = field_dies_round,
    p_field_all        = p_field_all,
    V_die              = V_die,
    V_survive          = V_survive,
    slot_team_scores   = slot_team_scores,
    sample_idx         = sample_idx,
    n_sims             = n_sims,
    full_field         = full_field,
    contest_size       = contest_size,
    our_n              = our_n,
    prize_pool         = prize_pool,
    max_round          = max_round,
    group_format       = group_format,
    used_teams         = used_teams,
    teams_dt           = teams_dt
  )
}

# ==============================================================================
# STEP 2b: CANDIDATE-AWARE FIELD SURVIVAL ADJUSTMENT
# ==============================================================================

#' Get all team_ids on the opposing side of team_id's bracket half in a given round.
#'
#' In a 64-team bracket, round r groups teams into blocks of 2^r. The opponent
#' half contains the teams our candidate would face. Their collective ownership
#' is what correlates with our candidate's win/loss.
# ==============================================================================
# STEP 3: BRACKET COMPATIBILITY & CANDIDATE EV CALCULATION
# ==============================================================================

#' Compute the earliest tournament round where two teams could meet.
#'
#' In a 64-team single-elimination bracket, team_ids are 1-indexed bracket
#' positions. Two teams' earliest meeting round is determined by their position
#' in the bracket tree.
#'
#' @param t1 Integer team_id (bracket position, 1-64)
#' @param t2 Integer team_id (bracket position, 1-64)
#' @return Integer round number (1-6) where these teams first could meet
earliest_meeting_round <- function(t1, t2) {
  p1 <- t1 - 1L  # 0-indexed
  p2 <- t2 - 1L
  for (r in 1:6) {
    if (bitwShiftR(p1, r) == bitwShiftR(p2, r)) return(r)
  }
  7L  # should never happen in 64-team bracket
}

#' Get the region number for a team (1=East, 2=South, 3=West, 4=Midwest)
#' @param team_id Integer team_id (bracket position, 1-64)
#' @return Integer 1-4
get_team_region <- function(team_id) {
  as.integer(ceiling(team_id / 16))
}

#' Check if a candidate team is bracket-compatible with all existing picks.
#'
#' Two checks:
#' 1. Pairwise: Two picks (team A, round r_a) and (team B, round r_b) are
#'    IMPOSSIBLE if earliest meeting round m <= min(r_a, r_b).
#' 2. Late-round region rule: E8 (rd 4), FF (rd 5), and CHAMP (rd 6) picks
#'    must each come from a different region (4 picks, 4 regions). This prevents
#'    same-FF-side E8 picks that block all viable FF/CHAMP options.
#'
#' @param candidate_id Team ID to check
#' @param candidate_round Round number for the candidate pick
#' @param path_picks Integer vector of team_ids already picked in this path
#' @param path_rounds Integer vector of round numbers for each pick
#' @return TRUE if compatible, FALSE if impossible
is_bracket_compatible <- function(candidate_id, candidate_round,
                                   path_picks, path_rounds) {
  # Pairwise bracket check
  for (i in seq_along(path_picks)) {
    if (is.na(path_picks[i])) next
    m <- earliest_meeting_round(path_picks[i], candidate_id)
    if (m <= min(path_rounds[i], candidate_round)) return(FALSE)
  }

  # Late-round region constraint for E8 + FF + CHAMP:
  # The 4 picks (2 E8, 1 FF, 1 CHAMP) must come from 4 different regions,
  # AND the 2 E8 picks must be from opposite FF sides:
  #   Side 1: East (region 1) + South (region 2) → FF game 61
  #   Side 2: West (region 3) + Midwest (region 4) → FF game 62
  # If E8 picks are same side, the FF game between them eliminates one,
  # and no viable CHAMP pick exists.
  if (candidate_round >= 4L) {
    cand_region <- get_team_region(candidate_id)
    cand_side <- if (cand_region <= 2L) 1L else 2L
    late_mask <- path_rounds >= 4L & !is.na(path_picks)
    if (any(late_mask)) {
      used_regions <- vapply(path_picks[late_mask], get_team_region, integer(1))
      used_rounds <- path_rounds[late_mask]

      # Must be a different region from all existing late-round picks
      if (cand_region %in% used_regions) return(FALSE)

      # E8 picks (round 4) must be from opposite FF sides
      if (candidate_round == 4L) {
        e8_mask <- used_rounds == 4L
        if (any(e8_mask)) {
          e8_sides <- ifelse(used_regions[e8_mask] <= 2L, 1L, 2L)
          if (cand_side %in% e8_sides) return(FALSE)
        }
      }

      # FF (round 5): must be from the opposite FF side of the FF pick's game
      # FF team must be on a side where we DON'T have an E8 pick, so the FF
      # team plays against our E8 team's opponent, not our E8 team itself.
      # Actually: FF team can be from EITHER side — they just need a different
      # region than any E8 pick. If E8 picks are side1+side2, FF can pick from
      # the unused region on either side. The pairwise check handles the rest.
    }
  }

  TRUE
}

# ==============================================================================
# PATH OPTIONALITY — rigorous measure of future path availability
# ==============================================================================

#' Compute path optionality score for candidate picks.
#'
#' For each candidate, measures the expected number of bracket-compatible,
#' winning teams available at each future slot. The log-sum across future
#' slots gives a simulation-backed optionality score. Higher = more future
#' paths = more resilience to ownership/matchup uncertainty.
#'
#' The key insight: picking AGAINST a used team (i.e., picking that team's
#' opponent) eliminates the used team from the bracket, freeing up bracket
#' positions in future rounds. This shows up naturally as higher available
#' counts because bracket compatibility constraints loosen.
#'
#' @param candidate_ids Integer vector of team_ids to evaluate
#' @param group_used_teams Integer vector of team_ids already used by this group
#' @param group_used_rounds Integer vector of round numbers for each used team
#' @param remaining_slots Character vector of remaining slot IDs (first = today)
#' @param slot_round_nums Integer vector of round numbers for each remaining slot
#' @param tw Precomputed team wins (list of team_round_wins matrices)
#' @param teams_dt Data frame with team_id, name, seed, region
#' @param sample_idx Integer vector of sim indices to use
#' @param n_subsample Number of sims to subsample (default 10000 for speed)
#' @return Numeric vector of log-optionality scores (one per candidate)
compute_path_optionality <- function(candidate_ids, group_used_teams,
                                      group_used_rounds,
                                      remaining_slots, slot_round_nums,
                                      tw, teams_dt, sample_idx,
                                      n_subsample = 10000L) {
  # Subsample for speed — 10K sims is enough for counting available teams
  if (length(sample_idx) > n_subsample) {
    sub_idx <- sample(sample_idx, n_subsample)
  } else {
    sub_idx <- sample_idx
  }
  n_sub <- length(sub_idx)
  n_future <- length(remaining_slots) - 1  # exclude today's slot

  if (n_future < 1) {
    return(rep(0, length(candidate_ids)))
  }

  optionality <- numeric(length(candidate_ids))

  for (ci in seq_along(candidate_ids)) {
    cand <- candidate_ids[ci]

    # Build path state: used teams + their assigned rounds
    path_teams  <- c(group_used_teams, cand)
    path_rounds <- c(group_used_rounds, slot_round_nums[1])  # today's round

    log_branch_sum <- 0

    for (si in 2:length(remaining_slots)) {
      rd <- slot_round_nums[si]
      slot_teams <- get_teams_in_slot(remaining_slots[si], teams_dt)

      if (length(slot_teams) == 0) {
        log_branch_sum <- log_branch_sum + log(0.1)
        next
      }

      # Win matrix for this slot (subsampled sims x candidate teams)
      win_mat <- tw$team_round_wins[[rd]][sub_idx, slot_teams, drop = FALSE]

      # Zero out unavailable teams: already used OR bracket-incompatible
      for (ti in seq_along(slot_teams)) {
        tid <- slot_teams[ti]
        if (tid %in% path_teams) {
          win_mat[, ti] <- 0L
        } else if (!is_bracket_compatible(tid, rd, path_teams, path_rounds)) {
          win_mat[, ti] <- 0L
        }
      }

      # Per-sim count of available (winning + compatible) teams
      avail_per_sim <- rowSums(win_mat > 0)
      mean_avail <- mean(avail_per_sim)

      # Log-sum: captures bottleneck effect (if any slot has few options,
      # total optionality collapses)
      log_branch_sum <- log_branch_sum + log(max(mean_avail, 0.1))

      # Greedily add the most-frequently-available team to path
      # This simulates a reasonable future pick for constraint propagation
      col_avail <- colMeans(win_mat > 0)
      best_ti <- which.max(col_avail)
      if (length(best_ti) > 0 && col_avail[best_ti] > 0) {
        path_teams  <- c(path_teams, slot_teams[best_ti])
        path_rounds <- c(path_rounds, rd)
      }
    }

    optionality[ci] <- log_branch_sum
  }

  optionality
}

# ==============================================================================
# CANDIDATE EV CALCULATION (BEAM SEARCH)
# ==============================================================================

#' Compute EV for a candidate pick using beam search forward simulation.
#'
#' Uses beam search to explore multiple future pick paths. At each slot,
#' filters candidates by bracket compatibility (teams that would create
#' impossible paths are excluded) and expands top teams by mini-EV score.
#'
#' @param candidate_id Integer team_id for today's pick
#' @param ctx Precomputed group context from precompute_group_context()
#' @param tw Precomputed team wins
#' @param diagnostics If TRUE, return extra info about the best path
#' @param beam_width Number of paths to keep at each step (default 5)
#' @param expand_top Number of top teams to try per path per slot (default 3)
#' @return List with: ev, p_survive_today, p_win_contest, mean_death_rd, p_survive_all,
#'         and optionally forward_picks
compute_candidate_ev <- function(candidate_id, ctx, tw, diagnostics = FALSE,
                                  beam_width = 6, expand_top = 12) {
  n_sims <- ctx$n_sims
  max_round <- ctx$max_round
  n_remaining <- ctx$n_remaining
  slot_round_nums <- ctx$slot_round_nums
  remaining_slots <- ctx$remaining_slots
  full_field <- ctx$full_field
  prize_pool <- ctx$prize_pool
  teams_dt <- ctx$teams_dt
  debug <- diagnostics  # print debug info when diagnostics requested

  # ---- Initialize beam with candidate pick in slot 1 ----
  rd1 <- slot_round_nums[1]
  slot1_survive <- tw$team_round_wins[[rd1]][ctx$sample_idx, candidate_id]
  slot1_n_picks <- get_n_picks(remaining_slots[1], ctx$group_format)

  if (debug) {
    cat(sprintf("  [DEBUG] candidate=%s (id=%d), rd1=%d, n_picks=%d\n",
                teams_dt$name[candidate_id], candidate_id, rd1, slot1_n_picks))
    cat(sprintf("  [DEBUG] slot1_survive: mean=%.4f, min=%.4f, max=%.4f, any_NaN=%s\n",
                mean(slot1_survive), min(slot1_survive), max(slot1_survive),
                any(is.nan(slot1_survive))))
    cat(sprintf("  [DEBUG] used_teams: %s\n",
                paste(ctx$used_teams, collapse = ",")))
    cat(sprintf("  [DEBUG] n_remaining=%d, remaining_slots=%s\n",
                n_remaining, paste(remaining_slots, collapse = ",")))
    cat(sprintf("  [DEBUG] V_die summary: %s\n",
                paste(sprintf("rd%d=%.4f", 1:6, colMeans(ctx$V_die)), collapse = ", ")))
    cat(sprintf("  [DEBUG] V_survive summary: %s\n",
                paste(sprintf("s%d=%.4f", 1:(n_remaining+1), colMeans(ctx$V_survive)), collapse = ", ")))
    cat(sprintf("  [DEBUG] any NaN in V_die=%s, V_survive=%s\n",
                any(is.nan(ctx$V_die)), any(is.nan(ctx$V_survive))))
  }

  # Handle multi-pick current slot (e.g., Format C: 2 picks per R1 day)
  # Use continuation-value scoring: companion should maximize marginal EV,
  # not just R1 win probability. This means the companion depends on the
  # primary candidate via bracket compatibility and used-team exclusion.
  slot1_extra_ids <- integer(0)
  init_used <- c(ctx$used_teams, candidate_id)
  init_bracket_teams <- candidate_id
  init_bracket_rounds <- rd1

  if (slot1_n_picks > 1) {
    slot1_team_ids <- get_teams_in_slot(remaining_slots[1], teams_dt)
    slot1_team_ids <- setdiff(slot1_team_ids, init_used)
    # Filter locked teams from companion picks
    if (length(ctx$locked_team_ids) > 0) {
      slot1_team_ids <- setdiff(slot1_team_ids, ctx$locked_team_ids)
    }

    # Region constraint for format C companions: must use different region from primary
    # Applies to R1 and R2 (2 picks per day in format C through R3)
    if (ctx$group_format == "C" && grepl("R[123]", remaining_slots[1])) {
      cand_region <- get_team_region(candidate_id)
      # Companion must be from a different region than the primary pick
      # (ensures within-day region diversity)
      slot1_team_ids <- slot1_team_ids[
        !vapply(slot1_team_ids, function(tid) get_team_region(tid) == cand_region, logical(1))
      ]
    }

    # Score companions using continuation value (same approach as beam search)
    # delta_V captures the value difference between surviving slot 1 and dying
    win_mat_rd1 <- tw$team_round_wins[[rd1]][ctx$sample_idx, , drop = FALSE]
    if (n_remaining >= 2) {
      delta_V <- ctx$V_survive[, 2] - ctx$V_die[, rd1]
    } else {
      # Only one slot remaining — value of surviving is the terminal payout
      delta_V <- ctx$V_survive[, n_remaining + 1] - ctx$V_die[, rd1]
    }

    for (p in 2:slot1_n_picks) {
      if (length(slot1_team_ids) == 0) {
        slot1_survive <- rep(0, n_sims)
        break
      }
      # Filter by bracket compatibility with all current picks
      compat_mask <- vapply(slot1_team_ids, function(tid) {
        is_bracket_compatible(tid, rd1, init_bracket_teams, init_bracket_rounds)
      }, logical(1))
      slot1_team_ids <- slot1_team_ids[compat_mask]
      if (length(slot1_team_ids) == 0) {
        slot1_survive <- rep(0, n_sims)
        break
      }

      # Score by marginal EV contribution: E[win_i * delta_V]
      companion_scores <- as.numeric(
        crossprod(win_mat_rd1[, slot1_team_ids, drop = FALSE], delta_V)
      ) / n_sims
      best_extra_idx <- which.max(companion_scores)
      best_extra_id <- slot1_team_ids[best_extra_idx]
      slot1_extra_ids <- c(slot1_extra_ids, best_extra_id)
      slot1_survive <- slot1_survive * win_mat_rd1[, best_extra_id]
      init_used <- c(init_used, best_extra_id)
      init_bracket_teams <- c(init_bracket_teams, best_extra_id)
      init_bracket_rounds <- c(init_bracket_rounds, rd1)
      slot1_team_ids <- setdiff(slot1_team_ids, best_extra_id)
    }
  }

  if (debug) {
    cat(sprintf("  [DEBUG] After companion: slot1_survive mean=%.4f, extras=%s\n",
                mean(slot1_survive),
                if (length(slot1_extra_ids) > 0)
                  paste(teams_dt$name[slot1_extra_ids], collapse = ",") else "none"))
    cat(sprintf("  [DEBUG] init_used: %s\n", paste(init_used, collapse = ",")))
  }

  # Each beam path: list(used_teams, cum_survive, partial_payout, picks, pick_rounds)
  # cum_survive = per-sim cumulative survival through slots so far
  # partial_payout = per-sim accumulated payout from death in slots 1..current
  # pick_rounds = round number for each pick (for bracket compatibility checks)
  slot1_die <- 1 - slot1_survive
  init_payout <- slot1_die * ctx$V_die[, rd1]

  beam <- list(list(
    used_teams     = init_used,
    cum_survive    = slot1_survive,
    partial_payout = init_payout,
    picks          = candidate_id,       # one per slot (for display)
    pick_rounds    = rd1,                # one per slot (for display)
    bracket_teams  = init_bracket_teams, # ALL picks incl. multi-pick extras
    bracket_rounds = init_bracket_rounds # round for each bracket_teams entry
  ))

  # ---- Beam search through future slots ----
  if (n_remaining >= 2) {
    for (si in 2:n_remaining) {
      scores <- ctx$slot_team_scores[[si]]
      sid <- remaining_slots[si]
      rd <- slot_round_nums[si]
      n_picks <- get_n_picks(sid, ctx$group_format)
      win_mat_rd <- tw$team_round_wins[[rd]]

      if (is.null(scores) || length(scores) == 0) {
        # No teams available for this slot — all paths die
        for (bi in seq_along(beam)) {
          beam[[bi]]$partial_payout <- beam[[bi]]$partial_payout +
            beam[[bi]]$cum_survive * ctx$V_die[, rd]
          beam[[bi]]$cum_survive <- rep(0, n_sims)
          beam[[bi]]$picks <- c(beam[[bi]]$picks, NA_integer_)
          beam[[bi]]$pick_rounds <- c(beam[[bi]]$pick_rounds, rd)
        }
        next
      }

      new_beam <- list()

      for (bi in seq_along(beam)) {
        path <- beam[[bi]]

        # Filter to available teams: not already used AND bracket-compatible
        # Use bracket_teams/bracket_rounds (includes multi-pick extras)
        avail_mask <- !names(scores) %in% as.character(path$used_teams)
        if (!any(avail_mask)) {
          new_beam[[length(new_beam) + 1]] <- list(
            used_teams     = path$used_teams,
            cum_survive    = rep(0, n_sims),
            partial_payout = path$partial_payout +
              path$cum_survive * ctx$V_die[, rd],
            picks          = c(path$picks, NA_integer_),
            pick_rounds    = c(path$pick_rounds, rd),
            bracket_teams  = path$bracket_teams,
            bracket_rounds = path$bracket_rounds
          )
          next
        }

        avail_scores <- scores[avail_mask]
        avail_ids <- as.integer(names(avail_scores))

# Filter by bracket compatibility against ALL picks (incl. multi-pick extras)
        compat_mask <- vapply(avail_ids, function(tid) {
          is_bracket_compatible(tid, rd, path$bracket_teams, path$bracket_rounds)
        }, logical(1))

        if (!any(compat_mask)) {
          new_beam[[length(new_beam) + 1]] <- list(
            used_teams     = path$used_teams,
            cum_survive    = rep(0, n_sims),
            partial_payout = path$partial_payout +
              path$cum_survive * ctx$V_die[, rd],
            picks          = c(path$picks, NA_integer_),
            pick_rounds    = c(path$pick_rounds, rd),
            bracket_teams  = path$bracket_teams,
            bracket_rounds = path$bracket_rounds
          )
          next
        }

        avail_scores <- avail_scores[compat_mask]
        avail_ids <- avail_ids[compat_mask]

        # Drop teams with score <= 0 before expanding
        alive_mask <- avail_scores > 0
        avail_scores <- avail_scores[alive_mask]
        avail_ids <- avail_ids[alive_mask]

        n_expand <- min(5, length(avail_scores))
        if (n_expand == 0) {
          # No viable picks — path dies here (same as other no-candidate cases)
          new_beam[[length(new_beam) + 1]] <- list(
            used_teams     = path$used_teams,
            cum_survive    = rep(0, n_sims),
            partial_payout = path$partial_payout +
              path$cum_survive * ctx$V_die[, rd],
            picks          = c(path$picks, NA_integer_),
            pick_rounds    = c(path$pick_rounds, rd),
            bracket_teams  = path$bracket_teams,
            bracket_rounds = path$bracket_rounds
          )
          next
        }
        
        top_idx <- order(avail_scores, decreasing = TRUE)[1:n_expand]

        for (ti in top_idx) {
          team_id <- avail_ids[ti]
          slot_survive <- win_mat_rd[ctx$sample_idx, team_id]

          new_bracket_teams <- c(path$bracket_teams, team_id)
          new_bracket_rounds <- c(path$bracket_rounds, rd)

          # Handle multi-pick slots: pick additional teams with bracket checks
          if (n_picks > 1) {
            extra_used <- c(path$used_teams, team_id)
            combined <- slot_survive
            for (p in 2:n_picks) {
              extra_avail <- !names(scores) %in% as.character(extra_used)
              if (!any(extra_avail)) {
                combined <- rep(0, n_sims)
                break
              }
              extra_scores <- scores[extra_avail]
              extra_ids <- as.integer(names(extra_scores))
              # Check bracket compatibility for extra picks too
              extra_compat <- vapply(extra_ids, function(tid) {
                is_bracket_compatible(tid, rd, new_bracket_teams, new_bracket_rounds)
              }, logical(1))
              if (!any(extra_compat)) {
                combined <- rep(0, n_sims)
                break
              }
              extra_scores <- extra_scores[extra_compat]
              next_tid <- as.integer(names(extra_scores)[which.max(extra_scores)])
              extra_used <- c(extra_used, next_tid)
              new_bracket_teams <- c(new_bracket_teams, next_tid)
              new_bracket_rounds <- c(new_bracket_rounds, rd)
              combined <- combined * win_mat_rd[ctx$sample_idx, next_tid]
            }
            slot_survive <- combined
            new_used <- extra_used
          } else {
            new_used <- c(path$used_teams, team_id)
          }

          new_cum <- path$cum_survive * slot_survive
          new_partial <- path$partial_payout +
            path$cum_survive * (1 - slot_survive) * ctx$V_die[, rd]

          new_beam[[length(new_beam) + 1]] <- list(
            used_teams     = new_used,
            cum_survive    = new_cum,
            partial_payout = new_partial,
            picks          = c(path$picks, team_id),
            pick_rounds    = c(path$pick_rounds, rd),
            bracket_teams  = new_bracket_teams,
            bracket_rounds = new_bracket_rounds
          )
        }
      }

      # Deduplicate beams: paths with identical pick sequences produce identical EVs.
      # Keep only unique paths (by picks so far) before pruning, so beam width
      # isn't wasted on duplicates.
      if (length(new_beam) > 1) {
        pick_fps <- vapply(new_beam, function(p) {
          paste(p$picks, collapse = ",")
        }, character(1))
        new_beam <- new_beam[!duplicated(pick_fps)]
      }

      # Prune to top beam_width paths by partial EV:
      # partial_payout (death contributions so far) + cum_survive * V_survive (continuation)
      if (length(new_beam) > beam_width) {
        V_cont <- ctx$V_survive[, si + 1]
        partial_evs <- vapply(new_beam, function(p) {
          mean(p$partial_payout + p$cum_survive * V_cont)
        }, numeric(1))
        keep_idx <- order(partial_evs, decreasing = TRUE)[1:beam_width]
        new_beam <- new_beam[keep_idx]
      }

      if (debug && length(new_beam) == 0) {
        cat(sprintf("  [DEBUG] BEAM EMPTY at slot %d (%s, rd=%d)! scores: %d total, range=[%.6f, %.6f]\n",
                    si, sid, rd,
                    length(scores),
                    if (length(scores) > 0) min(scores) else NA,
                    if (length(scores) > 0) max(scores) else NA))
      }
      beam <- new_beam
    }
  }

  # ---- Evaluate each surviving beam path and pick the best ----
  if (debug) {
    cat(sprintf("  [DEBUG] Beam has %d paths to evaluate\n", length(beam)))
  }
  best_ev <- -Inf
  best_result <- NULL

  for (bi in seq_along(beam)) {
    path <- beam[[bi]]
    pick_sequence <- path$picks

    # Build binary survival matrix from this path's pick sequence
    our_slot_survive <- matrix(0, nrow = n_sims, ncol = n_remaining)
    for (si in seq_along(remaining_slots)) {
      tid <- pick_sequence[si]
      if (is.na(tid)) {
        our_slot_survive[, si] <- 0
      } else {
        rd <- slot_round_nums[si]
        our_slot_survive[, si] <- tw$team_round_wins[[rd]][ctx$sample_idx, tid]
      }
    }

    # Re-handle multi-pick survival using bracket_teams from the beam path.
    # The beam expansion already picked bracket-compatible extras; we find them
    # in bracket_teams (they share the same round but aren't the primary pick).
    bt <- path$bracket_teams
    br <- path$bracket_rounds
    for (si in seq_along(remaining_slots)) {
      sid <- remaining_slots[si]
      n_picks_slot <- get_n_picks(sid, ctx$group_format)
      if (n_picks_slot > 1) {
        rd <- slot_round_nums[si]
        primary_tid <- pick_sequence[si]
        if (is.na(primary_tid)) next
        # Find extra picks: in bracket_teams with this round, excluding primary
        extra_tids <- bt[br == rd & bt != primary_tid]
        combined <- our_slot_survive[, si]
        for (etid in extra_tids) {
          combined <- combined * tw$team_round_wins[[rd]][ctx$sample_idx, etid]
        }
        our_slot_survive[, si] <- combined
      }
    }

    # Cumulative survival and death-round distribution
    our_cum <- our_slot_survive
    if (n_remaining >= 2) {
      for (si in 2:n_remaining) {
        our_cum[, si] <- our_cum[, si - 1] * our_slot_survive[, si]
      }
    }
    p_us_all <- our_cum[, n_remaining]

    us_dies_round <- matrix(0, nrow = n_sims, ncol = max_round)
    for (si in seq_along(remaining_slots)) {
      rd <- slot_round_nums[si]
      p_die <- if (si == 1) 1 - our_slot_survive[, si]
               else our_cum[, si - 1] * (1 - our_slot_survive[, si])
      us_dies_round[, rd] <- us_dies_round[, rd] + p_die
    }

    # ---- Last-man-standing EV computation ----
    # In survivor, the prize goes to whoever lasts LONGEST. If someone outlasts
    # you, you get nothing. The payout formula is:
    #
    # Case 1 (survived everything): guaranteed winner, split with co-survivors
    #   payout = prize / (1 + expected_field_co_survivors)
    #
    # Case 2 (died in round d): only win if nobody survives past round d
    #   payout = P(nobody outlasts) × prize / (1 + expected_same_round_deaths)
    #   where P(nobody outlasts) = (1 - p_outlast)^N
    #
    # This matches V_die from precompute_group_context. The portfolio-level
    # correction for self-competition among our entries is handled separately
    # by evaluate_portfolio_ev() after allocation.
    payouts <- numeric(n_sims)
    p_win_per_sim <- numeric(n_sims)

    # Vectorized: find each sim's death round (0 = survived all)
    our_death_rd <- rep(0L, n_sims)
    for (si in seq_along(remaining_slots)) {
      alive_before <- if (si == 1) rep(TRUE, n_sims) else our_cum[, si - 1] > 0
      died_here <- alive_before & (our_slot_survive[, si] == 0)
      our_death_rd[died_here & our_death_rd == 0L] <- slot_round_nums[si]
    }

    # Precompute P(field entry outlasts us from each round)
    p_outlast_from <- matrix(0, nrow = n_sims, ncol = max_round)
    for (d in 1:max_round) {
      p_outlast_from[, d] <- ctx$p_field_all
      if (d < max_round) {
        for (rd in (d + 1):max_round) {
          p_outlast_from[, d] <- p_outlast_from[, d] + ctx$field_dies_round[, rd]
        }
      }
    }

    # Case 1: survived all (our_death_rd == 0)
    survived_all <- (our_death_rd == 0L)
    if (any(survived_all)) {
      field_co <- ctx$p_field_all[survived_all] * full_field
      payouts[survived_all] <- prize_pool / (1 + field_co)
      p_win_per_sim[survived_all] <- 1 / (1 + field_co)
    }

    # Case 2: died in round d — we only win if nobody outlasts us
    for (d in 1:max_round) {
      mask <- (our_death_rd == d)
      if (!any(mask)) next

      p_outlast <- pmin(pmax(p_outlast_from[mask, d], 0), 1)
      p_nobody <- (1 - p_outlast) ^ full_field
      field_same <- pmax(ctx$field_dies_round[mask, d], 0) * full_field

      payouts[mask] <- p_nobody * prize_pool / (1 + field_same)
      p_win_per_sim[mask] <- p_nobody / (1 + field_same)
    }

    ev <- mean(payouts)

    if (debug) {
      cat(sprintf("  [DEBUG] Beam path %d: ev=%.6f, NaN_payouts=%d/%d, survived=%d, died_rd1=%d\n",
                  bi, ev, sum(is.nan(payouts)), length(payouts),
                  sum(our_death_rd == 0L), sum(our_death_rd == 1L)))
    }

    if (ev > best_ev) {
      best_ev <- ev

      # Track per-round EV breakdown for diagnostics
      ev_case1 <- mean(ifelse(our_death_rd == 0L, payouts, 0))
      ev_by_round <- numeric(max_round)
      for (d in 1:max_round) {
        ev_by_round[d] <- mean(ifelse(our_death_rd == d, payouts, 0))
      }

      best_result <- list(
        ev              = ev,
        p_survive_today = mean(our_slot_survive[, 1]),
        p_win_contest   = mean(p_win_per_sim),
        p_survive_all   = mean(p_us_all),
        pick_sequence   = pick_sequence,
        our_slot_survive = our_slot_survive,
        field_slot_survive = ctx$field_slot_survive,
        us_dies_round   = us_dies_round,
        our_death_rd    = our_death_rd,
        bracket_teams   = path$bracket_teams,
        bracket_rounds  = path$bracket_rounds,
        ev_case1        = ev_case1,
        ev_by_round     = ev_by_round
      )
    }
  }

  # ---- Metrics ----
  if (is.null(best_result)) {
    # All beam paths produced NaN/invalid EVs — return zero-EV result
    return(list(
      ev              = 0,
      p_survive_today = 0,
      p_win_contest   = 0,
      mean_death_rd   = slot_round_nums[1],
      p_survive_all   = 0,
      our_death_rd    = rep(slot_round_nums[1], n_sims),
      slot1_extra_ids = slot1_extra_ids
    ))
  }

  p_survive_all <- best_result$p_survive_all
  weighted_death <- 0
  for (d in 1:max_round) {
    weighted_death <- weighted_death + d * mean(best_result$us_dies_round[, d])
  }
  avg_death_rd <- weighted_death / max(1 - p_survive_all, 1e-12)

  result <- list(
    ev              = best_result$ev,
    p_survive_today = best_result$p_survive_today,
    p_win_contest   = best_result$p_win_contest,
    mean_death_rd   = avg_death_rd,
    p_survive_all   = p_survive_all,
    our_death_rd    = best_result$our_death_rd,
    slot1_extra_ids = slot1_extra_ids
  )

  if (diagnostics) {
    result$pick_sequence <- best_result$pick_sequence
    result$pick_names <- sapply(best_result$pick_sequence, function(tid) {
      if (is.na(tid)) "NONE" else teams_dt$name[tid]
    })
    result$pick_wp <- sapply(seq_along(best_result$pick_sequence), function(si) {
      tid <- best_result$pick_sequence[si]
      if (is.na(tid)) 0 else tw$team_round_probs[tid, slot_round_nums[si]]
    })
    result$our_slot_survive_mean <- colMeans(best_result$our_slot_survive)
    result$field_slot_survive_mean <- colMeans(best_result$field_slot_survive)
    result$n_beam_paths <- length(beam)

    # EV breakdown diagnostics
    result$ev_case1 <- best_result$ev_case1
    result$ev_by_round <- best_result$ev_by_round
    result$our_death_dist <- colMeans(best_result$us_dies_round)
    result$mean_V_die <- colMeans(ctx$V_die)
    result$mean_p_field_all <- mean(ctx$p_field_all)
    result$full_field <- full_field
    result$prize_pool <- prize_pool

    # For multi-pick slots, find the extra picks from bracket_teams
    result$extra_picks <- list()
    bt <- best_result$bracket_teams
    br <- best_result$bracket_rounds
    for (si in seq_along(remaining_slots)) {
      sid <- remaining_slots[si]
      n_picks_slot <- get_n_picks(sid, ctx$group_format)
      if (n_picks_slot > 1) {
        rd <- slot_round_nums[si]
        primary_tid <- best_result$pick_sequence[si]
        if (!is.na(primary_tid)) {
          extra_tids <- bt[br == rd & bt != primary_tid]
          extras <- character(0)
          for (etid in extra_tids) {
            extras <- c(extras, sprintf("%s (%.1f%%)",
                        teams_dt$name[etid],
                        100 * tw$team_round_probs[etid, rd]))
          }
          result$extra_picks[[as.character(si)]] <- extras
        }
      }
    }
  }

  result
}

# ==============================================================================
# STEP 3b: PORTFOLIO-LEVEL EV (NFL-STYLE SHARE FORMULA)
#
# The per-candidate EV from compute_candidate_ev() is useful for RANKING
# candidates but overstates absolute value because each entry independently
# uses 1 as its own contribution, ignoring our other entries competing for
# the same prize pool.
#
# This function computes realistic portfolio-level EVs using the NFL approach:
#   payout = (my_winners / total_winners) × prize_pool
# This guarantees portfolio EV ≤ prize_pool per contest.
# ==============================================================================

#' Evaluate portfolio EV using share-based formula
#'
#' After greedy allocation, computes realistic portfolio-level EVs that
#' properly account for self-competition among our entries within each contest.
#'
#' For each sim:
#'   1. Determine each allocated group's death_round (via forward simulation)
#'   2. Find our "best tier" = latest survival across all our entries
#'   3. Count our entries at that tier + expected field entries at that tier
#'   4. Payout = share × prize_pool (if nobody outlasts us)
#'
#' @param allocation data.table with: contest_id, group_id, team_name, team_id, n_assigned
#' @param groups data.table from group_entries()
#' @param current_slot_id Current slot
#' @param sim Sim results
#' @param tw Precomputed team wins
#' @param teams_dt Teams data table
#' @param ownership_by_slot Ownership vectors
#' @param sim_sample_size Sims to use
#' @param death_rd_cache Named list: "group_id:team_id" -> integer vector of death rounds
#'   from beam search scoring. If provided, uses these instead of re-simulating.
#' @param scoring_sample_idx Integer vector of sim indices used during scoring.
#'   Required when death_rd_cache is provided, to ensure field model uses same sims.
#' @return data.table with per-contest portfolio EVs
evaluate_portfolio_ev <- function(allocation, groups, current_slot_id,
                                   sim, tw, teams_dt, ownership_by_slot,
                                   sim_sample_size = 100000,
                                   death_rd_cache = NULL,
                                   scoring_sample_idx = NULL,
                                   field_sim_data = NULL,
                                   entry_field_data = NULL) {
  n_sims <- sim$n_sims
  n_teams <- nrow(teams_dt)
  max_round <- 6L

  # Use scoring sample_idx if provided (ensures consistency with cached death_rd)
  if (!is.null(scoring_sample_idx)) {
    sample_idx <- scoring_sample_idx
    sim_sample_size <- length(sample_idx)
  } else if (n_sims > sim_sample_size) {
    sample_idx <- sample.int(n_sims, sim_sample_size)
  } else {
    sample_idx <- seq_len(n_sims)
    sim_sample_size <- n_sims
  }

  results <- list()

  for (cid in unique(allocation$contest_id)) {
    ct_alloc <- allocation[contest_id == cid]
    ct_groups <- groups[contest_id == cid]

    prize_pool <- ct_groups$prize_pool[1]
    contest_size <- ct_groups$contest_size[1]
    our_n <- sum(ct_alloc$n_assigned)
    # Use alive field count from scrape if available
    ct_entry_field <- if (!is.null(entry_field_data)) entry_field_data[[cid]] else NULL
    ct_field_sim <- if (!is.null(field_sim_data)) field_sim_data[[cid]] else NULL
    full_field <- if (!is.null(ct_entry_field)) ct_entry_field$alive_count
                  else if (!is.null(ct_field_sim)) ct_field_sim$alive_count
                  else contest_size - our_n
    group_format <- if ("format" %in% names(ct_groups)) ct_groups$format[1] else "A"
    slot_order <- get_slot_order(group_format)
    current_idx <- match(current_slot_id, slot_order)
    remaining_slots <- slot_order[current_idx:length(slot_order)]
    n_remaining <- length(remaining_slots)
    n_alloc <- nrow(ct_alloc)
    alloc_n <- ct_alloc$n_assigned

    # --- Get death_rd for each allocation row ---
    alloc_deaths <- matrix(0L, nrow = n_alloc, ncol = sim_sample_size)
    for (ai in seq_len(n_alloc)) {
      a <- ct_alloc[ai]
      cache_key <- paste0(a$group_id, ":", a$team_id)

      if (!is.null(death_rd_cache) && !is.null(death_rd_cache[[cache_key]])) {
        # Use beam search death_rd directly (same sample_idx)
        alloc_deaths[ai, ] <- death_rd_cache[[cache_key]]
      } else {
        # Fallback: re-simulate with greedy forward sim
        g <- ct_groups[group_id == a$group_id]
        result <- forward_simulate_entry(
          a$team_id, g$used_teams[[1]], current_slot_id,
          sim, tw, teams_dt, slot_order = slot_order, format = group_format
        )
        alloc_deaths[ai, ] <- result$death_round[sample_idx]
        cat(sprintf("    WARNING: cache miss for %s (group %d, team %d), used greedy fallback\n",
                    a$team_name, a$group_id, a$team_id))
      }
    }

    # --- Field death distribution ---
    slot_round_nums <- integer(n_remaining)
    for (si in seq_along(remaining_slots)) {
      slot_round_nums[si] <- get_slot(remaining_slots[si])$round_num
    }

    if (!is.null(ct_field_sim) && !is.null(ct_field_sim$survival_curves)) {
      # Use actual field sim curves (averaged values, uniform across sims)
      fsc <- ct_field_sim$survival_curves
      n_fsc_slots <- length(fsc$field_dies_slot)
      p_field_dies_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)
      for (si in seq_along(remaining_slots)) {
        rd <- slot_round_nums[si]
        if (si <= n_fsc_slots) {
          p_field_dies_round[, rd] <- p_field_dies_round[, rd] + fsc$field_dies_slot[si]
        }
      }
      p_field_all <- rep(fsc$p_field_survives_all, sim_sample_size)
    } else {
      # Fallback: ownership-weighted field model
      slot_survive <- matrix(1, nrow = sim_sample_size, ncol = n_remaining)

      for (si in seq_along(remaining_slots)) {
        sid <- remaining_slots[si]
        slot_def <- get_slot(sid)
        n_picks <- get_n_picks(sid, group_format)

        own <- ownership_by_slot[[sid]]
        if (is.null(own) || length(own) == 0) next

        own_vec <- numeric(n_teams)
        tid <- teams_dt$team_id[match(names(own), teams_dt$name)]
        v <- !is.na(tid)
        own_vec[tid[v]] <- own[names(own)[v]]

        wm <- tw$team_round_wins[[slot_def$round_num]][sample_idx, , drop = FALSE]
        p_field_single <- as.numeric(wm %*% own_vec)
        if (n_picks > 1) {
          slot_survive[, si] <- (p_field_single / sum(own_vec)) ^ n_picks
        } else {
          slot_survive[, si] <- p_field_single
        }
      }

      cum_survive <- slot_survive
      if (n_remaining >= 2) {
        for (si in 2:n_remaining) {
          cum_survive[, si] <- cum_survive[, si - 1] * slot_survive[, si]
        }
      }

      p_field_dies_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)
      for (si in seq_along(remaining_slots)) {
        rd <- slot_round_nums[si]
        p_die <- if (si == 1) 1 - slot_survive[, si]
                 else cum_survive[, si - 1] * (1 - slot_survive[, si])
        p_field_dies_round[, rd] <- p_field_dies_round[, rd] + p_die
      }
      p_field_all <- cum_survive[, n_remaining]
    }

# --- Portfolio payout computation ---
    # Two paths: field_sim (deterministic fractions) vs ownership (probabilistic)

    # We still need our_best and our_at_best for diagnostics
    eff <- alloc_deaths
    eff[eff == 0L] <- max_round + 1L
    our_best <- if (n_alloc == 1) eff[1, ] else apply(eff, 2, max)
    our_at_best <- numeric(sim_sample_size)
    for (ai in seq_len(n_alloc)) {
      our_at_best <- our_at_best + alloc_n[ai] * (eff[ai, ] == our_best)
    }

    use_entry_field <- !is.null(ct_entry_field)
    use_field_sim <- !use_entry_field && !is.null(ct_field_sim) && !is.null(ct_field_sim$survival_curves)

    if (use_entry_field) {
      # --- ENTRY-LEVEL PATH: per-sim exact opponent counts ---
      # Derive field death distribution from n_alive_matrix
      n_alive_mat <- ct_entry_field$n_alive_matrix[sample_idx, , drop = FALSE]
      n_remaining_cols <- ncol(n_alive_mat)

      p_field_dies_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)
      prev_alive <- rep(full_field, sim_sample_size)
      for (si in seq_along(remaining_slots)) {
        rd <- slot_round_nums[si]
        if (si <= n_remaining_cols) {
          n_die <- pmax(prev_alive - n_alive_mat[, si], 0)
          p_field_dies_round[, rd] <- p_field_dies_round[, rd] + n_die / pmax(full_field, 1)
          prev_alive <- n_alive_mat[, si]
        }
      }
      p_field_all <- if (n_remaining_cols > 0) n_alive_mat[, n_remaining_cols] / pmax(full_field, 1)
                     else rep(0, sim_sample_size)

      # Use the same field_sim-style payout logic (deterministic fractions)
      n_field_outlast_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)
      n_field_same_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)

      for (d in 1:max_round) {
        outlast_frac <- p_field_all
        same_frac <- p_field_dies_round[, d]
        if (d < max_round) {
          for (rd in (d + 1):max_round) {
            outlast_frac <- outlast_frac + p_field_dies_round[, rd]
          }
        }
        n_field_outlast_round[, d] <- round(outlast_frac * full_field)
        n_field_same_round[, d] <- round(same_frac * full_field)
      }

      # Per-sim payout (same logic as field_sim path)
      payouts_per_alloc <- matrix(0, nrow = n_alloc, ncol = sim_sample_size)

      for (ai in seq_len(n_alloc)) {
        path_deaths <- eff[ai, ]

        for (d in 1:max_round) {
          mask_d <- which(path_deaths == d)
          if (length(mask_d) == 0) next
          nobody_outlasts <- n_field_outlast_round[mask_d, d] == 0
          n_same <- n_field_same_round[mask_d, d]
          our_same <- our_at_best[mask_d] - alloc_n[ai] * (our_best[mask_d] == d)
          payouts_per_alloc[ai, mask_d] <- ifelse(
            nobody_outlasts,
            prize_pool / (1 + n_same + our_same),
            0
          )
        }

        mask_surv <- which(path_deaths == (max_round + 1L))
        if (length(mask_surv) > 0) {
          n_field_surv <- round(p_field_all[mask_surv] * full_field)
          our_surv <- our_at_best[mask_surv] - alloc_n[ai] * (our_best[mask_surv] == (max_round + 1L))
          payouts_per_alloc[ai, mask_surv] <- prize_pool / (1 + n_field_surv + our_surv)
        }
      }

      portfolio_ev <- 0
      for (ai in seq_len(n_alloc)) {
        portfolio_ev <- portfolio_ev + alloc_n[ai] * mean(payouts_per_alloc[ai, ])
      }

      payouts <- numeric(sim_sample_size)
      for (ai in seq_len(n_alloc)) {
        payouts <- payouts + alloc_n[ai] * payouts_per_alloc[ai, ]
      }

    } else if (use_field_sim) {
      # --- FIELD SIM PATH: deterministic per-sim fractions ---
      # field_dies_slot/p_field_survives_all are exact fractions per sim, NOT probabilities.
      # We know exactly how many field entries die/survive in each sim.

      # Pre-compute: field entries surviving past each round (cumulative from end)
      # n_field_outlast[sim, d] = number of field entries dying after round d + surviving all
      n_field_outlast_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)
      n_field_same_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)

      for (d in 1:max_round) {
        # Entries surviving past round d = those dying in later rounds + survivors
        outlast_frac <- p_field_all  # fraction surviving all
        same_frac <- p_field_dies_round[, d]
        if (d < max_round) {
          for (rd in (d + 1):max_round) {
            outlast_frac <- outlast_frac + p_field_dies_round[, rd]
          }
        }
        n_field_outlast_round[, d] <- round(outlast_frac * full_field)
        n_field_same_round[, d] <- round(same_frac * full_field)
      }

      # Per-sim payout for each allocation
      payouts_per_alloc <- matrix(0, nrow = n_alloc, ncol = sim_sample_size)

      for (ai in seq_len(n_alloc)) {
        path_deaths <- eff[ai, ]

        for (d in 1:max_round) {
          mask_d <- which(path_deaths == d)
          if (length(mask_d) == 0) next

          # If ANY field entry outlasts us, we get $0
          # Otherwise, split with field entries dying in same round + our entries at same tier
          nobody_outlasts <- n_field_outlast_round[mask_d, d] == 0
          n_same <- n_field_same_round[mask_d, d]
          # Also count our own entries at this tier (minus this entry)
          our_same <- our_at_best[mask_d] - alloc_n[ai] * (our_best[mask_d] == d)
          payouts_per_alloc[ai, mask_d] <- ifelse(
            nobody_outlasts,
            prize_pool / (1 + n_same + our_same),
            0
          )
        }

        # Survived all: split with any field survivors + our survivors
        mask_surv <- which(path_deaths == (max_round + 1L))
        if (length(mask_surv) > 0) {
          n_field_surv <- round(p_field_all[mask_surv] * full_field)
          our_surv <- our_at_best[mask_surv] - alloc_n[ai] * (our_best[mask_surv] == (max_round + 1L))
          payouts_per_alloc[ai, mask_surv] <- prize_pool / (1 + n_field_surv + our_surv)
        }
      }

      # Portfolio EV = sum over allocations of (n_entries * mean_payout)
      portfolio_ev <- 0
      for (ai in seq_len(n_alloc)) {
        portfolio_ev <- portfolio_ev + alloc_n[ai] * mean(payouts_per_alloc[ai, ])
      }

      # Per-sim total portfolio payout for diagnostics
      payouts <- numeric(sim_sample_size)
      for (ai in seq_len(n_alloc)) {
        payouts <- payouts + alloc_n[ai] * payouts_per_alloc[ai, ]
      }

    } else {
      # --- OWNERSHIP PATH: probabilistic (original) ---
      portfolio_ev_raw <- 0

      for (ai in seq_len(n_alloc)) {
        path_deaths <- eff[ai, ]
        n_entries <- alloc_n[ai]
        path_ev_sum <- 0

        mask_surv <- which(path_deaths == (max_round + 1L))
        if (length(mask_surv) > 0) {
          field_surv <- p_field_all[mask_surv] * full_field
          path_ev_sum <- path_ev_sum + sum(prize_pool / (1 + field_surv))
        }

        for (d in 1:max_round) {
          mask_d <- which(path_deaths == d)
          if (length(mask_d) == 0) next

          p_outlast <- p_field_all[mask_d]
          if (d < max_round) {
            for (rd in (d + 1):max_round) {
              p_outlast <- p_outlast + p_field_dies_round[mask_d, rd]
            }
          }
          p_nobody <- (1 - p_outlast) ^ full_field
          field_same <- p_field_dies_round[mask_d, d] * full_field

          path_ev_sum <- path_ev_sum + sum(p_nobody * prize_pool / (1 + field_same))
        }

        portfolio_ev_raw <- portfolio_ev_raw + (path_ev_sum / sim_sample_size) * n_entries
      }

      discount_factor <- full_field / contest_size
      portfolio_ev <- portfolio_ev_raw * discount_factor
      payouts <- rep(portfolio_ev / our_n, sim_sample_size)
    }

    # --- Diagnostics (compact) ---
    # Build team distribution summary
    team_dist <- ct_alloc[, .(n = sum(n_assigned)), by = team_name]
    setorder(team_dist, -n)
    dist_str <- paste(sprintf("%s(%d)", team_dist$team_name, team_dist$n), collapse = ", ")

    cat(sprintf("  [Portfolio EV] %s: %d entries, field=%d, prize=$%s\n",
                substr(cid, 1, 14), our_n, full_field,
                format(prize_pool, big.mark = ",")))
    cat(sprintf("    Picks: %s\n", dist_str))
    cat(sprintf("    => Portfolio EV = $%.2f ($%.2f/entry)\n", portfolio_ev, portfolio_ev / our_n))

    results[[length(results) + 1]] <- data.table(
      contest_id   = cid,
      prize_pool   = prize_pool,
      our_entries  = our_n,
      portfolio_ev = portfolio_ev,
      ev_per_entry = portfolio_ev / our_n,
      pct_of_pool  = 100 * portfolio_ev / prize_pool
    )
  }

  rbindlist(results)
}

# ==============================================================================
# STEP 4: PORTFOLIO ALLOCATION
# ==============================================================================

#' Optimize today's allocation of entries across candidate teams
#'
#' For each group: precompute context once, then score all candidates cheaply.
#'
#' @param state data.table of entry state
#' @param candidates Character vector of team names available today
#' @param current_slot_id Current slot ID
#' @param sim Sim results list
#' @param tw Precomputed team wins
#' @param teams_dt Teams data frame
#' @param ownership_by_slot List of ownership vectors per slot
#' @param sim_sample_size Sims to use per EV calculation
#' @return data.table allocation
optimize_today <- function(state, candidates, current_slot_id, sim, tw,
                            teams_dt, ownership_by_slot,
                            sim_sample_size = 100000,
                            field_sim_data = NULL,
                            locked_team_ids = integer(0),
                            entry_field_data = NULL,
                            method = "beam") {
  groups <- group_entries(state)
  if (nrow(groups) == 0) {
    cat("No alive entries to optimize.\n")
    return(data.table())
  }

  slot <- get_slot(current_slot_id)
  ownership <- ownership_by_slot[[current_slot_id]]
  candidate_ids <- teams_dt$team_id[match(candidates, teams_dt$name)]

  cat(sprintf("\n=== OPTIMIZING: %s ===\n", slot$label))
  cat(sprintf("Candidates: %d teams | Alive entries: %d | Groups: %d\n",
              length(candidates), sum(state$alive), nrow(groups)))

  # Subsample sims once (shared across all groups for consistency)
  n_sims <- sim$n_sims
  if (n_sims > sim_sample_size) {
    sample_idx <- sample.int(n_sims, sim_sample_size)
  } else {
    sample_idx <- seq_len(n_sims)
  }

  # Prune candidates: skip teams with very low win probability
  rd1 <- get_slot(current_slot_id)$round_num
  min_wp <- 0.20  # 20% threshold — skip longshots
  cat(sprintf("\n--- CANDIDATE PRUNING DEBUG ---\n"))
  cat(sprintf("  current_slot_id=%s, round_num=%d\n", current_slot_id, rd1))
  cat(sprintf("  tw$team_round_wins has %d rounds\n", length(tw$team_round_wins)))
  cat(sprintf("  tw$team_round_wins[[%d]] dims: %s\n", rd1,
              paste(dim(tw$team_round_wins[[rd1]]), collapse=" x ")))
  cat(sprintf("  sample_idx length: %d, range: [%d, %d]\n",
              length(sample_idx), min(sample_idx), max(sample_idx)))
  cat(sprintf("  candidate_ids (%d): %s\n", length(candidate_ids),
              paste(head(candidate_ids, 10), collapse=", ")))
  # Check win probs for first few candidates
  for (tid in head(candidate_ids, 5)) {
    wp <- mean(tw$team_round_wins[[rd1]][sample_idx, tid])
    cat(sprintf("  team %d (%s): R%d win prob = %.4f\n",
                tid, teams_dt$name[tid], rd1, wp))
  }
  viable_cids <- candidate_ids[sapply(candidate_ids, function(tid) {
    mean(tw$team_round_wins[[rd1]][sample_idx, tid]) >= min_wp
  })]
  cat(sprintf("  After win prob filter: %d viable\n", length(viable_cids)))
  # Remove locked teams (games already started)
  if (length(locked_team_ids) > 0) {
    locked_names <- teams_dt$name[locked_team_ids]
    cat(sprintf("  Locked team IDs: %s\n", paste(locked_team_ids, collapse = ", ")))
    cat(sprintf("  Locked team names: %s\n", paste(locked_names, collapse = ", ")))
    before_lock <- length(viable_cids)
    viable_cids <- viable_cids[!viable_cids %in% locked_team_ids]
    cat(sprintf("  Removed %d locked teams from viable\n", before_lock - length(viable_cids)))
  }
  cat(sprintf("Pruned candidates: %d -> %d teams (>%.0f%% win prob, %d locked)\n",
              length(candidate_ids), length(viable_cids), min_wp * 100, length(locked_team_ids)))
  cat(sprintf("  Final viable: %s\n", paste(teams_dt$name[viable_cids], collapse = ", ")))

  # Score each candidate for each group
  all_scores <- list()
  death_rd_cache <- list()  # keyed by "group_id:team_id" -> integer vector
  overall_start <- proc.time()[["elapsed"]]

  if (method == "mc") {
    # ====================================================================
    # FORWARD-KNOWLEDGE MC SCORING
    # For each group × each sim, find the optimal survivable path with
    # full knowledge of sim outcomes. Resolves payouts against
    # n_field_alive_matrix. No beam search, no V_die approximation.
    # ====================================================================
    cat("\n=== MC SCORING (forward-knowledge optimal paths) ===\n")

    if (is.null(entry_field_data)) {
      stop("method='mc' requires entry_field_data (entry-level field simulation)")
    }

    mc_low_ev_printed <- FALSE
    mc_high_ev_printed <- FALSE

    for (gi in seq_len(nrow(groups))) {
      g <- groups[gi]
      if (gi == 1 || g$contest_id != groups[gi - 1]$contest_id) {
        n_contest_groups <- sum(groups$contest_id == g$contest_id)
        ct_entry_tmp <- entry_field_data[[g$contest_id]]
        if (!is.null(ct_entry_tmp)) {
          nfa_final <- ct_entry_tmp$n_alive_matrix[sample_idx, ncol(ct_entry_tmp$n_alive_matrix)]
          prize <- g$prize_pool
          ev_per_survivor <- mean(prize / (1 + nfa_final))
          cat(sprintf("\n--- Contest %s: %d groups, %d field, E[prize/(1+n_alive_final)]=$%.0f ---\n",
                      substr(g$contest_id, 1, 12), n_contest_groups, g$contest_size, ev_per_survivor))
        } else {
          cat(sprintf("\n--- Contest %s: %d groups, %d field ---\n",
                      substr(g$contest_id, 1, 12), n_contest_groups, g$contest_size))
        }
      }

      # Get entry-level field data for this contest
      ct_entry <- entry_field_data[[g$contest_id]]
      if (is.null(ct_entry)) {
        cat(sprintf("  WARNING: No entry field data for contest %s, skipping\n",
                    g$contest_id))
        next
      }
      ct_n_alive_matrix <- ct_entry$n_alive_matrix[sample_idx, , drop = FALSE]

      # Filter viable candidates for this group
      avail_cids <- viable_cids[!viable_cids %in% g$used_teams[[1]]]

      # Apply format-specific region constraints
      group_format <- if ("format" %in% names(g)) g$format else "A"
      if (group_format == "C" && grepl("R1_d2", current_slot_id)) {
        used_tids <- g$used_teams[[1]]
        if (length(used_tids) > 0) {
          used_regions <- unique(teams_dt$region[used_tids])
          allowed_regions <- setdiff(unique(teams_dt$region), used_regions)
          if (length(allowed_regions) > 0) {
            avail_cids <- avail_cids[teams_dt$region[avail_cids] %in% allowed_regions]
          }
        }
      }

      if (length(avail_cids) == 0) next

      # Remove locked teams
      if (length(locked_team_ids) > 0) {
        avail_cids <- avail_cids[!avail_cids %in% locked_team_ids]
      }

      if (length(avail_cids) == 0) next

      mc_start <- proc.time()[["elapsed"]]
      mc_result <- mc_score_candidates(
        group            = g,
        current_slot_id  = current_slot_id,
        sim              = sim,
        teams_dt         = teams_dt,
        sample_idx       = sample_idx,
        n_field_alive_matrix = ct_n_alive_matrix,
        viable_cids      = avail_cids,
        format           = group_format
      )
      mc_elapsed <- proc.time()[["elapsed"]] - mc_start

      if (nrow(mc_result) == 0) next

      # Cache death_rounds from MC for portfolio EV computation
      mc_death_rounds <- attr(mc_result, "death_rounds")
      if (!is.null(mc_death_rounds)) {
        for (k in seq_len(nrow(mc_result))) {
          cache_key <- paste0(g$group_id, ":", mc_result$team_id[k])
          death_rd_cache[[cache_key]] <- as.integer(mc_death_rounds[k, ])
        }
      } else if (gi == 1) {
        cat("  WARNING: death_rounds not returned by C++ — recompile simulate_tourney.cpp!\n")
      }

      # Diagnostic: show used_teams for groups with very low EV
      top_ev <- max(mc_result$ev)
      if (top_ev < 0.10) {
        best_k <- which.max(mc_result$ev)
        used_names <- teams_dt$name[g$used_teams[[1]]]
        cat(sprintf("  Group %d/%d: %d candidates scored in %.1fs (top: %s=$%.2f  p_surv=%.4f) [LOW EV] used_teams: %s\n",
                    gi, nrow(groups), nrow(mc_result), mc_elapsed,
                    mc_result$team_name[best_k],
                    top_ev,
                    mc_result$p_survive_all[best_k],
                    paste(used_names, collapse = ", ")))

        # Detailed debug for first low-EV group per contest
        if (gi == 1 || !isTRUE(mc_low_ev_printed)) {
          mc_low_ev_printed <- TRUE
          gfmt <- if ("format" %in% names(g)) g$format else "A"
          so <- get_slot_order(gfmt)
          cidx <- match(current_slot_id, so)
          rem <- so[cidx:length(so)]
          n_sims_mc <- length(sample_idx)
          cat(sprintf("    --- LOW EV DEBUG (group %d, %d sims) ---\n", g$group_id, n_sims_mc))
          cat(sprintf("    Remaining slots: %s\n", paste(rem, collapse = " -> ")))
          cat(sprintf("    n_field_alive dims: %d x %d\n", nrow(ct_n_alive_matrix), ncol(ct_n_alive_matrix)))
          nfa_means <- colMeans(ct_n_alive_matrix)
          cat(sprintf("    n_field_alive means: %s\n",
                      paste(sprintf("%.1f", nfa_means), collapse = ", ")))

          # Per-candidate summary from mc_result
          for (k in seq_len(nrow(mc_result))) {
            r <- mc_result[k]
            cat(sprintf("    %s: EV=$%.4f  p_today=%.3f  p_survive_all=%.6f  mean_death=%.2f\n",
                        r$team_name, r$ev, r$p_survive_today, r$p_survive_all, r$mean_death_rd))
          }

          # Check death_rounds attribute
          dr <- attr(mc_result, "death_rounds")
          if (!is.null(dr)) {
            cat(sprintf("    death_rounds: %d x %d matrix\n", nrow(dr), ncol(dr)))
            # Show death distribution for top candidate
            best_k <- which.max(mc_result$ev)
            dr_k <- dr[best_k, ]
            death_tab <- table(factor(dr_k, levels = 0:6))
            cat(sprintf("    %s death dist: [%s]\n",
                        mc_result$team_name[best_k],
                        paste(sprintf("rd%d=%d", 0:6, death_tab), collapse = " ")))
          } else {
            cat("    death_rounds: NULL (C++ not recompiled?)\n")
          }
          cat("    ---\n")
        }
      } else {
        best_k <- which.max(mc_result$ev)
        r <- mc_result[best_k]
        used_names <- teams_dt$name[g$used_teams[[1]]]
        # Compact diagnostics: EV, p_surv, conditional n_alive, conditional payout, max payout, die_zero count
        surv_info <- ""
        if ("mean_n_alive_surv" %in% names(mc_result) && !is.na(r$mean_n_alive_surv)) {
          surv_info <- sprintf("  E[n_alive|surv]=%.0f  E[payout|surv]=$%.0f  max=$%.0f  die0=%d",
                               r$mean_n_alive_surv, r$mean_payout_surv,
                               r$max_payout, r$die_zero_alive)
        }
        cat(sprintf("  Group %d/%d: %d cands in %.1fs (top: %s=$%.2f  p_surv=%.4f%s) | used: %s\n",
                    gi, nrow(groups), nrow(mc_result), mc_elapsed,
                    r$team_name, top_ev, r$p_survive_all,
                    surv_info,
                    paste(used_names, collapse = ", ")))
      }

      # Convert to all_scores format
      for (k in seq_len(nrow(mc_result))) {
        r <- mc_result[k]
        score_row <- data.table(
          group_id        = g$group_id,
          contest_id      = g$contest_id,
          n_entries       = g$n_entries,
          team_name       = r$team_name,
          team_id         = r$team_id,
          ev              = r$ev,
          ev_raw          = r$ev,
          optionality     = NA_real_,
          p_survive_today = r$p_survive_today,
          p_win_contest   = r$p_win_contest,
          mean_death_rd   = r$mean_death_rd,
          slot1_extra_name = NA_character_,
          group_id_orig   = g$group_id
        )
        all_scores[[length(all_scores) + 1]] <- score_row
      }
    }

    total_elapsed <- proc.time()[["elapsed"]] - overall_start
    if (total_elapsed < 60) {
      time_str <- sprintf("%.1fs", total_elapsed)
    } else {
      time_str <- sprintf("%.1fm", total_elapsed / 60)
    }
    cat(sprintf("\n=== MC scoring complete in %s ===\n", time_str))

  } else {
  # ====================================================================
  # BEAM SEARCH SCORING (original path)
  # ====================================================================

  # Cache precomputed context per contest (groups in same contest share V_die/V_survive)
  ctx_cache <- list()

  # Cache scores by contest+available_candidates fingerprint
  # Groups in the same contest with same available candidates get identical beam results
  score_cache <- list()

  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    # Print progress: one line per new contest, suppress per-group spam
    if (gi == 1 || g$contest_id != groups[gi - 1]$contest_id) {
      n_contest_groups <- sum(groups$contest_id == g$contest_id)
      cat(sprintf("\n--- Contest %s: %d groups, %d field ---\n",
                  substr(g$contest_id, 1, 12), n_contest_groups, g$contest_size))
    }

    # Precompute group context — cache by contest_id since field sim is per-contest
    ct_fsd <- if (!is.null(field_sim_data)) field_sim_data[[g$contest_id]] else NULL
    ct_survival <- if (!is.null(ct_fsd)) ct_fsd$survival_curves else NULL
    ct_alive <- if (!is.null(ct_fsd)) ct_fsd$alive_count else NULL

    # Entry-level field data (per-sim exact counts) — takes priority over field_sim
    ct_entry <- if (!is.null(entry_field_data)) entry_field_data[[g$contest_id]] else NULL
    ct_n_alive_matrix <- NULL
    if (!is.null(ct_entry)) {
      # Subsample to match optimizer's sample_idx
      ct_n_alive_matrix <- ct_entry$n_alive_matrix[sample_idx, , drop = FALSE]
      if (is.null(ct_alive)) ct_alive <- ct_entry$alive_count
    }

    ctx_start <- proc.time()[["elapsed"]]
    ctx <- precompute_group_context(
      g, current_slot_id, tw, teams_dt,
      ownership_by_slot, sample_idx,
      field_survival_curves = ct_survival,
      alive_field_count = ct_alive,
      n_field_alive_matrix = ct_n_alive_matrix
    )
    ctx_elapsed <- proc.time()[["elapsed"]] - ctx_start
    ctx$locked_team_ids <- locked_team_ids

    # Print diagnostic: beam search forward sim for several candidates in first group
    # Only print when verbose = TRUE (default FALSE for cleaner output)
    verbose <- getOption("splash.verbose", FALSE)
    if (verbose && gi == 1) {
      avail <- viable_cids[!viable_cids %in% g$used_teams[[1]]]
      # Apply region constraint for format C (same as scoring loop)
      diag_format <- if ("format" %in% names(g)) g$format else "A"
      if (diag_format == "C" && grepl("R1_d2", current_slot_id)) {
        used_tids <- g$used_teams[[1]]
        if (length(used_tids) > 0) {
          used_reg <- unique(teams_dt$region[used_tids])
          allowed_reg <- setdiff(unique(teams_dt$region), used_reg)
          if (length(allowed_reg) > 0) {
            avail <- avail[teams_dt$region[avail] %in% allowed_reg]
          }
        }
      }
      if (length(avail) == 0) {
        cat("  No viable candidates for diagnostics after region filtering.\n")
      } else {
      # Pick 3 diverse candidates by seed: a top seed, a mid seed, and a low seed
      avail_seeds <- teams_dt$seed[avail]
      diag_cids <- c(
        avail[which.min(avail_seeds)],              # best seed (1-seed)
        avail[which.min(abs(avail_seeds - 5))],     # ~5-seed
        avail[which.min(abs(avail_seeds - 9))]      # ~9-seed
      )
      diag_cids <- unique(diag_cids)

      for (dc in diag_cids) {
        diag <- compute_candidate_ev(dc, ctx, tw, diagnostics = TRUE)
        dc_seed <- teams_dt$seed[dc]
        cat(sprintf("\n  === Beam Search for %s (%d-seed) ===\n",
                    teams_dt$name[dc], dc_seed))

        if (is.null(diag$pick_names)) {
          # Early-return path (no valid beam)
          cat(sprintf("  => EV=$%.2f (no valid beam paths)\n", diag$ev))
          next
        }

        cat(sprintf("  %-10s %3s %10s %10s  %-30s %6s\n",
                    "Slot", "Rd", "Field_Surv", "Our_Surv", "Pick(s)", "WinP%"))
        cat(sprintf("  %s\n", strrep("-", 85)))
        for (si in seq_along(ctx$remaining_slots)) {
          pick_str <- diag$pick_names[si]
          si_key <- as.character(si)
          if (!is.null(diag$extra_picks[[si_key]])) {
            pick_str <- paste(c(sprintf("%s (%.1f%%)", pick_str,
                                100 * diag$pick_wp[si]),
                                diag$extra_picks[[si_key]]), collapse = " + ")
            cat(sprintf("  %-10s %3d %10.4f %10.4f  %-30s\n",
                        ctx$remaining_slots[si], ctx$slot_round_nums[si],
                        diag$field_slot_survive_mean[si],
                        diag$our_slot_survive_mean[si],
                        pick_str))
          } else {
            cat(sprintf("  %-10s %3d %10.4f %10.4f  %-30s %5.1f%%\n",
                        ctx$remaining_slots[si], ctx$slot_round_nums[si],
                        diag$field_slot_survive_mean[si],
                        diag$our_slot_survive_mean[si],
                        pick_str,
                        100 * diag$pick_wp[si]))
          }
        }
        n_survive <- round(diag$p_survive_all * length(ctx$sample_idx))
        cat(sprintf("  => EV=$%.2f, P(survive all)=%.6f%% (%d/%d sims), AvgDeath=%.1f, Beams=%d\n",
                    diag$ev, 100 * diag$p_survive_all, n_survive,
                    length(ctx$sample_idx), diag$mean_death_rd,
                    diag$n_beam_paths))
        cat(sprintf("  --- EV Breakdown (field=%d, prize=$%s) ---\n",
                    diag$full_field, format(diag$prize_pool, big.mark = ",")))
        cat(sprintf("  Case 1 (survive all): $%.2f\n", diag$ev_case1))
        cat(sprintf("  %-8s %10s %12s %12s %12s\n",
                    "Round", "P(we die)", "V_die(beam)", "EV share", "Cumul EV"))
        cumul <- diag$ev_case1
        for (d in 1:6) {
          cumul <- cumul + diag$ev_by_round[d]
          cat(sprintf("  Rd %-3d  %9.4f%% %11.2f %11.2f %11.2f\n",
                      d, 100 * diag$our_death_dist[d],
                      diag$mean_V_die[d], diag$ev_by_round[d], cumul))
        }
        cat(sprintf("  P(field survives all) = %.6f%%\n",
                    100 * diag$mean_p_field_all))
      }
      cat("\n")
      } # end else (avail > 0)
    }

    # Score all candidates for this group (using pruned viable candidates)
    avail_cids <- viable_cids[!viable_cids %in% g$used_teams[[1]]]

    # Region constraints for format C:
    # - R1_d2: picks must be from regions not used in R1_d1 (span all 4 by end of R1)
    # - R2_d1: 2 picks today must be from DIFFERENT regions
    # - R2_d2: 2 picks must be from regions not used in R2_d1 (span all 4 by end of R2)
    group_format <- if ("format" %in% names(g)) g$format else "A"
    if (group_format == "C") {
      if (grepl("R1_d2", current_slot_id)) {
        used_tids <- g$used_teams[[1]]
        if (length(used_tids) > 0) {
          used_regions <- unique(teams_dt$region[used_tids])
          allowed_regions <- setdiff(unique(teams_dt$region), used_regions)
          if (length(allowed_regions) > 0) {
            avail_cids <- avail_cids[teams_dt$region[avail_cids] %in% allowed_regions]
            cat(sprintf("  Format C region constraint (R1_d2): used=%s, allowed=%s (%d teams)\n",
                        paste(used_regions, collapse = ","),
                        paste(allowed_regions, collapse = ","),
                        length(avail_cids)))
          }
        }
      } else if (grepl("R2", current_slot_id)) {
        # For R2: enforce that today's 2 picks are from different regions.
        # The companion pick handler (slot1_n_picks > 1) already enforces this
        # at lines 734-742, but we also want to prefer unused regions.
        # Don't hard-filter primaries here — the companion logic handles pairing.
        # But for R2_d2: must pick from regions not used in R2_d1.
        if (grepl("R2_d2", current_slot_id)) {
          # Get R2_d1 picks (most recent 2 used teams from R2 round)
          used_tids <- g$used_teams[[1]]
          all_slots_fmt <- get_slot_order("C")
          r2d1_idx <- match("R2_d1", all_slots_fmt)
          # R2_d1 picks are the ones added after R1 slots
          # For format C: R1_d1 (2 picks) + R1_d2 (2 picks) = 4 used, then R2_d1 adds 2
          # So R2_d1 picks are used_tids[5:6] if 6 total
          n_prior <- 4  # R1 has 4 picks in format C
          if (length(used_tids) > n_prior) {
            r2d1_tids <- used_tids[(n_prior + 1):length(used_tids)]
            r2d1_regions <- unique(teams_dt$region[r2d1_tids])
            allowed_regions <- setdiff(unique(teams_dt$region), r2d1_regions)
            if (length(allowed_regions) > 0) {
              avail_cids <- avail_cids[teams_dt$region[avail_cids] %in% allowed_regions]
              cat(sprintf("  Format C region constraint (R2_d2): R2_d1 regions=%s, allowed=%s (%d teams)\n",
                          paste(r2d1_regions, collapse = ","),
                          paste(allowed_regions, collapse = ","),
                          length(avail_cids)))
            }
          }
        }
      }
    }
    # Build cache key: contest_id + sorted available candidate IDs
    # Groups with identical contest + available candidates produce identical beam results
    cache_fingerprint <- paste0(g$contest_id, ":", paste(sort(avail_cids), collapse = ","))

    if (!is.null(score_cache[[cache_fingerprint]])) {
      # Reuse cached scores — just update group_id and n_entries
      cached <- score_cache[[cache_fingerprint]]
      for (cs in cached) {
        cs_copy <- copy(cs)
        cs_copy$group_id <- g$group_id
        cs_copy$n_entries <- g$n_entries
        all_scores[[length(all_scores) + 1]] <- cs_copy

        # Also cache death_rd
        cache_key <- paste0(g$group_id, ":", cs$team_id)
        orig_key <- paste0(cached[[1]]$group_id_orig, ":", cs$team_id)
        death_rd_cache[[cache_key]] <- death_rd_cache[[orig_key]]
      }
      # (cached — no recomputation needed)
    } else {
      # --- Compute path optionality for all candidates in this group ---
      # Derive used_rounds from the slot structure: each used team was picked
      # in a specific round, determined by which slot column it appears in.
      group_used_rounds <- integer(0)
      if (length(g$used_teams[[1]]) > 0) {
        # Map used teams to their pick rounds using completed slot order
        all_slots <- get_slot_order(if ("format" %in% names(g)) g$format else "A")
        completed <- setdiff(all_slots, ctx$remaining_slots)
        for (cs in completed) {
          slot_info <- get_slot(cs)
          group_used_rounds <- c(group_used_rounds, slot_info$round_num)
        }
        # Trim to match length of used_teams (in case of multi-pick slots
        # where one slot contributes multiple picks)
        if (length(group_used_rounds) < length(g$used_teams[[1]])) {
          # Pad with the last round for multi-pick extras
          group_used_rounds <- c(group_used_rounds,
            rep(tail(group_used_rounds, 1),
                length(g$used_teams[[1]]) - length(group_used_rounds)))
        } else if (length(group_used_rounds) > length(g$used_teams[[1]])) {
          group_used_rounds <- group_used_rounds[seq_along(g$used_teams[[1]])]
        }
      }

      optionality_scores <- compute_path_optionality(
        candidate_ids    = avail_cids,
        group_used_teams = g$used_teams[[1]],
        group_used_rounds = group_used_rounds,
        remaining_slots  = ctx$remaining_slots,
        slot_round_nums  = ctx$slot_round_nums,
        tw               = tw,
        teams_dt         = teams_dt,
        sample_idx       = ctx$sample_idx
      )
      names(optionality_scores) <- avail_cids
      cat(sprintf("  Path optionality: range [%.2f, %.2f] across %d candidates\n",
                  min(optionality_scores), max(optionality_scores), length(avail_cids)))

      cached_scores <- list()
      pg_cand <- make_progress(length(avail_cids),
                               sprintf("Group %d/%d candidates", gi, nrow(groups)),
                               update_every = 1)
      for (cid in avail_cids) {
        cname <- teams_dt$name[cid]
        ev_result <- compute_candidate_ev(cid, ctx, tw)

        # Cache beam search death_rd for portfolio eval
        cache_key <- paste0(g$group_id, ":", cid)
        death_rd_cache[[cache_key]] <- ev_result$our_death_rd

        # Track companion picks for multi-pick current slot
        extra_names <- if (length(ev_result$slot1_extra_ids) > 0) {
          paste(teams_dt$name[ev_result$slot1_extra_ids], collapse = ", ")
        } else NA_character_

        # Combine beam search EV with path optionality bonus.
        # optionality_weight calibrated so a 2x branching factor difference
        # (~0.69 per slot * ~7 slots ≈ 4.8 total) maps to ~$0.50 adjustment.
        # Configurable via options(splash.optionality_weight = 0.15)
        optionality_weight <- getOption("splash.optionality_weight", 0.10)
        opt_score <- optionality_scores[as.character(cid)]
        adjusted_ev <- ev_result$ev + optionality_weight * opt_score

        # Pick-against bonus for format C: encourage picking opponents of used
        # teams. This eliminates the used team from the bracket and opens
        # future paths. Bonus scales with how favorable the matchup is.
        if (group_format == "C" && length(g$used_teams[[1]]) > 0) {
          current_round <- get_slot(current_slot_id)$round_num
          for (ut in g$used_teams[[1]]) {
            if (earliest_meeting_round(cid, ut) == current_round) {
              # Candidate plays directly against a used team this round.
              # Bonus proportional to candidate's win probability (favored = bigger bonus)
              cand_wp <- mean(tw$team_round_wins[[current_round]][ctx$sample_idx, cid])
              pick_against_bonus <- 0.15 * cand_wp  # ~$0.10-0.14 for strong favorites
              adjusted_ev <- adjusted_ev + pick_against_bonus
              break
            }
          }
        }

        score_row <- data.table(
          group_id        = g$group_id,
          contest_id      = g$contest_id,
          n_entries       = g$n_entries,
          team_name       = cname,
          team_id         = cid,
          ev              = adjusted_ev,
          ev_raw          = ev_result$ev,
          optionality     = opt_score,
          p_survive_today = ev_result$p_survive_today,
          p_win_contest   = ev_result$p_win_contest,
          mean_death_rd   = ev_result$mean_death_rd,
          slot1_extra_name = extra_names,
          group_id_orig   = g$group_id  # for death_rd_cache lookup
        )
        all_scores[[length(all_scores) + 1]] <- score_row
        cached_scores[[length(cached_scores) + 1]] <- score_row
        pg_cand$tick()
      }
      pg_cand$done()
      score_cache[[cache_fingerprint]] <- cached_scores
    }
  }

  total_elapsed <- proc.time()[["elapsed"]] - overall_start
  if (total_elapsed < 60) {
    time_str <- sprintf("%.1fs", total_elapsed)
  } else {
    time_str <- sprintf("%.1fm", total_elapsed / 60)
  }
  cat(sprintf("\n=== Scoring complete in %s ===\n", time_str))

  }  # end else (beam search path)

  scores <- rbindlist(all_scores)

  if (nrow(scores) == 0) {
    cat("No valid candidates for any group.\n")
    return(data.table())
  }

  # Print top candidates summary (one line per contest, not per group)
  contest_ids <- unique(scores$contest_id)
  for (cid in contest_ids) {
    c_scores <- scores[contest_id == cid]
    # Deduplicate to unique candidates (take best EV per team)
    c_best <- c_scores[, .SD[which.max(ev)], by = team_name]
    setorder(c_best, -ev)
    n_groups <- uniqueN(c_scores$group_id)
    cat(sprintf("\n  Contest %s (%d groups) — Top 5:\n", substr(cid, 1, 12), n_groups))
    top_n <- min(5, nrow(c_best))
    for (k in 1:top_n) {
      r <- c_best[k]
      name_str <- r$team_name
      if (!is.na(r$slot1_extra_name)) name_str <- paste0(name_str, " + ", r$slot1_extra_name)
      opt_str <- if ("optionality" %in% names(r) && !is.na(r$optionality)) {
        sprintf("  Opt=%.1f", r$optionality)
      } else ""
      raw_str <- if ("ev_raw" %in% names(r) && !is.na(r$ev_raw)) {
        sprintf("  (raw=$%.2f)", r$ev_raw)
      } else ""
      cat(sprintf("    %2d. %-35s EV=$%6.2f%s  WinToday=%5.1f%%  WinContest=%.3f%%  AvgDeath=%.1f%s\n",
                  k, name_str, r$ev, raw_str, 100 * r$p_survive_today,
                  100 * r$p_win_contest, r$mean_death_rd, opt_str))
    }
  }

# --- Greedy assignment (Marginal EV Maximization) ---
  allocation_list <- list()
  cat("\n--- Running Portfolio Diversification Allocator ---\n")

  # ---- Collect all groups per contest for joint allocation ----
  contest_groups <- list()
  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    g_scores <- scores[group_id == g$group_id]
    if (nrow(g_scores) == 0) next
    setorder(g_scores, -ev)

    cid <- g$contest_id
    if (is.null(contest_groups[[cid]])) {
      contest_groups[[cid]] <- list(groups = list(), scores = list())
    }
    contest_groups[[cid]]$groups[[length(contest_groups[[cid]]$groups) + 1]] <- g
    contest_groups[[cid]]$scores[[length(contest_groups[[cid]]$scores) + 1]] <- g_scores
  }

  for (cid in names(contest_groups)) {
    cg <- contest_groups[[cid]]
    total_entries <- sum(sapply(cg$groups, function(g) g$n_entries))

    # Combine all unique candidates across groups in this contest
    all_cand <- unique(rbindlist(cg$scores)[, .(team_name, team_id, ev,
      p_survive_today, p_win_contest, slot1_extra_name)])
    setorder(all_cand, -ev)

    # Keep top candidates (enough for good diversification)
    top_viable <- min(20, sum(all_cand$ev > 0.01))
    if (top_viable == 0) top_viable <- 1
    all_cand <- all_cand[1:top_viable]

    cat(sprintf("  Contest %s: %d entries, %d groups, %d viable candidates\n",
                substr(cid, 1, 12), total_entries, length(cg$groups), nrow(all_cand)))

    # ---- Allocate per group, but with portfolio-wide concentration penalty ----
    # Track how many entries are already assigned to each team across ALL groups
    portfolio_counts <- setNames(rep(0L, nrow(all_cand)), all_cand$team_name)

    # Path diversity: track "team:used_teams_hash" -> count to penalize
    # entries with the same used_teams all picking the same candidate
    path_diversity_weight <- 3.0
    path_counts <- list()

    for (gi_idx in seq_along(cg$groups)) {
      g <- cg$groups[[gi_idx]]
      g_scores <- cg$scores[[gi_idx]]
      n_ent <- g$n_entries

      # Filter to candidates available for this group (not in used_teams)
      avail_mask <- !g_scores$team_id %in% g$used_teams[[1]]
      g_avail <- g_scores[avail_mask]
      if (nrow(g_avail) == 0) {
        g_avail <- g_scores[1]  # fallback
      }
      setorder(g_avail, -ev)

      # Keep top candidates for this group
      g_top_n <- min(8, nrow(g_avail))
      g_top <- g_avail[1:g_top_n]

      if (n_ent <= 1 || g_top_n <= 1) {
        # Small group: just pick the best
        best <- g_top[1]
        allocation_list[[length(allocation_list) + 1]] <- data.table(
          contest_id      = g$contest_id,
          group_id        = g$group_id,
          team_name       = best$team_name,
          team_id         = best$team_id,
          n_assigned      = n_ent,
          ev              = best$ev,
          p_survive_today = best$p_survive_today,
          p_win_contest   = best$p_win_contest,
          slot1_extra_name = best$slot1_extra_name,
          used_teams      = list(g$used_teams[[1]])
        )
        portfolio_counts[best$team_name] <- portfolio_counts[best$team_name] + n_ent
        ut_hash_small <- paste(sort(g$used_teams[[1]]), collapse = ",")
        path_key_small <- paste0(best$team_name, ":", ut_hash_small)
        path_counts[[path_key_small]] <- (path_counts[[path_key_small]] %||% 0L) + n_ent
        next
      }

      # ---- Proportional allocation based on EV with concentration penalty ----
      # Target: spread entries proportional to EV^alpha, penalized by existing exposure
      # Alpha < 1 flattens distribution (more diversification)
      alpha <- 2.5  # Tunable: higher = more concentration on top EV teams
      concentration_penalty <- 0.5  # How much to penalize already-heavy teams

      base_weights <- pmax(g_top$ev, 0) ^ alpha
      # Penalize teams that already have many entries in the portfolio
      for (k in seq_len(g_top_n)) {
        tn <- g_top$team_name[k]
        existing <- portfolio_counts[tn]
        if (!is.na(existing) && existing > 0) {
          # Decay weight by factor of (1 / (1 + penalty * existing_fraction))
          existing_frac <- existing / max(total_entries, 1)
          base_weights[k] <- base_weights[k] / (1 + concentration_penalty * existing / 5)
        }
      }

      # Path diversity: penalize assigning entries with the same used_teams
      # to the same candidate — they would share identical future paths
      ut_hash <- paste(sort(g$used_teams[[1]]), collapse = ",")
      for (k in seq_len(g_top_n)) {
        path_key <- paste0(g_top$team_name[k], ":", ut_hash)
        path_existing <- path_counts[[path_key]]
        if (!is.null(path_existing) && path_existing > 0) {
          base_weights[k] <- base_weights[k] /
            (1 + path_diversity_weight * path_existing / max(n_ent, 1))
        }
      }

      if (sum(base_weights) == 0) base_weights <- rep(1, g_top_n)
      target_frac <- base_weights / sum(base_weights)

      # Convert fractions to integer allocations (largest remainder method)
      raw_alloc <- target_frac * n_ent
      floor_alloc <- floor(raw_alloc)
      remainders <- raw_alloc - floor_alloc
      leftover <- n_ent - sum(floor_alloc)
      if (leftover > 0) {
        top_rem <- order(remainders, decreasing = TRUE)[1:leftover]
        floor_alloc[top_rem] <- floor_alloc[top_rem] + 1L
      }

      # Append non-zero allocations
      for (k in seq_len(g_top_n)) {
        if (floor_alloc[k] > 0) {
          allocation_list[[length(allocation_list) + 1]] <- data.table(
            contest_id      = g$contest_id,
            group_id        = g$group_id,
            team_name       = g_top$team_name[k],
            team_id         = g_top$team_id[k],
            n_assigned      = as.integer(floor_alloc[k]),
            ev              = g_top$ev[k],
            p_survive_today = g_top$p_survive_today[k],
            p_win_contest   = g_top$p_win_contest[k],
            slot1_extra_name = g_top$slot1_extra_name[k],
            used_teams      = list(g$used_teams[[1]])
          )
          tn <- g_top$team_name[k]
          portfolio_counts[tn] <- portfolio_counts[tn] + floor_alloc[k]

          # Update path diversity tracker
          path_key <- paste0(tn, ":", ut_hash)
          path_counts[[path_key]] <- (path_counts[[path_key]] %||% 0L) +
            as.integer(floor_alloc[k])
        }
      }
    }

    # Print portfolio distribution
    active <- portfolio_counts[portfolio_counts > 0]
    cat(sprintf("  Portfolio spread: %d unique teams\n", length(active)))
    for (tn in names(sort(active, decreasing = TRUE))) {
      cat(sprintf("    %-30s: %d entries (%.0f%%)\n",
                  tn, active[tn], 100 * active[tn] / total_entries))
    }
  }

  allocation <- rbindlist(allocation_list)

  if (!is.null(ownership) && nrow(allocation) > 0) {
    allocation[, ownership := ownership[team_name], by = team_name]
    allocation[, leverage := p_survive_today / pmax(ownership, 0.001)]
  }

  # --- Portfolio-level EV (share-based, NFL-style) ---
  if (nrow(allocation) > 0) {
    cat("\n--- Computing portfolio-level EVs (share formula) ---\n")
    portfolio_ev <- evaluate_portfolio_ev(
      allocation, groups, current_slot_id, sim, tw, teams_dt,
      ownership_by_slot, sim_sample_size = sim_sample_size,
      death_rd_cache = death_rd_cache, scoring_sample_idx = sample_idx,
      field_sim_data = field_sim_data,
      entry_field_data = entry_field_data
    )
    attr(allocation, "portfolio_ev") <- portfolio_ev
  }

  attr(allocation, "scores") <- scores
  attr(allocation, "groups") <- groups
  allocation
}

# ==============================================================================
# STEP 5: OUTPUT & CONFIRMATION
# ==============================================================================

#' Print the allocation recommendation
print_allocation <- function(allocation, teams_dt, scores = NULL, groups = NULL,
                              entry_field_data = NULL, current_slot_id = NULL,
                              field_avail = NULL) {
  if (nrow(allocation) == 0) {
    cat("No allocation to display.\n")
    return(invisible(NULL))
  }

  portfolio_ev <- attr(allocation, "portfolio_ev")

  cat("\n========================================================\n")
  cat("          ALLOCATION RECOMMENDATION\n")
  cat("========================================================\n\n")

  # Summary by team
  has_extras <- "slot1_extra_name" %in% names(allocation) &&
    any(!is.na(allocation$slot1_extra_name))
  by_team <- allocation[, .(
    total_entries   = sum(n_assigned),
    avg_ev          = weighted.mean(ev, n_assigned),
    avg_win_today   = weighted.mean(p_survive_today, n_assigned),
    avg_win_contest = weighted.mean(p_win_contest, n_assigned),
    n_contests      = uniqueN(contest_id),
    slot1_extra_name = if (has_extras) slot1_extra_name[1] else NA_character_
  ), by = .(team_name, team_id)]

  # Add seed
  by_team[, seed := teams_dt$seed[match(team_id, teams_dt$team_id)]]
  by_team[, region := teams_dt$region[match(team_id, teams_dt$team_id)]]
  setorder(by_team, -total_entries)
  total_entries <- sum(by_team$total_entries)

  # Compute opponent ownership and availability from entry field data
  opp_own <- setNames(rep(NA_real_, nrow(by_team)), by_team$team_name)
  opp_avail <- setNames(rep(NA_real_, nrow(by_team)), by_team$team_name)
  if (!is.null(entry_field_data) && !is.null(current_slot_id)) {
    # Average implied ownership across all contests (weighted by alive count)
    total_alive <- sum(sapply(entry_field_data, function(efd) efd$alive_count))
    for (i in seq_len(nrow(by_team))) {
      tn <- by_team$team_name[i]
      tid <- by_team$team_id[i]
      weighted_own <- 0
      weighted_avail <- 0
      for (cid in names(entry_field_data)) {
        efd <- entry_field_data[[cid]]
        w <- efd$alive_count / total_alive
        # Ownership from implied_ownership
        own_vec <- efd$implied_ownership[[current_slot_id]]
        if (!is.null(own_vec) && tn %in% names(own_vec)) {
          weighted_own <- weighted_own + w * own_vec[tn]
        }
        # Availability: % of field entries that haven't used this team
        if (!is.null(field_avail) && cid %in% names(field_avail)) {
          fa <- field_avail[[cid]]
          n_used <- sum(sapply(fa$used_teams_by_entry, function(ut) tid %in% ut))
          avail_frac <- 1 - n_used / max(fa$alive_count, 1)
          weighted_avail <- weighted_avail + w * avail_frac
        }
      }
      opp_own[tn] <- weighted_own
      opp_avail[tn] <- weighted_avail
    }
  }
  has_opp_data <- any(!is.na(opp_own))

  if (has_opp_data) {
    cat(sprintf("%-25s %4s %-8s %7s %5s %9s %11s %8s %8s %8s\n",
                "Team", "Seed", "Region", "Entries", "Pct", "Win Today",
                "Win Contest", "Contests", "Opp Own%", "Avail%"))
    cat(paste(rep("-", 117), collapse = ""), "\n")
  } else {
    cat(sprintf("%-35s %4s %-8s %7s %5s %9s %11s %8s\n",
                "Team", "Seed", "Region", "Entries", "Pct", "Win Today", "Win Contest", "Contests"))
    cat(paste(rep("-", 93), collapse = ""), "\n")
  }

  for (i in seq_len(nrow(by_team))) {
    r <- by_team[i]
    name_str <- r$team_name
    if (has_extras && !is.na(r$slot1_extra_name)) {
      name_str <- paste0(name_str, " + ", r$slot1_extra_name)
    }
    pct <- 100 * r$total_entries / total_entries
    if (has_opp_data) {
      own_pct <- if (!is.na(opp_own[r$team_name])) sprintf("%6.1f%%", 100 * opp_own[r$team_name]) else "    N/A"
      avail_pct <- if (!is.na(opp_avail[r$team_name])) sprintf("%6.1f%%", 100 * opp_avail[r$team_name]) else "    N/A"
      cat(sprintf("%-25s  %2d   %-8s %5d %4.0f%%   %5.1f%%      %5.2f%%   %5d  %s  %s\n",
                  name_str, r$seed, r$region,
                  r$total_entries, pct, 100 * r$avg_win_today,
                  100 * r$avg_win_contest, r$n_contests,
                  own_pct, avail_pct))
    } else {
      cat(sprintf("%-35s  %2d   %-8s %5d %4.0f%%   %5.1f%%      %5.2f%%   %5d\n",
                  name_str, r$seed, r$region,
                  r$total_entries, pct, 100 * r$avg_win_today,
                  100 * r$avg_win_contest, r$n_contests))
    }
  }

  cat(paste(rep("-", if (has_opp_data) 117 else 93), collapse = ""), "\n")

  # --- Head-to-head EV comparison by contest ---
  # Shows the actual EV for each team within each contest, making
  # apples-to-apples comparison easy. Best EV per contest is starred.
  if (nrow(by_team) >= 2 && !is.null(scores) && !is.null(groups)) {
    top_team_names <- by_team$team_name[1:min(6, nrow(by_team))]
    # Get per-contest EVs for top teams (best EV across groups in that contest)
    contest_ev <- scores[team_name %in% top_team_names,
                          .(best_ev = max(ev)), by = .(contest_id, team_name)]
    contest_entries <- groups[, .(total = sum(n_entries)), by = contest_id]
    contest_ev <- merge(contest_ev, contest_entries, by = "contest_id")

    # Build prize lookup from groups
    prize_lookup <- groups[, .(prize = prize_pool[1]), by = contest_id]

    # Pivot to wide format: one row per contest, columns = teams
    contest_wide <- dcast(contest_ev, contest_id + total ~ team_name,
                           value.var = "best_ev", fill = NA)
    contest_wide <- merge(contest_wide, prize_lookup, by = "contest_id", all.x = TRUE)
    setorder(contest_wide, -total)

    cat("\n--- Head-to-Head EV by Contest (apples-to-apples) ---\n")
    cat(sprintf("  %-14s %5s %8s", "Contest", "Ents", "Prize"))
    for (tn in top_team_names) {
      cat(sprintf(" %12s", substr(tn, 1, 12)))
    }
    cat("\n")
    cat(sprintf("  %s\n", strrep("-", 29 + 13 * length(top_team_names))))

    for (i in seq_len(nrow(contest_wide))) {
      row <- contest_wide[i]
      label <- substr(row$contest_id, 1, 14)
      prize_str <- if (!is.na(row$prize)) sprintf("$%s", format(round(row$prize), big.mark = ",")) else ""
      cat(sprintf("  %-14s %5d %8s", label, row$total, prize_str))

      # Find best EV in this row to star it
      row_evs <- numeric(length(top_team_names))
      for (j in seq_along(top_team_names)) {
        val <- row[[top_team_names[j]]]
        row_evs[j] <- if (is.na(val)) -Inf else val
      }
      best_j <- which.max(row_evs)

      for (j in seq_along(top_team_names)) {
        val <- row[[top_team_names[j]]]
        if (is.na(val)) {
          cat(sprintf(" %12s", "--"))
        } else {
          star <- if (j == best_j) "*" else " "
          cat(sprintf("    %s$%6.2f", star, val))
        }
      }
      cat("\n")
    }
    cat("\n")
  }

  # --- Portfolio-level EVs (corrected, share-based) ---
  if (!is.null(portfolio_ev) && nrow(portfolio_ev) > 0) {
    total_portfolio_ev <- sum(portfolio_ev$portfolio_ev)
    total_prize_pool <- sum(portfolio_ev$prize_pool)

    cat(sprintf("TOTAL: %d entries | Portfolio EV: $%.2f ($%.2f/entry) | Prize Pool: $%s (%.1f%%)\n",
                total_entries, total_portfolio_ev,
                total_portfolio_ev / total_entries,
                format(total_prize_pool, big.mark = ","),
                100 * total_portfolio_ev / total_prize_pool))

    # Sanity check
    for (i in seq_len(nrow(portfolio_ev))) {
      r <- portfolio_ev[i]
      if (r$portfolio_ev > r$prize_pool) {
        cat(sprintf("\n  WARNING: Contest %s portfolio EV ($%.0f) > prize pool ($%.0f)!\n",
                    r$contest_id, r$portfolio_ev, r$prize_pool))
      }
    }
  } else {
    total_ev_ranking <- sum(allocation$ev * allocation$n_assigned)
    cat(sprintf("TOTAL: %d entries, Ranking EV = $%.2f (uncorrected)\n", total_entries, total_ev_ranking))
  }

  cat("\n")
  invisible(by_team)
}

# ==============================================================================
# MAIN ENTRYPOINT
# ==============================================================================

#' Run the full optimization pipeline for today
#'
#' @param sim_file Path to sim_results RDS file
#' @param state_file Path to portfolio state RDS file (or NULL to init fresh)
#' @param current_slot_id Which slot to optimize for (e.g., "R1_d1")
#' @param contests_df If state_file is NULL, a data.frame to init the portfolio
#' @param sim_sample_size Number of sims per EV calculation
#' @return List with: allocation, state, sim, tw
run_optimizer <- function(sim_file = NULL, state_file = NULL, current_slot_id,
                           contests_df = NULL, sim_sample_size = 100000,
                           scrape_inputs = NULL, ownership_override = NULL,
                           contest_filter = NULL, locked_teams = NULL,
                           entry_field_data = NULL,
                           method = "beam") {
  pipeline_start <- proc.time()[["elapsed"]]

  field_sim_data <- NULL
  ownership_by_slot <- list()

  if (!is.null(scrape_inputs)) {
    # ---- SCRAPE-BASED PATH: use real data ----
    cat("=== Using scrape-based inputs ===\n")
    sim <- scrape_inputs$sim
    teams_dt <- as.data.table(sim$teams)
    cat(sprintf("  %s sims x 63 games, %d teams\n",
                format(sim$n_sims, big.mark = ","), nrow(teams_dt)))

    tw <- precompute_team_wins(sim)
    state <- scrape_inputs$portfolio
    current_slot_id <- scrape_inputs$current_slot_id

    # Filter to specific contest(s) if requested
    if (!is.null(contest_filter)) {
      state <- state[contest_id %in% contest_filter]
      cat(sprintf("  Filtered to %d contest(s)\n", length(contest_filter)))
    }

    # Get candidates for today — filter to teams that survived prior rounds
    candidates_dt <- get_available_picks(current_slot_id, integer(0), teams_dt)
    # For R2+, get_teams_in_slot returns all feeder teams (including R64 losers).
    # Filter to teams that actually won their prior games using locked sim results.
    slot <- get_slot(current_slot_id)
    if (slot$round_num >= 2 && !is.null(sim$all_results)) {
      # A team survives to round R if it won games in rounds 1..(R-1).
      # In locked sims, all_results[, game_col] == team_id for every sim if locked.
      # Check round R-1 feeder games: each candidate must appear as winner of its
      # feeder game in sim row 1 (all sims agree for locked games).
      feeder_round <- slot$round_num - 1
      feeder_games <- which(sim$round_info$round_num == feeder_round)
      # Winners of feeder games (from first sim row — locked games are identical across sims)
      feeder_winners <- unique(sim$all_results[1, feeder_games])
      candidates_dt <- candidates_dt[candidates_dt$team_id %in% feeder_winners, ]
    }
    candidates <- candidates_dt$name

    cat(sprintf("Portfolio: %d entries across %d contests (%d alive)\n",
                nrow(state), uniqueN(state$contest_id), sum(state$alive)))
    cat(sprintf("Today's candidates (%d teams): %s\n",
                length(candidates), paste(candidates, collapse = ", ")))

    remaining_slots <- scrape_inputs$remaining_slots

    if (!is.null(entry_field_data)) {
      # ENTRY-LEVEL PATH: per-sim exact opponent survival counts
      # entry_field_data is pre-computed by simulate_entry_level_field()
      cat("\nUsing entry-level field simulation (per-sim exact counts)...\n")
      field_sim_data <- NULL  # not used — entry_field_data replaces it

      for (cid in names(entry_field_data)) {
        efd <- entry_field_data[[cid]]
        cat(sprintf("  [%s] %d alive entries, %d sims, %.0f avg alive after last slot\n",
                    cid, efd$alive_count, nrow(efd$n_alive_matrix),
                    mean(efd$n_alive_matrix[, ncol(efd$n_alive_matrix)])))
      }

      # Build ownership_by_slot from entry model's implied ownership (for diagnostics/fallback)
      # Also incorporate any manual overrides
      own_params <- tryCatch(load_calibrated_params(), error = function(e) NULL)
      all_future_slots <- unique(unlist(lapply(c("A", "C"), function(fmt) {
        so <- get_slot_order(fmt)
        idx <- match(current_slot_id, so)
        if (is.na(idx)) return(character(0))
        so[idx:length(so)]
      })))
      override_list <- if (!is.null(ownership_override)) {
        if (is.numeric(ownership_override)) setNames(list(ownership_override), current_slot_id)
        else ownership_override
      } else list()

      # Use entry-model implied ownership where available, override where specified
      first_efd <- entry_field_data[[1]]
      for (sid in all_future_slots) {
        if (sid %in% names(override_list)) {
          ownership_by_slot[[sid]] <- override_list[[sid]]
        } else if (!is.null(first_efd$implied_ownership[[sid]])) {
          # Convert implied ownership to named vector with team names
          ownership_by_slot[[sid]] <- first_efd$implied_ownership[[sid]]
        } else {
          ownership_by_slot[[sid]] <- estimate_ownership(
            sid, teams_dt, sim$all_results, sim$round_info, params = own_params)
        }
      }
      cat(sprintf("  Built ownership for %d future slots\n", length(all_future_slots)))

    } else if (!is.null(ownership_override)) {
      # FAST PATH: skip C++ field sim, use ownership-based model with override
      # This avoids the 28-minute field sim entirely
      cat("\nSkipping field sim (using ownership override for current slot)...\n")
      field_sim_data <- NULL

      # Build ownership_by_slot: override for provided slots, estimate the rest
      # ownership_override can be a named numeric vector (current slot only)
      # or a named list of named numeric vectors keyed by slot_id
      own_params <- tryCatch(load_calibrated_params(), error = function(e) NULL)
      all_future_slots <- unique(unlist(lapply(c("A", "C"), function(fmt) {
        so <- get_slot_order(fmt)
        idx <- match(current_slot_id, so)
        if (is.na(idx)) return(character(0))
        so[idx:length(so)]
      })))
      # Normalize: if ownership_override is a plain vector, wrap as current slot
      if (is.numeric(ownership_override)) {
        override_list <- setNames(list(ownership_override), current_slot_id)
      } else {
        override_list <- ownership_override  # already a list
      }
      n_overridden <- 0
      for (sid in all_future_slots) {
        if (sid %in% names(override_list)) {
          ownership_by_slot[[sid]] <- override_list[[sid]]
          n_overridden <- n_overridden + 1
        } else {
          ownership_by_slot[[sid]] <- estimate_ownership(
            sid, teams_dt, sim$all_results, sim$round_info, params = own_params)
        }
      }
      cat(sprintf("  Built ownership for %d slots (%d overridden, %d estimated)\n",
                  length(all_future_slots), n_overridden,
                  length(all_future_slots) - n_overridden))

      # Still compute field availability for alive counts
      field_cids <- names(scrape_inputs$field_avail)
      for (cid in field_cids) {
        fa <- scrape_inputs$field_avail[[cid]]
        cat(sprintf("  [%s] %d alive field entries\n", fa$contest_name, fa$alive_count))
      }

    } else {
      # FULL PATH: run C++ field sim per contest
      cat("\nRunning field simulations per contest...\n")
      field_sim_data <- list()
      field_cids <- names(scrape_inputs$field_avail)
      if (!is.null(contest_filter)) {
        field_cids <- intersect(field_cids, contest_filter)
      }
      pg_field <- make_progress(length(field_cids), "Field sims", update_every = 1)

      for (cid in field_cids) {
        fa <- scrape_inputs$field_avail[[cid]]
        cat(sprintf("\n  [%s] %d alive field entries\n",
                    fa$contest_name, fa$alive_count))

        contest_size_for_sim <- fa$alive_count +
          sum(state[contest_id == cid & alive == TRUE, .N])

        result <- run_contest_field_sim(fa, sim, remaining_slots, contest_size_for_sim)
        field_sim_data[[cid]] <- list(
          alive_count     = fa$alive_count,
          survival_curves = result$survival_curves,
          field_groups    = result$field_groups
        )
        pg_field$tick()
      }
      pg_field$done()
    }

    # Print ownership being used
    if (!is.null(ownership_override)) {
      cur_own <- ownership_by_slot[[current_slot_id]]
      cat("\n=== OWNERSHIP (user override) ===\n")
      own_ord <- order(cur_own, decreasing = TRUE)
      own_names <- names(cur_own)
      cat(sprintf("  %-20s %8s\n", "Team", "Own%"))
      cat(sprintf("  %s\n", paste(rep("-", 30), collapse = "")))
      for (j in own_ord) {
        if (cur_own[j] >= 0.001)
          cat(sprintf("  %-20s %7.1f%%\n", own_names[j], cur_own[j] * 100))
      }
      cat("\n")
    }

  } else {
    # ---- ORIGINAL PATH: estimate ownership ----
    if (is.null(sim_file)) stop("Must provide sim_file or scrape_inputs")

    cat(sprintf("Loading sim results from %s...\n", basename(sim_file)))
    sim <- readRDS(sim_file)
    teams_dt <- as.data.table(sim$teams)
    cat(sprintf("  %s sims x 63 games, %d teams\n",
                format(sim$n_sims, big.mark = ","), nrow(teams_dt)))

    tw <- precompute_team_wins(sim)

    if (!is.null(state_file) && file.exists(state_file)) {
      state <- load_state(state_file)
    } else if (!is.null(contests_df)) {
      state <- init_portfolio(contests_df)
    } else {
      stop("Must provide either state_file or contests_df")
    }

    candidates_dt <- get_available_picks(current_slot_id, integer(0), teams_dt)
    # Filter to teams that actually survived prior rounds (locked results)
    slot <- get_slot(current_slot_id)
    if (slot$round_num >= 2 && !is.null(sim$all_results)) {
      feeder_round <- slot$round_num - 1
      feeder_games <- which(sim$round_info$round_num == feeder_round)
      feeder_winners <- unique(sim$all_results[1, feeder_games])
      candidates_dt <- candidates_dt[candidates_dt$team_id %in% feeder_winners, ]
    }
    candidates <- candidates_dt$name
    cat(sprintf("Portfolio initialized: %d entries across %d contests",
                sum(state$alive), uniqueN(state$contest_id)))
    fmt_counts <- state[alive == TRUE, .N, by = format]
    fmt_strs <- sprintf("%d format %s", fmt_counts$N, fmt_counts$format)
    cat(sprintf(" (%s)\n", paste(fmt_strs, collapse = ", ")))

    cat(sprintf("\nToday's candidates (%d teams): %s\n",
                length(candidates), paste(candidates, collapse = ", ")))

    own_params <- load_calibrated_params()

    cat("\nEstimating ownership...\n")
    ownership_by_slot <- list()
    all_future_slots <- unique(unlist(lapply(c("A", "B"), function(fmt) {
      so <- get_slot_order(fmt)
      idx <- match(current_slot_id, so)
      if (is.na(idx)) return(character(0))
      so[idx:length(so)]
    })))

    pg_own <- make_progress(length(all_future_slots), "Ownership", update_every = 1)
    for (sid in all_future_slots) {
      own <- estimate_ownership(sid, teams_dt, sim$all_results, sim$round_info,
                                 params = own_params)
      ownership_by_slot[[sid]] <- own
      pg_own$tick()
    }
    pg_own$done()

    print_ownership(current_slot_id, ownership_by_slot[[current_slot_id]],
                    teams_dt, sim$all_results)
  }

  slot <- get_slot(current_slot_id)

  # Resolve locked teams to IDs (pass both teams in each locked game)
  locked_team_ids <- integer(0)
  if (!is.null(locked_teams)) {
    locked_team_ids <- teams_dt$team_id[teams_dt$name %in% locked_teams]
    missing <- locked_teams[!locked_teams %in% teams_dt$name]
    if (length(missing) > 0) {
      cat(sprintf("  WARNING: locked teams not found in sim: %s\n", paste(missing, collapse = ", ")))
    }
    cat(sprintf("Locked teams (%d): %s\n", length(locked_team_ids),
                paste(teams_dt$name[locked_team_ids], collapse = ", ")))
  }

  # Run optimization
  allocation <- optimize_today(
    state, candidates, current_slot_id, sim, tw, teams_dt,
    ownership_by_slot, sim_sample_size = sim_sample_size,
    field_sim_data = field_sim_data,
    locked_team_ids = locked_team_ids,
    entry_field_data = entry_field_data,
    method = method
  )

  # Print recommendation
  print_allocation(allocation, teams_dt,
                   scores = attr(allocation, "scores"),
                   groups = attr(allocation, "groups"),
                   entry_field_data = entry_field_data,
                   current_slot_id = current_slot_id,
                   field_avail = if (!is.null(scrape_inputs)) scrape_inputs$field_avail else NULL)

  # Pipeline summary
  pipeline_elapsed <- proc.time()[["elapsed"]] - pipeline_start
  if (pipeline_elapsed < 60) {
    pipe_str <- sprintf("%.1f seconds", pipeline_elapsed)
  } else if (pipeline_elapsed < 3600) {
    pipe_str <- sprintf("%.1f minutes", pipeline_elapsed / 60)
  } else {
    pipe_str <- sprintf("%.1f hours", pipeline_elapsed / 3600)
  }
  cat(sprintf("\n=== Pipeline complete in %s ===\n", pipe_str))

  result <- list(
    allocation       = allocation,
    portfolio_ev     = attr(allocation, "portfolio_ev"),
    state            = state,
    sim              = sim,
    tw               = tw,
    teams_dt         = teams_dt,
    ownership_by_slot = ownership_by_slot,
    scrape_inputs     = scrape_inputs
  )

  # Auto-export CSVs if we have scrape data
  if (!is.null(scrape_inputs) && nrow(allocation) > 0) {
    cat("\n--- Exporting pick CSVs ---\n")
    splash_team_map <- build_splash_team_id_map(scrape_inputs$scrape_results)
    export_allocation_csvs(
      allocation, scrape_inputs$scrape_results, state,
      splash_team_map, scrape_inputs$name_map,
      output_dir = ".",
      our_username = scrape_inputs$our_username %||% "TinkyTyler",
      locked_teams = locked_teams,
      completed_slots = scrape_inputs$completed_slots,
      teams_dt = teams_dt
    )
  }

  invisible(result)
}

cat("Splash optimizer loaded\n")
cat("Usage: result <- run_optimizer('sim_results_2026.rds', NULL, 'R1_d1', contests_df, 100000)\n")
