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
#' @return List (ctx) with precomputed matrices
precompute_group_context <- function(group, current_slot_id, tw, teams_dt,
                                      ownership_by_slot, sample_idx) {
  n_teams <- nrow(teams_dt)
  n_sims <- length(sample_idx)
  contest_size <- group$contest_size
  prize_pool <- group$prize_pool
  our_n <- group$n_entries
  # Field = non-our entries. Our entries are handled separately in portfolio eval.
  full_field <- contest_size - our_n
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

  # ---- 1. Field survival per slot (ownership-weighted) ----
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
      if (n_picks > 1) {
        field_slot_survive[, si] <- (p_field_single / sum(own_vec)) ^ n_picks
      } else {
        field_slot_survive[, si] <- p_field_single
      }
    }
  }

  # ---- 2. Field death-round distribution ----
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

  # ---- 3. V_die[sim, round] — payout if we die in each round ----
  V_die <- matrix(0, nrow = n_sims, ncol = max_round)
  for (d in 1:max_round) {
    # P(a single field entry outlasts us) = P(survive all) + P(die after round d)
    p_outlast <- p_field_all
    if (d < max_round) {
      for (rd in (d + 1):max_round) {
        p_outlast <- p_outlast + field_dies_round[, rd]
      }
    }
    p_nobody <- (1 - p_outlast) ^ full_field
    p_same <- field_dies_round[, d]
    expected_same <- p_same * full_field
    V_die[, d] <- p_nobody * prize_pool / (1 + expected_same)
  }

  # ---- 4. V_survive[sim, slot] — backward recursion ----
  # V_survive[, s] = expected value of being alive at slot s, using field survival
  # for all remaining slots from s onward.
  V_survive <- matrix(0, nrow = n_sims, ncol = n_remaining + 1)
  # Terminal: survived everything → Case 1 payout
  V_survive[, n_remaining + 1] <- prize_pool / (1 + p_field_all * full_field)

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

  # ---- Initialize beam with candidate pick in slot 1 ----
  rd1 <- slot_round_nums[1]
  slot1_survive <- tw$team_round_wins[[rd1]][ctx$sample_idx, candidate_id]
  slot1_n_picks <- get_n_picks(remaining_slots[1], ctx$group_format)

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

        # NEW: Drop mathematically dead teams (score <= 0) before expanding
        alive_mask <- avail_scores > 0
        avail_scores <- avail_scores[alive_mask]
        avail_ids <- avail_ids[alive_mask]

        # Dial expand_top back down to 5 (or whatever default you prefer). We only need the best viable teams.
        n_expand <- min(5, length(avail_scores))
        if (n_expand == 0) next
        
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

      beam <- new_beam
    }
  }

  # ---- Evaluate each surviving beam path and pick the best ----
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

      p_outlast <- p_outlast_from[mask, d]
      p_nobody <- (1 - p_outlast) ^ full_field
      field_same <- ctx$field_dies_round[mask, d] * full_field

      payouts[mask] <- p_nobody * prize_pool / (1 + field_same)
      p_win_per_sim[mask] <- p_nobody / (1 + field_same)
    }

    ev <- mean(payouts)

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
                                   scoring_sample_idx = NULL) {
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
    full_field <- contest_size - our_n
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

    # --- Field death distribution (analytical, once per contest) ---
    slot_survive <- matrix(1, nrow = sim_sample_size, ncol = n_remaining)
    slot_round_nums <- integer(n_remaining)

    for (si in seq_along(remaining_slots)) {
      sid <- remaining_slots[si]
      slot_def <- get_slot(sid)
      slot_round_nums[si] <- slot_def$round_num
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

# --- Vectorized portfolio payout computation (DIVERSIFIED ASSUMPTION) ---
    # We assume entries sharing an R1 pick will optimally diverge in future rounds.
    # We evaluate their path EVs independently, then apply a global portfolio collision discount.
    
    portfolio_ev_raw <- 0
    
    # We still need our_best and our_at_best for the diagnostic printouts to work
    eff <- alloc_deaths
    eff[eff == 0L] <- max_round + 1L
    our_best <- if (n_alloc == 1) eff[1, ] else apply(eff, 2, max)
    our_at_best <- numeric(sim_sample_size)
    for (ai in seq_len(n_alloc)) {
      our_at_best <- our_at_best + alloc_n[ai] * (eff[ai, ] == our_best)
    }

    # Calculate independent EV for each allocated path
    for (ai in seq_len(n_alloc)) {
      path_deaths <- eff[ai, ]
      n_entries <- alloc_n[ai]
      path_ev_sum <- 0
      
      # Case A: Survived all
      mask_surv <- which(path_deaths == (max_round + 1L))
      if (length(mask_surv) > 0) {
        field_surv <- p_field_all[mask_surv] * full_field
        path_ev_sum <- path_ev_sum + sum(prize_pool / (1 + field_surv))
      }
      
      # Case B: Died in round d
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
    
    # Apply global portfolio cannibalization discount
    # If we own 1.3% of the field, our independent paths still naturally collide ~1.3% of the time.
    discount_factor <- full_field / contest_size
    portfolio_ev <- portfolio_ev_raw * discount_factor
    
    # Dummy payouts array so the diagnostic print loop below doesn't break
    payouts <- rep(portfolio_ev / our_n, sim_sample_size)

    # --- Diagnostics ---
    cat(sprintf("\n  [Portfolio EV] Contest %s: %d allocs, %d entries, field=%d, prize=$%s\n",
                substr(cid, 1, 12), n_alloc, our_n, full_field,
                format(prize_pool, big.mark = ",")))

    for (ai in seq_len(n_alloc)) {
      dr <- alloc_deaths[ai, ]
      dist <- table(factor(dr, levels = 0:max_round))
      pcts <- 100 * as.numeric(dist) / sim_sample_size
      cat(sprintf("    Alloc %d (%s, n=%d): survived=%.2f%%, rd1=%.1f%%, rd2=%.1f%%, rd3=%.1f%%, rd4=%.1f%%, rd5=%.2f%%, rd6=%.3f%%\n",
                  ai, ct_alloc$team_name[ai], alloc_n[ai],
                  pcts[1], pcts[2], pcts[3], pcts[4], pcts[5], pcts[6], pcts[7]))
    }

    tier_dist <- table(factor(our_best, levels = c(1:max_round, max_round + 1L)))
    tier_pcts <- 100 * as.numeric(tier_dist) / sim_sample_size
    cat(sprintf("    Best tier: rd1=%.1f%%, rd2=%.1f%%, rd3=%.1f%%, rd4=%.1f%%, rd5=%.2f%%, rd6=%.3f%%, survived=%.3f%%\n",
                tier_pcts[1], tier_pcts[2], tier_pcts[3], tier_pcts[4],
                tier_pcts[5], tier_pcts[6], tier_pcts[7]))

    for (tier in sort(unique(our_best))) {
      tier_mask <- our_best == tier
      tier_label <- if (tier == max_round + 1L) "survived" else sprintf("rd%d", tier)
      n_sims_tier <- sum(tier_mask)
      mean_payout <- mean(payouts[tier_mask])
      mean_our <- mean(our_at_best[tier_mask])
      if (tier <= max_round) {
        p_out <- p_field_all[tier_mask]
        if (tier < max_round) {
          for (rd in (tier + 1):max_round) p_out <- p_out + p_field_dies_round[tier_mask, rd]
        }
        mean_p_nobody <- mean((1 - p_out) ^ full_field)
        mean_field_same <- mean(p_field_dies_round[tier_mask, tier] * full_field)
        cat(sprintf("    Tier %-8s: %6d sims, mean_payout=$%8.4f, our_at_best=%.1f, p_nobody=%.2e, field_same=%.1f\n",
                    tier_label, n_sims_tier, mean_payout, mean_our, mean_p_nobody, mean_field_same))
      } else {
        mean_field_surv <- mean(p_field_all[tier_mask] * full_field)
        cat(sprintf("    Tier %-8s: %6d sims, mean_payout=$%8.4f, our_at_best=%.1f, field_co_surv=%.1f\n",
                    tier_label, n_sims_tier, mean_payout, mean_our, mean_field_surv))
      }
    }

    cat(sprintf("    => Portfolio EV = $%.4f ($%.4f/entry)\n", portfolio_ev, portfolio_ev / our_n))

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
                            sim_sample_size = 100000) {
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

  # Score each candidate for each group
  all_scores <- list()
  death_rd_cache <- list()  # keyed by "group_id:team_id" -> integer vector
  overall_start <- proc.time()[["elapsed"]]

  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    cat(sprintf("\n--- Group %d/%d: contest=%s, %d entries, %d field ---\n",
                gi, nrow(groups), g$contest_id, g$n_entries, g$contest_size))

    # Precompute group context (field survival, continuation values, our future picks)
    ctx_start <- proc.time()[["elapsed"]]
    ctx <- precompute_group_context(
      g, current_slot_id, tw, teams_dt,
      ownership_by_slot, sample_idx
    )
    ctx_elapsed <- proc.time()[["elapsed"]] - ctx_start
    cat(sprintf("  Precomputed context in %.1fs\n", ctx_elapsed))

    # Print diagnostic: beam search forward sim for several candidates in first group
    if (gi == 1) {
      avail <- candidate_ids[!candidate_ids %in% g$used_teams[[1]]]
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
        cat(sprintf("\n  === Beam Search for %s (%d-seed) | beam=5, expand=3, bracket-aware ===\n",
                    teams_dt$name[dc], dc_seed))
        cat(sprintf("  %-10s %3s %10s %10s  %-30s %6s\n",
                    "Slot", "Rd", "Field_Surv", "Our_Surv", "Pick(s)", "WinP%"))
        cat(sprintf("  %s\n", strrep("-", 85)))
        for (si in seq_along(ctx$remaining_slots)) {
          pick_str <- diag$pick_names[si]
          # Append extra picks for multi-pick slots (E8)
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
        # EV breakdown
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
    }

    # Score all candidates for this group
    avail_cids <- candidate_ids[!candidate_ids %in% g$used_teams[[1]]]
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

      all_scores[[length(all_scores) + 1]] <- data.table(
        group_id        = g$group_id,
        contest_id      = g$contest_id,
        n_entries       = g$n_entries,
        team_name       = cname,
        team_id         = cid,
        ev              = ev_result$ev,
        p_survive_today = ev_result$p_survive_today,
        p_win_contest   = ev_result$p_win_contest,
        mean_death_rd   = ev_result$mean_death_rd,
        slot1_extra_name = extra_names
      )
    }
    cat(sprintf("  Scored %d candidates\n", length(avail_cids)))
  }

  total_elapsed <- proc.time()[["elapsed"]] - overall_start
  if (total_elapsed < 60) {
    time_str <- sprintf("%.1fs", total_elapsed)
  } else {
    time_str <- sprintf("%.1fm", total_elapsed / 60)
  }
  cat(sprintf("\n=== Scoring complete in %s ===\n", time_str))

  scores <- rbindlist(all_scores)

  if (nrow(scores) == 0) {
    cat("No valid candidates for any group.\n")
    return(data.table())
  }

  # Print top/bottom candidates for each group
  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    g_scores <- scores[group_id == g$group_id]
    if (nrow(g_scores) == 0) next
    setorder(g_scores, -ev)
    cat(sprintf("\n  Group %d top 5:\n", gi))
    top_n <- min(5, nrow(g_scores))
    for (k in 1:top_n) {
      r <- g_scores[k]
      name_str <- r$team_name
      if (!is.na(r$slot1_extra_name)) name_str <- paste0(name_str, " + ", r$slot1_extra_name)
      cat(sprintf("    %2d. %-35s EV=$%6.2f  WinToday=%5.1f%%  WinContest=%.3f%%  AvgDeath=%.1f\n",
                  k, name_str, r$ev, 100 * r$p_survive_today,
                  100 * r$p_win_contest, r$mean_death_rd))
    }
  }

# --- Greedy assignment (Marginal EV Maximization) ---
  allocation_list <- list()
  cat("\n--- Running Greedy Marginal Allocator ---\n")

  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    g_scores <- scores[group_id == g$group_id]
    if (nrow(g_scores) == 0) next

    setorder(g_scores, -ev)
    n_ent <- g$n_entries
    
    # Safe dynamic filter: Keep only teams that are mathematically viable.
    # We keep the top 15 teams, OR any team with an EV > 0.01, whichever is smaller.
    # This prevents the "NA" bug when EVs are naturally low in tough structures.
    top_viable <- min(15, sum(g_scores$ev > 0.01))
    if (top_viable == 0) top_viable <- 1 # Failsafe
    g_scores <- g_scores[1:top_viable]
    
    if (n_ent <= 3 || nrow(g_scores) <= 1) {
      best <- g_scores[1]
      allocation_list[[length(allocation_list) + 1]] <- data.table(
        contest_id      = g$contest_id,
        group_id        = g$group_id,
        team_name       = best$team_name,
        team_id         = best$team_id,
        n_assigned      = n_ent,
        ev              = best$ev,
        p_survive_today = best$p_survive_today,
        p_win_contest   = best$p_win_contest,
        slot1_extra_name = best$slot1_extra_name
      )
    } else {
      cat(sprintf("  Allocating %d entries for group %d...\n", n_ent, gi))
      
      # Recalculate context quickly for the marginal math
      ctx <- precompute_group_context(g, current_slot_id, tw, teams_dt,
                                      ownership_by_slot, sample_idx)
      
      # Scale targets: cap at 8 teams max, which is plenty for 150 entries
      target_n <- max(2, min(15, ceiling(n_ent / 15))) 
      top_n <- min(target_n, nrow(g_scores))
      top <- g_scores[1:top_n]
      
      # Pre-extract death rounds from cache
      max_round <- ctx$max_round
      n_sims <- ctx$n_sims
      D <- matrix(0L, nrow = top_n, ncol = n_sims)
      for (k in seq_len(top_n)) {
        cache_key <- paste0(g$group_id, ":", top$team_id[k])
        D[k, ] <- death_rd_cache[[cache_key]]
      }
      D[D == 0L] <- max_round + 1L
      
      # Precompute Base Payout Numerators and Denominator Offsets
      Num <- matrix(0, nrow = n_sims, ncol = max_round + 1L)
      DenomOffset <- matrix(0, nrow = n_sims, ncol = max_round + 1L)
      
      for (tier in 1:(max_round+1L)) {
        if (tier == max_round + 1L) {
          Num[, tier] <- ctx$prize_pool
          DenomOffset[, tier] <- ctx$p_field_all * ctx$full_field
        } else {
          p_out <- ctx$p_field_all
          if (tier < max_round) {
            for (rd in (tier + 1):max_round) {
              p_out <- p_out + ctx$field_dies_round[, rd]
            }
          }
          p_nobody <- (1 - p_out)^ctx$full_field
          Num[, tier] <- p_nobody * ctx$prize_pool
          DenomOffset[, tier] <- ctx$field_dies_round[, tier] * ctx$full_field
        }
      }
      
      # Marginal allocation loop
      alloc <- rep(0L, top_n)
      current_best <- rep(0L, n_sims)
      current_at_best <- rep(0L, n_sims)
      sim_idx <- 1:n_sims
      
      for (entry_idx in 1:n_ent) {
        best_k <- 1
        best_ev <- -Inf
        best_k_best <- NULL
        best_k_at_best <- NULL
        
        for (k in 1:top_n) {
          cand_deaths <- D[k, ]
          
          new_best <- pmax(current_best, cand_deaths)
          
          # Fast logical update of entry ties at max survival round
          new_at_best <- current_at_best
          higher_mask <- cand_deaths > current_best
          equal_mask <- cand_deaths == current_best
          new_at_best[higher_mask] <- 1L
          new_at_best[equal_mask] <- current_at_best[equal_mask] + 1L
          
          idx_mat <- cbind(sim_idx, new_best)
          
          # 1. Calculate the pure mathematical Marginal EV
          raw_ev <- sum( Num[idx_mat] * new_at_best / (new_at_best + DenomOffset[idx_mat]) )
          
          # 2. THE JITTER PARAMETER (Adjustable)
          # A value of 0.03 means we assume a 3% margin of error in our sims.
          # Increase this to 0.05 or 0.10 to force a wider, more diverse portfolio.
          ev_jitter <- 0.1
          
          # 3. Apply the random noise
          noisy_ev <- raw_ev * runif(1, min = 1 - ev_jitter, max = 1 + ev_jitter)
          
          # 4. Compare using the noisy EV
          if (noisy_ev > best_ev) {
            best_ev <- noisy_ev  # Store the noisy EV as the high score to beat
            best_k <- k
            best_k_best <- new_best
            best_k_at_best <- new_at_best
          }
        }
        
        alloc[best_k] <- alloc[best_k] + 1L
        current_best <- best_k_best
        current_at_best <- best_k_at_best
      }
      
      # Append to allocation list
      for (k in seq_len(top_n)) {
        if (alloc[k] > 0) {
          allocation_list[[length(allocation_list) + 1]] <- data.table(
            contest_id      = g$contest_id,
            group_id        = g$group_id,
            team_name       = top$team_name[k],
            team_id         = top$team_id[k],
            n_assigned      = alloc[k],
            ev              = top$ev[k],
            p_survive_today = top$p_survive_today[k],
            p_win_contest   = top$p_win_contest[k],
            slot1_extra_name = top$slot1_extra_name[k]
          )
        }
      }
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
      death_rd_cache = death_rd_cache, scoring_sample_idx = sample_idx
    )
    attr(allocation, "portfolio_ev") <- portfolio_ev
  }

  allocation
}

# ==============================================================================
# STEP 5: OUTPUT & CONFIRMATION
# ==============================================================================

#' Print the allocation recommendation
print_allocation <- function(allocation, teams_dt) {
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

  cat(sprintf("%-35s %4s %-8s %6s %8s %9s %11s %8s\n",
              "Team", "Seed", "Region", "Entries", "Rank EV", "Win Today", "Win Contest", "Contests"))
  cat(paste(rep("-", 97), collapse = ""), "\n")

  for (i in seq_len(nrow(by_team))) {
    r <- by_team[i]
    name_str <- r$team_name
    if (has_extras && !is.na(r$slot1_extra_name)) {
      name_str <- paste0(name_str, " + ", r$slot1_extra_name)
    }
    cat(sprintf("%-35s  %2d   %-8s %5d  $%6.2f   %5.1f%%      %5.2f%%   %5d\n",
                name_str, r$seed, r$region,
                r$total_entries, r$avg_ev, 100 * r$avg_win_today,
                100 * r$avg_win_contest, r$n_contests))
  }

  total_entries <- sum(by_team$total_entries)
  cat(paste(rep("-", 97), collapse = ""), "\n")

  # --- Portfolio-level EVs (corrected, share-based) ---
  if (!is.null(portfolio_ev) && nrow(portfolio_ev) > 0) {
    total_portfolio_ev <- sum(portfolio_ev$portfolio_ev)
    total_prize_pool <- sum(portfolio_ev$prize_pool)

    cat(sprintf("TOTAL: %d entries\n\n", total_entries))
    cat("--- Portfolio EV (share-based, corrected for self-competition) ---\n")
    cat(sprintf("%-40s %10s %8s %10s %10s %7s\n",
                "Contest", "Prize Pool", "Entries", "Port. EV", "EV/Entry", "% Pool"))
    cat(paste(rep("-", 90), collapse = ""), "\n")

    for (i in seq_len(nrow(portfolio_ev))) {
      r <- portfolio_ev[i]
      cat(sprintf("%-40s $%9s %7d  $%8.2f   $%7.2f  %5.1f%%\n",
                  substr(r$contest_id, 1, 40),
                  format(r$prize_pool, big.mark = ","),
                  r$our_entries, r$portfolio_ev, r$ev_per_entry, r$pct_of_pool))
    }
    cat(paste(rep("-", 90), collapse = ""), "\n")
    cat(sprintf("%-40s $%9s %7d  $%8.2f   $%7.2f  %5.1f%%\n",
                "TOTAL",
                format(total_prize_pool, big.mark = ","),
                total_entries, total_portfolio_ev,
                total_portfolio_ev / total_entries,
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

  # Detail by contest
  cat("\n--- By Contest (detail) ---\n")
  for (cid in unique(allocation$contest_id)) {
    ct <- allocation[contest_id == cid]
    pev <- if (!is.null(portfolio_ev)) portfolio_ev[contest_id == cid] else NULL
    ev_label <- if (!is.null(pev) && nrow(pev) > 0) {
      sprintf(" | Portfolio EV=$%.2f ($%.2f/entry)", pev$portfolio_ev, pev$ev_per_entry)
    } else ""
    cat(sprintf("\n  Contest: %s (%d entries%s)\n", cid, sum(ct$n_assigned), ev_label))
    for (i in seq_len(nrow(ct))) {
      r <- ct[i]
      name_str <- r$team_name
      if ("slot1_extra_name" %in% names(ct) && !is.na(r$slot1_extra_name)) {
        name_str <- paste0(name_str, " + ", r$slot1_extra_name)
      }
      cat(sprintf("    %-35s: %d entries (Rank EV=$%.2f, Win today=%.1f%%, Win contest=%.2f%%)\n",
                  name_str, r$n_assigned, r$ev,
                  100 * r$p_survive_today, 100 * r$p_win_contest))
    }
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
run_optimizer <- function(sim_file, state_file = NULL, current_slot_id,
                           contests_df = NULL, sim_sample_size = 100000) {
  pipeline_start <- proc.time()[["elapsed"]]

  # Load simulation results
  cat(sprintf("Loading sim results from %s...\n", basename(sim_file)))
  sim <- readRDS(sim_file)
  teams_dt <- as.data.table(sim$teams)
  cat(sprintf("  %s sims x 63 games, %d teams\n",
              format(sim$n_sims, big.mark = ","), nrow(teams_dt)))

  # Precompute team-round wins
  tw <- precompute_team_wins(sim)

  # Load or init state
  if (!is.null(state_file) && file.exists(state_file)) {
    state <- load_state(state_file)
  } else if (!is.null(contests_df)) {
    state <- init_portfolio(contests_df)
  } else {
    stop("Must provide either state_file or contests_df")
  }

  # Get candidates for today
  slot <- get_slot(current_slot_id)
  candidates_dt <- get_available_picks(current_slot_id, integer(0), teams_dt)
  candidates <- candidates_dt$name
  cat(sprintf("Portfolio initialized: %d entries across %d contests",
              sum(state$alive), uniqueN(state$contest_id)))
  # Summarize formats
  fmt_counts <- state[alive == TRUE, .N, by = format]
  fmt_strs <- sprintf("%d format %s", fmt_counts$N, fmt_counts$format)
  cat(sprintf(" (%s)\n", paste(fmt_strs, collapse = ", ")))

  cat(sprintf("\nToday's candidates (%d teams): %s\n",
              length(candidates), paste(candidates, collapse = ", ")))

  # Load calibrated ownership params (falls back to defaults if no calibration file)
  own_params <- load_calibrated_params()

  # Estimate ownership for all slots (current + future)
  # Use ALL_SLOT_IDS superset so ownership covers both format A and B entries
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

  # Print ownership for today's slot
  print_ownership(current_slot_id, ownership_by_slot[[current_slot_id]],
                  teams_dt, sim$all_results)

  # Run optimization
  allocation <- optimize_today(
    state, candidates, current_slot_id, sim, tw, teams_dt,
    ownership_by_slot, sim_sample_size = sim_sample_size
  )

  # Print recommendation
  print_allocation(allocation, teams_dt)

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

  invisible(list(
    allocation       = allocation,
    portfolio_ev     = attr(allocation, "portfolio_ev"),
    state            = state,
    sim              = sim,
    tw               = tw,
    teams_dt         = teams_dt,
    ownership_by_slot = ownership_by_slot
  ))
}

cat("Splash optimizer loaded\n")
cat("Usage: result <- run_optimizer('sim_results_2026.rds', NULL, 'R1_d1', contests_df, 100000)\n")
