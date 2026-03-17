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
# STEP 2: FORWARD SIMULATION & CONTEST OUTCOME MODELING
# ==============================================================================

#' For a candidate pick today, compute survival and death-round distribution
#' across simulations.
#'
#' @param candidate_id Integer team_id being considered for today
#' @param used_teams Integer vector of team_ids already picked by this entry
#' @param current_slot_id Character current slot ID
#' @param sim List of sim results
#' @param tw Precomputed team wins from precompute_team_wins()
#' @param teams_dt Teams data frame
#' @return List with:
#'   alive_mask: logical vector (length n_sims) — TRUE if entry survives to end
#'   death_round: integer vector (length n_sims) — round entry dies (0 = survived all)
#'   survival_prob: scalar probability of surviving all rounds
forward_simulate_entry <- function(candidate_id, used_teams, current_slot_id,
                                    sim, tw, teams_dt,
                                    slot_order = SLOT_ORDER, format = "A") {
  n_sims <- sim$n_sims
  n_teams <- nrow(teams_dt)

  # Start with all sims alive
  alive <- rep(TRUE, n_sims)
  death_round <- rep(0L, n_sims)  # 0 = survived everything

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

    # Find best available teams for this future slot (by marginal win prob)
    future_probs <- tw$team_round_probs[, future_round]
    future_probs[all_used] <- -1  # exclude used teams

    # Pick top n_future_picks teams
    best_ids <- order(future_probs, decreasing = TRUE)[1:n_future_picks]
    all_used <- c(all_used, best_ids)

    # Check survival
    for (tid in best_ids) {
      round_wins <- tw$team_round_wins[[future_round]][, tid]
      died_here <- alive & !round_wins
      death_round[died_here] <- future_round
      alive <- alive & round_wins
    }
  }

  list(
    alive_mask    = alive,
    death_round   = death_round,
    survival_prob = mean(alive)
  )
}

#' Filter sim matrix to only sims where all prior picks survived
#'
#' @param state_row One row of entry state (data.table)
#' @param current_slot_id Current slot being decided
#' @param tw Precomputed team wins
#' @return Logical vector of length n_sims (TRUE = this sim is consistent)
filter_sims_for_entry <- function(state_row, current_slot_id, tw,
                                    slot_order = SLOT_ORDER) {
  current_idx <- match(current_slot_id, slot_order)
  n_sims <- nrow(tw$team_round_wins[[1]])
  mask <- rep(TRUE, n_sims)

  for (s in seq_len(current_idx - 1)) {
    sid <- slot_order[s]
    col <- slot_col_name(sid)
    pick_id <- state_row[[col]]
    if (is.na(pick_id)) next

    round_num <- get_slot(sid)$round_num
    round_wins <- tw$team_round_wins[[round_num]][, pick_id]
    mask <- mask & round_wins
  }

  mask
}

# ==============================================================================
# STEP 3: CONTEST OUTCOME & EV CALCULATION
# ==============================================================================

#' Simulate a field entry's death round for a single sim
#'
#' For each future slot, the field entry picks a team drawn from the ownership
#' distribution. We check if that team wins in this sim.
#'
#' @param sim_idx Integer sim index
#' @param ownership_by_slot List of named vectors: slot_id -> (team_name -> fraction)
#' @param teams_dt Teams data frame
#' @param tw Precomputed team wins
#' @param field_used Integer vector of team_ids already used by this field entry
#' @param start_slot_idx Integer index into SLOT_ORDER to start from
#' @return Integer death round (0 = survived everything)
simulate_field_entry <- function(sim_idx, ownership_by_slot, teams_dt, tw,
                                  field_used, start_slot_idx,
                                  slot_order = SLOT_ORDER, format = "A") {
  used <- field_used

  for (s in start_slot_idx:length(slot_order)) {
    sid <- slot_order[s]
    slot <- get_slot(sid)
    round_num <- slot$round_num
    n_picks <- get_n_picks(sid, format)

    own <- ownership_by_slot[[sid]]
    if (is.null(own) || length(own) == 0) next

    # Remove teams this field entry already used
    avail_teams <- setdiff(names(own), teams_dt$name[teams_dt$team_id %in% used])
    if (length(avail_teams) == 0) {
      # Field entry has no available picks — dead
      return(round_num)
    }

    # Renormalize ownership over available teams
    avail_own <- own[avail_teams]
    avail_own <- avail_own / sum(avail_own)

    # Draw picks from ownership distribution
    picks <- sample(avail_teams, size = min(n_picks, length(avail_teams)),
                    prob = avail_own, replace = FALSE)
    pick_ids <- teams_dt$team_id[match(picks, teams_dt$name)]
    used <- c(used, pick_ids)

    # Check if picks survive
    for (tid in pick_ids) {
      if (!tw$team_round_wins[[round_num]][sim_idx, tid]) {
        return(round_num)  # died this round
      }
    }
  }

  return(0L)  # survived everything
}

#' Compute EV for a candidate pick in a specific contest
#'
#' Uses analytical field death-round computation: for each sim, we know which
#' teams won each round (from the sim matrix) and the ownership distribution.
#' P(field entry survives slot s) = sum(ownership on winning teams), computed
#' via matrix multiply. No Monte Carlo sampling of field entries needed.
#'
#' @param candidate_id Integer team_id for today's pick
#' @param group A row from group_entries() output
#' @param current_slot_id Current slot
#' @param sim Sim results list
#' @param tw Precomputed team wins
#' @param teams_dt Teams data frame
#' @param ownership_by_slot List of ownership vectors per slot
#' @param sim_sample_size Number of sims to evaluate (subsample for memory)
#' @return List with: ev, p_survive_today, p_win_contest, mean_death_rd
compute_candidate_ev <- function(candidate_id, group, current_slot_id,
                                  sim, tw, teams_dt, ownership_by_slot,
                                  sim_sample_size = 100000) {
  n_sims <- sim$n_sims
  n_teams <- nrow(teams_dt)
  contest_size <- group$contest_size
  prize_pool <- group$prize_pool
  our_n <- group$n_entries
  used_teams <- group$used_teams[[1]]
  full_field <- contest_size - our_n

  # Determine slot order for this group's format
  group_format <- if ("format" %in% names(group)) group$format else "A"
  slot_order <- get_slot_order(group_format)
  current_idx <- match(current_slot_id, slot_order)
  remaining_slots <- slot_order[current_idx:length(slot_order)]
  n_remaining <- length(remaining_slots)

  # Subsample sims for memory efficiency (matrices are n_sims × n_slots)
  if (n_sims > sim_sample_size) {
    sample_idx <- sample.int(n_sims, sim_sample_size)
  } else {
    sample_idx <- seq_len(n_sims)
    sim_sample_size <- n_sims
  }

  # --- Our entry's forward simulation ---
  our_result <- forward_simulate_entry(candidate_id, used_teams, current_slot_id,
                                        sim, tw, teams_dt,
                                        slot_order = slot_order, format = group_format)
  our_death <- our_result$death_round[sample_idx]  # subsample

  # --- Analytical field death-round distribution ---
  # For each remaining slot, compute P(random field entry survives this slot | sim)
  # as the ownership-weighted fraction of teams that won.
  # This is exact (no sampling noise) and computed via matrix multiply.

  # slot_survive[sim, slot_pos] = P(field entry survives this slot)
  slot_survive <- matrix(1, nrow = sim_sample_size, ncol = n_remaining)
  slot_round_nums <- integer(n_remaining)

  for (si in seq_along(remaining_slots)) {
    sid <- remaining_slots[si]
    slot <- get_slot(sid)
    slot_round_nums[si] <- slot$round_num
    n_picks <- get_n_picks(sid, group_format)

    own <- ownership_by_slot[[sid]]
    if (is.null(own) || length(own) == 0) next  # no pick needed, auto-survive

    # Map ownership (named by team_name) to team_id-indexed vector
    own_vec <- numeric(n_teams)
    team_ids <- teams_dt$team_id[match(names(own), teams_dt$name)]
    valid <- !is.na(team_ids)
    own_vec[team_ids[valid]] <- own[names(own)[valid]]

    # P(a random pick from ownership survives) = ownership . winners
    # = win_mat[sampled_sims, ] %*% own_vec
    win_mat_sub <- tw$team_round_wins[[slot$round_num]][sample_idx, , drop = FALSE]
    p_single <- as.numeric(win_mat_sub %*% own_vec)

    # Multi-pick slots (e.g., Format C R1): all picks must survive.
    # Approximate P(all n picks survive) ≈ p^n (independence assumption,
    # ignores without-replacement constraint — fine when ownership is spread
    # across many teams).
    if (n_picks > 1) {
      slot_survive[, si] <- p_single ^ n_picks
    } else {
      slot_survive[, si] <- p_single
    }
  }

  # Cumulative survival: P(field survives slots 1..s)
  cum_survive <- slot_survive
  if (n_remaining >= 2) {
    for (si in 2:n_remaining) {
      cum_survive[, si] <- cum_survive[, si - 1] * slot_survive[, si]
    }
  }

  # Aggregate per-slot death probabilities into per-round death probabilities.
  # Multiple slots can share a round_num (e.g., R1_d1 and R1_d2 both round 1).
  max_round <- 6L
  p_field_dies_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)

  for (si in seq_along(remaining_slots)) {
    rd <- slot_round_nums[si]
    if (si == 1) {
      p_die <- 1 - slot_survive[, si]
    } else {
      p_die <- cum_survive[, si - 1] * (1 - slot_survive[, si])
    }
    p_field_dies_round[, rd] <- p_field_dies_round[, rd] + p_die
  }

  # P(field entry survives everything | sim)
  p_field_all <- cum_survive[, n_remaining]

  # --- Compute expected payout per sim (vectorized by death round) ---
  payouts <- numeric(sim_sample_size)

  # Case 1: We survived everything (our_death == 0)
  survived_mask <- our_death == 0L
  if (any(survived_mask)) {
    expected_co <- p_field_all[survived_mask] * full_field
    payouts[survived_mask] <- prize_pool / (1 + expected_co)
  }

  # Case 2: We died in round d
  for (d in 1:max_round) {
    died_mask <- our_death == d
    if (!any(died_mask)) next

    # P(a single field entry outlasts us) = P(survive all) + P(die in round > d)
    p_outlast <- p_field_all[died_mask]
    if (d < max_round) {
      for (rd in (d + 1):max_round) {
        p_outlast <- p_outlast + p_field_dies_round[died_mask, rd]
      }
    }

    # P(nobody in the full field outlasts us) = (1 - p_outlast)^N
    p_nobody <- (1 - p_outlast) ^ full_field

    # E[field entries that die in same round as us]
    p_same <- p_field_dies_round[died_mask, d]
    expected_same <- p_same * full_field

    # EV = P(nobody outlasts) × prize / (1 + expected same-round deaths)
    payouts[died_mask] <- p_nobody * prize_pool / (1 + expected_same)
  }

  # --- Metrics ---
  today_round <- get_slot(current_slot_id)$round_num
  p_survive_today <- mean(tw$team_round_wins[[today_round]][, candidate_id])
  p_win_contest <- mean(payouts > 0)
  died_sims <- our_death[our_death > 0]
  avg_death_rd <- if (length(died_sims) > 0) mean(died_sims) else NA_real_

  list(
    ev              = mean(payouts),
    p_survive_today = p_survive_today,
    p_win_contest   = p_win_contest,
    mean_death_rd   = avg_death_rd,
    p_survive_all   = our_result$survival_prob
  )
}

# ==============================================================================
# STEP 4: PORTFOLIO ALLOCATION
# ==============================================================================

#' Optimize today's allocation of entries across candidate teams
#'
#' @param state data.table of entry state
#' @param candidates Character vector of team names available today
#' @param current_slot_id Current slot ID
#' @param sim Sim results list
#' @param tw Precomputed team wins
#' @param teams_dt Teams data frame
#' @param ownership_by_slot List of ownership vectors per slot
#' @param sim_sample_size Sims to use per EV calculation
#' @return data.table with columns: team_name, team_id, n_entries, ev_per_entry,
#'   survival_prob, ownership, leverage
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

  # Count total evaluations needed (for progress)
  total_evals <- 0L
  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    n_avail <- sum(!candidate_ids %in% g$used_teams[[1]])
    total_evals <- total_evals + n_avail
  }
  cat(sprintf("Total evaluations: %d groups x candidates = %d\n",
              nrow(groups), total_evals))

  # Score each candidate for each group
  all_scores <- list()
  eval_num <- 0L
  overall_start <- proc.time()[["elapsed"]]

  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    cat(sprintf("\n--- Group %d/%d: contest=%s, %d entries, %d field ---\n",
                gi, nrow(groups), g$contest_id, g$n_entries, g$contest_size))

    for (ci in seq_along(candidate_ids)) {
      cid <- candidate_ids[ci]
      cname <- candidates[ci]

      # Skip if this team is already used by this group
      if (cid %in% g$used_teams[[1]]) next

      eval_num <- eval_num + 1L
      elapsed <- proc.time()[["elapsed"]] - overall_start
      rate <- eval_num / max(elapsed, 0.001)
      remaining_evals <- total_evals - eval_num
      eta_secs <- remaining_evals / max(rate, 0.001)

      if (eta_secs < 60) {
        eta_str <- sprintf("%.0fs", eta_secs)
      } else if (eta_secs < 3600) {
        eta_str <- sprintf("%.1fm", eta_secs / 60)
      } else {
        eta_str <- sprintf("%.1fh", eta_secs / 3600)
      }

      cat(sprintf("  [%d/%d | ETA %s] %s (id=%d)...\n",
                  eval_num, total_evals, eta_str, cname, cid))

      ev_result <- compute_candidate_ev(
        cid, g, current_slot_id, sim, tw, teams_dt,
        ownership_by_slot, sim_sample_size = sim_sample_size
      )
      cat(sprintf("    => EV=$%.2f, P(win today)=%.1f%%, P(win contest)=%.3f%%, Avg death rd=%.1f\n",
                  ev_result$ev, 100 * ev_result$p_survive_today,
                  100 * ev_result$p_win_contest,
                  ifelse(is.na(ev_result$mean_death_rd), 0, ev_result$mean_death_rd)))

      all_scores[[length(all_scores) + 1]] <- data.table(
        group_id        = g$group_id,
        contest_id      = g$contest_id,
        n_entries       = g$n_entries,
        team_name       = cname,
        team_id         = cid,
        ev              = ev_result$ev,
        p_survive_today = ev_result$p_survive_today,
        p_win_contest   = ev_result$p_win_contest,
        mean_death_rd   = ev_result$mean_death_rd
      )
    }
  }

  total_elapsed <- proc.time()[["elapsed"]] - overall_start
  if (total_elapsed < 60) {
    time_str <- sprintf("%.1fs", total_elapsed)
  } else if (total_elapsed < 3600) {
    time_str <- sprintf("%.1fm", total_elapsed / 60)
  } else {
    time_str <- sprintf("%.1fh", total_elapsed / 3600)
  }
  cat(sprintf("\n=== Scoring complete: %d evaluations in %s ===\n",
              eval_num, time_str))

  scores <- rbindlist(all_scores)

  if (nrow(scores) == 0) {
    cat("No valid candidates for any group.\n")
    return(data.table())
  }

  # --- Greedy assignment ---
  # For each group, assign to highest-EV candidate
  allocation_list <- list()

  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    g_scores <- scores[group_id == g$group_id]
    if (nrow(g_scores) == 0) next

    setorder(g_scores, -ev)
    best <- g_scores[1]

    # --- Diversification within contest ---
    # If we have many entries in this contest, spread across top candidates
    # Rule of thumb: max 60% of entries on any single team
    n_ent <- g$n_entries
    max_per_team <- max(1, ceiling(n_ent * 0.6))

    if (n_ent <= 3 || nrow(g_scores) <= 1) {
      # Small group or only one option: all on best
      allocation_list[[length(allocation_list) + 1]] <- data.table(
        contest_id      = g$contest_id,
        group_id        = g$group_id,
        team_name       = best$team_name,
        team_id         = best$team_id,
        n_assigned      = n_ent,
        ev              = best$ev,
        p_survive_today = best$p_survive_today,
        p_win_contest   = best$p_win_contest
      )
    } else {
      # Spread across top candidates proportional to EV
      top_n <- min(3, nrow(g_scores))
      top <- g_scores[1:top_n]

      # Weight by EV (shifted so all positive)
      evs <- pmax(top$ev, 0.01)
      weights <- evs / sum(evs)

      # Allocate entries
      raw_alloc <- round(weights * n_ent)
      raw_alloc <- pmin(raw_alloc, max_per_team)
      # Ensure we don't over/under-allocate
      diff <- n_ent - sum(raw_alloc)
      if (diff > 0) raw_alloc[1] <- raw_alloc[1] + diff
      if (diff < 0) {
        for (k in top_n:1) {
          reduce <- min(-diff, raw_alloc[k] - 1)
          raw_alloc[k] <- raw_alloc[k] - reduce
          diff <- diff + reduce
          if (diff == 0) break
        }
      }

      for (k in seq_len(top_n)) {
        if (raw_alloc[k] > 0) {
          allocation_list[[length(allocation_list) + 1]] <- data.table(
            contest_id      = g$contest_id,
            group_id        = g$group_id,
            team_name       = top$team_name[k],
            team_id         = top$team_id[k],
            n_assigned      = raw_alloc[k],
            ev              = top$ev[k],
            p_survive_today = top$p_survive_today[k],
            p_win_contest   = top$p_win_contest[k]
          )
        }
      }
    }
  }

  allocation <- rbindlist(allocation_list)

  # Add ownership and leverage
  if (!is.null(ownership)) {
    allocation[, ownership := ownership[team_name], by = team_name]
    allocation[, leverage := p_survive_today / pmax(ownership, 0.001)]
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

  cat("\n========================================================\n")
  cat("          ALLOCATION RECOMMENDATION\n")
  cat("========================================================\n\n")

  # Summary by team
  by_team <- allocation[, .(
    total_entries   = sum(n_assigned),
    avg_ev          = weighted.mean(ev, n_assigned),
    avg_win_today   = weighted.mean(p_survive_today, n_assigned),
    avg_win_contest = weighted.mean(p_win_contest, n_assigned),
    n_contests      = uniqueN(contest_id)
  ), by = .(team_name, team_id)]

  # Add seed
  by_team[, seed := teams_dt$seed[match(team_id, teams_dt$team_id)]]
  by_team[, region := teams_dt$region[match(team_id, teams_dt$team_id)]]
  setorder(by_team, -total_entries)

  cat(sprintf("%-20s %4s %-8s %6s %8s %9s %11s %8s\n",
              "Team", "Seed", "Region", "Entries", "Avg EV", "Win Today", "Win Contest", "Contests"))
  cat(paste(rep("-", 82), collapse = ""), "\n")

  for (i in seq_len(nrow(by_team))) {
    r <- by_team[i]
    cat(sprintf("%-20s  %2d   %-8s %5d  $%6.2f   %5.1f%%      %5.2f%%   %5d\n",
                r$team_name, r$seed, r$region,
                r$total_entries, r$avg_ev, 100 * r$avg_win_today,
                100 * r$avg_win_contest, r$n_contests))
  }

  total_entries <- sum(by_team$total_entries)
  total_ev <- sum(allocation$ev * allocation$n_assigned)
  cat(paste(rep("-", 82), collapse = ""), "\n")
  cat(sprintf("TOTAL: %d entries, Portfolio EV = $%.2f\n\n", total_entries, total_ev))

  # Detail by contest
  cat("--- By Contest ---\n")
  for (cid in unique(allocation$contest_id)) {
    ct <- allocation[contest_id == cid]
    cat(sprintf("\n  Contest: %s (%d entries)\n", cid, sum(ct$n_assigned)))
    for (i in seq_len(nrow(ct))) {
      r <- ct[i]
      cat(sprintf("    %-20s: %d entries (EV=$%.2f, Win today=%.1f%%, Win contest=%.2f%%)\n",
                  r$team_name, r$n_assigned, r$ev,
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
  cat(sprintf("\nToday's candidates (%d teams): %s\n",
              length(candidates), paste(candidates, collapse = ", ")))

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
    own <- estimate_ownership(sid, teams_dt, sim$all_results, sim$round_info)
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
    state            = state,
    sim              = sim,
    tw               = tw,
    teams_dt         = teams_dt,
    ownership_by_slot = ownership_by_slot
  ))
}

cat("Splash optimizer loaded\n")
cat("Usage: result <- run_optimizer('sim_results_2026.rds', 'splash_state/portfolio.rds', 'R1_d1')\n")
