#!/usr/bin/env Rscript
# ==============================================================================
# splash_field_sim.R
# Grouped field entry simulation for Splash NCAA survivor pools.
#
# R orchestration layer that:
#   1. Groups alive field entries by identical pick history
#   2. Predicts field pick probabilities per group per slot
#   3. Calls simulate_field_survival_cpp() for the hot loop
#   4. Converts output to survival curves for the optimizer
#
# Dependencies:
#   source("R/splash_config.R")
#   source("R/splash_prepare.R")
#   Rcpp::sourceCpp("simulate_tourney.cpp")  # for simulate_field_survival_cpp
# ==============================================================================

library(data.table)

# ==============================================================================
# STEP 5a: GROUP FIELD ENTRIES
# ==============================================================================

#' Group alive field entries by identical pick history
#'
#' Entries with the same set of used team_ids have identical future behavior
#' in the simulation, so we can batch them.
#'
#' @param field_avail Single contest entry from compute_field_availability()
#'   (list with alive_count, used_teams_by_entry)
#' @return data.table with group_id, used_teams (list col), n_entries
group_field_entries <- function(field_avail) {
  used_lists <- field_avail$used_teams_by_entry

  if (length(used_lists) == 0) {
    return(data.table(group_id = integer(0),
                      used_teams = list(),
                      n_entries = integer(0)))
  }

  # Fingerprint = sorted, comma-joined team_ids
  fingerprints <- vapply(used_lists, function(ids) {
    paste(sort(ids), collapse = ",")
  }, character(1))

  # Group by fingerprint
  dt <- data.table(fingerprint = fingerprints)
  groups <- dt[, .(n_entries = .N), by = fingerprint]
  groups[, group_id := .I]

  # Reconstruct used_teams list column
  groups[, used_teams := lapply(fingerprint, function(fp) {
    parts <- strsplit(fp, ",")[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) integer(0) else as.integer(parts)
  })]

  groups[, fingerprint := NULL]
  setcolorder(groups, c("group_id", "used_teams", "n_entries"))

  cat(sprintf("  Field grouped: %d entries -> %d groups\n",
              sum(groups$n_entries), nrow(groups)))
  groups
}

# ==============================================================================
# STEP 5b: PREDICT FIELD GROUP PICKS
# ==============================================================================

#' Compute pick probability distribution for each field group in a slot
#'
#' @param field_groups data.table from group_field_entries()
#' @param slot_id Character slot ID
#' @param teams_dt Data frame with team_id, name, seed, region, rating
#' @param sim_matrix The all_results matrix (n_sims x 63)
#' @param contest_size Total entries in the contest (for save scaling)
#' @return List with:
#'   pick_probs: NumericMatrix (n_groups x n_candidates)
#'   team_ids:   IntegerVector of candidate team_ids
#'   game_cols:  IntegerVector of 0-indexed game columns per candidate
predict_field_group_picks <- function(field_groups, slot_id, teams_dt,
                                       sim_matrix, contest_size) {
  slot <- get_slot(slot_id)
  game_idxs <- slot$game_indices
  round_num <- slot$round_num
  n_groups <- nrow(field_groups)

  # --- Get candidate teams for this slot ---
  if (round_num == 1) {
    candidate_ids <- integer(0)
    for (g in game_idxs) {
      candidate_ids <- c(candidate_ids,
                         teams_dt$team_id[2 * g - 1],
                         teams_dt$team_id[2 * g])
    }
    candidates <- teams_dt[teams_dt$team_id %in% unique(candidate_ids), ]
  } else {
    # For later rounds, use sim to find possible participants
    all_winners <- integer(0)
    for (g in game_idxs) {
      all_winners <- c(all_winners, unique(sim_matrix[, g]))
    }
    candidates <- teams_dt[teams_dt$team_id %in% unique(all_winners), ]
  }

  n_cand <- nrow(candidates)
  if (n_cand == 0) {
    return(list(
      pick_probs = matrix(0, nrow = n_groups, ncol = 0),
      team_ids   = integer(0),
      game_cols  = integer(0)
    ))
  }

  # --- Win probability per candidate ---
  n_sims <- nrow(sim_matrix)
  win_probs <- numeric(n_cand)
  for (i in seq_len(n_cand)) {
    tid <- candidates$team_id[i]
    wins <- 0
    for (g in game_idxs) {
      wins <- wins + sum(sim_matrix[, g] == tid)
    }
    win_probs[i] <- wins / n_sims
  }

  # --- Future value (championship equity, for save effect) ---
  champ_col <- 63
  champ_probs <- numeric(n_cand)
  for (i in seq_len(n_cand)) {
    champ_probs[i] <- sum(sim_matrix[, champ_col] == candidates$team_id[i]) / n_sims
  }
  max_champ <- max(champ_probs, na.rm = TRUE)
  fv <- if (max_champ > 0) champ_probs / max_champ else rep(0, n_cand)

  # --- Save strength scaled by contest size ---
  # Larger contests → field values chalk more, saves more
  save_strengths <- c(0.60, 0.50, 0.35, 0.15, 0.0, 0.0)
  save_strength <- save_strengths[min(round_num, length(save_strengths))]

  # --- Path viability boost ---
  boost_val <- PATH_VIABILITY_BOOST[[as.character(round_num)]]
  if (is.null(boost_val)) boost_val <- 1.0

  # --- Map candidates to game columns (0-indexed for C++) ---
  # For each candidate, which game column in all_results could they win?
  game_col_map <- integer(n_cand)
  for (i in seq_len(n_cand)) {
    tid <- candidates$team_id[i]
    for (g in game_idxs) {
      if (any(sim_matrix[, g] == tid)) {
        game_col_map[i] <- g - 1L  # 0-indexed for C++
        break
      }
    }
  }

  # --- Build probability matrix: n_groups x n_candidates ---
  beta_wp <- 4.0  # softmax temperature for win prob
  pick_probs <- matrix(0.0, nrow = n_groups, ncol = n_cand)

  for (grp in seq_len(n_groups)) {
    used <- field_groups$used_teams[[grp]]

    # Base attractiveness (before availability/viability adjustments)
    log_attract <- beta_wp * win_probs - save_strength * fv

    for (i in seq_len(n_cand)) {
      tid <- candidates$team_id[i]

      # Zero out already-used teams
      if (tid %in% used) {
        log_attract[i] <- -Inf
        next
      }

      # Path viability boost
      if (!is.infinite(boost_val) && boost_val > 1.0) {
        # Check if this candidate's opponent in the bracket is in used_teams
        # For R64: opponent is the other team in the same game
        # For later rounds: opponent is whoever they'd face
        # Simplified: check if any team in the same game bracket is in used
        g_idx <- game_col_map[i] + 1  # back to 1-indexed
        opponent_used <- FALSE
        if (g_idx >= 1 && g_idx <= 32) {
          opp_pos <- if (candidates$team_id[i] == teams_dt$team_id[2 * g_idx - 1]) {
            2 * g_idx
          } else {
            2 * g_idx - 1
          }
          if (teams_dt$team_id[opp_pos] %in% used) {
            opponent_used <- TRUE
          }
        }
        if (opponent_used) {
          log_attract[i] <- log_attract[i] + log(boost_val)
        }
      } else if (is.infinite(boost_val)) {
        # Mandatory viability: zero out non-viable picks
        # A pick is "viable" if its opponent IS in used_teams
        # (meaning picking this team doesn't waste a future slot)
        # For E8+, this is critical
        g_idx <- game_col_map[i] + 1
        viable <- FALSE

        if (g_idx >= 1) {
          # Check if any opponent in this game's bracket region is in used
          # Simplified: for E8+, the team needs to face someone the entry
          # already used, so eliminating the opponent opens path
          if (g_idx <= 32) {
            opp_pos <- if (candidates$team_id[i] == teams_dt$team_id[2 * g_idx - 1]) {
              2 * g_idx
            } else {
              2 * g_idx - 1
            }
            viable <- teams_dt$team_id[opp_pos] %in% used
          } else {
            # For later rounds, any pick against a used team is viable
            # Check all teams in the game's feeder bracket
            viable <- TRUE  # simplification for now
          }
        }

        if (!viable) {
          log_attract[i] <- -Inf
        }
      }
    }

    # Handle all-zeroed case (dead end or all non-viable)
    finite_mask <- is.finite(log_attract)
    if (!any(finite_mask)) {
      # Fall back to full softmax (no viability constraint)
      log_attract <- beta_wp * win_probs - save_strength * fv
      for (i in seq_len(n_cand)) {
        if (candidates$team_id[i] %in% used) log_attract[i] <- -Inf
      }
      finite_mask <- is.finite(log_attract)
    }

    if (!any(finite_mask)) {
      # True dead end: no available teams
      pick_probs[grp, ] <- 0.0
      next
    }

    # Normalize to probabilities
    log_attract[!finite_mask] <- -Inf
    log_attract <- log_attract - max(log_attract[finite_mask])
    raw <- exp(log_attract)
    pick_probs[grp, ] <- raw / sum(raw)
  }

  list(
    pick_probs = pick_probs,
    team_ids   = as.integer(candidates$team_id),
    game_cols  = as.integer(game_col_map)
  )
}

# ==============================================================================
# STEP 5c: RUN FIELD SIMULATION
# ==============================================================================

#' Run field survival simulation for a single contest
#'
#' @param field_groups data.table from group_field_entries()
#' @param sim Sim object with all_results, teams, etc.
#' @param remaining_slots Character vector of slot IDs still to play
#' @param contest_size Total entries in the contest
#' @return IntegerMatrix (n_sims x n_groups): death slot (1-indexed),
#'   or n_slots+1 if survived all
run_field_sim <- function(field_groups, sim, remaining_slots, contest_size) {
  n_groups <- nrow(field_groups)
  n_slots <- length(remaining_slots)
  sim_matrix <- sim$all_results
  teams_dt <- sim$teams

  if (n_groups == 0 || n_slots == 0) {
    return(matrix(n_slots + 1L, nrow = nrow(sim_matrix), ncol = max(n_groups, 1)))
  }

  cat(sprintf("  Running field sim: %d groups x %d slots x %d sims...\n",
              n_groups, n_slots, nrow(sim_matrix)))

  # Build probability matrices and game mappings per slot
  all_team_ids <- list()
  all_game_cols <- list()
  all_pick_probs <- list()

  for (s in seq_along(remaining_slots)) {
    slot_id <- remaining_slots[s]
    preds <- predict_field_group_picks(field_groups, slot_id, teams_dt,
                                        sim_matrix, contest_size)
    all_team_ids[[s]] <- preds$team_ids
    all_game_cols[[s]] <- preds$game_cols
    all_pick_probs[[s]] <- preds$pick_probs
  }

  # Build group_used matrix (n_groups x max_used, 0-padded)
  max_used <- max(vapply(field_groups$used_teams, length, integer(1)), 1L)
  group_used <- matrix(0L, nrow = n_groups, ncol = max_used)
  for (g in seq_len(n_groups)) {
    ut <- field_groups$used_teams[[g]]
    if (length(ut) > 0) {
      group_used[g, seq_along(ut)] <- as.integer(ut)
    }
  }

  # Call C++ hot loop
  t0 <- proc.time()["elapsed"]
  death_matrix <- simulate_field_survival_cpp(
    all_results      = sim_matrix,
    group_used       = group_used,
    group_sizes      = as.integer(field_groups$n_entries),
    slot_team_ids    = all_team_ids,
    slot_game_cols   = all_game_cols,
    group_pick_probs = all_pick_probs,
    n_slots          = as.integer(n_slots)
  )
  elapsed <- proc.time()["elapsed"] - t0
  cat(sprintf("  C++ field sim: %.1f seconds\n", elapsed))

  death_matrix
}

# ==============================================================================
# STEP 5d: BUILD SURVIVAL CURVES
# ==============================================================================

#' Convert death_round matrix into survival curves for the optimizer
#'
#' @param death_matrix IntegerMatrix (n_sims x n_groups) from run_field_sim()
#' @param field_groups data.table with group_id, n_entries
#' @param n_slots Number of remaining slots simulated
#' @return List with:
#'   field_dies_slot: matrix (n_sims x n_slots) — weighted fraction dying in each slot
#'   p_field_survives_all: numeric vector (n_sims) — weighted fraction surviving everything
build_field_survival_curves <- function(death_matrix, field_groups, n_slots) {
  n_sims <- nrow(death_matrix)
  n_groups <- ncol(death_matrix)
  weights <- field_groups$n_entries
  total_weight <- sum(weights)

  if (total_weight == 0) {
    return(list(
      field_dies_slot       = matrix(0, nrow = n_sims, ncol = n_slots),
      p_field_survives_all  = rep(1, n_sims)
    ))
  }

  # field_dies_slot[sim, slot] = weighted fraction of field dying in exactly slot s
  field_dies_slot <- matrix(0.0, nrow = n_sims, ncol = n_slots)
  p_field_survives_all <- numeric(n_sims)

  for (sim in seq_len(n_sims)) {
    for (grp in seq_len(n_groups)) {
      d <- death_matrix[sim, grp]
      w <- weights[grp] / total_weight

      if (d <= n_slots) {
        field_dies_slot[sim, d] <- field_dies_slot[sim, d] + w
      } else {
        p_field_survives_all[sim] <- p_field_survives_all[sim] + w
      }
    }
  }

  list(
    field_dies_slot       = field_dies_slot,
    p_field_survives_all  = p_field_survives_all
  )
}

# ==============================================================================
# CONVENIENCE: RUN FULL FIELD SIM FOR A CONTEST
# ==============================================================================

#' Run complete field simulation pipeline for one contest
#'
#' @param field_avail Single contest entry from compute_field_availability()
#' @param sim Sim object
#' @param remaining_slots Character vector of remaining slot IDs
#' @param contest_size Total entries in the contest
#' @return List with field_groups, death_matrix, survival_curves
run_contest_field_sim <- function(field_avail, sim, remaining_slots, contest_size) {
  # Group field entries
  field_groups <- group_field_entries(field_avail)

  if (nrow(field_groups) == 0) {
    n_sims <- nrow(sim$all_results)
    n_slots <- length(remaining_slots)
    return(list(
      field_groups    = field_groups,
      death_matrix    = matrix(n_slots + 1L, nrow = n_sims, ncol = 0),
      survival_curves = list(
        field_dies_slot      = matrix(0, nrow = n_sims, ncol = n_slots),
        p_field_survives_all = rep(1, n_sims)
      )
    ))
  }

  # Run simulation
  death_matrix <- run_field_sim(field_groups, sim, remaining_slots, contest_size)

  # Build curves
  survival_curves <- build_field_survival_curves(
    death_matrix, field_groups, length(remaining_slots)
  )

  list(
    field_groups    = field_groups,
    death_matrix    = death_matrix,
    survival_curves = survival_curves
  )
}

cat("Splash field sim module loaded\n")
