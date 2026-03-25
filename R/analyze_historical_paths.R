#!/usr/bin/env Rscript
# ==============================================================================
# analyze_historical_paths.R
#
# Analyzes 2024 & 2025 Splash survivor pool data to understand field behavior
# around path viability in S16 and E8. Produces structured outputs for building
# an entry-level ownership model for 2026.
#
# Usage:
#   source("R/analyze_historical_paths.R")
#   # Returns a list with analysis data.tables
# ==============================================================================

library(data.table)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) {
  if (file.exists("R/splash_config.R")) "R"
  else if (file.exists("splash_config.R")) "."
  else stop("Cannot determine script_dir. Set working directory to the project root.")
})

source(file.path(script_dir, "splash_config.R"))
source(file.path(script_dir, "splash_ownership.R"))
source(file.path(script_dir, "splash_prepare.R"))
source(file.path(script_dir, "splash_optimizer.R"))

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

ROUND_BASE <- c(0L, 32L, 48L, 56L, 60L, 62L)

#' Map a team_id to its game index in a given round
game_for_team <- function(team_id, round_num) {
  ROUND_BASE[round_num] + as.integer(ceiling(team_id / 2^round_num))
}

#' Get the two feeder game indices for a given game
get_feeder_games <- function(game_idx) {
  if (game_idx <= 32L) return(NULL)  # R64 has no feeders
  if (game_idx <= 48L) { base_cur <- 32L; base_prev <- 0L }
  else if (game_idx <= 56L) { base_cur <- 48L; base_prev <- 32L }
  else if (game_idx <= 60L) { base_cur <- 56L; base_prev <- 48L }
  else if (game_idx <= 62L) { base_cur <- 60L; base_prev <- 56L }
  else { base_cur <- 62L; base_prev <- 60L }
  offset <- game_idx - base_cur
  c(base_prev + 2L * offset - 1L, base_prev + 2L * offset)
}

#' Get round number for a game index
game_to_round <- function(game_idx) {
  findInterval(game_idx, ROUND_BASE + 1L)
}

#' Reconstruct actual tournament winners from pick data
#'
#' @param picks_long data.table from parse_splash_results with team_id column added
#' @return Integer vector of length 63: actual_winners[g] = team_id of game g winner
reconstruct_results <- function(picks_long) {
  actual_winners <- integer(63)

  # Slot to round mapping
  slot_round <- c(R1_d1 = 1L, R1_d2 = 1L, R2_d1 = 2L, R2_d2 = 2L,
                  S16_d1 = 3L, S16_d2 = 3L, E8 = 4L, FF = 5L, CHAMP = 6L)

  # Get winning picks (unique teams that won in each round)
  winning <- picks_long[won == TRUE & !is.na(team_id),
                         .(team_id = unique(team_id)),
                         by = slot_id]
  winning[, round_num := slot_round[slot_id]]

  for (i in seq_len(nrow(winning))) {
    tid <- winning$team_id[i]
    rn <- winning$round_num[i]
    gidx <- game_for_team(tid, rn)
    actual_winners[gidx] <- tid
  }

  actual_winners
}

#' Get the opponent of a team in a given game
#'
#' @param team_id The team whose opponent we want
#' @param game_idx The game index (1-63)
#' @param actual_winners The reconstructed results vector
#' @return The opponent's team_id, or NA if can't determine
get_opponent <- function(team_id, game_idx, actual_winners) {
  if (game_idx <= 32L) {
    # R64: teams at positions 2g-1 and 2g
    t1 <- 2L * game_idx - 1L
    t2 <- 2L * game_idx
    if (team_id == t1) return(t2)
    if (team_id == t2) return(t1)
    return(NA_integer_)
  }
  # Later rounds: opponents are winners of feeder games
  feeders <- get_feeder_games(game_idx)
  participants <- actual_winners[feeders]
  if (team_id == participants[1]) return(participants[2])
  if (team_id == participants[2]) return(participants[1])
  NA_integer_
}

#' Get all teams participating in a game (for rounds 2+, these are feeder winners)
get_game_participants <- function(game_idx, actual_winners) {
  if (game_idx <= 32L) {
    return(c(2L * game_idx - 1L, 2L * game_idx))
  }
  feeders <- get_feeder_games(game_idx)
  actual_winners[feeders]
}

#' Compute conditional win probability for a team in a specific game
#' from the pre-tournament simulation matrix
#'
#' @param team_id Team
#' @param game_idx Game column in sim matrix
#' @param sim_matrix The all_results matrix (n_sims x 63)
#' @return Win probability conditional on reaching this game
compute_cond_win_prob <- function(team_id, game_idx, sim_matrix) {
  round_num <- game_to_round(game_idx)
  if (round_num == 1L) {
    # R64: win prob = fraction of sims where team wins this game
    return(sum(sim_matrix[, game_idx] == team_id) / nrow(sim_matrix))
  }
  # For later rounds: P(win game | reached game)
  # "reached game" = won the feeder game in prior round
  feeder_game <- game_for_team(team_id, round_num - 1L)
  reached <- sum(sim_matrix[, feeder_game] == team_id)
  if (reached == 0) return(0)
  won <- sum(sim_matrix[, game_idx] == team_id)
  won / reached
}

#' Compute championship equity for a team from sim matrix
#' @return Fraction of sims where this team wins the championship
compute_champ_equity <- function(team_id, sim_matrix) {
  sum(sim_matrix[, 63] == team_id) / nrow(sim_matrix)
}

#' Check if an entry has a viable path using actual tournament winners
#'
#' @param used_teams Integer vector of already-picked team_ids
#' @param used_rounds Integer vector of round numbers for each pick
#' @param remaining_rounds List of remaining (slot_label, round_num, game_indices) to fill
#' @param actual_winners The actual results vector
#' @return TRUE if a valid completion exists
check_viable_path_actual <- function(used_teams, used_rounds,
                                      remaining_rounds, actual_winners) {
  if (length(remaining_rounds) == 0L) return(TRUE)

  slot_info <- remaining_rounds[[1]]
  round_num <- slot_info$round_num
  game_indices <- slot_info$game_indices
  n_picks <- slot_info$n_picks

  # Get actual winners in these games that are unused
  candidates <- actual_winners[game_indices]
  candidates <- candidates[candidates > 0L]
  candidates <- setdiff(candidates, used_teams)

  # Filter by bracket compatibility
  compatible <- vapply(candidates, function(cid) {
    is_bracket_compatible(cid, round_num, used_teams, used_rounds)
  }, logical(1))
  candidates <- candidates[compatible]

  if (length(candidates) < n_picks) return(FALSE)

  # For single picks, try each candidate
  if (n_picks == 1L) {
    for (cid in candidates) {
      new_teams <- c(used_teams, cid)
      new_rounds <- c(used_rounds, round_num)
      if (check_viable_path_actual(new_teams, new_rounds,
                                    remaining_rounds[-1], actual_winners)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  # For double picks (E8), try all pairs
  if (n_picks == 2L && length(candidates) >= 2L) {
    for (i in 1:(length(candidates) - 1L)) {
      for (j in (i + 1L):length(candidates)) {
        c1 <- candidates[i]; c2 <- candidates[j]
        # Check mutual compatibility
        if (!is_bracket_compatible(c2, round_num,
                                    c(used_teams, c1), c(used_rounds, round_num))) next
        new_teams <- c(used_teams, c1, c2)
        new_rounds <- c(used_rounds, round_num, round_num)
        if (check_viable_path_actual(new_teams, new_rounds,
                                      remaining_rounds[-1], actual_winners)) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }

  FALSE
}

#' Check if an entry has ANY viable path (against any possible future outcome)
#'
#' Uses a region-based check: for E8+FF+CHAMP (rounds 4-6), picks must come
#' from 4 different regions. Check that the entry has at least one unused,
#' bracket-compatible team possibility in enough regions.
#'
#' @param used_teams Integer vector of already-picked team_ids
#' @param alive_teams Integer vector of team_ids still in the tournament
#' @param checkpoint Character: "S16" or "E8"
#' @return TRUE if a viable path could exist
check_viable_path_any <- function(used_teams, used_rounds, alive_teams, checkpoint) {
  unused_alive <- setdiff(alive_teams, used_teams)
  if (length(unused_alive) == 0L) return(FALSE)

  # For each remaining slot, check if at least one valid candidate exists
  # We use a simplified region-coverage check

  if (checkpoint == "S16") {
    # Need: 2 S16 picks + 2 E8 picks + 1 FF + 1 CHAMP = 6 picks from unused alive teams
    # E8+FF+CHAMP need 4 different regions
    regions_available <- unique(vapply(unused_alive, get_team_region, integer(1)))

    # Need at least 4 regions for late rounds
    if (length(regions_available) < 4L) return(FALSE)

    # Need unused teams on both FF sides (regions 1+2 = side 1, regions 3+4 = side 2)
    side1 <- any(vapply(unused_alive, get_team_region, integer(1)) <= 2L)
    side2 <- any(vapply(unused_alive, get_team_region, integer(1)) > 2L)
    if (!side1 || !side2) return(FALSE)

    # Also need at least 6 unused alive teams total
    if (length(unused_alive) < 6L) return(FALSE)

    # More detailed check: try to assign bracket-compatible teams to remaining slots
    # This is a heuristic — may have false positives but catches clear dead paths
    return(TRUE)
  }

  if (checkpoint == "E8") {
    # Need: 2 E8 picks + 1 FF + 1 CHAMP = 4 picks
    # All 4 must be from different regions, E8 picks from opposite FF sides
    regions_available <- unique(vapply(unused_alive, get_team_region, integer(1)))
    if (length(regions_available) < 4L) return(FALSE)

    side1_regions <- intersect(regions_available, c(1L, 2L))
    side2_regions <- intersect(regions_available, c(3L, 4L))
    if (length(side1_regions) < 2L || length(side2_regions) < 2L) {
      # Need at least 1 per side for E8, plus 1 each for FF/CHAMP
      # Actually: E8 is 1 from each side, FF from one side, CHAMP from other
      # So need 2 from each side
      return(FALSE)
    }

    if (length(unused_alive) < 4L) return(FALSE)

    # Check bracket compatibility with existing picks
    for (cid in unused_alive) {
      if (!is_bracket_compatible(cid, 4L, used_teams, used_rounds)) {
        unused_alive <- setdiff(unused_alive, cid)
      }
    }
    regions_after_compat <- unique(vapply(unused_alive, get_team_region, integer(1)))
    if (length(regions_after_compat) < 4L) return(FALSE)

    return(TRUE)
  }

  TRUE
}


# ==============================================================================
# DATA LOADING
# ==============================================================================

analyze_year <- function(year, results_file, bracket_file, sim_file,
                          team_names_csv) {
  cat(sprintf("\n{'='*60}\n"))
  cat(sprintf("ANALYZING %d\n", year))
  cat(sprintf("{'='*60}\n"))

  # --- Load bracket ---
  bracket <- read.csv(bracket_file, stringsAsFactors = FALSE)
  stopifnot(nrow(bracket) == 64)
  teams_dt <- data.frame(
    name    = bracket$team,
    seed    = bracket$seed,
    region  = bracket$region,
    team_id = 1:64,
    stringsAsFactors = FALSE
  )

  # --- Load and parse results ---
  results_dt <- readRDS(results_file)
  cat(sprintf("Loaded %d entries from %s\n", nrow(results_dt), basename(results_file)))

  picks_long <- parse_splash_results(results_dt, year = year, format = "A")

  # --- Build name map and add team_ids ---
  name_map <- build_name_map(teams_dt, team_names_csv)
  picks_long[, team_id := map_team_names(
    ifelse(!is.na(team_name), team_name, team_alias), name_map)]

  # Try alias for remaining NAs
  still_na <- is.na(picks_long$team_id) & !is.na(picks_long$team_alias)
  if (any(still_na)) {
    picks_long[still_na, team_id := map_team_names(team_alias, name_map)]
  }

  na_count <- sum(is.na(picks_long$team_id))
  if (na_count > 0) {
    cat(sprintf("WARNING: %d picks with unmapped team names\n", na_count))
    unmapped <- unique(picks_long[is.na(team_id), .(team_name, team_alias)])
    print(unmapped)
  }

  # --- Reconstruct actual results ---
  actual_winners <- reconstruct_results(picks_long)
  cat(sprintf("Reconstructed %d game winners out of 63\n",
              sum(actual_winners > 0)))

  # Sanity check: who won the championship?
  champ_id <- actual_winners[63]
  if (champ_id > 0) {
    champ_name <- teams_dt$name[champ_id]
    cat(sprintf("Champion: %s (team_id %d)\n", champ_name, champ_id))
  }

  # --- Load sim matrix (for win probs and future value) ---
  sim <- readRDS(sim_file)
  sim_matrix <- sim$all_results
  cat(sprintf("Loaded sim matrix: %d sims x %d games\n",
              nrow(sim_matrix), ncol(sim_matrix)))

  # --- Build wide pick history per entry ---
  # Pivot: one row per entry, columns pick_{slot} = team_id
  slot_order <- c("R1_d1", "R1_d2", "R2_d1", "R2_d2",
                  "S16_d1", "S16_d2", "E8", "FF", "CHAMP")

  # For most slots: one pick per entry
  single_slots <- setdiff(slot_order, "E8")
  wide_parts <- list()

  for (sl in single_slots) {
    sl_picks <- picks_long[slot_id == sl & !is.na(team_id),
                            .(entry_id, team_id)]
    if (nrow(sl_picks) == 0) next
    # Deduplicate (shouldn't happen but safety)
    sl_picks <- sl_picks[!duplicated(entry_id)]
    setnames(sl_picks, "team_id", paste0("pick_", sl))
    wide_parts[[sl]] <- sl_picks
  }

  # E8: two picks per entry
  e8_picks <- picks_long[slot_id == "E8" & !is.na(team_id),
                          .(entry_id, team_id)]
  if (nrow(e8_picks) > 0) {
    e8_wide <- e8_picks[, {
      tids <- team_id
      if (length(tids) >= 2) list(pick_E8_1 = tids[1], pick_E8_2 = tids[2])
      else if (length(tids) == 1) list(pick_E8_1 = tids[1], pick_E8_2 = NA_integer_)
      else list(pick_E8_1 = NA_integer_, pick_E8_2 = NA_integer_)
    }, by = entry_id]
    wide_parts[["E8"]] <- e8_wide
  }

  # Merge all parts
  if (length(wide_parts) == 0) {
    stop("No picks found for ", year)
  }
  entry_wide <- wide_parts[[1]]
  for (i in seq_along(wide_parts)[-1]) {
    entry_wide <- merge(entry_wide, wide_parts[[i]], by = "entry_id", all = TRUE)
  }

  cat(sprintf("Built wide pick history: %d entries\n", nrow(entry_wide)))

  # --- Determine S16 game-day assignments from pick data ---
  s16_d1_teams <- picks_long[slot_id == "S16_d1" & !is.na(team_id), unique(team_id)]
  s16_d2_teams <- picks_long[slot_id == "S16_d2" & !is.na(team_id), unique(team_id)]
  s16_d1_games <- unique(vapply(s16_d1_teams, function(t) game_for_team(t, 3L), integer(1)))
  s16_d2_games <- unique(vapply(s16_d2_teams, function(t) game_for_team(t, 3L), integer(1)))
  cat(sprintf("S16 day 1 games: %s\n", paste(sort(s16_d1_games), collapse = ",")))
  cat(sprintf("S16 day 2 games: %s\n", paste(sort(s16_d2_games), collapse = ",")))

  # ==============================================================================
  # Q1: S16 PICK-AGAINST-SELF RATE
  # ==============================================================================
  cat("\n--- Q1: S16 Pick-Against-Self Analysis ---\n")

  # Entries alive at S16 have picks in S16_d1 or S16_d2
  s16_cols <- c("pick_S16_d1", "pick_S16_d2")
  prior_cols <- c("pick_R1_d1", "pick_R1_d2", "pick_R2_d1", "pick_R2_d2")

  # Analyze each S16 pick separately
  q1_rows <- list()

  for (s16_slot in c("S16_d1", "S16_d2")) {
    pick_col <- paste0("pick_", s16_slot)
    if (!pick_col %in% names(entry_wide)) next

    s16_games <- if (s16_slot == "S16_d1") s16_d1_games else s16_d2_games

    entries_with_pick <- entry_wide[!is.na(get(pick_col))]
    if (nrow(entries_with_pick) == 0) next

    for (row_i in seq_len(nrow(entries_with_pick))) {
      entry <- entries_with_pick[row_i]
      my_pick <- entry[[pick_col]]
      my_game <- game_for_team(my_pick, 3L)

      # Prior used teams
      used <- na.omit(unlist(entry[, ..prior_cols]))
      if (length(used) == 0) next

      # Also include other S16 pick as used (for S16_d2, include d1 pick)
      other_s16_col <- if (s16_slot == "S16_d1") "pick_S16_d2" else "pick_S16_d1"
      if (other_s16_col %in% names(entry) && !is.na(entry[[other_s16_col]])) {
        # For S16_d2, the d1 pick is also "used" when considering path
        # But the decision was made for each day independently, so only R1+R2 are prior
      }

      # Find opponent in the game I picked
      opponent <- get_opponent(my_pick, my_game, actual_winners)

      # Did I pick against a used team?
      picked_against_self <- !is.na(opponent) && opponent %in% used

      # Win prob of my pick
      my_wp <- compute_cond_win_prob(my_pick, my_game, sim_matrix)

      # Check all S16 games on this day: could I have picked against a used team?
      could_pick_against <- FALSE
      best_against_wp <- 0
      best_against_tid <- NA_integer_
      best_against_fv <- 0  # future value of the team I'd "use up"

      for (g in s16_games) {
        participants <- get_game_participants(g, actual_winners)
        if (any(is.na(participants))) next

        for (p_idx in 1:2) {
          candidate <- participants[p_idx]
          opp <- participants[3 - p_idx]

          # If opponent is in my used set, picking this candidate = picking against self
          if (opp %in% used) {
            wp <- compute_cond_win_prob(candidate, g, sim_matrix)
            fv <- compute_champ_equity(candidate, sim_matrix)
            if (wp > best_against_wp) {
              could_pick_against <- TRUE
              best_against_wp <- wp
              best_against_tid <- candidate
              best_against_fv <- fv
            }
          }
        }
      }

      # Future value of my actual pick (am I saving it for later? no — I used it)
      # Future value of the pick-against option I skipped (would I be burning a good team?)
      my_fv <- compute_champ_equity(my_pick, sim_matrix)

      q1_rows[[length(q1_rows) + 1]] <- data.table(
        year = year,
        entry_id = entry$entry_id,
        slot_id = s16_slot,
        my_pick = my_pick,
        my_pick_name = teams_dt$name[my_pick],
        my_wp = my_wp,
        my_fv = my_fv,
        opponent = opponent,
        opponent_name = if (!is.na(opponent)) teams_dt$name[opponent] else NA_character_,
        picked_against_self = picked_against_self,
        could_pick_against = could_pick_against,
        best_against_wp = best_against_wp,
        best_against_tid = best_against_tid,
        best_against_name = if (!is.na(best_against_tid)) teams_dt$name[best_against_tid] else NA_character_,
        best_against_fv = best_against_fv
      )
    }
  }

  q1_dt <- rbindlist(q1_rows)

  if (nrow(q1_dt) > 0) {
    cat(sprintf("\nS16 picks analyzed: %d\n", nrow(q1_dt)))
    cat(sprintf("Picked against self: %d (%.1f%%)\n",
                sum(q1_dt$picked_against_self),
                100 * mean(q1_dt$picked_against_self)))
    cat(sprintf("Could have picked against self: %d (%.1f%%)\n",
                sum(q1_dt$could_pick_against),
                100 * mean(q1_dt$could_pick_against)))

    # Conditional rate: when they could have, did they?
    could <- q1_dt[could_pick_against == TRUE]
    if (nrow(could) > 0) {
      cat(sprintf("Rate when available: %.1f%% (%d/%d)\n",
                  100 * mean(could$picked_against_self),
                  sum(could$picked_against_self), nrow(could)))

      # When they skipped: what was the win prob of the option they skipped?
      skipped <- could[picked_against_self == FALSE]
      if (nrow(skipped) > 0) {
        cat(sprintf("\nWhen they SKIPPED pick-against-self (%d cases):\n", nrow(skipped)))
        cat(sprintf("  Skipped option avg win prob: %.1f%%\n",
                    100 * mean(skipped$best_against_wp)))
        cat(sprintf("  Skipped option avg future value: %.4f\n",
                    mean(skipped$best_against_fv)))
        cat(sprintf("  Their actual pick avg win prob: %.1f%%\n",
                    100 * mean(skipped$my_wp)))
        cat(sprintf("  Their actual pick avg future value: %.4f\n",
                    mean(skipped$my_fv)))

        # Breakdown by win prob buckets
        skipped[, wp_bucket := cut(best_against_wp,
                                    breaks = c(0, 0.3, 0.5, 0.7, 1.0),
                                    labels = c("<30%", "30-50%", "50-70%", ">70%"),
                                    include.lowest = TRUE)]
        bucket_summary <- skipped[, .(n = .N, pct = .N / nrow(skipped)),
                                   by = wp_bucket]
        cat("\n  Win prob of skipped pick-against option:\n")
        for (b in seq_len(nrow(bucket_summary))) {
          cat(sprintf("    %s: %d (%.0f%%)\n",
                      bucket_summary$wp_bucket[b],
                      bucket_summary$n[b],
                      100 * bucket_summary$pct[b]))
        }
      }
    }
  }

  # ==============================================================================
  # Q2: UNUSED TEAMS AT E8
  # ==============================================================================
  cat("\n--- Q2: Unused Teams at E8 ---\n")

  e8_participants <- actual_winners[49:56]  # S16 winners = E8 participants
  e8_participants <- e8_participants[e8_participants > 0L]

  prior_to_e8_cols <- c("pick_R1_d1", "pick_R1_d2", "pick_R2_d1", "pick_R2_d2",
                         "pick_S16_d1", "pick_S16_d2")
  prior_to_e8_cols <- intersect(prior_to_e8_cols, names(entry_wide))

  # Entries alive at E8
  e8_entries <- entry_wide[!is.na(pick_E8_1)]
  q2_rows <- list()

  if (nrow(e8_entries) > 0) {
    for (row_i in seq_len(nrow(e8_entries))) {
      entry <- e8_entries[row_i]
      used <- na.omit(unlist(entry[, ..prior_to_e8_cols]))
      n_unused <- sum(!e8_participants %in% used)
      unused_teams <- e8_participants[!e8_participants %in% used]

      q2_rows[[length(q2_rows) + 1]] <- data.table(
        year = year,
        entry_id = entry$entry_id,
        n_used_prior = length(used),
        n_e8_participants = length(e8_participants),
        n_unused_in_e8 = n_unused,
        unused_team_ids = list(unused_teams)
      )
    }

    q2_dt <- rbindlist(q2_rows)
    cat(sprintf("\nEntries alive at E8: %d\n", nrow(q2_dt)))
    cat(sprintf("Unused teams in E8 — mean: %.1f, median: %d\n",
                mean(q2_dt$n_unused_in_e8), as.integer(median(q2_dt$n_unused_in_e8))))
    cat("Distribution:\n")
    dist <- q2_dt[, .N, by = n_unused_in_e8][order(n_unused_in_e8)]
    for (d in seq_len(nrow(dist))) {
      cat(sprintf("  %d unused: %d entries (%.1f%%)\n",
                  dist$n_unused_in_e8[d], dist$N[d],
                  100 * dist$N[d] / nrow(q2_dt)))
    }
  } else {
    q2_dt <- data.table()
    cat("No entries survived to E8\n")
  }

  # ==============================================================================
  # Q3: E8 UNNECESSARY DEPENDENCIES
  # ==============================================================================
  cat("\n--- Q3: E8 Unnecessary Dependencies ---\n")

  q3_rows <- list()

  if (nrow(e8_entries) > 0) {
    for (row_i in seq_len(nrow(e8_entries))) {
      entry <- e8_entries[row_i]
      used <- na.omit(unlist(entry[, ..prior_to_e8_cols]))

      e8_pick_1 <- entry$pick_E8_1
      e8_pick_2 <- if ("pick_E8_2" %in% names(entry)) entry$pick_E8_2 else NA_integer_

      for (pick_num in 1:2) {
        e8_pick <- if (pick_num == 1) e8_pick_1 else e8_pick_2
        if (is.na(e8_pick)) next

        e8_game <- game_for_team(e8_pick, 4L)
        opponent <- get_opponent(e8_pick, e8_game, actual_winners)

        picked_against_used <- !is.na(opponent) && opponent %in% used
        unnecessary_dep <- !is.na(opponent) && !opponent %in% used

        wp <- compute_cond_win_prob(e8_pick, e8_game, sim_matrix)

        q3_rows[[length(q3_rows) + 1]] <- data.table(
          year = year,
          entry_id = entry$entry_id,
          pick_num = pick_num,
          e8_pick = e8_pick,
          e8_pick_name = teams_dt$name[e8_pick],
          e8_game = e8_game,
          opponent = opponent,
          opponent_name = if (!is.na(opponent)) teams_dt$name[opponent] else NA_character_,
          picked_against_used = picked_against_used,
          unnecessary_dep = unnecessary_dep,
          win_prob = wp
        )
      }

      # Track how far this entry survived (proxy for winnings)
      last_round <- 4L  # At minimum survived to E8
      if (!is.na(entry[["pick_FF"]])) last_round <- 5L
      if (!is.na(entry[["pick_CHAMP"]])) last_round <- 6L

      # Tag the dependency rows with survival info
      entry_rows <- which(vapply(q3_rows, function(r) {
        r$entry_id == entry$entry_id && r$year == year
      }, logical(1)))
    }

    q3_dt <- rbindlist(q3_rows)

    # Add survival info per entry
    survival_dt <- e8_entries[, {
      lr <- 4L
      if (!is.na(pick_FF)) lr <- 5L
      if (!is.na(pick_CHAMP)) lr <- 6L
      list(last_round_survived = lr)
    }, by = entry_id]
    # Check if columns exist before referencing
    ff_col <- "pick_FF"
    champ_col <- "pick_CHAMP"
    has_ff <- ff_col %in% names(e8_entries)
    has_champ <- champ_col %in% names(e8_entries)
    survival_dt <- e8_entries[, {
      lr <- 4L
      if (has_ff && !is.na(.SD[[ff_col]])) lr <- 5L
      if (has_champ && !is.na(.SD[[champ_col]])) lr <- 6L
      list(last_round_survived = lr)
    }, by = entry_id, .SDcols = c(ff_col, champ_col)]

    q3_dt <- merge(q3_dt, survival_dt, by = "entry_id", all.x = TRUE)

    # Summarize per entry: did they take any unnecessary deps?
    entry_dep_summary <- q3_dt[, .(
      any_unnecessary = any(unnecessary_dep),
      n_unnecessary = sum(unnecessary_dep)
    ), by = .(year, entry_id)]
    entry_dep_summary <- merge(entry_dep_summary, survival_dt, by = "entry_id")

    cat(sprintf("\nE8 picks analyzed: %d (from %d entries)\n",
                nrow(q3_dt), nrow(e8_entries)))
    cat(sprintf("Picks against used team: %d (%.1f%%)\n",
                sum(q3_dt$picked_against_used),
                100 * mean(q3_dt$picked_against_used)))
    cat(sprintf("Unnecessary dependencies: %d (%.1f%%)\n",
                sum(q3_dt$unnecessary_dep),
                100 * mean(q3_dt$unnecessary_dep)))

    cat(sprintf("\nEntries with 0 unnecessary deps: %d — avg last round: %.2f\n",
                sum(entry_dep_summary$n_unnecessary == 0),
                mean(entry_dep_summary[n_unnecessary == 0, last_round_survived])))
    cat(sprintf("Entries with 1+ unnecessary deps: %d — avg last round: %.2f\n",
                sum(entry_dep_summary$n_unnecessary > 0),
                mean(entry_dep_summary[n_unnecessary > 0, last_round_survived])))
  } else {
    q3_dt <- data.table()
    entry_dep_summary <- data.table()
  }

  # ==============================================================================
  # Q4: STRUCTURAL DEAD PATHS
  # ==============================================================================
  cat("\n--- Q4: Structural Dead Paths ---\n")

  # Define remaining rounds for each checkpoint
  s16_remaining <- list(
    list(slot = "S16_d1", round_num = 3L, game_indices = s16_d1_games, n_picks = 1L),
    list(slot = "S16_d2", round_num = 3L, game_indices = s16_d2_games, n_picks = 1L),
    list(slot = "E8", round_num = 4L, game_indices = 57:60, n_picks = 2L),
    list(slot = "FF", round_num = 5L, game_indices = 61:62, n_picks = 1L),
    list(slot = "CHAMP", round_num = 6L, game_indices = 63L, n_picks = 1L)
  )

  e8_remaining <- list(
    list(slot = "E8", round_num = 4L, game_indices = 57:60, n_picks = 2L),
    list(slot = "FF", round_num = 5L, game_indices = 61:62, n_picks = 1L),
    list(slot = "CHAMP", round_num = 6L, game_indices = 63L, n_picks = 1L)
  )

  # --- Q4a: Dead against actual outcome ---
  cat("\nQ4a: Dead against actual outcome\n")

  # Check at S16 checkpoint (entries alive at S16)
  s16_alive_ids <- unique(picks_long[slot_id %in% c("S16_d1", "S16_d2") & !is.na(team_id),
                                      entry_id])
  r1r2_cols <- c("pick_R1_d1", "pick_R1_d2", "pick_R2_d1", "pick_R2_d2")
  r1r2_cols <- intersect(r1r2_cols, names(entry_wide))

  q4_s16_rows <- list()
  s16_entries <- entry_wide[entry_id %in% s16_alive_ids]

  if (nrow(s16_entries) > 0) {
    cat(sprintf("Checking %d entries at S16 checkpoint...\n", nrow(s16_entries)))
    for (row_i in seq_len(nrow(s16_entries))) {
      entry <- s16_entries[row_i]
      used_teams <- na.omit(as.integer(unlist(entry[, ..r1r2_cols])))
      used_rounds <- rep(c(1L, 1L, 2L, 2L), length.out = length(used_teams))

      # Q4a: viable against actual?
      viable_actual <- check_viable_path_actual(
        used_teams, used_rounds, s16_remaining, actual_winners)

      # Q4b: viable against any outcome?
      s16_participants <- integer(0)
      for (g in 49:56) {
        parts <- get_game_participants(g, actual_winners)
        s16_participants <- c(s16_participants, parts[!is.na(parts)])
      }
      s16_participants <- unique(s16_participants)

      viable_any <- check_viable_path_any(
        used_teams, used_rounds, s16_participants, "S16")

      q4_s16_rows[[length(q4_s16_rows) + 1]] <- data.table(
        year = year,
        entry_id = entry$entry_id,
        checkpoint = "S16",
        n_used = length(used_teams),
        viable_actual = viable_actual,
        viable_any = viable_any
      )
    }
    q4_s16_dt <- rbindlist(q4_s16_rows)

    cat(sprintf("At S16: %d entries alive\n", nrow(q4_s16_dt)))
    cat(sprintf("  Dead against actual outcome: %d (%.1f%%)\n",
                sum(!q4_s16_dt$viable_actual),
                100 * mean(!q4_s16_dt$viable_actual)))
    cat(sprintf("  Dead against ANY outcome: %d (%.1f%%)\n",
                sum(!q4_s16_dt$viable_any),
                100 * mean(!q4_s16_dt$viable_any)))
  } else {
    q4_s16_dt <- data.table()
  }

  # Check at E8 checkpoint
  q4_e8_rows <- list()
  if (nrow(e8_entries) > 0) {
    cat(sprintf("\nChecking %d entries at E8 checkpoint...\n", nrow(e8_entries)))
    for (row_i in seq_len(nrow(e8_entries))) {
      entry <- e8_entries[row_i]
      used_teams <- na.omit(as.integer(unlist(entry[, ..prior_to_e8_cols])))
      used_rounds <- c(rep(1L, 2), rep(2L, 2), rep(3L, 2))[seq_along(used_teams)]

      viable_actual <- check_viable_path_actual(
        used_teams, used_rounds, e8_remaining, actual_winners)

      viable_any <- check_viable_path_any(
        used_teams, used_rounds, e8_participants, "E8")

      q4_e8_rows[[length(q4_e8_rows) + 1]] <- data.table(
        year = year,
        entry_id = entry$entry_id,
        checkpoint = "E8",
        n_used = length(used_teams),
        viable_actual = viable_actual,
        viable_any = viable_any
      )
    }
    q4_e8_dt <- rbindlist(q4_e8_rows)

    cat(sprintf("At E8: %d entries alive\n", nrow(q4_e8_dt)))
    cat(sprintf("  Dead against actual outcome: %d (%.1f%%)\n",
                sum(!q4_e8_dt$viable_actual),
                100 * mean(!q4_e8_dt$viable_actual)))
    cat(sprintf("  Dead against ANY outcome: %d (%.1f%%)\n",
                sum(!q4_e8_dt$viable_any),
                100 * mean(!q4_e8_dt$viable_any)))
  } else {
    q4_e8_dt <- data.table()
  }

  q4_dt <- rbind(q4_s16_dt, q4_e8_dt, fill = TRUE)

  # ==============================================================================
  # PREPARE CALIBRATION DATA (while sim_matrix is still in memory)
  # ==============================================================================
  cat("\n--- Preparing calibration data ---\n")

  n_sims <- nrow(sim_matrix)

  # S16 participants = winners of R32 games
  s16_participants <- actual_winners[33:48]
  s16_participants <- s16_participants[s16_participants > 0L]

  # Win probs and champ equity for S16 teams
  wp_s16 <- setNames(numeric(length(s16_participants)), as.character(s16_participants))
  ce_s16 <- setNames(numeric(length(s16_participants)), as.character(s16_participants))
  for (tid in s16_participants) {
    g <- game_for_team(tid, 3L)
    feeder_g <- game_for_team(tid, 2L)
    reached <- sum(sim_matrix[, feeder_g] == tid)
    won <- sum(sim_matrix[, g] == tid)
    wp_s16[as.character(tid)] <- if (reached > 0) won / reached else 0.5
    ce_s16[as.character(tid)] <- sum(sim_matrix[, 63] == tid) / n_sims
  }

  # E8 participants = S16 winners
  e8_participants_cal <- actual_winners[49:56]
  e8_participants_cal <- e8_participants_cal[e8_participants_cal > 0L]

  wp_e8 <- setNames(numeric(length(e8_participants_cal)), as.character(e8_participants_cal))
  ce_e8 <- setNames(numeric(length(e8_participants_cal)), as.character(e8_participants_cal))
  for (tid in e8_participants_cal) {
    g <- game_for_team(tid, 4L)
    feeder_g <- game_for_team(tid, 3L)
    reached <- sum(sim_matrix[, feeder_g] == tid)
    won <- sum(sim_matrix[, g] == tid)
    wp_e8[as.character(tid)] <- if (reached > 0) won / reached else 0.5
    ce_e8[as.character(tid)] <- sum(sim_matrix[, 63] == tid) / n_sims
  }

  # Build S16 calibration rows
  s16_calib_rows <- list()
  s16_prior_cols <- intersect(c("pick_R1_d1", "pick_R1_d2", "pick_R2_d1", "pick_R2_d2"),
                               names(entry_wide))

  for (s16_slot in c("S16_d1", "S16_d2")) {
    pick_col <- paste0("pick_", s16_slot)
    if (!pick_col %in% names(entry_wide)) next
    day_teams <- if (s16_slot == "S16_d1") s16_d1_teams else s16_d2_teams
    day_teams <- day_teams[day_teams > 0]
    entries <- entry_wide[!is.na(get(pick_col))]
    if (nrow(entries) == 0) next
    for (ri in seq_len(nrow(entries))) {
      entry <- entries[ri]
      used <- na.omit(as.integer(unlist(entry[, ..s16_prior_cols])))
      used_in_s16 <- intersect(used, s16_participants)
      cp <- length(used_in_s16) / length(s16_participants)

      # Remaining FV: sum of champ equity for all unused S16+ teams
      # (S16 participants that haven't been used yet)
      unused_s16 <- setdiff(s16_participants, used)
      remaining_fv <- sum(ce_s16[as.character(unused_s16)], na.rm = TRUE)

      # FV fraction per candidate: what share of remaining FV is this team?
      day_ces <- ce_s16[as.character(day_teams)]
      fv_fracs <- if (remaining_fv > 0) day_ces / remaining_fv else rep(0, length(day_ces))

      s16_calib_rows[[length(s16_calib_rows) + 1]] <- data.table(
        year = year, entry_id = entry$entry_id, slot_id = s16_slot,
        actual_pick = entry[[pick_col]],
        candidates = list(as.integer(day_teams)),
        used_teams = list(as.integer(used)),
        constraint_pressure = cp,
        win_probs = list(wp_s16[as.character(day_teams)]),
        champ_equities = list(ce_s16[as.character(day_teams)]),
        fv_fractions = list(fv_fracs),
        remaining_fv = remaining_fv
      )
    }
  }
  s16_calib <- if (length(s16_calib_rows) > 0) rbindlist(s16_calib_rows) else data.table()

  # Build E8 calibration rows
  e8_calib_rows <- list()
  e8_prior_cols <- intersect(c("pick_R1_d1", "pick_R1_d2", "pick_R2_d1", "pick_R2_d2",
                                 "pick_S16_d1", "pick_S16_d2"), names(entry_wide))
  e8_cal_entries <- entry_wide[!is.na(entry_wide[["pick_E8_1"]])]
  if (nrow(e8_cal_entries) > 0) {
    for (ri in seq_len(nrow(e8_cal_entries))) {
      entry <- e8_cal_entries[ri]
      used <- na.omit(as.integer(unlist(entry[, ..e8_prior_cols])))
      used_in_e8 <- intersect(used, e8_participants_cal)
      cp <- length(used_in_e8) / length(e8_participants_cal)

      # Remaining FV: sum of champ equity for unused E8+ teams
      unused_e8 <- setdiff(e8_participants_cal, used)
      remaining_fv <- sum(ce_e8[as.character(unused_e8)], na.rm = TRUE)

      e8_ces <- ce_e8[as.character(e8_participants_cal)]
      fv_fracs <- if (remaining_fv > 0) e8_ces / remaining_fv else rep(0, length(e8_ces))

      e8_calib_rows[[length(e8_calib_rows) + 1]] <- data.table(
        year = year, entry_id = entry$entry_id,
        pick_1 = entry$pick_E8_1,
        pick_2 = if ("pick_E8_2" %in% names(entry)) entry$pick_E8_2 else NA_integer_,
        candidates = list(as.integer(e8_participants_cal)),
        used_teams = list(as.integer(used)),
        constraint_pressure = cp,
        win_probs = list(wp_e8[as.character(e8_participants_cal)]),
        champ_equities = list(ce_e8[as.character(e8_participants_cal)]),
        fv_fractions = list(fv_fracs),
        remaining_fv = remaining_fv
      )
    }
  }
  e8_calib <- if (length(e8_calib_rows) > 0) rbindlist(e8_calib_rows) else data.table()

  cat(sprintf("Calibration data: %d S16 rows, %d E8 rows\n",
              nrow(s16_calib), nrow(e8_calib)))

  # Free sim matrix to avoid memory pressure between years
  rm(sim_matrix, sim); gc(verbose = FALSE)

  # ==============================================================================
  # RETURN STRUCTURED RESULTS
  # ==============================================================================
  list(
    year = year,
    teams_dt = teams_dt,
    actual_winners = actual_winners,
    entry_wide = entry_wide,
    q1_s16_pick_against = q1_dt,
    q2_unused_at_e8 = q2_dt,
    q3_e8_dependencies = q3_dt,
    q3_entry_summary = if (exists("entry_dep_summary")) entry_dep_summary else data.table(),
    q4_dead_paths = q4_dt,
    s16_calib = s16_calib,
    e8_calib = e8_calib
  )
}


# ==============================================================================
# RUN ANALYSIS (only when executed directly, not when sourced for functions)
# ==============================================================================

if (!exists(".ANALYZE_SKIP_AUTO_RUN") || !.ANALYZE_SKIP_AUTO_RUN) {

base_dir <- file.path(script_dir, "..")
team_names_csv <- file.path(base_dir, "team_names.csv")

results_2024 <- analyze_year(
  year = 2024L,
  results_file = file.path(base_dir, "results_2024.rds"),
  bracket_file = file.path(base_dir, "brackets", "bracket_2024.csv"),
  sim_file = file.path(base_dir, "sim_results_2024.rds"),
  team_names_csv = team_names_csv
)

results_2025 <- analyze_year(
  year = 2025L,
  results_file = file.path(base_dir, "results_2025.rds"),
  bracket_file = file.path(base_dir, "brackets", "bracket_2025.csv"),
  sim_file = file.path(base_dir, "sim_results_2025.rds"),
  team_names_csv = team_names_csv
)

# ==============================================================================
# COMBINED SUMMARY
# ==============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("COMBINED SUMMARY (2024 + 2025)\n")
cat(strrep("=", 60), "\n")

# Q1 combined
q1_all <- rbind(results_2024$q1_s16_pick_against,
                results_2025$q1_s16_pick_against, fill = TRUE)
if (nrow(q1_all) > 0) {
  cat(sprintf("\nQ1: S16 Pick-Against-Self\n"))
  cat(sprintf("  Total S16 picks: %d\n", nrow(q1_all)))
  cat(sprintf("  Picked against self: %.1f%%\n",
              100 * mean(q1_all$picked_against_self)))
  cat(sprintf("  Could have picked against self: %.1f%%\n",
              100 * mean(q1_all$could_pick_against)))
  could <- q1_all[could_pick_against == TRUE]
  if (nrow(could) > 0) {
    cat(sprintf("  Rate when available: %.1f%%\n",
                100 * mean(could$picked_against_self)))
  }
}

# Q2 combined
q2_all <- rbind(results_2024$q2_unused_at_e8,
                results_2025$q2_unused_at_e8, fill = TRUE)
if (nrow(q2_all) > 0) {
  cat(sprintf("\nQ2: Unused Teams at E8\n"))
  cat(sprintf("  Entries at E8: %d\n", nrow(q2_all)))
  cat(sprintf("  Avg unused: %.1f, Median: %d\n",
              mean(q2_all$n_unused_in_e8), as.integer(median(q2_all$n_unused_in_e8))))
}

# Q3 combined
q3_all <- rbind(results_2024$q3_e8_dependencies,
                results_2025$q3_e8_dependencies, fill = TRUE)
if (nrow(q3_all) > 0) {
  cat(sprintf("\nQ3: E8 Unnecessary Dependencies\n"))
  cat(sprintf("  E8 picks: %d\n", nrow(q3_all)))
  cat(sprintf("  Unnecessary deps: %.1f%%\n",
              100 * mean(q3_all$unnecessary_dep)))
}

# Q4 combined
q4_all <- rbind(results_2024$q4_dead_paths,
                results_2025$q4_dead_paths, fill = TRUE)
if (nrow(q4_all) > 0) {
  for (cp in c("S16", "E8")) {
    sub <- q4_all[checkpoint == cp]
    if (nrow(sub) > 0) {
      cat(sprintf("\nQ4: Dead Paths at %s\n", cp))
      cat(sprintf("  Entries: %d\n", nrow(sub)))
      cat(sprintf("  Dead (actual): %.1f%%\n", 100 * mean(!sub$viable_actual)))
      cat(sprintf("  Dead (any):    %.1f%%\n", 100 * mean(!sub$viable_any)))
    }
  }
}

# Return all results for downstream use
historical_analysis <- list(
  results_2024 = results_2024,
  results_2025 = results_2025,
  q1_combined = q1_all,
  q2_combined = q2_all,
  q3_combined = q3_all,
  q4_combined = q4_all
)

cat("\n\nAnalysis complete. Results stored in 'historical_analysis' list.\n")

} # end if (!.ANALYZE_SKIP_AUTO_RUN)
