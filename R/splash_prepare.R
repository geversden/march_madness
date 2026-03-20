#!/usr/bin/env Rscript
# ==============================================================================
# splash_prepare.R
# Bridge between scrape_splash_contests.R output and the splash optimizer.
#
# Handles:
#   - Re-simulating with locked results (no filtering, always fresh sims)
#   - Building portfolio state from scrape data
#   - Team name mapping (splash -> sim)
#   - Computing field availability per contest
#   - Orchestrating all inputs for the optimizer
#
# Dependencies:
#   source("R/splash_config.R")
#   source("R/splash_state.R")
#   Rcpp::sourceCpp("simulate_tourney.cpp")
#
# Usage:
#   inputs <- prepare_optimizer_inputs(scrape_results, sim, "R1_d2", ...)
# ==============================================================================

library(data.table)

# ==============================================================================
# TEAM NAME MAPPING
# ==============================================================================

#' Build a name map between splash team names and sim team names
#'
#' @param teams_dt Data frame with columns: team_id, name (sim names)
#' @param team_names_csv Path to team_names.csv
#' @return data.table with columns: splash_name, sim_name, team_id
build_name_map <- function(teams_dt, team_names_csv = "team_names.csv") {
  if (!file.exists(team_names_csv)) {
    stop("team_names.csv not found at: ", team_names_csv)
  }

  name_xwalk <- fread(team_names_csv)

  # Build mapping: splash_name -> bracket_name -> team_id
  # If splash_name column exists, use it; otherwise try to match directly
  if ("splash_name" %in% names(name_xwalk)) {
    map_dt <- name_xwalk[!is.na(splash_name), .(bracket_name, splash_name)]
  } else {
    # No splash_name column yet — use bracket_name as-is
    map_dt <- data.table(
      bracket_name = name_xwalk$bracket_name,
      splash_name  = name_xwalk$bracket_name
    )
  }

  # Join to sim teams
  teams <- as.data.table(teams_dt)
  result <- merge(map_dt, teams[, .(team_id, sim_name = name)],
                  by.x = "bracket_name", by.y = "sim_name", all.x = TRUE)

  # Also add direct matches for teams not in the crosswalk
  direct <- teams[!name %in% result$sim_name,
                  .(splash_name = name, bracket_name = name,
                    sim_name = name, team_id)]
  if (nrow(direct) > 0) {
    result[, sim_name := bracket_name]
    result <- rbind(result, direct, fill = TRUE)
  } else {
    result[, sim_name := bracket_name]
  }

  # Log unmatched
  unmatched <- result[is.na(team_id)]
  if (nrow(unmatched) > 0) {
    cat(sprintf("WARNING: %d splash names unmatched to sim teams:\n", nrow(unmatched)))
    cat(paste("  ", unmatched$splash_name, collapse = "\n"), "\n")
  }

  result[!is.na(team_id), .(splash_name, sim_name, team_id)]
}

#' Map a vector of splash team names to team_ids
#'
#' @param team_names Character vector of splash team names
#' @param name_map data.table from build_name_map()
#' @return Integer vector of team_ids (NA for unmatched)
map_team_names <- function(team_names, name_map) {
  # Try splash_name first, then sim_name for unmatched
  ids <- name_map$team_id[match(team_names, name_map$splash_name)]
  missing <- is.na(ids)
  if (any(missing)) {
    ids[missing] <- name_map$team_id[match(team_names[missing], name_map$sim_name)]
  }
  ids
}

# ==============================================================================
# LOCKED RESULT SIMULATION
# ==============================================================================

#' Build locked_winners vector from completed slot results
#'
#' @param completed_slots Named list: slot_id -> character vector of winning team names
#' @param teams_dt Data frame with team_id, name columns
#' @param name_map data.table from build_name_map()
#' @return Integer vector of length 63 (locked_results for C++).
#'   >0 = locked winner (1-indexed team_id), 0 = simulate normally
build_locked_results <- function(completed_slots, teams_dt, name_map) {
  locked <- integer(63)  # all zeros = simulate everything

  for (slot_id in names(completed_slots)) {
    slot <- get_slot(slot_id)
    game_indices <- slot$game_indices
    winners <- completed_slots[[slot_id]]

    # Map winner names to team_ids
    winner_ids <- map_team_names(winners, name_map)

    if (any(is.na(winner_ids))) {
      bad <- winners[is.na(winner_ids)]
      stop(sprintf("Cannot map winners in slot %s: %s", slot_id,
                   paste(bad, collapse = ", ")))
    }

    # For R64 slots, we know the exact game index for each winner
    # For later rounds, we need to figure out which game each winner played in
    for (g in game_indices) {
      # Find which winner played in this game
      # Get the teams that could appear in game g
      # For R64 (games 1-32): bracket positions 2g-1 and 2g
      # For later rounds: the winner comes from feeder games
      #
      # Simple approach: check which winner_id appears in this game's
      # sim results. Since results are locked sequentially, earlier
      # rounds are already locked.
      #
      # For R64: team_a = bracket_order[2*g-1], team_b = bracket_order[2*g]
      if (g <= 32) {
        team_a <- teams_dt$team_id[2 * g - 1]
        team_b <- teams_dt$team_id[2 * g]
        winner_in_game <- intersect(winner_ids, c(team_a, team_b))
        if (length(winner_in_game) == 1) {
          locked[g] <- winner_in_game
        }
      } else {
        # Later rounds: lock if any winner_id matches
        # The C++ code handles cascading — we just need the right team_id
        # For R32+ games, the participants are determined by prior results.
        # We lock the winner for this game index directly.
        # Match winners to games by checking which feeder bracket positions
        # could produce each winner.
        for (wid in winner_ids) {
          if (locked[g] > 0) next  # already locked

          # Check if this winner could play in this game
          # by tracing back to R64 feeder games
          if (g <= 48) {
            feeder_r64 <- (g - 33) * 2 + 1:2
          } else if (g <= 56) {
            feeder_r64 <- (g - 49) * 4 + 1:4
          } else if (g <= 60) {
            feeder_r64 <- (g - 57) * 8 + 1:8
          } else if (g <= 62) {
            feeder_r64 <- (g - 61) * 16 + 1:16
          } else {
            feeder_r64 <- 1:32
          }

          # Get all bracket positions that feed into this game
          feeder_positions <- sort(c(2 * feeder_r64 - 1, 2 * feeder_r64))
          feeder_team_ids <- teams_dt$team_id[feeder_positions]

          if (wid %in% feeder_team_ids) {
            locked[g] <- wid
          }
        }
      }
    }
  }

  locked
}

#' Re-simulate tournament with locked results
#'
#' Always re-simulates (never filters). Guarantees fresh, statistically
#' significant sample sizes even after major upsets.
#'
#' @param sim Existing sim object (from readRDS or run_tournament_sims)
#' @param completed_slots Named list: slot_id -> character vector of winning team names
#' @param name_map data.table from build_name_map()
#' @param n_sims Number of sims (default 100000)
#' @param seed Optional RNG seed
#' @return New sim object in standard format
resimulate_with_locks <- function(sim, completed_slots, name_map,
                                  n_sims = 2000000L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  teams_dt <- sim$teams
  locked <- build_locked_results(completed_slots, teams_dt, name_map)

  n_locked <- sum(locked > 0)
  cat(sprintf("Re-simulating with %d locked games, %d sims...\n", n_locked, n_sims))

  # Get R1 win probs if they exist
  r1_probs <- if (!is.null(sim$r1_win_probs)) sim$r1_win_probs else numeric(0)

  result <- run_tournament_sims_locked(
    ratings        = teams_dt$rating,
    bracket_order  = as.integer(sim$bracket_order),
    n_sims         = as.integer(n_sims),
    update_factor  = sim$update_factor,
    r1_win_probs   = r1_probs,
    locked_results = as.integer(locked)
  )

  # Build sim object in standard format
  list(
    all_results     = result$all_results,
    teams           = teams_dt,
    round_info      = sim$round_info,
    bracket_order   = sim$bracket_order,
    n_sims          = n_sims,
    update_factor   = sim$update_factor,
    year            = sim$year,
    locked_results  = locked,
    r1_win_probs    = r1_probs
  )
}

# ==============================================================================
# PORTFOLIO FROM SCRAPE
# ==============================================================================

#' Build portfolio state from scrape results
#'
#' @param scrape_results Output from scrape_all_splash()
#' @param teams_dt Data frame with team_id, name columns
#' @param name_map data.table from build_name_map()
#' @param our_username Our Splash username
#' @return data.table of entry state (same format as init_portfolio output)
init_portfolio_from_scrape <- function(scrape_results, teams_dt,
                                       name_map, our_username = "TinkyTyler") {
  # Get contest metadata from splash_contests (defined in scrape_splash_contests.R)
  sc <- as.data.table(splash_contests)

  all_entries <- list()

  for (contest_name in names(scrape_results$per_contest)) {
    contest <- scrape_results$per_contest[[contest_name]]
    cid <- contest$contest_id

    # Get our entries
    paths <- contest$entry_paths
    our_paths <- paths[paths$entryName == our_username, ]
    if (nrow(our_paths) == 0) next

    # Get contest metadata from splash_contests
    meta <- sc[contest_id == cid]
    if (nrow(meta) == 0) {
      cat(sprintf("WARNING: No metadata for contest %s (%s), skipping\n",
                  contest_name, cid))
      next
    }

    # Map format string to format code (A/B/C)
    fmt <- classify_format(meta$format[1])
    day_map <- DAY_SLOT_MAP[[fmt]]
    contest_size <- nrow(contest$entry_paths)
    entry_fee <- meta$fee[1]

    # Build entry rows
    for (i in seq_len(nrow(our_paths))) {
      row <- our_paths[i, ]
      entry <- data.table(
        entry_id     = row$entryId,
        contest_id   = cid,
        contest_name = contest_name,
        contest_size = contest_size,
        entry_fee    = entry_fee,
        prize_pool   = NA_real_,
        format       = fmt,
        alive        = isTRUE(row$alive)
      )

      # Populate pick columns from day1, day2, etc.
      for (day_key in names(day_map)) {
        slot_id <- day_map[day_key]
        col <- slot_col_name(slot_id)
        day_col <- day_key  # e.g., "day1"

        if (day_col %in% names(row) && !is.na(row[[day_col]])) {
          team_str <- as.character(row[[day_col]])
          # Split multi-pick days ("+"-separated)
          team_names <- strsplit(team_str, "\\+")[[1]]
          team_ids <- map_team_names(trimws(team_names), name_map)

          if (length(team_ids) == 1 && !is.na(team_ids[1])) {
            entry[, (col) := team_ids[1]]
          } else if (length(team_ids) > 1) {
            # Multi-pick slot (e.g., E8 combined or Format C R1)
            # Store first pick in the slot column
            # For E8 combined (2 picks), we need both
            valid_ids <- team_ids[!is.na(team_ids)]
            if (length(valid_ids) >= 1) entry[, (col) := valid_ids[1]]
            # TODO: Handle second pick for E8 combined / Format C
          }
        }
      }

      all_entries[[length(all_entries) + 1]] <- entry
    }
  }

  if (length(all_entries) == 0) {
    stop("No entries found for username: ", our_username)
  }

  entries_dt <- rbindlist(all_entries, fill = TRUE)
  init_portfolio_from_entries(entries_dt)
}

# ==============================================================================
# ALIVE STATUS INFERENCE
# ==============================================================================

#' Infer alive status for all entries based on completed results
#'
#' For each completed slot, checks if each entry's pick for that day won.
#' An entry is dead if ANY of its picks lost.
#'
#' @param scrape_results Output from scrape_all_splash()
#' @param completed_slots Named list: slot_id -> character vector of winning team names
#' @param name_map data.table from build_name_map()
#' @return scrape_results with `alive` column added to all entry_paths
infer_alive_status <- function(scrape_results, completed_slots, name_map) {
  if (length(completed_slots) == 0) {
    # No games played yet — everyone is alive
    for (cn in names(scrape_results$per_contest)) {
      scrape_results$per_contest[[cn]]$entry_paths$alive <- TRUE
    }
    scrape_results$entry_paths$alive <- TRUE
    return(scrape_results)
  }

  # Build day-to-winners mapping using team_ids to avoid name mismatches
  # (e.g., "Hawai'i" vs "Hawaii", "McNeese" vs "McNeese State")
  slot_to_day <- c(R1_d1="day1", R1_d2="day2", R2_d1="day3", R2_d2="day4",
                   S16_d1="day5", S16_d2="day6", E8="day7", E8_d1="day7",
                   E8_d2="day8", FF="day8", CHAMP="day9")

  # Build set of winning team_ids per completed day column
  day_winner_ids <- list()
  for (slot_id in names(completed_slots)) {
    day_col <- slot_to_day[[slot_id]]
    if (is.null(day_col)) next
    winners <- completed_slots[[slot_id]]
    winner_ids <- map_team_names(winners, name_map)
    if (any(is.na(winner_ids))) {
      bad <- winners[is.na(winner_ids)]
      warning(sprintf("Could not map winners in slot %s: %s",
                      slot_id, paste(bad, collapse = ", ")))
    }
    day_winner_ids[[day_col]] <- winner_ids[!is.na(winner_ids)]
  }

  cat(sprintf("Inferring alive status from %d completed slots...\n",
              length(completed_slots)))

  for (cn in names(scrape_results$per_contest)) {
    paths <- scrape_results$per_contest[[cn]]$entry_paths
    alive <- rep(TRUE, nrow(paths))

    for (day_col in names(day_winner_ids)) {
      if (!day_col %in% names(paths)) next
      winner_ids <- day_winner_ids[[day_col]]

      for (i in seq_len(nrow(paths))) {
        if (!alive[i]) next  # already dead
        pick <- paths[[day_col]][i]
        if (is.na(pick) || !nzchar(pick)) next  # no pick yet

        # Split multi-pick days and map to team_ids
        picks <- trimws(strsplit(as.character(pick), "\\+")[[1]])
        pick_ids <- map_team_names(picks, name_map)
        # ALL picks must be winners to survive
        for (pid in pick_ids) {
          if (is.na(pid) || !pid %in% winner_ids) {
            alive[i] <- FALSE
            break
          }
        }
      }
    }

    scrape_results$per_contest[[cn]]$entry_paths$alive <- alive
    n_alive <- sum(alive)
    n_total <- nrow(paths)
    cat(sprintf("  [%s] %d / %d alive (%.1f%%)\n",
                cn, n_alive, n_total, 100 * n_alive / n_total))
  }

  # Update combined entry_paths too
  if (!is.null(scrape_results$entry_paths)) {
    all_paths <- list()
    for (cn in names(scrape_results$per_contest)) {
      contest <- scrape_results$per_contest[[cn]]
      all_paths[[cn]] <- contest$entry_paths
    }
    scrape_results$entry_paths <- do.call(rbind, all_paths)
  }

  scrape_results
}

# ==============================================================================
# FIELD AVAILABILITY
# ==============================================================================

#' Compute field availability per contest from scrape data
#'
#' @param scrape_results Output from scrape_all_splash()
#' @param name_map data.table from build_name_map()
#' @param our_username Our Splash username
#' @return Named list: contest_id -> list(alive_count, used_teams_by_entry)
compute_field_availability <- function(scrape_results, name_map,
                                        our_username = "TinkyTyler") {
  result <- list()

  for (contest_name in names(scrape_results$per_contest)) {
    contest <- scrape_results$per_contest[[contest_name]]
    cid <- contest$contest_id
    paths <- contest$entry_paths

    # Field entries = alive, not ours
    if ("alive" %in% names(paths)) {
      field <- paths[paths$entryName != our_username & paths$alive == TRUE, ]
    } else {
      field <- paths[paths$entryName != our_username, ]
    }

    alive_count <- nrow(field)

    # Extract used teams per field entry
    day_cols <- grep("^day\\d+$", names(field), value = TRUE)
    used_by_entry <- lapply(seq_len(nrow(field)), function(i) {
      teams <- character(0)
      for (dc in day_cols) {
        val <- field[[dc]][i]
        if (!is.na(val) && nzchar(val)) {
          teams <- c(teams, strsplit(as.character(val), "\\+")[[1]])
        }
      }
      team_ids <- map_team_names(trimws(teams), name_map)
      team_ids[!is.na(team_ids)]
    })

    result[[cid]] <- list(
      alive_count       = alive_count,
      used_teams_by_entry = used_by_entry,
      contest_name      = contest_name
    )
  }

  result
}

# ==============================================================================
# ORCHESTRATOR
# ==============================================================================

#' Prepare all inputs for the splash optimizer
#'
#' @param scrape_results Output from scrape_all_splash()
#' @param sim Sim object (from readRDS)
#' @param current_slot_id Character slot ID for today
#' @param completed_slots Named list: slot_id -> winning team names (for locking)
#' @param our_username Our Splash username
#' @param team_names_csv Path to team_names.csv
#' @param n_sims Number of sims for re-simulation
#' @param seed Optional RNG seed
#' @return List with all optimizer inputs
prepare_optimizer_inputs <- function(scrape_results, sim, current_slot_id,
                                     completed_slots = list(),
                                     our_username = "TinkyTyler",
                                     team_names_csv = "team_names.csv",
                                     n_sims = 2000000L,
                                     seed = NULL) {

  cat("=== Preparing optimizer inputs ===\n")

  # 1. Build name map
  cat("Building name map...\n")
  name_map <- build_name_map(sim$teams, team_names_csv)
  cat(sprintf("  %d name mappings\n", nrow(name_map)))

  # 2. Infer alive status from completed results
  scrape_results <- infer_alive_status(scrape_results, completed_slots, name_map)

  # 3. Re-simulate with locked results (if any completed slots)
  if (length(completed_slots) > 0) {
    cat("Re-simulating with locked results...\n")
    sim <- resimulate_with_locks(sim, completed_slots, name_map,
                                 n_sims = n_sims, seed = seed)
  } else {
    cat("No completed slots — using original sim\n")
  }

  # 4. Build portfolio state from scrape
  cat("Building portfolio from scrape...\n")
  portfolio <- init_portfolio_from_scrape(scrape_results, sim$teams,
                                          name_map, our_username)

  # 5. Compute field availability
  cat("Computing field availability...\n")
  field_avail <- compute_field_availability(scrape_results, name_map, our_username)

  total_field_alive <- sum(sapply(field_avail, function(x) x$alive_count))
  cat(sprintf("  %d contests, %d total field entries alive\n",
              length(field_avail), total_field_alive))

  # 6. Determine remaining slots
  # Get format from first alive entry
  fmt <- portfolio[alive == TRUE, format[1]]
  if (is.na(fmt)) fmt <- "A"
  slot_order <- get_slot_order(fmt)
  current_idx <- match(current_slot_id, slot_order)
  if (is.na(current_idx)) stop("current_slot_id not found in format ", fmt)
  remaining_slots <- slot_order[current_idx:length(slot_order)]

  cat(sprintf("  Current slot: %s (%d of %d remaining)\n",
              current_slot_id, length(remaining_slots), length(slot_order)))

  cat("=== Preparation complete ===\n")

  list(
    sim             = sim,
    portfolio       = portfolio,
    field_avail     = field_avail,
    name_map        = name_map,
    current_slot_id = current_slot_id,
    remaining_slots = remaining_slots,
    format          = fmt
  )
}
