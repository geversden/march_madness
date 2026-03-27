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
  # Fallback: normalize parenthetical disambiguators
  # "Miami (FL)" -> "Miami", "Miami (OH)" -> "Miami OH"
  still_missing <- is.na(ids)
  if (any(still_missing)) {
    normalized <- sapply(team_names[still_missing], function(tn) {
      m <- regmatches(tn, regexec("^(.+?)\\s*\\(([^)]+)\\)\\s*$", tn))[[1]]
      if (length(m) == 3) {
        base <- m[2]; tag <- m[3]
        # State abbreviations that indicate a distinct school (OH, FL, etc.)
        # FL = same as base name, OH/TX/etc. = append
        if (toupper(tag) == "FL") return(base)
        return(paste(base, tag))
      }
      tn
    }, USE.NAMES = FALSE)
    ids[still_missing] <- name_map$team_id[match(normalized, name_map$splash_name)]
    still2 <- is.na(ids[still_missing])
    if (any(still2)) {
      idx <- which(still_missing)[still2]
      ids[idx] <- name_map$team_id[match(normalized[still2], name_map$sim_name)]
    }
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
# DAY COLUMN ORDER DETECTION
# ==============================================================================

#' Detect the correct mapping from scrape day columns to slot IDs
#'
#' Splash scrape columns (day1, day2, day3, ...) don't always map to
#' (R1_d1, R1_d2, R2_d1, ...) in order. Different contests can have columns
#' in different orders. This function scores each day column against each
#' completed slot's winners and finds the best permutation.
#'
# Simple permutation generator (no external deps)
# Returns list of integer vectors, each a permutation of 1:n
combinat_perms <- function(n) {
  if (n == 1) return(list(1L))
  if (n == 2) return(list(c(1L, 2L), c(2L, 1L)))
  result <- list()
  sub <- combinat_perms(n - 1L)
  for (p in sub) {
    for (pos in seq_len(n)) {
      new_perm <- append(p, n, after = pos - 1L)
      result[[length(result) + 1L]] <- as.integer(new_perm)
    }
  }
  result
}

#' @param paths data.frame with day1, day2, ... columns containing team names
#' @param completed_slots Named list: slot_id -> character vector of winning team names
#' @param name_map data.table from build_name_map()
#' @param default_day_map Named character vector: day1="R1_d1", day2="R1_d2", ...
#' @return Corrected day_map (same format as input, possibly reordered)
detect_day_column_order <- function(paths, completed_slots, name_map, default_day_map) {
  if (length(completed_slots) < 2) return(default_day_map)

  # Reverse mapping: slot_id -> day_col (from default)
  slot_to_day <- setNames(names(default_day_map), default_day_map)

  # Which completed slots appear in our day_map?
  completed_in_map <- intersect(unname(default_day_map), names(completed_slots))
  if (length(completed_in_map) < 2) return(default_day_map)

  # Which day columns correspond to these completed slots (by default mapping)?
  relevant_days <- unique(slot_to_day[completed_in_map])
  relevant_days <- relevant_days[relevant_days %in% names(paths)]
  if (length(relevant_days) < 2) return(default_day_map)

  # Gather unique team names per day column
  day_teams <- list()
  for (dc in relevant_days) {
    teams <- unique(trimws(unlist(strsplit(as.character(paths[[dc]]), "\\+"))))
    day_teams[[dc]] <- teams[!is.na(teams) & nzchar(teams)]
  }

  # Gather winner names per completed slot
  slot_winners <- list()
  for (sid in completed_in_map) {
    slot_winners[[sid]] <- completed_slots[[sid]]
  }

  # Determine round order of each slot (R1=1, R2=2, S16=3, etc.)
  slot_round <- function(sid) {
    if (grepl("^R1", sid)) return(1L)
    if (grepl("^R2", sid)) return(2L)
    if (grepl("^S16", sid)) return(3L)
    if (grepl("^E8", sid)) return(4L)
    if (grepl("^FF", sid)) return(5L)
    if (grepl("^CHAMP", sid)) return(6L)
    return(1L)
  }

  # Build set of ALL winners from each round (teams that survived prior rounds)
  # A later-round day column can only contain teams that won ALL prior rounds.
  # An earlier-round day column will contain teams that LOST — impossible for later slots.
  all_prior_winners <- list()
  for (sid in completed_in_map) {
    all_prior_winners[[sid]] <- completed_slots[[sid]]
  }

  # Score matrix: day_col (rows) x slot_id (cols)
  # Score = match_count - heavy penalty for impossible teams
  # "Impossible" = day column contains a team that is NOT a valid participant for that slot
  # For R2 slots: valid participants are R1 winners. If day col has a team that lost R1, it can't be R2.
  n_days <- length(relevant_days)
  n_slots <- length(completed_in_map)
  scores <- matrix(0, n_days, n_slots,
                   dimnames = list(relevant_days, completed_in_map))

  # Collect all winners from earlier rounds, keyed by slot
  # For a slot in round R, valid picks are teams that won all rounds < R
  # We approximate: for R2 slots, valid picks must be in R1 winners
  round_winners <- list()
  for (sid in completed_in_map) {
    round_winners[[sid]] <- completed_slots[[sid]]
  }

  # Build cumulative winner sets: teams that survived through round N
  # R1 winners = all R1 slot winners combined
  # For R2: valid participants = all R1 winners (from both d1 and d2)
  r1_winners <- character(0)
  r2_winners <- character(0)
  s16_winners <- character(0)
  for (sid in names(completed_slots)) {
    if (grepl("^R1", sid)) r1_winners <- c(r1_winners, completed_slots[[sid]])
    if (grepl("^R2", sid)) r2_winners <- c(r2_winners, completed_slots[[sid]])
    if (grepl("^S16", sid)) s16_winners <- c(s16_winners, completed_slots[[sid]])
  }

  # For each slot, what teams are valid picks? (i.e., were alive entering that round)
  valid_for_slot <- list()
  for (sid in completed_in_map) {
    rnd <- slot_round(sid)
    if (rnd == 1) {
      valid_for_slot[[sid]] <- NULL  # any team is valid for R1
    } else if (rnd == 2) {
      valid_for_slot[[sid]] <- r1_winners  # must have won R1
    } else if (rnd == 3) {
      valid_for_slot[[sid]] <- r2_winners  # must have won R2 (and R1)
    } else if (rnd >= 4) {
      valid_for_slot[[sid]] <- s16_winners
    }
  }

  for (i in seq_along(relevant_days)) {
    dc <- relevant_days[i]
    for (j in seq_along(completed_in_map)) {
      sid <- completed_in_map[j]
      # Positive: how many day teams match this slot's winners
      match_count <- sum(day_teams[[dc]] %in% slot_winners[[sid]])

      # Negative: if this slot requires prior-round winners, check for impossible teams
      penalty <- 0
      valid <- valid_for_slot[[sid]]
      if (!is.null(valid) && length(valid) > 0) {
        # Map day_teams to IDs for comparison, but simpler: check name-level
        # A team in this day column that is NOT in the valid set is impossible
        impossible <- day_teams[[dc]][!day_teams[[dc]] %in% valid]
        penalty <- length(impossible)
      }

      scores[i, j] <- match_count - penalty * 10  # heavy penalty for impossible teams
    }
  }

  # Greedy matching: repeatedly pick the (day, slot) pair with highest score
  new_map <- default_day_map
  temp <- scores
  for (k in seq_len(min(n_days, n_slots))) {
    if (all(temp == -Inf)) break
    best <- which(temp == max(temp), arr.ind = TRUE)
    best <- best[1, , drop = FALSE]
    best_day <- rownames(scores)[best[1, 1]]
    best_slot <- colnames(scores)[best[1, 2]]
    new_map[best_day] <- best_slot
    temp[best[1, 1], ] <- -Inf
    temp[, best[1, 2]] <- -Inf
  }

  # Report any remapping
  swapped <- which(new_map != default_day_map)
  if (length(swapped) > 0) {
    for (s in swapped) {
      cat(sprintf("    Remapped %s: %s -> %s (default was %s)\n",
                  names(new_map)[s], names(new_map)[s],
                  new_map[s], default_day_map[s]))
    }
  }

  new_map
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
                                       name_map, our_username = "TinkyTyler",
                                       completed_slots = NULL) {
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

    # Use detected day mapping from infer_alive_status if available
    # This ensures init_portfolio uses the same mapping that determined alive status
    detected <- contest$detected_day_map
    if (!is.null(detected)) {
      for (dk in names(detected)) {
        if (dk %in% names(day_map)) {
          day_map[dk] <- detected[dk]
        }
      }
    } else if (!is.null(completed_slots) && length(completed_slots) >= 2) {
      old_day_map <- day_map
      day_map <- detect_day_column_order(our_paths, completed_slots, name_map, day_map)
      swapped <- which(day_map != old_day_map)
      if (length(swapped) > 0) {
        cat(sprintf("  [%s] Detected day column reorder\n", contest_name))
      }
    }

    contest_size <- nrow(contest$entry_paths)
    entry_fee <- meta$fee[1]
    prize_pool <- if ("prize_pool" %in% names(meta)) meta$prize_pool[1] else entry_fee * contest_size * 0.85

    # DEBUG: Print day_map and raw scrape data for our entries
    cat(sprintf("\n  [DEBUG %s] day_map: %s\n", contest_name,
                paste(sprintf("%s->%s", names(day_map), day_map), collapse=", ")))
    day_cols_in_data <- grep("^day\\d+$", names(our_paths), value = TRUE)
    cat(sprintf("  [DEBUG] Day columns in scrape: %s\n", paste(day_cols_in_data, collapse=", ")))

    for (i in seq_len(min(nrow(our_paths), 3))) {
      row <- our_paths[i, ]
      cat(sprintf("  [DEBUG] Entry %s raw scrape: ", row$entryId))
      for (dc in sort(day_cols_in_data)) {
        val <- if (dc %in% names(row)) as.character(row[[dc]]) else "NA"
        cat(sprintf("%s=%s  ", dc, val))
      }
      cat("\n")
    }

    # Build entry rows
    for (i in seq_len(nrow(our_paths))) {
      row <- our_paths[i, ]
      entry <- data.table(
        entry_id     = row$entryId,
        contest_id   = cid,
        contest_name = contest_name,
        contest_size = contest_size,
        entry_fee    = entry_fee,
        prize_pool   = prize_pool,
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

          # DEBUG: Print name->id mapping for first few entries
          if (i <= 3) {
            resolved_names <- sapply(team_ids, function(tid) {
              if (is.na(tid)) "???" else teams_dt$name[tid]
            })
            cat(sprintf("    [DEBUG] %s -> %s: '%s' -> ids=%s -> names=%s\n",
                        day_col, slot_id, team_str,
                        paste(team_ids, collapse=","),
                        paste(resolved_names, collapse=",")))
          }

          if (length(team_ids) == 1 && !is.na(team_ids[1])) {
            entry[, (col) := team_ids[1]]
          } else if (length(team_ids) > 1) {
            # Multi-pick slot (e.g., E8 combined or Format C R1)
            # Store first pick in primary column, second in _b column
            valid_ids <- team_ids[!is.na(team_ids)]
            if (length(valid_ids) >= 1) entry[, (col) := valid_ids[1]]
            if (length(valid_ids) >= 2) {
              col_b <- paste0(col, "_b")
              entry[, (col_b) := valid_ids[2]]
            }
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
  portfolio <- init_portfolio_from_entries(entries_dt)

  # Diagnostic: print used teams per entry for verification against website
  cat("\n  Portfolio verification (compare against website):\n")
  pick_cols <- intersect(all_slot_cols(), names(portfolio))
  for (i in seq_len(nrow(portfolio))) {
    row <- portfolio[i]
    vals <- unlist(row[, ..pick_cols])
    used_ids <- vals[!is.na(vals)]
    used_names <- teams_dt$name[used_ids]
    cat(sprintf("    %s [%s] %s: %s\n",
                row$entry_id, row$contest_name,
                if (row$alive) "ALIVE" else "DEAD",
                paste(used_names, collapse = ", ")))
  }
  cat("\n")

  portfolio
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

  cat(sprintf("Inferring alive status from %d completed slots...\n",
              length(completed_slots)))

  for (cn in names(scrape_results$per_contest)) {
    paths <- scrape_results$per_contest[[cn]]$entry_paths
    n <- nrow(paths)

    # Map completed_slots to winner IDs
    slot_winner_ids <- list()
    for (slot_id in names(completed_slots)) {
      winner_ids <- map_team_names(completed_slots[[slot_id]], name_map)
      slot_winner_ids[[slot_id]] <- winner_ids[!is.na(winner_ids)]
    }

    # Default day->slot mapping
    default_day_map <- c(day1="R1_d1", day2="R1_d2", day3="R2_d1", day4="R2_d2",
                         day5="S16_d1", day6="S16_d2", day7="E8", day8="FF", day9="CHAMP")

    # Which day columns and slots are relevant?
    slot_ids <- names(completed_slots)
    day_keys <- names(default_day_map)[default_day_map %in% slot_ids]
    day_keys <- day_keys[day_keys %in% names(paths)]

    if (length(day_keys) == 0) {
      scrape_results$per_contest[[cn]]$entry_paths$alive <- rep(TRUE, n)
      cat(sprintf("  [%s] %d / %d alive (no day columns to check)\n", cn, n, n))
      next
    }

    # Pre-map all entry picks to team IDs for each day column
    pick_id_matrix <- list()
    for (dc in day_keys) {
      pick_id_matrix[[dc]] <- vector("list", n)
      for (i in seq_len(n)) {
        pick <- paths[[dc]][i]
        if (is.na(pick) || !nzchar(pick)) {
          pick_id_matrix[[dc]][[i]] <- integer(0)
        } else {
          picks <- trimws(strsplit(as.character(pick), "\\+")[[1]])
          pick_id_matrix[[dc]][[i]] <- map_team_names(picks, name_map)
        }
      }
    }

    # Helper: count alive entries for a given day->slot mapping
    count_alive <- function(day_to_slot) {
      alive_test <- rep(TRUE, n)
      for (dc in names(day_to_slot)) {
        sid <- day_to_slot[[dc]]
        winners <- slot_winner_ids[[sid]]
        if (is.null(winners)) next
        for (i in seq_len(n)) {
          if (!alive_test[i]) next
          pids <- pick_id_matrix[[dc]][[i]]
          if (length(pids) == 0) next
          for (pid in pids) {
            if (is.na(pid) || !pid %in% winners) {
              alive_test[i] <- FALSE
              break
            }
          }
        }
      }
      sum(alive_test)
    }

    # Determine the correct day->slot assignment
    # With the scrape now sorting slates by team pool size, day columns should
    # already be in approximately correct round order. We only need to resolve
    # within-round d1/d2 ambiguity (at most 2! per round pair).
    relevant_slots <- slot_ids[slot_ids %in% default_day_map[day_keys]]

    if (length(day_keys) <= 1) {
      # Only 1 day column — no permutation needed
      best_map <- setNames(as.list(relevant_slots), day_keys)
    } else if (length(day_keys) <= 4) {
      # For 4 or fewer slots (<=24 permutations), brute-force is fine
      perms <- combinat_perms(length(day_keys))
      best_alive <- -1
      best_map <- setNames(as.list(relevant_slots), day_keys)  # default

      for (perm in perms) {
        candidate_slots <- relevant_slots[perm]
        candidate_map <- setNames(as.list(candidate_slots), day_keys)
        n_alive <- count_alive(candidate_map)
        if (n_alive > best_alive) {
          best_alive <- n_alive
          best_map <- candidate_map
        }
      }
    } else {
      # For 5+ slots, use round-aware approach to avoid combinatorial explosion.
      # Group day columns by round (using team pool size), then permute within
      # round groups only. This reduces 5!=120 to at most 2!*2!*1!=4 permutations.

      # Count unique teams per day column to determine round
      day_team_counts <- sapply(day_keys, function(dc) {
        teams <- unique(trimws(unlist(strsplit(as.character(paths[[dc]]), "\\+"))))
        length(teams[!is.na(teams) & nzchar(teams)])
      })

      # Determine round for each slot
      slot_round_num <- function(sid) {
        if (grepl("^R1", sid)) return(1L)
        if (grepl("^R2", sid)) return(2L)
        if (grepl("^S16", sid)) return(3L)
        if (grepl("^E8", sid)) return(4L)
        if (grepl("^FF", sid)) return(5L)
        return(6L)
      }
      slot_rounds <- sapply(relevant_slots, slot_round_num)

      # Assign round to each day column by matching team counts to rounds
      # Sort day columns by team count descending (more teams = earlier round)
      day_order <- order(day_team_counts, decreasing = TRUE)
      sorted_days <- day_keys[day_order]

      # Sort slots by round
      slot_order <- order(slot_rounds)
      sorted_slots <- relevant_slots[slot_order]

      # Group slots by round
      sorted_slot_rounds <- slot_rounds[slot_order]
      round_groups <- split(seq_along(sorted_slots), sorted_slot_rounds)

      # Build candidate mappings by permuting within round groups only
      # Start with default: assign sorted_days to sorted_slots in order
      base_map <- setNames(as.list(sorted_slots), sorted_days)

      # Generate all within-round permutations
      all_maps <- list(base_map)
      for (rg in round_groups) {
        if (length(rg) <= 1) next
        # Permute the day columns in this round group
        sub_perms <- combinat_perms(length(rg))
        new_maps <- list()
        for (existing_map in all_maps) {
          for (sp in sub_perms) {
            new_map <- existing_map
            rg_days <- sorted_days[rg]
            rg_slots <- sorted_slots[rg]
            permuted_slots <- rg_slots[sp]
            for (k in seq_along(rg)) {
              new_map[[rg_days[k]]] <- permuted_slots[k]
            }
            new_maps[[length(new_maps) + 1]] <- new_map
          }
        }
        all_maps <- new_maps
      }

      cat(sprintf("  [%s] Round-aware detection: %d candidates (from %d day cols)\n",
                  cn, length(all_maps), length(day_keys)))
      cat(sprintf("    Day team counts: %s\n",
                  paste(sprintf("%s=%d", day_keys, day_team_counts), collapse=", ")))

      best_alive <- -1
      best_map <- base_map
      for (candidate_map in all_maps) {
        n_alive <- count_alive(candidate_map)
        if (n_alive > best_alive) {
          best_alive <- n_alive
          best_map <- candidate_map
        }
      }
    }

    # Report if mapping differs from default
    default_slots <- default_day_map[day_keys]
    detected_slots <- unlist(best_map)
    if (any(detected_slots != default_slots)) {
      cat(sprintf("  [%s] Day column reorder detected:\n", cn))
      for (di in seq_along(day_keys)) {
        if (detected_slots[di] != default_slots[di]) {
          cat(sprintf("    %s -> %s (default was %s)\n",
                      day_keys[di], detected_slots[di], default_slots[di]))
        }
      }
    }

    # Compute final alive status with best mapping
    alive <- rep(TRUE, n)
    for (dc in names(best_map)) {
      sid <- best_map[[dc]]
      winners <- slot_winner_ids[[sid]]
      if (is.null(winners)) next
      for (i in seq_len(n)) {
        if (!alive[i]) next
        pids <- pick_id_matrix[[dc]][[i]]
        if (length(pids) == 0) next
        for (pid in pids) {
          if (is.na(pid) || !pid %in% winners) {
            alive[i] <- FALSE
            break
          }
        }
      }
    }

    # Store detected day mapping so init_portfolio_from_scrape can reuse it
    scrape_results$per_contest[[cn]]$detected_day_map <- setNames(
      unlist(best_map), day_keys
    )

    scrape_results$per_contest[[cn]]$entry_paths$alive <- alive
    n_alive <- sum(alive)
    cat(sprintf("  [%s] %d / %d alive (%.1f%%)\n",
                cn, n_alive, n, 100 * n_alive / n))
  }

  # Update combined entry_paths too
  if (!is.null(scrape_results$entry_paths)) {
    all_paths <- list()
    for (cn in names(scrape_results$per_contest)) {
      contest <- scrape_results$per_contest[[cn]]
      all_paths[[cn]] <- contest$entry_paths
    }
    scrape_results$entry_paths <- rbindlist(all_paths, fill = TRUE)
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
                                          name_map, our_username,
                                          completed_slots = completed_slots)

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
    format          = fmt,
    scrape_results  = scrape_results,
    our_username    = our_username,
    completed_slots = completed_slots
  )
}

# ==============================================================================
# EXPORT ALLOCATION AS SPLASH PICK TEMPLATE CSVs
# ==============================================================================

#' Build Splash team ID mapping from scrape data
#'
#' Extracts the team_name -> splash_team_uuid mapping from the nested
#' entries_df$teams column in the scrape results.
#' @param scrape_results Output from scrape_all_splash()
#' @return Named character vector: splash_team_name -> splash_team_uuid
build_splash_team_id_map <- function(scrape_results) {
  team_map <- character(0)

  for (contest in scrape_results$per_contest) {
    edf <- contest$entries_df

    # Method 1: Extract from entries' teams lists
    if ("teams" %in% names(edf)) {
      for (t in edf$teams) {
        if (is.data.frame(t) && "name" %in% names(t) && "id" %in% names(t)) {
          for (i in seq_len(nrow(t))) {
            team_map[t$name[i]] <- t$id[i]
          }
        }
      }
    }

    # Method 2: Scan ALL columns of entries_df for slate pick data
    # Slate columns are UUID-named and contain data.frames with teamName/teamId
    slate_cols <- names(edf)[grepl("^[0-9a-f]{8}-", names(edf))]
    for (sc in slate_cols) {
      col_data <- edf[[sc]]
      if (is.data.frame(col_data)) {
        # Flat data.frame with teamName + teamId columns
        if ("teamName" %in% names(col_data) && "teamId" %in% names(col_data)) {
          valid <- !is.na(col_data$teamName) & !is.na(col_data$teamId)
          for (i in which(valid)) {
            team_map[col_data$teamName[i]] <- col_data$teamId[i]
          }
        }
      } else if (is.list(col_data)) {
        # List of per-entry picks (each element is a list or data.frame)
        for (pick in col_data) {
          if (is.null(pick)) next
          if (is.data.frame(pick)) {
            if ("teamName" %in% names(pick) && "teamId" %in% names(pick)) {
              valid <- !is.na(pick$teamName) & !is.na(pick$teamId)
              for (i in which(valid)) {
                team_map[pick$teamName[i]] <- pick$teamId[i]
              }
            }
          } else if (is.list(pick)) {
            tn <- pick$teamName %||% pick$name
            tid <- pick$teamId %||% pick$id
            if (!is.null(tn) && !is.null(tid) && !is.na(tn) && !is.na(tid)) {
              team_map[tn] <- tid
            }
          }
        }
      }
    }
  }

  # Method 3: Read pick template CSVs from Downloads folder
  # These contain Team Name + Team ID for ALL available teams (including today's)
  downloads <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
  templates <- list.files(downloads, pattern = "pick-template.*\\.csv$",
                          full.names = TRUE)
  if (length(templates) > 0) {
    for (tf in templates) {
      tryCatch({
        tpl <- read.csv(tf, stringsAsFactors = FALSE)
        # Template has columns like "Team.Name" and "Team.ID"
        tn_col <- grep("Team.*Name", names(tpl), value = TRUE)[1]
        tid_col <- grep("Team.*ID", names(tpl), value = TRUE)[1]
        if (!is.na(tn_col) && !is.na(tid_col)) {
          valid <- !is.na(tpl[[tn_col]]) & nzchar(tpl[[tn_col]]) &
                   !is.na(tpl[[tid_col]]) & nzchar(tpl[[tid_col]])
          for (i in which(valid)) {
            team_map[tpl[[tn_col]][i]] <- tpl[[tid_col]][i]
          }
        }
      }, error = function(e) NULL)
    }
    cat(sprintf("  Read %d pick template(s) from Downloads\n", length(templates)))
  }

  # Add common aliases so sim names resolve to template names
  aliases <- list(
    "Miami"       = "Miami (FL)",
    "Miami OH"    = "Miami (OH)",
    "Cal Baptist" = "California Baptist",
    "Queens"      = "Queens University"
  )
  for (alias_from in names(aliases)) {
    alias_to <- aliases[[alias_from]]
    if (alias_from %in% names(team_map)) next  # already has an entry
    if (alias_to %in% names(team_map)) {
      team_map[alias_from] <- team_map[alias_to]
    }
  }

  cat(sprintf("  Splash team ID map: %d teams\n", length(team_map)))
  team_map
}

#' Export optimizer allocation as Splash pick template CSVs
#'
#' Generates one CSV per contest in Splash's upload format, with Pick 1
#' filled in based on the optimizer's allocation.
#'
#' @param allocation The allocation result from run_optimizer() (the by_team table)
#' @param scrape_results Output from scrape_all_splash()
#' @param portfolio The portfolio state (data.table with entry_id, contest_id, etc.)
#' @param splash_team_map Named character vector from build_splash_team_id_map()
#' @param name_map data.table with splash_name, sim_name, team_id from build_name_map()
#' @param output_dir Directory to write CSVs (default: working directory)
#' @param our_username Our Splash username
#' @return Character vector of file paths written
#' @export
export_allocation_csvs <- function(allocation, scrape_results, portfolio,
                                    splash_team_map, name_map,
                                    output_dir = ".", our_username = "TinkyTyler",
                                    locked_teams = NULL,
                                    completed_slots = NULL,
                                    teams_dt = NULL) {

  if (is.null(allocation) || nrow(allocation) == 0) {
    cat("No allocation to export.\n")
    return(character(0))
  }

  # Map sim team names to splash team names
  sim_to_splash <- setNames(name_map$splash_name, name_map$sim_name)

  # Get contest info
  contest_names <- setNames(
    sapply(scrape_results$per_contest, function(c) c$contest_name),
    sapply(scrape_results$per_contest, function(c) c$contest_id)
  )

  # Group allocation by contest
  alloc_by_contest <- split(allocation, by = "contest_id")
  files_written <- character(0)

  for (cid in names(alloc_by_contest)) {
    ct_alloc <- alloc_by_contest[[cid]]
    ct_name <- contest_names[cid]
    if (is.na(ct_name)) ct_name <- cid

    # Get our alive entries for this contest
    ct_entries <- portfolio[contest_id == cid & alive == TRUE]
    if (nrow(ct_entries) == 0) next

    # Use group_entries() to assign entries to groups — same logic optimizer used
    ct_groups <- group_entries(ct_entries)

    # Build entry-to-group mapping using used_teams hash (not group_id, which is
    # numbered globally by the optimizer but per-contest here)
    # Hash: sorted used_teams pasted together
    make_ut_hash <- function(ut) paste(sort(ut), collapse = ",")

    entry_to_ut_hash <- data.table(
      entry_id = character(0),
      ut_hash = character(0)
    )
    for (gi in seq_len(nrow(ct_groups))) {
      eids <- ct_groups$entry_ids[[gi]]
      h <- make_ut_hash(ct_groups$used_teams[[gi]])
      entry_to_ut_hash <- rbind(entry_to_ut_hash, data.table(
        entry_id = eids,
        ut_hash = h
      ))
    }

    cat(sprintf("  [DEBUG] %d entries in %d groups\n",
                nrow(ct_entries), nrow(ct_groups)))

    # Build group assignment: expand allocation rows to individual entries
    entry_picks <- data.table(
      entry_id = character(0),
      pick_team_name = character(0),
      pick_splash_id = character(0),
      pick2_team_name = character(0),
      pick2_splash_id = character(0)
    )

    for (ai in seq_len(nrow(ct_alloc))) {
      a <- ct_alloc[ai]
      alloc_ut_hash <- make_ut_hash(a$used_teams[[1]])

      # Find entries with matching used_teams hash
      group_eids <- entry_to_ut_hash[ut_hash == alloc_ut_hash, entry_id]
      matching <- ct_entries[entry_id %in% group_eids]

      if (nrow(matching) == 0) {
        cat(sprintf("  [DEBUG] No match for alloc %d: group_id=%d, team=%s, ut_hash=%s\n",
                    ai, a$group_id, a$team_name, alloc_ut_hash))
        next
      }

      # Take n_assigned entries from matching (not yet assigned)
      already_assigned <- entry_picks$entry_id
      available <- matching[!entry_id %in% already_assigned]
      n_take <- min(a$n_assigned, nrow(available))
      if (n_take == 0) next

      selected <- available[1:n_take]

      # Map team name to splash ID
      team_name <- a$team_name

      # Resolve primary pick splash ID
      resolve_splash_id <- function(tn) {
        sid <- splash_team_map[tn]
        if (is.na(sid)) {
          sn <- sim_to_splash[tn]
          if (!is.na(sn)) sid <- splash_team_map[sn]
        }
        # Fallback: prefix match (e.g. "Arkansas" matches "Arkansas Razorbacks")
        if (is.na(sid)) {
          splash_names <- names(splash_team_map)
          matches <- splash_names[startsWith(splash_names, tn)]
          if (length(matches) == 1) {
            sid <- splash_team_map[matches[1]]
          } else if (length(matches) > 1) {
            # Take exact-length match or first
            exact <- matches[nchar(matches) == nchar(tn)]
            sid <- splash_team_map[if (length(exact) > 0) exact[1] else matches[1]]
          }
        }
        sid
      }

      splash_id <- resolve_splash_id(team_name)
      if (is.na(splash_id)) {
        cat(sprintf("  WARNING: No Splash team ID for '%s', skipping\n", team_name))
        next
      }

      # Handle companion pick (format C multi-pick)
      extra_name <- if ("slot1_extra_name" %in% names(a) && !is.na(a$slot1_extra_name)) {
        a$slot1_extra_name
      } else NA_character_
      extra_splash_id <- if (!is.na(extra_name)) resolve_splash_id(extra_name) else NA_character_

      entry_picks <- rbind(entry_picks, data.table(
        entry_id = selected$entry_id,
        pick_team_name = team_name,
        pick_splash_id = as.character(splash_id),
        pick2_team_name = extra_name,
        pick2_splash_id = if (!is.na(extra_splash_id)) as.character(extra_splash_id) else NA_character_
      ))
    }

    # Also include entries that already have picks locked in for today
    # Look up from scrape entry_paths which entries already have day2 picks
    ct_scrape_pre <- NULL
    today_col <- "day2"  # default, may be overridden below
    for (pc in scrape_results$per_contest) {
      if (pc$contest_id == cid) { ct_scrape_pre <- pc; break }
    }
    if (!is.null(ct_scrape_pre) && !is.null(ct_scrape_pre$entry_paths)) {
      our_paths <- ct_scrape_pre$entry_paths[ct_scrape_pre$entry_paths$entryName == our_username, ]
      # Find which day column corresponds to today's slot (R1_d2)
      # Use same swap detection as init_portfolio_from_scrape: check which
      # column has R1_d1 winners to determine column order, then pick the
      # column that maps to the current slot (R1_d2).
      day_cols <- grep("^day", names(our_paths), value = TRUE)
      # Get format for this contest from portfolio
      ct_fmt <- portfolio[contest_id == cid, format[1]]
      if (is.na(ct_fmt)) ct_fmt <- "A"
      ct_day_map <- DAY_SLOT_MAP[[ct_fmt]]

      # Detect column order using general permutation detection
      if (!is.null(completed_slots) && length(completed_slots) >= 2 && length(day_cols) >= 2) {
        ct_day_map <- detect_day_column_order(our_paths, completed_slots, name_map, ct_day_map)
      }

      # Find the day column that maps to R1_d2 (today's slot)
      today_col <- NULL
      for (dk in names(ct_day_map)) {
        if (ct_day_map[dk] == "R1_d2") { today_col <- dk; break }
      }
      if (is.null(today_col)) today_col <- "day2"  # fallback
      cat(sprintf("  [DEBUG] Using column '%s' for today's picks\n", today_col))
      if (today_col %in% names(our_paths) && nrow(our_paths) > 0) {
        locked <- our_paths[!is.na(our_paths[[today_col]]) & our_paths[[today_col]] != "", ]
        if (nrow(locked) > 0) {
          resolve_sid <- function(tn) {
            sid <- splash_team_map[tn]
            if (is.na(sid)) {
              sn <- sim_to_splash[tn]
              if (!is.na(sn)) sid <- splash_team_map[sn]
            }
            sid
          }
          n_locked <- 0
          for (li in seq_len(nrow(locked))) {
            eid <- locked$entryId[li]
            if (eid %in% entry_picks$entry_id) next  # already assigned by optimizer
            pick_raw <- locked[[today_col]][li]
            # Split multi-pick entries (e.g., "Arizona+UConn")
            pick_parts <- trimws(strsplit(as.character(pick_raw), "\\+")[[1]])
            # Check if ANY part is a locked team (using name_map for aliases like "Miami (FL)" -> "Miami")
            pick_parts_sim <- sapply(pick_parts, function(pn) {
              mid <- map_team_names(pn, name_map)
              # Use name_map to get sim name; fall back to teams_dt if available
              nm_row <- name_map[splash_name == pn]
              if (nrow(nm_row) > 0) nm_row$sim_name[1]
              else if (!is.null(teams_dt) && !is.na(mid)) teams_dt$name[mid]
              else pn
            })
            has_locked <- any(pick_parts_sim %in% locked_teams) || any(pick_parts %in% locked_teams)
            if (!is.null(locked_teams) && !has_locked) next
            # Resolve splash IDs for each pick
            pick1_name <- pick_parts[1]
            pick1_sid <- resolve_sid(pick1_name)
            if (is.na(pick1_sid)) pick1_sid <- splash_team_map[pick1_name]
            pick2_name <- if (length(pick_parts) >= 2) pick_parts[2] else NA_character_
            pick2_sid <- if (!is.na(pick2_name)) {
              sid <- resolve_sid(pick2_name)
              if (is.na(sid)) sid <- splash_team_map[pick2_name]
              as.character(sid)
            } else NA_character_
            if (is.na(pick1_sid)) {
              cat(sprintf("  WARNING: No Splash ID for locked pick '%s'\n", pick1_name))
              next
            }
            entry_picks <- rbind(entry_picks, data.table(
              entry_id = eid,
              pick_team_name = pick1_name,
              pick_splash_id = as.character(pick1_sid),
              pick2_team_name = pick2_name,
              pick2_splash_id = pick2_sid
            ))
            n_locked <- n_locked + 1
          }
          if (n_locked > 0) cat(sprintf("  [DEBUG] Added %d already-locked entries\n", n_locked))
        }
        # Also add any remaining entries not yet in entry_picks (unlocked but not in optimizer)
        n_remaining <- 0
        for (li in seq_len(nrow(our_paths))) {
          eid <- our_paths$entryId[li]
          if (eid %in% entry_picks$entry_id) next
          pick_raw <- our_paths[[today_col]][li]
          if (is.na(pick_raw) || !nzchar(pick_raw)) next
          pick_parts <- trimws(strsplit(as.character(pick_raw), "\\+")[[1]])
          pick1_sid <- resolve_sid(pick_parts[1])
          if (is.na(pick1_sid)) pick1_sid <- splash_team_map[pick_parts[1]]
          if (is.na(pick1_sid)) next
          pick2_name <- if (length(pick_parts) >= 2) pick_parts[2] else NA_character_
          pick2_sid <- if (!is.na(pick2_name)) {
            sid <- resolve_sid(pick2_name)
            if (is.na(sid)) sid <- splash_team_map[pick2_name]
            as.character(sid)
          } else NA_character_
          entry_picks <- rbind(entry_picks, data.table(
            entry_id = eid,
            pick_team_name = pick_parts[1],
            pick_splash_id = as.character(pick1_sid),
            pick2_team_name = pick2_name,
            pick2_splash_id = pick2_sid
          ))
          n_remaining <- n_remaining + 1
        }
        if (n_remaining > 0) cat(sprintf("  [DEBUG] Added %d remaining entries from scrape\n", n_remaining))
      }
    }

    if (nrow(entry_picks) == 0) {
      cat(sprintf("  [%s] No picks to export\n", ct_name))
      next
    }

    # Get the entry names and previous picks from scrape data
    ct_scrape <- NULL
    scrape_cids <- sapply(scrape_results$per_contest, function(pc) pc$contest_id)
    for (pc in scrape_results$per_contest) {
      if (pc$contest_id == cid) { ct_scrape <- pc; break }
    }
    if (is.null(ct_scrape)) {
      cat(sprintf("  [DEBUG] Contest %s not in scrape. Available: %s\n",
                  cid, paste(scrape_cids, collapse=", ")))
    }

    # Build output rows
    if (is.null(ct_scrape)) {
      cat(sprintf("  [%s] Contest not found in scrape data\n", ct_name))
      next
    }
    our_entries <- ct_scrape$entries_df[ct_scrape$entries_df$entryName == our_username, ]
    if (is.null(our_entries) || nrow(our_entries) == 0) {
      cat(sprintf("  [%s] No entries found for %s in scrape\n", ct_name, our_username))
      next
    }

    # Get previous picks from entry_paths
    ct_paths <- ct_scrape$entry_paths[ct_scrape$entry_paths$entryName == our_username, ]

    # Build the CSV data
    out_rows <- list()
    for (i in seq_len(nrow(entry_picks))) {
      ep <- entry_picks[i]
      # Find entry info
      entry_row <- our_entries[our_entries$entryId == ep$entry_id, ]
      if (nrow(entry_row) == 0) next

      path_row <- ct_paths[ct_paths$entryId == ep$entry_id, ]
      # Use the column that is NOT today's picks (the other day column)
      prev_col <- if (today_col == "day1") "day2" else "day1"
      prev_pick <- if (nrow(path_row) > 0 && prev_col %in% names(path_row)) {
        path_row[[prev_col]][1]
      } else ""

      row_data <- data.frame(
        Entry_Name = entry_row$entryDisplayName[1],
        Entry_ID = ep$entry_id,
        Previous_Picks = prev_pick,
        Pick_1 = ep$pick_splash_id,
        Team_Name = ep$pick_team_name,
        stringsAsFactors = FALSE
      )
      # Add Pick 2 for multi-pick formats (format C)
      if (!is.na(ep$pick2_splash_id)) {
        row_data$Pick_2 <- ep$pick2_splash_id
        row_data$Team_Name_2 <- ep$pick2_team_name
      }
      out_rows[[length(out_rows) + 1]] <- row_data
    }

    out_df <- rbindlist(lapply(out_rows, as.data.table), fill = TRUE)

    # Sort by entry number (extract numeric part from Entry_Name)
    entry_nums <- as.numeric(gsub("[^0-9]", "", out_df$Entry_Name))
    out_df <- out_df[order(entry_nums), ]

    # Write CSV
    safe_name <- gsub("[^A-Za-z0-9]", "_", ct_name)
    fname <- file.path(output_dir, sprintf("picks_%s.csv", safe_name))
    write.csv(out_df, fname, row.names = FALSE)
    files_written <- c(files_written, fname)
    cat(sprintf("  [%s] Wrote %d picks to %s\n", ct_name, nrow(out_df), fname))
  }

  cat(sprintf("\nExported %d contest CSVs to %s\n", length(files_written), output_dir))
  invisible(files_written)
}
