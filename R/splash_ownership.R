#!/usr/bin/env Rscript
# ==============================================================================
# splash_ownership.R
# Field ownership estimation for Splash NCAA survivor pools.
#
# Key differences from Hodes (ownership_model.R):
#   - 1 pick per day (not 3 per round) — higher chalk concentration
#   - No seed-sum tiebreaker — seed boost = 0
#   - Candidate pool is only teams playing TODAY
#   - Day-level granularity (R1 Thu vs R1 Fri are separate decisions)
#
# Calibration priority:
#   1. Live scrape from Splash API (actual ownership, post-hoc)
#   2. Splash historical data (when available)
#   3. Simple win-prob-based softmax as starting point
#
# Usage:
#   source("splash_config.R")
#   source("splash_ownership.R")
# ==============================================================================

library(data.table)

# ==============================================================================
# CORE OWNERSHIP MODEL
# ==============================================================================

#' Estimate field ownership for a given slot
#'
#' For each team playing today, compute the fraction of field entries expected
#' to pick that team. Shares sum to n_picks for the slot (1 for most, 2 for E8).
#'
#' @param slot_id Character slot ID (e.g., "R1_d1")
#' @param teams_dt Data frame with columns: team_id, name, seed, region, rating/AdjEM
#' @param sim_matrix The all_results matrix (n_sims x 63)
#' @param round_info Data frame from sim output
#' @param prior_ownership Optional named list of actual ownership from prior slots
#'   (team_name -> fraction). Used to estimate availability for later slots.
#' @param field_used_dist Optional: estimated distribution of teams the field has
#'   already used. A named numeric vector: team_name -> fraction of field that
#'   has used this team.
#' @return Named numeric vector: team_name -> ownership fraction.
#'   Sums to n_picks for the slot.
estimate_ownership <- function(slot_id, teams_dt, sim_matrix, round_info,
                                prior_ownership = NULL, field_used_dist = NULL,
                                params = NULL) {
  slot <- get_slot(slot_id)
  game_idxs <- slot$game_indices
  n_picks <- slot$n_picks
  round_num <- slot$round_num

  # Identify teams playing in this slot
  if (round_num == 1) {
    candidate_ids <- integer(0)
    for (g in game_idxs) {
      candidate_ids <- c(candidate_ids,
                         teams_dt$team_id[2 * g - 1],
                         teams_dt$team_id[2 * g])
    }
    candidates <- teams_dt[teams_dt$team_id %in% candidate_ids, ]
  } else {
    candidates <- get_round_candidates(game_idxs, teams_dt, sim_matrix)
  }

  if (nrow(candidates) == 0) {
    warning("No candidates found for slot ", slot_id)
    return(numeric(0))
  }

  # --- Factor 1: Win probability ---
  wp <- compute_slot_win_probs(candidates, game_idxs, round_num,
                                teams_dt, sim_matrix)

  # --- Factor 2: Future value (save effect) ---
  fv <- compute_future_value(candidates, sim_matrix, round_num)

  # --- Factor 3: Availability ---
  avail <- rep(1.0, nrow(candidates))
  if (!is.null(field_used_dist)) {
    for (i in seq_len(nrow(candidates))) {
      used_frac <- field_used_dist[[candidates$name[i]]]
      if (!is.null(used_frac)) {
        avail[i] <- max(1.0 - used_frac, 0.05)
      }
    }
  }

  # --- Get parameters (calibrated or defaults) ---
  round_to_group <- c("R1", "R2", "S16", "E8plus", "E8plus", "E8plus")
  rg <- round_to_group[round_num]

  if (!is.null(params) && !is.null(params$beta_wp)) {
    beta_wp <- params$beta_wp[rg]
    save_strength <- params$save_strengths[round_num]
    beta_avail <- if (!is.null(params$beta_avail)) params$beta_avail else 1.0
  } else {
    beta_wp <- 4.0
    save_strength <- c(0.60, 0.50, 0.35, 0.15, 0.0, 0.0)[round_num]
    beta_avail <- 1.0
  }

  # --- Combine into attractiveness score ---
  log_attract <- beta_wp * wp - save_strength * fv + beta_avail * log(avail)
  log_attract <- log_attract - max(log_attract)

  raw_shares <- exp(log_attract)
  ownership <- n_picks * raw_shares / sum(raw_shares)

  names(ownership) <- candidates$name
  ownership
}

# ==============================================================================
# HELPER: WIN PROBABILITIES FOR CANDIDATES IN A SLOT
# ==============================================================================

#' Compute win probability for each candidate team in the current slot
compute_slot_win_probs <- function(candidates, game_idxs, round_num,
                                    teams_dt, sim_matrix) {
  n_sims <- nrow(sim_matrix)
  wp <- numeric(nrow(candidates))

  for (i in seq_len(nrow(candidates))) {
    tid <- candidates$team_id[i]
    # Count how many sims this team wins a game in this slot
    wins <- 0
    for (g in game_idxs) {
      wins <- wins + sum(sim_matrix[, g] == tid)
    }
    wp[i] <- wins / n_sims
  }

  # For R64, wp is just the team's R1 win probability
  # For later rounds, wp is conditional on reaching this round
  # (the sim matrix already handles this -- a team only appears as
  # a winner if it won all prior rounds in that sim)
  wp
}

# ==============================================================================
# HELPER: FUTURE VALUE (CHAMPIONSHIP EQUITY)
# ==============================================================================

#' Compute future value for each candidate
#' Future value = championship probability, normalized to [0, 1]
compute_future_value <- function(candidates, sim_matrix, round_num) {
  n_sims <- nrow(sim_matrix)
  champ_col <- 63  # championship game column

  fv <- numeric(nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    tid <- candidates$team_id[i]
    fv[i] <- sum(sim_matrix[, champ_col] == tid) / n_sims
  }

  # Normalize to [0, 1]
  max_fv <- max(fv, na.rm = TRUE)
  if (max_fv > 0) fv <- fv / max_fv
  fv
}

# ==============================================================================
# HELPER: GET CANDIDATES FOR LATER ROUNDS
# ==============================================================================

#' For rounds 2+, identify which teams could be playing in these games
get_round_candidates <- function(game_idxs, teams_dt, sim_matrix) {
  # Find all unique team_ids that appear as winners in these game columns
  all_winners <- integer(0)
  for (g in game_idxs) {
    all_winners <- c(all_winners, unique(sim_matrix[, g]))
  }
  candidate_ids <- unique(all_winners)
  teams_dt[teams_dt$team_id %in% candidate_ids, ]
}

# ==============================================================================
# LIVE OWNERSHIP OVERRIDE
# ==============================================================================

#' Compute actual ownership from scraped Splash data
#'
#' After a slot is complete, compute the actual pick distribution from
#' scraped entry data. This overrides the model for future availability
#' calculations.
#'
#' @param picks_df Data frame of all entries' picks for a slot.
#'   Must have columns: entry_id, team_name (or team_id)
#' @param teams_dt Teams data frame for name resolution
#' @return Named numeric vector: team_name -> actual ownership fraction
compute_actual_ownership <- function(picks_df, teams_dt) {
  picks_df <- as.data.table(picks_df)

  if ("team_name" %in% names(picks_df)) {
    counts <- picks_df[, .N, by = team_name]
    total <- nrow(picks_df)
    result <- setNames(counts$N / total, counts$team_name)
  } else if ("team_id" %in% names(picks_df)) {
    counts <- picks_df[, .N, by = team_id]
    total <- nrow(picks_df)
    # Map back to names
    id_to_name <- setNames(teams_dt$name, teams_dt$team_id)
    result <- setNames(counts$N / total, id_to_name[as.character(counts$team_id)])
  } else {
    stop("picks_df must have 'team_name' or 'team_id' column")
  }

  result
}

#' Estimate field's cumulative team usage from prior slot ownership
#'
#' Given actual ownership for completed slots, estimate what fraction of
#' the field has used each team across all prior slots.
#'
#' @param prior_ownerships List of named vectors from compute_actual_ownership()
#'   or estimate_ownership(), one per completed slot.
#' @return Named numeric vector: team_name -> estimated fraction of field that
#'   has used this team in any prior slot.
estimate_field_usage <- function(prior_ownerships) {
  all_teams <- unique(unlist(lapply(prior_ownerships, names)))
  usage <- setNames(rep(0, length(all_teams)), all_teams)

  for (own in prior_ownerships) {
    for (tm in names(own)) {
      # P(field entry used team across slots) ≈ 1 - prod(1 - slot_ownership)
      # Simplified: just sum, capped at 1
      usage[tm] <- min(usage[tm] + own[tm], 1.0)
    }
  }

  usage
}

# ==============================================================================
# LEVERAGE CALCULATION
# ==============================================================================

#' Compute leverage (contrarian value) for each candidate
#'
#' Leverage = win_probability / ownership.
#' Higher leverage = team is underowned relative to their win probability.
#' Good contrarian picks have high leverage.
#'
#' @param win_probs Named numeric vector: team_name -> win probability
#' @param ownership Named numeric vector: team_name -> ownership fraction
#' @return Named numeric vector: team_name -> leverage ratio
compute_leverage <- function(win_probs, ownership) {
  common_teams <- intersect(names(win_probs), names(ownership))
  leverage <- setNames(numeric(length(common_teams)), common_teams)

  for (tm in common_teams) {
    if (ownership[tm] > 0) {
      leverage[tm] <- win_probs[tm] / ownership[tm]
    } else {
      leverage[tm] <- Inf
    }
  }

  leverage
}

#' Print a formatted ownership + leverage table for a slot
print_ownership <- function(slot_id, ownership, teams_dt, sim_matrix) {
  slot <- get_slot(slot_id)
  game_idxs <- slot$game_indices

  # Compute win probs for display
  n_sims <- nrow(sim_matrix)
  wp <- numeric(length(ownership))
  names(wp) <- names(ownership)

  for (tm in names(ownership)) {
    tid <- teams_dt$team_id[teams_dt$name == tm]
    wins <- 0
    for (g in game_idxs) {
      wins <- wins + sum(sim_matrix[, g] == tid)
    }
    wp[tm] <- wins / n_sims
  }

  leverage <- compute_leverage(wp, ownership)

  # Build display table
  display <- data.table(
    Team = names(ownership),
    Seed = teams_dt$seed[match(names(ownership), teams_dt$name)],
    Region = teams_dt$region[match(names(ownership), teams_dt$name)],
    WinProb = wp[names(ownership)],
    Ownership = ownership,
    Leverage = leverage[names(ownership)]
  )
  setorder(display, -Leverage)

  cat(sprintf("\n=== OWNERSHIP: %s ===\n", slot$label))
  cat(sprintf("%-20s %4s %-8s %7s %7s %8s\n",
              "Team", "Seed", "Region", "WinP%", "Own%", "Leverage"))
  cat(paste(rep("-", 58), collapse = ""), "\n")

  for (i in seq_len(nrow(display))) {
    r <- display[i]
    cat(sprintf("%-20s  %2d   %-8s %6.1f%% %6.1f%% %7.2fx\n",
                r$Team, r$Seed, r$Region,
                100 * r$WinProb, 100 * r$Ownership, r$Leverage))
  }
  cat("\n")

  invisible(display)
}

# ==============================================================================
# HISTORICAL PICK DATA (FULL ENTRY-LEVEL)
# ==============================================================================

# Fixed day-to-slot mappings for day7_8/day9/day10 (always E8/FF/CHAMP)
# Days 1-6 vary by year — auto-detected from entry counts in detect_day_mapping()
FORMAT_A_FIXED_SLOTS <- c("day7_8" = "E8", "day9" = "FF", "day10" = "CHAMP")
FORMAT_A_VARIABLE_DAYS <- c("day1", "day2", "day3", "day4", "day5", "day6")
FORMAT_A_VARIABLE_SLOTS <- c("R1_d1", "R1_d2", "R2_d1", "R2_d2", "S16_d1", "S16_d2")

#' Auto-detect the day-to-slot mapping from entry counts
#'
#' The Splash API's day numbering is NOT chronological and varies by year.
#' We detect the mapping by counting picks per day column: the first
#' chronological slot has the most entries (~all alive), each subsequent
#' slot has fewer as entries get eliminated. Sort by descending count
#' to recover the chronological order.
#'
#' @param results_dt data.table of raw results
#' @return Named character vector: day_col -> slot_id
detect_day_mapping <- function(results_dt) {
  n_entries <- nrow(results_dt)

  # Count non-NA picks per variable day column
  day_counts <- sapply(FORMAT_A_VARIABLE_DAYS, function(day_col) {
    # Try flat format (2025): day{X}_teamAlias
    alias_col <- paste0(day_col, "_teamAlias")
    if (alias_col %in% names(results_dt)) {
      return(sum(!is.na(results_dt[[alias_col]])))
    }
    # Try picks format (2024): day{X}_picks
    picks_col <- paste0(day_col, "_picks")
    if (picks_col %in% names(results_dt)) {
      pd <- results_dt[[picks_col]]
      if (is.list(pd) && "teamAlias" %in% names(pd)) {
        return(sum(!is.na(pd[["teamAlias"]])))
      }
    }
    return(0L)
  })

  # Sort by descending count = chronological order
  sorted_days <- names(sort(day_counts, decreasing = TRUE))

  # Map to slots in chronological order
  day_map <- setNames(FORMAT_A_VARIABLE_SLOTS, sorted_days)

  # Add fixed mappings
  day_map <- c(day_map, FORMAT_A_FIXED_SLOTS)

  cat(sprintf("  Auto-detected day mapping (by entry count):\n"))
  for (d in c(sorted_days, names(FORMAT_A_FIXED_SLOTS))) {
    slot <- day_map[d]
    cnt <- if (d %in% names(day_counts)) day_counts[d] else "—"
    cat(sprintf("    %7s -> %-8s (%s entries)\n", d, slot, cnt))
  }

  day_map
}

#' Extract picks from a single day column (handles both API formats)
#'
#' Splash API data comes in two formats:
#'   2025+: flat columns (day1_teamAlias, day1_teamName, day1_winning)
#'   2024:  list-of-vectors column (day1_picks$teamAlias, day1_picks$teamName, etc.)
#'   E8:    nested per-row data.frames (day7_8) OR list-of-vectors (day7_8$teamAlias)
#'
#' @param results_dt data.table of raw results
#' @param day_col Character day column prefix (e.g., "day1", "day7_8")
#' @param slot_id Character slot ID to assign
#' @param year Integer year
#' @return data.table with columns: year, entry_id, slot_id, team_alias, team_name, won
extract_day_picks <- function(results_dt, day_col, slot_id, year) {
  n_entries <- nrow(results_dt)
  entry_ids <- if ("entryId" %in% names(results_dt)) results_dt$entryId else seq_len(n_entries)

  # --- Format detection ---
  # Check for flat columns (2025 style): day1_teamAlias, day1_teamName, etc.
  flat_alias <- paste0(day_col, "_teamAlias")
  flat_name <- paste0(day_col, "_teamName")
  flat_win <- paste0(day_col, "_winning")

  # Check for list-of-vectors column (2024 style): day1_picks
  picks_col <- paste0(day_col, "_picks")

  # Check for the column itself being a nested structure (day7_8)
  has_flat <- flat_alias %in% names(results_dt) || flat_name %in% names(results_dt)
  has_picks_col <- picks_col %in% names(results_dt)
  has_direct <- day_col %in% names(results_dt)

  if (has_flat) {
    # --- 2025 flat format: day{X}_teamAlias, day{X}_teamName, day{X}_winning ---
    return(data.table(
      year       = year,
      entry_id   = entry_ids,
      slot_id    = slot_id,
      team_alias = if (flat_alias %in% names(results_dt)) as.character(results_dt[[flat_alias]]) else NA_character_,
      team_name  = if (flat_name %in% names(results_dt)) as.character(results_dt[[flat_name]]) else NA_character_,
      won        = if (flat_win %in% names(results_dt)) as.logical(results_dt[[flat_win]]) else NA
    ))
  }

  if (has_picks_col) {
    # --- 2024 format: day{X}_picks column ---
    # Could be stored as:
    #   (a) Named list of vectors: list(gameId=chr[n], teamAlias=chr[n], ...)
    #   (b) List column of per-row named lists: list(list(gameId="x", teamAlias="y",...), ...)
    picks_data <- results_dt[[picks_col]]

    if (is.list(picks_data)) {
      # Check if it's a named list/data.frame of vectors (pattern a)
      # data.table stores day1_picks as a data.frame (9 cols × n_entries rows)
      if (!is.null(names(picks_data)) && "teamAlias" %in% names(picks_data)) {
        alias_vec <- picks_data[["teamAlias"]]
        name_vec <- picks_data[["teamName"]]
        win_vec <- picks_data[["winning"]]

        return(data.table(
          year       = year,
          entry_id   = entry_ids,
          slot_id    = slot_id,
          team_alias = if (!is.null(alias_vec)) as.character(alias_vec) else NA_character_,
          team_name  = if (!is.null(name_vec)) as.character(name_vec) else NA_character_,
          won        = if (!is.null(win_vec)) as.logical(win_vec) else NA
        ))
      }

      # Check if it's a list of per-row named lists (pattern b)
      # Peek at the first non-NULL element to detect structure
      first_elem <- picks_data[[1]]
      if (is.list(first_elem) && !is.null(names(first_elem)) &&
          "teamAlias" %in% names(first_elem)) {
        picks_long <- rbindlist(lapply(seq_len(n_entries), function(i) {
          item <- picks_data[[i]]
          if (is.null(item)) {
            return(data.table(
              year = year, entry_id = entry_ids[i], slot_id = slot_id,
              team_alias = NA_character_, team_name = NA_character_, won = NA
            ))
          }
          # Item might be a named list with $picks sub-element (like day7_8 in 2024)
          if ("picks" %in% names(item) && is.data.frame(item$picks)) {
            item <- item$picks
          }
          data.table(
            year       = year,
            entry_id   = entry_ids[i],
            slot_id    = slot_id,
            team_alias = as.character(item[["teamAlias"]]),
            team_name  = as.character(item[["teamName"]]),
            won        = as.logical(item[["winning"]])
          )
        }), fill = TRUE)
        return(picks_long)
      }
    }
  }

  if (has_direct) {
    # --- Nested column (day7_8): could be per-row data.frames OR list-of-vectors ---
    nested_data <- results_dt[[day_col]]

    if (is.list(nested_data) && !is.null(names(nested_data)) &&
        "teamAlias" %in% names(nested_data)) {
      # List-of-vectors format (like 2024 day_picks)
      alias_vec <- nested_data[["teamAlias"]]
      name_vec <- nested_data[["teamName"]]
      win_vec <- nested_data[["winning"]]

      # These vectors may be longer than n_entries (E8 has 2 picks per entry)
      n_picks_per <- length(alias_vec) / n_entries
      if (n_picks_per == round(n_picks_per) && n_picks_per > 1) {
        # Multiple picks per entry — interleaved
        return(data.table(
          year       = year,
          entry_id   = rep(entry_ids, each = as.integer(n_picks_per)),
          slot_id    = slot_id,
          team_alias = as.character(alias_vec),
          team_name  = as.character(name_vec),
          won        = as.logical(win_vec)
        ))
      } else {
        return(data.table(
          year       = year,
          entry_id   = entry_ids,
          slot_id    = slot_id,
          team_alias = as.character(alias_vec),
          team_name  = as.character(name_vec),
          won        = as.logical(win_vec)
        ))
      }
    }

    # Per-row nested structures: either a data.frame directly (2025 day7_8)
    # or a list with $picks containing the data.frame (2024 day7_8)
    nested_picks <- rbindlist(lapply(seq_len(n_entries), function(i) {
      item <- nested_data[[i]]

      # 2024 format: item is a list with $picks as the actual data.frame
      if (is.list(item) && !is.data.frame(item) && "picks" %in% names(item)) {
        item <- item$picks
      }

      if (is.null(item) || !is.data.frame(item) || nrow(item) == 0) {
        return(data.table(
          year = year, entry_id = entry_ids[i], slot_id = slot_id,
          team_alias = NA_character_, team_name = NA_character_, won = NA
        ))
      }
      alias_col_name <- intersect(names(item), c("teamAlias", "team_alias"))
      name_col_name <- intersect(names(item), c("teamName", "team_name"))
      win_col_name <- intersect(names(item), c("winning", "won"))

      data.table(
        year       = year,
        entry_id   = entry_ids[i],
        slot_id    = slot_id,
        team_alias = if (length(alias_col_name)) as.character(item[[alias_col_name[1]]]) else NA_character_,
        team_name  = if (length(name_col_name)) as.character(item[[name_col_name[1]]]) else NA_character_,
        won        = if (length(win_col_name)) as.logical(item[[win_col_name[1]]]) else NA
      )
    }), fill = TRUE)

    return(nested_picks)
  }

  # Column not found at all — return empty
  warning("No data found for ", day_col, " in ", year, " results")
  data.table(year = integer(0), entry_id = character(0), slot_id = character(0),
             team_alias = character(0), team_name = character(0), won = logical(0))
}

#' Parse raw Splash results RDS into a long-form picks table
#'
#' Handles both API formats:
#'   2025+: flat columns (day1_teamAlias, day1_teamName, day1_winning)
#'   2024:  list-of-vectors columns (day1_picks$teamAlias, etc.)
#'   E8:    nested per-row data.frames (day7_8) in 2025, list-of-vectors in 2024
#'
#' @param results_dt data.table loaded from results_YYYY.rds
#' @param year Integer year (2024, 2025, etc.)
#' @param format Character format code (default "A")
#' @return data.table with columns: year, entry_id, slot_id, team_alias, team_name, won
parse_splash_results <- function(results_dt, year, format = "A") {
  if (format != "A") stop("Only Format A parsing is implemented so far")

  # Auto-detect day-to-slot mapping (API day numbering varies by year)
  day_map <- detect_day_mapping(results_dt)
  n_entries <- nrow(results_dt)

  picks_list <- lapply(seq_along(day_map), function(i) {
    extract_day_picks(results_dt, names(day_map)[i], day_map[i], year)
  })

  all_picks <- rbindlist(picks_list, fill = TRUE)

  # Drop rows where pick is NA (entry was eliminated before this slot)
  all_picks <- all_picks[!is.na(team_alias) | !is.na(team_name)]

  cat(sprintf("Parsed %d picks from %d entries (%d), %d unique slots\n",
              nrow(all_picks), n_entries, year, uniqueN(all_picks$slot_id)))

  # Summary
  slot_summary <- all_picks[, .(n_picks = .N, n_teams = uniqueN(team_alias)),
                             by = slot_id]
  setorder(slot_summary, slot_id)
  cat("  Picks per slot:\n")
  for (i in seq_len(nrow(slot_summary))) {
    r <- slot_summary[i]
    cat(sprintf("    %-8s: %5d picks across %2d teams\n",
                r$slot_id, r$n_picks, r$n_teams))
  }

  all_picks
}

#' Load and parse multiple years of Splash results
#'
#' @param files Named list: year -> file path (e.g., list("2024" = "results_2024.rds"))
#' @param format Format code (default "A")
#' @return Combined data.table of all picks
load_splash_results <- function(files, format = "A") {
  all_picks <- rbindlist(lapply(names(files), function(yr) {
    cat(sprintf("\nLoading %s results...\n", yr))
    dt <- readRDS(files[[yr]])
    parse_splash_results(dt, year = as.integer(yr), format = format)
  }), fill = TRUE)

  cat(sprintf("\nTotal: %d picks across %d years\n",
              nrow(all_picks), uniqueN(all_picks$year)))
  all_picks
}

# ==============================================================================
# EMPIRICAL OWNERSHIP FROM PICK HISTORIES
# ==============================================================================

#' Compute empirical ownership per slot from parsed pick data
#'
#' @param picks_dt data.table from parse_splash_results()
#' @param slot_id Optional: compute for a single slot. If NULL, compute all.
#' @return data.table: year, slot_id, team_alias, team_name, n_picks, ownership_pct
compute_empirical_ownership <- function(picks_dt, slot_id = NULL) {
  dt <- if (!is.null(slot_id)) picks_dt[slot_id == slot_id] else copy(picks_dt)

  # Count picks per team per slot per year
  # Use team_alias as primary key (shorter, more stable)
  team_col <- if (all(!is.na(dt$team_alias))) "team_alias" else "team_name"

  own <- dt[, .(
    n_picks = .N,
    team_name = team_name[1]  # keep for display
  ), by = c("year", "slot_id", team_col)]

  # Compute total entries per slot per year (for non-E8: n_picks = n_entries)
  slot_totals <- dt[, .(total_picks = .N), by = c("year", "slot_id")]

  # For E8 (2 picks per entry), total entries = total_picks / 2
  # Ownership should still sum to n_picks (2 for E8, 1 for others)
  own <- merge(own, slot_totals, by = c("year", "slot_id"))
  own[, ownership_pct := n_picks / total_picks]

  setorder(own, year, slot_id, -ownership_pct)

  cat(sprintf("Empirical ownership: %d team-slot-year combinations\n", nrow(own)))
  own
}

#' Print a comparison of empirical vs predicted ownership for a slot
print_ownership_comparison <- function(empirical_own, predicted_own, slot_id, year = NULL) {
  emp <- empirical_own[slot_id == slot_id]
  if (!is.null(year)) emp <- emp[year == year]

  # Average across years if multiple
  team_col <- if ("team_alias" %in% names(emp)) "team_alias" else "team_name"
  emp_avg <- emp[, .(emp_pct = mean(ownership_pct)), by = team_col]

  # Match predicted
  pred_names <- names(predicted_own)

  cat(sprintf("\n=== OWNERSHIP COMPARISON: %s ===\n", slot_id))
  cat(sprintf("%-20s %8s %8s %8s\n", "Team", "Emp%", "Pred%", "Delta"))
  cat(paste(rep("-", 50), collapse = ""), "\n")

  for (i in seq_len(nrow(emp_avg))) {
    tm <- emp_avg[[team_col]][i]
    ep <- emp_avg$emp_pct[i] * 100

    # Try to match predicted by alias or name
    pp <- 0
    if (tm %in% pred_names) {
      pp <- predicted_own[tm] * 100
    }

    cat(sprintf("%-20s %7.1f%% %7.1f%% %+7.1f%%\n", tm, ep, pp, pp - ep))
  }
  cat("\n")
}

# ==============================================================================
# NAME RESOLUTION: SPLASH PICKS ↔ SIM TEAMS
# ==============================================================================

#' Resolve Splash pick names to sim team names
#'
#' Splash API uses team_name ("McNeese", "St. John's") and team_alias ("MCNS", "SJU").
#' Sim teams use full names from KenPom ("McNeese State", "St. John's").
#' This builds a lookup table by trying: exact match, substring match, manual overrides.
#'
#' @param splash_names Character vector of unique Splash team_name values
#' @param sim_teams Data frame with name, seed, region, team_id columns
#' @return Named character vector: splash_name -> sim_name (NA if unmatched)
resolve_splash_to_sim <- function(splash_names, sim_teams) {
  sim_names <- sim_teams$name

  # Manual overrides for known mismatches
  overrides <- c(
    "McNeese"          = "McNeese State",
    "NC State"         = "NC State",
    "South Carolina"   = "South Carolina",
    "Mississippi State" = "Mississippi State",
    "Michigan State"   = "Michigan State",
    "Iowa State"       = "Iowa State",
    "Colorado State"   = "Colorado State",
    "Utah State"       = "Utah State",
    "Washington State" = "Washington State",
    "Saint Mary's"     = "Saint Mary's",
    "St. John's"       = "St. John's",
    "SIU Edwardsville" = "SIU Edwardsville",
    "UNC Wilmington"   = "UNC Wilmington",
    "UC San Diego"     = "UC San Diego",
    "High Point"       = "High Point",
    "Grand Canyon"     = "Grand Canyon",
    "Mount St. Mary's" = "Mount St. Mary's",
    "Morehead State"   = "Morehead State"
  )

  result <- setNames(rep(NA_character_, length(splash_names)), splash_names)

  for (sn in splash_names) {
    # 1. Check manual overrides
    if (sn %in% names(overrides) && overrides[sn] %in% sim_names) {
      result[sn] <- overrides[sn]
      next
    }
    # 2. Exact match
    if (sn %in% sim_names) {
      result[sn] <- sn
      next
    }
    # 3. Splash name is prefix of sim name (e.g., "McNeese" -> "McNeese State")
    prefix_match <- sim_names[startsWith(sim_names, sn)]
    if (length(prefix_match) == 1) {
      result[sn] <- prefix_match
      next
    }
    # 4. Sim name is prefix of splash name
    suffix_match <- sim_names[sapply(sim_names, function(x) startsWith(sn, x))]
    if (length(suffix_match) == 1) {
      result[sn] <- suffix_match
      next
    }
    # 5. Case-insensitive match
    ci_match <- sim_names[tolower(sim_names) == tolower(sn)]
    if (length(ci_match) == 1) {
      result[sn] <- ci_match
      next
    }
  }

  n_matched <- sum(!is.na(result))
  n_total <- length(splash_names)
  if (n_matched < n_total) {
    unmatched <- splash_names[is.na(result)]
    cat(sprintf("  Name resolution: %d/%d matched, unmatched: %s\n",
                n_matched, n_total, paste(unmatched, collapse = ", ")))
  }

  result
}

# ==============================================================================
# CALIBRATION DATASET BUILDER
# ==============================================================================

#' Build calibration dataset: join picks with sim-derived features per year
#'
#' For each year and slot, computes team-level features (win_prob, future_value,
#' seed, region, availability) and joins with empirical ownership from pick data.
#'
#' @param picks_dt data.table from load_splash_results()
#' @param sim_list Named list: year -> sim object (e.g., list("2024" = sim24, "2025" = sim25))
#' @param format Format code (default "A")
#' @return data.table with one row per team-slot-year
build_calibration_dataset <- function(picks_dt, sim_list, format = "A") {
  cat("Building calibration dataset...\n")

  slot_order <- get_slot_order(format)
  emp_own <- compute_empirical_ownership(picks_dt)

  all_rows <- list()

  for (yr_str in names(sim_list)) {
    yr <- as.integer(yr_str)
    sim <- sim_list[[yr_str]]
    sim_teams <- sim$teams

    cat(sprintf("\n  Processing %d (%d teams)...\n", yr, nrow(sim_teams)))

    # Build name mapping for this year
    yr_picks <- picks_dt[year == yr]
    splash_names <- unique(yr_picks$team_name)
    splash_names <- splash_names[!is.na(splash_names)]
    name_map <- resolve_splash_to_sim(splash_names, sim_teams)

    # Track cumulative field usage across slots (from empirical picks)
    field_used <- setNames(rep(0, nrow(sim_teams)), sim_teams$name)

    for (sid in slot_order) {
      slot <- tryCatch(get_slot(sid), error = function(e) NULL)
      if (is.null(slot)) next

      round_num <- slot$round_num
      game_idxs <- slot$game_indices

      # Get candidate teams from sim
      if (round_num == 1) {
        cids <- integer(0)
        for (g in game_idxs) {
          cids <- c(cids, sim_teams$team_id[2 * g - 1], sim_teams$team_id[2 * g])
        }
        candidates <- sim_teams[sim_teams$team_id %in% unique(cids), ]
      } else {
        candidates <- get_round_candidates(game_idxs, sim_teams, sim$all_results)
      }
      if (nrow(candidates) == 0) next

      # Compute features from sim
      wp <- compute_slot_win_probs(candidates, game_idxs, round_num,
                                    sim_teams, sim$all_results)
      fv <- compute_future_value(candidates, sim$all_results, round_num)

      # Availability: 1 - cumulative field usage
      avail <- sapply(candidates$name, function(nm) {
        max(1.0 - field_used[nm], 0.05)
      })

      # Get empirical ownership for this slot-year
      emp_slot <- emp_own[year == yr & slot_id == sid]

      # Build rows for each candidate
      for (i in seq_len(nrow(candidates))) {
        sim_name <- candidates$name[i]

        # Find matching splash name
        splash_match <- names(name_map)[name_map == sim_name & !is.na(name_map)]

        # Look up empirical ownership
        emp_pct <- 0
        n_picks_i <- 0
        if (length(splash_match) > 0) {
          for (sm in splash_match) {
            emp_row <- emp_slot[team_name == sm]
            if (nrow(emp_row) > 0) {
              emp_pct <- emp_row$ownership_pct[1]
              n_picks_i <- emp_row$n_picks[1]
              break
            }
          }
        }

        all_rows[[length(all_rows) + 1]] <- data.table(
          year         = yr,
          slot_id      = sid,
          round_num    = round_num,
          sim_name     = sim_name,
          team_id      = candidates$team_id[i],
          seed         = candidates$seed[i],
          region       = candidates$region[i],
          wp           = wp[i],
          fv           = fv[i],
          avail        = avail[i],
          field_used   = field_used[sim_name],
          empirical_own = emp_pct,
          n_picks      = n_picks_i
        )
      }

      # Update cumulative field usage from empirical ownership
      for (j in seq_len(nrow(emp_slot))) {
        splash_nm <- emp_slot$team_name[j]
        sim_nm <- name_map[splash_nm]
        if (!is.na(sim_nm) && sim_nm %in% names(field_used)) {
          field_used[sim_nm] <- min(field_used[sim_nm] + emp_slot$ownership_pct[j], 1.0)
        }
      }
    }
  }

  calib_dt <- rbindlist(all_rows)
  cat(sprintf("\nCalibration dataset: %d rows (%d team-slot-year observations)\n",
              nrow(calib_dt), sum(calib_dt$n_picks > 0)))
  calib_dt
}

# ==============================================================================
# BEHAVIORAL ANALYSIS FROM PICK HISTORIES
# ==============================================================================

#' Analyze field behavior from full pick histories
#'
#' Runs 5 behavioral analyses on the calibration dataset:
#' 1. Win-prob → ownership curve per round (chalk sensitivity)
#' 2. Save effect quantification (future value discount)
#' 3. Availability effect (prior usage suppression)
#' 4. Regional diversification / picking-against-self
#' 5. Seed-based behavior (residual seed preference)
#'
#' @param picks_dt data.table from load_splash_results()
#' @param calib_dt data.table from build_calibration_dataset() (NULL to skip sim-based analyses)
#' @return List with behavioral metrics
analyze_field_behavior <- function(picks_dt, calib_dt = NULL) {
  cat("\n========================================\n")
  cat("FIELD BEHAVIOR ANALYSIS\n")
  cat("========================================\n")
  results <- list()

  # --- 1. Win-prob → ownership curve per round ---
  if (!is.null(calib_dt)) {
    cat("\n--- 1. Win-Prob → Ownership (Chalk Sensitivity) ---\n")
    picked <- calib_dt[n_picks > 0]  # only teams that were actually picked

    for (rnd in sort(unique(picked$round_num))) {
      rnd_data <- picked[round_num == rnd & wp > 0 & empirical_own > 0]
      if (nrow(rnd_data) < 3) next

      # Fit log(own) ~ wp to get chalk sensitivity
      fit <- tryCatch(lm(log(empirical_own) ~ wp, data = rnd_data), error = function(e) NULL)
      if (!is.null(fit)) {
        beta <- coef(fit)[2]
        r2 <- summary(fit)$r.squared
        rnd_label <- c("R64", "R32", "S16", "E8", "FF", "CHAMP")[rnd]
        results[[paste0("beta_wp_round_", rnd)]] <- beta
        cat(sprintf("  %s: beta_wp ≈ %.2f (R² = %.3f, n = %d)\n",
                    rnd_label, beta, r2, nrow(rnd_data)))
      }
    }
  }

  # --- 2. Save effect quantification ---
  if (!is.null(calib_dt)) {
    cat("\n--- 2. Save Effect (Future Value Discount) ---\n")
    early <- calib_dt[round_num <= 2 & wp > 0 & empirical_own > 0]

    if (nrow(early) >= 5) {
      # Residual after controlling for wp
      fit_wp <- lm(log(empirical_own) ~ wp, data = early)
      early[, own_resid := residuals(fit_wp)]

      fit_save <- lm(own_resid ~ fv, data = early)
      save_coef <- coef(fit_save)[2]
      results$save_effect_coef <- save_coef
      cat(sprintf("  FV coefficient in R64/R32: %.3f (negative = save effect confirmed)\n",
                  save_coef))

      # Show 1-seeds specifically
      seeds_1 <- early[seed == 1]
      if (nrow(seeds_1) > 0) {
        cat("  1-seed ownership in early rounds:\n")
        for (i in seq_len(nrow(seeds_1))) {
          r <- seeds_1[i]
          cat(sprintf("    %d %s %-20s: wp=%.1f%%, own=%.1f%%, fv=%.3f\n",
                      r$year, r$slot_id, r$sim_name,
                      100 * r$wp, 100 * r$empirical_own, r$fv))
        }
      }
    }
  }

  # --- 3. Availability effect ---
  if (!is.null(calib_dt)) {
    cat("\n--- 3. Availability Effect ---\n")
    # Only slots where some teams have nonzero field_used
    avail_data <- calib_dt[field_used > 0.01 & wp > 0 & empirical_own > 0]

    if (nrow(avail_data) >= 5) {
      fit_avail <- lm(log(empirical_own) ~ wp + fv + log(avail),
                       data = avail_data)
      avail_coef <- coef(fit_avail)["log(avail)"]
      results$avail_effect_coef <- avail_coef
      cat(sprintf("  Availability coef (controlling for wp, fv): %.3f\n", avail_coef))
      cat(sprintf("  (positive = higher availability → more picks, n = %d)\n",
                  nrow(avail_data)))
    } else {
      cat("  Insufficient data with nonzero field_used for regression\n")
    }
  }

  # --- 4. Regional diversification / picking-against-self ---
  cat("\n--- 4. Regional Diversification ---\n")
  if (!is.null(calib_dt)) {
    # Merge region info into picks
    name_to_region <- setNames(calib_dt$region, calib_dt$sim_name)
    name_to_region <- name_to_region[!duplicated(names(name_to_region))]
  }

  # Use picks data directly — track regions per entry
  slot_order_a <- c("R1_d1", "R1_d2", "R2_d1", "R2_d2",
                     "S16_d1", "S16_d2", "E8", "FF", "CHAMP")

  entry_wide <- dcast(picks_dt, entry_id + year ~ slot_id,
                       value.var = "team_name", fun.aggregate = function(x) x[1])

  early_slots <- c("R1_d1", "R1_d2", "R2_d1", "R2_d2")
  available_early <- intersect(early_slots, names(entry_wide))

  if (length(available_early) >= 2 && !is.null(calib_dt)) {
    # For each entry, count distinct regions in early rounds
    region_counts <- sapply(seq_len(nrow(entry_wide)), function(i) {
      picks <- unlist(entry_wide[i, ..available_early])
      picks <- picks[!is.na(picks)]
      # Map splash names to sim names, then to regions
      regions <- character(0)
      for (p in picks) {
        # Try direct region lookup
        if (p %in% names(name_to_region)) {
          regions <- c(regions, name_to_region[p])
        }
      }
      length(unique(regions))
    })

    valid <- region_counts > 0
    if (sum(valid) > 0) {
      mean_regions <- mean(region_counts[valid])
      results$mean_distinct_regions_early <- mean_regions
      cat(sprintf("  Mean distinct regions in R1+R2: %.2f (max possible: %d)\n",
                  mean_regions, length(available_early)))

      # Distribution
      tbl <- table(region_counts[valid])
      for (n in sort(as.integer(names(tbl)))) {
        cat(sprintf("    %d regions: %d entries (%.1f%%)\n",
                    n, tbl[as.character(n)],
                    100 * tbl[as.character(n)] / sum(valid)))
      }
    }
  }

  # --- 5. Seed-based behavior ---
  if (!is.null(calib_dt)) {
    cat("\n--- 5. Seed Effect (Residual After wp) ---\n")
    r1_data <- calib_dt[round_num == 1 & wp > 0 & empirical_own > 0]

    if (nrow(r1_data) >= 5) {
      # Fit ownership with both wp and seed
      fit_full <- lm(log(empirical_own) ~ wp + seed, data = r1_data)
      seed_coef <- coef(fit_full)["seed"]
      results$seed_coef_r1 <- seed_coef
      cat(sprintf("  R64 seed coefficient (controlling for wp): %.3f\n", seed_coef))
      cat("  (negative = higher seeds get less ownership beyond what wp explains)\n")

      # Seed group analysis
      r1_data[, seed_group := ifelse(seed <= 4, "1-4",
                               ifelse(seed <= 8, "5-8",
                               ifelse(seed <= 12, "9-12", "13-16")))]
      seed_summary <- r1_data[, .(
        mean_wp = mean(wp),
        mean_own = mean(empirical_own),
        ratio = mean(empirical_own) / mean(wp)
      ), by = seed_group]
      setorder(seed_summary, seed_group)
      cat("  Ownership/WinProb ratio by seed group (>1 = over-picked):\n")
      for (i in seq_len(nrow(seed_summary))) {
        r <- seed_summary[i]
        cat(sprintf("    Seeds %5s: wp=%.3f, own=%.3f, ratio=%.2f\n",
                    r$seed_group, r$mean_wp, r$mean_own, r$ratio))
      }
    }
  }

  # --- Reuse and elimination checks ---
  cat("\n--- Data Quality Checks ---\n")
  reuse_check <- picks_dt[, .(n_uses = .N), by = .(entry_id, year, team_alias)]
  n_reuse <- sum(reuse_check$n_uses > 1)
  if (n_reuse > 0) {
    cat(sprintf("  WARNING: %d entries reused a team (should be 0)\n", n_reuse))
  } else {
    cat("  No team reuse detected (good)\n")
  }

  # Elimination curve
  if ("won" %in% names(picks_dt)) {
    for (yr in sort(unique(picks_dt$year))) {
      yr_picks <- picks_dt[year == yr]
      total <- uniqueN(yr_picks$entry_id)
      cat(sprintf("  %d elimination curve (%d entries):\n", yr, total))

      alive_ids <- unique(yr_picks$entry_id)
      for (sid in slot_order_a) {
        slot_picks <- yr_picks[slot_id == sid & entry_id %in% alive_ids]
        if (nrow(slot_picks) == 0) break
        n_alive_before <- length(alive_ids)
        died <- slot_picks[won == FALSE, unique(entry_id)]
        alive_ids <- setdiff(alive_ids, died)
        cat(sprintf("    %-8s: %5d alive -> %5d (%4.1f%% survival, %4.1f%% of field)\n",
                    sid, n_alive_before, length(alive_ids),
                    100 * length(alive_ids) / n_alive_before,
                    100 * length(alive_ids) / total))
      }
    }
  }

  cat("\n========================================\n")
  results
}

# ==============================================================================
# OWNERSHIP MODEL FITTING (MLE CALIBRATION)
# ==============================================================================

#' Fit ownership model parameters via maximum likelihood
#'
#' Model: log_attract = beta_wp[r] * wp - save[r] * fv + beta_avail * log(avail)
#' Predicted ownership = softmax(log_attract) within each slot.
#' Loss = multinomial negative log-likelihood across all slot-year combos.
#'
#' @param calib_dt data.table from build_calibration_dataset()
#' @param n_restarts Number of random restarts (default 5)
#' @return List with fitted params, loss, convergence info
fit_ownership_model <- function(calib_dt, n_restarts = 5) {
  cat("\nFitting ownership model via MLE...\n")

  # Only use observations where team was actually a candidate
  # (wp > 0 means team could appear in this slot)
  dt <- calib_dt[wp > 0]

  # Unique slot-year combos
  slot_years <- unique(dt[, .(year, slot_id, round_num)])
  cat(sprintf("  Fitting against %d slot-year combinations\n", nrow(slot_years)))

  # Round groupings: R1(1), R2(2), S16(3), E8+(4,5,6)
  round_to_group <- c(1, 2, 3, 4, 4, 4)

  # Parameter layout: [beta_wp_R1, beta_wp_R2, beta_wp_S16, beta_wp_E8+,
  #                     save_R1, save_R2, save_S16,
  #                     beta_avail]
  n_par <- 8

  loss_fn <- function(par) {
    beta_wp <- par[1:4]   # per round group
    save_str <- c(par[5], par[6], par[7], 0, 0, 0)  # per round (E8+ = 0)
    beta_avail <- par[8]

    total_nll <- 0

    for (i in seq_len(nrow(slot_years))) {
      sy <- slot_years[i]
      sub <- dt[year == sy$year & slot_id == sy$slot_id]
      if (nrow(sub) == 0) next

      rnd <- sy$round_num
      rg <- round_to_group[rnd]

      log_attract <- beta_wp[rg] * sub$wp -
                     save_str[rnd] * sub$fv +
                     beta_avail * log(sub$avail)
      log_attract <- log_attract - max(log_attract)

      log_pred <- log_attract - log(sum(exp(log_attract)))

      # Total picks in this slot-year
      total_picks <- sum(sub$n_picks)
      if (total_picks == 0) next

      # NLL contribution: -sum(n_picks_i * log(pred_i))
      # Add small epsilon to avoid log(0)
      nll <- -sum(sub$n_picks * pmax(log_pred, -30))
      total_nll <- total_nll + nll
    }

    total_nll
  }

  # Box constraints
  lower <- c(rep(0.5, 4), rep(0.0, 3), 0.0)
  upper <- c(rep(15.0, 4), rep(3.0, 3), 5.0)

  best_fit <- NULL
  best_val <- Inf

  for (restart in seq_len(n_restarts)) {
    if (restart == 1) {
      # First restart: use reasonable initial values
      init <- c(4.0, 4.0, 4.0, 4.0, 0.6, 0.5, 0.35, 1.0)
    } else {
      # Random restarts
      init <- runif(n_par, lower, upper)
    }

    fit <- tryCatch(
      optim(init, loss_fn, method = "L-BFGS-B",
            lower = lower, upper = upper,
            control = list(maxit = 5000)),
      error = function(e) NULL
    )

    if (!is.null(fit) && fit$value < best_val) {
      best_val <- fit$value
      best_fit <- fit
    }
  }

  if (is.null(best_fit)) {
    stop("All optimization restarts failed")
  }

  par <- best_fit$par
  result <- list(
    beta_wp = setNames(par[1:4], c("R1", "R2", "S16", "E8plus")),
    save_strengths = c(par[5], par[6], par[7], 0, 0, 0),
    beta_avail = par[8],
    nll = best_fit$value,
    converged = best_fit$convergence == 0
  )

  cat(sprintf("  beta_wp:    R1=%.2f  R2=%.2f  S16=%.2f  E8+=%.2f\n",
              result$beta_wp[1], result$beta_wp[2],
              result$beta_wp[3], result$beta_wp[4]))
  cat(sprintf("  save:       R1=%.3f  R2=%.3f  S16=%.3f\n",
              result$save_strengths[1], result$save_strengths[2],
              result$save_strengths[3]))
  cat(sprintf("  beta_avail: %.3f\n", result$beta_avail))
  cat(sprintf("  NLL: %.1f  (converged: %s)\n", result$nll, result$converged))

  result
}

#' Cross-validate ownership model (leave-one-year-out)
#'
#' @param calib_dt data.table from build_calibration_dataset()
#' @return List with per-year held-out metrics
cross_validate_ownership <- function(calib_dt) {
  cat("\nCross-validating ownership model...\n")
  years <- sort(unique(calib_dt$year))
  cv_results <- list()

  for (held_out in years) {
    train_dt <- calib_dt[year != held_out]
    test_dt <- calib_dt[year == held_out]

    cat(sprintf("\n  Hold out %d, train on %s:\n",
                held_out, paste(setdiff(years, held_out), collapse = ", ")))

    fit <- tryCatch(
      fit_ownership_model(train_dt, n_restarts = 3),
      error = function(e) { cat("    FAILED:", e$message, "\n"); NULL }
    )
    if (is.null(fit)) next

    # Predict on held-out year
    round_to_group <- c(1, 2, 3, 4, 4, 4)
    test_slots <- unique(test_dt[, .(slot_id, round_num)])
    total_mae <- 0
    n_obs <- 0

    for (i in seq_len(nrow(test_slots))) {
      ts <- test_slots[i]
      sub <- test_dt[slot_id == ts$slot_id & wp > 0]
      if (nrow(sub) == 0) next

      rnd <- ts$round_num
      rg <- round_to_group[rnd]
      save_str <- c(fit$save_strengths[1:3], 0, 0, 0)

      log_attract <- fit$beta_wp[rg] * sub$wp -
                     save_str[rnd] * sub$fv +
                     fit$beta_avail * log(sub$avail)
      log_attract <- log_attract - max(log_attract)
      pred <- exp(log_attract) / sum(exp(log_attract))

      mae <- mean(abs(pred - sub$empirical_own))
      total_mae <- total_mae + mae * nrow(sub)
      n_obs <- n_obs + nrow(sub)
    }

    avg_mae <- if (n_obs > 0) total_mae / n_obs else NA
    cat(sprintf("    Held-out MAE: %.4f (%.1f pp avg error)\n",
                avg_mae, 100 * avg_mae))
    cv_results[[as.character(held_out)]] <- list(fit = fit, mae = avg_mae)
  }

  cv_results
}

# ==============================================================================
# CALIBRATION PIPELINE (MAIN ENTRY POINT)
# ==============================================================================

#' Calibrate ownership model from pick histories and per-year sims
#'
#' @param pick_files Named list: year -> file path (e.g., list("2024" = "results_2024.rds"))
#' @param sim_files Named list: year -> file path (e.g., list("2024" = "sim_results_2024.rds"))
#' @param output_file File path to save calibrated params (default "splash_calibration.rds")
#' @param cross_validate Run leave-one-year-out CV (default TRUE)
#' @return List with: params, calib_dt, analyses, cv_results
calibrate_from_picks <- function(pick_files, sim_files,
                                  output_file = "splash_calibration.rds",
                                  cross_validate = TRUE) {
  cat("=== SPLASH OWNERSHIP CALIBRATION ===\n")

  # 1. Load picks
  picks_dt <- load_splash_results(pick_files)

  # 2. Load sims
  cat("\nLoading sim results...\n")
  sim_list <- list()
  for (yr in names(sim_files)) {
    cat(sprintf("  Loading %s sims...\n", yr))
    sim_list[[yr]] <- readRDS(sim_files[[yr]])
    cat(sprintf("    %d sims, %d teams\n",
                sim_list[[yr]]$n_sims, nrow(sim_list[[yr]]$teams)))
  }

  # 3. Build calibration dataset
  calib_dt <- build_calibration_dataset(picks_dt, sim_list)

  # 4. Run behavioral analyses
  analyses <- analyze_field_behavior(picks_dt, calib_dt)

  # 5. Fit model
  params <- fit_ownership_model(calib_dt)

  # 6. Cross-validate
  cv_results <- NULL
  if (cross_validate && length(sim_list) > 1) {
    cv_results <- cross_validate_ownership(calib_dt)
  }

  # 7. Save
  save_calibrated_params(params, output_file)

  list(
    params = params,
    calib_dt = calib_dt,
    analyses = analyses,
    cv_results = cv_results
  )
}

#' Save calibrated parameters to file
save_calibrated_params <- function(params, file = "splash_calibration.rds") {
  saveRDS(params, file)
  cat(sprintf("\nCalibrated params saved to: %s\n", file))
}

#' Load calibrated parameters from file (falls back to defaults if missing)
load_calibrated_params <- function(file = "splash_calibration.rds") {
  if (file.exists(file)) {
    params <- readRDS(file)
    cat(sprintf("Loaded calibrated params from: %s\n", file))
    return(params)
  }

  cat("No calibration file found, using defaults\n")
  list(
    beta_wp = c(R1 = 4.0, R2 = 4.0, S16 = 4.0, E8plus = 4.0),
    save_strengths = c(0.60, 0.50, 0.35, 0.15, 0.0, 0.0),
    beta_avail = 1.0
  )
}

cat("Splash ownership module loaded\n")
