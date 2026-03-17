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
                                prior_ownership = NULL, field_used_dist = NULL) {
  slot <- get_slot(slot_id)
  game_idxs <- slot$game_indices
  n_picks <- slot$n_picks
  round_num <- slot$round_num

  # Identify teams playing in this slot
  # For R64, we know the exact matchups from the bracket
  if (round_num == 1) {
    candidate_ids <- integer(0)
    for (g in game_idxs) {
      candidate_ids <- c(candidate_ids,
                         teams_dt$team_id[2 * g - 1],
                         teams_dt$team_id[2 * g])
    }
    candidates <- teams_dt[teams_dt$team_id %in% candidate_ids, ]
  } else {
    # For later rounds, candidates are teams that won their prior game
    # in at least some sims. Use sim data to identify them.
    candidates <- get_round_candidates(game_idxs, teams_dt, sim_matrix)
  }

  if (nrow(candidates) == 0) {
    warning("No candidates found for slot ", slot_id)
    return(numeric(0))
  }

  # --- Factor 1: Win probability in this round's game ---
  wp <- compute_slot_win_probs(candidates, game_idxs, round_num,
                                teams_dt, sim_matrix)

  # --- Factor 2: Future value (save effect) ---
  # Teams with high championship equity get saved for later rounds
  # Stronger effect in early rounds, weaker later
  fv <- compute_future_value(candidates, sim_matrix, round_num)

  # Save strength: how much future value suppresses current picks
  # With 1 pick per day (vs 3 in Hodes), save effect is even stronger
  # because you have fewer "slots" to use a team
  save_strength <- c(0.60, 0.50, 0.35, 0.15, 0.0, 0.0)[round_num]

  # --- Factor 3: Availability ---
  # Estimate what fraction of the field has already used each candidate
  avail <- rep(1.0, nrow(candidates))
  if (!is.null(field_used_dist)) {
    for (i in seq_len(nrow(candidates))) {
      used_frac <- field_used_dist[[candidates$name[i]]]
      if (!is.null(used_frac)) {
        avail[i] <- max(1.0 - used_frac, 0.05)
      }
    }
  }

  # --- Combine into attractiveness score ---
  # Softmax over: log(attract) = β_wp * wp - save_strength * fv + log(avail)
  # No seed tiebreaker boost (Splash has no seed-sum TB)

  # Concentration parameter: with only 1 pick, people concentrate more on chalk
  # Higher beta = more concentrated on high-wp teams
  beta_wp <- 4.0  # starting estimate; calibrate when Splash data arrives

  log_attract <- beta_wp * wp - save_strength * fv + log(avail)
  log_attract <- log_attract - max(log_attract)  # numerical stability

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

# Day-to-slot mapping for Format A
# day1=R1_d1, day2=R1_d2, day3=R2_d1, day4=R2_d2,
# day5=S16_d1, day6=S16_d2, day7_8=E8 (2 picks, nested df), day9=FF, day10=CHAMP
FORMAT_A_DAY_MAP <- list(
  list(col = "day1", slot = "R1_d1",  nested = FALSE),
  list(col = "day2", slot = "R1_d2",  nested = FALSE),
  list(col = "day3", slot = "R2_d1",  nested = FALSE),
  list(col = "day4", slot = "R2_d2",  nested = FALSE),
  list(col = "day5", slot = "S16_d1", nested = FALSE),
  list(col = "day6", slot = "S16_d2", nested = FALSE),
  list(col = "day7_8", slot = "E8",   nested = TRUE),
  list(col = "day9", slot = "FF",     nested = FALSE),
  list(col = "day10", slot = "CHAMP", nested = FALSE)
)

#' Parse raw Splash results RDS into a long-form picks table
#'
#' Takes the raw data.table from Splash API (one row per entry, with day1-day10
#' columns including nested data.frames for E8) and returns a clean long-format
#' table of all picks.
#'
#' @param results_dt data.table loaded from results_YYYY.rds
#' @param year Integer year (2024, 2025, etc.)
#' @param format Character format code (default "A")
#' @return data.table with columns: year, entry_id, slot_id, team_alias, team_name, won
parse_splash_results <- function(results_dt, year, format = "A") {
  if (format != "A") stop("Only Format A parsing is implemented so far")

  day_map <- FORMAT_A_DAY_MAP
  n_entries <- nrow(results_dt)
  picks_list <- vector("list", length(day_map))

  for (dm_idx in seq_along(day_map)) {
    dm <- day_map[[dm_idx]]
    slot_id <- dm$slot

    if (!dm$nested) {
      # Simple columns: day{X}_teamAlias, day{X}_teamName, day{X}_winning
      alias_col <- paste0(dm$col, "_teamAlias")
      name_col <- paste0(dm$col, "_teamName")
      win_col <- paste0(dm$col, "_winning")

      # Check which columns actually exist
      has_alias <- alias_col %in% names(results_dt)
      has_name <- name_col %in% names(results_dt)
      has_win <- win_col %in% names(results_dt)

      picks_list[[dm_idx]] <- data.table(
        year     = year,
        entry_id = if ("entryId" %in% names(results_dt)) results_dt$entryId else seq_len(n_entries),
        slot_id  = slot_id,
        team_alias = if (has_alias) as.character(results_dt[[alias_col]]) else NA_character_,
        team_name  = if (has_name) as.character(results_dt[[name_col]]) else NA_character_,
        won        = if (has_win) as.logical(results_dt[[win_col]]) else NA
      )
    } else {
      # Nested column (day7_8): each cell is a data.frame with 2 rows
      # Extract team_alias and team_name from the nested data.frames
      nested_col <- dm$col
      if (!(nested_col %in% names(results_dt))) {
        warning("Nested column '", nested_col, "' not found, skipping")
        picks_list[[dm_idx]] <- data.table(
          year = integer(0), entry_id = character(0), slot_id = character(0),
          team_alias = character(0), team_name = character(0), won = logical(0)
        )
        next
      }

      # Each row's nested df has columns like teamAlias, teamName, winning
      nested_picks <- rbindlist(lapply(seq_len(n_entries), function(i) {
        nested_df <- results_dt[[nested_col]][[i]]
        if (is.null(nested_df) || !is.data.frame(nested_df) || nrow(nested_df) == 0) {
          return(data.table(
            year = year,
            entry_id = if ("entryId" %in% names(results_dt)) results_dt$entryId[i] else i,
            slot_id = slot_id,
            team_alias = NA_character_,
            team_name = NA_character_,
            won = NA
          ))
        }
        # Try to find the right column names (may vary)
        alias_candidates <- intersect(names(nested_df), c("teamAlias", "team_alias"))
        name_candidates <- intersect(names(nested_df), c("teamName", "team_name"))
        win_candidates <- intersect(names(nested_df), c("winning", "won"))

        data.table(
          year = year,
          entry_id = if ("entryId" %in% names(results_dt)) results_dt$entryId[i] else i,
          slot_id = slot_id,
          team_alias = if (length(alias_candidates)) as.character(nested_df[[alias_candidates[1]]]) else NA_character_,
          team_name = if (length(name_candidates)) as.character(nested_df[[name_candidates[1]]]) else NA_character_,
          won = if (length(win_candidates)) as.logical(nested_df[[win_candidates[1]]]) else NA
        )
      }), fill = TRUE)

      picks_list[[dm_idx]] <- nested_picks
    }
  }

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
# BEHAVIORAL ANALYSIS FROM PICK HISTORIES
# ==============================================================================

#' Analyze field behavior from full pick histories
#'
#' Computes key behavioral metrics that inform the ownership model:
#' 1. Pick-vs-win-prob curve: how strongly does the field chase chalk?
#' 2. Save effect: do people avoid high-FV teams in early rounds?
#' 3. Anti-self behavior: how often do entries pick against their own future bracket?
#'
#' @param picks_dt data.table from parse_splash_results()
#' @param teams_dt Teams data frame (with name/seed for matching)
#' @param sim Sim list (for computing win probs + future values per year)
#' @return List with behavioral metrics and fitted params
analyze_field_behavior <- function(picks_dt, teams_dt, sim = NULL) {
  cat("Analyzing field behavior from pick histories...\n")
  results <- list()

  # --- 1. Pick concentration by seed ---
  # How much do people favor favorites?
  if ("team_alias" %in% names(picks_dt)) {
    # Match picks to teams_dt by alias
    pick_seeds <- merge(
      picks_dt, teams_dt[, .(name, seed)],
      by.x = "team_name", by.y = "name", all.x = TRUE
    )
  }

  if ("seed" %in% names(pick_seeds)) {
    seed_own <- pick_seeds[, .N, by = .(slot_id, seed, year)]
    slot_totals <- pick_seeds[, .(total = .N), by = .(slot_id, year)]
    seed_own <- merge(seed_own, slot_totals, by = c("slot_id", "year"))
    seed_own[, pct := N / total]

    # For R64 slots, seeds 1-4 should dominate
    r1_seeds <- seed_own[slot_id %in% c("R1_d1", "R1_d2")]
    if (nrow(r1_seeds) > 0) {
      chalk_rate <- r1_seeds[seed <= 4, sum(N)] / r1_seeds[, sum(N)]
      results$r1_chalk_rate <- chalk_rate
      cat(sprintf("  R64 chalk rate (seeds 1-4): %.1f%%\n", 100 * chalk_rate))
    }
  }

  # --- 2. Pick transition analysis ---
  # How do entries' picks evolve across rounds?
  # Convert to wide format: one row per entry, one column per slot
  entry_wide <- dcast(picks_dt, entry_id + year ~ slot_id,
                       value.var = "team_alias", fun.aggregate = function(x) x[1])

  results$n_entries_by_year <- picks_dt[, uniqueN(entry_id), by = year]

  # --- 3. Reuse analysis ---
  # This shouldn't happen (rules forbid it) but good to verify
  reuse_check <- picks_dt[, .(n_uses = .N), by = .(entry_id, year, team_alias)]
  n_reuse <- sum(reuse_check$n_uses > 1)
  if (n_reuse > 0) {
    cat(sprintf("  WARNING: %d entries reused a team (should be 0)\n", n_reuse))
  } else {
    cat("  No team reuse detected (good)\n")
  }

  # --- 4. Elimination curve ---
  # What fraction of entries are alive after each slot?
  if ("won" %in% names(picks_dt)) {
    slot_order_a <- c("R1_d1", "R1_d2", "R2_d1", "R2_d2",
                       "S16_d1", "S16_d2", "E8", "FF", "CHAMP")

    for (yr in unique(picks_dt$year)) {
      yr_picks <- picks_dt[year == yr]
      total <- uniqueN(yr_picks$entry_id)
      cat(sprintf("  %d elimination curve (%d entries):\n", yr, total))

      alive_ids <- unique(yr_picks$entry_id)
      for (sid in slot_order_a) {
        slot_picks <- yr_picks[slot_id == sid & entry_id %in% alive_ids]
        if (nrow(slot_picks) == 0) {
          cat(sprintf("    %-8s: no picks (all eliminated)\n", sid))
          break
        }
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

  results
}

#' Calibrate ownership model from full pick histories
#'
#' Fits beta_wp and save_strength parameters by minimizing MSE between
#' predicted and empirical ownership across all slots and years.
#'
#' @param picks_dt data.table from parse_splash_results()
#' @param teams_dt Teams data frame
#' @param sim_matrix Sim results matrix
#' @param round_info Round info from sim output
#' @return Named list of fitted parameters: beta_wp, save_strengths, loss
calibrate_from_picks <- function(picks_dt, teams_dt, sim_matrix, round_info) {
  cat("Calibrating ownership model from pick histories...\n")

  # Compute empirical ownership
  emp_own <- compute_empirical_ownership(picks_dt)

  # Get unique slots to fit against
  slots_to_fit <- unique(emp_own$slot_id)
  cat(sprintf("  Fitting against %d slots\n", length(slots_to_fit)))

  loss_fn <- function(par) {
    beta_wp <- par[1]
    save_str <- c(par[2], par[3], par[4], par[5], 0, 0)

    total_err <- 0
    n_obs <- 0

    for (sid in slots_to_fit) {
      slot <- tryCatch(get_slot(sid), error = function(e) NULL)
      if (is.null(slot)) next

      round_num <- slot$round_num
      game_idxs <- slot$game_indices

      # Get candidates
      candidates <- if (round_num == 1) {
        cids <- integer(0)
        for (g in game_idxs) cids <- c(cids, teams_dt$team_id[2*g-1], teams_dt$team_id[2*g])
        teams_dt[teams_dt$team_id %in% unique(cids), ]
      } else {
        get_round_candidates(game_idxs, teams_dt, sim_matrix)
      }
      if (nrow(candidates) == 0) next

      # Predicted ownership with trial params
      wp <- compute_slot_win_probs(candidates, game_idxs, round_num, teams_dt, sim_matrix)
      fv <- compute_future_value(candidates, sim_matrix, round_num)

      log_attract <- beta_wp * wp - save_str[round_num] * fv
      log_attract <- log_attract - max(log_attract)
      pred <- exp(log_attract) / sum(exp(log_attract))
      names(pred) <- candidates$name

      # Compare against empirical (average across years)
      emp_slot <- emp_own[slot_id == sid, .(pct = mean(ownership_pct)), by = team_name]

      # Match by name
      for (j in seq_len(nrow(emp_slot))) {
        tm <- emp_slot$team_name[j]
        actual_pct <- emp_slot$pct[j]
        pred_pct <- if (tm %in% names(pred)) pred[tm] else 0
        total_err <- total_err + (pred_pct - actual_pct)^2
        n_obs <- n_obs + 1
      }
    }

    if (n_obs == 0) return(1e6)
    total_err / n_obs
  }

  fit <- optim(c(4.0, 0.6, 0.5, 0.35, 0.15), loss_fn,
               method = "Nelder-Mead", control = list(maxit = 5000))

  result <- list(
    beta_wp = fit$par[1],
    save_strengths = c(fit$par[2], fit$par[3], fit$par[4], fit$par[5], 0, 0),
    loss = fit$value,
    converged = fit$convergence == 0
  )

  cat(sprintf("  Fitted: beta_wp=%.3f, save=[%.3f, %.3f, %.3f, %.3f]\n",
              result$beta_wp, result$save_strengths[1], result$save_strengths[2],
              result$save_strengths[3], result$save_strengths[4]))
  cat(sprintf("  MSE: %.8f  (converged: %s)\n", result$loss, result$converged))

  result
}

cat("Splash ownership module loaded\n")
