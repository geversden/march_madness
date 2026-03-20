#!/usr/bin/env Rscript
# ==============================================================================
# hodes_config.R
# Slot definitions and state management for the Hodes NCAA survivor contest.
#
# FORMAT (3-3-1+opt-1-1-1):
#   R1: Pick 3 teams from Round of 64 (no day split, no constraint between R1 picks)
#   R2: Pick 3 teams from Round of 32 (pod constraint: can't pick 2 from same 4-team pod)
#   S16: Pick 1 mandatory Sweet 16 team
#   S16_opt: CONDITIONAL - pick 1 optional Sweet 16 team IF entry survived R1+R2 weekend
#   E8: Pick 1 Elite 8 team
#   FF: Pick 1 Final Four team
#   CHAMP: Pick 1 Championship team
#   Total: 10 base picks + optional 11th
#
# SURVIVOR / WINNER-TAKE-ALL:
#   - Any wrong pick in a round = eliminated
#   - S16_opt failure does NOT eliminate (only affects tiebreaker)
#   - Last person standing wins
#
# TIEBREAKERS (among entries eliminated in same round, or both perfect):
#   TB1: Successfully picking S16_opt beats any entry without a successful S16_opt
#   TB2: Higher seed sum wins (larger seeds = lower seeded teams = more rewarded)
#
# Usage:
#   source("R/hodes_config.R")
# ==============================================================================

library(data.table)

# ==============================================================================
# SLOT DEFINITIONS
# ==============================================================================

HODES_SLOTS <- list(
  R1 = list(
    slot_id      = "R1",
    round_num    = 1L,
    n_picks      = 3L,
    is_conditional = FALSE,
    label        = "Round 1 (R64) — pick 3 teams"
  ),
  R2 = list(
    slot_id      = "R2",
    round_num    = 2L,
    n_picks      = 3L,
    is_conditional = FALSE,
    label        = "Round 2 (R32) — pick 3 teams (pod constraint)"
  ),
  S16 = list(
    slot_id      = "S16",
    round_num    = 3L,
    n_picks      = 1L,
    is_conditional = FALSE,
    label        = "Sweet 16 — 1 mandatory pick"
  ),
  S16_opt = list(
    slot_id      = "S16_opt",
    round_num    = 3L,
    n_picks      = 1L,
    is_conditional = TRUE,
    label        = "Sweet 16 optional (TB1 tiebreaker) — only if survived R1+R2"
  ),
  E8 = list(
    slot_id      = "E8",
    round_num    = 4L,
    n_picks      = 1L,
    is_conditional = FALSE,
    label        = "Elite 8 — 1 pick"
  ),
  FF = list(
    slot_id      = "FF",
    round_num    = 5L,
    n_picks      = 1L,
    is_conditional = FALSE,
    label        = "Final Four — 1 pick"
  ),
  CHAMP = list(
    slot_id      = "CHAMP",
    round_num    = 6L,
    n_picks      = 1L,
    is_conditional = FALSE,
    label        = "Championship — 1 pick"
  )
)

# Optimizer slot order: S16_opt is handled within the S16 beam step, not separately
HODES_SLOT_ORDER <- c("R1", "R2", "S16", "E8", "FF", "CHAMP")

# Map round number -> slot_id (for optimizer internal use)
HODES_ROUND_TO_SLOT <- c("1" = "R1", "2" = "R2", "3" = "S16",
                          "4" = "E8", "5" = "FF", "6" = "CHAMP")

# All pick columns (including S16_opt) — used to track used_teams
HODES_ALL_PICK_COLS <- c("pick_R1_a", "pick_R1_b", "pick_R1_c",
                          "pick_R2_a", "pick_R2_b", "pick_R2_c",
                          "pick_S16", "pick_S16_opt",
                          "pick_E8", "pick_FF", "pick_CHAMP")

# ==============================================================================
# SLOT HELPERS
# ==============================================================================

#' Get the Hodes slot definition for a given slot ID
get_hodes_slot <- function(slot_id) {
  slot <- HODES_SLOTS[[slot_id]]
  if (is.null(slot)) stop("Unknown hodes slot_id: ", slot_id)
  slot
}

#' Get the pick column name(s) for a slot
#' @return Character vector (3 cols for R1/R2, 1 col for other slots)
hodes_slot_col_names <- function(slot_id) {
  switch(slot_id,
    R1     = c("pick_R1_a", "pick_R1_b", "pick_R1_c"),
    R2     = c("pick_R2_a", "pick_R2_b", "pick_R2_c"),
    S16    = "pick_S16",
    S16_opt = "pick_S16_opt",
    E8     = "pick_E8",
    FF     = "pick_FF",
    CHAMP  = "pick_CHAMP",
    stop("Unknown slot_id: ", slot_id)
  )
}

# ==============================================================================
# BRACKET COMPATIBILITY
# ==============================================================================

#' Compute the earliest tournament round where two teams could meet.
#' @param t1, t2 Integer team_ids (bracket positions 1-64)
#' @return Integer round number (1-6)
earliest_meeting_round <- function(t1, t2) {
  p1 <- t1 - 1L
  p2 <- t2 - 1L
  for (r in 1:6) {
    if (bitwShiftR(p1, r) == bitwShiftR(p2, r)) return(r)
  }
  7L
}

#' Check bracket compatibility for Hodes (simplified: no E8 side constraint).
#'
#' Two picks are incompatible if earliest_meeting_round <= min(their assigned rounds).
#' For R2: pod constraint (earliest_meeting <= 2) is automatically handled.
#'
#' @param candidate_id Team ID to check
#' @param candidate_round Round assigned to candidate
#' @param path_picks Integer vector of team_ids in current path
#' @param path_rounds Integer vector of round numbers for each pick
#' @return TRUE if compatible, FALSE if impossible
is_hodes_bracket_compatible <- function(candidate_id, candidate_round,
                                         path_picks, path_rounds) {
  for (i in seq_along(path_picks)) {
    if (is.na(path_picks[i])) next
    m <- earliest_meeting_round(path_picks[i], candidate_id)
    if (m <= min(path_rounds[i], candidate_round)) return(FALSE)
  }
  TRUE
}

# ==============================================================================
# PORTFOLIO STATE INITIALIZATION
# ==============================================================================

#' Initialize a Hodes portfolio state
#'
#' @param n_entries Integer number of entries (our entries)
#' @param contest_size Integer total field size (including our entries)
#' @param prize_pool Numeric raw prize pool (before any allocation)
#' @param winner_fraction Numeric fraction of prize pool going to winner (default 0.931).
#'   93.1% goes to winner; 6.9% goes to charity.
#'   When 2+ entries survive the contest, those who lose the tiebreaker split
#'   an ADDITIONAL 6.9% of the pool (consolation addendum). This means:
#'   - Mid-round wins (last standing): effective prize = winner_fraction * prize_pool
#'   - Contest survivors (all-round): prize_pool / k per survivor (consolation math)
#' @param entry_fee Numeric entry fee (default 0)
#' @param contest_id Character contest identifier (default "hodes")
#' @return data.table with one row per entry
init_hodes_portfolio <- function(n_entries, contest_size, prize_pool,
                                  winner_fraction = 0.931,
                                  entry_fee = 0, contest_id = "hodes") {
  state <- data.table(
    entry_id         = sprintf("%s_%04d", contest_id, seq_len(n_entries)),
    contest_id       = contest_id,
    contest_size     = as.integer(contest_size),
    entry_fee        = entry_fee,
    prize_pool       = prize_pool,          # raw pool (= $58k)
    winner_fraction  = winner_fraction,     # 0.931 (93.1% to winner)
    alive            = TRUE,
    s16_opt_eligible = FALSE,
    seed_sum         = 0L
  )

  # Add all pick columns (NA = not yet assigned)
  for (col in HODES_ALL_PICK_COLS) {
    state[, (col) := NA_integer_]
  }

  cat(sprintf("Hodes portfolio initialized: %d entries | field=%d | prize=$%s (winner gets %.1f%%)\n",
              n_entries, contest_size, format(prize_pool, big.mark = ","),
              100 * winner_fraction))
  state
}

# ==============================================================================
# PICK RECORDING
# ==============================================================================

#' Record picks for a set of entries in a given slot.
#'
#' For R1/R2 (3-pick slots): team_ids must be length 3 (a, b, c).
#' For single-pick slots: team_ids must be length 1.
#' All entries in entry_ids receive the same picks.
#'
#' @param state data.table (modified in place)
#' @param entry_ids Character or integer vector of entry IDs (entry_id column values
#'   or row indices). Use integer row indices for efficiency.
#' @param slot_id Character slot ID ("R1", "R2", "S16", "S16_opt", "E8", "FF", "CHAMP")
#' @param team_ids Integer vector of team_id(s) to assign
record_hodes_picks <- function(state, entry_ids, slot_id, team_ids) {
  slot <- get_hodes_slot(slot_id)
  cols <- hodes_slot_col_names(slot_id)

  if (length(team_ids) != slot$n_picks) {
    stop(sprintf("Slot %s requires %d picks, but got %d team_ids",
                 slot_id, slot$n_picks, length(team_ids)))
  }

  # Resolve entry indices
  if (is.character(entry_ids)) {
    idx <- which(state$entry_id %in% entry_ids)
  } else {
    idx <- as.integer(entry_ids)
  }
  if (length(idx) == 0) stop("No matching entries found")

  # Validate no reuse across all other slots
  other_cols <- setdiff(HODES_ALL_PICK_COLS, cols)
  other_cols <- intersect(other_cols, names(state))

  for (i in idx) {
    prior <- integer(0)
    for (oc in other_cols) {
      v <- state[[oc]][i]
      if (!is.na(v)) prior <- c(prior, v)
    }
    bad <- intersect(team_ids, prior)
    if (length(bad) > 0) {
      warning(sprintf("Entry %s: team_id(s) %s already used in prior slot, skipping",
                      state$entry_id[i], paste(bad, collapse = ",")))
      next
    }
    for (k in seq_along(cols)) {
      set(state, i = i, j = cols[k], value = team_ids[k])
    }
  }

  # Update seed_sum
  # (Caller is responsible for passing correct team seeds via teams_dt lookup)
  # seed_sum is updated separately via update_hodes_seed_sum()

  invisible(state)
}

#' Update seed_sum for all entries based on current picks
#' @param state data.table
#' @param teams_dt data.table with team_id and seed columns
update_hodes_seed_sum <- function(state, teams_dt) {
  seed_lookup <- setNames(teams_dt$seed, teams_dt$team_id)

  for (i in seq_len(nrow(state))) {
    s <- 0L
    for (col in HODES_ALL_PICK_COLS) {
      v <- state[[col]][i]
      if (!is.na(v)) s <- s + as.integer(seed_lookup[as.character(v)])
    }
    set(state, i = i, j = "seed_sum", value = s)
  }
  invisible(state)
}

# ==============================================================================
# ELIMINATION TRACKING
# ==============================================================================

#' Mark eliminations after a round's results are known.
#'
#' @param state data.table (modified in place)
#' @param round_num Integer round number (1-6)
#' @param winners Integer vector of team_ids that won their game this round
#'   (all 32 for R1, 16 for R2, 8 for S16, 4 for E8, 2 for FF, 1 for CHAMP)
mark_hodes_eliminations <- function(state, round_num, winners) {
  winners <- as.integer(winners)

  if (round_num == 1L) {
    cols <- c("pick_R1_a", "pick_R1_b", "pick_R1_c")
    for (col in cols) {
      state[alive == TRUE & !is.na(get(col)) & !(get(col) %in% winners),
            alive := FALSE]
    }
  } else if (round_num == 2L) {
    cols <- c("pick_R2_a", "pick_R2_b", "pick_R2_c")
    for (col in cols) {
      state[alive == TRUE & !is.na(get(col)) & !(get(col) %in% winners),
            alive := FALSE]
    }
    # Mark entries that survived the first weekend as S16_opt eligible
    state[alive == TRUE, s16_opt_eligible := TRUE]
  } else if (round_num == 3L) {
    # Only mandatory S16 pick eliminates; S16_opt failure just loses the tiebreaker
    state[alive == TRUE & !is.na(pick_S16) & !(pick_S16 %in% winners),
          alive := FALSE]
  } else if (round_num == 4L) {
    state[alive == TRUE & !is.na(pick_E8) & !(pick_E8 %in% winners),
          alive := FALSE]
  } else if (round_num == 5L) {
    state[alive == TRUE & !is.na(pick_FF) & !(pick_FF %in% winners),
          alive := FALSE]
  } else if (round_num == 6L) {
    state[alive == TRUE & !is.na(pick_CHAMP) & !(pick_CHAMP %in% winners),
          alive := FALSE]
  }

  n_alive <- sum(state$alive)
  n_total <- nrow(state)
  cat(sprintf("After round %d: %d/%d entries alive\n", round_num, n_alive, n_total))
  invisible(state)
}

# ==============================================================================
# ENTRY GROUPING (for optimizer)
# ==============================================================================

#' Group alive entries by their pick history for batch computation.
#'
#' Entries with identical pick histories can be optimized together.
#' Returns a data.table with list columns `entry_ids` and `used_teams`.
#'
#' @param state data.table from init_hodes_portfolio
#' @return data.table with columns: group_id, contest_id, contest_size,
#'   prize_pool, n_entries, entry_ids (list), used_teams (list)
group_hodes_entries <- function(state) {
  alive <- state[alive == TRUE]
  if (nrow(alive) == 0) {
    cat("No alive entries.\n")
    return(data.table())
  }

  pick_cols <- intersect(HODES_ALL_PICK_COLS, names(alive))

  # Create a string hash from all current picks
  alive[, pick_hash := {
    apply(.SD, 1, function(row) {
      vals <- row[!is.na(row)]
      paste(c(contest_id[1], sort(vals)), collapse = "_")
    })
  }, .SDcols = c("contest_id", pick_cols)]

  hashes <- unique(alive$pick_hash)
  groups_list <- vector("list", length(hashes))

  for (hi in seq_along(hashes)) {
    h <- hashes[hi]
    g_entries <- alive[pick_hash == h]

    # Collect all used team_ids for this group
    used <- integer(0)
    for (col in pick_cols) {
      vals <- g_entries[[col]]
      used <- c(used, vals[!is.na(vals)])
    }
    used <- unique(as.integer(used))

    groups_list[[hi]] <- list(
      group_id        = hi,
      contest_id      = g_entries$contest_id[1],
      contest_size    = g_entries$contest_size[1],
      prize_pool      = g_entries$prize_pool[1],
      winner_fraction = g_entries$winner_fraction[1],
      n_entries       = nrow(g_entries),
      entry_ids       = list(g_entries$entry_id),
      used_teams      = list(used)
    )
  }

  groups <- rbindlist(lapply(groups_list, function(g) {
    data.table(
      group_id        = g$group_id,
      contest_id      = g$contest_id,
      contest_size    = g$contest_size,
      prize_pool      = g$prize_pool,
      winner_fraction = g$winner_fraction,
      n_entries       = g$n_entries,
      entry_ids       = g$entry_ids,
      used_teams      = g$used_teams
    )
  }))

  cat(sprintf("Grouped %d alive entries into %d group(s)\n",
              nrow(alive), nrow(groups)))
  groups
}

# ==============================================================================

cat("Hodes config loaded\n")
cat("  Format: 3-3-1+opt-1-1-1 (R1=3, R2=3, S16=1+optional, E8/FF/CHAMP=1)\n")
cat("  Tiebreakers: TB1=S16_opt success, TB2=seed sum (higher wins)\n")
cat("  No day split for R1 or R2\n")
