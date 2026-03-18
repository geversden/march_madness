#!/usr/bin/env Rscript
# ==============================================================================
# splash_config.R
# Contest format definition and tournament day schedule for Splash Sports
# NCAA tournament survivor pools.
#
# FORMAT A (E8 combined):
#   Pick 1 team per day in R1 (2 days), R2 (2 days), S16 (2 days).
#   Pick 2 teams in E8 (any 2 of 4 games, don't have to be from different days).
#   Pick 1 in Final Four, 1 in Championship.
#   Total: 10 picks. No team reuse. One loss = eliminated.
#
# FORMAT B (E8 split by day):
#   Same as Format A through S16.
#   Pick 1 team on E8 Day 1 (Sat), 1 team on E8 Day 2 (Sun).
#   Pick 1 in Final Four, 1 in Championship.
#   Total: 10 picks. No team reuse. One loss = eliminated.
#
# The E8 split significantly constrains strategy: you MUST have a viable
# pick on each E8 day, which changes save/future-value in earlier rounds.
#
# Usage:
#   source("splash_config.R")
# ==============================================================================

# ==============================================================================
# SLOT DEFINITIONS
# Each slot is one decision point (one "day" of picks).
# game_indices = columns in the 63-column sim matrix (1-indexed)
# ==============================================================================

SPLASH_SLOTS <- list(
  R1_d1 = list(
    slot_id     = "R1_d1",
    round_num   = 1,
    n_picks     = 1,
    label       = "Round of 64 - Day 1 (Thu Mar 19)",
    game_indices = NULL  # set below per year

  ),
  R1_d2 = list(
    slot_id     = "R1_d2",
    round_num   = 1,
    n_picks     = 1,
    label       = "Round of 64 - Day 2 (Fri Mar 20)",
    game_indices = NULL
  ),
  R2_d1 = list(
    slot_id     = "R2_d1",
    round_num   = 2,
    n_picks     = 1,
    label       = "Round of 32 - Day 1 (Sat Mar 21)",
    game_indices = NULL
  ),
  R2_d2 = list(
    slot_id     = "R2_d2",
    round_num   = 2,
    n_picks     = 1,
    label       = "Round of 32 - Day 2 (Sun Mar 22)",
    game_indices = NULL
  ),
  S16_d1 = list(
    slot_id     = "S16_d1",
    round_num   = 3,
    n_picks     = 1,
    label       = "Sweet 16 - Day 1 (Thu Mar 26)",
    game_indices = NULL
  ),
  S16_d2 = list(
    slot_id     = "S16_d2",
    round_num   = 3,
    n_picks     = 1,
    label       = "Sweet 16 - Day 2 (Fri Mar 27)",
    game_indices = NULL
  ),
  # Format A: pick any 2 of 4 E8 games
  E8 = list(
    slot_id     = "E8",
    round_num   = 4,
    n_picks     = 2,
    label       = "Elite 8 (Sat-Sun Mar 28-29)",
    game_indices = 57:60
  ),
  # Format B: one pick per E8 day (set game_indices below after E8 schedule known)
  E8_d1 = list(
    slot_id     = "E8_d1",
    round_num   = 4,
    n_picks     = 1,
    label       = "Elite 8 - Day 1 (Sat Mar 28)",
    game_indices = NULL  # set below
  ),
  E8_d2 = list(
    slot_id     = "E8_d2",
    round_num   = 4,
    n_picks     = 1,
    label       = "Elite 8 - Day 2 (Sun Mar 29)",
    game_indices = NULL  # set below
  ),
  FF = list(
    slot_id     = "FF",
    round_num   = 5,
    n_picks     = 1,
    label       = "Final Four (Sat Apr 4)",
    game_indices = 61:62
  ),
  CHAMP = list(
    slot_id     = "CHAMP",
    round_num   = 6,
    n_picks     = 1,
    label       = "Championship (Mon Apr 6)",
    game_indices = 63
  )
)

# ==============================================================================
# FORMAT VARIANTS
#
# Format A: 1/day, E8 combined  (2-2-2-2-1-1) = 10 picks — most common
# Format B: 1/day, E8 split     (2-2-2-2-1-1) = 10 picks — Spooky, MARCH MADNESS
# Format C: 2/day R1+R2, E8 combined (4-4-2-2-1-1) = 14 picks — Benkert
#
# Hodes (3-3-1-1-1-1) is a separate system and not handled here.
# ==============================================================================

FORMAT_DEFS <- list(
  A = list(
    label = "1/day, E8 combined (2-2-2-2-1-1)",
    slot_order = c("R1_d1", "R1_d2", "R2_d1", "R2_d2",
                   "S16_d1", "S16_d2", "E8", "FF", "CHAMP"),
    n_picks_override = list(),
    total_picks = 10
  ),
  B = list(
    label = "1/day, E8 split (2-2-2-2-1-1)",
    slot_order = c("R1_d1", "R1_d2", "R2_d1", "R2_d2",
                   "S16_d1", "S16_d2", "E8_d1", "E8_d2", "FF", "CHAMP"),
    n_picks_override = list(),
    total_picks = 10
  ),
  C = list(
    label = "2/day R1+R2, E8 combined (4-4-2-2-1-1)",
    slot_order = c("R1_d1", "R1_d2", "R2_d1", "R2_d2",
                   "S16_d1", "S16_d2", "E8", "FF", "CHAMP"),
    n_picks_override = list(R1_d1 = 2L, R1_d2 = 2L, R2_d1 = 2L, R2_d2 = 2L),
    total_picks = 14
  )
)

# Convenience vectors (backward compat)
SLOT_ORDER_A <- FORMAT_DEFS$A$slot_order
SLOT_ORDER_B <- FORMAT_DEFS$B$slot_order
SLOT_ORDER_C <- FORMAT_DEFS$C$slot_order

# All possible slot IDs (superset) — used for state columns
ALL_SLOT_IDS <- c("R1_d1", "R1_d2", "R2_d1", "R2_d2",
                  "S16_d1", "S16_d2", "E8", "E8_d1", "E8_d2", "FF", "CHAMP")

# Default: Format A (can be overridden per contest)
SLOT_ORDER <- SLOT_ORDER_A

#' Get the format definition for a given format code
#' @param format Character "A", "B", or "C"
#' @return List with slot_order, n_picks_override, total_picks, label
get_format_def <- function(format = "A") {
  fmt <- FORMAT_DEFS[[toupper(format)]]
  if (is.null(fmt)) {
    stop("Unknown format: ", format, ". Use 'A', 'B', or 'C'.")
  }
  fmt
}

#' Get the slot order for a given format
#' @param format Character "A", "B", or "C"
get_slot_order <- function(format = "A") {
  get_format_def(format)$slot_order
}

#' Get n_picks for a slot under a given format
#' Checks format-specific overrides first, then falls back to the slot default.
#' @param slot_id Character slot ID
#' @param format Character "A", "B", or "C"
#' @return Integer number of picks for this slot in this format
get_n_picks <- function(slot_id, format = "A") {
  fmt <- get_format_def(format)
  override <- fmt$n_picks_override[[slot_id]]
  if (!is.null(override)) return(override)
  get_slot(slot_id)$n_picks
}

# ==============================================================================
# 2026 GAME-DAY MAPPING
#
# Game index = column in sim matrix (1-indexed).
# R64 games 1-32: game g pairs bracket positions (2g-1) vs (2g).
# Bracket CSV order: East(1-16), South(17-32), West(33-48), Midwest(49-64)
# Within each region: 1v16, 8v9, 5v12, 4v13, 6v11, 3v14, 7v10, 2v15
#
# Thursday Mar 19 venues:
#   Greenville: Duke/Siena(1), OhioSt/TCU(2), UNC/VCU(13), Illinois/Penn(14)
#   OKC:        Vanderbilt/McNeese(11), Nebraska/Troy(12), StMary's/TexasA&M(15), Houston/Idaho(16)
#   Buffalo:    Louisville/USF(5), MichiganSt/NDSU(6), Michigan/UMBC(25), Georgia/StLouis(26)
#   Portland:   Wisconsin/HighPoint(19), Arkansas/Hawaii(20), BYU/Texas(21), Gonzaga/Kennesaw(22)
#
# Friday Mar 20 venues:
#   Tampa:       Florida/PrairieView(9), Clemson/Iowa(10)
#   Philadelphia: StJohn's/NIowa(3), Kansas/CalBaptist(4), UCLA/UCF(7), UConn/Furman(8)
#   San Diego:   Arizona/LIU(17), Villanova/UtahSt(18), Miami/Missouri(23), Purdue/Queens(24)
#   St. Louis:   TexasTech/Akron(27), Alabama/Hofstra(28), Tennessee/SMU(29),
#                Virginia/WrightSt(30), Kentucky/SantaClara(31), IowaState/TennSt(32)
# ==============================================================================

R64_THU_GAMES <- c(1, 2, 5, 6, 11, 12, 13, 14, 15, 16, 19, 20, 21, 22, 25, 26)
R64_FRI_GAMES <- c(3, 4, 7, 8, 9, 10, 17, 18, 23, 24, 27, 28, 29, 30, 31, 32)

SPLASH_SLOTS$R1_d1$game_indices <- R64_THU_GAMES
SPLASH_SLOTS$R1_d2$game_indices <- R64_FRI_GAMES

# R32 game indices: 33-48 (16 games)
# R32 game g (33-48) is the winner of R64 games (2*(g-33)+1) and (2*(g-33)+2)
# relative to the R64 block. In practice:
#   R32 game 33 = winner of R64 games 1,2  (East top quarter)
#   R32 game 34 = winner of R64 games 3,4  (East second quarter)
#   ...etc.
# Day mapping for R32 follows from which R64 games feed in:
# If both feeder R64 games were Thursday, the R32 game is Saturday.
# If both feeder R64 games were Friday, the R32 game is Sunday.
# Mixed feeders: determined by NCAA schedule (set after R64).

# R32 feeder mapping: R32 game g <- R64 games (2*(g-33)+1, 2*(g-33)+2)
# Game 33 <- (1,2) both Thu -> Sat
# Game 34 <- (3,4) both Fri -> Sun
# Game 35 <- (5,6) both Thu -> Sat
# Game 36 <- (7,8) both Fri -> Sun
# Game 37 <- (9,10) both Fri -> Sun
# Game 38 <- (11,12) both Thu -> Sat
# Game 39 <- (13,14) both Thu -> Sat
# Game 40 <- (15,16) both Thu -> Sat
# Game 41 <- (17,18) both Fri -> Sun
# Game 42 <- (19,20) both Thu -> Sat
# Game 43 <- (21,22) both Thu -> Sat
# Game 44 <- (23,24) both Fri -> Sun
# Game 45 <- (25,26) both Thu -> Sat
# Game 46 <- (27,28) both Fri -> Sun
# Game 47 <- (29,30) both Fri -> Sun
# Game 48 <- (31,32) both Fri -> Sun

R32_SAT_GAMES <- c(33, 35, 38, 39, 40, 42, 43, 45)  # Thu feeders -> Sat
R32_SUN_GAMES <- c(34, 36, 37, 41, 44, 46, 47, 48)   # Fri feeders -> Sun

SPLASH_SLOTS$R2_d1$game_indices <- R32_SAT_GAMES
SPLASH_SLOTS$R2_d2$game_indices <- R32_SUN_GAMES

# S16 game indices: 49-56 (8 games)
# S16 game g <- R32 games (2*(g-49)+33, 2*(g-49)+34)
# Game 49 <- (33,34) Sat+Sun -> Thu (typical NCAA pattern: mixed -> Thu)
# Game 50 <- (35,36) Sat+Sun -> Thu
# Game 51 <- (37,38) Sun+Sat -> Thu
# Game 52 <- (39,40) Sat+Sat -> Fri
# Game 53 <- (41,42) Sun+Sat -> Thu
# Game 54 <- (43,44) Sat+Sun -> Fri
# Game 55 <- (45,46) Sat+Sun -> Fri
# Game 56 <- (47,48) Sun+Sun -> Fri
#
# NOTE: S16 day assignments are typically announced after R32.
# Below is a reasonable default (East/South Thu, West/Midwest Fri)
# but should be updated once the NCAA publishes the S16 schedule.

S16_THU_GAMES <- c(51, 52, 53, 54)  # West + South (update when schedule announced)
S16_FRI_GAMES <- c(49, 50, 55, 56)  # East + Midwest (update when schedule announced)

SPLASH_SLOTS$S16_d1$game_indices <- S16_THU_GAMES
SPLASH_SLOTS$S16_d2$game_indices <- S16_FRI_GAMES

# E8 game indices: 57-60 (4 games)
# E8 day split depends on NCAA schedule (announced after S16).
# Typical pattern: 2 games Saturday, 2 games Sunday.
# E8 game 57 <- S16 games (49,50) -> East region champion matchup
# E8 game 58 <- S16 games (51,52) -> South region champion matchup
# E8 game 59 <- S16 games (53,54) -> West region champion matchup
# E8 game 60 <- S16 games (55,56) -> Midwest region champion matchup
#
# Default: East+South on Sat (57,58), West+Midwest on Sun (59,60)
# UPDATE when NCAA publishes E8 schedule.
E8_SAT_GAMES <- c(58, 59)
E8_SUN_GAMES <- c(57, 60)

SPLASH_SLOTS$E8_d1$game_indices <- E8_SAT_GAMES
SPLASH_SLOTS$E8_d2$game_indices <- E8_SUN_GAMES

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get the slot definition for a given slot ID
get_slot <- function(slot_id) {
  slot <- SPLASH_SLOTS[[slot_id]]
  if (is.null(slot)) stop("Unknown slot_id: ", slot_id)
  slot
}

#' Get all team IDs that are playing in a given slot
#' @param slot_id Character slot ID
#' @param teams_dt Data frame with columns: team_id, name, seed, region (64 rows)
#' @return Integer vector of team_ids
get_teams_in_slot <- function(slot_id, teams_dt) {
  slot <- get_slot(slot_id)
  game_idxs <- slot$game_indices

  team_ids <- integer(0)
  for (g in game_idxs) {
    # Map any game in the tournament back to its Round of 64 feeder games
    if (g <= 32) {
      r64_games <- g
    } else if (g <= 48) {
      r64_games <- (g - 33) * 2 + 1:2
    } else if (g <= 56) {
      r64_games <- (g - 49) * 4 + 1:4
    } else if (g <= 60) {
      r64_games <- (g - 57) * 8 + 1:8
    } else if (g <= 62) {
      r64_games <- (g - 61) * 16 + 1:16
    } else {
      r64_games <- 1:32
    }
    
    # Extract the original 64 teams that feed into those R64 games
    for (rg in r64_games) {
      team_ids <- c(team_ids, teams_dt$team_id[2 * rg - 1], teams_dt$team_id[2 * rg])
    }
  }
  unique(team_ids)
}

#' Get teams available to pick for a given entry in a given slot
#' @param slot_id Character slot ID
#' @param used_teams Integer vector of team_ids already picked by this entry
#' @param teams_dt Data frame with team info
#' @return Data frame subset of teams_dt that are eligible (playing today AND not used)
get_available_picks <- function(slot_id, used_teams, teams_dt) {
  playing <- get_teams_in_slot(slot_id, teams_dt)
  available <- setdiff(playing, used_teams)
  teams_dt[teams_dt$team_id %in% available, ]
}

#' Map a game index to the two bracket positions that play in that game
#' Only valid for R64 games (1-32)
#' @return Named list with team_a_pos and team_b_pos (1-indexed bracket positions)
game_to_bracket_positions <- function(game_idx) {
  stopifnot(game_idx >= 1 && game_idx <= 32)
  list(
    team_a_pos = 2 * game_idx - 1,
    team_b_pos = 2 * game_idx
  )
}

#' Get the column name used to store a pick for a given slot in entry state
slot_col_name <- function(slot_id) {
  paste0("pick_", slot_id)
}

#' Get all slot column names (superset across all formats)
all_slot_cols <- function() {
  sapply(ALL_SLOT_IDS, slot_col_name, USE.NAMES = FALSE)
}

#' Get slot column names for a specific format
format_slot_cols <- function(format = "A") {
  sapply(get_slot_order(format), slot_col_name, USE.NAMES = FALSE)
}

cat("Splash config loaded\n")
cat("  Format A: 9 slots, 10 picks (1/day, E8 combined)\n")
cat("  Format B: 10 slots, 10 picks (1/day, E8 split)\n")
cat("  Format C: 9 slots, 14 picks (2/day R1+R2, E8 combined)\n")
cat(sprintf("  R64 Thu: %d games | R64 Fri: %d games\n",
            length(R64_THU_GAMES), length(R64_FRI_GAMES)))
cat(sprintf("  R32 Sat: %d games | R32 Sun: %d games\n",
            length(R32_SAT_GAMES), length(R32_SUN_GAMES)))
cat(sprintf("  S16 Thu: %d games | S16 Fri: %d games\n",
            length(S16_THU_GAMES), length(S16_FRI_GAMES)))
cat(sprintf("  E8: %d games (combined) | E8 Sat: %d | E8 Sun: %d\n",
            length(SPLASH_SLOTS$E8$game_indices),
            length(E8_SAT_GAMES), length(E8_SUN_GAMES)))
cat(sprintf("  FF: %d games | Champ: %d game\n",
            length(SPLASH_SLOTS$FF$game_indices),
            length(SPLASH_SLOTS$CHAMP$game_indices)))
