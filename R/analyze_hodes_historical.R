#!/usr/bin/env Rscript
# ==============================================================================
# analyze_hodes_historical.R
#
# Parses Hodes contest results (2021-2025) and builds calibration datasets for
# modelling S16 pick-count decisions and team selection at S16/E8.
#
# Data sources:
#   - Hodes Results.xlsx (C:/Users/tyler/Downloads/Hodes Results.xlsx)
#   - brackets/bracket_YYYY.csv
#   - closing_lines/ncaat_YYYY_closing_lines.csv
#   - kenpom_data/kenpom_YYYY.csv
#
# Entry points:
#   analyze_hodes_year(year)        -- single year
#   analyze_all_hodes_years()       -- 2021-2025 combined
#
# Usage:
#   source("R/analyze_hodes_historical.R")
#   results <- analyze_all_hodes_years()
# ==============================================================================

# --- Determine project root -------------------------------------------------
.hodes_script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) {
  if (file.exists("R/hodes_config.R")) "R"
  else if (file.exists("hodes_config.R")) "."
  else stop("Cannot determine script_dir. Set working directory to the project root.")
})
.hodes_project_root <- normalizePath(file.path(.hodes_script_dir, ".."), winslash = "/")

# --- Constants ---------------------------------------------------------------
HODES_XLSX <- file.path("C:/Users/tyler/Downloads", "Hodes Results.xlsx")
KENPOM_K   <- 0.1457   # logistic scale from calibrate_win_prob.R
HODES_YEARS <- 2021:2025

# Column names as read by readxl
HODES_COL_NAMES <- c("Rank", "Entry", "R64_1", "R64_2", "R64_3",
                      "R32_1", "R32_2", "R32_3",
                      "R16_1", "R16_2", "QF", "SF", "Final", "TiebreakSeedSum")

# Bracket matchup order within a region (R64): 1v16, 8v9, 5v12, 4v13, 6v11, 3v14, 7v10, 2v15
# These correspond to rows 1-2, 3-4, 5-6, 7-8, 9-10, 11-12, 13-14, 15-16 in the bracket CSV.
# So row positions within each 16-team region block are already in game-order.

# ==============================================================================
# TEAM NAME MAPPING
# ==============================================================================

# Hodes -> bracket name aliases for teams that don't match by simple normalization.
# Hodes names are short: "Michigan St", "UConn", "Fla Atlantic", etc.
# Bracket names: "Michigan State", "UConn", "Florida Atlantic", etc.
.hodes_to_bracket_aliases <- c(
  "Michigan St"        = "Michigan State",
  "Michigan St."       = "Michigan State",
  "Mississippi St"     = "Mississippi State",
  "Mississippi St."    = "Mississippi State",
  "Ohio St"            = "Ohio State",
  "Ohio St."           = "Ohio State",
  "Iowa St"            = "Iowa State",
  "Iowa St."           = "Iowa State",
  "Boise St"           = "Boise State",
  "Boise St."          = "Boise State",
  "Utah St"            = "Utah State",
  "Utah St."           = "Utah State",
  "Colorado St"        = "Colorado State",
  "Colorado St."       = "Colorado State",
  "Fla Atlantic"       = "Florida Atlantic",
  "Florida Atlantic"   = "Florida Atlantic",
  "North Carolina St." = "NC State",
  "North Carolina St"  = "NC State",
  "NC St."             = "NC State",
  "NC St"              = "NC State",
  "San Diego St."      = "San Diego State",
  "San Diego St"       = "San Diego State",
  "Oklahoma St"        = "Oklahoma State",
  "Oklahoma St."       = "Oklahoma State",
  "Kansas St"          = "Kansas State",
  "Kansas St."         = "Kansas State",
  "Kent St"            = "Kent State",
  "Kent St."           = "Kent State",
  "McNeese St"         = "McNeese State",
  "McNeese St."        = "McNeese State",
  "Grambling St"       = "Grambling State",
  "Grambling St."      = "Grambling State",
  "Long Beach St"      = "Long Beach State",
  "Long Beach St."     = "Long Beach State",
  "South Dakota St"    = "South Dakota State",
  "South Dakota St."   = "South Dakota State",
  "UConn"              = "UConn",
  "Miami FL"           = "Miami FL",
  "Miami"              = "Miami FL",
  "St John\u2019s"     = "St. John's",
  "St John's"          = "St. John's",
  "St Johns"           = "St. John's",
  "St. Johns"          = "St. John's",
  "Saint Mary\u2019s"  = "Saint Mary's",
  "Saint Marys"        = "Saint Mary's",
  "Saint Mary's"       = "Saint Mary's",
  "Loyola Chicago"     = "Loyola Chicago",
  "Oral Roberts"       = "Oral Roberts",
  "Fla Atlantic"       = "Florida Atlantic",
  "VCU"                = "VCU",
  "Connecticut"        = "UConn",
  "UNC"                = "North Carolina",
  "Va Tech"            = "Virginia Tech",
  "St Marys"           = "Saint Mary's",
  "St Mary's"          = "Saint Mary's",
  "St. Mary's"         = "Saint Mary's",
  "St. Marys"          = "Saint Mary's",
  "NM State"           = "New Mexico State",
  "NM St"              = "New Mexico State",
  "St Peters"          = "Saint Peter's",
  "St. Peters"         = "Saint Peter's",
  "St Peter's"         = "Saint Peter's",
  "Charleston"         = "College of Charleston",
  "UCSB"               = "UC Santa Barbara",
  "Fla. Atlantic"      = "Florida Atlantic"
)

# Closing-line team name -> bracket name aliases
.cl_to_bracket_aliases <- c(
  "UConn Huskies"                        = "UConn",
  "Loyola (Chi) Ramblers"                = "Loyola Chicago",
  "NC State Wolfpack"                    = "NC State",
  "North Carolina St. Wolfpack"          = "NC State",
  "Ole Miss Rebels"                      = "Ole Miss",
  "UCF Knights"                          = "Central Florida",
  "FDU Knights"                          = "Fairleigh Dickinson",
  "Grambling Tigers"                     = "Grambling State",
  "Grambling State Tigers"               = "Grambling State",
  "Grambling St Tigers"                  = "Grambling State",
  "McNeese Cowboys"                      = "McNeese State",
  "McNeese State Cowboys"                = "McNeese State",
  "Long Beach State 49ers"               = "Long Beach State",
  "Long Beach St 49ers"                  = "Long Beach State",
  "Mt. St. Mary's Mountaineers"          = "Mount St. Mary's",
  "St. Peter's Peacocks"                 = "Saint Peter's",
  "Saint Peter's Peacocks"               = "Saint Peter's",
  "Miami (FL) Hurricanes"                = "Miami FL",
  "Miami Hurricanes"                     = "Miami FL",
  "SIU-Edwardsville Cougars"             = "SIU Edwardsville",
  "Texas A&M-CC Islanders"               = "Texas A&M Corpus Christi",
  "Omaha Mavericks"                      = "Omaha",
  "UC Santa Barbara Gauchos"             = "UC Santa Barbara",
  "UC San Diego Tritons"                 = "UC San Diego",
  "UC Irvine Anteaters"                  = "UC Irvine",
  "UNC Greensboro Spartans"              = "UNC Greensboro",
  "UNC Wilmington Seahawks"              = "UNC Wilmington",
  "UNC Asheville Bulldogs"               = "UNC Asheville",
  "CSU Fullerton Titans"                 = "Cal State Fullerton",
  "SE Missouri State Redhawks"           = "Southeast Missouri State",
  "Cleveland St Vikings"                 = "Cleveland State",
  "Boise State Broncos"                  = "Boise State",
  "Kent State Golden Flashes"            = "Kent State",
  "Fla Atlantic Owls"                    = "Florida Atlantic",
  "Florida Atlantic Owls"                = "Florida Atlantic",
  "St. John's Red Storm"                 = "St. John's",
  "Mississippi St Bulldogs"              = "Mississippi State",
  "Washington St Cougars"                = "Washington State",
  "San Diego St Aztecs"                  = "San Diego State",
  "Morehead St Eagles"                   = "Morehead State",
  "South Dakota St Jackrabbits"          = "South Dakota State",
  "Michigan St Spartans"                 = "Michigan State",
  "Oklahoma St Cowboys"                  = "Oklahoma State",
  "Colorado St Rams"                     = "Colorado State",
  "Iowa St Cyclones"                     = "Iowa State",
  "Utah St Aggies"                       = "Utah State",
  "Boise St Broncos"                     = "Boise State",
  "Kent St Golden Flashes"               = "Kent State",
  "Kansas St Wildcats"                   = "Kansas State",
  "Grambling St Tigers"                  = "Grambling State",
  "Norfolk St Spartans"                  = "Norfolk State",
  "Murray St Racers"                     = "Murray State",
  "Georgia St Panthers"                  = "Georgia State",
  "New Mexico St Aggies"                 = "New Mexico State",
  "Wright St Raiders"                    = "Wright State",
  "Jacksonville St Gamecocks"            = "Jacksonville State",
  "Montana St Bobcats"                   = "Montana State",
  "Kennesaw St Owls"                     = "Kennesaw State",
  "Long Beach St. 49ers"                 = "Long Beach State"
)


#' Match a closing-line team name to a bracket team name.
#'
#' Strategy: (1) check explicit alias table, (2) strip mascot and try direct
#' match, (3) prefix match (longest first).
#'
#' @param cl_name Character closing-line team name (e.g. "Auburn Tigers")
#' @param bracket_names Character vector of bracket team names
#' @return Matched bracket name, or NA_character_
.match_cl_to_bracket <- function(cl_name, bracket_names) {
  # 1. Explicit alias

  if (cl_name %in% names(.cl_to_bracket_aliases)) {
    alias <- .cl_to_bracket_aliases[[cl_name]]
    if (alias %in% bracket_names) return(alias)
  }

  # 2. Try stripping last word (mascot) and matching
  parts <- strsplit(cl_name, " ")[[1]]
  if (length(parts) >= 2) {
    for (n_keep in (length(parts) - 1):1) {
      candidate <- paste(parts[1:n_keep], collapse = " ")
      if (candidate %in% bracket_names) return(candidate)
      # Try with trailing period removed
      candidate_clean <- sub("\\.$", "", candidate)
      if (candidate_clean %in% bracket_names) return(candidate_clean)
    }
  }

  # 3. Prefix match: check if any bracket name is a prefix of the CL name
  bn_sorted <- bracket_names[order(nchar(bracket_names), decreasing = TRUE)]
  for (bn in bn_sorted) {
    if (startsWith(cl_name, bn) &&
        (nchar(cl_name) == nchar(bn) ||
         substr(cl_name, nchar(bn) + 1, nchar(bn) + 1) == " ")) {
      return(bn)
    }
  }

  NA_character_
}


#' Match a Hodes entry team name to a bracket team name.
#'
#' Hodes names are typically short: "Auburn", "Michigan St", "Fla Atlantic".
#'
#' @param hodes_name Character team name from Hodes spreadsheet
#' @param bracket_names Character vector of bracket team names
#' @return Matched bracket name, or NA_character_
.match_hodes_to_bracket <- function(hodes_name, bracket_names) {
  if (is.na(hodes_name) || hodes_name == "" || hodes_name == "No Pick" ||
      hodes_name == "0.0" || hodes_name == "0") {
    return(NA_character_)
  }

  # Fix smart quotes and encoding issues (including double-encoded UTF-8 on Windows)
  hodes_name <- gsub("\u2019", "'", hodes_name)
  hodes_name <- gsub("\u2018", "'", hodes_name)
  # Double-encoded right single quote: \xc3\xa2 \xe2\x82\xac \xe2\x84\xa2
  hodes_name <- gsub("\xc3\xa2\xe2\x82\xac\xe2\x84\xa2", "'", hodes_name, useBytes = TRUE)
  # Also handle other garbled variants
  hodes_name <- gsub("\xe2\x80\x99", "'", hodes_name, useBytes = TRUE)
  hodes_name <- gsub("\xe2\x80\x98", "'", hodes_name, useBytes = TRUE)

  # Direct match
  if (hodes_name %in% bracket_names) return(hodes_name)

  # Explicit alias
  if (hodes_name %in% names(.hodes_to_bracket_aliases)) {
    alias <- .hodes_to_bracket_aliases[[hodes_name]]
    if (alias %in% bracket_names) return(alias)
  }

  # Try adding "State" for abbreviations like "Michigan St" or "Michigan St."
  base <- sub("\\s+St\\.?$", " State", hodes_name)
  if (base != hodes_name && base %in% bracket_names) return(base)

  # Try removing period: "St." -> "St"
  no_period <- gsub("\\.", "", hodes_name)
  for (bn in bracket_names) {
    if (gsub("\\.", "", bn) == no_period) return(bn)
  }

  # Prefix match
  bn_sorted <- bracket_names[order(nchar(bracket_names), decreasing = TRUE)]
  for (bn in bn_sorted) {
    if (startsWith(hodes_name, bn) || startsWith(bn, hodes_name)) {
      return(bn)
    }
  }

  NA_character_
}


#' Build a name mapping table for a given year.
#'
#' @param year Integer tournament year
#' @return data.frame with columns: bracket_name, seed, region, team_id (1-64),
#'   hodes_aliases (list column), cl_name (closing-line name if found)
build_hodes_name_map <- function(year) {
  bracket_file <- file.path(.hodes_project_root, "brackets",
                            sprintf("bracket_%d.csv", year))
  cl_file <- file.path(.hodes_project_root, "closing_lines",
                        sprintf("ncaat_%d_closing_lines.csv", year))

  if (!file.exists(bracket_file)) stop("Bracket file not found: ", bracket_file)

  bracket <- read.csv(bracket_file, stringsAsFactors = FALSE)
  bracket$team_id <- seq_len(nrow(bracket))

  # Load closing lines for CL->bracket mapping
  cl_names <- character(0)
  if (file.exists(cl_file)) {
    cl <- read.csv(cl_file, stringsAsFactors = FALSE)
    cl_names <- unique(c(cl$home_team, cl$away_team))
  }

  bracket$cl_name <- NA_character_
  for (i in seq_len(nrow(bracket))) {
    for (cln in cl_names) {
      matched <- .match_cl_to_bracket(cln, bracket$team[i])
      if (!is.na(matched)) {
        bracket$cl_name[i] <- cln
        break
      }
    }
    # More robust: just check if any CL name maps to this bracket name
    if (is.na(bracket$cl_name[i])) {
      for (cln in cl_names) {
        m <- .match_cl_to_bracket(cln, bracket$team)
        if (!is.na(m) && m == bracket$team[i]) {
          bracket$cl_name[i] <- cln
          break
        }
      }
    }
  }

  bracket
}


# ==============================================================================
# BRACKET STRUCTURE HELPERS
# ==============================================================================

#' Get the game index for a team_id in a given round.
#' Bracket positions are 1-64. Game indices are 1-63.
#' R64: games 1-32, R32: 33-48, S16: 49-56, E8: 57-60, FF: 61-62, Final: 63
.game_for_team <- function(team_id, round_num) {
  # In round r, team_id's game index is:
  # base_for_round + ceiling(team_id / 2^round_num)
  bases <- c(0L, 32L, 48L, 56L, 60L, 62L)
  bases[round_num] + as.integer(ceiling(team_id / 2^round_num))
}

#' Get the two team_id ranges that play each other in a given game.
#' Returns the set of team_ids that COULD appear in that game slot.
.teams_in_game <- function(game_idx) {
  # Determine round
  round_num <- findInterval(game_idx, c(1, 33, 49, 57, 61, 63))
  bases <- c(0L, 32L, 48L, 56L, 60L, 62L)
  offset <- game_idx - bases[round_num]  # 1-based within round

  # Each game in round r covers a contiguous block of 2^r team_ids
  block_size <- 2^round_num
  start_id <- (offset - 1) * block_size + 1
  end_id <- offset * block_size

  list(round = round_num, team_ids = start_id:end_id)
}

#' Get the opponent team_id block for a given team in a given round.
#' In R64, team 1 plays team 2; in R32, teams {1,2} play {3,4}; etc.
.get_opponent_block <- function(team_id, round_num) {
  block_size <- 2^round_num
  game_offset <- ceiling(team_id / block_size)
  game_start <- (game_offset - 1) * block_size + 1
  game_end <- game_offset * block_size

  my_half_size <- 2^(round_num - 1)
  my_half_start <- ceiling(team_id / my_half_size)
  my_start <- (my_half_start - 1) * my_half_size + 1
  my_end <- my_half_start * my_half_size

  opp_ids <- setdiff(game_start:game_end, my_start:my_end)
  opp_ids
}


# ==============================================================================
# LOAD AND PARSE DATA
# ==============================================================================

#' Load bracket, closing lines, and KenPom data for a year.
#'
#' @param year Integer
#' @return List with: bracket, closing_lines, kenpom, cl_game_list
.load_year_data <- function(year) {
  bracket_file <- file.path(.hodes_project_root, "brackets",
                            sprintf("bracket_%d.csv", year))
  cl_file <- file.path(.hodes_project_root, "closing_lines",
                        sprintf("ncaat_%d_closing_lines.csv", year))
  kp_file <- file.path(.hodes_project_root, "kenpom_data",
                        sprintf("kenpom_%d.csv", year))

  if (!file.exists(bracket_file)) stop("Missing bracket file: ", bracket_file)
  if (!file.exists(cl_file)) stop("Missing closing lines: ", cl_file)
  if (!file.exists(kp_file)) stop("Missing kenpom file: ", kp_file)

  bracket <- read.csv(bracket_file, stringsAsFactors = FALSE)
  bracket$team_id <- seq_len(nrow(bracket))

  cl <- read.csv(cl_file, stringsAsFactors = FALSE)

  kp <- read.csv(kp_file, stringsAsFactors = FALSE)
  kp <- kp[!is.na(kp$Team) & kp$Team != "" & kp$Team != "Team", ]
  kp$Team <- gsub("\\s*\\d+$", "", trimws(kp$Team))
  kp$NetRtg <- as.numeric(kp$NetRtg)
  kp <- kp[!is.na(kp$NetRtg), ]

  # Build closing line -> bracket name mapping
  cl$home_bracket <- sapply(cl$home_team, .match_cl_to_bracket,
                            bracket_names = bracket$team, USE.NAMES = FALSE)
  cl$away_bracket <- sapply(cl$away_team, .match_cl_to_bracket,
                            bracket_names = bracket$team, USE.NAMES = FALSE)

  # Check for unmatched
  unmatched <- c(
    cl$home_team[is.na(cl$home_bracket)],
    cl$away_team[is.na(cl$away_bracket)]
  )
  if (length(unmatched) > 0) {
    warning(sprintf("[%d] Unmatched closing-line team names: %s",
                    year, paste(unique(unmatched), collapse = ", ")))
  }

  # Map bracket name -> team_id
  name_to_id <- setNames(bracket$team_id, bracket$team)
  cl$home_id <- name_to_id[cl$home_bracket]
  cl$away_id <- name_to_id[cl$away_bracket]

  # Determine round for each closing-line game based on date ordering
  # R64 = first 32 games, R32 = next 16, S16 = next 8, E8 = next 4,
  # FF = next 2, Final = last 1
  cl <- cl[order(cl$date), ]
  n_games <- nrow(cl)
  round_breaks <- cumsum(c(32, 16, 8, 4, 2, 1))
  cl$round_num <- findInterval(seq_len(n_games), c(1, round_breaks[-length(round_breaks)] + 1)) + 0L
  # Correct: game 1-32 = round 1, 33-48 = round 2, etc.
  cl$round_num <- ifelse(seq_len(n_games) <= 32, 1L,
                  ifelse(seq_len(n_games) <= 48, 2L,
                  ifelse(seq_len(n_games) <= 56, 3L,
                  ifelse(seq_len(n_games) <= 60, 4L,
                  ifelse(seq_len(n_games) <= 62, 5L, 6L)))))

  # Build game list: for each CL game, record home_id, away_id, wp, round
  cl_games <- cl[!is.na(cl$home_id) & !is.na(cl$away_id),
                 c("home_team", "away_team", "home_bracket", "away_bracket",
                   "home_id", "away_id", "home_win_prob", "round_num", "date")]

  list(bracket = bracket, closing_lines = cl, kenpom = kp,
       cl_games = cl_games, name_to_id = name_to_id)
}


#' Determine actual game winners from the closing lines + Hodes pick data.
#'
#' For each game (1-63), determine the winner. Primary method: a team that
#' appears in ANY entry's pick for the next round must have won. Fallback for
#' games where no one picked the winner for a later round: use the favorite
#' from closing lines (this can happen in Final or late rounds with few survivors).
#'
#' @param bracket data.frame with team_id column
#' @param cl_games data.frame from .load_year_data
#' @param entries data.frame parsed Hodes entries with bracket team_ids
#' @return Integer vector of length 63: winners[g] = team_id of game g winner
.determine_winners <- function(bracket, cl_games, entries) {
  n_teams <- nrow(bracket)
  winners <- rep(NA_integer_, 63)

  # Build set of teams that appear in picks at each round
  # entries has columns: R64_1_id, R64_2_id, R64_3_id, R32_1_id, ..., Final_id
  round_picks <- list()
  round_cols <- list(
    "2" = c("R32_1_id", "R32_2_id", "R32_3_id"),
    "3" = c("R16_1_id", "R16_2_id"),
    "4" = c("QF_id"),
    "5" = c("SF_id"),
    "6" = c("Final_id")
  )

  for (rn in names(round_cols)) {
    cols <- round_cols[[rn]]
    cols <- intersect(cols, names(entries))
    ids <- integer(0)
    for (col in cols) {
      vals <- entries[[col]]
      ids <- c(ids, vals[!is.na(vals)])
    }
    round_picks[[rn]] <- unique(ids)
  }

  # For R64 games (1-32): winner is whichever team appears in R32 picks
  # But also: the closing lines tell us the matchup. Each R64 game has
  # home_id vs away_id. The winner should appear as a pick in R32 for at
  # least one entry.
  for (gi in 1:32) {
    # Find the CL game for this bracket game
    # Game gi in R64: teams are (2*gi-1) and (2*gi)
    team_a <- 2L * gi - 1L
    team_b <- 2L * gi

    r32_teams <- round_picks[["2"]]
    if (team_a %in% r32_teams) {
      winners[gi] <- team_a
    } else if (team_b %in% r32_teams) {
      winners[gi] <- team_b
    } else {
      # Neither picked in R32 -- use closing line favorite
      cl_match <- cl_games[cl_games$round_num == 1 &
                           ((cl_games$home_id == team_a & cl_games$away_id == team_b) |
                            (cl_games$home_id == team_b & cl_games$away_id == team_a)), ]
      if (nrow(cl_match) > 0) {
        # Infer from R64 picks: whoever appears more often as an R64 pick won
        r64_teams <- c(entries$R64_1_id, entries$R64_2_id, entries$R64_3_id)
        r64_teams <- r64_teams[!is.na(r64_teams)]
        count_a <- sum(r64_teams == team_a)
        count_b <- sum(r64_teams == team_b)
        # Use favorite as tiebreaker
        if (count_a > 0 || count_b > 0) {
          # Can't determine from R64 picks alone, use CL game data
        }
        # For R64 we need actual results. Try: if team appears anywhere in entries
        # in ANY later round, it won R64.
        all_later <- unlist(round_picks, use.names = FALSE)
        if (team_a %in% all_later) {
          winners[gi] <- team_a
        } else if (team_b %in% all_later) {
          winners[gi] <- team_b
        } else {
          # True unknown -- use CL favorite
          if (nrow(cl_match) > 0) {
            if (cl_match$home_win_prob[1] >= 0.5) {
              winners[gi] <- cl_match$home_id[1]
            } else {
              winners[gi] <- cl_match$away_id[1]
            }
          }
        }
      }
    }
  }

  # R32 games (33-48): 16 games. Game 33 is teams {1,2,3,4} -> winner from {winner of g1, winner of g2}
  for (gi in 33:48) {
    offset <- gi - 32  # 1-16
    # The two feeder R64 games
    feeder_a <- 2 * offset - 1
    feeder_b <- 2 * offset
    team_a <- winners[feeder_a]
    team_b <- winners[feeder_b]

    s16_teams <- round_picks[["3"]]
    if (!is.na(team_a) && team_a %in% s16_teams) {
      winners[gi] <- team_a
    } else if (!is.na(team_b) && team_b %in% s16_teams) {
      winners[gi] <- team_b
    } else {
      # Check all later rounds
      all_later <- unlist(round_picks[as.character(3:6)], use.names = FALSE)
      if (!is.na(team_a) && team_a %in% all_later) {
        winners[gi] <- team_a
      } else if (!is.na(team_b) && team_b %in% all_later) {
        winners[gi] <- team_b
      } else {
        # Use CL favorite
        cl_match <- cl_games[cl_games$round_num == 2 &
                             ((!is.na(cl_games$home_id) & !is.na(team_a) & cl_games$home_id == team_a) |
                              (!is.na(cl_games$away_id) & !is.na(team_a) & cl_games$away_id == team_a) |
                              (!is.na(cl_games$home_id) & !is.na(team_b) & cl_games$home_id == team_b) |
                              (!is.na(cl_games$away_id) & !is.na(team_b) & cl_games$away_id == team_b)), ]
        if (nrow(cl_match) > 0) {
          if (cl_match$home_win_prob[1] >= 0.5) {
            winners[gi] <- cl_match$home_id[1]
          } else {
            winners[gi] <- cl_match$away_id[1]
          }
        }
      }
    }
  }

  # S16 games (49-56), E8 (57-60), FF (61-62), Final (63)
  later_rounds <- list(
    list(games = 49:56, feeder_base = 32, next_round = "4"),
    list(games = 57:60, feeder_base = 48, next_round = "5"),
    list(games = 61:62, feeder_base = 56, next_round = "6"),
    list(games = 63,     feeder_base = 60, next_round = NA)
  )

  for (lr in later_rounds) {
    for (gi in lr$games) {
      offset <- gi - lr$feeder_base
      feeder_a <- lr$feeder_base + 2 * offset - 1 - (lr$feeder_base - lr$feeder_base)
      # Actually: the two feeder games for game gi are at the previous round level.
      # For S16 game 49: feeders are R32 games 33 and 34
      # For S16 game 50: feeders are R32 games 35 and 36
      # Pattern: feeder_a = previous_base + 2*(offset)-1, feeder_b = previous_base + 2*offset
      feeder_a_idx <- lr$feeder_base + 2 * offset - 1
      feeder_b_idx <- lr$feeder_base + 2 * offset

      # But wait -- the feeder base calculation is wrong. Let me recalculate.
      # S16 games are 49-56 (8 games). Their feeders are R32 games 33-48 (16 games).
      # S16 game 49: feeders are R32 games 33, 34
      # S16 game 50: feeders are R32 games 35, 36
      # General: S16 game (48+k): feeders are R32 games (32 + 2k-1) and (32 + 2k)
      #
      # For round r, game gi, offset = gi - base[r]:
      # feeder_a = base[r-1] + 2*offset - 1
      # feeder_b = base[r-1] + 2*offset
      round_bases <- c(0, 32, 48, 56, 60, 62)
      round_num <- findInterval(gi, round_bases + 1)
      off <- gi - round_bases[round_num]
      fa <- round_bases[round_num - 1] + 2 * off - 1
      fb <- round_bases[round_num - 1] + 2 * off

      team_a <- if (fa >= 1 && fa <= 63) winners[fa] else NA_integer_
      team_b <- if (fb >= 1 && fb <= 63) winners[fb] else NA_integer_

      if (!is.na(lr$next_round)) {
        next_teams <- round_picks[[lr$next_round]]
        if (!is.na(team_a) && team_a %in% next_teams) {
          winners[gi] <- team_a
          next
        } else if (!is.na(team_b) && team_b %in% next_teams) {
          winners[gi] <- team_b
          next
        }
      }

      # Check ALL later rounds
      all_later_rounds <- character(0)
      if (!is.na(lr$next_round)) {
        rn_int <- as.integer(lr$next_round)
        all_later_rounds <- as.character(rn_int:6)
      }
      all_later <- unlist(round_picks[all_later_rounds], use.names = FALSE)

      if (!is.na(team_a) && team_a %in% all_later) {
        winners[gi] <- team_a
      } else if (!is.na(team_b) && team_b %in% all_later) {
        winners[gi] <- team_b
      } else {
        # For the championship game (gi=63), we need another method.
        # Check: if the final winner is one of our two teams, use that.
        # Otherwise: use CL favorite.
        cl_round <- round_num
        cl_match <- cl_games[cl_games$round_num == cl_round &
                             ((!is.na(team_a) & (cl_games$home_id == team_a | cl_games$away_id == team_a)) |
                              (!is.na(team_b) & (cl_games$home_id == team_b | cl_games$away_id == team_b))), ]
        if (nrow(cl_match) > 0) {
          if (cl_match$home_win_prob[1] >= 0.5) {
            winners[gi] <- cl_match$home_id[1]
          } else {
            winners[gi] <- cl_match$away_id[1]
          }
        }
      }
    }
  }

  winners
}


#' Parse Hodes entries from the Excel file for one year.
#'
#' @param year Integer
#' @param bracket data.frame with team, team_id columns
#' @return data.frame with one row per entry
.parse_hodes_entries <- function(year, bracket) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required. Install with install.packages('readxl')")
  }

  sheet_name <- as.character(year)
  raw <- readxl::read_excel(HODES_XLSX, sheet = sheet_name)

  # Standardise column names
  if (ncol(raw) != 14) {
    stop(sprintf("[%d] Expected 14 columns, got %d", year, ncol(raw)))
  }
  names(raw) <- HODES_COL_NAMES

  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  bracket_names <- bracket$team
  name_to_id <- setNames(bracket$team_id, bracket$team)
  name_to_seed <- setNames(bracket$seed, bracket$team)

  n <- nrow(raw)
  cat(sprintf("[%d] Parsing %d entries...\n", year, n))

  # Build result data.frame
  entries <- data.frame(
    year        = rep(year, n),
    entry_id    = seq_len(n),
    entry_name  = raw$Entry,
    rank        = as.integer(raw$Rank),
    R64_1       = character(n), R64_2 = character(n), R64_3 = character(n),
    R32_1       = character(n), R32_2 = character(n), R32_3 = character(n),
    R16_1       = character(n), R16_2 = character(n),
    QF          = character(n), SF = character(n), Final = character(n),
    R64_1_id = NA_integer_, R64_2_id = NA_integer_, R64_3_id = NA_integer_,
    R32_1_id = NA_integer_, R32_2_id = NA_integer_, R32_3_id = NA_integer_,
    R16_1_id = NA_integer_, R16_2_id = NA_integer_,
    QF_id = NA_integer_, SF_id = NA_integer_, Final_id = NA_integer_,
    tiebreak_seed_sum = as.numeric(raw$TiebreakSeedSum),
    stringsAsFactors = FALSE
  )

  pick_cols <- c("R64_1", "R64_2", "R64_3", "R32_1", "R32_2", "R32_3",
                 "R16_1", "R16_2", "QF", "SF", "Final")
  raw_cols  <- c("R64_1", "R64_2", "R64_3", "R32_1", "R32_2", "R32_3",
                 "R16_1", "R16_2", "QF", "SF", "Final")

  unmatched_names <- character(0)

  for (i in seq_len(n)) {
    for (j in seq_along(pick_cols)) {
      val <- as.character(raw[[raw_cols[j]]][i])
      if (is.na(val) || val == "" || val == "NA" || val == "No Pick" ||
          val == "0.0" || val == "0") {
        entries[[pick_cols[j]]][i] <- NA_character_
        next
      }

      matched <- .match_hodes_to_bracket(val, bracket_names)
      entries[[pick_cols[j]]][i] <- matched
      id_col <- paste0(pick_cols[j], "_id")
      if (!is.na(matched)) {
        entries[[id_col]][i] <- name_to_id[[matched]]
      } else {
        unmatched_names <- c(unmatched_names, val)
      }
    }
  }

  if (length(unmatched_names) > 0) {
    uu <- sort(table(unmatched_names), decreasing = TRUE)
    cat(sprintf("[%d] WARNING: %d unmatched name occurrences (%d unique):\n",
                year, sum(uu), length(uu)))
    for (nm in names(uu)) {
      cat(sprintf("    '%s' (%d times)\n", nm, uu[[nm]]))
    }
  }

  # Determine n_s16_picks: 1 if R16_2 is NA, 2 otherwise
  entries$n_s16_picks <- ifelse(is.na(entries$R16_2_id), 1L, 2L)
  # But some entries may not have made it to S16 at all
  entries$n_s16_picks[is.na(entries$R16_1_id)] <- 0L

  cat(sprintf("[%d] Parsed %d entries. S16 pick distribution: %s\n",
              year, n,
              paste(sprintf("%d=%d", 0:2,
                            c(sum(entries$n_s16_picks == 0),
                              sum(entries$n_s16_picks == 1),
                              sum(entries$n_s16_picks == 2))),
                    collapse = ", ")))

  entries
}


# ==============================================================================
# CHAMPIONSHIP EQUITY (Monte Carlo)
# ==============================================================================

#' Compute KenPom win probability between two teams.
#'
#' @param rating_a Numeric NetRtg for team A
#' @param rating_b Numeric NetRtg for team B
#' @param k Logistic scale parameter (default KENPOM_K)
#' @return P(A beats B)
.kenpom_wp <- function(rating_a, rating_b, k = KENPOM_K) {
  1 / (1 + exp(-k * (rating_a - rating_b)))
}


#' Build a KenPom rating lookup from bracket + kenpom data.
#'
#' @param bracket data.frame with team column
#' @param kenpom data.frame with Team, NetRtg columns
#' @return Named numeric vector: bracket_name -> NetRtg
.build_kenpom_lookup <- function(bracket, kenpom) {
  # Use the cl_to_kp alias approach from calibrate_win_prob.R but adapted
  # for bracket names
  kp_names <- kenpom$Team
  kp_ratings <- setNames(kenpom$NetRtg, kenpom$Team)

  # Build bracket_name -> kenpom_name mapping
  kp_to_bracket <- c(
    "Connecticut"    = "UConn",
    "Mississippi"    = "Ole Miss",
    "N.C. State"     = "NC State",
    "McNeese St."    = "McNeese State",
    "Grambling St."  = "Grambling State",
    "Long Beach St." = "Long Beach State",
    "Miami FL"       = "Miami FL",
    "Saint Peter's"  = "Saint Peter's",
    "SIUE"           = "SIU Edwardsville",
    "Fairleigh Dickinson" = "Fairleigh Dickinson",
    "Mount St. Mary's" = "Mount St. Mary's",
    "Nebraska Omaha" = "Omaha",
    "Cal St. Fullerton" = "Cal State Fullerton",
    "Charleston"     = "College of Charleston",
    "Col. of Charleston" = "College of Charleston"
  )

  result <- setNames(rep(NA_real_, nrow(bracket)), bracket$team)

  for (i in seq_len(nrow(bracket))) {
    bn <- bracket$team[i]

    # Direct match
    if (bn %in% kp_names) {
      result[bn] <- kp_ratings[[bn]]
      next
    }

    # Check reverse alias (kenpom -> bracket)
    for (kpn in names(kp_to_bracket)) {
      if (kp_to_bracket[[kpn]] == bn && kpn %in% kp_names) {
        result[bn] <- kp_ratings[[kpn]]
        break
      }
    }
    if (!is.na(result[bn])) next

    # Normalize: remove periods, lowercase prefix matching
    bn_norm <- tolower(gsub("\\.", "", bn))
    kp_norms <- tolower(gsub("\\.", "", kp_names))

    for (ki in order(nchar(kp_norms), decreasing = TRUE)) {
      kn <- kp_norms[ki]
      if (bn_norm == kn ||
          (startsWith(bn_norm, kn) && nchar(bn_norm) > nchar(kn) &&
           substr(bn_norm, nchar(kn) + 1, nchar(kn) + 1) == " ") ||
          (startsWith(kn, bn_norm) && nchar(kn) > nchar(bn_norm) &&
           substr(kn, nchar(bn_norm) + 1, nchar(bn_norm) + 1) == " ")) {
        result[bn] <- kp_ratings[[kp_names[ki]]]
        break
      }
    }

    # Last resort: fuzzy via abbreviation expansion
    if (is.na(result[bn])) {
      bn_state <- sub(" State$", " St.", bn)
      bn_st <- sub(" St\\.$", " State", bn)
      for (candidate in c(bn_state, bn_st)) {
        cand_norm <- tolower(gsub("\\.", "", candidate))
        idx <- which(kp_norms == cand_norm)
        if (length(idx) > 0) {
          result[bn] <- kp_ratings[[kp_names[idx[1]]]]
          break
        }
      }
    }
  }

  missing <- bracket$team[is.na(result)]
  if (length(missing) > 0) {
    warning(sprintf("No KenPom rating found for: %s",
                    paste(missing, collapse = ", ")))
    # Assign median rating for missing teams (likely 16 seeds)
    med_rating <- median(result, na.rm = TRUE)
    result[is.na(result)] <- med_rating - 15  # assume weak
  }

  result
}


#' Compute championship equity for every team at the start of a given round.
#'
#' Uses Monte Carlo simulation. For games already played, uses known winners.
#' For the current round and beyond, uses win probabilities from closing lines
#' (current round) and KenPom (future rounds).
#'
#' @param bracket data.frame with team, team_id, seed, region columns
#' @param cl_games data.frame of closing-line games with round_num, home_id, away_id, home_win_prob
#' @param kenpom_lookup Named numeric vector (bracket_name -> NetRtg)
#' @param winners Integer vector of length 63 (game winners, NA for unplayed)
#' @param round_num Integer round at which to compute equity (3=S16, 4=E8, etc.)
#' @param n_sims Integer number of MC simulations (default 10000)
#' @return Named numeric vector: bracket_name -> P(win championship)
compute_champ_equity_at_round <- function(bracket, cl_games, kenpom_lookup,
                                           winners, round_num, n_sims = 10000) {
  n_teams <- nrow(bracket)
  round_bases <- c(0L, 32L, 48L, 56L, 60L, 62L)

  # Build win probability lookup for each possible game
  # For games with closing lines, use them. For hypotheticals, use KenPom.
  # Key: "team_a_id-team_b_id" -> P(a beats b)

  # Build CL lookup: for each CL game, store wp
  cl_wp <- list()
  for (i in seq_len(nrow(cl_games))) {
    hid <- cl_games$home_id[i]
    aid <- cl_games$away_id[i]
    wp <- cl_games$home_win_prob[i]
    if (!is.na(hid) && !is.na(aid)) {
      cl_wp[[paste(hid, aid, sep = "-")]] <- wp
      cl_wp[[paste(aid, hid, sep = "-")]] <- 1 - wp
    }
  }

  # Get win prob for a matchup
  get_wp <- function(id_a, id_b, game_round) {
    key <- paste(id_a, id_b, sep = "-")
    if (!is.null(cl_wp[[key]]) && game_round <= round_num) {
      return(cl_wp[[key]])
    }
    # Use KenPom
    name_a <- bracket$team[id_a]
    name_b <- bracket$team[id_b]
    ra <- kenpom_lookup[name_a]
    rb <- kenpom_lookup[name_b]
    if (is.na(ra) || is.na(rb)) return(0.5)
    .kenpom_wp(ra, rb)
  }

  # Determine alive teams at start of round_num
  # A team is alive if it won all games through round_num - 1
  alive <- rep(TRUE, n_teams)
  for (r in 1:(round_num - 1)) {
    n_games_r <- n_teams / (2^r)
    for (g in 1:n_games_r) {
      game_idx <- round_bases[r] + g
      w <- winners[game_idx]
      if (is.na(w)) next
      # The loser of this game: determine who played
      block_size <- 2^r
      game_offset <- g
      game_start <- (game_offset - 1) * block_size + 1
      game_end <- game_offset * block_size
      team_ids_in_game <- game_start:game_end
      # All teams in this block that are not in the winner's half are eliminated
      # Actually, simpler: the loser is the other team that was alive going into this game
      # But we don't track that easily. Instead, set alive=FALSE for all teams in this
      # block EXCEPT those that could descend from the winner's path.
    }
  }

  # Simpler approach: build the "surviving team" for each bracket slot after
  # completed rounds. For each region of 2^round_num teams, determine the survivor.
  block_size <- 2^(round_num - 1)
  n_survivors <- n_teams / block_size
  survivors <- rep(NA_integer_, n_survivors)

  for (s in 1:n_survivors) {
    # The game that determines survivor s is the last game in rounds 1...(round_num-1)
    # for this block of teams
    # The block is team_ids: ((s-1)*block_size+1) to (s*block_size)
    # The final game for this block in round (round_num-1):
    if (round_num == 1) {
      # Everyone is alive
      survivors[s] <- (s - 1) * block_size + 1  # placeholder, handled below
    } else {
      last_game <- round_bases[round_num - 1] + s
      survivors[s] <- winners[last_game]
    }
  }

  if (round_num == 1) {
    survivors <- 1:n_teams
  }

  # MC simulation from round_num onward
  champ_count <- setNames(rep(0L, n_teams), bracket$team)

  for (sim in 1:n_sims) {
    # Current alive teams
    current <- survivors  # team_ids still in the tournament

    for (r in round_num:6) {
      n_games <- length(current) / 2
      if (n_games < 1) break
      next_round <- integer(n_games)

      for (g in 1:n_games) {
        id_a <- current[2 * g - 1]
        id_b <- current[2 * g]

        if (is.na(id_a) && is.na(id_b)) {
          next_round[g] <- NA_integer_
          next
        }
        if (is.na(id_a)) { next_round[g] <- id_b; next }
        if (is.na(id_b)) { next_round[g] <- id_a; next }

        wp <- get_wp(id_a, id_b, r)
        if (runif(1) < wp) {
          next_round[g] <- id_a
        } else {
          next_round[g] <- id_b
        }
      }
      current <- next_round
    }

    # Champion
    if (length(current) == 1 && !is.na(current[1])) {
      champ_name <- bracket$team[current[1]]
      champ_count[champ_name] <- champ_count[champ_name] + 1L
    }
  }

  champ_equity <- champ_count / n_sims
  champ_equity
}


# ==============================================================================
# ALIVE STATUS DETERMINATION
# ==============================================================================

#' Determine whether each entry is alive at each round.
#'
#' @param entries data.frame from .parse_hodes_entries
#' @param winners Integer vector of length 63 (game winners)
#' @param bracket data.frame
#' @return entries data.frame with added columns: alive_at_R32, alive_at_S16,
#'   alive_at_E8, alive_at_FF, alive_at_Final
.compute_alive_status <- function(entries, winners, bracket) {
  n <- nrow(entries)

  # SIMPLE APPROACH: use the Hodes data directly.
  # If an entry has a non-empty pick in round N, they were alive at round N.
  # This is more reliable than checking game results (which depends on correct ID mapping).

  # alive_at_R32: entry has at least one non-NA R32 pick
  entries$alive_at_R32 <- !is.na(entries$R32_1_id)

  # alive_at_S16: entry has at least one non-NA R16 pick
  entries$alive_at_S16 <- !is.na(entries$R16_1_id) | !is.na(entries$R16_2_id)

  # alive_at_E8: entry has a non-NA QF pick
  entries$alive_at_E8 <- !is.na(entries$QF_id)

  # alive_at_FF: entry has a non-NA SF pick
  entries$alive_at_FF <- !is.na(entries$SF_id)

  # alive_at_Final: entry has a non-NA Final pick
  entries$alive_at_Final <- !is.na(entries$Final_id)

  entries
}


# ==============================================================================
# BUILD CALIBRATION DATASETS
# ==============================================================================

#' Build the S16 pick-count calibration dataset.
#'
#' For each entry alive at S16: did they pick 1 or 2 teams?
#'
#' @param entries data.frame with alive_at_S16, n_s16_picks, etc.
#' @param bracket data.frame
#' @param winners Integer vector of length 63
#' @return data.frame for calibration
.build_s16_count_calib <- function(entries, bracket, winners) {
  alive <- entries[entries$alive_at_S16, ]
  if (nrow(alive) == 0) return(data.frame())

  n_alive <- nrow(alive)

  # Tiebreaker rank percentile
  tb_vals <- alive$tiebreak_seed_sum
  # Higher seed sum = better. Rank percentile: 1 = best, 0 = worst.
  tb_rank <- rank(tb_vals, ties.method = "average")
  tb_rank_pct <- (tb_rank - 1) / max(n_alive - 1, 1)

  # For each entry, count how many S16 teams they haven't used
  s16_winners <- winners[33:48]  # R32 winners = S16 participants
  s16_teams <- unique(s16_winners[!is.na(s16_winners)])

  n_available <- integer(nrow(alive))
  for (i in seq_len(nrow(alive))) {
    used <- c(alive$R64_1_id[i], alive$R64_2_id[i], alive$R64_3_id[i],
              alive$R32_1_id[i], alive$R32_2_id[i], alive$R32_3_id[i])
    used <- used[!is.na(used)]
    available <- setdiff(s16_teams, used)
    n_available[i] <- length(available)
  }

  data.frame(
    year             = alive$year,
    entry_id         = alive$entry_id,
    took_two         = as.integer(alive$n_s16_picks == 2),
    n_alive          = n_alive,
    tb_rank_pct      = tb_rank_pct,
    n_available_s16  = n_available,
    stringsAsFactors = FALSE
  )
}


#' Build S16 team-selection calibration dataset.
#'
#' For each entry alive at S16: what teams did they pick, and what were the
#' characteristics of all available S16 teams?
#'
#' @param entries data.frame with alive_at_S16 etc.
#' @param bracket data.frame
#' @param winners Integer vector of length 63
#' @param cl_games data.frame of closing-line games
#' @param champ_equity Named numeric vector from compute_champ_equity_at_round
#' @return data.frame with one row per (entry x candidate_team)
.build_s16_team_calib <- function(entries, bracket, winners, cl_games,
                                   champ_equity) {
  alive <- entries[entries$alive_at_S16, ]
  if (nrow(alive) == 0) return(data.frame())

  name_to_id <- setNames(bracket$team_id, bracket$team)
  id_to_name <- setNames(bracket$team, bracket$team_id)
  id_to_seed <- setNames(bracket$seed, bracket$team_id)

  # S16 participants: winners of R32
  s16_game_ids <- 33:48
  s16_participants <- winners[s16_game_ids]
  s16_participants <- s16_participants[!is.na(s16_participants)]

  # For each S16 game (49-56), identify matchup and closing-line WP
  s16_matchups <- data.frame(
    game_idx = 49:56,
    team_a = NA_integer_, team_b = NA_integer_,
    wp_a = NA_real_,
    stringsAsFactors = FALSE
  )
  for (g in 1:8) {
    gi <- 48 + g
    fa <- 32 + 2 * g - 1  # feeder R32 game A
    fb <- 32 + 2 * g      # feeder R32 game B
    s16_matchups$team_a[g] <- winners[fa]
    s16_matchups$team_b[g] <- winners[fb]

    # Find CL game for this matchup
    ta <- winners[fa]
    tb <- winners[fb]
    if (!is.na(ta) && !is.na(tb)) {
      cl_match <- cl_games[cl_games$round_num == 3 &
                           ((cl_games$home_id == ta & cl_games$away_id == tb) |
                            (cl_games$home_id == tb & cl_games$away_id == ta)), ]
      if (nrow(cl_match) > 0) {
        if (cl_match$home_id[1] == ta) {
          s16_matchups$wp_a[g] <- cl_match$home_win_prob[1]
        } else {
          s16_matchups$wp_a[g] <- 1 - cl_match$home_win_prob[1]
        }
      }
    }
  }

  # Build candidate rows
  rows <- list()
  row_idx <- 0

  for (i in seq_len(nrow(alive))) {
    used_ids <- c(alive$R64_1_id[i], alive$R64_2_id[i], alive$R64_3_id[i],
                  alive$R32_1_id[i], alive$R32_2_id[i], alive$R32_3_id[i])
    used_ids <- used_ids[!is.na(used_ids)]

    actual_pick_ids <- c(alive$R16_1_id[i], alive$R16_2_id[i])
    actual_pick_ids <- actual_pick_ids[!is.na(actual_pick_ids)]
    n_picks <- length(actual_pick_ids)

    # All available S16 teams (not already used)
    available <- setdiff(s16_participants, used_ids)

    for (tid in available) {
      tname <- id_to_name[as.character(tid)]
      tseed <- as.integer(id_to_seed[as.character(tid)])

      # Find this team's S16 matchup
      matchup_row <- which(
        (!is.na(s16_matchups$team_a) & s16_matchups$team_a == tid) |
        (!is.na(s16_matchups$team_b) & s16_matchups$team_b == tid)
      )
      wp <- NA_real_
      opp_id <- NA_integer_
      if (length(matchup_row) > 0) {
        mr <- matchup_row[1]
        if (!is.na(s16_matchups$team_a[mr]) && s16_matchups$team_a[mr] == tid) {
          wp <- s16_matchups$wp_a[mr]
          opp_id <- s16_matchups$team_b[mr]
        } else {
          wp <- if (!is.na(s16_matchups$wp_a[mr])) 1 - s16_matchups$wp_a[mr] else NA_real_
          opp_id <- s16_matchups$team_a[mr]
        }
      }

      ce <- if (!is.na(tname) && tname %in% names(champ_equity)) champ_equity[[tname]] else 0
      opp_used <- as.integer(!is.na(opp_id) && opp_id %in% used_ids)

      row_idx <- row_idx + 1
      rows[[row_idx]] <- data.frame(
        year           = alive$year[i],
        entry_id       = alive$entry_id[i],
        n_picks        = n_picks,
        candidate_id   = tid,
        candidate_name = as.character(tname),
        was_picked     = as.integer(tid %in% actual_pick_ids),
        wp             = wp,
        champ_equity   = ce,
        seed           = tseed,
        opponent_is_used = opp_used,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}


#' Build E8 team-selection calibration dataset.
#'
#' Same structure as S16 but for E8 (always 1 pick).
#'
#' @param entries data.frame with alive_at_E8 etc.
#' @param bracket data.frame
#' @param winners Integer vector of length 63
#' @param cl_games data.frame
#' @param champ_equity Named numeric vector at E8 start
#' @return data.frame with one row per (entry x candidate_team)
.build_e8_team_calib <- function(entries, bracket, winners, cl_games,
                                  champ_equity) {
  alive <- entries[entries$alive_at_E8, ]
  if (nrow(alive) == 0) return(data.frame())

  name_to_id <- setNames(bracket$team_id, bracket$team)
  id_to_name <- setNames(bracket$team, bracket$team_id)
  id_to_seed <- setNames(bracket$seed, bracket$team_id)

  # E8 participants: winners of S16
  e8_participants <- winners[49:56]
  e8_participants <- e8_participants[!is.na(e8_participants)]

  # E8 matchups (games 57-60)
  e8_matchups <- data.frame(
    game_idx = 57:60,
    team_a = NA_integer_, team_b = NA_integer_,
    wp_a = NA_real_,
    stringsAsFactors = FALSE
  )
  for (g in 1:4) {
    gi <- 56 + g
    fa <- 48 + 2 * g - 1
    fb <- 48 + 2 * g
    e8_matchups$team_a[g] <- winners[fa]
    e8_matchups$team_b[g] <- winners[fb]

    ta <- winners[fa]
    tb <- winners[fb]
    if (!is.na(ta) && !is.na(tb)) {
      cl_match <- cl_games[cl_games$round_num == 4 &
                           ((cl_games$home_id == ta & cl_games$away_id == tb) |
                            (cl_games$home_id == tb & cl_games$away_id == ta)), ]
      if (nrow(cl_match) > 0) {
        if (cl_match$home_id[1] == ta) {
          e8_matchups$wp_a[g] <- cl_match$home_win_prob[1]
        } else {
          e8_matchups$wp_a[g] <- 1 - cl_match$home_win_prob[1]
        }
      }
    }
  }

  rows <- list()
  row_idx <- 0

  for (i in seq_len(nrow(alive))) {
    used_ids <- c(alive$R64_1_id[i], alive$R64_2_id[i], alive$R64_3_id[i],
                  alive$R32_1_id[i], alive$R32_2_id[i], alive$R32_3_id[i],
                  alive$R16_1_id[i], alive$R16_2_id[i])
    used_ids <- used_ids[!is.na(used_ids)]

    actual_pick_id <- alive$QF_id[i]
    available <- setdiff(e8_participants, used_ids)

    for (tid in available) {
      tname <- id_to_name[as.character(tid)]
      tseed <- as.integer(id_to_seed[as.character(tid)])

      matchup_row <- which(
        (!is.na(e8_matchups$team_a) & e8_matchups$team_a == tid) |
        (!is.na(e8_matchups$team_b) & e8_matchups$team_b == tid)
      )
      wp <- NA_real_
      opp_id <- NA_integer_
      if (length(matchup_row) > 0) {
        mr <- matchup_row[1]
        if (!is.na(e8_matchups$team_a[mr]) && e8_matchups$team_a[mr] == tid) {
          wp <- e8_matchups$wp_a[mr]
          opp_id <- e8_matchups$team_b[mr]
        } else {
          wp <- if (!is.na(e8_matchups$wp_a[mr])) 1 - e8_matchups$wp_a[mr] else NA_real_
          opp_id <- e8_matchups$team_a[mr]
        }
      }

      ce <- if (!is.na(tname) && tname %in% names(champ_equity)) champ_equity[[tname]] else 0
      opp_used <- as.integer(!is.na(opp_id) && opp_id %in% used_ids)

      row_idx <- row_idx + 1
      rows[[row_idx]] <- data.frame(
        year           = alive$year[i],
        entry_id       = alive$entry_id[i],
        n_picks        = 1L,
        candidate_id   = tid,
        candidate_name = as.character(tname),
        was_picked     = as.integer(!is.na(actual_pick_id) && tid == actual_pick_id),
        wp             = wp,
        champ_equity   = ce,
        seed           = tseed,
        opponent_is_used = opp_used,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}


# ==============================================================================
# ENTRY POINTS
# ==============================================================================

#' Analyze a single year of Hodes results.
#'
#' @param year Integer (2021-2025)
#' @param n_sims Integer number of MC sims for champ equity (default 10000)
#' @return List with: s16_count_calib, s16_team_calib, e8_team_calib, summary_stats
analyze_hodes_year <- function(year, n_sims = 10000) {
  cat(sprintf("\n%s\n  ANALYZING HODES YEAR %d\n%s\n",
              paste(rep("=", 60), collapse = ""), year,
              paste(rep("=", 60), collapse = "")))

  # 1. Load all data
  cat("Loading data...\n")
  yd <- .load_year_data(year)
  bracket   <- yd$bracket
  cl_games  <- yd$cl_games
  kp_lookup <- .build_kenpom_lookup(bracket, yd$kenpom)

  cat(sprintf("  Bracket: %d teams\n", nrow(bracket)))
  cat(sprintf("  Closing lines: %d games matched\n", nrow(cl_games)))
  cat(sprintf("  KenPom: %d/%d teams rated\n",
              sum(!is.na(kp_lookup)), length(kp_lookup)))

  # 2. Parse entries
  entries <- .parse_hodes_entries(year, bracket)

  # 3. Determine actual winners
  cat("Determining actual tournament results...\n")
  winners <- .determine_winners(bracket, cl_games, entries)
  n_determined <- sum(!is.na(winners))
  cat(sprintf("  Determined %d/63 game winners\n", n_determined))

  # Quick sanity check: print S16 matchups
  cat("  S16 teams: ")
  s16_ids <- winners[33:48]
  s16_names <- bracket$team[s16_ids[!is.na(s16_ids)]]
  cat(paste(s16_names, collapse = ", "), "\n")

  # 4. Compute alive status
  entries <- .compute_alive_status(entries, winners, bracket)
  cat(sprintf("  Alive at R32: %d\n", sum(entries$alive_at_R32)))
  cat(sprintf("  Alive at S16: %d\n", sum(entries$alive_at_S16)))
  cat(sprintf("  Alive at E8:  %d\n", sum(entries$alive_at_E8)))
  cat(sprintf("  Alive at FF:  %d\n", sum(entries$alive_at_FF)))
  cat(sprintf("  Alive at Final: %d\n", sum(entries$alive_at_Final)))

  # 5. Compute championship equity at S16 and E8
  cat("Computing championship equity at S16 start...\n")
  set.seed(year * 100 + 3)
  champ_equity_s16 <- compute_champ_equity_at_round(
    bracket, cl_games, kp_lookup, winners, round_num = 3, n_sims = n_sims
  )

  cat("Computing championship equity at E8 start...\n")
  set.seed(year * 100 + 4)
  champ_equity_e8 <- compute_champ_equity_at_round(
    bracket, cl_games, kp_lookup, winners, round_num = 4, n_sims = n_sims
  )

  # 6. Build calibration datasets
  cat("Building S16 pick-count calibration data...\n")
  s16_count <- .build_s16_count_calib(entries, bracket, winners)
  cat(sprintf("  %d rows (%.1f%% took two)\n",
              nrow(s16_count),
              if (nrow(s16_count) > 0) 100 * mean(s16_count$took_two) else 0))

  cat("Building S16 team-selection calibration data...\n")
  s16_team <- .build_s16_team_calib(entries, bracket, winners, cl_games,
                                     champ_equity_s16)
  cat(sprintf("  %d candidate rows for %d entries\n",
              nrow(s16_team),
              length(unique(s16_team$entry_id))))

  cat("Building E8 team-selection calibration data...\n")
  e8_team <- .build_e8_team_calib(entries, bracket, winners, cl_games,
                                   champ_equity_e8)
  cat(sprintf("  %d candidate rows for %d entries\n",
              nrow(e8_team),
              length(unique(e8_team$entry_id))))

  # 7. Summary stats
  summary_stats <- data.frame(
    year           = year,
    n_entries      = nrow(entries),
    alive_at_R32   = sum(entries$alive_at_R32),
    alive_at_S16   = sum(entries$alive_at_S16),
    alive_at_E8    = sum(entries$alive_at_E8),
    alive_at_FF    = sum(entries$alive_at_FF),
    alive_at_Final = sum(entries$alive_at_Final),
    pct_took_two_s16 = if (sum(entries$alive_at_S16) > 0)
                         mean(entries$n_s16_picks[entries$alive_at_S16] == 2) else NA,
    stringsAsFactors = FALSE
  )

  cat(sprintf("\n[%d] Done.\n", year))


  list(
    s16_count_calib = s16_count,
    s16_team_calib  = s16_team,
    e8_team_calib   = e8_team,
    summary_stats   = summary_stats,
    entries         = entries,
    winners         = winners,
    bracket         = bracket
  )
}


#' Analyze all Hodes years (2021-2025) and combine calibration datasets.
#'
#' @param years Integer vector of years to process (default 2021:2025)
#' @param n_sims Integer number of MC sims per year (default 10000)
#' @return List with combined: s16_count_calib, s16_team_calib, e8_team_calib,
#'   summary_stats, and per_year (list of individual year results)
analyze_all_hodes_years <- function(years = HODES_YEARS, n_sims = 10000) {
  cat(sprintf("\n%s\n  HODES HISTORICAL ANALYSIS: %d-%d\n%s\n\n",
              paste(rep("=", 60), collapse = ""),
              min(years), max(years),
              paste(rep("=", 60), collapse = "")))

  per_year <- list()
  all_s16_count <- list()
  all_s16_team  <- list()
  all_e8_team   <- list()
  all_summary   <- list()

  for (yr in years) {
    result <- tryCatch(
      analyze_hodes_year(yr, n_sims = n_sims),
      error = function(e) {
        cat(sprintf("\n[%d] ERROR: %s\n", yr, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(result)) next

    per_year[[as.character(yr)]] <- result
    all_s16_count[[as.character(yr)]] <- result$s16_count_calib
    all_s16_team[[as.character(yr)]]  <- result$s16_team_calib
    all_e8_team[[as.character(yr)]]   <- result$e8_team_calib
    all_summary[[as.character(yr)]]   <- result$summary_stats
  }

  # Combine
  s16_count_combined <- do.call(rbind, all_s16_count)
  s16_team_combined  <- do.call(rbind, all_s16_team)
  e8_team_combined   <- do.call(rbind, all_e8_team)
  summary_combined   <- do.call(rbind, all_summary)

  # Reset rownames
  if (!is.null(s16_count_combined)) rownames(s16_count_combined) <- NULL
  if (!is.null(s16_team_combined))  rownames(s16_team_combined)  <- NULL
  if (!is.null(e8_team_combined))   rownames(e8_team_combined)   <- NULL
  if (!is.null(summary_combined))   rownames(summary_combined)   <- NULL

  cat(sprintf("\n%s\n  COMBINED RESULTS\n%s\n", paste(rep("=", 60), collapse = ""),
              paste(rep("=", 60), collapse = "")))
  cat(sprintf("  Years processed: %d\n", length(per_year)))
  cat(sprintf("  S16 count calibration: %d rows\n", nrow(s16_count_combined)))
  cat(sprintf("  S16 team calibration:  %d rows\n", nrow(s16_team_combined)))
  cat(sprintf("  E8 team calibration:   %d rows\n", nrow(e8_team_combined)))

  if (!is.null(summary_combined) && nrow(summary_combined) > 0) {
    cat("\n  Year-by-year summary:\n")
    cat(sprintf("  %-6s %6s %6s %6s %6s %6s %6s %8s\n",
                "Year", "Entries", "R32", "S16", "E8", "FF", "Final", "%Two_S16"))
    cat(sprintf("  %-6s %6s %6s %6s %6s %6s %6s %8s\n",
                "------", "------", "------", "------", "------", "------", "------", "--------"))
    for (i in seq_len(nrow(summary_combined))) {
      s <- summary_combined[i, ]
      cat(sprintf("  %-6d %6d %6d %6d %6d %6d %6d %7.1f%%\n",
                  s$year, s$n_entries, s$alive_at_R32, s$alive_at_S16,
                  s$alive_at_E8, s$alive_at_FF, s$alive_at_Final,
                  100 * s$pct_took_two_s16))
    }

    if (!is.null(s16_count_combined) && nrow(s16_count_combined) > 0) {
      cat(sprintf("\n  Overall S16 two-pick rate: %.1f%% (%d / %d)\n",
                  100 * mean(s16_count_combined$took_two),
                  sum(s16_count_combined$took_two),
                  nrow(s16_count_combined)))
    }
  }

  list(
    s16_count_calib = s16_count_combined,
    s16_team_calib  = s16_team_combined,
    e8_team_calib   = e8_team_combined,
    summary_stats   = summary_combined,
    per_year        = per_year
  )
}


cat("analyze_hodes_historical.R loaded.\n")
cat("  analyze_hodes_year(year)       -- single year analysis\n")
cat("  analyze_all_hodes_years()      -- all years (2021-2025)\n")
