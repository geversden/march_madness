#!/usr/bin/env Rscript
# ==============================================================================
# get_closing_lines_ncaat.R
# Fetch closing betting lines for NCAA Tournament games from the-odds-api.com
#
# Outputs per game: home_team, away_team, date, home_spread, home_win_prob
#
# Usage:  Rscript get_closing_lines_ncaat.R [YEAR]
#         Rscript get_closing_lines_ncaat.R 2025
#         Rscript get_closing_lines_ncaat.R all
# ==============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

ODDS_API_KEY <- Sys.getenv("ODDS_API_KEY")
if (ODDS_API_KEY == "") stop("Set the ODDS_API_KEY environment variable before running.")
SPORT        <- "basketball_ncaab"
BOOKMAKER    <- "betonlineag"

# ==============================================================================
# HELPERS
# ==============================================================================

null_na  <- function(x) if (is.null(x)) NA else x
to_list  <- function(x) if (is.null(x)) list() else x

# American odds -> raw implied probability (includes vig)
american_to_prob <- function(odds) {
  if (is.na(odds)) return(NA_real_)
  if (odds < 0) abs(odds) / (abs(odds) + 100)
  else 100 / (odds + 100)
}

# ==============================================================================
# TOURNAMENT GAME DATES BY YEAR
# First Four through Championship — adjust if any are off
# ==============================================================================

tourney_dates <- list(
  "2021" = as.Date(c(
    "2021-03-18",
    "2021-03-19", "2021-03-20", "2021-03-21", "2021-03-22",
    "2021-03-27", "2021-03-28", "2021-03-29", "2021-03-30",
    "2021-04-03", "2021-04-05"
  )),
  "2022" = as.Date(c(
    "2022-03-15", "2022-03-16",
    "2022-03-17", "2022-03-18", "2022-03-19", "2022-03-20",
    "2022-03-24", "2022-03-25", "2022-03-26", "2022-03-27",
    "2022-04-02", "2022-04-04"
  )),
  "2023" = as.Date(c(
    "2023-03-14", "2023-03-15",
    "2023-03-16", "2023-03-17", "2023-03-18", "2023-03-19",
    "2023-03-23", "2023-03-24", "2023-03-25", "2023-03-26",
    "2023-04-01", "2023-04-03"
  )),
  "2024" = as.Date(c(
    "2024-03-19", "2024-03-20",
    "2024-03-21", "2024-03-22", "2024-03-23", "2024-03-24",
    "2024-03-28", "2024-03-29", "2024-03-30", "2024-03-31",
    "2024-04-06", "2024-04-08"
  )),
  "2025" = as.Date(c(
    "2025-03-18", "2025-03-19",
    "2025-03-20", "2025-03-21", "2025-03-22", "2025-03-23",
    "2025-03-27", "2025-03-28", "2025-03-29", "2025-03-30",
    "2025-04-05", "2025-04-07", "2025-04-08"
  ))
)

# ==============================================================================
# NAME MAPPING: odds API name -> bracket CSV name
# The odds API may return slightly different team names than our bracket CSVs.
# Extend this as needed after inspecting unmatched teams in the output.
# ==============================================================================

odds_to_bracket <- c(
  # UConn
  "Connecticut Huskies" = "UConn",
  "UConn Huskies"       = "UConn",
  "Connecticut"         = "UConn",

  # Ole Miss
  "Ole Miss Rebels"     = "Ole Miss",
  "Mississippi"         = "Ole Miss",

  # "St" vs "State" abbreviations (API uses "St" without period)
  "Michigan St Spartans"     = "Michigan State",
  "Mississippi St Bulldogs"  = "Mississippi State",
  "Iowa St Cyclones"         = "Iowa State",
  "Utah St Aggies"           = "Utah State",
  "Colorado St Rams"         = "Colorado State",
  "Oklahoma St Cowboys"      = "Oklahoma State",
  "Ohio St Buckeyes"         = "Ohio State",
  "Florida St Seminoles"     = "Florida State",
  "Murray St Racers"         = "Murray State",
  "Norfolk St Spartans"      = "Norfolk State",
  "Alabama St Hornets"       = "Alabama State",
  "Georgia St Panthers"      = "Georgia State",
  "Washington St Cougars"    = "Washington State",
  "Jacksonville St Gamecocks"= "Jacksonville State",
  "McNeese St Cowboys"       = "McNeese State",
  "McNeese Cowboys"          = "McNeese State",
  "Morehead St Eagles"       = "Morehead State",
  "Grambling St Tigers"      = "Grambling State",
  "Kennesaw St Owls"         = "Kennesaw State",
  "Kansas St Wildcats"       = "Kansas State",
  "Boise St Broncos"         = "Boise State",
  "Kent St Golden Flashes"   = "Kent State",
  "Oregon St Beavers"        = "Oregon State",
  "San Diego St Aztecs"      = "San Diego State",
  "Wright St Raiders"        = "Wright State",
  "Arizona St Sun Devils"    = "Arizona State",
  "Penn St Nittany Lions"    = "Penn State",
  "Long Beach St 49ers"      = "Long Beach State",
  "South Dakota St Jackrabbits" = "South Dakota State",
  "New Mexico St Aggies"     = "New Mexico State",
  "Montana St Bobcats"       = "Montana State",
  "Cal St Fullerton Titans"  = "Cal State Fullerton",

  # Mount / Saint variants
  "Mt. St. Mary's Mountaineers" = "Mount St. Mary's",
  "Saint Mary's Gaels"          = "Saint Mary's",
  "St. Peter's Peacocks"        = "Saint Peter's",
  "St Peter's Peacocks"         = "Saint Peter's",
  "St. John's Red Storm"        = "St. John's",
  "St John's Red Storm"         = "St. John's",

  # SIU Edwardsville
  "SIU-Edwardsville Cougars" = "SIU Edwardsville",
  "SIUE Cougars"              = "SIU Edwardsville",

  # Loyola / Cleveland St / CSU Fullerton / Texas A&M-CC
  "Loyola Chicago Ramblers"  = "Loyola Chicago",
  "Loyola (Chi) Ramblers"    = "Loyola Chicago",
  "Cleveland St Vikings"     = "Cleveland State",
  "CSU Fullerton Titans"     = "Cal State Fullerton",
  "Texas A&M-CC Islanders"   = "Texas A&M-Corpus Christi",

  # Other
  "NC State Wolfpack"       = "NC State",
  "UCF Knights"             = "UCF",
  "UNC Wilmington Seahawks" = "UNC Wilmington",
  "UC San Diego Tritons"    = "UC San Diego",
  "Omaha Mavericks"         = "Omaha"
)

# Try to resolve an odds API team name to a bracket team name
resolve_name <- function(api_name, bracket_teams) {
  # 1) Exact match
  if (api_name %in% bracket_teams) return(api_name)
  # 2) Explicit alias
  if (api_name %in% names(odds_to_bracket)) return(odds_to_bracket[[api_name]])
  # 3) Check if bracket team is a prefix of API name (handles mascot suffixes)
  #    Sort longest-first to avoid "New Mexico" matching before "New Mexico State"
  sorted_bt <- bracket_teams[order(-nchar(bracket_teams))]
  for (bt in sorted_bt) {
    if (startsWith(api_name, bt)) return(bt)
  }
  return(api_name)  # unresolved — won't match bracket filter
}

# ==============================================================================
# FETCH ODDS FOR ONE GAME DATE
# ==============================================================================

fetch_day <- function(game_date) {
  # Snapshot at 11:00 AM ET — close to first tipoff on most tournament days
  snapshot_utc <- with_tz(
    ymd_hm(paste(game_date, "11:00"), tz = "America/New_York"),
    "UTC"
  )
  snapshot_iso <- format(snapshot_utc, "%Y-%m-%dT%H:%M:%SZ")

  cat(sprintf("  %s (snapshot %s) ... ", game_date, snapshot_iso))

  resp <- tryCatch({
    request(paste0(
      "https://api.the-odds-api.com/v4/historical/sports/", SPORT, "/odds"
    )) |>
      req_url_query(
        apiKey     = ODDS_API_KEY,
        regions    = "us,us2",
        markets    = "h2h,spreads",
        oddsFormat = "american",
        date       = snapshot_iso
      ) |>
      req_perform()
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", conditionMessage(e)))
    return(NULL)
  })

  if (is.null(resp)) return(NULL)

  j <- fromJSON(resp_body_string(resp), simplifyVector = FALSE)

  if (length(j$data) == 0) {
    cat("0 events\n")
    return(NULL)
  }

  games <- map_dfr(j$data, function(ev) {
    ct   <- ymd_hms(ev$commence_time, quiet = TRUE)
    home <- ev$home_team
    away <- ev$away_team

    # Compare dates in ET (as.Date on POSIXct silently converts to UTC)
    game_date_et <- format(with_tz(ct, "America/New_York"), "%Y-%m-%d")
    if (game_date_et != as.character(game_date)) return(NULL)

    home_spread <- NA_real_
    home_ml     <- NA_integer_
    away_ml     <- NA_integer_

    # Prefer betonlineag, fall back to first available bookmaker
    bm_list   <- to_list(ev$bookmakers)
    preferred <- Filter(function(b) b$key == BOOKMAKER, bm_list)
    fallback  <- Filter(function(b) b$key != BOOKMAKER, bm_list)

    for (bm in c(preferred, fallback)) {
      for (mkt in to_list(bm$markets)) {
        for (o in to_list(mkt$outcomes)) {
          if (mkt$key == "spreads" && o$name == home && is.na(home_spread))
            home_spread <- as.numeric(null_na(o$point))
          if (mkt$key == "h2h" && o$name == home && is.na(home_ml))
            home_ml <- as.integer(null_na(o$price))
          if (mkt$key == "h2h" && o$name == away && is.na(away_ml))
            away_ml <- as.integer(null_na(o$price))
        }
      }
    }

    # Devig moneyline -> fair home win probability
    h_imp <- american_to_prob(home_ml)
    a_imp <- american_to_prob(away_ml)
    home_win_prob <- if (!is.na(h_imp) && !is.na(a_imp) && (h_imp + a_imp) > 0) {
      round(h_imp / (h_imp + a_imp), 4)
    } else NA_real_

    tibble(
      home_team     = home,
      away_team     = away,
      date          = as.Date(game_date_et),
      home_spread   = home_spread,
      home_win_prob = home_win_prob
    )
  })

  cat(sprintf("%d games\n", if (is.null(games)) 0L else nrow(games)))
  Sys.sleep(1)  # rate-limit courtesy
  return(games)
}

# ==============================================================================
# MAIN
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && tolower(args[1]) == "all") {
  years <- names(tourney_dates)
} else if (length(args) >= 1) {
  years <- args[1]
} else {
  years <- "2025"
}

out_dir <- file.path(script_dir, "closing_lines")
dir.create(out_dir, showWarnings = FALSE)

for (yr in years) {
  cat(sprintf("\n=== %s NCAA Tournament ===\n", yr))

  dates <- tourney_dates[[yr]]
  if (is.null(dates)) {
    cat("No tournament dates defined, skipping\n")
    next
  }

  # Load bracket teams for filtering (keeps only actual tourney games)
  bracket_file <- file.path(script_dir, "brackets", sprintf("bracket_%s.csv", yr))
  bracket_teams <- if (file.exists(bracket_file)) {
    read.csv(bracket_file, stringsAsFactors = FALSE)$team
  } else {
    cat("  Warning: no bracket file, cannot filter to tourney games\n")
    NULL
  }

  all_games <- map_dfr(dates, fetch_day)

  if (is.null(all_games) || nrow(all_games) == 0) {
    cat(sprintf("No games found for %s\n", yr))
    next
  }

  # Filter to tournament games: both teams must be in the bracket
  if (!is.null(bracket_teams)) {
    all_games <- all_games |>
      mutate(
        home_resolved = sapply(home_team, resolve_name, bracket_teams),
        away_resolved = sapply(away_team, resolve_name, bracket_teams)
      )

    unmatched_home <- all_games |>
      filter(!home_resolved %in% bracket_teams) |>
      pull(home_team) |> unique()
    unmatched_away <- all_games |>
      filter(!away_resolved %in% bracket_teams) |>
      pull(away_team) |> unique()
    unmatched <- unique(c(unmatched_home, unmatched_away))
    if (length(unmatched) > 0) {
      cat(sprintf("  Unmatched teams (add to odds_to_bracket if tourney): %s\n",
                  paste(unmatched, collapse = ", ")))
    }

    all_games <- all_games |>
      filter(home_resolved %in% bracket_teams & away_resolved %in% bracket_teams) |>
      select(-home_resolved, -away_resolved)
  }

  all_games <- all_games |>
    distinct(home_team, away_team, date, .keep_all = TRUE) |>
    arrange(date, home_team)

  out_file <- file.path(out_dir, sprintf("ncaat_%s_closing_lines.csv", yr))
  write.csv(all_games, out_file, row.names = FALSE)

  cat(sprintf("\nSaved %d games -> %s\n", nrow(all_games), out_file))
  print(as.data.frame(all_games), row.names = FALSE)
}

cat("\nDone!\n")
