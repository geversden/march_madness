#!/usr/bin/env Rscript
# ==============================================================================
# run_day3.R — Day 3 (R2_d1) optimizer pipeline
#
# Orchestrates: scrape → lock R64 results → re-simulate → optimize → export CSVs
#
# Usage:
#   source("R/run_day3.R")
#   # OR interactively: run sections below
# ==============================================================================

library(data.table)
library(Rcpp)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) {
  # When sourcing interactively, find the R/ folder relative to working directory
  if (file.exists("R/splash_config.R")) "R"
  else if (file.exists("splash_config.R")) "."
  else stop("Cannot determine script_dir. Set working directory to the project root.")
})

# Source all modules
source(file.path(script_dir, "splash_config.R"))
source(file.path(script_dir, "splash_state.R"))
source(file.path(script_dir, "splash_ownership.R"))
source(file.path(script_dir, "splash_field_sim.R"))
source(file.path(script_dir, "splash_prepare.R"))
source(file.path(script_dir, "splash_optimizer.R"))
source(file.path(script_dir, "export_picks.R"))
source(file.path(script_dir, "scrape_splash_contests.R"))
source(file.path(script_dir, "calibrate_win_prob.R"))
sourceCpp(file.path(script_dir, "..", "simulate_tourney.cpp"))

# ==============================================================================
# 1. BUILD BRACKET + KENPOM RATINGS
#
# Loads the bracket and current KenPom ratings from CSV.
# No pre-built sim needed — we resimulate from scratch with locked results.
# ==============================================================================

YEAR <- 2026L
UPDATE_FACTOR <- 0.5

# Load bracket
bracket_file <- file.path(script_dir, "..", "brackets", sprintf("bracket_%d.csv", YEAR))
bracket <- read.csv(bracket_file, stringsAsFactors = FALSE)
stopifnot(nrow(bracket) == 64)

teams <- data.frame(
  name    = bracket$team,
  seed    = bracket$seed,
  region  = bracket$region,
  team_id = 1:64,
  stringsAsFactors = FALSE
)

# Load KenPom ratings
kenpom_file <- file.path(script_dir, "..", "kenpom_data", sprintf("kenpom_%d.csv", YEAR))
if (!file.exists(kenpom_file)) {
  # Fallback to dated files
  dated <- sort(Sys.glob(file.path(script_dir, "..", "kenpom_data",
                                    sprintf("kenpom_ratings_%d-*.csv", YEAR))),
                decreasing = TRUE)
  if (length(dated) > 0) kenpom_file <- dated[1]
}
if (!file.exists(kenpom_file)) stop("Cannot find KenPom data for ", YEAR)

kp <- read.csv(kenpom_file, stringsAsFactors = FALSE)
if ("team" %in% names(kp) && !"Team" %in% names(kp)) names(kp)[names(kp) == "team"] <- "Team"
if ("adj_em" %in% names(kp) && !"NetRtg" %in% names(kp)) names(kp)[names(kp) == "adj_em"] <- "NetRtg"
kp <- kp[!is.na(kp$Team) & kp$Team != "" & kp$Team != "Team", ]
kp$Team <- gsub("\\s*\\d+$", "", trimws(kp$Team))
kp$NetRtg <- as.numeric(kp$NetRtg)
kp <- kp[!is.na(kp$NetRtg), ]
kp_lookup <- setNames(kp$NetRtg, kp$Team)

# Name aliases
team_dict_file <- file.path(script_dir, "..", "team_names.csv")
team_dict <- read.csv(team_dict_file, stringsAsFactors = FALSE)
kp_alias <- setNames(team_dict$kenpom_name, team_dict$bracket_name)

get_rating <- function(name) {
  kp_name <- if (name %in% names(kp_alias)) kp_alias[[name]] else name
  if (kp_name %in% names(kp_lookup)) return(kp_lookup[[kp_name]])
  if (name %in% names(kp_lookup)) return(kp_lookup[[name]])
  matches <- grep(kp_name, names(kp_lookup), value = TRUE, ignore.case = TRUE)
  if (length(matches) == 0 && kp_name != name)
    matches <- grep(name, names(kp_lookup), value = TRUE, ignore.case = TRUE)
  if (length(matches) > 0) {
    cat(sprintf("  Fuzzy matched '%s' -> '%s'\n", name, matches[1]))
    return(kp_lookup[[matches[1]]])
  }
  warning(sprintf("No KenPom rating for '%s', using 0", name))
  0
}

teams$rating <- sapply(teams$name, get_rating)

# Build round_info (metadata for 63 tournament games)
region_names <- unique(teams$region)  # East, South, West, Midwest
round_info <- data.frame(
  round_num  = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), rep(5, 2), 6),
  round_name = c(rep("R64", 32), rep("R32", 16), rep("S16", 8),
                 rep("E8", 4), rep("FF", 2), "Championship"),
  game_col   = 1:63,
  stringsAsFactors = FALSE
)
round_info$region <- NA_character_
games_per_region <- c(8, 4, 2, 1)
for (rd in 1:4) {
  start_col <- c(1, 33, 49, 57)[rd]
  gpr <- games_per_region[rd]
  for (r in 1:4) {
    cols <- start_col + ((r - 1) * gpr):(r * gpr - 1)
    round_info$region[cols] <- region_names[r]
  }
}
round_info$region[61:63] <- "National"

# Build sim object (no all_results yet — resimulate_with_locks will generate them)
sim <- list(
  teams         = teams,
  round_info    = round_info,
  bracket_order = teams$team_id,
  update_factor = UPDATE_FACTOR,
  year          = YEAR,
  r1_win_probs  = numeric(0)  # R64 is locked, not needed
)

cat(sprintf("Built bracket: %d teams, KenPom ratings loaded\n", nrow(teams)))

# ==============================================================================
# 2. COMPLETED RESULTS — Fill in R64 winners
#
# Winner names must match bracket_2026.csv exactly.
# Order within each vector doesn't matter — build_locked_results() matches
# winners to games by checking bracket positions, not by position in the vector.
#
# Game-day mapping (from splash_config.R):
#   R1_d1 (Thu Mar 19): R64 games 1,2,5,6,11,12,13,14,15,16,19,20,21,22,25,26
#     → feed R32 Saturday games (R2_d1): 33,35,38,39,40,42,43,45
#   R1_d2 (Fri Mar 20): R64 games 3,4,7,8,9,10,17,18,23,24,27,28,29,30,31,32
#     → feed R32 Sunday games (R2_d2):   34,36,37,41,44,46,47,48
# ==============================================================================

# Print matchups for reference
cat("\n========================================\n")
cat("R64 GAME MAPPING\n")
cat("========================================\n")

thu_r64_games <- sort(unique(unlist(lapply(R32_SAT_GAMES, function(g) {
  c(2 * (g - 33) + 1, 2 * (g - 33) + 2)
}))))
fri_r64_games <- setdiff(1:32, thu_r64_games)

stopifnot(setequal(thu_r64_games, R64_THU_GAMES))  # sanity check vs config
stopifnot(setequal(fri_r64_games, R64_FRI_GAMES))

cat("\nThursday R64 games (feed Saturday R32):\n")
for (g in thu_r64_games) {
  t1 <- sim$teams$name[2 * g - 1]; s1 <- sim$teams$seed[2 * g - 1]
  t2 <- sim$teams$name[2 * g];     s2 <- sim$teams$seed[2 * g]
  cat(sprintf("  Game %2d: (%2d) %-20s vs (%2d) %s\n", g, s1, t1, s2, t2))
}

cat("\nFriday R64 games (feed Sunday R32):\n")
for (g in fri_r64_games) {
  t1 <- sim$teams$name[2 * g - 1]; s1 <- sim$teams$seed[2 * g - 1]
  t2 <- sim$teams$name[2 * g];     s2 <- sim$teams$seed[2 * g]
  cat(sprintf("  Game %2d: (%2d) %-20s vs (%2d) %s\n", g, s1, t1, s2, t2))
}

r1_d1_winners <- c(
  # Thursday R64 winners — in thu_r64_games order (1,2,5,6,11,12,13,14,15,16,19,20,21,22,25,26)
  "Duke",           # Game 1:  (1) Duke vs (16) Siena
  "TCU",            # Game 2:  (8) Ohio State vs (9) TCU
  "Louisville",     # Game 5:  (6) Louisville vs (11) South Florida
  "Michigan State", # Game 6:  (3) Michigan State vs (14) North Dakota State
  "Vanderbilt",     # Game 11: (5) Vanderbilt vs (12) McNeese State
  "Nebraska",       # Game 12: (4) Nebraska vs (13) Troy
  "VCU",            # Game 13: (6) North Carolina vs (11) VCU
  "Illinois",       # Game 14: (3) Illinois vs (14) Penn
  "Texas A&M",      # Game 15: (7) Saint Mary's vs (10) Texas A&M
  "Houston",        # Game 16: (2) Houston vs (15) Idaho
  "High Point",     # Game 19: (5) Wisconsin vs (12) High Point
  "Arkansas",       # Game 20: (4) Arkansas vs (13) Hawaii
  "Texas",          # Game 21: (6) BYU vs (11) Texas
  "Gonzaga",        # Game 22: (3) Gonzaga vs (14) Kennesaw State
  "Michigan",       # Game 25: (1) Michigan vs (16) Howard
  "Saint Louis"     # Game 26: (8) Georgia vs (9) Saint Louis
)

r1_d2_winners <- c(
  # Friday R64 winners — in fri_r64_games order (3,4,7,8,9,10,17,18,23,24,27,28,29,30,31,32)
  "St. John's",     # Game 3:  (5) St. John's vs (12) Northern Iowa
  "Kansas",         # Game 4:  (4) Kansas vs (13) Cal Baptist
  "UCLA",           # Game 7:  (7) UCLA vs (10) UCF
  "UConn",          # Game 8:  (2) UConn vs (15) Furman
  "Florida",        # Game 9:  (1) Florida vs (16) Prairie View A&M
  "Iowa",           # Game 10: (8) Clemson vs (9) Iowa
  "Arizona",        # Game 17: (1) Arizona vs (16) LIU
  "Utah State",     # Game 18: (8) Villanova vs (9) Utah State
  "Miami",          # Game 23: (7) Miami vs (10) Missouri
  "Purdue",         # Game 24: (2) Purdue vs (15) Queens
  "Texas Tech",     # Game 27: (5) Texas Tech vs (12) Akron
  "Alabama",        # Game 28: (4) Alabama vs (13) Hofstra
  "Tennessee",      # Game 29: (6) Tennessee vs (11) Miami OH
  "Virginia",       # Game 30: (3) Virginia vs (14) Wright State
  "Kentucky",       # Game 31: (7) Kentucky vs (10) Santa Clara
  "Iowa State"      # Game 32: (2) Iowa State vs (15) Tennessee State
)

completed_slots <- list(
  R1_d1 = r1_d1_winners,
  R1_d2 = r1_d2_winners
)

# ==============================================================================
# 3. CALIBRATE RATINGS TO R32 CLOSING LINES
#
# Adjusts KenPom ratings so the logistic model reproduces R32 market lines.
# These calibrated ratings are used for simulating R3+ matchups.
# R64 games are locked (already played), R32 games will also be locked by
# game time, so calibrated ratings only affect Sweet 16 and beyond.
#
# Update closing_lines/ncaat_2026_closing_lines.csv with R32 lines before
# running this section. The CSV should have rows for R32 game dates
# (2026-03-21 for Saturday, 2026-03-22 for Sunday).
# ==============================================================================

r32_dates <- c("2026-03-21", "2026-03-22")  # R32 Saturday + Sunday
cl_file <- file.path(script_dir, "..", "closing_lines", "ncaat_2026_closing_lines.csv")

sim$teams <- calibrate_ratings_to_lines(
  teams_dt         = sim$teams,
  closing_lines_csv = cl_file,
  team_names_csv   = file.path(script_dir, "..", "team_names.csv"),
  log_scale        = 0.0917,
  lambda           = 0.0001,   # ridge penalty toward KenPom (increase to trust KenPom more)
  round_dates      = r32_dates
)

cat(sprintf("\nCalibrated %d team ratings to R32 closing lines\n",
            sum(sim$teams$rating_delta != 0)))

# ==============================================================================
# 4. SCRAPE OPPONENT DATA
# ==============================================================================

# Option A: Live scrape (requires fresh bearer token from Splash)
bearer_token <- "eyJraWQiOiJENHJOR1pwNStnTzAxS21aVkg5YlZDZUd2bGNGYUNJSm1qVm5VOE4waUl3PSIsImFsZyI6IlJTMjU2In0.eyJmcmF1ZEZsYWciOiJ2ZXJpZmllZC1hY2NvdW50Iiwic3ViIjoiMTQ0OGE0MTgtOTBhMS03MDlhLTBmMTAtZDc0Y2MxOTUzMGMwIiwicm9sZSI6ImNvbW1pc3Npb25lciIsImVtYWlsX3ZlcmlmaWVkIjoidHJ1ZSIsInJvbGVzIjoiW1wiY29tbWlzc2lvbmVyXCJdIiwiaXNzIjoiaHR0cHM6XC9cL2NvZ25pdG8taWRwLnVzLWVhc3QtMS5hbWF6b25hd3MuY29tXC91cy1lYXN0LTFfNjRCOUJuQzVnIiwicmVzdHJpY3Rpb25zIjoiW10iLCJjb2duaXRvOnVzZXJuYW1lIjoiMTIyNGZjOTgtMDViNy00ZWQ3LTg5NmItZDhiMTZjMTk1ZTEyIiwib2ZwX3VzZXJfaWQiOiI0MzY4MzA2IiwicnlwX3VzZXJfaWQiOiIxNTMyNDA0Iiwib3JpZ2luX2p0aSI6ImI5MWI0YmJjLWU0ZDEtNDY1Yy1hNjk1LWU4OGVhZTg1NDhhZSIsImF1ZCI6IjU5aGJoYmpoa2FmOTg0bWVtb2M5ZmdhMTNxIiwiZXZlbnRfaWQiOiIyYWU4ZDE1ZS00NGFmLTQ4ZWUtOTI5NC1jYjQwYzAyZWU1ZTciLCJzcGxhc2hfdXNlcl9pZCI6IjEyMjRmYzk4LTA1YjctNGVkNy04OTZiLWQ4YjE2YzE5NWUxMiIsInRva2VuX3VzZSI6ImlkIiwiYXV0aF90aW1lIjoxNzczMjkzNTIzLCJuYW1lIjoiMTIyNGZjOTgtMDViNy00ZWQ3LTg5NmItZDhiMTZjMTk1ZTEyIiwiaWQiOiIxMjI0ZmM5OC0wNWI3LTRlZDctODk2Yi1kOGIxNmMxOTVlMTIiLCJleHAiOjE3NzQxMTAyNTUsImlhdCI6MTc3NDEwNjY1NSwiYWdlIjoiMzciLCJqdGkiOiIwYTlkNDE3ZS04MWQwLTQzOWItODg5Zi02ODU0NmY0OWNlMTMiLCJ1c2VybmFtZSI6IlRpbmt5VHlsZXIifQ.r4d8h-IcMwRus2pkBIqokgbFKB8puO12bdbUbJrRCHZYqqLX62Kc9vAZm7yI8QNHEa2l6PdKkY-Q5aiFnrVFIyqeaKssvY6te1z4EDyllkJWXZLW_gl2tkbA0uWzpWMFUy_KVrCt8d54EhcOsOtLWfeBUPUzKYxnSwUDyCY1zBu7RqXeRGwvWNx6eX0PlurEvFgFUXayrU6flSf3cNQRQaPjvguH-5LQHyZiY_tjAI-164GD0dkzLiYWyGNXyAFnfe1HtaPeFLmr5AolKzO3YN1ZWy-Rk5s_AyExQKfnyrzY90y6ZU5ZskfQyznd9nQw9uF8DE9sYr6ASSopVs-NnA"
scrape <- scrape_all_splash(bearer_token)
# saveRDS(scrape, file.path(script_dir, "..", "scrape_day3.rds"))

# Option B: Load cached scrape
# scrape <- readRDS(file.path(script_dir, "..", "scrape_day3.rds"))

# ==============================================================================
# 5. PREPARE OPTIMIZER INPUTS
#    (re-simulates with locked R64 results using calibrated ratings,
#     builds portfolio, infers alive status)
# ==============================================================================

inputs <- prepare_optimizer_inputs(
  scrape_results   = scrape,
  sim              = sim,
  current_slot_id  = "R2_d1",
  completed_slots  = completed_slots,
  our_username     = "TinkyTyler",
  team_names_csv   = file.path(script_dir, "..", "team_names.csv"),
  n_sims           = 2000000L,
  seed             = 42
)

cat(sprintf("\nRe-simulated with %d locked games\n",
            sum(inputs$sim$locked_results > 0)))
cat(sprintf("Portfolio: %d entries (%d alive)\n",
            nrow(inputs$portfolio),
            sum(inputs$portfolio$alive)))

# ==============================================================================
# 6. OWNERSHIP OVERRIDE (optional)
#
# If you have a strong read on R2_d1 field ownership, set it here.
# Format: named numeric vector, team_name -> ownership fraction (0-1).
# Set to NULL to let the model estimate ownership.
# ==============================================================================

ownership_override <- list(
  R2_d1 = c(
    "Arkansas"       = 0.4500,
    "Illinois"       = 0.2582,
    "Gonzaga"        = 0.0786,
    "Houston"        = 0.0742,
    "Michigan State" = 0.0482,
    "Michigan"       = 0.0285,
    "Duke"           = 0.0280,
    "Vanderbilt"     = 0.0229,
    "Nebraska"       = 0.0083,
    "Texas"          = 0.0014,
    "Louisville"     = 0.0013,
    "High Point"     = 0.0003,
    "VCU"            = 0.0002,
    "Texas A&M"      = 0.0001,
    "TCU"            = 0.0000,
    "Saint Louis"    = 0.0000
  ),
  R2_d2 = c(
    "Purdue"         = 0.1938,
    "St. John's"     = 0.1932,
    "Iowa State"     = 0.1171,
    "UConn"          = 0.1001,
    "Arizona"        = 0.0936,
    "Florida"        = 0.0877,
    "Tennessee"      = 0.0668,
    "Alabama"        = 0.0522,
    "Texas Tech"     = 0.0393,
    "Virginia"       = 0.0371,
    "Kansas"         = 0.0138,
    "UCLA"           = 0.0021,
    "Kentucky"       = 0.0021,
    "Miami"          = 0.0006,
    "Iowa"           = 0.0002,
    "Utah State"     = 0.0001
  )
)

# ==============================================================================
# 7. RUN OPTIMIZER
# ==============================================================================

# locked_teams = teams whose games have ALREADY STARTED today (can't pick them).
# At the start of R2_d1 (before any Saturday games tip), this is empty.
# If re-running mid-day after some games started, add those teams here.
locked_teams <- NULL

# options(splash.verbose = TRUE)  # Uncomment for detailed beam search diagnostics
result <- run_optimizer(
  scrape_inputs      = inputs,
  current_slot_id    = "R2_d1",
  ownership_override = ownership_override,
  locked_teams       = locked_teams,
  sim_sample_size    = 50000   # 50K sims per EV calc (halves scoring time vs 100K)
)

# ==============================================================================
# 8. EXPORT CSVs
#
# Fills Pick 1 in template CSVs. Day 3 templates must be downloaded from Splash
# and placed in splash_entry_csvs/ before running this step.
# ==============================================================================

csv_dir <- file.path(script_dir, "..", "splash_entry_csvs")
export_picks(result, csv_dir = csv_dir, locked_teams = locked_teams)

cat("\n========================================\n")
cat("Day 3 (R2_d1) optimization complete!\n")
cat("========================================\n")
