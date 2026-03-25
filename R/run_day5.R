#!/usr/bin/env Rscript
# ==============================================================================
# run_day5.R — Day 5 (S16_d1) optimizer pipeline
#
# Orchestrates: scrape → lock R64+R32 results → re-simulate → entry-level
# field sim → optimize → export CSVs
#
# Key improvements over Day 3/4:
#   - Entry-level field simulation: uses calibrated entry ownership model to
#     predict each opponent's S16+ picks based on their pick history
#   - Per-sim exact opponent survival counts: instead of averaged field survival
#     rates, computes exact number of surviving opponents in each simulation
#   - More accurate EV calculation from these per-sim counts
#
# Usage:
#   source("R/run_day5.R")
#   # OR interactively: run sections below
# ==============================================================================

library(data.table)
library(Rcpp)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) {
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
source(file.path(script_dir, "entry_ownership_model.R"))
source(file.path(script_dir, "entry_field_sim.R"))
source(file.path(script_dir, "contest_mc_sim.R"))
sourceCpp(file.path(script_dir, "..", "simulate_tourney.cpp"))

# ==============================================================================
# 1. BUILD BRACKET + KENPOM RATINGS
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

# Build round_info
region_names <- unique(teams$region)
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

sim <- list(
  teams         = teams,
  round_info    = round_info,
  bracket_order = teams$team_id,
  update_factor = UPDATE_FACTOR,
  year          = YEAR,
  r1_win_probs  = numeric(0)
)

cat(sprintf("Built bracket: %d teams, KenPom ratings loaded\n", nrow(teams)))

# ==============================================================================
# 2. COMPLETED RESULTS — R64 + R32 winners (all 48 games)
#
# By Day 5 (S16_d1), all R64 and R32 games are complete.
# Fill in actual winners before running.
# ==============================================================================

cat("\n========================================\n")
cat("LOCKING COMPLETED RESULTS\n")
cat("========================================\n")

r1_d1_winners <- c(
  # Thursday R64 winners (games 1,2,5,6,11,12,13,14,15,16,19,20,21,22,25,26)
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
  # Friday R64 winners (games 3,4,7,8,9,10,17,18,23,24,27,28,29,30,31,32)
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

# ---- R32 Day 1 (Saturday) results ----
# R32_SAT_GAMES: 33, 35, 38, 39, 40, 42, 43, 45
r2_d1_winners <- c(
  "Duke",           # Game 33: Duke vs TCU
  "Michigan State", # Game 35: Louisville vs Michigan State
  "Nebraska",       # Game 38: Vanderbilt vs Nebraska
  "Illinois",       # Game 39: VCU vs Illinois
  "Houston",        # Game 40: Texas A&M vs Houston
  "Arkansas",       # Game 42: High Point vs Arkansas
  "Texas",          # Game 43: Texas vs Gonzaga
  "Michigan"        # Game 45: Michigan vs Saint Louis
)

# ---- R32 Day 2 (Sunday) results ----
# R32_SUN_GAMES: 34, 36, 37, 41, 44, 46, 47, 48
# Game 34: St. John's vs Kansas
# Game 36: UCLA vs UConn
# Game 37: Florida vs Iowa
# Game 41: Arizona vs Utah State
# Game 44: Miami vs Purdue
# Game 46: Texas Tech vs Alabama
# Game 47: Tennessee vs Virginia
# Game 48: Kentucky vs Iowa State
r2_d2_winners <- c(
  # ---- FILL IN SUNDAY R32 WINNERS HERE ----
  "St. John's",     # Game 34
  "UConn",          # Game 36
  "Iowa",           # Game 37
  "Arizona",        # Game 41
  "Purdue",         # Game 44
  "Alabama",        # Game 46
  "Tennessee",      # Game 47
  "Iowa State"      # Game 48
)

completed_slots <- list(
  R1_d1 = r1_d1_winners,
  R1_d2 = r1_d2_winners,
  R2_d1 = r2_d1_winners,
  R2_d2 = r2_d2_winners
)

cat(sprintf("\nLocked: %d R64 + %d R32 = %d total games\n",
            length(r1_d1_winners) + length(r1_d2_winners),
            length(r2_d1_winners) + length(r2_d2_winners),
            length(unlist(completed_slots))))

# Print S16 matchups
cat("\n========================================\n")
cat("SWEET 16 MATCHUPS\n")
cat("========================================\n")

cat("\nThursday S16 (Day 5) games:\n")
for (g in S16_THU_GAMES) {
  feeders <- c(2L * (g - 49L) + 33L, 2L * (g - 49L) + 34L)
  # The winners of those R32 games play each other
  cat(sprintf("  Game %d: R32 game %d winner vs R32 game %d winner\n", g, feeders[1], feeders[2]))
}

cat("\nFriday S16 (Day 6) games:\n")
for (g in S16_FRI_GAMES) {
  feeders <- c(2L * (g - 49L) + 33L, 2L * (g - 49L) + 34L)
  cat(sprintf("  Game %d: R32 game %d winner vs R32 game %d winner\n", g, feeders[1], feeders[2]))
}

# ==============================================================================
# 3. CALIBRATE RATINGS TO CLOSING LINES
#
# By Day 5, we have closing lines for R32 and S16 Day 1.
# Calibrate ratings to all available market data.
# ==============================================================================

# Include R32 + S16 Day 1 closing lines
calibration_dates <- c("2026-03-21", "2026-03-22", "2026-03-26")
cl_file <- file.path(script_dir, "..", "closing_lines", "ncaat_2026_closing_lines.csv")

sim$teams <- calibrate_ratings_to_lines(
  teams_dt         = sim$teams,
  closing_lines_csv = cl_file,
  team_names_csv   = file.path(script_dir, "..", "team_names.csv"),
  log_scale        = 0.0917,
  lambda           = 0.0001,
  round_dates      = calibration_dates
)

cat(sprintf("\nCalibrated %d team ratings to R32+S16 closing lines\n",
            sum(sim$teams$rating_delta != 0)))

# ==============================================================================
# 4. SCRAPE OPPONENT DATA
# ==============================================================================

# Option A: Live scrape (requires fresh bearer token from Splash)
bearer_token <- "eyJraWQiOiJmMzRiN2YiLCJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzd3QiOiJlNWVhNmM5YS0zNDg4LTRhMDQtOTA5ZS02YmVkNGUxNjEyNzMifQ.3aJ-YW4YRp2v9bJGpdQWS84oh1WKupWz9SyLOFCk2bcMhXdc9ebX71kbZLshAcK58Wd1n-2VSLdl4DFdaBYX3g"
scrape <- scrape_all_splash(bearer_token)
# saveRDS(scrape, file.path(script_dir, "..", "scrape_day5.rds"))

# Option B: Load cached scrape
# scrape <- readRDS(file.path(script_dir, "..", "scrape_day5.rds"))

# ==============================================================================
# 5. PREPARE OPTIMIZER INPUTS
#    (re-simulates with locked R64+R32 results using calibrated ratings,
#     builds portfolio, infers alive status)
# ==============================================================================

inputs <- prepare_optimizer_inputs(
  scrape_results   = scrape,
  sim              = sim,
  current_slot_id  = "S16_d1",
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
# 5b. ENTRY-LEVEL FIELD SIMULATION
#
# By S16, we have 4 rounds of pick history for every field entry.
# Use the calibrated entry ownership model to predict each entry's S16+ picks
# and compute per-sim exact opponent survival counts.
#
# This replaces the generic ownership-based field model with precise,
# entry-level predictions. The model accounts for:
#   - Each entry's specific used_teams (hard constraint: can't reuse)
#   - Path viability (boost for teams whose opponent is already used)
#   - Future value saving (reduce pick prob for high championship equity teams)
#   - Calibrated on 2024+2025 historical Splash data
# ==============================================================================

cat("\n========================================\n")
cat("ENTRY-LEVEL FIELD SIMULATION\n")
cat("========================================\n")

entry_model_params <- readRDS(file.path(script_dir, "..", "entry_model_params.rds"))
cat(sprintf("Loaded entry model params: beta_wp_S16=%.2f, beta_save_S16=%.2f, beta_path_S16=%.2f\n",
            entry_model_params$beta_wp_S16, entry_model_params$beta_save_S16,
            entry_model_params$beta_path_S16))

entry_field_results <- list()
for (cid in names(inputs$field_avail)) {
  fa <- inputs$field_avail[[cid]]
  cat(sprintf("\n--- Contest: %s (%d alive entries) ---\n", fa$contest_name, fa$alive_count))

  entry_field_results[[cid]] <- simulate_entry_level_field(
    field_avail        = fa,
    sim                = inputs$sim,
    current_slot_id    = "S16_d1",
    entry_model_params = entry_model_params,
    teams_dt           = inputs$sim$teams
  )

  # Print implied ownership diagnostic
  efd <- entry_field_results[[cid]]
  if ("S16_d1" %in% names(efd$implied_ownership)) {
    cat("\n  Entry-model implied S16_d1 ownership:\n")
    own <- efd$implied_ownership[["S16_d1"]]
    ord <- order(own, decreasing = TRUE)
    for (j in ord) {
      if (own[j] >= 0.005) {
        cat(sprintf("    %-25s %7.1f%%\n", names(own)[j], own[j] * 100))
      }
    }
  }
}

# ==============================================================================
# 6. OWNERSHIP OVERRIDE (optional)
#
# The entry-level field sim provides ownership estimates from the model.
# You can still override if you have a strong read from manual analysis.
# Set to NULL to rely entirely on the entry-level field sim for EV.
# ==============================================================================

ownership_override <- NULL
# ownership_override <- list(
#   S16_d1 = c(
#     # ---- FILL IN IF OVERRIDING ----
#   )
# )

# ==============================================================================
# 7. RUN OPTIMIZER
#
# Uses entry-level field data for per-sim exact opponent survival counts.
# The optimizer's precompute_group_context() will use n_field_alive_matrix
# instead of the averaged field survival curves.
# ==============================================================================

locked_teams <- NULL

# options(splash.verbose = TRUE)
result <- run_optimizer(
  scrape_inputs      = inputs,
  current_slot_id    = "S16_d1",
  ownership_override = ownership_override,
  locked_teams       = locked_teams,
  sim_sample_size    = 50000,
  entry_field_data   = entry_field_results,
  method             = "mc"
)

# ==============================================================================
# 8. EXPORT CSVs
# ==============================================================================

csv_dir <- file.path(script_dir, "..", "splash_entry_csvs")
export_picks(result, csv_dir = csv_dir, locked_teams = locked_teams)

cat("\n========================================\n")
cat("Day 5 (S16_d1) optimization complete!\n")
cat("========================================\n")
