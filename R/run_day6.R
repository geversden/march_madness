#!/usr/bin/env Rscript
# ==============================================================================
# run_day6.R — Day 6 (S16_d2) optimizer pipeline
#
# Orchestrates: scrape → lock R64+R32+S16_d1 results → re-simulate →
# entry-level field sim → optimize → export CSVs
#
# Key differences from Day 5:
#   - 5 completed slots locked (R1_d1, R1_d2, R2_d1, R2_d2, S16_d1)
#   - current_slot_id = "S16_d2"
#   - 52 locked games (48 R64+R32 + 4 S16_d1)
#   - After today, only E8+FF+CHAMP remain — bracket is very constrained
#   - Calibration includes both S16 dates
#
# Usage:
#   source("R/run_day6.R")
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
sourceCpp(file.path(script_dir, "..", "simulate_tourney.cpp"))

# ==============================================================================
# 1. BUILD BRACKET + KENPOM RATINGS
# ==============================================================================

YEAR <- 2026L
UPDATE_FACTOR <- 0.5

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
# 2. COMPLETED RESULTS — R64 + R32 + S16_d1 winners (52 games)
# ==============================================================================

cat("\n========================================\n")
cat("LOCKING COMPLETED RESULTS\n")
cat("========================================\n")

r1_d1_winners <- c(
  "Duke",           # Game 1
  "TCU",            # Game 2
  "Louisville",     # Game 5
  "Michigan State", # Game 6
  "Vanderbilt",     # Game 11
  "Nebraska",       # Game 12
  "VCU",            # Game 13
  "Illinois",       # Game 14
  "Texas A&M",      # Game 15
  "Houston",        # Game 16
  "High Point",     # Game 19
  "Arkansas",       # Game 20
  "Texas",          # Game 21
  "Gonzaga",        # Game 22
  "Michigan",       # Game 25
  "Saint Louis"     # Game 26
)

r1_d2_winners <- c(
  "St. John's",     # Game 3
  "Kansas",         # Game 4
  "UCLA",           # Game 7
  "UConn",          # Game 8
  "Florida",        # Game 9
  "Iowa",           # Game 10
  "Arizona",        # Game 17
  "Utah State",     # Game 18
  "Miami",          # Game 23
  "Purdue",         # Game 24
  "Texas Tech",     # Game 27
  "Alabama",        # Game 28
  "Tennessee",      # Game 29
  "Virginia",       # Game 30
  "Kentucky",       # Game 31
  "Iowa State"      # Game 32
)

r2_d1_winners <- c(
  "Duke",           # Game 33
  "Michigan State", # Game 35
  "Nebraska",       # Game 38
  "Illinois",       # Game 39
  "Houston",        # Game 40
  "Arkansas",       # Game 42
  "Texas",          # Game 43
  "Michigan"        # Game 45
)

r2_d2_winners <- c(
  # ---- FILL IN SUNDAY R32 WINNERS ----
  "St. John's",     # Game 34
  "UConn",          # Game 36
  "Florida",        # Game 37
  "Arizona",        # Game 41
  "Purdue",         # Game 44
  "Alabama",        # Game 46
  "Tennessee",      # Game 47
  "Iowa State"      # Game 48
)

# ---- S16 Day 1 (Thursday) results ----
# S16_THU_GAMES: 51, 52, 53, 54
# Game 51: R32 winners from games 37, 38 (South region top half)
# Game 52: R32 winners from games 39, 40 (South region bottom half)
# Game 53: R32 winners from games 41, 42 (West region top half)
# Game 54: R32 winners from games 43, 44 (West region bottom half)
s16_d1_winners <- c(
  # ---- FILL IN THURSDAY S16 WINNERS HERE ----
  "PLACEHOLDER1",   # Game 51
  "PLACEHOLDER2",   # Game 52
  "PLACEHOLDER3",   # Game 53
  "PLACEHOLDER4"    # Game 54
)

completed_slots <- list(
  R1_d1  = r1_d1_winners,
  R1_d2  = r1_d2_winners,
  R2_d1  = r2_d1_winners,
  R2_d2  = r2_d2_winners,
  S16_d1 = s16_d1_winners
)

cat(sprintf("\nLocked: %d R64 + %d R32 + %d S16_d1 = %d total games\n",
            length(r1_d1_winners) + length(r1_d2_winners),
            length(r2_d1_winners) + length(r2_d2_winners),
            length(s16_d1_winners),
            length(unlist(completed_slots))))

# ==============================================================================
# 3. CALIBRATE RATINGS TO CLOSING LINES
#
# Include R32 + both S16 dates.
# ==============================================================================

calibration_dates <- c("2026-03-21", "2026-03-22", "2026-03-26", "2026-03-27")
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

bearer_token <- "PASTE_FRESH_BEARER_TOKEN_HERE"
scrape <- scrape_all_splash(bearer_token)
# saveRDS(scrape, file.path(script_dir, "..", "scrape_day6.rds"))

# Option B: Load cached scrape
# scrape <- readRDS(file.path(script_dir, "..", "scrape_day6.rds"))

# ==============================================================================
# 5. PREPARE OPTIMIZER INPUTS
# ==============================================================================

inputs <- prepare_optimizer_inputs(
  scrape_results   = scrape,
  sim              = sim,
  current_slot_id  = "S16_d2",
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
# By S16_d2, the field is even more constrained:
#   - 5 rounds of pick history per entry
#   - Fewer entries alive (S16_d1 eliminated many)
#   - Only 3-4 remaining slots (S16_d2, E8, FF, CHAMP)
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
    current_slot_id    = "S16_d2",
    entry_model_params = entry_model_params,
    teams_dt           = inputs$sim$teams
  )

  # Print implied ownership diagnostic
  efd <- entry_field_results[[cid]]
  if ("S16_d2" %in% names(efd$implied_ownership)) {
    cat("\n  Entry-model implied S16_d2 ownership:\n")
    own <- efd$implied_ownership[["S16_d2"]]
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
# ==============================================================================

ownership_override <- NULL

# ==============================================================================
# 7. RUN OPTIMIZER
# ==============================================================================

locked_teams <- NULL

# options(splash.verbose = TRUE)
options(splash.optionality_weight = 0.20)
result <- run_optimizer(
  scrape_inputs      = inputs,
  current_slot_id    = "S16_d2",
  ownership_override = ownership_override,
  locked_teams       = locked_teams,
  sim_sample_size    = 50000,
  entry_field_data   = entry_field_results
)

# ==============================================================================
# 8. EXPORT CSVs
# ==============================================================================

csv_dir <- file.path(script_dir, "..", "splash_entry_csvs")
export_picks(result, csv_dir = csv_dir, locked_teams = locked_teams)

cat("\n========================================\n")
cat("Day 6 (S16_d2) optimization complete!\n")
cat("========================================\n")
