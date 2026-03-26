#!/usr/bin/env Rscript
# ==============================================================================
# run_day5_hodes.R — Day 5 (S16_d1) optimizer pipeline for Hodes contest
#
# Orchestrates: load standings → lock R64+R32 → calibrate ratings →
#   entry-level field sim (hodes_entry_model_params.rds) → optimize
#
# Key differences from run_day5.R (Splash):
#   - Loads Hodes standings CSV instead of scraping Splash
#   - Uses hodes_entry_model_params.rds (calibrated on Hodes history)
#   - Tiebreaker-aware: entry model predicts P(take_two) → S16_opt ownership
#     for correct p_field_tb1 in EV calculation
#   - We do NOT pick S16_opt for our entries (pick_s16opt = FALSE) to reduce
#     beam complexity
#
# Usage:
#   source("R/run_day5_hodes.R")
# ==============================================================================

library(data.table)
library(Rcpp)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) {
  if (file.exists("R/hodes_config.R")) "R"
  else if (file.exists("hodes_config.R")) "."
  else stop("Cannot determine script_dir. Set working directory to project root.")
})

# Source all modules
source(file.path(script_dir, "hodes_config.R"))
source(file.path(script_dir, "hodes_optimizer.R"))
source(file.path(script_dir, "hodes_entry_model.R"))
source(file.path(script_dir, "calibrate_win_prob.R"))
source(file.path(script_dir, "splash_config.R"))   # for get_slot (needed by build_locked_results)
source(file.path(script_dir, "splash_prepare.R"))  # for resimulate_with_locks
source(file.path(script_dir, "contest_mc_sim.R"))  # for solve_optimal_paths_cpp
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
kp$Team   <- gsub("\\s*\\d+$", "", trimws(kp$Team))
kp$NetRtg <- as.numeric(kp$NetRtg)
kp <- kp[!is.na(kp$NetRtg), ]
kp_lookup <- setNames(kp$NetRtg, kp$Team)

team_dict_file <- file.path(script_dir, "..", "team_names.csv")
team_dict      <- read.csv(team_dict_file, stringsAsFactors = FALSE)
kp_alias       <- setNames(team_dict$kenpom_name, team_dict$bracket_name)

get_rating <- function(name) {
  kp_name <- if (name %in% names(kp_alias)) kp_alias[[name]] else name
  if (kp_name %in% names(kp_lookup)) return(kp_lookup[[kp_name]])
  if (name  %in% names(kp_lookup)) return(kp_lookup[[name]])
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
#    Same results as run_day5.R (shared tournament)
# ==============================================================================

cat("\n========================================\n")
cat("LOCKING COMPLETED RESULTS\n")
cat("========================================\n")

r1_d1_winners <- c(
  "Duke", "TCU", "Louisville", "Michigan State", "Vanderbilt", "Nebraska",
  "VCU", "Illinois", "Texas A&M", "Houston", "High Point", "Arkansas",
  "Texas", "Gonzaga", "Michigan", "Saint Louis"
)
r1_d2_winners <- c(
  "St. John's", "Kansas", "UCLA", "UConn", "Florida", "Iowa",
  "Arizona", "Utah State", "Miami", "Purdue", "Texas Tech", "Alabama",
  "Tennessee", "Virginia", "Kentucky", "Iowa State"
)
r2_d1_winners <- c(
  "Duke", "Michigan State", "Nebraska", "Illinois", "Houston", "Arkansas",
  "Texas", "Michigan"
)
r2_d2_winners <- c(
  "St. John's", "UConn", "Iowa", "Arizona", "Purdue", "Alabama",
  "Tennessee", "Iowa State"
)

s16_teams_bracket <- c(
  # These are the 16 teams alive in S16 (R32 winners)
  r2_d1_winners, r2_d2_winners
)

cat(sprintf("\nLocked: %d R64 + %d R32 = %d total games\n",
            length(r1_d1_winners) + length(r1_d2_winners),
            length(r2_d1_winners) + length(r2_d2_winners),
            length(r1_d1_winners) + length(r1_d2_winners) +
              length(r2_d1_winners) + length(r2_d2_winners)))

# ==============================================================================
# 3. CALIBRATE RATINGS TO CLOSING LINES
# ==============================================================================

calibration_dates <- c("2026-03-26", "2026-03-27")  # S16 Day 1 + Day 2
cl_file <- file.path(script_dir, "..", "closing_lines", "ncaat_2026_closing_lines.csv")

sim$teams <- calibrate_ratings_to_lines(
  teams_dt          = sim$teams,
  closing_lines_csv = cl_file,
  team_names_csv    = file.path(script_dir, "..", "team_names.csv"),
  log_scale         = 0.0917,
  lambda            = 0.0001,
  round_dates       = calibration_dates
)

cat(sprintf("\nCalibrated %d team ratings to S16 closing lines\n",
            sum(sim$teams$rating_delta != 0)))

# ==============================================================================
# 3b. RESIMULATE WITH LOCKED RESULTS + SAVE
#
# Build completed_slots from confirmed R64+R32 winners and re-run the
# tournament sim so eliminated teams (Florida, Texas Tech, etc.) have 0
# win probability in all future rounds.  Save to sim_results_2026.rds so
# run_hodes_optimizer reads the correct simulation.
# ==============================================================================

cat("\n========================================\n")
cat("RESIMULATING WITH LOCKED RESULTS\n")
cat("========================================\n")

completed_slots <- list(
  R1_d1 = r1_d1_winners,
  R1_d2 = r1_d2_winners,
  R2_d1 = r2_d1_winners,
  R2_d2 = r2_d2_winners
)

name_map <- build_name_map(
  teams_dt       = sim$teams,
  team_names_csv = file.path(script_dir, "..", "team_names.csv")
)

sim_locked <- resimulate_with_locks(
  sim             = sim,
  completed_slots = completed_slots,
  name_map        = name_map,
  n_sims          = 2000000L,
  seed            = 42L
)

sim_file <- file.path(script_dir, "..", "sim_results_2026.rds")
saveRDS(sim_locked, sim_file)
cat(sprintf("Locked sim saved to %s  (%s sims x 63 games)\n",
            basename(sim_file), format(sim_locked$n_sims, big.mark = ",")))

# ==============================================================================
# 4. LOAD HODES STANDINGS
#
# TSV file: C:/Github/march_madness/hodes_usage/hodes_entries_2026.tsv
# Format:   tab-separated, first line is header junk ("Standings · ...")
# Columns:  Rank  Entry  R64_1  R64_2  R64_3  R32_1  R32_2  R32_3
#           R16_1  R16_2  QF  SF  Final  TB   (14 columns total)
# Note:     Hodes uses short team names (e.g. "Michigan St", "St John's")
#           which differ from bracket names ("Michigan State", "St. John's")
# ==============================================================================

cat("\n========================================\n")
cat("LOADING HODES STANDINGS\n")
cat("========================================\n")

hodes_tsv <- file.path(script_dir, "..", "hodes_usage", "hodes_entries_2026.tsv")
if (!file.exists(hodes_tsv)) stop("hodes_entries_2026.tsv not found: ", hodes_tsv)

# Skip first line (page header junk), read tab-separated, no auto-header
hodes_raw <- read.delim(hodes_tsv, header = FALSE, sep = "\t",
                        skip = 1, stringsAsFactors = FALSE,
                        na.strings = c("", "NA"))

# Handle variable column counts: take first 14 columns
ncols <- ncol(hodes_raw)
if (ncols < 14) stop(sprintf("TSV has only %d columns, expected >= 14", ncols))
hodes <- hodes_raw[, 1:14]
names(hodes) <- c("Rank", "Entry", "R64_1", "R64_2", "R64_3",
                   "R32_1", "R32_2", "R32_3", "R16_1", "R16_2",
                   "QF", "SF", "Final", "TB")
# Trim trailing whitespace from all character columns (TSV has trailing spaces)
for (j in names(hodes)) if (is.character(hodes[[j]])) hodes[[j]] <- trimws(hodes[[j]])
hodes$TB <- suppressWarnings(as.numeric(hodes$TB))

# R32 winners in Hodes naming convention (short names used in TSV)
r32_winners_hodes <- c(
  "Duke", "Michigan St", "Nebraska", "Illinois", "Houston", "Arkansas",
  "Texas", "Michigan",
  "St John's", "UConn", "Iowa", "Arizona", "Purdue", "Alabama",
  "Tennessee", "Iowa State"
)

# Filter to alive entries: all 3 R32 picks must be actual R32 winners
# This excludes: entries with NA/empty R32 picks (died in R64),
#                entries with "No Pick" in R32 columns (withdrew),
#                and entries whose R32 picks lost
r32_cols <- c("R32_1", "R32_2", "R32_3")
alive_mask <- rowSums(sapply(r32_cols, function(col)
  hodes[[col]] %in% r32_winners_hodes)) == 3
hodes <- hodes[alive_mask, ]

cat(sprintf("Hodes entries alive after R32: %d\n", nrow(hodes)))
cat(sprintf("TB values range: %.0f – %.0f\n", min(hodes$TB, na.rm=TRUE), max(hodes$TB, na.rm=TRUE)))

# Name mapping: Hodes TSV short names -> bracket full names
# All team names as they appear in hodes_entries_2026.tsv
hodes_to_bracket <- c(
  "Duke" = "Duke", "St John's" = "St. John's", "Kansas" = "Kansas",
  "Michigan St" = "Michigan State", "UCLA" = "UCLA", "UConn" = "UConn",
  "Florida" = "Florida", "Iowa" = "Iowa", "Arkansas" = "Arkansas",
  "Texas" = "Texas", "Illinois" = "Illinois", "Alabama" = "Alabama",
  "Arizona" = "Arizona", "Houston" = "Houston", "Purdue" = "Purdue",
  "Tennessee" = "Tennessee", "Michigan" = "Michigan", "Nebraska" = "Nebraska",
  "Iowa State" = "Iowa State", "VCU" = "VCU",
  "Texas A&M" = "Texas A&M", "Utah State" = "Utah State",
  "Saint Louis" = "Saint Louis", "TCU" = "TCU",
  "Vanderbilt" = "Vanderbilt", "Gonzaga" = "Gonzaga",
  "Louisville" = "Louisville", "Kentucky" = "Kentucky",
  "Virginia" = "Virginia", "Texas Tech" = "Texas Tech",
  "High Point" = "High Point", "Miami" = "Miami",
  "Saint Mary's" = "Saint Mary's", "Miami OH" = "Miami OH",
  "South Florida" = "South Florida", "Ohio State" = "Ohio State",
  "Saint Joseph's" = "Saint Joseph's", "Akron" = "Akron",
  "Clemson" = "Clemson", "Missouri" = "Missouri", "Georgia" = "Georgia",
  "McNeese" = "McNeese State", "Santa Clara" = "Santa Clara",
  "Long Island" = "LIU", "No Pick" = NA_character_
)

# Check for any unmapped team names still appearing in R32 picks (alive entries)
all_r32_teams <- unique(c(hodes$R32_1, hodes$R32_2, hodes$R32_3))
all_r32_teams <- all_r32_teams[!is.na(all_r32_teams) & all_r32_teams != ""]
unmapped <- setdiff(all_r32_teams, names(hodes_to_bracket))
if (length(unmapped) > 0) {
  cat("\nWARNING: Unmapped R32 team names:", paste(unmapped, collapse = ", "), "\n")
}

# ==============================================================================
# 5. ENTRY-LEVEL FIELD SIMULATION
#
# Uses hodes_entry_model_params.rds to predict each field entry's S16 pick(s):
#   - own_mandatory: expected S16 mandatory pick distribution (1 pick per entry)
#   - own_opt:       expected S16_opt pick distribution (p2 fraction of entries)
#
# Tiebreaker handling:
#   - P(take_two) from count model depends on n_alive, tb_rank_pct, n_available
#   - own_opt goes into own_by_round[["3_opt"]] -> p_field_tb1 in optimizer
#   - Entries with higher TB rank (closer to elimination) are more likely to take_two
# ==============================================================================

cat("\n========================================\n")
cat("ENTRY-LEVEL FIELD SIMULATION\n")
cat("========================================\n")

hodes_params <- readRDS(file.path(script_dir, "..", "hodes_entry_model_params.rds"))
cat(sprintf("Loaded entry model params: beta_wp=%.2f, beta_save=%.2f, beta_path=%.2f\n",
            hodes_params$s16$beta_wp, hodes_params$s16$beta_save, hodes_params$s16$beta_path))

# Build S16 matchup data from closing lines
cl <- read.csv(cl_file, stringsAsFactors = FALSE)

.strip_mascot <- function(full_name) {
  mapping <- c(
    "Michigan Wolverines" = "Michigan", "Michigan St Spartans" = "Michigan State",
    "Michigan St. Spartans" = "Michigan State", "Duke Blue Devils" = "Duke",
    "St. John's Red Storm" = "St. John's", "Iowa State Cyclones" = "Iowa State",
    "UConn Huskies" = "UConn", "Alabama Crimson Tide" = "Alabama",
    "Houston Cougars" = "Houston", "Illinois Fighting Illini" = "Illinois",
    "Nebraska Cornhuskers" = "Nebraska", "Iowa Hawkeyes" = "Iowa",
    "Purdue Boilermakers" = "Purdue", "Texas Longhorns" = "Texas",
    "Arizona Wildcats" = "Arizona", "Arkansas Razorbacks" = "Arkansas",
    "Tennessee Volunteers" = "Tennessee", "Florida Gators" = "Florida"
  )
  if (full_name %in% names(mapping)) return(mapping[[full_name]])
  parts <- strsplit(full_name, " ")[[1]]
  for (n in length(parts):1) {
    candidate <- paste(parts[1:n], collapse = " ")
    if (candidate %in% bracket$team) return(candidate)
  }
  full_name
}

cl$home_bracket <- sapply(cl$home_team, .strip_mascot)
cl$away_bracket <- sapply(cl$away_team, .strip_mascot)

s16_games <- cl[cl$home_bracket %in% bracket$team & cl$away_bracket %in% bracket$team &
                cl$date < "2026-04-01", ]

cat(sprintf("\nIdentified %d S16 games:\n", nrow(s16_games)))
for (i in seq_len(nrow(s16_games))) {
  cat(sprintf("  Game %d: %s (wp=%.3f) vs %s  [%s]\n", i,
              s16_games$home_bracket[i], s16_games$home_win_prob[i],
              s16_games$away_bracket[i], s16_games$date[i]))
}

# Build s16_teams data frame
s16_teams_df <- data.frame(
  bracket_name = c(s16_games$home_bracket, s16_games$away_bracket),
  stringsAsFactors = FALSE
)
s16_teams_df$seed <- sapply(s16_teams_df$bracket_name, function(t)
  bracket$seed[bracket$team == t][1])
s16_teams_df$wp <- NA_real_
for (i in seq_len(nrow(s16_games))) {
  s16_teams_df$wp[s16_teams_df$bracket_name == s16_games$home_bracket[i]] <- s16_games$home_win_prob[i]
  s16_teams_df$wp[s16_teams_df$bracket_name == s16_games$away_bracket[i]] <- 1 - s16_games$home_win_prob[i]
}
s16_teams_df$game_id <- NA_integer_
for (i in seq_len(nrow(s16_games))) {
  s16_teams_df$game_id[s16_teams_df$bracket_name == s16_games$home_bracket[i]] <- i
  s16_teams_df$game_id[s16_teams_df$bracket_name == s16_games$away_bracket[i]] <- i
}
s16_teams_df$opponent <- NA_character_
for (i in seq_len(nrow(s16_games))) {
  s16_teams_df$opponent[s16_teams_df$bracket_name == s16_games$home_bracket[i]] <- s16_games$away_bracket[i]
  s16_teams_df$opponent[s16_teams_df$bracket_name == s16_games$away_bracket[i]] <- s16_games$home_bracket[i]
}

# Championship equity from futures odds
# ---- UPDATE ODDS AS NEEDED ----
futures_odds <- c(
  "Michigan"       = 330,
  "Arizona"        = 350,
  "Duke"           = 380,
  "Houston"        = 700,
  "Purdue"         = 1300,
  "Illinois"       = 1400,
  "Iowa State"     = 1700,
  "UConn"          = 2500,
  "Michigan State" = 3000,
  "St. John's"     = 3500,
  "Arkansas"       = 4000,
  "Nebraska"       = 5000,
  "Tennessee"      = 6000,
  "Iowa"           = 12000,
  "Alabama"        = 13000,
  "Texas"          = 30000
)
futures_implied <- 100 / (futures_odds + 100)
futures_ce      <- futures_implied / sum(futures_implied)
s16_teams_df$champ_equity <- as.numeric(futures_ce[s16_teams_df$bracket_name])
s16_teams_df$champ_equity[is.na(s16_teams_df$champ_equity)] <- 0

cat("\nS16 teams:\n")
print(s16_teams_df[order(-s16_teams_df$wp), c("bracket_name", "seed", "wp", "game_id", "champ_equity")])

# Run entry-level field simulation
s16_own <- predict_hodes_s16_ownership(
  hodes_dt        = hodes,
  s16_teams       = s16_teams_df,
  params          = hodes_params,
  hodes_to_bracket = hodes_to_bracket
)

cat(sprintf("\nEntry-level field sim results (%d valid entries):\n", s16_own$n_valid))
cat(sprintf("Average P(take_two) = %.1f%%\n", 100 * s16_own$avg_p2))
cat(sprintf("Total mandatory picks = %.1f  (expected: ~%d)\n",
            sum(s16_own$own_mandatory), s16_own$n_valid))
cat(sprintf("Total opt picks = %.1f  (expected: avg_p2 * n_valid = %.1f)\n",
            sum(s16_own$own_opt), s16_own$avg_p2 * s16_own$n_valid))

cat("\nMandatory S16 ownership (% of alive entries):\n")
n_alive <- nrow(hodes)
ord <- order(s16_own$own_mandatory, decreasing = TRUE)
for (i in ord) {
  tn <- names(s16_own$own_mandatory)[i]
  cat(sprintf("  %-22s  mandatory=%5.1f%%  opt=%5.1f%%\n",
              tn,
              100 * s16_own$own_mandatory[tn] / n_alive,
              100 * s16_own$own_opt[tn] / n_alive))
}

# ==============================================================================
# 6. BUILD own_by_round
#    - Round 3 (S16): from entry-level model (mandatory + opt)
#    - Rounds 4-6 (E8/FF/CHAMP): optimizer uses GAM (entry_own_by_round = NULL
#      for those rounds, so optimizer fills in via estimate_hodes_ownership)
# ==============================================================================

own_by_round <- list(
  "3"     = s16_own$own_mandatory,   # mandatory S16 picks -> field survival
  "3_opt" = s16_own$own_opt          # S16_opt picks -> p_field_tb1
  # Rounds 4-6 are left out; optimizer will fill via GAM
)

# Hodes contest parameters — define here so sections 6b and 7 can both use them
HODES_N_ENTRIES    <- 21L
HODES_CONTEST_SIZE <- 1250L
HODES_PRIZE_POOL   <- 58000   # total prize pool in dollars
HODES_WIN_FRAC     <- 0.931   # fraction to winner (93.1%)

# ==============================================================================
# 6b. BUILD PORTFOLIO STATE FROM OUR ACTUAL PICKS
#
# Identifies our alive entries from the already-loaded `hodes` data frame
# and pre-populates R64 + R32 pick history so the optimizer knows which
# S16 teams are available to each entry.
# ==============================================================================

cat("\n========================================\n")
cat("BUILDING OUR PORTFOLIO STATE\n")
cat("========================================\n")

our_entry_names <- c(
  "SYDLOWSKI/J 1", "SYDLOWSKI/J 2", "SYDLOWSKI/J 3",
  "LONGO/T 1",     "LONGO/T 2",     "LONGO/T 3",
  "EVERSDEN/G 1",  "EVERSDEN/G 2",  "EVERSDEN/G 3",
  "SHORE/H 1",     "SHORE/H 2",     "SHORE/H 3",
  "JONES/K 1",     "JONES/K 2",     "JONES/K 3",
  "GELLNER/N 1",   "GELLNER/N 2",   "GELLNER/N 3",
  "HORCICIAK/M 1", "HORCICIAK/M 2", "HORCICIAK/M 3"
)

# Filter to our alive entries (hodes already filtered to alive field entries)
our_alive <- hodes[hodes$Entry %in% our_entry_names, ]
cat(sprintf("Our entries: %d alive out of %d total\n",
            nrow(our_alive), length(our_entry_names)))

# Helper: Hodes short name -> team_id (via bracket name)
hodes_name_to_id <- function(nm) {
  bn <- hodes_to_bracket[nm]
  if (is.na(bn)) return(NA_integer_)
  id <- teams$team_id[teams$name == bn]
  if (length(id) == 0L) NA_integer_ else id[1L]
}

# Initialize state sized to our alive entries
our_state <- init_hodes_portfolio(
  n_entries       = nrow(our_alive),
  contest_size    = HODES_CONTEST_SIZE,
  prize_pool      = HODES_PRIZE_POOL,
  winner_fraction = HODES_WIN_FRAC
)
our_state$entry_id <- our_alive$Entry  # use real names for readability

# Pre-populate R64 (R1) and R32 (R2) picks from TSV data
n_ok <- 0L
for (i in seq_len(nrow(our_alive))) {
  r64_hodes <- c(our_alive$R64_1[i], our_alive$R64_2[i], our_alive$R64_3[i])
  r32_hodes <- c(our_alive$R32_1[i], our_alive$R32_2[i], our_alive$R32_3[i])

  r64_ids <- sapply(r64_hodes, hodes_name_to_id)
  r32_ids <- sapply(r32_hodes, hodes_name_to_id)

  if (anyNA(r64_ids)) {
    cat(sprintf("  WARNING: unmapped R64 pick(s) for %s: %s\n",
                our_alive$Entry[i],
                paste(r64_hodes[is.na(r64_ids)], collapse = ", ")))
    next
  }
  if (anyNA(r32_ids)) {
    cat(sprintf("  WARNING: unmapped R32 pick(s) for %s: %s\n",
                our_alive$Entry[i],
                paste(r32_hodes[is.na(r32_ids)], collapse = ", ")))
    next
  }

  record_hodes_picks(our_state, i, "R1", r64_ids)
  record_hodes_picks(our_state, i, "R2", r32_ids)
  n_ok <- n_ok + 1L
}
cat(sprintf("Pick history loaded for %d / %d alive entries\n", n_ok, nrow(our_alive)))

cat("\nEntry pick histories (used teams at S16):\n")
for (i in seq_len(nrow(our_state))) {
  r1 <- c(our_state$pick_R1_a[i], our_state$pick_R1_b[i], our_state$pick_R1_c[i])
  r2 <- c(our_state$pick_R2_a[i], our_state$pick_R2_b[i], our_state$pick_R2_c[i])
  r1_nm <- teams$name[match(r1, teams$team_id)]
  r2_nm <- teams$name[match(r2, teams$team_id)]
  cat(sprintf("  %-18s  R64: %-42s  R32: %s\n",
              our_state$entry_id[i],
              paste(r1_nm, collapse = ", "),
              paste(r2_nm, collapse = ", ")))
}

# ==============================================================================
# 7. RUN HODES OPTIMIZER
#
# pick_s16opt = FALSE: we do NOT pick S16_opt for our entries.
#   - Simplifies beam search (no S16_opt enumeration)
#   - Our EV uses V_die_base (not TB1-adjusted), which is correct since
#     we won't have TB1
#   - The field's TB1 behavior is still correctly modeled via own_by_round[["3_opt"]]
# ==============================================================================

cat("\n========================================\n")
cat("RUNNING HODES OPTIMIZER\n")
cat("========================================\n")

result <- run_hodes_optimizer(
  sim_file           = sim_file,
  current_round      = 3L,
  n_entries          = nrow(our_state),
  contest_size       = HODES_CONTEST_SIZE,
  prize_pool         = HODES_PRIZE_POOL,
  winner_fraction    = HODES_WIN_FRAC,
  entry_own_by_round = own_by_round,
  pick_s16opt        = FALSE,
  sim_sample_size    = 50000L,
  initial_state      = our_state
)

cat("\n========================================\n")
cat("Day 5 Hodes (S16_d1) optimization complete!\n")
cat("========================================\n")
