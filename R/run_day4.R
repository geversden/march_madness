#!/usr/bin/env Rscript
# ==============================================================================
# run_day4.R — Day 4 (R2_d2) optimizer pipeline
#
# Orchestrates: scrape → lock R64+R32_d1 results → re-simulate → optimize → export CSVs
#
# Key differences from Day 3:
#   - 3 completed slots locked (R1_d1, R1_d2, R2_d1) instead of 2
#   - current_slot_id = "R2_d2"
#   - More constrained bracket → optionality matters more
#   - S16 ownership projection diagnostic to sanity-check future field behavior
#
# Usage:
#   source("R/run_day4.R")
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

# Build round_info (metadata for 63 tournament games)
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

# Build sim object
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
# 2. COMPLETED RESULTS — Fill in R64 + R32 Day 1 winners
#
# Winner names must match bracket_2026.csv exactly.
# Game-day mapping (from splash_config.R):
#   R1_d1 (Thu Mar 19): R64 games 1,2,5,6,11,12,13,14,15,16,19,20,21,22,25,26
#   R1_d2 (Fri Mar 20): R64 games 3,4,7,8,9,10,17,18,23,24,27,28,29,30,31,32
#   R2_d1 (Sat Mar 21): R32 games 33,35,38,39,40,42,43,45
# ==============================================================================

cat("\n========================================\n")
cat("LOCKING COMPLETED RESULTS\n")
cat("========================================\n")

r1_d1_winners <- c(
  # Thursday R64 winners
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
  # Friday R64 winners
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
# R32 Saturday games: 33, 35, 38, 39, 40, 42, 43, 45
# Game 33: Duke/TCU winner (East top)
# Game 35: Louisville/Michigan State winner (East bottom)
# Game 38: Vanderbilt/Nebraska winner (South top)
# Game 39: VCU/Illinois winner (South mid)
# Game 40: Texas A&M/Houston winner (South bottom)
# Game 42: High Point/Arkansas winner (West mid)
# Game 43: Texas/Gonzaga winner (West bottom)
# Game 45: Michigan/Saint Louis winner (Midwest top)

cat("\nSaturday R32 matchups:\n")
for (g in R32_SAT_GAMES) {
  f1 <- 2 * (g - 33) + 1; f2 <- 2 * (g - 33) + 2
  t1_name <- if (f1 <= length(r1_d1_winners) + length(r1_d2_winners)) {
    # Find which R64 game feeds: r64 games f1, f2
    # Winners from the appropriate day
    NA  # placeholder - actual matchup from locked sims
  } else NA
  cat(sprintf("  Game %d: feeds from R64 games %d, %d\n", g, f1, f2))
}

r2_d1_winners <- c(
  # ---- FILL IN SATURDAY R32 WINNERS HERE ----
  # Match order to R32_SAT_GAMES: 33, 35, 38, 39, 40, 42, 43, 45
  "Duke",           # Game 33: Duke vs TCU
  "Michigan State", # Game 35: Louisville vs Michigan State
  "Nebraska",     # Game 38: Vanderbilt vs Nebraska
  "Illinois",       # Game 39: VCU vs Illinois
  "Houston",        # Game 40: Texas A&M vs Houston
  "Arkansas",       # Game 42: High Point vs Arkansas
  "Texas",          # Game 43: Texas vs Gonzaga
  "Michigan"        # Game 45: Michigan vs Saint Louis
)

completed_slots <- list(
  R1_d1 = r1_d1_winners,
  R1_d2 = r1_d2_winners,
  R2_d1 = r2_d1_winners
)

cat(sprintf("\nLocked: %d R64 games + %d R32_d1 games = %d total\n",
            length(r1_d1_winners) + length(r1_d2_winners),
            length(r2_d1_winners),
            length(r1_d1_winners) + length(r1_d2_winners) + length(r2_d1_winners)))

# ==============================================================================
# 3. CALIBRATE RATINGS TO R32 CLOSING LINES
#
# By Day 4, we have closing lines for all R32 games.
# Calibrate to both Saturday and Sunday R32 lines.
# These calibrated ratings feed S16+ simulations.
# ==============================================================================

r32_dates <- c("2026-03-21", "2026-03-22")  # R32 Saturday + Sunday
cl_file <- file.path(script_dir, "..", "closing_lines", "ncaat_2026_closing_lines.csv")

sim$teams <- calibrate_ratings_to_lines(
  teams_dt         = sim$teams,
  closing_lines_csv = cl_file,
  team_names_csv   = file.path(script_dir, "..", "team_names.csv"),
  log_scale        = 0.0917,
  lambda           = 0.0001,
  round_dates      = r32_dates
)

cat(sprintf("\nCalibrated %d team ratings to R32 closing lines\n",
            sum(sim$teams$rating_delta != 0)))

# ==============================================================================
# 4. SCRAPE OPPONENT DATA
# ==============================================================================

# Option A: Live scrape (requires fresh bearer token from Splash)
bearer_token <- "eyJraWQiOiJENHJOR1pwNStnTzAxS21aVkg5YlZDZUd2bGNGYUNJSm1qVm5VOE4waUl3PSIsImFsZyI6IlJTMjU2In0.eyJmcmF1ZEZsYWciOiJ2ZXJpZmllZC1hY2NvdW50Iiwic3ViIjoiMTQ0OGE0MTgtOTBhMS03MDlhLTBmMTAtZDc0Y2MxOTUzMGMwIiwicm9sZSI6ImNvbW1pc3Npb25lciIsImVtYWlsX3ZlcmlmaWVkIjoidHJ1ZSIsInJvbGVzIjoiW1wiY29tbWlzc2lvbmVyXCJdIiwiaXNzIjoiaHR0cHM6XC9cL2NvZ25pdG8taWRwLnVzLWVhc3QtMS5hbWF6b25hd3MuY29tXC91cy1lYXN0LTFfNjRCOUJuQzVnIiwicmVzdHJpY3Rpb25zIjoiW10iLCJjb2duaXRvOnVzZXJuYW1lIjoiMTIyNGZjOTgtMDViNy00ZWQ3LTg5NmItZDhiMTZjMTk1ZTEyIiwib2ZwX3VzZXJfaWQiOiI0MzY4MzA2IiwicnlwX3VzZXJfaWQiOiIxNTMyNDA0Iiwib3JpZ2luX2p0aSI6ImI5MWI0YmJjLWU0ZDEtNDY1Yy1hNjk1LWU4OGVhZTg1NDhhZSIsImF1ZCI6IjU5aGJoYmpoa2FmOTg0bWVtb2M5ZmdhMTNxIiwiZXZlbnRfaWQiOiIyYWU4ZDE1ZS00NGFmLTQ4ZWUtOTI5NC1jYjQwYzAyZWU1ZTciLCJzcGxhc2hfdXNlcl9pZCI6IjEyMjRmYzk4LTA1YjctNGVkNy04OTZiLWQ4YjE2YzE5NWUxMiIsInRva2VuX3VzZSI6ImlkIiwiYXV0aF90aW1lIjoxNzczMjkzNTIzLCJuYW1lIjoiMTIyNGZjOTgtMDViNy00ZWQ3LTg5NmItZDhiMTZjMTk1ZTEyIiwiaWQiOiIxMjI0ZmM5OC0wNWI3LTRlZDctODk2Yi1kOGIxNmMxOTVlMTIiLCJleHAiOjE3NzQxOTU0MzEsImlhdCI6MTc3NDE5MTgzMSwiYWdlIjoiMzciLCJqdGkiOiIzZDFkOTM3MS1iN2UwLTQxNTUtOTU4Mi0zYzA1NDNlNDVjMjQiLCJ1c2VybmFtZSI6IlRpbmt5VHlsZXIifQ.e7GUOYJX46rIa_H6vMKB1mPSiiYq1SZKvKlCno290ePedHvKIFlHNRL5GqQ60x4ykXDrbJoVU9QB1kvk-_0xvT8sYN-0kM3t-FUq2U5pDqhVHXQ20cJ-UX1dXpe8BRFPDZh1q_eZGxTk1vModYPchha5FUVrmsA-7XQBiIgO5BMBXhcAOAHsynu-m7hhe9f5NQXoXKrdP6RLooHJJAu8m8DlMJqpTxZHgdSx-nM7dCYVnIxbM9pzyvkudOvi5TVRk_xTFLiSL24WrZNXmnPz1xnkQfBOp9fk9G_4zdUkhVwo_F9kfXElsII5HOHjrAU2-X435ni1RidFp_lRduw3Yw"
scrape <- scrape_all_splash(bearer_token)
# saveRDS(scrape, file.path(script_dir, "..", "scrape_day4.rds"))

# Option B: Load cached scrape
# scrape <- readRDS(file.path(script_dir, "..", "scrape_day4.rds"))

# ==============================================================================
# 5. PREPARE OPTIMIZER INPUTS
#    (re-simulates with locked R64 + R32_d1 results using calibrated ratings,
#     builds portfolio, infers alive status)
# ==============================================================================

inputs <- prepare_optimizer_inputs(
  scrape_results   = scrape,
  sim              = sim,
  current_slot_id  = "R2_d2",
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
# 6. S16 OWNERSHIP PROJECTION DIAGNOSTIC
#
# After R32, we have rich information about the field's pick histories:
#   - Which teams each opponent used in R1+R2
#   - Their remaining available teams for S16+
#   - Revealed preferences (chalk-heavy pickers will keep picking chalk)
#
# The model projects S16 ownership through field group simulation:
#   predict_field_group_picks() uses each group's used_teams to:
#     1. Zero out already-used teams (hard constraint)
#     2. Pick-against boost for teams facing used opponents (path viability)
#     3. Base attractiveness via beta_wp * win_prob - save_strength * fv
#
# This diagnostic shows projected S16 ownership for several scenarios,
# conditioned on different R2_d2 outcomes. This helps verify the model
# isn't being naive about opponent future behavior.
#
# KEY INSIGHT: If Gonzaga was very popular in R2_d1, those Gonzaga-pickers:
#   - Can't use Gonzaga again (constraint ✓ - handled by used_teams)
#   - Likely need Arizona (same region, feeds into same S16 game) to stay
#     viable, OR need to pick against Arizona's bracket path
#   - The model captures this via grouping: Gonzaga-pickers form their own
#     group with Gonzaga in used_teams, which affects their S16 distribution
#
# WHAT THE MODEL DOES NOT DO (potential gap):
#   - It doesn't upweight chalk for groups that historically picked chalk
#   - The base attractiveness (win_prob) is the same for all groups;
#     only the constraint (used_teams) and pick-against boost differ
#   - In practice this is a reasonable approximation because the constraints
#     themselves are very powerful at narrowing the option set
# ==============================================================================

cat("\n========================================\n")
cat("S16 OWNERSHIP PROJECTION DIAGNOSTIC\n")
cat("========================================\n")

project_s16_ownership <- function(inputs, r2_d2_scenario, scenario_label) {
  # We need field groups from the scrape. Use the largest contest for diagnostics.
  if (is.null(inputs$field_avail) || length(inputs$field_avail) == 0) {
    cat("  [SKIP] No field availability data — run scrape first\n")
    return(invisible(NULL))
  }

  # Find largest contest
  biggest_cid <- names(which.max(sapply(inputs$field_avail, function(fa) fa$alive_count)))
  fa <- inputs$field_avail[[biggest_cid]]

  sim_matrix <- inputs$sim$all_results
  teams_dt <- inputs$sim$teams

  cat(sprintf("\n--- Scenario: %s ---\n", scenario_label))
  cat(sprintf("  R2_d2 winners: %s\n", paste(r2_d2_scenario, collapse = ", ")))
  cat(sprintf("  Contest: %s (%d alive field entries)\n", fa$contest_name, fa$alive_count))

  # ---- Step 1: Filter sims to those consistent with R2_d2 outcomes ----
  # This ensures S16 candidates are only teams that actually advanced.
  scenario_ids <- match(r2_d2_scenario, teams_dt$name)
  if (any(is.na(scenario_ids))) {
    bad <- r2_d2_scenario[is.na(scenario_ids)]
    cat(sprintf("  [ERROR] Can't map scenario teams: %s\n", paste(bad, collapse = ", ")))
    return(invisible(NULL))
  }
  r2d2_games <- R32_SUN_GAMES  # 34, 36, 37, 41, 44, 46, 47, 48
  stopifnot(length(r2d2_games) == length(scenario_ids))

  match_mask <- rep(TRUE, nrow(sim_matrix))
  for (i in seq_along(r2d2_games)) {
    match_mask <- match_mask & (sim_matrix[, r2d2_games[i]] == scenario_ids[i])
  }
  n_match <- sum(match_mask)
  cat(sprintf("  Sims matching scenario: %s / %s (%.1f%%)\n",
              format(n_match, big.mark = ","),
              format(nrow(sim_matrix), big.mark = ","),
              100 * n_match / nrow(sim_matrix)))

  if (n_match < 1000) {
    cat("  [SKIP] Too few matching sims for reliable projection (need 1000+)\n")
    return(invisible(NULL))
  }

  # Subsample to 200K for speed if needed
  filtered_matrix <- sim_matrix[match_mask, , drop = FALSE]
  if (nrow(filtered_matrix) > 200000) {
    filtered_matrix <- filtered_matrix[sample.int(nrow(filtered_matrix), 200000), , drop = FALSE]
  }

  # ---- Step 2: Group field entries (used_teams through R1+R2_d1 from scrape) ----
  field_groups <- group_field_entries(fa)
  if (nrow(field_groups) == 0) {
    cat("  No field groups to analyze\n")
    return(invisible(NULL))
  }

  calib_params <- tryCatch(load_calibrated_params(), error = function(e) NULL)

  # ---- Step 3: Simulate R2_d2 picks per group, then expand with R2_d2 in used_teams ----
  # Use the UNFILTERED sim matrix for R2_d2 pick prediction (these are pre-game probs).
  r2d2_preds <- predict_field_group_picks(
    field_groups, "R2_d2", teams_dt, sim_matrix,
    contest_size = fa$alive_count,
    calib_params = calib_params,
    ownership_override = NULL
  )

  # For each original group, expand: each possible R2_d2 pick that is a scenario
  # winner creates a sub-group with that pick added to used_teams.
  # Groups whose R2_d2 pick was a LOSER are eliminated (don't reach S16).
  scenario_winner_set <- scenario_ids
  expanded_list <- list()
  total_survived <- 0
  total_killed <- 0

  for (gi in seq_len(nrow(field_groups))) {
    grp <- field_groups[gi]
    pick_probs <- r2d2_preds$pick_probs[gi, ]

    for (ci in seq_along(r2d2_preds$team_ids)) {
      if (pick_probs[ci] < 0.001) next
      tid <- r2d2_preds$team_ids[ci]

      if (tid %in% scenario_winner_set) {
        # This group subset picked a winner — they advance to S16
        new_weight <- grp$n_entries * pick_probs[ci]
        expanded_list[[length(expanded_list) + 1]] <- data.table(
          group_id   = length(expanded_list) + 1L,
          used_teams = list(c(grp$used_teams[[1]], tid)),
          n_entries  = new_weight  # fractional OK, used only for weighting
        )
        total_survived <- total_survived + new_weight
      } else {
        # This group subset picked a loser — eliminated
        total_killed <- total_killed + grp$n_entries * pick_probs[ci]
      }
    }
  }

  if (length(expanded_list) == 0) {
    cat("  No field entries survive to S16 in this scenario\n")
    return(invisible(NULL))
  }

  expanded_dt <- rbindlist(expanded_list)
  # Collapse groups with identical used_teams for efficiency
  expanded_dt[, fingerprint := vapply(used_teams, function(ut) {
    paste(sort(ut), collapse = ",")
  }, character(1))]
  collapsed <- expanded_dt[, .(
    used_teams = used_teams[1],
    n_entries  = sum(n_entries)
  ), by = fingerprint]
  collapsed[, group_id := .I]
  collapsed[, fingerprint := NULL]
  setcolorder(collapsed, c("group_id", "used_teams", "n_entries"))

  cat(sprintf("  Field after R2_d2: %.0f survive (%.0f eliminated) -> %d groups\n",
              total_survived, total_killed, nrow(collapsed)))

  # ---- Step 4: Project S16 ownership using expanded groups + filtered sims ----
  for (s16_slot in c("S16_d1", "S16_d2")) {
    preds <- predict_field_group_picks(
      collapsed, s16_slot, teams_dt, filtered_matrix,
      contest_size = round(total_survived),
      calib_params = calib_params,
      ownership_override = NULL
    )

    if (length(preds$team_ids) == 0) {
      cat(sprintf("  %s: no candidates\n", s16_slot))
      next
    }

    # Weighted average ownership across expanded groups
    weights <- collapsed$n_entries / sum(collapsed$n_entries)
    avg_own <- as.numeric(crossprod(weights, preds$pick_probs))
    names(avg_own) <- teams_dt$name[preds$team_ids]

    # Sort and display
    ord <- order(avg_own, decreasing = TRUE)
    cat(sprintf("\n  %s projected ownership (model):\n", s16_slot))
    cat(sprintf("    %-25s %8s %6s\n", "Team", "Own%", "Seed"))
    cat(sprintf("    %s\n", paste(rep("-", 42), collapse = "")))
    for (j in ord) {
      if (avg_own[j] >= 0.005) {
        tid <- preds$team_ids[j]
        cat(sprintf("    %-25s %7.1f%% %5d\n",
                    names(avg_own)[j], avg_own[j] * 100, teams_dt$seed[tid]))
      }
    }

    # Show breakdown for top 3 expanded groups (biggest weights)
    top_grp_idx <- order(collapsed$n_entries, decreasing = TRUE)[1:min(3, nrow(collapsed))]
    cat(sprintf("\n    Breakdown by top field groups:\n"))
    for (gi in top_grp_idx) {
      grp <- collapsed[gi]
      used_names <- teams_dt$name[grp$used_teams[[1]]]
      grp_probs <- preds$pick_probs[gi, ]
      top3_idx <- order(grp_probs, decreasing = TRUE)[1:min(3, length(grp_probs))]
      top3_str <- paste(sprintf("%s(%.0f%%)",
                                teams_dt$name[preds$team_ids[top3_idx]],
                                grp_probs[top3_idx] * 100),
                        collapse = ", ")
      cat(sprintf("    Group %d (%.0f entries, used: %s)\n",
                  gi, grp$n_entries, paste(used_names, collapse = "+")))
      cat(sprintf("      -> Top picks: %s\n", top3_str))
    }
  }

  invisible(NULL)
}

# ---- Define R2_d2 scenarios ----
# R32 Sunday games: 34, 36, 37, 41, 44, 46, 47, 48
# Game 34: St. John's vs Kansas         (East)
# Game 36: UCLA vs UConn                (East)
# Game 37: Florida vs Iowa              (South)
# Game 41: Arizona vs Utah State        (West)
# Game 44: Miami vs Purdue              (West)
# Game 46: Texas Tech vs Alabama        (Midwest)
# Game 47: Tennessee vs Virginia         (Midwest)
# Game 48: Kentucky vs Iowa State       (Midwest)

# Scenario A: Chalk holds (favorites win)
r2_d2_chalk <- c(
  "St. John's",     # Game 34
  "UConn",          # Game 36
  "Florida",        # Game 37
  "Arizona",        # Game 41
  "Purdue",         # Game 44
  "Alabama",        # Game 46
  "Tennessee",      # Game 47
  "Iowa State"      # Game 48
)

# Scenario B: A few upsets (mid-seeds advance)
r2_d2_mixed <- c(
  "Kansas",         # Game 34 — 4-seed over 5-seed
  "UConn",          # Game 36
  "Florida",        # Game 37
  "Arizona",        # Game 41
  "Purdue",         # Game 44
  "Texas Tech",     # Game 46 — 5-seed over 4-seed
  "Tennessee",      # Game 47
  "Iowa State"      # Game 48
)

# Scenario C: Major upsets (longshots advance)
r2_d2_upset <- c(
  "Kansas",         # Game 34
  "UCLA",           # Game 36 — 7-seed over 2-seed
  "Iowa",           # Game 37 — 9-seed over 1-seed
  "Utah State",     # Game 41 — 9-seed over 1-seed
  "Miami",          # Game 44 — 7-seed over 2-seed
  "Alabama",        # Game 46
  "Virginia",       # Game 47 — 3-seed over 6-seed
  "Kentucky"        # Game 48 — 7-seed over 2-seed
)

# Run projections for each scenario
# NOTE: These only work after prepare_optimizer_inputs() has been run (step 5)
# because they need the field availability data and sim results.
tryCatch({
  project_s16_ownership(inputs, r2_d2_chalk, "Chalk Holds")
  project_s16_ownership(inputs, r2_d2_mixed, "Mixed Results")
  project_s16_ownership(inputs, r2_d2_upset, "Major Upsets")
}, error = function(e) {
  cat(sprintf("\n  [NOTE] S16 projection skipped: %s\n", e$message))
  cat("  This is expected if running sections out of order.\n")
  cat("  Run steps 4-5 first, then re-run this section.\n")
})

cat("\n========================================\n")
cat("END S16 OWNERSHIP DIAGNOSTIC\n")
cat("========================================\n")

# ==============================================================================
# 7. OWNERSHIP OVERRIDE
#
# After R32 Day 1, we know the field's used teams through 3 rounds.
# For R2_d2, provide actual or estimated ownership.
# For S16, consider providing rough overrides if you have a strong read.
#
# NOTE: The model's S16 estimates (from the diagnostic above) are based on
# the generic win_prob model. If you believe certain teams will be more/less
# popular than the model suggests (e.g., Arizona will be extremely popular
# because all the Gonzaga-pickers need a West team), override S16 here.
# ==============================================================================

ownership_override <- list(
  R2_d2 = c(
    # ---- FILL IN R2_d2 OWNERSHIP FROM SCRAPE/READS ----
    # These should sum to ~1.0
    "Purdue"         = 0.18,
    "St. John's"     = 0.24,
    "Iowa State"     = 0.12,
    "UConn"          = 0.09,
    "Arizona"        = 0.09,
    "Florida"        = 0.08,
    "Tennessee"      = 0.05,
    "Alabama"        = 0.03,
    "Texas Tech"     = 0.05,
    "Virginia"       = 0.04,
    "Kansas"         = 0.01,
    "UCLA"           = 0.002,
    "Kentucky"       = 0.002,
    "Miami"          = 0.001,
    "Iowa"           = 0.000,
    "Utah State"     = 0.000
  ),
  S16_d1 = c(
    "Purdue"         = 0.3098,
    "Houston"        = 0.2730,
    "Illinois"       = 0.1139,
    "Arizona"        = 0.1290,
    "Florida"        = 0.1449,
    "Arkansas"       = 0.0074,
    "Nebraska"       = 0.0116,
    "Texas"          = 0.0102
  ),
  S16_d2 = c(
    "Iowa State"     = 0.2598,
    "Michigan State" = 0.2512,
    "UConn"          = 0.1422,
    "Duke"           = 0.1307,
    "Tennessee"      = 0.0995,
    "Michigan"       = 0.0989,
    "St. John's"     = 0.0140,
    "Alabama"        = 0.0036
  )
  # Optional: add S16 overrides if you have a strong read
  # S16_d1 = c( ... ),
  # S16_d2 = c( ... )
)

# ==============================================================================
# 8. RUN OPTIMIZER
# ==============================================================================

# locked_teams = teams whose games have ALREADY STARTED today (can't pick them).
# At the start of R2_d2 (before any Sunday games tip), this is empty.
# If re-running mid-day after some games started, add those teams here.
locked_teams <- NULL

# options(splash.verbose = TRUE)           # Uncomment for detailed beam search diagnostics
options(splash.optionality_weight = 0.20)  # Bump if tighter constraints warrant it (default 0.10)
result <- run_optimizer(
  scrape_inputs      = inputs,
  current_slot_id    = "R2_d2",
  ownership_override = ownership_override,
  locked_teams       = locked_teams,
  sim_sample_size    = 10000
)

# ==============================================================================
# 9. EXPORT CSVs
#
# Fills Pick 1 in template CSVs. Day 4 templates must be downloaded from Splash
# and placed in splash_entry_csvs/ before running this step.
# ==============================================================================

csv_dir <- file.path(script_dir, "..", "splash_entry_csvs")
export_picks(result, csv_dir = csv_dir, locked_teams = locked_teams)

cat("\n========================================\n")
cat("Day 4 (R2_d2) optimization complete!\n")
cat("========================================\n")
