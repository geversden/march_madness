#!/usr/bin/env Rscript
# ==============================================================================
# run_day5_sim_only.R — Re-simulate 2026 with R64 + R32 locked
#
# Outputs updated championship / FF / E8 / S16 probabilities.
# No scrape, no optimizer, no CSV export.
#
# Usage:  source("run_day5_sim_only.R")
#   or    Rscript run_day5_sim_only.R
# ==============================================================================

library(Rcpp)
library(data.table)

# Capture project root before any source() calls (sourced files may overwrite script_dir)
PROJ <- normalizePath(tryCatch(dirname(sys.frame(1)$ofile), error = function(e) "."))
sourceCpp(file.path(PROJ, "simulate_tourney.cpp"))
source(file.path(PROJ, "R/splash_config.R"))
source(file.path(PROJ, "R/splash_state.R"))
source(file.path(PROJ, "R/calibrate_win_prob.R"))
source(file.path(PROJ, "R/splash_prepare.R"))

YEAR          <- 2026L
UPDATE_FACTOR <- 0.5
N_SIMS        <- 2000000L

# ==============================================================================
# 1. BRACKET + KENPOM
# ==============================================================================

bracket <- read.csv(file.path(PROJ, "brackets", sprintf("bracket_%d.csv", YEAR)),
                    stringsAsFactors = FALSE)
stopifnot(nrow(bracket) == 64)

teams <- data.frame(
  name    = bracket$team,
  seed    = bracket$seed,
  region  = bracket$region,
  team_id = 1:64,
  stringsAsFactors = FALSE
)

# Prefer tournament-specific KenPom (more current ratings) over pre-tournament file
kenpom_file <- file.path(PROJ, "kenpom_data", sprintf("kenpom_%d_tournament.csv", YEAR))
if (!file.exists(kenpom_file)) {
  kenpom_file <- file.path(PROJ, "kenpom_data", sprintf("kenpom_%d.csv", YEAR))
  if (!file.exists(kenpom_file)) {
    dated <- sort(Sys.glob(file.path(PROJ, "kenpom_data",
                                     sprintf("kenpom_ratings_%d-*.csv", YEAR))),
                  decreasing = TRUE)
    if (length(dated) > 0) kenpom_file <- dated[1]
  }
}
cat(sprintf("Using KenPom file: %s\n", basename(kenpom_file)))
kp <- read.csv(kenpom_file, stringsAsFactors = FALSE)
if ("team"           %in% names(kp)) names(kp)[names(kp) == "team"]           <- "Team"
if ("adj_em"         %in% names(kp)) names(kp)[names(kp) == "adj_em"]         <- "NetRtg"
if ("adj_kempom_rtg" %in% names(kp)) names(kp)[names(kp) == "adj_kempom_rtg"] <- "NetRtg"
if ("ken_pom_rating" %in% names(kp) && !"NetRtg" %in% names(kp))
  names(kp)[names(kp) == "ken_pom_rating"] <- "NetRtg"
kp <- kp[!is.na(kp$Team) & kp$Team != "" & kp$Team != "Team", ]
kp$Team   <- gsub("\\s*\\d+$", "", trimws(kp$Team))
kp$NetRtg <- as.numeric(kp$NetRtg)
kp <- kp[!is.na(kp$NetRtg), ]
kp_lookup <- setNames(kp$NetRtg, kp$Team)

team_dict  <- read.csv(file.path(PROJ, "team_names.csv"), stringsAsFactors = FALSE)
kp_alias   <- setNames(team_dict$kenpom_name, team_dict$bracket_name)

get_rating <- function(name) {
  kp_name <- if (name %in% names(kp_alias)) kp_alias[[name]] else name
  if (kp_name %in% names(kp_lookup)) return(kp_lookup[[kp_name]])
  if (name   %in% names(kp_lookup)) return(kp_lookup[[name]])
  matches <- grep(kp_name, names(kp_lookup), value = TRUE, ignore.case = TRUE)
  if (length(matches) == 0 && kp_name != name)
    matches <- grep(name, names(kp_lookup), value = TRUE, ignore.case = TRUE)
  if (length(matches) > 0) return(kp_lookup[[matches[1]]])
  warning(sprintf("No KenPom rating for '%s', using 0", name)); 0
}

teams$rating <- sapply(teams$name, get_rating)
cat(sprintf("Loaded %d teams from KenPom %d\n", nrow(teams), YEAR))

# Build round_info
region_names    <- unique(teams$region)
round_info      <- data.frame(
  round_num  = c(rep(1,32), rep(2,16), rep(3,8), rep(4,4), rep(5,2), 6),
  round_name = c(rep("R64",32), rep("R32",16), rep("S16",8),
                 rep("E8",4), rep("FF",2), "Championship"),
  game_col   = 1:63,
  stringsAsFactors = FALSE
)
round_info$region <- NA_character_
games_per_region  <- c(8, 4, 2, 1)
for (rd in 1:4) {
  start_col <- c(1, 33, 49, 57)[rd]
  gpr <- games_per_region[rd]
  for (r in 1:4) {
    cols <- start_col + ((r-1)*gpr):(r*gpr - 1)
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

# ==============================================================================
# 2. CALIBRATE TO CLOSING LINES (R32 + S16 Day 1)
# ==============================================================================

calibration_dates <- c("2026-03-24", "2026-03-25", "2026-03-26", "2026-03-27")
cl_file <- file.path(PROJ, "closing_lines", "ncaat_2026_closing_lines.csv")

sim$teams <- calibrate_ratings_to_lines(
  teams_dt          = sim$teams,
  closing_lines_csv = cl_file,
  team_names_csv    = file.path(PROJ, "team_names.csv"),
  log_scale         = 0.0917,
  lambda            = 0.0001,
  round_dates       = calibration_dates
)
cat(sprintf("Calibrated %d team ratings to closing lines\n",
            sum(sim$teams$rating_delta != 0)))

# Manual rating overrides (positive = bump up, negative = bump down)
# Use to correct for market signals not captured in closing lines
rating_overrides <- c(
  "Arizona" = 2.25,
  "Michigan" = 1.25,
  "Purdue" = 2.25,
  "Houston" = 1.25,
  "Illinois" = 1,
  "UConn" = 0.5
)
for (tm in names(rating_overrides)) {
  idx <- which(sim$teams$name == tm)
  if (length(idx) == 1) {
    sim$teams$rating[idx] <- sim$teams$rating[idx] + rating_overrides[[tm]]
    cat(sprintf("Manual override: %s rating %+.1f -> %.1f\n",
                tm, rating_overrides[[tm]], sim$teams$rating[idx]))
  }
}

# ==============================================================================
# 3. LOCK COMPLETED RESULTS
# ==============================================================================

completed_slots <- list(
  R1_d1 = c(
    "Duke", "TCU", "Louisville", "Michigan State",
    "Vanderbilt", "Nebraska", "VCU", "Illinois",
    "Texas A&M", "Houston", "High Point", "Arkansas",
    "Texas", "Gonzaga", "Michigan", "Saint Louis"
  ),
  R1_d2 = c(
    "St. John's", "Kansas", "UCLA", "UConn",
    "Florida", "Iowa", "Arizona", "Utah State",
    "Miami", "Purdue", "Texas Tech", "Alabama",
    "Tennessee", "Virginia", "Kentucky", "Iowa State"
  ),
  R2_d1 = c(
    "Duke", "Michigan State", "Nebraska", "Illinois",
    "Houston", "Arkansas", "Texas", "Michigan"
  ),
  R2_d2 = c(
    "St. John's", "UConn", "Iowa", "Arizona",
    "Purdue", "Alabama", "Tennessee", "Iowa State"
  )
)

cat(sprintf("Locking %d completed games (R64 + R32)\n", length(unlist(completed_slots))))

# ==============================================================================
# 4. RE-SIMULATE
# ==============================================================================

name_map <- build_name_map(sim$teams, file.path(PROJ, "team_names.csv"))
set.seed(42)

t0     <- proc.time()
new_sim <- resimulate_with_locks(sim, completed_slots, name_map,
                                 n_sims = N_SIMS, seed = 42)
elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("Re-simulated %s sims in %.1fs\n\n",
            format(N_SIMS, big.mark = ","), elapsed))

# ==============================================================================
# 5. PRINT UPDATED PROBABILITIES
# ==============================================================================

ar    <- new_sim$all_results
ri    <- new_sim$round_info
nsims <- new_sim$n_sims
tm    <- new_sim$teams

champ_counts <- tabulate(ar[, 63], nbins = 64)
ff_cols      <- which(ri$round_num == 5)
e8_cols      <- which(ri$round_num == 4)
s16_cols     <- which(ri$round_num == 3)

ff_counts  <- tabulate(as.vector(ar[, ff_cols]),  nbins = 64)
e8_counts  <- tabulate(as.vector(ar[, e8_cols]),  nbins = 64)
s16_counts <- tabulate(as.vector(ar[, s16_cols]), nbins = 64)

# Only show surviving teams (those who won their R32 game)
r32_winners <- unique(as.vector(ar[1, which(ri$round_num == 2)]))  # just check alive set
alive_ids <- unique(as.vector(ar[, which(ri$round_num == 2)]))

prob_df <- data.frame(
  seed   = tm$seed,
  region = tm$region,
  team   = tm$name,
  rating = round(tm$rating, 1),
  s16    = round(100 * s16_counts / nsims, 1),
  e8     = round(100 * e8_counts  / nsims, 1),
  ff     = round(100 * ff_counts  / nsims, 1),
  champ  = round(100 * champ_counts / nsims, 1),
  stringsAsFactors = FALSE
)

# Only show teams still alive
prob_df <- prob_df[prob_df$team %in% tm$name[alive_ids], ]
prob_df <- prob_df[order(-prob_df$champ), ]

cat("========================================================\n")
cat("   2026 UPDATED PROBABILITIES (after Round 2)\n")
cat("========================================================\n")
cat(sprintf("%-22s %2s  %-8s  %6s  %5s %5s %5s %6s\n",
            "Team", "Sd", "Region", "AdjEM", "S16%", "E8%", "FF%", "Champ%"))
cat(paste(rep("-", 72), collapse=""), "\n")
for (i in seq_len(nrow(prob_df))) {
  r <- prob_df[i, ]
  cat(sprintf("%-22s %2d  %-8s  %5.1f  %5.1f %5.1f %5.1f %6.1f\n",
              r$team, r$seed, r$region, r$rating,
              r$s16, r$e8, r$ff, r$champ))
}
cat("========================================================\n")

# Save results
out_file <- file.path(PROJ, sprintf("sim_results_%d_after_r2.rds", YEAR))
saveRDS(new_sim, out_file)
cat(sprintf("\nSaved to %s\n", basename(out_file)))
