#!/usr/bin/env Rscript
# ==============================================================================
# NCAA March Madness Tournament Simulator
# Uses Rcpp for fast bracket simulation with actual KenPom ratings
#
# Usage:  Rscript march_madness.R [YEAR]
#         Rscript march_madness.R 2024
#   or    TOURNEY_YEAR=2024; source("march_madness.R")
# ==============================================================================

library(Rcpp)

# Compile and load the C++ simulation engine
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
sourceCpp(file.path(script_dir, "simulate_tourney.cpp"))

# ==============================================================================
# DETERMINE TOURNAMENT YEAR
# ==============================================================================

# Priority: command-line arg > TOURNEY_YEAR env var > default 2025
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  YEAR <- as.integer(args[1])
} else if (nzchar(Sys.getenv("TOURNEY_YEAR", ""))) {
  YEAR <- as.integer(Sys.getenv("TOURNEY_YEAR"))
} else {
  YEAR <- 2026L
}
cat(sprintf("Tournament year: %d\n\n", YEAR))

# ==============================================================================
# LOAD KENPOM RATINGS
# ==============================================================================

kenpom_file <- file.path(script_dir, "kenpom_data",
                         sprintf("kenpom_%d.csv", YEAR))

# Fallback: look for dated kenpom_ratings_YYYY-MM-DD.csv files (most recent first)
if (!file.exists(kenpom_file)) {
  dated_files <- sort(
    Sys.glob(file.path(script_dir, "kenpom_data",
                       sprintf("kenpom_ratings_%d-*.csv", YEAR))),
    decreasing = TRUE)
  if (length(dated_files) > 0) {
    kenpom_file <- dated_files[1]
    cat(sprintf("Using dated KenPom file: %s\n", basename(kenpom_file)))
  }
}
if (!file.exists(kenpom_file)) stop("Cannot find KenPom data for ", YEAR)

kp <- read.csv(kenpom_file, stringsAsFactors = FALSE)

# Normalise column names: dated scrape uses "team"/"adj_em", legacy uses "Team"/"NetRtg"
if ("team" %in% names(kp) && !"Team" %in% names(kp)) names(kp)[names(kp) == "team"] <- "Team"
if ("adj_em" %in% names(kp) && !"NetRtg" %in% names(kp)) names(kp)[names(kp) == "adj_em"] <- "NetRtg"

# Clean up: remove repeated header rows and blank rows from KenPom scrape
kp <- kp[!is.na(kp$Team) & kp$Team != "" & kp$Team != "Team", ]

# Clean team names — KenPom appends seed numbers during tournament
kp$Team <- gsub("\\s*\\d+$", "", trimws(kp$Team))

# NetRtg is the AdjEM column — force numeric (may be character due to header rows)
kp$NetRtg <- as.numeric(kp$NetRtg)
kp <- kp[!is.na(kp$NetRtg), ]

kp_lookup <- setNames(kp$NetRtg, kp$Team)

cat(sprintf("Loaded %d teams from KenPom %d ratings\n\n", nrow(kp), YEAR))

# ==============================================================================
# LOAD BRACKET FROM CSV
# ==============================================================================

bracket_file <- file.path(script_dir, "brackets", sprintf("bracket_%d.csv", YEAR))
if (!file.exists(bracket_file)) stop("Cannot find ", bracket_file)

bracket <- read.csv(bracket_file, stringsAsFactors = FALSE)
stopifnot(nrow(bracket) == 64)

teams <- data.frame(
  name    = bracket$team,
  seed    = bracket$seed,
  region  = bracket$region,
  team_id = 1:64,
  stringsAsFactors = FALSE
)

# ==============================================================================
# KENPOM NAME ALIASES (from team_names.csv dictionary)
# ==============================================================================

team_dict_file <- file.path(script_dir, "team_names.csv")
if (!file.exists(team_dict_file)) stop("Cannot find team_names.csv")
team_dict <- read.csv(team_dict_file, stringsAsFactors = FALSE)
kp_alias <- setNames(team_dict$kenpom_name, team_dict$bracket_name)
cat(sprintf("Loaded team name dictionary: %d entries\n", length(kp_alias)))

resolve_name <- function(name) {
  if (name %in% names(kp_alias)) kp_alias[[name]] else name
}

get_rating <- function(name) {
  # Try alias first, then original name
  kp_name <- resolve_name(name)
  if (kp_name %in% names(kp_lookup)) {
    return(kp_lookup[[kp_name]])
  }
  # Alias didn't match — try original name directly
  if (name %in% names(kp_lookup)) {
    return(kp_lookup[[name]])
  }
  # Try fuzzy match on alias
  matches <- grep(kp_name, names(kp_lookup), value = TRUE, ignore.case = TRUE)
  if (length(matches) == 0 && kp_name != name) {
    # Try fuzzy match on original name
    matches <- grep(name, names(kp_lookup), value = TRUE, ignore.case = TRUE)
  }
  if (length(matches) > 0) {
    cat(sprintf("  Fuzzy matched '%s' -> '%s'\n", name, matches[1]))
    return(kp_lookup[[matches[1]]])
  }
  warning(sprintf("No KenPom rating found for '%s' (looked up '%s'), using 0",
                  name, kp_name))
  return(0)
}

# Look up KenPom ratings
teams$rating <- sapply(teams$name, get_rating)

cat("\n")

# ==============================================================================
# BRACKET CONSTRUCTION
# Teams are already in bracket order (1v16, 8v9, 5v12, 4v13, ...)
# and regions are ordered so regions 1&2 meet in semi 1, 3&4 in semi 2
# so bracket_order is just 1:64
# ==============================================================================

bracket_order <- teams$team_id  # already in correct matchup order

# ==============================================================================
# LOAD R1 CLOSING LINES (market-implied win probabilities)
# ==============================================================================

cl_file <- file.path(script_dir, "closing_lines",
                     sprintf("ncaat_%d_closing_lines.csv", YEAR))

r1_win_probs <- numeric(0)  # empty = fall back to KenPom logistic in C++

if (file.exists(cl_file)) {
  cl <- read.csv(cl_file, stringsAsFactors = FALSE)

  # Build closing-lines name → bracket name mapping from dictionary
  cl_to_bracket <- setNames(team_dict$bracket_name, team_dict$closing_lines_name)

  resolve_cl_name <- function(cl_name) {
    # Dictionary lookup first
    if (cl_name %in% names(cl_to_bracket)) return(cl_to_bracket[[cl_name]])
    # Prefix match: longest bracket name first to avoid "Texas" matching before "Texas Tech"
    sorted_names <- teams$name[order(nchar(teams$name), decreasing = TRUE)]
    for (bn in sorted_names) {
      if (startsWith(cl_name, bn)) return(bn)
    }
    cl_name
  }

  cl$home_bracket <- sapply(cl$home_team, resolve_cl_name)
  cl$away_bracket <- sapply(cl$away_team, resolve_cl_name)

  # Identify R1 games: first 32 unique matchups by date
  cl$game_date <- as.Date(cl$date)
  cl <- cl[order(cl$game_date), ]
  r1_teams_seen <- character(0)
  r1_rows <- integer(0)
  for (i in seq_len(nrow(cl))) {
    h <- cl$home_bracket[i]; a <- cl$away_bracket[i]
    if (!(h %in% r1_teams_seen) && !(a %in% r1_teams_seen)) {
      r1_rows <- c(r1_rows, i)
      r1_teams_seen <- c(r1_teams_seen, h, a)
    }
    if (length(r1_rows) == 32) break
  }
  r1_cl <- cl[r1_rows, ]

  # Build per-team win probability lookup
  cl_wp <- list()
  for (i in seq_len(nrow(r1_cl))) {
    cl_wp[[r1_cl$home_bracket[i]]] <- r1_cl$home_win_prob[i]
    cl_wp[[r1_cl$away_bracket[i]]] <- 1 - r1_cl$home_win_prob[i]
  }

  # Build R1 win probs in bracket matchup order (32 games)
  # Game g: team at position 2g-1 vs team at position 2g
  r1_probs <- numeric(32)
  n_matched <- 0
  for (g in 1:32) {
    team_a <- teams$name[2*g - 1]
    wp_a <- cl_wp[[team_a]]

    if (!is.null(wp_a)) {
      r1_probs[g] <- wp_a
      n_matched <- n_matched + 1
    } else {
      # Fallback to KenPom logistic
      r1_probs[g] <- 1 / (1 + exp(-0.0917 * (teams$rating[2*g-1] - teams$rating[2*g])))
      cat(sprintf("  WARNING: No closing line for %s, using KenPom fallback\n", team_a))
    }
  }
  r1_win_probs <- r1_probs
  cat(sprintf("Using closing lines for R1 win probabilities (%d/32 games matched)\n\n",
              n_matched))
} else {
  cat("No closing lines found, using KenPom ratings for all rounds\n\n")
}

# ==============================================================================
# RUN SIMULATIONS
# ==============================================================================

N_SIMS        <- 10000000
UPDATE_FACTOR <- 3.0    # max rating bump per win; actual boost = factor * (1 - win_prob)

set.seed(42)  # for reproducibility

cat("========================================================\n")
cat(sprintf("    NCAA MARCH MADNESS %d TOURNAMENT SIMULATOR\n", YEAR))
cat("========================================================\n")
cat(sprintf("Simulating %s tournaments ...\n\n",
            format(N_SIMS, big.mark = ",")))

t0      <- proc.time()
results <- run_tournament_sims(teams$rating, bracket_order, N_SIMS, UPDATE_FACTOR,
                               r1_win_probs)
elapsed <- (proc.time() - t0)["elapsed"]

cat(sprintf("Done in %.2f seconds\n\n", elapsed))

# ==============================================================================
# BUILD GAME METADATA LOOKUP
# ==============================================================================

# Region names in bracket order (from the CSV)
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
  gpr       <- games_per_region[rd]
  for (r in 1:4) {
    cols <- start_col + ((r - 1) * gpr):(r * gpr - 1)
    round_info$region[cols] <- region_names[r]
  }
}
round_info$region[61:63] <- "National"

# ==============================================================================
# SAVE FULL SIMULATION RESULTS
# ==============================================================================

sim_output <- list(
  all_results   = results$all_results,
  teams         = teams,
  round_info    = round_info,
  bracket_order = bracket_order,
  n_sims        = N_SIMS,
  update_factor = UPDATE_FACTOR,
  year          = YEAR
)

# Save as 2 Parquet files (split all_results) + small RDS (metadata)
library(arrow)
ar_df <- as.data.frame(results$all_results)
colnames(ar_df) <- paste0("game_", 1:63)

pq_file_1 <- file.path(script_dir, sprintf("sim_results_%d_part1.parquet", YEAR))
pq_file_2 <- file.path(script_dir, sprintf("sim_results_%d_part2.parquet", YEAR))
write_parquet(ar_df[, 1:32],  pq_file_1, compression = "zstd", compression_level = 9)
write_parquet(ar_df[, 33:63], pq_file_2, compression = "zstd", compression_level = 9)

meta_output <- sim_output
meta_output$all_results <- NULL  # strip the big matrix
rds_file <- file.path(script_dir, sprintf("sim_results_%d_meta.rds", YEAR))
saveRDS(meta_output, rds_file)

cat(sprintf("Results saved: %s + %s + %s  (%s sims x 63 games)\n\n",
            basename(pq_file_1), basename(pq_file_2), basename(rds_file),
            format(N_SIMS, big.mark = ",")))

# ==============================================================================
# DISPLAY: CHAMPIONSHIP PROBABILITIES
# ==============================================================================

champ_pct <- 100 * results$champ_counts / N_SIMS
ff_pct    <- 100 * results$final_four_counts / N_SIMS
e8_pct    <- 100 * results$elite_eight_counts / N_SIMS
s16_pct   <- 100 * results$sweet_sixteen_counts / N_SIMS

prob_df <- data.frame(
  Seed   = teams$seed,
  Region = teams$region,
  Team   = teams$name,
  Rating = teams$rating,
  S16    = round(s16_pct, 1),
  E8     = round(e8_pct, 1),
  FF     = round(ff_pct, 1),
  Champ  = round(champ_pct, 1),
  stringsAsFactors = FALSE
)

prob_df <- prob_df[order(-prob_df$Champ), ]

cat("========================================================\n")
cat("            CHAMPIONSHIP PROBABILITIES\n")
cat("========================================================\n")
cat(sprintf("%-20s %4s %-8s %5s  %5s %5s %5s %6s\n",
            "Team", "Seed", "Region", "AdjEM",
            "S16%", "E8%", "FF%", "Champ%"))
cat(paste(rep("-", 72), collapse = ""), "\n")

for (i in 1:nrow(prob_df)) {
  row <- prob_df[i, ]
  if (row$Champ > 0 || i <= 20) {
    cat(sprintf("%-20s  %2d   %-8s %5.1f  %5.1f %5.1f %5.1f %6.1f\n",
                row$Team, row$Seed, row$Region, row$Rating,
                row$S16, row$E8, row$FF, row$Champ))
  }
}

# ==============================================================================
# DISPLAY: ONE EXAMPLE BRACKET (first simulation)
# ==============================================================================

cat("\n========================================================\n")
cat("       EXAMPLE SIMULATED BRACKET (Sim #1)\n")
cat("========================================================\n")

ex <- results$all_results[1, ]
participants <- bracket_order

round_names <- c("ROUND OF 64", "ROUND OF 32", "SWEET SIXTEEN",
                 "ELITE EIGHT", "FINAL FOUR", "CHAMPIONSHIP")
game_idx <- 1

for (round in 1:6) {
  n_games <- length(participants) / 2
  cat(sprintf("\n--- %s ---\n", round_names[round]))

  next_participants <- integer(n_games)
  prev_region <- ""

  for (g in 1:n_games) {
    a_id <- participants[2 * g - 1]
    b_id <- participants[2 * g]
    w_id <- ex[game_idx]

    a_row <- teams[teams$team_id == a_id, ]
    b_row <- teams[teams$team_id == b_id, ]
    w_row <- teams[teams$team_id == w_id, ]

    if (round <= 4) {
      cur_region <- a_row$region
      if (cur_region != prev_region) {
        cat(sprintf("  [%s]\n", cur_region))
        prev_region <- cur_region
      }
    }

    upset_tag <- ""
    if (w_id == b_id && a_row$rating > b_row$rating) {
      upset_tag <- " ** UPSET **"
    } else if (w_id == a_id && b_row$rating > a_row$rating) {
      upset_tag <- " ** UPSET **"
    }

    cat(sprintf("  (%2d) %-18s vs (%2d) %-18s  ->  %s wins%s\n",
                a_row$seed, a_row$name,
                b_row$seed, b_row$name,
                w_row$name, upset_tag))

    next_participants[g] <- w_id
    game_idx <- game_idx + 1
  }

  participants <- next_participants
}

cat(sprintf("\n*** CHAMPION: %s ***\n",
            teams$name[teams$team_id == participants[1]]))

# ==============================================================================
# SUMMARY STATS
# ==============================================================================

cat("\n========================================================\n")
cat("                  QUICK STATS\n")
cat("========================================================\n")

top_champ <- prob_df[1, ]
cat(sprintf("Most likely champion:  %s (%s %d-seed, %.1f%%)\n",
            top_champ$Team, top_champ$Region, top_champ$Seed, top_champ$Champ))

n_unique_champs <- sum(results$champ_counts > 0)
cat(sprintf("Unique champions across %s sims: %d of 64 teams\n",
            format(N_SIMS, big.mark = ","), n_unique_champs))

champ_seeds <- rep(teams$seed, results$champ_counts)
cat(sprintf("Average champion seed: %.1f\n", mean(champ_seeds)))

cat("\nFinal Four most common lineups (by region):\n")
for (reg in region_names) {
  reg_teams <- teams[teams$region == reg, ]
  reg_ff    <- results$final_four_counts[reg_teams$team_id]
  best      <- which.max(reg_ff)
  cat(sprintf("  %-8s: %s (%d-seed, %.1f%% of the time)\n",
              reg, reg_teams$name[best], reg_teams$seed[best],
              100 * reg_ff[best] / N_SIMS))
}

# ==============================================================================
# SURVIVOR POOL HELPER
# ==============================================================================

evaluate_survivor <- function(sim, r1, r2, s16, e8, ff, champ) {
  tm   <- sim$teams
  ar   <- sim$all_results
  ri   <- sim$round_info
  nsim <- sim$n_sims

  name_to_id <- function(nms) {
    ids <- match(nms, tm$name)
    if (any(is.na(ids))) stop("Unknown team(s): ",
                               paste(nms[is.na(ids)], collapse = ", "))
    tm$team_id[ids]
  }

  picks <- list(
    R64   = name_to_id(r1),
    R32   = name_to_id(r2),
    S16   = name_to_id(s16),
    E8    = name_to_id(e8),
    FF    = name_to_id(ff),
    Champ = name_to_id(champ)
  )

  rcols <- list(
    R64   = which(ri$round_num == 1),
    R32   = which(ri$round_num == 2),
    S16   = which(ri$round_num == 3),
    E8    = which(ri$round_num == 4),
    FF    = which(ri$round_num == 5),
    Champ = which(ri$round_num == 6)
  )

  survived <- matrix(TRUE, nrow = nsim, ncol = 6)
  colnames(survived) <- names(picks)

  for (rd in seq_along(picks)) {
    rd_name  <- names(picks)[rd]
    pick_ids <- picks[[rd]]
    cols     <- rcols[[rd_name]]
    round_winners <- ar[, cols, drop = FALSE]
    hit <- apply(round_winners, 1, function(row) any(row %in% pick_ids))
    survived[, rd] <- hit
  }

  cum_surv <- t(apply(survived, 1, cumprod))
  surv_rates <- colMeans(cum_surv) * 100

  cat("\n========================================================\n")
  cat("             SURVIVOR PICK EVALUATION\n")
  cat("========================================================\n")
  cat(sprintf("  R64 picks:   %s\n", paste(r1, collapse = ", ")))
  cat(sprintf("  R32 picks:   %s\n", paste(r2, collapse = ", ")))
  cat(sprintf("  S16 pick:    %s\n", s16))
  cat(sprintf("  E8 pick:     %s\n", e8))
  cat(sprintf("  FF pick:     %s\n", ff))
  cat(sprintf("  Champ pick:  %s\n", champ))
  cat("--------------------------------------------------------\n")
  cat(sprintf("  Survive R64:          %6.2f%%\n", surv_rates["R64"]))
  cat(sprintf("  Survive thru R32:     %6.2f%%\n", surv_rates["R32"]))
  cat(sprintf("  Survive thru S16:     %6.2f%%\n", surv_rates["S16"]))
  cat(sprintf("  Survive thru E8:      %6.2f%%\n", surv_rates["E8"]))
  cat(sprintf("  Survive thru FF:      %6.2f%%\n", surv_rates["FF"]))
  cat(sprintf("  WIN THE POOL:         %6.2f%%\n", surv_rates["Champ"]))
  cat("========================================================\n\n")

  invisible(list(survived = survived, cum_surv = cum_surv, rates = surv_rates))
}

cat("\n")
