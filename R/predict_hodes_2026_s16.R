#!/usr/bin/env Rscript
# ==============================================================================
# predict_hodes_2026_s16.R
#
# Given the current 2026 Hodes standings and closing lines, predict:
# 1. P(take_two) for each entry
# 2. Team-level ownership for S16
# ==============================================================================

setwd("C:/Users/tyler/Documents/R Workspace/march_madness")
source("R/hodes_entry_model.R")

library(readxl)

# --- Load calibrated model ---------------------------------------------------
params <- readRDS("hodes_entry_model_params.rds")

# --- Load 2026 data ----------------------------------------------------------
cat("=== Loading 2026 Data ===\n")

# Bracket
bracket <- read.csv("brackets/bracket_2026.csv", stringsAsFactors = FALSE)
cat("Bracket: ", nrow(bracket), " teams\n")

# Closing lines (remaining games)
cl <- read.csv("closing_lines/ncaat_2026_closing_lines.csv", stringsAsFactors = FALSE)
cat("Closing lines: ", nrow(cl), " games\n")

# KenPom for championship equity
kp <- read.csv("kenpom_data/kenpom_2026.csv", stringsAsFactors = FALSE)
# Handle different column name formats
if ("adj_em" %in% names(kp)) {
  kp$NetRtg <- as.numeric(kp$adj_em)
  kp$Team <- kp$team
} else {
  kp$NetRtg <- as.numeric(kp$NetRtg)
  kp$Team <- kp$Team
}
kp <- kp[!is.na(kp$NetRtg), ]

# Hodes current standings
hodes <- read.csv("C:/Users/tyler/Downloads/Hodes Results - 2026.csv",
                   stringsAsFactors = FALSE)
names(hodes) <- c("Rank", "Entry", "R64_1", "R64_2", "R64_3",
                   "R32_1", "R32_2", "R32_3", "R16_1", "R16_2",
                   "QF", "SF", "Final", "TB")
cat("Entries alive: ", nrow(hodes), "\n")

# --- Identify S16 matchups from closing lines ---------------------------------
# S16 games are on Mar 24-27 and involve bracket teams
# Filter to the 8 S16 NCAA games

# Build name mapping: closing line name -> bracket name
.strip_mascot <- function(full_name) {
  # Check explicit mapping FIRST (before partial matching)
  mapping <- c(
    "Michigan Wolverines" = "Michigan",
    "Michigan St Spartans" = "Michigan State",
    "Michigan St. Spartans" = "Michigan State",
    "Duke Blue Devils" = "Duke",
    "St. John's Red Storm" = "St. John's",
    "Iowa State Cyclones" = "Iowa State",
    "UConn Huskies" = "UConn",
    "Alabama Crimson Tide" = "Alabama",
    "Houston Cougars" = "Houston",
    "Illinois Fighting Illini" = "Illinois",
    "Nebraska Cornhuskers" = "Nebraska",
    "Iowa Hawkeyes" = "Iowa",
    "Purdue Boilermakers" = "Purdue",
    "Texas Longhorns" = "Texas",
    "Arizona Wildcats" = "Arizona",
    "Arkansas Razorbacks" = "Arkansas",
    "Tennessee Volunteers" = "Tennessee",
    "Auburn Tigers" = "Auburn",
    "Nevada Wolf Pack" = "Nevada",
    "Dayton Flyers" = "Dayton",
    "New Mexico Lobos" = "New Mexico",
    "Saint Joseph's Hawks" = "Saint Joseph's",
    "Illinois St Redbirds" = "Illinois State",
    "Colorado Buffaloes" = "Colorado",
    "Oklahoma Sooners" = "Oklahoma",
    "Minnesota Golden Gophers" = "Minnesota",
    "Baylor Bears" = "Baylor",
    "Creighton Bluejays" = "Creighton",
    "Rutgers Scarlet Knights" = "Rutgers",
    "West Virginia Mountaineers" = "West Virginia",
    "Stanford Cardinal" = "Stanford",
    "Florida Gators" = "Florida"
  )
  if (full_name %in% names(mapping)) return(mapping[[full_name]])

  # Try progressively shorter prefixes
  parts <- strsplit(full_name, " ")[[1]]
  for (n in length(parts):1) {
    candidate <- paste(parts[1:n], collapse = " ")
    if (candidate %in% bracket$team) return(candidate)
  }
  return(full_name)
}

# Map closing line teams to bracket names
cl$home_bracket <- sapply(cl$home_team, .strip_mascot)
cl$away_bracket <- sapply(cl$away_team, .strip_mascot)

# S16 games: both teams must be in the bracket
s16_games <- cl[cl$home_bracket %in% bracket$team & cl$away_bracket %in% bracket$team &
                cl$date < "2026-04-01", ]

cat("\nS16 Games identified:\n")
for (i in 1:nrow(s16_games)) {
  home <- s16_games$home_bracket[i]
  away <- s16_games$away_bracket[i]
  home_seed <- bracket$seed[bracket$team == home]
  away_seed <- bracket$seed[bracket$team == away]
  cat(sprintf("  %s (%d) vs %s (%d)  |  home_wp=%.3f  |  %s\n",
              home, home_seed, away, away_seed,
              s16_games$home_win_prob[i], s16_games$date[i]))
}

# --- Build Hodes -> Bracket name mapping --------------------------------------
# Hodes uses short names, bracket uses full names
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
  "Saint Mary's" = "Saint Mary's"
)

# Get all unique team names from Hodes entries
all_hodes_teams <- unique(c(hodes$R64_1, hodes$R64_2, hodes$R64_3,
                            hodes$R32_1, hodes$R32_2, hodes$R32_3))
all_hodes_teams <- all_hodes_teams[!is.na(all_hodes_teams) & all_hodes_teams != ""]

# Check for unmapped teams
unmapped <- setdiff(all_hodes_teams, names(hodes_to_bracket))
if (length(unmapped) > 0) {
  cat("\nWARNING: Unmapped Hodes teams:", paste(unmapped, collapse=", "), "\n")
  # Try to add them
  for (t in unmapped) {
    # Direct match in bracket
    if (t %in% bracket$team) {
      hodes_to_bracket[t] <- t
    }
  }
  unmapped2 <- setdiff(all_hodes_teams, names(hodes_to_bracket))
  if (length(unmapped2) > 0) {
    cat("STILL unmapped:", paste(unmapped2, collapse=", "), "\n")
  }
}

# --- S16 teams and their properties -------------------------------------------
s16_teams <- data.frame(
  bracket_name = c(s16_games$home_bracket, s16_games$away_bracket),
  stringsAsFactors = FALSE
)
# Add seed
s16_teams$seed <- sapply(s16_teams$bracket_name, function(t) {
  bracket$seed[bracket$team == t][1]
})

# Add win probability (from perspective of that team)
s16_teams$wp <- NA
for (i in 1:nrow(s16_games)) {
  home <- s16_games$home_bracket[i]
  away <- s16_games$away_bracket[i]
  home_wp <- s16_games$home_win_prob[i]
  s16_teams$wp[s16_teams$bracket_name == home] <- home_wp
  s16_teams$wp[s16_teams$bracket_name == away] <- 1 - home_wp
}

# Add game_id (which game this team is in)
s16_teams$game_id <- NA
for (i in 1:nrow(s16_games)) {
  home <- s16_games$home_bracket[i]
  away <- s16_games$away_bracket[i]
  s16_teams$game_id[s16_teams$bracket_name == home] <- i
  s16_teams$game_id[s16_teams$bracket_name == away] <- i
}

# Add opponent
s16_teams$opponent <- NA
for (i in 1:nrow(s16_games)) {
  home <- s16_games$home_bracket[i]
  away <- s16_games$away_bracket[i]
  s16_teams$opponent[s16_teams$bracket_name == home] <- away
  s16_teams$opponent[s16_teams$bracket_name == away] <- home
}

cat("\nS16 Teams:\n")
print(s16_teams[order(-s16_teams$wp), c("bracket_name", "seed", "wp", "game_id")])

# --- Compute championship equity via KenPom -----------------------------------
# Match bracket names to KenPom
.match_kp <- function(bracket_name, kp_teams) {
  if (bracket_name %in% kp_teams) return(bracket_name)
  # Try without period
  bn2 <- gsub("\\.", "", bracket_name)
  idx <- which(gsub("\\.", "", kp_teams) == bn2)
  if (length(idx) > 0) return(kp_teams[idx[1]])
  # Try partial
  idx <- grep(paste0("^", gsub("\\.", "\\\\.", bracket_name)), kp_teams)
  if (length(idx) > 0) return(kp_teams[idx[1]])
  return(NA)
}

kp_ratings <- setNames(kp$NetRtg, kp$Team)
# Deduplicate (take first match)
kp_ratings <- kp_ratings[!duplicated(names(kp_ratings))]
s16_teams$kp_name <- sapply(s16_teams$bracket_name, .match_kp, kp_teams = names(kp_ratings))
s16_teams$kp_rating <- as.numeric(kp_ratings[s16_teams$kp_name])

# Championship equity from futures market odds (more accurate than any model)
# American odds -> implied probability: P = 100 / (odds + 100) for positive odds
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
# Convert to implied prob (includes vig)
futures_implied <- 100 / (futures_odds + 100)
# Remove vig by normalizing to sum to 1
futures_ce <- futures_implied / sum(futures_implied)

s16_teams$champ_equity <- futures_ce[s16_teams$bracket_name]

# Still need regions for display
s16_teams$region <- sapply(s16_teams$bracket_name, function(t) {
  bracket$region[bracket$team == t][1]
})

games_by_region <- split(1:nrow(s16_games), sapply(1:nrow(s16_games), function(g) {
  t <- s16_games$home_bracket[g]
  bracket$region[bracket$team == t][1]
}))

cat("\nGames by region:\n")
for (r in names(games_by_region)) {
  gids <- games_by_region[[r]]
  for (g in gids) {
    cat(sprintf("  %s: Game %d - %s vs %s\n", r, g,
                s16_games$home_bracket[g], s16_games$away_bracket[g]))
  }
}

cat("\nChampionship Equity:\n")
ce_order <- order(-s16_teams$champ_equity)
for (i in ce_order) {
  cat(sprintf("  %-20s seed=%2d  wp=%.3f  CE=%.4f\n",
              s16_teams$bracket_name[i], s16_teams$seed[i],
              s16_teams$wp[i], s16_teams$champ_equity[i]))
}

# --- For each entry, compute used_teams and predict ---------------------------
cat("\n=== Predicting S16 Ownership ===\n")

# Build reverse mapping: bracket_name -> hodes names that map to it
bracket_to_hodes <- setNames(names(hodes_to_bracket), hodes_to_bracket)

n_entries <- nrow(hodes)

# Aggregate ownership
team_own_1pick <- setNames(rep(0, nrow(s16_teams)), s16_teams$bracket_name)
team_own_2pick <- setNames(rep(0, nrow(s16_teams)), s16_teams$bracket_name)
total_p2 <- 0
n_valid <- 0

for (e in 1:n_entries) {
  # Get used teams (convert Hodes names to bracket names)
  used_hodes <- c(hodes$R64_1[e], hodes$R64_2[e], hodes$R64_3[e],
                  hodes$R32_1[e], hodes$R32_2[e], hodes$R32_3[e])
  used_hodes <- used_hodes[!is.na(used_hodes) & used_hodes != ""]
  used_bracket <- unname(hodes_to_bracket[used_hodes])
  used_bracket <- used_bracket[!is.na(used_bracket)]

  # Available S16 candidates (not already used)
  available <- s16_teams[!(s16_teams$bracket_name %in% used_bracket), ]
  if (nrow(available) == 0) next
  n_valid <- n_valid + 1

  # Compute features for each candidate
  remaining_ce <- sum(available$champ_equity)
  available$fv_fraction <- ifelse(remaining_ce > 0,
                                  available$champ_equity / remaining_ce, 0)
  available$opponent_is_used <- as.integer(available$opponent %in% used_bracket)
  available$seed_tb <- available$seed / 16

  # Predict P(take_two)
  tb_sum <- hodes$TB[e]
  all_tb <- hodes$TB
  tb_rank <- rank(all_tb, ties.method = "average")[e]
  tb_rank_pct <- (tb_rank - 1) / max(n_entries - 1, 1)

  p2 <- predict_hodes_s16_count(params$count_model,
                                n_alive = n_entries,
                                tb_rank_pct = tb_rank_pct,
                                n_available_s16 = nrow(available))
  total_p2 <- total_p2 + p2

  # Predict team selection for 1-pick case
  # S16 uses seed-prestige for save, not fv_fraction
  seed_prestige <- (17 - available$seed) / 16
  beta_oneseed <- if (!is.null(params$s16$beta_oneseed)) params$s16$beta_oneseed else 0
  beta_wp2 <- if (!is.null(params$s16$beta_wp2)) params$s16$beta_wp2 else 0

  log_wp <- log(pmax(available$wp, 0.01))

  scores_1 <- params$s16$beta_wp * log_wp +
              beta_wp2 * available$wp^2 -
              params$s16$beta_save * seed_prestige +
              params$s16$beta_path * available$opponent_is_used +
              params$s16$beta_seed * available$seed_tb -
              beta_oneseed * as.numeric(available$seed == 1)

  probs_1 <- exp(scores_1) / sum(exp(scores_1))

  # Predict team selection for 2-pick case (pair enumeration)
  if (nrow(available) >= 2) {
    # Enumerate all valid pairs (different games)
    pairs <- list()
    pair_scores <- c()
    for (a in 1:(nrow(available)-1)) {
      for (b in (a+1):nrow(available)) {
        if (available$game_id[a] != available$game_id[b]) {
          pairs[[length(pairs)+1]] <- c(a, b)
          # Score: score(t1,U) + score(t2, U+t1)
          # For t2, recompute fv_fraction with t1 removed
          remaining_ce_2 <- remaining_ce - available$champ_equity[a]
          fv_frac_b <- ifelse(remaining_ce_2 > 0,
                              available$champ_equity[b] / remaining_ce_2, 0)
          opp_used_b <- available$opponent_is_used[b]
          # If t1's opponent is t2's team... no, opponent_is_used is about used_teams
          # But after picking t1, t1 is now "used" for t2's perspective
          # Check if t2's opponent is t1
          if (available$opponent[b] == available$bracket_name[a]) {
            opp_used_b <- 1
          }

          s1 <- scores_1[a]  # score(t1, U) already computed
          seed_prest_b <- (17 - available$seed[b]) / 16
          log_wp_b <- log(max(available$wp[b], 0.01))
          s2 <- params$s16$beta_wp * log_wp_b +
                beta_wp2 * available$wp[b]^2 -
                params$s16$beta_save * seed_prest_b +
                params$s16$beta_path * opp_used_b +
                params$s16$beta_seed * available$seed_tb[b] -
                beta_oneseed * as.numeric(available$seed[b] == 1)

          pair_scores <- c(pair_scores, s1 + s2)
        }
      }
    }

    if (length(pairs) > 0) {
      pair_probs <- exp(pair_scores) / sum(exp(pair_scores))

      # Extract marginal probabilities
      probs_2 <- rep(0, nrow(available))
      for (p_idx in seq_along(pairs)) {
        probs_2[pairs[[p_idx]][1]] <- probs_2[pairs[[p_idx]][1]] + pair_probs[p_idx]
        probs_2[pairs[[p_idx]][2]] <- probs_2[pairs[[p_idx]][2]] + pair_probs[p_idx]
      }
      # probs_2 sums to 2 (marginals)
    } else {
      probs_2 <- probs_1  # fallback
      p2 <- 0
    }
  } else {
    probs_2 <- probs_1
    p2 <- 0
  }

  # Weighted ownership: (1-p2)*1pick_probs + p2*2pick_marginals
  # For 1-pick: each team gets probs_1[t] picks
  # For 2-pick: each team gets probs_2[t] picks (sums to 2)
  for (t in 1:nrow(available)) {
    tn <- available$bracket_name[t]
    team_own_1pick[tn] <- team_own_1pick[tn] + (1 - p2) * probs_1[t]
    team_own_2pick[tn] <- team_own_2pick[tn] + p2 * probs_2[t]
  }
}

# --- Output results -----------------------------------------------------------
cat("\n============================================================\n")
cat("  2026 HODES S16 OWNERSHIP PREDICTIONS\n")
cat("============================================================\n")
cat(sprintf("\nEntries alive: %d\n", n_entries))
cat(sprintf("Average P(take_two): %.1f%%\n", 100 * total_p2 / n_valid))
cat(sprintf("Expected total picks: %.1f (from %d entries)\n",
            sum(team_own_1pick) + sum(team_own_2pick), n_entries))

cat("\n%-20s %4s %6s %6s %8s %8s %8s\n",
    "Team", "Seed", "WP", "CE", "1-pick", "2-pick", "Total%")
cat(paste(rep("-", 78), collapse=""), "\n")

total_own <- team_own_1pick + team_own_2pick
own_pct <- total_own / n_entries * 100

for (i in order(-own_pct)) {
  tn <- names(own_pct)[i]
  cat(sprintf("%-20s %4d %6.1f %6.2f %8.1f %8.1f %7.1f%%\n",
              tn, s16_teams$seed[s16_teams$bracket_name == tn],
              100 * s16_teams$wp[s16_teams$bracket_name == tn],
              100 * s16_teams$champ_equity[s16_teams$bracket_name == tn],
              team_own_1pick[tn], team_own_2pick[tn], own_pct[i]))
}

cat("\nNote: 'Total%' = expected fraction of entries picking this team.\n")
cat("Values >100% are possible because entries taking 2 picks contribute twice.\n")
