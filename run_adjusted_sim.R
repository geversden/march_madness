#!/usr/bin/env Rscript
# Run 2026 tournament sim using adj_kempom_rtg from the tournament CSV
# WITH pace adjustment and Thursday R64 results locked in
# Saves results to adjusted_2026_sims.rds

library(Rcpp)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
sourceCpp(file.path(script_dir, "simulate_tourney.cpp"))

YEAR <- 2026L
LOG_SCALE <- 0.0917

# Load tournament CSV with adjusted ratings
tourney_file <- file.path(script_dir, "kenpom_data", "kenpom_2026_tournament.csv")
tourney <- read.csv(tourney_file, stringsAsFactors = FALSE)

# Load bracket to get correct ordering
bracket_file <- file.path(script_dir, "brackets", sprintf("bracket_%d.csv", YEAR))
bracket <- read.csv(bracket_file, stringsAsFactors = FALSE)
stopifnot(nrow(bracket) == 64)

teams <- data.frame(
  name    = bracket$team,
  seed    = bracket$seed,
  region  = bracket$region,
  team_id = 1:64,
  stringsAsFactors = FALSE
)

# Build lookup from tournament CSV using adj_kempom_rtg
adj_lookup <- setNames(tourney$adj_kempom_rtg, tourney$team)

# Match ratings to bracket order
teams$rating <- sapply(teams$name, function(nm) {
  val <- adj_lookup[nm]
  if (is.na(val)) {
    warning(sprintf("No adjusted rating for '%s', using 0", nm))
    return(0)
  }
  val
})

# Load team name dictionary (needed for tempo and closing line matching)
team_dict_file <- file.path(script_dir, "team_names.csv")
team_dict <- read.csv(team_dict_file, stringsAsFactors = FALSE)
kp_alias <- setNames(team_dict$kenpom_name, team_dict$bracket_name)

# ==============================================================================
# LOAD TEMPO DATA from kenpom_2026_pace.csv (adjusted tempos)
# ==============================================================================

pace_file <- file.path(script_dir, "kenpom_data", "kenpom_2026_pace.csv")
pace <- read.csv(pace_file, stringsAsFactors = FALSE)
pace$Team <- trimws(pace$Team)
# Column is "Tempo.adjusted" after CSV read
tempo_col <- grep("empo.*adj", names(pace), value = TRUE)[1]
if (is.null(tempo_col)) tempo_col <- "Tempo.adjusted"
pace_lookup <- setNames(as.numeric(pace[[tempo_col]]), pace$Team)

# Also load standard kenpom for fallback
kp_file <- file.path(script_dir, "kenpom_data", sprintf("kenpom_%d.csv", YEAR))
kp <- read.csv(kp_file, stringsAsFactors = FALSE)
kp <- kp[!is.na(kp$team) & kp$team != "" & kp$team != "team", ]
if ("team" %in% names(kp)) names(kp)[names(kp) == "team"] <- "Team"
kp$Team <- gsub("\\s*\\d+$", "", trimws(kp$Team))

# Use tournament CSV tempo where available, fall back to pace file, then kenpom
tourney_tempo_lookup <- setNames(tourney$adj_tempo, tourney$team)

AVG_TEMPO <- mean(as.numeric(pace[[tempo_col]]), na.rm = TRUE)
cat(sprintf("D1 average tempo: %.1f\n", AVG_TEMPO))

# Match tempos to bracket order
teams$tempo <- sapply(teams$name, function(nm) {
  # Try tournament CSV first
  val <- tourney_tempo_lookup[nm]
  if (!is.na(val)) return(val)
  # Try pace file directly
  if (nm %in% names(pace_lookup)) return(pace_lookup[[nm]])
  # Try kenpom alias
  kn <- if (nm %in% names(kp_alias)) kp_alias[[nm]] else nm
  if (kn %in% names(pace_lookup)) return(pace_lookup[[kn]])
  warning(sprintf("No tempo for '%s', using avg %.1f", nm, AVG_TEMPO))
  AVG_TEMPO
})

cat("\nTeams with adjusted KenPom ratings + tempos:\n")
print(teams[order(-teams$rating), c("name", "seed", "region", "rating", "tempo")], row.names = FALSE)
cat("\n")

# ==============================================================================
# BUILD R1 WIN PROBABILITIES
# Thursday 3/19 results: LOCKED IN (prob = 1.0 or 0.0)
# Friday 3/20 games: pace-adjusted KenPom probabilities
# ==============================================================================

# Bracket position -> game mapping (1-indexed):
# Game g: team at position 2g-1 (team_a) vs team at position 2g (team_b)
# r1_probs[g] = probability that team_a wins

# Thursday results - map winner to game slot
# Bracket order from bracket_2026.csv:
# Pos 1-2:   Duke(1) vs Siena(16)         -> Game 1:  Duke won      -> 1.0
# Pos 3-4:   Ohio State(8) vs TCU(9)      -> Game 2:  TCU won       -> 0.0
# Pos 5-6:   St. John's(5) vs N.Iowa(12)  -> Game 3:  FRIDAY
# Pos 7-8:   Kansas(4) vs Cal Baptist(13) -> Game 4:  FRIDAY
# Pos 9-10:  Louisville(6) vs S.Fla(11)   -> Game 5:  Louisville    -> 1.0
# Pos 11-12: Michigan St(3) vs NDSU(14)   -> Game 6:  Michigan St   -> 1.0
# Pos 13-14: UCLA(7) vs UCF(10)           -> Game 7:  FRIDAY
# Pos 15-16: UConn(2) vs Furman(15)       -> Game 8:  FRIDAY
# Pos 17-18: Florida(1) vs PVAM(16)       -> Game 9:  FRIDAY
# Pos 19-20: Clemson(8) vs Iowa(9)        -> Game 10: FRIDAY
# Pos 21-22: Vanderbilt(5) vs McNeese(12) -> Game 11: Vanderbilt    -> 1.0
# Pos 23-24: Nebraska(4) vs Troy(13)      -> Game 12: Nebraska      -> 1.0
# Pos 25-26: UNC(6) vs VCU(11)            -> Game 13: VCU won       -> 0.0
# Pos 27-28: Illinois(3) vs Penn(14)      -> Game 14: Illinois      -> 1.0
# Pos 29-30: St. Mary's(7) vs TA&M(10)    -> Game 15: Texas A&M won -> 0.0
# Pos 31-32: Houston(2) vs Idaho(15)      -> Game 16: Houston       -> 1.0
# Pos 33-34: Arizona(1) vs LIU(16)        -> Game 17: FRIDAY
# Pos 35-36: Villanova(8) vs Utah St(9)   -> Game 18: FRIDAY
# Pos 37-38: Wisconsin(5) vs High Pt(12)  -> Game 19: High Pt won   -> 0.0
# Pos 39-40: Arkansas(4) vs Hawaii(13)    -> Game 20: Arkansas      -> 1.0
# Pos 41-42: BYU(6) vs Texas(11)          -> Game 21: Texas won     -> 0.0
# Pos 43-44: Gonzaga(3) vs Kennesaw(14)   -> Game 22: Gonzaga       -> 1.0
# Pos 45-46: Miami(7) vs Missouri(10)     -> Game 23: FRIDAY
# Pos 47-48: Purdue(2) vs Queens(15)      -> Game 24: FRIDAY
# Pos 49-50: Michigan(1) vs Howard(16)    -> Game 25: Michigan      -> 1.0
# Pos 51-52: Georgia(8) vs St.Louis(9)    -> Game 26: St.Louis won  -> 0.0
# Pos 53-54: Texas Tech(5) vs Akron(12)   -> Game 27: FRIDAY
# Pos 55-56: Alabama(4) vs Hofstra(13)    -> Game 28: FRIDAY
# Pos 57-58: Tennessee(6) vs Miami OH(11) -> Game 29: FRIDAY
# Pos 59-60: Virginia(3) vs Wright St(14) -> Game 30: FRIDAY
# Pos 61-62: Kentucky(7) vs Santa Clara(10)-> Game 31: FRIDAY
# Pos 63-64: Iowa St(2) vs Tenn St(15)    -> Game 32: FRIDAY

# Helper: pace-adjusted win prob for team_a in game g
pace_wp <- function(g) {
  a <- 2*g - 1
  b <- 2*g
  rating_a <- teams$rating[a]
  rating_b <- teams$rating[b]
  tempo_a  <- teams$tempo[a]
  tempo_b  <- teams$tempo[b]
  game_tempo <- (tempo_a * tempo_b) / AVG_TEMPO
  diff <- rating_a - rating_b
  pace_factor <- game_tempo / AVG_TEMPO
  1 / (1 + exp(-LOG_SCALE * diff * pace_factor))
}

r1_probs <- numeric(32)

# Thursday LOCKED results
r1_probs[1]  <- 1.0   # Duke
r1_probs[2]  <- 0.0   # TCU won (Ohio St was team_a)
r1_probs[5]  <- 1.0   # Louisville
r1_probs[6]  <- 1.0   # Michigan State
r1_probs[11] <- 1.0   # Vanderbilt
r1_probs[12] <- 1.0   # Nebraska
r1_probs[13] <- 0.0   # VCU won (UNC was team_a)
r1_probs[14] <- 1.0   # Illinois
r1_probs[15] <- 0.0   # Texas A&M won (St. Mary's was team_a)
r1_probs[16] <- 1.0   # Houston
r1_probs[19] <- 0.0   # High Point won (Wisconsin was team_a)
r1_probs[20] <- 1.0   # Arkansas
r1_probs[21] <- 0.0   # Texas won (BYU was team_a)
r1_probs[22] <- 1.0   # Gonzaga
r1_probs[25] <- 1.0   # Michigan
r1_probs[26] <- 0.0   # Saint Louis won (Georgia was team_a)

# ==============================================================================
# LOAD CLOSING LINES for Friday R64 + Saturday R32
# ==============================================================================

cl_file <- file.path(script_dir, "closing_lines",
                     sprintf("ncaat_%d_closing_lines.csv", YEAR))
cl <- read.csv(cl_file, stringsAsFactors = FALSE)
cl_to_bracket <- setNames(team_dict$bracket_name, team_dict$closing_lines_name)

resolve_cl_name <- function(cl_name) {
  if (cl_name %in% names(cl_to_bracket)) return(cl_to_bracket[[cl_name]])
  sorted_names <- teams$name[order(nchar(teams$name), decreasing = TRUE)]
  for (bn in sorted_names) {
    if (startsWith(cl_name, bn)) return(bn)
  }
  cl_name
}
cl$home_bracket <- sapply(cl$home_team, resolve_cl_name)
cl$away_bracket <- sapply(cl$away_team, resolve_cl_name)

# Build per-team closing line win prob lookup
cl_wp <- list()
for (i in seq_len(nrow(cl))) {
  cl_wp[[cl$home_bracket[i]]] <- cl$home_win_prob[i]
  cl_wp[[cl$away_bracket[i]]] <- 1 - cl$home_win_prob[i]
}

# Friday R64: use closing lines where available, fall back to pace-adjusted
friday_games <- c(3, 4, 7, 8, 9, 10, 17, 18, 23, 24, 27, 28, 29, 30, 31, 32)
n_cl_r1 <- 0
for (g in friday_games) {
  ta <- teams$name[2*g-1]
  wp_a <- cl_wp[[ta]]
  if (!is.null(wp_a)) {
    r1_probs[g] <- wp_a
    n_cl_r1 <- n_cl_r1 + 1
  } else {
    r1_probs[g] <- pace_wp(g)
    cat(sprintf("  WARNING: No closing line for R64 %s, using pace-adj fallback\n", ta))
  }
}
cat(sprintf("Using closing lines for %d/16 Friday R64 games\n\n", n_cl_r1))

cat("========================================================\n")
cat("  R1 WIN PROBABILITIES (Thursday locked, Friday lines)\n")
cat("========================================================\n")
for (g in 1:32) {
  ta <- teams$name[2*g-1]
  tb <- teams$name[2*g]
  status <- if (r1_probs[g] == 1.0) "LOCKED"
            else if (r1_probs[g] == 0.0) "LOCKED"
            else if (g %in% friday_games && !is.null(cl_wp[[ta]])) "CL"
            else "pace-adj"
  winner_note <- if (r1_probs[g] == 1.0) sprintf("-> %s", ta)
                 else if (r1_probs[g] == 0.0) sprintf("-> %s", tb)
                 else ""
  cat(sprintf("  Game %2d: (%2d) %-18s vs (%2d) %-18s  p=%.3f [%s] %s\n",
              g, teams$seed[2*g-1], ta, teams$seed[2*g], tb,
              r1_probs[g], status, winner_note))
}
cat("\n")

r1_win_probs <- r1_probs

# ==============================================================================
# BUILD R2 (ROUND OF 32) WIN PROBABILITIES FROM CLOSING LINES
# ==============================================================================
# R32 has 16 games. Use -1.0 to signal "no line, use pace-adjusted model"
# R32 game g (0-indexed): winner of R64 game 2g vs winner of R64 game 2g+1
# team_a = winner from upper bracket position

# Known R32 matchups (from Thursday results) with closing lines:
# R32 Game 1:  Duke vs TCU                -> Duke -11.0
# R32 Game 3:  Louisville vs Michigan St   -> MSU -4.5  (Louisville=team_a)
# R32 Game 6:  Vanderbilt vs Nebraska      -> Vandy -2.5 (Vandy=team_a)
# R32 Game 7:  VCU vs Illinois             -> Illinois -10.5 (VCU=team_a)
# R32 Game 8:  Texas A&M vs Houston        -> Houston -9.5 (TA&M=team_a)
# R32 Game 10: High Point vs Arkansas      -> Arkansas -11.0 (HP=team_a)
# R32 Game 11: Texas vs Gonzaga            -> no line yet
# R32 Game 13: Michigan vs Saint Louis     -> Michigan -12.5
# R32 remaining: unknown R64 outcomes, use pace-adjusted

r2_probs <- rep(-1.0, 16)  # -1 = use pace-adjusted model

# Helper: look up closing line WP for team_a in an R32 matchup
r2_cl <- function(team_a_name, team_b_name) {
  # Check if we have a closing line for this matchup
  for (i in seq_len(nrow(cl))) {
    h <- cl$home_bracket[i]; a <- cl$away_bracket[i]
    if ((h == team_a_name && a == team_b_name) ||
        (h == team_b_name && a == team_a_name)) {
      if (h == team_a_name) return(cl$home_win_prob[i])
      else return(1 - cl$home_win_prob[i])
    }
  }
  return(-1.0)  # no line found
}

# R32 Game 1 (idx 1): Duke vs TCU
r2_probs[1] <- r2_cl("Duke", "TCU")
# R32 Game 3 (idx 3): Louisville vs Michigan State
r2_probs[3] <- r2_cl("Louisville", "Michigan State")
# R32 Game 6 (idx 6): Vanderbilt vs Nebraska
r2_probs[6] <- r2_cl("Vanderbilt", "Nebraska")
# R32 Game 7 (idx 7): VCU vs Illinois
r2_probs[7] <- r2_cl("VCU", "Illinois")
# R32 Game 8 (idx 8): Texas A&M vs Houston
r2_probs[8] <- r2_cl("Texas A&M", "Houston")
# R32 Game 10 (idx 10): High Point vs Arkansas
r2_probs[10] <- r2_cl("High Point", "Arkansas")
# R32 Game 11 (idx 11): Texas vs Gonzaga
r2_probs[11] <- r2_cl("Texas", "Gonzaga")
# R32 Game 13 (idx 13): Michigan vs Saint Louis
r2_probs[13] <- r2_cl("Michigan", "Saint Louis")

cat("========================================================\n")
cat("  R2 WIN PROBABILITIES (closing lines where available)\n")
cat("========================================================\n")
r32_team_a <- c("Duke","?","Louisville","?","?","Vanderbilt","VCU","Texas A&M",
                "?","High Point","Texas","?","Michigan","?","?","?")
r32_team_b <- c("TCU","?","Michigan State","?","?","Nebraska","Illinois","Houston",
                "?","Arkansas","Gonzaga","?","Saint Louis","?","?","?")
for (g in 1:16) {
  if (r2_probs[g] >= 0) {
    cat(sprintf("  R32 Game %2d: %-18s vs %-18s  p=%.3f [CL]\n",
                g, r32_team_a[g], r32_team_b[g], r2_probs[g]))
  }
}
n_cl_r2 <- sum(r2_probs >= 0)
cat(sprintf("\nUsing closing lines for %d/16 R32 games (rest use pace-adjusted)\n\n", n_cl_r2))

r2_win_probs <- r2_probs

# ==============================================================================
# RUN SIMULATION (closing lines for R1+R2, pace-adjusted for S16+)
# ==============================================================================

N_SIMS        <- 2000000
UPDATE_FACTOR <- 0.5

set.seed(42)

bracket_order <- teams$team_id

cat("========================================================\n")
cat(sprintf("  ADJUSTED KENPOM %d TOURNAMENT SIMULATOR\n", YEAR))
cat("  WITH PACE ADJUSTMENT + THURSDAY RESULTS LOCKED\n")
cat("========================================================\n")
cat(sprintf("Simulating %s tournaments ...\n\n", format(N_SIMS, big.mark = ",")))

t0      <- proc.time()
results <- run_tournament_sims(teams$rating, teams$tempo, AVG_TEMPO,
                               bracket_order, N_SIMS, UPDATE_FACTOR,
                               r1_win_probs, r2_win_probs)
elapsed <- (proc.time() - t0)["elapsed"]

cat(sprintf("Done in %.2f seconds\n\n", elapsed))

# Build round info
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

# Save results
sim_output <- list(
  all_results   = results$all_results,
  teams         = teams,
  round_info    = round_info,
  bracket_order = bracket_order,
  n_sims        = N_SIMS,
  update_factor = UPDATE_FACTOR,
  year          = YEAR,
  rating_source = "adj_kempom_rtg_pace_adjusted",
  thursday_locked = TRUE
)

rds_file <- file.path(script_dir, "adjusted_2026_sims.rds")
saveRDS(sim_output, rds_file)
cat(sprintf("Results saved to %s\n\n", basename(rds_file)))

# ==============================================================================
# DISPLAY: CHAMPIONSHIP PROBABILITIES
# ==============================================================================

champ_pct <- 100 * results$champ_counts / N_SIMS
ff_pct    <- 100 * results$final_four_counts / N_SIMS
e8_pct    <- 100 * results$elite_eight_counts / N_SIMS
s16_pct   <- 100 * results$sweet_sixteen_counts / N_SIMS
r32_pct   <- 100 * results$sweet_sixteen_counts / N_SIMS  # R32 = win R64

prob_df <- data.frame(
  Seed   = teams$seed,
  Region = teams$region,
  Team   = teams$name,
  Rating = teams$rating,
  Tempo  = teams$tempo,
  S16    = round(s16_pct, 1),
  E8     = round(e8_pct, 1),
  FF     = round(ff_pct, 1),
  Champ  = round(champ_pct, 1),
  stringsAsFactors = FALSE
)
prob_df <- prob_df[order(-prob_df$Champ), ]

cat("========================================================\n")
cat("   CHAMPIONSHIP PROBABILITIES (PACE-ADJUSTED + LOCKED)\n")
cat("========================================================\n")
cat(sprintf("%-20s %4s %-8s %5s %5s  %5s %5s %5s %5s %6s\n",
            "Team", "Seed", "Region", "AdjEM", "Tempo",
            "S16%", "E8%", "FF%", "Champ%", ""))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (i in 1:nrow(prob_df)) {
  row <- prob_df[i, ]
  if (row$Champ > 0 || i <= 20) {
    cat(sprintf("%-20s  %2d   %-8s %5.1f %5.1f  %5.1f %5.1f %5.1f %6.1f\n",
                row$Team, row$Seed, row$Region, row$Rating, row$Tempo,
                row$S16, row$E8, row$FF, row$Champ))
  }
}

# ==============================================================================
# ELIMINATED TEAMS (Thursday losers - should show 0% everywhere)
# ==============================================================================
cat("\n========================================================\n")
cat("  ELIMINATED (Thursday R64 losers)\n")
cat("========================================================\n")
eliminated <- c("Siena", "Ohio State", "South Florida", "North Dakota State",
                "McNeese State", "Troy", "North Carolina", "Penn",
                "Saint Mary's", "Idaho", "Wisconsin", "Hawaii",
                "BYU", "Kennesaw State", "Howard", "Georgia")
for (tm in eliminated) {
  idx <- which(teams$name == tm)
  if (length(idx) > 0) {
    cat(sprintf("  %-20s (%2d) %s - ELIMINATED\n", tm, teams$seed[idx], teams$region[idx]))
  }
}

# ==============================================================================
# R32 MATCHUP PREVIEWS (pace-adjusted spreads)
# ==============================================================================
cat("\n========================================================\n")
cat("  R32 MATCHUP PREVIEWS (known Thursday matchups)\n")
cat("========================================================\n\n")

# Thursday winners are locked - show their R32 matchups
# R32 game g (1-16): winner of R64 game 2g-1 vs winner of R64 game 2g
r32_matchups <- list(
  # East
  list(a="Duke", b="TCU", region="East"),              # G1 winner vs G2 winner
  list(a="Louisville", b="Michigan State", region="East"), # G5 vs G6
  # South
  list(a="Vanderbilt", b="Nebraska", region="South"),   # G11 vs G12
  list(a="VCU", b="Illinois", region="South"),          # G13 vs G14 (VCU upset!)
  list(a="Texas A&M", b="Houston", region="South"),     # G15 vs G16
  # West
  list(a="High Point", b="Arkansas", region="West"),    # G19 vs G20 (High Point upset!)
  list(a="Texas", b="Gonzaga", region="West"),          # G21 vs G22 (Texas upset!)
  # Midwest
  list(a="Michigan", b="Saint Louis", region="Midwest") # G25 vs G26
)

for (m in r32_matchups) {
  ia <- which(teams$name == m$a)
  ib <- which(teams$name == m$b)
  if (length(ia) > 0 && length(ib) > 0) {
    ra <- teams$rating[ia]; rb <- teams$rating[ib]
    ta <- teams$tempo[ia];  tb <- teams$tempo[ib]
    gt <- (ta * tb) / AVG_TEMPO
    diff <- ra - rb
    pf <- gt / AVG_TEMPO
    spread <- diff * pf
    wp <- 1 / (1 + exp(-LOG_SCALE * spread))
    fav <- if (spread > 0) m$a else m$b
    cat(sprintf("  [%s] (%d) %-18s vs (%d) %-18s | Spread: %s -%.1f | WP: %.1f%%\n",
                m$region, teams$seed[ia], m$a, teams$seed[ib], m$b,
                fav, abs(spread), if(spread > 0) wp*100 else (1-wp)*100))
  }
}

cat("\n")
