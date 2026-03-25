#!/usr/bin/env Rscript
# ==============================================================================
# best_parlays.R — Top 11 no-repeat parlays from S16 onwards
#
# Parlay structure (11 legs):
#   S16  (4 legs): pick 4 teams, one per region — each wins their S16 game
#   E8   (4 legs): pick 2 teams from ONE FF half (E+S or W+Mw)
#                  each beats your designated S16 pick in E8
#                  (2 explicit legs + 2 implied: they first win their own S16 game)
#   FF   (1 leg) : your 2 E8 picks face each other — pick which one loses
#   Champ(2 legs): pick a team from the OTHER bracket half who wins their FF
#                  game AND beats your FF survivor in the championship
#   Total = 4 + 4 + 1 + 2 = 11 legs
#
# No-repeat: S16 picks, E8 picks, and Champ are all distinct teams
# ==============================================================================

library(data.table)
library(Rcpp)

sim <- readRDS("sim_results_2026_after_r2.rds")
ar  <- sim$all_results
tm  <- sim$teams
n   <- nrow(ar)

# Team lookup helpers
id2name <- setNames(tm$name, tm$team_id)
id2seed <- setNames(tm$seed, tm$team_id)
name2id <- setNames(tm$team_id, tm$name)

# Game assignments (verified from locked R32 results):
#  Game 49 (East-a):    Duke     vs St. John's
#  Game 50 (East-b):    Mich St  vs UConn
#  Game 51 (South-a):   Iowa     vs Nebraska
#  Game 52 (South-b):   Illinois vs Houston
#  Game 53 (West-a):    Arizona  vs Arkansas
#  Game 54 (West-b):    Texas    vs Purdue
#  Game 55 (Midwest-a): Michigan vs Alabama
#  Game 56 (Midwest-b): Tennessee vs Iowa State
#  Game 57: E8 East  (G49 winner vs G50 winner)
#  Game 58: E8 South (G51 winner vs G52 winner)
#  Game 59: E8 West  (G53 winner vs G54 winner)
#  Game 60: E8 Mwest (G55 winner vs G56 winner)
#  Game 61: FF  (E8 East vs E8 South)
#  Game 62: FF  (E8 West vs E8 Midwest)
#  Game 63: Champ

# Region game groups
S16_GAMES <- list(East=c(49L,50L), South=c(51L,52L), West=c(53L,54L), Midwest=c(55L,56L))
E8_GAMES  <- c(East=57L, South=58L, West=59L, Midwest=60L)
FF_GAMES  <- c(61L, 62L)  # [1]=East+South half, [2]=West+Mwest half

# All possible S16 teams by region (team_id)
s16_teams <- lapply(S16_GAMES, function(gs) unique(c(ar[1:1000, gs[1]], ar[1:1000, gs[2]])))

# ==============================================================================
# COMPUTE PARLAYS
#
# For each FF bracket half (E+S or W+Mw):
#   - Enumerate all (s16_E, s16_S/W/Mw) combos × (which E8 pick loses FF) × champ
#   - Compute probability from sims
# ==============================================================================

compute_parlays <- function(upset_half, n_top = 200) {
  # upset_half: "ES" (East+South upset, FF game 61) or "WM" (West+Midwest, FF game 62)
  if (upset_half == "ES") {
    upset_regions  <- c("East", "South")
    plain_regions  <- c("West", "Midwest")
    ff_upset_game  <- 61L   # the E8 upset pair meet here
    ff_plain_game  <- 62L   # opposite half
  } else {
    upset_regions  <- c("West", "Midwest")
    plain_regions  <- c("East", "South")
    ff_upset_game  <- 62L
    ff_plain_game  <- 61L
  }

  # Pre-extract relevant game columns
  # S16 games for all 4 regions
  all_s16 <- lapply(S16_GAMES, function(gs) list(a=ar[,gs[1]], b=ar[,gs[2]]))
  e8_u1   <- ar[, E8_GAMES[upset_regions[1]]]   # E8 game for upset region 1
  e8_u2   <- ar[, E8_GAMES[upset_regions[2]]]   # E8 game for upset region 2
  ff_u    <- ar[, ff_upset_game]                 # FF between the two E8 upset picks
  ff_p    <- ar[, ff_plain_game]                 # FF on the other side
  champ   <- ar[, 63L]

  # Get all teams that appear in upset region S16 games
  ur1 <- upset_regions[1]; ur2 <- upset_regions[2]
  pr1 <- plain_regions[1]; pr2 <- plain_regions[2]

  teams_ur1 <- sort(unique(c(all_s16[[ur1]]$a, all_s16[[ur1]]$b)))
  teams_ur2 <- sort(unique(c(all_s16[[ur2]]$a, all_s16[[ur2]]$b)))
  teams_pr1 <- sort(unique(c(all_s16[[pr1]]$a, all_s16[[pr1]]$b)))
  teams_pr2 <- sort(unique(c(all_s16[[pr2]]$a, all_s16[[pr2]]$b)))

  results <- list()

  for (t_ur1 in teams_ur1) {
    # Determine which S16 game t_ur1 plays in
    game_ur1 <- if (t_ur1 %in% unique(all_s16[[ur1]]$a)) S16_GAMES[[ur1]][1] else S16_GAMES[[ur1]][2]
    s16_ur1_ok <- ar[, game_ur1] == t_ur1

    for (t_ur2 in teams_ur2) {
      game_ur2 <- if (t_ur2 %in% unique(all_s16[[ur2]]$a)) S16_GAMES[[ur2]][1] else S16_GAMES[[ur2]][2]
      s16_ur2_ok <- ar[, game_ur2] == t_ur2

      # E8 upset conditions: E8 winner ≠ S16 pick in that region
      e8_u1_upset <- e8_u1 != t_ur1  # E8 upset region 1: someone else wins E8
      e8_u2_upset <- e8_u2 != t_ur2  # E8 upset region 2

      for (t_pr1 in teams_pr1) {
        game_pr1 <- if (t_pr1 %in% unique(all_s16[[pr1]]$a)) S16_GAMES[[pr1]][1] else S16_GAMES[[pr1]][2]
        s16_pr1_ok <- ar[, game_pr1] == t_pr1

        for (t_pr2 in teams_pr2) {
          game_pr2 <- if (t_pr2 %in% unique(all_s16[[pr2]]$a)) S16_GAMES[[pr2]][1] else S16_GAMES[[pr2]][2]
          s16_pr2_ok <- ar[, game_pr2] == t_pr2

          # Base mask: all 4 S16 picks correct + both E8 upsets happen
          base_mask <- s16_ur1_ok & s16_ur2_ok & s16_pr1_ok & s16_pr2_ok &
                       e8_u1_upset & e8_u2_upset

          if (sum(base_mask) == 0) next

          # For each sim in base_mask, what are the E8 upset picks and FF/Champ?
          e8p1 <- e8_u1[base_mask]  # E8 pick for upset region 1 (whoever won E8)
          e8p2 <- e8_u2[base_mask]  # E8 pick for upset region 2
          ff_u_w <- ff_u[base_mask]  # FF upset-side winner (one of e8p1 or e8p2)
          ff_p_w <- ff_p[base_mask]  # FF plain-side winner
          champ_w <- champ[base_mask]

          # Both E8 picks must meet in the upset FF game
          # Check: FF winner is one of the E8 picks (e8p1 or e8p2)
          ff_valid <- (ff_u_w == e8p1 | ff_u_w == e8p2)

          # Champ must come from plain FF side (not the upset FF winner)
          # and Champ wins game 63 by beating the upset FF winner
          champ_from_plain <- (champ_w == ff_p_w)  # champ is ff plain side winner
          champ_beats_ff   <- (champ_w != ff_u_w)  # champ is not the ff upset winner

          valid <- ff_valid & champ_from_plain & champ_beats_ff

          if (sum(valid) == 0) next

          # Build data.table of valid sim outcomes
          dt <- data.table(
            e8_upset1  = e8p1[valid],
            e8_upset2  = e8p2[valid],
            ff_winner  = ff_u_w[valid],   # which E8 pick survived FF (the "FF pick to LOSE" is the other one)
            ff_loser   = ifelse(ff_u_w[valid] == e8p1[valid], e8p2[valid], e8p1[valid]),
            champ      = champ_w[valid]
          )
          dt[, cnt := .N, by = .(e8_upset1, e8_upset2, ff_winner, ff_loser, champ)]
          top_dt <- unique(dt)[order(-cnt)][1:min(.N, 5)]

          for (j in seq_len(nrow(top_dt))) {
            row <- top_dt[j]
            # No-repeat check: all 7 picks unique
            all_picks <- c(t_ur1, t_ur2, t_pr1, t_pr2,
                           row$e8_upset1, row$e8_upset2, row$champ)
            if (length(unique(all_picks)) < 7) next

            results[[length(results)+1]] <- list(
              upset_half = upset_half,
              s16 = setNames(c(t_ur1, t_ur2, t_pr1, t_pr2),
                             c(ur1, ur2, pr1, pr2)),
              e8_upset = c(row$e8_upset1, row$e8_upset2),
              ff_winner = row$ff_winner,
              ff_loser  = row$ff_loser,
              champ     = row$champ,
              n_hits    = row$cnt,
              prob      = row$cnt / n * 100
            )
          }
        }
      }
    }
  }

  # Sort by probability
  results[order(sapply(results, `[[`, "prob"), decreasing=TRUE)]
}

cat("Computing East+South pairing parlays...\n")
parlays_es <- compute_parlays("ES")
cat(sprintf("  Found %d candidate parlays\n", length(parlays_es)))

cat("Computing West+Midwest pairing parlays...\n")
parlays_wm <- compute_parlays("WM")
cat(sprintf("  Found %d candidate parlays\n", length(parlays_wm)))

# Merge and deduplicate, take top 11
all_parlays <- c(parlays_es, parlays_wm)
all_parlays <- all_parlays[order(sapply(all_parlays, `[[`, "prob"), decreasing=TRUE)]

# Deduplicate by signature
seen <- character(0)
top11 <- list()
for (p in all_parlays) {
  sig <- paste(sort(c(p$s16, p$e8_upset, p$champ)), collapse="-")
  if (sig %in% seen) next
  seen <- c(seen, sig)
  top11[[length(top11)+1]] <- p
  if (length(top11) >= 11) break
}

# ==============================================================================
# PRINT RESULTS
# ==============================================================================

region_order <- c("East", "South", "West", "Midwest")

cat("\n================================================================\n")
cat("  TOP 11 NO-REPEAT PARLAYS (11 legs each)\n")
cat("================================================================\n")

for (k in seq_along(top11)) {
  p <- top11[[k]]

  # Identify which FF half and E8 upset regions
  if (p$upset_half == "ES") {
    ff_game <- 61; ur <- c("East","South"); pr <- c("West","Midwest")
  } else {
    ff_game <- 62; ur <- c("West","Midwest"); pr <- c("East","South")
  }

  s16_names  <- id2name[as.character(p$s16)]
  e8_names   <- id2name[as.character(p$e8_upset)]
  ff_win_nm  <- id2name[as.character(p$ff_winner)]
  ff_los_nm  <- id2name[as.character(p$ff_loser)]
  champ_nm   <- id2name[as.character(p$champ)]

  cat(sprintf("\n#%d  Prob: %.4f%%  (%d / %s sims)\n",
              k, p$prob, p$n_hits, format(n, big.mark=",")))
  cat(sprintf("  S16  East:    %s\n", s16_names["East"]))
  cat(sprintf("  S16  South:   %s\n", s16_names["South"]))
  cat(sprintf("  S16  West:    %s\n", s16_names["West"]))
  cat(sprintf("  S16  Midwest: %s\n", s16_names["Midwest"]))
  cat(sprintf("  E8 upset (%s):  %s beats %s\n",
              ur[1], e8_names[1], s16_names[ur[1]]))
  cat(sprintf("  E8 upset (%s): %s beats %s\n",
              ur[2], e8_names[2], s16_names[ur[2]]))
  cat(sprintf("  FF  game %d:   %s beats %s  ← %s advances\n",
              ff_game, ff_win_nm, ff_los_nm, ff_win_nm))
  cat(sprintf("  Champ:         %s beats %s\n", champ_nm, ff_win_nm))

  # Leg count summary
  cat(sprintf("  Legs: [S16: %s, %s, %s, %s] [E8: %s wins S16, %s wins S16, %s beats %s, %s beats %s] [FF: %s beats %s] [Champ: %s wins g%d, beats %s] = 11\n",
              s16_names["East"], s16_names["South"], s16_names["West"], s16_names["Midwest"],
              e8_names[1], e8_names[2],
              e8_names[1], s16_names[ur[1]], e8_names[2], s16_names[ur[2]],
              ff_win_nm, ff_los_nm,
              champ_nm, ifelse(p$upset_half=="ES",62,61), ff_win_nm))
}

# ==============================================================================
# MARGINAL PROBABILITIES SUMMARY
# ==============================================================================

cat("\n================================================================\n")
cat("  MARGINAL WIN PROBABILITIES (reference)\n")
cat("================================================================\n")

print_marg <- function(games, labels) {
  for (i in seq_along(games)) {
    w <- sort(table(ar[, games[i]]), decreasing=TRUE)
    for (tid in names(w)) {
      p <- as.integer(w[tid]) / n * 100
      if (p >= 1) cat(sprintf("  %-28s  %-22s  %5.1f%%\n",
                               labels[i], id2name[tid], p))
    }
  }
}

print_marg(c(49,50,51,52,53,54,55,56),
           c("S16 East-a (Duke/StJ)","S16 East-b (MSU/UConn)",
             "S16 South-a (Iowa/Neb)","S16 South-b (Ill/Hou)",
             "S16 West-a (Ariz/Ark)","S16 West-b (Tex/Pur)",
             "S16 Mid-a (Mich/Ala)","S16 Mid-b (Ten/ISt)"))
print_marg(57:60, c("E8 East (g57)","E8 South (g58)","E8 West (g59)","E8 Midwest (g60)"))
print_marg(61:63, c("FF East+South (g61)","FF West+Mwest (g62)","Champ (g63)"))
