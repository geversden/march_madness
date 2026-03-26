#!/usr/bin/env Rscript
# ==============================================================================
# survivor_challenge.R — Survivor Challenge Probability Pricer
#
# Challenge structure (8 picks, no team repeated):
#   S16  (4 picks): one team per region wins their S16 game
#   E8   (2 picks): one from East/South half + one from West/Midwest half
#                   each pick must beat your S16 pick in that region (upset)
#   FF   (1 pick) : winner of game 61 OR game 62 — must be a new team
#   Champ(1 pick) : winner of game 63 — must be a new team
#
# For each of 256 S16 quads (4 teams, one per region), finds the best
# achievable completion probability. Ranks quads and individual entries.
# ==============================================================================

library(data.table)

sim <- readRDS("sim_results_2026_after_r2.rds")
ar  <- sim$all_results
tm  <- sim$teams
n   <- nrow(ar)

# Team lookup helpers
id2name <- setNames(tm$name, tm$team_id)
id2seed <- setNames(tm$seed, tm$team_id)

# Game assignments (same as best_parlays.R):
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
#  Game 60: E8 Midwest (G55 winner vs G56 winner)
#  Game 61: FF  (E8 East vs E8 South)
#  Game 62: FF  (E8 West vs E8 Midwest)
#  Game 63: Championship

S16_GAMES <- list(East=c(49L,50L), South=c(51L,52L), West=c(53L,54L), Midwest=c(55L,56L))
E8_GAMES  <- c(East=57L, South=58L, West=59L, Midwest=60L)

# Identify which S16 game each team plays in (from first 1000 sims)
game_a_teams <- lapply(S16_GAMES, function(gs) unique(ar[1:1000, gs[1]]))
game_b_teams <- lapply(S16_GAMES, function(gs) unique(ar[1:1000, gs[2]]))

# All teams per region
teams_by_region <- lapply(S16_GAMES, function(gs) {
  sort(unique(c(ar[1:1000, gs[1]], ar[1:1000, gs[2]])))
})

# Helper: which S16 game does team t play in for region reg?
s16_game_for <- function(t, reg) {
  if (t %in% game_a_teams[[reg]]) S16_GAMES[[reg]][1] else S16_GAMES[[reg]][2]
}

# ==============================================================================
# COMPUTE SURVIVOR ENTRIES
# ==============================================================================

compute_survivor <- function() {
  # Pre-extract all game columns for speed
  g49 <- ar[,49L]; g50 <- ar[,50L]
  g51 <- ar[,51L]; g52 <- ar[,52L]
  g53 <- ar[,53L]; g54 <- ar[,54L]
  g55 <- ar[,55L]; g56 <- ar[,56L]
  g57 <- ar[,57L]; g58 <- ar[,58L]
  g59 <- ar[,59L]; g60 <- ar[,60L]
  g61 <- ar[,61L]; g62 <- ar[,62L]
  g63 <- ar[,63L]

  regions   <- c("East","South","West","Midwest")
  es_games  <- c(57L, 58L)   # E8 game choices for East+South half
  wm_games  <- c(59L, 60L)   # E8 game choices for West+Midwest half

  all_entries <- list()
  quad_best   <- list()

  total_quads <- prod(sapply(teams_by_region, length))
  cat(sprintf("Evaluating %d S16 quads...\n", total_quads))

  quad_count <- 0L

  for (t_E in teams_by_region[["East"]]) {
    gE <- s16_game_for(t_E, "East")
    s16_E <- (if (gE==49L) g49 else g50) == t_E

    for (t_S in teams_by_region[["South"]]) {
      gS <- s16_game_for(t_S, "South")
      s16_ES <- s16_E & ((if (gS==51L) g51 else g52) == t_S)

      for (t_W in teams_by_region[["West"]]) {
        gW <- s16_game_for(t_W, "West")
        s16_ESW <- s16_ES & ((if (gW==53L) g53 else g54) == t_W)

        for (t_M in teams_by_region[["Midwest"]]) {
          gM <- s16_game_for(t_M, "Midwest")
          s16_mask <- s16_ESW & ((if (gM==55L) g55 else g56) == t_M)

          n_s16 <- sum(s16_mask)
          if (n_s16 == 0L) next

          quad_count <- quad_count + 1L
          prior_4 <- c(t_E, t_S, t_W, t_M)
          quad_best_prob <- 0
          quad_best_entry <- NULL

          # ── 4 E8 game combos ────────────────────────────────────────────────
          for (es_game in es_games) {
            # Which S16 pick gets upset in the E+S half?
            es_s16_pick <- if (es_game == 57L) t_E else t_S
            e8_es_raw   <- if (es_game == 57L) g57 else g58

            for (wm_game in wm_games) {
              wm_s16_pick <- if (wm_game == 59L) t_W else t_M
              e8_wm_raw   <- if (wm_game == 59L) g59 else g60

              # E8 mask: S16 picks are upset in selected games
              e8_mask <- s16_mask &
                         (e8_es_raw != es_s16_pick) &
                         (e8_wm_raw != wm_s16_pick)

              n_e8 <- sum(e8_mask)
              if (n_e8 == 0L) next

              # Extract outcomes for valid sims
              e8_es_w <- e8_es_raw[e8_mask]
              e8_wm_w <- e8_wm_raw[e8_mask]
              ff61_w  <- g61[e8_mask]
              ff62_w  <- g62[e8_mask]
              champ_w <- g63[e8_mask]

              # Unique E8 pick pairs
              dt <- data.table(e8es=e8_es_w, e8wm=e8_wm_w,
                               ff61=ff61_w, ff62=ff62_w, champ=champ_w)
              uniq_e8 <- unique(dt[, .(e8es, e8wm)])

              for (i in seq_len(nrow(uniq_e8))) {
                e8es_pick <- uniq_e8$e8es[i]
                e8wm_pick <- uniq_e8$e8wm[i]
                prior_6   <- c(prior_4, e8es_pick, e8wm_pick)

                # No-repeat: e8 picks must be distinct from each other
                # (guaranteed by region separation, but check anyway)
                if (length(unique(prior_6)) < 6L) next

                sub <- dt[e8es == e8es_pick & e8wm == e8wm_pick]

                # ── FF game choice ─────────────────────────────────────────
                for (ff_game in c(61L, 62L)) {
                  ff_col <- if (ff_game == 61L) sub$ff61 else sub$ff62

                  # FF pick must not be any of prior 6
                  ff_ok_mask <- !(ff_col %in% prior_6)
                  if (!any(ff_ok_mask)) next

                  unique_ff <- unique(ff_col[ff_ok_mask])

                  for (ff_pick in unique_ff) {
                    prior_7 <- c(prior_6, ff_pick)
                    ff_and_ok <- ff_ok_mask & (ff_col == ff_pick)

                    champ_sub <- sub$champ[ff_and_ok]
                    champ_ok  <- !(champ_sub %in% prior_7)
                    if (!any(champ_ok)) next

                    unique_champ <- unique(champ_sub[champ_ok])

                    for (champ_pick in unique_champ) {
                      n_hits <- sum(champ_ok & (champ_sub == champ_pick))
                      prob   <- n_hits / n

                      entry <- list(
                        s16_E = t_E, s16_S = t_S, s16_W = t_W, s16_M = t_M,
                        es_game = es_game, wm_game = wm_game,
                        e8_es = e8es_pick, e8_wm = e8wm_pick,
                        ff_game = ff_game, ff_pick = ff_pick,
                        champ = champ_pick,
                        n_hits = n_hits,
                        prob = prob
                      )

                      all_entries[[length(all_entries)+1]] <- entry

                      if (prob > quad_best_prob) {
                        quad_best_prob  <- prob
                        quad_best_entry <- entry
                      }
                    }
                  }
                }
              }
            }
          }

          if (!is.null(quad_best_entry)) {
            quad_best[[length(quad_best)+1]] <- quad_best_entry
          }
        }
      }
    }
  }

  cat(sprintf("Evaluated %d quads, found %d valid entries.\n",
              quad_count, length(all_entries)))

  list(all_entries = all_entries, quad_best = quad_best)
}

cat("Running survivor challenge analysis...\n")
results <- compute_survivor()

# Sort all entries and quad bests by probability
all_entries <- results$all_entries
quad_best   <- results$quad_best

all_entries <- all_entries[order(sapply(all_entries, `[[`, "prob"), decreasing=TRUE)]
quad_best   <- quad_best[order(sapply(quad_best,   `[[`, "prob"), decreasing=TRUE)]

# ==============================================================================
# PRINT SECTION 1 — TOP S16 QUAD RANKINGS
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("  S16 QUAD RANKINGS — Best Achievable Completion Probability\n")
cat("================================================================\n")
cat(sprintf("  %-4s  %-14s %-14s %-14s %-14s  %7s  %-10s %-10s  %-12s  %-12s\n",
            "Rank", "East", "South", "West", "Midwest",
            "Prob%", "E8-ES(g)", "E8-WM(g)", "FF Pick(g)", "Champ"))
cat(sprintf("  %s\n", paste(rep("-", 116), collapse="")))

for (k in seq_len(min(50L, length(quad_best)))) {
  e <- quad_best[[k]]
  es_region <- if (e$es_game == 57L) "East" else "South"
  wm_region <- if (e$wm_game == 59L) "West"  else "Midwest"
  cat(sprintf("  %-4d  %-14s %-14s %-14s %-14s  %6.4f%%  %-10s %-10s  %-12s  %-12s\n",
              k,
              id2name[e$s16_E], id2name[e$s16_S],
              id2name[e$s16_W], id2name[e$s16_M],
              e$prob * 100,
              sprintf("%s(g%d)", id2name[e$e8_es], e$es_game),
              sprintf("%s(g%d)", id2name[e$e8_wm], e$wm_game),
              sprintf("%s(g%d)", id2name[e$ff_pick], e$ff_game),
              id2name[e$champ]))
}

# ==============================================================================
# PRINT SECTION 2 — TOP INDIVIDUAL COMPLETE ENTRIES
# ==============================================================================

cat("\n")
cat("================================================================\n")
cat("  TOP 20 INDIVIDUAL 8-PICK SURVIVOR ENTRIES\n")
cat("================================================================\n")

top20 <- head(all_entries, 20L)

for (k in seq_along(top20)) {
  e <- top20[[k]]

  es_region  <- if (e$es_game == 57L) "East"  else "South"
  wm_region  <- if (e$wm_game == 59L) "West"  else "Midwest"

  # Identify which S16 pick was upset in each E8 game
  es_s16_nm <- if (e$es_game == 57L) id2name[e$s16_E] else id2name[e$s16_S]
  wm_s16_nm <- if (e$wm_game == 59L) id2name[e$s16_W] else id2name[e$s16_M]

  cat(sprintf("\n#%-2d  Prob: %.4f%%  (%d / %s sims)\n",
              k, e$prob * 100, e$n_hits, format(n, big.mark=",")))
  cat(sprintf("  S16  East:    %s\n", id2name[e$s16_E]))
  cat(sprintf("  S16  South:   %s\n", id2name[e$s16_S]))
  cat(sprintf("  S16  West:    %s\n", id2name[e$s16_W]))
  cat(sprintf("  S16  Midwest: %s\n", id2name[e$s16_M]))
  cat(sprintf("  E8   %s (g%d): %s beats %s\n",
              es_region, e$es_game, id2name[e$e8_es], es_s16_nm))
  cat(sprintf("  E8   %s (g%d): %s beats %s\n",
              wm_region, e$wm_game, id2name[e$e8_wm], wm_s16_nm))
  cat(sprintf("  FF   game %d: %s wins\n", e$ff_game, id2name[e$ff_pick]))
  cat(sprintf("  Champ(g63):  %s wins championship\n", id2name[e$champ]))

  # Sanity: confirm all 8 picks are distinct
  all8 <- c(e$s16_E, e$s16_S, e$s16_W, e$s16_M,
            e$e8_es, e$e8_wm, e$ff_pick, e$champ)
  if (length(unique(all8)) < 8L) cat("  *** WARNING: duplicate team in picks! ***\n")
}

# ==============================================================================
# PRINT SECTION 3 — MARGINAL WIN PROBABILITIES (reference)
# ==============================================================================

cat("\n")
cat("================================================================\n")
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

# ==============================================================================
# SECTION 4 — ALL PARLAYS AS A TIBBLE
# ==============================================================================

parlays_dt <- rbindlist(lapply(all_entries, function(e) {
  es_region <- if (e$es_game == 57L) "East"  else "South"
  wm_region <- if (e$wm_game == 59L) "West"  else "Midwest"
  data.table(
    s16_east    = id2name[e$s16_E],
    s16_south   = id2name[e$s16_S],
    s16_west    = id2name[e$s16_W],
    s16_midwest = id2name[e$s16_M],
    e8_es_region = es_region,
    e8_es_game  = e$es_game,
    e8_es_pick  = id2name[e$e8_es],
    e8_wm_region = wm_region,
    e8_wm_game  = e$wm_game,
    e8_wm_pick  = id2name[e$e8_wm],
    ff_game     = e$ff_game,
    ff_pick     = id2name[e$ff_pick],
    champ       = id2name[e$champ],
    n_hits      = e$n_hits,
    prob        = e$prob
  )
}))
setorder(parlays_dt, -prob)

cat("\n")
cat("================================================================\n")
cat(sprintf("  ALL PARLAYS TIBBLE  (%d rows)\n", nrow(parlays_dt)))
cat("================================================================\n")
print(parlays_dt, topn = 10)

# ==============================================================================
# SECTION 5 — SUMMARY BY S16 QUAD (4-LEG STARTING PARLAY)
# ==============================================================================

s16_summary <- parlays_dt[, .(
  n_completions = .N,
  best_prob     = max(prob),
  sum_prob      = sum(prob),
  best_e8_es    = e8_es_pick[which.max(prob)],
  best_e8_wm    = e8_wm_pick[which.max(prob)],
  best_ff       = ff_pick[which.max(prob)],
  best_champ    = champ[which.max(prob)]
), by = .(s16_east, s16_south, s16_west, s16_midwest)]
setorder(s16_summary, -best_prob)

N_ENTRIES <- 112L

# Proportional allocation by sum_prob, rounded, adjusted to sum exactly to N_ENTRIES
s16_summary[, alloc := round(sum_prob / sum(sum_prob) * N_ENTRIES)]
diff <- N_ENTRIES - sum(s16_summary$alloc)
if (diff != 0L) {
  # Add/remove from quads with largest rounding remainders
  remainders <- s16_summary$sum_prob / sum(s16_summary$sum_prob) * N_ENTRIES - s16_summary$alloc
  idx <- order(remainders, decreasing = (diff > 0))[seq_len(abs(diff))]
  s16_summary$alloc[idx] <- s16_summary$alloc[idx] + sign(diff)
}

cat("\n")
cat("================================================================\n")
cat("  S16 QUAD SUMMARY — All 256 Starting 4-Pick Combos\n")
cat("================================================================\n")
print(s16_summary, nrows = nrow(s16_summary))

# ==============================================================================
# SECTION 5b — GREEDY TOP-112 ENTRIES
# ==============================================================================

top112 <- head(parlays_dt, N_ENTRIES)

cat("\n")
cat("================================================================\n")
cat(sprintf("  GREEDY TOP %d ENTRIES (by individual prob)\n", N_ENTRIES))
cat("================================================================\n")
print(top112[, .(s16_east, s16_south, s16_west, s16_midwest,
                 e8_es_pick, e8_wm_pick, ff_pick, champ, prob)],
      nrows = N_ENTRIES)

# ==============================================================================
# SECTION 6 — OWNERSHIP ACROSS TOP-112 ENTRIES
# ==============================================================================

ownership <- rbindlist(list(
  top112[, .(slot = "S16 East",    team = s16_east)],
  top112[, .(slot = "S16 South",   team = s16_south)],
  top112[, .(slot = "S16 West",    team = s16_west)],
  top112[, .(slot = "S16 Midwest", team = s16_midwest)],
  top112[, .(slot = "E8 ES pick",  team = e8_es_pick)],
  top112[, .(slot = "E8 WM pick",  team = e8_wm_pick)],
  top112[, .(slot = "FF pick",     team = ff_pick)],
  top112[, .(slot = "Champ",       team = champ)]
))

slot_order <- c("S16 East","S16 South","S16 West","S16 Midwest",
                "E8 ES pick","E8 WM pick","FF pick","Champ")
ownership[, slot := factor(slot, levels = slot_order)]

own_summary <- ownership[, .(n = .N, pct = .N / N_ENTRIES * 100), by = .(slot, team)]
setorder(own_summary, slot, -n)

cat("\n")
cat("================================================================\n")
cat(sprintf("  OWNERSHIP — Top %d Entries\n", N_ENTRIES))
cat("================================================================\n")
print(own_summary, nrows = nrow(own_summary))
