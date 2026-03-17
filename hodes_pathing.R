#!/usr/bin/env Rscript
# ==============================================================================
# hodes_pathing.R
# Optimal pick pathing for Hodes-format NCAA tournament survivor pool
#
# FORMAT:
#   R1: Pick 3 teams       R2: Pick 3 teams
#   R3: Pick 1 or 2 teams  R4: Pick 1 team
#   R5: Pick 1 team        R6: Pick 1 team
#   No team can be picked twice across rounds.
#
# SURVIVOR / WINNER TAKE ALL:
#   - One wrong pick = eliminated in that round
#   - Last person standing wins
#   - If multiple people survive the same deepest round, tiebreakers apply
#   - If everyone dies in the same round, tiebreakers among those who died there
#
# TIEBREAKERS (among players eliminated in the same round, or both perfect):
#   1st: Picking 2 in R3 ALWAYS beats picking 1 in R3
#   2nd: Higher sum of seeds across all picks wins
#
# STRATEGY:
#   - Always pick 2 in R3 (automatic tiebreaker edge)
#   - Maximize depth of survival (survive as many rounds as possible)
#   - Among equal depth, maximize seed sum for TB2
#   - R6 champion pick matters most — getting it right = going perfect
#
# Usage:
#   source("hodes_pathing.R")
# ==============================================================================

library(data.table)

# ==============================================================================
# LOAD SIMULATION RESULTS
# ==============================================================================

if (file.exists("sim_results_part1.parquet") && file.exists("sim_results_part2.parquet") &&
    file.exists("sim_results_meta.rds")) {
  library(arrow)
  sim <- readRDS("sim_results_meta.rds")
  sim$all_results <- as.matrix(cbind(read_parquet("sim_results_part1.parquet"),
                                     read_parquet("sim_results_part2.parquet")))
} else {
  sim <- readRDS("sim_results.rds")
}
teams <- as.data.table(sim$teams)
ar <- sim$all_results       # n_sims x 63 integer matrix
ri <- sim$round_info
n_sims <- sim$n_sims
n_teams <- nrow(teams)

# Round column indices (into all_results columns)
round_cols <- list(
  R1 = which(ri$round_num == 1),   # R64:  32 games
  R2 = which(ri$round_num == 2),   # R32:  16 games
  R3 = which(ri$round_num == 3),   # S16:   8 games
  R4 = which(ri$round_num == 4),   # E8:    4 games
  R5 = which(ri$round_num == 5),   # FF:    2 games
  R6 = which(ri$round_num == 6)    # Champ: 1 game
)

# ==============================================================================
# PRECOMPUTE TEAM-ROUND WIN MATRICES
# team_round_wins[[rd]][sim, team_id] = TRUE if team won a game in that round
# ==============================================================================

cat("Precomputing team-round win matrices...\n")

team_round_wins <- vector("list", 6)
team_round_probs <- matrix(0, nrow = n_teams, ncol = 6,
                           dimnames = list(teams$name, paste0("R", 1:6)))

for (rd in 1:6) {
  cols <- round_cols[[rd]]
  round_results <- ar[, cols, drop = FALSE]

  win_mat <- matrix(FALSE, nrow = n_sims, ncol = n_teams)
  for (j in seq_len(ncol(round_results))) {
    idx <- cbind(seq_len(n_sims), round_results[, j])
    win_mat[idx] <- TRUE
  }

  team_round_wins[[rd]] <- win_mat
  team_round_probs[, rd] <- colMeans(win_mat)
}

cat(sprintf("Done. %s sims x %d teams x 6 rounds.\n\n",
            format(n_sims, big.mark = ","), n_teams))

# ==============================================================================
# HELPERS
# ==============================================================================

name_to_id <- function(nms) {
  ids <- match(nms, teams$name)
  if (any(is.na(ids))) stop("Unknown team(s): ", paste(nms[is.na(ids)], collapse = ", "))
  teams$team_id[ids]
}

id_to_name <- function(ids) teams$name[ids]
id_to_seed <- function(ids) teams$seed[ids]

make_path <- function(R1, R2, R3, R4, R5, R6) {
  list(R1 = name_to_id(R1), R2 = name_to_id(R2), R3 = name_to_id(R3),
       R4 = name_to_id(R4), R5 = name_to_id(R5), R6 = name_to_id(R6))
}

# ==============================================================================
# EVALUATE A PATH
# ==============================================================================
#
# Returns:
#   death_round - integer vector (length n_sims): round where eliminated (1-6),
#                 or 7 if survived all 6 (perfect). This is the KEY metric.
#   per_round   - cumulative survival rates through each round
#   seed_sum    - tiebreaker seed sum
#   r3_bonus    - TRUE if 2 picks in R3
#
evaluate_path <- function(picks) {
  all_ids <- unlist(picks, use.names = FALSE)
  if (length(unique(all_ids)) != length(all_ids)) {
    dupes <- all_ids[duplicated(all_ids)]
    stop("Duplicate team(s): ", paste(id_to_name(dupes), collapse = ", "))
  }
  stopifnot(length(picks$R1) == 3)
  stopifnot(length(picks$R2) == 3)
  stopifnot(length(picks$R3) %in% c(1, 2))
  stopifnot(length(picks$R4) == 1)
  stopifnot(length(picks$R5) == 1)
  stopifnot(length(picks$R6) == 1)

  # Track which round each sim dies in (7 = survived everything)
  death_round <- rep(7L, n_sims)
  alive <- rep(TRUE, n_sims)
  per_round <- numeric(6)
  names(per_round) <- paste0("R", 1:6)

  for (rd in 1:6) {
    round_ok <- rep(TRUE, n_sims)
    for (tid in picks[[rd]]) {
      round_ok <- round_ok & team_round_wins[[rd]][, tid]
    }
    # Mark deaths: alive going into this round but failed
    new_deaths <- alive & !round_ok
    death_round[new_deaths] <- as.integer(rd)
    alive <- alive & round_ok
    per_round[rd] <- mean(alive)
  }

  seed_sum  <- sum(id_to_seed(all_ids))
  r3_bonus  <- length(picks$R3) == 2

  list(
    death_round   = death_round,   # 1-6 = died in that round, 7 = perfect
    per_round     = per_round,     # cumulative survival through each round
    survival_rate = mean(alive),   # P(perfect) = P(death_round == 7)
    seed_sum      = seed_sum,
    r3_bonus      = r3_bonus,
    n_picks       = length(all_ids)
  )
}

# ==============================================================================
# PRINT A PATH
# ==============================================================================

print_path <- function(picks, label = "PATH") {
  ev <- evaluate_path(picks)

  cat(sprintf("\n===== %s =====\n", label))
  round_labels <- c("R1 (R64)", "R2 (R32)", "R3 (S16)", "R4 (E8 )", "R5 (FF )", "R6 (Chp)")
  for (rd in 1:6) {
    tids <- picks[[rd]]
    info <- sprintf("(%d) %s", id_to_seed(tids), id_to_name(tids))
    cat(sprintf("  %-12s  %s\n", round_labels[rd], paste(info, collapse = ",  ")))
  }
  cat(sprintf("\n  P(perfect):   %6.2f%%  (%d / %s sims)\n",
              100 * ev$survival_rate, sum(ev$death_round == 7),
              format(n_sims, big.mark = ",")))
  cat("  Cumulative survival by round:\n")
  for (rd in 1:6) {
    cat(sprintf("    Thru %-6s  %6.2f%%\n", round_labels[rd], 100 * ev$per_round[rd]))
  }
  cat("  Elimination distribution:\n")
  for (rd in 1:6) {
    n_dead <- sum(ev$death_round == rd)
    if (n_dead > 0) {
      cat(sprintf("    Died in R%d: %6.2f%%\n", rd, 100 * n_dead / n_sims))
    }
  }
  cat(sprintf("  R3 bonus:     %s\n", ifelse(ev$r3_bonus, "YES (2 picks)", "NO (1 pick)")))
  cat(sprintf("  Seed sum:     %d\n", ev$seed_sum))
  cat("\n")

  invisible(ev)
}

# ==============================================================================
# COMPARE TWO PLAYERS: who wins each sim?
#
# Logic per sim:
#   - Whoever survived deeper wins outright
#   - If both died in the same round (or both perfect):
#       TB1: R3=2 beats R3=1
#       TB2: higher seed sum wins
# ==============================================================================

resolve_tb <- function(r3a, r3b, ss_a, ss_b) {
  # Returns: 1 = A wins, 2 = B wins, 0 = true tie
  if (r3a && !r3b) return(1L)
  if (r3b && !r3a) return(2L)
  if (ss_a > ss_b) return(1L)
  if (ss_b > ss_a) return(2L)
  0L
}

head_to_head <- function(path_a, path_b, label_a = "Path A", label_b = "Path B") {
  ea <- evaluate_path(path_a)
  eb <- evaluate_path(path_b)

  a_wins <- 0L; b_wins <- 0L; ties <- 0L

  tb_result <- resolve_tb(ea$r3_bonus, eb$r3_bonus, ea$seed_sum, eb$seed_sum)

  for (s in seq_len(n_sims)) {
    da <- ea$death_round[s]
    db <- eb$death_round[s]

    if (da > db) {
      # A survived deeper
      a_wins <- a_wins + 1L
    } else if (db > da) {
      # B survived deeper
      b_wins <- b_wins + 1L
    } else {
      # Same depth — tiebreakers (precomputed since they're static)
      if (tb_result == 1L) { a_wins <- a_wins + 1L }
      else if (tb_result == 2L) { b_wins <- b_wins + 1L }
      else { ties <- ties + 1L }
    }
  }

  cat(sprintf("\n===== H2H: %s vs %s =====\n", label_a, label_b))
  cat(sprintf("  %s wins:       %6.2f%%  (%d sims)\n", label_a, 100 * a_wins / n_sims, a_wins))
  cat(sprintf("  %s wins:       %6.2f%%  (%d sims)\n", label_b, 100 * b_wins / n_sims, b_wins))
  if (ties > 0)
    cat(sprintf("  True ties:        %6.2f%%  (%d sims)\n", 100 * ties / n_sims, ties))
  cat(sprintf("  P(perfect): %s = %.2f%%,  %s = %.2f%%\n",
              label_a, 100 * ea$survival_rate, label_b, 100 * eb$survival_rate))
  cat(sprintf("  Seed sums:  %s = %d,  %s = %d\n",
              label_a, ea$seed_sum, label_b, eb$seed_sum))
  cat(sprintf("  R3 bonus:   %s = %s,  %s = %s\n",
              label_a, ifelse(ea$r3_bonus, "YES", "NO"),
              label_b, ifelse(eb$r3_bonus, "YES", "NO")))
  cat("\n")

  invisible(list(a_wins = a_wins, b_wins = b_wins, ties = ties))
}

# ==============================================================================
# FIELD EVALUATION  (winner take all against N opponents)
#
# Per sim: find who survived the deepest. If tied on depth, apply tiebreakers.
# Only 1 winner per sim.
# ==============================================================================

field_eval <- function(my_path, opponent_paths, my_label = "Mine") {
  my_ev <- evaluate_path(my_path)
  opp_evs <- lapply(opponent_paths, evaluate_path)
  n_opp <- length(opp_evs)

  my_win <- 0L

  for (s in seq_len(n_sims)) {
    my_depth <- my_ev$death_round[s]

    # Check if any opponent survived deeper or ties me + wins TB
    i_win <- TRUE
    for (opp in opp_evs) {
      opp_depth <- opp$death_round[s]

      if (opp_depth > my_depth) {
        i_win <- FALSE
        break
      } else if (opp_depth == my_depth) {
        # Tiebreaker
        # TB1: R3 bonus
        if (opp$r3_bonus && !my_ev$r3_bonus) { i_win <- FALSE; break }
        if (!opp$r3_bonus && my_ev$r3_bonus) next
        # TB2: seed sum
        if (opp$seed_sum > my_ev$seed_sum) { i_win <- FALSE; break }
        if (my_ev$seed_sum > opp$seed_sum) next
        # True tie — conservative: count as loss
        i_win <- FALSE; break
      }
      # opp_depth < my_depth → I outlasted them, keep going
    }

    if (i_win) my_win <- my_win + 1L
  }

  cat(sprintf("\n===== FIELD: %s vs %d opponents =====\n", my_label, n_opp))
  cat(sprintf("  I win pool:   %6.2f%%  (%d / %s sims)\n",
              100 * my_win / n_sims, my_win, format(n_sims, big.mark = ",")))
  cat(sprintf("  P(perfect):   %6.2f%%\n", 100 * my_ev$survival_rate))
  cat(sprintf("  Seed sum:     %d\n", my_ev$seed_sum))
  cat(sprintf("  R3 bonus:     %s\n", ifelse(my_ev$r3_bonus, "YES", "NO")))
  cat("\n")

  invisible(list(win = my_win, win_pct = 100 * my_win / n_sims))
}

# ==============================================================================
# DISPLAY WIN PROBABILITY TABLE
# ==============================================================================

print_round_probs <- function(min_prob = 0.01) {
  cat("\n===== TEAM WIN PROBABILITIES BY ROUND =====\n")
  cat(sprintf("%-20s %4s %-8s  %6s %6s %6s %6s %6s %6s\n",
              "Team", "Seed", "Region", "R1%", "R2%", "R3%", "R4%", "R5%", "R6%"))
  cat(paste(rep("-", 82), collapse = ""), "\n")

  ord <- order(-team_round_probs[, 6], -team_round_probs[, 5],
               -team_round_probs[, 4], -team_round_probs[, 3])

  for (i in ord) {
    probs <- team_round_probs[i, ]
    if (max(probs) < min_prob) next
    cat(sprintf("%-20s  %2d   %-8s  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f  %5.1f\n",
                teams$name[i], teams$seed[i], teams$region[i],
                100 * probs[1], 100 * probs[2], 100 * probs[3],
                100 * probs[4], 100 * probs[5], 100 * probs[6]))
  }
  cat("\n")
}

# ==============================================================================
# OPTIMIZER: MAXIMIZE P(WIN) = survive deepest + win tiebreakers
#
# Core insight: since it's last-man-standing, the key metric is NOT just
# P(perfect). A path that consistently survives to R4/R5 even when the
# champ pick busts can still win if opponents die earlier.
#
# We optimize by building paths anchored on a champion, then scoring
# each path by its full death_round distribution (weighted toward deeper
# survival) plus seed sum tiebreaker.
# ==============================================================================

#' Score a path for optimization. Higher is better.
#' Weights deep survival heavily — surviving to R5 is worth much more than R3.
path_score <- function(ev) {
  # Weighted survival depth: exponentially more valuable to survive deeper
  depth_weights <- c(1, 2, 4, 8, 16, 32, 64)  # R1 thru perfect
  depth_dist <- tabulate(ev$death_round, nbins = 7) / n_sims
  weighted_depth <- sum(depth_dist * depth_weights)

  # Seed sum as minor tiebreaker (normalized to ~0-1 range)
  seed_bonus <- ev$seed_sum / 200

  weighted_depth + seed_bonus
}

optimize_paths <- function(champ_candidates = 8,
                           late_candidates  = 12,
                           early_candidates = 20) {

  champ_order <- order(-team_round_probs[, 6])
  champ_pool  <- champ_order[seq_len(champ_candidates)]

  all_paths <- list()
  path_idx  <- 0

  for (r6_team in champ_pool) {
    used <- r6_team

    # --- R5: best available FF winner ---
    avail <- setdiff(seq_len(n_teams), used)
    r5_probs <- team_round_probs[avail, 5]
    r5_pool <- avail[order(-r5_probs)[seq_len(min(late_candidates, length(avail)))]]

    for (r5_team in r5_pool[1:min(3, length(r5_pool))]) {
      used2 <- c(used, r5_team)

      # --- R4: best available E8 winner ---
      avail2 <- setdiff(seq_len(n_teams), used2)
      r4_probs <- team_round_probs[avail2, 4]
      r4_pool <- avail2[order(-r4_probs)[seq_len(min(late_candidates, length(avail2)))]]

      for (r4_team in r4_pool[1:min(3, length(r4_pool))]) {
        used3 <- c(used2, r4_team)

        # --- R3: best 2 available S16 winners ---
        avail3 <- setdiff(seq_len(n_teams), used3)
        r3_probs <- team_round_probs[avail3, 3]
        r3_order <- avail3[order(-r3_probs)]
        r3_pool  <- r3_order[seq_len(min(late_candidates, length(r3_order)))]

        n3 <- min(6, length(r3_pool))
        for (i3a in seq_len(n3 - 1)) {
          for (i3b in (i3a + 1):n3) {
            r3a <- r3_pool[i3a]
            r3b <- r3_pool[i3b]
            used4 <- c(used3, r3a, r3b)

            # --- R2: best 3 available R32 winners ---
            avail4 <- setdiff(seq_len(n_teams), used4)
            r2_probs <- team_round_probs[avail4, 2]
            r2_order <- avail4[order(-r2_probs)]
            r2_pool <- r2_order[seq_len(min(early_candidates, length(r2_order)))]
            r2_picks <- r2_pool[1:3]
            used5 <- c(used4, r2_picks)

            # --- R1: among safe picks (>90% R1 prob), take highest seeds ---
            avail5 <- setdiff(seq_len(n_teams), used5)
            r1_probs <- team_round_probs[avail5, 1]

            safe_r1 <- avail5[r1_probs > 0.90]
            if (length(safe_r1) >= 3) {
              safe_seeds <- id_to_seed(safe_r1)
              r1_picks <- safe_r1[order(-safe_seeds)][1:3]
            } else {
              r1_picks <- avail5[order(-r1_probs)][1:3]
            }

            path <- list(R1 = r1_picks, R2 = r2_picks,
                         R3 = c(r3a, r3b), R4 = r4_team,
                         R5 = r5_team, R6 = r6_team)

            path_idx <- path_idx + 1
            all_paths[[path_idx]] <- path
          }
        }
      }
    }
  }

  cat(sprintf("Evaluated %s candidate paths.\n", format(length(all_paths), big.mark = ",")))

  # Evaluate and score all paths
  results <- lapply(all_paths, function(p) {
    ev <- evaluate_path(p)
    list(path = p, score = path_score(ev),
         survival_rate = ev$survival_rate,
         seed_sum = ev$seed_sum, r3_bonus = ev$r3_bonus)
  })

  # Sort by composite score descending
  ord <- order(-vapply(results, `[[`, numeric(1), "score"))
  results <- results[ord]

  cat(sprintf("Top path: P(perfect) = %.2f%%, seed sum = %d, score = %.3f\n",
              100 * results[[1]]$survival_rate,
              results[[1]]$seed_sum,
              results[[1]]$score))

  results
}

# ==============================================================================
# SEED SUM MAXIMIZER
# ==============================================================================

maximize_seed_sum <- function(path, prob_floor = 0.95) {
  base_ev <- evaluate_path(path)
  base_score <- path_score(base_ev)
  min_score <- base_score * prob_floor

  best_path <- path
  best_seed_sum <- base_ev$seed_sum
  improved <- TRUE

  while (improved) {
    improved <- FALSE
    for (rd in 1:2) {
      for (slot in seq_along(best_path[[rd]])) {
        current_tid <- best_path[[rd]][slot]
        current_seed <- id_to_seed(current_tid)
        used_others <- setdiff(unlist(best_path, use.names = FALSE), current_tid)

        available <- setdiff(seq_len(n_teams), used_others)
        for (cand in available) {
          if (id_to_seed(cand) <= current_seed) next

          test_path <- best_path
          test_path[[rd]][slot] <- cand
          test_ev <- evaluate_path(test_path)

          if (path_score(test_ev) >= min_score && test_ev$seed_sum > best_seed_sum) {
            best_path <- test_path
            best_seed_sum <- test_ev$seed_sum
            improved <- TRUE
          }
        }
      }
    }
  }

  best_path
}

# ==============================================================================
# RUN OPTIMIZATION
# ==============================================================================

print_round_probs(min_prob = 0.005)

cat("========================================================\n")
cat("  OPTIMIZING PATHS (Survivor / Last Man Standing)\n")
cat("  Always R3=2 for tiebreaker edge.\n")
cat("  Maximize survival depth, then seed sum.\n")
cat("========================================================\n\n")

top_paths <- optimize_paths(champ_candidates = 8, late_candidates = 12)

cat("\n--- TOP 5 PATHS ---\n")
for (i in seq_len(min(5, length(top_paths)))) {
  print_path(top_paths[[i]]$path, sprintf("PATH #%d", i))
}

# Seed sum boost on #1
cat("--- SEED SUM MAXIMIZED (from Path #1) ---\n")
boosted <- maximize_seed_sum(top_paths[[1]]$path, prob_floor = 0.95)
print_path(boosted, "BOOSTED #1")

head_to_head(top_paths[[1]]$path, boosted, "Original #1", "Boosted #1")

if (length(top_paths) >= 2) {
  head_to_head(top_paths[[1]]$path, top_paths[[2]]$path, "Path #1", "Path #2")
}

cat("\n========================================================\n")
cat("  USAGE:\n")
cat("    my_path <- make_path(\n")
cat("      R1 = c(\"Team1\", \"Team2\", \"Team3\"),\n")
cat("      R2 = c(\"Team4\", \"Team5\", \"Team6\"),\n")
cat("      R3 = c(\"Team7\", \"Team8\"),\n")
cat("      R4 = \"Team9\", R5 = \"Team10\", R6 = \"Team11\"\n")
cat("    )\n")
cat("    print_path(my_path)\n")
cat("    head_to_head(my_path, top_paths[[1]]$path, \"Mine\", \"Optimal\")\n")
cat("    field_eval(my_path, list(p1, p2, p3), \"Mine\")\n")
cat("========================================================\n")
