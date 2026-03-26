// simulate_tourney.cpp
// Rcpp simulation engine for NCAA March Madness bracket
// Handles win probability, game simulation, rating updates, and bulk sims

#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
using namespace Rcpp;

// Logistic win probability
// Fitted via logistic regression on 315 NCAAT games (2021-2025):
//   logit(market_prob) = k * KenPom_AdjEM_diff
// k = 0.0917 (95% CI: [0.0886, 0.0949]), R² = 0.91
// See calibrate_win_prob.R for derivation
static const double LOG_SCALE = 0.0917;

double win_prob(double rating_a, double rating_b) {
  double diff = rating_a - rating_b;
  return 1.0 / (1.0 + std::exp(-LOG_SCALE * diff));
}

// Simulate a full 64-team bracket
// ratings:       length-64 vector of power ratings (0-indexed internally)
// bracket_order: length-64 vector of 1-indexed team IDs in bracket position order
// update_factor: base rating boost given to a winner after each game
// r1_win_probs:  length-32 vector of R1 win probabilities for the higher-seeded
//                team (team_a) in each game. Use empty vector to fall back to
//                rating-based logistic for all games.
//
// Returns: IntegerVector of length 63, the 1-indexed team ID of each game winner
//   Games 0-31:  Round of 64 (32 games)
//   Games 32-47: Round of 32 (16 games)
//   Games 48-55: Sweet 16    (8 games)
//   Games 56-59: Elite 8     (4 games)
//   Games 60-61: Final Four  (2 games)
//   Game  62:    Championship (1 game)
//
// [[Rcpp::export]]
IntegerVector simulate_bracket_cpp(NumericVector ratings,
                                   IntegerVector bracket_order,
                                   double update_factor,
                                   NumericVector r1_win_probs) {
  int n = bracket_order.size(); // 64
  bool has_r1_probs = (r1_win_probs.size() == 32);

  // Work with mutable copies of ratings
  NumericVector cur_ratings = clone(ratings);

  // Current round participants (0-indexed team IDs)
  std::vector<int> participants(n);
  for (int i = 0; i < n; i++) {
    participants[i] = bracket_order[i] - 1; // convert to 0-indexed
  }

  IntegerVector game_winners(63);
  int game_idx = 0;
  int round_size = n;
  bool first_round = true;

  // 6 rounds: 32 -> 16 -> 8 -> 4 -> 2 -> 1
  while (round_size > 1) {
    int n_games = round_size / 2;
    std::vector<int> next_round(n_games);

    for (int g = 0; g < n_games; g++) {
      int team_a = participants[2 * g];
      int team_b = participants[2 * g + 1];

      double p;
      if (first_round && has_r1_probs) {
        // Use market-derived R1 win probability for team_a
        p = r1_win_probs[g];
      } else {
        p = win_prob(cur_ratings[team_a], cur_ratings[team_b]);
      }
      double r = R::runif(0.0, 1.0);

      int winner, loser;
      if (r < p) {
        winner = team_a;
        loser  = team_b;
      } else {
        winner = team_b;
        loser  = team_a;
      }

      // Rating update: boost scales with how unlikely the win was
      // If winner had 90% chance, boost = update_factor * (1 - 0.9) = small
      // If winner had 10% chance, boost = update_factor * (1 - 0.1) = large
      double winner_prob = (winner == team_a) ? p : 1.0 - p;
      double boost = update_factor * (1.0 - winner_prob);
      cur_ratings[winner] += boost;

      next_round[g] = winner;
      game_winners[game_idx++] = winner + 1; // store as 1-indexed
    }

    participants = next_round;
    round_size = n_games;
    first_round = false;
  }

  return game_winners;
}

// Run many tournament simulations and store ALL results
//
// Returns a List with:
//   all_results          – IntegerMatrix (n_sims x 63), every game winner
//                          from every sim (1-indexed team IDs)
//   champ_counts         – how many times each team won the title
//   final_four_counts    – how many times each team reached the Final Four
//   elite_eight_counts   – how many times each team reached the Elite 8
//   sweet_sixteen_counts – how many times each team reached the Sweet 16
//   n_sims               – number of simulations run
//
// Game index layout per row (0-based columns):
//   0-31:  Round of 64  (32 games)
//   32-47: Round of 32  (16 games)
//   48-55: Sweet 16     (8 games)
//   56-59: Elite 8      (4 games)
//   60-61: Final Four   (2 games)
//   62:    Championship  (1 game)
//
// [[Rcpp::export]]
List run_tournament_sims(NumericVector ratings,
                         IntegerVector bracket_order,
                         int n_sims,
                         double update_factor,
                         NumericVector r1_win_probs) {
  int n_teams = ratings.size();

  // Full results matrix: every game of every sim
  IntegerMatrix all_results(n_sims, 63);

  // Aggregate counters (computed on the fly to avoid a second pass)
  IntegerVector champ_counts(n_teams, 0);
  IntegerVector final_four_counts(n_teams, 0);
  IntegerVector elite_eight_counts(n_teams, 0);
  IntegerVector sweet_sixteen_counts(n_teams, 0);

  for (int s = 0; s < n_sims; s++) {
    IntegerVector results = simulate_bracket_cpp(ratings, bracket_order,
                                                 update_factor, r1_win_probs);

    // Store full 63-game result row
    for (int g = 0; g < 63; g++) {
      all_results(s, g) = results[g];
    }

    // Champion = winner of game index 62 (the final)
    int champ = results[62] - 1; // 0-indexed
    champ_counts[champ]++;

    // Final Four = winners of Elite 8 games (indices 56-59)
    for (int g = 56; g <= 59; g++) {
      final_four_counts[results[g] - 1]++;
    }

    // Elite 8 = winners of Sweet 16 games (indices 48-55)
    for (int g = 48; g <= 55; g++) {
      elite_eight_counts[results[g] - 1]++;
    }

    // Sweet 16 = winners of Round of 32 games (indices 32-47)
    for (int g = 32; g <= 47; g++) {
      sweet_sixteen_counts[results[g] - 1]++;
    }
  }

  return List::create(
    Named("all_results")          = all_results,
    Named("champ_counts")         = champ_counts,
    Named("final_four_counts")    = final_four_counts,
    Named("elite_eight_counts")   = elite_eight_counts,
    Named("sweet_sixteen_counts") = sweet_sixteen_counts,
    Named("n_sims")               = n_sims
  );
}

// =============================================================================
// Locked-results tournament simulation
//
// Same as simulate_bracket_cpp but with locked_results: IntegerVector of
// length 63. If locked_results[game_idx] > 0, that team wins (skip RNG).
// If <= 0, simulate normally. Rating updates still apply to locked winners.
// =============================================================================

// Internal: simulate one bracket with locked results
IntegerVector simulate_bracket_locked(NumericVector ratings,
                                      IntegerVector bracket_order,
                                      double update_factor,
                                      NumericVector r1_win_probs,
                                      IntegerVector locked_results) {
  int n = bracket_order.size(); // 64
  bool has_r1_probs = (r1_win_probs.size() == 32);

  NumericVector cur_ratings = clone(ratings);

  std::vector<int> participants(n);
  for (int i = 0; i < n; i++) {
    participants[i] = bracket_order[i] - 1; // 0-indexed
  }

  IntegerVector game_winners(63);
  int game_idx = 0;
  int round_size = n;
  bool first_round = true;

  while (round_size > 1) {
    int n_games = round_size / 2;
    std::vector<int> next_round(n_games);

    for (int g = 0; g < n_games; g++) {
      int team_a = participants[2 * g];
      int team_b = participants[2 * g + 1];

      int winner, loser;

      // Check if this game is locked
      if (locked_results[game_idx] > 0) {
        int locked_team = locked_results[game_idx] - 1; // 0-indexed
        if (locked_team == team_a) {
          winner = team_a;
          loser = team_b;
        } else {
          winner = team_b;
          loser = team_a;
        }
      } else {
        // Simulate normally
        double p;
        if (first_round && has_r1_probs) {
          p = r1_win_probs[g];
        } else {
          p = win_prob(cur_ratings[team_a], cur_ratings[team_b]);
        }
        double r = R::runif(0.0, 1.0);
        if (r < p) {
          winner = team_a;
          loser = team_b;
        } else {
          winner = team_b;
          loser = team_a;
        }
      }

      // Rating update (applied even when locked, so downstream games use
      // updated ratings)
      double p_for_update = win_prob(cur_ratings[team_a], cur_ratings[team_b]);
      double winner_prob = (winner == team_a) ? p_for_update : 1.0 - p_for_update;
      double boost = update_factor * (1.0 - winner_prob);
      cur_ratings[winner] += boost;

      next_round[g] = winner;
      game_winners[game_idx++] = winner + 1; // 1-indexed
    }

    participants = next_round;
    round_size = n_games;
    first_round = false;
  }

  return game_winners;
}

// Run many tournament sims with locked results
//
// locked_results: IntegerVector of length 63.
//   > 0  = winner is locked (1-indexed team_id), skip RNG
//   <= 0 = simulate normally
//
// [[Rcpp::export]]
List run_tournament_sims_locked(NumericVector ratings,
                                IntegerVector bracket_order,
                                int n_sims,
                                double update_factor,
                                NumericVector r1_win_probs,
                                IntegerVector locked_results) {
  int n_teams = ratings.size();

  IntegerMatrix all_results(n_sims, 63);
  IntegerVector champ_counts(n_teams, 0);
  IntegerVector final_four_counts(n_teams, 0);
  IntegerVector elite_eight_counts(n_teams, 0);
  IntegerVector sweet_sixteen_counts(n_teams, 0);

  for (int s = 0; s < n_sims; s++) {
    IntegerVector results = simulate_bracket_locked(ratings, bracket_order,
                                                     update_factor, r1_win_probs,
                                                     locked_results);

    for (int g = 0; g < 63; g++) {
      all_results(s, g) = results[g];
    }

    int champ = results[62] - 1;
    champ_counts[champ]++;

    for (int g = 56; g <= 59; g++) {
      final_four_counts[results[g] - 1]++;
    }
    for (int g = 48; g <= 55; g++) {
      elite_eight_counts[results[g] - 1]++;
    }
    for (int g = 32; g <= 47; g++) {
      sweet_sixteen_counts[results[g] - 1]++;
    }
  }

  return List::create(
    Named("all_results")          = all_results,
    Named("champ_counts")         = champ_counts,
    Named("final_four_counts")    = final_four_counts,
    Named("elite_eight_counts")   = elite_eight_counts,
    Named("sweet_sixteen_counts") = sweet_sixteen_counts,
    Named("n_sims")               = n_sims
  );
}

// =============================================================================
// Field survival simulation (C++ hot loop)
//
// Simulates how field entry groups survive through remaining tournament slots.
// Each group has a pre-computed probability distribution over candidate teams
// per slot. Within a sim, picks are sampled, checked against sim results, and
// dynamically excluded from future slots.
//
// Parameters:
//   all_results      n_sims x 63 IntegerMatrix, 1-indexed team IDs
//   group_used       n_groups x max_used IntegerMatrix, 1-indexed team IDs,
//                    0-padded (teams already picked in prior real slots)
//   group_sizes      n_groups IntegerVector (weight of each group)
//   slot_team_ids    List of n_slots IntegerVectors: candidate team IDs per slot
//   slot_game_cols   List of n_slots IntegerVectors: 0-indexed game column in
//                    all_results for each candidate (parallel to slot_team_ids)
//   group_pick_probs List of n_slots NumericMatrices: n_groups x n_candidates
//                    pick probability for each group for each candidate team.
//                    Pre-computed in R accounting for known used teams.
//   n_slots          Number of remaining slots to simulate
//
// Returns:
//   IntegerMatrix (n_sims x n_groups): death slot (1-indexed), or
//   n_slots+1 if the group survived all slots.
// =============================================================================

// [[Rcpp::export]]
IntegerMatrix simulate_field_survival_cpp(
    IntegerMatrix all_results,
    IntegerMatrix group_used,
    IntegerVector group_sizes,
    List slot_team_ids,
    List slot_game_cols,
    List group_pick_probs,
    int n_slots) {

  int n_sims = all_results.nrow();
  int n_groups = group_used.nrow();
  int max_used = group_used.ncol();

  // Pre-extract slot data for fast access
  std::vector<IntegerVector> s_team_ids(n_slots);
  std::vector<IntegerVector> s_game_cols(n_slots);
  std::vector<NumericMatrix> s_pick_probs(n_slots);
  std::vector<int> s_n_candidates(n_slots);

  for (int s = 0; s < n_slots; s++) {
    s_team_ids[s]    = as<IntegerVector>(slot_team_ids[s]);
    s_game_cols[s]   = as<IntegerVector>(slot_game_cols[s]);
    s_pick_probs[s]  = as<NumericMatrix>(group_pick_probs[s]);
    s_n_candidates[s] = s_team_ids[s].size();
  }

  // Output: death slot per sim per group
  IntegerMatrix death_slot(n_sims, n_groups);

  // Temporary buffers (reused per sim×group to avoid allocation)
  std::vector<double> adj_probs;
  std::vector<int> sim_used;

  for (int sim = 0; sim < n_sims; sim++) {
    // Check for user interrupt periodically
    if (sim % 10000 == 0) Rcpp::checkUserInterrupt();

    for (int grp = 0; grp < n_groups; grp++) {

      // Initialize sim_used from group's known used teams
      sim_used.clear();
      for (int u = 0; u < max_used; u++) {
        int tid = group_used(grp, u);
        if (tid == 0) break; // 0-padded
        sim_used.push_back(tid);
      }

      int death = n_slots + 1; // survived all by default

      for (int slot = 0; slot < n_slots; slot++) {
        int n_cand = s_n_candidates[slot];
        NumericMatrix& probs = s_pick_probs[slot];

        // Build adjusted probabilities: zero out already-used teams
        adj_probs.resize(n_cand);
        double prob_sum = 0.0;

        for (int c = 0; c < n_cand; c++) {
          int tid = s_team_ids[slot][c];
          double p = probs(grp, c);

          // Check if this team is in sim_used
          bool used = false;
          for (size_t u = 0; u < sim_used.size(); u++) {
            if (sim_used[u] == tid) { used = true; break; }
          }

          if (used) {
            adj_probs[c] = 0.0;
          } else {
            adj_probs[c] = p;
            prob_sum += p;
          }
        }

        // Dead end: no available teams
        if (prob_sum <= 0.0) {
          death = slot + 1;
          break;
        }

        // Sample team from adjusted distribution
        double draw = R::runif(0.0, 1.0) * prob_sum;
        double cumul = 0.0;
        int picked_idx = n_cand - 1; // fallback to last

        for (int c = 0; c < n_cand; c++) {
          cumul += adj_probs[c];
          if (draw <= cumul) {
            picked_idx = c;
            break;
          }
        }

        int picked_team = s_team_ids[slot][picked_idx];
        int game_col = s_game_cols[slot][picked_idx];

        // Check if picked team won in this sim
        int sim_winner = all_results(sim, game_col);
        if (sim_winner != picked_team) {
          death = slot + 1;
          break;
        }

        // Team survived — mark as used for future slots
        sim_used.push_back(picked_team);
      }

      death_slot(sim, grp) = death;
    }
  }

  return death_slot;
}

// =============================================================================
// Forward-knowledge Monte Carlo path solver
//
// For each sim × each first-slot candidate, find the longest survivable path
// through remaining slots using full knowledge of sim outcomes (all_results).
// Enforces used_teams exclusion and bracket compatibility (earliest_meeting_round).
//
// This replaces the beam search EV computation with exact per-sim optimal paths.
// The tree is small (~5 remaining slots × ~8 candidates each = hundreds of
// nodes max), so brute-force DFS is fast even across 50K+ sims.
//
// Parameters:
//   all_results         n_sims x 63 IntegerMatrix, 1-indexed team IDs
//   n_field_alive       n_sims x n_slots NumericMatrix: expected # of field
//                       entries alive after each slot (from entry-level field sim)
//   slot_team_ids       List of n_slots IntegerVectors: candidate team IDs per slot
//   slot_game_cols      List of n_slots IntegerVectors: 0-indexed game column for
//                       each candidate (parallel to slot_team_ids)
//   slot_n_picks        IntegerVector length n_slots: how many picks per slot
//                       (1 for most; 2 for E8 combined)
//   used_teams          IntegerVector: 1-indexed team IDs already picked
//   first_slot_cands    IntegerVector: candidate team IDs for slot 0 (today)
//   first_slot_gcols    IntegerVector: 0-indexed game columns for slot 0 candidates
//   prize_pool          double: contest prize pool
//   n_slots             int: number of remaining slots
//
// Returns:
//   List with:
//     ev          NumericVector(n_first_cands): mean EV per first-slot candidate
//     p_survive   NumericVector(n_first_cands): P(survive all slots)
//     mean_death  NumericVector(n_first_cands): mean death slot (0 = survived all)
// =============================================================================

// Bracket compatibility: earliest round two teams could meet
// team IDs are 1-indexed bracket positions (1-64)
static int earliest_meeting_round_cpp(int t1, int t2) {
  int p1 = t1 - 1;  // 0-indexed
  int p2 = t2 - 1;
  for (int r = 1; r <= 6; r++) {
    if ((p1 >> r) == (p2 >> r)) return r;
  }
  return 7;
}

// Get region (1-4) from team_id (1-64)
static int get_region_cpp(int team_id) {
  return ((team_id - 1) / 16) + 1;
}

// Check bracket compatibility: can we add candidate_id at candidate_round
// given existing path_teams at their respective rounds?
// Also enforces: E8+ picks must be from distinct regions, E8 pair must be
// from opposite FF sides.
static bool is_compat_cpp(int candidate_id, int candidate_round,
                          const std::vector<int>& path_teams,
                          const std::vector<int>& path_rounds) {
  // Pairwise bracket check
  for (size_t i = 0; i < path_teams.size(); i++) {
    int m = earliest_meeting_round_cpp(path_teams[i], candidate_id);
    int min_rd = std::min(path_rounds[i], candidate_round);
    if (m <= min_rd) return false;
  }

  // Late-round region constraint (E8+ picks: rounds 4, 5, 6)
  if (candidate_round >= 4) {
    int cand_region = get_region_cpp(candidate_id);
    int cand_side = (cand_region <= 2) ? 1 : 2;

    for (size_t i = 0; i < path_teams.size(); i++) {
      if (path_rounds[i] >= 4) {
        int used_region = get_region_cpp(path_teams[i]);
        // Must be different region from all late-round picks
        if (used_region == cand_region) return false;

        // E8 picks must be from opposite FF sides
        if (candidate_round == 4 && path_rounds[i] == 4) {
          int used_side = (used_region <= 2) ? 1 : 2;
          if (cand_side == used_side) return false;
        }
      }
    }
  }

  return true;
}

// DFS: find the longest survivable path from slot_idx onward.
// Returns the best death_slot: n_slots if survived all, else the 0-indexed
// slot where we first die.
// best_path is populated with the team picked at each slot along the best path.
static int dfs_best_path(
    int slot_idx, int n_slots, int sim,
    const IntegerMatrix& all_results,
    const std::vector<IntegerVector>& s_team_ids,
    const std::vector<IntegerVector>& s_game_cols,
    const std::vector<int>& s_n_picks,
    const std::vector<int>& s_round_nums,
    std::vector<int>& used_teams,
    std::vector<int>& bracket_teams,
    std::vector<int>& bracket_rounds) {

  if (slot_idx >= n_slots) {
    return n_slots;  // survived all
  }

  int rd = s_round_nums[slot_idx];
  int n_picks = s_n_picks[slot_idx];
  int n_cand = s_team_ids[slot_idx].size();

  int best_depth = slot_idx;  // worst case: die at this slot

  if (n_picks == 1) {
    // Single-pick slot: try each candidate
    for (int c = 0; c < n_cand; c++) {
      int tid = s_team_ids[slot_idx][c];
      int gcol = s_game_cols[slot_idx][c];

      // Skip if already used
      bool is_used = false;
      for (size_t u = 0; u < used_teams.size(); u++) {
        if (used_teams[u] == tid) { is_used = true; break; }
      }
      if (is_used) continue;

      // Check if this team won in this sim
      if (all_results(sim, gcol) != tid) continue;

      // Check bracket compatibility
      if (!is_compat_cpp(tid, rd, bracket_teams, bracket_rounds)) continue;

      // Recurse
      used_teams.push_back(tid);
      bracket_teams.push_back(tid);
      bracket_rounds.push_back(rd);

      int depth = dfs_best_path(slot_idx + 1, n_slots, sim, all_results,
                                s_team_ids, s_game_cols, s_n_picks,
                                s_round_nums, used_teams,
                                bracket_teams, bracket_rounds);

      used_teams.pop_back();
      bracket_teams.pop_back();
      bracket_rounds.pop_back();

      if (depth > best_depth) {
        best_depth = depth;
        if (best_depth == n_slots) return n_slots;  // can't do better
      }
    }
  } else {
    // Multi-pick slot (e.g., E8 combined: 2 picks)
    // Try all valid pairs of candidates
    for (int c1 = 0; c1 < n_cand; c1++) {
      int tid1 = s_team_ids[slot_idx][c1];
      int gcol1 = s_game_cols[slot_idx][c1];

      // Skip if used or didn't win
      bool is_used1 = false;
      for (size_t u = 0; u < used_teams.size(); u++) {
        if (used_teams[u] == tid1) { is_used1 = true; break; }
      }
      if (is_used1) continue;
      if (all_results(sim, gcol1) != tid1) continue;
      if (!is_compat_cpp(tid1, rd, bracket_teams, bracket_rounds)) continue;

      // Add first pick to state
      used_teams.push_back(tid1);
      bracket_teams.push_back(tid1);
      bracket_rounds.push_back(rd);

      for (int c2 = c1 + 1; c2 < n_cand; c2++) {
        int tid2 = s_team_ids[slot_idx][c2];
        int gcol2 = s_game_cols[slot_idx][c2];

        bool is_used2 = false;
        for (size_t u = 0; u < used_teams.size(); u++) {
          if (used_teams[u] == tid2) { is_used2 = true; break; }
        }
        if (is_used2) continue;
        if (all_results(sim, gcol2) != tid2) continue;
        if (!is_compat_cpp(tid2, rd, bracket_teams, bracket_rounds)) continue;

        // Both picks are valid winners — recurse
        used_teams.push_back(tid2);
        bracket_teams.push_back(tid2);
        bracket_rounds.push_back(rd);

        int depth = dfs_best_path(slot_idx + 1, n_slots, sim, all_results,
                                  s_team_ids, s_game_cols, s_n_picks,
                                  s_round_nums, used_teams,
                                  bracket_teams, bracket_rounds);

        used_teams.pop_back();
        bracket_teams.pop_back();
        bracket_rounds.pop_back();

        if (depth > best_depth) {
          best_depth = depth;
          if (best_depth == n_slots) {
            // Undo first pick and return
            used_teams.pop_back();
            bracket_teams.pop_back();
            bracket_rounds.pop_back();
            return n_slots;
          }
        }
      }

      // Also try with just one pick in a 2-pick slot
      // (maybe only one candidate won, still valid to survive this slot
      //  as long as we got n_picks winners... actually no: if the slot
      //  requires 2 picks, we must find 2 winners. Try single pick too
      //  in case no valid pair exists — better to survive with 1 than die.)
      // Actually in Splash, a multi-pick slot means you MUST make n_picks
      // selections. If you can't field 2, you're forced to pick a loser
      // and you die. So single-pick survival is NOT valid for n_picks=2.
      // But wait — the first pick already survived. If no second valid
      // winner exists, we die at this slot. The loop above covers all pairs.
      // If no pair was found, best_depth stays at slot_idx (die here).

      // Undo first pick
      used_teams.pop_back();
      bracket_teams.pop_back();
      bracket_rounds.pop_back();

      if (best_depth == n_slots) return n_slots;
    }
  }

  return best_depth;
}

// [[Rcpp::export]]
List solve_optimal_paths_cpp(
    IntegerMatrix all_results,
    NumericMatrix n_field_alive,
    List slot_team_ids_list,
    List slot_game_cols_list,
    IntegerVector slot_n_picks,
    IntegerVector slot_round_nums,
    IntegerVector used_teams_vec,
    IntegerVector first_slot_cands,
    IntegerVector first_slot_gcols,
    double prize_pool,
    int n_slots) {

  int n_sims = all_results.nrow();
  int n_first = first_slot_cands.size();
  int first_round = slot_round_nums[0];

  // Pre-extract slot data (slots 1..n_slots-1 are future slots)
  std::vector<IntegerVector> s_team_ids(n_slots);
  std::vector<IntegerVector> s_game_cols(n_slots);
  std::vector<int> s_n_picks(n_slots);
  std::vector<int> s_round_nums(n_slots);

  for (int s = 0; s < n_slots; s++) {
    s_team_ids[s] = as<IntegerVector>(slot_team_ids_list[s]);
    s_game_cols[s] = as<IntegerVector>(slot_game_cols_list[s]);
    s_n_picks[s] = slot_n_picks[s];
    s_round_nums[s] = slot_round_nums[s];
  }

  // Pre-extract used teams
  std::vector<int> base_used;
  for (int i = 0; i < used_teams_vec.size(); i++) {
    if (used_teams_vec[i] > 0) base_used.push_back(used_teams_vec[i]);
  }

  // Build base bracket_teams/bracket_rounds from used_teams
  // We don't know the exact round for historical picks from R, so we
  // infer: team_id -> its earliest possible round based on bracket position.
  // Actually, we need this from R. For now, we just use the used_teams
  // for the "is already used" check, and bracket compatibility is checked
  // only for new picks against each other.
  // The R wrapper will pass bracket_teams and bracket_rounds separately
  // if needed. For now, we skip bracket compat for historical picks
  // (they've already been validated).
  std::vector<int> base_bracket_teams;
  std::vector<int> base_bracket_rounds;

  // Output accumulators
  NumericVector ev_out(n_first, 0.0);
  NumericVector p_survive_out(n_first, 0.0);
  NumericVector mean_death_out(n_first, 0.0);

  // Per-candidate diagnostics
  NumericVector sum_n_alive_when_survived(n_first, 0.0);
  NumericVector sum_payout_when_survived(n_first, 0.0);
  NumericVector max_payout_out(n_first, 0.0);
  IntegerVector die_with_zero_alive(n_first, 0);  // died but got payout (n_alive_after==0)

  // Per-sim death rounds: 0 = survived all, round_num (1-6) = died in that round
  IntegerMatrix death_rounds_out(n_first, n_sims);

  for (int ci = 0; ci < n_first; ci++) {
    int cand_tid = first_slot_cands[ci];
    int cand_gcol = first_slot_gcols[ci];

    double ev_sum = 0.0;
    int survive_count = 0;
    double death_sum = 0.0;

    for (int sim = 0; sim < n_sims; sim++) {
      if (sim % 50000 == 0 && ci == 0) Rcpp::checkUserInterrupt();

      // Check if candidate won in this sim
      if (all_results(sim, cand_gcol) != cand_tid) {
        // Candidate lost in slot 0 — field survivors remain, payout = $0
        death_sum += 1.0;
        death_rounds_out(ci, sim) = s_round_nums[0];
        continue;
      }

      // Candidate won slot 0 — DFS for best path from slot 1 onward
      std::vector<int> cur_used = base_used;
      cur_used.push_back(cand_tid);

      std::vector<int> cur_bracket = base_bracket_teams;
      cur_bracket.push_back(cand_tid);
      std::vector<int> cur_bracket_rds = base_bracket_rounds;
      cur_bracket_rds.push_back(first_round);

      int best_depth = dfs_best_path(
        1, n_slots, sim, all_results,
        s_team_ids, s_game_cols, s_n_picks, s_round_nums,
        cur_used, cur_bracket, cur_bracket_rds
      );

      // Compute payout: integer field alive counts, simple split
      double payout;
      if (best_depth == n_slots) {
        // Survived all slots — split prize with surviving field entries
        int n_alive_final = (int)std::round(n_field_alive(sim, n_slots - 1));
        payout = prize_pool / (1.0 + n_alive_final);
        survive_count++;
        sum_n_alive_when_survived[ci] += n_alive_final;
        sum_payout_when_survived[ci] += payout;
        death_rounds_out(ci, sim) = 0;  // survived all
      } else {
        // Died at slot best_depth
        // If anyone survives past us, we get $0
        // If nobody survives past us, split with others dying at same slot
        int n_alive_after = (int)std::round(n_field_alive(sim, best_depth));
        if (n_alive_after > 0) {
          // Field entries outlast us — $0
          payout = 0.0;
        } else {
          // Nobody outlasts us — split with field entries dying at this slot
          int n_alive_before;
          if (best_depth == 0) {
            n_alive_before = (int)std::round(n_field_alive(sim, 0));
          } else {
            n_alive_before = (int)std::round(n_field_alive(sim, best_depth - 1));
          }
          int n_dying = std::max(n_alive_before - n_alive_after, 0);
          payout = prize_pool / (1.0 + n_dying);
          die_with_zero_alive[ci]++;
        }
        death_sum += (best_depth + 1.0);
        death_rounds_out(ci, sim) = s_round_nums[best_depth];
      }

      if (payout > max_payout_out[ci]) max_payout_out[ci] = payout;
      ev_sum += payout;
    }

    ev_out[ci] = ev_sum / n_sims;
    p_survive_out[ci] = (double)survive_count / n_sims;
    mean_death_out[ci] = death_sum / n_sims;
  }

  return List::create(
    Named("ev")           = ev_out,
    Named("p_survive")    = p_survive_out,
    Named("mean_death")   = mean_death_out,
    Named("death_rounds") = death_rounds_out,
    Named("mean_n_alive_when_survived") = sum_n_alive_when_survived / Rcpp::pmax(p_survive_out * n_sims, 1.0),
    Named("mean_payout_when_survived")  = sum_payout_when_survived / Rcpp::pmax(p_survive_out * n_sims, 1.0),
    Named("max_payout")   = max_payout_out,
    Named("die_with_zero_alive") = die_with_zero_alive
  );
}
