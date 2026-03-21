// simulate_tourney.cpp
// Rcpp simulation engine for NCAA March Madness bracket
// Handles win probability, game simulation, rating updates, and bulk sims

#include <Rcpp.h>
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

// Pace-adjusted win probability
// Scales the per-100-possessions rating diff by expected game tempo
// game_tempo = (tempo_a * tempo_b) / avg_tempo
// pace_factor = game_tempo / avg_tempo  (>1 for fast games, <1 for slow)
double win_prob_pace(double rating_a, double rating_b,
                     double tempo_a, double tempo_b, double avg_tempo) {
  double game_tempo = (tempo_a * tempo_b) / avg_tempo;
  double diff = rating_a - rating_b;
  double pace_factor = game_tempo / avg_tempo;
  return 1.0 / (1.0 + std::exp(-LOG_SCALE * diff * pace_factor));
}

// Simulate a full 64-team bracket
// ratings:       length-64 vector of power ratings (0-indexed internally)
// tempos:        length-64 vector of adjusted tempos (0-indexed internally)
// avg_tempo:     D1 average tempo for pace normalization
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
                                   NumericVector tempos,
                                   double avg_tempo,
                                   IntegerVector bracket_order,
                                   double update_factor,
                                   NumericVector r1_win_probs,
                                   NumericVector r2_win_probs) {
  int n = bracket_order.size(); // 64
  bool has_r1_probs = (r1_win_probs.size() == 32);
  bool has_r2_probs = (r2_win_probs.size() == 16);
  bool has_tempos = (tempos.size() == n);

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
  int round_num = 1;  // 1=R64, 2=R32, 3=S16, ...

  // 6 rounds: 32 -> 16 -> 8 -> 4 -> 2 -> 1
  while (round_size > 1) {
    int n_games = round_size / 2;
    std::vector<int> next_round(n_games);

    for (int g = 0; g < n_games; g++) {
      int team_a = participants[2 * g];
      int team_b = participants[2 * g + 1];

      double p;
      if (round_num == 1 && has_r1_probs) {
        // Use market-derived or locked-in R1 win probability for team_a
        p = r1_win_probs[g];
      } else if (round_num == 2 && has_r2_probs && r2_win_probs[g] >= 0.0) {
        // Use closing line R2 win probability (negative = not available, use model)
        p = r2_win_probs[g];
      } else if (has_tempos) {
        // Pace-adjusted win probability
        p = win_prob_pace(cur_ratings[team_a], cur_ratings[team_b],
                          tempos[team_a], tempos[team_b], avg_tempo);
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
    round_num++;
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
                         NumericVector tempos,
                         double avg_tempo,
                         IntegerVector bracket_order,
                         int n_sims,
                         double update_factor,
                         NumericVector r1_win_probs,
                         NumericVector r2_win_probs) {
  int n_teams = ratings.size();

  // Full results matrix: every game of every sim
  IntegerMatrix all_results(n_sims, 63);

  // Aggregate counters (computed on the fly to avoid a second pass)
  IntegerVector champ_counts(n_teams, 0);
  IntegerVector final_four_counts(n_teams, 0);
  IntegerVector elite_eight_counts(n_teams, 0);
  IntegerVector sweet_sixteen_counts(n_teams, 0);

  for (int s = 0; s < n_sims; s++) {
    IntegerVector results = simulate_bracket_cpp(ratings, tempos, avg_tempo,
                                                 bracket_order,
                                                 update_factor, r1_win_probs,
                                                 r2_win_probs);

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
