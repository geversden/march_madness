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
