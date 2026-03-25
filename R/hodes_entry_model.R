#!/usr/bin/env Rscript
# ==============================================================================
# hodes_entry_model.R
#
# Models for Hodes contest entry behavior:
#   1. Pick Count Model (S16): logistic regression for P(take_two)
#   2. Team Selection Model (S16 and E8): softmax MLE
#
# Entry points:
#   fit_hodes_pick_count_model(s16_count_calib) -> glm object
#   predict_hodes_s16_count(model, n_alive, tb_rank_pct, n_available_s16) -> P(take_two)
#   fit_hodes_team_selection(calib_data, round_name) -> list of params
#   predict_hodes_pick(used_teams, candidates_df, n_picks, params) -> named prob vector
#   calibrate_hodes_model(all_data) -> list of all fitted params
# ==============================================================================

cat("Loading hodes_entry_model.R...\n")

# ==============================================================================
# PICK COUNT MODEL (S16)
# ==============================================================================

#' Fit logistic regression for S16 pick count (1 vs 2).
#'
#' logit(P(take_two)) = alpha + gamma_alive * log(n_alive) + gamma_tb * tb_rank_pct
#'                      + gamma_avail * n_available_s16
#'
#' @param s16_count_calib data.frame with: took_two, n_alive, tb_rank_pct, n_available_s16
#' @return glm model object
fit_hodes_pick_count_model <- function(s16_count_calib) {
  cat("\n--- Fitting S16 Pick Count Model ---\n")

  df <- s16_count_calib
  df$log_n_alive <- log(df$n_alive)

  model <- glm(took_two ~ log_n_alive + tb_rank_pct + n_available_s16,
               data = df, family = binomial(link = "logit"))

  cat("Coefficients:\n")
  print(coef(model))
  cat(sprintf("Deviance: %.2f  |  AIC: %.2f  |  N: %d\n",
              deviance(model), AIC(model), nrow(df)))

  model
}


#' Predict P(take_two) for given state.
#'
#' @param model glm object from fit_hodes_pick_count_model
#' @param n_alive Integer number of entries alive at S16
#' @param tb_rank_pct Numeric tiebreaker rank percentile (0-1)
#' @param n_available_s16 Integer number of unused S16 teams available
#' @return Numeric P(take_two)
predict_hodes_s16_count <- function(model, n_alive, tb_rank_pct, n_available_s16) {
  newdata <- data.frame(
    log_n_alive     = log(n_alive),
    tb_rank_pct     = tb_rank_pct,
    n_available_s16 = n_available_s16
  )
  as.numeric(predict(model, newdata = newdata, type = "response"))
}


# ==============================================================================
# FV_FRACTION COMPUTATION
# ==============================================================================

#' Add fv_fraction column to calibration data.
#'
#' For each entry, fv_fraction(t) = champ_equity(t) / remaining_fv
#' where remaining_fv = sum of champ_equity for all candidates (teams available
#' to this entry at this round, i.e., not in used_teams).
#'
#' Since the calib data already contains only candidates NOT in used_teams,
#' remaining_fv = sum of champ_equity for all candidates for that entry.
#'
#' @param calib_data data.frame with year, entry_id, champ_equity columns
#' @return calib_data with fv_fraction column added
.add_fv_fraction <- function(calib_data) {
  if (nrow(calib_data) == 0) return(calib_data)

  # Compute remaining_fv per entry (sum of champ_equity across all candidates)
  entry_key <- paste(calib_data$year, calib_data$entry_id, sep = "_")
  remaining_fv <- tapply(calib_data$champ_equity, entry_key, sum, na.rm = TRUE)

  calib_data$fv_fraction <- calib_data$champ_equity /
    pmax(remaining_fv[entry_key], 1e-10)

  calib_data
}


# ==============================================================================
# TEAM SELECTION MODEL - SOFTMAX MLE
# ==============================================================================
#
# score(t, U) = beta_wp * wp(t) - beta_save * fv_fraction(t, U)
#               + beta_path * I(opponent_is_used) + beta_seed * seed_tb_value(t)
#               - beta_oneseed * I(seed == 1)
#
# seed_tb_value(t) = seed / 16 (normalized: 16-seed = 1.0, 1-seed = 0.0625)
# beta_oneseed captures the field's reluctance to burn 1-seeds in S16
# (beyond what CE/fv_fraction already explains)
#
# For N=1: standard softmax P(t) = exp(score(t)) / sum(exp(score(j)))
# For N=2: enumerate all valid pairs (different games), softmax over pairs

#' Compute scores for all candidates of a single entry.
#'
#' @param wp Numeric vector of win probabilities
#' @param fv_fraction Numeric vector of fv_fraction values
#' @param opponent_is_used Integer vector (0/1)
#' @param seed Integer vector of seeds
#' @param params Named list: beta_wp, beta_save, beta_path, beta_seed, beta_oneseed
#' @param round_name "S16" or "E8" — controls save feature:
#'   S16: seed-prestige save (field saves by seed heuristic)
#'   E8: fv_fraction save (field more constrained, CE-aware)
#' @return Numeric vector of scores
.compute_scores <- function(wp, fv_fraction, opponent_is_used, seed, params,
                            round_name = "S16") {
  # Handle NA win probs by using median of non-NA
  wp_clean <- wp
  wp_clean[is.na(wp_clean)] <- median(wp, na.rm = TRUE)
  # If all NA, use 0.5
  wp_clean[is.na(wp_clean)] <- 0.5

  seed_tb_value <- seed / 16

  beta_oneseed <- if (!is.null(params$beta_oneseed)) params$beta_oneseed else 0

  if (round_name == "S16") {
    # S16: field saves based on seed prestige, not CE
    # seed_prestige: 1-seed = 1.0, 16-seed ≈ 0.06
    seed_prestige <- (17 - seed) / 16
    save_term <- params$beta_save * seed_prestige
  } else {
    # E8: use CE-based fv_fraction
    save_term <- params$beta_save * fv_fraction
  }

  beta_wp2 <- if (!is.null(params$beta_wp2)) params$beta_wp2 else 0

  # Use log(wp) instead of wp — creates steeper dropoff at low WP
  # log(0.8) = -0.22, log(0.5) = -0.69, log(0.2) = -1.61
  # The gap between 0.5 and 0.2 is much larger than between 0.8 and 0.5
  log_wp <- log(pmax(wp_clean, 0.01))

  params$beta_wp * log_wp +
    beta_wp2 * wp_clean^2 -
    save_term +
    params$beta_path * opponent_is_used +
    params$beta_seed * seed_tb_value -
    beta_oneseed * as.numeric(seed == 1)
}


#' Log-softmax for numerical stability.
#' @param scores Numeric vector
#' @return Numeric vector of log-probabilities
.log_softmax <- function(scores) {
  max_s <- max(scores)
  shifted <- scores - max_s
  log_sum_exp <- max_s + log(sum(exp(shifted)))
  scores - log_sum_exp
}


#' Compute NLL for team selection model on calibration data.
#'
#' @param beta Numeric vector c(beta_wp, beta_save, beta_path, beta_seed, beta_oneseed, beta_wp2)
#' @param calib_data data.frame with fv_fraction already added
#' @param round_name "S16" or "E8"
#' @return Scalar NLL
.team_selection_nll <- function(beta, calib_data, round_name) {
  params <- list(
    beta_wp      = beta[1],
    beta_save    = beta[2],
    beta_path    = beta[3],
    beta_seed    = beta[4],
    beta_oneseed = beta[5],
    beta_wp2     = beta[6]
  )

  # Group by entry
  entry_key <- paste(calib_data$year, calib_data$entry_id, sep = "_")
  entries <- unique(entry_key)

  total_nll <- 0

  for (ek in entries) {
    idx <- which(entry_key == ek)
    edata <- calib_data[idx, ]

    n_picks <- edata$n_picks[1]
    n_cands <- nrow(edata)

    # Skip degenerate cases
    if (n_cands == 0) next
    if (n_cands == 1) next  # deterministic, contributes 0 to NLL

    scores <- .compute_scores(
      edata$wp, edata$fv_fraction, edata$opponent_is_used, edata$seed, params,
      round_name = round_name
    )

    picked_idx <- which(edata$was_picked == 1)
    if (length(picked_idx) == 0) next

    if (n_picks == 1 || round_name == "E8") {
      # Standard softmax: -log P(picked team)
      if (length(picked_idx) != 1) {
        # Multiple picks flagged but round is E8 - take first
        picked_idx <- picked_idx[1]
      }
      log_probs <- .log_softmax(scores)
      total_nll <- total_nll - log_probs[picked_idx]

    } else if (n_picks == 2) {
      # Enumerate all valid pairs (from different S16 matchups)
      # We need to identify which S16 game each candidate belongs to
      # Use candidate_id to determine game: S16 game = ceiling(candidate_id / 8)
      # Actually, more precise: the S16 game for a team_id is ceiling(team_id / 8)
      # because each S16 game covers 8 team_ids (a quarter-region)
      game_idx <- ceiling(edata$candidate_id / 8)

      # Generate all valid pairs
      pair_scores <- c()
      pair_indices <- list()
      pi <- 0

      for (a in 1:(n_cands - 1)) {
        for (b in (a + 1):n_cands) {
          if (game_idx[a] != game_idx[b]) {
            # Valid pair: different games
            # Score = score(t1) + score(t2, U union {t1})
            # For the second pick, fv_fraction changes because t1 is now "used"
            # But for simplicity in calibration, we use the original fv_fraction
            # since the second pick's fv_fraction adjustment is small
            ps <- scores[a] + scores[b]
            pi <- pi + 1
            pair_scores <- c(pair_scores, ps)
            pair_indices[[pi]] <- c(a, b)
          }
        }
      }

      if (length(pair_scores) == 0) next

      # Find which pair matches the actual picks
      actual_set <- sort(picked_idx)
      found <- FALSE
      for (k in seq_along(pair_indices)) {
        if (identical(sort(pair_indices[[k]]), actual_set)) {
          # Log-softmax over pairs
          log_probs <- .log_softmax(pair_scores)
          total_nll <- total_nll - log_probs[k]
          found <- TRUE
          break
        }
      }

      if (!found) {
        # Actual pair not in valid pairs (same game?) -- fall back to marginals
        # This shouldn't happen often; treat each pick independently
        for (pi_idx in picked_idx) {
          log_probs <- .log_softmax(scores)
          total_nll <- total_nll - log_probs[pi_idx]
        }
      }
    }
  }

  total_nll
}


#' Fit team selection model via optim with multiple restarts.
#'
#' @param calib_data data.frame from analyze_hodes_historical (s16_team_calib or e8_team_calib)
#' @param round_name "S16" or "E8"
#' @return Named list: beta_wp, beta_save, beta_path, beta_seed
fit_hodes_team_selection <- function(calib_data, round_name = "S16") {
  cat(sprintf("\n--- Fitting %s Team Selection Model ---\n", round_name))

  # Add fv_fraction
  calib_data <- .add_fv_fraction(calib_data)

  # Remove entries with no picked team (edge case)
  entry_key <- paste(calib_data$year, calib_data$entry_id, sep = "_")
  has_pick <- tapply(calib_data$was_picked, entry_key, sum, na.rm = TRUE)
  valid_entries <- names(has_pick)[has_pick > 0]
  calib_data <- calib_data[entry_key %in% valid_entries, ]

  cat(sprintf("  Calibration data: %d candidate rows, %d entries\n",
              nrow(calib_data), length(valid_entries)))

  # Bounds: 6 parameters
  lower <- c(0.01, 0, -5, -5, 0, 0)
  upper <- c(30, 30, 15, 15, 15, 30)

  # Multiple random restarts
  n_restarts <- 8
  best_nll <- Inf
  best_beta <- NULL

  set.seed(42)
  for (restart in 1:n_restarts) {
    # Random starting point
    init <- c(
      runif(1, 0.5, 10),   # beta_wp
      runif(1, 0, 5),       # beta_save
      runif(1, -2, 5),      # beta_path
      runif(1, -2, 5),      # beta_seed
      runif(1, 0, 3),       # beta_oneseed
      runif(1, 0, 5)        # beta_wp2
    )

    result <- tryCatch(
      optim(init, .team_selection_nll,
            calib_data = calib_data, round_name = round_name,
            method = "L-BFGS-B",
            lower = lower, upper = upper,
            control = list(maxit = 500)),
      error = function(e) {
        cat(sprintf("  Restart %d failed: %s\n", restart, conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(result) && result$value < best_nll) {
      best_nll <- result$value
      best_beta <- result$par
      cat(sprintf("  Restart %d: NLL=%.4f [best so far]\n", restart, result$value))
    } else if (!is.null(result)) {
      cat(sprintf("  Restart %d: NLL=%.4f\n", restart, result$value))
    }
  }

  if (is.null(best_beta)) {
    warning(sprintf("All optimization restarts failed for %s model", round_name))
    best_beta <- c(1, 0, 0, 0)
    best_nll <- NA
  }

  params <- list(
    beta_wp      = best_beta[1],
    beta_save    = best_beta[2],
    beta_path    = best_beta[3],
    beta_seed    = best_beta[4],
    beta_oneseed = best_beta[5],
    beta_wp2     = best_beta[6]
  )

  cat(sprintf("\n  %s Final Parameters:\n", round_name))
  cat(sprintf("    beta_wp      = %.4f  (win probability linear)\n", params$beta_wp))
  cat(sprintf("    beta_wp2     = %.4f  (win probability squared)\n", params$beta_wp2))
  cat(sprintf("    beta_save    = %.4f  (future value preservation)\n", params$beta_save))
  cat(sprintf("    beta_path    = %.4f  (opponent-is-used bonus)\n", params$beta_path))
  cat(sprintf("    beta_seed    = %.4f  (seed/tiebreaker value)\n", params$beta_seed))
  cat(sprintf("    beta_oneseed = %.4f  (1-seed avoidance)\n", params$beta_oneseed))
  cat(sprintf("    NLL          = %.4f\n", best_nll))

  params
}


# ==============================================================================
# PREDICTION FUNCTIONS
# ==============================================================================

#' Predict team selection probabilities.
#'
#' @param used_teams Character vector of team names already used
#' @param candidates_df data.frame with columns: team, wp, champ_equity, seed, opponent_is_used
#' @param n_picks Integer (1 or 2)
#' @param params Named list from fit_hodes_team_selection
#' @param round_name "S16" or "E8" (controls save feature)
#' @return Named numeric vector of pick probabilities (sums to 1)
predict_hodes_pick <- function(used_teams, candidates_df, n_picks, params,
                               round_name = "S16") {
  if (nrow(candidates_df) == 0) return(numeric(0))

  # Compute fv_fraction
  remaining_fv <- sum(candidates_df$champ_equity, na.rm = TRUE)
  candidates_df$fv_fraction <- candidates_df$champ_equity /
    max(remaining_fv, 1e-10)

  scores <- .compute_scores(
    candidates_df$wp,
    candidates_df$fv_fraction,
    candidates_df$opponent_is_used,
    candidates_df$seed,
    params,
    round_name = round_name
  )

  n_cands <- nrow(candidates_df)

  if (n_picks == 1 || n_cands <= 1) {
    # Standard softmax
    if (n_cands == 1) {
      probs <- setNames(1.0, candidates_df$team[1])
      return(probs)
    }
    log_probs <- .log_softmax(scores)
    probs <- exp(log_probs)
    names(probs) <- candidates_df$team
    return(probs)

  } else if (n_picks == 2) {
    # Enumerate valid pairs (different games)
    game_idx <- ceiling(candidates_df$candidate_id / 8)
    # If candidate_id not present, use a generic game grouping from wp
    # For prediction, we need the game index. We'll use a simpler approach:
    # group by the matchup - candidates_df should indicate which game each team is in
    # Fallback: if no candidate_id, assume all pairs are valid
    if (!"candidate_id" %in% names(candidates_df)) {
      # All pairs valid
      game_idx <- seq_len(n_cands)
    }

    # Compute pair scores and marginals
    marginal_probs <- setNames(rep(0, n_cands), candidates_df$team)
    pair_scores_list <- c()
    pair_indices <- list()
    pi <- 0

    for (a in 1:(n_cands - 1)) {
      for (b in (a + 1):n_cands) {
        if (game_idx[a] != game_idx[b]) {
          ps <- scores[a] + scores[b]
          pi <- pi + 1
          pair_scores_list <- c(pair_scores_list, ps)
          pair_indices[[pi]] <- c(a, b)
        }
      }
    }

    if (length(pair_scores_list) == 0) {
      # No valid pairs -- fall back to individual softmax
      log_probs <- .log_softmax(scores)
      probs <- exp(log_probs)
      names(probs) <- candidates_df$team
      return(probs)
    }

    # Softmax over pairs
    log_pair_probs <- .log_softmax(pair_scores_list)
    pair_probs <- exp(log_pair_probs)

    # Extract marginals
    for (k in seq_along(pair_indices)) {
      idx_pair <- pair_indices[[k]]
      marginal_probs[idx_pair[1]] <- marginal_probs[idx_pair[1]] + pair_probs[k]
      marginal_probs[idx_pair[2]] <- marginal_probs[idx_pair[2]] + pair_probs[k]
    }

    return(marginal_probs)
  }
}


# ==============================================================================
# MASTER CALIBRATION
# ==============================================================================

#' Calibrate all Hodes entry model components.
#'
#' @param all_data List from analyze_all_hodes_years()
#' @return List with: count_model, s16, e8
calibrate_hodes_model <- function(all_data) {
  # Fit pick count model
  count_model <- fit_hodes_pick_count_model(all_data$s16_count_calib)

  # Fit S16 team selection
  s16_params <- fit_hodes_team_selection(all_data$s16_team_calib, "S16")

  # Fit E8 team selection
  e8_params <- fit_hodes_team_selection(all_data$e8_team_calib, "E8")

  # Save
  params <- list(
    count_model = count_model,
    s16 = s16_params,
    e8 = e8_params
  )
  saveRDS(params, "hodes_entry_model_params.rds")
  cat("\nSaved to hodes_entry_model_params.rds\n")
  return(params)
}


cat("hodes_entry_model.R loaded.\n")
cat("  calibrate_hodes_model(all_data)  -- fit all models\n")
cat("  predict_hodes_pick(...)          -- predict team probabilities\n")
