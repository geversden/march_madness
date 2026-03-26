#!/usr/bin/env Rscript
# ==============================================================================
# hodes_optimizer.R
# Round-by-round portfolio optimizer for Hodes NCAA survivor contests.
#
# Mirrors the splash_optimizer.R architecture:
#   1. Precompute team-round win matrices from simulation
#   2. Estimate field ownership (calibrated from historical hodes_usage CSVs)
#   3. Precompute group context: field survival, V_die (with TB1 adjustment), V_survive
#   4. Beam search: for each candidate primary pick, find the best triple/path
#   5. Greedy marginal EV allocation across 21 entries (with jitter)
#   6. Portfolio-level EV correction for self-competition
#
# Key hodes differences from splash:
#   - R1 and R2 are 3-pick slots (not 1-2)
#   - No day split — all R1 picks are one batch, all R2 picks are one batch
#   - Tiebreaker-aware EV: TB1 (S16_opt success) boosts V_die for rounds 3+
#   - S16_opt is picked greedily within the S16 beam step
#
# Usage:
#   source("R/hodes_config.R")
#   source("R/hodes_optimizer.R")
#   result <- run_hodes_optimizer("sim_results_2026.rds", current_round = 1,
#                                 n_entries = 21, contest_size = 1250,
#                                 prize_pool = 5000)
# ==============================================================================

library(data.table)

if (!exists("HODES_SLOTS")) source(file.path(dirname(sys.frame(1)$ofile %||% "."), "hodes_config.R"))

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================================================================
# PROGRESS TRACKING (mirrors splash_optimizer.R)
# ==============================================================================

make_progress <- function(total, label = "Processing", update_every = 2) {
  env <- new.env(parent = emptyenv())
  env$total <- total; env$current <- 0L; env$label <- label
  env$start_time <- proc.time()[["elapsed"]]
  env$last_print <- env$start_time; env$update_every <- update_every

  tick <- function(n = 1L) {
    env$current <- env$current + n
    now <- proc.time()[["elapsed"]]
    since_print <- now - env$last_print
    if (since_print >= env$update_every || env$current == env$total) {
      pct <- env$current / env$total * 100
      rate <- env$current / max(now - env$start_time, 0.001)
      remaining <- (env$total - env$current) / max(rate, 0.001)
      eta <- if (remaining < 60) sprintf("%.0fs", remaining)
             else sprintf("%.1fm", remaining / 60)
      filled <- round(pct / 100 * 30)
      bar <- paste0("[", strrep("=", filled),
                    ifelse(filled < 30, ">", ""),
                    strrep(" ", max(0, 30 - filled - 1)), "]")
      cat(sprintf("\r  %s %s %5.1f%% (%s/%s) ETA %s   ",
                  env$label, bar, pct,
                  format(env$current, big.mark = ","),
                  format(env$total, big.mark = ","), eta))
      if (env$current == env$total) cat("\n")
      flush.console()
      env$last_print <- now
    }
  }

  done <- function() {
    elapsed <- proc.time()[["elapsed"]] - env$start_time
    time_str <- if (elapsed < 60) sprintf("%.1fs", elapsed) else sprintf("%.1fm", elapsed / 60)
    cat(sprintf("\r  %s: done (%s in %s)%s\n", env$label,
                format(env$total, big.mark = ","), time_str, strrep(" ", 30)))
    flush.console()
  }

  list(tick = tick, done = done)
}

# ==============================================================================
# STEP 1: PRECOMPUTE TEAM-ROUND WIN MATRICES (mirrors splash_optimizer.R:108-149)
# ==============================================================================

#' @param sim List: all_results (n_sims x 63), teams, round_info, n_sims
#' @return List: team_round_wins (list of 6 logical matrices n_sims x n_teams),
#'   team_round_probs (matrix n_teams x 6)
precompute_team_wins <- function(sim) {
  ar <- sim$all_results; ri <- sim$round_info
  n_sims <- sim$n_sims; n_teams <- nrow(sim$teams)

  round_cols <- lapply(1:6, function(rd) which(ri$round_num == rd))

  team_round_wins <- vector("list", 6)
  team_round_probs <- matrix(0, nrow = n_teams, ncol = 6)

  cat("Precomputing team-round win matrices...\n")
  pg <- make_progress(6, "Rounds", update_every = 1)
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
    pg$tick()
  }
  pg$done()

  list(team_round_wins = team_round_wins, team_round_probs = team_round_probs)
}

# ==============================================================================
# STEP 2: OWNERSHIP MODEL
# ==============================================================================

#' Calibrate hodes ownership model using GAM with factor seeds and wp interaction.
#'
#' Fits gam(log_share ~ s(wp, k=4) + seed_f + s(wp, by=seed_f, k=3, m=1))
#' per round group (R1, R2, S16, E8plus) using hodes_usage_YYYY.csv + sim RDS files.
#' Mirrors the pattern in ownership_model_joe.R.
#'
#' @param csv_dir Directory containing hodes_usage_YYYY.csv files
#' @param sim_rds_pattern sprintf pattern for sim file paths, e.g. "sim_results_%d.rds"
#' @param years Integer vector of years to include (default 2021:2025)
#' @param output_file Path to save calibration RDS (default "hodes_calibration.rds")
#' @return List with gam_models (named list per round group) and cal_data
load_hodes_calibration <- function(csv_dir = "hodes_usage",
                                    sim_rds_pattern = "sim_results_%d.rds",
                                    years = 2021:2025,
                                    output_file = "hodes_calibration.rds") {
  if (!requireNamespace("mgcv", quietly = TRUE)) stop("mgcv package required")
  library(mgcv)

  all_rows <- list()

  for (yr in years) {
    csv_path <- file.path(csv_dir, sprintf("hodes_usage_%d.csv", yr))
    sim_path <- sprintf(sim_rds_pattern, yr)

    if (!file.exists(csv_path)) { cat(sprintf("  Missing CSV: %s\n", csv_path)); next }
    if (!file.exists(sim_path))  { cat(sprintf("  Missing sim: %s\n", sim_path)); next }

    cat(sprintf("  Loading %d data...\n", yr))
    dat <- fread(csv_path, header = TRUE)

    # Normalize column names (fread renders '"Seed"' as X.Seed after make.names)
    setnames(dat, make.names(names(dat)))
    seed_col <- grep("^X\\.?Seed", names(dat), value = TRUE)[1]
    if (!is.na(seed_col)) setnames(dat, seed_col, "Seed")
    if (!"Team" %in% names(dat)) next

    # Remove "No Pick" rows and parse percentages
    dat <- dat[Team != "No Pick" & !is.na(Team) & trimws(Team) != ""]
    parse_pct <- function(x) as.numeric(sub("%", "", as.character(x))) / 100
    dat[, raw_picks_n := suppressWarnings(as.numeric(raw_picks))]
    dat[, prev_frac   := parse_pct(previous_usage_perc)]
    dat <- dat[!is.na(raw_picks_n) & raw_picks_n > 0]

    sim_yr   <- readRDS(sim_path)
    teams_yr <- as.data.table(sim_yr$teams)
    tw_yr    <- precompute_team_wins(sim_yr)

    for (rd in unique(dat$Round)) {
      sub <- dat[Round == rd]
      wp_vec    <- numeric(nrow(sub))
      avail_vec <- numeric(nrow(sub))

      for (i in seq_len(nrow(sub))) {
        tid_idx <- match(sub$Team[i], teams_yr$name)
        if (is.na(tid_idx)) { wp_vec[i] <- NA; next }
        tid <- teams_yr$team_id[tid_idx]
        wp_vec[i]    <- tw_yr$team_round_probs[tid, rd]
        avail_vec[i] <- if (is.na(sub$prev_frac[i])) 1.0 else max(1 - sub$prev_frac[i], 0.05)
      }

      sub[, wp    := wp_vec]
      sub[, avail := avail_vec]
      sub[, year  := yr]
      sub[, round := rd]
      all_rows[[length(all_rows) + 1]] <- sub[!is.na(wp) & wp > 0]
    }
  }

  if (length(all_rows) == 0) {
    cat("  No calibration data found. Using fallback defaults.\n")
    return(list(gam_models = list()))
  }

  cal_data <- rbindlist(all_rows, fill = TRUE)

  # Within-round share (log scale) — matches Joe's pattern
  cal_data[, group_total := sum(raw_picks_n), by = .(year, round)]
  cal_data[, share       := raw_picks_n / group_total]
  cal_data[, log_share   := log(pmax(share, 1e-6))]
  cal_data[, seed_f      := factor(pmin(suppressWarnings(as.integer(Seed)), 9),
                                    levels = 1:9,
                                    labels = c("1","2","3","4","5","6","7","8","9+"))]
  cal_data[, log_avail   := log(pmax(avail, 0.05))]

  # Fit GAM per round group
  round_groups <- list(R1 = 1L, R2 = 2L, S16 = 3L, E8plus = 4:6)
  gam_models <- list()

  for (grp_name in names(round_groups)) {
    rds <- round_groups[[grp_name]]
    sub <- cal_data[round %in% rds & share > 0 & wp > 0 & !is.na(seed_f)]
    if (nrow(sub) < 15) {
      cat(sprintf("  %s: insufficient data (%d rows), skipping GAM\n", grp_name, nrow(sub)))
      next
    }

    # R1 has no availability factor; R2+ include log_avail
    formula_str <- if (grp_name == "R1") {
      "log_share ~ s(wp, k=4) + seed_f + s(wp, by=seed_f, k=3, m=1)"
    } else {
      "log_share ~ s(wp, k=4) + seed_f + s(wp, by=seed_f, k=3, m=1) + s(log_avail, k=3)"
    }

    fit <- tryCatch(
      mgcv::gam(as.formula(formula_str), data = sub,
                method = "REML", gamma = 1.5, select = TRUE),
      error = function(e) {
        cat(sprintf("  %s: GAM failed (%s), skipping\n", grp_name, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(fit)) {
      gam_models[[grp_name]] <- fit
      cat(sprintf("  %s: GAM fitted (%d rows, R-sq=%.3f)\n",
                  grp_name, nrow(sub), summary(fit)$r.sq))
    }
  }

  result <- list(gam_models = gam_models, cal_data = cal_data)
  saveRDS(result, output_file)
  cat(sprintf("  Saved %s\n", output_file))
  result
}

#' Load calibrated GAM models or return empty list (fallback to defaults)
load_hodes_params <- function(path = "hodes_calibration.rds") {
  if (file.exists(path)) {
    cat(sprintf("Loading calibration from %s\n", path))
    obj <- readRDS(path)
    # Support both old (list with beta_wp) and new (list with gam_models) formats
    if (!is.null(obj$gam_models)) return(obj)
    # Old format: wrap so downstream code can detect
    return(list(gam_models = list(), legacy_params = obj))
  }
  list(gam_models = list())
}

#' Estimate field ownership for a given round using the GAM model.
#'
#' Returns a named numeric vector (team_name -> expected fraction of field picks)
#' summing to n_picks (3 for R1/R2, 1 for S16+).
#' Uses GAM with factor(seed) + s(wp) interaction when available; falls back to
#' linear log_attract model if not.
#'
#' @param round_num Integer round (1-6)
#' @param teams_dt data.table with team_id, name, seed, region
#' @param tw Precomputed team wins (from precompute_team_wins)
#' @param params Calibrated params from load_hodes_params()
#' @param field_used Named numeric vector: team_name -> fraction of field already
#'   used this team (for availability adjustment). NULL for R1.
#' @param n_picks_override Integer override for n_picks (default from HODES_SLOTS)
estimate_hodes_ownership <- function(round_num, teams_dt, tw, params = NULL,
                                      field_used = NULL, n_picks_override = NULL) {
  slot_id <- HODES_ROUND_TO_SLOT[as.character(round_num)]
  n_picks <- n_picks_override %||% HODES_SLOTS[[slot_id]]$n_picks
  rg      <- switch(as.character(round_num), "1" = "R1", "2" = "R2", "3" = "S16", "E8plus")

  n_teams <- nrow(teams_dt)
  wp      <- tw$team_round_probs[, round_num]

  # Availability (R2+)
  avail <- rep(1.0, n_teams)
  if (!is.null(field_used)) {
    for (i in seq_len(n_teams)) {
      used_frac <- field_used[[teams_dt$name[i]]]
      if (!is.null(used_frac)) avail[i] <- max(1.0 - used_frac, 0.05)
    }
  }

  # Use round-appropriate threshold to exclude near-zero teams (prevents GAM extrapolation artifacts)
  # Later rounds need lower thresholds since unconditional win probs shrink each round
  can_play_thresh <- if (round_num <= 2L) {
    if (round_num == 1L) 0.02 else 0.01
  } else if (round_num <= 4L) 0.005 else 0.002
  can_play <- wp > can_play_thresh
  if (!any(can_play)) {
    cat(sprintf("  WARNING: No teams with positive win prob in round %d\n", round_num))
    return(setNames(numeric(0), character(0)))
  }

  # Try GAM prediction
  gam_models <- if (!is.null(params$gam_models)) params$gam_models else list()
  gam_model  <- gam_models[[rg]]

  scores <- rep(-Inf, n_teams)

  if (!is.null(gam_model)) {
    pred_dt <- data.table(
      wp        = wp[can_play],
      seed_f    = factor(pmin(suppressWarnings(as.integer(teams_dt$seed[can_play])), 9),
                          levels = 1:9,
                          labels = c("1","2","3","4","5","6","7","8","9+")),
      log_avail = log(pmax(avail[can_play], 0.05))
    )
    scores[can_play] <- tryCatch(
      as.numeric(predict(gam_model, newdata = pred_dt)),
      error = function(e) {
        cat(sprintf("  GAM predict failed for round %d: %s\n", round_num, conditionMessage(e)))
        rep(0, sum(can_play))
      }
    )
  } else {
    # Fallback: linear log_attract (legacy behaviour)
    lp  <- if (!is.null(params$legacy_params)) params$legacy_params else list()
    beta_wp   <- (lp$beta_wp[[rg]])   %||% 4.0
    save_str  <- (lp$save_strength[round_num]) %||% c(0.60, 0.50, 0.35, 0.15, 0.0, 0.0)[round_num]
    beta_avail <- (lp$beta_avail) %||% 1.0
    fv <- tw$team_round_probs[, 6]
    scores <- beta_wp * wp - save_str * fv + beta_avail * log(avail)
    scores[!can_play] <- -Inf
  }

  scores[!can_play] <- -Inf
  scores <- scores - max(scores[can_play])
  raw_shares <- rep(0.0, n_teams)
  raw_shares[can_play] <- exp(scores[can_play])

  ownership <- n_picks * raw_shares / sum(raw_shares)
  names(ownership) <- teams_dt$name
  ownership
}

# ==============================================================================
# STEP 3: PRECOMPUTE GROUP CONTEXT
# ==============================================================================

#' Precompute all group-level quantities for fast candidate EV evaluation.
#'
#' Computes field survival per slot, V_die (base + TB1-adjusted), V_survive,
#' slot_team_scores, and TB1 probability.
#'
#' @param group A row from group_hodes_entries()
#' @param current_round Integer round number (1-6)
#' @param tw Precomputed team wins
#' @param teams_dt Teams data frame
#' @param own_by_round Named list "1"/"2"/"3"/"4"/"5"/"6" -> ownership vector
#' @param sample_idx Integer vector of sim indices to use
#' @return List (ctx) with precomputed matrices
precompute_hodes_context <- function(group, current_round, tw, teams_dt,
                                      own_by_round, sample_idx) {
  n_teams  <- nrow(teams_dt)
  n_sims   <- length(sample_idx)
  full_field      <- group$contest_size - group$n_entries
  prize_pool      <- group$prize_pool          # raw pool ($58k)
  winner_fraction <- group$winner_fraction %||% 0.931
  # effective_prize: what winner gets when dying mid-round (last standing, no consolation)
  effective_prize <- prize_pool * winner_fraction  # $53,998
  # When 2+ entries survive all rounds, non-winning survivors share an ADDITIONAL
  # 6.9% (consolation addendum).  EV per co-finalist = prize_pool / k (full pool).
  # So V_survive_terminal uses prize_pool (not effective_prize).
  used_teams <- group$used_teams[[1]]
  max_round  <- 6L

  # Remaining slots from current_round onward
  remaining_rounds <- current_round:6L
  remaining_slots  <- HODES_ROUND_TO_SLOT[as.character(remaining_rounds)]
  n_remaining      <- length(remaining_slots)

  # ---- 1. Field survival per slot ----
  field_slot_survive <- matrix(1, nrow = n_sims, ncol = n_remaining)

  for (si in seq_along(remaining_slots)) {
    rd  <- remaining_rounds[si]
    sid <- remaining_slots[si]
    n_picks <- HODES_SLOTS[[sid]]$n_picks

    own <- own_by_round[[as.character(rd)]]
    if (is.null(own) || length(own) == 0) next

    own_vec <- numeric(n_teams)
    own_ids <- teams_dt$team_id[match(names(own), teams_dt$name)]
    valid <- !is.na(own_ids)
    own_vec[own_ids[valid]] <- own[names(own)[valid]]
    if (sum(own_vec) < 1e-12) next

    win_mat_sub <- tw$team_round_wins[[rd]][sample_idx, , drop = FALSE]
    p_field_wins <- as.numeric(win_mat_sub %*% own_vec)
    if (rd == 1L) {
      # R1: all teams play, unconditional win prob
      cond_p <- p_field_wins / sum(own_vec)
    } else {
      # R2+: field picks are drawn from teams that already won round rd-1.
      # Use conditional P(wins rd | plays rd) = P(wins rd) / P(won rd-1),
      # estimated per-sim as ownership-weighted rd wins / ownership-weighted rd-1 wins.
      prev_win_mat <- tw$team_round_wins[[rd - 1L]][sample_idx, , drop = FALSE]
      p_field_plays <- as.numeric(prev_win_mat %*% own_vec)
      cond_p <- ifelse(p_field_plays > 1e-8, p_field_wins / p_field_plays, 0)
    }
    # P(all n_picks win) under independence
    field_slot_survive[, si] <- cond_p^n_picks
  }

  # ---- 2. Cumulative field survival ----
  field_cum <- field_slot_survive
  if (n_remaining >= 2) {
    for (si in 2:n_remaining) {
      field_cum[, si] <- field_cum[, si - 1] * field_slot_survive[, si]
    }
  }
  p_field_all <- field_cum[, n_remaining]

  # ---- 3. Field death-round distribution ----
  field_dies_round <- matrix(0, nrow = n_sims, ncol = max_round)
  for (si in seq_along(remaining_slots)) {
    rd    <- remaining_rounds[si]
    p_die <- if (si == 1) 1 - field_slot_survive[, si]
              else field_cum[, si - 1] * (1 - field_slot_survive[, si])
    field_dies_round[, rd] <- field_dies_round[, rd] + p_die
  }

  # ---- 4. p_field_tb1: per-sim P(field entry gets TB1) ----
  # = P(field entry survived weekend R1+R2) * P(field's S16_opt pick wins)
  p_field_tb1 <- rep(0, n_sims)

  si_R2  <- match(2L, remaining_rounds)
  si_S16 <- match(3L, remaining_rounds)

  # TB1 uses S16_opt ownership (not mandatory S16) when available.
  # own_by_round[["3_opt"]] contains expected opt pick counts from entry-level model.
  # Fall back to mandatory S16 ownership if opt ownership not provided.
  .build_tb1_p <- function(own_vec_raw, sample_idx, n_teams, teams_dt, tw) {
    own_vec <- numeric(n_teams)
    ids <- teams_dt$team_id[match(names(own_vec_raw), teams_dt$name)]
    v   <- !is.na(ids)
    own_vec[ids[v]] <- own_vec_raw[names(own_vec_raw)[v]]
    s <- sum(own_vec)
    if (s < 1e-12) return(rep(0, length(sample_idx)))
    wm <- tw$team_round_wins[[3L]][sample_idx, , drop = FALSE]
    as.numeric(wm %*% own_vec) / s
  }

  if (!is.na(si_S16)) {
    own_s16opt <- own_by_round[["3_opt"]] %||% own_by_round[["3"]]
    if (!is.null(own_s16opt) && length(own_s16opt) > 0) {
      p_s16opt_win <- .build_tb1_p(own_s16opt, sample_idx, n_teams, teams_dt, tw)
      p_weekend    <- if (!is.na(si_R2)) field_cum[, si_R2] else rep(1, n_sims)
      p_field_tb1  <- p_weekend * p_s16opt_win
    }
  } else if (current_round >= 3L) {
    # S16 is already past; TB1 is decided. Use opt ownership if available.
    own_s16opt <- own_by_round[["3_opt"]] %||% own_by_round[["3"]]
    if (!is.null(own_s16opt) && length(own_s16opt) > 0) {
      p_field_tb1 <- .build_tb1_p(own_s16opt, sample_idx, n_teams, teams_dt, tw)
    }
  }

  # ---- 5. V_die_base[sim, round] — payout if we die in each round ----
  # No tiebreaker shares: win only if we are strictly last (no co-deaths, no outlasters).
  # P(win | die at round d) = P(all field entries die strictly before round d).
  V_die <- matrix(0, nrow = n_sims, ncol = max_round)
  for (d in 1:max_round) {
    p_outlast <- p_field_all
    if (d < max_round) {
      for (rd in (d + 1):max_round) p_outlast <- p_outlast + field_dies_round[, rd]
    }
    # P(field entry dies strictly before round d) = 1 - p_outlast - P(co-die in round d)
    p_no_tie <- pmax(1 - p_outlast - field_dies_round[, d], 0)^full_field
    V_die[, d] <- p_no_tie * effective_prize
  }

  # ---- 6. V_die_tb1[sim, round] — TB1-adjusted payout ----
  # With TB1: a field entry "blocks" us only if it outlasts us OR co-dies with TB1.
  # P(no blocker) = P(field entry dies before rd d, OR co-dies without TB1)^full_field
  #               = (1 - p_outlast - field_dies_round[,d] * p_field_tb1)^full_field
  V_die_tb1 <- V_die  # rounds 1-2: TB1 not applicable, same as base
  for (d in 3:max_round) {
    p_outlast <- p_field_all
    if (d < max_round) {
      for (rd in (d + 1):max_round) p_outlast <- p_outlast + field_dies_round[, rd]
    }
    p_no_tie_tb1 <- pmax(1 - p_outlast - field_dies_round[, d] * p_field_tb1, 0)^full_field
    V_die_tb1[, d] <- p_no_tie_tb1 * effective_prize
  }

  # ---- 7. V_survive[sim, slot] — backward-recursed continuation value ----
  # Terminal value: 6.9% charity always removed, consolation pool (also 6.9%, additional)
  # simplifies so all k co-survivors share effective_prize / k each.
  # => V_terminal = effective_prize / (1 + E[field survivors])
  V_survive <- matrix(0, nrow = n_sims, ncol = n_remaining + 1)
  V_survive[, n_remaining + 1] <- effective_prize / (1 + p_field_all * full_field)

  for (s in n_remaining:2) {
    rd <- remaining_rounds[s]
    V_survive[, s] <- field_slot_survive[, s] * V_survive[, s + 1] +
                      (1 - field_slot_survive[, s]) * V_die[, rd]
  }

  # ---- 8. Slot-team scores for beam search initialization ----
  # For each future slot: E[win[sim,team] * delta_V[sim]] / n_sims
  slot_team_scores <- vector("list", n_remaining)

  for (si in 2:n_remaining) {
    rd  <- remaining_rounds[si]
    delta_V <- V_survive[, si + 1] - V_die[, rd]

    valid_teams <- which(tw$team_round_probs[, rd] > 0.01)
    if (length(valid_teams) == 0) {
      slot_team_scores[[si]] <- setNames(numeric(0), character(0))
      next
    }

    win_mat_sub <- tw$team_round_wins[[rd]][sample_idx, valid_teams, drop = FALSE]
    scores <- as.numeric(crossprod(win_mat_sub, delta_V)) / n_sims
    slot_team_scores[[si]] <- setNames(scores, as.character(valid_teams))
  }

  list(
    remaining_slots    = remaining_slots,
    remaining_rounds   = remaining_rounds,
    n_remaining        = n_remaining,
    field_slot_survive = field_slot_survive,
    field_cum          = field_cum,
    field_dies_round   = field_dies_round,
    p_field_all        = p_field_all,
    p_field_tb1        = p_field_tb1,
    V_die              = V_die,
    V_die_tb1          = V_die_tb1,
    V_survive          = V_survive,
    slot_team_scores   = slot_team_scores,
    sample_idx         = sample_idx,
    n_sims             = n_sims,
    full_field         = full_field,
    prize_pool         = prize_pool,          # raw pool (stored for reference)
    effective_prize    = effective_prize,     # 93.1% × raw — used in all EV formulas
    max_round          = max_round,
    used_teams         = used_teams,
    teams_dt           = teams_dt
  )
}

# ==============================================================================
# STEP 4: MC CANDIDATE SCORING (mirrors splash contest_mc_sim.R)
# ==============================================================================

#' 0-indexed game column for a team in a given round (mirrors contest_mc_sim.R)
game_col_for_team <- function(team_id, round_num) {
  ROUND_BASE <- c(0L, 32L, 48L, 56L, 60L, 62L)
  ROUND_BASE[round_num] + as.integer(ceiling(team_id / 2^round_num)) - 1L
}

#' Build per-sim × per-remaining-round matrix of expected field entries alive.
#'
#' Uses ownership vectors from own_by_round and the team win matrices to
#' compute, for each sim, how many field entries survive each remaining round.
#' This is the hodes equivalent of splash's n_alive_matrix from entry_field_sim.
#'
#' @param remaining_rounds Integer vector of remaining round numbers
#' @param tw Precomputed team-round win matrices
#' @param own_by_round Named list "3".."6" -> ownership count vectors
#' @param teams_dt Teams data frame
#' @param sample_idx Sim indices to use
#' @param n_field Total number of field entries (contest_size minus our entries)
#' @return Numeric matrix (length(sample_idx) x length(remaining_rounds))
build_hodes_n_alive_matrix <- function(remaining_rounds, tw, own_by_round,
                                        teams_dt, sample_idx, n_field) {
  n_sims   <- length(sample_idx)
  n_rounds <- length(remaining_rounds)
  n_teams  <- nrow(teams_dt)
  mat      <- matrix(0.0, nrow = n_sims, ncol = n_rounds)

  cum_surv <- rep(1.0, n_sims)  # cumulative P(field entry alive) per sim

  for (si in seq_along(remaining_rounds)) {
    rd      <- remaining_rounds[si]
    own_raw <- own_by_round[[as.character(rd)]]

    wp <- tw$team_round_probs[, rd]   # sim-estimated win probs for this round

    own_vec <- numeric(n_teams)
    if (!is.null(own_raw) && sum(own_raw) > 1e-10) {
      ids   <- teams_dt$team_id[match(names(own_raw), teams_dt$name)]
      valid <- !is.na(ids)
      own_vec[ids[valid]] <- own_raw[names(own_raw)[valid]]
    }
    # Normalize to fractions: own_raw may be counts (entry model) or fractions (GAM)
    s <- sum(own_vec)
    if (s > 1e-10) own_vec <- own_vec / s

    # Blend with win-probability proportional weights for teams the model missed.
    # GAM for rounds 4-6 often fails to cover all alive teams (e.g. Michigan, Purdue),
    # leaving their n_field_alive=0 in those sims and making the C++ think we win alone.
    wp_s <- sum(wp)
    if (wp_s > 1e-10) {
      wp_norm    <- wp / wp_s
      # How well does the model cover teams that are actually likely to win?
      model_cov  <- sum(own_vec[wp > 0.05])       # model weight on reasonably-likely teams
      if (model_cov < 0.5) {
        # Model coverage is poor — use win-prob proportional directly
        own_vec <- wp_norm
      } else {
        # Blend: 80% model + 20% win-prob to fill in any zero gaps
        own_vec <- 0.8 * own_vec + 0.2 * wp_norm
        own_vec <- own_vec / sum(own_vec)
      }
    }

    wm          <- tw$team_round_wins[[rd]][sample_idx, , drop = FALSE]
    # P(a random field entry picks the winner of this round) — independent per round
    p_slot_surv <- as.numeric(wm %*% own_vec)

    cum_surv  <- cum_surv * p_slot_surv
    mat[, si] <- cum_surv * n_field
  }
  mat
}

#' Score all candidates for a hodes entry group using forward-knowledge MC.
#'
#' Hodes-specific wrapper around solve_optimal_paths_cpp — uses round numbers
#' (3-6) instead of splash slot IDs, and assumes 1 pick per round for S16+.
#' For each candidate S16 pick, the C++ finds the optimal future path across
#' E8/FF/Champ using full knowledge of each sim's outcomes.
#'
#' @param group A row from group_hodes_entries() with used_teams, prize_pool, etc.
#' @param current_round Integer round number (3=S16, 4=E8, 5=FF, 6=Champ)
#' @param sim Sim object with all_results (n_sims x 63)
#' @param teams_dt Teams data frame (64 rows)
#' @param sample_idx Integer vector of sim indices to use
#' @param n_field_alive_matrix Numeric matrix (length(sample_idx) x n_remaining_rounds)
#' @param viable_cids Integer vector of alive team_ids for the current round
#' @return data.table with columns: team_id, team_name, ev, p_survive_today,
#'         p_win_contest, mean_death_rd. Has "death_rounds" attribute (matrix).
mc_score_hodes_candidates <- function(group, current_round, sim, teams_dt,
                                       sample_idx, n_field_alive_matrix,
                                       viable_cids) {
  remaining_rounds <- current_round:6L
  n_remaining      <- length(remaining_rounds)
  used_teams       <- group$used_teams[[1]]
  wf               <- group$winner_fraction %||% 0.931
  effective_prize  <- as.double(group$prize_pool * wf)

  ar_sub <- sim$all_results[sample_idx, , drop = FALSE]

  # Build slot metadata for C++
  slot_team_ids_list  <- vector("list", n_remaining)
  slot_game_cols_list <- vector("list", n_remaining)
  slot_n_picks        <- rep(1L, n_remaining)
  slot_round_nums     <- as.integer(remaining_rounds)

  for (si in seq_along(remaining_rounds)) {
    rd    <- remaining_rounds[si]
    # Current slot: use alive teams only; future slots: all 64 (C++ filters by winner)
    cands <- if (si == 1L) viable_cids else teams_dt$team_id
    gcols <- vapply(cands, function(tid) game_col_for_team(tid, rd), integer(1))
    slot_team_ids_list[[si]]  <- as.integer(cands)
    slot_game_cols_list[[si]] <- as.integer(gcols)
  }

  first_cands <- viable_cids[!viable_cids %in% used_teams]
  if (length(first_cands) == 0L) {
    return(data.table(team_id = integer(0), team_name = character(0),
                      ev = numeric(0), p_survive_today = numeric(0),
                      p_win_contest = numeric(0), mean_death_rd = numeric(0)))
  }

  first_gcols <- vapply(first_cands,
    function(tid) game_col_for_team(tid, remaining_rounds[1L]), integer(1))

  cpp_result <- solve_optimal_paths_cpp(
    all_results         = ar_sub,
    n_field_alive       = n_field_alive_matrix,
    slot_team_ids_list  = slot_team_ids_list,
    slot_game_cols_list = slot_game_cols_list,
    slot_n_picks        = slot_n_picks,
    slot_round_nums     = slot_round_nums,
    used_teams_vec      = as.integer(used_teams),
    first_slot_cands    = as.integer(first_cands),
    first_slot_gcols    = as.integer(first_gcols),
    prize_pool          = effective_prize,
    n_slots             = as.integer(n_remaining)
  )

  p_today <- vapply(seq_along(first_cands), function(i) {
    gcol <- first_gcols[i] + 1L   # 1-indexed for R
    mean(ar_sub[, gcol] == first_cands[i])
  }, numeric(1))

  result_dt <- data.table(
    team_id         = first_cands,
    team_name       = teams_dt$name[first_cands],
    ev              = cpp_result$ev,
    p_survive_today = p_today,
    p_win_contest   = cpp_result$p_survive,
    mean_death_rd   = cpp_result$mean_death
  )
  attr(result_dt, "death_rounds") <- cpp_result$death_rounds
  result_dt
}

# ==============================================================================
# STEP 4b: BEAM SEARCH CANDIDATE EV (kept for multi-pick rounds R1/R2)
# ==============================================================================

#' Compute EV for a candidate primary pick using beam search.
#'
#' Handles 3-pick slots (R1, R2) with greedy companion selection.
#' Picks S16_opt greedily within the S16 beam step.
#' Uses V_die_tb1 vs V_die in final payout based on s16opt_wins.
#'
#' @param primary_id Integer team_id for today's primary pick
#' @param ctx Precomputed context from precompute_hodes_context()
#' @param tw Precomputed team wins
#' @param diagnostics Logical: return extra diagnostic info
#' @param beam_width Integer: paths to keep at each step (default 6)
#' @param expand_top Integer: top teams to expand per path per slot (default 5)
#' @return List: ev, p_survive_today, p_win_contest, mean_death_rd, p_survive_all,
#'   our_death_rd, slot1_extra_ids, s16opt_id, seed_sum
compute_hodes_candidate_ev <- function(primary_id, ctx, tw,
                                        diagnostics = FALSE,
                                        beam_width = 6L, expand_top = 5L,
                                        comp_jitter = 0,
                                        companion_ids = NULL,
                                        pick_s16opt = TRUE) {
  n_sims          <- ctx$n_sims
  max_round       <- ctx$max_round
  n_remaining     <- ctx$n_remaining
  rem_rounds      <- ctx$remaining_rounds
  rem_slots       <- ctx$remaining_slots
  full_field      <- ctx$full_field
  prize_pool      <- ctx$prize_pool       # raw ($58k) — for co-survivor payouts
  effective_prize <- ctx$effective_prize  # 93.1% × raw — for mid-round deaths
  teams_dt        <- ctx$teams_dt

  # ---- Slot 1: primary pick (+ companions for multi-pick slots) ----
  rd1     <- rem_rounds[1]
  sid1    <- rem_slots[1]
  n_picks1 <- HODES_SLOTS[[sid1]]$n_picks

  slot1_survive <- as.numeric(tw$team_round_wins[[rd1]][ctx$sample_idx, primary_id])
  slot1_extra_ids <- integer(0)
  init_used    <- c(ctx$used_teams, primary_id)
  init_bt      <- primary_id   # bracket_teams
  init_br      <- rd1           # bracket_rounds
  seed_sum_init <- teams_dt$seed[primary_id]

  if (n_picks1 > 1L && !is.null(companion_ids)) {
    # Pre-specified companions: use them directly (full-enumeration mode)
    win_mat_rd1 <- tw$team_round_wins[[rd1]][ctx$sample_idx, , drop = FALSE]
    for (cid in companion_ids) {
      slot1_extra_ids <- c(slot1_extra_ids, cid)
      slot1_survive   <- slot1_survive * win_mat_rd1[, cid]
      seed_sum_init   <- seed_sum_init + teams_dt$seed[cid]
      init_used <- c(init_used, cid)
      init_bt   <- c(init_bt, cid)
      init_br   <- c(init_br, rd1)
    }
  } else if (n_picks1 > 1L) {
    # Greedy companion selection (legacy fallback)
    delta_V1 <- if (n_remaining >= 2L) ctx$V_survive[, 2] - ctx$V_die[, rd1]
                else ctx$V_survive[, n_remaining + 1] - ctx$V_die[, rd1]

    win_mat_rd1 <- tw$team_round_wins[[rd1]][ctx$sample_idx, , drop = FALSE]

    for (p in 2:n_picks1) {
      avail_ids <- setdiff(which(tw$team_round_probs[, rd1] > 0.01), init_used)
      if (length(avail_ids) == 0) { slot1_survive <- rep(0, n_sims); break }

      compat <- vapply(avail_ids, function(tid)
        is_hodes_bracket_compatible(tid, rd1, init_bt, init_br), logical(1))
      avail_ids <- avail_ids[compat]
      if (length(avail_ids) == 0) { slot1_survive <- rep(0, n_sims); break }

      scores <- as.numeric(crossprod(win_mat_rd1[, avail_ids, drop = FALSE], delta_V1)) / n_sims
      if (comp_jitter > 0 && length(scores) > 1)
        scores <- scores * runif(length(scores), 1 - comp_jitter, 1 + comp_jitter)
      best_id <- avail_ids[which.max(scores)]

      slot1_extra_ids <- c(slot1_extra_ids, best_id)
      slot1_survive   <- slot1_survive * win_mat_rd1[, best_id]
      seed_sum_init   <- seed_sum_init + teams_dt$seed[best_id]
      init_used <- c(init_used, best_id)
      init_bt   <- c(init_bt, best_id)
      init_br   <- c(init_br, rd1)
    }
  }

  # Set alive_after_r2 for the initial beam path
  if (rd1 == 2L) {
    init_alive_r2 <- slot1_survive > 0
  } else if (rd1 >= 3L) {
    init_alive_r2 <- rep(TRUE, n_sims)
  } else {
    init_alive_r2 <- rep(NA, n_sims)  # filled after R2 step
  }

  slot1_die   <- 1 - slot1_survive
  init_payout <- slot1_die * ctx$V_die[, rd1]

  beam <- list(list(
    used_teams     = init_used,
    cum_survive    = slot1_survive,
    partial_payout = init_payout,
    picks          = primary_id,
    pick_rounds    = rd1,
    bracket_teams  = init_bt,
    bracket_rounds = init_br,
    seed_sum       = seed_sum_init,
    alive_after_r2 = init_alive_r2,
    s16opt_id      = NA_integer_,
    s16opt_wins    = rep(FALSE, n_sims)
  ))

  # ---- Beam search through future slots ----
  if (n_remaining >= 2L) {
    for (si in 2:n_remaining) {
      sid    <- rem_slots[si]
      rd     <- rem_rounds[si]
      n_pick <- HODES_SLOTS[[sid]]$n_picks
      win_mat_rd <- tw$team_round_wins[[rd]]
      scores <- ctx$slot_team_scores[[si]]

      if (is.null(scores) || length(scores) == 0) {
        for (bi in seq_along(beam)) {
          beam[[bi]]$partial_payout <- beam[[bi]]$partial_payout +
            beam[[bi]]$cum_survive * ctx$V_die[, rd]
          beam[[bi]]$cum_survive <- rep(0, n_sims)
          beam[[bi]]$picks       <- c(beam[[bi]]$picks, NA_integer_)
          beam[[bi]]$pick_rounds <- c(beam[[bi]]$pick_rounds, rd)
        }
        next
      }

      new_beam <- list()

      for (bi in seq_along(beam)) {
        path <- beam[[bi]]

        avail_mask <- !names(scores) %in% as.character(path$used_teams)
        if (!any(avail_mask)) {
          new_beam[[length(new_beam) + 1]] <- c(path, list(
            cum_survive    = rep(0, n_sims),
            partial_payout = path$partial_payout + path$cum_survive * ctx$V_die[, rd],
            picks          = c(path$picks, NA_integer_),
            pick_rounds    = c(path$pick_rounds, rd)
          ))
          next
        }

        avail_scores <- scores[avail_mask]
        avail_ids    <- as.integer(names(avail_scores))

        compat_mask <- vapply(avail_ids, function(tid)
          is_hodes_bracket_compatible(tid, rd, path$bracket_teams, path$bracket_rounds),
          logical(1))

        if (!any(compat_mask)) {
          new_beam[[length(new_beam) + 1]] <- c(path, list(
            cum_survive    = rep(0, n_sims),
            partial_payout = path$partial_payout + path$cum_survive * ctx$V_die[, rd],
            picks          = c(path$picks, NA_integer_),
            pick_rounds    = c(path$pick_rounds, rd)
          ))
          next
        }

        avail_scores <- avail_scores[compat_mask]
        avail_ids    <- avail_ids[compat_mask]
        alive_mask   <- avail_scores > 0
        avail_scores <- avail_scores[alive_mask]
        avail_ids    <- avail_ids[alive_mask]
        if (length(avail_ids) == 0) next

        n_expand <- min(expand_top, length(avail_ids))
        top_idx  <- order(avail_scores, decreasing = TRUE)[1:n_expand]

        for (ti in top_idx) {
          team_id      <- avail_ids[ti]
          slot_survive <- as.numeric(win_mat_rd[ctx$sample_idx, team_id])

          new_bt <- c(path$bracket_teams, team_id)
          new_br <- c(path$bracket_rounds, rd)
          new_used <- c(path$used_teams, team_id)
          new_seed_sum <- path$seed_sum + teams_dt$seed[team_id]

          # Handle multi-pick slots (R1, R2: pick 3 total)
          if (n_pick > 1L) {
            extra_used <- new_used
            combined   <- slot_survive
            for (p in 2:n_pick) {
              ex_avail <- !names(scores) %in% as.character(extra_used)
              if (!any(ex_avail)) { combined <- rep(0, n_sims); break }
              ex_scores <- scores[ex_avail]
              ex_ids    <- as.integer(names(ex_scores))
              ex_compat <- vapply(ex_ids, function(tid)
                is_hodes_bracket_compatible(tid, rd, new_bt, new_br), logical(1))
              if (!any(ex_compat)) { combined <- rep(0, n_sims); break }
              ex_scores <- ex_scores[ex_compat]
              next_tid  <- as.integer(names(ex_scores)[which.max(ex_scores)])
              extra_used   <- c(extra_used, next_tid)
              new_bt  <- c(new_bt, next_tid)
              new_br  <- c(new_br, rd)
              combined <- combined * as.numeric(win_mat_rd[ctx$sample_idx, next_tid])
              new_seed_sum <- new_seed_sum + teams_dt$seed[next_tid]
            }
            slot_survive <- combined
            new_used     <- extra_used
          }

          # S16 step: greedily pick S16_opt (tiebreaker, not survival)
          new_s16opt_id   <- path$s16opt_id
          new_s16opt_wins <- path$s16opt_wins
          new_alive_r2    <- path$alive_after_r2

          if (rd == 3L && sid == "S16") {
            # Update alive_after_r2 if we just came from R2
            if (any(is.na(new_alive_r2))) {
              new_alive_r2 <- path$cum_survive > 0
            }

            if (pick_s16opt) {
              # Pick S16_opt greedily: best available S16 team
              s16opt_pool <- setdiff(which(tw$team_round_probs[, 3L] > 1e-6), new_used)
              s16opt_compat <- Filter(function(tid)
                is_hodes_bracket_compatible(tid, 3L, new_bt, new_br),
                s16opt_pool)

              if (length(s16opt_compat) > 0) {
                s16opt_probs <- tw$team_round_probs[s16opt_compat, 3L]
                best_s16opt  <- s16opt_compat[which.max(s16opt_probs)]
                new_s16opt_id <- best_s16opt
                # S16_opt only "wins" if entry survived weekend AND S16_opt team wins S16
                new_s16opt_wins <- new_alive_r2 & as.logical(
                  tw$team_round_wins[[3L]][ctx$sample_idx, best_s16opt])
                new_used <- c(new_used, best_s16opt)
                new_bt   <- c(new_bt, best_s16opt)
                new_br   <- c(new_br, 3L)
                new_seed_sum <- new_seed_sum + teams_dt$seed[best_s16opt]
              }
            }
            # When pick_s16opt = FALSE: skip S16_opt; s16opt_id stays NA,
            # s16opt_wins stays FALSE -> EV uses V_die_base (not V_die_tb1)
          }

          new_cum     <- path$cum_survive * slot_survive
          new_partial <- path$partial_payout +
            path$cum_survive * (1 - slot_survive) * ctx$V_die[, rd]

          # Update alive_after_r2 when finishing R2
          if (rd == 2L) new_alive_r2 <- new_cum > 0

          new_beam[[length(new_beam) + 1]] <- list(
            used_teams     = new_used,
            cum_survive    = new_cum,
            partial_payout = new_partial,
            picks          = c(path$picks, team_id),
            pick_rounds    = c(path$pick_rounds, rd),
            bracket_teams  = new_bt,
            bracket_rounds = new_br,
            seed_sum       = new_seed_sum,
            alive_after_r2 = new_alive_r2,
            s16opt_id      = new_s16opt_id,
            s16opt_wins    = new_s16opt_wins
          )
        }
      }

      # Prune beam to top beam_width by partial EV
      if (length(new_beam) > beam_width) {
        V_cont <- ctx$V_survive[, si + 1]
        pev    <- vapply(new_beam, function(p)
          mean(p$partial_payout + p$cum_survive * V_cont), numeric(1))
        new_beam <- new_beam[order(pev, decreasing = TRUE)[1:beam_width]]
      }

      beam <- new_beam
    }
  }

  # ---- Evaluate all beam paths, pick best EV ----
  best_ev <- -Inf
  best_result <- NULL

  for (bi in seq_along(beam)) {
    path <- beam[[bi]]

    # Reconstruct per-slot survival from picks and bracket_teams
    our_slot_survive <- matrix(0, nrow = n_sims, ncol = n_remaining)
    for (si in seq_along(rem_slots)) {
      rd       <- rem_rounds[si]
      sid      <- rem_slots[si]
      n_pick_s <- HODES_SLOTS[[sid]]$n_picks
      primary_tid <- if (si <= length(path$picks)) path$picks[si] else NA_integer_

      if (is.na(primary_tid)) { our_slot_survive[, si] <- 0; next }

      combined <- as.numeric(tw$team_round_wins[[rd]][ctx$sample_idx, primary_tid])

      if (n_pick_s > 1L) {
        # Add companion picks from bracket_teams with same round, excluding primary
        bt <- path$bracket_teams; br <- path$bracket_rounds
        extras <- bt[br == rd & bt != primary_tid]
        # Exclude s16opt companion (same round 3 but different purpose)
        if (rd == 3L && !is.na(path$s16opt_id)) {
          extras <- setdiff(extras, path$s16opt_id)
        }
        for (etid in extras) {
          combined <- combined * as.numeric(tw$team_round_wins[[rd]][ctx$sample_idx, etid])
        }
      }

      our_slot_survive[, si] <- combined
    }

    # Cumulative survival
    our_cum <- our_slot_survive
    if (n_remaining >= 2L) {
      for (si in 2:n_remaining) our_cum[, si] <- our_cum[, si - 1] * our_slot_survive[, si]
    }
    p_us_all <- our_cum[, n_remaining]

    # Death round per sim
    our_death_rd <- rep(0L, n_sims)
    for (si in seq_along(rem_slots)) {
      alive_before <- if (si == 1L) rep(TRUE, n_sims) else our_cum[, si - 1] > 0
      died_here    <- alive_before & (our_slot_survive[, si] == 0)
      our_death_rd[died_here & our_death_rd == 0L] <- rem_rounds[si]
    }

    # P(field outlasts us from each round d)
    p_outlast_from <- matrix(0, nrow = n_sims, ncol = max_round)
    for (d in 1:max_round) {
      p_outlast_from[, d] <- ctx$p_field_all
      if (d < max_round) {
        for (rd in (d + 1):max_round) {
          p_outlast_from[, d] <- p_outlast_from[, d] + ctx$field_dies_round[, rd]
        }
      }
    }

    # Payout with TB1 tiebreaker adjustment
    payouts      <- numeric(n_sims)
    p_win_per_sim <- numeric(n_sims)

    # Case 1: survived all rounds — effective_prize / k for all k co-survivors
    surv_mask <- (our_death_rd == 0L)
    if (any(surv_mask)) {
      field_co <- ctx$p_field_all[surv_mask] * full_field
      payouts[surv_mask]       <- effective_prize / (1 + field_co)
      p_win_per_sim[surv_mask] <- 1 / (1 + field_co)
    }

    # Case 2: died in round d — no tiebreaker shares; win only if strictly last
    for (d in 1:max_round) {
      mask <- (our_death_rd == d)
      if (!any(mask)) next

      p_outlast    <- p_outlast_from[mask, d]
      field_same_d <- ctx$field_dies_round[mask, d]
      has_tb1      <- path$s16opt_wins[mask]

      if (any(has_tb1)) {
        # TB1: blocked only by field entries that outlast us OR co-die with TB1
        p_no_tie_tb1 <- pmax(
          1 - p_outlast[has_tb1] - field_same_d[has_tb1] * ctx$p_field_tb1[mask][has_tb1], 0
        )^full_field
        payouts[mask][has_tb1]       <- p_no_tie_tb1 * effective_prize
        p_win_per_sim[mask][has_tb1] <- p_no_tie_tb1
      }
      if (any(!has_tb1)) {
        # No TB1: blocked by any field entry that outlasts us OR co-dies same round
        p_no_tie <- pmax(1 - p_outlast[!has_tb1] - field_same_d[!has_tb1], 0)^full_field
        payouts[mask][!has_tb1]       <- p_no_tie * effective_prize
        p_win_per_sim[mask][!has_tb1] <- p_no_tie
      }
    }

    ev <- mean(payouts)

    if (ev > best_ev) {
      best_ev <- ev
      best_result <- list(
        ev              = ev,
        p_survive_today = mean(our_slot_survive[, 1]),
        p_win_contest   = mean(p_win_per_sim),
        p_survive_all   = mean(p_us_all),
        our_death_rd    = our_death_rd,
        s16opt_id       = path$s16opt_id,
        s16opt_wins     = path$s16opt_wins,
        seed_sum        = path$seed_sum,
        our_slot_survive = our_slot_survive
      )
    }
  }

  if (is.null(best_result)) {
    return(list(ev = 0, p_survive_today = 0, p_win_contest = 0,
                p_survive_all = 0, our_death_rd = rep(1L, n_sims),
                seed_sum = 0L, slot1_extra_ids = integer(0),
                s16opt_id = NA_integer_))
  }

  dead_mask <- best_result$our_death_rd > 0
  best_result$mean_death_rd <- if (any(dead_mask)) mean(best_result$our_death_rd[dead_mask]) else 6.5
  best_result$slot1_extra_ids <- slot1_extra_ids
  best_result
}

# ==============================================================================
# STEP 5: PORTFOLIO ALLOCATION
# ==============================================================================

#' Main allocation function: optimize round picks for all 21 entries.
#'
#' @param state data.table from init_hodes_portfolio (or updated state)
#' @param current_round Integer round to optimize for (1-6)
#' @param sim Sim results list
#' @param tw Precomputed team wins
#' @param teams_dt Teams data frame
#' @param own_by_round Named list "1".."6" -> ownership vector from estimate_hodes_ownership
#' @param candidates Optional character vector of team names available this round.
#'   If NULL, uses all teams with non-trivial win probability.
#' @param sim_sample_size Integer sims to use per EV calculation
#' @return data.table allocation with n_assigned per team triple
optimize_hodes_today <- function(state, current_round, sim, tw, teams_dt,
                                  own_by_round, candidates = NULL,
                                  sim_sample_size = 50000L,
                                  pick_s16opt = TRUE) {
  groups <- group_hodes_entries(state)
  if (nrow(groups) == 0) { cat("No alive entries.\n"); return(data.table()) }

  # Subsample sims once (shared across all groups)
  n_sims_full <- sim$n_sims
  sample_idx <- if (n_sims_full > sim_sample_size) sample.int(n_sims_full, sim_sample_size)
                else seq_len(n_sims_full)
  sim_sample_size <- length(sample_idx)

  # Resolve candidate team IDs for current round
  if (!is.null(candidates)) {
    candidate_ids <- teams_dt$team_id[match(candidates, teams_dt$name)]
    candidate_ids <- candidate_ids[!is.na(candidate_ids)]
  } else {
    candidate_ids <- which(tw$team_round_probs[, current_round] > 0.01)
  }

  slot_label <- HODES_SLOTS[[HODES_ROUND_TO_SLOT[as.character(current_round)]]]$label
  cat(sprintf("\n=== OPTIMIZING: %s ===\n", slot_label))
  cat(sprintf("Candidates: %d teams | Alive entries: %d | Groups: %d\n",
              length(candidate_ids), sum(state$alive), nrow(groups)))

  all_scores     <- list()
  death_rd_cache <- list()

  n_picks_current <- HODES_SLOTS[[HODES_ROUND_TO_SLOT[as.character(current_round)]]]$n_picks
  n_companions    <- n_picks_current - 1L  # 2 companions for R1/R2, 0 for later rounds
  # Top-K companions per primary to enumerate; (K choose n_companions) combos each
  # K=10 gives (10 choose 2) = 45 combos per primary — wide enough to capture
  # same-pod pairings even when their R1 win probs aren't top-5
  comp_enum_k     <- if (n_companions >= 1L) 10L else 0L

  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    cat(sprintf("\n--- Group %d/%d: %d entries, %d field ---\n",
                gi, nrow(groups), g$n_entries, g$contest_size))

    avail_cids <- setdiff(candidate_ids, g$used_teams[[1]])
    seen_triples <- character(0)  # dedup within group

    if (n_companions >= 1L) {
      # Full enumeration mode: beam search with companion enumeration (R1/R2 only)
      ctx <- precompute_hodes_context(g, current_round, tw, teams_dt, own_by_round, sample_idx)
      all_comp_pool <- setdiff(which(tw$team_round_probs[, current_round] > 0.01),
                                g$used_teams[[1]])
      n_total_evals <- 0L
      for (cid in avail_cids) {
        comp_pool <- setdiff(all_comp_pool, cid)
        # Filter bracket-compatible companions
        comp_compat <- vapply(comp_pool, function(tid)
          is_hodes_bracket_compatible(tid, current_round, cid, current_round), logical(1))
        comp_pool <- comp_pool[comp_compat]
        if (length(comp_pool) < n_companions) next

        # Rank by R1 win probability, take top K
        comp_probs <- tw$team_round_probs[comp_pool, current_round]
        top_k <- min(comp_enum_k, length(comp_pool))
        top_comp <- comp_pool[order(comp_probs, decreasing = TRUE)[1:top_k]]

        # Generate all (top_k choose n_companions) combos
        combos <- combn(top_comp, n_companions, simplify = FALSE)
        n_total_evals <- n_total_evals + length(combos)
      }
      pg <- make_progress(n_total_evals, sprintf("Grp %d scoring", gi))

      for (cid in avail_cids) {
        comp_pool <- setdiff(all_comp_pool, cid)
        comp_compat <- vapply(comp_pool, function(tid)
          is_hodes_bracket_compatible(tid, current_round, cid, current_round), logical(1))
        comp_pool <- comp_pool[comp_compat]
        if (length(comp_pool) < n_companions) next

        comp_probs <- tw$team_round_probs[comp_pool, current_round]
        top_k <- min(comp_enum_k, length(comp_pool))
        top_comp <- comp_pool[order(comp_probs, decreasing = TRUE)[1:top_k]]
        combos <- combn(top_comp, n_companions, simplify = FALSE)

        for (combo in combos) {
          # Check mutual bracket compatibility among all companions
          all_triple <- c(cid, combo)
          all_compat <- TRUE
          if (n_companions >= 2L) {
            for (ci in 1:(length(combo) - 1)) {
              for (cj in (ci + 1):length(combo)) {
                if (!is_hodes_bracket_compatible(combo[cj], current_round,
                      c(cid, combo[1:ci]), rep(current_round, 1 + ci))) {
                  all_compat <- FALSE; break
                }
              }
              if (!all_compat) break
            }
          }

          if (!all_compat) { pg$tick(); next }

          triple_picks <- sort(all_triple)
          triple_key   <- paste(c(g$group_id, triple_picks), collapse = ":")
          if (triple_key %in% seen_triples) { pg$tick(); next }

          result <- compute_hodes_candidate_ev(cid, ctx, tw, companion_ids = combo,
                                               pick_s16opt = pick_s16opt)
          seen_triples <- c(seen_triples, triple_key)
          death_rd_cache[[triple_key]] <- result$our_death_rd

          extra_names <- paste(teams_dt$name[combo], collapse = " + ")
          s16opt_name <- if (!is.na(result$s16opt_id)) teams_dt$name[result$s16opt_id] else NA_character_

          all_scores[[length(all_scores) + 1]] <- data.table(
            group_id        = g$group_id,
            contest_id      = g$contest_id,
            n_entries       = g$n_entries,
            team_name       = teams_dt$name[cid],
            team_id         = cid,
            triple_key      = triple_key,
            ev              = result$ev,
            p_survive_today = result$p_survive_today,
            p_win_contest   = result$p_win_contest,
            mean_death_rd   = result$mean_death_rd,
            seed_sum        = result$seed_sum,
            extra_names     = extra_names,
            s16opt_name     = s16opt_name
          )
          pg$tick()
        }
      }
      pg$done()
    } else {
      # Single-pick rounds (S16, E8, FF, Champ): MC scoring via solve_optimal_paths_cpp
      n_field <- g$contest_size - g$n_entries
      remaining_rounds <- current_round:6L
      n_alive_mat <- build_hodes_n_alive_matrix(remaining_rounds, tw, own_by_round,
                                                 teams_dt, sample_idx, n_field)
      cat(sprintf("  n_field_alive_matrix summary (rounds %s):\n", paste(remaining_rounds, collapse=",")))
      for (ci in seq_along(remaining_rounds)) {
        col <- n_alive_mat[, ci]
        cat(sprintf("    Round %d: mean=%.2f, median=%.2f, min=%.4f, max=%.2f, pct_zero=%.1f%%\n",
            remaining_rounds[ci], mean(col), median(col), min(col), max(col), 100*mean(col < 0.01)))
      }
      cat(sprintf("  MC scoring %d candidates...\n", length(avail_cids)))
      mc_result <- mc_score_hodes_candidates(g, current_round, sim, teams_dt,
                                              sample_idx, n_alive_mat, avail_cids)
      mc_deaths <- attr(mc_result, "death_rounds")  # matrix: n_cands x n_sims

      for (k in seq_len(nrow(mc_result))) {
        r          <- mc_result[k]
        triple_key <- paste(c(g$group_id, r$team_id), collapse = ":")
        death_rd_cache[[triple_key]] <- if (!is.null(mc_deaths)) mc_deaths[k, ] else integer(sim_sample_size)

        all_scores[[length(all_scores) + 1]] <- data.table(
          group_id        = g$group_id,
          contest_id      = g$contest_id,
          n_entries       = g$n_entries,
          team_name       = r$team_name,
          team_id         = r$team_id,
          triple_key      = triple_key,
          ev              = r$ev,
          p_survive_today = r$p_survive_today,
          p_win_contest   = r$p_win_contest,
          mean_death_rd   = r$mean_death_rd,
          seed_sum        = teams_dt$seed[r$team_id],
          extra_names     = NA_character_,
          s16opt_name     = NA_character_
        )
      }
    }
  }

  scores <- rbindlist(all_scores)
  if (nrow(scores) == 0) { cat("No valid candidates.\n"); return(data.table()) }

  # Print top candidates per group
  for (gi in seq_len(nrow(groups))) {
    g <- groups[gi]
    gs <- scores[group_id == g$group_id]
    if (nrow(gs) == 0) next
    setorder(gs, -ev)
    cat(sprintf("\n  Group %d top 5:\n", gi))
    for (k in 1:min(5, nrow(gs))) {
      r <- gs[k]
      nm <- r$team_name
      if (!is.na(r$extra_names)) nm <- paste0(nm, " + ", r$extra_names)
      cat(sprintf("    %2d. %-40s EV=$%6.2f  P(surv)=%5.1f%%  P(win)=%.3f%%  seed=%d\n",
                  k, nm, r$ev, 100 * r$p_survive_today,
                  100 * r$p_win_contest, r$seed_sum))
    }
  }

  # --- Greedy marginal allocator (mirrors splash_optimizer.R:1467-1516) ---
  allocation_list <- list()
  cat("\n--- Greedy Marginal Allocator ---\n")

  for (gi in seq_len(nrow(groups))) {
    g  <- groups[gi]
    gs <- scores[group_id == g$group_id]
    if (nrow(gs) == 0) next
    setorder(gs, -ev)
    n_ent <- g$n_entries

    # Need at least n_ent unique triples so every entry gets its own combo
    top_viable <- max(n_ent, min(nrow(gs), sum(gs$ev > 0.001)))
    gs <- gs[1:top_viable]

    entry_ids_g <- g$entry_ids[[1]]
    if (n_ent <= 3L || nrow(gs) <= 1L) {
      best <- gs[1]
      allocation_list[[length(allocation_list) + 1]] <- data.table(
        contest_id      = g$contest_id,
        group_id        = g$group_id,
        entry_label     = paste(entry_ids_g, collapse = ", "),
        team_name       = best$team_name,
        team_id         = best$team_id,
        triple_key      = best$triple_key,
        n_assigned      = n_ent,
        ev              = best$ev,
        p_survive_today = best$p_survive_today,
        p_win_contest   = best$p_win_contest,
        seed_sum        = best$seed_sum,
        extra_names     = best$extra_names,
        s16opt_name     = best$s16opt_name
      )
    } else {
      cat(sprintf("  Allocating %d entries for group %d...\n", n_ent, gi))

      ctx <- precompute_hodes_context(g, current_round, tw, teams_dt, own_by_round, sample_idx)
      top_n <- min(nrow(gs), max(n_ent + 10L, 20L))
      top   <- gs[1:top_n]

      # Pre-extract death rounds from cache — keyed by triple_key
      max_round <- 6L
      D <- matrix(0L, nrow = top_n, ncol = sim_sample_size)
      for (k in seq_len(top_n)) {
        ck <- top$triple_key[k]
        if (!is.null(death_rd_cache[[ck]])) D[k, ] <- death_rd_cache[[ck]]
      }
      D[D == 0L] <- max_round + 1L

      # Precompute payout numerator and denominator offset per tier
      max_round_ctx <- ctx$max_round
      Num         <- matrix(0, nrow = sim_sample_size, ncol = max_round_ctx + 1L)
      DenomOffset <- matrix(0, nrow = sim_sample_size, ncol = max_round_ctx + 1L)

      for (tier in 1:(max_round_ctx + 1L)) {
        if (tier == max_round_ctx + 1L) {
          Num[, tier]         <- ctx$effective_prize
          DenomOffset[, tier] <- ctx$p_field_all * ctx$full_field
        } else {
          p_out <- ctx$p_field_all
          if (tier < max_round_ctx) {
            for (rd in (tier + 1):max_round_ctx) p_out <- p_out + ctx$field_dies_round[, rd]
          }
          p_nobody <- (1 - p_out)^ctx$full_field
          Num[, tier]         <- p_nobody * ctx$effective_prize
          DenomOffset[, tier] <- ctx$field_dies_round[, tier] * ctx$full_field
        }
      }

      # Marginal EV allocation: each triple assigned at most once (uniqueness constraint)
      alloc         <- rep(0L, top_n)
      current_best  <- rep(0L, sim_sample_size)
      current_at_best <- rep(0L, sim_sample_size)
      sim_idx       <- seq_len(sim_sample_size)
      ev_jitter     <- 0.03

      for (entry_idx in seq_len(n_ent)) {
        best_k <- NA_integer_; best_mev <- -Inf
        best_k_best <- NULL; best_k_at_best <- NULL

        for (k in seq_len(top_n)) {
          if (alloc[k] > 0L) next   # uniqueness: skip already-allocated triples
          cand_deaths <- D[k, ]
          new_best    <- pmax(current_best, cand_deaths)

          new_at_best <- current_at_best
          higher_mask <- cand_deaths > current_best
          equal_mask  <- cand_deaths == current_best
          new_at_best[higher_mask] <- 1L
          new_at_best[equal_mask]  <- current_at_best[equal_mask] + 1L

          idx_mat  <- cbind(sim_idx, new_best)
          raw_ev   <- sum(Num[idx_mat] * new_at_best / (new_at_best + DenomOffset[idx_mat]))
          noisy_ev <- raw_ev * runif(1, 1 - ev_jitter, 1 + ev_jitter)

          if (noisy_ev > best_mev) {
            best_mev <- noisy_ev; best_k <- k
            best_k_best <- new_best; best_k_at_best <- new_at_best
          }
        }

        if (is.na(best_k)) {
          best_k <- 1L; best_k_best <- pmax(current_best, D[1, ]); best_k_at_best <- current_at_best
        }
        alloc[best_k]   <- alloc[best_k] + 1L
        current_best    <- best_k_best
        current_at_best <- best_k_at_best
      }

      assigned_idx <- 0L
      for (k in seq_len(top_n)) {
        if (alloc[k] > 0L) {
          entry_label_k <- if (length(entry_ids_g) >= assigned_idx + alloc[k])
            paste(entry_ids_g[(assigned_idx + 1L):(assigned_idx + alloc[k])], collapse = ", ")
          else paste(entry_ids_g, collapse = ", ")
          assigned_idx <- assigned_idx + alloc[k]
          allocation_list[[length(allocation_list) + 1]] <- data.table(
            contest_id      = g$contest_id,
            group_id        = g$group_id,
            entry_label     = entry_label_k,
            team_name       = top$team_name[k],
            team_id         = top$team_id[k],
            triple_key      = top$triple_key[k],
            n_assigned      = alloc[k],
            ev              = top$ev[k],
            p_survive_today = top$p_survive_today[k],
            p_win_contest   = top$p_win_contest[k],
            seed_sum        = top$seed_sum[k],
            extra_names     = top$extra_names[k],
            s16opt_name     = top$s16opt_name[k]
          )
        }
      }
    }
  }

  allocation <- rbindlist(allocation_list)

  # Portfolio-level EV
  if (nrow(allocation) > 0) {
    cat("\n--- Computing portfolio-level EVs ---\n")
    port_ev <- evaluate_hodes_portfolio_ev(
      allocation, groups, current_round, sim, tw, teams_dt, own_by_round,
      sim_sample_size = sim_sample_size, death_rd_cache = death_rd_cache,
      scoring_sample_idx = sample_idx
    )
    attr(allocation, "portfolio_ev") <- port_ev
  }

  allocation
}

# ==============================================================================
# STEP 6: PORTFOLIO-LEVEL EV (share-based, self-competition corrected)
# ==============================================================================

#' Evaluate portfolio EV using share-based formula.
#' Mirrors evaluate_portfolio_ev() from splash_optimizer.R.
evaluate_hodes_portfolio_ev <- function(allocation, groups, current_round,
                                         sim, tw, teams_dt, own_by_round,
                                         sim_sample_size = 50000L,
                                         death_rd_cache = NULL,
                                         scoring_sample_idx = NULL) {
  if (!is.null(scoring_sample_idx)) {
    sample_idx <- scoring_sample_idx
    sim_sample_size <- length(sample_idx)
  } else if (sim$n_sims > sim_sample_size) {
    sample_idx <- sample.int(sim$n_sims, sim_sample_size)
  } else {
    sample_idx <- seq_len(sim$n_sims)
    sim_sample_size <- sim$n_sims
  }

  n_teams   <- nrow(teams_dt)
  max_round <- 6L
  results   <- list()

  for (cid in unique(allocation$contest_id)) {
    ct_alloc  <- allocation[contest_id == cid]
    ct_groups <- groups[contest_id == cid]
    prize_pool      <- ct_groups$prize_pool[1]        # raw ($58k)
    winner_fraction <- ct_groups$winner_fraction[1] %||% 0.931
    effective_prize <- prize_pool * winner_fraction   # $53,998
    contest_size <- ct_groups$contest_size[1]
    our_n       <- sum(ct_alloc$n_assigned)
    full_field  <- contest_size - our_n
    n_alloc     <- nrow(ct_alloc)
    alloc_n     <- ct_alloc$n_assigned

    # Collect death_rd per allocation row — keyed by triple_key
    alloc_deaths <- matrix(0L, nrow = n_alloc, ncol = sim_sample_size)
    for (ai in seq_len(n_alloc)) {
      a  <- ct_alloc[ai]
      ck <- if ("triple_key" %in% names(a)) a$triple_key else paste0(a$group_id, ":", a$team_id)
      if (!is.null(death_rd_cache) && !is.null(death_rd_cache[[ck]])) {
        alloc_deaths[ai, ] <- death_rd_cache[[ck]]
      }
    }

    # Field death distribution
    rem_rounds <- current_round:6L
    n_rem      <- length(rem_rounds)
    slot_surv  <- matrix(1, nrow = sim_sample_size, ncol = n_rem)

    for (si in seq_along(rem_rounds)) {
      rd  <- rem_rounds[si]
      own <- own_by_round[[as.character(rd)]]
      if (is.null(own) || length(own) == 0) next
      own_vec <- numeric(n_teams)
      tids <- teams_dt$team_id[match(names(own), teams_dt$name)]
      v    <- !is.na(tids)
      own_vec[tids[v]] <- own[names(own)[v]]
      if (sum(own_vec) < 1e-12) next
      n_pick <- HODES_SLOTS[[HODES_ROUND_TO_SLOT[as.character(rd)]]]$n_picks
      wm <- tw$team_round_wins[[rd]][sample_idx, , drop = FALSE]
      p_wins <- as.numeric(wm %*% own_vec)
      if (rd == 1L) {
        p_single <- p_wins / sum(own_vec)
      } else {
        prev_wm <- tw$team_round_wins[[rd - 1L]][sample_idx, , drop = FALSE]
        p_plays <- as.numeric(prev_wm %*% own_vec)
        p_single <- ifelse(p_plays > 1e-8, p_wins / p_plays, 0)
      }
      slot_surv[, si] <- p_single^n_pick
    }

    cum_surv <- slot_surv
    if (n_rem >= 2L) {
      for (si in 2:n_rem) cum_surv[, si] <- cum_surv[, si - 1] * slot_surv[, si]
    }
    p_field_dies_round <- matrix(0, nrow = sim_sample_size, ncol = max_round)
    for (si in seq_along(rem_rounds)) {
      rd    <- rem_rounds[si]
      p_die <- if (si == 1L) 1 - slot_surv[, si] else cum_surv[, si - 1] * (1 - slot_surv[, si])
      p_field_dies_round[, rd] <- p_field_dies_round[, rd] + p_die
    }
    p_field_all <- cum_surv[, n_rem]

    # Independent EV per allocated path + portfolio discount
    portfolio_ev_raw <- 0
    eff <- alloc_deaths
    eff[eff == 0L] <- max_round + 1L

    for (ai in seq_len(n_alloc)) {
      path_deaths <- eff[ai, ]
      n_entries   <- alloc_n[ai]
      path_ev_sum <- 0

      mask_surv <- which(path_deaths == max_round + 1L)
      if (length(mask_surv) > 0) {
        path_ev_sum <- path_ev_sum +
          sum(effective_prize / (1 + p_field_all[mask_surv] * full_field))
      }
      for (d in 1:max_round) {
        # Died mid-round: no consolation pool; tiebreaker winner gets effective_prize
        mask_d <- which(path_deaths == d)
        if (length(mask_d) == 0) next
        p_out <- p_field_all[mask_d]
        if (d < max_round) {
          for (rd in (d + 1):max_round) p_out <- p_out + p_field_dies_round[mask_d, rd]
        }
        p_nobody   <- (1 - p_out)^full_field
        field_same <- p_field_dies_round[mask_d, d] * full_field
        path_ev_sum <- path_ev_sum + sum(p_nobody * effective_prize / (1 + field_same))
      }
      portfolio_ev_raw <- portfolio_ev_raw + (path_ev_sum / sim_sample_size) * n_entries
    }

    discount_factor <- full_field / contest_size  # self-competition discount
    portfolio_ev    <- portfolio_ev_raw * discount_factor

    cat(sprintf("  [Portfolio EV] %d entries, field=%d, prize=$%s => $%.4f ($%.4f/entry)\n",
                our_n, full_field, format(prize_pool, big.mark = ","),
                portfolio_ev, portfolio_ev / our_n))

    results[[length(results) + 1]] <- data.table(
      contest_id   = cid,
      prize_pool   = prize_pool,
      our_entries  = our_n,
      portfolio_ev = portfolio_ev,
      ev_per_entry = portfolio_ev / our_n,
      pct_of_pool  = 100 * portfolio_ev / prize_pool
    )
  }

  rbindlist(results)
}

# ==============================================================================
# STEP 7: OUTPUT
# ==============================================================================

#' Print allocation recommendation in readable format.
print_hodes_allocation <- function(allocation, teams_dt, current_round = NULL) {
  if (nrow(allocation) == 0) { cat("No allocation.\n"); return(invisible(NULL)) }

  port_ev <- attr(allocation, "portfolio_ev")
  n_pick  <- if (!is.null(current_round)) HODES_SLOTS[[HODES_ROUND_TO_SLOT[as.character(current_round)]]]$n_picks else 1L

  cat("\n========================================================\n")
  cat("          HODES ALLOCATION RECOMMENDATION\n")
  cat("========================================================\n\n")

  setorder(allocation, -n_assigned)

  has_labels <- "entry_label" %in% names(allocation)

  if (n_pick == 3L) {
    cat(sprintf("%-40s %6s %8s %8s %6s %10s\n",
                "Triple (primary + companions)", "Assign", "Rank EV",
                "P(surv)", "P(win)", "Seed Sum"))
    cat(paste(rep("-", 88), collapse = ""), "\n")
    for (i in seq_len(nrow(allocation))) {
      r  <- allocation[i]
      nm <- r$team_name
      if (!is.na(r$extra_names)) nm <- paste0(nm, " + ", r$extra_names)
      lbl <- if (has_labels && !is.na(r$entry_label)) sprintf("  [%s]", r$entry_label) else ""
      cat(sprintf("%-40s %5d  $%6.2f  %6.1f%%  %.3f%%   %4d%s\n",
                  nm, r$n_assigned, r$ev,
                  100 * r$p_survive_today, 100 * r$p_win_contest, r$seed_sum, lbl))
    }
  } else {
    cat(sprintf("%-22s %-30s %8s %8s %6s\n",
                "Entry", "Team", "Rank EV", "P(surv)", "P(win)"))
    cat(paste(rep("-", 80), collapse = ""), "\n")
    for (i in seq_len(nrow(allocation))) {
      r   <- allocation[i]
      lbl <- if (has_labels && !is.na(r$entry_label)) r$entry_label else ""
      cat(sprintf("%-22s %-30s $%6.2f  %6.1f%%  %.3f%%\n",
                  lbl, r$team_name, r$ev,
                  100 * r$p_survive_today, 100 * r$p_win_contest))
    }
  }

  if (!is.null(port_ev) && nrow(port_ev) > 0) {
    total_ev   <- sum(port_ev$portfolio_ev)
    total_pool <- sum(port_ev$prize_pool)
    cat(paste(rep("-", 88), collapse = ""), "\n")
    cat(sprintf("Portfolio EV = $%.4f ($%.4f/entry, %.2f%% of $%s pool)\n",
                total_ev, total_ev / sum(allocation$n_assigned),
                100 * total_ev / total_pool, format(total_pool, big.mark = ",")))
  }

  cat("\n")
  invisible(allocation)
}

# ==============================================================================
# MAIN ENTRY POINT
# ==============================================================================

#' Run the full hodes optimization pipeline for the current round.
#'
#' @param sim_file Path to sim_results RDS file
#' @param current_round Integer round to optimize for (1 = R1, 2 = R2, etc.)
#' @param n_entries Integer our entries (default 21)
#' @param contest_size Integer field size (default 1250)
#' @param prize_pool Numeric prize pool
#' @param state_file Optional path to saved state RDS
#' @param candidates Optional character vector of team names playing this round
#' @param sim_sample_size Integer sims to use per EV calculation
#' @param calibrate Logical: re-run ownership calibration (default FALSE)
#' @param winner_fraction Numeric: fraction of prize pool available to winner(s).
#'   Accounts for charity cut and the consolation pool math:
#'   - 6.9% always goes to charity
#'   - When 2+ survive, 86.2% to winner + 6.9% consolation split among others
#'   - Math: EV per survivor = (86.2% + 6.9%) / k = 93.1% / k for any k
#'   - So effective prize = 0.931 * prize_pool in all EV formulas
#'   Default: 0.931 (93.1%)
#' @return Invisible list: allocation, state, sim, tw, teams_dt, own_by_round
run_hodes_optimizer <- function(sim_file, current_round,
                                 n_entries          = 21L,
                                 contest_size       = 1250L,
                                 prize_pool,
                                 winner_fraction    = 0.931,
                                 state_file         = NULL,
                                 candidates         = NULL,
                                 sim_sample_size    = 50000L,
                                 calibrate          = FALSE,
                                 csv_dir            = "hodes_usage",
                                 entry_own_by_round = NULL,
                                 pick_s16opt        = TRUE,
                                 initial_state      = NULL) {
  pipeline_start <- proc.time()[["elapsed"]]

  # Prize structure:
  #   winner_fraction (93.1%) goes to the winner always.
  #   Consolation addendum: when 2+ entries survive all rounds, non-winning survivors
  #   split an ADDITIONAL 6.9% (extra money, not carved from pool).
  #   => Mid-round deaths: effective prize = winner_fraction * prize_pool ($53,998)
  #   => Contest survivors (2+): EV = prize_pool / k full split ($58k / k)
  cat(sprintf("Prize pool: $%s | Winner: %.1f%% ($%s) | Consolation addendum: %.1f%% ($%s)\n",
              format(prize_pool, big.mark = ","),
              100 * winner_fraction,
              format(round(prize_pool * winner_fraction), big.mark = ","),
              100 * (1 - winner_fraction),
              format(round(prize_pool * (1 - winner_fraction)), big.mark = ",")))

  # Load simulation
  cat(sprintf("Loading sim from %s...\n", basename(sim_file)))
  sim      <- readRDS(sim_file)
  teams_dt <- as.data.table(sim$teams)
  cat(sprintf("  %s sims x %d games, %d teams\n",
              format(sim$n_sims, big.mark = ","),
              ncol(sim$all_results), nrow(teams_dt)))

  # Precompute team wins
  tw <- precompute_team_wins(sim)

  # Load or init portfolio state
  if (!is.null(initial_state)) {
    state <- initial_state
    cat(sprintf("Using provided state: %d entries, %d alive\n",
                nrow(state), sum(state$alive)))
  } else if (!is.null(state_file) && file.exists(state_file)) {
    state <- readRDS(state_file)
    cat(sprintf("Loaded state: %d alive entries\n", sum(state$alive)))
  } else {
    state <- init_hodes_portfolio(n_entries, contest_size, prize_pool,
                                   winner_fraction = winner_fraction)
  }

  # Load ownership calibration
  if (calibrate) {
    params <- load_hodes_calibration(csv_dir = csv_dir)
  } else {
    params <- load_hodes_params()
  }

  # Estimate ownership for all rounds from current onward
  cat("\nEstimating field ownership...\n")

  if (!is.null(entry_own_by_round)) {
    # Use entry-level model ownership provided by caller (e.g. from run_day5_hodes.R).
    # For rounds not covered, fall back to GAM estimation.
    own_by_round <- entry_own_by_round
    covered_rds  <- suppressWarnings(as.integer(setdiff(names(entry_own_by_round), "3_opt")))
    covered_rds  <- covered_rds[!is.na(covered_rds)]
    cat("  Using entry-level ownership for rounds:", paste(sort(covered_rds), collapse = ", "), "\n")

    # GAM fill-in for any uncovered rounds
    field_used <- list()
    for (rd in current_round:6L) {
      if (!as.character(rd) %in% names(own_by_round)) {
        own <- estimate_hodes_ownership(rd, teams_dt, tw, params, field_used)
        own_by_round[[as.character(rd)]] <- own
        cat(sprintf("  Round %d (GAM): ", rd))
      } else {
        own <- own_by_round[[as.character(rd)]]
        cat(sprintf("  Round %d (entry-model): ", rd))
      }
      for (nm in names(own)) {
        prev <- field_used[[nm]] %||% 0
        field_used[[nm]] <- min(1, prev + own[nm] / max(sum(own), 1))
      }
      top_n <- head(sort(own[own > 0.001], decreasing = TRUE), 8)
      cat(paste(sprintf("%s %.0f%%", names(top_n), 100 * top_n), collapse = "  "), "\n")
    }
  } else {
    own_by_round <- list()
    field_used   <- list()

    for (rd in current_round:6L) {
      own <- estimate_hodes_ownership(rd, teams_dt, tw, params, field_used)
      own_by_round[[as.character(rd)]] <- own
      for (nm in names(own)) {
        prev <- field_used[[nm]] %||% 0
        field_used[[nm]] <- min(1, prev + own[nm] / max(sum(own), 1))
      }
      top_own <- sort(own[own > 0.001], decreasing = TRUE)
      top_n <- head(top_own, 10)
      cat(sprintf("  Round %d ownership (top %d):\n", rd, length(top_n)))
      for (j in seq_along(top_n)) {
        cat(sprintf("    %-20s %5.1f%%\n", names(top_n)[j], 100 * top_n[j]))
      }
    }
  }

  # Run optimizer
  allocation <- optimize_hodes_today(
    state, current_round, sim, tw, teams_dt, own_by_round,
    candidates = candidates, sim_sample_size = sim_sample_size,
    pick_s16opt = pick_s16opt
  )

  # Print results
  print_hodes_allocation(allocation, teams_dt, current_round)

  # Pipeline timing
  elapsed <- proc.time()[["elapsed"]] - pipeline_start
  cat(sprintf("=== Done in %.1f%s ===\n", if (elapsed < 60) elapsed else elapsed / 60,
              if (elapsed < 60) "s" else "m"))

  invisible(list(
    allocation   = allocation,
    portfolio_ev = attr(allocation, "portfolio_ev"),
    state        = state,
    sim          = sim,
    tw           = tw,
    teams_dt     = teams_dt,
    own_by_round = own_by_round
  ))
}

cat("Hodes optimizer loaded\n")
cat("Usage: result <- run_hodes_optimizer('sim_results_2026.rds',\n")
cat("         current_round=1, n_entries=21, contest_size=1250, prize_pool=5000)\n")
