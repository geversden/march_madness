#!/usr/bin/env Rscript
# ==============================================================================
# calibrate_win_prob.R
# Fit the logistic win-probability parameter (LOG_SCALE) from real data:
#   Predictor:  KenPom AdjEM difference (home_rating - away_rating)
#   Target:     Closing-line implied win probability
# Data: NCAA Tournament 2021-2025 (all rounds, ~63 games x 5 years)
#
# Model:  P(home_wins) = 1 / (1 + exp(-k * (home_rating - away_rating)))
#         logit(P) = k * rating_diff
# ==============================================================================

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

YEARS <- 2021:2025

cat("==============================================================\n")
cat("    CALIBRATING WIN PROBABILITY MODEL\n")
cat("    Using KenPom ratings + closing moneylines (2021-2025)\n")
cat("==============================================================\n\n")

# ==============================================================================
# CLOSING-LINE TEAM NAME -> KENPOM NAME ALIASES
# Most names can be matched by checking if the KenPom name (minus periods)
# appears as a prefix of the closing-line name. This table handles exceptions.
# ==============================================================================

cl_to_kp <- c(
  # Parenthetical / abbreviated schools
  "Loyola (Chi) Ramblers"            = "Loyola Chicago",
  "UConn Huskies"                    = "Connecticut",
  "SIU-Edwardsville Cougars"         = "SIUE",

  # "Ole Miss", "UCF", etc.
  "Ole Miss Rebels"                  = "Mississippi",
  "UCF Knights"                      = "Central Florida",
  "Omaha Mavericks"                  = "Nebraska Omaha",

  # UNC system
  "UNC Greensboro Spartans"          = "UNC Greensboro",
  "UNC Wilmington Seahawks"          = "UNC Wilmington",
  "UNC Asheville Bulldogs"           = "UNC Asheville",

  # UC system
  "UC Santa Barbara Gauchos"         = "UC Santa Barbara",
  "UC San Diego Tritons"             = "UC San Diego",
  "UC Irvine Anteaters"              = "UC Irvine",

  # McNeese (closing lines may omit "State")
  "McNeese Cowboys"                  = "McNeese St.",
  "McNeese State Cowboys"            = "McNeese St.",

  # NC State -> N.C. State in KenPom
  "NC State Wolfpack"                = "N.C. State",

  # Miami disambiguation
  "Miami (FL) Hurricanes"            = "Miami FL",
  "Miami Hurricanes"                 = "Miami FL",

  # Fairleigh Dickinson
  "FDU Knights"                      = "Fairleigh Dickinson",

  # Texas A&M Corpus Christi
  "Texas A&M-CC Islanders"           = "Texas A&M Corpus Chris",

  # Grambling
  "Grambling Tigers"                 = "Grambling St.",
  "Grambling State Tigers"           = "Grambling St.",

  # "State" suffix sometimes missing or different
  "Boise State Broncos"              = "Boise St.",
  "Kent State Golden Flashes"        = "Kent St.",
  "CSU Fullerton Titans"             = "Cal St. Fullerton",

  # Mount / Saint / St. variants
  "Mt. St. Mary's Mountaineers"      = "Mount St. Mary's",
  "St. Peter's Peacocks"             = "Saint Peter's",
  "Saint Peter's Peacocks"           = "Saint Peter's",

  # Long Beach
  "Long Beach State 49ers"           = "Long Beach St.",
  "Long Beach St 49ers"              = "Long Beach St.",

  # SE Missouri
  "SE Missouri State Redhawks"       = "Southeast Missouri St."
)

# ==============================================================================
# MATCH A CLOSING-LINE TEAM NAME TO ITS KENPOM NAME
# ==============================================================================

match_to_kenpom <- function(cl_name, kp_names) {
  # 1. Explicit alias
  if (cl_name %in% names(cl_to_kp)) {
    alias <- cl_to_kp[[cl_name]]
    if (alias %in% kp_names) return(alias)
    # Try with periods removed
    idx <- which(gsub("\\.", "", kp_names) == gsub("\\.", "", alias))
    if (length(idx) > 0) return(kp_names[idx[1]])
  }

  # 2. Prefix matching (normalised: lowercase, no periods)
  cl_norm <- tolower(gsub("\\.", "", cl_name))
  kp_norms <- tolower(gsub("\\.", "", kp_names))

  # Try longest KenPom name first to avoid partial matches
  for (i in order(nchar(kp_norms), decreasing = TRUE)) {
    kn <- kp_norms[i]
    if (startsWith(cl_norm, kn) &&
        (nchar(cl_norm) == nchar(kn) ||
         substr(cl_norm, nchar(kn) + 1, nchar(kn) + 1) == " ")) {
      return(kp_names[i])
    }
  }

  return(NA_character_)
}

# ==============================================================================
# LOAD DATA AND BUILD TRAINING SET
# ==============================================================================

all_games <- data.frame()

for (yr in YEARS) {
  cat(sprintf("--- %d ---\n", yr))

  # Load KenPom
  kp_file <- file.path(script_dir, "kenpom_data", sprintf("kenpom_%d.csv", yr))
  if (!file.exists(kp_file)) { cat("  KenPom file not found, skipping\n"); next }

  kp <- read.csv(kp_file, stringsAsFactors = FALSE)
  kp <- kp[!is.na(kp$Team) & kp$Team != "" & kp$Team != "Team", ]
  kp$Team <- gsub("\\s*\\d+$", "", trimws(kp$Team))
  kp$NetRtg <- as.numeric(kp$NetRtg)
  kp <- kp[!is.na(kp$NetRtg), ]
  kp_lookup <- setNames(kp$NetRtg, kp$Team)
  kp_names  <- names(kp_lookup)

  cat(sprintf("  Loaded %d KenPom teams\n", length(kp_names)))

  # Load closing lines
  cl_file <- file.path(script_dir, "closing_lines",
                        sprintf("ncaat_%d_closing_lines.csv", yr))
  if (!file.exists(cl_file)) { cat("  Closing lines file not found, skipping\n"); next }

  cl <- read.csv(cl_file, stringsAsFactors = FALSE)
  cat(sprintf("  Loaded %d games from closing lines\n", nrow(cl)))

  # Match teams
  unmatched <- character(0)
  games     <- data.frame()

  for (i in seq_len(nrow(cl))) {
    home_cl <- cl$home_team[i]
    away_cl <- cl$away_team[i]

    home_kp <- match_to_kenpom(home_cl, kp_names)
    away_kp <- match_to_kenpom(away_cl, kp_names)

    if (is.na(home_kp)) unmatched <- c(unmatched, home_cl)
    if (is.na(away_kp)) unmatched <- c(unmatched, away_cl)

    if (!is.na(home_kp) && !is.na(away_kp)) {
      games <- rbind(games, data.frame(
        year        = yr,
        home_team   = home_cl,
        away_team   = away_cl,
        home_rating = kp_lookup[[home_kp]],
        away_rating = kp_lookup[[away_kp]],
        market_prob = cl$home_win_prob[i],
        spread      = cl$home_spread[i],
        stringsAsFactors = FALSE
      ))
    }
  }

  if (length(unmatched) > 0) {
    cat(sprintf("  WARNING: %d unmatched team name(s):\n", length(unique(unmatched))))
    for (u in unique(unmatched)) cat(sprintf("    - \"%s\"\n", u))
  }

  cat(sprintf("  Matched %d / %d games\n\n", nrow(games), nrow(cl)))
  all_games <- rbind(all_games, games)
}

cat(sprintf("Total training data: %d games across %d years\n\n",
            nrow(all_games), length(YEARS)))

if (nrow(all_games) == 0) stop("No matched games found — check name mappings")

# ==============================================================================
# COMPUTE FEATURES
# ==============================================================================

all_games$rating_diff <- all_games$home_rating - all_games$away_rating
all_games$logit_prob  <- log(all_games$market_prob / (1 - all_games$market_prob))

# ==============================================================================
# FIT MODEL: logit(market_prob) = k * rating_diff
# ==============================================================================

cat("==============================================================\n")
cat("    MODEL FIT: logit(P) = k * rating_diff\n")
cat("==============================================================\n\n")

# No-intercept model (pure rating difference -> probability)
fit_no_int <- lm(logit_prob ~ 0 + rating_diff, data = all_games)
k_no_int   <- coef(fit_no_int)[["rating_diff"]]

cat("No-intercept model:\n")
cat(sprintf("  LOG_SCALE (k) = %.6f\n", k_no_int))
cat(sprintf("  R-squared     = %.4f\n", summary(fit_no_int)$r.squared))
cat(sprintf("  Std. error    = %.6f\n",
            summary(fit_no_int)$coefficients["rating_diff", "Std. Error"]))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n",
            confint(fit_no_int)["rating_diff", 1],
            confint(fit_no_int)["rating_diff", 2]))

cat("\n")

# With intercept (captures any systematic home/away bias)
fit_with_int <- lm(logit_prob ~ rating_diff, data = all_games)
k_with_int   <- coef(fit_with_int)[["rating_diff"]]
intercept    <- coef(fit_with_int)[["(Intercept)"]]

cat("With-intercept model:\n")
cat(sprintf("  LOG_SCALE (k) = %.6f\n", k_with_int))
cat(sprintf("  Intercept     = %.6f  (implies %.1f%% win prob at diff=0)\n",
            intercept, 100 / (1 + exp(-intercept))))
cat(sprintf("  Adj R-squared = %.4f\n", summary(fit_with_int)$adj.r.squared))
cat(sprintf("  Intercept p   = %.4f  %s\n",
            summary(fit_with_int)$coefficients["(Intercept)", "Pr(>|t|)"],
            ifelse(summary(fit_with_int)$coefficients["(Intercept)", "Pr(>|t|)"] < 0.05,
                   "(significant)", "(not significant)")))

# ==============================================================================
# COMPARE WITH CURRENT LOG_SCALE = 0.09
# ==============================================================================

cat("\n==============================================================\n")
cat("    COMPARISON: CURRENT vs FITTED\n")
cat("==============================================================\n\n")

current_k <- 0.09

cat(sprintf("Current LOG_SCALE:    %.4f\n", current_k))
cat(sprintf("Fitted LOG_SCALE:     %.4f\n", k_no_int))
cat(sprintf("Difference:           %+.4f (%.1f%%)\n",
            k_no_int - current_k,
            100 * (k_no_int - current_k) / current_k))

cat("\nSample matchup predictions (rating_diff -> win_prob):\n")
cat(sprintf("  %5s  %10s  %10s  %10s\n", "Diff", "Current", "Fitted", "Delta"))
cat(sprintf("  %5s  %10s  %10s  %10s\n", "-----", "-------", "------", "-----"))
for (d in c(5, 10, 12, 15, 20, 25, 30, 35)) {
  p_old <- 1 / (1 + exp(-current_k * d))
  p_new <- 1 / (1 + exp(-k_no_int * d))
  cat(sprintf("  %5d  %9.1f%%  %9.1f%%  %+8.1f pp\n",
              d, 100 * p_old, 100 * p_new, 100 * (p_new - p_old)))
}

# ==============================================================================
# RESIDUAL DIAGNOSTICS
# ==============================================================================

cat("\n==============================================================\n")
cat("    RESIDUAL DIAGNOSTICS\n")
cat("==============================================================\n\n")

# Predicted probabilities using fitted k
all_games$pred_prob_fitted  <- 1 / (1 + exp(-k_no_int  * all_games$rating_diff))
all_games$pred_prob_current <- 1 / (1 + exp(-current_k * all_games$rating_diff))

all_games$resid_fitted  <- all_games$market_prob - all_games$pred_prob_fitted
all_games$resid_current <- all_games$market_prob - all_games$pred_prob_current

cat("Fitted model:\n")
cat(sprintf("  MAE:  %.4f (%.2f pp)\n",
            mean(abs(all_games$resid_fitted)),
            100 * mean(abs(all_games$resid_fitted))))
cat(sprintf("  RMSE: %.4f (%.2f pp)\n",
            sqrt(mean(all_games$resid_fitted^2)),
            100 * sqrt(mean(all_games$resid_fitted^2))))
cat(sprintf("  Mean: %+.5f\n", mean(all_games$resid_fitted)))

cat("\nCurrent model (k=0.09):\n")
cat(sprintf("  MAE:  %.4f (%.2f pp)\n",
            mean(abs(all_games$resid_current)),
            100 * mean(abs(all_games$resid_current))))
cat(sprintf("  RMSE: %.4f (%.2f pp)\n",
            sqrt(mean(all_games$resid_current^2)),
            100 * sqrt(mean(all_games$resid_current^2))))
cat(sprintf("  Mean: %+.5f\n", mean(all_games$resid_current)))

cat("\nResiduals by rating_diff bucket (fitted model):\n")
cat(sprintf("  %-18s  %7s  %7s  %5s\n", "Bucket", "Mean", "MAE", "N"))
cat(sprintf("  %-18s  %7s  %7s  %5s\n", "------------------", "-------", "-------", "-----"))
breaks <- c(-Inf, -10, -5, 0, 5, 10, 20, 35, Inf)
all_games$bucket <- cut(all_games$rating_diff, breaks)

for (b in levels(all_games$bucket)) {
  sub <- all_games[all_games$bucket == b, ]
  if (nrow(sub) > 0) {
    cat(sprintf("  %-18s  %+.4f  %.4f  %5d\n",
                b, mean(sub$resid_fitted), mean(abs(sub$resid_fitted)), nrow(sub)))
  }
}

# ==============================================================================
# SUMMARY
# ==============================================================================

cat(sprintf("\n==============================================================\n"))
cat(sprintf("  RECOMMENDATION: Update LOG_SCALE from %.4f to %.4f\n", current_k, round(k_no_int, 4)))
cat(sprintf("  Based on %d NCAA Tournament games (2021-2025)\n", nrow(all_games)))
cat(sprintf("==============================================================\n"))
