#!/usr/bin/env Rscript
# ==============================================================================
# run_calibration.R
#
# Runs the entry-level ownership model calibration in memory-safe steps:
# 1. Process each year separately (saves to RDS to avoid 2x sim matrix in memory)
# 2. Load saved results and run MLE calibration
#
# Usage:
#   source("R/run_calibration.R")
# ==============================================================================

library(data.table)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) {
  if (file.exists("R/splash_config.R")) "R"
  else if (file.exists("splash_config.R")) "."
  else stop("Cannot determine script_dir.")
})

# Source dependencies (functions only, skip auto-run)
.ANALYZE_SKIP_AUTO_RUN <- TRUE
source(file.path(script_dir, "splash_config.R"))
source(file.path(script_dir, "splash_ownership.R"))
source(file.path(script_dir, "splash_prepare.R"))
source(file.path(script_dir, "splash_optimizer.R"))
source(file.path(script_dir, "analyze_historical_paths.R"))
source(file.path(script_dir, "entry_ownership_model.R"))

base_dir <- file.path(script_dir, "..")
team_names_csv <- file.path(base_dir, "team_names.csv")

# ==============================================================================
# STEP 1: Analyze each year (one at a time to manage memory)
# ==============================================================================

cat("=== Processing 2024 ===\n")
r2024 <- analyze_year(
  year = 2024L,
  results_file = file.path(base_dir, "results_2024.rds"),
  bracket_file = file.path(base_dir, "brackets", "bracket_2024.csv"),
  sim_file = file.path(base_dir, "sim_results_2024.rds"),
  team_names_csv = team_names_csv
)
saveRDS(r2024, file.path(base_dir, "analysis_2024.rds"))
cat(sprintf("Saved analysis_2024.rds (%d S16, %d E8 calib rows)\n",
            nrow(r2024$s16_calib), nrow(r2024$e8_calib)))

# Force garbage collection before loading next year's sim
gc(verbose = FALSE)

cat("\n=== Processing 2025 ===\n")
r2025 <- analyze_year(
  year = 2025L,
  results_file = file.path(base_dir, "results_2025.rds"),
  bracket_file = file.path(base_dir, "brackets", "bracket_2025.csv"),
  sim_file = file.path(base_dir, "sim_results_2025.rds"),
  team_names_csv = team_names_csv
)
saveRDS(r2025, file.path(base_dir, "analysis_2025.rds"))
cat(sprintf("Saved analysis_2025.rds (%d S16, %d E8 calib rows)\n",
            nrow(r2025$s16_calib), nrow(r2025$e8_calib)))

# ==============================================================================
# STEP 2: Calibrate
# ==============================================================================

cat("\n=== Calibrating ===\n")

historical_analysis <- list(
  results_2024 = r2024,
  results_2025 = r2025
)

params <- calibrate_entry_model(historical_analysis, n_restarts = 10)

cat("\n=== FITTED PARAMETERS ===\n")
cat(sprintf("S16: beta_wp=%.3f, beta_save=%.3f, beta_path=%.3f\n",
            params$beta_wp_S16, params$beta_save_S16, params$beta_path_S16))
cat(sprintf("E8:  beta_wp=%.3f, beta_save=%.3f, beta_path=%.3f\n",
            params$beta_wp_E8, params$beta_save_E8, params$beta_path_E8))

# Save calibrated parameters
saveRDS(params, file.path(base_dir, "entry_model_params.rds"))
cat("\nSaved entry_model_params.rds\n")
