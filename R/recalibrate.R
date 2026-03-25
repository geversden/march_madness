#!/usr/bin/env Rscript
# Lightweight recalibration script - loads saved analysis data, runs MLE only
library(data.table)
source("R/entry_ownership_model.R")

r2024 <- readRDS("analysis_2024.rds")
r2025 <- readRDS("analysis_2025.rds")

# Drop heavy data we don't need for calibration
r2024[["entry_wide"]] <- NULL
r2025[["entry_wide"]] <- NULL
r2024[["q1_s16_pick_against"]] <- NULL
r2025[["q1_s16_pick_against"]] <- NULL
r2024[["q4_dead_paths"]] <- NULL
r2025[["q4_dead_paths"]] <- NULL

ha <- list(results_2024 = r2024, results_2025 = r2025)
params <- calibrate_entry_model(ha, n_restarts = 10)

cat(sprintf("\nS16: beta_wp=%.3f, beta_save=%.3f, beta_path=%.3f\n",
    params[["beta_wp_S16"]], params[["beta_save_S16"]], params[["beta_path_S16"]]))
cat(sprintf("E8:  beta_wp=%.3f, beta_save=%.3f, beta_path=%.3f\n",
    params[["beta_wp_E8"]], params[["beta_save_E8"]], params[["beta_path_E8"]]))

saveRDS(params, "entry_model_params.rds")
cat("Saved entry_model_params.rds\n")
