source("R/analyze_hodes_historical.R")
source("R/hodes_entry_model.R")

cat("=== Analyzing Hodes Historical Data ===\n")
all_data <- analyze_all_hodes_years()

cat("\n=== Calibrating Hodes Entry Model ===\n")
params <- calibrate_hodes_model(all_data)

cat("\n=== Done ===\n")
