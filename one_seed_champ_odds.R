#!/usr/bin/env Rscript
# ==============================================================================
# one_seed_champ_odds.R
# Compare 1-seed championship probabilities across 2024-2026 simulations.
# For 2026, also shows FanDuel market implied odds.
# ==============================================================================

library(data.table)

script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")

YEARS <- 2024:2026

# ==============================================================================
# LOAD SIM RESULTS
# ==============================================================================

load_sim_champ <- function(year) {
  rds <- file.path(script_dir, sprintf("sim_results_%d.rds", year))
  if (!file.exists(rds)) {
    cat(sprintf("  WARNING: %s not found\n", basename(rds)))
    return(NULL)
  }
  sim <- readRDS(rds)
  champ_game <- sim$all_results[, 63]
  counts     <- tabulate(champ_game, nbins = nrow(sim$teams))
  teams      <- as.data.table(sim$teams)
  teams[, champ_prob := counts[team_id] / sim$n_sims]
  teams[seed == 1, .(team = name, region, seed, champ_prob, year = year)]
}

sim_list <- lapply(YEARS, load_sim_champ)
sim_dt   <- rbindlist(sim_list[!sapply(sim_list, is.null)])

# ==============================================================================
# LOAD 2026 MARKET ODDS (FanDuel)
# ==============================================================================

fd_file <- file.path(script_dir, "fd_ncaa_futures_latest.csv")
fd_ones <- NULL
if (file.exists(fd_file)) {
  fd <- fread(fd_file)
  fd_ones <- fd[seed == 1, .(team, region, fd_champ = champion)]
}

# ==============================================================================
# PRINT RESULTS
# ==============================================================================

cat("==============================================================\n")
cat("  1-SEED CHAMPIONSHIP ODDS BY YEAR\n")
cat("==============================================================\n\n")

for (yr in YEARS) {
  g <- sim_dt[year == yr][order(region)]
  if (nrow(g) == 0) next

  total_sim <- sum(g$champ_prob)

  cat(sprintf("--- %d (n_sims = %s) ---\n", yr,
              format(readRDS(file.path(script_dir,
                sprintf("sim_results_%d.rds", yr)))$n_sims, big.mark = ",")))

  if (yr == 2026 && !is.null(fd_ones)) {
    g <- merge(g, fd_ones[, .(team, fd_champ)], by = "team", all.x = TRUE)
    cat(sprintf("  %-20s  %8s  %8s  %8s  %8s\n",
                "Team", "Region", "Sim%", "Mkt%", "Sim/Mkt"))
    cat(paste(rep("-", 60), collapse = ""), "\n")
    setorder(g, -champ_prob)
    for (i in seq_len(nrow(g))) {
      ratio <- if (!is.na(g$fd_champ[i]) && g$fd_champ[i] > 0)
                 g$champ_prob[i] / g$fd_champ[i] else NA_real_
      ratio_str <- if (is.na(ratio)) "   —  " else sprintf("%+.2fx", ratio)
      cat(sprintf("  %-20s  %8s  %7.2f%%  %7.2f%%  %8s\n",
                  g$team[i], g$region[i],
                  100 * g$champ_prob[i],
                  100 * ifelse(is.na(g$fd_champ[i]), 0, g$fd_champ[i]),
                  ratio_str))
    }
  } else {
    cat(sprintf("  %-20s  %8s  %8s\n", "Team", "Region", "Sim%"))
    cat(paste(rep("-", 42), collapse = ""), "\n")
    setorder(g, -champ_prob)
    for (i in seq_len(nrow(g))) {
      cat(sprintf("  %-20s  %8s  %7.2f%%\n",
                  g$team[i], g$region[i], 100 * g$champ_prob[i]))
    }
  }

  cat(sprintf("  %s\n", paste(rep("-", if (yr == 2026) 60 else 42), collapse = "")))
  cat(sprintf("  %-20s  %8s  %7.2f%%\n\n", "TOTAL (all 4 ones)", "",
              100 * total_sim))
}

# ==============================================================================
# CROSS-YEAR COMPARISON
# ==============================================================================

cat("==============================================================\n")
cat("  COMBINED 1-SEED CHAMPIONSHIP % (all 4 seeds combined)\n")
cat("==============================================================\n\n")

totals <- sim_dt[, .(total_pct = 100 * sum(champ_prob)), by = year][order(year)]
cat(sprintf("  %s  %s\n", "Year", "All-4 Sim%"))
cat(paste(rep("-", 20), collapse = ""), "\n")
for (i in seq_len(nrow(totals))) {
  cat(sprintf("  %d    %6.2f%%\n", totals$year[i], totals$total_pct[i]))
}

cat("\n==============================================================\n")
cat("  BY REGION — 1-SEED CHAMPIONSHIP % ACROSS YEARS\n")
cat("==============================================================\n\n")

regions <- c("East", "West", "South", "Midwest")
cat(sprintf("  %-20s", "Region/Team"))
for (yr in YEARS) cat(sprintf("  %6d", yr))
cat("\n")
cat(paste(rep("-", 20 + 9 * length(YEARS)), collapse = ""), "\n")

for (reg in regions) {
  cat(sprintf("  %-20s", reg))
  for (yr in YEARS) {
    row <- sim_dt[year == yr & region == reg]
    if (nrow(row) == 0) { cat("       —"); next }
    cat(sprintf("  %5.2f%%", 100 * row$champ_prob[1]))
  }
  cat(sprintf("   (%s)", paste(sim_dt[region == reg, team], collapse = " / ")))
  cat("\n")
}
cat("\n")
