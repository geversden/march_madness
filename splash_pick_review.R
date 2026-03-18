library(tidyverse)

years <- 2024:2025

# Day → Round mapping (each round spans 2 days except FF and Champ)
day_to_round <- tibble(
  day_num = 1:10,
  round   = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 6),
  round_label = c("R1 (R64)", "R1 (R64)", "R2 (R32)", "R2 (R32)",
                  "R3 (S16)",  "R3 (S16)",  "R4 (E8)",  "R4 (E8)",
                  "R5 (FF)",   "R6 (Champ)")
)

# ── Load and tidy splash ownership ───────────────────────────────────────────
splash_raw <- map_dfr(years, ~ read_csv(
  paste0("splash_ownership/ownership_", .x, ".csv"),
  show_col_types = FALSE
))

splash <- splash_raw %>%
  mutate(day_num = as.integer(str_extract(day, "\\d+"))) %>%
  left_join(day_to_round, by = "day_num")

# ── Load brackets for seed info ───────────────────────────────────────────────
brackets <- map_dfr(years, ~ read_csv(
  paste0("brackets/bracket_", .x, ".csv"),
  show_col_types = FALSE
) %>% mutate(year = .x))

# Name fixes: splash uses short names; bracket uses full names
name_fixes <- c(
  "McNeese"        = "McNeese State",
  "NC State"       = "NC State",        # already matches
  "UConn"          = "UConn",           # already matches
  "St. John's"     = "St. John's",      # already matches
  "Florida Atlantic" = "Florida Atlantic"
)

splash <- splash %>%
  mutate(team_bracket = recode(team, !!!name_fixes))

# Join seed data
splash_seeded <- splash %>%
  left_join(brackets %>% select(year, team, seed),
            by = c("year", "team_bracket" = "team"))

# Warn about any unmatched teams (seed = NA after join)
unmatched <- splash_seeded %>%
  filter(is.na(seed), !str_detect(day, "day(9|10)")) %>%  # FF/Champ teams won't all be in bracket lookup trivially
  distinct(year, team) %>%
  arrange(year, team)

if (nrow(unmatched) > 0) {
  cat("⚠ Teams without seed match:\n")
  print(unmatched)
  cat("\n")
}

# ── Compute pick% by round (team picks / total picks in that round) ───────────
round_picks <- splash_seeded %>%
  group_by(year, round, round_label) %>%
  mutate(total_round_picks = sum(picks)) %>%
  ungroup() %>%
  mutate(pick_pct = picks / total_round_picks * 100)

# ── Section 1: Pick% by seed × round, averaged across years ──────────────────
seed_summary <- round_picks %>%
  filter(!is.na(seed)) %>%
  group_by(year, round, round_label, seed) %>%
  summarise(seed_pct = sum(pick_pct), .groups = "drop")

avg_by_seed <- seed_summary %>%
  group_by(round_label, seed) %>%
  summarise(avg_pct = round(mean(seed_pct), 1), .groups = "drop") %>%
  pivot_wider(names_from = round_label, values_from = avg_pct,
              names_sort = TRUE) %>%
  arrange(seed)

cat("========================================================\n")
cat(" Avg pick% by seed × round — Splash 2024-2025\n")
cat(" (pick% = team's picks / total round picks × 100)\n")
cat("========================================================\n")
print(avg_by_seed, n = 20)

# ── Section 2: Year-by-year by round ─────────────────────────────────────────
for (rnd in sort(unique(seed_summary$round))) {
  lbl <- unique(seed_summary$round_label[seed_summary$round == rnd])
  cat(sprintf("\n────────────────────────────────────\n %s — by seed\n────────────────────────────────────\n", lbl))

  tbl <- seed_summary %>%
    filter(round == rnd) %>%
    select(year, seed, seed_pct) %>%
    pivot_wider(names_from = year, values_from = seed_pct,
                names_prefix = "") %>%
    arrange(seed) %>%
    mutate(avg = round(rowMeans(across(any_of(as.character(years))), na.rm = TRUE), 1))

  print(tbl, n = 20)
}

# ── Section 3: Top 5 most-picked teams per round per year ────────────────────
cat("\n========================================================\n")
cat(" Top picked teams per round per year\n")
cat("========================================================\n")

top_picks <- round_picks %>%
  filter(!is.na(seed)) %>%
  group_by(year, round, round_label, team, seed) %>%
  summarise(total_pct = sum(pick_pct), .groups = "drop") %>%
  group_by(year, round, round_label) %>%
  slice_max(total_pct, n = 5) %>%
  mutate(label = sprintf("%s (s%d, %.1f%%)", team, seed, total_pct)) %>%
  summarise(top5 = paste(label, collapse = " | "), .groups = "drop") %>%
  arrange(year, round)

for (yr in years) {
  cat(sprintf("\n── %d ──\n", yr))
  yr_data <- top_picks %>% filter(year == yr)
  for (i in seq_len(nrow(yr_data))) {
    cat(sprintf("  %-14s %s\n", yr_data$round_label[i], yr_data$top5[i]))
  }
}
