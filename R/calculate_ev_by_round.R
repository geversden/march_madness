library(tidyverse)

# =============================================================================
# NCAA SURVIVOR POOL — ROUND 1 EV CALCULATOR (Pick 3)
# Uses pre-built tournament sims from RDS + real field entries
# =============================================================================

# --- 1. LOAD SIMS ------------------------------------------------------------
data <- readRDS("/home/joes/projects/march_madness/adjusted_2026_sims.rds")

all_results <- data$all_results
teams <- data$teams
round_info <- data$round_info
bracket_order <- data$bracket_order
n_sims <- data$n_sims

cat("Loaded", format(n_sims, big.mark = ","), "simulations\n")

# --- 2. MAP R64 GAMES --------------------------------------------------------
r64_games <- round_info %>% filter(round_name == "R64")

r64_matchups <- tibble(
  game_col = r64_games$game_col,
  team1_id = bracket_order[seq(1, 63, by = 2)],
  team2_id = bracket_order[seq(2, 64, by = 2)]
) %>%
  left_join(teams %>% select(team_id, name), by = c("team1_id" = "team_id")) %>%
  rename(team1_name = name) %>%
  left_join(teams %>% select(team_id, name), by = c("team2_id" = "team_id")) %>%
  rename(team2_name = name)

# --- 3. EXTRACT R64 WINNERS --------------------------------------------------
r64_cols <- r64_matchups$game_col
r64_results <- all_results[, r64_cols]
team1_ids <- r64_matchups$team1_id
team2_ids <- r64_matchups$team2_id

# all_results stores the winning team_id directly
r64_winners <- r64_results

cat("\n=== VERIFYING SIM ENCODING ===\n")
for (g in 1:3) {
  vals <- sort(unique(r64_winners[1:10000, g]))
  cat(sprintf(
    "  Game %d (%s vs %s): winner IDs = %s\n",
    g,
    teams$name[team1_ids[g]],
    teams$name[team2_ids[g]],
    paste(vals, collapse = ", ")
  ))
}

# --- 4. TEAM LOOKUPS ---------------------------------------------------------
team_name_to_id <- deframe(teams %>% select(name, team_id))

team_to_r64_game <- integer(max(teams$team_id))
for (g in 1:32) {
  team_to_r64_game[team1_ids[g]] <- g
  team_to_r64_game[team2_ids[g]] <- g
}

# --- 5. LOAD FIELD ENTRIES ----------------------------------------------------
field_raw <- read.delim(
  "/home/joes/projects/march_madness/field_entries.tsv",
  header = FALSE,
  sep = "\t",
  stringsAsFactors = FALSE,
  fill = TRUE
)

field_entries_df <- field_raw %>%
  as_tibble() %>%
  transmute(
    entry_name = V2,
    pick1 = str_trim(V3),
    pick2 = str_trim(V4),
    pick3 = str_trim(V5)
  ) %>%
  filter(!is.na(pick1), pick1 != "")

cat("\nParsed", nrow(field_entries_df), "field entries\n")

# --- 6. NAME FIXING ----------------------------------------------------------
entry_name_fixes <- c(
  "Michigan St" = "Michigan State",
  "St John's" = "St. John's",
  "N Dakota St" = "North Dakota State",
  "McNeese" = "McNeese State"
  # Add more after checking output
)

sim_team_names <- sort(teams$name)

for (old_name in names(entry_name_fixes)) {
  new_name <- entry_name_fixes[[old_name]]
  if (new_name %in% sim_team_names) {
    field_entries_df$pick1[field_entries_df$pick1 == old_name] <- new_name
    field_entries_df$pick2[field_entries_df$pick2 == old_name] <- new_name
    field_entries_df$pick3[field_entries_df$pick3 == old_name] <- new_name
    cat("  Fixed: '", old_name, "' -> '", new_name, "'\n")
  }
}

# Check for unmatched
field_team_names <- sort(unique(c(
  field_entries_df$pick1,
  field_entries_df$pick2,
  field_entries_df$pick3
)))
field_missing <- setdiff(field_team_names, teams$name)
if (length(field_missing) > 0) {
  cat("\n⚠️  Field names not in sims:\n")
  for (m in field_missing) {
    close <- teams$name[agrep(m, teams$name, max.distance = 0.3)]
    cat("  '", m, "' -> maybe: ", paste(close, collapse = ", "), "\n")
  }
} else {
  cat("\n✓ All field names matched.\n")
}

# --- 7. MAP FIELD TO TEAM IDs ------------------------------------------------
field_entries_df <- field_entries_df %>%
  mutate(
    id1 = team_name_to_id[pick1],
    id2 = team_name_to_id[pick2],
    id3 = team_name_to_id[pick3],
    valid = !is.na(id1) & !is.na(id2) & !is.na(id3)
  )

cat(
  "Valid field entries:",
  sum(field_entries_df$valid),
  "of",
  nrow(field_entries_df),
  "\n"
)

valid_field <- field_entries_df %>% filter(valid)
n_field <- nrow(valid_field)

# Build game/tid matrices
field_game_matrix <- matrix(0L, nrow = n_field, ncol = 3)
field_tid_matrix <- matrix(0L, nrow = n_field, ncol = 3)

for (i in 1:n_field) {
  tids <- c(valid_field$id1[i], valid_field$id2[i], valid_field$id3[i])
  field_tid_matrix[i, ] <- tids
  field_game_matrix[i, ] <- team_to_r64_game[tids]
}

# --- 8. IDENTIFY OUR ENTRIES IN THE FIELD -------------------------------------
our_last_names <- c(
  "SYDLOWSKI",
  "LONGO",
  "EVERSDEN",
  "JONES",
  "HORCICIAK",
  "GELLNER",
  "SHORE"
)

our_entries <- valid_field %>%
  mutate(field_idx = row_number()) %>%
  filter(str_detect(
    str_to_upper(entry_name),
    paste(our_last_names, collapse = "|")
  ))

cat("\n=== OUR ENTRIES FOUND IN FIELD ===\n")
our_entries %>% select(entry_name, pick1, pick2, pick3) %>% print(n = Inf)
cat("Found", nrow(our_entries), "of our entries\n")

our_field_idx <- our_entries$field_idx

# --- 9. FIELD OWNERSHIP ------------------------------------------------------
field_all_picks <- c(valid_field$pick1, valid_field$pick2, valid_field$pick3)
field_ownership <- table(field_all_picks) / n_field

cat("\n=== FIELD OWNERSHIP (from", n_field, "entries) ===\n")
tibble(team = names(field_ownership), own = as.numeric(field_ownership)) %>%
  arrange(desc(own)) %>%
  mutate(own_pct = scales::percent(own, accuracy = 0.1)) %>%
  print(n = 50)

# =============================================================================
# 10. COMPUTE EV FOR ALL FIELD ENTRIES
# For each sim:
#   - Determine 32 R64 winners
#   - Count n_survivors among ALL n_field entries
#   - Each surviving entry gets pot_share = n_field / n_survivors
#   - Average across all sims = EV
# =============================================================================

cat(
  "\nComputing EV for all",
  n_field,
  "entries across",
  format(n_sims, big.mark = ","),
  "sims...\n"
)

ev_accum <- numeric(n_field)
survival_count <- integer(n_field)

chunk_size <- 50000
n_chunks <- ceiling(n_sims / chunk_size)

pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)

for (ch in 1:n_chunks) {
  s <- (ch - 1) * chunk_size + 1
  e <- min(ch * chunk_size, n_sims)
  chunk_n <- e - s + 1
  cw <- r64_winners[s:e, , drop = FALSE]

  # Count field survivors (batched for memory)
  n_fs <- integer(chunk_n)
  fb <- 200
  for (fbs in seq(1, n_field, by = fb)) {
    fbe <- min(fbs + fb - 1, n_field)
    fi <- fbs:fbe
    fn <- length(fi)
    fs <- matrix(TRUE, nrow = chunk_n, ncol = fn)
    for (p in 1:3) {
      pw <- cw[, field_game_matrix[fi, p], drop = FALSE]
      pn <- matrix(
        field_tid_matrix[fi, p],
        nrow = chunk_n,
        ncol = fn,
        byrow = TRUE
      )
      fs <- fs & (pw == pn)
    }
    n_fs <- n_fs + rowSums(fs)
  }

  # Now compute EV for OUR entries only (they're already in n_fs count)
  pot_share <- ifelse(n_fs > 0, n_field / n_fs, 0)

  for (i in seq_along(our_field_idx)) {
    idx <- our_field_idx[i]
    alive <- (cw[, field_game_matrix[idx, 1]] == field_tid_matrix[idx, 1]) &
      (cw[, field_game_matrix[idx, 2]] == field_tid_matrix[idx, 2]) &
      (cw[, field_game_matrix[idx, 3]] == field_tid_matrix[idx, 3])
    survival_count[idx] <- survival_count[idx] + sum(alive)
    ev_accum[idx] <- ev_accum[idx] + sum(pot_share[alive])
  }

  setTxtProgressBar(pb, ch)
}
close(pb)

# --- 11. OUR ENTRY RESULTS ---------------------------------------------------
sim_win_probs <- sapply(1:max(teams$team_id), function(tid) {
  g <- team_to_r64_game[tid]
  if (g == 0) {
    return(NA_real_)
  }
  mean(r64_winners[, g] == tid)
})
names(sim_win_probs) <- teams$name[1:length(sim_win_probs)]

our_results <- our_entries %>%
  mutate(
    survival_prob = map_dbl(field_idx, function(fi) {
      tids <- field_tid_matrix[fi, ]
      prod(sapply(tids, function(tid) {
        g <- team_to_r64_game[tid]
        mean(r64_winners[, g] == tid)
      }))
    }),
    avg_field_own = map_dbl(field_idx, function(fi) {
      picks <- c(
        valid_field$pick1[fi],
        valid_field$pick2[fi],
        valid_field$pick3[fi]
      )
      mean(as.numeric(field_ownership[picks]), na.rm = TRUE)
    }),
    sim_survival = survival_count[field_idx] / n_sims,
    ev = ev_accum[field_idx] / n_sims
  ) %>%
  arrange(desc(ev))

# Extract owner name from entry_name (before the /)
our_results <- our_results %>%
  mutate(owner = str_extract(entry_name, "^[^/]+"))

cat("\n")
cat(
  "╔══════════════════════════════════════════════════════════════════════╗\n"
)
cat("║               ENTRY EV RESULTS — ROUND 1 (Pick 3)                 ║\n")
cat("║   EV > 1.0 = better than average | EV < 1.0 = worse than avg     ║\n")
cat(sprintf(
  "║   Field: %d entries | Sims: %s                             ║\n",
  n_field,
  format(n_sims, big.mark = ",")
))
cat(
  "╚══════════════════════════════════════════════════════════════════════╝\n\n"
)

our_results %>%
  transmute(
    owner,
    entry_name,
    picks = paste(pick1, pick2, pick3, sep = " / "),
    surv_prob = scales::percent(survival_prob, 0.1),
    surv_sim = scales::percent(sim_survival, 0.1),
    field_own = scales::percent(avg_field_own, 0.1),
    EV = round(ev, 4)
  ) %>%
  print(n = Inf, width = Inf)

# --- 12. SUMMARY BY OWNER ----------------------------------------------------
cat("\n=== AVERAGE EV BY OWNER ===\n")
our_results %>%
  group_by(owner) %>%
  summarise(
    entries = n(),
    avg_ev = round(mean(ev), 4),
    max_ev = round(max(ev), 4),
    min_ev = round(min(ev), 4),
    total_ev = round(sum(ev), 4),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_ev)) %>%
  print(n = Inf)

# --- 13. PER-TEAM EV vs FIELD ------------------------------------------------
cat("\n=== PER-TEAM EV vs FIELD ===\n")
cat("Using", n_field, "actual field entries\n")

team_ev_accum <- numeric(max(teams$team_id))
team_win_count <- integer(max(teams$team_id))

cat("Computing per-team EV across", format(n_sims, big.mark = ","), "sims...\n")
pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)

for (ch in 1:n_chunks) {
  s <- (ch - 1) * chunk_size + 1
  e <- min(ch * chunk_size, n_sims)
  chunk_n <- e - s + 1
  cw <- r64_winners[s:e, , drop = FALSE]

  # Field survivors (batched)
  n_fs <- integer(chunk_n)
  fb <- 200
  for (fbs in seq(1, n_field, by = fb)) {
    fbe <- min(fbs + fb - 1, n_field)
    fi <- fbs:fbe
    fn <- length(fi)
    fs <- matrix(TRUE, nrow = chunk_n, ncol = fn)
    for (p in 1:3) {
      pw <- cw[, field_game_matrix[fi, p], drop = FALSE]
      pn <- matrix(
        field_tid_matrix[fi, p],
        nrow = chunk_n,
        ncol = fn,
        byrow = TRUE
      )
      fs <- fs & (pw == pn)
    }
    n_fs <- n_fs + rowSums(fs)
  }

  pot_share <- ifelse(n_fs > 0, n_field / n_fs, 0)

  for (g in 1:32) {
    t1 <- team1_ids[g]
    t2 <- team2_ids[g]

    t1_won <- cw[, g] == t1
    t2_won <- !t1_won

    team_win_count[t1] <- team_win_count[t1] + sum(t1_won)
    team_win_count[t2] <- team_win_count[t2] + sum(t2_won)

    team_ev_accum[t1] <- team_ev_accum[t1] + sum(pot_share[t1_won])
    team_ev_accum[t2] <- team_ev_accum[t2] + sum(pot_share[t2_won])
  }

  setTxtProgressBar(pb, ch)
}
close(pb)

# Compute our ownership vs rest-of-field ownership
our_picks <- c(our_entries$pick1, our_entries$pick2, our_entries$pick3)
our_own_table <- table(our_picks) / nrow(our_entries)

n_others <- n_field - nrow(our_entries)
other_field <- valid_field %>%
  mutate(field_idx = row_number()) %>%
  filter(!field_idx %in% our_field_idx)
other_picks <- c(other_field$pick1, other_field$pick2, other_field$pick3)
other_own_table <- table(other_picks) / nrow(other_field)

team_table <- tibble(
  team = teams$name,
  team_id = teams$team_id,
  seed = teams$seed,
  sim_wp = team_win_count[teams$team_id] / n_sims,
  ev_given_win = ifelse(
    team_win_count[teams$team_id] > 0,
    team_ev_accum[teams$team_id] / team_win_count[teams$team_id],
    NA_real_
  ),
  team_ev = team_ev_accum[teams$team_id] / n_sims
) %>%
  left_join(
    tibble(
      team = names(field_ownership),
      field_own = as.numeric(field_ownership)
    ),
    by = "team"
  ) %>%
  left_join(
    tibble(team = names(our_own_table), our_own = as.numeric(our_own_table)),
    by = "team"
  ) %>%
  left_join(
    tibble(
      team = names(other_own_table),
      others_own = as.numeric(other_own_table)
    ),
    by = "team"
  ) %>%
  mutate(
    field_own = replace_na(field_own, 0),
    our_own = replace_na(our_own, 0),
    others_own = replace_na(others_own, 0)
  )

r64_team_ids <- c(team1_ids, team2_ids)
team_table_r64 <- team_table %>%
  filter(team_id %in% r64_team_ids) %>%
  arrange(desc(team_ev))

team_table_r64 %>%
  transmute(
    team,
    seed,
    sim_wp = scales::percent(sim_wp, accuracy = 0.1),
    others_own = scales::percent(others_own, accuracy = 0.1),
    our_own = scales::percent(our_own, accuracy = 0.1),
    field_own = scales::percent(field_own, accuracy = 0.1),
    ev_given_win = round(ev_given_win, 4),
    team_ev = round(team_ev, 4)
  ) %>%
  print(n = 64)

# --- TSV OUTPUT FOR SHEETS ---------------------------------------------------
cat("\n=== COPY-PASTE FOR SHEETS (team table) ===\n")


sheets_team <- team_table_r64 %>%
  transmute(
    team,
    seed,
    sim_wp,
    others_own,
    our_own,
    field_own,
    ev_given_win,
    team_ev
  )
cat(paste(names(sheets_team), collapse = "\t"), "\n")
for (i in 1:nrow(sheets_team)) {
  cat(paste(sheets_team[i, ], collapse = "\t"), "\n")
}

cat("\n=== COPY-PASTE FOR SHEETS (our entries) ===\n")
sheets_entries <- our_results %>%
  transmute(
    owner,
    entry_name,
    pick1,
    pick2,
    pick3,
    survival_prob,
    avg_field_own,
    sim_survival,
    ev
  )
cat(paste(names(sheets_entries), collapse = "\t"), "\n")
for (i in 1:nrow(sheets_entries)) {
  cat(paste(sheets_entries[i, ], collapse = "\t"), "\n")
}

# --- 14. VALIDATION ----------------------------------------------------------
cat("\n=== EV VALIDATION ===\n")

# Ownership-weighted team_ev sum (normalized to pick-1)
field_own_norm <- as.numeric(field_ownership) / sum(as.numeric(field_ownership))
names(field_own_norm) <- names(field_ownership)

weighted_sum <- sum(map_dbl(names(field_own_norm), function(t) {
  row <- team_table %>% filter(team == t)
  if (nrow(row) == 0) {
    return(0)
  }
  field_own_norm[t] * row$team_ev
}))
cat(
  "  Ownership-weighted team EV sum:",
  round(weighted_sum, 4),
  "(should be ≈ 1.0 for pick-1; may differ for pick-3)\n"
)

# Exact check: compute mean EV across ALL field entries
# In each sim, sum of pot_shares = n_field (when survivors > 0)
# So sum of all entry EVs = n_field * P(≥1 survivor)
# Mean entry EV = P(≥1 survivor) ≈ 1.0
cat("  Computing mean EV across all", n_field, "field entries...\n")

all_ev_accum <- numeric(n_field)
pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)

for (ch in 1:n_chunks) {
  s <- (ch - 1) * chunk_size + 1
  e <- min(ch * chunk_size, n_sims)
  chunk_n <- e - s + 1
  cw <- r64_winners[s:e, , drop = FALSE]

  # Field survivors (batched)
  n_fs <- integer(chunk_n)
  fb <- 200
  for (fbs in seq(1, n_field, by = fb)) {
    fbe <- min(fbs + fb - 1, n_field)
    fi <- fbs:fbe
    fn <- length(fi)
    fs <- matrix(TRUE, nrow = chunk_n, ncol = fn)
    for (p in 1:3) {
      pw <- cw[, field_game_matrix[fi, p], drop = FALSE]
      pn <- matrix(
        field_tid_matrix[fi, p],
        nrow = chunk_n,
        ncol = fn,
        byrow = TRUE
      )
      fs <- fs & (pw == pn)
    }
    n_fs <- n_fs + rowSums(fs)
  }

  pot_share <- ifelse(n_fs > 0, n_field / n_fs, 0)

  # Compute EV for every field entry in batches
  for (fbs in seq(1, n_field, by = fb)) {
    fbe <- min(fbs + fb - 1, n_field)
    fi <- fbs:fbe
    for (j in seq_along(fi)) {
      idx <- fi[j]
      alive <- (cw[, field_game_matrix[idx, 1]] == field_tid_matrix[idx, 1]) &
        (cw[, field_game_matrix[idx, 2]] == field_tid_matrix[idx, 2]) &
        (cw[, field_game_matrix[idx, 3]] == field_tid_matrix[idx, 3])
      all_ev_accum[idx] <- all_ev_accum[idx] + sum(pot_share[alive])
    }
  }

  setTxtProgressBar(pb, ch)
}
close(pb)

all_evs <- all_ev_accum / n_sims
cat(
  "\n  Mean EV across all",
  n_field,
  "field entries:",
  round(mean(all_evs), 4),
  "(should be ≈ 1.0)\n"
)
cat("  Sum of all EVs:", round(sum(all_evs), 1), "(should be ≈", n_field, ")\n")

if (abs(mean(all_evs) - 1.0) > 0.05) {
  cat("  ⚠️  Mean EV is off — check the formula!\n")
} else {
  cat("  ✓ Validation passed\n")
}