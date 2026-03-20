library(tidyverse)

# ==============================================================================
# NCAA Survivor — Dollar EV Across All Splash Contests
# Adapted from scripts/calculate_ev_by_round.R
#
# Usage:
#   1. Scrape Splash contests and save:
#        source("R/scrape_splash_contests.R")
#        splash_data <- scrape_all_splash(bearer_token = "eyJ...")
#        saveRDS(splash_data, "splash_results.rds")
#   2. Run this script.
# ==============================================================================


# ---- 1. LOAD SIM DATA --------------------------------------------------------

data <- readRDS("adjusted_2026_sims.rds")

all_results   <- data$all_results     # n_sims × 63 matrix (winner team_id per game)
teams         <- data$teams           # team_id, name, seed
bracket_order <- data$bracket_order   # length-64 vector of team_ids in bracket order
n_sims        <- data$n_sims

cat("Loaded", format(n_sims, big.mark = ","), "simulations\n")
cat("Teams:", nrow(teams), "\n")

n_teams <- max(teams$team_id)


# ---- 2. SOURCE SPLASH CONFIG -------------------------------------------------

source("R/splash_config.R")   # loads SPLASH_SLOTS, FORMAT_DEFS, helper fns


# ---- 3. CONTEST METADATA (prize pools) ---------------------------------------

contests_meta <- read_csv("NCAABEntries.csv", show_col_types = FALSE) %>%
  rename(contest_name = Contest) %>%
  mutate(
    # Normalize names to match splash_contests names
    contest_name = case_when(
      contest_name == "Spooky Express $30K Guaranteed Survivor Madness Contest" ~
        "Spooky Express $30K Guaranteed Survivor Madness",
      TRUE ~ contest_name
    ),
    prizepool = as.numeric(gsub("[$,]", "", Prizepool)),
    our_cost  = as.numeric(gsub("[$,]", "", `Our Cost`))
  )

# Format codes keyed by contest name (mirrors R/scrape_splash_contests.R)
contest_formats <- tribble(
  ~contest_name,                                          ~format_code,
  "Kelly's $250K Survivor Madness",                       "A",
  "Field of 68 Survivor Madness",                         "A",
  "Ryan Hammer's $100K Survivor Madness",                 "A",
  "Bet The Process Survivor Madness",                     "A",
  "Ross Tucker's $35K Survivor Madness",                  "A",
  "Frank Michael Smith's $30K Survivor Madness",          "A",
  "Kurt Benkert's $100K Survivor Madness",                "C",
  "Spooky Express $30K Guaranteed Survivor Madness",      "A",
  "SGPN $25K Survivor Madness",                           "A",
  "MARCH MADNESS SURVIVOR",                               "A",
  "FOR THE FANS - SURVIVOR MADNESS",                      "C"
)

# Join prize pool + format; fuzzy-fallback for name mismatches
meta_full <- contest_formats %>%
  left_join(
    contests_meta %>% select(contest_name, prizepool, our_cost, Entries),
    by = "contest_name"
  )

# Fallback: also try matching NCAABEntries rows whose name contains a prefix
fill_meta <- function(cname, meta_full, contests_meta) {
  row <- meta_full %>% filter(contest_name == cname)
  if (nrow(row) > 0 && !is.na(row$prizepool[1])) return(row[1, ])
  # Partial match on first 20 chars
  prefix <- substr(cname, 1, 20)
  guess <- contests_meta %>% filter(str_detect(contest_name, fixed(prefix, ignore_case = TRUE)))
  if (nrow(guess) == 0) return(NULL)
  meta_full %>% filter(contest_name == cname) %>%
    mutate(prizepool = guess$prizepool[1], our_cost = guess$our_cost[1])
}


# ---- 4. LOAD PRE-SCRAPED SPLASH DATA -----------------------------------------

if (!file.exists("splash_results.rds")) {
  stop(paste(
    "splash_results.rds not found. Run the following first:\n",
    '  source("R/scrape_splash_contests.R")\n',
    '  splash_data <- scrape_all_splash(bearer_token = "eyJ...")\n',
    '  saveRDS(splash_data, "splash_results.rds")'
  ))
}
splash_data <- readRDS("splash_results.rds")
cat("Loaded", length(splash_data$per_contest), "Splash contests\n")


# ---- 5. BUILD TEAM LOOKUPS ---------------------------------------------------

team_name_to_id <- deframe(teams %>% select(name, team_id))

# Game-column lookup for each team across all rounds.
# Team at bracket position p:
#   R64 game g = ceil(p/2)               [cols 1-32]
#   R32 game   = 33 + floor((g-1)/2)     [cols 33-48]
#   S16 game   = 49 + floor((R32-33)/2)  [cols 49-56]
#   E8  game   = 57 + floor((S16-49)/2)  [cols 57-60]
#   FF  game   = 61 + floor((E8-57)/2)   [cols 61-62]
#   CHAMP      = 63                      [col 63]
team_game_cols_tbl <- tibble(
  team_id = bracket_order,
  bpos    = seq_along(bracket_order),
  R64     = as.integer(ceiling(seq_along(bracket_order) / 2))
) %>%
  mutate(
    R32   = 33L + as.integer(floor((R64 - 1L) / 2L)),
    S16   = 49L + as.integer(floor((R32 - 33L) / 2L)),
    E8    = 57L + as.integer(floor((S16 - 49L) / 2L)),
    FF    = 61L + as.integer(floor((E8  - 57L) / 2L)),
    CHAMP = 63L
  )

# Pre-index by team_id for O(1) lookup
tid_to_row <- integer(n_teams + 1L)
for (i in seq_len(nrow(team_game_cols_tbl))) {
  tid_to_row[team_game_cols_tbl$team_id[i]] <- i
}

# Returns the sim-matrix column for team `tid` in round `round_num` (1=R64 … 6=CHAMP)
get_game_col <- function(tid, round_num) {
  r <- tid_to_row[tid]
  if (r == 0L) return(NA_integer_)
  row <- team_game_cols_tbl[r, ]
  switch(as.character(round_num),
    "1" = row$R64,
    "2" = row$R32,
    "3" = row$S16,
    "4" = row$E8,
    "5" = row$FF,
    "6" = row$CHAMP,
    NA_integer_
  )
}

# Slot → round number (from splash_config)
slot_round <- sapply(names(SPLASH_SLOTS), function(s) SPLASH_SLOTS[[s]]$round_num)


# ---- 6. TEAM NAME NORMALIZATION ---------------------------------------------

splash_name_fixes <- c(
  "Michigan St"     = "Michigan State",
  "St John's"       = "St. John's",
  "N Dakota St"     = "North Dakota State",
  "McNeese"         = "McNeese State",
  "Hawai\u2018i"   = "Hawaii",   # right single quotation mark variant
  "Hawai\u2019i"   = "Hawaii",   # typographic apostrophe variant
  "Hawai'i"         = "Hawaii"   # straight apostrophe variant
  # Add more after inspecting ⚠️ warnings on first run
)

normalize_name <- function(nm) {
  nm <- str_trim(nm)
  if (!is.na(nm) && nm %in% names(splash_name_fixes)) {
    nm <- splash_name_fixes[[nm]]
  }
  nm
}


# ---- 7. HELPER: BUILD ENTRY PICK LIST ----------------------------------------
# Returns a list of n_entries elements.
# Each element is a list of picks: {game_col, team_id, team_name, slot_id, round_num}
# Multi-team picks (E8 pick-2, Format-C day1 pick-2) are flattened into separate items.

build_entry_picks <- function(entry_paths, format_code) {
  slot_order <- FORMAT_DEFS[[format_code]]$slot_order
  day_cols_raw <- names(entry_paths)[grepl("^day\\d+$", names(entry_paths))]
  # Sort numerically (day1, day2, ..., day10 not day1, day10, day2)
  day_cols <- day_cols_raw[order(as.integer(str_extract(day_cols_raw, "\\d+")))]
  n_days <- min(length(day_cols), length(slot_order))

  n_entries  <- nrow(entry_paths)
  unmatched  <- character(0)

  picks_list <- lapply(seq_len(n_entries), function(i) {
    entry_picks <- list()
    for (d in seq_len(n_days)) {
      slot_id    <- slot_order[d]
      round_num  <- slot_round[[slot_id]]
      raw_pick   <- entry_paths[[day_cols[d]]][i]
      if (is.na(raw_pick) || nchar(raw_pick) == 0) next

      team_strs <- str_split(raw_pick, "\\+")[[1]]
      for (tn_raw in team_strs) {
        tn  <- normalize_name(tn_raw)
        tid <- team_name_to_id[tn]
        if (is.na(tid)) {
          unmatched <<- c(unmatched, tn)
          next
        }
        gc <- get_game_col(as.integer(tid), round_num)
        if (!is.na(gc)) {
          entry_picks[[length(entry_picks) + 1L]] <- list(
            game_col  = gc,
            team_id   = as.integer(tid),
            team_name = tn,
            slot_id   = slot_id,
            round_num = round_num
          )
        }
      }
    }
    entry_picks
  })

  if (length(unmatched) > 0) {
    cat(sprintf("  ⚠️  Unmatched names (%d unique):\n", length(unique(unmatched))))
    for (m in unique(unmatched)) {
      close <- teams$name[agrep(m, teams$name, max.distance = 0.3)]
      cat(sprintf("      '%s' → maybe: %s\n", m, paste(close, collapse = ", ")))
    }
  }

  picks_list
}


# ---- 8. OUR ENTRY IDENTIFICATION --------------------------------------------

our_last_names <- c(
  "SYDLOWSKI", "LONGO", "EVERSDEN", "JONES",
  "HORCICIAK", "GELLNER", "SHORE"
)
is_our_entry <- function(entry_name) {
  str_detect(str_to_upper(entry_name), paste(our_last_names, collapse = "|"))
}


# ---- 9. R64 MATCHUP TABLE ---------------------------------------------------
# Bracket positions (2g-1, 2g) → team IDs for game g
r64_team1_ids <- bracket_order[seq(1L, 63L, by = 2L)]   # length 32
r64_team2_ids <- bracket_order[seq(2L, 64L, by = 2L)]   # length 32

# Precompute R64 sim win probabilities (used in final team table)
r64_sim_wp <- numeric(n_teams + 1L)
for (g in 1:32) {
  t1 <- r64_team1_ids[g]; t2 <- r64_team2_ids[g]
  p1 <- mean(all_results[, g] == t1)
  r64_sim_wp[t1] <- p1
  r64_sim_wp[t2] <- 1.0 - p1
}


# ==============================================================================
# MAIN LOOP: COMPUTE EV FOR EACH CONTEST
# ==============================================================================

chunk_size <- 50000L
batch_size <- 200L
n_chunks   <- ceiling(n_sims / chunk_size)

our_results_list      <- list()
team_ev_sum           <- numeric(n_teams + 1L)   # dollar EV summed across all contests
team_win_sum          <- integer(n_teams + 1L)   # for computing avg across contests

for (cname in names(splash_data$per_contest)) {

  cat(sprintf("\n=== %s ===\n", cname))

  contest_res <- splash_data$per_contest[[cname]]
  if (is.null(contest_res)) { cat("  NULL result, skipping\n"); next }

  entry_paths <- contest_res$entry_paths
  if (is.null(entry_paths) || nrow(entry_paths) == 0) {
    cat("  No entry paths, skipping\n"); next
  }

  # --- Prize pool & format ---
  meta_row <- meta_full %>% filter(contest_name == cname)
  if (nrow(meta_row) == 0 || is.na(meta_row$prizepool[1])) {
    cat(sprintf("  ⚠️  No prize pool found for '%s', skipping\n", cname))
    next
  }
  prizepool   <- meta_row$prizepool[1]
  format_code <- coalesce(meta_row$format_code[1], "A")
  n_entries   <- nrow(entry_paths)

  cat(sprintf("  %d entries | Prizepool: $%s | Format: %s\n",
              n_entries, format(prizepool, big.mark = ","), format_code))

  # --- Build entry picks ---
  entry_picks <- build_entry_picks(entry_paths, format_code)
  n_picks_per <- lengths(entry_picks)
  cat(sprintf("  Avg picks/entry: %.1f | Entries w/ no picks: %d\n",
              mean(n_picks_per), sum(n_picks_per == 0L)))

  # --- Our entries ---
  our_mask <- is_our_entry(entry_paths$entryName)
  our_idxs <- which(our_mask)
  cat(sprintf("  Our entries: %d\n", length(our_idxs)))

  # --- Accumulators ---
  ev_accum       <- numeric(n_entries)
  team_ev_accum  <- numeric(n_teams + 1L)
  team_win_count <- integer(n_teams + 1L)

  pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)

  for (ch in seq_len(n_chunks)) {
    s       <- (ch - 1L) * chunk_size + 1L
    e       <- min(ch * chunk_size, n_sims)
    chunk_n <- e - s + 1L
    cw      <- all_results[s:e, , drop = FALSE]

    # -- A: Count field survivors per sim (batched over entries) ---------------
    n_fs <- integer(chunk_n)

    for (fbs in seq(1L, n_entries, by = batch_size)) {
      fbe <- min(fbs + batch_size - 1L, n_entries)
      fi  <- fbs:fbe
      fn  <- length(fi)

      fs <- matrix(TRUE, nrow = chunk_n, ncol = fn)

      for (j in seq_len(fn)) {
        for (pk in entry_picks[[fi[j]]]) {
          fs[, j] <- fs[, j] & (cw[, pk$game_col] == pk$team_id)
        }
      }
      n_fs <- n_fs + rowSums(fs)
    }

    pot_share <- ifelse(n_fs > 0L, prizepool / n_fs, 0)

    # -- B: Dollar EV for our entries ------------------------------------------
    for (i in our_idxs) {
      if (length(entry_picks[[i]]) == 0L) next
      alive <- rep(TRUE, chunk_n)
      for (pk in entry_picks[[i]]) {
        alive <- alive & (cw[, pk$game_col] == pk$team_id)
      }
      ev_accum[i] <- ev_accum[i] + sum(pot_share[alive])
    }

    # -- C: Per-team dollar EV (R64 games) ------------------------------------
    for (g in 1:32) {
      t1     <- r64_team1_ids[g]
      t2     <- r64_team2_ids[g]
      t1_won <- cw[, g] == t1

      team_win_count[t1] <- team_win_count[t1] + sum(t1_won)
      team_win_count[t2] <- team_win_count[t2] + sum(!t1_won)

      team_ev_accum[t1]  <- team_ev_accum[t1]  + sum(pot_share[t1_won])
      team_ev_accum[t2]  <- team_ev_accum[t2]  + sum(pot_share[!t1_won])
    }

    setTxtProgressBar(pb, ch)
  }
  close(pb)

  # --- Finalize per-contest ---------------------------------------------------
  dollar_ev <- ev_accum / n_sims

  our_results_list[[cname]] <- entry_paths %>%
    select(entryId, entryName) %>%
    mutate(
      contest   = cname,
      prizepool = prizepool,
      dollar_ev = dollar_ev,
      is_ours   = our_mask,
      owner     = str_extract(entryName, "^[^/]+")
    ) %>%
    filter(is_ours)

  # Accumulate across contests
  team_ev_sum  <- team_ev_sum  + team_ev_accum  / n_sims
  team_win_sum <- team_win_sum + team_win_count

  # Per-contest top-10 preview
  r64_tids <- c(r64_team1_ids, r64_team2_ids)
  cat(sprintf("\n  Top 10 R64 teams by dollar EV (this contest):\n"))
  teams %>%
    filter(team_id %in% r64_tids) %>%
    mutate(
      contest_dollar_ev = team_ev_accum[team_id] / n_sims,
      ev_given_win = ifelse(
        team_win_count[team_id] > 0,
        team_ev_accum[team_id] / team_win_count[team_id],
        NA_real_
      ),
      sim_wp = r64_sim_wp[team_id]
    ) %>%
    arrange(desc(contest_dollar_ev)) %>%
    head(10) %>%
    transmute(
      team          = name,
      seed,
      sim_wp        = scales::percent(sim_wp, 0.1),
      dollar_ev     = scales::dollar(contest_dollar_ev, 0.01),
      ev_given_win  = scales::dollar(ev_given_win, 0.01)
    ) %>%
    print()
}


# ==============================================================================
# AGGREGATE OUTPUT
# ==============================================================================

all_our <- bind_rows(our_results_list)

cat("\n\n╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║         DOLLAR EV — OUR ENTRIES ACROSS ALL SPLASH CONTESTS          ║\n")
cat("║     EV = expected $ payout if prizepool split among survivors        ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n\n")

all_our %>%
  arrange(desc(dollar_ev)) %>%
  select(owner, entryName, contest, dollar_ev) %>%
  mutate(dollar_ev = scales::dollar(dollar_ev, 0.01)) %>%
  print(n = Inf, width = Inf)


# --- By owner -----------------------------------------------------------------
cat("\n=== TOTAL DOLLAR EV BY OWNER (across all contests) ===\n")
all_our %>%
  group_by(owner) %>%
  summarise(
    entries   = n(),
    total_ev  = sum(dollar_ev),
    avg_ev    = mean(dollar_ev),
    max_ev    = max(dollar_ev),
    .groups   = "drop"
  ) %>%
  arrange(desc(total_ev)) %>%
  mutate(across(c(total_ev, avg_ev, max_ev), ~ scales::dollar(.x, 0.01))) %>%
  print()


# --- By contest ---------------------------------------------------------------
cat("\n=== OUR EV BY CONTEST ===\n")
all_our %>%
  group_by(contest) %>%
  summarise(
    our_entries  = n(),
    our_total_ev = sum(dollar_ev),
    our_avg_ev   = mean(dollar_ev),
    .groups      = "drop"
  ) %>%
  left_join(
    meta_full %>% select(contest_name, prizepool),
    by = c("contest" = "contest_name")
  ) %>%
  arrange(desc(our_total_ev)) %>%
  mutate(
    our_total_ev = scales::dollar(our_total_ev, 0.01),
    our_avg_ev   = scales::dollar(our_avg_ev, 0.01),
    prizepool    = scales::dollar(prizepool, 1)
  ) %>%
  print()


# ==============================================================================
# PER-TEAM DOLLAR EV — SUMMED ACROSS ALL SPLASH CONTESTS (R64)
# ==============================================================================

cat("\n=== PER-TEAM DOLLAR EV — SUMMED ACROSS ALL SPLASH CONTESTS (R64) ===\n")
cat("(Expected $ value if you picked each team in Round 1, given field picks)\n\n")

r64_all_tids <- c(r64_team1_ids, r64_team2_ids)

team_table <- teams %>%
  filter(team_id %in% r64_all_tids) %>%
  mutate(
    sim_wp          = r64_sim_wp[team_id],
    total_dollar_ev = team_ev_sum[team_id],
    n_sims_won      = team_win_sum[team_id],
    # ev_given_win = avg $ payout per contest when this team wins R64
    # = sum_contests(team_ev_accum_c) / sum_contests(team_win_count_c)
    ev_given_win    = ifelse(
      n_sims_won > 0,
      team_ev_sum[team_id] * n_sims / n_sims_won,
      NA_real_
    )
  ) %>%
  arrange(desc(total_dollar_ev))

team_table %>%
  transmute(
    team            = name,
    seed,
    sim_wp          = scales::percent(sim_wp, 0.1),
    total_dollar_ev = scales::dollar(total_dollar_ev, 0.01),
    ev_given_win    = scales::dollar(ev_given_win, 0.01)
  ) %>%
  print(n = 64)


# ==============================================================================
# TSV OUTPUT FOR GOOGLE SHEETS
# ==============================================================================

cat("\n=== COPY-PASTE FOR SHEETS: Our Entries ===\n")
sheets_entries <- all_our %>%
  arrange(desc(dollar_ev)) %>%
  select(owner, entryName, contest, dollar_ev)
cat(paste(names(sheets_entries), collapse = "\t"), "\n")
for (i in seq_len(nrow(sheets_entries))) {
  cat(paste(sheets_entries[i, ], collapse = "\t"), "\n")
}

cat("\n=== COPY-PASTE FOR SHEETS: Per-Team EV (All Contests) ===\n")
sheets_teams <- team_table %>%
  transmute(team = name, seed, sim_wp, total_dollar_ev, ev_given_win)
cat(paste(names(sheets_teams), collapse = "\t"), "\n")
for (i in seq_len(nrow(sheets_teams))) {
  cat(paste(sheets_teams[i, ], collapse = "\t"), "\n")
}


# ==============================================================================
# QUICK SANITY CHECK
# ==============================================================================

cat("\n=== SANITY CHECK ===\n")
total_our_ev   <- sum(all_our$dollar_ev, na.rm = TRUE)
total_our_cost <- sum(meta_full$our_cost, na.rm = TRUE)
total_prizepool <- sum(meta_full$prizepool, na.rm = TRUE)

cat(sprintf(
  "  Total dollar EV (our entries):  %s\n",
  scales::dollar(total_our_ev, 0.01)
))
cat(sprintf(
  "  Total cost (our entries):        %s\n",
  scales::dollar(total_our_cost, 0.01)
))
cat(sprintf(
  "  Combined prizepool (all contests): %s\n",
  scales::dollar(total_prizepool, 1)
))
cat(sprintf(
  "  Our share of prizepool (by entries): ~%.2f%%\n",
  100 * total_our_cost / total_prizepool
))
cat(sprintf(
  "  Implied EV ratio (EV / cost): %.3fx\n",
  total_our_ev / total_our_cost
))
cat("\nNote: EV > cost implies edge over the field.\n")
cat("EV computed as: E[prizepool / n_survivors] when entry survives all picks.\n")
