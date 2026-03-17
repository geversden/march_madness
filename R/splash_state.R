#!/usr/bin/env Rscript
# ==============================================================================
# splash_state.R
# Portfolio & entry state tracking for Splash NCAA survivor contests.
#
# Tracks ~800 entries across multiple contests:
#   - Which picks have been made per entry per slot
#   - Which entries are still alive
#   - Contest metadata (size, entry fee, payout structure)
#
# Usage:
#   source("splash_config.R")
#   source("splash_state.R")
# ==============================================================================

library(data.table)
library(httr)

# ==============================================================================
# PORTFOLIO INITIALIZATION
# ==============================================================================

#' Create a new portfolio state from a contests definition
#'
#' @param contests_df Data frame with columns:
#'   contest_id  (character) - unique contest identifier
#'   contest_size (integer)  - total entries in the contest (including ours)
#'   entry_fee   (numeric)   - entry fee per entry
#'   prize_pool  (numeric)   - total prize pool
#'   n_entries   (integer)   - how many entries WE have in this contest
#'   format      (character) - optional, "A" (E8 combined) or "B" (E8 split). Default "A".
#' @return data.table of entry state
init_portfolio <- function(contests_df) {
  setDT(contests_df)
  contests_df <- as.data.table(contests_df)
  required_cols <- c("contest_id", "contest_size", "entry_fee", "prize_pool", "n_entries")
  missing <- setdiff(required_cols, names(contests_df))
  if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))

  # Default format to "A" if not provided
  if (!"format" %in% names(contests_df)) {
    contests_df[, format := "A"]
  }

  # Expand: one row per entry
  expanded <- contests_df[rep(seq_len(.N), n_entries)]
  expanded[, entry_seq := sequence(contests_df$n_entries)]
  expanded[, entry_id := sprintf("%s_%04d", contest_id, entry_seq)]
  expanded[, entry_seq := NULL]
  expanded[, n_entries := NULL]
  expanded[, alive := TRUE]

  # Add pick columns for ALL possible slots (superset across formats)
  # Entries only use the columns relevant to their format;
  # unused columns stay NA.
  for (slot_id in ALL_SLOT_IDS) {
    col <- slot_col_name(slot_id)
    expanded[, (col) := NA_integer_]
  }

  fmt_counts <- table(expanded$format)
  fmt_str <- paste(sprintf("%d format %s", fmt_counts, names(fmt_counts)), collapse = ", ")
  cat(sprintf("Portfolio initialized: %d entries across %d contests (%s)\n",
              nrow(expanded), nrow(contests_df), fmt_str))
  expanded
}

# ==============================================================================
# PICK RECORDING
# ==============================================================================

#' Record picks for a set of entries
#'
#' @param state data.table of entry state
#' @param entry_ids Character vector of entry IDs to update
#' @param slot_id Character slot ID (e.g., "R1_d1")
#' @param team_ids Integer vector of team_ids to assign.
#'   If length 1, all entries get the same pick.
#'   If same length as entry_ids, each entry gets its own pick.
#' @return Modified state (also modified in place via data.table reference)
record_picks <- function(state, entry_ids, slot_id, team_ids) {
  col <- slot_col_name(slot_id)
  if (!(col %in% names(state))) stop("Unknown slot column: ", col)

  idx <- which(state$entry_id %in% entry_ids)
  if (length(idx) == 0) stop("No matching entry_ids found")

  if (length(team_ids) == 1) {
    team_ids <- rep(team_ids, length(idx))
  }
  stopifnot(length(team_ids) == length(idx))

  # Validate no reuse (check other slot columns, not the current one)
  other_cols <- setdiff(all_slot_cols(), col)
  other_cols <- intersect(other_cols, names(state))

  for (k in seq_along(idx)) {
    i <- idx[k]
    # Check only other slots for reuse (not the current slot being set)
    prior_picks <- integer(0)
    for (oc in other_cols) {
      val <- state[[oc]][[i]]
      if (!is.na(val)) prior_picks <- c(prior_picks, val)
    }
    if (team_ids[k] %in% prior_picks) {
      warning(sprintf("Entry %s already used team %d in a prior slot, skipping",
                      state$entry_id[i], team_ids[k]))
      next
    }
    set(state, i = i, j = col, value = team_ids[k])
  }

  invisible(state)
}

#' Bulk record: assign team picks to entries based on an allocation table
#'
#' @param state data.table of entry state
#' @param allocation data.table with columns: entry_id, team_id
#' @param slot_id Character slot ID
#' @return Modified state
record_allocation <- function(state, allocation, slot_id) {
  allocation <- as.data.table(allocation)
  for (tid in unique(allocation$team_id)) {
    eids <- allocation[team_id == tid, entry_id]
    record_picks(state, eids, slot_id, tid)
  }
  invisible(state)
}

# ==============================================================================
# ELIMINATION TRACKING
# ==============================================================================

#' After real results come in, mark eliminated entries
#'
#' @param state data.table of entry state
#' @param slot_id Character slot ID for the slot that just completed
#' @param winners Integer vector of team_ids that won their games in this slot
#' @return Modified state with alive column updated
mark_eliminations <- function(state, slot_id, winners) {
  col <- slot_col_name(slot_id)
  slot <- get_slot(slot_id)

  # Only check alive entries that have a pick for this slot
  alive_idx <- which(state$alive & !is.na(state[[col]]))

  n_eliminated <- 0
  for (i in alive_idx) {
    pick <- state[[col]][i]
    if (!(pick %in% winners)) {
      set(state, i = i, j = "alive", value = FALSE)
      n_eliminated <- n_eliminated + 1
    }
  }

  cat(sprintf("Slot %s: %d entries eliminated, %d still alive\n",
              slot_id, n_eliminated, sum(state$alive)))
  invisible(state)
}

# ==============================================================================
# STATE QUERIES
# ==============================================================================

#' Get all team_ids already picked by an entry
get_used_teams <- function(state, entry_id) {
  row <- state[state$entry_id == entry_id]
  if (nrow(row) == 0) stop("Unknown entry_id: ", entry_id)

  picks <- integer(0)
  for (slot_id in ALL_SLOT_IDS) {
    col <- slot_col_name(slot_id)
    val <- row[[col]][[1]]  # extract scalar from data.table column
    if (!is.na(val)) picks <- c(picks, val)
  }
  picks
}

#' Get used teams for all entries at once (vectorized)
#' @return Named list: entry_id -> integer vector of used team_ids
get_all_used_teams <- function(state) {
  pick_cols <- all_slot_cols()
  pick_cols <- intersect(pick_cols, names(state))

  result <- vector("list", nrow(state))
  names(result) <- state$entry_id

  pick_mat <- as.matrix(state[, ..pick_cols])
  for (i in seq_len(nrow(state))) {
    vals <- pick_mat[i, ]
    result[[i]] <- as.integer(vals[!is.na(vals)])
  }
  result
}

#' Group entries by (contest_id, used_teams) for efficient batch computation
#' Entries in the same group have identical EV calculations.
#' @return data.table with columns: group_id, contest_id, contest_size, entry_fee,
#'         prize_pool, used_teams (list), entry_ids (list), n_entries
group_entries <- function(state) {
  alive <- state[alive == TRUE]
  if (nrow(alive) == 0) return(data.table())

  pick_cols <- all_slot_cols()
  pick_cols <- intersect(pick_cols, names(alive))

  # Create a hash of used teams for grouping
  alive[, used_hash := apply(.SD, 1, function(row) {
    vals <- sort(row[!is.na(row)])
    paste(vals, collapse = ",")
  }), .SDcols = pick_cols]

  # Include format in grouping if it exists
  group_by_cols <- c("contest_id", "used_hash")
  if ("format" %in% names(alive)) group_by_cols <- c(group_by_cols, "format")

  groups <- alive[, .(
    entry_ids    = list(entry_id),
    n_entries    = .N,
    contest_size = contest_size[1],
    entry_fee    = entry_fee[1],
    prize_pool   = prize_pool[1],
    used_teams   = list({
      parts <- strsplit(used_hash[1], ",")[[1]]
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) integer(0) else as.integer(parts)
    })
  ), by = group_by_cols]

  # Ensure format column exists
  if (!"format" %in% names(groups)) groups[, format := "A"]

  groups[, group_id := .I]
  groups[, used_hash := NULL]
  groups
}

# ==============================================================================
# PERSISTENCE
# ==============================================================================

save_state <- function(state, path) {
  saveRDS(state, path)
  cat(sprintf("State saved: %d entries -> %s\n", nrow(state), path))
}

load_state <- function(path) {
  if (!file.exists(path)) stop("State file not found: ", path)
  state <- readRDS(path)
  cat(sprintf("State loaded: %d entries (%d alive) from %s\n",
              nrow(state), sum(state$alive), path))
  state
}

# ==============================================================================
# SPLASH API IMPORT
# ==============================================================================

#' Import entry state from Splash API for a given contest
#'
#' @param contest_id Character Splash contest UUID
#' @param auth_headers Named character vector of HTTP headers (incl. Authorization)
#' @param our_username Character username to identify our entries
#' @return data.table with entry state
import_from_splash <- function(contest_id, auth_headers, our_username = "TinkyTyler") {
  base_url <- sprintf(
    "https://api.splashsports.com/contests-service/api/contests/%s/picks/available",
    contest_id
  )

  # Paginate to get all entries
  all_rows <- list()
  limit <- 100
  offset <- 0

  repeat {
    res <- httr::GET(
      url = base_url,
      httr::add_headers(.headers = auth_headers),
      query = list(limit = limit, offset = offset)
    )
    json_obj <- httr::content(res, simplifyVector = TRUE)

    if (length(json_obj$data) == 0) break
    all_rows[[length(all_rows) + 1]] <- json_obj$data

    total <- json_obj$total
    offset <- offset + limit
    if (offset >= total) break
    Sys.sleep(0.15)
  }

  entries_raw <- rbindlist(all_rows, fill = TRUE)
  cat(sprintf("Fetched %d entries from contest %s\n", nrow(entries_raw), contest_id))

  # Fetch picks for all entries
  entry_ids <- unique(entries_raw$entryId)
  batch_size <- 100
  entry_batches <- split(entry_ids, ceiling(seq_along(entry_ids) / batch_size))

  all_picks <- list()
  for (batch in entry_batches) {
    res <- httr::POST(
      url = "https://api.splashsports.com/contests-service/api/team-picks",
      httr::add_headers(.headers = auth_headers),
      body = list(entryIds = batch),
      encode = "json"
    )
    pick_data <- httr::content(res, simplifyVector = TRUE)
    all_picks[[length(all_picks) + 1]] <- pick_data
    Sys.sleep(0.15)
  }

  cat(sprintf("Fetched picks for %d entries\n", length(entry_ids)))

  list(
    entries_raw = entries_raw,
    picks_raw   = all_picks,
    contest_id  = contest_id
  )
}

# ==============================================================================
# CSV IMPORT
# ==============================================================================

#' Load contests from Tyler's master CSV
#'
#' Parses the CSV exported from Google Sheets, extracts contest metadata,
#' determines format from the Rules column, and returns a data.table ready
#' for init_portfolio().
#'
#' @param file Path to CSV file
#' @param include_hodes Logical, if FALSE (default) exclude Hodes (different system)
#' @return data.table with columns: contest_id, contest_name, contest_size,
#'   entry_fee, prize_pool, n_entries, format, url
load_contests_csv <- function(file, include_hodes = FALSE) {
  raw <- fread(file)
  cat(sprintf("Read %d contest rows from %s\n", nrow(raw), basename(file)))
  
  # Standardize column names
  nms <- names(raw)
  # Handle exact column names from your CSV
  col_map <- c(
    "Contest"             = "contest_name",
    "URL"                 = "url",
    "Fee"                 = "entry_fee",
    "Entries"             = "contest_size",
    "Entries Remaining"   = "entries_remaining",
    "Prizepool"           = "prize_pool",
    "Our Entries"         = "n_entries",
    "Our Entries Remaining" = "our_entries_remaining",
    "Our Cost"            = "our_cost",
    "Our Equity"          = "our_equity",
    "Rules"               = "rules"
  )
  for (old_name in names(col_map)) {
    if (old_name %in% nms) setnames(raw, old_name, col_map[[old_name]])
  }
  
  # Safety net: Ensure required columns exist to prevent regex crashes later
  if (!"url" %in% names(raw)) raw$url <- NA_character_
  if (!"rules" %in% names(raw)) raw$rules <- NA_character_
  if (!"contest_name" %in% names(raw)) raw$contest_name <- "Unknown Contest"
  
  # Parse currency/numeric fields (remove $, commas)
  parse_currency <- function(x) {
    as.numeric(gsub("[\\$,]", "", x))
  }
  
  setDT(raw)
  
  for (col in c("entry_fee", "prize_pool", "our_cost", "our_equity")) {
    if (col %in% names(raw)) {
      raw[[col]] <- parse_currency(raw[[col]])
    }
  }
  
  # contest_size and n_entries should be integer (stripping commas first to prevent NAs)
  for (col in c("contest_size", "n_entries", "entries_remaining", "our_entries_remaining")) {
    if (col %in% names(raw)) {
      raw[[col]] <- as.integer(gsub(",", "", raw[[col]]))
    }
  }
  
  # Extract contest UUID from Splash URL
  raw$contest_id <- NA_character_
  m <- regexpr("[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}", raw$url)
  
  # which() safely gets the row numbers of matches and drops any NAs
  valid_idx <- which(m != -1) 
  
  if (length(valid_idx) > 0) {
    # Pass the full 'url' and full 'm' to preserve attributes.
    # regmatches automatically outputs a vector of ONLY the successful matches.
    raw$contest_id[valid_idx] <- regmatches(raw$url, m)
  }
  
  # For non-Splash URLs (Hodes), use sanitized contest_name
  missing_id <- is.na(raw$contest_id)
  if (any(missing_id, na.rm = TRUE)) {
    raw$contest_id[missing_id] <- gsub("[^A-Za-z0-9]", "_", tolower(raw$contest_name[missing_id]))
  }
  
  # Determine format from Rules column
  raw$format <- sapply(raw$rules, classify_format)
  
  # Flag Hodes
  raw$is_hodes <- grepl("hodes", raw$contest_name, ignore.case = TRUE) |
    grepl("3-3-1-1", raw$rules, ignore.case = TRUE)
  
  if (!include_hodes) {
    n_hodes <- sum(raw$is_hodes, na.rm = TRUE)
    if (n_hodes > 0) {
      cat(sprintf("  Excluding %d Hodes contest(s) (different format, use hodes_pathing.R)\n",
                  n_hodes))
      # Subsetting safely
      raw <- raw[raw$is_hodes == FALSE, ]
    }
  }
  
  # Summarize
  cat(sprintf("  %d contests: %d entries total\n", nrow(raw), sum(raw$n_entries, na.rm = TRUE)))
  for (fmt in sort(unique(raw$format))) {
    fmt_rows <- raw[raw$format == fmt, ]
    cat(sprintf("    Format %s: %d contests, %d entries (%s)\n",
                fmt, nrow(fmt_rows), sum(fmt_rows$n_entries, na.rm = TRUE),
                get_format_def(fmt)$label))
  }
  
  # Return columns needed for init_portfolio plus extras
  keep_cols <- intersect(
    c("contest_id", "contest_name", "contest_size", "entry_fee",
      "prize_pool", "n_entries", "format", "url", "entries_remaining",
      "our_entries_remaining", "our_cost", "our_equity", "rules"),
    names(raw)
  )
  
  subset(raw, select = keep_cols)
}

#' Classify contest format from the Rules string
#'
#' @param rules Character string describing the rules
#' @return Character "A", "B", or "C"
classify_format <- function(rules) {
  rules <- tolower(rules)

  # Hodes or other non-Splash formats
  if (grepl("3-3-1-1", rules)) return("A")  # placeholder, filtered separately

  # Check for (4-4-2-2-1-1) pattern — Format C
  if (grepl("4-4-2-2", rules) || grepl("pick 2 days? 1,?2", rules)) return("C")

  # Check for E8 not combined — Format B
  if (grepl("not combined", rules) || grepl("7/8 not combined", rules)) return("B")

  # Default: E8 combined — Format A
  return("A")
}

cat("Splash state module loaded\n")
