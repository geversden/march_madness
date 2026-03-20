#!/usr/bin/env Rscript
# ==============================================================================
# scrape_splash_contests.R
# Scrape Splash Sports NCAA survivor contests for field + our ownership
#
# Usage:
#   source("scrape_splash_contests.R")
#
#   # Grab bearer token from browser network tab, then:
#   results <- scrape_all_splash(bearer_token = "eyJ...")
#
#   # Results contain:
#   #   results$entries     - all entries with picks per contest
#   #   results$field_own   - field ownership by team by day by contest
#   #   results$our_own     - our ownership by team by day by contest
#   #   results$entry_paths - full entry-level paths for sim integration
# ==============================================================================

library(httr)
library(tidyverse)

# ==============================================================================
# CONTEST CONFIGURATION
# ==============================================================================

# All 11 Splash contests (Hodes is separate, not on Splash API)
splash_contests <- tribble(
  ~contest_name,                                         ~contest_id,                              ~fee, ~format,
  "Kelly's $250K Survivor Madness",                      "9eba15b3-e62d-42f0-b399-5957cd33d62c",   25,   "2-2-2-2-1-1",
  "Field of 68 Survivor Madness",                        "3f84b670-bd9e-4786-967a-f7cc63821c8c",   25,   "2-2-2-2-1-1",
  "Ryan Hammer's $100K Survivor Madness",                "a68d9a2b-7317-4e43-9ddb-c0fa5f7b96ec",   15,   "2-2-2-2-1-1",
  "Bet The Process Survivor Madness",                    "28f81d62-3fac-4c0f-ae18-159a3307d9c5",   100,  "2-2-2-2-1-1",
  "Ross Tucker's $35K Survivor Madness",                 "80db5738-a417-4524-b4a3-24be60c63fd4",   50,   "2-2-2-2-1-1",
  "Frank Michael Smith's $30K Survivor Madness",         "96acf9b2-2def-44ee-add0-d67df18ba470",   30,   "2-2-2-2-1-1",
  "Kurt Benkert's $100K Survivor Madness",               "b3f535dc-b014-4cd6-93a1-546f800caf1a",   5,    "4-4-2-2-1-1",
  "Spooky Express $30K Guaranteed Survivor Madness",     "6cc0a29e-86bb-4a1a-8251-2b581ad80320",   75,   "2-2-2-2-1-1",
  "SGPN $25K Survivor Madness",                          "919e7294-fd5d-4ae0-9b87-e8969d657282",   25,   "2-2-2-2-1-1",
  "MARCH MADNESS SURVIVOR",                              "3950c489-4150-4a19-bf01-0056a46d81c1",   10,   "2-2-2-2-1-1",
  "FOR THE FANS - SURVIVOR MADNESS",                     "cf3889a3-84a2-4165-990b-891f67e26f9b",   40,   "4-4-2-2-1-1"
)

# ==============================================================================
# API HELPERS
# ==============================================================================

#' Build headers for Splash API requests
#' @param bearer_token Character string - the JWT token from browser network tab
make_headers <- function(bearer_token) {
  c(
    `User-Agent`       = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:136.0) Gecko/20100101 Firefox/136.0",
    Accept             = "application/json",
    `Accept-Language`  = "en-US,en;q=0.5",
    `Accept-Encoding`  = "gzip, deflate",
    Referer            = "https://app.splashsports.com/",
    `Content-Type`     = "application/json",
    `X-App-Platform`   = "web",
    `X-App-Version`    = "1.220.0",
    Authorization      = paste("Bearer", bearer_token),
    Origin             = "https://app.splashsports.com",
    DNT                = "1",
    `Sec-GPC`          = "1",
    Connection         = "keep-alive",
    `Sec-Fetch-Dest`   = "empty",
    `Sec-Fetch-Mode`   = "cors",
    `Sec-Fetch-Site`   = "same-site"
  )
}

# ==============================================================================
# CORE SCRAPING FUNCTION
# ==============================================================================

#' Scrape a single Splash Sports contest
#'
#' @param contest_id UUID string for the contest
#' @param headers Named character vector of HTTP headers (from make_headers)
#' @param our_username Our Splash username (default "TinkyTyler")
#' @return List with entries_raw, picks_raw, and parsed ownership data
scrape_splash_contest <- function(contest_id, headers, our_username = "TinkyTyler") {

  base_url <- "https://api.splashsports.com/contests-service/api/contests"

  # -------------------------------------------
  # Step 1: Paginate through entries
  # -------------------------------------------
  cat(sprintf("  Fetching entries for %s...\n", contest_id))

  # First request to get total count
  res <- httr::GET(
    url = paste0(base_url, "/", contest_id, "/picks/available"),
    httr::add_headers(.headers = headers),
    query = list(limit = 100, offset = 0)
  )

  if (httr::status_code(res) != 200) {
    warning(sprintf("Failed to fetch contest %s (HTTP %d). Token may be expired.",
                    contest_id, httr::status_code(res)))
    return(NULL)
  }

  json_obj <- httr::content(res, simplifyVector = TRUE)
  total <- json_obj$total
  cat(sprintf("    Total entries: %d\n", total))

  all_rows <- list()
  limit <- 100

  for (off in seq(0, total - 1, by = limit)) {
    res <- httr::GET(
      url = paste0(base_url, "/", contest_id, "/picks/available"),
      httr::add_headers(.headers = headers),
      query = list(limit = limit, offset = off)
    )

    json_obj <- httr::content(res, simplifyVector = TRUE)
    if (length(json_obj$data) == 0) break

    all_rows[[length(all_rows) + 1]] <- json_obj$data
    Sys.sleep(0.15)
  }

  entries_df <- tibble(all_rows = all_rows) |>
    unnest(all_rows)

  entry_ids <- unique(entries_df$entryId)
  cat(sprintf("    Unique entry IDs: %d\n", length(entry_ids)))

  # -------------------------------------------
  # Step 2: Batch fetch picks via team-picks
  # -------------------------------------------
  cat("    Fetching picks...\n")

  batch_size <- 100
  entry_batches <- split(entry_ids, ceiling(seq_along(entry_ids) / batch_size))

  responses <- purrr::map(entry_batches, \(ids) {
    res <- httr::POST(
      url = "https://api.splashsports.com/contests-service/api/team-picks",
      httr::add_headers(.headers = headers),
      body = list(entryIds = ids),
      encode = "json"
    )
    Sys.sleep(0.15)

    list(
      entry_ids = ids,
      response  = res
    )
  })

  json_responses <- purrr::map(responses, \(x) {
    data <- httr::content(x$response, simplifyVector = TRUE)
    if (!"entryId" %in% names(data)) {
      data <- purrr::map2(data, x$entry_ids, ~ c(.x, entryId = .y))
    }
    data
  })

  cat(sprintf("    Fetched picks for %d batches\n", length(json_responses)))

  # -------------------------------------------
  # Step 3: Parse picks into tidy format
  # -------------------------------------------
  picks_raw <- tibble(resp = json_responses) |>
    unnest(resp) |>
    unnest_wider(resp)

  # The 'picks' column is a named list keyed by slate GUIDs.
  # Each value contains team pick info (teamName, teamAlias, etc.)
  # For multi-pick slates, it's a data frame with multiple rows.

  # Unnest the picks column to get slate GUIDs as columns
  picks_wide <- picks_raw |>
    unnest_wider(picks)

  # Get slate GUID column names (everything that's a UUID pattern)
  slate_cols <- names(picks_wide)[grepl("^[0-9a-f]{8}-", names(picks_wide))]
  cat(sprintf("    Found %d slates (days)\n", length(slate_cols)))

  # -------------------------------------------
  # Step 4: Extract team names from each slate
  # -------------------------------------------
  # For each slate, extract the team name(s) picked.
  # Single-pick slates: value is a list with teamName
  # Multi-pick slates: value is a data frame with teamName column

  pick_long <- purrr::map_dfr(seq_along(slate_cols), function(i) {
    slate_id <- slate_cols[i]
    col_data <- picks_wide[[slate_id]]

    if (is.null(col_data)) return(tibble())

    # Try to extract team names from this column
    tryCatch({
      # Build a tibble associating entryId with this slate's picks
      entry_ids_col <- picks_wide$entryId

      if (is.data.frame(col_data)) {
        # It's already a data frame - could be single-pick (one row per entry)
        # or multi-pick (multiple rows)
        result <- tibble(entryId = entry_ids_col)

        if ("teamName" %in% names(col_data)) {
          result$team <- col_data$teamName
        } else if ("teamAlias" %in% names(col_data)) {
          # teamAlias might be nested (list of vectors for multi-pick)
          if (is.list(col_data$teamAlias)) {
            result$team <- purrr::map_chr(col_data$teamAlias, ~ {
              if (is.null(.x) || length(.x) == 0) NA_character_
              else paste(sort(.x), collapse = "|")
            })
          } else {
            result$team <- col_data$teamAlias
          }
        } else {
          return(tibble())
        }

        result |>
          mutate(slate_id = slate_id, slate_order = i) |>
          filter(!is.na(team))

      } else if (is.list(col_data)) {
        # It's a list of lists - one per entry
        purrr::map2_dfr(col_data, entry_ids_col, function(pick, eid) {
          if (is.null(pick) || length(pick) == 0) return(tibble())

          if (is.data.frame(pick)) {
            # Multi-pick: data frame with teamName column
            teams <- if ("teamName" %in% names(pick)) pick$teamName
                     else if ("teamAlias" %in% names(pick)) pick$teamAlias
                     else return(tibble())
            tibble(entryId = eid, team = teams,
                   slate_id = slate_id, slate_order = i)
          } else if (is.list(pick)) {
            team <- pick$teamName %||% pick$teamAlias %||% NA_character_
            if (is.null(team) || is.na(team)) return(tibble())
            tibble(entryId = eid, team = team,
                   slate_id = slate_id, slate_order = i)
          } else {
            tibble()
          }
        })

      } else {
        tibble()
      }
    }, error = function(e) {
      cat(sprintf("    Warning: error parsing slate %s: %s\n", slate_id, e$message))
      tibble()
    })
  })

  # Join entry names
  pick_long <- pick_long |>
    left_join(
      entries_df |> select(entryId, entryName) |> distinct(),
      by = "entryId"
    )

  # Map slate order to day numbers
  slate_mapping <- pick_long |>
    distinct(slate_id, slate_order) |>
    arrange(slate_order) |>
    mutate(day = row_number())

  pick_long <- pick_long |>
    left_join(slate_mapping, by = c("slate_id", "slate_order"))

  # -------------------------------------------
  # Step 5: Compute ownership
  # -------------------------------------------

  # Split multi-team picks ("|" separated) into separate rows
  pick_expanded <- pick_long |>
    separate_rows(team, sep = "\\|")

  # Field ownership
  field_own <- pick_expanded |>
    group_by(day, slate_id) |>
    mutate(day_entries = n_distinct(entryId)) |>
    group_by(day, slate_id, team) |>
    summarise(
      picks       = n(),
      day_entries = first(day_entries),
      ownership   = picks / day_entries,
      .groups     = "drop"
    ) |>
    arrange(day, desc(ownership))

  # Our ownership
  our_own <- pick_expanded |>
    filter(entryName == our_username) |>
    group_by(day, slate_id) |>
    mutate(day_entries = n_distinct(entryId)) |>
    group_by(day, slate_id, team) |>
    summarise(
      picks       = n(),
      day_entries = first(day_entries),
      ownership   = picks / day_entries,
      .groups     = "drop"
    ) |>
    arrange(day, desc(ownership))

  # Entry paths: one row per entry with all picks
  entry_paths <- pick_expanded |>
    group_by(entryId, entryName, day) |>
    summarise(teams = paste(sort(team), collapse = "+"), .groups = "drop") |>
    pivot_wider(
      id_cols     = c(entryId, entryName),
      names_from  = day,
      values_from = teams,
      names_prefix = "day"
    )

  # Eliminated status from entries_df
  if ("eliminatedSlateId" %in% names(entries_df)) {
    entry_paths <- entry_paths |>
      left_join(
        entries_df |> select(entryId, eliminatedSlateId) |> distinct(),
        by = "entryId"
      ) |>
      mutate(alive = is.na(eliminatedSlateId))
  }

  cat(sprintf("    Done: %d entries, %d alive, %d days\n",
              nrow(entry_paths),
              sum(entry_paths$alive, na.rm = TRUE),
              max(pick_long$day, na.rm = TRUE)))

  list(
    entries_df  = entries_df,
    pick_long   = pick_long,
    field_own   = field_own,
    our_own     = our_own,
    entry_paths = entry_paths,
    slate_map   = slate_mapping
  )
}


# ==============================================================================
# MAIN WRAPPER: SCRAPE ALL CONTESTS
# ==============================================================================

#' Scrape all Splash NCAA survivor contests
#'
#' @param bearer_token Character string - JWT from browser network tab
#' @param our_username Our Splash username (default "TinkyTyler")
#' @param contests Optional tibble of contests to scrape (default: all 11)
#' @return List with combined results across all contests
scrape_all_splash <- function(bearer_token,
                              our_username = "TinkyTyler",
                              contests = splash_contests) {

  headers <- make_headers(bearer_token)

  cat(sprintf("Scraping %d Splash contests...\n\n", nrow(contests)))

  results <- purrr::map2(contests$contest_id, contests$contest_name, function(cid, cname) {
    cat(sprintf("[%s]\n", cname))
    result <- scrape_splash_contest(cid, headers, our_username)
    if (!is.null(result)) {
      result$contest_id   <- cid
      result$contest_name <- cname
    }
    cat("\n")
    result
  })

  names(results) <- contests$contest_name

  # Filter out failures
  results <- purrr::compact(results)

  if (length(results) == 0) {
    stop("All contests failed. Bearer token is likely expired.")
  }

  cat(sprintf("\nSuccessfully scraped %d / %d contests.\n", length(results), nrow(contests)))

  # Combine field ownership across contests (weighted by entries)
  combined_field <- purrr::map_dfr(results, function(r) {
    r$field_own |> mutate(contest_name = r$contest_name, contest_id = r$contest_id)
  })

  combined_our <- purrr::map_dfr(results, function(r) {
    r$our_own |> mutate(contest_name = r$contest_name, contest_id = r$contest_id)
  })

  combined_paths <- purrr::map_dfr(results, function(r) {
    r$entry_paths |> mutate(contest_name = r$contest_name, contest_id = r$contest_id)
  })

  list(
    per_contest = results,
    field_own   = combined_field,
    our_own     = combined_our,
    entry_paths = combined_paths
  )
}


# ==============================================================================
# SUMMARY HELPERS
# ==============================================================================

#' Print field ownership for a specific day across all contests
#' @param results Output from scrape_all_splash()
#' @param day_num Day number to display
print_field_ownership <- function(results, day_num) {
  results$field_own |>
    filter(day == day_num) |>
    group_by(contest_name, team) |>
    summarise(ownership = sum(ownership), .groups = "drop") |>
    arrange(contest_name, desc(ownership)) |>
    print(n = 100)
}

#' Get aggregate field ownership across contests, weighted by entry count
#' @param results Output from scrape_all_splash()
#' @param day_num Day number
aggregate_field_ownership <- function(results, day_num) {
  results$field_own |>
    filter(day == day_num) |>
    group_by(team) |>
    summarise(
      total_picks   = sum(picks),
      total_entries = sum(day_entries),
      avg_ownership = total_picks / total_entries,
      n_contests    = n(),
      .groups       = "drop"
    ) |>
    arrange(desc(avg_ownership))
}

#' Compare our ownership vs field ownership for a day
#' @param results Output from scrape_all_splash()
#' @param day_num Day number
compare_ownership <- function(results, day_num) {
  field <- aggregate_field_ownership(results, day_num) |>
    select(team, field_own = avg_ownership)

  our <- results$our_own |>
    filter(day == day_num) |>
    group_by(team) |>
    summarise(
      our_picks   = sum(picks),
      our_entries = sum(day_entries),
      our_own     = our_picks / our_entries,
      .groups     = "drop"
    ) |>
    select(team, our_own)

  full_join(field, our, by = "team") |>
    mutate(
      field_own = replace_na(field_own, 0),
      our_own   = replace_na(our_own, 0),
      diff      = our_own - field_own
    ) |>
    arrange(desc(field_own))
}
