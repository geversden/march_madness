# ==============================================================================
# scrape_splash_contests.R
# Scrape Splash Sports NCAA survivor contests for field + our ownership
#
# Usage:
#   results <- scrape_all_splash(bearer_token = "eyJ...")
#
#   # Results contain:
#   #   results$field_own   - field ownership by team by day by contest
#   #   results$our_own     - our ownership by team by day by contest
#   #   results$entry_paths - full entry-level paths for sim integration
# ==============================================================================

library(httr)
library(purrr)
library(dplyr)
library(tidyr)

# ==============================================================================
# CONTEST CONFIGURATION
# ==============================================================================

#' Splash contest definitions for 2026 NCAA survivor
#' @export
splash_contests <- data.frame(
  contest_name = c(
    "Kelly's $250K Survivor Madness",
    "Field of 68 Survivor Madness",
    "Ryan Hammer's $100K Survivor Madness",
    "Bet The Process Survivor Madness",
    "Ross Tucker's $35K Survivor Madness",
    "Frank Michael Smith's $30K Survivor Madness",
    "Kurt Benkert's $100K Survivor Madness",
    "Spooky Express $30K Guaranteed Survivor Madness",
    "SGPN $25K Survivor Madness",
    "MARCH MADNESS SURVIVOR",
    "FOR THE FANS - SURVIVOR MADNESS"
  ),
  contest_id = c(
    "9eba15b3-e62d-42f0-b399-5957cd33d62c",
    "3f84b670-bd9e-4786-967a-f7cc63821c8c",
    "a68d9a2b-7317-4e43-9ddb-c0fa5f7b96ec",
    "28f81d62-3fac-4c0f-ae18-159a3307d9c5",
    "80db5738-a417-4524-b4a3-24be60c63fd4",
    "96acf9b2-2def-44ee-add0-d67df18ba470",
    "b3f535dc-b014-4cd6-93a1-546f800caf1a",
    "6cc0a29e-86bb-4a1a-8251-2b581ad80320",
    "919e7294-fd5d-4ae0-9b87-e8969d657282",
    "3950c489-4150-4a19-bf01-0056a46d81c1",
    "cf3889a3-84a2-4165-990b-891f67e26f9b"
  ),
  fee = c(25, 25, 15, 100, 50, 30, 5, 75, 25, 10, 40),
  prize_pool = c(250020, 100012.50, 100008, 50040, 35010, 30024, 100003.50, 30037.50, 25020, 8750, 25305),
  format = c(
    "2-2-2-2-1-1", "2-2-2-2-1-1", "2-2-2-2-1-1", "2-2-2-2-1-1",
    "2-2-2-2-1-1", "2-2-2-2-1-1", "4-4-2-2-1-1", "2-2-2-2-1-1",
    "2-2-2-2-1-1", "2-2-2-2-1-1", "4-4-2-2-1-1"
  ),
  stringsAsFactors = FALSE
)

# ==============================================================================
# API HELPERS
# ==============================================================================

#' Build headers for Splash API requests
#' @param bearer_token Character string - the JWT token from browser network tab
#' @return Named character vector of HTTP headers
#' @export
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
#' @return List with entries_df, pick_long, field_own, our_own, entry_paths, slate_map
#' @export
scrape_splash_contest <- function(contest_id, headers, our_username = "TinkyTyler") {

  base_url <- "https://api.splashsports.com/contests-service/api/contests"

  # -------------------------------------------
  # Step 1: Paginate through entries
  # -------------------------------------------
  cat(sprintf("  Fetching entries for %s...\n", contest_id))

  res <- GET(
    url = paste0(base_url, "/", contest_id, "/picks/available"),
    add_headers(.headers = headers),
    query = list(limit = 100, offset = 0)
  )

  if (status_code(res) != 200) {
    warning(sprintf("Failed to fetch contest %s (HTTP %d). Token may be expired.",
                    contest_id, status_code(res)))
    return(NULL)
  }

  json_obj <- content(res, simplifyVector = TRUE)
  total <- json_obj$total
  cat(sprintf("    Total entries: %d\n", total))

  all_rows <- list()
  limit <- 100

  for (off in seq(0, total - 1, by = limit)) {
    res <- GET(
      url = paste0(base_url, "/", contest_id, "/picks/available"),
      add_headers(.headers = headers),
      query = list(limit = limit, offset = off)
    )

    json_obj <- content(res, simplifyVector = TRUE)
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

  responses <- map(entry_batches, \(ids) {
    res <- POST(
      url = "https://api.splashsports.com/contests-service/api/team-picks",
      add_headers(.headers = headers),
      body = list(entryIds = ids),
      encode = "json"
    )
    Sys.sleep(0.15)

    list(
      entry_ids = ids,
      response  = res
    )
  })

  json_responses <- map(responses, \(x) {
    data <- content(x$response, simplifyVector = TRUE)
    if (!"entryId" %in% names(data)) {
      data <- map2(data, x$entry_ids, ~ c(.x, entryId = .y))
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

  picks_wide <- picks_raw |>
    unnest_wider(picks)

  # Slate GUIDs as column names

  slate_cols <- names(picks_wide)[grepl("^[0-9a-f]{8}-", names(picks_wide))]
  cat(sprintf("    Found %d slates (days)\n", length(slate_cols)))

  # -------------------------------------------
  # Step 4: Extract team names from each slate
  # -------------------------------------------
  pick_long <- map_dfr(seq_along(slate_cols), function(i) {
    slate_id <- slate_cols[i]
    col_data <- picks_wide[[slate_id]]

    if (is.null(col_data)) return(tibble())

    tryCatch({
      entry_ids_col <- picks_wide$entryId

      if (is.data.frame(col_data)) {
        result <- tibble(entryId = entry_ids_col)

        if ("teamName" %in% names(col_data)) {
          result$team <- col_data$teamName
        } else if ("teamAlias" %in% names(col_data)) {
          if (is.list(col_data$teamAlias)) {
            result$team <- map_chr(col_data$teamAlias, ~ {
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
        map2_dfr(col_data, entry_ids_col, function(pick, eid) {
          if (is.null(pick) || length(pick) == 0) return(tibble())

          if (is.data.frame(pick)) {
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
#' @return List with per_contest, field_own, our_own, entry_paths
#' @export
scrape_all_splash <- function(bearer_token,
                              our_username = "TinkyTyler",
                              contests = splash_contests) {

  headers <- make_headers(bearer_token)

  cat(sprintf("Scraping %d Splash contests...\n\n", nrow(contests)))

  results <- map2(contests$contest_id, contests$contest_name, function(cid, cname) {
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
  results <- compact(results)

  if (length(results) == 0) {
    stop("All contests failed. Bearer token is likely expired.")
  }

  cat(sprintf("\nSuccessfully scraped %d / %d contests.\n", length(results), nrow(contests)))

  # Combine across contests
  combined_field <- map_dfr(results, function(r) {
    r$field_own |> mutate(contest_name = r$contest_name, contest_id = r$contest_id)
  })

  combined_our <- map_dfr(results, function(r) {
    r$our_own |> mutate(contest_name = r$contest_name, contest_id = r$contest_id)
  })

  combined_paths <- map_dfr(results, function(r) {
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
#' @export
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
#' @return Tibble with team, total_picks, total_entries, avg_ownership, n_contests
#' @export
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
#' @return Tibble with team, field_own, our_own, diff
#' @export
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
