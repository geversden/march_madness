library(httr)
library(tidyverse)
library(httr)

headers = c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:146.0) Gecko/20100101 Firefox/146.0",
  Accept = "application/json",
  `Accept-Language` = "en-US,en;q=0.5",
  `Accept-Encoding` = "gzip, deflate",
  Referer = "https://app.splashsports.com/",
  `Content-Type` = "application/json",
  `X-App-Platform` = "web",
  `X-App-Version` = "1.220.0",
  Authorization = "Bearer eyJraWQiOiJENHJOR1pwNStnTzAxS21aVkg5YlZDZUd2bGNGYUNJSm1qVm5VOE4waUl3PSIsImFsZyI6IlJTMjU2In0.eyJmcmF1ZEZsYWciOiJ2ZXJpZmllZC1hY2NvdW50Iiwic3ViIjoiMTQ0OGE0MTgtOTBhMS03MDlhLTBmMTAtZDc0Y2MxOTUzMGMwIiwicm9sZSI6ImNvbW1pc3Npb25lciIsImVtYWlsX3ZlcmlmaWVkIjoidHJ1ZSIsInJvbGVzIjoiW1wiY29tbWlzc2lvbmVyXCJdIiwiaXNzIjoiaHR0cHM6XC9cL2NvZ25pdG8taWRwLnVzLWVhc3QtMS5hbWF6b25hd3MuY29tXC91cy1lYXN0LTFfNjRCOUJuQzVnIiwicmVzdHJpY3Rpb25zIjoiW10iLCJjb2duaXRvOnVzZXJuYW1lIjoiMTIyNGZjOTgtMDViNy00ZWQ3LTg5NmItZDhiMTZjMTk1ZTEyIiwib2ZwX3VzZXJfaWQiOiI0MzY4MzA2IiwicnlwX3VzZXJfaWQiOiIxNTMyNDA0Iiwib3JpZ2luX2p0aSI6ImI5MWI0YmJjLWU0ZDEtNDY1Yy1hNjk1LWU4OGVhZTg1NDhhZSIsImF1ZCI6IjU5aGJoYmpoa2FmOTg0bWVtb2M5ZmdhMTNxIiwiZXZlbnRfaWQiOiIyYWU4ZDE1ZS00NGFmLTQ4ZWUtOTI5NC1jYjQwYzAyZWU1ZTciLCJzcGxhc2hfdXNlcl9pZCI6IjEyMjRmYzk4LTA1YjctNGVkNy04OTZiLWQ4YjE2YzE5NWUxMiIsInRva2VuX3VzZSI6ImlkIiwiYXV0aF90aW1lIjoxNzczMjkzNTIzLCJuYW1lIjoiMTIyNGZjOTgtMDViNy00ZWQ3LTg5NmItZDhiMTZjMTk1ZTEyIiwiaWQiOiIxMjI0ZmM5OC0wNWI3LTRlZDctODk2Yi1kOGIxNmMxOTVlMTIiLCJleHAiOjE3NzM3Nzc4NTcsImlhdCI6MTc3Mzc3NDI1NywiYWdlIjoiMzciLCJqdGkiOiJlNzI3N2Y0MC1iMTgyLTRiOWItYjlmMy0zYjMzYTM2YjViZWQiLCJ1c2VybmFtZSI6IlRpbmt5VHlsZXIifQ.rS8K9wnW6i9AMINPmynjljBp5CEQ-KdwzMchlNLeNRzN_IvEy2Le0-Cr15Zomuo0QK4sraU_etkpKjPSOs056sDrks4R7-5UFUYVn4dfjPNcS0FTb_fnBIvJTuyTUmMi7SQq0dTDJouRBGkpcjdqm5pMqLCtvHP1iQI8Yv5ZUnt1s_MYnnvfqVF0KGqI0cr_T5YoJCrbVyqBSoyR9aD9lglD7jsa1g6LmiIQ6ER0z-LJ0RgRg4orxSmpY8K4LZIDJK4h6OSf3XLRtSv69ipaS0PhmC6igrHPe4psLAFVnVEl_Id98GTYxAtZSjL2LUF6kqGI96mkInm-9QHKdnm9zA",
  Origin = "https://app.splashsports.com",
  DNT = "1",
  `Sec-GPC` = "1",
  Connection = "keep-alive",
  `Sec-Fetch-Dest` = "empty",
  `Sec-Fetch-Mode` = "cors",
  `Sec-Fetch-Site` = "same-site"
)

# -------------------------
# FIRST REQUEST (get total)
# -------------------------
params <- list(
  limit  = "100",
  offset = "0"
)

res <- httr::GET(
  # 2024
  url = "https://api.splashsports.com/contests-service/api/contests/ab5145fd-1260-46a0-8590-1b5895b7b2af/picks/available",
  httr::add_headers(.headers = headers),
  query = params
)

json_obj <- content(res, simplifyVector = TRUE)

total <- json_obj$total
limit <- 100

# -------------------------
# LOOP THROUGH ALL PAGES
# -------------------------
all_rows <- list()

for (off in seq(0, total - 1, by = limit)) {

  params <- list(
    limit  = limit,
    offset = off
  )

  res <- httr::GET(
    # 2024
    url = "https://api.splashsports.com/contests-service/api/contests/ab5145fd-1260-46a0-8590-1b5895b7b2af/picks/available",
    httr::add_headers(.headers = headers),
    query = params
  )

  json_obj <- content(res, simplifyVector = TRUE)

  if (length(json_obj$data) == 0) break

  all_rows[[length(all_rows) + 1]] <- json_obj$data

  Sys.sleep(0.15)
}

# -------------------------
# FLATTEN + EXTRACT ENTRY IDS
# -------------------------
# entries <-
#   all_rows |>
#   unlist(recursive = FALSE) |>
#   map_df(as_tibble, .name_repair = "unique")

entries <-
  tibble(all_rows = all_rows) |>
  unnest(all_rows)

entry_ids <- unique(entries$entryId)

length(entry_ids)

library(purrr)
library(httr)

batch_size <- 100

# Split into batches
entry_batches <- split(
  entry_ids,
  ceiling(seq_along(entry_ids) / batch_size)
)

# Fetch data with batch and attach entry_ids
responses <- map(entry_batches, \(ids) {
  res <- POST(
    url = "https://api.splashsports.com/contests-service/api/team-picks",
    add_headers(.headers = headers),
    body = list(entryIds = ids),
    encode = "json"
  )

  # Include the entry IDs with the raw response for tracking
  list(
    entry_ids = ids,
    response = res
  )
})

# Parse JSON and keep entry ID association
json_responses <- map(
  responses,
  \(x) {
    data <- content(x$response, simplifyVector = TRUE)

    # Add entry_id to each returned pick if not already included
    if(!"entryId" %in% names(data)) {
      # Assuming data is a list of picks in same order as x$entry_ids
      data <- map2(data, x$entry_ids, ~ c(.x, entryId = .y))
    }

    data
  }
)

entries <-
  tibble(resp = json_responses) |>
  unnest(resp) |>
  unnest_wider(resp) |>
  unnest_wider(picks) |>
  rename(
    day1 = `aa9ba724-ff31-4fc4-b156-d059a2e7d03d`,  # previous day6
    day2 = `4a5ee0d3-95e8-4771-b6af-08af7d68aa19`,  # previous day3
    day3 = `3bce540b-d4d7-4d21-8609-45c011eb773a`,  # previous day2
    day4 = `cc440a4e-b7be-469a-98ef-ae7896b94efe`,  # previous day9
    day5 = `818f047d-9b9a-4381-b123-710bc84e0c55`,  # previous day4
    day6 = `22564518-e45a-4a30-8d01-47309a1aa154`,  # previous day1
    day7_8 = `45f6ab06-7b18-4840-b605-1bac50053bc4`,  # previous day7_8
    day9 = `92e7568c-75a7-4479-9bf6-968318dacdec`,  # previous day5
    day10 = `7b66d724-e71a-4349-b2fa-5d7d1e5394c5`  # previous day10
  ) |>
  unnest_wider(day1, names_sep = "_") |>
  unnest_wider(day2, names_sep = "_") |>
  unnest_wider(day3, names_sep = "_") |>
  unnest_wider(day4, names_sep = "_") |>
  unnest_wider(day5, names_sep = "_") |>
  unnest_wider(day6, names_sep = "_") |>
  unnest_wider(day9, names_sep = "_") |>
  unnest_wider(day10, names_sep = "_") |>
  # --- handle day7_8 picks ---
  rowwise() |>
  mutate(
    day7_8_sorted = list({
      teams <- if (is.data.frame(day7_8)) day7_8$teamName else character(0)
      teams <- sort(teams, na.last = TRUE)
      length(teams) <- 2
      set_names(teams, c("day7_teamName", "day8_teamName"))
    })
  ) |>
  ungroup() |>
  # expand the two picks into separate columns
  unnest_wider(day7_8_sorted)

ownership <-
  entries |>
  select(
    entryId,
    matches("^day\\d+_teamName$")
  ) |>
  pivot_longer(
    cols = -entryId,
    names_to = "day",
    values_to = "team"
  ) |>
  filter(!is.na(team)) |>
  group_by(day) |>
  mutate(day_total = n()) |>
  group_by(day, team) |>
  summarise(
    picks = n(),
    ownership = picks / first(day_total),
    .groups = "drop"
  ) |>
  mutate(year = 2025) |>
  arrange(day, desc(ownership))

ownership |>
  group_by(day) |>
  summarise(total_ownership = sum(picks))

write_csv(ownership, "splash_ownership/ownership_2025.csv")

