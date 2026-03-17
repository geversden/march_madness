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
  Authorization = "Bearer eyJraWQiOiJENHJOR1pwNStnTzAxS21aVkg5YlZDZUd2bGNGYUNJSm1qVm5VOE4waUl3PSIsImFsZyI6IlJTMjU2In0.eyJmcmF1ZEZsYWciOiIiLCJzdWIiOiIwNDE4NzQzOC1kMDMxLTcwMTUtNjI3OC1hMmVlZDAyOTJlNjMiLCJyb2xlIjoiY29tbWlzc2lvbmVyIiwiZW1haWxfdmVyaWZpZWQiOiJ0cnVlIiwicm9sZXMiOiJbXCJjb21taXNzaW9uZXJcIl0iLCJpc3MiOiJodHRwczpcL1wvY29nbml0by1pZHAudXMtZWFzdC0xLmFtYXpvbmF3cy5jb21cL3VzLWVhc3QtMV82NEI5Qm5DNWciLCJyZXN0cmljdGlvbnMiOiJbXSIsImNvZ25pdG86dXNlcm5hbWUiOiI5OGQxNTQyMy1iOTI1LTRjMzctOGQxMy1jMDhlZTljNWIyODQiLCJyeXBfdXNlcl9pZCI6IjQwODg4NDIiLCJvcmlnaW5fanRpIjoiNWMyZDI4YTAtMTE3MS00NTM4LTlhYTYtYWMwNDhiZWQ0OGQwIiwiYXVkIjoiNTloYmhiamhrYWY5ODRtZW1vYzlmZ2ExM3EiLCJldmVudF9pZCI6IjhkM2RhY2E0LWQzODItNDA2MC1iMmM4LTE2NDNkZTkxY2FlMCIsInNwbGFzaF91c2VyX2lkIjoiOThkMTU0MjMtYjkyNS00YzM3LThkMTMtYzA4ZWU5YzViMjg0IiwidG9rZW5fdXNlIjoiaWQiLCJhdXRoX3RpbWUiOjE3NzIwNTIxOTcsIm5hbWUiOiI5OGQxNTQyMy1iOTI1LTRjMzctOGQxMy1jMDhlZTljNWIyODQiLCJpZCI6Ijk4ZDE1NDIzLWI5MjUtNGMzNy04ZDEzLWMwOGVlOWM1YjI4NCIsImV4cCI6MTc3MzcxODUyNiwiaWF0IjoxNzczNzE0OTI2LCJhZ2UiOiI1NiIsImp0aSI6Ijc4ZTcxMWZiLTMzM2YtNDVhOC1hYjdlLTg0ZjQ4M2MxOWIwMiIsInVzZXJuYW1lIjoiSlN5ZCJ9.F9MeHb-zq9CpDnW_SmF17rL4-IJS8Bmm3cEk4G07C9RCdvezgaFTY0zRwsn1_4BTpSWVhEkdMdvB_qhVYcNA_fy3h9Bm2Tq5WOOd1fZ7myU1qCg0Lw5dq5wUQI5UioXE3O84bdqnGnrwHv6vxdF1w4jPIG9qc-AEyEdD1AfdUg_95XlVRbYw9oYibM9jPnAPxS1rLUXqh4bvVdiIZPxTy4cUFvyHQQlSt1OiOTWgfdYkp6Dbb9rNnpsd1Y6DYFbXb3B5hpoh7aRzqcu8DDIQ83wbUYh1goq5IA41VnS-tc1kSXJBWBkj-VkX9p-Fku0PtkbiFW8oYxzXlKdvKD8LLA",
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
    day1 = `b071bc1b-0242-4889-9af6-42239b934fca`,
    day2 = `bf851ad7-d814-468f-8e20-c8c469bab8a2`,
    day3 = `82fc6804-c444-4d53-af8a-3a527c318fef`,
    day4 = `5d7846ea-38a8-4aa9-84a3-b6e44da95b26`,
    day5 = `824beac4-fd0b-4563-aa65-eb21f4f1d417`,
    day6 = `a1629a51-9c51-485b-bebf-43c6180e08e7`,
    day7_8 = `4c30852e-b1fe-49fc-96ae-01ad2a61aba9`,
    day9 = `86522ba4-8289-4e42-9e1c-fffb2af95778`,
    day10 = `c3b94232-9c06-4713-8a3e-54cfff85de45`
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

