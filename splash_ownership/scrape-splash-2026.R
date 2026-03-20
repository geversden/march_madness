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
  Authorization = "Bearer eyJraWQiOiJENHJOR1pwNStnTzAxS21aVkg5YlZDZUd2bGNGYUNJSm1qVm5VOE4waUl3PSIsImFsZyI6IlJTMjU2In0.eyJmcmF1ZEZsYWciOiIiLCJzdWIiOiIwNDE4NzQzOC1kMDMxLTcwMTUtNjI3OC1hMmVlZDAyOTJlNjMiLCJyb2xlIjoiY29tbWlzc2lvbmVyIiwiZW1haWxfdmVyaWZpZWQiOiJ0cnVlIiwicm9sZXMiOiJbXCJjb21taXNzaW9uZXJcIl0iLCJpc3MiOiJodHRwczpcL1wvY29nbml0by1pZHAudXMtZWFzdC0xLmFtYXpvbmF3cy5jb21cL3VzLWVhc3QtMV82NEI5Qm5DNWciLCJyZXN0cmljdGlvbnMiOiJbXSIsImNvZ25pdG86dXNlcm5hbWUiOiI5OGQxNTQyMy1iOTI1LTRjMzctOGQxMy1jMDhlZTljNWIyODQiLCJyeXBfdXNlcl9pZCI6IjQwODg4NDIiLCJvcmlnaW5fanRpIjoiYzkzYzQ3YmMtYTM0ZS00NTdkLTk4YWQtY2I4YTQ0NGE1NDhiIiwiYXVkIjoiNTloYmhiamhrYWY5ODRtZW1vYzlmZ2ExM3EiLCJldmVudF9pZCI6Ijk1OTQ1ZWZlLTg5ZTQtNDA2NS05MDk4LTIxMzdkMGUzNDc3MyIsInNwbGFzaF91c2VyX2lkIjoiOThkMTU0MjMtYjkyNS00YzM3LThkMTMtYzA4ZWU5YzViMjg0IiwidG9rZW5fdXNlIjoiaWQiLCJhdXRoX3RpbWUiOjE3NzM5MzYwOTYsIm5hbWUiOiI5OGQxNTQyMy1iOTI1LTRjMzctOGQxMy1jMDhlZTljNWIyODQiLCJpZCI6Ijk4ZDE1NDIzLWI5MjUtNGMzNy04ZDEzLWMwOGVlOWM1YjI4NCIsImV4cCI6MTc3NDAyNzYwMywiaWF0IjoxNzc0MDI0MDAzLCJhZ2UiOiI1NiIsImp0aSI6IjFlMmM2ZmFhLTg1ZjEtNDFiOS1iODE0LTViYWY3OWY5MzYyZSIsInVzZXJuYW1lIjoiSlN5ZCJ9.Aj4ShilmULz-446dsHbntXFqNwCSbWwzpTs77c0JH_UZd_PkbvVs-j5P7x0ZJF9OBLRudiOM7LShxihCIsSuIZ3ETkNwJ3RMP7cfbvE8-yVgbspkpbJCUteWIK9wW--eLQ3wneUZk-P5kONPFbVZAQf-l4RTb6cxtNoxut8_jqeHctLr_e9K9KqhEiu05AKlLfBXRRgIScXwf0V3JssPiBaq0_XWIM618BKQyn2_CA4HtGeljydfTzQINCcafrtJ5kArxzoKXMbWjSvAv_ZhdsoJuo9sG-3ES-m_d1MeoFoNmjDZHPHJVgrlHwvPGWZr3hDolCb83Df93aol2_NqnA",
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
  url = "https://api.splashsports.com/contests-service/api/contests/4297079f-b5ec-47ce-9283-d865ec87c805/picks/available",
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
    url = "https://api.splashsports.com/contests-service/api/contests/4297079f-b5ec-47ce-9283-d865ec87c805/picks/available",
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

