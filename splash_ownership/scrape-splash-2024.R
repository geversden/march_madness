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
  url = "https://api.splashsports.com/contests-service/api/contests/7717ab88-f3a4-4f31-af0e-42d90b9277e1/picks/available",
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
    url = "https://api.splashsports.com/contests-service/api/contests/7717ab88-f3a4-4f31-af0e-42d90b9277e1/picks/available",
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
  mutate(year = 2024) |>
  arrange(day, desc(ownership))

ownership |>
  group_by(day) |>
  summarise(total_ownership = sum(picks))

write_csv(ownership, "splash_ownership/ownership_2024.csv")


# =========================================================
# 2025 MARCH MADNESS
# =========================================================
# Day mapping (9 slates — derived from unique team counts across 1000 entries):
#   day1  = 4a5ee0d3  Day 1 (Round of 64)  — 16 unique teams
#   day2  = aa9ba724  Day 2 (Round of 64)  — 15 unique teams
#   day3  = 818f047d  Day 3 (Round of 32)  — 4 unique: ALA/TTU/DUKE/FLA   (verify order)
#   day4  = 22564518  Day 4 (Round of 32)  — 4 unique: HOU/MSU/TENN/AUB   (verify order)
#   day5  = cc440a4e  Day 5 (Round of 16)  — 8 unique: MD/UK/ARIZ/MSU/ALA/MISS/DUKE/FLA (verify order)
#   day6  = 3bce540b  Day 6 (Round of 16)  — 8 unique: PUR/TTU/TENN/BYU/MICH/ARK/AUB/HOU (verify order)
#   day7_8= 45f6ab06  Day 7+8 (Elite 8)    — double pick, DUKE/AUB/HOU/FLA
#   day9  = 92e7568c  Day 9 (Final Four)   — FLA/HOU/DUKE/AUB
#   day10 = 7b66d724  National Championship — HOU/FLA

# -------------------------
# FIRST REQUEST (get total)
# -------------------------
params_25 <- list(limit = "100", offset = "0")

res_25 <- httr::GET(
  url = "https://api.splashsports.com/contests-service/api/contests/ab5145fd-1260-46a0-8590-1b5895b7b2af/picks/available",
  httr::add_headers(.headers = headers),
  query = params_25
)

json_obj_25 <- content(res_25, simplifyVector = TRUE)
total_25 <- json_obj_25$total
limit_25  <- 100

# -------------------------
# LOOP THROUGH ALL PAGES
# -------------------------
all_rows_25 <- list()

for (off in seq(0, total_25 - 1, by = limit_25)) {
  params_25 <- list(limit = limit_25, offset = off)

  res_25 <- httr::GET(
    url = "https://api.splashsports.com/contests-service/api/contests/ab5145fd-1260-46a0-8590-1b5895b7b2af/picks/available",
    httr::add_headers(.headers = headers),
    query = params_25
  )

  json_obj_25 <- content(res_25, simplifyVector = TRUE)
  if (length(json_obj_25$data) == 0) break
  all_rows_25[[length(all_rows_25) + 1]] <- json_obj_25$data
  Sys.sleep(0.15)
}

entries_25_raw <- tibble(all_rows = all_rows_25) |> unnest(all_rows)
entry_ids_25   <- unique(entries_25_raw$entryId)
length(entry_ids_25)

# -------------------------
# BATCH FETCH PICKS
# -------------------------
entry_batches_25 <- split(entry_ids_25, ceiling(seq_along(entry_ids_25) / 100))

responses_25 <- map(entry_batches_25, \(ids) {
  res <- POST(
    url = "https://api.splashsports.com/contests-service/api/team-picks",
    add_headers(.headers = headers),
    body = list(entryIds = ids),
    encode = "json"
  )
  list(entry_ids = ids, response = res)
})

# -------------------------
# PARSE
# -------------------------
json_responses_25 <- map(responses_25, \(x) {
  data <- content(x$response, simplifyVector = TRUE)
  if (!"entryId" %in% names(data)) {
    data <- map2(data, x$entry_ids, ~ c(.x, entryId = .y))
  }
  data
})

entries_25 <-
  tibble(resp = json_responses_25) |>
  unnest(resp) |>
  unnest_wider(resp) |>
  unnest_wider(slates) |>
  rename(
    day1   = `4a5ee0d3-95e8-4771-b6af-08af7d68aa19`,
    day2   = `aa9ba724-ff31-4fc4-b156-d059a2e7d03d`,
    day3   = `818f047d-9b9a-4381-b123-710bc84e0c55`,
    day4   = `22564518-e45a-4a30-8d01-47309a1aa154`,
    day5   = `cc440a4e-b7be-469a-98ef-ae7896b94efe`,
    day6   = `3bce540b-d4d7-4d21-8609-45c011eb773a`,
    day7_8 = `45f6ab06-7b18-4840-b605-1bac50053bc4`,
    day9   = `92e7568c-75a7-4479-9bf6-968318dacdec`,
    day10  = `7b66d724-e71a-4349-b2fa-5d7d1e5394c5`
  ) |>
  unnest_wider(day1,   names_sep = "_") |>
  unnest_wider(day2,   names_sep = "_") |>
  unnest_wider(day3,   names_sep = "_") |>
  unnest_wider(day4,   names_sep = "_") |>
  unnest_wider(day5,   names_sep = "_") |>
  unnest_wider(day6,   names_sep = "_") |>
  unnest_wider(day9,   names_sep = "_") |>
  unnest_wider(day10,  names_sep = "_") |>
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
  unnest_wider(day7_8_sorted)

# -------------------------
# OWNERSHIP
# -------------------------
ownership_25 <-
  entries_25 |>
  select(entryId, matches("^day\\d+_teamName$")) |>
  pivot_longer(cols = -entryId, names_to = "day", values_to = "team") |>
  filter(!is.na(team)) |>
  group_by(day) |>
  mutate(day_total = n()) |>
  group_by(day, team) |>
  summarise(
    picks     = n(),
    ownership = picks / first(day_total),
    .groups   = "drop"
  ) |>
  mutate(year = 2025) |>
  arrange(day, desc(ownership))

ownership_25 |>
  group_by(day) |>
  summarise(total_ownership = sum(picks))

write_csv(ownership_25, "splash_ownership/ownership_2025.csv")




# 
# # 2025
# r2_matchups <- tibble(
#   team = c("DEN","BUF","SF","SEA","LA","CHI","HOU","NE"),
#   opponent = c("BUF","DEN","SEA","SF","CHI","LA","NE","HOU")
# )
# 
# 
# entries_flagged <-
#   entries %>%
#   left_join(r2_matchups, by = c("r2_teams_1" = "team")) %>%
#   rename(r2_opp_1 = opponent) %>%
#   left_join(r2_matchups, by = c("r2_teams_2" = "team")) %>%
#   rename(r2_opp_2 = opponent)
# 
# entries_flagged <-
#   entries_flagged %>%
#   rowwise() %>%
#   mutate(
#     doa_flag = case_when("HOU" %in% c(r1_teams_1, r1_teams_2) &
#                            "NE" %in% c(r1_teams_1, r1_teams_2) ~ TRUE,
#                          "CHI" %in% c(r1_teams_1, r1_teams_2) &
#                            "LA" %in% c(r1_teams_1, r1_teams_2) ~ TRUE,
#                          TRUE ~ FALSE),
#     num_pivots = case_when(!(r2_opp_1 %in% c(r1_teams_1, r1_teams_2)) &
#                              !(r2_opp_2 %in% c(r1_teams_1, r1_teams_2)) ~ "double_pivot",
#                                !(r2_opp_1 %in% c(r1_teams_1, r1_teams_2)) |
#                                  !(r2_opp_2 %in% c(r1_teams_1, r1_teams_2)) ~ "single_pivot",
#                                    TRUE ~ "no_pivot"),
#     pivot_flag =
#       !(r2_opp_1 %in% c(r1_teams_1, r1_teams_2)) |
#       !(r2_opp_2 %in% c(r1_teams_1, r1_teams_2))
#   ) %>%
#   ungroup()
# 
# 
# entries_flagged |> 
#   group_by(doa_flag, pivot_flag, num_pivots) |> 
#   tally()
# 
# 
# entries <- 
#   tibble(resp = json_responses) |> 
#   unnest(resp) |> 
#   unnest_wider(resp) |> 
#   unnest_wider(picks) |> 
#   unnest_wider(`09160cd4-bfd7-44e9-ac24-48201aad6bf6`)
# 
# entries_joins <- 
#   tibble(all_rows = all_rows) |> 
#   unnest(all_rows)
# 
# entries_clean <- 
#   entries |> 
#   # unnest_wider(teamAlias, names_sep = "_") |> 
#   left_join(entries_joins |> select(entryId, entryName),
#             by = c("entryId"))
# 
# entries_clean |> 
#   unnest_wider(record) |>
#   mutate(
#     # team1 = pmin(teamAlias_1, teamAlias_2),
#     # team2 = pmax(teamAlias_1, teamAlias_2),
#     # team1 = if_else(team1 == "LA", "LAR", team1),
#     # team2 = if_else(team2 == "LA", "LAR", team2),
#     # team1 = if_else(team1 == "JAC", "JAX", team1),
#     # team2 = if_else(team2 == "JAC", "JAX", team2),
#     label = glue::glue("{r1_teams_1}_{r1_teams_2}_{r2_teams_1}_{r2_teams_2}_{r3_teamAlias}")
#   ) |>
#   summarise(
#     entry_count = n(),
#     alive = sum(is.na(eliminatedSlateId)),
#     .by = c(entryName, label)
#   ) |> 
#   pivot_wider(id_cols = entryName,
#               names_from = label,
#               values_from = entry_count) |>
#   mutate(total = rowSums(across(-entryName), na.rm = TRUE)) |> 
#   clipr::write_clip()
#   
