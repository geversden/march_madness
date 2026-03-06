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
  Authorization = "Bearer eyJraWQiOiJENHJOR1pwNStnTzAxS21aVkg5YlZDZUd2bGNGYUNJSm1qVm5VOE4waUl3PSIsImFsZyI6IlJTMjU2In0.eyJmcmF1ZEZsYWciOiIiLCJzdWIiOiIwNDE4NzQzOC1kMDMxLTcwMTUtNjI3OC1hMmVlZDAyOTJlNjMiLCJyb2xlIjoiY29tbWlzc2lvbmVyIiwiZW1haWxfdmVyaWZpZWQiOiJ0cnVlIiwicm9sZXMiOiJbXCJjb21taXNzaW9uZXJcIl0iLCJpc3MiOiJodHRwczpcL1wvY29nbml0by1pZHAudXMtZWFzdC0xLmFtYXpvbmF3cy5jb21cL3VzLWVhc3QtMV82NEI5Qm5DNWciLCJyZXN0cmljdGlvbnMiOiJbXSIsImNvZ25pdG86dXNlcm5hbWUiOiI5OGQxNTQyMy1iOTI1LTRjMzctOGQxMy1jMDhlZTljNWIyODQiLCJyeXBfdXNlcl9pZCI6IjQwODg4NDIiLCJvcmlnaW5fanRpIjoiYjE0N2IyNTQtMTNjMi00YTI1LWI3ODctNDQ3NTM0ZTRlMTU5IiwiYXVkIjoiNTloYmhiamhrYWY5ODRtZW1vYzlmZ2ExM3EiLCJldmVudF9pZCI6ImJiMzdlNDlkLTlhMzctNDgzZC04N2JlLWZlOGYxZjVkZjdiOCIsInNwbGFzaF91c2VyX2lkIjoiOThkMTU0MjMtYjkyNS00YzM3LThkMTMtYzA4ZWU5YzViMjg0IiwidG9rZW5fdXNlIjoiaWQiLCJhdXRoX3RpbWUiOjE3Njg1OTg3NzYsIm5hbWUiOiI5OGQxNTQyMy1iOTI1LTRjMzctOGQxMy1jMDhlZTljNWIyODQiLCJpZCI6Ijk4ZDE1NDIzLWI5MjUtNGMzNy04ZDEzLWMwOGVlOWM1YjI4NCIsImV4cCI6MTc2OTM3Nzc4MSwiaWF0IjoxNzY5Mzc0MTgxLCJhZ2UiOiI1NiIsImp0aSI6ImZkNWViYmZkLTYzZGUtNDdlMS05MzcxLTQ0ODE2NjcyOGRlZCIsInVzZXJuYW1lIjoiSlN5ZCJ9.rNbvGFomt5XrrEGIInAt42TZs_vpwr5dlOnRf1w3S84IhpHcXhDrudFEhzRH_8pjzRw1BGaICcubrDo2SvOQkPaNEh5Zp2kdmhTMbrum3t2P8lImC-fEANK2v36dOxlXY2XcphS36-dsY6rtPJbmFwF3uBXoaQ42V31AD8qSIaGunF_LUyrmBypPmsP1eejzREWJfUEpafA7PlQd1j8NyfkKPHKZ6Dd5bcrsGMCT626IWvegYTJMdJMq6ccEORIsD_XRpk6I58yImai_jNdOi3u1GDfze9RudWtst7Sft9ePbk0-We1idVwMN7SaElmWEJZFmySWeXv4-NZ4StY42A",
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
  # url = "https://api.splashsports.com/contests-service/api/contests/e2cd862c-6af7-479e-ab58-484521c0eaf4/picks/available",
  
  # url = "https://api.splashsports.com/contests-service/api/contests/89eb2193-b7be-4df5-9aa1-0404788ff032/picks/available",
  # url = "https://api.splashsports.com/contests-service/api/contests/f5efdf00-0a02-4f4a-8bc4-91b07bb4f546/picks/available",
  url = "https://api.splashsports.com/contests-service/api/contests/b0977276-c87b-4a05-9c20-6e62ae4336fd/picks/available",
  # url = "https://api.splashsports.com/contests-service/api/contests/89856186-3cbf-4862-b080-66923d2147b5/picks/available",
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
    # url = "https://api.splashsports.com/contests-service/api/contests/e2cd862c-6af7-479e-ab58-484521c0eaf4/picks/available",
    
    # url = "https://api.splashsports.com/contests-service/api/contests/89eb2193-b7be-4df5-9aa1-0404788ff032/picks/available",
    # url = "https://api.splashsports.com/contests-service/api/contests/f5efdf00-0a02-4f4a-8bc4-91b07bb4f546/picks/available",
    url = "https://api.splashsports.com/contests-service/api/contests/b0977276-c87b-4a05-9c20-6e62ae4336fd/picks/available",
    # url = "https://api.splashsports.com/contests-service/api/contests/89856186-3cbf-4862-b080-66923d2147b5/picks/available",
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

# 2025
sort_vec <- function(...) sort(c(...))

entries <- 
  tibble(resp = json_responses) |> 
  unnest(resp) |> 
  unnest_wider(resp) |> 
  unnest_wider(picks) |> 
  rename(r1 = `09160cd4-bfd7-44e9-ac24-48201aad6bf6`,
         r2 = `824bb33d-c242-4d65-8a74-6c769b710745`,
         r3 = `0aeb1bb0-40e9-4406-b7ac-c23ab4e40d98`) |> 
  # rename(r1 = `0873b740-f458-4f20-860f-f0c4a2bd73f3`,
  #        r2 = `13fa43b7-569e-4bb4-8a9b-5e46a18f47a8`,
  #        r3 = `448eb125-86f8-4750-af0b-e4aba62066eb`) |> 
  unnest_wider(r1, names_sep = "_") |> 
  unnest_wider(r1_teamAlias, names_sep = "_") |> 
  unnest_wider(r2, names_sep = "_") |> 
  unnest_wider(r2_teamAlias, names_sep = "_") |> 
  unnest_wider(r3, names_sep = "_") |> 
  rowwise() |> 
  mutate(r1_teams = list(sort_vec(r1_teamAlias_1,
                                  r1_teamAlias_2)),
         r2_teams = list(sort_vec(r2_teamAlias_1,
                                  r2_teamAlias_2))) |> 
  ungroup() |>
  unnest_wider(r1_teams, names_sep = "_") |> 
  unnest_wider(r2_teams, names_sep = "_") |> 
  filter(!is.na(r3_teamAlias))

# 2025
r2_matchups <- tibble(
  team = c("DEN","BUF","SF","SEA","LA","CHI","HOU","NE"),
  opponent = c("BUF","DEN","SEA","SF","CHI","LA","NE","HOU")
)


entries_flagged <-
  entries %>%
  left_join(r2_matchups, by = c("r2_teams_1" = "team")) %>%
  rename(r2_opp_1 = opponent) %>%
  left_join(r2_matchups, by = c("r2_teams_2" = "team")) %>%
  rename(r2_opp_2 = opponent)

entries_flagged <-
  entries_flagged %>%
  rowwise() %>%
  mutate(
    doa_flag = case_when("HOU" %in% c(r1_teams_1, r1_teams_2) &
                           "NE" %in% c(r1_teams_1, r1_teams_2) ~ TRUE,
                         "CHI" %in% c(r1_teams_1, r1_teams_2) &
                           "LA" %in% c(r1_teams_1, r1_teams_2) ~ TRUE,
                         TRUE ~ FALSE),
    num_pivots = case_when(!(r2_opp_1 %in% c(r1_teams_1, r1_teams_2)) &
                             !(r2_opp_2 %in% c(r1_teams_1, r1_teams_2)) ~ "double_pivot",
                               !(r2_opp_1 %in% c(r1_teams_1, r1_teams_2)) |
                                 !(r2_opp_2 %in% c(r1_teams_1, r1_teams_2)) ~ "single_pivot",
                                   TRUE ~ "no_pivot"),
    pivot_flag =
      !(r2_opp_1 %in% c(r1_teams_1, r1_teams_2)) |
      !(r2_opp_2 %in% c(r1_teams_1, r1_teams_2))
  ) %>%
  ungroup()


entries_flagged |> 
  group_by(doa_flag, pivot_flag, num_pivots) |> 
  tally()


entries <- 
  tibble(resp = json_responses) |> 
  unnest(resp) |> 
  unnest_wider(resp) |> 
  unnest_wider(picks) |> 
  unnest_wider(`09160cd4-bfd7-44e9-ac24-48201aad6bf6`)

entries_joins <- 
  tibble(all_rows = all_rows) |> 
  unnest(all_rows)

entries_clean <- 
  entries |> 
  # unnest_wider(teamAlias, names_sep = "_") |> 
  left_join(entries_joins |> select(entryId, entryName),
            by = c("entryId"))

entries_clean |> 
  unnest_wider(record) |>
  mutate(
    # team1 = pmin(teamAlias_1, teamAlias_2),
    # team2 = pmax(teamAlias_1, teamAlias_2),
    # team1 = if_else(team1 == "LA", "LAR", team1),
    # team2 = if_else(team2 == "LA", "LAR", team2),
    # team1 = if_else(team1 == "JAC", "JAX", team1),
    # team2 = if_else(team2 == "JAC", "JAX", team2),
    label = glue::glue("{r1_teams_1}_{r1_teams_2}_{r2_teams_1}_{r2_teams_2}_{r3_teamAlias}")
  ) |>
  summarise(
    entry_count = n(),
    alive = sum(is.na(eliminatedSlateId)),
    .by = c(entryName, label)
  ) |> 
  pivot_wider(id_cols = entryName,
              names_from = label,
              values_from = entry_count) |>
  mutate(total = rowSums(across(-entryName), na.rm = TRUE)) |> 
  clipr::write_clip()
  

pairings <-
  entries_clean |>
  summarise(
    entry_count = n(),
    our_entries = sum(entryName == "TinkyTyler"),
    .by = c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2, r3_teamAlias)
  ) |>
  arrange(desc(entry_count)) |>
  mutate(
    has_LA  = if_any(c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2), ~ .x == "LA"),
    has_SEA = if_any(c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2), ~ .x == "SEA"),
    has_DEN = if_any(c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2), ~ .x == "DEN"),
    has_NE  = if_any(c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2), ~ .x == "NE"),
    
    sb_implied = case_when(
      # AFC winner picked, both NFC teams already used
      r3_teamAlias %in% c("DEN", "NE") & has_LA & has_SEA ~ "dead",
      
      # AFC winner, no NFC finalist yet
      r3_teamAlias %in% c("DEN", "NE") & !has_LA & !has_SEA ~ "NFC Champ",
      
      # AFC winner, one NFC team forced
      r3_teamAlias %in% c("DEN", "NE") & has_LA  ~ "SEA",
      r3_teamAlias %in% c("DEN", "NE") & has_SEA ~ "LA",
      
      # NFC winner picked, both AFC teams already used
      r3_teamAlias %in% c("SEA", "LA") & has_DEN & has_NE ~ "dead",
      
      # NFC winner, no AFC finalist yet
      r3_teamAlias %in% c("SEA", "LA") & !has_DEN & !has_NE ~ "AFC Champ",
      
      # NFC winner, one AFC team forced
      r3_teamAlias %in% c("SEA", "LA") & has_NE  ~ "DEN",
      r3_teamAlias %in% c("SEA", "LA") & has_DEN ~ "NE",
      
      TRUE ~ "err"
    )
  ) |>
  clipr::write_clip()



  pairings <-
    entries_clean |>
    summarise(
      entry_count = n(),
      our_entries = sum(entryName == "TinkyTyler"),
      .by = c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2, r3_teamAlias)
    ) |>
    arrange(desc(entry_count)) |> 
    mutate(sb_implied = case_when(r3_teamAlias == "DEN" &
                                    "LA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) &
                                    "SEA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "dead",
                                  r3_teamAlias == "NE" &
                                    "LA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) &
                                    "SEA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "dead",
                                  r3_teamAlias == "DEN" &
                                    !("LA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2)) &
                                    !("SEA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2)) ~ "NFC Champ",
                                  r3_teamAlias == "DEN" &
                                    "LA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "SEA",
                                  r3_teamAlias == "DEN" &
                                    "SEA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "LA",
                                  r3_teamAlias == "NE" &
                                    !("LA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2)) &
                                    !("SEA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2)) ~ "NFC Champ",
                                  r3_teamAlias == "NE" &
                                    "LA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "SEA",
                                  r3_teamAlias == "NE" &
                                    "SEA" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "LA",
                                  
                                  r3_teamAlias == "SEA" &
                                    "NE" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) &
                                    "DEN" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "dead",
                                  r3_teamAlias == "LA" &
                                    "NE" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) &
                                    "DEN" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "dead",
                                  r3_teamAlias == "SEA" &
                                    !("NE" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2)) &
                                    !("DEN" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2)) ~ "AFC Champ",
                                  r3_teamAlias == "SEA" &
                                    "NE" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "DEN",
                                  r3_teamAlias == "SEA" &
                                    "DEN" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "NE",
                                  r3_teamAlias == "LA" &
                                    !("DEN" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2)) &
                                    !("NE" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2)) ~ "AFC Champ",
                                  r3_teamAlias == "LA" &
                                    "NE" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "DEN",
                                  r3_teamAlias == "LA" &
                                    "DEN" %in% c(r1_teams_1, r1_teams_2, r2_teams_1, r2_teams_2) ~ "NE",
                                  TRUE ~ "err")) |> 
    clipr::write_clip()

sheeet <- 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1l6s-B9WkCRBJJIggG_SM0TcZdeXvg8OJenXoqLHYAIQ/edit?gid=1702851649#gid=1702851649",
                            sheet = "Updated") |> 
  janitor::clean_names()


mesh <- 
  sheeet |> 
  full_join(pairings, by = c("column_1" = "team1",
                             "team2")) |> 
  mutate(pct = entry_count / 7408) |> 
  select(pct) |> 
  clipr::write_clip()
  


# CALCULATE EV ------------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)
library(combinat)

### ------------------------------------------------
### 1. Build per-simulation win matrix
### ------------------------------------------------

wins_by_sim <-
  simsq$games |> 
  filter(week == 19) |> 
  mutate(
    CAR = if_else(home_team == "CAR" & result > 0, 1, 0),
    LA = if_else(home_team == "CAR" & result < 0, 1, 0),
    CHI = if_else(home_team == "CHI" & result > 0, 1, 0),
    GB = if_else(home_team == "CHI" & result < 0, 1, 0),
    
    JAX = if_else(home_team == "JAX" & result < 0, 1, 0),
    BUF = if_else(home_team == "JAX" & result > 0, 1, 0),
    PHI = if_else(home_team == "PHI" & result < 0, 1, 0),
    SF = if_else(home_team == "PHI" & result > 0, 1, 0),
    NE = if_else(home_team == "NE" & result < 0, 1, 0),
    LAC = if_else(home_team == "NE" & result > 0, 1, 0),
    PIT = if_else(home_team == "PIT" & result < 0, 1, 0),
    HOU = if_else(home_team == "PIT" & result > 0, 1, 0)
  ) |> 
  summarise(across(CAR:HOU, sum), .by = sim)

n_sims <- nrow(wins_by_sim)


new_entries <-
  entries_clean |>
  mutate(
    team1 = pmin(teamAlias_1, teamAlias_2),
    team2 = pmax(teamAlias_1, teamAlias_2)
  ) |>
  mutate(team1 = if_else(team1 == "LA", "LA", team1),
         team2 = if_else(team2 == "LA", "LA", team2),
         team1 = if_else(team1 == "JAC", "JAX", team1),
         team2 = if_else(team2 == "JAC", "JAX", team2)) 

unique_pairings <- 
  new_entries |> 
  filter(!is.na(team1)) |> 
  distinct(team1, team2) |>
  mutate(pairing_id = row_number())
  

# For each unique pairing, compute which sims it wins
pairing_hits <- unique_pairings |>
  mutate(
    wins = pmap(
      list(team1, team2),
      function(t1, t2) {
        # Both teams must win in the simulation
        wins_by_sim[[t1]] == 1 & wins_by_sim[[t2]] == 1
      }
    )
  )

entries_with_wins <-
  new_entries |>
  inner_join(
    pairing_hits |> select(team1, team2, wins),
    by = c("team1", "team2")
  )


library(furrr)
library(future)

plan(multisession, workers = 3)
options(future.globals.maxSize = 8 * 1024^3)  # increase if needed
# Count how many entries win in each simulation
winners_by_sim <- future_map_dfc(
  1:n_sims,
  function(sim_idx) {
    tibble(
      !!paste0("sim_", sim_idx) := map_lgl(
        entries_with_wins$wins,
        ~ .x[sim_idx]
      )
    )
  }
)

total_winners_per_sim <- winners_by_sim |>
  summarise(across(everything(), sum)) |>
  unlist()


### ------------------------------------------------
### 6. Compute EV for each entry
### ------------------------------------------------

prize_pool <- 1000000  # adjust as needed
fallback_prize <- 1000000  # if no one wins

entries_with_ev <- entries_with_wins |>
  mutate(
    ev = map_dbl(
      wins,
      function(entry_wins) {
        payout_vec <- map2_dbl(
          entry_wins,
          total_winners_per_sim,
          ~ if (.x) prize_pool / .y else 0
        )
        mean(payout_vec)
      }
    )
  ) |>
  select(-wins) |>
  arrange(desc(ev))


entries_joins <- 
  tibble(all_rows = all_rows) |> 
  unnest(all_rows)

entries_with_ev |> 
  left_join(entries_joins |> select(entryId, entryName),
            by = c("entryId")) |> 
  summarise(ev = sum(ev),
            count = n(),
            .by = c(entryName)) |> 
  mutate(entry_fees = 150 * count,
         roi = ev / entry_fees) |>
  view()



# LEVERAGE ----------------------------------------------------------------

# Filter to only our entries
my_entries <- entries_with_wins |> 
  left_join(entries_joins |> select(entryId, entryName),
            by = c("entryId")) |>
  filter(entryName == "TinkyTyler")
games <- c("LA", "CAR", "CHI", "GB", "NE", "LAC", "BUF", "JAX", "HOU", "PIT", "SF", "PHI")


ev_by_game_outcome <- map_dfr(games, function(game_team) {
  
  # Sims where this team wins or loses
  wins_sims <- which(wins_by_sim[[game_team]] == 1)
  loses_sims <- which(wins_by_sim[[game_team]] == 0)
  
  # Conditional EV for just TinkyTyler
  entries_conditional <- my_entries |>
    mutate(
      ev_if_wins = map_dbl(
        wins,
        function(entry_wins) {
          if (length(wins_sims) == 0) return(NA_real_)
          payout_vec <- ifelse(
            entry_wins[wins_sims],
            ifelse(
              total_winners_per_sim[wins_sims] > 0,
              prize_pool / total_winners_per_sim[wins_sims],
              fallback_prize
            ),
            0
          )
          mean(payout_vec)
        }
      ),
      ev_if_loses = map_dbl(
        wins,
        function(entry_wins) {
          if (length(loses_sims) == 0) return(NA_real_)
          payout_vec <- ifelse(
            entry_wins[loses_sims],
            ifelse(
              total_winners_per_sim[loses_sims] > 0,
              prize_pool / total_winners_per_sim[loses_sims],
              fallback_prize
            ),
            0
          )
          mean(payout_vec)
        }
      )
    )
  
  tibble(
    game = game_team,
    mean_ev_if_wins = mean(entries_conditional$ev_if_wins, na.rm = TRUE),
    mean_ev_if_loses = mean(entries_conditional$ev_if_loses, na.rm = TRUE),
    entries_detail = list(entries_conditional |> 
                            select(entryId, team1, team2, ev_if_wins, ev_if_loses))
  )
})

# View summary
ev_by_game_outcome |>
  select(game, mean_ev_if_wins, mean_ev_if_loses)

# Detailed EVs per game for TinkyTyler
ev_by_game_outcome |>
  mutate(entries_detail = map(entries_detail, ~ .x |> filter(entryId == "TinkyTyler"))) |>
  select(game, entries_detail)





# Let's EV paths ----------------------------------------------------------


