#!/usr/bin/env Rscript
# ==============================================================================
# KenPom Daily Scraper
# Pulls Adjusted Efficiency Margin (AdjEM), Adjusted Offense (AdjO),
# and Adjusted Defense (AdjD) for all D1 teams.
#
# Requires a KenPom subscription.
# Uses chromote (headless Chrome) to bypass Cloudflare bot protection.
#
# Dependencies:  install.packages(c("chromote", "rvest"))
#
# Credentials (in priority order):
#   1. Environment variables  KENPOM_EMAIL / KENPOM_PASSWORD
#      (set these as GitHub Secrets for the Actions workflow)
#   2. Local file  .kenpom_creds  with two lines:
#        email=you@example.com
#        password=yourpassword
#
# Usage:  Rscript scrape_kenpom.R
# Output: kenpom_ratings_YYYY-MM-DD.csv in the kenpom_data/ subfolder
# ==============================================================================

library(chromote)
library(rvest)

# --- Load credentials --------------------------------------------------------
# Prefer env vars (GitHub Actions), fall back to local creds file

email    <- Sys.getenv("KENPOM_EMAIL",    unset = "")
password <- Sys.getenv("KENPOM_PASSWORD", unset = "")

if (email == "" || password == "") {
  # Find script directory: works via Rscript, source(), or interactive RStudio
  script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
  creds_file <- file.path(script_dir, ".kenpom_creds")
  if (file.exists(creds_file)) {
    creds_raw <- readLines(creds_file, warn = FALSE)
    for (line in creds_raw) {
      parts <- strsplit(trimws(line), "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        key <- parts[1]
        val <- paste(parts[-1], collapse = "=")  # handle = in password
        if (key == "email")    email    <- val
        if (key == "password") password <- val
      }
    }
  }
}

if (email == "" || password == "") {
  stop("KenPom credentials not found.\n",
       "Set KENPOM_EMAIL and KENPOM_PASSWORD env vars (or GitHub Secrets),\n",
       "or create a .kenpom_creds file with email=... and password=... lines.")
}

# --- script_dir fallback (needed for output later) --------------------------
if (!exists("script_dir")) {
  script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
}

# --- Login to KenPom via headless Chrome ------------------------------------

cat("Launching headless Chrome ...\n")

b <- ChromoteSession$new()

# Navigate to KenPom homepage
cat("  Navigating to kenpom.com ...\n")
b$Page$navigate("https://kenpom.com/")
b$Page$loadEventFired(timeout = 30)
Sys.sleep(2)  # let Cloudflare challenge resolve

# Fill in login form and submit
cat("  Logging in ...\n")
b$Runtime$evaluate(sprintf(
  'document.querySelector("#email").value = %s;
   document.querySelector("#password").value = %s;',
  jsonlite::toJSON(email, auto_unbox = TRUE),
  jsonlite::toJSON(password, auto_unbox = TRUE)
))

# Click the submit button
b$Runtime$evaluate('document.querySelector("input[type=submit], button[type=submit]").click();')

# Wait for navigation after login
Sys.sleep(3)
b$Page$loadEventFired(timeout = 30)
Sys.sleep(1)

# --- Fetch the ratings page --------------------------------------------------

cat("Fetching ratings table ...\n")

# Navigate to the main ratings page (may already be there after login redirect)
b$Page$navigate("https://kenpom.com/")
b$Page$loadEventFired(timeout = 30)
Sys.sleep(2)

# Get the full page HTML
result <- b$Runtime$evaluate("document.documentElement.outerHTML")
page_source <- result$result$value

# Close the browser session
b$close()

page_html <- read_html(page_source)

# --- Parse the ratings table --------------------------------------------------

# The main table has id "ratings-table"
ratings_node <- html_element(page_html, "#ratings-table")

if (is.na(ratings_node)) {
  # Fallback: try first big table on the page
  ratings_node <- html_element(page_html, "table")
}

if (is.na(ratings_node)) {
  stop("Could not find ratings table. Login may have failed or page structure changed.\n",
       "Check that your credentials are correct.")
}

raw_table <- html_table(ratings_node, header = TRUE)

# --- Clean up columns ---------------------------------------------------------

# KenPom table columns vary slightly but the core layout is:
# Rk | Team | Conf | W-L | AdjEM | AdjO | AdjO Rk | AdjD | AdjD Rk | ...
# Some columns share header names; we rename by position

# Standardize column names
n_cols <- ncol(raw_table)
cat(sprintf("  Parsed table with %d rows, %d columns\n", nrow(raw_table), n_cols))

# Find the columns we care about by looking at headers
headers <- names(raw_table)

# Clean the table: remove separator rows (KenPom inserts blank rows between ranks)
raw_table <- raw_table[!is.na(raw_table[[1]]) & raw_table[[1]] != "", ]
raw_table <- raw_table[!grepl("^\\s*$", raw_table[[1]]), ]

# Build clean output
# Column positions (typical kenpom layout):
#  1=Rk, 2=Team, 3=Conf, 4=W-L, 5=AdjEM, 6=AdjO, 7=AdjO_Rk, 8=AdjD, 9=AdjD_Rk, ...
ratings <- data.frame(
  rank   = as.integer(raw_table[[1]]),
  team   = trimws(raw_table[[2]]),
  conf   = trimws(raw_table[[3]]),
  record = trimws(raw_table[[4]]),
  adj_em = as.numeric(raw_table[[5]]),
  adj_o  = as.numeric(raw_table[[6]]),
  adj_d  = as.numeric(raw_table[[8]]),
  stringsAsFactors = FALSE
)

# Drop any rows that didn't parse (header repeats, separators, etc.)
ratings <- ratings[!is.na(ratings$rank), ]

# Remove seed numbers that KenPom appends to team names during tournament
ratings$team <- gsub("\\s*\\d+$", "", ratings$team)

cat(sprintf("  Cleaned to %d teams\n", nrow(ratings)))

# --- Save to CSV --------------------------------------------------------------

out_dir <- file.path(script_dir, "kenpom_data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

today    <- Sys.Date()
out_file <- file.path(out_dir, sprintf("kenpom_ratings_%s.csv", today))

write.csv(ratings, out_file, row.names = FALSE)
cat(sprintf("\nSaved %d teams to %s\n", nrow(ratings), out_file))

# --- Quick preview ------------------------------------------------------------

cat("\nTop 20 teams:\n")
cat(sprintf("  %3s  %-25s %-6s %6s %6s %6s\n",
            "Rk", "Team", "Conf", "AdjEM", "AdjO", "AdjD"))
cat(paste(rep("-", 62), collapse = ""), "\n")
for (i in 1:min(20, nrow(ratings))) {
  r <- ratings[i, ]
  cat(sprintf("  %3d  %-25s %-6s %6.1f %6.1f %6.1f\n",
              r$rank, r$team, r$conf, r$adj_em, r$adj_o, r$adj_d))
}

cat("\nDone.\n")
