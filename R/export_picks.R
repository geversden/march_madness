# export_picks.R — CSV export for Splash Sports survivor picks
#
# Matches CSV entries to optimizer allocation groups by used_teams fingerprint.
# Does NOT rely on state alive status — uses CSV day columns as source of truth.

parse_csv_line <- function(line) {
  con <- textConnection(line)
  result <- tryCatch(
    read.csv(con, header = FALSE, stringsAsFactors = FALSE,
             check.names = FALSE, na.strings = ""),
    error = function(e) NULL
  )
  close(con)
  if (is.null(result)) return(NULL)
  as.character(result[1, ])
}

rebuild_csv_line <- function(fields) {
  out <- sapply(fields, function(f) {
    if (is.na(f) || f == "") return("")
    if (grepl(",", f)) return(paste0('"', f, '"'))
    return(f)
  }, USE.NAMES = FALSE)
  paste(out, collapse = ",")
}

export_picks <- function(result, csv_dir = "splash_entry_csvs", locked_teams = NULL) {

  allocation  <- result$allocation
  state       <- result$state
  teams_dt    <- result$teams_dt
  name_map    <- result$scrape_inputs$name_map
  sim_to_splash <- setNames(name_map$splash_name, name_map$sim_name)

  resolve_uuid <- function(team_name, uuid_lookup) {
    splash <- if (team_name %in% names(sim_to_splash)) sim_to_splash[[team_name]] else team_name
    uuid <- uuid_lookup[splash]
    if (is.null(uuid) || is.na(uuid)) uuid <- uuid_lookup[team_name]
    if (is.null(uuid) || is.na(uuid)) {
      lookup_names <- names(uuid_lookup)
      matches <- lookup_names[startsWith(lookup_names, team_name)]
      if (length(matches) >= 1) uuid <- uuid_lookup[matches[1]]
    }
    if (is.null(uuid)) return(NA_character_)
    unname(uuid)
  }

  # --- Step 1: Build fingerprint -> assignment queue from optimizer allocation ---
  # Each group has a used_teams fingerprint. The allocation says how many entries
  # in that group get each team. We build a queue of team assignments per fingerprint.
  groups <- group_entries(state)
  fp_queues <- list()  # "fingerprint" -> list of list(team, extra) assignments

  if (nrow(allocation) > 0 && nrow(groups) > 0) {
    for (gid in unique(allocation$group_id)) {
      g <- groups[group_id == gid]
      if (nrow(g) == 0) next
      ut <- sort(g$used_teams[[1]])
      fp <- paste(ut, collapse = ",")

      allocs <- allocation[group_id == gid]
      # Build assignment queue: repeat each team n_assigned times
      queue <- list()
      for (ai in seq_len(nrow(allocs))) {
        a <- allocs[ai]
        extra <- if (!is.na(a$slot1_extra_name)) a$slot1_extra_name else NULL
        for (k in seq_len(a$n_assigned)) {
          queue[[length(queue) + 1]] <- list(team = a$team_name, extra = extra)
        }
      }
      # Merge with existing queue for this fingerprint (across contest_ids)
      if (fp %in% names(fp_queues)) {
        fp_queues[[fp]] <- c(fp_queues[[fp]], queue)
      } else {
        fp_queues[[fp]] <- queue
      }
    }
  }

  # Track position in each queue
  fp_pos <- setNames(rep(1L, length(fp_queues)), names(fp_queues))

  cat(sprintf("Fingerprint groups: %d, total assignments: %d\n",
              length(fp_queues), sum(sapply(fp_queues, length))))

  # --- Also build entry_id -> assignment for direct matching ---
  direct_assignments <- list()
  if (nrow(allocation) > 0 && nrow(groups) > 0) {
    for (i in seq_len(nrow(allocation))) {
      row <- allocation[i]
      g <- groups[group_id == row$group_id & contest_id == row$contest_id]
      if (nrow(g) == 0) next
      eids <- g$entry_ids[[1]]
      available <- setdiff(eids, names(direct_assignments))
      n <- min(row$n_assigned, length(available))
      if (n == 0) next
      extra <- if (!is.na(row$slot1_extra_name)) row$slot1_extra_name else NULL
      for (j in seq_len(n)) {
        direct_assignments[[available[j]]] <- list(team = row$team_name, extra = extra)
      }
    }
  }

  # --- Step 2: Process each CSV file ---
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  updated_files <- character(0)

  for (csv_file in csv_files) {
    lines <- readLines(csv_file, warn = FALSE)
    if (length(lines) < 2) next

    header_fields <- parse_csv_line(lines[1])
    if (is.null(header_fields)) next

    entry_id_col <- which(header_fields == "Entry ID")
    pick1_col    <- which(header_fields == "Pick 1")
    pick2_col    <- which(header_fields == "Pick 2")

    if (length(entry_id_col) == 0 || length(pick1_col) == 0) next
    is_format_c <- length(pick2_col) > 0

    # Find Day columns (NOT "Previous Picks")
    day_cols <- which(grepl("^Day ", header_fields))

    # Build UUID lookup from Team Name / Team ID reference columns
    team_name_col <- which(header_fields == "Team Name")
    team_id_col   <- which(header_fields == "Team ID")
    uuid_lookup <- character(0)

    if (length(team_name_col) > 0 && length(team_id_col) > 0) {
      for (li in 2:length(lines)) {
        fields <- parse_csv_line(lines[li])
        if (is.null(fields)) next
        if (team_name_col > length(fields) || team_id_col > length(fields)) next
        tn <- fields[team_name_col]
        tid <- fields[team_id_col]
        if (!is.na(tn) && nchar(tn) > 0 && !is.na(tid) && nchar(tid) > 0) {
          uuid_lookup[tn] <- tid
        }
      }
    }

    uuid_to_name <- setNames(names(uuid_lookup), uuid_lookup)
    locked_splash <- character(0)
    if (!is.null(locked_teams)) {
      locked_splash <- unique(c(
        locked_teams,
        sapply(locked_teams, function(lt) {
          if (lt %in% names(sim_to_splash)) sim_to_splash[[lt]] else lt
        }, USE.NAMES = FALSE)
      ))
    }

    matched <- 0
    unmatched <- 0

    for (li in 2:length(lines)) {
      fields <- parse_csv_line(lines[li])
      if (is.null(fields) || entry_id_col > length(fields)) next

      eid <- fields[entry_id_col]
      assign <- NULL

      # Method 1: Direct entry_id match
      if (eid %in% names(direct_assignments)) {
        assign <- direct_assignments[[eid]]
      }

      # Method 2: Parse Day columns -> fingerprint -> match to group allocation
      if (is.null(assign) && length(day_cols) > 0) {
        prior_names <- character(0)
        for (dc in day_cols) {
          if (dc <= length(fields) && !is.na(fields[dc]) && nchar(fields[dc]) > 0) {
            parts <- trimws(unlist(strsplit(fields[dc], "\\+")))
            prior_names <- c(prior_names, parts[nchar(parts) > 0])
          }
        }
        if (length(prior_names) > 0) {
          prior_ids <- map_team_names(prior_names, name_map)
          prior_ids <- sort(unique(na.omit(as.integer(prior_ids))))
          fp <- paste(prior_ids, collapse = ",")

          if (fp %in% names(fp_queues)) {
            pos <- fp_pos[[fp]]
            queue <- fp_queues[[fp]]
            if (pos <= length(queue)) {
              assign <- queue[[pos]]
              fp_pos[[fp]] <- pos + 1L
            } else {
              # Queue exhausted — wrap around (more CSV entries than optimizer expected)
              assign <- queue[[(pos - 1L) %% length(queue) + 1L]]
              fp_pos[[fp]] <- pos + 1L
            }
          }
        }
      }

      if (is.null(assign)) {
        unmatched <- unmatched + 1
        next
      }

      # Handle locked picks
      existing_pick1 <- if (pick1_col <= length(fields)) fields[pick1_col] else ""
      pick1_locked <- FALSE
      if (!is.na(existing_pick1) && nchar(existing_pick1) > 0) {
        existing_name1 <- uuid_to_name[existing_pick1]
        if (!is.na(existing_name1) && existing_name1 %in% locked_splash) {
          pick1_locked <- TRUE
        }
      }

      # For pair-based scoring, team_name is "TeamA + TeamB" — split into two picks
      team1_name <- assign$team
      team2_name <- assign$extra
      if (grepl(" \\+ ", team1_name)) {
        pair_parts <- trimws(strsplit(team1_name, " \\+ ")[[1]])
        team1_name <- pair_parts[1]
        team2_name <- pair_parts[2]
      }

      new_uuid1 <- resolve_uuid(team1_name, uuid_lookup)

      if (is_format_c) {
        existing_pick2 <- if (pick2_col <= length(fields)) fields[pick2_col] else ""
        pick2_locked <- FALSE
        if (!is.na(existing_pick2) && nchar(existing_pick2) > 0) {
          existing_name2 <- uuid_to_name[existing_pick2]
          if (!is.na(existing_name2) && existing_name2 %in% locked_splash) {
            pick2_locked <- TRUE
          }
        }
        new_uuid2 <- if (!is.null(team2_name)) resolve_uuid(team2_name, uuid_lookup) else NA
        if (pick1_locked && pick2_locked) next
        else if (pick1_locked) {
          if (!is.na(new_uuid1)) fields[pick2_col] <- new_uuid1
        } else if (pick2_locked) {
          if (!is.na(new_uuid1)) fields[pick1_col] <- new_uuid1
        } else {
          if (!is.na(new_uuid1)) fields[pick1_col] <- new_uuid1
          if (!is.na(new_uuid2)) fields[pick2_col] <- new_uuid2
        }
      } else {
        if (!pick1_locked && !is.na(new_uuid1)) {
          fields[pick1_col] <- new_uuid1
        }
      }

      lines[li] <- rebuild_csv_line(fields)
      matched <- matched + 1
    }

    if (matched > 0 || unmatched > 0) {
      writeLines(lines, csv_file)
      total <- matched + unmatched
      cat(sprintf("  %s: %d/%d entries filled", basename(csv_file), matched, total))
      if (unmatched > 0) cat(sprintf("  (%d unmatched)", unmatched))
      cat("\n")
      updated_files <- c(updated_files, csv_file)
    }
  }

  cat(sprintf("\nDone. Updated %d CSV files.\n", length(updated_files)))
  invisible(updated_files)
}
