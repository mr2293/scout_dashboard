# ============================================================
# sc_physical_acumulado_fallback.R
#
# Builds the Acumulado-only SkillCorner physical row per player,
# following the scouting org's sample-size guidelines: default to the
# player's last 2 seasons; if that doesn't reach 5 matches, fall back
# to 2 older seasons (~3-4 years back), then to lower playing-time
# thresholds (20, then 10 minutes/match), stopping as soon as 5
# matches is reached. Every step accumulates ON TOP of the previous
# one (never replaces it) -- "extended_seasons" means default+older
# seasons blended together, not older seasons alone.
#
# Reads physical.ipynb's RAW per-league CSVs directly (the same files
# sc_data.R already reads for {league}_fisico) -- not {league}_fisico/
# liga_mx_full, which for Liga MX also carries Game Intelligence
# columns this file has no business touching. The season-selector
# feature (SbScMetricBySeason, sync_season_rows_to_mongo.R) reads a
# completely separate path and is untouched by this file existing.
#
# dedup_transfers() (app.R) is NOT used or modified here -- this file
# implements its own minutes-weighted blend, deliberately scoped to
# just the candidate sets a given player's fallback ladder actually
# needs, which dedup_transfers()'s "blend everything the player ever
# has" semantics can't express.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(tibble)
}))

# Every league this notebook pulls, with its file-name stem (matches
# Task 1's pull_and_save_physical() filenames exactly).
SC_PHYSICAL_LEAGUE_STEMS <- c(
  "ligamx", "arg", "brasil", "col", "chile", "mls", "premier", "laliga",
  "laliga2", "seriea", "tur", "ucl", "uel", "paraguay", "uruguay", "peru",
  "ecuador", "championship", "bundesliga", "bundesliga2", "ligue1",
  "eredivisie", "belgica", "portugal", "escocia", "rusia"
)

# Same defensive empty-frame fallback read_physical_csv_safe() (sc_data.R)
# already uses for Ecuador/Championship/Rusia -- here every candidate
# file can legitimately be missing (a league with no low-sample players
# never triggers its own fallback tiers to be USED, but the files still
# get written by Task 1 unconditionally; a league/season with 0 rows
# returned skips the file write entirely, same as today).
.read_physical_candidate <- function(path) {
  if (!file.exists(path)) return(NULL)
  suppressMessages(read_csv(path, show_col_types = FALSE))
}

# Columns that are genuine cumulative totals (raw distance in meters,
# raw counts, raw minutes, match counts) over whatever window of
# matches contributed to a row -- these SUM when blending more matches
# in, unlike rate/per-X columns where summing would be meaningless.
# Peak-speed columns (psv99 and friends) are a third case: the TRUE
# value across a wider sample is the max, not a sum or an average (an
# average would understate a player's real top speed).
.sc_physical_sum_pattern <- "_full_(all|tip|otip|bip)$|^count_match(_failed)?$"
# "^peak" catches peak_velocity/peak_velocity_top3 (confirmed present in
# a real downloaded CSV during planning) -- "^psv99" alone would have
# missed the bare peak_velocity column (no _top3/_top5/_match_peak
# suffix), silently weighted-averaging a peak-speed column instead of
# maxing it. Caught during this plan's own self-review, not live.
.sc_physical_peak_pattern <- "^psv99|^peak|_top3$|_top5$|_match_peak$"

# Blends a list of candidate data.frames (already read, some possibly
# empty/NULL) into one row per player_id. Weight = minutes_full_all
# (falls back to equal weight if a player has no minutes on any
# candidate row, which shouldn't happen in practice but mirrors
# dedup_transfers()'s own defensive fallback).
.blend_physical_candidates <- function(dfs) {
  dfs <- purrr::compact(dfs)
  if (!length(dfs)) return(tibble())
  combined <- dplyr::bind_rows(dfs)
  if (!nrow(combined)) return(tibble())

  numeric_cols <- names(combined)[sapply(combined, is.numeric)]
  id_like <- c("player_id", "team_id", "competition_id", "season_id")
  numeric_cols <- setdiff(numeric_cols, id_like)

  sum_cols  <- numeric_cols[grepl(.sc_physical_sum_pattern, numeric_cols)]
  peak_cols <- numeric_cols[grepl(.sc_physical_peak_pattern, numeric_cols)]
  avg_cols  <- setdiff(numeric_cols, c(sum_cols, peak_cols))

  combined |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(
      player_name = dplyr::first(player_name),
      # avg_cols MUST be computed before sum_cols/peak_cols below:
      # minutes_full_all is itself a sum_cols member (matches
      # .sc_physical_sum_pattern), and dplyr::summarise() builds its
      # data mask progressively across arguments in this same call --
      # once the sum_cols across() below runs, the bare name
      # `minutes_full_all` referenced here would resolve to the
      # already-summed (length-1) scalar instead of the raw per-row
      # column, silently breaking weighted.mean()'s length check.
      # Computing avg_cols first means `minutes_full_all` still refers
      # to the untouched per-row column from `combined`.
      dplyr::across(dplyr::all_of(avg_cols), function(x) {
        w <- minutes_full_all
        w[is.na(w)] <- 0
        if (sum(w) == 0 || all(is.na(x))) return(if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
        stats::weighted.mean(x, w, na.rm = TRUE)
      }),
      dplyr::across(dplyr::all_of(sum_cols), ~ sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(peak_cols), ~ if (all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#' Reads one candidate suffix (e.g. "_physical_standardized.csv") across
#' every league stem and stacks the results into a single data.frame.
#' Candidate pools are built GLOBALLY (across leagues), not per-league --
#' a player transferred between two of this notebook's leagues within
#' the lookback window (confirmed: 363 players appear in more than one
#' league's default CSV, e.g. a LaLiga<->Portugal move) must have both
#' leagues' matches blended into one row, exactly like the fallback
#' ladder already blends across seasons/tiers. Processing league-by-
#' league would give that player two separate output rows, breaking the
#' "one row per player_id" contract.
.read_physical_candidate_all_leagues <- function(suffix) {
  paths <- file.path("data", "physical", paste0(SC_PHYSICAL_LEAGUE_STEMS, suffix))
  dplyr::bind_rows(purrr::compact(purrr::map(paths, .read_physical_candidate)))
}

#' Builds the Acumulado-only SkillCorner physical row per player, with
#' the sample-size fallback ladder applied. One row per player_id.
get_sc_physical_acumulado_fallback <- function() {
  df_default  <- .read_physical_candidate_all_leagues("_physical_standardized.csv")
  df_extended <- .read_physical_candidate_all_leagues("_physical_extended_seasons.csv")
  df_tier20   <- .read_physical_candidate_all_leagues("_physical_tier20.csv")
  df_tier10   <- .read_physical_candidate_all_leagues("_physical_tier10.csv")

  if (!nrow(df_default)) return(tibble())

  player_ids <- unique(df_default$player_id)

  purrr::map_dfr(player_ids, function(pid) {
    one <- function(df) {
      if (!nrow(df)) return(NULL)
      sub <- df[df$player_id == pid, , drop = FALSE]
      if (!nrow(sub)) NULL else sub
    }

    # Step 1: default only.
    cand <- list(one(df_default))
    blend <- .blend_physical_candidates(cand)
    if (nrow(blend) && isTRUE(blend$count_match[1] >= 5)) {
      return(dplyr::mutate(blend, sc_physical_quality_warning = FALSE, sc_physical_quality_reason = "ok"))
    }

    # Step 2: + extended seasons.
    cand <- list(one(df_default), one(df_extended))
    blend <- .blend_physical_candidates(cand)
    if (nrow(blend) && isTRUE(blend$count_match[1] >= 5)) {
      return(dplyr::mutate(blend, sc_physical_quality_warning = TRUE, sc_physical_quality_reason = "extended_seasons"))
    }

    # Step 3: + tier20.
    cand <- list(one(df_default), one(df_extended), one(df_tier20))
    blend <- .blend_physical_candidates(cand)
    if (nrow(blend) && isTRUE(blend$count_match[1] >= 5)) {
      return(dplyr::mutate(blend, sc_physical_quality_warning = TRUE, sc_physical_quality_reason = "lower_threshold"))
    }

    # Step 4: + tier10. Use whatever this produces, even if still < 5.
    cand <- list(one(df_default), one(df_extended), one(df_tier20), one(df_tier10))
    blend <- .blend_physical_candidates(cand)
    if (!nrow(blend)) return(NULL)
    reason <- if (isTRUE(blend$count_match[1] >= 5)) "lower_threshold" else "insufficient_sample"
    dplyr::mutate(blend, sc_physical_quality_warning = TRUE, sc_physical_quality_reason = reason)
  })
}

# ============================================================
# Validation harness -- only runs when executed directly, matching
# this repo's convention for pipeline-script sanity checks (see
# confidence_shrinkage.R, ame_score_gate.R, datascore_v2.R).
# ============================================================
if (sys.nframe() == 0) {
  result <- get_sc_physical_acumulado_fallback()
  cat(sprintf("rows: %d\n", nrow(result)))
  cat(sprintf("distinct player_id: %d\n", dplyr::n_distinct(result$player_id)))
  cat("reason breakdown:\n")
  print(table(result$sc_physical_quality_reason, useNA = "ifany"))
  stopifnot("duplicate player_id in output" = !any(duplicated(result$player_id)))
  cat("PASS: no duplicate player_id\n")
}
