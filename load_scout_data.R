# ============================================================
# load_scout_data.R
#
# Shared loader for the DataScore v2 / AmeScore pipeline scripts
# (normalization.R, capacity_subscores.R, base_scores.R, ...). Joins the
# raw per-season metrics (data/scout_joined.rds, keyed by human league
# name + season_id/competition_id) with the match-based role
# classification (data/role_eligibility_matchbased.rds, keyed by
# .var_name) through dashboard_scout.R's league/season pairs -- see
# role_eligibility_match_based.R for why that regex-parse exists instead
# of sourcing dashboard_scout.R directly (never executes its live pull,
# just reads it as text).
#
# Factored out 2026-10-07 after this exact ~40-line block got duplicated
# a third time (normalization.R, capacity_subscores.R) -- extracted
# before a base_scores.R copy could drift out of sync with the other two.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
}))

load_scout_data <- function() {
  src <- readLines("dashboard_scout.R", warn = FALSE)
  src_text <- paste(src, collapse = "\n")
  matches <- gregexpr(
    "(\\w+)\\s*<-\\s*safe_matchesvector\\(username,\\s*password,\\s*season_id\\s*=\\s*(\\d+),\\s*competition_id\\s*=\\s*(\\d+)\\)",
    src_text, perl = TRUE
  )
  raw_matches <- regmatches(src_text, matches)[[1]]
  league_pairs <- purrr::map_dfr(raw_matches, function(m) {
    var <- sub("\\s*<-.*", "", m)
    sid <- as.integer(sub(".*season_id\\s*=\\s*(\\d+).*", "\\1", m))
    cid <- as.integer(sub(".*competition_id\\s*=\\s*(\\d+)\\).*", "\\1", m))
    tibble(var_name = trimws(var), season_id = sid, competition_id = cid)
  })

  message("Loading data/scout_joined.rds ...")
  joined <- readRDS("data/scout_joined.rds")
  all_leagues <- c(joined$joined_leagues, joined$sb_only_leagues)
  # player_id viene como integer en algunas ligas y character en otras --
  # normalizado ANTES de bind_rows().
  all_leagues <- purrr::map(all_leagues, function(df) {
    if ("player_id" %in% names(df)) df$player_id <- as.character(df$player_id)
    df
  })
  raw <- dplyr::bind_rows(all_leagues) |>
    dplyr::inner_join(league_pairs, by = c("competition_id", "season_id"))

  message("Loading data/role_eligibility_matchbased.rds ...")
  roles <- readRDS("data/role_eligibility_matchbased.rds") |>
    dplyr::mutate(player_id = as.character(player_id)) |>
    dplyr::rename(var_name = .var_name) |>
    dplyr::select(var_name, player_id, role_group_matchbased)

  dat <- raw |>
    dplyr::inner_join(roles, by = c("var_name", "player_id")) |>
    dplyr::mutate(exposure_90s = suppressWarnings(as.numeric(player_season_minutes)) / 90)

  message(sprintf("Joined: %d rows, %d role_group_matchbased groups", nrow(dat), dplyr::n_distinct(dat$role_group_matchbased)))
  dat
}
