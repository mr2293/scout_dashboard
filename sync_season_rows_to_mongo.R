# ============================================================
# sync_season_rows_to_mongo.R
#
# Syncs ONE DOCUMENT PER (transfermarkt_id, season_name) -- unlike
# sync_to_mongo.R's `sb_sc_metrics`, which is already blended across
# seasons via dedup_transfers() (the "Acumulado" view). Feeds the
# ca_scouting_dashboard player-profile season filter (25/26 vs. 26/27 vs.
# Acumulado for the radar + stats table + SkillCorner physical/GI
# tables) -- that feature needs to see a SPECIFIC season's row, which
# `sb_sc_metrics` structurally cannot provide. "Acumulado" itself still
# reads from the existing `sb_sc_metrics` collection on the Vercel side,
# unchanged -- this collection only needs to cover the season-SPECIFIC
# case.
#
# get_all_players_raw_df() (app.R) already carries StatsBomb AND
# SkillCorner columns merged into the same row (same join as
# scout_joined.rds) -- confirmed 2026-10-14 (psv99 etc. present, just
# NA for players/leagues without SkillCorner coverage), so this is ONE
# sync, not a separate StatsBomb/SkillCorner pair like the SC-only pools
# (get_all_sc_df()/get_liga_mx_sc_df()) a58db68 uses in the Shiny app
# for a WIDER percentile reference pool. The Vercel side already
# restricts its percentile pools to Transfermarkt-matched players only
# (see page.jsx's existing SbScMetric.find({position_group: pg}, ...)
# for "Acumulado") -- using this same collection as both the player's
# row AND the reference pool for a specific season keeps that same
# restriction, just season-scoped instead of blended.
#
# Within a season, a player can still have >1 raw row (a genuine
# same-season team transfer, not just the join-artifact duplicates
# get_all_players_raw_df() already collapses via dedup_same_team()) --
# each season is re-run through dedup_transfers() independently before
# syncing, so "one row per player per season" holds the same way
# dedup_transfers() already makes "Acumulado" one row per player across
# ALL seasons.
#
# Standalone script, run manually alongside sync_to_mongo.R (same
# MONGO_URI/MONGO_DB env vars) -- not yet wired into the GitHub Actions
# weekly cadence.
#
# UNLIKE sync_to_mongo.R (which mirrors the full ~539-column row), this
# syncs ONLY the columns ca_scouting_dashboard's season-filter feature
# actually reads -- lib/positionMetrics.js's POSITION_METRIC_MAP (every
# position's radar/stats metrics)/PHYSICAL_VARS/GI_VARS, plus the key
# columns the API route filters/labels by. Multiplying the full row by
# ~4 seasons instead of 1 would have been ~4x sync_to_mongo.R's own
# payload; this trims it to roughly 1/10th the column count instead.
# Found the hard way 2026-10-14: a first attempt syncing full rows blew
# through the Atlas free tier's 512MB storage quota mid-insert (the
# cluster also hosts sb_sc_metrics/Player/etc., not just this
# collection) and had to be dropped and rebuilt with this trimmed shape.
# If lib/positionMetrics.js's metric lists ever change, SEASON_SYNC_COLS
# below must be updated to match -- same "duplicated constant, flagged
# here" precedent as SEASON_CHRONO_ORDER (league_transition_gate.R).
# ============================================================

# Union of every value in POSITION_METRIC_MAP across all 7 position
# groups (lib/positionMetrics.js) -- the StatsBomb metrics any position's
# radar/stats table can ask for.
SEASON_SYNC_SB_COLS <- c(
  "player_season_obv_gk_90", "player_season_long_ball_ratio", "player_season_pass_into_danger_ratio",
  "player_season_clcaa", "player_season_da_aggressive_distance", "player_season_np_optimal_gk_dlength",
  "player_season_save_ratio", "player_season_np_xg_90", "player_season_np_shots_90",
  "player_season_conversion_ratio", "player_season_npxgxa_90", "player_season_xa_90",
  "player_season_key_passes_90", "player_season_dribbles_90", "player_season_dribble_ratio",
  "player_season_deep_progressions_90", "player_season_aerial_wins_90", "player_season_aerial_ratio",
  "player_season_padj_pressures_90", "player_season_pressure_regains_90", "player_season_passing_ratio",
  "player_season_forward_pass_proportion", "player_season_long_balls_90", "player_season_padj_interceptions_90",
  "player_season_padj_tackles_90", "player_season_padj_clearances_90", "player_season_ball_recoveries_90",
  "player_season_fouls_90", "player_season_total_dribbles_90", "player_season_carries_90",
  "player_season_xgbuildup", "player_season_dribbled_past_90", "player_season_shots_key_passes_90",
  "player_season_carry_ratio", "player_season_aggressive_actions_90", "player_season_np_xg_per_shot",
  "player_season_op_xa_90", "player_season_defensive_actions_90", "player_season_crossing_ratio"
)

# PHYSICAL_VARS + GI_VARS_PASES + GI_VARS_DESMARQUES (lib/positionMetrics.js)
# -- a column can be saved with or without a "sc_" prefix (resolveScCol()'s
# fallback on the JS side), so both forms are selected where present.
SEASON_SYNC_SC_COLS <- c(
  "psv99", "psv99_match_peak", "total_distance_per_60_bip", "running_distance_per_60_bip",
  "hsr_distance_per_60_bip", "sprint_distance_per_60_bip", "sprint_count_per_60_bip",
  "hsr_count_per_60_bip", "highaccel_count_per_60_bip", "highdecel_count_per_60_bip",
  "meters_per_minute_tip", "meters_per_minute_otip",
  "count_pressures_received_per_30_min_tip_p30tip", "count_forced_losses_under_pressure_per_30_min_tip_p30tip",
  "ball_retention_ratio_under_pressure_p30tip", "pass_completion_ratio_under_pressure_p30tip",
  "dangerous_pass_completion_ratio_under_pressure_p30tip", "count_completed_passes_under_pressure_per_30_min_tip_p30tip",
  "count_completed_dangerous_passes_under_pressure_per_30_min_tip_p30tip",
  "count_dangerous_pass_attempts_under_pressure_per_30_min_tip_p30tip",
  "count_pass_attempts_under_pressure_per_30_min_tip_p30tip",
  "offballrun_count_dangerous_per_90", "offballrun_count_goalwithin10s_per_90",
  "offballrun_count_shotwithin10s_per_90", "offballrun_count_dangerous_targeted_per_90",
  "offballrun_count_dangerous_received_per_90", "offballrun_avgdistance", "offballrun_target_percentage",
  "offballrun_dangerous_target_percentage", "offballrun_receive_percentage",
  "offballrun_dangerous_receive_percentage", "offballrun_shotwithin10s_percentage",
  "offballrun_goalwithin10s_percentage", "offballrun_dangerous_percentage"
)
SEASON_SYNC_SC_COLS <- c(SEASON_SYNC_SC_COLS, paste0("sc_", SEASON_SYNC_SC_COLS))

SEASON_SYNC_KEY_COLS <- c("player_name", "transfermarkt_id", "season_name", "position_group", "Liga")

suppressWarnings(suppressMessages({
  library(mongolite)
  library(dplyr)
}))

MONGO_URI        <- Sys.getenv("MONGO_URI")
MONGO_DB         <- Sys.getenv("MONGO_DB")
MONGO_COLLECTION <- Sys.getenv("MONGO_SEASON_COLLECTION", "sb_sc_metrics_by_season")

if (!nzchar(MONGO_URI) || !nzchar(MONGO_DB)) {
  stop("MONGO_URI and MONGO_DB must be set (see header comment in sync_to_mongo.R)")
}

message("Building get_all_players_raw_df() via app.R (source, not run) ...")
suppressWarnings(suppressMessages({
  source("app.R")
}))

raw <- get_all_players_raw_df()

seasons <- raw$season_name |> unique() |> stats::na.omit()
season_rows <- lapply(seasons, function(s) {
  raw |> dplyr::filter(season_name == s) |> dedup_transfers()
}) |> dplyr::bind_rows()

message(sprintf("Per-season rows after dedup_transfers() within each season: %d (seasons: %s)",
                nrow(season_rows), paste(seasons, collapse = ", ")))

# Same TM-crosswalk construction as build_database_master() (app.R) --
# duplicated rather than refactoring that function, since it also does a
# lot of Acumulado-specific work (profiles, Pie, Posiciones_jugadas,
# Hispanohablante) this per-season sync doesn't need.
contract_year <- sub("^.*/(\\d{4})$", "\\1", tm_crosswalk$contract_expires)
contract_year[!grepl("^\\d{4}$", contract_year)] <- NA_character_
tm_lookup <- tm_crosswalk
tm_lookup$contract_year <- contract_year
tm_lookup <- tm_lookup |> dplyr::filter(!is.na(player_name), !is.na(team_name))

season_rows <- season_rows |>
  dplyr::mutate(Liga = .league_label) |>
  join_tm_crosswalk(tm_lookup)

synced <- season_rows |>
  dplyr::filter(!is.na(tm_player_id), nzchar(tm_player_id)) |>
  dplyr::mutate(transfermarkt_id = suppressWarnings(as.integer(tm_player_id))) |>
  dplyr::filter(!is.na(transfermarkt_id)) |>
  dplyr::distinct(transfermarkt_id, season_name, .keep_all = TRUE) |>
  dplyr::select(dplyr::any_of(c(SEASON_SYNC_KEY_COLS, SEASON_SYNC_SB_COLS, SEASON_SYNC_SC_COLS))) |>
  dplyr::mutate(dplyr::across(where(is.factor), as.character))

message(sprintf(
  "%d per-season rows total, %d with a resolved transfermarkt_id -- syncing those.",
  nrow(season_rows), nrow(synced)
))

if (nrow(synced) == 0) {
  message("Nothing to sync -- exiting without touching MongoDB.")
  quit(save = "no", status = 0)
}

conn <- mongolite::mongo(collection = MONGO_COLLECTION, db = MONGO_DB, url = MONGO_URI)
conn$drop()
conn$insert(synced, pagesize = 500)
tryCatch(
  conn$index(add = '{"transfermarkt_id": 1, "season_name": 1}'),
  error = function(e) message("Note: index() skipped (likely already exists): ", conditionMessage(e))
)

message(sprintf(
  "Synced %d docs to %s.%s (collection dropped and reinserted)",
  nrow(synced), MONGO_DB, MONGO_COLLECTION
))
