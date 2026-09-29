# ============================================================
# seed_score_history_baseline.R
#
# ONE-TIME backfill for player_score_history (see sync_to_mongo.R's
# weekly snapshot step) -- the América data score treemap needs a "past"
# value to compare against for every lookback window (1D/1W/2W/1M/2M/6M/
# 1Y), but real weekly snapshots only started this week (see
# sync_to_mongo.R). Rather than showing "no data" for every window until
# real snapshots accumulate over the next year, this seeds a single
# baseline snapshot per player computed from LAST season's data (25/26,
# StatsBomb season_id 318 -- see dashboard_scout.R's season_id pairs,
# shared across every league pulled there) instead of the current
# blended/combined-seasons row build_database_master() normally uses.
#
# Dated far enough in the past (400 days, > the largest window the chart
# offers, 1 year) so it's picked up as the fallback "as of" comparison
# for EVERY window right now -- as real weekly snapshots accumulate,
# they'll naturally take over first for the shortest windows, then
# progressively the longer ones, phasing this baseline out on their own
# without any code change here.
#
# NOT wired into .github/workflows/deploy.yml -- this is a one-time
# backfill, run manually once. Ongoing weekly snapshots are
# sync_to_mongo.R's job.
#
# Requires the same env vars as sync_to_mongo.R (MONGO_URI, MONGO_DB).
# ============================================================

suppressWarnings(suppressMessages({
  library(mongolite)
  library(dplyr)
}))

MONGO_URI <- Sys.getenv("MONGO_URI")
MONGO_DB  <- Sys.getenv("MONGO_DB")

if (!nzchar(MONGO_URI) || !nzchar(MONGO_DB)) {
  stop("MONGO_URI and MONGO_DB must be set (see header comment in sync_to_mongo.R)")
}

message("Building all_players_raw_df + db_master via app.R (source, not run) ...")
suppressWarnings(suppressMessages({
  source("app.R")
}))

# get_all_players_raw_df(): dedup_same_team() already run (collapses
# join-artifact duplicate rows), but NOT dedup_transfers() -- so a player
# keeps one row per real season instead of being blended into a single
# combined "Acumulado" row. That's exactly what's needed here: isolate
# 25/26 only, not 25/26 blended with 26/27.
raw <- get_all_players_raw_df()

# season_id 318 == "2025/2026" for the Europe+Liga MX calendar leagues and
# "2025" for calendar-year leagues (Colombia, etc.) -- matching on the
# leading 4 digits is the same trick get_player_row_for_season() already
# uses (see app.R) to bridge both naming schemes off one target year.
season_2526 <- raw |>
  dplyr::filter(!is.na(season_name), startsWith(as.character(season_name), "2025"))

message(sprintf("25/26 rows before dedup: %d", nrow(season_2526)))

# A player traded mid-season (still within 25/26) can have >1 row here
# even after dedup_same_team() -- keep the row with the most minutes as
# the representative one for that season (simplest reasonable pick;
# unlike dedup_transfers() this deliberately does NOT blend rows, since
# blending across teams-within-a-season is a different question than the
# across-seasons blending dedup_transfers() does).
season_2526 <- season_2526 |>
  dplyr::group_by(player_id) |>
  dplyr::slice_max(order_by = player_season_minutes, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()

message(sprintf("25/26 rows after per-player dedup: %d", nrow(season_2526)))

# Same scoring functions build_database_master() uses for the live
# DataScore/DataScoreAmerica, run here against the 25/26-only slice
# instead of the current combined-seasons one.
season_2526 <- add_datascore(season_2526)
season_2526 <- add_america_fit(season_2526)

# player_id -> transfermarkt_id is a stable identity mapping independent
# of season -- reuse the crosswalk db_master already resolved rather than
# rebuilding join_tm_crosswalk() logic here.
master <- get_db_master()
tm_map <- master |>
  dplyr::filter(!is.na(tm_player_id), nzchar(tm_player_id)) |>
  dplyr::mutate(transfermarkt_id = suppressWarnings(as.integer(tm_player_id))) |>
  dplyr::filter(!is.na(transfermarkt_id)) |>
  dplyr::distinct(player_id, .keep_all = TRUE) |>
  dplyr::select(player_id, transfermarkt_id)

baseline <- season_2526 |>
  dplyr::inner_join(tm_map, by = "player_id") |>
  dplyr::filter(!is.na(DataScoreAmerica) | !is.na(DataScore)) |>
  dplyr::distinct(transfermarkt_id, .keep_all = TRUE) |>
  dplyr::transmute(
    transfermarkt_id,
    snapshot_date      = format(Sys.Date() - 400, "%Y-%m-%d"),
    data_score         = DataScore,
    data_score_america = DataScoreAmerica,
    liga               = .league_label
  )

message(sprintf("Baseline rows with a resolved transfermarkt_id + score: %d", nrow(baseline)))

if (nrow(baseline) == 0) {
  message("Nothing to seed -- exiting without touching MongoDB.")
  quit(save = "no", status = 0)
}

conn <- mongolite::mongo(collection = "player_score_history", db = MONGO_DB, url = MONGO_URI)
# Scoped to this baseline's exact snapshot_date only -- re-running this
# script (e.g. after fixing a bug) replaces just the baseline batch,
# never touches the real weekly snapshots sync_to_mongo.R writes.
conn$remove(sprintf('{"snapshot_date": "%s"}', unique(baseline$snapshot_date)))
conn$insert(baseline, pagesize = 500)
conn$index(add = '{"transfermarkt_id": 1, "snapshot_date": 1}')

message(sprintf(
  "Seeded %d 25/26-season baseline docs to %s.player_score_history, dated %s",
  nrow(baseline), MONGO_DB, unique(baseline$snapshot_date)
))
