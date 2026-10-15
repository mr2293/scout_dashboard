# ============================================================
# seed_score_history_2526_fresh.R
#
# Adds a SECOND backfill snapshot to player_score_history, distinct from
# seed_score_history_baseline.R's (dated Aug 25, 2025, ~400 days ago at
# the time it was seeded) -- this one is scored with TODAY's pipeline
# (every weight/lambda/gate/scale-mapping fix made since Aug 2025) but
# restricted to 25/26-season rows only, dated AFTER the 25/26 season
# concluded instead of near its start.
#
# WHY: as of 2026-10-15, the Tendencias treemap's 1M/2M/6M/1A tabs all
# silently collapse onto the SAME Aug 25, 2025 baseline (no real weekly
# snapshot old enough yet to fill the 30/60/180/365-day windows), which
# misrepresents "1M" as a ~14-month comparison. User asked (2026-10-15)
# to keep 6M/1A on the Aug 2025 baseline but give 1M/2M a genuinely
# different, more recent comparison point -- using the freshest possible
# 25/26-season-specific scores, not this week's 26/27 data (that would
# defeat the point of a SEASONAL comparison).
#
# SNAPSHOT_DATE below is deliberately NOT "today minus 45 days" or
# similar -- it's pinned to shortly after the 25/26 season genuinely
# ended (domestic leagues conclude ~May-June), so it reads as "last
# season's final form," not an arbitrary lookback count. Chosen to land
# AFTER the 6M cutoff (so 6M/1A keep resolving to the Aug 2025 baseline,
# per the user's request) and BEFORE the 2M cutoff as of the day this
# was seeded -- as real weekly snapshots (sync_to_mongo.R) progressively
# fall inside the 1M/2M windows over the coming weeks, they'll naturally
# supersede this one with no code change needed, same self-phasing-out
# design as seed_score_history_baseline.R.
#
# Scoring logic: build_scored_rows() on ALL data, then filtered to rows
# whose season_name starts with "2025" BEFORE running
# apply_transition_gate()/apply_confidence_shrinkage()/
# apply_ame_score_gate() -- those three functions all operate correctly
# on a pre-filtered row set with no changes needed: restricted to one
# season, the gate's "most recent row" picks the 25/26 row with the most
# minutes (its own within-season tie-break), "transitioned" is always
# FALSE (there's no later season left to compare against), and role
# means/reference pools are computed from the SAME 25/26-only rows, so
# the whole snapshot is internally self-contained to that season --
# never blended with 26/27, and never leaking current-season reference
# pools into a supposedly historical comparison.
#
# NOT wired into .github/workflows/deploy.yml -- one-time seed, run
# manually like seed_score_history_baseline.R. Requires the same env
# vars (MONGO_URI, MONGO_DB).
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

# After essentially every 25/26 domestic league has finished (Liga MX
# Clausura, European leagues, etc. all conclude by ~May/June) -- see
# file header for why this specific placement (after the 6M cutoff,
# before the 2M cutoff) matters.
SNAPSHOT_DATE <- "2026-06-01"

message("Building the scoring pipeline via app.R (source, not run) ...")
suppressWarnings(suppressMessages({
  source("app.R")
}))

tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
dat_v2 <- load_scout_data()

rows_all <- build_scored_rows(dat_v2, tiers)
rows_2526 <- rows_all |>
  dplyr::filter(!is.na(season_name), startsWith(as.character(season_name), "2025"))
message(sprintf("25/26 scored rows: %d (of %d total across all seasons)", nrow(rows_2526), nrow(rows_all)))

gated <- apply_transition_gate(rows_2526)

role_means_ds <- rows_2526 |>
  dplyr::filter(!is.na(DataScore_previo)) |>
  dplyr::group_by(role_group_matchbased) |>
  dplyr::summarise(mean_ds = mean(DataScore_previo), .groups = "drop") |>
  tibble::deframe()

ds_result <- apply_confidence_shrinkage(gated, role_means_ds)

role_means_ame <- rows_2526 |>
  dplyr::filter(!is.na(AmeScore_Base)) |>
  dplyr::group_by(role_group_matchbased) |>
  dplyr::summarise(mean_ame = mean(AmeScore_Base), .groups = "drop") |>
  tibble::deframe()

source_rows <- rows_2526 |>
  dplyr::distinct(var_name, player_id, .keep_all = TRUE) |>
  dplyr::select(var_name, player_id, AmeScore_Base, AmeScore_Base_cobertura,
                dplyr::all_of(GATE_CAPACITY), role_group_matchbased)

ame_joined <- ds_result |>
  dplyr::select(player_id, var_name = source_var_name, source_role) |>
  dplyr::left_join(source_rows, by = c("var_name", "player_id"))

ame_gate <- apply_ame_score_gate(
  ame_joined$AmeScore_Base, ame_joined[[GATE_CAPACITY]],
  ame_joined$role_group_matchbased, role_means_ame
)

scored <- ds_result |>
  dplyr::transmute(
    player_id,
    DataScore = round(scale_datascore(DataScore_final), 1),
    DataScoreAmerica = round(scale_amescore(ame_gate$AmeScore_final), 1),
    SourceLeague = source_league
  )

# player_id -> transfermarkt_id, same crosswalk db_master already
# resolved -- same pattern as seed_score_history_baseline.R.
master <- get_db_master()
tm_map <- master |>
  dplyr::filter(!is.na(tm_player_id), nzchar(tm_player_id)) |>
  dplyr::mutate(transfermarkt_id = suppressWarnings(as.integer(tm_player_id))) |>
  dplyr::filter(!is.na(transfermarkt_id)) |>
  dplyr::distinct(player_id, .keep_all = TRUE) |>
  dplyr::select(player_id, transfermarkt_id)

baseline <- scored |>
  dplyr::inner_join(tm_map, by = "player_id") |>
  dplyr::filter(!is.na(DataScoreAmerica) | !is.na(DataScore)) |>
  dplyr::distinct(transfermarkt_id, .keep_all = TRUE) |>
  dplyr::transmute(
    transfermarkt_id,
    snapshot_date      = SNAPSHOT_DATE,
    data_score         = DataScore,
    data_score_america = DataScoreAmerica,
    liga               = SourceLeague
  )

message(sprintf("25/26-season (fresh pipeline) rows with a resolved transfermarkt_id + score: %d", nrow(baseline)))

if (nrow(baseline) == 0) {
  message("Nothing to seed -- exiting without touching MongoDB.")
  quit(save = "no", status = 0)
}

conn <- mongolite::mongo(collection = "player_score_history", db = MONGO_DB, url = MONGO_URI)
# Scoped to this exact snapshot_date only -- never touches the Aug 2025
# baseline or the real weekly snapshots sync_to_mongo.R writes.
conn$remove(sprintf('{"snapshot_date": "%s"}', SNAPSHOT_DATE))
conn$insert(baseline, pagesize = 500)
tryCatch(
  conn$index(add = '{"transfermarkt_id": 1, "snapshot_date": 1}'),
  error = function(e) message("Note: index() skipped (likely already exists): ", conditionMessage(e))
)

message(sprintf(
  "Seeded %d 25/26-season (fresh-pipeline) docs to %s.player_score_history, dated %s",
  nrow(baseline), MONGO_DB, SNAPSHOT_DATE
))
