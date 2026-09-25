# ============================================================
# sync_to_mongo.R
#
# Pushes the full SB+SC player table (get_db_master(), the same 539-column
# table app.R's "Base de Datos" tab reads) into a MongoDB collection in
# the colleague's Vercel dashboard's database, keyed by transfermarkt_id --
# so his app can join his Transfermarkt collection to this one and surface
# StatsBomb/SkillCorner metrics in his UI.
#
# Wired into .github/workflows/deploy.yml as the "Sync to MongoDB (Club
# América dashboard)" step, right after "Precompute app cache" -- runs
# every Tuesday ~6am CDMX alongside the ShinyApps data refresh, using the
# same fresh StatsBomb/SkillCorner pull. MONGO_URI/MONGO_DB come from
# repo secrets there; no Vercel redeploy needed since that app reads
# Mongo live.
#
# Requires:
#   - the `mongolite` R package (already in renv.lock)
#   - env vars MONGO_URI (connection string) and MONGO_DB (database name)
#   - optionally MONGO_SB_SC_COLLECTION (defaults to "sb_sc_metrics")
#
# Only rows with a resolved Transfermarkt id (i.e. present and "matched"
# in data/transfermarkt_crosswalk.csv) are synced -- a player his app has
# no Transfermarkt document for can't be joined to anything on his side
# anyway.
# ============================================================

suppressWarnings(suppressMessages({
  library(mongolite)
  library(dplyr)
  library(jsonlite)
}))

MONGO_URI        <- Sys.getenv("MONGO_URI")
MONGO_DB         <- Sys.getenv("MONGO_DB")
MONGO_COLLECTION <- Sys.getenv("MONGO_SB_SC_COLLECTION", "sb_sc_metrics")

if (!nzchar(MONGO_URI) || !nzchar(MONGO_DB)) {
  stop("MONGO_URI and MONGO_DB must be set (see header comment in sync_to_mongo.R)")
}

message("Building db_master via app.R (source, not run) ...")
suppressWarnings(suppressMessages({
  source("app.R")
}))

master <- get_db_master()

# tm_player_id comes from data/transfermarkt_crosswalk.csv (joined into
# db_master via tm_lookup -- see app.R's TRANSFERMARKT CROSSWALK section).
# Rows with no match there get NA and are dropped: nothing to key a Mongo
# upsert on for a player his app doesn't have a Transfermarkt doc for.
synced <- master |>
  dplyr::filter(!is.na(tm_player_id), nzchar(tm_player_id)) |>
  dplyr::mutate(transfermarkt_id = suppressWarnings(as.integer(tm_player_id))) |>
  dplyr::filter(!is.na(transfermarkt_id)) |>
  dplyr::distinct(transfermarkt_id, .keep_all = TRUE)

message(sprintf(
  "db_master: %d rows total, %d with a resolved transfermarkt_id -- syncing those.",
  nrow(master), nrow(synced)
))

if (nrow(synced) == 0) {
  message("Nothing to sync -- exiting without touching MongoDB.")
  quit(save = "no", status = 0)
}

# This script always resyncs the *entire* current SB+SC table (not an
# incremental diff), so drop-and-reinsert is equivalent to upserting every
# row: whatever was in the collection before this run is, by definition,
# superseded by the current get_db_master() pull. insert() natively
# accepts a data frame and chunks it internally (`pagesize`), which avoids
# both problems the update()-based approach hit: this mongolite version's
# update() only takes a single query/update JSON object (not the
# newline-delimited bulk form some articles describe), and one combined
# ~17k-row x ~540-col payload in a single command was well past MongoDB's
# ~16MB command size limit -- which surfaced as a confusing "socket
# timeout calling hello" error rather than a clear size error.
synced <- synced |>
  dplyr::mutate(dplyr::across(where(is.factor), as.character))

conn <- mongolite::mongo(collection = MONGO_COLLECTION, db = MONGO_DB, url = MONGO_URI)
conn$drop()
conn$insert(synced, pagesize = 500)
conn$index(add = '{"transfermarkt_id": 1}')

message(sprintf(
  "Synced %d docs to %s.%s (collection dropped and reinserted)",
  nrow(synced), MONGO_DB, MONGO_COLLECTION
))
