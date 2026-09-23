# ============================================================
# compute_similarity.R
#
# Precomputes each non-goalkeeper player's top-20 most similar players
# (cosine similarity over StatsBomb + SkillCorner per-90 metrics) and syncs
# the result to a MongoDB collection ("player_similarity") for the
# colleague's Vercel dashboard's player-profile modal to look up instantly.
#
# Why precomputed instead of live: benchmarked the live version (fetch full
# non-keeper pool from Mongo + compute) at ~15s per request (2026-09-23,
# ~19,550 players x ~483 candidate metric columns) -- past Vercel's default
# 10s serverless timeout. The cosine-similarity math itself is fast
# (~200ms); the bottleneck is pulling the whole pool over the network on
# every profile open. Precomputing here, once a week alongside the
# scheduled sb_sc_metrics sync, turns each profile-open lookup into two
# tiny indexed Mongo queries instead.
#
# Algorithm is build_similarity_pool()/cosine_sim_to_i() from app.R (moved
# to top-level, not duplicated here, specifically so this script and the
# live "Jugadores Similares" tab can never drift apart) -- see that file
# for the actual pool construction (NA-rate/variance filtering,
# mean-imputation) and cosine similarity definition.
#
# Not wired into .github/workflows/deploy.yml yet -- same reasoning as
# sync_to_mongo.R: run manually and verify the Mongo output first.
#
# Requires the same MONGO_URI/MONGO_DB (and mongolite) as sync_to_mongo.R.
# ============================================================

suppressWarnings(suppressMessages({
  library(mongolite)
  library(dplyr)
}))

MONGO_URI        <- Sys.getenv("MONGO_URI")
MONGO_DB         <- Sys.getenv("MONGO_DB")
MONGO_COLLECTION <- Sys.getenv("MONGO_SIMILARITY_COLLECTION", "player_similarity")
TOP_K            <- 20
BLOCK            <- 1000  # rows per matrix-multiply block, keeps peak memory to BLOCK x n instead of n x n

if (!nzchar(MONGO_URI) || !nzchar(MONGO_DB)) {
  stop("MONGO_URI and MONGO_DB must be set (see header comment in sync_to_mongo.R)")
}

message("Sourcing app.R for get_all_players_sc_df()/get_db_master()/build_similarity_pool() ...")
suppressWarnings(suppressMessages({
  source("app.R")
}))

dat_all <- get_all_players_sc_df()
built   <- build_similarity_pool(dat_all)  # pg = NULL -> excludes Portero, same default the live tab uses
pool    <- built$pool
metrics <- built$metric_cols
message(sprintf("Pool: %d players, %d metric columns survived NA-rate/variance filtering.", nrow(pool), length(metrics)))

if (length(metrics) < 5 || nrow(pool) < 10) {
  stop("Similarity pool too small to compute anything meaningful -- check get_all_players_sc_df()/build_similarity_pool().")
}

# transfermarkt_id isn't in `pool` (build_similarity_pool() only keeps
# player_name/team_name/primary_position/minutes + metrics) -- get_db_master()
# already resolves it per real player via the Transfermarkt crosswalk (see
# sync_to_mongo.R), so borrow it here by player_name rather than
# rebuilding that join. distinct(Jugador) first: db_master has one row per
# player already (post dedup_transfers()), this is just defensive.
db_master  <- get_db_master()
tm_by_name <- db_master |>
  dplyr::filter(!is.na(tm_player_id), nzchar(tm_player_id)) |>
  dplyr::distinct(Jugador, .keep_all = TRUE) |>
  dplyr::transmute(Jugador, transfermarkt_id = suppressWarnings(as.integer(tm_player_id))) |>
  dplyr::filter(!is.na(transfermarkt_id))

pool$transfermarkt_id <- tm_by_name$transfermarkt_id[match(pool$player_name, tm_by_name$Jugador)]

n_before <- nrow(pool)
pool <- pool[!is.na(pool$transfermarkt_id), ]
message(sprintf("Dropped %d/%d pool players with no resolved transfermarkt_id (can't be looked up on the Node side).", n_before - nrow(pool), n_before))

# Same normalization cosine_sim_to_i() does per-call (scale() = z-score,
# then unit-length rows) -- done once here since we need it for every row,
# not just one target index.
M <- as.matrix(pool[, metrics, drop = FALSE])
M <- scale(M)
M[is.na(M)] <- 0
row_norms <- sqrt(rowSums(M^2))
row_norms[row_norms == 0 | !is.finite(row_norms)] <- 1
Mn <- M / row_norms

n <- nrow(Mn)
tm_ids <- pool$transfermarkt_id

message(sprintf("Computing top-%d similar players for %d players in blocks of %d ...", TOP_K, n, BLOCK))
t0 <- Sys.time()

# One row per player: { transfermarkt_id, top: [{transfermarkt_id, similarity}, ...] }.
# Building the nested list directly (not a flat data.frame) since mongolite
# inserts a data.frame with a list-column as a proper embedded array.
out_transfermarkt_id <- integer(n)
out_top <- vector("list", n)

for (start in seq(1, n, by = BLOCK)) {
  end <- min(start + BLOCK - 1, n)
  block_sims <- Mn[start:end, , drop = FALSE] %*% t(Mn)  # (end-start+1) x n

  for (r in seq_len(end - start + 1)) {
    i <- start + r - 1
    row <- block_sims[r, ]
    row[i] <- -Inf  # exclude self
    top_idx <- order(row, decreasing = TRUE)[seq_len(min(TOP_K, n - 1))]
    out_transfermarkt_id[i] <- tm_ids[i]
    out_top[[i]] <- data.frame(
      transfermarkt_id = tm_ids[top_idx],
      similarity = round(pmin(pmax(row[top_idx], -1), 1), 4)
    )
  }
  message(sprintf("  ...%d/%d done (%.0fs elapsed)", end, n, as.numeric(Sys.time() - t0, units = "secs")))
}

result <- data.frame(transfermarkt_id = out_transfermarkt_id)
result$top <- out_top

message(sprintf("Computed in %.1f minutes. Syncing to %s.%s ...", as.numeric(Sys.time() - t0, units = "mins"), MONGO_DB, MONGO_COLLECTION))

conn <- mongolite::mongo(collection = MONGO_COLLECTION, db = MONGO_DB, url = MONGO_URI)
conn$drop()
conn$insert(result, pagesize = 500)
conn$index(add = '{"transfermarkt_id": 1}')

message(sprintf("Synced %d docs to %s.%s (collection dropped and reinserted)", nrow(result), MONGO_DB, MONGO_COLLECTION))
