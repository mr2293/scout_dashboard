# ============================================================
# precompute_app_cache.R
#
# Runs in the CI pipeline after join_scout_data.R (needs
# data/scout_joined.rds) and before deploy. Builds the expensive
# cross-league derived tables app.R would otherwise compute lazily on a
# live worker's first touch of each tab -- get_all_players_df(),
# get_all_players_raw_df(), get_db_master(), get_all_players_sc_df(),
# get_all_sc_df(), get_liga_mx_sc_df() -- and writes each one to its own
# file under data/app_cache/.
#
# Sourcing app.R (rather than reimplementing the same logic here) means
# there's one source of truth for how each table is built, so this can't
# silently drift from what the deployed app actually does.
# shinyApp(ui, server) at the end of app.R just constructs an app object
# when sourced from Rscript -- it doesn't start listening, so this is
# safe to run as a plain script.
#
# Why this exists: shinyapps.io workers are far more memory-constrained
# than this CI runner. Every one of the tables above involves binding
# ~17k+ rows across 42 leagues and running dedup/join/profile-scoring
# passes that hold multiple large intermediate frames in memory at once.
# Doing that on a live worker -- even lazily, only when a session first
# opens the relevant tab -- was enough to occasionally push a worker's
# container over its memory limit and get it OOM-killed, taking every
# other output on that worker down with it (confirmed via shinyapps.io
# logs on 2026-07-26). Building these once here, on a machine with far
# more headroom, and having app.R's get_*() functions load the bundled
# result via a fast readRDS() instead, moves that cost off the
# runtime path entirely.
#
# One file per table, not one combined app_cache.rds (2026-07-27): a
# single bundle file means readRDS() materializes ALL SIX tables the
# moment ANY getter is first touched -- e.g. just opening the Dashboard
# tab triggers get_all_players_df() for the player-search dropdown, which
# used to pull in db_master/all_sc_df/liga_mx_sc_df etc. right along with
# it. That front-loaded everything a session could possibly need within
# the first ~20s of a worker's life, which is exactly when a fresh
# worker's memory headroom is tightest -- confirmed via shinyapps.io logs
# OOM-killing a worker before a session ever reached the Jugadores
# Similares tab. Splitting into per-table files means each get_*() only
# pays for the one table it actually needs, when it needs it -- same
# lazy-per-tab shape the original (pre-bundle) runtime computation had,
# just with a fast readRDS() instead of a slow rebuild.
# ============================================================

suppressWarnings(suppressMessages({
  source("app.R")
}))

message("Precomputing derived tables for data/app_cache/ ...")
t0 <- Sys.time()

app_cache <- list(
  all_players_df     = get_all_players_df(),
  all_players_raw_df = get_all_players_raw_df(),
  db_master          = get_db_master(),
  all_players_sc_df  = get_all_players_sc_df(),
  all_sc_df          = get_all_sc_df(),
  liga_mx_sc_df      = get_liga_mx_sc_df()
)

message(sprintf("Done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

out_dir <- "data/app_cache"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

for (nm in names(app_cache)) {
  path <- file.path(out_dir, paste0(nm, ".rds"))
  saveRDS(app_cache[[nm]], file = path, compress = "gzip")
  message(sprintf("Wrote %s (%.1f MB)", path, file.info(path)$size / 1024^2))
}
