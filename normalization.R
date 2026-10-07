# ============================================================
# normalization.R
#
# Normalization layer for the DataScore v2 / AmeScore rebuild (see "Ideas
# AmeScore.pdf" S12.1/S19.1, brainstormed with the user 2026-10-07 -- see
# the datascore-v2-overhaul project memory). Converts a raw player_season_*
# metric into a 0-100 percentile WITHIN the player's role_group_matchbased
# pool (role_eligibility_match_based.R's minutes-weighted match-level
# classification, confirmed 2026-10-07 as the classification source for
# the real pipeline -- NOT role_eligibility.R's static-label version).
#
# Pool is GLOBAL across leagues (never scoped to a single league) -- the
# doc explicitly lists "confusing a P95 within a weak league for global
# excellence" as an error to avoid (S20), and Competition Strength is the
# separate additive lever that already handles league effects (S8); doing
# both would double-count league strength.
#
# Ratio metrics (StatsBomb's `_ratio`-suffixed columns) get shrunk toward
# the role-pool mean BEFORE the percentile step, weighted by minutes
# played, so a 3/4 sample doesn't percentile-rank the same as a 70/100
# sample (S12.1: "los ratios con muy pocas acciones deben... suavizarse").
# Companion attempt-count columns only exist for about half of the 18
# ratio metrics actually used in DATASCORE_MODELS/AMERICA_FIT_MODELS/
# PROFILE_METRIC_DEFS (app.R) -- rather than a fragile metric-by-metric
# mapping, minutes/90 is used as a uniform exposure proxy for all of them
# (user's explicit call, 2026-10-07).
#
# Standalone script, not wired into app.R yet -- same convention as
# role_eligibility.R / role_eligibility_match_based.R this session: fast
# to validate independently before this feeds capacity subscores.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
}))

# ---- 1. Lower-is-better metrics (duplicated from app.R rather than
# sourcing the whole live app file -- same reasoning as the role-mapping
# constants duplicated in role_eligibility_match_based.R) ----
LOWER_IS_BETTER_METRICS <- paste0("player_season_", c(
  "fouls_90", "yellow_cards_90", "second_yellow_cards_90", "red_cards_90",
  "errors_90", "turnovers_90", "dispossessions_90", "failed_dribbles_90",
  "dribbled_past_90", "shots_faced_90", "goals_faced_90", "np_xg_faced_90",
  "np_psxg_faced_90", "ot_shots_faced_90", "npot_psxg_faced_90",
  "penalties_faced_90", "penalties_conceded_90"
))

# Placeholder pseudo-count, "por calibrar" like every other weight/threshold
# at this stage (doc S12/S12.8) -- ~10 90s (900 minutes) of weight given to
# the role-pool mean before a ratio's own sample starts to dominate it.
RATIO_SHRINKAGE_K_90S <- 10

is_ratio_metric <- function(metric_name) grepl("_ratio$", metric_name)

# ---- 2. Core normalization function ----
# values:       numeric raw metric vector
# role_group:   character vector, same length -- player's role_group_matchbased
# exposure_90s: numeric vector, same length -- minutes/90 (only used for
#               ratio-metric shrinkage; ignored for every other metric)
# metric_name:  decides ratio-shrinkage + direction (lower-is-better)
#
# Returns a 0-100 percentile vector, computed independently within each
# role_group's own pool -- NA in, NA out, and a player with no role_group
# gets NA (can't be pooled).
normalize_metric <- function(values, role_group, exposure_90s, metric_name) {
  values <- suppressWarnings(as.numeric(values))
  n <- length(values)
  stopifnot(length(role_group) == n, length(exposure_90s) == n)

  out <- rep(NA_real_, n)
  groups <- unique(role_group[!is.na(role_group)])

  for (grp in groups) {
    idx <- which(role_group == grp & !is.na(values))
    if (!length(idx)) next

    v <- values[idx]

    if (is_ratio_metric(metric_name)) {
      pool_mean <- mean(v, na.rm = TRUE)
      exp90 <- exposure_90s[idx]
      exp90[is.na(exp90) | exp90 < 0] <- 0
      v <- (v * exp90 + pool_mean * RATIO_SHRINKAGE_K_90S) / (exp90 + RATIO_SHRINKAGE_K_90S)
    }

    pct <- dplyr::percent_rank(v) * 100
    if (metric_name %in% LOWER_IS_BETTER_METRICS) pct <- 100 - pct
    out[idx] <- pct
  }

  out
}

# ============================================================
# Validation harness below -- only runs when this script is executed
# directly (Rscript normalization.R), not when normalize_metric() is
# sourced elsewhere.
# ============================================================
if (sys.nframe() == 0) {

  # ---- 3. Load + join raw metrics and role classification (shared loader,
  # see load_scout_data.R -- this exact block used to be duplicated here). ----
  source("load_scout_data.R")
  dat <- load_scout_data()

  # ---- 6. Normalize every metric used in DATASCORE_MODELS (app.R) as the
  # validation set -- spans all 6 outfield position groups and most of the
  # 18 ratio metrics in use, without needing to parse app.R's full model
  # lists (AMERICA_FIT_MODELS/PROFILE_METRIC_DEFS get wired in at the
  # capacity-subscore stage, not here). ----
  datascore_metrics <- c(
    "player_season_obv_defensive_action_90", "player_season_padj_interceptions_90",
    "player_season_padj_tackles_and_interceptions_90", "player_season_dribble_faced_ratio",
    "player_season_aerial_ratio", "player_season_obv_pass_90", "player_season_deep_progressions_90",
    "player_season_obv_lbp_90", "player_season_passing_ratio", "player_season_pressured_passing_ratio",
    "player_season_xgbuildup_90", "player_season_errors_90", "player_season_padj_tackles_90",
    "player_season_challenge_ratio", "player_season_obv_dribble_carry_90", "player_season_crosses_90",
    "player_season_box_cross_ratio", "player_season_op_xa_90", "player_season_op_passes_into_box_90",
    "player_season_pressure_regains_90", "player_season_op_key_passes_90", "player_season_ball_recoveries_90",
    "player_season_fhalf_pressures_90", "player_season_xgchain_90", "player_season_npg_90",
    "player_season_through_balls_90", "player_season_op_f3_passes_90", "player_season_fhalf_ball_recoveries_90",
    "player_season_touches_inside_box_90", "player_season_np_xg_90", "player_season_counterpressure_regains_90",
    "player_season_dribbles_90", "player_season_dribble_ratio", "player_season_np_xg_per_shot",
    "player_season_shot_on_target_ratio", "player_season_np_shots_90"
  )
  datascore_metrics <- intersect(datascore_metrics, names(dat))

  normalized <- dat |> dplyr::select(var_name, player_id, player_name, role_group_matchbased, exposure_90s)
  for (m in datascore_metrics) {
    normalized[[paste0(m, "__pct")]] <- normalize_metric(
      values = dat[[m]], role_group = dat$role_group_matchbased,
      exposure_90s = dat$exposure_90s, metric_name = m
    )
  }

  out_path <- "data/normalized_metrics_sample.rds"
  saveRDS(normalized, out_path)
  message(sprintf("Wrote %s (%d rows, %d normalized metrics)", out_path, nrow(normalized), length(datascore_metrics)))

  # ---- 7. Sanity checks ----
  message("\n=== Percentile range per metric (should span ~0-100 within each role pool) ===")
  pct_cols <- paste0(datascore_metrics, "__pct")
  ranges <- purrr::map_dfr(pct_cols, function(c) {
    v <- normalized[[c]]
    tibble(metric = c, min = round(min(v, na.rm = TRUE), 1), max = round(max(v, na.rm = TRUE), 1),
           n_valid = sum(!is.na(v)))
  })
  print(ranges, n = Inf)

  message("\n=== Ratio-shrinkage spot check: dribble_ratio, low vs high exposure ===")
  spot <- dat |>
    dplyr::filter(!is.na(player_season_dribble_ratio)) |>
    dplyr::transmute(
      player_name, role_group_matchbased, exposure_90s,
      raw_ratio = player_season_dribble_ratio,
      pct = normalize_metric(player_season_dribble_ratio, role_group_matchbased, exposure_90s, "player_season_dribble_ratio")
    )
  message("-- Lowest-exposure players with a raw ratio >= 90th raw percentile in their role --")
  print(
    spot |>
      dplyr::group_by(role_group_matchbased) |>
      dplyr::filter(raw_ratio >= stats::quantile(raw_ratio, 0.9, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::arrange(exposure_90s) |>
      head(10)
  )
  message("-- Highest-exposure players for comparison --")
  print(spot |> dplyr::arrange(dplyr::desc(exposure_90s)) |> head(5))

  message("\n=== Coverage by role_group_matchbased ===")
  print(dat |> dplyr::count(role_group_matchbased, sort = TRUE))
}
