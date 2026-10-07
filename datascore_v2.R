# ============================================================
# datascore_v2.R
#
# Wires the standalone DataScore v2 / AmeScore pipeline (normalization.R
# through ame_score_plus.R, built 2026-10-07/09) into app.R. Exposes
# get_datascore_v2_scores(): one row per player_id with DataScore,
# Cobertura_DataScore, DataScoreAmerica, Cobertura_DataScoreAmerica --
# the exact fields add_datascore()/add_america_fit() populated before,
# computed by the new pipeline instead of DATASCORE_MODELS/
# AMERICA_FIT_MODELS/.weighted_profile_score() (left in place below,
# unused, for easy rollback/comparison -- not deleted).
#
# NOT a per-row transform of app.R's `dat` -- app.R's dat comes from
# get_all_players_df(), which is ALREADY deduplicated across leagues/
# seasons (dedup_transfers() blends a multi-league player's stints into
# one weighted-average row). The new pipeline instead keeps every
# (league, season) row separate and uses the league-transition gate to
# decide which ONE counts as "current" -- so the integration point is
# joining that gate's already-resolved per-player result onto dat by
# player_id, not re-deriving scores inside dat's own blended rows.
#
# AmeScore uses the SAME source row the gate picked for DataScore (not
# independently re-resolved) -- whichever league-season context is
# "current" for DataScore also provides AmeScore, so a player gets one
# consistent picture instead of two separately-resolved ones.
#
# EXPLORATORY INTEGRATION PASS, 2026-10-09 -- not committed yet. See the
# datascore-v2-overhaul project memory for what's validated vs. still
# placeholder (every weight/threshold/lambda/rho in the pipeline this
# sources is explicitly illustrative, "por calibrar").
# ============================================================

source("ame_score_plus.R")  # everything upstream: normalize_metric(),
                             # capacity_subscore(), combine_capacities(),
                             # apply_competition_strength(), build_scored_rows(),
                             # apply_transition_gate(), apply_confidence_shrinkage(),
                             # apply_ame_score_gate(), classify_delantero_perfil(),
                             # GATE_CAPACITY, etc. (via its own source() chain)

.datascore_v2_cache <- NULL

get_datascore_v2_scores <- function(force_refresh = FALSE) {
  if (!force_refresh && !is.null(.datascore_v2_cache)) return(.datascore_v2_cache)

  tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
  dat_v2 <- load_scout_data()

  rows <- build_scored_rows(dat_v2, tiers)
  gated <- apply_transition_gate(rows)

  role_means_ds <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::group_by(role_group_matchbased) |>
    dplyr::summarise(mean_ds = mean(DataScore_previo), .groups = "drop") |>
    tibble::deframe()

  ds_result <- apply_confidence_shrinkage(gated, role_means_ds)

  role_means_ame <- rows |>
    dplyr::filter(!is.na(AmeScore_Base)) |>
    dplyr::group_by(role_group_matchbased) |>
    dplyr::summarise(mean_ame = mean(AmeScore_Base), .groups = "drop") |>
    tibble::deframe()

  # Pull AmeScore_Base/coverage/GATE_CAPACITY from the SAME row the gate
  # already resolved as "current" (source_var_name) -- de-duplicated
  # defensively first (a small, known subset of (var_name, player_id)
  # pairs repeat in the raw data; harmless for per-row scoring, but would
  # fan out this join otherwise).
  source_rows <- rows |>
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

  ds_result |>
    dplyr::transmute(
      player_id,
      DataScore = DataScore_final,
      # Redefinition vs. the old column: C is minutes x coverage
      # confidence (doc S12.3 step 4), not just capacity-weight coverage
      # -- a richer "how much do we trust this score" than before, same
      # intended purpose, different (better-grounded) computation.
      Cobertura_DataScore = round(C * 100, 1),
      DataScoreAmerica = ame_gate$AmeScore_final,
      Cobertura_DataScoreAmerica = ame_joined$AmeScore_Base_cobertura,
      role_group_matchbased_v2 = source_role
    ) -> result

  .datascore_v2_cache <<- result
  result
}
