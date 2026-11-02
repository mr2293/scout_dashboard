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
# AmeScore+ (SkillCorner physical enrichment, ame_score_plus.R) wired in
# 2026-10-14 as DataScoreAmericaPlus/DataScoreAmericaPlus_published --
# the "_published" flag exists because the doc explicitly forbids
# zero-imputing AmeScore+ when a player has no real physical coverage
# (only ~13-30% of players do, depending on role); NA, never a silent
# fallback to DataScoreAmerica.
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
source("scale_mapping.R")   # scale_datascore(), scale_amescore() -- final
                             # 0-100 mapping, applied as the LAST step below.

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

  # AmeScore+ (SkillCorner physical enrichment, ame_score_plus.R): joined
  # onto the SAME source row ame_joined already resolved above (not
  # independently re-resolved) -- same "one consistent league/season
  # context per player" rule as AmeScore itself (see file header).
  # AmeScorePlus_published is FALSE whenever that role/perfil has no
  # physical-enrichment mapping or the player lacks real SkillCorner
  # coverage there -- per doc S10.2's "never zero-impute" rule, those
  # players get NA, not a value that silently equals AmeScore.
  amescore_plus <- compute_amescore_plus_all_roles(dat_v2) |>
    dplyr::distinct(var_name, player_id, .keep_all = TRUE) |>
    dplyr::select(var_name, player_id, AmeScorePlus_final, AmeScorePlus_published)

  ame_plus_joined <- ame_joined |>
    dplyr::select(player_id, var_name) |>
    dplyr::left_join(amescore_plus, by = c("var_name", "player_id"))

  ds_result |>
    dplyr::transmute(
      player_id,
      # scale_datascore()/scale_amescore() (scale_mapping.R) apply the
      # final 0-100 mapping HERE -- the stored/synced value is already
      # display-ready, not a raw pipeline intermediate.
      DataScore = round(scale_datascore(DataScore_final), 1),
      # Redefinition vs. the old column: C is minutes x coverage
      # confidence (doc S12.3 step 4), not just capacity-weight coverage
      # -- a richer "how much do we trust this score" than before, same
      # intended purpose, different (better-grounded) computation.
      Cobertura_DataScore = round(C * 100, 1),
      DataScoreAmerica = round(scale_amescore(ame_gate$AmeScore_final), 1),
      Cobertura_DataScoreAmerica = ame_joined$AmeScore_Base_cobertura,
      role_group_matchbased_v2 = source_role,
      # Reuses AmeScore's own scale bounds (same gate/techo mechanism,
      # just with some capacities physical-blended) -- NA, not a
      # fallback to DataScoreAmerica, when AmeScorePlus_published is
      # FALSE (see note above).
      DataScoreAmericaPlus = ifelse(
        ame_plus_joined$AmeScorePlus_published,
        round(scale_amescore(ame_plus_joined$AmeScorePlus_final), 1),
        NA_real_
      ),
      DataScoreAmericaPlus_published = ame_plus_joined$AmeScorePlus_published,
      # Same source row backs BOTH DataScore and DataScoreAmerica (see
      # file header) -- one label covers both. For season-context
      # labeling in the UI: which league/season a displayed score is
      # actually from, since the transition gate can hold over an
      # old-league row (gate_active) rather than always showing "current".
      SourceLeague = source_league,
      SourceSeason = source_season_name
    ) -> result

  .datascore_v2_cache <<- result
  result
}

# ============================================================
# get_datascore_v2_scores_by_season()
#
# Sibling to get_datascore_v2_scores() above -- NOT a modification of
# it, and does not call it. Returns DataScore/DataScoreAmerica/
# DataScoreAmericaPlus at (player_id, season_name) granularity instead
# of career-blended Acumulado.
#
# The per-row scoring this needs already exists inside
# get_datascore_v2_scores() as the intermediate `rows` table --
# build_scored_rows() scores every (player_id, league, season) row
# individually; get_datascore_v2_scores() only looks career-level
# because apply_transition_gate() collapses `rows` to one row per
# player right before confidence shrinkage / the AmeScore gate run.
# This function reuses `rows` directly and skips that collapse.
#
# Role-mean baselines (role_means_ds/role_means_ame) are recomputed
# here with the EXACT SAME expression get_datascore_v2_scores() uses --
# same global pool, so a player's per-season score and their Acumulado
# score shrink toward the same reference point. Deliberately not
# cached/shared between the two functions: each is a 3-line
# group_by/summarise over `rows`, cheaper than adding a shared-state
# dependency between two functions that must otherwise stay independent
# (see the "Acumulado must stay untouched" constraint in the design
# spec, docs/superpowers/specs/2026-11-01-per-season-datascore-design.md).
#
# apply_ame_score_gate() is called directly on the full `rows` table
# here -- the exact same call ame_score_gate.R's own validation harness
# (its `if (sys.nframe() == 0)` block) already makes; this just reuses
# that already-exercised pattern instead of introducing a new one.
# ============================================================

.datascore_v2_by_season_cache <- NULL

get_datascore_v2_scores_by_season <- function(force_refresh = FALSE) {
  if (!force_refresh && !is.null(.datascore_v2_by_season_cache)) return(.datascore_v2_by_season_cache)

  tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
  dat_v2 <- load_scout_data()
  rows <- build_scored_rows(dat_v2, tiers)

  # Compute role-mean baselines from the full `rows` table BEFORE dedup,
  # same as get_datascore_v2_scores() does. This ensures per-season scores
  # shrink toward the same global baseline as Acumulado (the binding
  # constraint in the design spec), not a dedup-narrowed pool.
  role_means_ds <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::group_by(role_group_matchbased) |>
    dplyr::summarise(mean_ds = mean(DataScore_previo), .groups = "drop") |>
    tibble::deframe()

  role_means_ame <- rows |>
    dplyr::filter(!is.na(AmeScore_Base)) |>
    dplyr::group_by(role_group_matchbased) |>
    dplyr::summarise(mean_ame = mean(AmeScore_Base), .groups = "drop") |>
    tibble::deframe()

  # Ensure one row per (player_id, season_name) by keeping the row with
  # the most minutes played -- same tiebreak apply_transition_gate()
  # (league_transition_gate.R) already uses elsewhere in this pipeline
  # ("Ties... broken by minutes played, most first"). This handles the
  # edge case where a player transferred mid-season between two leagues
  # with the same season_name (e.g., Aug-May calendar), so raw rows has
  # multiple var_name values for one (player_id, season_name).
  rows <- rows |>
    dplyr::slice_max(player_season_minutes, n = 1, by = c(player_id, season_name), with_ties = FALSE)

  # DataScore: rename each row's own columns to what
  # apply_confidence_shrinkage() expects (source_role/source_minutes/
  # source_coverage/DataScore_shown), instead of routing through
  # apply_transition_gate()'s one-row-per-player collapse. Every
  # (player_id, league, season) row gets shrunk independently.
  ds_by_season <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      source_role     = role_group_matchbased,
      source_minutes  = player_season_minutes,
      source_coverage = DataScore_Base_cobertura,
      DataScore_shown = DataScore_previo
    ) |>
    apply_confidence_shrinkage(role_means_ds) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      DataScore = round(scale_datascore(DataScore_final), 1),
      Cobertura_DataScore = round(C * 100, 1)
    )

  # AmeScore: apply_ame_score_gate() directly on every row of `rows` --
  # no join needed, AmeScore_Base/GATE_CAPACITY already live on `rows`
  # itself (unlike get_datascore_v2_scores(), which has to re-join them
  # back on after the transition gate already dropped them).
  ame_gate <- apply_ame_score_gate(
    rows$AmeScore_Base, rows[[GATE_CAPACITY]],
    rows$role_group_matchbased, role_means_ame
  )
  ame_by_season <- dplyr::bind_cols(
    rows |> dplyr::select(player_id, var_name, season_name, AmeScore_Base_cobertura),
    ame_gate
  ) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      DataScoreAmerica = round(scale_amescore(AmeScore_final), 1),
      Cobertura_DataScoreAmerica = AmeScore_Base_cobertura
    )

  # AmeScore+: compute_amescore_plus_all_roles() is already keyed by
  # (var_name, player_id) and never collapses on its own -- used as-is.
  amescore_plus <- compute_amescore_plus_all_roles(dat_v2) |>
    dplyr::distinct(var_name, player_id, .keep_all = TRUE) |>
    dplyr::transmute(
      var_name, player_id,
      DataScoreAmericaPlus = ifelse(
        AmeScorePlus_published,
        round(scale_amescore(AmeScorePlus_final), 1),
        NA_real_
      ),
      DataScoreAmericaPlus_published = AmeScorePlus_published
    )

  result <- ds_by_season |>
    dplyr::left_join(ame_by_season, by = c("player_id", "var_name", "season_name")) |>
    dplyr::left_join(amescore_plus, by = c("player_id", "var_name")) |>
    dplyr::select(player_id, season_name, DataScore, Cobertura_DataScore,
                  DataScoreAmerica, Cobertura_DataScoreAmerica,
                  DataScoreAmericaPlus, DataScoreAmericaPlus_published)

  .datascore_v2_by_season_cache <<- result
  result
}
