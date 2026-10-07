# ============================================================
# capacity_subscores.R
#
# Capacity subscores for the DataScore v2 / AmeScore rebuild (doc S12.2,
# S11 "Ejemplo de construcción: Interior/8"). A capacity (e.g. "Progresión")
# is a weighted average of several ALREADY-NORMALIZED metrics (see
# normalization.R's normalize_metric() -- percentile 0-100 within the
# player's role_group_matchbased pool), redistributing weight among
# whichever metrics are actually available for a player and withholding
# the subscore (NA) below a minimum coverage, same shape as doc S12.6 and
# structurally identical to app.R's existing .weighted_profile_score()
# (just operating on pre-normalized percentiles instead of raw values,
# since normalization is now its own decoupled layer).
#
# Brainstormed with the user 2026-10-07: capacities are SHARED building
# blocks between DataScore and AmeScore (built once from normalized
# metrics); each score combines them with its OWN weights at the next
# stage ("combine capacities" -- not built yet, this file stops at
# producing the subscores themselves).
#
# Interior is the doc's own worked "laboratory" role (S11) -- built here
# first as the validation case, same convention as normalization.R
# validating against DATASCORE_MODELS' metric set. Other roles' capacity
# definitions follow once this shape is confirmed to work end to end.
#
# A few of the doc's S11 candidate metrics for "Juego bajo presión"
# (Ball Retention Under Pressure, Forced Losses, Pressures Received)
# turned out to be SkillCorner-sourced columns in our data (no
# player_season_ prefix) -- substituted with StatsBomb equivalents
# (pressured_passing_ratio, change_in_passing_ratio) since DataScore/
# AmeScore must stay StatsBomb-only (SkillCorner only enters at
# AmeScore+). turnovers_90 moved to Ball Efficiency instead, to avoid
# reusing it in two capacities at once.
#
# Standalone script, not wired into app.R yet -- same convention as
# role_eligibility*.R / normalization.R this session.
#
# EXTENDED 2026-10-07/08 to Central, Lateral/Carrilero, Medio de
# Contención, Mediapunta and Delantero after the AmeScore-only-capacity
# fix (see ALMADA_IDENTITY_CAPACITIES below) was validated on Interior
# and confirmed to generalize (correlation drop of ~0.04-0.07 on every
# role tested). Mediapunta needs no capacity definition of its own -- it
# reuses INTERIOR_CAPACITIES verbatim (same 8 capacities), only its
# DataScore/AmeScore WEIGHTS differ (see base_scores.R). Volante/Extremo
# is deliberately excluded -- frozen pending the user's discussion with
# colleagues, per their explicit instruction 2026-10-07.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
}))

source("normalization.R")  # normalize_metric() -- sys.nframe() guard means
                            # this does NOT re-run normalization.R's own
                            # validation harness, just defines the function.

# ---- 0. AmeScore-only capacities, shared across every role -----------
# Found by reviewing the StatsBomb Player Season Stats v6.0.0 spec for
# metrics tied to Almada's identity stats (doc S4: PPDA, % presiones en
# campo rival, directness, etc.) that weren't in any capacity yet.
# Genuinely role-agnostic: pressing intensity/positioning and
# team-outcome connection are team-wide identity traits in Almada's
# system, not specific to any one position -- confirmed empirically,
# same ~0.04-0.07 correlation drop on every role tested (Central,
# Lateral/Carrilero, Medio de Contención, Volante/Extremo, Delantero,
# Interior). Considered adding role-specific extras (e.g.
# predicted_headers_90 for Central's aerial duels) but rejected --
# that's a general QUALITY signal, not an identity/fit one, so it
# belongs in DataScore's territory, not a manufactured AmeScore addition
# (see correlation_analysis.html for the full reasoning).
#
# These capacities NEVER get a DataScore weight anywhere (every
# *_DATASCORE_WEIGHTS vector below omits them) -- the whole point is
# content that distinguishes AmeScore, not just a reweighting of what
# DataScore already sees.
ALMADA_IDENTITY_CAPACITIES <- list(
  # Directly operationalizes the doc's own "% presiones en campo rival"
  # identity stat (44.0% for América, S4) via fhalf_pressures_ratio,
  # which nothing else used; plus responsibility-weighted defensive
  # involvement (v5/v6 StatsBomb fields), a genuinely new signal.
  "Presión posicional" = c(
    "player_season_fhalf_pressures_ratio" = 0.30,
    "player_season_counterpressures_90" = 0.25,
    "player_season_defensive_responsibility_actions_90" = 0.25,
    "player_season_obv_conceded_responsibility_weighted_90" = 0.20
  ),
  # positive_outcome_90/score measure whether a player's involvement
  # connects to a TEAM-level attacking outcome (shot, f-half free kick,
  # corner), the closest thing in the API to "does this player's game
  # feed what the team is trying to do" rather than individual output;
  # op_f3_forward_pass_proportion/pass_length_ratio sharpen the doc's
  # "Directness" identity stat to the final third specifically.
  "Conexión ofensiva" = c(
    "player_season_positive_outcome_90" = 0.30,
    "player_season_positive_outcome_score" = 0.30,
    "player_season_op_f3_forward_pass_proportion" = 0.20,
    "player_season_pass_length_ratio" = 0.20
  )
)

# ---- 1. Interior/8 capacity definitions -----------------------------
# Weights are placeholders ("pesos ilustrativos", doc S12 framing) except
# Progresión, which reuses the doc's own worked example (S12.2 table)
# verbatim. Every other capacity's weights are illustrative, to calibrate.
INTERIOR_CAPACITIES <- list(
  "Progresión" = c(
    "player_season_deep_progressions_90" = 0.30,
    "player_season_obv_pass_90" = 0.25,
    "player_season_obv_dribble_carry_90" = 0.20,
    "player_season_obv_lbp_90" = 0.25
  ),
  # Doc S6: "Menos presión por volumen -- dar más valor a regains,
  # counterpressure regains y contexto" (altura de presión) que al volumen
  # crudo de presiones.
  "Presión/contrapresión" = c(
    "player_season_pressure_regains_90" = 0.35,
    "player_season_counterpressure_regains_90" = 0.30,
    "player_season_average_x_pressure" = 0.20,
    "player_season_padj_pressures_90" = 0.15
  ),
  # StatsBomb substitutes for the doc's SkillCorner-sourced candidates --
  # see file header.
  "Juego bajo presión" = c(
    "player_season_pressured_passing_ratio" = 0.55,
    "player_season_change_in_passing_ratio" = 0.45
  ),
  # Doc S6: "Security -> Ball Efficiency: no premiar simplemente no
  # perderla; valorar cuánto genera el jugador respecto al riesgo
  # asumido." turnovers_90 lives here (not in Juego bajo presión) so it's
  # only used once across the 6 capacities.
  "Ball Efficiency" = c(
    "player_season_xgbuildup_90" = 0.40,
    "player_season_turnovers_90" = 0.35,
    "player_season_passing_ratio" = 0.25
  ),
  "Dinamismo/influencia entre fases" = c(
    "player_season_obv_dribble_carry_90" = 0.30,
    "player_season_transition_obv_90" = 0.30,
    "player_season_carry_length" = 0.20,
    "player_season_fhalf_pressures_90" = 0.20
  ),
  "Impacto ofensivo" = c(
    "player_season_op_xa_90" = 0.30,
    "player_season_op_xgchain_90" = 0.30,
    "player_season_touches_inside_box_90" = 0.20,
    "player_season_np_xg_90" = 0.20
  )
)
INTERIOR_CAPACITIES <- c(INTERIOR_CAPACITIES, ALMADA_IDENTITY_CAPACITIES)

# ---- 1b. Central -------------------------------------------------------
CENTRAL_CAPACITIES <- list(
  "Defensa/Duelos" = c(
    "player_season_aerial_ratio" = 0.30,
    "player_season_padj_tackles_and_interceptions_90" = 0.30,
    "player_season_dribble_faced_ratio" = 0.20,
    "player_season_padj_interceptions_90" = 0.20
  ),
  "Posicionamiento/Presión" = c(
    "player_season_obv_defensive_action_90" = 0.30,
    "player_season_average_x_defensive_action" = 0.25,
    "player_season_defensive_actions_above_expectation" = 0.25,
    "player_season_padj_pressures_90" = 0.20
  ),
  "Progresión con balón" = c(
    "player_season_obv_pass_90" = 0.40,
    "player_season_deep_progressions_90" = 0.35,
    "player_season_obv_lbp_90" = 0.25
  ),
  "Distribución/Seguridad" = c(
    "player_season_passing_ratio" = 0.25,
    "player_season_pressured_passing_ratio" = 0.20,
    "player_season_xgbuildup_90" = 0.20,
    "player_season_errors_90" = 0.20,
    "player_season_turnovers_90" = 0.15
  )
)
CENTRAL_CAPACITIES <- c(CENTRAL_CAPACITIES, ALMADA_IDENTITY_CAPACITIES)

# ---- 1c. Lateral/Carrilero ----------------------------------------------
LATERAL_CAPACITIES <- list(
  "Presión/Recuperación" = c(
    "player_season_obv_defensive_action_90" = 0.18,
    "player_season_padj_tackles_90" = 0.15,
    "player_season_padj_interceptions_90" = 0.13,
    "player_season_challenge_ratio" = 0.12,
    "player_season_pressure_regains_90" = 0.12,
    "player_season_aggressive_actions_90" = 0.10,
    "player_season_defensive_actions_above_expectation" = 0.10,
    "player_season_padj_pressures_90" = 0.10
  ),
  "Progresión/Conducción" = c(
    "player_season_obv_pass_90" = 0.40,
    "player_season_deep_progressions_90" = 0.35,
    "player_season_obv_dribble_carry_90" = 0.25
  ),
  "Creación/Centros" = c(
    "player_season_crosses_90" = 0.20,
    "player_season_box_cross_ratio" = 0.20,
    "player_season_op_xa_90" = 0.25,
    "player_season_op_passes_into_box_90" = 0.20,
    "player_season_op_key_passes_90" = 0.15
  ),
  "Seguridad" = c(
    "player_season_passing_ratio" = 0.50,
    "player_season_turnovers_90" = 0.50
  )
)
LATERAL_CAPACITIES <- c(LATERAL_CAPACITIES, ALMADA_IDENTITY_CAPACITIES)

# ---- 1d. Medio de Contención ---------------------------------------------
MC_CAPACITIES <- list(
  "Presión/Recuperación" = c(
    "player_season_obv_defensive_action_90" = 0.16,
    "player_season_padj_interceptions_90" = 0.13,
    "player_season_padj_tackles_90" = 0.11,
    "player_season_pressure_regains_90" = 0.10,
    "player_season_ball_recoveries_90" = 0.10,
    "player_season_challenge_ratio" = 0.08,
    "player_season_padj_pressures_90" = 0.10,
    "player_season_counterpressure_regains_90" = 0.09,
    "player_season_average_x_pressure" = 0.08,
    "player_season_defensive_actions_above_expectation" = 0.05
  ),
  # S6/S13: Line Breaking Passes añadidas 2026-10-07 para dar más
  # profundidad al componente de progresión (ver capacity_weights_review.html).
  "Circulación/Progresión" = c(
    "player_season_obv_pass_90" = 0.28,
    "player_season_deep_progressions_90" = 0.20,
    "player_season_obv_lbp_90" = 0.16,
    "player_season_forward_pass_proportion" = 0.14,
    "player_season_transition_obv_90" = 0.10,
    "player_season_lbp_90" = 0.07,
    "player_season_lbp_pass_ratio" = 0.05
  ),
  "Juego bajo presión" = c(
    "player_season_pressured_passing_ratio" = 0.55,
    "player_season_change_in_passing_ratio" = 0.45
  ),
  "Seguridad/Distribución" = c(
    "player_season_passing_ratio" = 0.35,
    "player_season_pressured_passing_ratio" = 0.25,
    "player_season_xgbuildup_90" = 0.20,
    "player_season_turnovers_90" = 0.20
  )
)
MC_CAPACITIES <- c(MC_CAPACITIES, ALMADA_IDENTITY_CAPACITIES)

# ---- 1e. Delantero (generic -- perfil-specific weights not built yet) --
DELANTERO_CAPACITIES <- list(
  "Finalización" = c(
    "player_season_npg_90" = 0.30,
    "player_season_np_xg_90" = 0.25,
    "player_season_np_xg_per_shot" = 0.15,
    "player_season_shot_on_target_ratio" = 0.15,
    "player_season_np_shots_90" = 0.15
  ),
  "Juego aéreo/área" = c(
    "player_season_touches_inside_box_90" = 0.50,
    "player_season_aerial_ratio" = 0.50
  ),
  "Presión alta" = c(
    "player_season_fhalf_pressures_90" = 0.25,
    "player_season_counterpressure_regains_90" = 0.20,
    "player_season_padj_pressures_90" = 0.20,
    "player_season_aggressive_actions_90" = 0.20,
    "player_season_average_x_pressure" = 0.15
  ),
  "Juego asociativo" = c(
    "player_season_op_xa_90" = 0.18,
    "player_season_op_key_passes_90" = 0.16,
    "player_season_obv_pass_90" = 0.14,
    "player_season_obv_dribble_carry_90" = 0.14,
    "player_season_xgchain_90" = 0.14,
    "player_season_op_xgchain_90" = 0.14,
    "player_season_transition_obv_90" = 0.10
  ),
  "Seguridad" = c(
    "player_season_turnovers_90" = 1.0
  )
)
DELANTERO_CAPACITIES <- c(DELANTERO_CAPACITIES, ALMADA_IDENTITY_CAPACITIES)

# ---- 1f. Volante/Extremo -- unfrozen 2026-10-09 ------------------------
# Weights/metrics as finalized in capacity_weights_review.html after the
# user's colleague discussion -- obv_pass_90 lives in Creación (not
# Seguridad), and Finalización uses shot-quality signals (obv_shot_90,
# np_psxg_90) instead of touches_inside_box_90 (box presence, not shot
# quality -- user's own correction, 2026-10-09).
VOLANTE_CAPACITIES <- list(
  "Conducción/Progresión" = c(
    "player_season_obv_dribble_carry_90" = 0.25,
    "player_season_dribbles_90" = 0.15,
    "player_season_dribble_ratio" = 0.10,
    "player_season_deep_progressions_90" = 0.20,
    "player_season_carries_90" = 0.15,
    "player_season_transition_obv_90" = 0.15
  ),
  "Presión alta" = c(
    "player_season_counterpressure_regains_90" = 0.30,
    "player_season_padj_pressures_90" = 0.25,
    "player_season_fhalf_pressures_90" = 0.25,
    "player_season_average_x_pressure" = 0.20
  ),
  "Creación" = c(
    "player_season_op_xa_90" = 0.25,
    "player_season_op_key_passes_90" = 0.20,
    "player_season_op_passes_into_box_90" = 0.20,
    "player_season_crosses_90" = 0.12,
    "player_season_box_cross_ratio" = 0.08,
    "player_season_obv_pass_90" = 0.15
  ),
  "Finalización" = c(
    "player_season_np_xg_90" = 0.35,
    "player_season_npg_90" = 0.30,
    "player_season_obv_shot_90" = 0.20,
    "player_season_np_psxg_90" = 0.15
  ),
  "Seguridad" = c(
    "player_season_turnovers_90" = 1.0
  )
)
VOLANTE_CAPACITIES <- c(VOLANTE_CAPACITIES, ALMADA_IDENTITY_CAPACITIES)

# ---- 1g. Delantero perfiles -- unfrozen 2026-10-09 ----------------------
# Reuses app.R's existing DC_WEIGHTED_PROFILES (5 archetypes already
# curated for the "Perfil"/"Perfil_secundario" system) for PERFIL
# CLASSIFICATION only (unchanged, ported verbatim below as
# DC_WEIGHTED_PROFILES) -- deciding which of the 5 archetypes a given
# Delantero row fits best. The 5 capacity-based scoring definitions below
# are a SEPARATE thing: within-capacity metric weights are EQUAL (same
# precedent as PROFILE_METRIC_DEFS' own Portero block in app.R,
# .equal_weights()) rather than invented per-metric judgment calls, since
# DC_WEIGHTED_PROFILES' own weights don't cover every one of the 5
# capacity buckets for every perfil (e.g. Cazador has zero press/
# associative metrics in its original definition -- it's a pure
# finishing classifier, not a 5-capacity scoring model). Capacity-level
# weights (which of the 5 capacities matters how much per perfil) come
# from the user's own Perfiles Delanteros.pdf, 2026-10-07 -- NOT
# equal-weighted, those are real football judgment calls already made.
.equal_weights <- function(metrics) {
  w <- rep(1 / length(metrics), length(metrics))
  names(w) <- metrics
  w
}

DC_WEIGHTED_PROFILES <- list(
  "Cazador" = c(
    "player_season_npg_90" = 0.24, "player_season_np_xg_90" = 0.2,
    "player_season_np_xg_per_shot" = 0.12, "player_season_shot_on_target_ratio" = 0.1,
    "player_season_np_shots_90" = 0.1, "player_season_touches_inside_box_90" = 0.12,
    "player_season_np_psxg_90" = 0.07, "player_season_over_under_performance_90" = 0.05
  ),
  "Móvil" = c(
    "player_season_deep_progressions_90" = 0.12, "player_season_obv_dribble_carry_90" = 0.12,
    "player_season_carries_90" = 0.1, "player_season_op_key_passes_90" = 0.12,
    "player_season_op_xa_90" = 0.12, "player_season_obv_pass_90" = 0.1,
    "player_season_xgchain_90" = 0.12, "player_season_touches_inside_box_90" = 0.08,
    "player_season_fouls_won_90" = 0.05, "player_season_counterpressure_regains_90" = 0.07
  ),
  "Retenedor" = c(
    "player_season_aerial_ratio" = 0.18, "player_season_aerial_wins_90" = 0.12,
    "player_season_obv_pass_90" = 0.12, "player_season_passing_ratio" = 0.1,
    "player_season_op_key_passes_90" = 0.1, "player_season_op_xa_90" = 0.1,
    "player_season_xgchain_90" = 0.1, "player_season_dispossessions_90" = 0.08,
    "player_season_turnovers_90" = 0.05, "player_season_touches_inside_box_90" = 0.05
  ),
  "Aéreo" = c(
    "player_season_aerial_ratio" = 0.3, "player_season_aerial_wins_90" = 0.18,
    "player_season_npg_90" = 0.14, "player_season_np_xg_90" = 0.12,
    "player_season_np_shots_90" = 0.08, "player_season_touches_inside_box_90" = 0.08,
    "player_season_np_xg_per_shot" = 0.05, "player_season_np_psxg_90" = 0.05
  ),
  "Acosador" = c(
    "player_season_padj_pressures_90" = 0.18, "player_season_fhalf_pressures_90" = 0.16,
    "player_season_counterpressures_90" = 0.14, "player_season_fhalf_counterpressures_90" = 0.12,
    "player_season_pressure_regains_90" = 0.14, "player_season_counterpressure_regains_90" = 0.12,
    "player_season_aggressive_actions_90" = 0.08, "player_season_fhalf_pressures_ratio" = 0.06
  )
)

DELANTERO_PERFIL_CAPACITIES <- list(
  "Cazador" = list(
    "Finalización" = .equal_weights(c("player_season_npg_90", "player_season_np_xg_90",
      "player_season_np_xg_per_shot", "player_season_shot_on_target_ratio",
      "player_season_np_shots_90", "player_season_np_psxg_90", "player_season_over_under_performance_90")),
    "Juego aéreo/área" = .equal_weights(c("player_season_touches_inside_box_90", "player_season_aerial_ratio")),
    "Presión alta" = .equal_weights(c("player_season_fhalf_pressures_90", "player_season_counterpressure_regains_90",
      "player_season_padj_pressures_90", "player_season_aggressive_actions_90", "player_season_average_x_pressure")),
    "Juego asociativo" = .equal_weights(c("player_season_op_xa_90", "player_season_op_key_passes_90",
      "player_season_obv_pass_90", "player_season_obv_dribble_carry_90", "player_season_xgchain_90",
      "player_season_op_xgchain_90", "player_season_transition_obv_90")),
    "Seguridad" = c("player_season_turnovers_90" = 1.0)
  ),
  "Móvil" = list(
    "Finalización" = .equal_weights(c("player_season_npg_90", "player_season_np_xg_90",
      "player_season_np_xg_per_shot", "player_season_shot_on_target_ratio", "player_season_np_shots_90")),
    "Juego aéreo/área" = .equal_weights(c("player_season_touches_inside_box_90", "player_season_aerial_ratio")),
    "Presión alta" = .equal_weights(c("player_season_counterpressure_regains_90", "player_season_fhalf_pressures_90",
      "player_season_padj_pressures_90", "player_season_aggressive_actions_90", "player_season_average_x_pressure")),
    "Juego asociativo" = .equal_weights(c("player_season_deep_progressions_90", "player_season_obv_dribble_carry_90",
      "player_season_carries_90", "player_season_op_key_passes_90", "player_season_op_xa_90",
      "player_season_obv_pass_90", "player_season_xgchain_90")),
    "Seguridad" = c("player_season_turnovers_90" = 1.0)
  ),
  "Retenedor" = list(
    "Finalización" = .equal_weights(c("player_season_npg_90", "player_season_np_xg_90",
      "player_season_np_xg_per_shot", "player_season_shot_on_target_ratio", "player_season_np_shots_90")),
    "Juego aéreo/área" = .equal_weights(c("player_season_aerial_ratio", "player_season_aerial_wins_90", "player_season_touches_inside_box_90")),
    "Presión alta" = .equal_weights(c("player_season_fhalf_pressures_90", "player_season_counterpressure_regains_90",
      "player_season_padj_pressures_90", "player_season_aggressive_actions_90", "player_season_average_x_pressure")),
    "Juego asociativo" = .equal_weights(c("player_season_obv_pass_90", "player_season_op_key_passes_90",
      "player_season_op_xa_90", "player_season_xgchain_90")),
    "Seguridad" = .equal_weights(c("player_season_passing_ratio", "player_season_dispossessions_90", "player_season_turnovers_90"))
  ),
  "Aéreo" = list(
    "Finalización" = .equal_weights(c("player_season_npg_90", "player_season_np_xg_90",
      "player_season_np_shots_90", "player_season_np_xg_per_shot", "player_season_np_psxg_90")),
    "Juego aéreo/área" = .equal_weights(c("player_season_aerial_ratio", "player_season_aerial_wins_90", "player_season_touches_inside_box_90")),
    "Presión alta" = .equal_weights(c("player_season_fhalf_pressures_90", "player_season_counterpressure_regains_90",
      "player_season_padj_pressures_90", "player_season_aggressive_actions_90", "player_season_average_x_pressure")),
    "Juego asociativo" = .equal_weights(c("player_season_op_xa_90", "player_season_op_key_passes_90",
      "player_season_obv_pass_90", "player_season_obv_dribble_carry_90", "player_season_xgchain_90",
      "player_season_op_xgchain_90", "player_season_transition_obv_90")),
    "Seguridad" = c("player_season_turnovers_90" = 1.0)
  ),
  "Acosador" = list(
    "Finalización" = .equal_weights(c("player_season_npg_90", "player_season_np_xg_90",
      "player_season_np_xg_per_shot", "player_season_shot_on_target_ratio", "player_season_np_shots_90")),
    "Juego aéreo/área" = .equal_weights(c("player_season_touches_inside_box_90", "player_season_aerial_ratio")),
    "Presión alta" = .equal_weights(c("player_season_padj_pressures_90", "player_season_fhalf_pressures_90",
      "player_season_counterpressures_90", "player_season_fhalf_counterpressures_90", "player_season_pressure_regains_90",
      "player_season_counterpressure_regains_90", "player_season_aggressive_actions_90", "player_season_fhalf_pressures_ratio")),
    "Juego asociativo" = .equal_weights(c("player_season_op_xa_90", "player_season_op_key_passes_90",
      "player_season_obv_pass_90", "player_season_obv_dribble_carry_90", "player_season_xgchain_90",
      "player_season_op_xgchain_90", "player_season_transition_obv_90")),
    "Seguridad" = c("player_season_turnovers_90" = 1.0)
  )
)
DELANTERO_PERFIL_CAPACITIES <- lapply(DELANTERO_PERFIL_CAPACITIES, function(caps) c(caps, ALMADA_IDENTITY_CAPACITIES))

# ---- 1h. Master role -> capacities map, for iteration downstream -------
# Mediapunta deliberately absent as a key here -- it reuses
# INTERIOR_CAPACITIES verbatim (see base_scores.R).
ROLE_CAPACITIES <- list(
  "Interior" = INTERIOR_CAPACITIES,
  "Central" = CENTRAL_CAPACITIES,
  "Lateral/Carrilero" = LATERAL_CAPACITIES,
  "Medio de Contención" = MC_CAPACITIES,
  "Delantero" = DELANTERO_CAPACITIES,
  "Volante/Extremo" = VOLANTE_CAPACITIES
)

MIN_SUBSCORE_COVERAGE <- 0.60  # same threshold app.R's MIN_PROFILE_COVERAGE uses

# ---- 2. Generic capacity-combination function ------------------------
# normalized_pct: data.frame/matrix, one column per metric in `weights`,
#                 values already 0-100 percentiles (normalize_metric() output)
# weights:        named numeric vector, names = column names in normalized_pct
# Returns list(score = 0-100 vector, coverage = 0-1 vector) -- score is NA
# wherever coverage < MIN_SUBSCORE_COVERAGE.
capacity_subscore <- function(normalized_pct, weights, min_coverage = MIN_SUBSCORE_COVERAGE) {
  cols <- names(weights)
  pct_mat <- as.matrix(normalized_pct[, cols, drop = FALSE])
  w <- unname(weights[cols])

  have <- !is.na(pct_mat)
  covered_weight <- as.numeric(have %*% w)
  weighted_sum <- as.numeric(ifelse(have, pct_mat, 0) %*% w)

  score <- weighted_sum / covered_weight
  coverage <- covered_weight / sum(w)
  score[coverage < min_coverage] <- NA_real_

  list(score = score, coverage = coverage)
}

# ============================================================
# Validation harness -- only runs when executed directly.
# ============================================================
if (sys.nframe() == 0) {

  # ---- 3. Load + join raw metrics and role classification (shared loader,
  # see load_scout_data.R -- this exact block used to be duplicated here
  # and in normalization.R). ----
  source("load_scout_data.R")
  dat <- load_scout_data()
  message(sprintf("%d Interior rows", sum(dat$role_group_matchbased == "Interior")))

  # ---- 4/5. For each role: normalize its metrics, combine into capacity
  # subscores. Mediapunta reuses INTERIOR_CAPACITIES verbatim -- added
  # here under its own key purely for this validation pass (its player
  # pool is "Mediapunta", not "Interior"). ----
  all_results <- list()
  for (role in c(names(ROLE_CAPACITIES), "Mediapunta")) {
    caps_def <- if (role == "Mediapunta") INTERIOR_CAPACITIES else ROLE_CAPACITIES[[role]]
    metrics <- unique(unlist(lapply(caps_def, names)))
    metrics <- intersect(metrics, names(dat))
    missing <- setdiff(unique(unlist(lapply(caps_def, names))), metrics)
    if (length(missing)) message(sprintf("WARNING [%s] -- missing columns, not normalized: %s", role, paste(missing, collapse = ", ")))

    role_idx <- which(dat$role_group_matchbased == role)
    normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
    for (m in metrics) {
      normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
    }

    result <- normalized |> dplyr::select(var_name, player_id, player_name)
    for (cap in names(caps_def)) {
      res <- capacity_subscore(normalized, caps_def[[cap]])
      result[[cap]] <- round(res$score, 1)
      result[[paste0(cap, "__cobertura")]] <- round(res$coverage * 100, 1)
    }
    result$role_group_matchbased <- role
    all_results[[role]] <- result

    message(sprintf("\n=== %s: coverage summary per capacity (n=%d) ===", role, nrow(result)))
    for (cap in names(caps_def)) {
      v <- result[[cap]]
      message(sprintf("%-35s n_valid=%d/%d (%.1f%%)  range=[%.1f, %.1f]  mean=%.1f",
                       cap, sum(!is.na(v)), length(v), 100 * mean(!is.na(v)),
                       min(v, na.rm = TRUE), max(v, na.rm = TRUE), mean(v, na.rm = TRUE)))
    }
  }

  out_path <- "data/capacity_subscores_by_role.rds"
  saveRDS(all_results, out_path)
  message(sprintf("\nWrote %s (%d roles: %s)", out_path, length(all_results), paste(names(all_results), collapse = ", ")))

  message("\n=== Spot check: top 10 Interior players by Progresión ===")
  print(
    all_results[["Interior"]] |>
      dplyr::filter(!is.na(Progresión)) |>
      dplyr::arrange(dplyr::desc(Progresión)) |>
      dplyr::select(player_name, Progresión, `Presión/contrapresión`, `Ball Efficiency`, `Impacto ofensivo`) |>
      head(10)
  )
}
