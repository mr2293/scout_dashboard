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
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
}))

source("normalization.R")  # normalize_metric() -- sys.nframe() guard means
                            # this does NOT re-run normalization.R's own
                            # validation harness, just defines the function.

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

  # ---- 4. Normalize every metric Interior's 6 capacities need ----
  interior_metrics <- unique(unlist(lapply(INTERIOR_CAPACITIES, names)))
  interior_metrics <- intersect(interior_metrics, names(dat))
  missing <- setdiff(unique(unlist(lapply(INTERIOR_CAPACITIES, names))), interior_metrics)
  if (length(missing)) message("WARNING -- missing columns, not normalized: ", paste(missing, collapse = ", "))

  normalized <- dat |> dplyr::select(var_name, player_id, player_name, role_group_matchbased)
  for (m in interior_metrics) {
    normalized[[m]] <- normalize_metric(dat[[m]], dat$role_group_matchbased, dat$exposure_90s, m)
  }

  # ---- 5. Combine into Interior's 6 capacity subscores ----
  interior_idx <- which(dat$role_group_matchbased == "Interior")
  interior_normalized <- normalized[interior_idx, ]

  result <- interior_normalized |> dplyr::select(var_name, player_id, player_name)
  for (cap in names(INTERIOR_CAPACITIES)) {
    res <- capacity_subscore(interior_normalized, INTERIOR_CAPACITIES[[cap]])
    result[[cap]] <- round(res$score, 1)
    result[[paste0(cap, "__cobertura")]] <- round(res$coverage * 100, 1)
  }

  out_path <- "data/interior_capacity_subscores.rds"
  saveRDS(result, out_path)
  message(sprintf("Wrote %s (%d Interior player-seasons, %d capacities)", out_path, nrow(result), length(INTERIOR_CAPACITIES)))

  # ---- 6. Sanity checks ----
  message("\n=== Coverage summary per capacity ===")
  for (cap in names(INTERIOR_CAPACITIES)) {
    v <- result[[cap]]
    message(sprintf("%-35s n_valid=%d/%d (%.1f%%)  range=[%.1f, %.1f]  mean=%.1f",
                     cap, sum(!is.na(v)), length(v), 100 * mean(!is.na(v)),
                     min(v, na.rm = TRUE), max(v, na.rm = TRUE), mean(v, na.rm = TRUE)))
  }

  message("\n=== Spot check: top 10 Interior players by Progresión ===")
  print(
    result |>
      dplyr::filter(!is.na(Progresión)) |>
      dplyr::arrange(dplyr::desc(Progresión)) |>
      dplyr::select(player_name, Progresión, `Presión/contrapresión`, `Ball Efficiency`, `Impacto ofensivo`) |>
      head(10)
  )
}
