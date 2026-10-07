# ============================================================
# confidence_shrinkage.R
#
# Final step of the DataScore pipeline (doc S12.3 step 4): shrinks
# DataScore_shown (the gate's output -- see league_transition_gate.R)
# toward the role mean when the sample behind it is small, so a 300-
# minute flash-in-the-pan doesn't carry the same certainty as a
# 2,500-minute season (doc S20's own list of errors to avoid).
#
#   DataScore final = Media_rol + C x (DataScore_shown - Media_rol)
#
# C is "una confianza entre 0 y 1 basada en minutos, cobertura y
# exposición" (doc S12.3) -- the exact function is explicitly "por
# calibrar" (S12.7/S12.8), so this implements a placeholder in the SAME
# empirical-Bayes shrinkage shape already used twice elsewhere in this
# pipeline (normalize_metric()'s ratio-metric shrinkage, and
# mathematically this whole step IS that same formula one level up --
# see the derivation in the comment above C_FROM_MINUTES_K below):
#
#   C = C_minutes x C_coverage
#   C_minutes  = exposure_90s / (exposure_90s + K)      (K = pseudo-count)
#   C_coverage = source_coverage / 100                   (capacity-weight
#                                                          coverage, from
#                                                          combine_capacities())
#
# Uses the SOURCE row's minutes/coverage (from league_transition_gate.R's
# extended output), not the player's current-league minutes -- a gated
# player's shown score still rests on their old league's sample, so that's
# what confidence should reflect.
#
# Standalone script, not wired into app.R yet -- same convention as every
# other file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("league_transition_gate.R")  # apply_transition_gate(),
                                     # build_scored_rows(), everything
                                     # upstream (via its own source() chain)

# Placeholder pseudo-count, "por calibrar" like RATIO_SHRINKAGE_K_90S in
# normalization.R -- same 10 (≈900 minutes) of weight given to the role
# mean before a player's own sample starts to dominate it. Mathematically,
# this whole step is identical to that same shrinkage formula:
#   Media + C x (shown - Media) = (shown x exposure + Media x K) / (exposure + K)
#   when C = exposure / (exposure + K) -- same empirical-Bayes shape,
# just applied to the whole score instead of one ratio metric.
CONFIDENCE_SHRINKAGE_K_90S <- 10

# ---- Core function ------------------------------------------------------
# gated:       apply_transition_gate() output (one row per player)
# role_means:  named numeric vector, names = role_group_matchbased,
#              values = that role's mean DataScore_previo (global pool,
#              same scope normalize_metric() pools within)
# Returns gated, with C, Media_rol and DataScore_final columns added.
apply_confidence_shrinkage <- function(gated, role_means, k_90s = CONFIDENCE_SHRINKAGE_K_90S) {
  media_rol <- unname(role_means[gated$source_role])
  exposure_90s <- gated$source_minutes / 90

  c_minutes <- exposure_90s / (exposure_90s + k_90s)
  c_coverage <- gated$source_coverage / 100
  c <- c_minutes * c_coverage

  gated |>
    dplyr::mutate(
      Media_rol = round(media_rol, 1),
      C = round(c, 3),
      DataScore_final = round(media_rol + c * (DataScore_shown - media_rol), 1)
    )
}

# ============================================================
# Validation harness -- only runs when executed directly.
# ============================================================
if (sys.nframe() == 0) {

  tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
  dat <- load_scout_data()

  rows <- build_scored_rows(dat, tiers)
  gated <- apply_transition_gate(rows)

  # Media_rol per role -- global pool mean of DataScore_previo, same
  # scope normalize_metric()'s percentile pools already use.
  role_means <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::group_by(role_group_matchbased) |>
    dplyr::summarise(mean_ds = mean(DataScore_previo), .groups = "drop") |>
    tibble::deframe()
  message("=== Media_rol per role ===")
  print(round(role_means, 1))

  result <- apply_confidence_shrinkage(gated, role_means)

  out_path <- "data/datascore_final.rds"
  saveRDS(result, out_path)
  message(sprintf("\nWrote %s (%d players)", out_path, nrow(result)))

  valid <- !is.na(result$DataScore_final)
  message(sprintf(
    "\n=== DataScore_final: n_valid=%d/%d  range=[%.1f,%.1f]  mean=%.1f ===",
    sum(valid), nrow(result), min(result$DataScore_final, na.rm = TRUE),
    max(result$DataScore_final, na.rm = TRUE), mean(result$DataScore_final, na.rm = TRUE)
  ))

  # Shrinkage should barely touch big-sample players and pull small-sample
  # players hard toward their role mean -- same spot-check shape used to
  # validate the ratio-metric shrinkage in normalization.R.
  message("\n=== Spot check: LOWEST-minute players (should shrink HARD toward Media_rol) ===")
  print(
    result |>
      dplyr::filter(!is.na(DataScore_final)) |>
      dplyr::arrange(source_minutes) |>
      dplyr::select(player_name, source_role, source_minutes, DataScore_shown, Media_rol, C, DataScore_final) |>
      head(10)
  )

  message("\n=== Spot check: HIGHEST-minute players (DataScore_final should stay close to DataScore_shown) ===")
  print(
    result |>
      dplyr::filter(!is.na(DataScore_final)) |>
      dplyr::arrange(dplyr::desc(source_minutes)) |>
      dplyr::select(player_name, source_role, source_minutes, DataScore_shown, Media_rol, C, DataScore_final) |>
      head(10)
  )

  message("\n=== Top 10 by DataScore_final ===")
  print(
    result |>
      dplyr::filter(!is.na(DataScore_final)) |>
      dplyr::arrange(dplyr::desc(DataScore_final)) |>
      dplyr::select(player_name, source_role, source_minutes, C, DataScore_shown, DataScore_final) |>
      head(10)
  )
}
