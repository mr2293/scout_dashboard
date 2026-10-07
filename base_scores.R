# ============================================================
# base_scores.R
#
# Combines capacity subscores into DataScore Base and AmeScore Base (doc
# S12.3 step 1 / S12.4 -- Score Base = Sigma(peso_capacidad x
# subscore_capacidad)). This stops at the BASE score -- Competition
# Strength (DataScore only), the league-transition gate, confidence
# shrinkage C, and AmeScore's structural gate/techo are all separate,
# later pipeline steps per the agreed sequencing, not built here.
#
# Weights below are the current artifact proposal (capacity_weights_review
# html, published 2026-10-07) -- STILL PENDING confirmation with the
# user's team, same "pesos ilustrativos, por calibrar" framing as
# everything else at this stage. Only Interior has validated capacity
# definitions in code so far (capacity_subscores.R) -- base combination is
# only wired up for Interior here; the other roles stay artifact-only
# until their capacities are built the same way.
#
# Standalone script, not wired into app.R yet -- same convention as every
# other file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("capacity_subscores.R")  # capacity_subscore(), INTERIOR_CAPACITIES,
                                 # normalize_metric() (via its own source())
source("load_scout_data.R")     # load_scout_data() -- capacity_subscores.R
                                 # only sources this inside its own
                                 # sys.nframe()==0 guard, so it's not
                                 # picked up by sourcing that file alone.

MIN_BASE_COVERAGE <- 0.60  # same threshold as capacity_subscore()'s own gate

# ---- DataScore / AmeScore capacity weights -- Interior only ----------
# Both sum to 1.00. Source: capacity_weights_review.html, as edited
# 2026-10-07 (Interior's own section) -- NOT yet confirmed with the
# user's team.
INTERIOR_DATASCORE_WEIGHTS <- c(
  "Progresión" = 0.25,
  "Presión/contrapresión" = 0.10,
  "Juego bajo presión" = 0.10,
  "Ball Efficiency" = 0.20,
  "Dinamismo/influencia entre fases" = 0.15,
  "Impacto ofensivo" = 0.20
)

INTERIOR_AMESCORE_WEIGHTS <- c(
  "Progresión" = 0.25,
  "Presión/contrapresión" = 0.15,
  "Juego bajo presión" = 0.10,
  "Ball Efficiency" = 0.15,
  "Dinamismo/influencia entre fases" = 0.20,
  "Impacto ofensivo" = 0.15
)

# ---- Generic capacity-combination function ----------------------------
# capacity_scores: data.frame/matrix, one column per capacity in `weights`,
#                   values already 0-100 (capacity_subscore() output)
# weights:          named numeric vector, names = capacity names
# Same coverage-gated weighted-average shape as capacity_subscore() itself
# -- a capacity missing for a player (NA, e.g. it fell below ITS OWN
# min-coverage gate) has its weight redistributed among the player's other
# available capacities; the combined score is withheld (NA) if too much
# weight is missing.
combine_capacities <- function(capacity_scores, weights, min_coverage = MIN_BASE_COVERAGE) {
  caps <- names(weights)
  score_mat <- as.matrix(capacity_scores[, caps, drop = FALSE])
  w <- unname(weights[caps])

  have <- !is.na(score_mat)
  covered_weight <- as.numeric(have %*% w)
  weighted_sum <- as.numeric(ifelse(have, score_mat, 0) %*% w)

  score <- weighted_sum / covered_weight
  coverage <- covered_weight / sum(w)
  score[coverage < min_coverage] <- NA_real_

  list(score = score, coverage = coverage)
}

# ============================================================
# Validation harness -- only runs when executed directly.
# ============================================================
if (sys.nframe() == 0) {

  dat <- load_scout_data()

  # ---- Normalize + build Interior's 6 capacity subscores (same as
  # capacity_subscores.R's own harness) ----
  interior_metrics <- unique(unlist(lapply(INTERIOR_CAPACITIES, names)))
  interior_metrics <- intersect(interior_metrics, names(dat))

  normalized <- dat |> dplyr::select(var_name, player_id, player_name, role_group_matchbased)
  for (m in interior_metrics) {
    normalized[[m]] <- normalize_metric(dat[[m]], dat$role_group_matchbased, dat$exposure_90s, m)
  }

  interior_idx <- which(dat$role_group_matchbased == "Interior")
  interior_normalized <- normalized[interior_idx, ]

  capacities <- interior_normalized |> dplyr::select(var_name, player_id, player_name)
  for (cap in names(INTERIOR_CAPACITIES)) {
    res <- capacity_subscore(interior_normalized, INTERIOR_CAPACITIES[[cap]])
    capacities[[cap]] <- res$score
  }

  # ---- Combine into DataScore Base and AmeScore Base ----
  ds <- combine_capacities(capacities, INTERIOR_DATASCORE_WEIGHTS)
  ame <- combine_capacities(capacities, INTERIOR_AMESCORE_WEIGHTS)

  result <- capacities |>
    dplyr::mutate(
      DataScore_Base = round(ds$score, 1),
      DataScore_Base_cobertura = round(ds$coverage * 100, 1),
      AmeScore_Base = round(ame$score, 1),
      AmeScore_Base_cobertura = round(ame$coverage * 100, 1)
    )

  out_path <- "data/interior_base_scores.rds"
  saveRDS(result, out_path)
  message(sprintf("Wrote %s (%d Interior player-seasons)", out_path, nrow(result)))

  # ---- Sanity checks ----
  message("\n=== DataScore Base vs AmeScore Base ===")
  cat(sprintf(
    "DataScore_Base:  n_valid=%d/%d  range=[%.1f, %.1f]  mean=%.1f\n",
    sum(!is.na(result$DataScore_Base)), nrow(result),
    min(result$DataScore_Base, na.rm = TRUE), max(result$DataScore_Base, na.rm = TRUE),
    mean(result$DataScore_Base, na.rm = TRUE)
  ))
  cat(sprintf(
    "AmeScore_Base:   n_valid=%d/%d  range=[%.1f, %.1f]  mean=%.1f\n",
    sum(!is.na(result$AmeScore_Base)), nrow(result),
    min(result$AmeScore_Base, na.rm = TRUE), max(result$AmeScore_Base, na.rm = TRUE),
    mean(result$AmeScore_Base, na.rm = TRUE)
  ))

  # The whole point of separating these: they should NOT be near-identical
  # for the same players (doc S1/S5 -- today's DataScore/DataScoreAmerica
  # correlate ~0.77 precisely because they're built from the same metrics,
  # just reweighted. Different capacity weights per score is the fix).
  valid_both <- !is.na(result$DataScore_Base) & !is.na(result$AmeScore_Base)
  message(sprintf(
    "\n=== Correlación DataScore_Base x AmeScore_Base (n=%d) ===\ncor = %.3f (la vieja DataScore/DataScoreAmerica correlacionaba ~0.77 -- esto debería ser bastante más bajo)",
    sum(valid_both), cor(result$DataScore_Base[valid_both], result$AmeScore_Base[valid_both])
  ))

  message("\n=== Top 10 por DataScore_Base ===")
  print(result |> dplyr::filter(!is.na(DataScore_Base)) |> dplyr::arrange(dplyr::desc(DataScore_Base)) |>
          dplyr::select(player_name, DataScore_Base, AmeScore_Base) |> head(10))

  message("\n=== Top 10 por AmeScore_Base ===")
  print(result |> dplyr::filter(!is.na(AmeScore_Base)) |> dplyr::arrange(dplyr::desc(AmeScore_Base)) |>
          dplyr::select(player_name, DataScore_Base, AmeScore_Base) |> head(10))

  message("\n=== Casos donde los dos scores DIVERGEN más (|DataScore_Base - AmeScore_Base| alto) ===")
  print(
    result |>
      dplyr::filter(valid_both <- !is.na(DataScore_Base) & !is.na(AmeScore_Base)) |>
      dplyr::mutate(gap = DataScore_Base - AmeScore_Base) |>
      dplyr::arrange(dplyr::desc(abs(gap))) |>
      dplyr::select(player_name, DataScore_Base, AmeScore_Base, gap) |>
      head(10)
  )
}
