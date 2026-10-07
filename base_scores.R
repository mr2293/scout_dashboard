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
# everything else at this stage.
#
# Every role's AmeScore weights include the 2 AmeScore-only capacities
# (Presión posicional, Conexión ofensiva -- see capacity_subscores.R)
# built specifically to give AmeScore distinguishing content instead of
# just reweighted DataScore inputs. With these + each role's original
# (non-widened) weights, cor(DataScore_Base, AmeScore_Base) drops
# consistently (~0.04-0.07) on every role tested -- real improvement,
# still high in absolute terms; see correlation_analysis.html for the
# full investigation (including why widening weight deltas alone barely
# helps, matching rather than beating the old DataScore/DataScoreAmerica's
# ~0.77).
#
# Implemented for Interior, Central, Lateral/Carrilero, Medio de
# Contención, Mediapunta (reuses INTERIOR_CAPACITIES, own weights) and
# Delantero (generic only -- the 5 DC_WEIGHTED_PROFILES perfiles from
# app.R have their own weight proposal in capacity_weights_review.html
# but aren't wired into code yet). Volante/Extremo deliberately excluded
# -- frozen, per the user's explicit instruction 2026-10-07.
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

# The original 6 capacities (ratios preserved, 25/15/10/15/20/15) scaled
# to 70% to make room for the 2 new AmeScore-only capacities at 15% each
# -- NOT the widened-delta version (user explicitly chose to keep the
# original weights, 2026-10-07, after the widened version pushed Impacto
# ofensivo down to an indefensible 4%). DataScore never sees these two
# capacities at all -- INTERIOR_DATASCORE_WEIGHTS above has no entry for
# them, so combine_capacities() simply never includes them for DataScore.
INTERIOR_AMESCORE_WEIGHTS <- c(
  "Progresión" = 0.18,
  "Presión/contrapresión" = 0.10,
  "Juego bajo presión" = 0.07,
  "Ball Efficiency" = 0.10,
  "Dinamismo/influencia entre fases" = 0.14,
  "Impacto ofensivo" = 0.11,
  "Presión posicional" = 0.15,
  "Conexión ofensiva" = 0.15
)

# Mediapunta reuses INTERIOR_CAPACITIES verbatim -- same 8 capacities,
# own weight profile (less press/positional-identity emphasis than
# Interior, more Impacto ofensivo -- a 10 vs an 8, doc S5/S6's "separar
# 6/8/10" rule).
MEDIAPUNTA_DATASCORE_WEIGHTS <- c(
  "Progresión" = 0.20,
  "Presión/contrapresión" = 0.10,
  "Juego bajo presión" = 0.10,
  "Ball Efficiency" = 0.15,
  "Dinamismo/influencia entre fases" = 0.15,
  "Impacto ofensivo" = 0.30
)
MEDIAPUNTA_AMESCORE_WEIGHTS <- c(
  "Progresión" = 0.11,
  "Presión/contrapresión" = 0.10,
  "Juego bajo presión" = 0.11,
  "Ball Efficiency" = 0.07,
  "Dinamismo/influencia entre fases" = 0.14,
  "Impacto ofensivo" = 0.17,
  "Presión posicional" = 0.15,
  "Conexión ofensiva" = 0.15
)

CENTRAL_DATASCORE_WEIGHTS <- c(
  "Defensa/Duelos" = 0.30,
  "Posicionamiento/Presión" = 0.20,
  "Progresión con balón" = 0.30,
  "Distribución/Seguridad" = 0.20
)
CENTRAL_AMESCORE_WEIGHTS <- c(
  "Defensa/Duelos" = 0.21,
  "Posicionamiento/Presión" = 0.25,
  "Progresión con balón" = 0.14,
  "Distribución/Seguridad" = 0.10,
  "Presión posicional" = 0.15,
  "Conexión ofensiva" = 0.15
)

LATERAL_DATASCORE_WEIGHTS <- c(
  "Presión/Recuperación" = 0.25,
  "Progresión/Conducción" = 0.30,
  "Creación/Centros" = 0.30,
  "Seguridad" = 0.15
)
LATERAL_AMESCORE_WEIGHTS <- c(
  "Presión/Recuperación" = 0.25,
  "Progresión/Conducción" = 0.18,
  "Creación/Centros" = 0.17,
  "Seguridad" = 0.10,
  "Presión posicional" = 0.15,
  "Conexión ofensiva" = 0.15
)

MC_DATASCORE_WEIGHTS <- c(
  "Presión/Recuperación" = 0.35,
  "Circulación/Progresión" = 0.32,
  "Juego bajo presión" = 0.10,
  "Seguridad/Distribución" = 0.23
)
MC_AMESCORE_WEIGHTS <- c(
  "Presión/Recuperación" = 0.30,
  "Circulación/Progresión" = 0.23,
  "Juego bajo presión" = 0.04,
  "Seguridad/Distribución" = 0.13,
  "Presión posicional" = 0.15,
  "Conexión ofensiva" = 0.15
)

DELANTERO_DATASCORE_WEIGHTS <- c(
  "Finalización" = 0.40,
  "Juego aéreo/área" = 0.19,
  "Presión alta" = 0.17,
  "Juego asociativo" = 0.22,
  "Seguridad" = 0.02
)
DELANTERO_AMESCORE_WEIGHTS <- c(
  "Finalización" = 0.22,
  "Juego aéreo/área" = 0.11,
  "Presión alta" = 0.20,
  "Juego asociativo" = 0.14,
  "Seguridad" = 0.03,
  "Presión posicional" = 0.15,
  "Conexión ofensiva" = 0.15
)

# Unfrozen 2026-10-09 -- user confirmed the capacity_weights_review.html
# proposal as final after the colleague discussion.
VOLANTE_DATASCORE_WEIGHTS <- c(
  "Conducción/Progresión" = 0.30,
  "Presión alta" = 0.10,
  "Creación" = 0.25,
  "Finalización" = 0.25,
  "Seguridad" = 0.10
)
VOLANTE_AMESCORE_WEIGHTS <- c(
  "Conducción/Progresión" = 0.21,
  "Presión alta" = 0.18,
  "Creación" = 0.14,
  "Finalización" = 0.14,
  "Seguridad" = 0.03,
  "Presión posicional" = 0.15,
  "Conexión ofensiva" = 0.15
)

# Delantero perfil capacity weights -- straight from the user's own
# Perfiles Delanteros.pdf, 2026-10-07. Unlike every other *_WEIGHTS pair
# above, these are football judgment calls the user supplied directly,
# not placeholders derived by this pipeline.
DELANTERO_PERFIL_WEIGHTS <- list(
  "Cazador" = list(
    ds = c("Finalización" = 0.30, "Juego aéreo/área" = 0.20, "Presión alta" = 0.25, "Juego asociativo" = 0.15, "Seguridad" = 0.10),
    ame = c("Finalización" = 0.35, "Juego aéreo/área" = 0.20, "Presión alta" = 0.30, "Juego asociativo" = 0.10, "Seguridad" = 0.05)
  ),
  "Móvil" = list(
    ds = c("Finalización" = 0.25, "Juego aéreo/área" = 0.15, "Presión alta" = 0.20, "Juego asociativo" = 0.30, "Seguridad" = 0.10),
    ame = c("Finalización" = 0.25, "Juego aéreo/área" = 0.15, "Presión alta" = 0.25, "Juego asociativo" = 0.25, "Seguridad" = 0.10)
  ),
  "Retenedor" = list(
    ds = c("Finalización" = 0.20, "Juego aéreo/área" = 0.20, "Presión alta" = 0.20, "Juego asociativo" = 0.25, "Seguridad" = 0.15),
    ame = c("Finalización" = 0.20, "Juego aéreo/área" = 0.15, "Presión alta" = 0.25, "Juego asociativo" = 0.25, "Seguridad" = 0.15)
  ),
  "Aéreo" = list(
    ds = c("Finalización" = 0.25, "Juego aéreo/área" = 0.30, "Presión alta" = 0.15, "Juego asociativo" = 0.15, "Seguridad" = 0.15),
    ame = c("Finalización" = 0.25, "Juego aéreo/área" = 0.35, "Presión alta" = 0.20, "Juego asociativo" = 0.10, "Seguridad" = 0.10)
  ),
  "Acosador" = list(
    ds = c("Finalización" = 0.30, "Juego aéreo/área" = 0.15, "Presión alta" = 0.30, "Juego asociativo" = 0.15, "Seguridad" = 0.10),
    ame = c("Finalización" = 0.30, "Juego aéreo/área" = 0.15, "Presión alta" = 0.35, "Juego asociativo" = 0.15, "Seguridad" = 0.05)
  )
)

# ---- Master role -> {capacities, ds weights, ame weights} map ----------
ROLE_SCORE_DEFS <- list(
  "Interior" = list(caps = INTERIOR_CAPACITIES, ds = INTERIOR_DATASCORE_WEIGHTS, ame = INTERIOR_AMESCORE_WEIGHTS),
  "Mediapunta" = list(caps = INTERIOR_CAPACITIES, ds = MEDIAPUNTA_DATASCORE_WEIGHTS, ame = MEDIAPUNTA_AMESCORE_WEIGHTS),
  "Central" = list(caps = CENTRAL_CAPACITIES, ds = CENTRAL_DATASCORE_WEIGHTS, ame = CENTRAL_AMESCORE_WEIGHTS),
  "Lateral/Carrilero" = list(caps = LATERAL_CAPACITIES, ds = LATERAL_DATASCORE_WEIGHTS, ame = LATERAL_AMESCORE_WEIGHTS),
  "Medio de Contención" = list(caps = MC_CAPACITIES, ds = MC_DATASCORE_WEIGHTS, ame = MC_AMESCORE_WEIGHTS),
  "Delantero" = list(caps = DELANTERO_CAPACITIES, ds = DELANTERO_DATASCORE_WEIGHTS, ame = DELANTERO_AMESCORE_WEIGHTS),
  "Volante/Extremo" = list(caps = VOLANTE_CAPACITIES, ds = VOLANTE_DATASCORE_WEIGHTS, ame = VOLANTE_AMESCORE_WEIGHTS)
)

# ---- Delantero perfiles: role -> {perfil -> {capacities, ds, ame}} -----
# Separate from ROLE_SCORE_DEFS because perfil scoring is TWO-stage
# (classify which perfil first, then score with THAT perfil's weights)
# rather than a straight lookup by role -- see the two functions below.
DELANTERO_PERFIL_DEFS <- setNames(
  lapply(names(DELANTERO_PERFIL_WEIGHTS), function(p) {
    list(caps = DELANTERO_PERFIL_CAPACITIES[[p]], ds = DELANTERO_PERFIL_WEIGHTS[[p]]$ds, ame = DELANTERO_PERFIL_WEIGHTS[[p]]$ame)
  }),
  names(DELANTERO_PERFIL_WEIGHTS)
)

# ---- Delantero perfil classification + scoring --------------------------
# Moved here (from delantero_perfiles.R) 2026-10-09 so build_scored_rows()
# (competition_strength.R, downstream of this file) can call them directly
# when it hits role == "Delantero" -- the whole point of wiring perfiles
# into the FULL pipeline (Competition Strength, the transition gate,
# confidence shrinkage, the AmeScore gate) rather than leaving them as a
# standalone DataScore_Base/AmeScore_Base-only validation.
#
# classify_delantero_perfil(): which of the 5 archetypes does this player
# fit best? Ports app.R's own assign_player_profiles() logic: normalize
# DC_WEIGHTED_PROFILES' metrics within the Delantero pool, combine per
# perfil (capacity_subscore() treating the whole weight vector as one
# composite, min_coverage=0 so a thin-data player still gets a best-fit
# guess rather than no classification at all), argmax.
classify_delantero_perfil <- function(dat, role_idx) {
  metrics <- unique(unlist(lapply(DC_WEIGHTED_PROFILES, names)))
  metrics <- intersect(metrics, names(dat))

  normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
  for (m in metrics) {
    normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
  }

  perfil_scores <- sapply(names(DC_WEIGHTED_PROFILES), function(p) {
    capacity_subscore(normalized, DC_WEIGHTED_PROFILES[[p]], min_coverage = 0)$score
  })
  colnames(perfil_scores) <- names(DC_WEIGHTED_PROFILES)

  apply(perfil_scores, 1, function(row) {
    if (all(is.na(row))) return(NA_character_)
    names(row)[which.max(row)]
  })
}

# score_delantero_by_perfil(): DataScore_Base/AmeScore_Base using each
# player's OWN classified perfil's capacities/weights. Output is
# explicitly REORDERED back to match role_idx's original row order before
# returning -- processing happens perfil-group-by-perfil-group internally
# (bind_rows() of 5 subsets), which would otherwise scramble row order
# and silently break any positional alignment a caller does against
# dat[role_idx, ] (e.g. build_scored_rows() attaching season_id/league).
score_delantero_by_perfil <- function(dat, role_idx, perfil) {
  all_metrics <- unique(unlist(lapply(DELANTERO_PERFIL_DEFS, function(d) unlist(lapply(d$caps, names)))))
  all_metrics <- intersect(all_metrics, names(dat))

  normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
  for (m in all_metrics) {
    normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
  }
  normalized$perfil <- perfil
  normalized$.orig_pos <- seq_len(nrow(normalized))

  result <- vector("list", length(DELANTERO_PERFIL_DEFS))
  names(result) <- names(DELANTERO_PERFIL_DEFS)

  for (p in names(DELANTERO_PERFIL_DEFS)) {
    idx <- which(normalized$perfil == p)
    if (!length(idx)) next
    sub <- normalized[idx, ]
    def <- DELANTERO_PERFIL_DEFS[[p]]

    caps <- sub |> dplyr::select(var_name, player_id, player_name, .orig_pos)
    for (cap in names(def$caps)) caps[[cap]] <- capacity_subscore(sub, def$caps[[cap]])$score

    ds <- combine_capacities(caps, def$ds)
    ame <- combine_capacities(caps, def$ame)

    result[[p]] <- caps |>
      dplyr::mutate(
        perfil = p,
        DataScore_Base = round(ds$score, 1),
        DataScore_Base_cobertura = round(ds$coverage * 100, 1),
        AmeScore_Base = round(ame$score, 1),
        AmeScore_Base_cobertura = round(ame$coverage * 100, 1)
      )
  }
  dplyr::bind_rows(result) |> dplyr::arrange(.orig_pos) |> dplyr::select(-.orig_pos)
}

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
  all_results <- list()

  for (role in names(ROLE_SCORE_DEFS)) {
    def <- ROLE_SCORE_DEFS[[role]]
    metrics <- unique(unlist(lapply(def$caps, names)))
    metrics <- intersect(metrics, names(dat))

    role_idx <- which(dat$role_group_matchbased == role)
    normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
    for (m in metrics) {
      normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
    }

    capacities <- normalized |> dplyr::select(var_name, player_id, player_name)
    for (cap in names(def$caps)) {
      res <- capacity_subscore(normalized, def$caps[[cap]])
      capacities[[cap]] <- res$score
    }

    ds <- combine_capacities(capacities, def$ds)
    ame <- combine_capacities(capacities, def$ame)

    result <- capacities |>
      dplyr::mutate(
        role_group_matchbased = role,
        DataScore_Base = round(ds$score, 1),
        DataScore_Base_cobertura = round(ds$coverage * 100, 1),
        AmeScore_Base = round(ame$score, 1),
        AmeScore_Base_cobertura = round(ame$coverage * 100, 1)
      )
    all_results[[role]] <- result

    valid_both <- !is.na(result$DataScore_Base) & !is.na(result$AmeScore_Base)
    message(sprintf(
      "%-20s n=%-5d DataScore[%.1f,%.1f] AmeScore[%.1f,%.1f]  cor=%.3f",
      role, sum(valid_both),
      min(result$DataScore_Base, na.rm = TRUE), max(result$DataScore_Base, na.rm = TRUE),
      min(result$AmeScore_Base, na.rm = TRUE), max(result$AmeScore_Base, na.rm = TRUE),
      cor(result$DataScore_Base[valid_both], result$AmeScore_Base[valid_both])
    ))
  }

  out_path <- "data/base_scores_by_role.rds"
  saveRDS(all_results, out_path)
  message(sprintf("\nWrote %s (%d roles: %s)", out_path, length(all_results), paste(names(all_results), collapse = ", ")))

  # ---- Spot checks on Interior (same cases shown in earlier validation
  # passes, to confirm this refactor didn't change anything) ----
  interior <- all_results[["Interior"]]
  message("\n=== Interior -- top 10 por DataScore_Base ===")
  print(interior |> dplyr::filter(!is.na(DataScore_Base)) |> dplyr::arrange(dplyr::desc(DataScore_Base)) |>
          dplyr::select(player_name, DataScore_Base, AmeScore_Base) |> head(10))

  message("\n=== Interior -- casos donde los dos scores DIVERGEN más ===")
  print(
    interior |>
      dplyr::filter(!is.na(DataScore_Base) & !is.na(AmeScore_Base)) |>
      dplyr::mutate(gap = DataScore_Base - AmeScore_Base) |>
      dplyr::arrange(dplyr::desc(abs(gap))) |>
      dplyr::select(player_name, DataScore_Base, AmeScore_Base, gap) |>
      head(10)
  )
}
