# ============================================================
# delantero_perfiles.R
#
# Wires in the 5 Delantero perfiles (Cazador, Móvil, Retenedor, Aéreo,
# Acosador) so a 9 gets scored against the archetype they actually fit,
# instead of one generic Delantero model -- the thing the user explicitly
# asked for when the capacity_weights_review.html Perfiles section was
# first built ("so we don't make a one-size-fits-all for delanteros").
#
# Two-stage process, not a simple capacity lookup by role:
#   1. CLASSIFY -- which of the 5 perfiles does this player fit best?
#      Reuses app.R's own DC_WEIGHTED_PROFILES (ported to
#      capacity_subscores.R) exactly like assign_player_profiles() does
#      there: normalize each perfil's metrics within the Delantero pool,
#      combine with that perfil's weights (capacity_subscore() treating
#      the whole weight vector as one composite), take the argmax as the
#      player's primary perfil.
#   2. SCORE -- once classified, compute DataScore_Base/AmeScore_Base
#      using THAT perfil's own capacities/weights (DELANTERO_PERFIL_DEFS),
#      not the generic DELANTERO_CAPACITIES.
#
# Normalization stays scoped to the full "Delantero" role pool
# (role_group_matchbased), NOT a per-perfil sub-pool -- comparing a
# Cazador's np_xg_90 against every delantero, not just other Cazadores,
# consistent with every other role's reference-pool scope in this
# pipeline, and avoids fragmenting an already smaller sample into 5
# even-smaller pools.
#
# Standalone script, not wired into app.R yet -- same convention as
# every other file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("ame_score_plus.R")  # everything upstream (via its own source() chain)

# ---- Stage 1: classification ---------------------------------------------
# dat, role_idx: the "Delantero" subset of load_scout_data()'s output and
#                its row indices (same pattern every other script uses).
# Returns a character vector, same length as role_idx, naming each row's
# best-fit perfil.
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

# ---- Stage 2: score with the classified perfil's own weights -------------
# Returns a data.frame of capacities + DataScore_Base/AmeScore_Base for
# the Delantero role, one row per player, SOURCED FROM each player's own
# classified perfil (not a single shared model).
score_delantero_by_perfil <- function(dat, role_idx, perfil) {
  all_metrics <- unique(unlist(lapply(DELANTERO_PERFIL_DEFS, function(d) unlist(lapply(d$caps, names)))))
  all_metrics <- intersect(all_metrics, names(dat))

  normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
  for (m in all_metrics) {
    normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
  }
  normalized$perfil <- perfil

  result <- vector("list", length(DELANTERO_PERFIL_DEFS))
  names(result) <- names(DELANTERO_PERFIL_DEFS)

  for (p in names(DELANTERO_PERFIL_DEFS)) {
    idx <- which(normalized$perfil == p)
    if (!length(idx)) next
    sub <- normalized[idx, ]
    def <- DELANTERO_PERFIL_DEFS[[p]]

    caps <- sub |> dplyr::select(var_name, player_id, player_name)
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
  dplyr::bind_rows(result)
}

# ============================================================
# Validation harness -- only runs when executed directly.
# ============================================================
if (sys.nframe() == 0) {

  dat <- load_scout_data()
  role_idx <- which(dat$role_group_matchbased == "Delantero")

  perfil <- classify_delantero_perfil(dat, role_idx)
  message("=== Perfil distribution (primary) ===")
  print(table(perfil, useNA = "ifany"))

  result <- score_delantero_by_perfil(dat, role_idx, perfil)
  out_path <- "data/delantero_perfil_scores.rds"
  saveRDS(result, out_path)
  message(sprintf("\nWrote %s (%d rows)", out_path, nrow(result)))

  # ---- Compare to the generic Delantero model (same players) ----
  generic_def <- ROLE_SCORE_DEFS[["Delantero"]]
  generic_metrics <- unique(unlist(lapply(generic_def$caps, names)))
  generic_metrics <- intersect(generic_metrics, names(dat))
  normalized_generic <- dat[role_idx, c("var_name", "player_id", "player_name")]
  for (m in generic_metrics) {
    normalized_generic[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
  }
  caps_generic <- normalized_generic |> dplyr::select(var_name, player_id, player_name)
  for (cap in names(generic_def$caps)) caps_generic[[cap]] <- capacity_subscore(normalized_generic, generic_def$caps[[cap]])$score
  ds_generic <- combine_capacities(caps_generic, generic_def$ds)
  ame_generic <- combine_capacities(caps_generic, generic_def$ame)

  comp <- result |>
    dplyr::select(var_name, player_id, player_name, perfil, DataScore_Base, AmeScore_Base) |>
    dplyr::rename(DataScore_perfil = DataScore_Base, AmeScore_perfil = AmeScore_Base) |>
    dplyr::mutate(
      DataScore_generico = round(ds_generic$score, 1)[match(paste(var_name, player_id), paste(dat$var_name[role_idx], dat$player_id[role_idx]))],
      AmeScore_generico = round(ame_generic$score, 1)[match(paste(var_name, player_id), paste(dat$var_name[role_idx], dat$player_id[role_idx]))]
    ) |>
    dplyr::mutate(delta_ds = DataScore_perfil - DataScore_generico, delta_ame = AmeScore_perfil - AmeScore_generico)

  message("\n=== Where perfil-based scoring diverges most from the generic Delantero model (AmeScore) ===")
  print(
    comp |>
      dplyr::filter(!is.na(delta_ame)) |>
      dplyr::arrange(dplyr::desc(abs(delta_ame))) |>
      dplyr::select(player_name, perfil, AmeScore_generico, AmeScore_perfil, delta_ame) |>
      head(10)
  )

  message("\n=== Spot check by perfil: top 3 AmeScore_perfil each ===")
  print(
    comp |>
      dplyr::filter(!is.na(AmeScore_perfil)) |>
      dplyr::group_by(perfil) |>
      dplyr::arrange(dplyr::desc(AmeScore_perfil), .by_group = TRUE) |>
      dplyr::slice_head(n = 3) |>
      dplyr::ungroup() |>
      dplyr::select(perfil, player_name, AmeScore_perfil, AmeScore_generico)
  )
}
