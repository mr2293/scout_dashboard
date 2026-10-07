# ============================================================
# ame_score_plus.R
#
# AmeScore+ (doc S10, S12.5): AmeScore enriched with SkillCorner physical
# data, but ONLY within the specific capacities where physical genuinely
# explains that capacity -- "AmeScore+ no debe ser un 80% AmeScore + 20%
# físico universal" (S10). Per capacity enriched:
#
#   Subscore_k+ = (1 - rho_k) x Subscore_eventos + rho_k x Subscore_físico
#   AmeScore+ Base = Sigma(peso_capacidad x subscore_capacidad+)
#   AmeScore+ final = mínimo(AmeScore+ Base, Techo estructural)
#
# Subscore_físico is built with the SAME capacity_subscore() machinery
# already used for Subscore_eventos -- it's just a weighted composite of
# normalized physical metrics instead of normalized event metrics, so no
# new combination logic is needed, just a new metric set per capacity.
#
# PHYSICAL_ENRICHMENT below maps ROLE -> {capacity -> {rho, metrics}}.
# Brainstormed with the user 2026-10-08:
#   - Central -> Defensa/Duelos (rho=0.35): doc's own table says PSV99
#     should weigh MORE for Central ("defensa de profundidad... línea
#     alta") than for 8/10.
#   - Lateral/Carrilero -> Presión/Recuperación (rho=0.30): "repetición
#     de incorporaciones, retorno y defensa de banda."
#   - Interior -> Dinamismo (rho=0.40) + Presión/contrapresión (rho=0.25):
#     doc's OWN worked numerical example (S12.5's table), used verbatim.
#   - Mediapunta -> Presión/contrapresión ONLY (rho=0.20, no PSV99): doc
#     explicitly says PSV99 should weigh LESS for 10 than for 8, and
#     describes a 10's physical need as "activación de presión y rupturas
#     cortas" -- shorter, more selective, not sustained running.
#   - Medio de Contención -> Presión/Recuperación (rho=0.30): the doc's
#     S10 table has NO entry at all for the 6 -- this is an EXTRAPOLATION
#     from Interior's profile (user's explicit choice when asked), not
#     sourced from the doc. Flagged here as weaker-grounded than the rest.
#   - Volante/Extremo -> Conducción/Progresión (rho=0.35): added 2026-10-09
#     after the role was unfrozen -- doc's S10 table is explicit here
#     ("PSV99, sprint, HSR, aceleraciones -> amenaza al espacio").
#   - Delantero: handled SEPARATELY, below (DELANTERO_PERFIL_PHYSICAL_
#     ENRICHMENT) -- doc explicitly says "9: depende del perfil... no
#     imponer un único físico de delantero," so this is PERFIL -> capacity,
#     not ROLE -> capacity, now that the 5 perfiles are wired in
#     (delantero_perfiles.R).
#
# A capacity NOT listed for a role keeps rho=0 implicitly -- its
# Subscore_k+ is just Subscore_eventos, unchanged.
#
# Per doc S10.2 ("la ausencia de datos físicos NUNCA se rellena con
# cero"): AmeScore+_published is FALSE (AmeScore+_final left NA) for a
# player unless at least one of their role's enriched capacities had a
# real (non-NA) physical subscore -- otherwise AmeScore+ would just
# silently equal AmeScore, which the doc says should never be shown as
# if it were a real physical enrichment.
#
# Standalone script, not wired into app.R yet -- same convention as
# every other file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("ame_score_gate.R")  # apply_ame_score_gate(), GATE_CAPACITY,
                             # GATE_THRESHOLD, build_scored_rows(), and
                             # everything upstream (via its own source() chain)

PHYSICAL_ENRICHMENT <- list(
  "Central" = list(
    "Defensa/Duelos" = list(rho = 0.35, metrics = c(
      "psv99" = 0.30, "highaccel_count_per_90" = 0.25,
      "hsr_distance_per_90" = 0.25, "highdecel_count_per_90" = 0.20
    ))
  ),
  "Lateral/Carrilero" = list(
    "Presión/Recuperación" = list(rho = 0.30, metrics = c(
      "hsr_distance_per_90" = 0.25, "sprint_distance_per_90" = 0.25,
      "highaccel_count_per_90" = 0.20, "highdecel_count_per_90" = 0.20,
      "psv99" = 0.10
    ))
  ),
  "Interior" = list(
    "Dinamismo/influencia entre fases" = list(rho = 0.40, metrics = c(
      "hi_distance_per_90" = 0.30, "hsr_distance_per_90" = 0.25,
      "highaccel_count_per_90" = 0.15, "highdecel_count_per_90" = 0.15,
      "total_distance_per_90" = 0.10, "psv99" = 0.05
    )),
    "Presión/contrapresión" = list(rho = 0.25, metrics = c(
      "highaccel_count_per_90" = 0.40, "hsr_distance_per_90" = 0.35,
      "highdecel_count_per_90" = 0.25
    ))
  ),
  "Mediapunta" = list(
    "Presión/contrapresión" = list(rho = 0.20, metrics = c(
      "highaccel_count_per_90" = 0.50, "hsr_distance_per_90" = 0.50
    ))
  ),
  # Extrapolated from Interior -- see file header.
  "Medio de Contención" = list(
    "Presión/Recuperación" = list(rho = 0.30, metrics = c(
      "hi_distance_per_90" = 0.30, "hsr_distance_per_90" = 0.25,
      "highaccel_count_per_90" = 0.20, "highdecel_count_per_90" = 0.15,
      "total_distance_per_90" = 0.10
    ))
  ),
  # Added 2026-10-09, after Volante/Extremo was unfrozen. Doc's S10 table
  # IS explicit for this role: "Extremo: PSV99, sprint, HSR,
  # aceleraciones -> Amenaza al espacio y repetición de alta velocidad" --
  # maps directly onto Conducción/Progresión (the capacity built from
  # carrying/running-at-pace metrics), with PSV99 weighted heavily since
  # the doc explicitly says it "debe pesar más" for Central/Lateral/
  # Extremo (vs. less for 8/10).
  "Volante/Extremo" = list(
    "Conducción/Progresión" = list(rho = 0.35, metrics = c(
      "psv99" = 0.30, "sprint_distance_per_90" = 0.30,
      "hsr_distance_per_90" = 0.25, "highaccel_count_per_90" = 0.15
    ))
  )
)

# ---- Delantero perfil physical enrichment --------------------------------
# Doc S10 deliberately withholds a single physical model for Delantero
# ("9: depende del perfil... no imponer un único físico de delantero") --
# so instead of ROLE -> capacity, this is PERFIL -> capacity. Judgment
# calls (not doc-sourced, since the doc intentionally leaves this open):
#   - Cazador (poacher): light enrichment on Finalización -- explosive
#     short bursts IN the box matter some, but a poacher's skill is
#     finishing technique/positioning more than raw pace (rho=0.15, lowest
#     of the 5).
#   - Móvil (false-9/dynamic): heavier enrichment on Juego asociativo --
#     covering ground to combine across the pitch is central to the
#     archetype (rho=0.30, matches Interior's Dinamismo-style logic for a
#     similarly dynamic profile).
#   - Retenedor (target/hold-up): light enrichment on Juego aéreo/área --
#     target men win position through timing/strength more than sprint
#     speed (rho=0.15).
#   - Aéreo (aerial focal point): light enrichment on Juego aéreo/área,
#     same reasoning as Retenedor -- aerial duels are about jump
#     timing, not PSV99 (rho=0.15).
#   - Acosador (presser): heaviest enrichment, on Presión alta -- doc's
#     general framing elsewhere (Central/Lateral/Interior) consistently
#     gives real running output a bigger role wherever pressing intensity
#     is the defining trait, and that's Acosador's entire identity
#     (rho=0.35, matches Central/Volante's rho).
DELANTERO_PERFIL_PHYSICAL_ENRICHMENT <- list(
  "Cazador" = list(
    "Finalización" = list(rho = 0.15, metrics = c(
      "highaccel_count_per_90" = 0.6, "psv99" = 0.4
    ))
  ),
  "Móvil" = list(
    "Juego asociativo" = list(rho = 0.30, metrics = c(
      "hi_distance_per_90" = 0.4, "total_distance_per_90" = 0.3, "hsr_distance_per_90" = 0.3
    ))
  ),
  "Retenedor" = list(
    "Juego aéreo/área" = list(rho = 0.15, metrics = c(
      "psv99" = 0.5, "highaccel_count_per_90" = 0.5
    ))
  ),
  "Aéreo" = list(
    "Juego aéreo/área" = list(rho = 0.15, metrics = c(
      "highaccel_count_per_90" = 0.6, "psv99" = 0.4
    ))
  ),
  "Acosador" = list(
    "Presión alta" = list(rho = 0.35, metrics = c(
      "highaccel_count_per_90" = 0.35, "hsr_distance_per_90" = 0.35, "highdecel_count_per_90" = 0.30
    ))
  )
)

# ---- Core function ------------------------------------------------------
# normalized_events: the SAME normalized (event-metric) data.frame
#                     build_scored_rows()/capacity_subscore() already use
#                     -- one row per player, columns = event metric names.
# dat:                raw joined data (for the physical metric columns,
#                      scoped to the same role_idx rows as normalized_events).
# role:               single role name (function processes one role at a time)
# capacities_events:   data.frame of this role's Subscore_eventos per
#                       capacity (capacity_subscore() output, one column
#                       per capacity, same row order as normalized_events)
# Returns capacities_events with each enriched capacity's column REPLACED
# by Subscore_k+ (blended where physical is available, unchanged
# Subscore_eventos otherwise), plus a `physical_available` logical column.
apply_physical_enrichment <- function(role, dat, role_idx, exposure_90s, capacities_events, enrichment_map = PHYSICAL_ENRICHMENT) {
  enrich <- enrichment_map[[role]]
  if (is.null(enrich)) {
    capacities_events$physical_available <- FALSE
    return(capacities_events)
  }

  physical_available <- rep(FALSE, nrow(capacities_events))

  for (cap in names(enrich)) {
    rho <- enrich[[cap]]$rho
    metric_weights <- enrich[[cap]]$metrics
    phys_metrics <- names(metric_weights)
    phys_metrics <- intersect(phys_metrics, names(dat))

    normalized_phys <- capacities_events |> dplyr::select(var_name, player_id, player_name)
    for (m in phys_metrics) {
      normalized_phys[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], exposure_90s, m)
    }
    subscore_fisico <- capacity_subscore(normalized_phys, metric_weights[phys_metrics])$score

    has_physical <- !is.na(subscore_fisico)
    physical_available <- physical_available | has_physical

    subscore_eventos <- capacities_events[[cap]]
    subscore_plus <- ifelse(
      has_physical,
      (1 - rho) * subscore_eventos + rho * subscore_fisico,
      subscore_eventos
    )
    capacities_events[[cap]] <- round(subscore_plus, 1)
  }

  capacities_events$physical_available <- physical_available
  capacities_events
}

# ============================================================
# Validation harness -- only runs when executed directly.
# ============================================================
if (sys.nframe() == 0) {

  tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
  dat <- load_scout_data()

  enriched_roles <- names(PHYSICAL_ENRICHMENT)
  all_results <- list()

  for (role in enriched_roles) {
    def <- ROLE_SCORE_DEFS[[role]]
    metrics <- unique(unlist(lapply(def$caps, names)))
    metrics <- intersect(metrics, names(dat))

    role_idx <- which(dat$role_group_matchbased == role)
    normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
    for (m in metrics) {
      normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
    }

    capacities <- normalized |> dplyr::select(var_name, player_id, player_name)
    for (cap in names(def$caps)) capacities[[cap]] <- capacity_subscore(normalized, def$caps[[cap]])$score

    capacities_plus <- apply_physical_enrichment(role, dat, role_idx, dat$exposure_90s[role_idx], capacities)

    ame_plus <- combine_capacities(capacities_plus, def$ame)
    techo_rol <- mean(combine_capacities(capacities, def$ame)$score, na.rm = TRUE)
    gate <- apply_ame_score_gate(ame_plus$score, capacities_plus[[GATE_CAPACITY]], rep(role, nrow(capacities_plus)), c(setNames(techo_rol, role)))

    result <- capacities_plus |>
      dplyr::select(var_name, player_id, player_name, physical_available) |>
      dplyr::mutate(
        role_group_matchbased = role,
        AmeScorePlus_Base = round(ame_plus$score, 1),
        AmeScorePlus_final = ifelse(physical_available, gate$AmeScore_final, NA_real_),
        AmeScorePlus_published = physical_available & !is.na(gate$AmeScore_final)
      )
    all_results[[role]] <- result

    message(sprintf(
      "%-20s n=%-5d physical_available=%d (%.1f%%)  published=%d (%.1f%%)",
      role, nrow(result), sum(result$physical_available), 100 * mean(result$physical_available),
      sum(result$AmeScorePlus_published), 100 * mean(result$AmeScorePlus_published)
    ))
  }

  # ---- Delantero: per-perfil physical enrichment ----
  # Same mechanics as the role loop above, but keyed by perfil instead of
  # role -- classify first, score each perfil-subset with its OWN
  # DELANTERO_PERFIL_DEFS capacities, enrich with that perfil's own
  # physical mapping, gate against that perfil's own AmeScore mean.
  role_idx <- which(dat$role_group_matchbased == "Delantero")
  perfil <- classify_delantero_perfil(dat, role_idx)

  all_metrics <- unique(unlist(lapply(DELANTERO_PERFIL_DEFS, function(d) unlist(lapply(d$caps, names)))))
  all_metrics <- intersect(all_metrics, names(dat))
  normalized_del <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
  for (m in all_metrics) normalized_del[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
  normalized_del$perfil <- perfil
  normalized_del$.orig_pos <- seq_len(nrow(normalized_del))

  delantero_results <- list()
  for (p in names(DELANTERO_PERFIL_DEFS)) {
    idx <- which(normalized_del$perfil == p)
    if (!length(idx)) next
    sub <- normalized_del[idx, ]
    def <- DELANTERO_PERFIL_DEFS[[p]]
    sub_role_idx <- role_idx[idx]

    capacities <- sub |> dplyr::select(var_name, player_id, player_name, .orig_pos)
    for (cap in names(def$caps)) capacities[[cap]] <- capacity_subscore(sub, def$caps[[cap]])$score

    capacities_plus <- apply_physical_enrichment(p, dat, sub_role_idx, dat$exposure_90s[sub_role_idx], capacities, DELANTERO_PERFIL_PHYSICAL_ENRICHMENT)

    ame_plus <- combine_capacities(capacities_plus, def$ame)
    techo_perfil <- mean(combine_capacities(capacities, def$ame)$score, na.rm = TRUE)
    gate <- apply_ame_score_gate(ame_plus$score, capacities_plus[[GATE_CAPACITY]], rep(p, nrow(capacities_plus)), c(setNames(techo_perfil, p)))

    delantero_results[[p]] <- capacities_plus |>
      dplyr::select(var_name, player_id, player_name, physical_available) |>
      dplyr::mutate(
        role_group_matchbased = "Delantero",
        perfil = p,
        AmeScorePlus_Base = round(ame_plus$score, 1),
        AmeScorePlus_final = ifelse(physical_available, gate$AmeScore_final, NA_real_),
        AmeScorePlus_published = physical_available & !is.na(gate$AmeScore_final)
      )
  }
  delantero_results <- dplyr::bind_rows(delantero_results)
  all_results[["Delantero"]] <- delantero_results

  for (p in names(DELANTERO_PERFIL_DEFS)) {
    r <- delantero_results |> dplyr::filter(perfil == p)
    message(sprintf(
      "Delantero/%-10s n=%-5d physical_available=%d (%.1f%%)  published=%d (%.1f%%)",
      p, nrow(r), sum(r$physical_available), 100 * mean(r$physical_available),
      sum(r$AmeScorePlus_published), 100 * mean(r$AmeScorePlus_published)
    ))
  }

  all_results <- dplyr::bind_rows(all_results)
  out_path <- "data/amescore_plus_by_role.rds"
  saveRDS(all_results, out_path)
  message(sprintf("\nWrote %s (%d rows, %d roles + 5 Delantero perfiles)", out_path, nrow(all_results), length(enriched_roles)))

  message("\n=== Interior: AmeScore vs AmeScore+ for published players (physical actually changed something) ===")
  interior_ame <- build_scored_rows(dat, tiers) |> dplyr::filter(role_group_matchbased == "Interior") |>
    dplyr::distinct(var_name, player_id, .keep_all = TRUE) |>
    dplyr::select(var_name, player_id, AmeScore_Base)
  comp <- all_results |>
    dplyr::filter(role_group_matchbased == "Interior", AmeScorePlus_published) |>
    dplyr::distinct(var_name, player_id, .keep_all = TRUE) |>
    dplyr::left_join(interior_ame, by = c("var_name", "player_id")) |>
    dplyr::mutate(delta = AmeScorePlus_Base - AmeScore_Base) |>
    dplyr::arrange(dplyr::desc(abs(delta)))
  print(comp |> dplyr::select(player_name, AmeScore_Base, AmeScorePlus_Base, delta) |> head(10))
}
