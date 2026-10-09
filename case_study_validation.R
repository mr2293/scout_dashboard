# ============================================================
# case_study_validation.R
#
# Re-runs the doc's own 3 named case studies (S14.2.1/S15, written
# against early exploratory piloting of the OLD architecture) through
# the now-COMPLETE v2 pipeline, to check whether what the doc flagged as
# a problem is actually fixed -- not to reproduce the doc's exact
# numbers (it explicitly says those exploratory figures "NO representan
# una fórmula final ni un ranking definitivo").
#
# Three cases, three different questions:
#   - Oliver Larraz (S14.2.1): doc's MAIN explicit, falsifiable
#     prediction -- "la calibración de Competition Strength y la escala
#     final debe conseguir que 90+ signifique nivel extraordinario a
#     escala global. Un rendimiento P90/P95 dentro de MLS no debe
#     convertirse automáticamente en DataScore 90+." Doc's exploratory
#     DataScore was 91.0 in a raw/unadjusted exercise. Pass/fail test:
#     is v2's final (Competition-Strength-adjusted, confidence-shrunk,
#     0-100-scaled) DataScore meaningfully below 90?
#   - Nicolás Castro (S14.1): doc's read was qualitative -- "se mantuvo
#     en la zona alta sin necesidad de forzarlo" (stayed near the top of
#     the Liga MX Interior/8 pool without the model being tuned to put
#     him there). Check: does he still land in the upper tier of his
#     role's AmeScore distribution?
#   - Gonçalo Franco (S15): doc's read was about CAPACITY-LEVEL shape,
#     not a single number -- extreme Presión (~100) with much lower
#     Progresión/Ball Efficiency/Impacto ofensivo (~36-41), and
#     AmeScore+ should lift him over AmeScore once physical is added
#     (illustrative example: ~48.0 -> ~55.4 AmeScore+ in the doc's OWN
#     exploratory piloting, NOT a value to reproduce exactly). Check:
#     does the capacity breakdown still show that same shape, and does
#     AmeScore+ > AmeScore for him?
#
# Standalone script, not wired into app.R -- same convention as every
# other analysis file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("datascore_v2.R")   # full DataScore/AmeScore pipeline + scale_mapping.R
source("ame_score_plus.R") # apply_physical_enrichment(), PHYSICAL_ENRICHMENT, etc.
                            # (already sourced transitively by datascore_v2.R,
                            # explicit here since this script calls its
                            # functions directly, not just through the pipeline)

CASE_STUDY_PLAYERS <- c("Nicolás Castro", "Oliver Larraz", "Gonçalo Franco")

# ---- Final DataScore/AmeScore (the actual live pipeline output) --------
tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
dat <- load_scout_data()
rows <- build_scored_rows(dat, tiers)

final_scores <- get_datascore_v2_scores(force_refresh = TRUE)
name_lookup <- dat |> dplyr::distinct(player_id, player_name)
final_scores <- final_scores |> dplyr::left_join(name_lookup, by = "player_id")

# A case-study player can have MULTIPLE scored rows (one per season/
# league they appear in across the raw data) -- the gate resolves which
# ONE actually backs their live DataScore/AmeScore (source_var_name,
# recovered here the same way datascore_v2.R does it internally). Every
# capacity-level lookup below must use THIS exact row, not just any row
# matching the player's name, or a stale/uncovered season could get
# compared instead of the one actually shown in the app.
gated <- apply_transition_gate(rows)
source_lookup <- gated |> dplyr::select(player_name, source_var_name)

cases <- final_scores |> dplyr::filter(player_name %in% CASE_STUDY_PLAYERS) |>
  dplyr::left_join(source_lookup, by = "player_name")
message("=== Final v2 DataScore/AmeScore (live pipeline, scaled 0-100) ===")
print(cases |> dplyr::select(player_name, DataScore, DataScoreAmerica, role_group_matchbased_v2, SourceLeague, SourceSeason))

# ---- AmeScore+ for each case's role (recomputed fresh, same mechanics ---
# as ame_score_plus.R's own validation harness) --------------------------
amescore_plus_for_role <- function(role) {
  def <- ROLE_SCORE_DEFS[[role]]
  metrics <- unique(unlist(lapply(def$caps, names)))
  metrics <- intersect(metrics, names(dat))

  role_idx <- which(dat$role_group_matchbased == role)
  normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
  for (m in metrics) normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)

  capacities <- normalized |> dplyr::select(var_name, player_id, player_name)
  for (cap in names(def$caps)) capacities[[cap]] <- capacity_subscore(normalized, def$caps[[cap]])$score

  capacities_plus <- apply_physical_enrichment(role, dat, role_idx, dat$exposure_90s[role_idx], capacities)
  ame_plus <- combine_capacities(capacities_plus, def$ame)
  techo_rol <- mean(combine_capacities(capacities, def$ame)$score, na.rm = TRUE)
  gate <- apply_ame_score_gate(ame_plus$score, capacities_plus[[GATE_CAPACITY]], rep(role, nrow(capacities_plus)), c(setNames(techo_rol, role)))

  capacities |>
    dplyr::mutate(
      AmeScore_Base = combine_capacities(capacities, def$ame)$score,
      physical_available = capacities_plus$physical_available,
      AmeScorePlus_Base = round(ame_plus$score, 1),
      AmeScorePlus_final = ifelse(physical_available, gate$AmeScore_final, NA_real_)
    )
}

roles_needed <- cases$role_group_matchbased_v2 |> unique() |> stats::na.omit()
message("\n=== AmeScore vs AmeScore+ (physical enrichment), capacity breakdown ===")
role_results <- list()
for (role in roles_needed) {
  if (is.null(PHYSICAL_ENRICHMENT[[role]])) {
    message(sprintf("(%s has no physical-enrichment mapping -- AmeScore+ not applicable)", role))
    next
  }
  res <- amescore_plus_for_role(role)
  role_results[[role]] <- res
  # Only the ONE row the gate actually resolved as current for this
  # player (see source_var_name above) -- not every row matching their
  # name.
  sub <- res |> dplyr::semi_join(
    cases |> dplyr::filter(role_group_matchbased_v2 == role),
    by = c("var_name" = "source_var_name")
  )
  if (!nrow(sub)) next
  print(sub |> dplyr::select(player_name, dplyr::any_of(names(ROLE_SCORE_DEFS[[role]]$caps)), AmeScore_Base, AmeScorePlus_Base))
}

# ---- Case-by-case verdicts -----------------------------------------------
message("\n=== Verdicts ===")

larraz <- cases |> dplyr::filter(player_name == "Oliver Larraz")
if (nrow(larraz)) {
  ds <- larraz$DataScore[1]
  message(sprintf(
    "Oliver Larraz: v2 DataScore = %.1f (doc's exploratory flagged figure was 91.0). %s",
    ds,
    if (ds < 90) "PASS -- no longer reads as a 90+ 'extraordinary' score; Competition Strength + confidence shrinkage + the fixed 0-100 scale appear to have corrected the MLS-specific inflation the doc flagged."
    else "STILL HIGH -- the doc's concern may not be fully resolved; worth checking Larraz's specific CompetitionStrength/confidence C values directly."
  ))
} else {
  message("Oliver Larraz: no scored row found (missing role implementation or data coverage) -- cannot verify.")
}

castro <- cases |> dplyr::filter(player_name == "Nicolás Castro")
if (nrow(castro) && !is.na(castro$role_group_matchbased_v2[1])) {
  role <- castro$role_group_matchbased_v2[1]
  pool <- final_scores |> dplyr::filter(role_group_matchbased_v2 == role, !is.na(DataScoreAmerica))
  pctile <- mean(pool$DataScoreAmerica <= castro$DataScoreAmerica[1], na.rm = TRUE) * 100
  message(sprintf(
    "Nicolás Castro: AmeScore (DataScoreAmerica) = %.1f, %.0fth percentile within %s (n=%d). %s",
    castro$DataScoreAmerica[1], pctile, role, nrow(pool),
    if (pctile >= 80) "PASS -- still in the upper tier, matching the doc's qualitative read ('se mantuvo en la zona alta')."
    else "DOES NOT MATCH -- no longer in the upper tier; worth a closer look at what changed (role assignment, Competition Strength, capacity weights)."
  ))
} else {
  message("Nicolás Castro: no scored row found -- cannot verify.")
}

franco <- cases |> dplyr::filter(player_name == "Gonçalo Franco")
if (nrow(franco) && !is.na(franco$role_group_matchbased_v2[1])) {
  role <- franco$role_group_matchbased_v2[1]
  res <- role_results[[role]]
  if (is.null(res)) res <- amescore_plus_for_role(role)
  frow <- res |> dplyr::filter(var_name == franco$source_var_name[1])
  if (nrow(frow) && !is.null(PHYSICAL_ENRICHMENT[[role]])) {
    lift <- frow$AmeScorePlus_Base[1] - frow$AmeScore_Base[1]
    message(sprintf(
      "Gonçalo Franco (%s): AmeScore_Base = %.1f, AmeScorePlus_Base = %.1f (delta = %+.1f). %s",
      role, frow$AmeScore_Base[1], frow$AmeScorePlus_Base[1], lift,
      if (lift > 0) "PASS -- physical enrichment lifts him, matching the doc's read that his 'motor fuerte, alta intensidad' wasn't fully captured by events alone."
      else if (!frow$physical_available[1]) "FLAT, BUT EXPECTED -- see physical_available note below: this is a data-coverage gap the doc itself already flagged for Championship, not a pipeline defect."
      else "DOES NOT MATCH -- physical coverage is present but doesn't lift him; his physical profile on OUR SkillCorner metrics may genuinely differ from the doc's manually-supplied numbers."
    ))
    caps <- names(ROLE_SCORE_DEFS[[role]]$caps)
    present <- intersect(caps, names(frow))
    message("  Capacity breakdown (doc expected high Presión-type, low Progresión/Ball Efficiency/Impacto ofensivo):")
    print(frow |> dplyr::select(dplyr::all_of(present)))
    message(sprintf(
      "  physical_available = %s -- %s",
      frow$physical_available[1],
      if (!frow$physical_available[1]) "no real SkillCorner physical coverage for Franco's enriched capacities in OUR current data pull, which matches the doc's OWN note that 'la cobertura física del archivo Championship era insuficiente' and that its AmeScore+ for Franco specifically required manually-supplied physical data outside the regular pipeline. The flat AmeScore+ here isn't a pipeline bug -- it's the same real-world data gap the doc already flagged, just not (yet) patched with a manual data drop the way the doc's own exercise was."
      else "physical coverage IS present, so the flat delta reflects his actual physical profile on our metrics, not a coverage gap."
    ))
  } else {
    message(sprintf("Gonçalo Franco: role '%s' has no physical-enrichment mapping or no scored row -- cannot verify the AmeScore+ lift.", role))
  }
} else {
  message("Gonçalo Franco: no scored row found -- cannot verify.")
}
