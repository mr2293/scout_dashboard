# ============================================================
# sensitivity_analysis.R
#
# Doc S21.15/S23: "Probar sensibilidad: mover pesos +-10-20%, retirar
# métricas y cambiar mínimos de minutos para medir estabilidad" /
# "Pequeños cambios de pesos no destruyen el ranking." Never run against
# the v2 pipeline until now -- this is that validation pass.
#
# Scope: Interior, the doc's own "laboratory role" (S11) and the role
# validated most thoroughly already in this pipeline -- deep-diving one
# role properly beats a shallow pass across all 7 (each of the other 6
# shares the same capacity_subscore()/combine_capacities() machinery, so
# a stability finding here is mechanism-level, not Interior-specific;
# only the exact numbers would differ per role).
#
# Three tests, each measuring RANK stability (Spearman correlation vs.
# the unperturbed baseline, and % overlap in the top 20) rather than just
# eyeballing score deltas -- "does the ranking survive this" is the
# doc's own acceptance criterion, not "do the raw numbers move a bit"
# (some movement is expected and fine; what matters is whether it
# reorders who looks good).
#
#   1. WEIGHT PERTURBATION -- jitter each capacity's weight by a random
#      amount within +-10% and +-20% (uniform, independent per capacity),
#      renormalize to sum to 1, recompute. Repeated N times per magnitude
#      to avoid one lucky/unlucky draw.
#   2. METRIC REMOVAL -- drop one metric at a time from whichever
#      capacity it lives in (remaining metrics in that capacity
#      renormalized to sum to 1), recompute. Tests whether the model
#      leans too hard on any single metric.
#   3. MIN-COVERAGE THRESHOLD -- vary MIN_SUBSCORE_COVERAGE (the
#      capacity_subscore()/combine_capacities() gate, currently 0.60)
#      and check how much the scored population and top-20 change.
#
# Standalone script, not wired into app.R -- same convention as every
# other file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("ame_score_plus.R")  # everything upstream (via its own source() chain)

N_PERTURBATIONS <- 30  # random draws per magnitude, weight perturbation test

# ---- Helpers --------------------------------------------------------------

# Jitter a named weight vector by +-pct (uniform per element), renormalize
# to sum 1. Never lets a weight go negative (floors at 1% of its original).
jitter_weights <- function(weights, pct) {
  factors <- 1 + stats::runif(length(weights), -pct, pct)
  jittered <- pmax(weights * factors, weights * 0.01)
  jittered / sum(jittered)
}

# Spearman rank correlation + top-20 overlap between a baseline and a
# perturbed score vector (NAs dropped pairwise).
rank_stability <- function(baseline, perturbed) {
  valid <- !is.na(baseline) & !is.na(perturbed)
  if (sum(valid) < 20) return(list(spearman = NA_real_, top20_overlap = NA_real_))
  b <- baseline[valid]; p <- perturbed[valid]
  spearman <- stats::cor(b, p, method = "spearman")
  top20_b <- order(b, decreasing = TRUE)[1:20]
  top20_p <- order(p, decreasing = TRUE)[1:20]
  list(spearman = spearman, top20_overlap = length(intersect(top20_b, top20_p)) / 20)
}

# ============================================================
# Main -- always runs (this script's whole purpose is the analysis, no
# separate "harness" needed).
# ============================================================

tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
dat <- load_scout_data()

role_idx <- which(dat$role_group_matchbased == "Interior")
metrics <- unique(unlist(lapply(INTERIOR_CAPACITIES, names)))
metrics <- intersect(metrics, names(dat))

normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
for (m in metrics) normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)

capacities_baseline <- normalized |> dplyr::select(var_name, player_id, player_name)
for (cap in names(INTERIOR_CAPACITIES)) capacities_baseline[[cap]] <- capacity_subscore(normalized, INTERIOR_CAPACITIES[[cap]])$score

ds_baseline <- combine_capacities(capacities_baseline, INTERIOR_DATASCORE_WEIGHTS)$score
ame_baseline <- combine_capacities(capacities_baseline, INTERIOR_AMESCORE_WEIGHTS)$score

message(sprintf("Baseline: n=%d, DataScore range=[%.1f,%.1f], AmeScore range=[%.1f,%.1f]",
                 nrow(capacities_baseline), min(ds_baseline, na.rm=TRUE), max(ds_baseline, na.rm=TRUE),
                 min(ame_baseline, na.rm=TRUE), max(ame_baseline, na.rm=TRUE)))

# ---- Test 1: weight perturbation ----
message("\n================ TEST 1: WEIGHT PERTURBATION ================")
for (pct in c(0.10, 0.20)) {
  ds_results <- vector("list", N_PERTURBATIONS)
  ame_results <- vector("list", N_PERTURBATIONS)
  for (i in seq_len(N_PERTURBATIONS)) {
    ds_w <- jitter_weights(INTERIOR_DATASCORE_WEIGHTS, pct)
    ame_w <- jitter_weights(INTERIOR_AMESCORE_WEIGHTS, pct)
    ds_results[[i]] <- rank_stability(ds_baseline, combine_capacities(capacities_baseline, ds_w)$score)
    ame_results[[i]] <- rank_stability(ame_baseline, combine_capacities(capacities_baseline, ame_w)$score)
  }
  ds_spearman <- sapply(ds_results, `[[`, "spearman")
  ds_overlap <- sapply(ds_results, `[[`, "top20_overlap")
  ame_spearman <- sapply(ame_results, `[[`, "spearman")
  ame_overlap <- sapply(ame_results, `[[`, "top20_overlap")
  message(sprintf(
    "+-%.0f%% (n=%d draws): DataScore spearman=%.3f [min %.3f] top20_overlap=%.0f%% [min %.0f%%]  |  AmeScore spearman=%.3f [min %.3f] top20_overlap=%.0f%% [min %.0f%%]",
    pct * 100, N_PERTURBATIONS,
    mean(ds_spearman), min(ds_spearman), mean(ds_overlap) * 100, min(ds_overlap) * 100,
    mean(ame_spearman), min(ame_spearman), mean(ame_overlap) * 100, min(ame_overlap) * 100
  ))
}

# ---- Test 2: metric removal ----
message("\n================ TEST 2: METRIC REMOVAL (one at a time) ================")
all_metric_caps <- unlist(lapply(names(INTERIOR_CAPACITIES), function(cap) {
  setNames(rep(cap, length(INTERIOR_CAPACITIES[[cap]])), names(INTERIOR_CAPACITIES[[cap]]))
}))

removal_results <- purrr::map_dfr(names(all_metric_caps), function(metric) {
  cap <- all_metric_caps[[metric]]
  cap_weights <- INTERIOR_CAPACITIES[[cap]]
  if (length(cap_weights) <= 1) return(NULL)  # can't remove the only metric in a capacity

  reduced_weights <- cap_weights[names(cap_weights) != metric]
  reduced_weights <- reduced_weights / sum(reduced_weights)
  reduced_caps_def <- INTERIOR_CAPACITIES
  reduced_caps_def[[cap]] <- reduced_weights

  capacities_reduced <- normalized |> dplyr::select(var_name, player_id, player_name)
  for (c2 in names(reduced_caps_def)) capacities_reduced[[c2]] <- capacity_subscore(normalized, reduced_caps_def[[c2]])$score

  ds_reduced <- combine_capacities(capacities_reduced, INTERIOR_DATASCORE_WEIGHTS)$score
  ame_reduced <- combine_capacities(capacities_reduced, INTERIOR_AMESCORE_WEIGHTS)$score
  ds_stab <- rank_stability(ds_baseline, ds_reduced)
  ame_stab <- rank_stability(ame_baseline, ame_reduced)

  tibble::tibble(
    metric = metric, capacity = cap, metric_weight_in_capacity = unname(cap_weights[metric]),
    ds_spearman = ds_stab$spearman, ds_top20_overlap = ds_stab$top20_overlap,
    ame_spearman = ame_stab$spearman, ame_top20_overlap = ame_stab$top20_overlap
  )
})

print(removal_results |> dplyr::arrange(ame_spearman) |>
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3))))

message(sprintf(
  "\nWorst single-metric-removal impact: DataScore spearman min=%.3f (%s), AmeScore spearman min=%.3f (%s)",
  min(removal_results$ds_spearman), removal_results$metric[which.min(removal_results$ds_spearman)],
  min(removal_results$ame_spearman), removal_results$metric[which.min(removal_results$ame_spearman)]
))

# ---- Test 3: min-coverage threshold ----
message("\n================ TEST 3: MIN-COVERAGE THRESHOLD ================")
for (thresh in c(0.40, 0.50, 0.60, 0.70, 0.80)) {
  caps_thresh <- normalized |> dplyr::select(var_name, player_id, player_name)
  for (cap in names(INTERIOR_CAPACITIES)) caps_thresh[[cap]] <- capacity_subscore(normalized, INTERIOR_CAPACITIES[[cap]], min_coverage = thresh)$score
  ds_thresh <- combine_capacities(caps_thresh, INTERIOR_DATASCORE_WEIGHTS, min_coverage = thresh)$score
  ame_thresh <- combine_capacities(caps_thresh, INTERIOR_AMESCORE_WEIGHTS, min_coverage = thresh)$score
  stab <- rank_stability(ds_baseline, ds_thresh)
  message(sprintf(
    "coverage>=%.0f%%: n_scored=%d/%d (%.1f%%)  DataScore spearman vs baseline(60%%)=%.3f  top20_overlap=%.0f%%",
    thresh * 100, sum(!is.na(ds_thresh)), nrow(caps_thresh), 100 * mean(!is.na(ds_thresh)),
    stab$spearman, stab$top20_overlap * 100
  ))
}

# ---- Test 4: confidence-shrinkage pseudo-count K ----------------------
# Unlike weights (a smooth average across many metrics, inherently
# robust -- see Test 1), K is a single multiplicative cutoff controlling
# how hard a low-minutes player gets pulled toward the role mean.
# Expected to behave differently: overall rank correlation should stay
# high (most players have plenty of minutes, so K barely touches them),
# but the LOW-MINUTES subset -- exactly who this parameter exists to
# affect -- should show real movement. Reports both so a flat "looks
# stable" overall number doesn't hide a real effect in the tail.
message("\n================ TEST 4: CONFIDENCE SHRINKAGE K ================")
ds_combo <- combine_capacities(capacities_baseline, INTERIOR_DATASCORE_WEIGHTS)
cs <- apply_competition_strength(capacities_baseline$var_name, ds_combo$score, tiers)
ds_previo <- cs$datascore_previo
minutes <- suppressWarnings(as.numeric(dat$player_season_minutes[role_idx]))
media_rol_ds <- mean(ds_previo, na.rm = TRUE)
exposure_90s <- minutes / 90

shrink <- function(k) {
  c_minutes <- exposure_90s / (exposure_90s + k)
  c <- c_minutes * ds_combo$coverage
  media_rol_ds + c * (ds_previo - media_rol_ds)
}

ds_final_baseline <- shrink(CONFIDENCE_SHRINKAGE_K_90S)  # current K=10
low_min_idx <- which(minutes <= stats::quantile(minutes, 0.25, na.rm = TRUE))

message(sprintf("Baseline K=%d (current). Low-minutes quartile: n=%d, minutes<=%.0f",
                 CONFIDENCE_SHRINKAGE_K_90S, length(low_min_idx), stats::quantile(minutes, 0.25, na.rm = TRUE)))

for (k in c(2, 5, 10, 20, 40)) {
  ds_final_k <- shrink(k)
  stab_all <- rank_stability(ds_final_baseline, ds_final_k)
  valid_low <- intersect(low_min_idx, which(!is.na(ds_final_baseline) & !is.na(ds_final_k)))
  mean_abs_delta_low <- mean(abs(ds_final_k[valid_low] - ds_final_baseline[valid_low]))
  mean_abs_delta_all <- mean(abs(ds_final_k - ds_final_baseline), na.rm = TRUE)
  message(sprintf(
    "K=%-3d  spearman_vs_K10=%.3f  top20_overlap=%.0f%%  mean|delta| low-minutes quartile=%.1f pts  mean|delta| overall=%.1f pts",
    k, stab_all$spearman, stab_all$top20_overlap * 100, mean_abs_delta_low, mean_abs_delta_all
  ))
}

# ---- Test 5: AmeScore structural gate threshold ------------------------
# Same "cutoff, not average" reasoning as Test 4 -- only players below
# the percentile threshold on Presión posicional are affected at all, so
# this reports gated-population size and the actual score reduction
# among THOSE players, not just an overall correlation that would stay
# near 1.0 almost by construction (only ~3.6% of rows are gated at the
# current threshold).
message("\n================ TEST 5: AMESCORE GATE THRESHOLD ================")
ame_combo <- combine_capacities(capacities_baseline, INTERIOR_AMESCORE_WEIGHTS)
techo_rol <- mean(ame_combo$score, na.rm = TRUE)
gate_capacity_v <- capacities_baseline[[GATE_CAPACITY]]

ame_final_baseline <- apply_ame_score_gate(
  ame_combo$score, gate_capacity_v, rep("Interior", nrow(capacities_baseline)),
  c(Interior = techo_rol), threshold = GATE_THRESHOLD
)$AmeScore_final

message(sprintf("Baseline threshold=%d (current, percentile on %s)", GATE_THRESHOLD, GATE_CAPACITY))

for (thresh in c(10, 15, 20, 25, 30)) {
  gate <- apply_ame_score_gate(
    ame_combo$score, gate_capacity_v, rep("Interior", nrow(capacities_baseline)),
    c(Interior = techo_rol), threshold = thresh
  )
  stab <- rank_stability(ame_final_baseline, gate$AmeScore_final)
  n_gated <- sum(gate$gate_active)
  gated_idx <- which(gate$gate_active)
  mean_reduction <- if (length(gated_idx)) mean(ame_combo$score[gated_idx] - gate$AmeScore_final[gated_idx], na.rm = TRUE) else NA_real_
  message(sprintf(
    "threshold=%2d pctile  n_gated=%4d (%.1f%%)  mean_reduction_when_gated=%.1f pts  spearman_vs_th20=%.3f  top20_overlap=%.0f%%",
    thresh, n_gated, 100 * n_gated / nrow(capacities_baseline), mean_reduction, stab$spearman, stab$top20_overlap * 100
  ))
}

# ---- Test 6: AmeScore+ physical blend rho ------------------------------
# Same "single cutoff/blend, not an average" reasoning as K and the gate
# threshold (Tests 4-5) -- rho controls how much AmeScore+ for ONE
# capacity leans on physical data vs events, and only matters for
# players who actually HAVE physical data (~22% of Interior). Interior
# has two rho-enriched capacities (Dinamismo rho=0.40, Presión/
# contrapresión rho=0.25 -- both the doc's own S12.5 worked example),
# swept independently (holding the other at its baseline) so a finding
# on one doesn't get muddied by the other moving at the same time.
message("\n================ TEST 6: AMESCORE+ PHYSICAL BLEND RHO ================")
enrich <- PHYSICAL_ENRICHMENT[["Interior"]]

# Subscore_físico per enriched capacity, computed ONCE -- the rho sweep
# below only varies the blend, not the underlying physical normalization.
subscore_fisico_by_cap <- list()
for (cap in names(enrich)) {
  metric_weights <- enrich[[cap]]$metrics
  phys_metrics <- intersect(names(metric_weights), names(dat))
  normalized_phys <- capacities_baseline |> dplyr::select(var_name, player_id, player_name)
  for (m in phys_metrics) normalized_phys[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], exposure_90s, m)
  subscore_fisico_by_cap[[cap]] <- capacity_subscore(normalized_phys, metric_weights[phys_metrics])$score
}

techo_rol_ame <- mean(combine_capacities(capacities_baseline, INTERIOR_AMESCORE_WEIGHTS)$score, na.rm = TRUE)

# rho_overrides: named list, capacity -> rho to use INSTEAD of its
# PHYSICAL_ENRICHMENT default, for just the capacities named in it.
build_ame_plus <- function(rho_overrides = list()) {
  caps_plus <- capacities_baseline
  physical_available <- rep(FALSE, nrow(caps_plus))
  for (cap in names(enrich)) {
    rho <- if (!is.null(rho_overrides[[cap]])) rho_overrides[[cap]] else enrich[[cap]]$rho
    fisico <- subscore_fisico_by_cap[[cap]]
    has_phys <- !is.na(fisico)
    physical_available <- physical_available | has_phys
    eventos <- capacities_baseline[[cap]]
    caps_plus[[cap]] <- ifelse(has_phys, (1 - rho) * eventos + rho * fisico, eventos)
  }
  ame_plus <- combine_capacities(caps_plus, INTERIOR_AMESCORE_WEIGHTS)
  gate <- apply_ame_score_gate(ame_plus$score, caps_plus[[GATE_CAPACITY]], rep("Interior", nrow(caps_plus)), c(Interior = techo_rol_ame))
  list(final = ifelse(physical_available, gate$AmeScore_final, NA_real_), physical_available = physical_available)
}

baseline_plus <- build_ame_plus()
message(sprintf("Physical-available subset: n=%d (%.1f%% of Interior)",
                 sum(baseline_plus$physical_available), 100 * mean(baseline_plus$physical_available)))

for (cap in names(enrich)) {
  base_rho <- enrich[[cap]]$rho
  message(sprintf("\n-- %s (baseline rho=%.2f) --", cap, base_rho))
  sweep <- sort(unique(pmax(0, pmin(1, round(c(base_rho - 0.20, base_rho - 0.10, base_rho, base_rho + 0.10, base_rho + 0.20), 2)))))
  for (rho in sweep) {
    res <- build_ame_plus(setNames(list(rho), cap))
    valid_subset <- which(baseline_plus$physical_available & res$physical_available &
                             !is.na(baseline_plus$final) & !is.na(res$final))
    stab <- rank_stability(baseline_plus$final[valid_subset], res$final[valid_subset])
    mean_abs_delta <- mean(abs(res$final[valid_subset] - baseline_plus$final[valid_subset]))
    message(sprintf(
      "rho=%.2f  spearman_vs_baseline=%.3f  top20_overlap=%.0f%%  mean|delta|=%.1f pts  n=%d",
      rho, stab$spearman, stab$top20_overlap * 100, mean_abs_delta, length(valid_subset)
    ))
  }
}
