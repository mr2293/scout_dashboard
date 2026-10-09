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
