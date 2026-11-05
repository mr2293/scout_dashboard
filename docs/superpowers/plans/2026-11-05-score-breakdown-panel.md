# Score Breakdown Panel Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Clicking a DataScore/AmeScore/AmeScore+ tag opens a floating panel showing that score's own category/metric breakdown (value, percentile, continuous color gradient), with a season toggle (Acumulado default).

**Architecture:** `scout_dashboard`'s scoring pipeline (`competition_strength.R`, `base_scores.R`, `ame_score_plus.R`) already computes a per-metric percentile and per-capacity sub-score for every row, for both Acumulado and every per-season row, then discards everything except the final composite. A new shared helper retains this as a nested structure (3 new fields: `DataScoreBreakdown`, `AmeScoreBreakdown`, `AmeScorePlusBreakdown`), which flows through the existing Acumulado sync automatically and needs one explicit addition to the per-season sync's column list. `ca_scouting_dashboard` gets a new floating panel component and a click handler on the existing score tags.

**Tech Stack:** R (dplyr), Next.js server components, plain React/SVG.

**Spec:** `docs/superpowers/specs/2026-11-05-score-breakdown-panel-design.md`

## Global Constraints

- Nothing about the existing `DataScore`/`DataScoreAmerica`/`DataScoreAmericaPlus`/their coverage columns, or the existing hover popover (`ScorePercentilePopover`), may change. This is strictly additive.
- Each score (DataScore/AmeScore/AmeScore+) gets its OWN breakdown reflecting its own capacity-level weights (`def$ds` vs `def$ame`) — never one shared breakdown reused across scores.
- AmeScore+'s breakdown must tag each metric `"event"` or `"physical"` (per `PHYSICAL_ENRICHMENT`/`DELANTERO_PERFIL_PHYSICAL_ENRICHMENT`), since physically-enriched capacities blend both.
- List-columns destined for Mongo must use `jsonlite::unbox()` per scalar from the start (the exact bug found and fixed in `sc_physical_acumulado_fallback.R` this session — a value arriving as `[72.3]` instead of `72.3`, or `NA` arriving as the string `"NA"` instead of `null`) — never the bare `as.list()`-only pattern that caused that bug.
- The new panel's color gradient (continuous, red → gold by percentile) is scoped to this panel only — every other percentile display in the app keeps its existing 3-tier convention.
- Season toggle defaults to Acumulado.

---

### Task 1: Shared breakdown helper + wire into the generic (non-Delantero) DataScore/AmeScore path

**Files:**
- Modify: `capacity_subscores.R` (add `build_score_breakdown()`)
- Modify: `competition_strength.R:114-174` (`build_scored_rows()`, generic branch)

**Interfaces:**
- Produces: `build_score_breakdown(normalized, raw_values, capacities, caps_defs, score_weights, metric_source = character(0))` → a list, one element per row (same order as its inputs), each element a list of capacity entries: `list(category=, capacityScore=, capacityWeight=, metrics=list(list(key=, value=, percentile=, weight=, source=), ...))`. Every scalar inside is `jsonlite::unbox()`-wrapped already (never a bare `NA`/length-1 vector) so it serializes correctly through the real `mongolite` path without the bug found earlier this session.
- Consumes (from the existing, unmodified pipeline): `normalized` (one column per raw metric, 0-100 percentile, already computed via `normalize_metric()`), `capacities` (one column per capacity, 0-100, already computed via `capacity_subscore()`), `def$caps` (named list, capacity → named metric-weight vector), `def$ds`/`def$ame` (named weight vectors, capacity → its weight in that composite).

- [ ] **Step 1: Add `build_score_breakdown()` to `capacity_subscores.R`**

Add this function immediately after `capacity_subscore()` (after its closing `}` at `capacity_subscores.R:478`):

```r
# Retains what capacity_subscore()/combine_capacities() already compute
# internally and normally discard -- every metric's raw value/percentile/
# weight, grouped by capacity, for ONE score's own capacity-level
# weighting (score_weights = def$ds or def$ame -- the same capacities,
# different weights per score). Nothing new is computed here.
#
# metric_source: named character vector, metric key -> "event"|"physical"
# (default "event" for any key not listed) -- lets AmeScore+'s physically
# enriched capacities tag their physical-sourced metrics distinctly from
# event-sourced ones within the same capacity (see ame_score_plus.R).
#
# jsonlite::unbox() wraps every scalar from the start -- NOT an
# afterthought. A bare as.list()-only list-column serialized through the
# REAL mongolite insert path (not jsonlite::toJSON() directly) turned NA
# into the JSON string "NA" and every scalar into a 1-element array (see
# sc_physical_acumulado_fallback.R's fix, 2026-11-05) -- building the
# unboxed shape here from the start avoids repeating that bug.
build_score_breakdown <- function(normalized, raw_values, capacities, caps_defs, score_weights, metric_source = character(0)) {
  cap_names <- intersect(names(score_weights), names(caps_defs))
  n <- nrow(capacities)

  lapply(seq_len(n), function(i) {
    lapply(cap_names, function(cap) {
      metric_weights <- caps_defs[[cap]]
      metric_keys <- names(metric_weights)
      metrics <- lapply(metric_keys, function(m) {
        val <- if (m %in% names(raw_values)) suppressWarnings(as.numeric(raw_values[[m]][i])) else NA_real_
        pct <- if (m %in% names(normalized)) suppressWarnings(as.numeric(normalized[[m]][i])) else NA_real_
        src <- if (m %in% names(metric_source)) unname(metric_source[m]) else "event"
        list(
          key = jsonlite::unbox(m),
          value = jsonlite::unbox(if (is.na(val)) NA else val),
          percentile = jsonlite::unbox(if (is.na(pct)) NA else pct),
          weight = jsonlite::unbox(unname(metric_weights[m])),
          source = jsonlite::unbox(src)
        )
      })
      cap_score <- suppressWarnings(as.numeric(capacities[[cap]][i]))
      list(
        category = jsonlite::unbox(cap),
        capacityScore = jsonlite::unbox(if (is.na(cap_score)) NA else cap_score),
        capacityWeight = jsonlite::unbox(unname(score_weights[cap])),
        metrics = metrics
      )
    })
  })
}
```

- [ ] **Step 2: Wire into `build_scored_rows()`'s generic (non-Delantero) branch**

In `competition_strength.R`, the `else` branch (non-Delantero) currently reads (lines 128-144):

```r
    } else {
      def <- ROLE_SCORE_DEFS[[role]]
      metrics <- unique(unlist(lapply(def$caps, names)))
      metrics <- intersect(metrics, names(dat))

      normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
      for (m in metrics) {
        normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
      }

      capacities <- normalized |> dplyr::select(var_name, player_id, player_name)
      for (cap in names(def$caps)) {
        capacities[[cap]] <- capacity_subscore(normalized, def$caps[[cap]])$score
      }

      ds <- combine_capacities(capacities, def$ds)
      ame <- combine_capacities(capacities, def$ame)
    }
```

Add two lines right after the `ame <- combine_capacities(...)` line (still inside the `else` branch, before its closing `}`):

```r
      ds <- combine_capacities(capacities, def$ds)
      ame <- combine_capacities(capacities, def$ame)

      raw_values <- dat[role_idx, metrics, drop = FALSE]
      ds_breakdown <- build_score_breakdown(normalized, raw_values, capacities, def$caps, def$ds)
      ame_breakdown <- build_score_breakdown(normalized, raw_values, capacities, def$caps, def$ame)
    }
```

Then, in the SAME function's `dplyr::mutate(...)` block that builds `all_results[[role]]` (lines 156-171), add two new columns. The Delantero branch (the `if` side) does not define `ds_breakdown`/`ame_breakdown` yet — Task 2 adds them there. For THIS task, add the columns as list-columns via a plain `$<-` assignment (not inside the `dplyr::mutate()` pipe, since `dplyr::mutate()` cannot easily assign a list produced outside the piped data unless passed as a literal list — simpler and clearer as a separate step):

```r
    all_results[[role]] <- capacities |>
      dplyr::mutate(
        role_group_matchbased = role,
        season_id = dat$season_id[role_idx],
        season_name = dat$season_name[role_idx],
        league = unname(VAR_TO_LEAGUE[var_name]),
        DataScore_Base = round(ds$score, 1),
        DataScore_Base_cobertura = round(ds$coverage * 100, 1),
        CompetitionStrength = cs$strength,
        Ajuste_Liga = cs$ajuste_liga,
        DataScore_previo = cs$datascore_previo,
        AmeScore_Base = round(ame$score, 1),
        AmeScore_Base_cobertura = round(ame$coverage * 100, 1),
        player_season_minutes = suppressWarnings(as.numeric(dat$player_season_minutes[role_idx])),
        player_season_appearances = suppressWarnings(as.numeric(dat$player_season_appearances[role_idx]))
      )
    all_results[[role]]$DataScoreBreakdown <- ds_breakdown
    all_results[[role]]$AmeScoreBreakdown <- ame_breakdown
```

(This assigns `NULL` for the Delantero branch in this task's state -- Task 2 fixes that by defining `ds_breakdown`/`ame_breakdown` in the Delantero branch too, before this same two-line assignment runs. Do not skip ahead; Task 2 depends on this exact assignment shape.)

- [ ] **Step 3: Verify the cross-check (breakdown matches the actual DataScore/AmeScore already produced)**

Run (from the repo root):

```r
Rscript -e '
suppressWarnings(suppressMessages(source("competition_strength.R")))
tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
dat <- load_scout_data()
rows <- build_scored_rows(dat, tiers)

stopifnot("DataScoreBreakdown present" = "DataScoreBreakdown" %in% names(rows))
stopifnot("AmeScoreBreakdown present" = "AmeScoreBreakdown" %in% names(rows))

# Para una fila no-Delantero con DataScore_Base real: la suma ponderada de
# capacityScore*capacityWeight sobre su DataScoreBreakdown debe reproducir
# (aprox, por redondeo) el propio DataScore_Base de esa fila.
check_rows <- which(!is.na(rows$DataScore_Base) & rows$role_group_matchbased != "Delantero")[1:30]
for (i in check_rows) {
  bd <- rows$DataScoreBreakdown[[i]]
  total_w <- sum(sapply(bd, function(c) c$capacityWeight))
  weighted <- sum(sapply(bd, function(c) {
    s <- c$capacityScore
    if (is.null(s) || (length(s) == 1 && is.na(s))) 0 else s * c$capacityWeight
  }))
  have_w <- sum(sapply(bd, function(c) {
    s <- c$capacityScore
    if (is.null(s) || (length(s) == 1 && is.na(s))) 0 else c$capacityWeight
  }))
  if (have_w == 0) next
  reconstructed <- round(weighted / have_w, 1)
  stopifnot(isTRUE(all.equal(reconstructed, rows$DataScore_Base[i], tolerance = 1.0)))
}
cat("PASS: DataScoreBreakdown reconstructs DataScore_Base within rounding tolerance\n")
'
```

Expected: `PASS: DataScoreBreakdown reconstructs DataScore_Base within rounding tolerance`. A `tolerance = 1.0` (not stricter) is deliberate -- `combine_capacities()`'s own coverage-gated redistribution and this check's simplified reconstruction can differ by a rounding point without indicating a real bug; a much larger mismatch would.

- [ ] **Step 4: Verify the list-column serializes correctly through the REAL mongolite path**

```r
Rscript -e '
suppressWarnings(suppressMessages({ library(mongolite); library(jsonlite) }))
suppressWarnings(suppressMessages(source("competition_strength.R")))
tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
dat <- load_scout_data()
rows <- build_scored_rows(dat, tiers)

sample_row <- rows[!is.na(rows$DataScore_Base) & rows$role_group_matchbased != "Delantero", ][1, ]
df_test <- data.frame(player_id = sample_row$player_id, stringsAsFactors = FALSE)
df_test$DataScoreBreakdown <- sample_row$DataScoreBreakdown
j <- mongolite:::mongo_to_json(df_test, collapse = FALSE)
cat(substr(j, 1, 400), "\n")
parsed <- jsonlite::fromJSON(j)
cat("PASS: parses, is a list of capacity objects:", is.list(parsed$DataScoreBreakdown[[1]]), "\n")
'
```

Expected: the printed JSON shows plain numbers for `value`/`percentile`/`weight`/`capacityScore`/`capacityWeight` (not `[72.3]`-style arrays), and a missing/NA value prints as `null` (not the string `"NA"`). If it does not, stop and report BLOCKED with the actual output -- this is the exact failure mode found and fixed in `sc_physical_acumulado_fallback.R` earlier this session; do not assume `jsonlite::unbox()` alone guarantees correctness without re-confirming against the real serializer.

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add capacity_subscores.R competition_strength.R
git commit -m "$(cat <<'EOF'
Add build_score_breakdown(): retain per-metric DataScore/AmeScore detail

Wires into build_scored_rows()'s generic (non-Delantero) branch --
Task 2 wires the Delantero branch, Task 3 wires AmeScore+. Every
scalar is jsonlite::unbox()-wrapped from the start, avoiding the
array-wrapping/NA-as-string bug found in sc_physical_acumulado_
fallback.R earlier this session.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Wire the breakdown helper into the Delantero (perfil-based) DataScore/AmeScore path

**Files:**
- Modify: `base_scores.R:276-312` (`score_delantero_by_perfil()`)

**Interfaces:**
- Consumes: `build_score_breakdown()` (Task 1, `capacity_subscores.R`).
- Produces: `DataScoreBreakdown`/`AmeScoreBreakdown` columns on `score_delantero_by_perfil()`'s returned data.frame, same shape as Task 1's non-Delantero rows -- `build_scored_rows()`'s Delantero branch (`competition_strength.R:119-127`) reads `scored |> dplyr::select(-DataScore_Base, ...)` for `capacities`, so these two new columns must survive that `select()` too (add them to the exclusion list the same way the other score columns already are, OR simply don't exclude them -- see Step 2).

- [ ] **Step 1: Wire into `score_delantero_by_perfil()`'s per-perfil loop**

In `base_scores.R`, the per-perfil loop currently reads (lines 290-310):

```r
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
```

Replace it with:

```r
  for (p in names(DELANTERO_PERFIL_DEFS)) {
    idx <- which(normalized$perfil == p)
    if (!length(idx)) next
    sub <- normalized[idx, ]
    def <- DELANTERO_PERFIL_DEFS[[p]]
    sub_role_idx <- role_idx[idx]

    caps <- sub |> dplyr::select(var_name, player_id, player_name, .orig_pos)
    for (cap in names(def$caps)) caps[[cap]] <- capacity_subscore(sub, def$caps[[cap]])$score

    ds <- combine_capacities(caps, def$ds)
    ame <- combine_capacities(caps, def$ame)

    raw_values <- dat[sub_role_idx, names(sub)[names(sub) %in% names(dat)], drop = FALSE]
    ds_breakdown <- build_score_breakdown(sub, raw_values, caps, def$caps, def$ds)
    ame_breakdown <- build_score_breakdown(sub, raw_values, caps, def$caps, def$ame)

    result[[p]] <- caps |>
      dplyr::mutate(
        perfil = p,
        DataScore_Base = round(ds$score, 1),
        DataScore_Base_cobertura = round(ds$coverage * 100, 1),
        AmeScore_Base = round(ame$score, 1),
        AmeScore_Base_cobertura = round(ame$coverage * 100, 1)
      )
    result[[p]]$DataScoreBreakdown <- ds_breakdown
    result[[p]]$AmeScoreBreakdown <- ame_breakdown
  }
```

- [ ] **Step 2: Confirm the new columns survive `build_scored_rows()`'s Delantero-branch `select()`**

In `competition_strength.R`, the Delantero branch (lines 119-127) currently reads:

```r
    if (role == "Delantero") {
      # Perfil-based, not a straight capacity lookup -- see
      # classify_delantero_perfil()/score_delantero_by_perfil()
      # (base_scores.R). scored is already reordered to match role_idx.
      perfil <- classify_delantero_perfil(dat, role_idx)
      scored <- score_delantero_by_perfil(dat, role_idx, perfil)
      capacities <- scored |> dplyr::select(-DataScore_Base, -DataScore_Base_cobertura, -AmeScore_Base, -AmeScore_Base_cobertura)
      ds <- list(score = scored$DataScore_Base, coverage = scored$DataScore_Base_cobertura / 100)
      ame <- list(score = scored$AmeScore_Base, coverage = scored$AmeScore_Base_cobertura / 100)
    } else {
```

This `select(-DataScore_Base, -DataScore_Base_cobertura, -AmeScore_Base, -AmeScore_Base_cobertura)` drops exactly those 4 named columns and keeps everything else -- `DataScoreBreakdown`/`AmeScoreBreakdown` are NOT in that exclusion list, so they survive into `capacities` automatically. Add two lines right after this `if` block's last line (`ame <- list(...)`), still inside the `if (role == "Delantero") { ... }` branch, so the names match what Task 1's trailing `all_results[[role]]$DataScoreBreakdown <- ds_breakdown` assignment expects:

```r
      perfil <- classify_delantero_perfil(dat, role_idx)
      scored <- score_delantero_by_perfil(dat, role_idx, perfil)
      capacities <- scored |> dplyr::select(-DataScore_Base, -DataScore_Base_cobertura, -AmeScore_Base, -AmeScore_Base_cobertura)
      ds <- list(score = scored$DataScore_Base, coverage = scored$DataScore_Base_cobertura / 100)
      ame <- list(score = scored$AmeScore_Base, coverage = scored$AmeScore_Base_cobertura / 100)
      ds_breakdown <- scored$DataScoreBreakdown
      ame_breakdown <- scored$AmeScoreBreakdown
    } else {
```

This makes `ds_breakdown`/`ame_breakdown` defined in BOTH branches before Task 1's trailing `all_results[[role]]$DataScoreBreakdown <- ds_breakdown` / `$AmeScoreBreakdown <- ame_breakdown` lines run (those two lines sit after the `if/else` block closes, per Task 1 Step 2).

- [ ] **Step 3: Verify**

```r
Rscript -e '
suppressWarnings(suppressMessages(source("competition_strength.R")))
tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
dat <- load_scout_data()
rows <- build_scored_rows(dat, tiers)

del_rows <- rows[rows$role_group_matchbased == "Delantero" & !is.na(rows$DataScore_Base), ]
stopifnot(nrow(del_rows) > 0)
stopifnot(!is.null(del_rows$DataScoreBreakdown[[1]]))
cat(sprintf("PASS: %d Delantero rows have a non-null DataScoreBreakdown\n", nrow(del_rows)))
'
```

Expected: `PASS: N Delantero rows have a non-null DataScoreBreakdown` with `N > 0`.

- [ ] **Step 4: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add base_scores.R competition_strength.R
git commit -m "$(cat <<'EOF'
Wire build_score_breakdown() into the Delantero perfil-based path

score_delantero_by_perfil() now also retains DataScoreBreakdown/
AmeScoreBreakdown, using that player's own classified perfil's
capacity definitions -- same shape as every other role (Task 1),
completing DataScore/AmeScore breakdown coverage for every role.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Wire the breakdown helper into AmeScore+ (event + physical blend, all roles)

**Files:**
- Modify: `ame_score_plus.R:184-219` (`apply_physical_enrichment()`)
- Modify: `ame_score_plus.R:231-270` (`compute_amescore_plus_all_roles()`, generic role loop)
- Modify: `ame_score_plus.R:272-320` (`compute_amescore_plus_all_roles()`, Delantero-per-perfil branch)

**Interfaces:**
- Consumes: `build_score_breakdown()` (Task 1).
- Produces: `apply_physical_enrichment()` now returns `list(capacities = <existing return value, unchanged shape>, normalized_phys = <data.frame, one column per physical metric actually used by any of this role's enriched capacities, same row order as capacities>)` instead of just the capacities data.frame -- a breaking change to its own return shape, so BOTH call sites (generic loop and Delantero branch) must be updated together in this same task.
- Produces: `AmeScorePlusBreakdown` column on both of `compute_amescore_plus_all_roles()`'s result branches, with each metric tagged `source = "event"` or `source = "physical"`.

- [ ] **Step 1: Change `apply_physical_enrichment()`'s return shape**

Current function (`ame_score_plus.R:184-219`):

```r
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
```

Replace with (change is: accumulate every capacity's `normalized_phys` columns into one combined data.frame across the loop, instead of rebuilding and discarding a fresh one per capacity; return both pieces):

```r
apply_physical_enrichment <- function(role, dat, role_idx, exposure_90s, capacities_events, enrichment_map = PHYSICAL_ENRICHMENT) {
  enrich <- enrichment_map[[role]]
  if (is.null(enrich)) {
    capacities_events$physical_available <- FALSE
    return(list(capacities = capacities_events, normalized_phys = capacities_events |> dplyr::select(var_name, player_id, player_name)))
  }

  physical_available <- rep(FALSE, nrow(capacities_events))
  normalized_phys_all <- capacities_events |> dplyr::select(var_name, player_id, player_name)

  for (cap in names(enrich)) {
    rho <- enrich[[cap]]$rho
    metric_weights <- enrich[[cap]]$metrics
    phys_metrics <- names(metric_weights)
    phys_metrics <- intersect(phys_metrics, names(dat))

    normalized_phys <- capacities_events |> dplyr::select(var_name, player_id, player_name)
    for (m in phys_metrics) {
      normalized_phys[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], exposure_90s, m)
      # Acumula en la combinada -- distinto cap puede reusar el mismo
      # metric físico (p.ej. psv99 en más de una capacidad), así que solo
      # se agrega la columna si todavía no está.
      if (!(m %in% names(normalized_phys_all))) normalized_phys_all[[m]] <- normalized_phys[[m]]
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
  list(capacities = capacities_events, normalized_phys = normalized_phys_all)
}
```

- [ ] **Step 2: Update the generic role loop call site**

In `ame_score_plus.R`'s `compute_amescore_plus_all_roles()`, the generic loop currently reads (lines 231-263, relevant excerpt):

```r
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
        perfil = NA_character_,
        AmeScorePlus_Base = round(ame_plus$score, 1),
        AmeScorePlus_final = ifelse(physical_available, gate$AmeScore_final, NA_real_),
        AmeScorePlus_published = physical_available & !is.na(gate$AmeScore_final)
      )
    all_results[[role]] <- result
```

Replace with:

```r
    capacities <- normalized |> dplyr::select(var_name, player_id, player_name)
    for (cap in names(def$caps)) capacities[[cap]] <- capacity_subscore(normalized, def$caps[[cap]])$score

    enrich_result <- apply_physical_enrichment(role, dat, role_idx, dat$exposure_90s[role_idx], capacities)
    capacities_plus <- enrich_result$capacities

    ame_plus <- combine_capacities(capacities_plus, def$ame)
    techo_rol <- mean(combine_capacities(capacities, def$ame)$score, na.rm = TRUE)
    gate <- apply_ame_score_gate(ame_plus$score, capacities_plus[[GATE_CAPACITY]], rep(role, nrow(capacities_plus)), c(setNames(techo_rol, role)))

    # Breakdown de AmeScore+: percentiles/valores EVENT ya están en
    # `normalized`; los FÍSICOS están en enrich_result$normalized_phys
    # (una columna por métrica física usada por CUALQUIER capacidad
    # enriquecida de este rol). metric_source marca cuáles son físicas --
    # todo lo demás queda "event" por default en build_score_breakdown().
    enrich_map <- PHYSICAL_ENRICHMENT[[role]]
    metric_source <- character(0)
    if (!is.null(enrich_map)) {
      for (cap in names(enrich_map)) {
        phys_metrics <- names(enrich_map[[cap]]$metrics)
        metric_source[phys_metrics] <- "physical"
      }
    }
    # def_caps_plus: cada capacidad enriquecida suma sus métricas físicas
    # a sus métricas de evento (una sola lista de pesos por capacidad,
    # para que build_score_breakdown() las muestre juntas bajo esa
    # categoría) -- los pesos de cada grupo siguen siendo los suyos
    # propios (de def$caps o de enrich_map[[cap]]$metrics), no se
    # re-derivan a un peso único "rho-ajustado".
    def_caps_plus <- def$caps
    if (!is.null(enrich_map)) {
      for (cap in names(enrich_map)) {
        if (cap %in% names(def_caps_plus)) {
          def_caps_plus[[cap]] <- c(def_caps_plus[[cap]], enrich_map[[cap]]$metrics)
        }
      }
    }
    normalized_combined <- dplyr::left_join(normalized, enrich_result$normalized_phys, by = c("var_name", "player_id", "player_name"))
    raw_metrics_plus <- unique(c(names(normalized), names(enrich_result$normalized_phys)))
    raw_metrics_plus <- setdiff(raw_metrics_plus, c("var_name", "player_id", "player_name"))
    raw_metrics_plus <- intersect(raw_metrics_plus, names(dat))
    raw_values_plus <- dat[role_idx, raw_metrics_plus, drop = FALSE]
    ameplus_breakdown <- build_score_breakdown(normalized_combined, raw_values_plus, capacities_plus, def_caps_plus, def$ame, metric_source)

    result <- capacities_plus |>
      dplyr::select(var_name, player_id, player_name, physical_available) |>
      dplyr::mutate(
        role_group_matchbased = role,
        perfil = NA_character_,
        AmeScorePlus_Base = round(ame_plus$score, 1),
        AmeScorePlus_final = ifelse(physical_available, gate$AmeScore_final, NA_real_),
        AmeScorePlus_published = physical_available & !is.na(gate$AmeScore_final)
      )
    result$AmeScorePlusBreakdown <- ameplus_breakdown
    all_results[[role]] <- result
```

- [ ] **Step 3: Update the Delantero-per-perfil branch call site**

Same change, applied to the Delantero branch (lines 272-320 excerpt):

```r
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
```

Replace with:

```r
    capacities <- sub |> dplyr::select(var_name, player_id, player_name, .orig_pos)
    for (cap in names(def$caps)) capacities[[cap]] <- capacity_subscore(sub, def$caps[[cap]])$score

    enrich_result <- apply_physical_enrichment(p, dat, sub_role_idx, dat$exposure_90s[sub_role_idx], capacities, DELANTERO_PERFIL_PHYSICAL_ENRICHMENT)
    capacities_plus <- enrich_result$capacities

    ame_plus <- combine_capacities(capacities_plus, def$ame)
    techo_perfil <- mean(combine_capacities(capacities, def$ame)$score, na.rm = TRUE)
    gate <- apply_ame_score_gate(ame_plus$score, capacities_plus[[GATE_CAPACITY]], rep(p, nrow(capacities_plus)), c(setNames(techo_perfil, p)))

    enrich_map <- DELANTERO_PERFIL_PHYSICAL_ENRICHMENT[[p]]
    metric_source <- character(0)
    if (!is.null(enrich_map)) {
      for (cap in names(enrich_map)) {
        phys_metrics <- names(enrich_map[[cap]]$metrics)
        metric_source[phys_metrics] <- "physical"
      }
    }
    def_caps_plus <- def$caps
    if (!is.null(enrich_map)) {
      for (cap in names(enrich_map)) {
        if (cap %in% names(def_caps_plus)) {
          def_caps_plus[[cap]] <- c(def_caps_plus[[cap]], enrich_map[[cap]]$metrics)
        }
      }
    }
    normalized_combined <- dplyr::left_join(sub, enrich_result$normalized_phys, by = c("var_name", "player_id", "player_name"))
    raw_metrics_plus <- unique(c(names(sub), names(enrich_result$normalized_phys)))
    raw_metrics_plus <- setdiff(raw_metrics_plus, c("var_name", "player_id", "player_name", "perfil", ".orig_pos"))
    raw_metrics_plus <- intersect(raw_metrics_plus, names(dat))
    raw_values_plus <- dat[sub_role_idx, raw_metrics_plus, drop = FALSE]
    ameplus_breakdown <- build_score_breakdown(normalized_combined, raw_values_plus, capacities_plus, def_caps_plus, def$ame, metric_source)

    delantero_results[[p]] <- capacities_plus |>
      dplyr::select(var_name, player_id, player_name, physical_available) |>
      dplyr::mutate(
        role_group_matchbased = "Delantero",
        perfil = p,
        AmeScorePlus_Base = round(ame_plus$score, 1),
        AmeScorePlus_final = ifelse(physical_available, gate$AmeScore_final, NA_real_),
        AmeScorePlus_published = physical_available & !is.na(gate$AmeScore_final)
      )
    delantero_results[[p]]$AmeScorePlusBreakdown <- ameplus_breakdown
  }
```

- [ ] **Step 4: Verify**

```r
Rscript -e '
suppressWarnings(suppressMessages(source("ame_score_plus.R")))
dat <- load_scout_data()
result <- compute_amescore_plus_all_roles(dat)

stopifnot("AmeScorePlusBreakdown present" = "AmeScorePlusBreakdown" %in% names(result))

# Al menos una fila publicada de un rol con enriquecimiento físico
# (p.ej. "Central") debe tener AL MENOS un metric con source=="physical"
# en alguna de sus categorías.
central_published <- result[result$role_group_matchbased == "Central" & isTRUE(result$AmeScorePlus_published[1]), ]
found_physical <- FALSE
for (i in seq_len(min(20, nrow(central_published)))) {
  bd <- central_published$AmeScorePlusBreakdown[[i]]
  for (cap in bd) {
    srcs <- sapply(cap$metrics, function(m) m$source)
    if ("physical" %in% srcs) { found_physical <- TRUE; break }
  }
  if (found_physical) break
}
stopifnot("found a physical-sourced metric in a Central AmeScorePlusBreakdown" = found_physical)
cat("PASS: AmeScorePlusBreakdown tags physical-sourced metrics correctly\n")
'
```

Expected: `PASS: AmeScorePlusBreakdown tags physical-sourced metrics correctly`.

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add ame_score_plus.R
git commit -m "$(cat <<'EOF'
Wire build_score_breakdown() into AmeScore+ (event + physical blend)

apply_physical_enrichment() now also returns the physical metrics it
already computes internally (previously discarded after producing the
blended subscore). Both AmeScore+ call sites (generic role loop,
Delantero-per-perfil branch) build AmeScorePlusBreakdown with each
metric tagged event/physical, so an enriched capacity's blend is
legible in the UI.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Thread breakdowns through the per-season sync path

**Files:**
- Modify: `datascore_v2.R:219-272` (`get_datascore_v2_scores_by_season()`)
- Modify: `sync_season_rows_to_mongo.R:131-133` (`SEASON_SYNC_SCORE_COLS`)

**Interfaces:**
- Consumes: `DataScoreBreakdown`/`AmeScoreBreakdown`/`AmeScorePlusBreakdown` columns, already present on `build_scored_rows()`'s output (Tasks 1-2) and `compute_amescore_plus_all_roles()`'s output (Task 3) -- both of which `get_datascore_v2_scores_by_season()` already calls.
- Produces: the same three fields, now also present in `sync_season_rows_to_mongo.R`'s synced per-season documents (`sb_sc_metrics_by_season`).

- [ ] **Step 1: Retain the breakdown columns through `get_datascore_v2_scores_by_season()`'s transforms**

In `datascore_v2.R`, three `transmute()` calls currently drop every column except the final score + coverage (lines 219-265). Update each to also keep its breakdown column:

The `ds_by_season` block (lines 219-233) currently ends:
```r
    ) |>
    apply_confidence_shrinkage(role_means_ds) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      DataScore = round(scale_datascore(DataScore_final), 1),
      Cobertura_DataScore = round(C * 100, 1)
    )
```
`apply_confidence_shrinkage()` does not carry `DataScoreBreakdown` through on its own (it operates on `DataScore_shown` only) -- so the breakdown must be joined back on after shrinkage, keyed by `(player_id, var_name, season_name)`. Replace the whole `ds_by_season` block with:

```r
  ds_breakdown_lookup <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::select(player_id, var_name, season_name, DataScoreBreakdown)

  ds_by_season <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      source_role     = role_group_matchbased,
      source_minutes  = player_season_minutes,
      source_coverage = DataScore_Base_cobertura,
      DataScore_shown = DataScore_previo
    ) |>
    apply_confidence_shrinkage(role_means_ds) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      DataScore = round(scale_datascore(DataScore_final), 1),
      Cobertura_DataScore = round(C * 100, 1)
    ) |>
    dplyr::left_join(ds_breakdown_lookup, by = c("player_id", "var_name", "season_name"))
```

The `ame_by_season` block (lines 239-251) currently ends:
```r
  ame_by_season <- dplyr::bind_cols(
    rows |> dplyr::select(player_id, var_name, season_name, AmeScore_Base_cobertura),
    ame_gate
  ) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      DataScoreAmerica = round(scale_amescore(AmeScore_final), 1),
      Cobertura_DataScoreAmerica = AmeScore_Base_cobertura
    )
```
Replace with (same pattern -- `dplyr::bind_cols` already has `rows`' full row set in the same order, so `AmeScoreBreakdown` can be selected alongside `AmeScore_Base_cobertura` directly, no separate join needed):
```r
  ame_by_season <- dplyr::bind_cols(
    rows |> dplyr::select(player_id, var_name, season_name, AmeScore_Base_cobertura, AmeScoreBreakdown),
    ame_gate
  ) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      DataScoreAmerica = round(scale_amescore(AmeScore_final), 1),
      Cobertura_DataScoreAmerica = AmeScore_Base_cobertura,
      AmeScoreBreakdown
    )
```

The `amescore_plus` block (lines 255-265) currently ends:
```r
  amescore_plus <- compute_amescore_plus_all_roles(dat_v2) |>
    dplyr::distinct(var_name, player_id, .keep_all = TRUE) |>
    dplyr::transmute(
      var_name, player_id,
      DataScoreAmericaPlus = ifelse(
        AmeScorePlus_published,
        round(scale_amescore(AmeScorePlus_final), 1),
        NA_real_
      ),
      DataScoreAmericaPlus_published = AmeScorePlus_published
    )
```
Replace with:
```r
  amescore_plus <- compute_amescore_plus_all_roles(dat_v2) |>
    dplyr::distinct(var_name, player_id, .keep_all = TRUE) |>
    dplyr::transmute(
      var_name, player_id,
      DataScoreAmericaPlus = ifelse(
        AmeScorePlus_published,
        round(scale_amescore(AmeScorePlus_final), 1),
        NA_real_
      ),
      DataScoreAmericaPlus_published = AmeScorePlus_published,
      AmeScorePlusBreakdown
    )
```

Finally, `result`'s own closing `dplyr::select(...)` (lines 267-272) currently reads:
```r
  result <- ds_by_season |>
    dplyr::left_join(ame_by_season, by = c("player_id", "var_name", "season_name")) |>
    dplyr::left_join(amescore_plus, by = c("player_id", "var_name")) |>
    dplyr::select(player_id, season_name, DataScore, Cobertura_DataScore,
                  DataScoreAmerica, Cobertura_DataScoreAmerica,
                  DataScoreAmericaPlus, DataScoreAmericaPlus_published)
```
Replace with:
```r
  result <- ds_by_season |>
    dplyr::left_join(ame_by_season, by = c("player_id", "var_name", "season_name")) |>
    dplyr::left_join(amescore_plus, by = c("player_id", "var_name")) |>
    dplyr::select(player_id, season_name, DataScore, Cobertura_DataScore, DataScoreBreakdown,
                  DataScoreAmerica, Cobertura_DataScoreAmerica, AmeScoreBreakdown,
                  DataScoreAmericaPlus, DataScoreAmericaPlus_published, AmeScorePlusBreakdown)
```

- [ ] **Step 2: Add the three fields to `SEASON_SYNC_SCORE_COLS`**

In `sync_season_rows_to_mongo.R`, change:

```r
SEASON_SYNC_SCORE_COLS <- c("DataScore", "Cobertura_DataScore",
                            "DataScoreAmerica", "Cobertura_DataScoreAmerica",
                            "DataScoreAmericaPlus", "DataScoreAmericaPlus_published")
```

to:

```r
SEASON_SYNC_SCORE_COLS <- c("DataScore", "Cobertura_DataScore", "DataScoreBreakdown",
                            "DataScoreAmerica", "Cobertura_DataScoreAmerica", "AmeScoreBreakdown",
                            "DataScoreAmericaPlus", "DataScoreAmericaPlus_published", "AmeScorePlusBreakdown")
```

- [ ] **Step 3: Verify**

```r
Rscript -e '
suppressWarnings(suppressMessages(source("datascore_v2.R")))
result <- get_datascore_v2_scores_by_season()
stopifnot(all(c("DataScoreBreakdown", "AmeScoreBreakdown", "AmeScorePlusBreakdown") %in% names(result)))
have_ds <- !sapply(result$DataScoreBreakdown, is.null)
cat(sprintf("PASS: %d/%d season rows have a non-null DataScoreBreakdown\n", sum(have_ds), nrow(result)))
stopifnot(sum(have_ds) > 0)
'
```

Expected: `PASS: N/M season rows have a non-null DataScoreBreakdown` with `N > 0`.

- [ ] **Step 4: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add datascore_v2.R sync_season_rows_to_mongo.R
git commit -m "$(cat <<'EOF'
Thread score breakdowns through the per-season sync path

get_datascore_v2_scores_by_season() now retains DataScoreBreakdown/
AmeScoreBreakdown/AmeScorePlusBreakdown (already computed by the same
build_scored_rows()/compute_amescore_plus_all_roles() calls this
function already makes -- nothing new computed, just not dropped).
SEASON_SYNC_SCORE_COLS extended so sync_season_rows_to_mongo.R carries
them into sb_sc_metrics_by_season.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: `ScoreBreakdownPanel.jsx` — new floating panel component

**Files:**
- Create: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/ScoreBreakdownPanel.jsx`

**Interfaces:**
- Produces: default export `ScoreBreakdownPanel({ scoreLabel, playerName, team, games, minutes, breakdownsBySeason, onClose })`:
  - `scoreLabel`: string, e.g. `"DataScore"`.
  - `breakdownsBySeason`: object, season label (or `"Acumulado"`) → breakdown array (the `category/capacityScore/capacityWeight/metrics[]` shape Task 1 produces) or `null`/`undefined` if that score has no breakdown for that season.
  - `onClose`: callback, closes the panel.

- [ ] **Step 1: Create the component**

```jsx
"use client";

import { useState } from "react";

// Degradado continuo rojo -> dorado por percentil, EXCLUSIVO de este
// panel (pedido explícito del usuario, 2026-11-05) -- el resto de la app
// sigue con su convención de 3 tonos (rojo/dim/verde), sin tocar acá.
function percentileColor(pct) {
  if (pct == null || !Number.isFinite(pct)) return "var(--text-faint)";
  const t = Math.max(0, Math.min(100, pct)) / 100;
  // rojo (#e5484d) -> dorado (var(--gold-a), aprox #d4af37) interpolado en RGB.
  const from = [229, 72, 77];
  const to = [212, 175, 55];
  const rgb = from.map((c, i) => Math.round(c + (to[i] - c) * t));
  return `rgb(${rgb[0]}, ${rgb[1]}, ${rgb[2]})`;
}

// "player_season_npg_90" -> "Npg 90" como fallback legible -- este panel
// no reusa el LABEL_MAP de lib/positionMetrics.js a propósito: esos
// mapas están indexados por los grupos de posición de Perfil (Portero/
// Defensa Central/Lateral/Volante/Extremo/Delantero), una taxonomía
// DISTINTA de los roles de DataScore/AmeScore (Interior/Mediapunta/
// Central/Lateral-Carrilero/Medio de Contención/Delantero/Volante-
// Extremo, ver ROLE_SCORE_DEFS en base_scores.R) -- no hay una forma
// confiable de mapear un rol al otro para elegir qué entrada de
// LABEL_MAP usar. Un fallback derivado de la propia clave del metric,
// sin depender de esa taxonomía, evita el problema por completo.
function fallbackLabel(key) {
  return key
    .replace(/^player_season_/, "")
    .replace(/_90$/, "")
    .replace(/_/g, " ")
    .replace(/\b\w/g, (c) => c.toUpperCase());
}

function MetricRow({ metric }) {
  const pct = metric.percentile;
  const color = percentileColor(pct);
  return (
    <div className="sc-breakdown-metric-row">
      <div className="sc-breakdown-metric-label">
        {fallbackLabel(metric.key)}
        {metric.source === "physical" && (
          <span className="sc-fichameta" style={{ marginLeft: 6 }}>
            (físico)
          </span>
        )}
      </div>
      <div className="sc-breakdown-metric-value">{metric.value != null ? metric.value : "—"}</div>
      <div className="sc-breakdown-metric-pct" style={{ color }}>
        {pct != null ? pct.toFixed(1) : "—"}
      </div>
      <div className="sc-breakdown-metric-bar">
        <div
          className="sc-breakdown-metric-bar-fill"
          style={{ width: `${pct != null ? Math.max(0, Math.min(100, pct)) : 0}%`, background: color }}
        />
      </div>
    </div>
  );
}

function CategorySection({ category }) {
  return (
    <div className="sc-breakdown-category">
      <div className="sc-breakdown-category-head">
        <span className="sc-breakdown-category-name">{category.category}</span>
        <span className="sc-breakdown-category-score">
          {category.capacityScore != null ? category.capacityScore.toFixed(1) : "—"}
          <span className="sc-fichameta" style={{ marginLeft: 6 }}>
            (peso {Math.round(category.capacityWeight * 100)}%)
          </span>
        </span>
      </div>
      {category.metrics.map((m) => (
        <MetricRow key={m.key} metric={m} />
      ))}
    </div>
  );
}

export default function ScoreBreakdownPanel({ scoreLabel, playerName, team, games, minutes, breakdownsBySeason, onClose }) {
  const seasons = Object.keys(breakdownsBySeason || {});
  const [season, setSeason] = useState(seasons.includes("Acumulado") ? "Acumulado" : seasons[0]);
  const breakdown = breakdownsBySeason?.[season];

  return (
    <div className="sc-breakdown-overlay" onClick={onClose}>
      <div className="sc-breakdown-panel" onClick={(e) => e.stopPropagation()}>
        <div className="sc-breakdown-panel-head">
          <div>
            <div className="sc-breakdown-panel-title">{scoreLabel} — {playerName}</div>
            <div className="sc-fichameta">
              {team || "—"} · {games != null ? `${games} partidos` : "—"} · {minutes != null ? `${minutes} min` : "—"}
            </div>
          </div>
          <button type="button" className="sc-btn ghost small" onClick={onClose}>
            Cerrar
          </button>
        </div>

        {seasons.length > 1 && (
          <div className="sc-breakdown-season-toggle">
            {seasons.map((s) => (
              <button
                key={s}
                type="button"
                className={`sc-btn ghost small ${s === season ? "active" : ""}`}
                onClick={() => setSeason(s)}
              >
                {s}
              </button>
            ))}
          </div>
        )}

        {breakdown && breakdown.length > 0 ? (
          <div className="sc-breakdown-body">
            {breakdown.map((cat) => (
              <CategorySection key={cat.category} category={cat} />
            ))}
          </div>
        ) : (
          <div className="sc-fichameta" style={{ marginTop: 16 }}>
            No hay detalle disponible para {scoreLabel} en esta temporada.
          </div>
        )}
      </div>
    </div>
  );
}
```

- [ ] **Step 2: Add the new CSS classes**

In `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/globals.css`, add (near the other `.sc-fichameta`/`.sc-table` rules):

```css
/* ScoreBreakdownPanel.jsx -- overlay flotante (no pantalla completa) para
   el detalle de DataScore/AmeScore/AmeScore+, abierto con click sobre el
   tag del score (el popover de hover existente sigue intacto, es un
   elemento/evento distinto). */
.sc-breakdown-overlay {
  position: fixed;
  inset: 0;
  background: rgba(0, 0, 0, 0.55);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 200;
}
.sc-breakdown-panel {
  background: var(--surface);
  border: 1px solid var(--border);
  border-radius: 12px;
  padding: 24px;
  width: min(640px, 92vw);
  max-height: 85vh;
  overflow-y: auto;
}
.sc-breakdown-panel-head {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
  margin-bottom: 12px;
}
.sc-breakdown-panel-title {
  font-size: 18px;
  font-weight: 700;
  color: var(--text);
}
.sc-breakdown-season-toggle {
  display: flex;
  gap: 6px;
  margin-bottom: 16px;
}
.sc-breakdown-category {
  margin-bottom: 20px;
}
.sc-breakdown-category-head {
  display: flex;
  justify-content: space-between;
  font-weight: 700;
  color: var(--text);
  margin-bottom: 6px;
  padding-bottom: 4px;
  border-bottom: 1px solid var(--border);
}
.sc-breakdown-metric-row {
  display: grid;
  grid-template-columns: 1fr 72px 56px 80px;
  align-items: center;
  gap: 8px;
  padding: 4px 0;
  font-size: 13px;
}
.sc-breakdown-metric-label {
  color: var(--text-dim);
}
.sc-breakdown-metric-value {
  text-align: right;
  color: var(--text);
}
.sc-breakdown-metric-pct {
  text-align: right;
  font-weight: 700;
}
.sc-breakdown-metric-bar {
  height: 6px;
  border-radius: 3px;
  background: var(--surface3);
  overflow: hidden;
}
.sc-breakdown-metric-bar-fill {
  height: 100%;
}
```

- [ ] **Step 3: Verify**

Run `npm run build` from `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard` and confirm it compiles clean (this component isn't wired into any page yet -- that's Task 6 -- so confirm only that the file itself is syntactically valid and unused-but-present doesn't break the build).

- [ ] **Step 4: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git add "app/jugadores/[id]/ScoreBreakdownPanel.jsx" app/globals.css
git commit -m "$(cat <<'EOF'
Add ScoreBreakdownPanel.jsx: floating score breakdown with continuous gradient

New component, not yet wired into any page. Season toggle (Acumulado
default), one section per capacity/category, each metric showing
value/percentile with a continuous red->gold gradient bar -- scoped to
this panel only, per explicit user request; the rest of the app keeps
its existing 3-tier percentile color convention.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: Wire the panel into the player profile

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/page.jsx`
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerProfileView.jsx`

**Interfaces:**
- Consumes: `ScoreBreakdownPanel` (Task 5). `sbSc.DataScoreBreakdown`/`AmeScoreBreakdown`/`AmeScorePlusBreakdown` (Acumulado, Tasks 1-3) and the same three fields on each of this player's `SbScMetricBySeason` docs (Task 4) -- `seasonDocs` is already fetched in `page.jsx` for the existing season dropdown/ficha-timeline features, reused here, no new query.
- Produces: `meta.dataScoreBreakdowns`/`meta.ameScoreBreakdowns`/`meta.ameScorePlusBreakdowns` -- each an object `{ "Acumulado": <breakdown|null>, "2025/2026": <breakdown|null>, ... }` ready to pass straight into `ScoreBreakdownPanel`'s `breakdownsBySeason` prop.

- [ ] **Step 1: Build the per-score `breakdownsBySeason` objects in `page.jsx`**

In `app/jugadores/[id]/page.jsx`, inside the existing `meta` object literal (same block Task 3 of the ficha-tracking plan already extended with `perfilGroup`/`perfilScores`), add:

```js
    // Breakdown de DataScore/AmeScore/AmeScore+ (build_score_breakdown(),
    // scout_dashboard) -- Acumulado + cada temporada de seasonDocs (ya
    // resuelto arriba para el dropdown de temporada, reusado acá sin
    // consulta nueva). null para una temporada sin breakdown propio (no
    // "sin ficha" para ese score esa temporada).
    dataScoreBreakdowns: {
      Acumulado: sbSc?.DataScoreBreakdown ?? null,
      ...Object.fromEntries(seasonDocs.map((d) => [d.season_name, d.DataScoreBreakdown ?? null])),
    },
    ameScoreBreakdowns: {
      Acumulado: sbSc?.AmeScoreBreakdown ?? null,
      ...Object.fromEntries(seasonDocs.map((d) => [d.season_name, d.AmeScoreBreakdown ?? null])),
    },
    ameScorePlusBreakdowns: {
      Acumulado: sbSc?.AmeScorePlusBreakdown ?? null,
      ...Object.fromEntries(seasonDocs.map((d) => [d.season_name, d.AmeScorePlusBreakdown ?? null])),
    },
    // Partidos jugados (no 90s jugados, ver meta.nineties arriba -- son
    // dos métricas distintas) -- ya existe como columna cruda en el
    // documento de Mongo (lib/sbSc.js la lista como "Partidos
    // Disputados"), solo no estaba expuesta en `meta` todavía.
    games: sbSc?.player_season_appearances ?? null,
```

No new props need threading on `<PlayerProfileView>` beyond what's already there -- `meta={meta}` already carries these.

- [ ] **Step 2: Add the click handler + panel state in `PlayerProfileView.jsx`**

Add the import alongside the other same-directory imports:

```js
import ScoreBreakdownPanel from "./ScoreBreakdownPanel";
```

Add new state near the component's other `useState` calls (e.g. near `comparePlayer`):

```js
  const [openBreakdown, setOpenBreakdown] = useState(null); // "DataScore" | "AmeScore" | "AmeScore+" | null
```

Replace each of the three `.sc-tag` spans (lines ~687-745) to add an `onClick` -- for example, the DataScore tag currently reads:

```jsx
                <span
                  className="sc-tag"
                  style={{ color: meta.dataScore < 35 ? "var(--red-a)" : meta.dataScore > 70 ? "var(--green-a)" : "var(--text-dim)" }}
                >
                  DataScore {meta.dataScore.toFixed(1)}
                </span>
```

becomes:

```jsx
                <span
                  className="sc-tag"
                  style={{
                    color: meta.dataScore < 35 ? "var(--red-a)" : meta.dataScore > 70 ? "var(--green-a)" : "var(--text-dim)",
                    cursor: "pointer",
                  }}
                  onClick={() => setOpenBreakdown("DataScore")}
                >
                  DataScore {meta.dataScore.toFixed(1)}
                </span>
```

Apply the identical pattern to the AmeScore tag (`onClick={() => setOpenBreakdown("AmeScore")}`) and the AmeScore+ tag (`onClick={() => setOpenBreakdown("AmeScore+")}`) -- same `cursor: "pointer"` addition, same click handler shape, different state value. The existing `<ScorePercentilePopover>` sibling elements (hover-triggered) are untouched.

- [ ] **Step 3: Render the panel conditionally**

Near the end of the component's JSX (e.g. right before its closing return's final `</div>`, alongside other modal-style conditionals already in this file), add:

```jsx
      {openBreakdown && (
        <ScoreBreakdownPanel
          scoreLabel={openBreakdown}
          playerName={player.nombre}
          team={player.clubActual}
          games={meta.games}
          minutes={meta.minutos}
          breakdownsBySeason={
            openBreakdown === "DataScore"
              ? meta.dataScoreBreakdowns
              : openBreakdown === "AmeScore"
                ? meta.ameScoreBreakdowns
                : meta.ameScorePlusBreakdowns
          }
          onClose={() => setOpenBreakdown(null)}
        />
      )}
```

- [ ] **Step 4: Verify**

Run `npm run build` and confirm it passes. Live-data verification (whether a real player's panel shows a real, non-empty breakdown) isn't possible until Task 7 deploys the R-side changes -- rely on the build pass plus a careful read-through of the wiring, consistent with this session's established precedent for this exact situation (the data genuinely doesn't exist yet at this point in the task sequence).

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git add app/jugadores/\[id\]/page.jsx app/jugadores/\[id\]/PlayerProfileView.jsx
git commit -m "$(cat <<'EOF'
Wire ScoreBreakdownPanel into the player profile

Clicking a DataScore/AmeScore/AmeScore+ tag opens its own breakdown
panel (season toggle, Acumulado default); the existing hover popover
is untouched -- same elements, two independent event handlers.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 7: Deploy + live verification

**Files:** none (operational task — push, trigger CI, verify).

- [ ] **Step 1: Push `scout_dashboard`'s commits (Tasks 1-4), batched with the already-committed 24/25 fix**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git push origin main
```

- [ ] **Step 2: Trigger the CI pipeline and wait for both jobs to complete**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
gh workflow run deploy.yml
```

Poll with `gh run list --limit 1` / `gh run watch <run-id> --exit-status` until `sync-and-deploy` completes. If either job fails, read `gh run view <run-id> --log-failed` -- if the failure traces to this plan's own changes (Tasks 1-4) or the already-batched 24/25 fix, that is a real defect to fix and re-verify, not something to silently route around.

- [ ] **Step 3: Confirm the breakdown fields landed in Mongo correctly, and confirm the 24/25 fix worked**

Spot-check a few synced `sb_sc_metrics` and `sb_sc_metrics_by_season` documents: `DataScoreBreakdown`/`AmeScoreBreakdown`/`AmeScorePlusBreakdown` should be proper nested arrays of category objects (not flattened, not `"NA"` strings, not array-wrapped scalars). Separately, confirm the batched 24/25 fix actually worked this time: at least one league's `sb_sc_metrics_by_season` should now have `season_name == "2024/2025"` (or `"2024"`) rows that didn't exist before.

- [ ] **Step 4: Push `ca_scouting_dashboard`'s commits (Tasks 5-6)**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git push origin main
```

Confirm with the user before pushing, per this repo's established per-instance push confirmation practice.

- [ ] **Step 5: Live verification**

Open a real player's profile (ideally one per role group, since the breakdown's category set differs by role -- Delantero, Central, Lateral/Carrilero, Interior/Mediapunta, Medio de Contención, Volante/Extremo) and click each of DataScore/AmeScore/AmeScore+ (where available). Confirm: the panel opens, shows the right categories for that role, metric values/percentiles look plausible, the color gradient is continuous (not 3-tier), the season toggle offers the seasons that player actually has, and switching seasons shows a different breakdown. Confirm a player with no AmeScore+ (most players) shows no AmeScore+ tag at all (unchanged existing behavior) rather than an empty panel.

## Execution Handoff

After saving this plan, offer the user the subagent-driven-development vs. inline-execution choice per the writing-plans skill. Tasks 1-4 live in `scout_dashboard` and are strictly sequential (each depends on the previous task's exact column/variable names). Tasks 5-6 live in `ca_scouting_dashboard`; Task 5 has no dependency on Tasks 1-4 (it's a self-contained new component, buildable/reviewable in parallel with the R-side work if desired, though the skill's own rule against parallel implementer dispatch means sequential is still the controller's actual execution order) -- Task 6 depends on Task 5's component export. Task 7 spans both repos and must run last, batched with the already-committed `dashboard_scout.R` 24/25 fix per the user's explicit request.
