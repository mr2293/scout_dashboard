# Per-season DataScore/AmeScore/AmeScore+ Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Compute DataScore/AmeScore(DataScoreAmerica)/AmeScore+(DataScoreAmericaPlus) at `(player, season)` granularity, sync them onto the existing per-season Mongo collection, and let `ca_scouting_dashboard`'s Gráficos view filter on them by a specific season the same way it already filters raw stat metrics.

**Architecture:** The DataScore v2 pipeline already scores every `(player_id, league, season)` row internally in `build_scored_rows()`; the existing `get_datascore_v2_scores()` only looks career-level because `apply_transition_gate()` collapses that table to one row per player right before scoring finishes. A new sibling function reuses the same per-row table and the same global role-mean baselines, skips the collapse, and returns one row per `(player_id, season_name)`. That output gets folded into the existing `sync_season_rows_to_mongo.R` pass via a `left_join`. The frontend requires no logic change beyond removing a now-stale warning message — `page.jsx`'s Mongo query is already generic over whatever fields exist on a document.

**Tech Stack:** R (dplyr, tibble, mongolite), Next.js/React (`ca_scouting_dashboard`), MongoDB.

**Spec:** `docs/superpowers/specs/2026-11-01-per-season-datascore-design.md`

## Global Constraints

- The existing `get_datascore_v2_scores()` (Acumulado path) must not be modified. Its output must be byte-for-byte identical before and after this plan.
- Per-season rows must shrink toward the *same* global `role_means_ds`/`role_means_ame` baselines Acumulado already computes — not a baseline computed separately per season.
- No new Mongo collection. Scores become new fields on the existing `sb_sc_metrics_by_season` documents.
- Join key between the new per-season score table and `season_rows` is `(player_id, season_name)` — both sides already use StatsBomb's `player_id` (character) at that point in the pipeline, before the TM-crosswalk step converts to `transfermarkt_id`.

---

## Task 1: `get_datascore_v2_scores_by_season()` in `datascore_v2.R`

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/datascore_v2.R` (append after line 145, the closing `}` of `get_datascore_v2_scores()`)
- Test: `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/tmp/smoke_test_by_season.R` (scratch script, not committed — this repo validates pipeline scripts via standalone `Rscript` runs against saved fixtures, not a `testthat` suite; see the `if (sys.nframe() == 0)` blocks in `confidence_shrinkage.R`/`ame_score_gate.R` for the existing convention)

**Interfaces:**
- Consumes: `load_scout_data()` (`load_scout_data.R`), `build_scored_rows(dat, tiers)` (`competition_strength.R`), `apply_confidence_shrinkage(gated, role_means, k_90s)` (`confidence_shrinkage.R`), `apply_ame_score_gate(ame_base, gate_capacity_v, role, techo_rol, threshold)` (`ame_score_gate.R`), `compute_amescore_plus_all_roles(dat)` (`ame_score_plus.R`), `GATE_CAPACITY` (constant, `ame_score_gate.R`, value `"Presión posicional"`), `scale_datascore()`/`scale_amescore()` (`scale_mapping.R`) — all already `source()`-d into this file's namespace via the existing `source("ame_score_plus.R")`/`source("scale_mapping.R")` lines at the top of `datascore_v2.R`.
- Produces: `get_datascore_v2_scores_by_season()` — a function taking no arguments, returning a tibble with columns `player_id` (character), `season_name` (character), `DataScore` (numeric 0-100), `Cobertura_DataScore` (numeric 0-100), `DataScoreAmerica` (numeric 0-100), `Cobertura_DataScoreAmerica` (numeric 0-100), `DataScoreAmericaPlus` (numeric 0-100 or `NA`), `DataScoreAmericaPlus_published` (logical). One row per real `(player_id, season_name)` pair the pipeline has StatsBomb data for. Task 2 joins onto this by `(player_id, season_name)`.

- [ ] **Step 1: Read the current end of the file to confirm the exact insertion point**

Run: `tail -5 /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/datascore_v2.R`
Expected output ends with:
```
  .datascore_v2_cache <<- result
  result
}
```
If it doesn't match, STOP and re-read the whole file before proceeding — line numbers below assume this exact current state.

- [ ] **Step 2: Append the new function**

Add this to the end of `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/datascore_v2.R`:

```r

# ============================================================
# get_datascore_v2_scores_by_season()
#
# Sibling to get_datascore_v2_scores() above -- NOT a modification of
# it, and does not call it. Returns DataScore/DataScoreAmerica/
# DataScoreAmericaPlus at (player_id, season_name) granularity instead
# of career-blended Acumulado.
#
# The per-row scoring this needs already exists inside
# get_datascore_v2_scores() as the intermediate `rows` table --
# build_scored_rows() scores every (player_id, league, season) row
# individually; get_datascore_v2_scores() only looks career-level
# because apply_transition_gate() collapses `rows` to one row per
# player right before confidence shrinkage / the AmeScore gate run.
# This function reuses `rows` directly and skips that collapse.
#
# Role-mean baselines (role_means_ds/role_means_ame) are recomputed
# here with the EXACT SAME expression get_datascore_v2_scores() uses --
# same global pool, so a player's per-season score and their Acumulado
# score shrink toward the same reference point. Deliberately not
# cached/shared between the two functions: each is a 3-line
# group_by/summarise over `rows`, cheaper than adding a shared-state
# dependency between two functions that must otherwise stay independent
# (see the "Acumulado must stay untouched" constraint in the design
# spec, docs/superpowers/specs/2026-11-01-per-season-datascore-design.md).
#
# apply_ame_score_gate() is called directly on the full `rows` table
# here -- the exact same call ame_score_gate.R's own validation harness
# (its `if (sys.nframe() == 0)` block) already makes; this just reuses
# that already-exercised pattern instead of introducing a new one.
# ============================================================

.datascore_v2_by_season_cache <- NULL

get_datascore_v2_scores_by_season <- function(force_refresh = FALSE) {
  if (!force_refresh && !is.null(.datascore_v2_by_season_cache)) return(.datascore_v2_by_season_cache)

  tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
  dat_v2 <- load_scout_data()
  rows <- build_scored_rows(dat_v2, tiers)

  # A player can have two different var_name rows under the SAME
  # season_name -- e.g. a mid-season transfer between two leagues that
  # share the same Aug-May calendar label (confirmed live 2026-11-01:
  # player 10003 had two separate 2025/2026 rows with different
  # DataScore). Collapse to exactly one row per (player_id, season_name)
  # by keeping the higher-minutes one -- same tiebreak
  # apply_transition_gate() (league_transition_gate.R) already uses for
  # its own "which row wins" decision ("Ties... broken by minutes
  # played, most first"), so this stays consistent with precedent
  # already established in this exact pipeline rather than inventing a
  # new rule.
  rows <- rows |>
    dplyr::slice_max(player_season_minutes, n = 1, by = c(player_id, season_name), with_ties = FALSE)

  role_means_ds <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::group_by(role_group_matchbased) |>
    dplyr::summarise(mean_ds = mean(DataScore_previo), .groups = "drop") |>
    tibble::deframe()

  role_means_ame <- rows |>
    dplyr::filter(!is.na(AmeScore_Base)) |>
    dplyr::group_by(role_group_matchbased) |>
    dplyr::summarise(mean_ame = mean(AmeScore_Base), .groups = "drop") |>
    tibble::deframe()

  # DataScore: rename each row's own columns to what
  # apply_confidence_shrinkage() expects (source_role/source_minutes/
  # source_coverage/DataScore_shown), instead of routing through
  # apply_transition_gate()'s one-row-per-player collapse. Every
  # (player_id, league, season) row gets shrunk independently.
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
    )

  # AmeScore: apply_ame_score_gate() directly on every row of `rows` --
  # no join needed, AmeScore_Base/GATE_CAPACITY already live on `rows`
  # itself (unlike get_datascore_v2_scores(), which has to re-join them
  # back on after the transition gate already dropped them).
  ame_gate <- apply_ame_score_gate(
    rows$AmeScore_Base, rows[[GATE_CAPACITY]],
    rows$role_group_matchbased, role_means_ame
  )
  ame_by_season <- dplyr::bind_cols(
    rows |> dplyr::select(player_id, var_name, season_name, AmeScore_Base_cobertura),
    ame_gate
  ) |>
    dplyr::transmute(
      player_id, var_name, season_name,
      DataScoreAmerica = round(scale_amescore(AmeScore_final), 1),
      Cobertura_DataScoreAmerica = AmeScore_Base_cobertura
    )

  # AmeScore+: compute_amescore_plus_all_roles() is already keyed by
  # (var_name, player_id) and never collapses on its own -- used as-is.
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

  result <- ds_by_season |>
    dplyr::left_join(ame_by_season, by = c("player_id", "var_name", "season_name")) |>
    dplyr::left_join(amescore_plus, by = c("player_id", "var_name")) |>
    dplyr::select(player_id, season_name, DataScore, Cobertura_DataScore,
                  DataScoreAmerica, Cobertura_DataScoreAmerica,
                  DataScoreAmericaPlus, DataScoreAmericaPlus_published)

  .datascore_v2_by_season_cache <<- result
  result
}
```

- [ ] **Step 3: Syntax-check the file**

Run: `cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard && Rscript -e 'parse("datascore_v2.R"); cat("PARSE OK\n")'`
Expected: last line of output is `PARSE OK` (renv/cache warnings above it are pre-existing noise, not failures).

- [ ] **Step 4: Write and run the local smoke test**

This validates the function runs end-to-end and produces sane output against whatever fixtures are currently on disk (`data/scout_joined.rds`, `data/role_eligibility_matchbased.rds`, `data/competition_strength_tiers.csv` — these may be several weeks stale relative to the live pipeline; this step checks the *mechanism*, not current production numbers. Final confirmation of real numbers happens in Task 4's CI run.)

Create `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/tmp/smoke_test_by_season.R`:

```r
setwd("/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard")
source("datascore_v2.R")

by_season <- get_datascore_v2_scores_by_season()

cat(sprintf("rows: %d\n", nrow(by_season)))
cat(sprintf("distinct player_id: %d\n", dplyr::n_distinct(by_season$player_id)))
cat(sprintf("distinct season_name: %s\n", paste(sort(unique(by_season$season_name)), collapse = ", ")))

dup_check <- by_season |> dplyr::count(player_id, season_name) |> dplyr::filter(n > 1)
cat(sprintf("duplicate (player_id, season_name) pairs: %d\n", nrow(dup_check)))

cat(sprintf("DataScore range: [%.1f, %.1f], n_valid=%d/%d\n",
            min(by_season$DataScore, na.rm = TRUE), max(by_season$DataScore, na.rm = TRUE),
            sum(!is.na(by_season$DataScore)), nrow(by_season)))
cat(sprintf("DataScoreAmerica range: [%.1f, %.1f], n_valid=%d/%d\n",
            min(by_season$DataScoreAmerica, na.rm = TRUE), max(by_season$DataScoreAmerica, na.rm = TRUE),
            sum(!is.na(by_season$DataScoreAmerica)), nrow(by_season)))
cat(sprintf("DataScoreAmericaPlus published: %d/%d\n",
            sum(by_season$DataScoreAmericaPlus_published, na.rm = TRUE), nrow(by_season)))

# Cross-check against Acumulado for one real player present in both --
# not expected to match (different confidence/shrinkage denominators),
# but should be in the same ballpark (within ~25 points) as a sanity
# bound, not wildly different.
acumulado <- get_datascore_v2_scores()
sample_id <- by_season$player_id[which(!is.na(by_season$DataScore))[1]]
acum_row <- acumulado[acumulado$player_id == sample_id, ]
season_rows_for_id <- by_season[by_season$player_id == sample_id, ]
cat(sprintf("\nsample player_id=%s\nAcumulado DataScore=%s\nper-season DataScore(s): %s\n",
            sample_id,
            if (nrow(acum_row)) acum_row$DataScore[1] else "NA",
            paste(sprintf("%s=%.1f", season_rows_for_id$season_name, season_rows_for_id$DataScore), collapse = ", ")))
```

Run: `cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard && Rscript tmp/smoke_test_by_season.R 2>&1 | tail -20`

Expected:
- `duplicate (player_id, season_name) pairs: 0` — if this is nonzero, STOP; the join-key-safety reasoning in the spec was wrong and needs revisiting before Task 2.
- `rows` in the thousands (comparable to the ~29,800 total docs already in `sb_sc_metrics_by_season` as of 2026-11-01, though this smoke test's `rows` count is pre-TM-crosswalk so will be larger).
- Both DataScore/DataScoreAmerica ranges inside roughly `[0, 100]` with a meaningful fraction `n_valid > 0`.
- The sample player's per-season DataScore(s) within roughly 25 points of their Acumulado DataScore — not identical (different math), just not nonsensical.

- [ ] **Step 5: Delete the scratch test file**

Run: `rm /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/tmp/smoke_test_by_season.R`

(Not committed — `tmp/` is this repo's scratch directory, already used elsewhere this session for throwaway checks.)

- [ ] **Step 6: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add datascore_v2.R
git commit -m "$(cat <<'EOF'
Add get_datascore_v2_scores_by_season(): DataScore/AmeScore/AmeScore+ per (player, season)

Sibling to get_datascore_v2_scores() -- reuses the same per-row `rows`
table build_scored_rows() already computes and the same global
role-mean baselines, but skips apply_transition_gate()'s collapse to
one row per player, so every (player, league, season) row gets its own
shrunk DataScore/AmeScore and its own AmeScore+ lookup. Acumulado's own
get_datascore_v2_scores() is untouched.

See docs/superpowers/specs/2026-11-01-per-season-datascore-design.md.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 2: Sync the new columns in `sync_season_rows_to_mongo.R`

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/sync_season_rows_to_mongo.R`

**Interfaces:**
- Consumes: `get_datascore_v2_scores_by_season()` (Task 1) — returns `player_id`, `season_name`, `DataScore`, `Cobertura_DataScore`, `DataScoreAmerica`, `Cobertura_DataScoreAmerica`, `DataScoreAmericaPlus`, `DataScoreAmericaPlus_published`.
- Produces: six new fields on every document in the `sb_sc_metrics_by_season` Mongo collection, matching the field names `ca_scouting_dashboard`'s `lib/graficos.js` already expects (`DataScore`, `DataScoreAmerica`, `DataScoreAmericaPlus`, `DataScoreAmericaPlus_published` — confirmed exact match against `GRAFICOS_METRIC_OPTIONS`/`PUBLISHED_FLAG` in that file).

- [ ] **Step 1: Replace the stale "why these columns aren't here" comment and add the new column list**

In `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/sync_season_rows_to_mongo.R`, find this block (currently lines 103-121):

```r
# Added 2026-10-28: player_season_minutes wasn't synced here at all before
# (confirmed live -- no field on sb_sc_metrics_by_season docs contains
# "minut"), despite several Next.js features needing it per-season (see
# PlayerProfileView.jsx's minutes-row, and the Gráficos min-minutes filter
# investigated the same day). DataScore/DataScoreAmerica/DataScoreAmericaPlus/
# Perfil are deliberately NOT added here even though Gráficos also wants them
# per-season -- they don't exist at this granularity anywhere in the
# pipeline. build_database_master() (app.R) computes them by calling
# add_datascore()/add_america_fit()/assign_player_profiles() on
# get_all_players_df(), the career-BLENDED Acumulado table (one row per
# player, dedup_transfers() weighted-averaged across every season they've
# ever played) -- there's no per-season equivalent of that scoring step to
# sync from. Making DataScore/AmeScore truly season-specific would mean
# teaching datascore_v2.R to compute them against a single season's stats
# and percentile pool instead of a career blend -- a real change to the
# scoring methodology itself, not a sync-script column addition. Flagged to
# the user 2026-10-28 rather than attempted here.
SEASON_SYNC_KEY_COLS <- c("player_name", "transfermarkt_id", "season_name", "position_group", "Liga",
                          "player_season_minutes")
```

Replace it with:

```r
# Added 2026-10-28: player_season_minutes wasn't synced here at all before
# (confirmed live -- no field on sb_sc_metrics_by_season docs contains
# "minut"), despite several Next.js features needing it per-season (see
# PlayerProfileView.jsx's minutes-row, and the Gráficos min-minutes filter
# investigated the same day).
SEASON_SYNC_KEY_COLS <- c("player_name", "transfermarkt_id", "season_name", "position_group", "Liga",
                          "player_season_minutes")

# Added 2026-11-01: DataScore/DataScoreAmerica/DataScoreAmericaPlus,
# computed per (player_id, season_name) by
# get_datascore_v2_scores_by_season() (datascore_v2.R) instead of
# career-blended like the rest of this file's columns -- joined onto
# season_rows below, same (player_id, season_name) key dedup_transfers()
# already made unique per season. Perfil/Perfil_secundario/Edad are
# still NOT here -- those remain Acumulado-only, out of scope (see
# docs/superpowers/specs/2026-11-01-per-season-datascore-design.md).
SEASON_SYNC_SCORE_COLS <- c("DataScore", "Cobertura_DataScore",
                            "DataScoreAmerica", "Cobertura_DataScoreAmerica",
                            "DataScoreAmericaPlus", "DataScoreAmericaPlus_published")
```

- [ ] **Step 2: Source `datascore_v2.R` and join the scores onto `season_rows`**

Find this block (currently lines 189-195):

```r
seasons <- raw$season_name |> unique() |> stats::na.omit()
season_rows <- lapply(seasons, function(s) {
  raw |> dplyr::filter(season_name == s) |> dedup_transfers()
}) |> dplyr::bind_rows()

message(sprintf("Per-season rows after dedup_transfers() within each season: %d (seasons: %s)",
                nrow(season_rows), paste(seasons, collapse = ", ")))
```

Immediately after it (still before the `# TEMP DIAGNOSTIC 2026-10-29 (cont.)` block that follows), insert:

```r
message("Computing per-season DataScore/AmeScore/AmeScore+ ...")
suppressWarnings(suppressMessages({
  source("datascore_v2.R")
}))
scores_by_season <- get_datascore_v2_scores_by_season()
message(sprintf("get_datascore_v2_scores_by_season(): %d rows, %d distinct player_id",
                nrow(scores_by_season), dplyr::n_distinct(scores_by_season$player_id)))

season_rows <- season_rows |>
  dplyr::left_join(scores_by_season, by = c("player_id", "season_name"))
```

(This runs before `join_tm_crosswalk()` -- same point in the pipeline every other per-season StatsBomb/SkillCorner column already joins in, and before the TM crosswalk converts the row's identity from StatsBomb `player_id` to `transfermarkt_id`.)

- [ ] **Step 3: Add the new columns to the final `select()`**

Find (currently line 235):

```r
  dplyr::select(dplyr::any_of(c(SEASON_SYNC_KEY_COLS, SEASON_SYNC_SB_COLS, SEASON_SYNC_SC_COLS))) |>
```

Replace with:

```r
  dplyr::select(dplyr::any_of(c(SEASON_SYNC_KEY_COLS, SEASON_SYNC_SB_COLS, SEASON_SYNC_SC_COLS, SEASON_SYNC_SCORE_COLS))) |>
```

- [ ] **Step 4: Syntax-check**

Run: `cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard && Rscript -e 'parse("sync_season_rows_to_mongo.R"); cat("PARSE OK\n")'`
Expected: last line `PARSE OK`.

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add sync_season_rows_to_mongo.R
git commit -m "$(cat <<'EOF'
Sync per-season DataScore/AmeScore/AmeScore+ onto sb_sc_metrics_by_season

Joins get_datascore_v2_scores_by_season()'s output (datascore_v2.R)
onto season_rows by (player_id, season_name), same point in the
pipeline every other per-season StatsBomb/SkillCorner column already
joins in, before the TM-crosswalk step. Six new fields:
DataScore/Cobertura_DataScore/DataScoreAmerica/
Cobertura_DataScoreAmerica/DataScoreAmericaPlus/
DataScoreAmericaPlus_published -- field names match what
ca_scouting_dashboard's lib/graficos.js already expects.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 3: Remove the stale "Scores don't exist per season" warning in `ca_scouting_dashboard`

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/graficos/GraficosView.jsx:357-374`

**Interfaces:**
- Consumes: nothing new — `page.jsx`'s Mongo query (`app/graficos/page.jsx`) already reads whatever fields exist on a `SbScMetricBySeason` document generically (its `matchFilter` builds `{ [m]: { $ne: null, $exists: true } }` for whichever metric keys are selected, with no Score-specific branch) — it needs **no code change**, it simply starts returning real rows once Task 2's sync has run.
- Produces: nothing new — this task only removes dead-code-once-Task-2-ships (the now-incorrect half of a warning message).

- [ ] **Step 1: Read current state to confirm line numbers**

Run: `sed -n '352,375p' "/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/graficos/GraficosView.jsx"`

Expected to show (confirmed present as of 2026-11-01):
```jsx
  const isBee = vista === "beeswarm";
  const labelX = SB_METRIC_LABEL[initialMetricX] || initialMetricX;
  const labelY = SB_METRIC_LABEL[initialMetricY] || initialMetricY;
  const labelBee = SB_METRIC_LABEL[initialMetricBee] || initialMetricBee;

  // Con una temporada específica elegida (no "Todas"), page.jsx lee de
  // sb_sc_metrics_by_season en vez de sb_sc_metrics (Acumulado) -- esa
  // colección no tiene Scores (DataScore/AmeScore/AmeScore+) ni
  // Perfil/Edad todavía (se calculan una sola vez sobre la fila blendeada
  // de Acumulado, no por temporada puntual; ver el comentario en
  // page.jsx y en sync_season_rows_to_mongo.R). Elegir cualquiera de esas
  // cosas con una temporada puntual activa simplemente no devuelve
  // jugadores -- este aviso explica por qué en vez de dejar un gráfico
  // vacío sin contexto.
  const SCORE_KEYS = new Set(SCORE_METRICS.map((m) => m.key));
  const activeMetricKeys = isBee ? [initialMetricBee] : [initialMetricX, initialMetricY];
  const usesScoreMetric = activeMetricKeys.some((k) => SCORE_KEYS.has(k));
  const usesPerfilOrEdad = initialPerfiles.length > 0 || initialEdadMin != null || initialEdadMax != null;
  const seasonUnsupportedNote = usesScoreMetric
    ? "DataScore/AmeScore/AmeScore+ todavía no existen por temporada puntual (solo Acumulado) -- con una temporada específica elegida esta vista sale vacía. Cambiá a \"Todas\" para verlos."
    : usesPerfilOrEdad
      ? "Perfil y Edad todavía no existen por temporada puntual -- esos filtros no aplican con una temporada específica elegida."
      : null;
```
If it doesn't match, re-read the surrounding area before editing (something else may have changed the file since this plan was written).

- [ ] **Step 2: Replace the block**

Replace the full block shown in Step 1 (from the `isBee` line through the `seasonUnsupportedNote` assignment's closing `;`) with:

```jsx
  const isBee = vista === "beeswarm";
  const labelX = SB_METRIC_LABEL[initialMetricX] || initialMetricX;
  const labelY = SB_METRIC_LABEL[initialMetricY] || initialMetricY;
  const labelBee = SB_METRIC_LABEL[initialMetricBee] || initialMetricBee;

  // Con una temporada específica elegida (no "Todas"), page.jsx lee de
  // sb_sc_metrics_by_season en vez de sb_sc_metrics (Acumulado) -- esa
  // colección no tiene Perfil/Edad (se calculan una sola vez sobre la
  // fila blendeada de Acumulado, no por temporada puntual; ver el
  // comentario en page.jsx). DataScore/AmeScore/AmeScore+ SÍ existen por
  // temporada desde 2026-11 (ver sync_season_rows_to_mongo.R, repo
  // scout_dashboard) -- ya no hace falta avisar sobre ellos acá.
  const usesPerfilOrEdad = initialPerfiles.length > 0 || initialEdadMin != null || initialEdadMax != null;
  const seasonUnsupportedNote = usesPerfilOrEdad
    ? "Perfil y Edad todavía no existen por temporada puntual -- esos filtros no aplican con una temporada específica elegida."
    : null;
```

- [ ] **Step 3: Check whether `SCORE_METRICS` is still used elsewhere in the file**

Run: `grep -n "SCORE_METRICS" "/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/graficos/GraficosView.jsx"`

If the only remaining match is the `import { ..., SCORE_METRICS, ... }` line itself (i.e., it's now unused after Step 2 removed its one usage), remove `SCORE_METRICS` from that import statement. If it's still used elsewhere (e.g. to populate a metric-picker dropdown), leave the import as-is — only `SCORE_KEYS`/`usesScoreMetric` (removed in Step 2) were the dead code this task targets.

- [ ] **Step 4: Verify the dev server compiles clean**

The dev server should already be running in the background on port 3001 (started earlier this session). Confirm no new compile error appeared:

Run: `tail -15 /private/tmp/claude-501/-Users-mateorodriguez-Desktop-analisis-CA-scout-dashboard/a2ab2cbb-5e54-48a9-9879-98cb212656c2/tasks/by2lbslcq.output`

Expected: no red `Error` lines after this file's save-triggered recompile (Next.js fast refresh recompiles automatically; a ` ✓ Compiled` line with no preceding error is enough — this task doesn't require fresh Mongo data, since that only lands after Task 4's CI run completes).

If the dev server isn't running anymore (check via `lsof -i :3001 -sTCP:LISTEN`), start it: `cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard && npm run dev` with `run_in_background: true`, then repeat this step against its fresh output file.

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git add app/graficos/GraficosView.jsx
git commit -m "$(cat <<'EOF'
Remove stale "DataScore/AmeScore/AmeScore+ don't exist per season" warning

scout_dashboard's sync_season_rows_to_mongo.R now syncs these fields
per season (see that repo's 2026-11-01 commits) -- the Perfil/Edad
half of this warning is still accurate and stays; only the Score half
is removed.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 4: Deploy and verify end-to-end

**Files:** none (operational task — triggers CI, no code changes)

**Interfaces:**
- Consumes: Tasks 1-3's committed changes.
- Produces: a live, verified feature — no further interface for later tasks (this is the plan's last task).

- [ ] **Step 1: Push `scout_dashboard`'s commits and trigger the pipeline**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git push origin main
gh workflow run deploy.yml --ref main
sleep 5
gh run list --workflow=deploy.yml --limit 1 --json databaseId,status,createdAt
```

Note the `databaseId` from the output — call it `<run_id>` below.

- [ ] **Step 2: Wait for the run to complete**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
while true; do
  run_status=$(gh run view <run_id> --json status -q .status 2>/dev/null)
  if [ "$run_status" = "completed" ]; then
    gh run view <run_id> --json status,conclusion -q '.status + " " + .conclusion'
    break
  fi
  sleep 300
done
```

Run this with `run_in_background: true` (full pipeline run takes roughly 5.5 hours) and wait for the completion notification rather than polling synchronously.

Expected final output: `completed success`. If `completed failure`, run `gh run view <run_id> --json jobs -q '.jobs[].steps[] | select(.conclusion=="failure") | .name'` to find which step failed, then `gh run view <run_id> --log-failed` to see why, before proceeding.

- [ ] **Step 3: Confirm the new fields actually landed in Mongo**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
cat > tmp_verify_scores.mjs << 'EOF'
import dotenv from "dotenv";
dotenv.config({ path: ".env.local" });
import mongoose from "mongoose";
import SbScMetricBySeason from "./models/SbScMetricBySeason.js";

await mongoose.connect(process.env.MONGODB_URI);
const total = await SbScMetricBySeason.countDocuments({});
const withScore = await SbScMetricBySeason.countDocuments({ DataScore: { $ne: null } });
const ligaMx2627 = await SbScMetricBySeason.countDocuments({
  Liga: /Liga MX/, season_name: "2026/2027", DataScore: { $ne: null }, DataScoreAmerica: { $ne: null },
  player_season_minutes: { $gte: 250 },
});
console.log("total docs:", total);
console.log("docs with DataScore populated:", withScore);
console.log("Liga MX 2026/2027, DataScore+AmeScore both present, minutes>=250:", ligaMx2627);
await mongoose.disconnect();
EOF
node tmp_verify_scores.mjs 2>&1 | grep -v "Warning\|MODULE_TYPELESS\|Reparsing\|eliminate\|trace-warnings"
rm tmp_verify_scores.mjs
```

Expected: `withScore` is a large majority of `total` (not zero, not some tiny fraction — a handful of players may legitimately lack a score if they have no role classification), and the Liga MX 2026/2027 count is close to the 293 the user originally reported wanting to see (exact number may differ slightly since this plan's confidence-shrinkage math differs from whatever ad-hoc number produced 293 in the original bug report — the point is it's nonzero and in the same ballpark, not an exact match requirement).

If `withScore` is 0 or unexpectedly tiny: check the CI run's logs for messages from `get_datascore_v2_scores_by_season()` (the `message(sprintf("get_datascore_v2_scores_by_season(): %d rows..."))` added in Task 2 Step 2) via `gh run view <run_id> --log 2>&1 | grep "get_datascore_v2_scores_by_season"` to see whether the function itself returned rows, narrowing whether the bug is in scoring or in the join.

- [ ] **Step 4: Verify in the browser**

Navigate to `http://localhost:3001/graficos` (or whatever port the dev server is on), set Liga = "Liga MX", Temporada = "2026/2027", Eje X = DataScore, Eje Y = AmeScore (the defaults), and confirm the chart now shows real points instead of the "DataScore/AmeScore/AmeScore+ todavía no existen por temporada puntual" message — this is the exact scenario from the user's original screenshot.

Use the Chrome extension tools (`mcp__claude-in-chrome__navigate`, `mcp__claude-in-chrome__get_page_text` or a screenshot) the same way this session verified the injury/transfer-history tables earlier.

- [ ] **Step 5: Report to the user**

Summarize: both repos' commits, the CI run result, the verified Mongo counts from Step 3, and a screenshot or text confirmation from Step 4 showing the previously-empty chart now populated.

---

## Self-Review Notes

- **Spec coverage:** Task 1 covers spec section 1 (the new scoring function). Task 2 covers section 2 (sync). Task 3 covers section 3 (frontend). Task 4 covers the spec's testing plan step 3 (full CI run + live verification); testing plan steps 1-2 are covered inline in Tasks 1 and 2 respectively (local smoke test, syntax-check). The "Expected behavior (not a bug)" note about early-season shrinkage isn't a task on its own — it's context for Task 4 Step 3's "ballpark, not exact match" expectation, not a code change.
- **Placeholder scan:** no TBD/TODO; every step has literal code or literal commands, not descriptions of what to do.
- **Type consistency:** `get_datascore_v2_scores_by_season()`'s return columns (`player_id`, `season_name`, `DataScore`, `Cobertura_DataScore`, `DataScoreAmerica`, `Cobertura_DataScoreAmerica`, `DataScoreAmericaPlus`, `DataScoreAmericaPlus_published`) are the same six field names (plus the two join keys) used consistently in Task 1's Interfaces block, Task 2's join, and Task 2's `SEASON_SYNC_SCORE_COLS` constant — cross-checked against `ca_scouting_dashboard/lib/graficos.js`'s existing `GRAFICOS_METRIC_OPTIONS`/`PUBLISHED_FLAG` definitions, which already expect exactly these names.
- **Correctness fix found during self-review:** the first draft of Task 1's function reused `rows` from `build_scored_rows()` without the defensive dedup `get_datascore_v2_scores()` applies before using it downstream (a small, known subset of `(var_name, player_id)` pairs repeat in the raw data — see that function's own comment). Missing it would have risked fan-out through the `ame_by_season`/`ds_by_season` joins. Task 1 Step 4's smoke test explicitly asserts zero duplicate `(player_id, season_name)` pairs in the final output to catch a regression here.
- **Correctness fix found during execution (Task 1's implementer, confirmed via the smoke test):** the original fix above (`dplyr::distinct(var_name, player_id, season_name, .keep_all = TRUE)`) was itself insufficient — a player can have two different `var_name` rows under the SAME `season_name` (a mid-season transfer between two leagues sharing the same Aug-May calendar), which that key doesn't collapse. 2,990 duplicate `(player_id, season_name)` pairs surfaced in the real smoke-test run. Ruling: resolve by keeping the higher-`player_season_minutes` row per `(player_id, season_name)` via `dplyr::slice_max(player_season_minutes, n = 1, by = c(player_id, season_name), with_ties = FALSE)` — same tiebreak `apply_transition_gate()` already uses elsewhere in this pipeline. See the ledger for the full ruling and its cost-if-wrong.
