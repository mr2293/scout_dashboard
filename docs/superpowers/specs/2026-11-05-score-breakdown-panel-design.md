---
title: DataScore/AmeScore/AmeScore+ breakdown panel
date: 2026-11-05
status: approved
---

# DataScore/AmeScore/AmeScore+ breakdown panel — design

## Problem

DataScore, AmeScore, and AmeScore+ are each a single composite number. A
scout has no way to see *why* a player scored what they did — which
underlying capacities (e.g. "Finalización", "Presión alta") and metrics
drove it, what each metric's raw value and percentile were, or how that
compares across seasons. The underlying per-metric percentiles and
per-capacity sub-scores already get computed inside this pipeline today —
they're simply discarded once the final composite is produced.

## Goal

Clicking a DataScore/AmeScore/AmeScore+ tag on the player profile opens a
floating (not full-screen) panel showing that score's own breakdown:
player header (team, games played, minutes played), then one section per
capacity/category, each listing its metrics with value, percentile, and a
continuous color gradient keyed to percentile — plus a season toggle
(Acumulado default, switchable to any season the player has coverage for).

## Non-goals

- Changing the existing hover popover (`ScorePercentilePopover`) in any
  way. It stays exactly as-is; the new panel opens on a **click** of the
  same tag, a different interaction entirely.
- A single shared breakdown reused across all three scores. DataScore and
  AmeScore weight the same capacities differently per role
  (`*_DATASCORE_WEIGHTS` vs. `*_AMESCORE_WEIGHTS` in `base_scores.R`); each
  score gets its own breakdown reflecting its own weights.
- Recomputing percentiles/categories in the frontend. Per the user's
  explicit choice, this is computed and persisted from R (same approach
  as the perfil-fit radar), guaranteeing the breakdown always matches
  exactly what the backend's scoring pipeline actually used.
- A new percentile color convention for the rest of the app. The
  continuous gradient is scoped to this new panel only; every other
  percentile display (stat tables, the existing hover popover) keeps its
  current 3-tier red/dim/green convention.

## Key findings

- The per-metric percentile + per-capacity sub-score breakdown already
  exists as an internal computation for all three scores, for both
  Acumulado and every per-season row — it's simply discarded after
  `capacity_subscore()`/`combine_capacities()` produce the final number.
  Retaining it is additive, not a new computation.
- `build_scored_rows()` (`competition_strength.R`) is the SINGLE shared
  function that computes capacity scores for every context — Acumulado
  (via `get_datascore_v2_scores()`) and every per-season row (via
  `get_datascore_v2_scores_by_season()`, `datascore_v2.R:181-276`) both
  call it on the same underlying per-row data. Retaining the breakdown
  once, inside this shared function, covers both Acumulado and per-season
  automatically — no separate per-season computation path needed.
- DataScore and AmeScore are each a flat list of named capacities per role
  (e.g. `DELANTERO_CAPACITIES`, `base_scores.R:214-224`'s `ROLE_SCORE_DEFS`
  maps `caps`/`ds`/`ame` weight vectors per role), each capacity a named
  weighted list of raw metric columns (e.g. `DELANTERO_CAPACITIES`'s
  `"Finalización"` capacity in `capacity_subscores.R:252-283`). These
  category names map directly to the reference image's "ATAQUE"/
  "CRIAÇÃO"/etc. grouping the user wants.
- AmeScore+ is NOT a separate flat category list. It reuses AmeScore's own
  capacities, but specific ones get "enriched" (`apply_physical_enrichment()`,
  `ame_score_plus.R:184-219`): that capacity's final score becomes a
  `rho`-weighted blend of its existing event-based sub-score and a
  separately-computed physical (SkillCorner) sub-score, built from its own
  metric set (`PHYSICAL_ENRICHMENT[[role]][[capacity]]`). A capacity not in
  the enrichment map stays pure event-based, unchanged from AmeScore. The
  breakdown panel's per-category metric list therefore needs each metric
  tagged by source (`"event"` or `"physical"`) so an enriched category can
  show both groups of metrics feeding its one blended score.
- `sync_to_mongo.R` (Acumulado) syncs the whole table with no column
  allow-list — new fields ride along automatically, same as the perfil
  radar fix. `sync_season_rows_to_mongo.R` (per-season) DOES have an
  explicit column list (`SEASON_SYNC_SCORE_COLS`) that must be extended to
  include the new breakdown fields.
- Team, games played (`90s Jugados`), and minutes played are already
  available in existing props (`player.clubActual`/`clubName`,
  `meta.nineties`, `meta.minutos`) — no new data needed for the panel
  header.
- The existing `ScorePercentilePopover` (hover-triggered) and the tags that
  trigger it (`app/jugadores/[id]/PlayerProfileView.jsx:687-745`) are
  exactly where the new click handler attaches — same elements, additive
  interaction, no change to the existing hover behavior.

## Design

### 1. R side — retain the breakdown (scout_dashboard)

Inside `build_scored_rows()` (`competition_strength.R`), alongside the
existing `capacity_subscore()`/`combine_capacities()` calls for DataScore
and AmeScore, retain a new per-row nested structure (one per score type):
a list, one entry per capacity, each entry `{category, capacityScore,
metrics: [{key, value, percentile, weight, source}]}` — `source` is
`"event"` for every DataScore/AmeScore metric, and `"event"` or
`"physical"` for AmeScore+ metrics (per the enrichment finding above).
Stored as new columns: `DataScoreBreakdown`, `AmeScoreBreakdown`,
`AmeScorePlusBreakdown` (list-columns, same `jsonlite::unbox()`-per-scalar
technique already fixed into `sc_physical_acumulado_fallback.R` this
session, to avoid the array-wrapping/NA-as-string bug found there).

### 2. R side — thread through both Acumulado and per-season paths

- Acumulado: flows automatically through `get_db_master()`/
  `sync_to_mongo.R` (no column list to update), identical to the perfil
  radar fix.
- Per-season: `get_datascore_v2_scores_by_season()`'s `ds_by_season`/
  `ame_by_season`/`amescore_plus` `transmute()` calls
  (`datascore_v2.R:219-265`) must explicitly keep the new breakdown
  columns (currently they drop everything except the final score +
  coverage). `sync_season_rows_to_mongo.R`'s `SEASON_SYNC_SCORE_COLS` must
  add the three new field names.

### 3. Frontend — new floating panel component

New component, e.g. `ScoreBreakdownPanel.jsx`: a floating panel (centered
overlay with a backdrop, not full-screen — same visual weight as the
existing `PlayerModal`/popover patterns already in this app, not a new
full-page route). Props: `breakdown` (the category/metric structure for
whichever score + season is selected), `scoreType` (label), player header
info (team/games/minutes, passed from existing data already on the page).

- Header: team, games played, minutes played.
- Season toggle: button group, Acumulado (default) + every season this
  player has a `*Breakdown` field for (from `SbScMetricBySeason`).
  Switching seasons re-fetches/re-selects that season's own breakdown
  (client-side if already loaded for all seasons up front — given at most
  3 seasons' worth of a nested object per player, this is cheap enough to
  fetch all at once rather than lazily per toggle click, avoiding a new
  API round-trip per click).
- Body: one section per capacity/category, each metric row shows label
  (via the existing `LABEL_MAP`/`POSITION_METRIC_MAP` JS maps already used
  for the stat tables — reused here for the metric key → display label
  lookup, not re-derived), value, percentile, and a continuous color bar
  (red → gold gradient by percentile 0-100, a new color treatment scoped
  to just this panel per the user's explicit choice). An AmeScore+
  category with a mix of `"event"`/`"physical"`-sourced metrics shows both
  groups together under that one category, each metric still tagged
  (e.g. a small physical-source indicator) so the blend is legible.

### 4. Trigger wiring

Add an `onClick` handler to each existing `.sc-tag` score element
(`PlayerProfileView.jsx:687-745`) that opens `ScoreBreakdownPanel` for that
specific score. The existing `ScorePercentilePopover` (hover) is
untouched — same DOM element, two independent event handlers (hover vs.
click), no interaction conflict since one opens inline on hover and the
other opens an overlay on click.

## Testing plan

- R: verify a sample player's `DataScoreBreakdown` sums/matches the actual
  `DataScore` the existing pipeline produces (same cross-check pattern as
  the perfil radar fix: the weighted combination of retained capacity
  scores should reproduce the existing final score, confirming the
  retained breakdown reads the same underlying computation, not a
  separately-derived one).
- R: verify the list-column serializes correctly through the REAL
  `mongolite` serializer (not just `jsonlite::toJSON` directly) — this
  exact bug was found and fixed once already this session
  (`sc_physical_acumulado_fallback.R`), the same verification must be
  repeated here before trusting it.
- R: verify at least one AmeScore+-enriched capacity for a known physical-
  enriched role shows both `"event"` and `"physical"`-tagged metrics.
- Frontend: verify the panel renders correctly for a player with only
  Acumulado data (no per-season breakdown available) vs. one with full
  season coverage; verify the season toggle only offers seasons that
  actually have a breakdown.
- Live verification: open a real player's DataScore/AmeScore/AmeScore+
  panels, cross-check at least one capacity's metrics/weights by hand
  against `base_scores.R`'s actual weight definitions for that player's
  role.

## Open items

None — all clarifying questions were resolved with the user before this
spec was written (per-score breakdowns, hover-vs-click split, R-side
persistence, continuous gradient, season toggle defaulting to Acumulado).
