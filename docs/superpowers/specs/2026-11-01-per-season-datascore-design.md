# Per-season DataScore/AmeScore/AmeScore+ — design

Date: 2026-11-01

## Problem

`ca_scouting_dashboard`'s Gráficos view lets a user filter by season
(e.g. "2026/2027"). Today, DataScore/AmeScore/AmeScore+ only exist on
the career-blended Acumulado row (`SbScMetric`/`sb_sc_metrics`) — the
per-season collection (`SbScMetricBySeason`/`sb_sc_metrics_by_season`)
only carries raw StatsBomb/SkillCorner stat columns, no scores. Picking
a specific season and a Score metric (DataScore × AmeScore, the
default) always renders an empty chart with an explicit "doesn't exist
per season yet" message, even when there's real underlying data for
that season (confirmed live: Liga MX 2026/2027 has 384 players with a
season row, 293 above the 250-minute default threshold).

## Goal

Compute DataScore, AmeScore (DataScoreAmerica), and AmeScore+
(DataScoreAmericaPlus) at `(player, season)` granularity, sync them
into the existing per-season Mongo collection, and let Gráficos filter
on them the same way it already filters raw stat metrics by season.

## Non-goals

- Changing Acumulado's own DataScore/AmeScore/AmeScore+ numbers in any
  way. This is purely additive — confirmed explicitly with the user.
- A separate per-season role-mean/confidence baseline. Per-season rows
  shrink toward the exact same global role-mean baseline Acumulado
  already uses — confirmed explicitly with the user.
- Perfil/Perfil_secundario/Edad at per-season granularity. These stay
  Acumulado-only; out of scope here, and `GraficosView.jsx`'s existing
  warning about them is untouched.
- A new Mongo collection. Scores become new fields on the existing
  `SbScMetricBySeason` documents.

## Key finding that shapes this design

The DataScore v2 pipeline already computes everything at
`(player_id, var_name)` granularity internally — `var_name` identifies
a specific league pull, and after `load_scout_data()`'s
`inner_join(league_pairs, by = c("competition_id", "season_id"))`,
each row also carries its own `season_id`/`season_name` distinctly.
(`var_name` here is the `safe_matchesvector()` call variable that
`league_pairs` is built from in `load_scout_data()` — e.g.
`ligamx_2526`/`ligamx_2627`, `champions_2526`/`champions_2627` — which
`dashboard_scout.R` already suffixes per season. This is a distinct set
of variables from the `jugs_*` `safe_player_season()` pulls, which
*do* `bind_rows()` two seasons under one shared name; those never feed
`var_name` in the scoring pipeline, so they don't affect this
design.) `build_scored_rows(dat, tiers)` scores every one of those
rows individually. The existing `get_datascore_v2_scores()` only
*looks* career-level because `apply_transition_gate()` collapses that
per-row table down to one "current" row per player right before
confidence shrinkage / the AmeScore gate run.

Confirmed two of this design's core calls are already exercised,
unmodified, directly on the full per-row table — not hypothetical:
- `ame_score_gate.R`'s own validation harness (`sys.nframe() == 0`
  block) calls
  `apply_ame_score_gate(rows$AmeScore_Base, rows[[GATE_CAPACITY]], rows$role_group_matchbased, techo_rol)`
  directly on the ungated `rows`.
- `compute_amescore_plus_all_roles(dat)` (`ame_score_plus.R`) is
  already keyed by `(var_name, player_id)` and never collapses to one
  row per player on its own — `datascore_v2.R`'s existing
  `get_datascore_v2_scores()` is the thing that collapses it, by
  joining it down to the gate-selected row.

So this design is "stop collapsing, reuse the same per-row scores the
pipeline already computes" rather than a new scoring subsystem.

## Design

### 1. `get_datascore_v2_scores_by_season()` — new function in `datascore_v2.R`

Sibling to `get_datascore_v2_scores()`, not a modification of it.

```
tiers   <- read.csv("data/competition_strength_tiers.csv", ...)
dat_v2  <- load_scout_data()
rows    <- build_scored_rows(dat_v2, tiers)

role_means_ds  <- <same computation get_datascore_v2_scores() already does>
role_means_ame <- <same computation get_datascore_v2_scores() already does>

# DataScore: rename rows' own columns to what apply_confidence_shrinkage()
# expects, instead of routing through apply_transition_gate()'s collapse.
ds_rows <- rows |>
  dplyr::filter(!is.na(DataScore_previo)) |>
  dplyr::transmute(
    player_id, player_name, var_name, league, season_id, season_name,
    source_role     = role_group_matchbased,
    source_minutes  = player_season_minutes,
    source_coverage = DataScore_Base_cobertura,
    DataScore_shown = DataScore_previo
  ) |>
  apply_confidence_shrinkage(role_means_ds)

# AmeScore: call apply_ame_score_gate() directly on every row of `rows`,
# same call ame_score_gate.R's own validation harness already makes.
ame_gate <- apply_ame_score_gate(
  rows$AmeScore_Base, rows[[GATE_CAPACITY]],
  rows$role_group_matchbased, role_means_ame
)
ame_rows <- dplyr::bind_cols(
  rows |> dplyr::select(player_id, var_name, season_name, AmeScore_Base_cobertura),
  ame_gate
)

# AmeScore+: already per (var_name, player_id), used as-is.
amescore_plus <- compute_amescore_plus_all_roles(dat_v2) |>
  dplyr::distinct(var_name, player_id, .keep_all = TRUE)
```

Then join `ds_rows` + `ame_rows` + `amescore_plus` by `(player_id,
var_name)` (all three come from the same `rows`/`dat_v2`, so this join
is exact, not fuzzy), apply `scale_datascore()`/`scale_amescore()` the
same way `get_datascore_v2_scores()` does, and return one row per
`(player_id, season_name)`:

```
player_id, season_name,
DataScore, Cobertura_DataScore,
DataScoreAmerica, Cobertura_DataScoreAmerica,
DataScoreAmericaPlus, DataScoreAmericaPlus_published
```

No transition gate, no "current vs. prior league" logic — every
league/season row a player has gets its own independent score.

### 2. Sync — fold into existing `sync_season_rows_to_mongo.R`

Not a new script, not a new collection. `get_datascore_v2_scores_by_season()`'s
output gets `left_join`ed onto the existing per-season pipeline by
`(player_id, season_name)`, before the TM-crosswalk step, the same
point every other per-season stat column already joins in. Add the six
score fields to a new `SEASON_SYNC_SCORE_COLS` constant alongside the
existing `SEASON_SYNC_SB_COLS`/`SEASON_SYNC_SC_COLS`, included in the
final `dplyr::select(dplyr::any_of(c(...)))`.

Join-key safety: a player CAN collide on `(player_id, season_name)`,
and does in production — Task 1's implementation hit 2,990 duplicate
`(player_id, season_name)` pairs in the full fixture. The real cause
isn't a calendar-format coincidence: `dashboard_scout.R` maps multiple
different leagues/competitions to the *same* `season_id` for a given
year. For example, `season_id = 355` is shared by `arg_2026`,
`brasil_2026`, `colombia_2026`, `ecuador_2026`, `chile_2026`,
`paraguay_2026`, `uruguay_2026`, `mls_2026`, and `ccl_2026` alike —
all bare-year `"2026"` format, all resolving to the same
`season_name` string. A player who appears in two competitions
sharing a `season_id` in the same year (a genuine mid-season
cross-league transfer, or dual registration) collides at
`(player_id, season_name)` regardless of slash-format vs. bare-year
format; `dedup_transfers()` only dedupes same-team splits, not this.
Fixed in `get_datascore_v2_scores_by_season()` by resolving to one row
per `(player_id, season_name)` via `dplyr::slice_max(player_season_minutes, ...)`
— i.e. keeping the higher-`player_season_minutes` row, the same
tiebreak `apply_transition_gate()` (`league_transition_gate.R`) already
uses for its own "which row wins" decision.

### 3. Frontend — `ca_scouting_dashboard`

In `app/graficos/page.jsx` / `GraficosView.jsx`: once
`SbScMetricBySeason` carries the six score fields, remove the
Score-specific branch of the "doesn't exist per season" block (around
`GraficosView.jsx:357-373` as of this writing). The
Perfil/Perfil_secundario/Edad branch of that same message stays
unchanged — those genuinely remain Acumulado-only per the Non-goals
above.

## Expected behavior (not a bug)

A season only a couple months old (like 2026/2027 right now) means
most players have low `exposure_90s` for *that season specifically*,
so confidence shrinkage will pull most current-season scores harder
toward the role mean than the same players' Acumulado scores are
pulled. This is the exact same shrinkage formula already validated for
low-sample players elsewhere in the pipeline — it loosens naturally as
the season accumulates more minutes, not something this design needs
to special-case.

Note also an asymmetry this design inherits rather than introduces:
`DataScore` gets per-season confidence shrinkage (pulling toward the
role mean in proportion to how many minutes a player has *that
season*), but `AmeScore`/`DataScoreAmerica` only gets a gate/ceiling
(`apply_ame_score_gate()`) with no equivalent minutes-based shrinkage.
This asymmetry already exists in the Acumulado pipeline, but it's far
more visible per-season, since a 2026/2027 row can rest on just 2-3
matches' worth of minutes, whereas Acumulado's gate-selected row is
usually a near-complete season. Practically, this means a low-minutes
player's per-season AmeScore can land at a more extreme value than
their (shrunk) DataScore would suggest — sampling noise more than
signal. Making AmeScore's per-season shrinkage symmetric with
DataScore's is a methodology decision for a human to make; this
implementation does not change that behavior.

## Testing plan

1. Run `get_datascore_v2_scores_by_season()` locally against the
   already-downloaded `data/scout_joined.rds` /
   `data/role_eligibility_matchbased.rds` fixtures. Sanity-check output
   shape (one row per real `(player_id, season_name)` pair) and spot
   that a known player's per-season score sits in a plausible range
   relative to their Acumulado score.
2. Wire into `sync_season_rows_to_mongo.R`, syntax-check
   (`Rscript -e 'parse(...)'`).
3. One full CI pipeline run, then verify end-to-end against the live
   Gráficos page — the chart from the reported screenshot (Liga MX,
   2026/2027, DataScore × AmeScore) should populate with real points
   instead of the empty-state message.

## Open items for the implementation plan (not blocking this spec)

- Exact column list/order for `SEASON_SYNC_SCORE_COLS`.
- Whether `GraficosView.jsx`'s warning copy needs a smaller tweak (it
  currently names all three scores in one sentence; once scores exist
  per season, only the Perfil/Edad half of that message remains true).
- `SourceLeague`/`SourceSeason` fields on
  `get_datascore_v2_scores_by_season()`'s output, mirroring what
  `get_datascore_v2_scores()`'s Acumulado output already has, so a
  future reader can tell which specific league/competition stint a
  per-season score came from when a player has a same-`season_name`
  collision resolved by the minutes tiebreak. Deferred as a follow-up
  enhancement (observability, not correctness) — not implemented in
  this round, since it would require another full pipeline run.
