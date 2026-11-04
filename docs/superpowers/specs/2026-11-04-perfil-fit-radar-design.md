---
title: Perfil-fit radar on the player profile
date: 2026-11-04
status: approved
---

# Perfil-fit radar on the player profile — design

## Problem

A player's position already maps to a set of "perfiles" (sub-archetypes
within that position — e.g. a Delantero can be a Cazador, Móvil, Retenedor,
Aéreo, or Acosador), and `assign_player_profiles()` (`app.R`) already scores
every player against every perfil in their position's set (0-100, a
weighted-percentile-rank composite). Today only the top two scores survive
(`Perfil`/`Score_perfil`, `Perfil_secundario`/`Score_perfil_secundario`) —
the full per-perfil score matrix is computed and then discarded. There is
no visual way to see, at a glance, how a player's whole profile shape
compares across every perfil their position defines.

## Goal

Add a radar chart to the player profile modal where each axis is one perfil
defined for that player's position group, and each axis's radius is that
player's score (0-100) for that perfil — surfacing data that already exists
inside `assign_player_profiles()` but is currently thrown away.

## Non-goals

- Per-season scoring. `assign_player_profiles()` runs once on the
  career-blended (Acumulado) table; this stays Acumulado-only, confirmed
  with the user — no new per-season profile-scoring pipeline.
- Changing `Perfil`/`Perfil_secundario`/their existing scores, or anything
  that reads them today (Base de Datos, Gráficos, Rankings). This is purely
  additive: a new field alongside the existing ones, nothing removed or
  recomputed differently.
- Reusing `PlayerRadarChart.jsx` as-is. That component filters out any row
  with a null percentile (`rows.filter(r => r.percentile != null)`) — the
  opposite of this feature's requirement that every perfil axis always
  appears regardless of data coverage. A new sibling component is needed;
  the existing one is untouched.

## Key findings

- `assign_player_profiles()` (`app.R:638-690`) already groups every player
  into one of 6 profile groups via `POSITION_GROUP_TO_PROFILE_GROUP[dat$position_group]`
  — Portero, Defensa Central, Lateral, Volante, Extremo, Delantero — and
  computes a full `scores` matrix (`app.R:649-657`, one column per perfil in
  that group, via `.weighted_profile_score()`) before collapsing to
  `top_two` (`app.R:659-669`). The matrix already has everything this
  feature needs; nothing new needs to be computed, only retained.
- Confirmed exact perfil counts per profile group (= radar axis count per
  position): Portero=4 (Atajador, Líbero, Salidor, Organizador), Defensa
  Central=4 (Posicional, Anticipador, Físico, Progresivo), Lateral=4
  (Defensivo, Ofensivo, Equilibrado, Organizador), Volante=5 (Destructor,
  Orquestador, Organizador, Box to Box, Creativo), Extremo=4 (Profundo,
  Interior, Regateador, Llegador), Delantero=5 (Cazador, Móvil, Retenedor,
  Aéreo, Acosador).
- `MIN_PROFILE_COVERAGE = 0.60` (`app.R:597`) already gates an individual
  perfil's score to `NA` for a player under 60% metric coverage for that
  perfil specifically — this happens inside the existing `scores` matrix
  per-cell, so a player can have a real score for 3 of 4 perfiles and `NA`
  for the 4th today, invisibly (only visible if that NA perfil happened to
  rank in the top two, which it structurally cannot since `NA` never wins
  an `order()`).
- Confirmed with the user: a `NA`/missing perfil score renders as a
  zero/flat spoke, visually distinct (e.g. dashed) from a genuine low
  score, never by dropping the axis — axis count must always equal the
  position's full defined perfil count.
- `SbScMetric`'s Mongoose schema is `strict: false` (same as
  `SbScMetricBySeason`), so adding new nested fields needs no schema
  migration.
- Placement (confirmed with user): a new radar alongside the existing
  StatsBomb and SkillCorner radars in the player profile's radar grid, not
  next to the existing Perfil/Perfil_secundario text display.

## Design

### 1. `app.R` — retain the full per-perfil score matrix

Inside `assign_player_profiles()`'s per-group loop (`app.R:643-687`), after
computing `scores` (already there, one column per perfil, `NA` cells
already handled by the coverage gate), add two new fields to each group's
output `data.frame` alongside the existing `Perfil`/`Score_perfil`/etc.
columns:

- `PerfilGroup`: the group name (`grp`) repeated for every row in that
  group — tells the frontend which perfil set applies (and therefore which
  axis labels/count to expect), without having to re-derive it from
  `position_group`.
- `PerfilScores`: a list-column, one named numeric vector per player
  (`setNames(scores[i, ], colnames(scores))`), covering every perfil in
  that group's `PROFILE_METRIC_DEFS` entry, `NA` preserved for cells under
  the coverage gate rather than dropped.

Both fields flow through the existing `dplyr::bind_rows(out)` and the
existing `left_join(profiles, by = "player_id")` at `app.R:2588` with zero
other changes — this is strictly additive to the function's output shape.

### 2. `sync_to_mongo.R` — persist to Mongo

Add `PerfilGroup` and `PerfilScores` to whatever column list
`sync_to_mongo.R` already selects for the Acumulado sync. `PerfilScores`'s
R list-column (named numeric vectors) needs to serialize into a JSON object
per document (`{"Cazador": 72.3, "Móvil": null, ...}`), not an array —
verify this is what `mongolite`'s insert actually produces for a
list-column of named vectors; if it instead flattens or mangles the names,
convert each player's vector to an explicit named list
(`as.list(scores_row)`) before insert, which `jsonlite`/`mongolite` reliably
serializes as a JSON object. Either way, every perfil key for that player's
group must be present in the synced document (with `null` for
under-coverage perfiles), never omitted — the frontend radar needs the full
key set to know how many axes to draw.

### 3. `ca_scouting_dashboard` — new radar component

New file `app/jugadores/[id]/PlayerPerfilRadar.jsx`. Same hand-rolled SVG
visual language as `PlayerRadarChart.jsx` (same `SIZE`/`RADIUS`/`RINGS`
constants, same gold palette, axes evenly distributed around a full
circle, `wrapLabel` for long perfil names) — but a new component, not a
modification of the existing one, because its null-handling must differ:
every key in the `perfilScores` object becomes an axis, in a fixed order
(e.g. the order the keys were defined in `PROFILE_METRIC_DEFS` on the R
side, threaded through via `PerfilScores`'s own key order — confirm
JSON/Mongo preserves insertion order, which it does for plain objects in
both `JSON.stringify`/parse and MongoDB's own document field order); a
`null` score draws that axis's point at the center (radius 0) with a
dashed stroke to that vertex, distinguishing "no data" from "scored
genuinely low."

Props: `perfilScores` (object, perfil name → score|null), `profileGroup`
(string, used only for a title/label like "Perfil: Delantero" above the
radar, not for any axis-count logic — the object's own keys already define
that).

### 4. Wiring

`app/jugadores/[id]/page.jsx`: read `PerfilGroup`/`PerfilScores` off the
player's existing `SbScMetric` (Acumulado) document fetch (no new query —
same document already fetched for every other Acumulado field), pass both
down as new props to `PlayerProfileView`. `PlayerProfileView.jsx`: render
`<PlayerPerfilRadar>` in the same grid/section as the existing StatsBomb
and SkillCorner radars, gated on `perfilScores` being present and
non-empty (a player whose `position_group` doesn't map to any profile
group — if that ever happens — simply shows no radar, same null-safety
convention as the rest of this modal).

## Testing plan

- R: after modifying `assign_player_profiles()`, spot-check a handful of
  real players (one per profile group) that `PerfilScores` contains every
  expected perfil key for their group, with values matching the
  already-existing `Score_perfil`/`Score_perfil_secundario` for whichever
  two keys are the top two (a direct cross-check: the max of `PerfilScores`
  should equal `Score_perfil`, the second-max should equal
  `Score_perfil_secundario`, for every row). Confirm a player known to be
  under the 60% coverage gate for one perfil shows `NA` for exactly that
  key, not missing entirely.
- R/Mongo: after `sync_to_mongo.R` runs, spot-check a few synced documents
  directly in Mongo to confirm `PerfilScores` landed as a proper nested
  object with all expected keys (not flattened, not an array, nulls
  preserved) and `PerfilGroup` matches the player's actual position group.
- Frontend: unit-level reasoning (no test framework in this repo, per
  established convention — verify by reading the component and a live
  render) that a profile group with 4 perfiles renders exactly 4 evenly-
  spaced axes, 5 perfiles renders 5, and a `null`-valued key renders as a
  dashed zero-radius spoke, not a dropped axis.
- Live verification: open a real player's profile (one per profile group
  ideally) and visually confirm axis count, labels, and that the shape's
  points align with the already-shown `Score_perfil`/`Score_perfil_secundario`
  values for that same player.

## Open items

None — all clarifying questions were resolved with the user before this
spec was written (NA-axis treatment, placement, Acumulado-only scope).
