# SkillCorner physical data: sample-size fallback + quality warning — design

Date: 2026-11-03

## Problem

The SkillCorner physical data pull (`notebooks/physical.ipynb`) and the
Acumulado physical row it eventually feeds (via `dedup_transfers()` in
`app.R`) have no minimum-sample-size policy. A player with too few
qualifying matches either doesn't appear at all, or gets blended into
Acumulado on a thin, unflagged sample. The user supplied explicit
scouting-org guidelines (photographed) for how this should work:

```
Por default:
  - Min. 5 partidos
  - Últimas dos temporadas acumuladas
  - Mínimo 30min por partido

Si no cumple con lo de arriba:
  - Completar con temporadas anteriores (hace 3 o 4 años) acumuladas
  - Bajar el mínimo de minutos hasta que se cumplan al menos 5 partidos jugados
  - Que en la página se muestre un símbolo de advertencia para indicar
    que se tomaron datos viejos o que la muestra es muy pequeña
```

## Goal

Build one physical-data row per player for Acumulado that:
1. By default blends the player's last 2 seasons.
2. Falls back to older seasons (~3-4 years back) and/or a lower
   per-match minutes threshold when the default doesn't reach 5
   matches.
3. Carries a quality flag recording whether — and why — a fallback was
   used, surfaced as a warning badge in the player profile.

## Non-goals (confirmed explicitly with the user)

- **StatsBomb's Acumulado blending is untouched.** `dedup_transfers()`
  keeps blending StatsBomb columns across a player's full career
  exactly as it does today. This design only replaces how the
  **SkillCorner/physical** columns of the final Acumulado row are
  produced. Applying the same 2-season/fallback/warning policy to
  StatsBomb is an explicitly separate, later project.
- **The season-selector feature is untouched.** The player profile's
  per-season SkillCorner tables (`SbScMetricBySeason`, the by-season
  sync, `PHYSICAL_VARS`) keep reading the existing season-separated
  `{league}_fisico` data exactly as today — this design does not
  change what feeds that path.
- **No UI surface beyond the player profile.** Rankings and Base de
  Datos do not get the warning badge in this round.

## Key findings that shape this design

- **`count_match__gte` is a per-request, per-season server-side
  filter.** A player with 3 matches in 25/26 and 3 in 26/27 appears in
  *neither* season's pull today, since neither season alone clears 5.
  To accumulate across seasons, this filter has to come off the API
  call and get enforced afterward, on the combined count.
- **`playing_time__gte` is baked into a season-aggregate endpoint, not
  match-level data.** There's no way to compute "what if the threshold
  were lower" from an already-pulled aggregate — each fallback minute
  tier needs its own API call.
- **The season-separated `{league}_fisico` dataframes (`sc_data.R`)
  feed two independent consumers**: the season-selector sync
  (`sync_season_rows_to_mongo.R`, via `get_all_players_raw_df()`
  *before* `dedup_transfers()` collapses across seasons) and Acumulado
  (`dedup_transfers()` itself, which blends every season/league row a
  player has, full career, minutes-weighted). This design's new
  fallback-aware blend is built **separately** from `{league}_fisico`
  and layered onto Acumulado's physical columns afterward — it never
  touches `{league}_fisico` itself, so the season-selector path is
  structurally unaffected by construction, not by care taken at each
  call site.
- **Championship and Rusia already extrapolate older season IDs**
  (23/24 through 26/27, 4 seasons) using a documented "+2 per year"
  pattern, confirmed only by `df.empty` gracefully skipping a wrong
  guess. The "3-4 years back" fallback reuses this exact mechanism and
  pattern for every league, not a new one.

## Design

### 1. `physical.ipynb` — pull candidate data (Python)

Per league, per the already-shipped Part 1 (last 2 seasons,
`playing_time__gte: 30`):
- Drop `count_match__gte` from the API params entirely (or set to 1) —
  the 5-match minimum is enforced downstream, on the combined count,
  not server-side per season.
- This changes the CSV's row count (more players, including low-sample
  ones) but not its shape — same columns, same per-season-row
  structure as today.

Two **new, separate** candidate pulls per league, written to their own
CSVs (not merged into the existing `{league}_physical_standardized.csv`,
which stays exactly as it is for the season-selector):
- **Extended seasons**: 2 more seasons older than the default pair,
  using the same season-ID extrapolation already proven for
  Championship/Rusia (`{league}_physical_extended_seasons.csv`).
- **Lower minute tiers**: the same last-2-seasons pull repeated at
  `playing_time__gte: 20` and again at `10`
  (`{league}_physical_tier20.csv`, `{league}_physical_tier10.csv`).
  Both tiers get pulled unconditionally per league (simpler than
  conditionally deciding in Python whether any player needs them) —
  the R-side blending step (next) decides per-player whether to
  actually use a given tier's data.

### 2. New file: a fallback-aware physical blender (R)

One new R file (plan decides exact name/location, e.g.
`physical_acumulado_fallback.R`), sourced from `sc_data.R` after the
existing `{league}_fisico` CSVs are read, producing one tibble with one
row per `player_id`:

```
player_id, <same physical metric columns {league}_fisico already has>,
sc_physical_quality_warning (logical),
sc_physical_quality_reason (character: "ok" | "extended_seasons" |
                             "lower_threshold" | "insufficient_sample")
```

Per player, walk the fallback ladder only as far as needed:
1. Blend the default last-2-seasons rows (minutes-weighted average for
   rate/peak metrics, sum for cumulative counters — the exact same
   split `{league}_fisico`'s own multi-competition cells and
   `dedup_transfers()` already use, reused here rather than
   reinvented). If combined `count_match` ≥ 5 → `reason = "ok"`, done.
2. Else, add in the extended-seasons rows, re-blend. If ≥ 5 →
   `reason = "extended_seasons"`, done.
3. Else, add in the tier-20 rows (still accumulating seasons as
   needed), re-blend. If ≥ 5 → `reason = "lower_threshold"`, done.
4. Else, add in the tier-10 rows, re-blend. If ≥ 5 →
   `reason = "lower_threshold"`, done.
5. Else, use whatever was accumulated through step 4 (even if still
   < 5 matches) → `reason = "insufficient_sample"`.

`sc_physical_quality_warning` is `TRUE` whenever `reason != "ok"`.

### 3. Propagation into Acumulado (app.R)

`dedup_transfers()` itself is **not modified**. After
`get_all_players_df()` produces the final Acumulado table (post
`dedup_same_team()` + `dedup_transfers()`), left-join the new blended
physical table from step 2 onto it by `player_id`, and:
- Overwrite the existing SC/physical numeric columns with the new
  blended values (the ones `dedup_transfers()` computed from the
  season-separated rows get replaced).
- Add `sc_physical_quality_warning` /`sc_physical_quality_reason` as
  new columns — these never go through `dedup_transfers()`'s numeric
  weighted-average path (they're created after it runs), so no special
  string-handling case needs to be added there (unlike `team_name`/
  `.league_label`, which do go through it).
- A player entirely missing from the new blended table (no SkillCorner
  coverage in the candidate pulls at all) keeps `NA` for both new
  columns — `sc_physical_quality_warning` should read as "no SC data",
  not "warning", so the UI checks `!is.na(DataScoreAmericaPlus) %||%` -
  style coverage the same way existing physical-coverage checks already
  do, not the warning flag alone.

### 4. Sync + frontend

- `sync_to_mongo.R` already mirrors the full Acumulado row — the two
  new columns ride along automatically, no changes needed there.
- `ca_scouting_dashboard`: `models/SbScMetric.js` is `strict: false`
  (confirmed earlier this session), so the new fields round-trip
  without a schema change.
- `app/jugadores/[id]/page.jsx` passes `sbSc` through to
  `PlayerProfileView`/the SkillCorner section already — thread
  `sc_physical_quality_warning`/`sc_physical_quality_reason` through
  the same `meta` object the existing score fields use.
- UI: an `sc-tag warn` badge (existing CSS class, confirmed in use for
  the "no ficha" indicator) next to the "⚙️ Skill Corner" section
  header in `PlayerProfileView.jsx`/`PlayerSkillCornerTable.jsx`, shown
  only when `sc_physical_quality_warning` is `true`. Reason text
  (`"Datos de temporadas anteriores"` for `extended_seasons`,
  `"Muestra pequeña de partidos"` for `lower_threshold` or
  `insufficient_sample`) as a title/tooltip attribute, not inline copy
  — matching how the existing ficha tag communicates state primarily
  through color/position, not a paragraph.

## Testing plan

1. Run the new R blending step locally against a saved/local copy of a
   few leagues' CSVs (including the two new fallback-tier pulls for at
   least one league with known low-sample players) — verify the
   fallback ladder produces the expected `reason` per player and that
   `count_match` actually reaches 5 (or correctly doesn't, for
   `insufficient_sample`).
2. Confirm `dedup_transfers()` and the rest of `get_all_players_df()`
   are provably untouched (diff review — the new logic is additive,
   after the existing pipeline runs, not inside it).
3. One full CI pipeline run (the extra API calls add real time — two
   new candidate pulls per league beyond today's two, so budget for a
   longer physical.ipynb step than before), then verify live: a known
   low-sample player shows the warning badge with the right reason,
   and a normal player shows no badge and unchanged physical numbers.

## Open items for the implementation plan

- Exact file/function names for the new R blending step.
- Exact column list to carry through the blend (should mirror
  whatever `{league}_fisico` already carries, not a hand-picked
  subset).
- Whether the "lower minute tier" fallback pulls 20 and 10 together
  unconditionally (simpler, more API calls) or 20 first and only 10 if
  still needed (fewer calls, more conditional logic in the notebook) —
  spec defaults to unconditional-both for simplicity; plan can revisit
  if pipeline runtime becomes a real concern.
- StatsBomb-side application of this same policy (explicitly deferred,
  separate project per the user's sequencing decision).
