---
title: Ficha pre/post performance tracking
date: 2026-11-03
status: approved
---

# Ficha pre/post performance tracking — design

## Problem

Scouts upload a "ficha" (PDF/PPTX scouting report) for a player at a point
in time, recorded as `Player.ficha.archivos[].fecha` in `ca_scouting_dashboard`
(the file's own last-modified date, not the upload date — an existing,
deliberate design choice in `app/api/players/[id]/ficha/route.js`). There is
currently no way to see whether a player's on-pitch performance changed
since that scouting read was made. Today's data only covers 25/26 and
26/27 per season in `sb_sc_metrics_by_season` — most fichas, written over
the past couple of years, predate all available season data, leaving no
real "pre" side to compare against.

## Goal

Add a third, older season (24/25 / 2024) to the StatsBomb pull, and surface
a season-by-season performance timeline on the player profile, with each
season tagged relative to the player's most recent ficha date (pre / post /
straddling), covering both the DataScore/AmeScore/AmeScore+ trend and raw
per-90 stats.

## Non-goals

- True date-level (sub-season) splitting. No match-by-match, date-stamped
  stat history exists in either repo; building one is a separate, much
  larger effort and not needed here (confirmed with the user — season-level
  granularity is the intended scope).
- Averaging/collapsing pre and post into a single number per side. With at
  most 3 seasons of history, a per-season timeline is more transparent than
  an average and avoids DataScore-scale-averaging ambiguity.
- Touching `dedup_transfers()` or the Acumulado blend. This feature reads
  exclusively from `sb_sc_metrics_by_season` (already season-separated),
  never from the career-blended Acumulado row.
- Retroactively back-filling fichas created before this feature ships with
  anything beyond their existing `fecha` field — no ficha-side changes are
  needed; `fecha` already exists and already means what we need.
- Applying this to every ficha a player has. Scoped to the single most
  recent ficha per player (confirmed with the user).

## Key findings

- `sync_season_rows_to_mongo.R` builds `season_rows` generically by looping
  `unique(raw$season_name)` (line ~201) — it is NOT hardcoded to specific
  season labels. Once `dashboard_scout.R` pulls 24/25 data (tagged with
  `season_name` "2024/2025" or "2024" by StatsBomb's API itself, the same
  way existing seasons are labeled), it syncs to Mongo automatically. No
  changes needed in `sync_season_rows_to_mongo.R`, `join_scout_data.R`, or
  `app.R`.
- `SbScMetricBySeason` already carries `DataScore`, `DataScoreAmerica`
  (AmeScore), `DataScoreAmericaPlus` (AmeScore+) per season (added
  2026-11-01, see `SEASON_SYNC_SCORE_COLS` in `sync_season_rows_to_mongo.R`)
  alongside raw StatsBomb/SkillCorner per-90 columns. Everything the
  timeline needs already exists per season — no new R-side score
  computation required.
- `app/jugadores/[id]/PlayerProfileView.jsx`'s season dropdown
  (`availableSeasons` prop) is populated dynamically from whatever seasons
  exist for that player, not hardcoded to "25/26"/"26/27" — 24/25 will
  appear as a selectable season automatically once synced.
- `dashboard_scout.R`'s season-pull pattern is per-league, two lines each:
  `<league>_<season> <- safe_matchesvector(username, password, season_id =
  X, competition_id = Y)` then `<league> <- c(<league>_a, <league>_b)`.
  Purely additive — add one more `safe_matchesvector()` call and include it
  in the `c(...)` concatenation, for every league. `safe_matchesvector()`
  already tolerates a season/competition combo that doesn't exist or isn't
  licensed (catches the error, returns `integer(0)`), so leagues without
  24/25 StatsBomb access degrade gracefully, consistent with the existing
  26/27-rollout precedent.
- Season IDs (confirmed by the user, StatsBomb's numbering is not
  predictable by formula the way SkillCorner's is):
  - `317` = 2024/2025 (Europe + Liga MX calendar — same group that uses 318
    for 25/26 and 351 for 26/27)
  - `282` = 2024 (Americas calendar — same group that uses 316 for 2025 and
    355 for 2026)
- `Player.ficha.archivos` is an array; a player can have multiple fichas.
  Confirmed with the user: anchor on the **most recent** entry's `fecha`
  only.
- Season label formats already in use (confirmed via `join_scout_data.R`
  and `sync_season_rows_to_mongo.R`): Europe/Liga MX-calendar seasons are
  `"YYYY/YYYY+1"` (e.g. `"2025/2026"`); Americas-calendar seasons are plain
  `"YYYY"` (e.g. `"2025"`).

## Design

### 1. `dashboard_scout.R` — add the 24/25 season pull

For every league currently pulling two seasons, add a third
`safe_matchesvector()` call using `season_id = 317` (if the league is in
the Europe/Liga MX-calendar group, i.e. currently uses 318/351) or
`season_id = 282` (if in the Americas-calendar group, i.e. currently uses
316/355), with that league's existing `competition_id`, and fold the result
into the existing `c(...)` concatenation. Libertadores (season_id 315 only,
no 26/27 mapping either) is left as-is — no 24/25 addition, consistent with
its existing single-season treatment. This is a mechanical, same-shape edit
repeated across every league block in the file (~35 leagues); no other
file in the R pipeline needs to change (see Key Findings).

### 2. Season-vs-ficha classification

A pure function, `classifySeasonVsFicha(seasonLabel, fichaDate)`, added to
`ca_scouting_dashboard`'s `lib/` (new file `lib/fichaPerformance.js`):

1. Parse `seasonLabel`:
   - Matches `/^(\d{4})\/(\d{4})$/` → Europe/Liga MX-style window: start =
     Aug 1 of the first year, end = Jun 30 of the second year.
   - Matches `/^\d{4}$/` → Americas-style window: start = Jan 1, end = Dec
     31 of that year.
   - Anything else → return `null` (unclassifiable; excluded from the
     timeline rather than guessed at).
2. Compare `fichaDate` against `[start, end]`:
   - `fichaDate < start` → `"post"` (season entirely after the ficha)
   - `fichaDate > end` → `"pre"` (season entirely before the ficha)
   - otherwise → `"straddling"` (ficha was made during this season; shown
     on the timeline but not counted as either pre or post)

`fichaDate` is always `player.ficha.archivos[player.ficha.archivos.length -
1].fecha` (the most recent entry; array is append-only per
`app/api/players/[id]/ficha/route.js`, so the last element is the most
recent).

A second function, `buildFichaPerformanceTimeline(transfermarktId,
fichaDate)`, queries `SbScMetricBySeason` for all season rows matching that
`transfermarkt_id`, classifies each via `classifySeasonVsFicha`, sorts
chronologically (by parsed start date), and returns an array of `{
seasonLabel, classification, dataScore, ameScore, ameScorePlus,
rawStatRows }` — `rawStatRows` built the same way `season-profile/route.js`
already builds `statRows` (via `buildPositionStatRows`), reused rather than
duplicated.

### 3. UI — new player-profile section

A new component (e.g. `FichaPerformanceTimeline.jsx`), rendered inside
`PlayerProfileView.jsx` below the existing ficha history, only when
`player.ficha.archivos.length > 0`. Shows one row per season in the
timeline: season label, pre/post/straddling tag, DataScore/AmeScore/
AmeScore+ as the visible headline columns, with raw per-90 stats collapsed
under a "ver detalle" toggle per row (reusing the existing stat-row
rendering patterns already used elsewhere in this file). The straddling
season (if present) is visually marked (e.g. a divider or distinct
background) to anchor where the ficha falls in the timeline. If there are
zero pre seasons or zero post seasons, that side of the timeline is simply
empty — no synthetic placeholder row, no error state.

### 4. Where it's computed

Server-side in `app/jugadores/[id]/page.jsx`, in the same block that
already resolves `availableSeasons` for the Acumulado view: look up the
player's most recent ficha date, call `buildFichaPerformanceTimeline`, pass
the result to `PlayerProfileView` as a new prop (`fichaPerformanceTimeline`).
At most 3 season rows per player — cheap enough to compute eagerly
alongside the page's existing server-side data resolution; no new API
route needed.

## Testing plan

- `classifySeasonVsFicha`: unit tests covering Europe-style and
  Americas-style labels in all three classification buckets, plus the
  unparseable-label case, plus boundary dates (exactly at season start/end).
- `buildFichaPerformanceTimeline`: integration-style test against a few
  real `transfermarkt_id`s with known season coverage and a known ficha
  date, confirming season count and classification match hand-calculation.
- `dashboard_scout.R`: after the edit, spot-check a handful of leagues'
  `<league>` match-id vectors include the 24/25 matches (non-empty for
  licensed leagues, gracefully empty for leagues without 24/25 access),
  and confirm downstream `sync_season_rows_to_mongo.R` output contains
  `season_name` values of `"2024/2025"`/`"2024"` after a full pipeline run.
- Live verification: open a player profile for a player with a ficha whose
  date is confirmed, verify the timeline shows the correct pre/post/
  straddling tags and that DataScore/AmeScore/AmeScore+ values match what
  the season selector already shows for those same seasons.

## Open items

- None — all clarifying questions were resolved with the user before this
  spec was written (season-based split, most-recent-ficha anchor, both
  DataScore and raw stats, player-profile new section, season IDs 317/282).
