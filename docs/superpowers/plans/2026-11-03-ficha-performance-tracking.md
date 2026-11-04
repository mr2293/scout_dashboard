# Ficha Pre/Post Performance Tracking Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a 24/25 StatsBomb season pull, then surface a season-by-season performance timeline on the player profile, tagging each season as before/after/during the player's most recent ficha date.

**Architecture:** A mechanical third-season addition to `dashboard_scout.R`'s existing per-league match-id pulls (scout_dashboard repo), feeding the already-generic `sync_season_rows_to_mongo.R` pipeline with no further R-side changes. On the `ca_scouting_dashboard` side, a new pure classification module reads the player's existing per-season Mongo rows and their most recent ficha date, builds a sorted timeline, and a new UI component renders it on the player profile page.

**Tech Stack:** R (StatsBombR, dplyr), Next.js server components, Mongoose/MongoDB.

**Spec:** `docs/superpowers/specs/2026-11-03-ficha-performance-tracking-design.md`

## Global Constraints

- `dedup_transfers()` and the Acumulado blend (`app.R`) are never touched by this plan — the timeline reads exclusively from `sb_sc_metrics_by_season` (per-season rows), never from the career-blended `sb_sc_metrics` collection.
- Season label formats are fixed by existing convention, not invented here: Europe/Liga MX-calendar seasons are `"YYYY/YYYY+1"` (e.g. `"2025/2026"`); Americas-calendar seasons are plain `"YYYY"` (e.g. `"2025"`).
- The anchor date is always the **most recent** entry in `player.ficha.archivos` (array is append-only, so the last element is the most recent) — never the first ficha, never an average/all-fichas view.
- Classification is season-level only: `"pre"` (season fully ended before the ficha date), `"post"` (season fully started after the ficha date), or `"straddling"` (ficha date falls inside that season's window) — never a sub-season/date-level split.
- `SbScMetricBySeason` already carries `DataScore`, `DataScoreAmerica`, `DataScoreAmericaPlus` per season (added 2026-11-01) — this plan reads those fields directly, it does not compute or re-derive scores.
- `sb_sc_metrics_by_season` does not sync `player_season_minutes` (confirmed in `app/jugadores/[id]/page.jsx`'s existing comments) — do not rely on that field being present on season docs from this collection.
- No JS test framework (Jest/Vitest/etc.) exists in `ca_scouting_dashboard` today — verification of new JS logic in this plan uses one-off Node scripts (`node -e` / a scratch `.mjs` file), matching this repo's existing practice of live/manual verification rather than a committed test suite.

---

### Task 1: Add the 24/25 StatsBomb season pull to every league

**Files:**
- Modify: `dashboard_scout.R:100-266` (the `# SEASON IDs` block and every league's match-id pull)

**Interfaces:**
- Consumes: `safe_matchesvector(username, password, season_id, competition_id)` (already defined at `dashboard_scout.R:29-34`) — no changes to this function.
- Produces: nothing new is exported; this task only changes which match ids flow into each league's existing `<league>` variable, which every downstream `jugs_<league>`/`goles_<league>` call already consumes unchanged.

This is a single mechanical edit repeated across every league block — same two-line shape each time: add one `safe_matchesvector()` call for the 24/25 season, then add that new variable into the existing `c(...)` concatenation. Apply this transformation to every row in the table below. Two worked examples (one per calendar group) are shown first so the pattern is unambiguous; the table gives the exact `competition_id` and new line text for every remaining league — this is not a placeholder, every row is literal content to apply exactly as written.

**Worked example — Europe/Liga MX-calendar group (season_id 317 added):**

Before (`dashboard_scout.R:100-102`):
```r
ligamx_2526 <- safe_matchesvector(username, password, season_id = 318, competition_id = 73)
ligamx_2627 <- safe_matchesvector(username, password, season_id = 351, competition_id = 73)
ligamx <- c(ligamx_2526, ligamx_2627)
```

After:
```r
ligamx_2425 <- safe_matchesvector(username, password, season_id = 317, competition_id = 73)
ligamx_2526 <- safe_matchesvector(username, password, season_id = 318, competition_id = 73)
ligamx_2627 <- safe_matchesvector(username, password, season_id = 351, competition_id = 73)
ligamx <- c(ligamx_2425, ligamx_2526, ligamx_2627)
```

**Worked example — Americas-calendar group (season_id 282 added):**

Before (`dashboard_scout.R:104-106`):
```r
ccl_2025 <- safe_matchesvector(username, password, season_id = 316, competition_id = 101)
ccl_2026 <- safe_matchesvector(username, password, season_id = 355, competition_id = 101)
ccl <- c(ccl_2025, ccl_2026)
```

After:
```r
ccl_2024 <- safe_matchesvector(username, password, season_id = 282, competition_id = 101)
ccl_2025 <- safe_matchesvector(username, password, season_id = 316, competition_id = 101)
ccl_2026 <- safe_matchesvector(username, password, season_id = 355, competition_id = 101)
ccl <- c(ccl_2024, ccl_2025, ccl_2026)
```

**Every league to edit** (apply the exact same transformation shown above, using that league's own variable-name prefix and `competition_id`):

| Calendar group (new var suffix) | League variable prefix | `competition_id` |
|---|---|---|
| Europe/Liga MX (`_2425`, `season_id = 317`) | `ligamx` | 73 |
| Europe/Liga MX | `champions` | 16 |
| Europe/Liga MX | `premier` | 2 |
| Europe/Liga MX | `championship` | 3 |
| Europe/Liga MX | `laliga` | 11 |
| Europe/Liga MX | `laliga_2` | 42 |
| Europe/Liga MX | `serie_a` | 12 |
| Europe/Liga MX | `serie_b` | 84 |
| Europe/Liga MX | `bundesliga` | 9 |
| Europe/Liga MX | `bundesliga_2` | 10 |
| Europe/Liga MX | `ligue_1` | 7 |
| Europe/Liga MX | `eredivisie` | 6 |
| Europe/Liga MX | `belgica` | 46 |
| Europe/Liga MX | `portugal` | 13 |
| Europe/Liga MX | `turquia` | 85 |
| Europe/Liga MX | `escocia` | 51 |
| Europe/Liga MX | `europa_league` | 35 |
| Europe/Liga MX | `australia` | 93 |
| Europe/Liga MX | `chequia` | 76 |
| Europe/Liga MX | `efl_1` | 4 |
| Europe/Liga MX | `efl_2` | 5 |
| Europe/Liga MX | `ligue_2` | 8 |
| Europe/Liga MX | `polonia` | 38 |
| Europe/Liga MX | `rusia` | 128 |
| Europe/Liga MX | `suecia_allsvenskan` | 75 |
| Europe/Liga MX | `suecia_superettan` | 249 |
| Europe/Liga MX | `suiza_super_league` | 80 |
| Europe/Liga MX | `suiza_challenger` | 177 |
| Americas (`_2024`, `season_id = 282`) | `ccl` | 101 |
| Americas | `arg` | 81 |
| Americas | `brasil` | 71 |
| Americas | `colombia` | 74 |
| Americas | `ecuador` | 105 |
| Americas | `chile` | 103 |
| Americas | `paraguay` | 247 |
| Americas | `uruguay` | 111 |
| Americas | `mls` | 44 |
| Americas | `noruega` | 88 |
| Americas | `china` | 104 |
| Americas | `peru` | 97 |
| Americas | `usl_championship` | 89 |

**Do NOT touch:** Libertadores (`season_id = 315` only, no 26/27 mapping exists for it either — stays single-season, per the spec's non-goals).

- [ ] **Step 1: Apply the edit to every league in the table above**

For each row, replace that league's 3-line block (two `safe_matchesvector()` calls + one `c(...)` line) with the 4-line version: insert a new `safe_matchesvector()` call using the new season_id and that row's `competition_id`, named `<prefix>_2425` (Europe/Liga MX group) or `<prefix>_2024` (Americas group), placed immediately before the existing first season's line, and add that new variable as the first argument in the `c(...)` call.

- [ ] **Step 2: Update the `# SEASON IDs` comment block**

In `dashboard_scout.R`, immediately above the first league pull (currently starting around line 88), update the comment block to document the new season IDs:

```r
# ============================================================
# SEASON IDs
#   317 = 2024/2025 (Europe + Liga MX calendar)
#   318 = 2025/2026 (Europe + Liga MX calendar)  | 351 = 2026/2027
#   282 = 2024      (Americas calendar)
#   316 = 2025      (Americas calendar)          | 355 = 2026
# Each league below pulls the current, newly-added, and (as of
# 2026-11-03) one prior season, concatenating the match id vectors so
# every downstream variable (ligamx, jugs_ligamx, goles_ligamx, ...)
# covers all three seasons at once. Added to feed the ficha pre/post
# performance timeline (ca_scouting_dashboard) a real "pre" season for
# fichas written before 25/26 existed -- see
# docs/superpowers/specs/2026-11-03-ficha-performance-tracking-design.md.
# Libertadores is left on season_id 315 only -- no 2024 or 2026 mapping
# was given for it, unlike the other Americas-calendar competitions.
# ============================================================
```

- [ ] **Step 3: Syntax check**

Run: `Rscript -e 'parse("dashboard_scout.R"); cat("PARSE OK\n")'`
Expected: output ends with `PARSE OK` (pre-existing `renv`/cache warnings before it are unrelated noise, same as every other `Rscript` invocation in this repo this session).

- [ ] **Step 4: Confirm every edited league still has exactly one `c(...)` line, now with 3 arguments**

Run: `grep -c "safe_matchesvector(username, password, season_id = 317\|safe_matchesvector(username, password, season_id = 282" dashboard_scout.R`
Expected: `41` (28 Europe/Liga MX leagues + 13 Americas leagues from the table above).

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add dashboard_scout.R
git commit -m "$(cat <<'EOF'
Add 24/25 (2024) StatsBomb season pull to every league

Gives the ficha pre/post performance timeline (ca_scouting_dashboard)
a real "pre" season for fichas written before 25/26 existed. Purely
additive -- sync_season_rows_to_mongo.R already loops
unique(raw$season_name) generically, so no other R file needs changes.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: `lib/fichaPerformance.js` — season classification + timeline builder

**Files:**
- Create: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/lib/fichaPerformance.js`
- Verification script (not committed): `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/.scratch-verify-ficha-performance.mjs`

**Interfaces:**
- Consumes: `LABEL_MAP`, `realColumnsForPosition(pg)`, `statsProjection(cols)`, `buildPositionStatRows(doc, poolDocs, pg)` — all already exported from `lib/positionMetrics.js` (signatures confirmed by reading that file; no changes to it). `SbScMetricBySeason` model (default export from `models/SbScMetricBySeason.js`, fields include `transfermarkt_id`, `season_name`, `position_group`, `Liga`, `DataScore`, `DataScoreAmerica`, `DataScoreAmericaPlus`, plus the raw StatsBomb/SkillCorner stat columns — schema is `strict: false`, no fixed field list).
- Produces:
  - `classifySeasonVsFicha(seasonLabel: string, fichaDate: Date): "pre" | "post" | "straddling" | null` — pure function, no I/O.
  - `buildFichaPerformanceTimeline(seasonDocs: object[], fichaDate: Date): Promise<Array<{ seasonLabel: string, classification: "pre"|"post"|"straddling", dataScore: number|null, ameScore: number|null, ameScorePlus: number|null, rawStatRows: Array<{key, metric, value, percentile}> }>>` — `seasonDocs` is the CALLER's already-fetched array of that one player's own `SbScMetricBySeason.find({transfermarkt_id}).lean()` docs (Task 3 already has this fetched; this function does not re-query for the anchor player, only for each season's comparison pool).

- [ ] **Step 1: Write the verification script (acts as this task's test, since no test framework exists in this repo)**

Create `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/.scratch-verify-ficha-performance.mjs`:

```js
import assert from "node:assert/strict";
import { classifySeasonVsFicha } from "./lib/fichaPerformance.js";

// Europe/Liga MX-style label, ficha squarely before the season
assert.equal(classifySeasonVsFicha("2025/2026", new Date("2024-01-15")), "post");
// Europe/Liga MX-style label, ficha squarely after the season
assert.equal(classifySeasonVsFicha("2024/2025", new Date("2026-01-15")), "pre");
// Europe/Liga MX-style label, ficha inside the season window
assert.equal(classifySeasonVsFicha("2025/2026", new Date("2025-11-01")), "straddling");
// Europe/Liga MX boundary: exactly at window start (Aug 1) -- inside, so straddling
assert.equal(classifySeasonVsFicha("2025/2026", new Date("2025-08-01T00:00:00Z")), "straddling");
// Europe/Liga MX boundary: one day before window start -- post
assert.equal(classifySeasonVsFicha("2025/2026", new Date("2025-07-31T23:59:59Z")), "post");
// Americas-style label, ficha squarely before the season
assert.equal(classifySeasonVsFicha("2025", new Date("2024-06-01")), "post");
// Americas-style label, ficha squarely after the season
assert.equal(classifySeasonVsFicha("2024", new Date("2025-06-01")), "pre");
// Americas-style label, ficha inside the season window
assert.equal(classifySeasonVsFicha("2025", new Date("2025-06-01")), "straddling");
// Unparseable label
assert.equal(classifySeasonVsFicha("Acumulado", new Date("2025-06-01")), null);
// Invalid ficha date
assert.equal(classifySeasonVsFicha("2025", new Date("not-a-date")), null);

console.log("ALL ASSERTIONS PASSED");
```

- [ ] **Step 2: Run it to confirm it fails (module doesn't exist yet)**

Run: `cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard && node .scratch-verify-ficha-performance.mjs`
Expected: FAIL with `Cannot find module './lib/fichaPerformance.js'` (or similar "module not found").

- [ ] **Step 3: Implement `lib/fichaPerformance.js`**

Create `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/lib/fichaPerformance.js`:

```js
import SbScMetricBySeason from "@/models/SbScMetricBySeason";
import { LABEL_MAP, realColumnsForPosition, statsProjection, buildPositionStatRows } from "@/lib/positionMetrics";

// Ventana calendario aproximada de una temporada, a partir de su propio
// season_name -- "YYYY/YYYY+1" (liga europea/Liga MX) arranca ago 1 del
// primer año y termina jun 30 del segundo; "YYYY" (calendario americano)
// cubre el año calendario completo. No hay fechas de inicio/fin reales
// de temporada en ningún lado del pipeline -- esta aproximación es
// suficiente para una clasificación pre/post/straddling a nivel de
// temporada (ver spec, sección "Non-goals": no se intenta un split a
// nivel de fecha exacta).
function seasonWindow(seasonLabel) {
  if (typeof seasonLabel !== "string") return null;

  let m = seasonLabel.match(/^(\d{4})\/(\d{4})$/);
  if (m) {
    const [, y1, y2] = m;
    return {
      start: new Date(`${y1}-08-01T00:00:00Z`),
      end: new Date(`${y2}-06-30T23:59:59Z`),
    };
  }

  m = seasonLabel.match(/^(\d{4})$/);
  if (m) {
    const [, y] = m;
    return {
      start: new Date(`${y}-01-01T00:00:00Z`),
      end: new Date(`${y}-12-31T23:59:59Z`),
    };
  }

  return null;
}

// "pre" = la temporada terminó antes de la ficha. "post" = la temporada
// empezó después de la ficha. "straddling" = la ficha se hizo DURANTE esa
// temporada -- no cuenta para ninguno de los dos lados (ver spec).
export function classifySeasonVsFicha(seasonLabel, fichaDate) {
  const window = seasonWindow(seasonLabel);
  if (!window) return null;
  if (!(fichaDate instanceof Date) || Number.isNaN(fichaDate.getTime())) return null;

  const t = fichaDate.getTime();
  if (t < window.start.getTime()) return "post";
  if (t > window.end.getTime()) return "pre";
  return "straddling";
}

// seasonDocs: el propio historial de SbScMetricBySeason del jugador
// (ya resuelto por el caller -- ver app/jugadores/[id]/page.jsx, que ya
// lo trae para el dropdown de temporada, así que no hace falta volver a
// pedirlo acá). fichaDate: fecha de la ficha más reciente del jugador.
export async function buildFichaPerformanceTimeline(seasonDocs, fichaDate) {
  if (!Array.isArray(seasonDocs) || !seasonDocs.length) return [];
  if (!(fichaDate instanceof Date) || Number.isNaN(fichaDate.getTime())) return [];

  const withWindow = [];
  for (const doc of seasonDocs) {
    const classification = classifySeasonVsFicha(doc.season_name, fichaDate);
    if (!classification) continue;

    const pg = doc.position_group;
    let rawStatRows = [];
    if (pg && LABEL_MAP[pg]) {
      const statCols = realColumnsForPosition(pg);
      const statPool = await SbScMetricBySeason.find(
        { position_group: pg, season_name: doc.season_name },
        statsProjection(statCols)
      ).lean();
      rawStatRows = buildPositionStatRows(doc, statPool, pg);
    }

    withWindow.push({
      seasonLabel: doc.season_name,
      classification,
      dataScore: doc.DataScore ?? null,
      ameScore: doc.DataScoreAmerica ?? null,
      ameScorePlus: doc.DataScoreAmericaPlus ?? null,
      rawStatRows,
      _start: seasonWindow(doc.season_name).start,
    });
  }

  withWindow.sort((a, b) => a._start - b._start);
  return withWindow.map(({ _start, ...rest }) => rest);
}
```

- [ ] **Step 4: Run the verification script again**

Run: `cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard && node .scratch-verify-ficha-performance.mjs`
Expected: `ALL ASSERTIONS PASSED` printed, exit code 0.

- [ ] **Step 5: Delete the scratch verification script (it is not a committed test, per this repo's existing convention of no test suite)**

Run: `rm /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/.scratch-verify-ficha-performance.mjs`

- [ ] **Step 6: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git add lib/fichaPerformance.js
git commit -m "$(cat <<'EOF'
Add lib/fichaPerformance.js: season-vs-ficha classification + timeline

classifySeasonVsFicha() buckets a season as pre/post/straddling the
player's most recent ficha date; buildFichaPerformanceTimeline() builds
the sorted per-season timeline (DataScore/AmeScore/AmeScore+ + raw
per-90 stat rows) consumed by the player profile.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Wire the timeline into `app/jugadores/[id]/page.jsx`

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/page.jsx`

**Interfaces:**
- Consumes: `buildFichaPerformanceTimeline(seasonDocs, fichaDate)` from Task 2 (`@/lib/fichaPerformance`). `seasonDocs` is the array already fetched at `page.jsx:72` (`const seasonDocs = await SbScMetricBySeason.find({ transfermarkt_id: transfermarktId }).lean();`) — reused as-is, no new Mongo query for the anchor player's own rows. `playerDoc.ficha.archivos` (array, already loaded via the existing `Player.findOne(...)` at `page.jsx:39`).
- Produces: a new prop, `fichaPerformanceTimeline` (the array `buildFichaPerformanceTimeline` returns, or `[]`), passed to `<PlayerProfileView>` alongside the existing `availableSeasons`/`injuryHistory`/`teamHistory` props.

- [ ] **Step 1: Add the import**

In `app/jugadores/[id]/page.jsx`, alongside the other `@/lib/...` imports near the top of the file (after the existing `import { displayDataScore, ... } from "@/lib/scoreScale";` line), add:

```js
import { buildFichaPerformanceTimeline } from "@/lib/fichaPerformance";
```

- [ ] **Step 2: Compute the timeline right after `availableSeasons` is built**

In `app/jugadores/[id]/page.jsx`, immediately after the existing block that computes `availableSeasons` (ends with `.sort();` around line 87), add:

```js
// Timeline de performance pre/post ficha -- ancla en la ficha MAS
// RECIENTE del jugador (archivos.length - 1; el array es append-only,
// ver app/api/players/[id]/ficha/route.js). seasonDocs ya está resuelto
// arriba (sin filtrar por formato de calendario -- a diferencia de
// availableSeasons, el timeline quiere el historial real del jugador
// en cualquier calendario, no solo el "principal"). Vacío sin fichas
// (consulta ni siquiera corre) o sin fecha válida.
const fichaArchivos = playerDoc.ficha?.archivos || [];
const ultimaFicha = fichaArchivos.length ? fichaArchivos[fichaArchivos.length - 1] : null;
const fichaPerformanceTimeline = ultimaFicha?.fecha
  ? await buildFichaPerformanceTimeline(seasonDocs, new Date(ultimaFicha.fecha))
  : [];
```

- [ ] **Step 3: Pass the new prop to `<PlayerProfileView>`**

In the same file, in the `<PlayerProfileView ... />` call (around line 238-246, where `availableSeasons={availableSeasons}` already appears), add:

```jsx
      fichaPerformanceTimeline={fichaPerformanceTimeline}
```

- [ ] **Step 4: Accept the prop in `PlayerProfileView` (no rendering yet -- that's Task 4)**

In `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerProfileView.jsx`, in the component's destructured props (around line 309-319, where `availableSeasons = []` already appears), add:

```js
  fichaPerformanceTimeline = [],
```

- [ ] **Step 5: Verify the dev server compiles clean**

Run (if not already running): `cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard && npm run dev`
Open any player profile with a ficha in the browser (e.g. via the Database page's player search) and confirm the page loads with no server-side error overlay, no new console errors, and no visible change yet (the prop is accepted but not rendered until Task 4).

- [ ] **Step 6: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git add app/jugadores/\[id\]/page.jsx app/jugadores/\[id\]/PlayerProfileView.jsx
git commit -m "$(cat <<'EOF'
Compute fichaPerformanceTimeline server-side, thread into PlayerProfileView

Reuses the already-fetched seasonDocs (no new query for the anchor
player) and the most recent entry in player.ficha.archivos. Prop is
accepted but not yet rendered -- UI lands in the next commit.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: `FichaPerformanceTimeline.jsx` component + render it on the profile

**Files:**
- Create: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/FichaPerformanceTimeline.jsx`
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerProfileView.jsx`
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/globals.css`

**Interfaces:**
- Consumes: the `fichaPerformanceTimeline` array shape produced by Task 2/3 (`{ seasonLabel, classification, dataScore, ameScore, ameScorePlus, rawStatRows }[]`). Existing CSS classes `sc-table`, `sc-fichameta`, `sc-btn ghost small` (all already defined in `app/globals.css` and used elsewhere in this same file, e.g. `RankingTable.jsx:744` for the `sc-btn ghost small` expand-toggle pattern this task follows).
- Produces: default export `FichaPerformanceTimeline({ timeline })` — renders `null` when `timeline` is empty, otherwise one table row per season.

- [ ] **Step 1: Create the component**

Create `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/FichaPerformanceTimeline.jsx`:

```jsx
"use client";

import { useState } from "react";

const CLASSIFICATION_LABEL = {
  pre: "Antes de la ficha",
  post: "Después de la ficha",
  straddling: "Temporada de la ficha",
};

function ScoreCell({ value }) {
  return value == null ? <span className="sc-fichameta">—</span> : <span>{value}</span>;
}

function SeasonRow({ row }) {
  const [open, setOpen] = useState(false);
  const rowClass = row.classification === "straddling" ? "sc-ficha-timeline-straddle" : "";

  return (
    <>
      <tr className={rowClass}>
        <td>{row.seasonLabel}</td>
        <td>{CLASSIFICATION_LABEL[row.classification]}</td>
        <td><ScoreCell value={row.dataScore} /></td>
        <td><ScoreCell value={row.ameScore} /></td>
        <td><ScoreCell value={row.ameScorePlus} /></td>
        <td>
          {row.rawStatRows.length > 0 && (
            <button type="button" className="sc-btn ghost small" onClick={() => setOpen((o) => !o)}>
              {open ? "Ocultar detalle" : "Ver detalle"}
            </button>
          )}
        </td>
      </tr>
      {open && (
        <tr className={rowClass}>
          <td colSpan={6}>
            <table className="sc-table">
              <tbody>
                {row.rawStatRows.map((r) => (
                  <tr key={r.key}>
                    <td>{r.metric}</td>
                    <td>{r.value}</td>
                    <td>{r.percentile != null ? `${r.percentile}%` : "—"}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </td>
        </tr>
      )}
    </>
  );
}

// Timeline temporada-por-temporada del jugador, cada fila marcada como
// anterior/posterior/durante su ficha más reciente (ver
// lib/fichaPerformance.js). No se renderiza nada si timeline viene
// vacío -- ni el título de la sección ni la tabla (ver gate en
// PlayerProfileView.jsx, que además evita llamar a este componente si
// el jugador no tiene ninguna ficha).
export default function FichaPerformanceTimeline({ timeline }) {
  if (!timeline || !timeline.length) return null;

  return (
    <table className="sc-table">
      <thead>
        <tr>
          <th>Temporada</th>
          <th>Momento</th>
          <th>DataScore</th>
          <th>AmeScore</th>
          <th>AmeScore+</th>
          <th></th>
        </tr>
      </thead>
      <tbody>
        {timeline.map((row) => (
          <SeasonRow key={row.seasonLabel} row={row} />
        ))}
      </tbody>
    </table>
  );
}
```

- [ ] **Step 2: Add the straddling-row highlight CSS**

In `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/globals.css`, immediately after the existing `.sc-fichameta { ... }` rule (around line 3888), add:

```css
/* Fila de la temporada en la que se hizo la ficha más reciente del
   jugador (FichaPerformanceTimeline.jsx) -- marca visualmente el punto
   de referencia del timeline pre/post, sin inventar un nuevo color de
   marca (reusa el mismo tono de advertencia que .sc-tag.warn). */
.sc-ficha-timeline-straddle {
  background: rgba(234, 179, 8, 0.08);
}
```

- [ ] **Step 3: Import the component and render it in `PlayerProfileView.jsx`**

In `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerProfileView.jsx`, add the import alongside the other same-directory imports (near `import PlayerTeamHistoryTable from "./PlayerTeamHistoryTable";`, around line 45):

```js
import FichaPerformanceTimeline from "./FichaPerformanceTimeline";
```

Then, in the "info" tab's JSX, immediately after the existing `teamHistory` block and before the tab's closing `</>` (around line 1080, right after `PlayerTeamHistoryTable`'s closing `)}`), add:

```jsx
            {player.ficha?.archivos?.length > 0 && fichaPerformanceTimeline.length > 0 && (
              <div style={{ marginTop: 36 }}>
                <div className="sc-section-title">
                  <span>📈</span> Performance Pre/Post Ficha
                </div>
                <FichaPerformanceTimeline timeline={fichaPerformanceTimeline} />
                <div className="sc-fichameta" style={{ marginTop: 8 }}>
                  Ancla: fecha de la ficha más reciente de este jugador.
                </div>
              </div>
            )}
```

- [ ] **Step 4: Verify live in the browser**

With the dev server running (`npm run dev`), open the profile of a player who has at least one ficha AND at least one season of `sb_sc_metrics_by_season` coverage (e.g. search in Database page, open their profile). Confirm:
- The new "Performance Pre/Post Ficha" section appears below "Historial de Equipos", with one row per season.
- Each row's "Momento" column shows a sensible pre/post/straddling label relative to that player's ficha date (cross-check the ficha's displayed date against the season rows shown).
- "Ver detalle" expands/collapses the raw per-90 stat rows for that season.
- A player with NO ficha shows no trace of this section at all (open a player without one to confirm).
- A player with a ficha but no `sb_sc_metrics_by_season` coverage at all also shows no trace of the section (the `fichaPerformanceTimeline.length > 0` half of the gate handles this).

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git add "app/jugadores/[id]/FichaPerformanceTimeline.jsx" "app/jugadores/[id]/PlayerProfileView.jsx" app/globals.css
git commit -m "$(cat <<'EOF'
Render the ficha pre/post performance timeline on the player profile

New season-by-season table below Historial de Equipos, gated on the
player having both a ficha and at least one classified season. Each
row shows DataScore/AmeScore/AmeScore+ with raw per-90 stats collapsed
under a per-row toggle; the ficha's own season is visually marked.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

---

### Task 5: Deploy + live verification

**Files:** none (operational task — push, trigger CI, verify).

**Interfaces:**
- Consumes: commits from Tasks 1-4 in both repos.
- Produces: nothing new; this task only confirms the prior four tasks work end-to-end against real data.

- [ ] **Step 1: Push `scout_dashboard`'s commit (Task 1) to `main`**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git push origin main
```

- [ ] **Step 2: Trigger the CI pipeline and wait for it to complete**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
gh workflow run deploy.yml
```

Poll with `gh run list --limit 1` / `gh run watch <run-id> --exit-status` until it completes. Expected: success. If it fails, read `gh run view <run-id> --log-failed` — if the failure is in the newly-added 24/25 pulls specifically (not a pre-existing/unrelated infra issue), that is a real defect in Task 1 and must be fixed and re-verified before continuing; do not treat it as this task's own scope to silently patch around.

- [ ] **Step 3: Confirm 24/25 season rows landed in Mongo**

After the pipeline succeeds, spot-check that `sb_sc_metrics_by_season` has `season_name` values of `"2024/2025"` and/or `"2024"` for at least a few players (e.g. via a quick `mongosh`/Compass query against the production `MONGO_URI`, or by temporarily adding a console log in a local script using the `mongolite`/Mongoose connection already configured in this project). Expected: non-zero rows for at least the leagues known to have 24/25 StatsBomb access.

- [ ] **Step 4: Push `ca_scouting_dashboard`'s commits (Tasks 2-4)**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git push origin main
```

Per this repo's established session practice, confirm with the user before pushing if this session has not already gotten a standing "pushes OK" for this repo.

- [ ] **Step 5: Live verification in production/Vercel**

Once Vercel redeploys (or immediately if reading live from Mongo without a redeploy being required — confirm which applies), open a real player's profile who has both a ficha and 24/25 StatsBomb coverage. Confirm the "Performance Pre/Post Ficha" section shows a `"pre"`-tagged 24/25 row (or whichever season genuinely precedes their ficha date) alongside the `"post"`/`"straddling"` rows, and that the values match what the season dropdown already shows for 25/26 and 26/27.

## Execution Handoff

After saving this plan, offer the user the subagent-driven-development vs. inline-execution choice per the writing-plans skill. Task 1 lives in `scout_dashboard`; Tasks 2-4 live in `ca_scouting_dashboard` and are strictly sequential (each depends on the previous task's exported interface) — no parallelization opportunity regardless of execution mode chosen. Task 5 spans both repos and must run last.
