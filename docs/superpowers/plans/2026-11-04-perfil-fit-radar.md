# Perfil-Fit Radar Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a radar chart to the player profile modal showing each player's fit score (0-100) against every perfil defined for their position, surfacing data `assign_player_profiles()` already computes and currently discards.

**Architecture:** `app.R`'s `assign_player_profiles()` gains two new fields (`PerfilGroup`, `PerfilScores`) retaining the full per-perfil score matrix it already builds internally. These flow automatically through the existing `get_db_master()` → `sync_to_mongo.R` pipeline (which syncs the whole table, no column allow-list) with zero changes to the sync script itself. `ca_scouting_dashboard` gets one new hand-rolled-SVG radar component (sibling to the existing `PlayerRadarChart.jsx`, not a modification of it, since null-handling must differ) and two new fields threaded through the player profile page's existing `meta` object.

**Tech Stack:** R (dplyr), Next.js server components, plain React/SVG (no charting library, matching this repo's existing convention).

**Spec:** `docs/superpowers/specs/2026-11-04-perfil-fit-radar-design.md`

## Global Constraints

- `Perfil`/`Perfil_secundario`/`Score_perfil`/`Score_perfil_secundario`/`Gap_perfil`/`Cobertura_perfil` (the existing top-two fields) must not change in any way — this plan is strictly additive.
- A perfil score under `MIN_PROFILE_COVERAGE` (0.60) must appear in `PerfilScores` as `null`, never omitted from the object and never silently coerced to 0 at the data layer — the radar component is what turns `null` into a visible zero-radius dashed spoke, not the data.
- Radar axis count must always equal the full perfil count for that player's profile group (4 or 5 depending on group) — never drop an axis for missing data.
- This is Acumulado-only — no per-season version, no changes to `SbScMetricBySeason` or the season selector.
- No new charting dependency — the new component is hand-rolled SVG, following `PlayerRadarChart.jsx`'s existing visual conventions (same `SIZE`/`RADIUS`/`RINGS` scale, same gold palette via `var(--gold-a)`/`var(--gold-b)`).

---

### Task 1: `app.R` — retain and verify the full per-perfil score matrix

**Files:**
- Modify: `app.R:677-687` (inside `assign_player_profiles()`)

**Interfaces:**
- Consumes: the existing `scores` matrix (`app.R:649-657`, already computed, one column per perfil in `PROFILE_METRIC_DEFS[[grp]]`, column order = that list's definition order) and `grp` (the current profile-group name in the loop).
- Produces: two new columns on `out[[grp]]`'s `data.frame` — `PerfilGroup` (character, repeated `grp` for every row) and `PerfilScores` (list-column; each element is a named R list, e.g. `list(Cazador = 72.3, Móvil = 55.1, Retenedor = NA, Aéreo = 40.2, Acosador = NA)`, one entry per perfil in that group, in `scores`' column order). Both flow unchanged through `dplyr::bind_rows(out)` (`app.R:689`) and the existing `left_join(profiles, by = "player_id")` (`app.R:2588`) into `get_db_master()`'s output — no other file needs to reference these by name for this task.

- [ ] **Step 1: Add the two new columns**

In `app.R`, immediately after the existing line `out[[grp]]$Cobertura_perfil <- main_coverage * 100` (`app.R:686`), add:

```r
    out[[grp]]$PerfilGroup <- grp
    # Lista de listas (no vector con nombres) -- as.list() por fila evita
    # el riesgo de que dplyr::bind_rows()/mongolite serialicen un vector
    # con nombres de forma inconsistente entre perfiles de distintos
    # grupos (cada grupo tiene sus propios nombres de columna en `scores`,
    # distintos de grupo a grupo). NA se preserva tal cual -- se traduce a
    # null en JSON, que el radar del frontend interpreta como "sin datos"
    # (ver spec).
    out[[grp]]$PerfilScores <- lapply(seq_len(nrow(scores)), function(i) as.list(scores[i, ]))
```

- [ ] **Step 2: Verify the new columns against the existing top-two, for real data**

Run (from the repo root, same pattern as every other R verification this session):

```r
Rscript -e '
suppressWarnings(suppressMessages(source("app.R")))
dat <- get_all_players_df()
profiles <- assign_player_profiles(dat)
stopifnot("PerfilGroup present" = "PerfilGroup" %in% names(profiles))
stopifnot("PerfilScores present" = "PerfilScores" %in% names(profiles))

# Para cada fila con un Perfil asignado, el score de esa misma clave dentro
# de PerfilScores debe ser EXACTAMENTE igual a Score_perfil (y, si hay
# secundario, Score_perfil_secundario con la clave de Perfil_secundario) --
# si no coinciden, el nuevo campo no está leyendo la misma matriz `scores`
# que ya produce el top-two existente.
check_rows <- which(!is.na(profiles$Perfil))[1:50]
for (i in check_rows) {
  row <- profiles[i, ]
  ps <- row$PerfilScores[[1]]
  stopifnot(isTRUE(all.equal(ps[[row$Perfil]], row$Score_perfil, tolerance = 1e-6)))
  if (!is.na(row$Perfil_secundario)) {
    stopifnot(isTRUE(all.equal(ps[[row$Perfil_secundario]], row$Score_perfil_secundario, tolerance = 1e-6)))
  }
}
cat("PASS: PerfilScores matches Score_perfil/Score_perfil_secundario for", length(check_rows), "sampled rows\n")

# Cuenta de ejes por grupo -- debe coincidir con PROFILE_METRIC_DEFS.
for (grp in names(PROFILE_METRIC_DEFS)) {
  sample_row <- profiles[profiles$PerfilGroup == grp, ][1, ]
  n_axes <- length(sample_row$PerfilScores[[1]])
  n_defined <- length(PROFILE_METRIC_DEFS[[grp]])
  cat(sprintf("%s: %d axes (expected %d) -- %s\n", grp, n_axes, n_defined, if (n_axes == n_defined) "PASS" else "FAIL"))
}
'
```

Expected: the cross-check prints `PASS: PerfilScores matches...`, and every profile group line prints `PASS` (Portero/Defensa Central/Lateral/Extremo = 4, Volante/Delantero = 5).

- [ ] **Step 3: Verify the list-column serializes to a proper JSON object (no live Mongo needed)**

Run:

```r
Rscript -e '
suppressWarnings(suppressMessages(source("app.R")))
dat <- get_all_players_df()
profiles <- assign_player_profiles(dat)
sample_row <- profiles[!is.na(profiles$Perfil), ][1, ]
j <- jsonlite::toJSON(list(PerfilGroup = sample_row$PerfilGroup, PerfilScores = sample_row$PerfilScores[[1]]), auto_unbox = TRUE, na = "null")
cat(j, "\n")
parsed <- jsonlite::fromJSON(j)
stopifnot(is.list(parsed$PerfilScores), !is.null(names(parsed$PerfilScores)))
cat("PASS: PerfilScores round-trips as a named JSON object, not an array\n")
'
```

Expected: the printed JSON shows `"PerfilScores":{"Cazador":72.3,"Móvil":55.1,...}` (an object, not `[72.3, 55.1, ...]`), `NA` values print as `null`, and the round-trip check prints `PASS`. If instead it prints as a JSON array (losing the names), stop and report this as a BLOCKED finding rather than guessing at a fix — the representation chosen in Step 1 was deliberately picked to avoid this, so a failure here means something unexpected about this R version/jsonlite version's behavior needs to be understood before continuing.

- [ ] **Step 4: Confirm nothing existing changed**

Run: `git diff app.R | grep "^-"` (expect no output beyond pure context lines — this task is purely additive, never removes or modifies an existing line). Also re-run the existing `Score_perfil`/`Perfil` logic mentally-unchanged check: `git diff app.R` should show exactly 2 new lines added (`out[[grp]]$PerfilGroup <- ...` and `out[[grp]]$PerfilScores <- ...`) plus their comment, nothing else touched.

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add app.R
git commit -m "$(cat <<'EOF'
Retain full per-perfil score matrix in assign_player_profiles()

PerfilGroup/PerfilScores surface what the function already computes
internally (every perfil's score for every player, not just the
top-two that survive today) -- feeds a new fit-radar on the player
profile (ca_scouting_dashboard). Flows automatically through
get_db_master() -> sync_to_mongo.R (full-table sync, no column
allow-list) with no changes needed to the sync script itself.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: `PlayerPerfilRadar.jsx` — new radar component

**Files:**
- Create: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerPerfilRadar.jsx`

**Interfaces:**
- Consumes: `perfilScores` (object, perfil name → number|null, e.g. `{ Cazador: 72.3, Móvil: 55.1, Retenedor: null, Aéreo: 40.2, Acosador: null }`) and `profileGroup` (string, e.g. `"Delantero"`, used only for a title label).
- Produces: default export `PlayerPerfilRadar({ perfilScores, profileGroup })` — a React component rendering one axis per key of `perfilScores`, in that object's own key order.

- [ ] **Step 1: Create the component**

Create `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerPerfilRadar.jsx`:

```jsx
"use client";

// Mismo lenguaje visual que PlayerRadarChart.jsx (mismos SIZE/RADIUS/RINGS,
// misma paleta dorada) pero un componente nuevo, no una modificación de
// ese -- ese otro FILTRA los ejes sin percentil (rows.filter(r =>
// r.percentile != null)), justo lo opuesto de lo que este radar necesita:
// el eje de un perfil sin cobertura suficiente (score null, ver
// MIN_PROFILE_COVERAGE en app.R) debe seguir apareciendo, marcado como
// "sin datos", nunca desaparecer -- la forma del radar siempre debe tener
// tantos ejes como perfiles tiene esa posición.
const SIZE = 420;
const CENTER = SIZE / 2;
const RADIUS = 130;
const RINGS = [20, 40, 60, 80, 100];
const LABEL_RING = 100 + 24;

function pointFor(angle, value, radius = RADIUS) {
  const r = (Math.max(0, value) / 100) * radius;
  return { x: CENTER + r * Math.sin(angle), y: CENTER - r * Math.cos(angle) };
}

function polygonPath(points) {
  return points.map((p, i) => `${i === 0 ? "M" : "L"}${p.x} ${p.y}`).join(" ") + " Z";
}

export default function PlayerPerfilRadar({ perfilScores, profileGroup }) {
  const names = perfilScores ? Object.keys(perfilScores) : [];
  if (names.length < 3) {
    return <div className="sc-fichameta">No hay suficientes perfiles definidos para esta posición.</div>;
  }

  const n = names.length;
  const angleStep = (2 * Math.PI) / n;
  const values = names.map((name) => perfilScores[name]);
  const dataPoints = values.map((v, i) => pointFor(i * angleStep, v ?? 0));
  const dataPath = polygonPath(dataPoints);

  return (
    <div style={{ width: "100%" }}>
      {profileGroup && (
        <div className="sc-fichameta" style={{ textAlign: "center", marginBottom: 4 }}>
          Perfil: {profileGroup}
        </div>
      )}
      <svg
        width="100%"
        viewBox={`0 0 ${SIZE} ${SIZE}`}
        style={{ maxWidth: "100%", display: "block", margin: "0 auto", overflow: "visible" }}
      >
        <defs>
          <radialGradient id="perfilRadarFill" cx="50%" cy="50%" r="65%">
            <stop offset="0%" stopColor="var(--gold-a)" stopOpacity="0.38" />
            <stop offset="100%" stopColor="var(--gold-b)" stopOpacity="0.14" />
          </radialGradient>
          <linearGradient id="perfilRadarStroke" x1="0%" y1="0%" x2="100%" y2="100%">
            <stop offset="0%" stopColor="var(--gold-a)" />
            <stop offset="100%" stopColor="var(--gold-b)" />
          </linearGradient>
        </defs>

        <circle cx={CENTER} cy={CENTER} r={RADIUS} fill="var(--surface3)" opacity="0.6" stroke="none" />

        {RINGS.map((ring, i) => (
          <circle
            key={`band-${ring}`}
            cx={CENTER}
            cy={CENTER}
            r={(ring / 100) * RADIUS}
            fill={i % 2 === 0 ? "rgba(255,255,255,0.02)" : "transparent"}
            stroke="none"
          />
        ))}
        {RINGS.filter((ring) => ring !== 100).map((ring) => (
          <circle
            key={`ring-${ring}`}
            cx={CENTER}
            cy={CENTER}
            r={(ring / 100) * RADIUS}
            fill="none"
            stroke="var(--border)"
            strokeWidth="1"
            opacity="0.3"
          />
        ))}

        {names.map((_, i) => {
          const outer = pointFor(i * angleStep, 100);
          return (
            <line
              key={i}
              x1={CENTER}
              y1={CENTER}
              x2={outer.x}
              y2={outer.y}
              stroke="var(--border)"
              strokeWidth="1"
              opacity="0.4"
            />
          );
        })}

        <circle cx={CENTER} cy={CENTER} r="2" fill="var(--text-faint)" />

        <path
          d={dataPath}
          fill="url(#perfilRadarFill)"
          stroke="url(#perfilRadarStroke)"
          strokeWidth="2.5"
          strokeLinejoin="round"
        />

        {/* Cada vértice: relleno dorado sólido con el número si hay score
            real; marcador hueco con trazo punteado y sin número si es
            null (sin cobertura suficiente) -- nunca se omite el eje
            entero, solo se distingue visualmente "sin datos" de "score
            bajo real". */}
        {dataPoints.map((p, i) => {
          const hasScore = values[i] != null;
          return (
            <g key={i}>
              <circle
                cx={p.x}
                cy={p.y}
                r="11"
                fill={hasScore ? "var(--surface)" : "none"}
                stroke="var(--gold-a)"
                strokeWidth="2"
                strokeDasharray={hasScore ? undefined : "3,2"}
              />
              {hasScore && (
                <text
                  x={p.x}
                  y={p.y}
                  textAnchor="middle"
                  dominantBaseline="central"
                  style={{ fontSize: 11, fontWeight: 700, fill: "var(--gold-a)" }}
                >
                  {Math.round(values[i])}
                </text>
              )}
            </g>
          );
        })}

        {names.map((name, i) => {
          const anglePos = i * angleStep;
          const labelPoint = pointFor(anglePos, LABEL_RING);
          const anchor = Math.abs(labelPoint.x - CENTER) < 8 ? "middle" : labelPoint.x > CENTER ? "start" : "end";
          return (
            <text
              key={name}
              textAnchor={anchor}
              style={{ fontSize: 13, fontWeight: 600, fill: "var(--text-dim)" }}
            >
              <tspan x={labelPoint.x} y={labelPoint.y}>
                {name}
              </tspan>
            </text>
          );
        })}
      </svg>
    </div>
  );
}
```

- [ ] **Step 2: Verify by reading (no test framework in this repo, per established convention)**

Confirm by inspection: a `perfilScores` object with 4 keys produces `n = 4`, `angleStep = 90°`, 4 evenly-spaced axes; 5 keys produces 5 axes at 72° apart. A key with value `null` produces `hasScore = false` → hollow dashed marker at `pointFor(angle, 0)` (center point, radius 0) with no number, while a key with a real number produces a solid gold marker with that number — both still contribute one line/label (axis), never omitted from `names.map`.

- [ ] **Step 3: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git add "app/jugadores/[id]/PlayerPerfilRadar.jsx"
git commit -m "$(cat <<'EOF'
Add PlayerPerfilRadar.jsx: per-perfil fit radar component

Sibling to PlayerRadarChart.jsx (same visual language), not a
modification of it -- that component drops any axis with a null
percentile, the opposite of what this radar needs (every perfil for
the position must always appear as an axis, null scores shown as a
hollow dashed marker instead of being dropped).

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: Wire `PerfilGroup`/`PerfilScores` into the player profile

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/page.jsx`
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerProfileView.jsx`

**Interfaces:**
- Consumes: `PlayerPerfilRadar` (Task 2, props `perfilScores`/`profileGroup`). `sbSc.PerfilGroup`/`sbSc.PerfilScores` from Mongo (Task 1's fields, already flowing into the `SbScMetric` document `page.jsx` already fetches at `page.jsx:46` — no new query).
- Produces: `meta.perfilGroup`/`meta.perfilScores` (added to the existing `meta` object already passed whole to `<PlayerProfileView>` — no new prop needs threading separately, since `meta` is already a prop).

- [ ] **Step 1: Add the two fields to `meta`**

In `app/jugadores/[id]/page.jsx`, inside the existing `meta` object literal (ends with `scPhysicalQualityReason: sbSc?.sc_physical_quality_reason ?? null,` around line 222), add:

```js
    // Radar de ajuste a perfil (assign_player_profiles(), app.R) --
    // Acumulado únicamente, no existe por temporada (ver spec
    // 2026-11-04-perfil-fit-radar-design.md, Non-goals).
    perfilGroup: sbSc?.PerfilGroup ?? null,
    perfilScores: sbSc?.PerfilScores ?? null,
```

No change to the `<PlayerProfileView>` call itself — `meta={meta}` already passes these through.

- [ ] **Step 2: Import and render the new radar in `PlayerProfileView.jsx`**

In `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerProfileView.jsx`, add the import alongside `PlayerRadarChart`'s own import (near line 41):

```js
import PlayerPerfilRadar from "./PlayerPerfilRadar";
```

Then, immediately after Grid 2's closing `</div>` (the SkillCorner table/radar grid, which ends around line 1046 — the `{!scTable.message && (...)}` block's closing), add a new section, gated on `meta.perfilScores` being present and non-empty:

```jsx
            {meta.perfilScores && Object.keys(meta.perfilScores).length > 0 && (
              <div style={{ marginTop: 24 }}>
                <div className="sc-section-title">
                  <span>🎯</span> Ajuste a Perfil
                </div>
                <div className="sc-profile-radarwrap">
                  <PlayerPerfilRadar perfilScores={meta.perfilScores} profileGroup={meta.perfilGroup} />
                </div>
                <div className="sc-fichameta" style={{ textAlign: "center", marginTop: 6, width: "100%" }}>
                  Score (0-100) contra cada perfil de su posición -- percentil ponderado, Acumulado. Un eje
                  punteado/vacío indica cobertura de métricas insuficiente para ese perfil específico.
                </div>
              </div>
            )}
```

(Placed as its own full-width section, like the existing Historial de Lesiones/Equipos sections below it, rather than forced into a two-column grid — this radar has no natural paired table to sit beside, unlike the StatsBomb/SkillCorner radars which each pair with their own stat table.)

- [ ] **Step 3: Verify**

Run `npm run build` from `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard` and confirm the `/jugadores/[id]` route compiles clean. If the dev server/browser is reachable, open a real player's profile from each of the 6 profile groups (one Portero, one Defensa Central, one Lateral, one Volante, one Extremo, one Delantero) and confirm: the new "Ajuste a Perfil" section appears with the right axis count (4 or 5), the two highest-scoring axes' numbers match that same player's already-displayed `Score_perfil`/`Score_perfil_secundario`, and a player known to be under-coverage for one perfil shows a hollow dashed marker on that axis specifically. If live browser verification is blocked by the same sandbox/auth limitation hit on earlier tasks this session, say so plainly and rely on the build pass plus a careful code read-through as the substitute, consistent with that established precedent.

- [ ] **Step 4: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git add app/jugadores/\[id\]/page.jsx app/jugadores/\[id\]/PlayerProfileView.jsx
git commit -m "$(cat <<'EOF'
Render the perfil-fit radar on the player profile

New "Ajuste a Perfil" section below the SkillCorner grid, gated on
the player having a non-empty PerfilScores (i.e. their position_group
maps to one of the 6 profile groups). meta.perfilGroup/perfilScores
ride on the existing meta prop -- no new prop threading needed.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Deploy + live verification

**Files:** none (operational task — push, trigger CI, verify).

- [ ] **Step 1: Push `scout_dashboard`'s commit (Task 1)**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git push origin main
```

- [ ] **Step 2: Trigger the CI pipeline and wait for both jobs to complete**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
gh workflow run deploy.yml
```

Poll with `gh run list --limit 1` / `gh run watch <run-id> --exit-status` until `sync-and-deploy` completes. Expected: success. If either job fails, read `gh run view <run-id> --log-failed` — if the failure traces to this plan's own change (Task 1's `app.R` edit, or the sync step choking on the new list-column despite Task 1 Step 3's local verification passing), that is a real defect to fix and re-verify, not something to silently route around.

- [ ] **Step 3: Confirm `PerfilGroup`/`PerfilScores` landed in Mongo correctly**

Spot-check a few synced `sb_sc_metrics` documents (one per profile group, matching Task 3's Step 3 live-verification list) to confirm `PerfilScores` is a proper nested object (not an array, not flattened into dotted keys) with the right key count and `null`s preserved, and `PerfilGroup` matches that player's actual position.

- [ ] **Step 4: Push `ca_scouting_dashboard`'s commits (Tasks 2-3)**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git push origin main
```

Confirm with the user before pushing, per this repo's established per-instance push confirmation practice.

- [ ] **Step 5: Live verification**

Once live (Vercel reads Mongo directly, no redeploy required for data, but the new component/page code does need Vercel to pick up the push), open real player profiles across all 6 profile groups and confirm the radar renders correctly end-to-end: right axis count, correct top-two alignment, correct null-axis treatment, matching what Task 3 Step 3 already verified against the build (now against live production data).

## Execution Handoff

After saving this plan, offer the user the subagent-driven-development vs. inline-execution choice per the writing-plans skill. Task 1 lives in `scout_dashboard`; Tasks 2-3 live in `ca_scouting_dashboard` and are sequential (Task 3 depends on Task 2's component; neither depends on Task 1's live data to build or review, only to show real numbers). Task 4 spans both repos and must run last, after Tasks 1-3 are all complete and reviewed clean.
