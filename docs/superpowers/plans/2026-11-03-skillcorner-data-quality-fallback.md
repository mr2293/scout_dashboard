# SkillCorner Physical Data Quality Fallback Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build one physical-data row per player for Acumulado that defaults to the last 2 seasons, falls back to older seasons and/or a lower minutes-per-match threshold when fewer than 5 matches are available, and carries a quality flag that surfaces as a warning badge in the player profile.

**Architecture:** `physical.ipynb` gains a shared pull helper and, per league, three new candidate CSVs (extended seasons, tier-20, tier-10) alongside the existing default pull (now with `count_match__gte` dropped). A new R file, sourced from `sc_data.R`, reads all four candidate sets per league and walks a fallback ladder per player, producing one blended row with a quality flag. `app.R` layers this onto the existing Acumulado table (built by `dedup_transfers()`, which is not modified) by overwriting the SC/physical columns and adding the two new quality columns. `ca_scouting_dashboard` threads the flag through to the player profile and shows a badge using the app's existing `sc-tag warn` pattern, only when viewing Acumulado (not a specific season).

**Tech Stack:** Python/Jupyter (SkillCorner API client, pandas), R (dplyr/readr), Next.js/React.

**Spec:** `docs/superpowers/specs/2026-11-03-skillcorner-data-quality-fallback-design.md`

## Global Constraints

- `dedup_transfers()` (`app.R`) must not be modified — StatsBomb's existing full-career Acumulado blend stays exactly as is.
- The season-selector feature must not be touched — `SbScMetricBySeason`, `PHYSICAL_VARS`, `sync_season_rows_to_mongo.R`'s existing columns, and the `{league}_fisico` dataframes sc_data.R already builds all stay exactly as they are today. The new blending logic reads physical.ipynb's raw per-league CSVs directly (the same files `sc_data.R` already reads), not `{league}_fisico`/`liga_mx_full`.
- No UI surface beyond the player profile (no Rankings/Base de Datos badge) in this round.
- Fallback tiers are exactly 3 playing-time thresholds: 30 (default), 20, 10.
- Extended seasons: exactly 2 seasons older than the default pair, using the same season-ID "+2 per year" extrapolation already proven for the Championship/Rusia cells.
- A player blended row's `sc_physical_quality_reason` is one of exactly: `"ok"`, `"extended_seasons"`, `"lower_threshold"`, `"insufficient_sample"`. `sc_physical_quality_warning` is `TRUE` whenever `reason != "ok"`, and both columns are `NA` (not `FALSE`/`"ok"`) for a player with no SkillCorner coverage at all.

---

## Task 1: `physical.ipynb` — shared pull helper + per-league candidate pulls

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/notebooks/physical.ipynb`

**Interfaces:**
- Produces: for every league currently pulled in this notebook, up to 4 CSVs in `data/physical/`:
  - `{league}_physical_standardized.csv` (existing filename, same shape as today, now with `count_match__gte` dropped from its params — more rows than before, not fewer)
  - `{league}_physical_extended_seasons.csv` (new — only for leagues whose default pull is exactly 2 seasons; Ecuador/Championship/Rusia already pull 4 seasons and get no new extended file)
  - `{league}_physical_tier20.csv`, `{league}_physical_tier10.csv` (new, every league, same season list as the default pull, `playing_time__gte` 20 and 10 respectively, `count_match__gte` also dropped)
  - All four (where applicable) have the identical column shape `read_physical_csv_safe()`-style downstream code already expects (same columns as today's `{league}_physical_standardized.csv`, since they go through the same `p_utils.add_standard_metrics()` + `compute_psv99_match_peak()` pipeline).

- [ ] **Step 1: Read the current notebook to confirm cell indices are unchanged since this plan was written**

Run:
```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
python3 -c "
import json
nb = json.load(open('notebooks/physical.ipynb'))
for i, c in enumerate(nb['cells']):
    if c['cell_type'] == 'code':
        src = ''.join(c['source'])
        first_line = src.split(chr(10))[0]
        print(i, first_line)
"
```
Expected: cell 2 starts with the `get_physical_with_retry` comment, cell 3 with the `compute_psv99_match_peak` comment, and cells 6-31 are the 26 league cells (`# LIGA MX`, `# ARGENTINA`, ... `# RUSIA`) in the order listed in Step 3's table. If the indices differ, re-read the relevant cells before editing — the rest of this task assumes this layout.

- [ ] **Step 2: Add a shared pull helper, as a new cell right after cell 3 (`compute_psv99_match_peak`)**

This DRYs up the ~30-line "loop over seasons, pull each competition, combine same-season competitions, concat seasons, save" block that's currently duplicated almost verbatim across every league cell — each league cell will now call this once per candidate set (default/extended/tier20/tier10) instead of inlining it 4 times.

Insert a new cell with this exact content:

```python
# Shared pull helper for every league cell below -- factors out the
# "loop over seasons, pull each competition for that season, combine
# same-season competitions (rate-like metrics via max, cumulative
# counters via sum -- summing a rate like PSV99 across two editions of
# the same season silently doubled it, see the original Liga MX cell's
# comment for where this was first caught), concat seasons, compute
# psv99_match_peak, save to CSV" logic that used to be duplicated
# almost verbatim in every league cell. Added 2026-11-03 alongside the
# data-quality fallback system -- each league now calls this up to 4
# times (default/extended-seasons/tier20/tier10), so de-duplicating it
# first keeps each league cell to a handful of lines instead of
# ~120.
#
# Uses get_physical_with_retry() for every call now (previously only
# Ecuador/Championship/Rusia) -- the fallback system triples the
# number of API calls per league, so the same transient-empty-response
# flakiness those three leagues showed is now more likely to bite
# others too; retrying is strictly safer and costs nothing when a
# league doesn't need it.
#
# count_match__gte is deliberately never passed here -- the 5-match
# minimum is enforced downstream (sc_physical_acumulado_fallback.R, R
# side) on the count summed ACROSS whichever candidate sets a player's
# blend ends up using, not server-side on a single request.
def pull_and_save_physical(client, season_plan, playing_time_gte, filename, label):
    """
    season_plan: list of (season_id, [comp_ids]) tuples.
    playing_time_gte: int, passed through to the API params.
    filename: output CSV filename (saved under DATA_DIR/physical/).
    label: short string used only in print() messages (e.g. "Liga MX default").
    Returns the saved DataFrame (empty if nothing was returned for any season).
    """
    all_params_used = []
    season_dfs = []

    for season_id, comp_ids in season_plan:
        dfs = []
        for comp_id in comp_ids:
            params = {
                "season": season_id,
                "competition": comp_id,
                "group_by": "player,team,competition,season,group",
                "possession": "all,tip,otip",
                "data_version": "3",
                "playing_time__gte": playing_time_gte,
            }
            all_params_used.append(dict(params))
            df_comp = get_physical_with_retry(client, params)
            dfs.append(df_comp)
            print(f"  [{label}] Season {season_id}, Competition {comp_id}: {len(df_comp)} rows")

        df = pd.concat(dfs, ignore_index=True) if dfs else pd.DataFrame()
        if df.empty:
            print(f"  [{label}] Season {season_id}: no rows returned, skipping.")
            continue

        if len(comp_ids) > 1:
            meta_cols = ["player_id", "player_name", "team_id", "team_name", "season", "group", "competition",
                         "season_id", "season_name", "competition_id", "competition_name"]
            rate_like_patterns = ["per_60", "per_minute", "psv", "_pct", "percentage", "ratio"]
            numeric_cols = [c for c in df.columns if c not in meta_cols and df[c].dtype in ["float64", "int64"]]
            max_cols = [c for c in numeric_cols if any(p in c.lower() for p in rate_like_patterns)]
            sum_cols = [c for c in numeric_cols if c not in max_cols]
            other_meta = [c for c in df.columns if c not in ["player_id", "player_name"] and c not in sum_cols and c not in max_cols]

            df_season = df.groupby(["player_id", "player_name"], as_index=False).agg(
                **{col: (col, "last") for col in other_meta},
                **{col: (col, "sum") for col in sum_cols},
                **{col: (col, "max") for col in max_cols}
            )
        else:
            df_season = df

        before = set(df_season.columns)
        p_utils.add_standard_metrics(df_season)
        added = sorted(set(df_season.columns) - before)
        print(f"  [{label}] Season {season_id} -- Added columns:", added)

        season_dfs.append(df_season)

    if not season_dfs:
        print(f"  [{label}] No data returned for any season -- skipping file write.")
        return pd.DataFrame()

    df_all = pd.concat(season_dfs, ignore_index=True)

    desktop_path = os.path.join(DATA_DIR, "physical")
    os.makedirs(desktop_path, exist_ok=True)
    file_path = os.path.join(desktop_path, filename)

    psv99_peaks = compute_psv99_match_peak(client, all_params_used)
    df_all = df_all.merge(psv99_peaks, on="player_id", how="left")
    print(f"  [{label}] psv99_match_peak non-null:", df_all["psv99_match_peak"].notna().sum(), "/", len(df_all))

    df_all.to_csv(file_path, index=False)
    print(f"  [{label}] Saved to:", file_path, "-- Shape:", df_all.shape)
    return df_all
```

- [ ] **Step 3: Rewrite every league cell to call the helper for its 2-4 candidate sets**

Run this script, which rewrites cells 6-31 in place. It embeds the exact season-pull config for every league (default plan unchanged from Part 1's commit, extended plan computed as "subtract 4 from each default season_id", matching the existing Championship/Rusia "+2 per year" extrapolation already in this notebook):

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
cat > /tmp/add_physical_fallback.py << 'PYEOF'
import json

PATH = "notebooks/physical.ipynb"
nb = json.load(open(PATH))

# (cell_index, header_comment, label, filename_stem, default_plan, extended_plan_or_None)
# default_plan / extended_plan: list of (season_id, [comp_ids])
# extended_plan=None means this league already pulls >=4 seasons by
# default (Ecuador/Championship/Rusia) -- no extended-seasons file for it.
LEAGUES = [
    (6,  "LIGA MX",       "ligamx",      [(129, [610, 97]), (131, [610, 97])],
                                           [(125, [610, 97]), (127, [610, 97])]),
    (7,  "ARGENTINA",     "arg",         [(128, [70]), (130, [70, 376])],
                                           [(124, [70]), (126, [70])]),
    (8,  "BRASIL",        "brasil",      [(128, [77]), (130, [77])],
                                           [(124, [77]), (126, [77])]),
    (9,  "COLOMBIA",      "col",         [(128, [100]), (130, [100, 653])],
                                           [(124, [100]), (126, [100])]),
    (10, "CHILE",         "chile",       [(128, [57]), (130, [57])],
                                           [(124, [57]), (126, [57])]),
    (11, "MLS",           "mls",         [(128, [60]), (130, [60])],
                                           [(124, [60]), (126, [60])]),
    (12, "PREMIER LEAGUE","premier",     [(129, [1]), (131, [1])],
                                           [(125, [1]), (127, [1])]),
    (13, "LA LIGA",       "laliga",      [(129, [4]), (131, [4])],
                                           [(125, [4]), (127, [4])]),
    (14, "LA LIGA 2",     "laliga2",     [(129, [42]), (131, [42])],
                                           [(125, [42]), (127, [42])]),
    (15, "SERIE A",       "seriea",      [(129, [5]), (131, [5])],
                                           [(125, [5]), (127, [5])]),
    (16, "TURQUÍA",       "tur",         [(129, [29]), (131, [29])],
                                           [(125, [29]), (127, [29])]),
    (17, "UEFA CHAMPIONS LEAGUE", "ucl", [(129, [10]), (131, [10])],
                                           [(125, [10]), (127, [10])]),
    (18, "UEFA EUROPA LEAGUE", "uel",    [(129, [9]), (131, [9])],
                                           [(125, [9]), (127, [9])]),
    (19, "PARAGUAY",      "paraguay",    [(128, [140]), (130, [140])],
                                           [(124, [140]), (126, [140])]),
    (20, "URUGUAY",       "uruguay",     [(128, [95]), (130, [95, 655])],
                                           [(124, [95]), (126, [95])]),
    (21, "PERU",          "peru",        [(128, [139]), (130, [139, 652])],
                                           [(124, [139]), (126, [139])]),
    (22, "ECUADOR",       "ecuador",     [(126, [163]), (128, [163]), (130, [163])],
                                           None),
    (23, "CHAMPIONSHIP (ENG)", "championship", [(125, [31]), (127, [31]), (129, [31]), (131, [31])],
                                           None),
    (24, "BUNDESLIGA",    "bundesliga",  [(129, [6]), (131, [6])],
                                           [(125, [6]), (127, [6])]),
    (25, "2. BUNDESLIGA", "bundesliga2", [(129, [40]), (131, [40])],
                                           [(125, [40]), (127, [40])]),
    (26, "LIGUE 1",       "ligue1",      [(129, [3]), (131, [3])],
                                           [(125, [3]), (127, [3])]),
    (27, "EREDIVISIE",    "eredivisie",  [(129, [17]), (131, [17])],
                                           [(125, [17]), (127, [17])]),
    (28, "BÉLGICA",       "belgica",     [(129, [16]), (131, [16])],
                                           [(125, [16]), (127, [16])]),
    (29, "PORTUGAL",      "portugal",    [(129, [25]), (131, [25])],
                                           [(125, [25]), (127, [25])]),
    (30, "ESCOCIA",       "escocia",     [(129, [18]), (131, [18])],
                                           [(125, [18]), (127, [18])]),
    (31, "RUSIA",         "rusia",       [(125, [27]), (127, [27]), (129, [27]), (131, [27])],
                                           None),
]

def fmt_plan(plan):
    return "[" + ", ".join(f"({sid}, {comps})" for sid, comps in plan) + "]"

def make_cell(header, label, stem, default_plan, extended_plan):
    lines = [
        f"# {header}",
        "",
        "client = SkillcornerClient(username=USERNAME, password=PASSWORD)",
        "",
        f"default_plan = {fmt_plan(default_plan)}",
        f"pull_and_save_physical(client, default_plan, 30, \"{stem}_physical_standardized.csv\", \"{label} default\")",
        "",
    ]
    if extended_plan is not None:
        lines += [
            f"extended_plan = {fmt_plan(extended_plan)}",
            f"pull_and_save_physical(client, extended_plan, 30, \"{stem}_physical_extended_seasons.csv\", \"{label} extended\")",
            "",
        ]
    else:
        lines += [
            "# Already pulls >=4 seasons by default -- no separate extended-seasons file needed.",
            "",
        ]
    lines += [
        f"pull_and_save_physical(client, default_plan, 20, \"{stem}_physical_tier20.csv\", \"{label} tier20\")",
        f"pull_and_save_physical(client, default_plan, 10, \"{stem}_physical_tier10.csv\", \"{label} tier10\")",
    ]
    return [l + "\n" for l in lines[:-1]] + [lines[-1]]

for idx, header, label, stem, default_plan, extended_plan in LEAGUES:
    assert nb["cells"][idx]["cell_type"] == "code", f"cell {idx} is not code"
    nb["cells"][idx]["source"] = make_cell(header, label, stem, default_plan, extended_plan)
    nb["cells"][idx]["outputs"] = []
    nb["cells"][idx]["execution_count"] = None

print(f"Rewrote {len(LEAGUES)} league cells.")
json.dump(nb, open(PATH, "w"), indent=1, ensure_ascii=False)
print("Saved.")
PYEOF
python3 /tmp/add_physical_fallback.py
```

- [ ] **Step 4: Verify every cell parses as valid Python and the row count is sane**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
python3 -c "
import json, ast
nb = json.load(open('notebooks/physical.ipynb'))
errors = 0
for i, c in enumerate(nb['cells']):
    if c['cell_type'] != 'code':
        continue
    try:
        ast.parse(''.join(c['source']))
    except SyntaxError as e:
        errors += 1
        print(f'Cell {i}: SYNTAX ERROR: {e}')
print('Total cells:', len(nb['cells']), '| syntax errors:', errors)
"
rm -f /tmp/add_physical_fallback.py
```
Expected: `syntax errors: 0`.

- [ ] **Step 5: Spot-check one rewritten cell by eye**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
python3 -c "
import json
nb = json.load(open('notebooks/physical.ipynb'))
print(''.join(nb['cells'][12]['source']))
"
```
Expected: cell 12 (Premier League) shows `default_plan = [(129, [1]), (131, [1])]`, `extended_plan = [(125, [1]), (127, [1])]`, and four `pull_and_save_physical(...)` calls (default/extended/tier20/tier10) with filenames `premier_physical_standardized.csv`, `premier_physical_extended_seasons.csv`, `premier_physical_tier20.csv`, `premier_physical_tier10.csv`.

- [ ] **Step 6: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add notebooks/physical.ipynb
git commit -m "$(cat <<'EOF'
physical.ipynb: add extended-seasons + lower-threshold candidate pulls

Part 2 of the SkillCorner data-quality fallback system. Added a shared
pull_and_save_physical() helper (de-duplicating the ~30-line
pull/combine/save block every league cell repeated) and, per league,
two new unconditional candidate pulls beyond the existing default:
2 older seasons (same "+2 per year" ID extrapolation Championship/
Rusia already use) and the same seasons again at playing_time__gte 20
and 10. count_match__gte is dropped from every pull -- the 5-match
minimum is now enforced downstream, on the count summed across
whichever candidate sets a player's blend ends up using.

Every league now uses get_physical_with_retry() (previously only
Ecuador/Championship/Rusia) -- ~4x the API calls per league makes the
same transient-empty-response flakiness more likely to hit others too.

See docs/superpowers/specs/2026-11-03-skillcorner-data-quality-fallback-design.md.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 2: New R file — fallback-aware physical blender

**Files:**
- Create: `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/sc_physical_acumulado_fallback.R`
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/sc_data.R` (source the new file near its other `read_csv(...)` calls)

**Interfaces:**
- Consumes: the 4 CSV sets Task 1 produces (`data/physical/{league}_physical_standardized.csv`, `_extended_seasons.csv`, `_tier20.csv`, `_tier10.csv` — the last three missing/empty for leagues where `df.empty` meant nothing was written, same as every existing `read_physical_csv_safe()` call site already handles).
- Produces: `get_sc_physical_acumulado_fallback()` — a function taking no arguments, returning a tibble with one row per `player_id`, columns = every physical metric column `{league}_physical_standardized.csv` has (same shape Task 1's pulls already produce) plus `sc_physical_quality_warning` (logical) and `sc_physical_quality_reason` (character, one of `"ok"`/`"extended_seasons"`/`"lower_threshold"`/`"insufficient_sample"`). Task 3 consumes this function's output directly.

- [ ] **Step 1: Write the new file**

Create `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/sc_physical_acumulado_fallback.R`:

```r
# ============================================================
# sc_physical_acumulado_fallback.R
#
# Builds the Acumulado-only SkillCorner physical row per player,
# following the scouting org's sample-size guidelines: default to the
# player's last 2 seasons; if that doesn't reach 5 matches, fall back
# to 2 older seasons (~3-4 years back), then to lower playing-time
# thresholds (20, then 10 minutes/match), stopping as soon as 5
# matches is reached. Every step accumulates ON TOP of the previous
# one (never replaces it) -- "extended_seasons" means default+older
# seasons blended together, not older seasons alone.
#
# Reads physical.ipynb's RAW per-league CSVs directly (the same files
# sc_data.R already reads for {league}_fisico) -- not {league}_fisico/
# liga_mx_full, which for Liga MX also carries Game Intelligence
# columns this file has no business touching. The season-selector
# feature (SbScMetricBySeason, sync_season_rows_to_mongo.R) reads a
# completely separate path and is untouched by this file existing.
#
# dedup_transfers() (app.R) is NOT used or modified here -- this file
# implements its own minutes-weighted blend, deliberately scoped to
# just the candidate sets a given player's fallback ladder actually
# needs, which dedup_transfers()'s "blend everything the player ever
# has" semantics can't express.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(tibble)
}))

# Every league this notebook pulls, with its file-name stem (matches
# Task 1's pull_and_save_physical() filenames exactly).
SC_PHYSICAL_LEAGUE_STEMS <- c(
  "ligamx", "arg", "brasil", "col", "chile", "mls", "premier", "laliga",
  "laliga2", "seriea", "tur", "ucl", "uel", "paraguay", "uruguay", "peru",
  "ecuador", "championship", "bundesliga", "bundesliga2", "ligue1",
  "eredivisie", "belgica", "portugal", "escocia", "rusia"
)

# Same defensive empty-frame fallback read_physical_csv_safe() (sc_data.R)
# already uses for Ecuador/Championship/Rusia -- here every candidate
# file can legitimately be missing (a league with no low-sample players
# never triggers its own fallback tiers to be USED, but the files still
# get written by Task 1 unconditionally; a league/season with 0 rows
# returned skips the file write entirely, same as today).
.read_physical_candidate <- function(path) {
  if (!file.exists(path)) return(NULL)
  suppressMessages(read_csv(path, show_col_types = FALSE))
}

# Columns that are genuine cumulative totals (raw distance in meters,
# raw counts, raw minutes, match counts) over whatever window of
# matches contributed to a row -- these SUM when blending more matches
# in, unlike rate/per-X columns where summing would be meaningless.
# Peak-speed columns (psv99 and friends) are a third case: the TRUE
# value across a wider sample is the max, not a sum or an average (an
# average would understate a player's real top speed).
.sc_physical_sum_pattern <- "_full_(all|tip|otip|bip)$|^count_match(_failed)?$"
# "^peak" catches peak_velocity/peak_velocity_top3 (confirmed present in
# a real downloaded CSV during planning) -- "^psv99" alone would have
# missed the bare peak_velocity column (no _top3/_top5/_match_peak
# suffix), silently weighted-averaging a peak-speed column instead of
# maxing it. Caught during this plan's own self-review, not live.
.sc_physical_peak_pattern <- "^psv99|^peak|_top3$|_top5$|_match_peak$"

# Blends a list of candidate data.frames (already read, some possibly
# empty/NULL) into one row per player_id. Weight = minutes_full_all
# (falls back to equal weight if a player has no minutes on any
# candidate row, which shouldn't happen in practice but mirrors
# dedup_transfers()'s own defensive fallback).
.blend_physical_candidates <- function(dfs) {
  dfs <- purrr::compact(dfs)
  if (!length(dfs)) return(tibble())
  combined <- dplyr::bind_rows(dfs)
  if (!nrow(combined)) return(tibble())

  numeric_cols <- names(combined)[sapply(combined, is.numeric)]
  id_like <- c("player_id", "team_id", "competition_id", "season_id")
  numeric_cols <- setdiff(numeric_cols, id_like)

  sum_cols  <- numeric_cols[grepl(.sc_physical_sum_pattern, numeric_cols)]
  peak_cols <- numeric_cols[grepl(.sc_physical_peak_pattern, numeric_cols)]
  avg_cols  <- setdiff(numeric_cols, c(sum_cols, peak_cols))

  combined |>
    dplyr::group_by(player_id) |>
    dplyr::summarise(
      player_name = dplyr::first(player_name),
      dplyr::across(dplyr::all_of(sum_cols), ~ sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(peak_cols), ~ if (all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(avg_cols), function(x) {
        # minutes_full_all is a column of the SAME grouped tibble
        # dplyr::summarise() already has in scope here -- referencing it
        # by name (not re-filtering `combined` independently by
        # player_id, which risks a row-order mismatch against `x`'s
        # internal group view) is both simpler and correct.
        w <- minutes_full_all
        w[is.na(w)] <- 0
        if (sum(w) == 0 || all(is.na(x))) return(if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE))
        stats::weighted.mean(x, w, na.rm = TRUE)
      }),
      .groups = "drop"
    )
}

#' Builds the Acumulado-only SkillCorner physical row per player, with
#' the sample-size fallback ladder applied. One row per player_id.
get_sc_physical_acumulado_fallback <- function() {
  results <- purrr::map(SC_PHYSICAL_LEAGUE_STEMS, function(stem) {
    base <- file.path("data", "physical", paste0(stem, "_physical_standardized.csv"))
    extended <- file.path("data", "physical", paste0(stem, "_physical_extended_seasons.csv"))
    tier20 <- file.path("data", "physical", paste0(stem, "_physical_tier20.csv"))
    tier10 <- file.path("data", "physical", paste0(stem, "_physical_tier10.csv"))

    df_default  <- .read_physical_candidate(base)
    df_extended <- .read_physical_candidate(extended)
    df_tier20   <- .read_physical_candidate(tier20)
    df_tier10   <- .read_physical_candidate(tier10)

    if (is.null(df_default) || !nrow(df_default)) return(NULL)

    player_ids <- unique(df_default$player_id)

    purrr::map_dfr(player_ids, function(pid) {
      one <- function(df) if (is.null(df)) NULL else df[df$player_id == pid, , drop = FALSE]

      # Step 1: default only.
      cand <- list(one(df_default))
      blend <- .blend_physical_candidates(cand)
      if (nrow(blend) && isTRUE(blend$count_match[1] >= 5)) {
        return(dplyr::mutate(blend, sc_physical_quality_warning = FALSE, sc_physical_quality_reason = "ok"))
      }

      # Step 2: + extended seasons.
      cand <- list(one(df_default), one(df_extended))
      blend <- .blend_physical_candidates(cand)
      if (nrow(blend) && isTRUE(blend$count_match[1] >= 5)) {
        return(dplyr::mutate(blend, sc_physical_quality_warning = TRUE, sc_physical_quality_reason = "extended_seasons"))
      }

      # Step 3: + tier20.
      cand <- list(one(df_default), one(df_extended), one(df_tier20))
      blend <- .blend_physical_candidates(cand)
      if (nrow(blend) && isTRUE(blend$count_match[1] >= 5)) {
        return(dplyr::mutate(blend, sc_physical_quality_warning = TRUE, sc_physical_quality_reason = "lower_threshold"))
      }

      # Step 4: + tier10. Use whatever this produces, even if still < 5.
      cand <- list(one(df_default), one(df_extended), one(df_tier20), one(df_tier10))
      blend <- .blend_physical_candidates(cand)
      if (!nrow(blend)) return(NULL)
      reason <- if (isTRUE(blend$count_match[1] >= 5)) "lower_threshold" else "insufficient_sample"
      dplyr::mutate(blend, sc_physical_quality_warning = TRUE, sc_physical_quality_reason = reason)
    })
  })

  dplyr::bind_rows(purrr::compact(results))
}

# ============================================================
# Validation harness -- only runs when executed directly, matching
# this repo's convention for pipeline-script sanity checks (see
# confidence_shrinkage.R, ame_score_gate.R, datascore_v2.R).
# ============================================================
if (sys.nframe() == 0) {
  result <- get_sc_physical_acumulado_fallback()
  cat(sprintf("rows: %d\n", nrow(result)))
  cat(sprintf("distinct player_id: %d\n", dplyr::n_distinct(result$player_id)))
  cat("reason breakdown:\n")
  print(table(result$sc_physical_quality_reason, useNA = "ifany"))
  stopifnot("duplicate player_id in output" = !any(duplicated(result$player_id)))
  cat("PASS: no duplicate player_id\n")
}
```

- [ ] **Step 2: Source the new file from `sc_data.R`**

Run: `sed -n '1,18p' /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/sc_data.R`

Expected to show (confirmed present as of 2026-11-03) 16 `library(...)` calls (lines 1-16: `tidyverse`, `readr`, `devtools`, `StatsBombR`, `ggsoccer`, `soccermatics`, `stats`, `gt`, `grid`, `viridis`, `glue`, `cluster`, `ggrepel`, `purrr`, `lubridate`, `scales`, `fmsb`), a blank line 17, then a comment starting at line 18 (`# Canonical Liga MX season label...`). If it doesn't match, find the actual end of the library block before editing.

Insert, right after `library(fmsb)` (the last library call) and before the blank line/comment that follows it:

```r
source("sc_physical_acumulado_fallback.R")
```

- [ ] **Step 3: Syntax-check both files**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
Rscript -e 'parse("sc_physical_acumulado_fallback.R"); cat("PARSE OK\n")'
Rscript -e 'parse("sc_data.R"); cat("PARSE OK\n")'
```
Both must end with `PARSE OK`.

- [ ] **Step 4: Local smoke test against whatever physical CSVs are currently on disk**

The repo has stale local copies in `data/physical/` from a previous notebook run (no `_extended_seasons`/`_tier20`/`_tier10` files yet, since Task 1 hasn't been run live). This step validates the MECHANISM against the one candidate set that does exist (`df_default` only) — every player should come back with `reason` either `"ok"` (if they already had ≥5 matches) or `"insufficient_sample"` (no fallback data available locally to climb the ladder with) — never `"extended_seasons"` or `"lower_threshold"`, since those files don't exist yet locally.

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
Rscript sc_physical_acumulado_fallback.R 2>&1 | tail -20
```

Expected: no errors, `distinct player_id` in the thousands, the reason breakdown shows only `ok` and `insufficient_sample` (confirming the ladder correctly stopped at step 1 for everyone, since no fallback files exist to climb further), and the `stopifnot` duplicate check passes.

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add sc_physical_acumulado_fallback.R sc_data.R
git commit -m "$(cat <<'EOF'
Add get_sc_physical_acumulado_fallback(): sample-size fallback ladder

New file, sourced from sc_data.R. Blends a player's default 2-season
physical data, falling back to +extended seasons, +tier20, +tier10
(each step accumulating on top of the last) only as far as needed to
reach 5 matches, producing sc_physical_quality_warning/_reason
alongside the blended metrics. Reads physical.ipynb's raw per-league
CSVs directly, not {league}_fisico -- the season-selector feature is
untouched by this file existing. dedup_transfers() (app.R) is not
used or modified here.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 3: Wire the fallback blend into Acumulado (`app.R`)

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/app.R:2381-2418` (`get_all_players_df()`)

**Interfaces:**
- Consumes: `get_sc_physical_acumulado_fallback()` (Task 2) — one row per `player_id`, physical metric columns + `sc_physical_quality_warning`/`sc_physical_quality_reason`.
- Produces: `get_all_players_df()`'s existing return value, now with its SC/physical numeric columns overwritten by the fallback-aware blend, plus the two new quality columns. No new exported function — this is a pure modification of an existing one, consumed the same way by every existing caller (Rankings, Base de Datos, DataScore, `sync_to_mongo.R`, etc.).

- [ ] **Step 1: Confirm the current state of `get_all_players_df()`**

Run: `sed -n '2377,2418p' /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/app.R`

Expected to show (confirmed present as of 2026-11-03):
```r
.all_players_df_cache <- NULL
get_all_players_df <- function() {
  if (is.null(.all_players_df_cache)) {
    piece <- load_app_cache_piece("all_players_df")
    if (!is.null(piece)) {
      .all_players_df_cache <<- piece
      return(.all_players_df_cache)
    }
    # dedup_same_team() first shrinks join-artifact duplicate rows (up to 4x
    # per player) down to one before dedup_transfers() does its per-column
    # weighted-average merge -- skipping it made dedup_transfers() churn
    # through inflated group sizes, ~33s vs ~9s measured on the full dataset.
    dat <- all_players_df_from_cache(scout, league_map) |>
      dedup_same_team() |>
      dedup_transfers()

    # % of minutes played vs. the most-used player in the same league --
    ...
```
If it doesn't match, re-read the surrounding area before editing.

- [ ] **Step 2: Insert the override, right after `dedup_transfers()` runs**

Replace:
```r
    dat <- all_players_df_from_cache(scout, league_map) |>
      dedup_same_team() |>
      dedup_transfers()

    # % of minutes played vs. the most-used player in the same league --
```
with:
```r
    dat <- all_players_df_from_cache(scout, league_map) |>
      dedup_same_team() |>
      dedup_transfers()

    # SkillCorner physical columns for Acumulado get OVERWRITTEN here with
    # the sample-size-fallback-aware blend (sc_physical_acumulado_fallback.R)
    # instead of whatever dedup_transfers() computed from the full-career
    # season rows above -- dedup_transfers() itself is untouched; this is a
    # layer applied after it runs, scoped to just the physical columns. A
    # player with no row in the fallback blend (no SkillCorner coverage at
    # all) keeps whatever dedup_transfers() already had for those columns
    # (typically NA) and both new quality columns stay NA, not a warning --
    # "no data" and "a warning about thin data" are different states.
    sc_fallback <- get_sc_physical_acumulado_fallback()
    if (nrow(sc_fallback)) {
      override_cols <- setdiff(names(sc_fallback), c("player_id", "player_name",
                                                        "sc_physical_quality_warning",
                                                        "sc_physical_quality_reason"))
      dat <- dat |>
        dplyr::select(-dplyr::any_of(override_cols)) |>
        dplyr::left_join(
          sc_fallback |> dplyr::mutate(player_id = as.character(player_id)),
          by = "player_id"
        )
    } else {
      dat$sc_physical_quality_warning <- NA
      dat$sc_physical_quality_reason <- NA_character_
    }

    # % of minutes played vs. the most-used player in the same league --
```

Note: `sc_fallback$player_id` must be cast to character before the join — `dat$player_id` is character (confirmed throughout this pipeline, e.g. `load_scout_data.R`'s own `as.character(player_id)` normalization), while `get_sc_physical_acumulado_fallback()`'s output inherits whatever type `read_csv()` inferred for the physical CSVs' `player_id` column (typically numeric) — an uncast join here would silently match nothing.

- [ ] **Step 3: Syntax-check**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
Rscript -e 'parse("app.R"); cat("PARSE OK\n")'
```

- [ ] **Step 4: Confirm `dedup_transfers()` itself is untouched**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git diff app.R | grep -A3 -B3 "^-.*dedup_transfers\|^+.*dedup_transfers"
```
Expected: no output (the diff never touches a line containing `dedup_transfers`, confirming the function definition itself and its two call sites inside `get_all_players_df()` are unchanged — only new code was added after it).

- [ ] **Step 5: Commit**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git add app.R
git commit -m "$(cat <<'EOF'
Layer the SkillCorner fallback blend onto Acumulado's physical columns

get_all_players_df() now overwrites its SC/physical numeric columns
with get_sc_physical_acumulado_fallback()'s output (sample-size-aware,
see sc_physical_acumulado_fallback.R) and adds
sc_physical_quality_warning/_reason. dedup_transfers() itself is
provably untouched (diff confirms no line containing it changed) --
this is a layer applied after it runs, not a modification of it.
StatsBomb's own Acumulado columns are unaffected.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 4: Frontend — thread the quality flag + warning badge

**Files:**
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/page.jsx`
- Modify: `/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerProfileView.jsx`

**Interfaces:**
- Consumes: `sbSc.sc_physical_quality_warning` (boolean or null/undefined), `sbSc.sc_physical_quality_reason` (string or null/undefined) — new fields on `SbScMetric` documents once Task 3's data syncs (field names must match exactly; `models/SbScMetric.js` is `strict: false`, confirmed earlier this session, so no schema change is needed for these to round-trip).
- Produces: two new fields on the `meta` object `page.jsx` already passes to `PlayerProfileView` — `scPhysicalQualityWarning` (boolean), `scPhysicalQualityReason` (string or null).

- [ ] **Step 1: Confirm current state of `page.jsx`'s `meta` object**

Run: `grep -n "scoreSourceLeague: sbSc" -A 3 "/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/page.jsx"`

Expected to show (confirmed present as of 2026-11-03):
```js
    scoreSourceLeague: sbSc?.SourceLeague ?? null,
    scoreSourceSeason: sbSc?.SourceSeason ?? null,
  };
```

- [ ] **Step 2: Add the two new meta fields**

Replace:
```js
    scoreSourceLeague: sbSc?.SourceLeague ?? null,
    scoreSourceSeason: sbSc?.SourceSeason ?? null,
  };
```
with:
```js
    scoreSourceLeague: sbSc?.SourceLeague ?? null,
    scoreSourceSeason: sbSc?.SourceSeason ?? null,
    // SkillCorner physical data-quality fallback (sc_physical_acumulado_
    // fallback.R, repo scout_dashboard) -- only meaningful for Acumulado,
    // never for a specific season's SkillCorner table (PlayerProfileView.jsx
    // only shows the badge when seasonSel === "Acumulado").
    scPhysicalQualityWarning: Boolean(sbSc?.sc_physical_quality_warning),
    scPhysicalQualityReason: sbSc?.sc_physical_quality_reason ?? null,
  };
```

- [ ] **Step 3: Confirm current state of the SkillCorner section header in `PlayerProfileView.jsx`**

Run: `sed -n '999,1012p' "/Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard/app/jugadores/[id]/PlayerProfileView.jsx"`

Expected to show (confirmed present as of 2026-11-03):
```jsx
            {!scTable.message && (
              <div className="sc-profile-infogrid" style={{ marginTop: 24 }}>
                <div className="sc-profile-infocol">
                  <div className="sc-section-title"><span>⚙️</span> Skill Corner</div>
                  {scDataFallback && (
                    <div className="sc-fichameta" style={{ marginBottom: 8, color: "var(--text-dim)" }}>
                      Esta temporada no tiene cobertura propia de SkillCorner -- mostrando el físico Acumulado del jugador.
                    </div>
                  )}
```

- [ ] **Step 4: Add the warning badge, gated on viewing Acumulado**

Replace:
```jsx
                  <div className="sc-section-title"><span>⚙️</span> Skill Corner</div>
                  {scDataFallback && (
```
with:
```jsx
                  <div className="sc-section-title">
                    <span>⚙️</span> Skill Corner
                    {seasonSel === "Acumulado" && meta.scPhysicalQualityWarning && (
                      <span
                        className="sc-tag warn"
                        style={{ marginLeft: 8 }}
                        title={
                          meta.scPhysicalQualityReason === "extended_seasons"
                            ? "Datos de temporadas anteriores"
                            : "Muestra pequeña de partidos"
                        }
                      >
                        ⚠️
                      </span>
                    )}
                  </div>
                  {scDataFallback && (
```

- [ ] **Step 5: Verify the dev server compiles clean**

Confirm the dev server is running (check `lsof -i :3001 -sTCP:LISTEN`; if not, start it: `cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard && npm run dev` with `run_in_background: true`). Then check its output log for a clean recompile with no new error after these edits (no syntax/runtime error following the save).

Since no live data has `sc_physical_quality_warning` set yet (Task 3 hasn't deployed), the badge won't visibly appear for any player yet — a clean compile with the existing player profile still rendering correctly (no crash on `meta.scPhysicalQualityWarning`/`meta.scPhysicalQualityReason` being `false`/`null`) is the full extent of what's verifiable locally right now. Live verification with a real warning happens in Task 5.

- [ ] **Step 6: Commit (this file only, matching this repo's established pattern of not sweeping in the unrelated pre-existing uncommitted pile)**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
git status --short
```
Confirm only `app/jugadores/[id]/page.jsx` and `app/jugadores/[id]/PlayerProfileView.jsx` show as newly modified beyond whatever was already in the pre-existing dirty pile (same files as before, just with this task's additional diff). If `PlayerProfileView.jsx` was already dirty from earlier unrelated work, stage and commit precisely these two files' current state together (same technique as the November 2nd SkillCorner-table commit: `git commit <path> <path> -m "..."`, not `git add -A`):

```bash
git commit "app/jugadores/[id]/page.jsx" "app/jugadores/[id]/PlayerProfileView.jsx" -m "$(cat <<'EOF'
Show a warning badge for SkillCorner physical data-quality fallback

Threads sc_physical_quality_warning/_reason (new fields on SbScMetric,
see scout_dashboard's sc_physical_acumulado_fallback.R) through to the
player profile's Skill Corner section header as an sc-tag warn badge
(same CSS class the "no ficha" indicator already uses), shown only
while viewing Acumulado -- a specific season's SkillCorner table is
unaffected by this flag, since it's not blended/fallback data at all.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 5: Deploy and verify end-to-end

**Files:** none (operational task)

**Interfaces:**
- Consumes: Tasks 1-4's committed changes.
- Produces: a live, verified feature.

- [ ] **Step 1: Push `scout_dashboard`'s commits and trigger the pipeline**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard
git push origin main
gh workflow run deploy.yml --ref main
sleep 5
gh run list --workflow=deploy.yml --limit 1 --json databaseId,status,createdAt
```
Note the `databaseId` as `<run_id>`.

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
Run with `run_in_background: true`. **This run will take noticeably longer than the usual ~5.5 hours** — `physical.ipynb` now makes roughly 4x the API calls per league (default + extended + tier20 + tier10, vs. just default before). Budget accordingly before assuming a stall.

Expected: `completed success`. If `completed failure`, find the failing step with `gh run view <run_id> --json jobs -q '.jobs[].steps[] | select(.conclusion=="failure") | .name'`, then `gh run view <run_id> --log-failed`.

- [ ] **Step 3: Confirm the new fields landed in Mongo**

```bash
cd /Users/mateorodriguez/Desktop/analisis_CA/ca_scouting_dashboard
cat > tmp_verify_physical_fallback.mjs << 'EOF'
import dotenv from "dotenv";
dotenv.config({ path: ".env.local" });
import mongoose from "mongoose";
import SbScMetric from "./models/SbScMetric.js";

await mongoose.connect(process.env.MONGODB_URI);
const total = await SbScMetric.countDocuments({});
const withReason = await SbScMetric.countDocuments({ sc_physical_quality_reason: { $ne: null } });
const byReason = await SbScMetric.aggregate([
  { $group: { _id: "$sc_physical_quality_reason", n: { $sum: 1 } } },
]);
console.log("total docs:", total);
console.log("docs with sc_physical_quality_reason set:", withReason);
console.log("breakdown:", byReason);
const sample = await SbScMetric.findOne({ sc_physical_quality_warning: true }).lean();
console.log("sample warned player:", sample?.player_name, sample?.sc_physical_quality_reason);
await mongoose.disconnect();
EOF
node tmp_verify_physical_fallback.mjs 2>&1 | grep -v "Warning\|MODULE_TYPELESS\|Reparsing\|eliminate\|trace-warnings"
rm tmp_verify_physical_fallback.mjs
```

Expected: `withReason` is a large majority of players with any SkillCorner coverage (not zero), the breakdown shows a mix of `ok`/`extended_seasons`/`lower_threshold`/`insufficient_sample` (not 100% `ok` — if everyone is `ok`, the fallback ladder likely never got exercised, worth double-checking Task 1's extended/tier CSVs actually have rows), and a real sample warned player's name prints.

- [ ] **Step 4: Verify in the browser**

Navigate to the sample warned player's profile from Step 3 (`http://localhost:3001/jugadores/<their transfermarktId>`, dev server from Task 4) with "Acumulado" selected (the default), and confirm the ⚠️ badge appears next to "⚙️ Skill Corner" with the right tooltip reason. Then switch the season selector to a specific season for the same player and confirm the badge disappears (per the `seasonSel === "Acumulado"` gate).

Use the Chrome extension tools the same way this session verified the SkillCorner P90 metrics change and the per-season DataScore chart earlier.

- [ ] **Step 5: Report to the user**

Summarize: both repos' commits, the CI run result, the Mongo breakdown from Step 3, and a screenshot or text confirmation from Step 4 showing the badge.

---

## Self-Review Notes

- **Spec coverage:** Task 1 covers spec section 1 (candidate pulls). Task 2 covers section 2 (blending/fallback ladder). Task 3 covers section 3 (propagation into Acumulado, `dedup_transfers()` untouched). Task 4 covers section 4 (sync needs no changes, confirmed — `sync_to_mongo.R` already mirrors the full row; frontend threading + badge). Task 5 covers the spec's full testing plan (local smoke test in Task 2 Step 4, diff-based `dedup_transfers()` check in Task 3 Step 4, full CI run + live verification in Task 5).
- **Placeholder scan:** no TBD/TODO; every step has literal code, exact commands, or an exact `grep`/`sed` check with its expected output written out.
- **Type consistency:** `get_sc_physical_acumulado_fallback()`'s output columns (`player_id`, `player_name`, every physical metric column, `sc_physical_quality_warning`, `sc_physical_quality_reason`) are referenced identically in Task 2's Interfaces block, Task 3's consumption of it, and Task 4's frontend field names (`sc_physical_quality_warning`/`sc_physical_quality_reason` on the Mongo side → `scPhysicalQualityWarning`/`scPhysicalQualityReason` in `meta`, the same camelCase convention `scoreSourceLeague`/`scoreSourceSeason` already establish in the same object).
- **A genuine design risk, flagged rather than silently resolved:** Task 2's `.blend_physical_candidates()` classifies ~160 physical columns into sum/peak/weighted-average buckets by name pattern rather than an exhaustive hand-reviewed list (impractical at this column count — no such list exists anywhere else in the codebase either, the closest precedent being the 2-pattern `rate_like_patterns` list the existing per-season cells already use for a narrower same-season case). The three patterns (`_full_(all|tip|otip|bip)$|^count_match`, `^psv99|^peak|_top3$|_top5$|_match_peak$`, "everything else") are a reasoned generalization grounded in the actual column list (verified against a real downloaded CSV during planning), not a guess — but Task 2 Step 4's smoke test only exercises the `ok`/`insufficient_sample` path locally (no fallback files exist yet), so the weighted-blend arithmetic for a multi-candidate-set player is only exercised for real once Task 5's live data exists. If Task 5 Step 3's verification surfaces an implausible blended value for a warned player, re-check this classification first.
- **Two correctness bugs caught and fixed during this self-review, before dispatch:** (1) the weighted-average `across()` step originally re-filtered `combined$minutes_full_all` by `dplyr::cur_group()$player_id` independently of `x`, risking a row-order mismatch against dplyr's internal per-group view — fixed to reference `minutes_full_all` directly by name, which dplyr's data-masking already scopes correctly to the same group `x` is drawn from. (2) `.sc_physical_peak_pattern` originally only matched `^psv99|_top3$|_top5$|_match_peak$`, which misses the bare `peak_velocity` column confirmed present in a real downloaded CSV (it has no qualifying suffix) — it would have been silently weighted-averaged instead of maxed. Fixed by adding `^peak` to the pattern.
