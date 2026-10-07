# ============================================================
# league_transition_gate.R
#
# Decides, for each player, which single row's DataScore_previo is their
# CURRENT DataScore (doc's league-transition rule, locked 2026-10-06): a
# player who changes leagues keeps their OLD-league DataScore unchanged
# (hard cutoff, not a gradual blend) until they cross 8 matches or 600
# minutes in the new league, at which point it switches fully to the
# new-league-anchored score.
#
# This is a genuinely different shape from every prior pipeline step --
# normalize/capacity/base/Competition Strength all transform ONE row
# independently. The transition gate needs to compare a player's most
# recent row against their most recent PRIOR row (possibly a different
# league), which means operating across every role a player has a scored
# row in, not within a single role's pool.
#
# Season chronology gotcha: season_id does NOT sort chronologically across
# calendar-year leagues (315="2025", 316="2026") and split-year leagues
# (318="2025/2026", 351="2026/2027") -- 316's window (Jan-Dec 2026)
# actually starts AFTER 318's (Aug 2025-May 2026) despite 316 < 318
# numerically. Ordering by raw season_id would silently get mixed-system
# transitions backwards. SEASON_CHRONO_ORDER below ranks by each season's
# rough midpoint instead (315 < 318 < 316 < 351) -- a reasonable, explicit
# heuristic, not a locked calibration decision; flagged here rather than
# buried.
#
# Known limitation: this can only fall back to a player's PRIOR league's
# DataScore_previo if that prior row's role has been implemented in
# base_scores.R. Volante/Extremo (frozen) and any role not yet built
# simply have no DataScore_previo to fall back to -- a transition INTO an
# unimplemented role, or FROM one, leaves the gate unable to resolve;
# handled by leaving DataScore_shown as NA rather than guessing.
#
# Standalone script, not wired into app.R yet -- same convention as every
# other file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("competition_strength.R")  # apply_competition_strength(),
                                   # VAR_TO_LEAGUE, ROLE_SCORE_DEFS, etc.
                                   # (via its own source() chain)

MATCH_THRESHOLD <- 8
MINUTES_THRESHOLD <- 600

# Rank, not the raw season_id -- see file header. Higher = more recent.
SEASON_CHRONO_ORDER <- c("315" = 1, "318" = 2, "316" = 3, "351" = 4)

# Continental competitions run ALONGSIDE a domestic league, not instead
# of one -- a player's Libertadores/CCL/UCL/UEL row and their domestic
# row are the SAME real stint, not a before/after pair. Discovered
# 2026-10-08: Copa Libertadores pulls as season_id 315 while the South
# American domestic leagues (Colombia/Argentina/Paraguay/...) pull as
# 316, which made the gate see "Libertadores -> domestic league" as a
# sequential transfer for dozens of players who never actually moved.
# Extends the already-locked Competition-Strength rule ("domestic league
# always takes precedence over continental competition") to this gate:
# a continental row is dropped from a player's row set whenever they
# also have at least one domestic row, before chronology is computed.
CONTINENTAL_LEAGUES <- c(
  "UEFA Champions League", "UEFA Europa League",
  "Copa Libertadores", "CONCACAF Champions Cup"
)

# ---- Core function ------------------------------------------------------
# rows: data.frame with (at least) player_id, var_name, season_id,
#       league (from VAR_TO_LEAGUE), DataScore_previo,
#       player_season_minutes, player_season_appearances -- ALL rows
#       across ALL implemented roles for ALL players (one row per
#       player-league-season, same shape as competition_strength.R's
#       per-role output, row-bound together).
# Returns one row PER PLAYER: their current league/role context, whether
# they're mid-transition, whether the gate is actively holding over an
# old score, and DataScore_shown -- the single number the app would
# actually display today.
apply_transition_gate <- function(rows) {
  rows <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::mutate(chrono_rank = unname(SEASON_CHRONO_ORDER[as.character(season_id)]))

  # Drop a continental-competition row whenever that player ALSO has a
  # domestic row -- see CONTINENTAL_LEAGUES above.
  rows <- rows |>
    dplyr::group_by(player_id) |>
    dplyr::filter(!(league %in% CONTINENTAL_LEAGUES & any(!league %in% CONTINENTAL_LEAGUES))) |>
    dplyr::ungroup()

  rows |>
    dplyr::group_by(player_id) |>
    dplyr::group_modify(function(df, key) {
      # Most recent row = current context. Ties (e.g. two competitions in
      # the same chrono rank, like a domestic league + a continental cup
      # in the same season) broken by minutes played, most first.
      df <- df |> dplyr::arrange(dplyr::desc(chrono_rank), dplyr::desc(player_season_minutes))
      current <- df[1, ]

      # Most recent STRICTLY PRIOR row, regardless of league -- the
      # player's last context before whatever "current" represents.
      prior_candidates <- df |> dplyr::filter(chrono_rank < current$chrono_rank)
      prior <- if (nrow(prior_candidates)) prior_candidates[1, ] else NULL

      transitioned <- !is.null(prior) && !identical(prior$league, current$league)

      if (!transitioned) {
        gate_active <- FALSE
        shown <- current
      } else {
        meets_threshold <- (current$player_season_minutes >= MINUTES_THRESHOLD) |
          (current$player_season_appearances >= MATCH_THRESHOLD)
        gate_active <- !meets_threshold
        shown <- if (meets_threshold) current else prior
      }

      tibble::tibble(
        player_name = current$player_name,
        current_var_name = current$var_name,
        current_league = current$league,
        current_role = current$role_group_matchbased,
        prior_league = if (is.null(prior)) NA_character_ else prior$league,
        transitioned = transitioned,
        gate_active = gate_active,
        minutes_in_current_league = current$player_season_minutes,
        appearances_in_current_league = current$player_season_appearances,
        source_var_name = shown$var_name,
        DataScore_shown = shown$DataScore_previo
      )
    }) |>
    dplyr::ungroup()
}

# ============================================================
# Validation harness -- only runs when executed directly.
# ============================================================
if (sys.nframe() == 0) {

  tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
  dat <- load_scout_data()

  all_rows <- list()
  for (role in names(ROLE_SCORE_DEFS)) {
    def <- ROLE_SCORE_DEFS[[role]]
    metrics <- unique(unlist(lapply(def$caps, names)))
    metrics <- intersect(metrics, names(dat))

    role_idx <- which(dat$role_group_matchbased == role)
    normalized <- dat[role_idx, c("var_name", "player_id", "player_name", "role_group_matchbased")]
    for (m in metrics) {
      normalized[[m]] <- normalize_metric(dat[[m]][role_idx], dat$role_group_matchbased[role_idx], dat$exposure_90s[role_idx], m)
    }

    capacities <- normalized |> dplyr::select(var_name, player_id, player_name)
    for (cap in names(def$caps)) capacities[[cap]] <- capacity_subscore(normalized, def$caps[[cap]])$score

    ds <- combine_capacities(capacities, def$ds)
    cs <- apply_competition_strength(capacities$var_name, ds$score, tiers)

    all_rows[[role]] <- capacities |>
      dplyr::select(var_name, player_id, player_name) |>
      dplyr::mutate(
        role_group_matchbased = role,
        season_id = dat$season_id[role_idx],
        league = unname(VAR_TO_LEAGUE[var_name]),
        DataScore_previo = cs$datascore_previo,
        player_season_minutes = suppressWarnings(as.numeric(dat$player_season_minutes[role_idx])),
        player_season_appearances = suppressWarnings(as.numeric(dat$player_season_appearances[role_idx]))
      )
  }
  rows <- dplyr::bind_rows(all_rows)
  message(sprintf("Built %d scored rows across %d roles for the gate to work with", nrow(rows), length(all_rows)))

  gated <- apply_transition_gate(rows)

  out_path <- "data/datascore_gated.rds"
  saveRDS(gated, out_path)
  message(sprintf("Wrote %s (%d players)", out_path, nrow(gated)))

  message("\n=== Summary ===")
  cat(sprintf("Players with a resolved current score: %d\n", sum(!is.na(gated$DataScore_shown))))
  cat(sprintf("Players who transitioned leagues (most recent two rows differ): %d\n", sum(gated$transitioned)))
  cat(sprintf("Players currently GATED (held on old-league score, <8 matches/600 min in new league): %d\n", sum(gated$gate_active)))

  message("\n=== Spot check: gated players (old score held over) ===")
  print(
    gated |>
      dplyr::filter(gate_active) |>
      dplyr::arrange(dplyr::desc(minutes_in_current_league)) |>
      dplyr::select(player_name, prior_league, current_league, minutes_in_current_league,
                     appearances_in_current_league, source_var_name, DataScore_shown) |>
      head(10)
  )

  message("\n=== Spot check: transitioned players where the gate has ALREADY released (new score active) ===")
  print(
    gated |>
      dplyr::filter(transitioned & !gate_active) |>
      dplyr::arrange(dplyr::desc(minutes_in_current_league)) |>
      dplyr::select(player_name, prior_league, current_league, minutes_in_current_league,
                     appearances_in_current_league, source_var_name, DataScore_shown) |>
      head(10)
  )
}
