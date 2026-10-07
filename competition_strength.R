# ============================================================
# competition_strength.R
#
# Applies Competition Strength to DataScore ONLY (doc S8, S12.3 steps
# 2-3) -- AmeScore never touches this, per the doc's own rule, reconfirmed
# by the user even under the tier-based (not transfer-delta-regression)
# approach to Competition Strength (see datascore-v2-overhaul memory).
#
#   Ajuste Liga      = lambda x (Competition Strength - nivel de referencia)
#   DataScore previo = DataScore Base + Ajuste Liga
#
# Stops at "DataScore previo" -- step 4 (confidence shrinkage C toward
# the role mean, "DataScore final") and the league-transition gate (8
# matches/600 min hard cutoff for a player who just changed leagues) are
# separate, later pipeline steps per the agreed sequencing, not built
# here.
#
# lambda is explicitly "por calibrar" in the doc (S12.7/S12.8) -- it
# should come from real transfer-delta calibration (observing how the
# same players' production changes when they switch leagues), never
# picked to hit a target number. Using the doc's OWN illustrative value
# (S12.3's worked example, lambda=0.25) here rather than any value found
# useful during the correlation investigation, for exactly that reason.
#
# Standalone script, not wired into app.R yet -- same convention as
# every other file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("base_scores.R")  # combine_capacities(), ROLE_SCORE_DEFS,
                          # capacity_subscore(), normalize_metric(),
                          # load_scout_data() (all via base_scores.R's
                          # own source() chain)

# ---- Competition Strength parameters -- PLACEHOLDER, por calibrar -----
# Doc's own worked example (S12.3) uses lambda=0.25 and reference=50 (the
# rough midpoint of the 16-100 strength scale) -- explicitly NOT fixed as
# real parameters there either ("Este ejemplo NO fija lambda=0.25 ... como
# parámetros reales; únicamente muestra cómo funcionaría la mecánica").
COMPETITION_STRENGTH_LAMBDA <- 0.25
COMPETITION_STRENGTH_REFERENCE <- 50

# ---- var_name -> tier CSV league name -------------------------------
# dashboard_scout.R's variable names (e.g. "ligamx_2526") don't match the
# tier CSV's human league names (e.g. "Liga MX") directly -- same mapping
# problem solved ad hoc in the tmp/ sensitivity-check script during the
# correlation investigation, now made permanent here since Competition
# Strength depends on it structurally. Covers all 65 var_name values
# found in the data (confirmed via `unique(dat$var_name)`, 2026-10-08).
VAR_TO_LEAGUE <- c(
  arg_2025 = "Argentina", australia_2526 = "Australia",
  belgica_2526 = "Bélgica", belgica_2627 = "Bélgica",
  brasil_2025 = "Brasil – Série A",
  bundesliga_2_2526 = "2. Bundesliga", bundesliga_2_2627 = "2. Bundesliga",
  bundesliga_2526 = "Bundesliga", bundesliga_2627 = "Bundesliga",
  ccl_2025 = "CONCACAF Champions Cup",
  champions_2526 = "UEFA Champions League", champions_2627 = "UEFA Champions League",
  championship_2526 = "Championship (Inglaterra 2ª)", championship_2627 = "Championship (Inglaterra 2ª)",
  chequia_2526 = "Chequia (República Checa)", chequia_2627 = "Chequia (República Checa)",
  chile_2025 = "Chile", china_2025 = "China", colombia_2025 = "Colombia",
  ecuador_2025 = "Ecuador",
  efl_1_2526 = "EFL League One", efl_1_2627 = "EFL League One",
  efl_2_2526 = "EFL League Two", efl_2_2627 = "EFL League Two",
  eredivisie_2526 = "Eredivisie", eredivisie_2627 = "Eredivisie",
  escocia_2526 = "Escocia", escocia_2627 = "Escocia",
  europa_league_2526 = "UEFA Europa League", europa_league_2627 = "UEFA Europa League",
  laliga_2_2526 = "LaLiga 2", laliga_2_2627 = "LaLiga 2",
  laliga_2526 = "LaLiga", laliga_2627 = "LaLiga",
  libertadores = "Copa Libertadores",
  ligamx_2526 = "Liga MX", ligamx_2627 = "Liga MX",
  ligue_1_2526 = "Ligue 1", ligue_1_2627 = "Ligue 1",
  ligue_2_2526 = "Ligue 2", ligue_2_2627 = "Ligue 2",
  mls_2025 = "MLS", noruega_2025 = "Noruega", paraguay_2025 = "Paraguay", peru_2025 = "Perú",
  polonia_2526 = "Polonia", polonia_2627 = "Polonia",
  portugal_2526 = "Primeira Liga (Portugal)", portugal_2627 = "Primeira Liga (Portugal)",
  premier_2526 = "Premier League", premier_2627 = "Premier League",
  rusia_2526 = "Rusia", rusia_2627 = "Rusia",
  serie_a_2526 = "Serie A", serie_a_2627 = "Serie A",
  serie_b_2526 = "Serie B (Italia)", serie_b_2627 = "Serie B (Italia)",
  suiza_challenger_2526 = "Suiza Challenger League", suiza_challenger_2627 = "Suiza Challenger League",
  suiza_super_league_2526 = "Suiza Super League", suiza_super_league_2627 = "Suiza Super League",
  turquia_2526 = "Süper Lig (Turquía)", turquia_2627 = "Süper Lig (Turquía)",
  uruguay_2025 = "Uruguay", usl_championship_2025 = "USL Championship"
)

# ---- Core function ------------------------------------------------------
# var_name:        character vector -- league/season key (e.g. "ligamx_2526")
# data_score_base: numeric vector, same length -- DataScore_Base
# tiers:           the competition_strength_tiers.csv data.frame
# Returns a data.frame(strength, ajuste_liga, datascore_previo) -- NA
# wherever var_name has no tier-CSV mapping (should not happen now that
# every league in the data has an entry; kept as a safety net, not an
# expected path).
apply_competition_strength <- function(var_name, data_score_base, tiers,
                                        lambda = COMPETITION_STRENGTH_LAMBDA,
                                        reference = COMPETITION_STRENGTH_REFERENCE) {
  liga <- unname(VAR_TO_LEAGUE[var_name])
  strength <- tiers$strength[match(liga, tiers$league)]
  ajuste_liga <- lambda * (strength - reference)
  datascore_previo <- data_score_base + ajuste_liga
  data.frame(strength = strength, ajuste_liga = round(ajuste_liga, 2), datascore_previo = round(datascore_previo, 1))
}

# ---- Shared row-builder -------------------------------------------------
# Normalize -> capacities -> DataScore_Base/AmeScore_Base -> Competition
# Strength, for every implemented role, row-bound into one table. Factored
# out 2026-10-08 after this exact loop got duplicated a third time
# (league_transition_gate.R, confidence_shrinkage.R) -- same reasoning as
# load_scout_data().  dat and tiers are both required (no defaults) so
# callers can't forget to load either.
build_scored_rows <- function(dat, tiers) {
  all_results <- list()
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
    for (cap in names(def$caps)) {
      capacities[[cap]] <- capacity_subscore(normalized, def$caps[[cap]])$score
    }

    ds <- combine_capacities(capacities, def$ds)
    ame <- combine_capacities(capacities, def$ame)
    cs <- apply_competition_strength(capacities$var_name, ds$score, tiers)

    # Keep the individual capacity columns too (not just the combined
    # DataScore_Base/AmeScore_Base) -- AmeScore's structural gate
    # (ame_score_gate.R) needs to check a specific capacity's value per
    # row, not just the weighted composite. Capacity names differ by
    # role (e.g. "Defensa/Duelos" only exists for Central) except the
    # shared ALMADA_IDENTITY_CAPACITIES ones, which bind_rows() aligns
    # across roles automatically; a role without a given capacity just
    # gets NA there, same as any other bind_rows() column mismatch.
    all_results[[role]] <- capacities |>
      dplyr::mutate(
        role_group_matchbased = role,
        season_id = dat$season_id[role_idx],
        league = unname(VAR_TO_LEAGUE[var_name]),
        DataScore_Base = round(ds$score, 1),
        DataScore_Base_cobertura = round(ds$coverage * 100, 1),
        CompetitionStrength = cs$strength,
        Ajuste_Liga = cs$ajuste_liga,
        DataScore_previo = cs$datascore_previo,
        AmeScore_Base = round(ame$score, 1),
        AmeScore_Base_cobertura = round(ame$coverage * 100, 1),
        player_season_minutes = suppressWarnings(as.numeric(dat$player_season_minutes[role_idx])),
        player_season_appearances = suppressWarnings(as.numeric(dat$player_season_appearances[role_idx]))
      )
  }
  dplyr::bind_rows(all_results)
}

# ============================================================
# Validation harness -- only runs when executed directly.
# ============================================================
if (sys.nframe() == 0) {

  tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
  message(sprintf("Loaded %d leagues from competition_strength_tiers.csv", nrow(tiers)))

  dat <- load_scout_data()

  unmapped <- setdiff(unique(dat$var_name), names(VAR_TO_LEAGUE))
  if (length(unmapped)) message("WARNING -- var_name with no league mapping: ", paste(unmapped, collapse = ", "))
  no_strength <- setdiff(unique(unname(VAR_TO_LEAGUE)), tiers$league)
  if (length(no_strength)) message("WARNING -- league with no tier CSV entry: ", paste(no_strength, collapse = ", "))

  rows <- build_scored_rows(dat, tiers)
  all_results <- split(rows, rows$role_group_matchbased)

  for (role in names(all_results)) {
    result <- all_results[[role]]
    valid <- !is.na(result$DataScore_previo)
    message(sprintf(
      "%-20s n=%-5d strength_coverage=%.1f%%  DataScore_previo[%.1f,%.1f]",
      role, nrow(result), 100 * mean(valid),
      min(result$DataScore_previo, na.rm = TRUE), max(result$DataScore_previo, na.rm = TRUE)
    ))
  }

  out_path <- "data/datascore_previo_by_role.rds"
  saveRDS(all_results, out_path)
  message(sprintf("\nWrote %s (%d roles)", out_path, length(all_results)))

  # ---- Sanity checks on Interior ----
  interior <- all_results[["Interior"]]
  message("\n=== Interior: effect of Competition Strength on real players ===")
  print(
    interior |>
      dplyr::filter(!is.na(DataScore_previo)) |>
      dplyr::distinct(player_name, .keep_all = TRUE) |>
      dplyr::arrange(dplyr::desc(abs(Ajuste_Liga))) |>
      dplyr::select(player_name, CompetitionStrength, DataScore_Base, Ajuste_Liga, DataScore_previo) |>
      head(10)
  )

  message("\n=== Interior: top 10 by DataScore_previo (post-Competition Strength) vs DataScore_Base ===")
  print(
    interior |>
      dplyr::filter(!is.na(DataScore_previo)) |>
      dplyr::arrange(dplyr::desc(DataScore_previo)) |>
      dplyr::select(player_name, CompetitionStrength, DataScore_Base, DataScore_previo) |>
      head(10)
  )

  # Ranking stability check -- how much does applying Competition Strength
  # reshuffle the ranking vs raw DataScore_Base?
  comp <- interior |> dplyr::filter(!is.na(DataScore_Base) & !is.na(DataScore_previo))
  message(sprintf(
    "\n=== Interior: Spearman rank correlation DataScore_Base vs DataScore_previo = %.3f (lower = more reshuffling from Competition Strength) ===",
    cor(comp$DataScore_Base, comp$DataScore_previo, method = "spearman")
  ))
}
