# ============================================================
# role_eligibility_match_based.R
#
# Match-level (lineup) position tracking -- replaces the static
# primary_position/secondary_position approach in role_eligibility.R with
# minutes-weighted role shares computed from every match a player actually
# appeared in, picking up in-season position variance that a single
# season-level label can't (see the Nicolás Castro case: Left Wing in Liga
# MX vs. Left Defensive Midfielder in CONCACAF Champions Cup, same season,
# found 2026-10-06).
#
# Per the 2026-10-06/07 discussion: NOT re-deriving performance stats from
# player_match() (redundant -- player_season() already aggregates the same
# underlying match data StatsBomb's way, correctly, confirmed since
# dashboard_scout.R already calls it separately per competition, so there's
# no cross-competition blending to undo). This pulls ONLY the lineups
# endpoint, for position, nothing else.
#
# StatsBombR's alllineups()/cleanlineups() are broken -- they fail to
# decode gzip-compressed responses ("parse error: premature EOF" on every
# call, confirmed live 2026-10-06). fetch_lineup() below bypasses them
# with a direct httr call instead, which works correctly.
#
# League -> (season_id, competition_id) pairs are parsed directly out of
# dashboard_scout.R's safe_matchesvector(...) calls via regex (not by
# sourcing that file -- it pulls live StatsBomb data top to bottom and
# would be extremely slow/wasteful to execute just to read off some
# constants), so this never drifts out of sync with the real pipeline's
# league list.
#
# Standalone script, not wired into dashboard_scout.R's live pipeline yet
# -- same reasoning as role_eligibility.R (fast to iterate on while this
# is still being validated).
#
# Usage: Rscript role_eligibility_match_based.R [league_var_pattern]
#   With no argument, runs every league found in dashboard_scout.R.
#   With an argument (regex), only runs matching var names -- e.g.
#   "^ligamx" to validate on just Liga MX before committing to a full run.
# ============================================================

suppressWarnings(suppressMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
}))

readRenviron("~/.Renviron")
username <- Sys.getenv("SB_USERNAME")
password <- Sys.getenv("SB_PASSWORD")

# Mismo mapping conceptual que RAW_POSITION_TO_ROLE_GROUP en
# role_eligibility.R, pero cubriendo DOS convenciones de nombres --
# player_season()/secondary_position usa "Centre"/"...Midfielder"
# (ortografía británica completa), pero el endpoint de lineups usa
# "Center"/"...Midfield" (sin el "-er" final) para los mismos puestos,
# confirmado en vivo 2026-10-07: 68% de los stints de este primer corrido
# mapeaban a NA en silencio por este desajuste de strings (p.ej. "Left
# Defensive Midfield" acá vs. "Left Defensive Midfielder" en
# player_season). También aparece acá un "Center Back" sin calificar
# Left/Right que no existe en la taxonomía de player_season.
# Duplicado a propósito en vez de source()-ar role_eligibility.R entero
# (que además carga data/scout_joined.rds y corre toda su clasificación --
# costo innecesario acá). Si se retoca el split Interior/Mediapunta, hay
# que tocar las dos copias.
RAW_POSITION_TO_ROLE_GROUP <- c(
  "Goalkeeper" = "Portero",

  "Centre Back" = "Central",
  "Center Back" = "Central",
  "Right Centre Back" = "Central",
  "Right Center Back" = "Central",
  "Left Centre Back" = "Central",
  "Left Center Back" = "Central",

  "Left Back" = "Lateral/Carrilero",
  "Left Wing Back" = "Lateral/Carrilero",
  "Right Back" = "Lateral/Carrilero",
  "Right Wing Back" = "Lateral/Carrilero",

  "Centre Defensive Midfielder" = "Medio de Contención",
  "Center Defensive Midfield" = "Medio de Contención",
  "Right Defensive Midfielder" = "Medio de Contención",
  "Right Defensive Midfield" = "Medio de Contención",
  "Left Defensive Midfielder" = "Medio de Contención",
  "Left Defensive Midfield" = "Medio de Contención",

  "Left Centre Midfielder" = "Interior",
  "Left Center Midfield" = "Interior",
  "Right Centre Midfielder" = "Interior",
  "Right Center Midfield" = "Interior",
  # Sin calificar Left/Right -- visto 16 veces en el corrido completo
  # 2026-10-06/07 (0.003% de los stints), no existe en player_season().
  "Center Midfield" = "Interior",

  "Centre Attacking Midfielder" = "Mediapunta",
  "Center Attacking Midfield" = "Mediapunta",
  "Left Attacking Midfielder" = "Mediapunta",
  "Left Attacking Midfield" = "Mediapunta",
  "Right Attacking Midfielder" = "Mediapunta",
  "Right Attacking Midfield" = "Mediapunta",

  "Left Midfielder" = "Volante/Extremo",
  "Left Midfield" = "Volante/Extremo",
  "Left Wing" = "Volante/Extremo",
  "Right Midfielder" = "Volante/Extremo",
  "Right Midfield" = "Volante/Extremo",
  "Right Wing" = "Volante/Extremo",

  "Centre Forward" = "Delantero",
  "Center Forward" = "Delantero",
  "Left Centre Forward" = "Delantero",
  "Left Center Forward" = "Delantero",
  "Right Centre Forward" = "Delantero",
  "Right Center Forward" = "Delantero"
)
map_role_group <- function(raw_position) unname(RAW_POSITION_TO_ROLE_GROUP[raw_position])

# ---- 1. Parse league -> (season_id, competition_id) out of dashboard_scout.R ----
src <- readLines("dashboard_scout.R", warn = FALSE)
src_text <- paste(src, collapse = "\n")
matches <- gregexpr(
  "(\\w+)\\s*<-\\s*safe_matchesvector\\(username,\\s*password,\\s*season_id\\s*=\\s*(\\d+),\\s*competition_id\\s*=\\s*(\\d+)\\)",
  src_text, perl = TRUE
)
raw_matches <- regmatches(src_text, matches)[[1]]
league_pairs <- purrr::map_dfr(raw_matches, function(m) {
  var <- sub("\\s*<-.*", "", m)
  sid <- as.integer(sub(".*season_id\\s*=\\s*(\\d+).*", "\\1", m))
  cid <- as.integer(sub(".*competition_id\\s*=\\s*(\\d+)\\).*", "\\1", m))
  tibble(var_name = trimws(var), season_id = sid, competition_id = cid)
})
message(sprintf("Parsed %d league/season pairs from dashboard_scout.R", nrow(league_pairs)))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) {
  league_pairs <- league_pairs |> filter(grepl(args[1], var_name))
  message(sprintf("Filtered to %d pairs matching '%s'", nrow(league_pairs), args[1]))
}

# ---- 2. Direct lineup fetch (bypasses StatsBombR's broken alllineups()) ----
fetch_lineup <- function(match_id) {
  url <- sprintf("https://data.statsbomb.com/api/v4/lineups/%d", match_id)
  resp <- httr::GET(url, httr::authenticate(username, password))
  if (httr::status_code(resp) != 200) return(NULL)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), flatten = FALSE)
}

# "HH:MM:SS.mmm" -> minutos (decimal). El reloj NO se reinicia por período
# (confirmado en vivo 2026-10-06: un cambio en el segundo tiempo trae
# from="01:22:43.087", es decir, tiempo de partido corrido, no relativo
# al período) -- parsearlo como duración simple alcanza.
clock_to_minutes <- function(clock_str) {
  parts <- strsplit(clock_str, ":")
  vapply(parts, function(p) {
    if (length(p) != 3 || any(is.na(p))) return(NA_real_)
    as.numeric(p[1]) * 60 + as.numeric(p[2]) + as.numeric(p[3]) / 60
  }, numeric(1))
}

# Convierte el lineup crudo de un partido en filas player x position-stint
# con minutos jugados en cada stint. `to` ausente significa "hasta el
# final del partido" -- se usa el `to` máximo observado en CUALQUIER
# jugador de ese partido como proxy del pitazo final (evita otra llamada
# a la API solo para la duración exacta).
parse_lineup_stints <- function(lineup_raw, match_id) {
  if (is.null(lineup_raw) || length(lineup_raw) == 0) return(NULL)

  all_rows <- purrr::map2_dfr(lineup_raw$team_id, lineup_raw$team_name, function(tid, tname) {
    idx <- which(lineup_raw$team_id == tid)
    players <- lineup_raw$lineup[[idx[1]]]
    if (is.null(players) || nrow(players) == 0) return(NULL)
    purrr::map_dfr(seq_len(nrow(players)), function(i) {
      pos <- players$positions[[i]]
      if (is.null(pos) || nrow(pos) == 0) return(NULL)
      pos |>
        mutate(
          player_id = players$player_id[i],
          player_name = players$player_name[i],
          team_id = tid,
          team_name = tname,
          match_id = match_id
        )
    })
  })

  if (is.null(all_rows) || nrow(all_rows) == 0) return(NULL)

  all_rows$from_min <- clock_to_minutes(all_rows$from)
  all_rows$to_min <- clock_to_minutes(all_rows$to)
  match_end <- suppressWarnings(max(all_rows$to_min, na.rm = TRUE))
  if (!is.finite(match_end)) match_end <- 95  # fallback if truly nothing has a `to`
  all_rows$to_min[is.na(all_rows$to_min)] <- match_end
  all_rows$stint_minutes <- pmax(0, all_rows$to_min - all_rows$from_min)

  all_rows |>
    mutate(role_group = map_role_group(position)) |>
    select(match_id, player_id, player_name, team_id, team_name, position, role_group, stint_minutes)
}

# ---- 3. Pull every match for every league/season pair ----
all_stints <- list()
pair_i <- 0
for (r in seq_len(nrow(league_pairs))) {
  var_name <- league_pairs$var_name[r]
  season_id <- league_pairs$season_id[r]
  competition_id <- league_pairs$competition_id[r]
  pair_i <- pair_i + 1

  mids <- tryCatch(
    StatsBombR::matchesvector(username, password, season_id = season_id, competition_id = competition_id, version = "v6"),
    error = function(e) {
      message(sprintf("  [WARN] %s: matchesvector failed (%s)", var_name, conditionMessage(e)))
      integer(0)
    }
  )
  message(sprintf("[%d/%d] %s (season=%d, competition=%d): %d matches",
                   pair_i, nrow(league_pairs), var_name, season_id, competition_id, length(mids)))
  if (length(mids) == 0) next

  pair_stints <- purrr::map(mids, function(mid) {
    lu <- tryCatch(fetch_lineup(mid), error = function(e) NULL)
    tryCatch(parse_lineup_stints(lu, mid), error = function(e) NULL)
  })
  pair_df <- dplyr::bind_rows(pair_stints)
  if (nrow(pair_df) > 0) {
    pair_df$.var_name <- var_name
    all_stints[[var_name]] <- pair_df
  }
}

stints <- dplyr::bind_rows(all_stints)
saveRDS(stints, "data/role_eligibility_match_stints_raw.rds")
message(sprintf("\nWrote data/role_eligibility_match_stints_raw.rds (%d stint rows)", nrow(stints)))

# ---- 4. Aggregate to minutes-per-role per player per league-season ----
by_role <- stints |>
  group_by(.var_name, player_id, player_name, role_group) |>
  summarise(minutes = sum(stint_minutes, na.rm = TRUE), .groups = "drop")

ranked <- by_role |>
  group_by(.var_name, player_id, player_name) |>
  arrange(desc(minutes), .by_group = TRUE) |>
  mutate(
    total_minutes = sum(minutes),
    share = minutes / total_minutes,
    rank = row_number()
  ) |>
  ungroup()

primary <- ranked |> filter(rank == 1) |>
  transmute(.var_name, player_id, player_name,
            role_group_matchbased = role_group,
            role_group_minutes = minutes,
            total_minutes)

# Secundario: el segundo rol más jugado, solo si cubre al menos 20% de los
# minutos totales -- evita que una sola aparición de emergencia en otra
# posición cuente como "elegibilidad secundaria" real.
secondary <- ranked |> filter(rank == 2, share >= 0.20) |>
  transmute(.var_name, player_id, role_group_secondary_matchbased = role_group)

result <- primary |> left_join(secondary, by = c(".var_name", "player_id"))

out_path <- "data/role_eligibility_matchbased.rds"
saveRDS(result, out_path)
message(sprintf("Wrote %s (%d player-season rows)", out_path, nrow(result)))

message("\n=== Distribución role_group_matchbased (primario) ===")
print(sort(table(result$role_group_matchbased, useNA = "ifany"), decreasing = TRUE))

message("\n=== Cobertura del mapping de posiciones crudas (debería ser ~0 sin mapear) ===")
cat(sprintf("stints con role_group=NA: %d / %d (%.1f%%)\n",
            sum(is.na(stints$role_group)), nrow(stints), 100 * mean(is.na(stints$role_group))))
if (any(is.na(stints$role_group))) {
  print(sort(table(stints$position[is.na(stints$role_group)]), decreasing = TRUE))
}

message("\n=== % con role_group_secondary_matchbased ===")
cat(sprintf("%.1f%%\n", 100 * mean(!is.na(result$role_group_secondary_matchbased))))
