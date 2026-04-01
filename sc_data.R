library(tidyverse)
library(readr)
library(devtools)
library(StatsBombR)
library(ggsoccer)
library(soccermatics)
library(stats)
library(gt)
library(grid)
library(viridis)
library(glue)
library(cluster)
library(ggrepel)
library(purrr)
library(lubridate)
library(scales)
library(fmsb)

# Datos Desmarques ----

ligamx_des <- read_csv("data/off_ball_runs/ligamx_obr_normalized.csv")

# write_csv(ligamx_des, "/Users/mateorodriguez/Desktop/ligamx_des.csv")

# Datos Pases ----

ligamx_passes_pm <- read_csv("data/passes/ligamx_per_match.csv")
ligamx_passes_p100o <- read_csv("data/passes/ligamx_per_100_pass_opportunities.csv")
ligamx_passes_p30mtip <- read_csv("data/passes/ligamx_per_30_min_tip.csv")

liga_mx_passes <- ligamx_passes_pm |>
  full_join(ligamx_passes_p100o, by = c("player_id", "player_name", "short_name", "player_birthdate", "match_id", "match_name",
                                        "match_date", "team_id", "team_name", "competition_id", "competition_name", "season_id", 
                                        "season_name", "competition_edition_id", "position", "group", "result", "venue", "third",
                                        "channel", "minutes_played_per_match", "adjusted_min_tip_per_match", "quality_check",
                                        "pass_completion_ratio_to_runs", "count_opportunities_to_pass_to_runs_in_sample")) |>
  full_join(ligamx_passes_p30mtip, by = c("player_id", "player_name", "short_name", "player_birthdate", "match_id", "match_name",
                                        "match_date", "team_id", "team_name", "competition_id", "competition_name", "season_id", 
                                        "season_name", "competition_edition_id", "position", "group", "result", "venue", "third",
                                        "channel", "minutes_played_per_match", "adjusted_min_tip_per_match", "quality_check",
                                        "pass_completion_ratio_to_runs", "count_opportunities_to_pass_to_runs_in_sample"))


# Build 1-row-per-player season table (averages across matches)
liga_mx_passes_season <- liga_mx_passes |>
  group_by(player_id, player_name) |>
  summarise(
    # keep player/team/competition “info” columns (take first non-NA)
    short_name       = dplyr::first(short_name[!is.na(short_name)]),
    player_birthdate = dplyr::first(player_birthdate[!is.na(player_birthdate)]),
    team_id          = dplyr::first(team_id[!is.na(team_id)]),
    team_name        = dplyr::first(team_name[!is.na(team_name)]),
    competition_id   = dplyr::first(competition_id[!is.na(competition_id)]),
    competition_name = dplyr::first(competition_name[!is.na(competition_name)]),
    
    # useful season-level counts
    games_played = n_distinct(match_id),
    
    # average ALL numeric performance metrics, but do NOT average ID columns / match_id
    across(
      where(is.numeric) & !any_of(c("player_id", "team_id", "competition_id", "match_id")),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# write_csv(liga_mx_passes_season, "/Users/mateorodriguez/Desktop/liga_mx_passes_season.csv")

# Datos Presiones ----

ligamx_pres <- read_csv("data/pressures/ligamx_on_ball_pressures.csv")

ligamx_pres_season <- ligamx_pres |>
  group_by(player_id, player_name) |>
  summarise(
    # keep player/team/competition “info” columns (take first non-NA)
    short_name       = dplyr::first(short_name[!is.na(short_name)]),
    player_birthdate = dplyr::first(player_birthdate[!is.na(player_birthdate)]),
    team_id          = dplyr::first(team_id[!is.na(team_id)]),
    team_name        = dplyr::first(team_name[!is.na(team_name)]),
    competition_id   = dplyr::first(competition_id[!is.na(competition_id)]),
    competition_name = dplyr::first(competition_name[!is.na(competition_name)]),
    
    # useful season-level counts
    games_played = n_distinct(match_id),
    
    # average ALL numeric performance metrics, but do NOT average ID columns / match_id
    across(
      where(is.numeric) & !any_of(c("player_id", "team_id", "competition_id", "match_id")),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# write_csv(ligamx_pres_season, "/Users/mateorodriguez/Desktop/ligamx_pres_season.csv")

# Datos Físicos ----

ligamx_fisico <- read_csv("data/physical/ligamx_physical_standardized.csv")
# write_csv(ligamx_fisico, "/Users/mateorodriguez/Desktop/ligamx_fisico.csv")
arg_fisico <- read_csv("data/physical/arg_physical_standardized.csv")
bra_fisico <- read_csv("data/physical/brasil_physical_standardized.csv")
col_fisico <- read_csv("data/physical/col_physical_standardized.csv")
mls_fisico <- read_csv("data/physical/mls_physical_standardized.csv")
laliga_fisico <- read_csv("data/physical/laliga_physical_standardized.csv")
laliga2_fisico <- read_csv("data/physical/laliga2_physical_standardized.csv")
chile_fisico <- read_csv("data/physical/chile_physical_standardized.csv")
premier_fisico <- read_csv("data/physical/premier_physical_standardized.csv")
serie_a_fisico <- read_csv("data/physical/seriea_physical_standardized.csv")
tur_fisico <- read_csv("data/physical/tur_physical_standardized.csv")
uel_fisico <- read_csv("data/physical/uel_physical_standardized.csv")
champions_fisico <- read_csv("data/physical/ucl_physical_standardized.csv")

# 4 Data Frames Combinados ----
liga_mx_mitad_1 <- liga_mx_passes_season |>
  full_join(ligamx_pres_season, by = c("player_id", "player_name", "short_name", "player_birthdate", "team_id", "team_name", 
                                              "competition_id", "competition_name", "season_id", "competition_edition_id",
                                              "minutes_played_per_match", "adjusted_min_tip_per_match", "games_played")) 

# write_csv(liga_mx_mitad_1, "/Users/mateorodriguez/Desktop/liga_mx_mitad_1.csv")

liga_mx_mitad_1 <- liga_mx_mitad_1 |>
  group_by(player_id) |>
  summarise(
    across(
      everything(),
      function(x) {
        if (all(is.na(x))) NA else x[which(!is.na(x))[1]]
      }
    ),
    .groups = "drop"
  )

if ("player_short_name" %in% names(ligamx_fisico)) {
  ligamx_fisico <- ligamx_fisico |> rename(short_name = player_short_name)
}
ligamx_fisico <- ligamx_fisico |> select(-any_of("competition_name"))

ligamx_des <- ligamx_des |>
  select(-competition_name)

liga_mx_mitad_2 <- ligamx_fisico |>
  full_join(ligamx_des, by = c("player_id", "player_name", "short_name", "player_birthdate", "team_id", "team_name", 
                              "competition_id", "season_id", "season_name", "count_match",
                               "count_match_failed", "minutes_played_per_match")) 

# n_distinct(liga_mx_mitad_2$player_name)

# The columns that define a "unique player-season identity" in your merged table
player_key <- c(
  "short_name",
  "player_birthdate",
  "team_id",
  "team_name",
  "competition_id"
  )

# Helper: return first non-NA value (works for numeric/character/date/logical)
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}

liga_mx_mitad_2 <- liga_mx_mitad_2 |>
  group_by(across(all_of(player_key))) |>
  summarise(
    across(everything(), first_non_na),
    .groups = "drop"
  )

liga_mx_full <- liga_mx_mitad_1 |>
  full_join(liga_mx_mitad_2, by = c("player_id", "player_name", "short_name", "player_birthdate", 
                                    "team_id", "team_name", "competition_id", "season_id"))

# ligamx_sc <- list(ligamx = liga_mx_mitad_2)
# 
# saveRDS(
#   ligamx_sc,
#   file = "/Users/mateorodriguez/Desktop/ligamx_sc.rds",
#   compress = "gzip"
# )

# Helpers (your dropdown mapping) ----
LEAGUE_CATALOG_FIS <- list(
  "Liga MX"               = liga_mx_full,
  "Argentina"             = arg_fisico,
  "Brasil – Série A"      = bra_fisico,
  "Colombia"              = col_fisico,
  "Chile"                 = chile_fisico,
  "MLS"                   = mls_fisico,
  "Premier League"        = premier_fisico,
  "LaLiga"                = laliga_fisico,
  "LaLiga 2"              = laliga2_fisico,
  "Serie A"               = serie_a_fisico,
  "Turquía – Süper Lig"   = tur_fisico,
  "UEFA Champions League" = champions_fisico,
  "UEFA Europa League"    = uel_fisico
)

# --- CACHE: bundle physical dfs into one RDS ------------------------------

# Create /data if it doesn't exist (relative to the project root)
if (!dir.exists("data")) dir.create("data", recursive = TRUE)

# Put only what the app needs into a named list
fis_data <- list(
  liga_mx_full    = liga_mx_full,
  arg_fisico       = arg_fisico,
  bra_fisico       = bra_fisico,
  col_fisico       = col_fisico,
  chile_fisico     = chile_fisico,
  mls_fisico       = mls_fisico,
  premier_fisico   = premier_fisico,
  laliga_fisico    = laliga_fisico,
  laliga2_fisico   = laliga2_fisico,
  serie_a_fisico   = serie_a_fisico,
  tur_fisico       = tur_fisico,
  champions_fisico = champions_fisico,
  uel_fisico       = uel_fisico
)

# Optional: basic sanity check (helps avoid silent NULLs)
stopifnot(all(vapply(fis_data, is.data.frame, logical(1))))

# Save one compact file
saveRDS(
  object   = list(
    fis_data = fis_data,
    league_catalog_fis = names(LEAGUE_CATALOG_FIS) # or store the whole mapping if you prefer
  ),
  file     = "data/scout_skill_corner.rds",
  compress = "gzip"
)

message(
  sprintf("Wrote cache: data/fis_data.rds (%.1f MB)",
          file.info("data/scout_skill_corner.rds")$size / 1024^2)
)


# hormiga <- fis_data[[1]]
# 
# horm_uni <- hormiga |>
#   distinct(player_id, .keep_all = TRUE)

# # Datos Físicos ----
# 
# ligamx_fisico <- read_csv("data/physical/liga_mx_physical.csv")
# # write_csv(ligamx_fisico, "/Users/mateorodriguez/Desktop/ligamx_fisico.csv")
# arg_fisico <- read_csv("data/physical/argentina_physical.csv")
# bra_fisico <- read_csv("data/physical/brasil_physical.csv")
# col_fisico <- read_csv("data/physical/colombia_physical.csv")
# mls_fisico <- read_csv("data/physical/mls_physical.csv")
# laliga_fisico <- read_csv("data/physical/laliga_physical.csv")
# laliga2_fisico <- read_csv("data/physical/laliga2_physical.csv")
# chile_fisico <- read_csv("data/physical/chile_physical.csv")
# premier_fisico <- read_csv("data/physical/premier_physical.csv")
# serie_a_fisico <- read_csv("data/physical/serie_a_physical.csv")
# tur_fisico <- read_csv("data/physical/turquia_physical.csv")
# uel_fisico <- read_csv("data/physical/uel_physical.csv")
# champions_fisico <- read_csv("data/physical/champions_physical.csv")
# 
# fisico <- bind_rows(
#   ligamx_fisico,
#   arg_fisico,
#   bra_fisico,
#   col_fisico,
#   mls_fisico,
#   laliga_fisico,
#   laliga2_fisico,
#   chile_fisico,
#   premier_fisico,
#   serie_a_fisico,
#   tur_fisico,
#   uel_fisico,
#   champions_fisico
# )
# 
# # 4 Data Frames Combinados ----
# liga_mx_mitad_1 <- liga_mx_passes_season |>
#   full_join(ligamx_pres_season, by = c("player_id", "player_name", "short_name", "player_birthdate", "team_id", "team_name", 
#                                        "competition_id", "competition_name", "season_id", "competition_edition_id",
#                                        "minutes_played_per_match", "adjusted_min_tip_per_match", "games_played")) 
# 
# # write_csv(liga_mx_mitad_1, "/Users/mateorodriguez/Desktop/liga_mx_mitad_1.csv")
# 
# liga_mx_mitad_1 <- liga_mx_mitad_1 |>
#   group_by(player_id) |>
#   summarise(
#     across(
#       everything(),
#       function(x) {
#         if (all(is.na(x))) NA else x[which(!is.na(x))[1]]
#       }
#     ),
#     .groups = "drop"
#   )
# 
# fisico <- fisico |>
#   select(-competition_name, -player_short_name) 
# # rename(short_name = player_short_name) 
# 
# ligamx_des <- ligamx_des |>
#   select(-competition_name)
# 
# liga_mx_mitad_2 <- fisico |>
#   full_join(ligamx_des, by = c("player_id", "player_name", "short_name", "player_birthdate", "team_id", "team_name", 
#                                "competition_id", "season_id", "season_name", "count_match",
#                                "count_match_failed", "minutes_played_per_match")) 
# 
# # n_distinct(liga_mx_mitad_2$player_name)
# 
# # The columns that define a "unique player-season identity" in your merged table
# player_key <- c(
#   "short_name",
#   "player_birthdate",
#   "team_id",
#   "team_name",
#   "competition_id"
# )
# 
# # Helper: return first non-NA value (works for numeric/character/date/logical)
# first_non_na <- function(x) {
#   x <- x[!is.na(x)]
#   if (length(x) == 0) NA else x[1]
# }
# 
# liga_mx_mitad_2 <- liga_mx_mitad_2 |>
#   group_by(across(all_of(player_key))) |>
#   summarise(
#     across(everything(), first_non_na),
#     .groups = "drop"
#   )
# 
# liga_mx_full <- liga_mx_mitad_1 |>
#   full_join(liga_mx_mitad_2, by = c("player_id", "player_name", "short_name", "player_birthdate", 
#                                     "team_id", "team_name", "competition_id", "season_id"))
# 
# # ligamx_sc <- list(ligamx = liga_mx_mitad_2)
# # 
# # saveRDS(
# #   ligamx_sc,
# #   file = "/Users/mateorodriguez/Desktop/ligamx_sc.rds",
# #   compress = "gzip"
# # )
# 
# saveRDS(
#   liga_mx_full,
#   file = "/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/data/scout_skill_corner.rds"
# )
