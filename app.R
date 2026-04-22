# ============================================================
# app.R — Scouting Dashboard + SkillCorner percentile table
#
# Data source: data/scout_joined.rds  (built by crosswalk_v4.R)
#   $league_catalog   — named list: all 27 leagues, SC cols merged in for 13
#   $joined_leagues   — 13 leagues with SC columns on each player row
#   $sb_only_leagues  — 14 SB-only leagues
#   $final_crosswalk  — crosswalk table (for reference)
#
# SkillCorner notes:
#   - SC columns are directly on each player row (not a separate dataframe)
#   - Physical metrics: available for all 13 SC leagues
#   - Game Intelligence (gi_vars): Liga MX only
#   - Percentiles: physical vs all 13 SC leagues by position bucket
#                  GI vs Liga MX only by position bucket
#   - Porteros excluded from SC table
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(rlang)
library(stringr)
library(tidyr)
library(shinythemes)
library(fmsb)
library(scales)
library(DT)

source("radars.R")

options(scipen = 999)

# ============================================================
# LOAD CACHE
# ============================================================
cache_path <- "data/scout_joined.rds"
if (!file.exists(cache_path)) stop("Missing: data/scout_joined.rds — run crosswalk_v4.R first")

joined_cache  <- readRDS(cache_path)
scout         <- joined_cache$league_catalog   # named list, all 27 leagues

# ============================================================
# LEAGUE MAP
# Keys  = display names shown in UI selectInput
# Values = names(scout) keys — now match league_catalog names directly
# ============================================================
league_map <- c(
  "Liga MX"                  = "Liga MX",
  "CONCACAF Champions Cup"   = "Concacaf Champions Cup",
  "Argentina"                = "Argentina",
  "Brasil"                   = "Brasil – Série A",
  "Colombia"                 = "Colombia",
  "Ecuador"                  = "Ecuador",
  "Chile"                    = "Chile",
  "Paraguay"                 = "Paraguay",
  "Uruguay"                  = "Uruguay",
  "MLS"                      = "MLS",
  "Premier League"           = "Premier League",
  "Championship"             = "Championship (EFL)",
  "LaLiga"                   = "LaLiga",
  "LaLiga 2"                 = "LaLiga 2",
  "Serie A"                  = "Serie A",
  "Serie B"                  = "Serie B",
  "Bundesliga"               = "Bundesliga",
  "2. Bundesliga"            = "2. Bundesliga",
  "Ligue 1"                  = "Ligue 1",
  "Eredivisie"               = "Eredivisie",
  "Bélgica"                  = "Bélgica",
  "Portugal"                 = "Portugal – Primeira Liga",
  "Turquía"                  = "Turquía – Süper Lig",
  "Escocia"                  = "Escocia – Premiership",
  "UEFA Champions League"    = "UEFA Champions League",
  "Copa Libertadores"        = "CONMEBOL Libertadores",
  "UEFA Europa League"       = "UEFA Europa League"
)

# Leagues that have SC data (joined_leagues keys)
SC_LEAGUES <- names(joined_cache$joined_leagues)

# ============================================================
# COUNTRY ID → NAME LOOKUP
# StatsBomb internal country_id → Spanish display name
# ============================================================
country_id_names <- c(
  "11"  = "Argentina",
  "22"  = "Bélgica",
  "31"  = "Brasil",
  "39"  = "Bolivia",
  "40"  = "Bosnia y Herzegovina",
  "45"  = "Chile",
  "49"  = "Colombia",
  "54"  = "Costa Rica",
  "65"  = "Ecuador",
  "68"  = "Inglaterra",
  "78"  = "Francia",
  "85"  = "Alemania",
  "101" = "Honduras",
  "109" = "Rep. Irlanda",
  "112" = "Italia",
  "113" = "Jamaica",
  "147" = "México",
  "152" = "Serbia",
  "154" = "Marruecos",
  "160" = "Países Bajos",
  "166" = "Nigeria",
  "176" = "Panamá",
  "178" = "Paraguay",
  "179" = "Perú",
  "182" = "Polonia",
  "183" = "Portugal",
  "201" = "Escocia",
  "202" = "Senegal",
  "214" = "España",
  "233" = "Turquía",
  "241" = "Estados Unidos",
  "242" = "Uruguay",
  "246" = "Venezuela",
  "249" = "Zimbabue"
)

# ============================================================
# CHARTS CONFIG (unchanged from your original)
# ============================================================
charts_cfg <- list(
  "Delantero" = list(
    list(id="del_goles",
         title="Goles vs xG (sin penales)", subtitle="Cada punto es un jugador",
         xlab="Goles de temporada (por 90)", ylab="xG (sin penales) de temporada (por 90)",
         kpis = c(season_goals="season_goals", season_goals_90="player_season_goals_90",
                  np_xg_90="player_season_np_xg_90"),
         x="season_goals_90", y="np_xg_90", arrange_by="season_goals"),
    list(id="del_asist",
         title="Mejores Asistidores", subtitle="Cada punto es un jugador",
         xlab="Asistencias de temporada (por 90)", ylab="xA de temporada (por 90)",
         kpis = c(season_assists="season_assists", season_assists_90="player_season_assists_90",
                  xa_90="player_season_xa_90"),
         x="season_assists_90", y="xa_90", arrange_by="season_assists"),
    list(id="del_peligro",
         title="Jugadores más Peligrosos", subtitle="Cada punto es un jugador",
         xlab="On-ball Value (por 90)", ylab="Tiros y Pases Clave (por 90)",
         kpis = c(obv_90="player_season_obv_90", shots_key_passes="player_season_shots_key_passes_90"),
         x="obv_90", y="shots_key_passes", arrange_by="shots_key_passes"),
    list(id="del_aereo",
         title="Mejor Juego Aéreo", subtitle="Cada punto es un jugador",
         xlab="% de Balones Aéreos Ganados", ylab="Balones Aéreos Ganados (por 90)",
         kpis = c(aerial_ratio="player_season_aerial_ratio", aerial_wins="player_season_aerial_wins_90"),
         x="aerial_ratio", y="aerial_wins", arrange_by="aerial_wins"),
    list(id="del_def",
         title="Balones Recuperados vs Presiones en Campo Rival", subtitle="Cada punto es un jugador",
         xlab="Presiones en Campo Rival (por 90)", ylab="Balones Recuperados (por 90)",
         kpis = c(presiones_campo_rival="player_season_fhalf_pressures_90",
                  recuperaciones_balon="player_season_ball_recoveries_90"),
         x="presiones_campo_rival", y="recuperaciones_balon", arrange_by="recuperaciones_balon"),
    list(id="del_outcomes",
         title="Participación en Posesiones con Resultado Positivo", subtitle="Cada punto es un jugador",
         xlab="Participación (por 90)", ylab="Frecuencia de participación",
         kpis = c(outcome_90="player_season_positive_outcome_90",
                  outcome_score="player_season_positive_outcome_score"),
         x="outcome_90", y="outcome_score", arrange_by="outcome_score")
  ),
  "Central" = list(
    list(id="cent_goles",  title="Goles vs xG (sin penales)", subtitle="Cada punto es un jugador",
         xlab="Goles (por 90)", ylab="xG sin penales (por 90)",
         kpis = c(season_goals="season_goals", season_goals_90="player_season_goals_90",
                  np_xg_90="player_season_np_xg_90"),
         x="season_goals_90", y="np_xg_90", arrange_by="season_goals"),
    list(id="cent_asist",  title="Mejores Asistidores", subtitle="Cada punto es un jugador",
         xlab="Asistencias (por 90)", ylab="xA (por 90)",
         kpis = c(season_assists="season_assists", season_assists_90="player_season_assists_90",
                  xa_90="player_season_xa_90"),
         x="season_assists_90", y="xa_90", arrange_by="season_assists"),
    list(id="cent_aereo",  title="Mejor Juego Aéreo", subtitle="Cada punto es un jugador",
         xlab="% de Aéreos Ganados", ylab="Aéreos Ganados (por 90)",
         kpis = c(aerial_ratio="player_season_aerial_ratio", aerial_wins="player_season_aerial_wins_90"),
         x="aerial_ratio", y="aerial_wins", arrange_by="aerial_wins"),
    list(id="cent_ag",     title="Agresividad", subtitle="Cada punto es un jugador",
         xlab="Acciones Agresivas (por 90)", ylab="Entradas+Intercepciones (por 90)",
         kpis = c(ag_ac="player_season_aggressive_actions_90",
                  tack_int_90="player_season_tackles_and_interceptions_90"),
         x="ag_ac", y="tack_int_90", arrange_by="tack_int_90"),
    list(id="cent_peligro",title="OBV vs Tiros+Pases Clave", subtitle="Cada punto es un jugador",
         xlab="OBV (por 90)", ylab="Tiros y Pases Clave (por 90)",
         kpis = c(obv_90="player_season_obv_90", shots_key_passes="player_season_shots_key_passes_90"),
         x="obv_90", y="shots_key_passes", arrange_by="shots_key_passes"),
    list(id="cent_pases",  title="Pases Frontales vs LBP", subtitle="Cada punto es un jugador",
         xlab="% Pases al Frente", ylab="LBP (por 90)",
         kpis = c(fwd_pases="player_season_forward_pass_proportion",
                  lbp_90="player_season_lbp_90"),
         x="fwd_pases", y="lbp_90", arrange_by="fwd_pases"),
    list(id="cent_outcomes",title="Positive Outcomes", subtitle="Cada punto es un jugador",
         xlab="Participación (por 90)", ylab="Frecuencia de participación",
         kpis = c(outcome_90="player_season_positive_outcome_90",
                  outcome_score="player_season_positive_outcome_score"),
         x="outcome_90", y="outcome_score", arrange_by="outcome_score")
  ),
  "Lateral/Carrilero" = list(
    list(id="lat_goles",   title="Goles vs xG (sin penales)", subtitle="Cada punto es un jugador",
         xlab="Goles (por 90)", ylab="xG sin penales (por 90)",
         kpis = c(season_goals="season_goals", season_goals_90="player_season_goals_90",
                  np_xg_90="player_season_np_xg_90"),
         x="season_goals_90", y="np_xg_90", arrange_by="season_goals"),
    list(id="lat_asist",   title="Mejores Asistidores", subtitle="Cada punto es un jugador",
         xlab="Asistencias (por 90)", ylab="xA (por 90)",
         kpis = c(season_assists="season_assists", season_assists_90="player_season_assists_90",
                  xa_90="player_season_xa_90"),
         x="season_assists_90", y="xa_90", arrange_by="season_assists"),
    list(id="lat_ag",      title="Agresividad", subtitle="Cada punto es un jugador",
         xlab="Acciones Agresivas (por 90)", ylab="Entradas+Intercepciones (por 90)",
         kpis = c(ag_ac="player_season_aggressive_actions_90",
                  tack_int_90="player_season_tackles_and_interceptions_90"),
         x="ag_ac", y="tack_int_90", arrange_by="tack_int_90"),
    list(id="lat_peligro", title="OBV vs Tiros+Pases Clave", subtitle="Cada punto es un jugador",
         xlab="OBV (por 90)", ylab="Tiros y Pases Clave (por 90)",
         kpis = c(obv_90="player_season_obv_90", shots_key_passes="player_season_shots_key_passes_90"),
         x="obv_90", y="shots_key_passes", arrange_by="shots_key_passes"),
    list(id="lat_pases",   title="Pases Frontales vs LBP", subtitle="Cada punto es un jugador",
         xlab="% Pases al Frente", ylab="LBP (por 90)",
         kpis = c(fwd_pases="player_season_forward_pass_proportion",
                  lbp_90="player_season_lbp_90"),
         x="fwd_pases", y="lbp_90", arrange_by="fwd_pases"),
    list(id="lat_centros", title="Centros vs Pases al Área", subtitle="Cada punto es un jugador",
         xlab="Pases al Área (por 90)", ylab="% al área que son centros",
         kpis = c(pases_area="player_season_passes_into_box_90",
                  cross_ratio="player_season_box_cross_ratio"),
         x="pases_area", y="cross_ratio", arrange_by="cross_ratio"),
    list(id="lat_outcomes",title="Positive Outcomes", subtitle="Cada punto es un jugador",
         xlab="Participación (por 90)", ylab="Frecuencia de participación",
         kpis = c(outcome_90="player_season_positive_outcome_90",
                  outcome_score="player_season_positive_outcome_score"),
         x="outcome_90", y="outcome_score", arrange_by="outcome_score")
  ),
  "Medio de Contención" = list(
    list(id="mc_goles",    title="Goles vs xG (sin penales)", subtitle="Cada punto es un jugador",
         xlab="Goles (por 90)", ylab="xG sin penales (por 90)",
         kpis = c(season_goals="season_goals", season_goals_90="player_season_goals_90",
                  np_xg_90="player_season_np_xg_90"),
         x="season_goals_90", y="np_xg_90", arrange_by="season_goals"),
    list(id="mc_asist",    title="Mejores Asistidores", subtitle="Cada punto es un jugador",
         xlab="Asistencias (por 90)", ylab="xA (por 90)",
         kpis = c(season_assists="season_assists", season_assists_90="player_season_assists_90",
                  xa_90="player_season_xa_90"),
         x="season_assists_90", y="xa_90", arrange_by="season_assists"),
    list(id="mc_aereo",    title="Mejor Juego Aéreo", subtitle="Cada punto es un jugador",
         xlab="% de Aéreos Ganados", ylab="Aéreos Ganados (por 90)",
         kpis = c(aerial_ratio="player_season_aerial_ratio", aerial_wins="player_season_aerial_wins_90"),
         x="aerial_ratio", y="aerial_wins", arrange_by="aerial_wins"),
    list(id="mc_ag",       title="Agresividad", subtitle="Cada punto es un jugador",
         xlab="Acciones Agresivas (por 90)", ylab="Entradas+Intercepciones (por 90)",
         kpis = c(ag_ac="player_season_aggressive_actions_90",
                  tack_int_90="player_season_tackles_and_interceptions_90"),
         x="ag_ac", y="tack_int_90", arrange_by="tack_int_90"),
    list(id="mc_peligro",  title="OBV vs Tiros+Pases Clave", subtitle="Cada punto es un jugador",
         xlab="OBV (por 90)", ylab="Tiros y Pases Clave (por 90)",
         kpis = c(obv_90="player_season_obv_90", shots_key_passes="player_season_shots_key_passes_90"),
         x="obv_90", y="shots_key_passes", arrange_by="shots_key_passes"),
    list(id="mc_pases",    title="Pases Frontales vs LBP", subtitle="Cada punto es un jugador",
         xlab="% Pases al Frente", ylab="LBP (por 90)",
         kpis = c(fwd_pases="player_season_forward_pass_proportion",
                  lbp_90="player_season_lbp_90"),
         x="fwd_pases", y="lbp_90", arrange_by="fwd_pases"),
    list(id="mc_carry",    title="Deep Progressions vs Carry Ratio", subtitle="Cada punto es un jugador",
         xlab="Pases/Acarreos al Tercio Ofensivo (por 90)", ylab="% de Acarreos Exitosos",
         kpis = c(deep_prog_90="player_season_deep_progressions_90",
                  carry_ratio="player_season_carry_ratio"),
         x="deep_prog_90", y="carry_ratio", arrange_by="deep_prog_90"),
    list(id="mc_outcomes", title="Positive Outcomes", subtitle="Cada punto es un jugador",
         xlab="Participación (por 90)", ylab="Frecuencia de participación",
         kpis = c(outcome_90="player_season_positive_outcome_90",
                  outcome_score="player_season_positive_outcome_score"),
         x="outcome_90", y="outcome_score", arrange_by="outcome_score")
  ),
  "Interior/Mediapunta" = list(
    list(id="int_goles",   title="Goles vs xG (sin penales)", subtitle="Cada punto es un jugador",
         xlab="Goles (por 90)", ylab="xG sin penales (por 90)",
         kpis = c(season_goals="season_goals", season_goals_90="player_season_goals_90",
                  np_xg_90="player_season_np_xg_90"),
         x="season_goals_90", y="np_xg_90", arrange_by="season_goals"),
    list(id="int_asist",   title="Mejores Asistidores", subtitle="Cada punto es un jugador",
         xlab="Asistencias (por 90)", ylab="xA (por 90)",
         kpis = c(season_assists="season_assists", season_assists_90="player_season_assists_90",
                  xa_90="player_season_xa_90"),
         x="season_assists_90", y="xa_90", arrange_by="season_assists"),
    list(id="int_peligro", title="OBV vs Tiros+Pases Clave", subtitle="Cada punto es un jugador",
         xlab="OBV (por 90)", ylab="Tiros y Pases Clave (por 90)",
         kpis = c(obv_90="player_season_obv_90", shots_key_passes="player_season_shots_key_passes_90"),
         x="obv_90", y="shots_key_passes", arrange_by="shots_key_passes"),
    list(id="int_pases",   title="Pases Frontales vs LBP", subtitle="Cada punto es un jugador",
         xlab="% Pases al Frente", ylab="LBP (por 90)",
         kpis = c(fwd_pases="player_season_forward_pass_proportion",
                  lbp_90="player_season_lbp_90"),
         x="fwd_pases", y="lbp_90", arrange_by="fwd_pases"),
    list(id="int_pases2",  title="Pases al Área vs Pases Filtrados", subtitle="Cada punto es un jugador",
         xlab="Pases al Área (por 90)", ylab="Pases Filtrados (por 90)",
         kpis = c(pases_area_90="player_season_passes_into_box_90",
                  through_balls_90="player_season_through_balls_90"),
         x="pases_area_90", y="through_balls_90", arrange_by="pases_area_90"),
    list(id="int_drib",    title="Dribles", subtitle="Cada punto es un jugador",
         xlab="Dribles Exitosos (por 90)", ylab="% de Dribles Exitosos",
         kpis = c(dribbles_90="player_season_dribbles_90",
                  dribble_ratio="player_season_dribble_ratio"),
         x="dribbles_90", y="dribble_ratio", arrange_by="dribble_ratio"),
    list(id="int_def",     title="Acciones Defensivas vs Presiones", subtitle="Cada punto es un jugador",
         xlab="Acciones Defensivas (por 90)", ylab="Presiones (por 90)",
         kpis = c(def_actions_90="player_season_defensive_actions_90",
                  press_90="player_season_pressures_90"),
         x="def_actions_90", y="press_90", arrange_by="def_actions_90"),
    list(id="int_outcomes",title="Positive Outcomes", subtitle="Cada punto es un jugador",
         xlab="Participación (por 90)", ylab="Frecuencia de participación",
         kpis = c(outcome_90="player_season_positive_outcome_90",
                  outcome_score="player_season_positive_outcome_score"),
         x="outcome_90", y="outcome_score", arrange_by="outcome_score")
  ),
  "Volante/Extremo" = list(
    list(id="vol_goles",   title="Goles vs xG (sin penales)", subtitle="Cada punto es un jugador",
         xlab="Goles (por 90)", ylab="xG sin penales (por 90)",
         kpis = c(season_goals="season_goals", season_goals_90="player_season_goals_90",
                  np_xg_90="player_season_np_xg_90"),
         x="season_goals_90", y="np_xg_90", arrange_by="season_goals"),
    list(id="vol_asist",   title="Mejores Asistidores", subtitle="Cada punto es un jugador",
         xlab="Asistencias (por 90)", ylab="xA (por 90)",
         kpis = c(season_assists="season_assists", season_assists_90="player_season_assists_90",
                  xa_90="player_season_xa_90"),
         x="season_assists_90", y="xa_90", arrange_by="season_assists"),
    list(id="vol_peligro", title="OBV vs Tiros+Pases Clave", subtitle="Cada punto es un jugador",
         xlab="OBV (por 90)", ylab="Tiros y Pases Clave (por 90)",
         kpis = c(obv_90="player_season_obv_90", shots_key_passes="player_season_shots_key_passes_90"),
         x="obv_90", y="shots_key_passes", arrange_by="shots_key_passes"),
    list(id="vol_centros1",title="Pases al Área vs Centros", subtitle="Cada punto es un jugador",
         xlab="Pases al Área (por 90)", ylab="% al área que son centros",
         kpis = c(pases_area="player_season_passes_into_box_90",
                  cross_ratio="player_season_box_cross_ratio"),
         x="pases_area", y="cross_ratio", arrange_by="cross_ratio"),
    list(id="vol_centros2",title="Centros Tirados vs % Exitosos", subtitle="Cada punto es un jugador",
         xlab="Centros (por 90)", ylab="% de Centros Exitosos",
         kpis = c(cross_90="player_season_crosses_90",
                  cross_ratio="player_season_crossing_ratio"),
         x="cross_90", y="cross_ratio", arrange_by="cross_90"),
    list(id="vol_drib",    title="Dribles", subtitle="Cada punto es un jugador",
         xlab="Dribles Exitosos (por 90)", ylab="% de Dribles Exitosos",
         kpis = c(dribbles_90="player_season_dribbles_90",
                  dribble_ratio="player_season_dribble_ratio"),
         x="dribbles_90", y="dribble_ratio", arrange_by="dribble_ratio"),
    list(id="vol_def",     title="Acciones Defensivas vs Presiones", subtitle="Cada punto es un jugador",
         xlab="Acciones Defensivas (por 90)", ylab="Presiones (por 90)",
         kpis = c(def_actions_90="player_season_defensive_actions_90",
                  press_90="player_season_pressures_90"),
         x="def_actions_90", y="press_90", arrange_by="def_actions_90"),
    list(id="vol_outcomes",title="Positive Outcomes", subtitle="Cada punto es un jugador",
         xlab="Participación (por 90)", ylab="Frecuencia de participación",
         kpis = c(outcome_90="player_season_positive_outcome_90",
                  outcome_score="player_season_positive_outcome_score"),
         x="outcome_90", y="outcome_score", arrange_by="outcome_score")
  ),
  "Portero" = list(
    list(id="por_goles",   title="Goles Recibidos vs xG en Contra", subtitle="Cada punto es un jugador",
         xlab="Goles Recibidos (por 90)", ylab="xG en Contra (por 90)",
         kpis = c(goles_recibidos="player_season_goals_faced_90",
                  xg_en_contra="player_season_np_xg_faced_90"),
         x="goles_recibidos", y="xg_en_contra", arrange_by="goles_recibidos"),
    list(id="por_save",    title="Save Ratio vs xS Ratio", subtitle="Cada punto es un jugador",
         xlab="% de Tiros Parados", ylab="Expectativa de Tiros Parados",
         kpis = c(save_ratio="player_season_save_ratio",
                  xs_ratio="player_season_xs_ratio"),
         x="save_ratio", y="xs_ratio", arrange_by="save_ratio"),
    list(id="por_gsaa",    title="GSAA", subtitle="Cada punto es un jugador",
         xlab="GSAA (por 90)", ylab="% GSAA sobre la media",
         kpis = c(gsaa="player_season_gsaa_90",
                  gsaa_ratio="player_season_gsaa_ratio"),
         x="gsaa", y="gsaa_ratio", arrange_by="gsaa"),
    list(id="por_pases",   title="Pases Largos", subtitle="Cada punto es un jugador",
         xlab="Pases Largos (por 90)", ylab="% Pases Largos Completos",
         kpis = c(lg="player_season_long_balls_90",
                  lg_ratio="player_season_long_ball_ratio"),
         x="lg", y="lg_ratio", arrange_by="lg_ratio"),
    list(id="por_def",     title="Acciones Defensivas (salidas)", subtitle="Cada punto es un jugador",
         xlab="CLCAA% (salidas sobre la media)", ylab="Distancia agresiva (m)",
         kpis = c(clcaa="player_season_clcaa",
                  da_ag="player_season_da_aggressive_distance"),
         x="clcaa", y="da_ag", arrange_by="clcaa")
  )
)

# ============================================================
# GENERAL HELPERS (unchanged)
# ============================================================
`%||%` <- function(x, y) if (is.null(x)) y else x

get_league_df <- function(scout_list, league_label) {
  nm <- league_map[[league_label]]
  if (is.null(nm) || is.null(scout_list[[nm]])) {
    validate(need(FALSE, paste("League not found in catalog:", league_label)))
  }
  df <- scout_list[[nm]]
  if ("player_id"    %in% names(df)) df$player_id    <- as.character(df$player_id)
  if ("sc_player_id" %in% names(df)) df$sc_player_id <- as.character(df$sc_player_id)
  df
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "", formatC(as.numeric(x), digits = digits, format = "f"))
}

compute_age_years <- function(birth_date_chr) {
  bd  <- suppressWarnings(as.Date(birth_date_chr))
  age <- floor(as.numeric(difftime(Sys.Date(), bd, units = "days")) / 365.25)
  age[is.na(age)] <- 10^6
  age
}

apply_scatter_filters <- function(df, minutes_range = c(0, Inf), age_range = c(0, 100)) {
  df |>
    mutate(
      .age_years = compute_age_years(birth_date),
      .mins      = suppressWarnings(as.numeric(player_season_minutes))
    ) |>
    filter(
      is.na(.mins) | dplyr::between(.mins, minutes_range[1], minutes_range[2]),
      dplyr::between(.age_years, age_range[1], age_range[2])
    )
}

summarise_kpis <- function(df, pg, kpis_named_vec) {
  sy <- syms(unname(kpis_named_vec))
  names(sy) <- names(kpis_named_vec)
  df |>
    summarise(!!!sy,
              .by = c("player_name","team_name","primary_position","position_group","player_season_minutes")) |>
    filter(position_group == pg)
}

var_map <- function(df_pg) {
  drop_cols <- c(
    "player_name","team_name","position_group","primary_position",
    "birth_date","league","season","country","competition",
    "player_season_minutes","player_season_90s_played",
    ".league_label",".league_key",
    "sc_player_id","match_type"
  )
  num_cols <- names(df_pg)[vapply(df_pg, is.numeric, TRUE)]
  keep     <- setdiff(num_cols, drop_cols)
  if (!length(keep)) return(setNames(character(0), character(0)))
  display <- sub("^player_season_", "", keep)
  setNames(keep, display)
}

make_scatter <- function(dat, x, y, title, subtitle, xlab, ylab, src, selected = NULL) {
  if (is.null(dat) || nrow(dat) == 0) {
    p <- ggplot() +
      annotate("text", x = 0, y = 0, label = "Sin datos para los filtros actuales") +
      theme_void()
    return(ggplotly(p))
  }
  dat <- dat |>
    mutate(
      player_name = as.character(player_name),
      team_name   = as.character(team_name),
      .hover = sprintf(
        "<b>%s</b><br>Equipo: %s<br>%s: %s<br>%s: %s",
        player_name, team_name,
        as.character(xlab), round(.data[[x]], 3),
        as.character(ylab), round(.data[[y]], 3)
      ),
      .is_sel = if (!is.null(selected)) player_name == selected else FALSE
    )
  
  others      <- dat |> filter(!.is_sel)
  selected_df <- dat |> filter(.is_sel)
  
  p <- suppressWarnings({
    ggplot() +
      { if (nrow(others) > 0)
        geom_point(data=others,
                   aes(x=.data[[x]], y=.data[[y]], text=.hover, key=player_name),
                   size=2.6, alpha=0.25, color="grey50") } +
      { if (nrow(selected_df) > 0)
        geom_point(data=selected_df,
                   aes(x=.data[[x]], y=.data[[y]], text=.hover, key=player_name),
                   size=4.0, alpha=1, color="blue3") } +
      geom_abline(slope=1, intercept=0, linetype="dashed", linewidth=0.5, color="grey45") +
      scale_x_continuous(expand=expansion(mult=c(0.02, 0.05))) +
      scale_y_continuous(expand=expansion(mult=c(0.02, 0.05))) +
      labs(x=as.character(xlab), y=as.character(ylab),
           title=as.character(title), subtitle=as.character(subtitle)) +
      theme_minimal(base_size=12) +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_line(linewidth=0.2, color="grey85"),
            axis.title=element_text(face="bold"),
            plot.title=element_text(face="bold"),
            plot.margin=margin(10,12,10,12))
  })
  ggplotly(p, tooltip="text", source=as.character(src)) |>
    layout(hoverlabel=list(align="left"))
}

ends_with_goles <- function(id) grepl("_goles$", id)

make_top_goals_bar <- function(dat) {
  dat_bar <- dat |>
    arrange(desc(.data[["season_goals"]])) |>
    slice_head(n = 10) |>
    mutate(player_name = forcats::fct_reorder(player_name, season_goals))
  
  p <- ggplot(dat_bar,
              aes(x=season_goals, y=player_name,
                  text=paste0("<b>",player_name,"</b><br>Equipo: ",team_name,
                              "<br>Goles esta temporada: ",season_goals))) +
    geom_col(fill="blue3") +
    scale_x_continuous(expand=expansion(mult=c(0, 0.05))) +
    labs(x="Goles esta temporada", y=NULL,
         title="Top 10 goleadores", subtitle="Cada barra representa a un jugador") +
    theme_minimal(base_size=12) +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title=element_text(face="bold"),
          plot.title=element_text(face="bold"),
          plot.margin=margin(10,12,10,12))
  ggplotly(p, tooltip="text") |> layout(hoverlabel=list(align="left"))
}

nice_metric_label <- function(x) {
  rename_map <- c(
    "goals_90"="Goles/90","xg_90"="xG/90","shots_90"="Tiros/90",
    "shot_obv"="OBV Tiro","pass_obv"="OBV Pase","op_xga"="OP xG Asistido",
    "padj_pressures"="Presiones (PAdj)","padj_pressures_90"="Presiones (PAdj)/90",
    "d_c_obv_90"="OBV Drible \n+ Conducción","tib_90"="Toques en Área/90",
    "xg_per_shot"="xG por Tiro","turnovers_90"="Pérdidas/90",
    "fouls_90"="Faltas Cometidas/90","fouls_won_90"="Faltas Recibidas/90",
    "aerial_wins_90"="Duelos Aéreos\nGanados/90","aerial_ratio"="% Duelos Aéreos \nGanados",
    "padj_ti_90"="Entradas + \nIntercepciones \n(PAdj)/90",
    "deep_progressions_90"="Progresiones \nProfundas/90",
    "da_obv"="OBV Acciones \nDefensivas","challenge_ratio"="% Duelos Ganados",
    "obv_90"="OBV Total","def_actions_90"="Acciones Defensivas/90",
    "press_90"="Presiones \n(PAdj)/90","cross_90"="Centros/90",
    "cross_ratio"="% Centros Exitosos","pases_area"="Pases al Área/90",
    "blocks_per_shot"="Bloqueos por Tiro",
    "change_in_passing_ratio"="% de pase bajo presión\nvs % de pase",
    "long_ball_ratio"="Efectividad de\n Pases Largos",
    "clcaa"="CLCAA%","da_aggressive_distance"="Distancia Promedio \nde Acción Defensiva",
    "np_optimal_gk_dlength"="Efectividad de \nPosicionamiento",
    "save_ratio"="% de Paradas",
    "pass_into_danger_ratio"="% de pases que sus compañeros \nreciben bajo presión",
    "obv_gk_90"="OBV/90","padj_clearances_90"="Despejes (PAdj)/90",
    "dribbles_90"="Dribles Exitosos/90","dribble_ratio"="% de Efectividad \nde Dribles"
  )
  sapply(x, function(nm) if (nm %in% names(rename_map)) rename_map[[nm]] else
    tools::toTitleCase(gsub("_", " ", nm)), USE.NAMES=FALSE)
}

position_metric_map <- function(pg) {
  switch(pg,
         "Portero" = c(
           gk_obv_90="player_season_obv_gk_90",
           long_ball_ratio="player_season_long_ball_ratio",
           pass_danger_ratio="player_season_pass_into_danger_ratio",
           clcaa="player_season_clcaa",
           gk_agg_dist="player_season_da_aggressive_distance",
           positioning_error="player_season_np_optimal_gk_dlength",
           save_ratio="player_season_save_ratio"
         ),
         "Delantero" = c(
           goals_90="player_season_goals_90",
           xg_90="player_season_np_xg_90",
           shots_90="player_season_np_shots_90",
           shot_obv="player_season_obv_shot_90",
           pass_obv="player_season_obv_pass_90",
           op_xga="player_season_op_xa_90",
           padj_pressures="player_season_padj_pressures_90",
           aerial_wins="player_season_aerial_wins_90",
           turnovers_90="player_season_turnovers_90",
           d_c_obv_90="player_season_obv_dribble_carry_90",
           tib_90="player_season_touches_inside_box_90",
           xg_per_shot="player_season_np_xg_per_shot"
         ),
         "Central" = c(
           goals_90="player_season_goals_90",
           pressure_pass_ratio="player_season_change_in_passing_ratio",
           obv_90="player_season_obv_90",
           pass_obv="player_season_obv_pass_90",
           d_c_obv_90="player_season_obv_dribble_carry_90",
           blocks_per_shot="player_season_blocks_per_shot",
           aerial_wins_90="player_season_aerial_wins_90",
           aerial_ratio="player_season_aerial_ratio",
           padj_clearances_90="player_season_padj_clearances_90",
           fouls_90="player_season_fouls_90",
           challenge_ratio="player_season_challenge_ratio",
           da_obv="player_season_obv_defensive_action_90"
         ),
         "Lateral/Carrilero" = c(
           goals_90="player_season_goals_90",
           padj_ti_90="player_season_tackles_and_interceptions_90",
           deep_progressions_90="player_season_deep_progressions_90",
           pass_obv="player_season_obv_pass_90",
           op_xga="player_season_op_xa_90",
           d_c_obv_90="player_season_obv_dribble_carry_90",
           turnovers_90="player_season_turnovers_90",
           aerial_ratio="player_season_aerial_ratio",
           padj_pressures_90="player_season_padj_pressures_90",
           fouls_90="player_season_fouls_90",
           challenge_ratio="player_season_challenge_ratio",
           da_obv="player_season_obv_defensive_action_90"
         ),
         "Medio de Contención" = c(
           goals_90="player_season_goals_90",
           padj_ti_90="player_season_tackles_and_interceptions_90",
           deep_progressions_90="player_season_deep_progressions_90",
           obv_90="player_season_obv_90",
           pass_obv="player_season_obv_pass_90",
           op_xga="player_season_op_xa_90",
           d_c_obv_90="player_season_obv_dribble_carry_90",
           turnovers_90="player_season_turnovers_90",
           padj_pressures_90="player_season_padj_pressures_90",
           fouls_won_90="player_season_fouls_won_90",
           challenge_ratio="player_season_challenge_ratio",
           da_obv="player_season_obv_defensive_action_90"
         ),
         "Interior/Mediapunta" = c(
           goals_90="player_season_goals_90",
           xg_90="player_season_np_xg_90",
           shots_90="player_season_np_shots_90",
           shot_obv="player_season_obv_shot_90",
           pass_obv="player_season_obv_pass_90",
           op_xga="player_season_op_xa_90",
           padj_pressures="player_season_padj_pressures_90",
           fouls_won_90="player_season_fouls_won_90",
           turnovers_90="player_season_turnovers_90",
           d_c_obv_90="player_season_obv_dribble_carry_90",
           tib_90="player_season_touches_inside_box_90",
           xg_per_shot="player_season_np_xg_per_shot"
         ),
         "Volante/Extremo" = c(
           goals_90="player_season_goals_90",
           xg_90="player_season_np_xg_90",
           shot_obv="player_season_obv_shot_90",
           pass_obv="player_season_obv_pass_90",
           op_xga="player_season_op_xa_90",
           dribbles_90="player_season_dribbles_90",
           dribble_ratio="player_season_dribble_ratio",
           def_actions_90="player_season_defensive_actions_90",
           press_90="player_season_pressures_90",
           cross_90="player_season_crosses_90",
           cross_ratio="player_season_crossing_ratio",
           pases_area="player_season_passes_into_box_90",
           shots_90="player_season_np_shots_90",
           d_c_obv_90="player_season_obv_dribble_carry_90"
         ),
         character(0)
  )
}

label_map <- list(
  "Portero" = c(
    gk_obv_90="OBV (GK)", long_ball_ratio="% Pases Largos Completos",
    pass_danger_ratio="% Pases a Zona Peligro", clcaa="CLCAA%",
    gk_agg_dist="Distancia agresiva (m)", positioning_error="Error de Posicionamiento",
    save_ratio="% Tiros Parados"
  ),
  "Delantero" = c(
    goals_90="Goles/90", xg_90="xG/90", shots_90="Tiros/90",
    shot_obv="OBV Tiro", pass_obv="OBV Pase", op_xga="OP xG Asistido",
    padj_pressures="Presiones (PAdj)", aerial_wins="Aéreos Ganados/90",
    turnovers_90="Pérdidas/90", d_c_obv_90="OBV Drible+Conducción",
    tib_90="Toques en Área", xg_per_shot="xG/Tiro"
  ),
  "Central" = c(
    goals_90="Goles/90", pressure_pass_ratio="Cambio Ratio Pase Bajo Presión",
    obv_90="OBV/90", pass_obv="OBV Pase", d_c_obv_90="OBV Drible+Conducción",
    blocks_per_shot="Bloqueos por Tiro", aerial_wins_90="Aéreos Ganados/90",
    aerial_ratio="% Aéreos Ganados", padj_clearances_90="Despejes (PAdj)",
    fouls_90="Faltas/90", challenge_ratio="% Duelo Ganado", da_obv="OBV Acción Defensiva"
  ),
  "Lateral/Carrilero" = c(
    goals_90="Goles/90", padj_ti_90="Entradas+Intercepciones (PAdj)",
    deep_progressions_90="Progresiones Profundas/90", pass_obv="OBV Pase",
    op_xga="OP xG Asistido", d_c_obv_90="OBV Drible+Conducción",
    turnovers_90="Pérdidas/90", aerial_ratio="% Aéreos Ganados",
    padj_pressures_90="Presiones (PAdj)", fouls_90="Faltas/90",
    challenge_ratio="% Duelo Ganado", da_obv="OBV Acciónes Defensiva"
  ),
  "Medio de Contención" = c(
    goals_90="Goles/90", padj_ti_90="Entradas+Intercepciones (PAdj)",
    deep_progressions_90="Progresiones Profundas/90", obv_90="OBV/90",
    pass_obv="OBV Pase", op_xga="OP xG Asistido", d_c_obv_90="OBV Drible+Conducción",
    turnovers_90="Pérdidas/90", padj_pressures_90="Presiones (PAdj)/90",
    fouls_won_90="Faltas Recibidas/90", challenge_ratio="% Duelo Ganado",
    da_obv="OBV Acción Defensiva"
  ),
  "Interior/Mediapunta" = c(
    goals_90="Goles/90", xg_90="xG/90", shots_90="Tiros/90",
    shot_obv="OBV Tiro", pass_obv="OBV Pase", op_xga="OP xG Asistido",
    padj_pressures="Presiones (PAdj)", fouls_won_90="Faltas Recibidas/90",
    turnovers_90="Pérdidas/90", d_c_obv_90="OBV Drible+Conducción",
    tib_90="Toques en Área", xg_per_shot="xG/Tiro"
  ),
  "Volante/Extremo" = c(
    goals_90="Goles/90", xg_90="xG/90", shot_obv="OBV Tiro",
    pass_obv="OBV Pase", op_xga="OP xG Asistido",
    dribbles_90="Dribles Exitosos/90", dribble_ratio="% Dribles Exitosos",
    def_actions_90="Acciones Defensivas/90", press_90="Presiones/90",
    cross_90="Centros/90", cross_ratio="% Centros Exitosos",
    pases_area="Pases al Área/90", shots_90="Tiros/90",
    d_c_obv_90="OBV Drible+Conducción"
  )
)

build_position_stat_rows_with_percentiles <- function(row, dat) {
  pg <- row$position_group[1]
  req(pg %in% names(label_map))
  labels  <- label_map[[pg]]
  col_map <- position_metric_map(pg)
  df_pos  <- dat |> dplyr::filter(position_group == pg)
  
  rows <- lapply(names(labels), function(alias) {
    real_col <- if (alias %in% names(df_pos)) alias else col_map[[alias]]
    if (is.null(real_col) || !real_col %in% names(df_pos)) return(NULL)
    vals <- suppressWarnings(as.numeric(df_pos[[real_col]]))
    val  <- suppressWarnings(as.numeric(row[[real_col]]))
    if (is.na(val)) return(NULL)
    perc <- if (all(is.na(vals))) NA_real_ else 100 * ecdf(vals)(val)
    data.frame(Métrica=labels[[alias]], Valor=round(val,3), Percentil=round(perc,1),
               stringsAsFactors=FALSE)
  })
  
  out <- Filter(Negate(is.null), rows)
  if (!length(out))
    return(data.frame(Métrica=character(), Valor=character(), Percentil=character(),
                      stringsAsFactors=FALSE))
  do.call(rbind, out)
}

# ============================================================
# SKILLCORNER CONFIG
# ============================================================

# All physical variables (available in all 13 SC leagues)
physical_vars <- c(
  "psv99",
  "total_distance_per_60_bip",
  "running_distance_per_60_bip",
  "hsr_distance_per_60_bip",
  "sprint_distance_per_60_bip",
  "sprint_count_per_60_bip",
  "hsr_count_per_60_bip",
  "highaccel_count_per_60_bip",
  "highdecel_count_per_60_bip",
  "meters_per_minute_tip",
  "meters_per_minute_otip"
)

# Game Intelligence variables (Liga MX only)
gi_vars <- c(
  "count_opportunities_to_pass_to_runs_per_30_min_tip",
  "count_pass_attempts_to_runs_per_30_min_tip",
  "runs_to_which_pass_attempted_threat_per_30_min_tip",
  "runs_to_which_pass_completed_threat_per_30_min_tip",
  "pass_completion_ratio_to_runs",
  "count_completed_pass_to_runs_leading_to_shot_per_30_min_tip",
  "count_completed_pass_to_runs_leading_to_goal_per_30_min_tip",
  "count_pass_attempts_to_dangerous_runs_per_30_min_tip",
  "count_completed_pass_to_dangerous_runs_per_30_min_tip",
  "count_pressures_received_per_30_min_tip_p30tip",
  "count_forced_losses_under_pressure_per_30_min_tip_p30tip",
  "ball_retention_ratio_under_pressure_p30tip",
  "pass_completion_ratio_under_pressure_p30tip",
  "dangerous_pass_completion_ratio_under_pressure_p30tip",
  "count_completed_passes_under_pressure_per_30_min_tip_p30tip",
  "count_completed_dangerous_passes_under_pressure_per_30_min_tip_p30tip",
  "count_dangerous_pass_attempts_under_pressure_per_30_min_tip_p30tip",
  "count_pass_attempts_under_pressure_per_30_min_tip_p30tip",
  "count_dangerous_runs_per_30_tip",
  "count_runs_leading_to_goal_per_30_tip",
  "count_runs_leading_to_shot_per_30_tip",
  "count_dangerous_runs_targeted_per_30_tip",
  "count_dangerous_runs_received_per_30_tip",
  "runs_threat_per_100",
  "runs_target_percentage",
  "dangerous_runs_target_percentage",
  "runs_receive_percentage",
  "dangerous_runs_receive_percentage",
  "runs_leading_to_shot_percentage_all_runs",
  "runs_leading_to_goal_percentage_all_runs",
  "runs_dangerous_percentage"
)

# GI run vars excluded for Centrales (indices 19–31 of gi_vars)
gi_run_vars_centrales <- gi_vars[19:31]

# GI variable groupings for tabbed SC table display
gi_vars_pases <- c(
  "count_opportunities_to_pass_to_runs_per_30_min_tip",
  "count_pass_attempts_to_runs_per_30_min_tip",
  "runs_to_which_pass_attempted_threat_per_30_min_tip",
  "runs_to_which_pass_completed_threat_per_30_min_tip",
  "pass_completion_ratio_to_runs",
  "count_completed_pass_to_runs_leading_to_shot_per_30_min_tip",
  "count_completed_pass_to_runs_leading_to_goal_per_30_min_tip",
  "count_pass_attempts_to_dangerous_runs_per_30_min_tip",
  "count_completed_pass_to_dangerous_runs_per_30_min_tip",
  "count_pressures_received_per_30_min_tip_p30tip",
  "count_forced_losses_under_pressure_per_30_min_tip_p30tip",
  "ball_retention_ratio_under_pressure_p30tip",
  "pass_completion_ratio_under_pressure_p30tip",
  "dangerous_pass_completion_ratio_under_pressure_p30tip",
  "count_completed_passes_under_pressure_per_30_min_tip_p30tip",
  "count_completed_dangerous_passes_under_pressure_per_30_min_tip_p30tip",
  "count_dangerous_pass_attempts_under_pressure_per_30_min_tip_p30tip",
  "count_pass_attempts_under_pressure_per_30_min_tip_p30tip"
)

# gi_vars_presiones <- c(
#   "count_pressures_received_per_30_min_tip_p30tip",
#   "count_forced_losses_under_pressure_per_30_min_tip_p30tip",
#   "ball_retention_ratio_under_pressure_p30tip",
#   "pass_completion_ratio_under_pressure_p30tip",
#   "dangerous_pass_completion_ratio_under_pressure_p30tip",
#   "count_completed_passes_under_pressure_per_30_min_tip_p30tip",
#   "count_completed_dangerous_passes_under_pressure_per_30_min_tip_p30tip",
#   "count_dangerous_pass_attempts_under_pressure_per_30_min_tip_p30tip",
#   "count_pass_attempts_under_pressure_per_30_min_tip_p30tip"
# )

gi_vars_desmarques <- c(
  "count_dangerous_runs_per_30_tip",
  "count_runs_leading_to_goal_per_30_tip",
  "count_runs_leading_to_shot_per_30_tip",
  "count_dangerous_runs_targeted_per_30_tip",
  "count_dangerous_runs_received_per_30_tip",
  "runs_threat_per_100",
  "runs_target_percentage",
  "dangerous_runs_target_percentage",
  "runs_receive_percentage",
  "dangerous_runs_receive_percentage",
  "runs_leading_to_shot_percentage_all_runs",
  "runs_leading_to_goal_percentage_all_runs",
  "runs_dangerous_percentage"
)

# ---- SC helper functions ----

# Try to find a metric column on df, with or without "sc_" prefix
resolve_sc_col <- function(df, metric_code) {
  if (metric_code %in% names(df)) return(metric_code)
  with_prefix <- paste0("sc_", metric_code)
  if (with_prefix %in% names(df)) return(with_prefix)
  NA_character_
}

safe_percentile <- function(v, x) {
  v <- suppressWarnings(as.numeric(v))
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) || all(is.na(v))) return(NA_real_)
  v2 <- v[is.finite(v)]
  if (!length(v2)) return(NA_real_)
  if (diff(range(v2, na.rm=TRUE)) == 0) return(50)
  100 * stats::ecdf(v2)(x)
}

# Map app position_group -> SkillCorner position bucket
sc_bucket_for_pg <- function(app_pg) {
  switch(app_pg,
         "Delantero"          = "Center Forward",
         "Volante/Extremo"    = "Wide Attacker",
         "Interior/Mediapunta"= "Midfield",
         "Medio de Contención"= "Midfield",
         "Lateral/Carrilero"  = "Fullback",
         "Central"            = "Central Defender",
         NULL
  )
}

# Clean up a raw SC column name into a readable Spanish label
clean_sc_col_name <- function(x) {
  # Hardcoded Spanish overrides for physical metrics
  overrides <- c(
    "psv99"                          = "PSV99 (km/h)",
    "total_distance_per_60_bip"      = "Distancia Total (m) /60 BIP",
    "running_distance_per_60_bip"    = "Distancia Running (m) /60 BIP",
    "hsr_distance_per_60_bip"        = "Distancia en HSR (m) /60 BIP",
    "sprint_distance_per_60_bip"     = "Distancia en Sprint (m) /60 BIP",
    "sprint_count_per_60_bip"        = "Acciones de Sprint /60 BIP",
    "hsr_count_per_60_bip"           = "Acciones de HSR /60 BIP",
    "highaccel_count_per_60_bip"     = "Aceleraciones Altas /60 BIP",
    "highdecel_count_per_60_bip"     = "Desaceleraciones Altas /60 BIP",
    "meters_per_minute_tip"          = "Metros por Minuto TIP",
    "meters_per_minute_otip"         = "Metros por Minuto OTIP"
  )
  x_clean <- sub("^sc_", "", x)
  if (x_clean %in% names(overrides)) return(unname(overrides[[x_clean]]))
  
  # Fallback: clean up automatically
  x_clean |>
    stringr::str_replace_all("_per_60_bip$", " /60 BIP") |>
    stringr::str_replace_all("_per_30_min_tip(_p30tip)?$", " /30 TIP") |>
    stringr::str_replace_all("_per_30_tip$", " /30 TIP") |>
    stringr::str_replace_all("_per_100$", " /100") |>
    stringr::str_replace_all("_ratio$", " ratio") |>
    stringr::str_replace_all("_percentage$", " %") |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_squish() |>
    stringr::str_replace_all("\\bhsr\\b", "HSR") |>
    stringr::str_replace_all("\\bbip\\b", "BIP") |>
    stringr::str_replace_all("\\btip\\b", "TIP") |>
    stringr::str_replace_all("\\botip\\b", "OTIP") |>
    stringr::str_replace_all("\\bcount\\b", "#") |>
    tools::toTitleCase()
}

# ============================================================
# SC TABLE BUILDER
# Works entirely from the already-joined player rows.
# all_sc_df  = bind_rows of all joined_leagues, rows with SC data
# liga_mx_df = Liga MX joined data, rows with SC data
# player_row = the selected player's single row from league_df()
# league_label = e.g. "Liga MX"
# ============================================================
build_skillcorner_table <- function(all_sc_df, liga_mx_df,
                                    player_row, league_label) {
  
  pname <- as.character(player_row$player_name[1])
  pg    <- as.character(player_row$position_group[1])
  
  # ---- Porteros not covered by SC ----
  if (identical(pg, "Portero")) {
    return(data.frame(
      Tipo = "–", Métrica = "Datos de Skill Corner no incluyen porteros",
      Valor = "–", Percentil = NA_real_, stringsAsFactors = FALSE
    ))
  }
  
  is_ligamx <- identical(as.character(league_label), "Liga MX")
  
  # Console trace — remove once confirmed working
  message(sprintf("[SC] player=%s | league=%s | is_ligamx=%s | liga_mx_df rows=%d",
                  pname, league_label, is_ligamx, nrow(liga_mx_df)))
  
  # ---- Look up the player's row directly in the SC source data ----
  # Always try liga_mx_df first for Liga MX (has GI cols), fall back to all_sc_df.
  sc_data_row <- NULL
  
  if (is_ligamx && nrow(liga_mx_df) > 0) {
    r <- liga_mx_df |>
      dplyr::filter(player_name == pname) |>
      dplyr::slice_head(n = 1)
    message(sprintf("[SC] liga_mx_df lookup rows matched: %d", nrow(r)))
    if (nrow(r) > 0) sc_data_row <- r
  }
  
  # For non-Liga MX, or if Liga MX lookup failed, try all_sc_df
  if (is.null(sc_data_row) && nrow(all_sc_df) > 0) {
    r <- all_sc_df |>
      dplyr::filter(player_name == pname) |>
      dplyr::slice_head(n = 1)
    message(sprintf("[SC] all_sc_df lookup rows matched: %d", nrow(r)))
    if (nrow(r) > 0) sc_data_row <- r
  }
  
  # If no SC row found anywhere, player has no SC data
  if (is.null(sc_data_row)) {
    message(sprintf("[SC] No SC row found for: %s", pname))
    return(data.frame(
      Tipo = "–",
      Métrica = sprintf("No hay datos de Skill Corner disponibles para %s.", pname),
      Valor = "–", Percentil = NA_real_, stringsAsFactors = FALSE
    ))
  }
  
  # ---- Position bucket for percentile reference pool ----
  sc_bucket <- sc_bucket_for_pg(pg)
  if (is.null(sc_bucket)) {
    return(data.frame(
      Tipo = "–",
      Métrica = sprintf("No hay mapeo de posición SkillCorner para %s.", pname),
      Valor = "–", Percentil = NA_real_, stringsAsFactors = FALSE
    ))
  }
  
  # Detect which column holds the SC position group (try all known variants)
  sc_pos_col <- NA_character_
  for (cn in c("sc_position_group", "position_group_sc", "skillcorner_position_group",
               "sc_pos_group", "skillcorner_pos")) {
    if (cn %in% names(all_sc_df)) { sc_pos_col <- cn; break }
  }
  message(sprintf("[SC] sc_pos_col detected: %s", sc_pos_col))
  
  get_ref_pool <- function(df) {
    if (!is.na(sc_pos_col) && sc_pos_col %in% names(df)) {
      pool <- df |> dplyr::filter(.data[[sc_pos_col]] == sc_bucket)
      message(sprintf("[SC] ref pool via sc_pos_col '%s'='%s': %d rows", sc_pos_col, sc_bucket, nrow(pool)))
      pool
    } else if ("position_group" %in% names(df)) {
      pool <- df |> dplyr::filter(position_group == pg)
      message(sprintf("[SC] ref pool via position_group='%s': %d rows", pg, nrow(pool)))
      pool
    } else {
      # Last resort: use all rows (percentiles across all positions)
      message("[SC] ref pool: no position col found, using all rows")
      df
    }
  }
  
  ref_all <- get_ref_pool(all_sc_df)   # physical percentile reference (13 leagues)
  ref_mx  <- get_ref_pool(liga_mx_df)  # GI percentile reference (Liga MX only)
  
  # GI vars to show: all 31 for non-Centrales, skip run vars (19-31) for Centrales
  gi_use <- if (!is_ligamx) character(0) else
    if (pg == "Central") setdiff(gi_vars, gi_run_vars_centrales) else gi_vars
  
  message(sprintf("[SC] gi_use length: %d | physical_vars: %d", length(gi_use), length(physical_vars)))
  
  # Quick check: how many GI cols exist on sc_data_row?
  gi_found_on_row <- intersect(gi_vars, names(sc_data_row))
  message(sprintf("[SC] GI cols on sc_data_row: %d / %d", length(gi_found_on_row), length(gi_vars)))
  
  out_rows <- list()
  
  # ---- Physical metrics (all SC leagues) ----
  for (m in physical_vars) {
    col <- resolve_sc_col(sc_data_row, m)
    if (is.na(col)) next
    val <- suppressWarnings(as.numeric(sc_data_row[[col]][1]))
    if (!is.finite(val)) next
    
    ref_col <- if (col %in% names(ref_all)) col else NA_character_
    perc    <- if (!is.na(ref_col)) safe_percentile(ref_all[[ref_col]], val) else NA_real_
    
    out_rows[[length(out_rows) + 1]] <- data.frame(
      Tipo = "Físico", Métrica = clean_sc_col_name(m),
      Valor = formatC(val, digits = 3, format = "f"),
      Percentil = round(perc, 1), stringsAsFactors = FALSE
    )
  }
  
  # ---- Game Intelligence metrics (Liga MX only) ----
  for (m in gi_use) {
    col <- resolve_sc_col(sc_data_row, m)
    if (is.na(col)) next
    val <- suppressWarnings(as.numeric(sc_data_row[[col]][1]))
    if (!is.finite(val)) next
    
    ref_col <- if (col %in% names(ref_mx)) col else NA_character_
    perc    <- if (!is.na(ref_col)) safe_percentile(ref_mx[[ref_col]], val) else NA_real_
    
    out_rows[[length(out_rows) + 1]] <- data.frame(
      Tipo = "Game Intelligence", Métrica = clean_sc_col_name(m),
      Valor = formatC(val, digits = 3, format = "f"),
      Percentil = round(perc, 1), stringsAsFactors = FALSE
    )
  }
  
  if (!length(out_rows)) {
    return(data.frame(
      Tipo = "–",
      Métrica = sprintf("No hay datos de Skill Corner disponibles para %s.", pname),
      Valor = "–", Percentil = NA_real_, stringsAsFactors = FALSE
    ))
  }
  
  dplyr::bind_rows(out_rows)
}

# ============================================================
# ALL-LEAGUES HELPERS (for radar + similarity + player search)
# ============================================================
all_players_df_from_cache <- function(scout_list, league_map) {
  dfs <- lapply(names(league_map), function(lbl) {
    key <- league_map[[lbl]]
    df  <- scout_list[[key]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
    # Coerce ID columns to character to prevent type-mismatch errors on bind_rows
    if ("player_id"    %in% names(df)) df$player_id    <- as.character(df$player_id)
    if ("sc_player_id" %in% names(df)) df$sc_player_id <- as.character(df$sc_player_id)
    df$.league_label <- lbl
    df$.league_key   <- key
    df
  })
  dplyr::bind_rows(dfs)
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(tags$style(HTML("
    .dataTables_wrapper { width: 100% !important; }
    table.dataTable       { width: 100% !important; }
    .sc-section-header    { font-size: 1.1em; font-weight: bold;
                            color: #1a5276; margin-top: 8px; }
  "))),
  
  titlePanel("Scouting Dashboard — Club América"),
  
  fluidRow(
    column(2, selectInput("league", "Liga", choices=names(league_map), selected="Liga MX")),
    column(2, selectInput("pg", "Grupo de posición", choices=names(charts_cfg), selected="Delantero")),
    column(2, selectInput("team_filter", "Equipo", choices="Todos los equipos", selected="Todos los equipos")),
    column(6, selectizeInput(
      "player_search", "Buscar jugador", choices=NULL, multiple=FALSE,
      options=list(placeholder="Escribe un nombre…", selectOnTab=TRUE,
                   maxOptions=5000, openOnFocus=TRUE)
    ))
  ),
  
  fluidRow(
    column(2, selectInput("pos_filter", "Posición", choices="Todas", selected="Todas")),
    column(2, selectInput("country_filter", "Nacionalidad", choices="Todas", selected="Todas")),
    column(6,
           sliderInput("min_minutes","Minutos jugados", min=0, max=4000,
                       value=c(0,4000), step=50, width="100%"),
           sliderInput("age_range","Rango de edad", min=15, max=45,
                       value=c(17,45), step=1, width="100%")
    ),
    column(2, "")
  ),
  
  tags$hr(),
  
  fluidRow(
    column(7, uiOutput("tabs_ui")),
    column(5, uiOutput("radar_panel"))
  ),
  
  uiOutput("second_row"),
  
  tags$hr(),
  
  # ---- SkillCorner section ----
  fluidRow(
    column(12,
           tags$h4("SkillCorner — Datos Físicos y de Game Intelligence"),
           tags$p(
             style="color:#555; font-size:0.9em; margin-bottom:4px;",
             "Percentiles vs jugadores de la misma posición. ",
             "Game Intelligence solo disponible para Liga MX. ",
             "Porteros no incluidos en datos SkillCorner."
           ),
           tags$details(
             style="margin-bottom:8px;",
             tags$summary(
               style="cursor:pointer; font-size:0.85em; color:#1a5276; font-weight:600;",
               "Ligas con datos SkillCorner disponibles. (No todas las ligas incluyen todos los equipos ni todos los jugadores)"
             ),
             tags$ul(
               style="margin:4px 0 0 16px; padding:0; font-size:0.85em; color:#333;
                 columns:3; list-style-type:disc;",
               lapply(sort(SC_LEAGUES), function(lg) tags$li(lg))
             )
           ),
           uiOutput("sc_no_data_msg"),
           uiOutput("skillcorner_ui")
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ---- Primary data ----
  league_df <- reactive({
    df <- get_league_df(scout, input$league)
    if ("country_id" %in% names(df)) {
      id_str <- as.character(df$country_id)
      mapped  <- country_id_names[id_str]
      mapped[is.na(mapped)] <- paste0("Otro (", id_str[is.na(mapped)], ")")
      df$player_country <- mapped
    }
    df
  })
  
  # ---- All players across all leagues (for radar, similarity) ----
  all_players_df <- reactive(all_players_df_from_cache(scout, league_map))
  
  # ---- All players enriched with SC physical cols (for SC-based similarity) ----
  # Merges physical SC columns from joined_leagues onto the SB player rows by name.
  # Only players in SC leagues will have SC values; the rest get NA (handled by
  # the 60% NA-rate filter inside build_similarity_pool).
  all_players_sc_df <- reactive({
    base <- all_players_df()
    
    # Bind all joined_leagues into one SC reference table (physical cols only)
    sc_cols_keep <- c("player_name", physical_vars)
    sc_dfs <- lapply(SC_LEAGUES, function(lname) {
      df <- joined_cache$joined_leagues[[lname]]
      if (is.null(df) || nrow(df) == 0) return(NULL)
      available <- intersect(sc_cols_keep, names(df))
      if (!"player_name" %in% available) return(NULL)
      df[, available, drop = FALSE]
    })
    sc_ref <- dplyr::bind_rows(sc_dfs) |>
      dplyr::distinct(player_name, .keep_all = TRUE)
    
    # Left-join SC cols onto SB base; prefix cols with "sc_" to avoid clashes
    phys_in_sc <- intersect(physical_vars, names(sc_ref))
    if (length(phys_in_sc) == 0) return(base)
    
    sc_ref_renamed <- sc_ref |>
      dplyr::rename_with(~ paste0("sc_", .), dplyr::all_of(phys_in_sc))
    
    dplyr::left_join(base, sc_ref_renamed, by = "player_name")
  })
  
  # ---- SC reference pool: all 13 joined leagues, NO row filtering ----
  # Bind all rows from every joined league as-is. Lookup is by player_name.
  # Filtering risks dropping players we need; let resolve_sc_col handle missing cols.
  all_sc_df <- reactive({
    dfs <- lapply(SC_LEAGUES, function(lname) {
      df <- joined_cache$joined_leagues[[lname]]
      if (is.null(df) || nrow(df) == 0) return(NULL)
      if ("player_id"    %in% names(df)) df$player_id    <- as.character(df$player_id)
      if ("sc_player_id" %in% names(df)) df$sc_player_id <- as.character(df$sc_player_id)
      df$.league_label <- lname
      df
    })
    dplyr::bind_rows(dfs)
  })
  
  # ---- Liga MX SC pool — raw joined_leagues[["Liga MX"]], NO filtering ----
  # Return every row as-is. The GI columns live here and we look up players
  # by name. Any filtering risks dropping rows we need.
  liga_mx_sc_df <- reactive({
    df <- joined_cache$joined_leagues[["Liga MX"]]
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    if ("player_id"    %in% names(df)) df$player_id    <- as.character(df$player_id)
    if ("sc_player_id" %in% names(df)) df$sc_player_id <- as.character(df$sc_player_id)
    df
  })
  
  # ---- Sync sliders with league ----
  observeEvent(league_df(), {
    df <- league_df()
    max_min <- suppressWarnings(max(df$player_season_minutes %||% 0, na.rm=TRUE))
    lo <- min(450, max_min); hi <- max(1, max_min)
    updateSliderInput(session, "min_minutes", min=0, max=hi, value=c(lo, hi))
    
    ages <- compute_age_years(df$birth_date)
    ages <- ages[is.finite(ages) & ages < 200]
    amin <- if (length(ages)) max(15, floor(min(ages, na.rm=TRUE))) else 15
    amax <- if (length(ages)) ceiling(max(ages, na.rm=TRUE)) else 45
    updateSliderInput(session, "age_range", min=amin, max=max(amin+1,amax), value=c(amin,amax))

    teams <- sort(unique(as.character(df$team_name[!is.na(df$team_name) & df$team_name != "NA"])))
    updateSelectInput(session, "team_filter", choices = c("Todos los equipos", teams),
                      selected = "Todos los equipos")

    if ("player_country" %in% names(df)) {
      countries <- sort(unique(df$player_country[!is.na(df$player_country)]))
      updateSelectInput(session, "country_filter", choices = c("Todas", countries),
                        selected = "Todas")
    }

    # Reset pos filter (choices repopulated by pg observer below)
    updateSelectInput(session, "pos_filter", choices = "Todas", selected = "Todas")
  }, ignoreInit=FALSE)

  # ---- Populate primary-position sub-filter when league or pg changes ----
  observeEvent(list(league_df(), input$pg), {
    df_pg <- league_df() |> dplyr::filter(position_group == input$pg)
    positions <- sort(unique(as.character(
      df_pg$primary_position[!is.na(df_pg$primary_position)]
    )))
    updateSelectInput(session, "pos_filter",
                      choices  = c("Todas", positions),
                      selected = "Todas")
  }, ignoreInit = FALSE)
  
  league_df_scatter <- reactive({
    req(!is.null(input$min_minutes), length(input$min_minutes)==2,
        !is.null(input$age_range),   length(input$age_range)==2)
    df <- apply_scatter_filters(league_df(), minutes_range=input$min_minutes,
                                age_range=input$age_range)
    if (!is.null(input$team_filter) && nzchar(input$team_filter) &&
        input$team_filter != "Todos los equipos") {
      df <- df |> dplyr::filter(team_name == input$team_filter)
    }
    if (!is.null(input$pos_filter) && nzchar(input$pos_filter) &&
        input$pos_filter != "Todas") {
      df <- df |> dplyr::filter(primary_position == input$pos_filter)
    }
    if (!is.null(input$country_filter) && nzchar(input$country_filter) &&
        input$country_filter != "Todas" && "player_country" %in% names(df)) {
      df <- df |> dplyr::filter(player_country == input$country_filter)
    }
    df
  })
  
  # ---- Populate player search ----
  observeEvent(league_df(), {
    players <- league_df() |> arrange(player_name) |> distinct(player_name) |> pull()
    updateSelectizeInput(session, "player_search", choices=players,
                         selected=character(0), server=TRUE)
  }, ignoreInit=FALSE)
  
  selected_player <- reactiveVal(NULL)
  
  observeEvent(input$player_search, {
    if (!is.null(input$player_search) && nzchar(input$player_search))
      selected_player(input$player_search)
    else selected_player(NULL)
  }, ignoreInit=TRUE)
  
  # ---- Tabs UI ----
  output$tabs_ui <- renderUI({
    req(input$pg)
    cfg <- charts_cfg[[ input$pg ]]
    req(!is.null(cfg), length(cfg) > 0)
    
    std_tabs <- lapply(cfg, function(sp) {
      tabPanel(title=as.character(sp$title), value=sp$id,
               plotlyOutput(outputId=sp$id, height="600px"))
    })
    
    custom_tab <- tabPanel(
      title="Comparador Libre", value="custom_xy",
      fluidRow(
        column(6, selectInput("xvar","Eje X (elige métrica)", choices=character(0), selected=NULL)),
        column(6, selectInput("yvar","Eje Y (elige métrica)", choices=character(0), selected=NULL))
      ),
      plotlyOutput("custom_xy_plot", height="600px")
    )

    cross_liga_tab <- tabPanel(
      title="Cross-Liga", value="cross_liga",
      fluidRow(
        column(3, selectInput("cross_league1", "Liga 1", choices=names(league_map), selected=input$league)),
        column(3, selectInput("cross_league2", "Liga 2", choices=names(league_map),
                              selected=setdiff(names(league_map), input$league)[1])),
        column(3, selectInput("cross_xvar", "Eje X", choices=character(0), selected=NULL)),
        column(3, selectInput("cross_yvar", "Eje Y", choices=character(0), selected=NULL))
      ),
      plotlyOutput("cross_liga_plot", height="600px")
    )

    do.call(tabsetPanel, c(id="tabs_pos", std_tabs, list(custom_tab, cross_liga_tab)))
  })
  
  observeEvent(list(league_df(), input$pg), {
    df <- league_df() |> dplyr::filter(position_group == input$pg)
    vm <- var_map(df)
    if (!length(vm)) {
      updateSelectInput(session,"xvar",choices=character(0),selected=NULL)
      updateSelectInput(session,"yvar",choices=character(0),selected=NULL)
      return()
    }
    sel_x <- if ("goals_90" %in% names(vm)) "goals_90" else names(vm)[1]
    sel_y <- if ("np_xg_90" %in% names(vm)) "np_xg_90" else names(vm)[min(2,length(vm))]
    updateSelectInput(session,"xvar",choices=names(vm),selected=sel_x)
    updateSelectInput(session,"yvar",choices=names(vm),selected=sel_y)
  }, ignoreInit=FALSE)
  
  # ---- Radar panel ----
  output$radar_panel <- renderUI({
    if (is.null(selected_player())) {
      tagList(
        h4("Radar del jugador seleccionado"),
        tags$em("Selecciona un jugador (click en un punto o usa el buscador) para ver el radar.")
      )
    } else {
      all_players <- all_players_df() |> arrange(player_name) |> distinct(player_name) |> pull()
      tagList(
        h4("Radar del jugador seleccionado"),
        fluidRow(
          column(7, tags$strong(selected_player())),
          column(5, selectizeInput(
            "compare_player","Comparar con (cualquier liga):",
            choices=setdiff(all_players, selected_player()), multiple=FALSE,
            options=list(placeholder="Opcional", openOnFocus=TRUE, maxOptions=5000),
            selected=character(0)
          ))
        ),
        plotlyOutput("radar_plot", height="520px")
      )
    }
  })
  
  observeEvent(selected_player(), {
    updateSelectizeInput(session,"compare_player",selected=character(0))
  }, ignoreInit=TRUE)
  
  # ---- Scatter plots ----
  observe({
    req(input$pg)
    cfg      <- charts_cfg[[input$pg]]
    df_base  <- league_df_scatter()
    
    lapply(cfg, function(sp) {
      local({
        sp_local <- sp
        output[[sp_local$id]] <- renderPlotly({
          dat <- summarise_kpis(df_base, input$pg, sp_local$kpis)
          if (!is.null(sp_local$arrange_by) && sp_local$arrange_by %in% names(dat))
            dat <- dat |> arrange(desc(.data[[sp_local$arrange_by]]))
          make_scatter(dat=dat, x=sp_local$x, y=sp_local$y,
                       title=sp_local$title, subtitle=sp_local$subtitle,
                       xlab=sp_local$xlab, ylab=sp_local$ylab,
                       src=sp_local$id, selected=selected_player())
        })
        
        observeEvent(event_data("plotly_click", source=sp_local$id), {
          ed <- event_data("plotly_click", source=sp_local$id)
          if (!is.null(ed$key) && nzchar(ed$key)) selected_player(ed$key)
        }, ignoreInit=TRUE)
        
        if (ends_with_goles(sp_local$id)) {
          output[[paste0(sp_local$id,"_bar")]] <- renderPlotly({
            dat_goals <- df_base |>
              summarise(season_goals=.data[["season_goals"]],
                        .by=c("player_name","team_name","primary_position","position_group")) |>
              filter(position_group == input$pg)
            make_top_goals_bar(dat_goals)
          })
        }
      })
    })
  })
  
  # ---- Custom XY plot ----
  output$custom_xy_plot <- renderPlotly({
    df <- league_df() |> dplyr::filter(position_group == input$pg)
    vm <- var_map(df)
    req(length(vm)>0, input$xvar %in% names(vm), input$yvar %in% names(vm))
    col_x <- vm[[input$xvar]]; col_y <- vm[[input$yvar]]
    req(col_x %in% names(df), col_y %in% names(df))
    
    df_plot <- df |> dplyr::mutate(
      .hover = sprintf("<b>%s</b><br>Equipo: %s<br>%s: %s<br>%s: %s",
                       player_name, team_name,
                       input$xvar, round(.data[[col_x]],3),
                       input$yvar, round(.data[[col_y]],3)),
      .is_sel = if (!is.null(selected_player())) player_name==selected_player() else FALSE
    )
    others <- dplyr::filter(df_plot, !.is_sel)
    sel    <- dplyr::filter(df_plot, .is_sel)
    
    p <- ggplot() +
      { if (nrow(others)) geom_point(data=others,
                                     aes(x=.data[[col_x]], y=.data[[col_y]], text=.hover, key=player_name),
                                     size=2.6, alpha=0.25, color="grey50") } +
      { if (nrow(sel)) geom_point(data=sel,
                                  aes(x=.data[[col_x]], y=.data[[col_y]], text=.hover, key=player_name),
                                  size=4.0, alpha=1, color="blue3") } +
      labs(x=input$xvar, y=input$yvar, title="Comparador Libre") +
      theme_minimal(base_size=12) +
      theme(panel.grid.minor=element_blank(),
            panel.grid.major=element_line(linewidth=0.2,color="grey85"),
            axis.title=element_text(face="bold"),
            plot.title=element_text(face="bold"),
            plot.margin=margin(10,12,10,12))
    
    ggplotly(p, tooltip="text", source="custom_xy") |>
      layout(hoverlabel=list(align="left"))
  })
  
  observeEvent(event_data("plotly_click", source="custom_xy"), {
    ed <- event_data("plotly_click", source="custom_xy")
    if (!is.null(ed$key) && nzchar(ed$key)) selected_player(ed$key)
  }, ignoreInit=TRUE)

  # ---- Cross-liga: populate metric selectors from league 1 ----
  observeEvent(list(input$cross_league1, input$cross_league2, input$pg), {
    req(input$cross_league1, input$pg)
    df1 <- tryCatch(
      get_league_df(scout, input$cross_league1) |> dplyr::filter(position_group == input$pg),
      error = function(e) NULL
    )
    if (is.null(df1) || nrow(df1) == 0) return()
    vm <- var_map(df1)
    if (!length(vm)) return()
    sel_x <- if ("goals_90" %in% names(vm)) "goals_90" else names(vm)[1]
    sel_y <- if ("np_xg_90" %in% names(vm)) "np_xg_90" else names(vm)[min(2, length(vm))]
    updateSelectInput(session, "cross_xvar", choices = names(vm), selected = sel_x)
    updateSelectInput(session, "cross_yvar", choices = names(vm), selected = sel_y)
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # ---- Cross-liga scatter plot ----
  output$cross_liga_plot <- renderPlotly({
    req(input$cross_league1, input$cross_league2, input$cross_xvar, input$cross_yvar,
        input$min_minutes, input$age_range)

    load_league <- function(lbl) {
      tryCatch(
        get_league_df(scout, lbl) |>
          dplyr::filter(position_group == input$pg) |>
          apply_scatter_filters(minutes_range = input$min_minutes, age_range = input$age_range) |>
          dplyr::mutate(.league_src = lbl),
        error = function(e) NULL
      )
    }
    df1 <- load_league(input$cross_league1)
    df2 <- load_league(input$cross_league2)
    req(!is.null(df1) || !is.null(df2))
    df_both <- dplyr::bind_rows(df1, df2)

    vm <- var_map(if (!is.null(df1)) df1 else df2)
    req(input$cross_xvar %in% names(vm), input$cross_yvar %in% names(vm))
    col_x <- vm[[input$cross_xvar]]
    col_y <- vm[[input$cross_yvar]]
    req(col_x %in% names(df_both), col_y %in% names(df_both))

    df_plot <- df_both |>
      dplyr::mutate(
        .hover  = sprintf("<b>%s</b><br>%s<br>Equipo: %s<br>%s: %s<br>%s: %s",
                          player_name, .league_src, team_name,
                          input$cross_xvar, round(.data[[col_x]], 3),
                          input$cross_yvar, round(.data[[col_y]], 3)),
        .is_sel = if (!is.null(selected_player())) player_name == selected_player() else FALSE
      )

    others <- dplyr::filter(df_plot, !.is_sel)
    sel    <- dplyr::filter(df_plot,  .is_sel)

    p <- suppressWarnings(
      ggplot() +
        { if (nrow(others) > 0)
          geom_point(data = others,
                     aes(x = .data[[col_x]], y = .data[[col_y]], text = .hover,
                         key = player_name, color = .league_src),
                     size = 2.6, alpha = 0.35) } +
        { if (nrow(sel) > 0)
          geom_point(data = sel,
                     aes(x = .data[[col_x]], y = .data[[col_y]], text = .hover,
                         key = player_name),
                     size = 4.5, alpha = 1, color = "black", shape = 18) } +
        scale_color_brewer(type = "qual", palette = "Set1") +
        labs(x = input$cross_xvar, y = input$cross_yvar,
             title = paste("Cross-Liga:", input$cross_league1, "vs", input$cross_league2),
             color = "Liga") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor  = element_blank(),
              panel.grid.major  = element_line(linewidth = 0.2, color = "grey85"),
              axis.title        = element_text(face = "bold"),
              plot.title        = element_text(face = "bold"),
              plot.margin       = margin(10, 12, 10, 12))
    )

    ggplotly(p, tooltip = "text", source = "cross_liga") |>
      layout(hoverlabel = list(align = "left"))
  })

  observeEvent(event_data("plotly_click", source = "cross_liga"), {
    ed <- event_data("plotly_click", source = "cross_liga")
    if (!is.null(ed$key) && nzchar(ed$key)) selected_player(ed$key)
  }, ignoreInit = TRUE)

  # ---- Similarity ----
  build_similarity_pool <- function(dat, pg = NULL) {
    df <- if (is.null(pg)) dplyr::filter(dat, position_group != "Portero")
          else dplyr::filter(dat, position_group == pg)
    id_cols <- c("player_name","team_name","primary_position","position_group",
                 "player_season_90s_played","player_season_minutes",
                 "league","season","country","competition","birth_date",
                 ".league_label",".league_key","sc_player_id","match_type")
    num_cols <- names(df)[vapply(df, is.numeric, TRUE)]
    keep     <- setdiff(num_cols, id_cols)
    pool     <- dplyr::select(df, player_name, team_name, primary_position,
                              player_season_90s_played, dplyr::all_of(keep)) |>
      dplyr::distinct(player_name, .keep_all=TRUE)
    na_rate  <- vapply(pool[keep], function(v) mean(is.na(v)), numeric(1))
    keep1    <- keep[na_rate <= 0.60]
    pool     <- dplyr::select(pool, player_name, team_name, primary_position,
                              player_season_90s_played, dplyr::all_of(keep1))
    if (length(keep1)) {
      for (cn in keep1) {
        v <- pool[[cn]]; m <- if (all(is.na(v))) 0 else mean(v, na.rm=TRUE)
        v[!is.finite(v)] <- NA; v[is.na(v)] <- m; pool[[cn]] <- as.numeric(v)
      }
      sdv  <- vapply(pool[keep1], stats::sd, numeric(1))
      keep2 <- keep1[sdv > 1e-8]
      pool  <- dplyr::select(pool, player_name, team_name, primary_position,
                             player_season_90s_played, dplyr::all_of(keep2))
    } else { keep2 <- character(0) }
    list(pool=pool, metric_cols=keep2)
  }
  
  cosine_sim_to_i <- function(M, i) {
    M <- scale(M); M[is.na(M)] <- 0
    rn <- sqrt(rowSums(M^2)); rn[rn==0|!is.finite(rn)] <- 1
    Mn <- M/rn; drop(Mn %*% Mn[i,])
  }
  
  # ---- Similarity using SB + SC data ----
  similar_players_sc <- reactive({
    req(selected_player())
    dat_all <- all_players_sc_df()
    sel_row <- dat_all |> dplyr::filter(player_name == selected_player()) |> dplyr::slice_head(n = 1)
    req(nrow(sel_row) == 1)
    pg   <- sel_row$position_group[1]
    ppos <- sel_row$primary_position[1]
    built   <- build_similarity_pool(dat_all)
    pool    <- built$pool; metrics <- built$metric_cols
    if (length(metrics) < 5 || nrow(pool) < 10)
      return(data.frame(Jugador = character(), Equipo = character(),
                        Liga = character(), Posicion = character(),
                        Edad = integer(), Similitud = numeric()))
    pool_use <- pool
    M   <- as.matrix(pool_use[, metrics, drop = FALSE])
    idx <- match(selected_player(), pool_use$player_name)
    req(!is.na(idx))
    sims <- cosine_sim_to_i(M, idx)

    meta <- dat_all |>
      dplyr::distinct(player_name, .keep_all = TRUE) |>
      dplyr::select(player_name, .league_label, birth_date)

    pool_use |>
      dplyr::mutate(similarity = sims) |>
      dplyr::filter(player_name != selected_player()) |>
      dplyr::left_join(meta, by = "player_name") |>
      dplyr::mutate(Edad = as.integer(compute_age_years(birth_date))) |>
      dplyr::arrange(dplyr::desc(similarity)) |>
      dplyr::transmute(
        Jugador   = player_name,
        Equipo    = team_name,
        Liga      = .league_label,
        Posicion  = primary_position,
        Edad      = Edad,
        Similitud = round(pmin(pmax(similarity, -1), 1), 3)
      )
  })

  # ---- Filtered similarity table ----
  similar_players_sc_filtered <- reactive({
    df <- similar_players_sc()
    if (nrow(df) == 0) return(df)

    league_sel <- input$sim_league_filter
    pos_sel    <- input$sim_pos_filter
    age_range  <- input$sim_age_filter
    min_sim    <- input$sim_min_similarity

    if (length(league_sel) > 0 && !("" %in% league_sel))
      df <- dplyr::filter(df, Liga %in% league_sel)
    if (!is.null(pos_sel) && nzchar(pos_sel))
      df <- dplyr::filter(df, Posicion == pos_sel)
    if (!is.null(age_range) && length(age_range) == 2)
      df <- dplyr::filter(df, Edad >= age_range[1], Edad <= age_range[2])
    if (!is.null(min_sim) && is.numeric(min_sim))
      df <- dplyr::filter(df, Similitud >= min_sim)

    df
  })

  observe({
    df <- similar_players_sc()
    positions <- sort(unique(df$Posicion))
    updateSelectInput(session, "sim_pos_filter",
                      choices = c("Todas" = "", positions),
                      selected = "")
  })
  
  output$second_row <- renderUI({
    fluidRow(
      column(7,
             tagList(
               tags$h5("Jugadores más similares"),
               tags$p(style="color:#666;font-size:0.85em;margin:2px 0 4px;",
                      "Similitud calculada usando métricas de StatsBomb y datos físicos de SkillCorner."),
               fluidRow(
                 column(4, selectizeInput("sim_league_filter", "Liga",
                                          choices  = names(league_map),
                                          selected = NULL,
                                          multiple = TRUE,
                                          options  = list(placeholder = "Todas las ligas"))),
                 column(4, selectInput("sim_pos_filter", "Posición",
                                       choices  = c("Todas" = ""),
                                       selected = "")),
                 column(4, numericInput("sim_min_similarity", "Similitud mín.",
                                        value = 0.45, min = -1, max = 1, step = 0.05))
               ),
               fluidRow(
                 column(8, sliderInput("sim_age_filter", "Rango de edad",
                                       min = 15, max = 45, value = c(15, 45),
                                       step = 1, width = "100%"))
               ),
               DT::DTOutput("similar_players_sc_table"),
               tags$small(HTML(
                 "Interpretación (coseno):<br>
             <b>≥ 0.60</b> = Muy similares &nbsp;|&nbsp;
             <b>0.60–0.45</b> = Similares &nbsp;|&nbsp;
             <b>< 0.45</b> = Baja similitud"
               ))
             )
      ),
      column(5, tableOutput("player_stats_table"))
    )
  })
  
  # ---- Radar plot ----
  draw_radar_for_player_plotly <- function(player, compare_to=NULL, dat_all) {
    row_sel <- dat_all |> dplyr::filter(player_name==player) |> dplyr::slice_head(n=1)
    req(nrow(row_sel)==1)
    pg      <- row_sel$position_group[1]; req(!is.na(pg))
    col_map <- position_metric_map(pg);   req(length(col_map)>0)
    real_cols  <- unname(col_map)
    have_cols  <- real_cols %in% names(dat_all)
    col_map    <- col_map[have_cols]; real_cols <- real_cols[have_cols]
    metric_cols <- names(col_map); req(length(metric_cols)>=3)
    dat_pg  <- dat_all |> dplyr::filter(position_group==pg)
    df_all  <- dat_pg |>
      dplyr::select(player_name,team_name,birth_date,primary_position,
                    player_season_90s_played, dplyr::all_of(real_cols))
    names(df_all)[match(real_cols,names(df_all))] <- metric_cols
    df_all  <- df_all |> dplyr::filter(dplyr::if_any(dplyr::all_of(metric_cols),~!is.na(.x)))
    df_all[metric_cols] <- lapply(df_all[metric_cols], function(v)
      suppressWarnings(as.numeric(v)))
    if (!is.null(compare_to) && !compare_to %in% dat_all$player_name) compare_to <- NULL
    
    get_stats <- function(who) {
      row_who <- dat_all |> dplyr::filter(player_name==who) |> dplyr::slice_head(n=1)
      req(nrow(row_who)==1)
      vals_raw <- sapply(metric_cols, function(m)
        suppressWarnings(as.numeric(row_who[[col_map[[m]]]])))
      vals_pct <- sapply(seq_along(metric_cols), function(i) {
        m <- metric_cols[i]; rawv <- vals_raw[i]; v_dist <- df_all[[m]]
        if (is.na(rawv)||all(is.na(v_dist))||diff(range(v_dist,na.rm=TRUE))==0) 50
        else 100*stats::ecdf(v_dist)(rawv)
      })
      list(raw=vals_raw, pct=vals_pct)
    }
    
    build_trace <- function(who) {
      s <- get_stats(who)
      theta <- nice_metric_label(c(metric_cols,metric_cols[1]))
      list(theta=theta, r=c(s$pct,s$pct[1]), custom=c(s$raw,s$raw[1]))
    }
    
    pal <- c("#d62728","#2ca02c")
    tr  <- build_trace(player)
    
    p <- plotly::plot_ly(type="scatterpolar",mode="lines+markers") |>
      plotly::add_trace(
        r=tr$r, theta=tr$theta, customdata=tr$custom, name=player,
        line=list(width=2,color=pal[1]), fill="toself",
        fillcolor=paste0(pal[1],"66"), marker=list(size=6,color=pal[1]),
        hovertemplate=paste0("<b>",player,"</b><br>%{theta}: %{customdata:.2f}<br>Percentil: %{r:.0f}<extra></extra>")
      )
    
    if (!is.null(compare_to)) {
      tr2 <- build_trace(compare_to)
      p   <- p |> plotly::add_trace(
        r=tr2$r, theta=tr2$theta, customdata=tr2$custom, name=compare_to,
        line=list(width=2,color=pal[2]), fill="toself",
        fillcolor=paste0(pal[2],"55"), marker=list(size=6,color=pal[2]),
        hovertemplate=paste0("<b>",compare_to,"</b><br>%{theta}: %{customdata:.2f}<br>Percentil: %{r:.0f}<extra></extra>")
      )
    }
    
    plotly::layout(p,
                   polar=list(
                     radialaxis=list(range=c(0,100),tickvals=c(0,20,40,60,80,100),
                                     gridcolor="#e5e5e5",tickfont=list(size=10)),
                     angularaxis=list(tickfont=list(size=10))
                   ),
                   legend=list(orientation="h",x=0,y=-0.1),
                   margin=list(l=20,r=20,t=30,b=40)
    )
  }
  
  output$radar_plot <- renderPlotly({
    req(selected_player())
    compare_to <- if (!is.null(input$compare_player) && nzchar(input$compare_player))
      input$compare_player else NULL
    draw_radar_for_player_plotly(player=selected_player(), compare_to=compare_to,
                                 dat_all=all_players_df())
  })
  
  # ---- Player stats table (StatsBomb percentiles within league) ----
  player_stats_tbl <- reactive({
    req(selected_player())
    dat <- league_df()
    row <- dat |> dplyr::filter(player_name==selected_player()) |> dplyr::slice_head(n=1)
    req(nrow(row)==1)
    
    age_val <- compute_age_years(row$birth_date)
    age_str <- if (is.finite(age_val) && age_val <= 100) paste0(age_val, " años") else "–"

    meta <- tibble::tribble(
      ~Métrica, ~Valor, ~Percentil,
      "Nombre",          as.character(row$player_name),          "",
      "Equipo",          as.character(row$team_name),            "",
      "Liga",            as.character(input$league),             "",
      "Nacimiento",      as.character(row$birth_date),           "",
      "Edad",            age_str,                                "",
      "90s Jugados",     fmt_num(row$player_season_90s_played,1),"",
      "Minutos Jugados", fmt_num(row$player_season_minutes,0),   ""
    )
    
    stat_rows <- build_position_stat_rows_with_percentiles(row, dat) |>
      dplyr::mutate(Valor=as.character(Valor), Percentil=as.character(Percentil))
    
    dplyr::bind_rows(
      meta |> dplyr::mutate(Valor=as.character(Valor), Percentil=as.character(Percentil)),
      stat_rows
    )
  })
  
  output$player_stats_table <- renderTable(
    { req(selected_player()); player_stats_tbl() },
    striped=TRUE, hover=TRUE, bordered=TRUE, align="lrr", width="100%"
  )
  
  output$similar_players_sc_table <- DT::renderDT({
    req(selected_player())
    df <- similar_players_sc_filtered()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hay jugadores con los filtros seleccionados."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    DT::datatable(
      df,
      rownames  = FALSE,
      options   = list(
        pageLength = 10,
        dom        = "tp",
        order      = list(list(5L, "desc"))
      )
    ) |>
      DT::formatStyle(
        "Similitud",
        backgroundColor = DT::styleInterval(
          c(0.45, 0.60),
          c("#ffcccc", "#fff3cd", "#ccffcc")
        )
      )
  })
  
  # ============================================================
  # SKILLCORNER TABLE
  # ============================================================
  
  # Reactive: build the SC table for the selected player
  sc_table_data <- reactive({
    req(selected_player())
    
    message(sprintf("[SC_REACTIVE] triggered for player='%s' league='%s'",
                    selected_player(), input$league))
    message(sprintf("[SC_REACTIVE] liga_mx_sc_df rows=%d  all_sc_df rows=%d",
                    nrow(liga_mx_sc_df()), nrow(all_sc_df())))
    
    player_row <- league_df() |>
      dplyr::filter(player_name == selected_player()) |>
      dplyr::slice_head(n = 1)
    
    message(sprintf("[SC_REACTIVE] player_row found: %d rows", nrow(player_row)))
    
    if (nrow(player_row) == 0) {
      message("[SC_REACTIVE] player_row is empty — returning NULL")
      return(NULL)
    }
    
    result <- tryCatch(
      build_skillcorner_table(
        all_sc_df    = all_sc_df(),
        liga_mx_df   = liga_mx_sc_df(),
        player_row   = player_row,
        league_label = input$league
      ),
      error = function(e) {
        message(sprintf("[SC_REACTIVE] ERROR in build_skillcorner_table: %s", conditionMessage(e)))
        NULL
      }
    )
    message(sprintf("[SC_REACTIVE] result rows=%d", if(is.null(result)) -1 else nrow(result)))
    result
  })
  
  # Optional message above the table
  output$sc_no_data_msg <- renderUI({
    req(selected_player())
    tbl <- sc_table_data()
    if (is.null(tbl)) return(NULL)
    if (nrow(tbl) == 1 && tbl$Tipo[1] == "–") {
      tags$div(
        style="background:#fff3cd;border:1px solid #ffc107;border-radius:4px;
                padding:10px 14px;margin-bottom:8px;color:#856404;font-weight:500;",
        tbl$Métrica[1]
      )
    } else NULL
  })
  
  # Helper: render a filtered slice of sc_table_data as a styled DT
  make_sc_dt <- function(tbl_slice) {
    if (is.null(tbl_slice) || nrow(tbl_slice) == 0) {
      return(DT::datatable(
        data.frame(Métrica=character(), Valor=character(), Percentil=numeric()),
        rownames=FALSE, options=list(dom="t", paging=FALSE)
      ))
    }
    # Drop the Tipo column for individual tabs (redundant within a tab)
    tbl_slice <- tbl_slice |>
      dplyr::select(-Tipo) |>
      dplyr::mutate(Percentil = suppressWarnings(as.numeric(Percentil)))
    
    DT::datatable(
      tbl_slice,
      rownames = FALSE,
      escape   = TRUE,
      options  = list(
        dom        = "t",
        paging     = FALSE,
        ordering   = TRUE,
        autoWidth  = TRUE,
        scrollX    = FALSE,
        columnDefs = list(
          list(targets=0, width="780px"),
          list(targets=1, width="120px", className="dt-right"),
          list(targets=2, width="120px", className="dt-right")
        )
      ),
      class = "stripe hover compact"
    ) |>
      DT::formatRound("Percentil", digits=1) |>
      DT::formatStyle(
        "Percentil",
        backgroundColor = DT::styleInterval(
          c(35, 70), c("#f8d7da", "transparent", "#d1e7dd")
        ),
        fontWeight = "bold"
      ) |>
      DT::formatStyle("Métrica", whiteSpace="normal", lineHeight="1.1em")
  }
  
  # Decide whether to show tabbed (Liga MX) or flat layout
  output$skillcorner_ui <- renderUI({
    req(selected_player())
    tbl <- sc_table_data()
    is_ligamx <- identical(as.character(input$league), "Liga MX")
    
    # No data / portero case — just show empty
    if (is.null(tbl) || (nrow(tbl) == 1 && tbl$Tipo[1] == "–")) {
      return(DTOutput("sc_tab_flat"))
    }
    
    if (is_ligamx) {
      # Check if GI rows actually exist
      has_gi <- any(tbl$Tipo == "Game Intelligence")
      if (has_gi) {
        tabsetPanel(
          tabPanel("Físico",      DTOutput("sc_tab_fisico")),
          tabPanel("Con Balón",       DTOutput("sc_tab_pases")),
          tabPanel("Desmarques",  DTOutput("sc_tab_desmarques"))
          # tabPanel("Presiones",   DTOutput("sc_tab_presiones"))
        )
      } else {
        DTOutput("sc_tab_flat")
      }
    } else {
      DTOutput("sc_tab_flat")
    }
  })
  
  # ---- Flat table (non-Liga MX or no GI data) ----
  output$sc_tab_flat <- DT::renderDT({
    req(selected_player())
    tbl <- sc_table_data()
    if (is.null(tbl) || (nrow(tbl) == 1 && tbl$Tipo[1] == "–")) {
      return(DT::datatable(
        data.frame(Tipo=character(), Métrica=character(),
                   Valor=character(), Percentil=numeric()),
        rownames=FALSE, options=list(dom="t", paging=FALSE)
      ))
    }
    tbl <- tbl |> dplyr::mutate(Percentil = suppressWarnings(as.numeric(Percentil)))
    DT::datatable(
      tbl, rownames=FALSE, escape=TRUE,
      options=list(
        dom="t", paging=FALSE, ordering=TRUE, autoWidth=TRUE, scrollX=FALSE,
        columnDefs=list(
          list(targets=0, width="160px", className="dt-center"),
          list(targets=1, width="780px"),
          list(targets=2, width="120px", className="dt-right"),
          list(targets=3, width="120px", className="dt-right")
        )
      ),
      class="stripe hover compact"
    ) |>
      DT::formatRound("Percentil", digits=1) |>
      DT::formatStyle("Percentil",
                      backgroundColor=DT::styleInterval(c(35,70),c("#f8d7da","transparent","#d1e7dd")),
                      fontWeight="bold") |>
      DT::formatStyle("Métrica", whiteSpace="normal", lineHeight="1.1em") |>
      DT::formatStyle("Tipo", fontWeight="bold", color="#1a5276")
  }, server=FALSE)
  
  # ---- Físico tab ----
  output$sc_tab_fisico <- DT::renderDT({
    req(selected_player())
    tbl <- sc_table_data()
    req(!is.null(tbl), nrow(tbl) > 1 || tbl$Tipo[1] != "–")
    make_sc_dt(tbl |> dplyr::filter(Tipo == "Físico"))
  }, server=FALSE)
  
  # ---- Pases tab (GI pass vars) ----
  output$sc_tab_pases <- DT::renderDT({
    req(selected_player())
    tbl <- sc_table_data()
    req(!is.null(tbl), nrow(tbl) > 1 || tbl$Tipo[1] != "–")
    # Map gi_vars_pases to their display labels (clean_sc_col_name)
    pases_labels <- sapply(gi_vars_pases, clean_sc_col_name)
    make_sc_dt(tbl |> dplyr::filter(Métrica %in% pases_labels))
  }, server=FALSE)
  
  # ---- Desmarques tab (GI run vars) ----
  output$sc_tab_desmarques <- DT::renderDT({
    req(selected_player())
    tbl <- sc_table_data()
    req(!is.null(tbl), nrow(tbl) > 1 || tbl$Tipo[1] != "–")
    desmarques_labels <- sapply(gi_vars_desmarques, clean_sc_col_name)
    make_sc_dt(tbl |> dplyr::filter(Métrica %in% desmarques_labels))
  }, server=FALSE)
  
  # # ---- Presiones tab (GI pressure vars) ----
  # output$sc_tab_presiones <- DT::renderDT({
  #   req(selected_player())
  #   tbl <- sc_table_data()
  #   req(!is.null(tbl), nrow(tbl) > 1 || tbl$Tipo[1] != "–")
  #   presiones_labels <- sapply(gi_vars_presiones, clean_sc_col_name)
  #   make_sc_dt(tbl |> dplyr::filter(Métrica %in% presiones_labels))
  # }, server=FALSE)
  
}

shinyApp(ui, server)


# final_scout <- readRDS("/Users/mateorodriguez/Desktop/analisis_CA/scout_dashboard/data/scout_joined.rds")
# 
# final_scout <- final_scout$joined_leagues$`Liga MX`
# 
# glimpse(final_scout)

# rsconnect::deployApp(
#   appFiles = c("app.R", "radars.R", "data/scout_joined.rds")
# )