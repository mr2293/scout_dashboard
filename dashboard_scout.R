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


username = Sys.getenv("SB_USERNAME")
password = Sys.getenv("SB_PASSWORD")

# comps <- competitions(username,password)

ligamx <- matchesvector(username, password, season_id = 318, competition_id = 73)
ccl <- matchesvector(username, password, season_id = 316, competition_id = 101)
# arg <- matchesvector(username, password, season_id = 316, competition_id = 81)
arg <- matchesvector(username, password, season_id = 315, competition_id = 81)
brasil <- matchesvector(username, password, season_id = 315, competition_id = 71)
# colombia <- matchesvector(username, password, season_id = 316, competition_id = 74)
colombia <- matchesvector(username, password, season_id = 315, competition_id = 74)
ecuador <- matchesvector(username, password, season_id = 315, competition_id = 105)
chile <- matchesvector(username, password, season_id = 315, competition_id = 103)
paraguay <- matchesvector(username, password, season_id = 315, competition_id = 247)
uruguay <- matchesvector(username, password, season_id = 315, competition_id = 111)
mls <- matchesvector(username, password, season_id = 315, competition_id = 44)
champions <- matchesvector(username, password, season_id = 318, competition_id = 16)
premier <- matchesvector(username, password, season_id = 318, competition_id = 2)
championship <- matchesvector(username, password, season_id = 318, competition_id = 3)
laliga <- matchesvector(username, password, season_id = 318, competition_id = 11)
laliga_2 <- matchesvector(username, password, season_id = 318, competition_id = 42)
serie_a <- matchesvector(username, password, season_id = 318, competition_id = 12)
serie_b <- matchesvector(username, password, season_id = 318, competition_id = 84)
bundesliga <- matchesvector(username, password, season_id = 318, competition_id = 9)
bundesliga_2 <- matchesvector(username, password, season_id = 318, competition_id = 10)
ligue_1 <- matchesvector(username, password, season_id = 318, competition_id = 7)
eredivisie <- matchesvector(username, password, season_id = 318, competition_id = 6)
belgica <- matchesvector(username, password, season_id = 318, competition_id = 46)
libertadores <- matchesvector(username, password, season_id = 315, competition_id = 102)
portugal <- matchesvector(username, password, season_id = 318, competition_id = 13)
turquia <- matchesvector(username, password, season_id = 318, competition_id = 85)
escocia <- matchesvector(username, password, season_id = 318, competition_id = 51)
europa_league <- matchesvector(username, password, season_id = 318, competition_id = 35)

# ligamxlineups <- alllineups(username, password, ligamx, parallel = T)
# ligamxlineups <- cleanlineups(ligamxlineups)
# 
# ccl_lineups <- alllineups(username, password, ccl, parallel = T)
# ccl_lineups <- cleanlineups(ccl_lineups)
# 
# arg_lineups <- alllineups(username, password, arg, parallel = T)
# arg_lineups <- cleanlineups(arg_lineups)
# 
# brasil_lineups <- alllineups(username, password, brasil, parallel = T)
# brasil_lineups <- cleanlineups(brasil_lineups)
# 
# colombia_lineups <- alllineups(username, password, colombia, parallel = T)
# colombia_lineups <- cleanlineups(colombia_lineups)
# 
# ecuador_lineups <- alllineups(username, password, ecuador, parallel = T)
# ecuador_lineups <- cleanlineups(ecuador_lineups)
# 
# chile_lineups <- alllineups(username, password, chile, parallel = T)
# chile_lineups <- cleanlineups(chile_lineups)
# 
# paraguay_lineups <- alllineups(username, password, paraguay, parallel = T)
# paraguay_lineups <- cleanlineups(paraguay_lineups)
# 
# uruguay_lineups <- alllineups(username, password, uruguay, parallel = T)
# uruguay_lineups <- cleanlineups(uruguay_lineups)
# 
# mls_lineups <- alllineups(username, password, mls, parallel = T)
# mls_lineups <- cleanlineups(mls_lineups)
# 
# premier_lineups <- alllineups(username, password, premier, parallel = T)
# premier_lineups <- cleanlineups(premier_lineups)
# 
# championship_lineups <- alllineups(username, password, championship, parallel = T)
# championship_lineups <- cleanlineups(championship_lineups)
# 
# laliga_lineups <- alllineups(username, password, laliga, parallel = T)
# laliga_lineups <- cleanlineups(laliga_lineups)
# 
# laliga2_lineups <- alllineups(username, password, laliga_2, parallel = T)
# laliga2_lineups <- cleanlineups(laliga2_lineups)
# 
# seriea_lineups <- alllineups(username, password, serie_a, parallel = T)
# seriea_lineups <- cleanlineups(seriea_lineups)
# 
# serieb_lineups <- alllineups(username, password, serie_b, parallel = T)
# serieb_lineups <- cleanlineups(serieb_lineups)
# 
# bundesliga_lineups <- alllineups(username, password, bundesliga, parallel = T)
# bundesliga_lineups <- cleanlineups(bundesliga_lineups)
# 
# bundesliga2_lineups <- alllineups(username, password, bundesliga_2, parallel = T)
# bundesliga2_lineups <- cleanlineups(bundesliga2_lineups)
# 
# ligue1_lineups <- alllineups(username, password, ligue_1, parallel = T)
# ligue1_lineups <- cleanlineups(ligue1_lineups)
# 
# eredivisie_lineups <- alllineups(username, password, eredivisie, parallel = T)
# eredivisie_lineups <- cleanlineups(eredivisie_lineups)
# 
# belgica_lineups <- alllineups(username, password, belgica, parallel = T)
# belgica_lineups <- cleanlineups(belgica_lineups)
# 
# turquia_lineups <- alllineups(username, password, turquia, parallel = T)
# turquia_lineups <- cleanlineups(turquia_lineups)
# 
# portugal_lineups <- alllineups(username, password, portugal, parallel = T)
# portugal_lineups <- cleanlineups(portugal_lineups)
# 
# escocia_lineups <- alllineups(username, password, escocia, parallel = T)
# escocia_lineups <- cleanlineups(escocia_lineups)
# 
# champions_lineups <- alllineups(username, password, champions, parallel = T)
# champions_lineups <- cleanlineups(champions_lineups)
# 
# uel_lineups <- alllineups(username, password, europa_league, parallel = T)
# uel_lineups <- cleanlineups(uel_lineups)
# 
# libertadores_lineups <- alllineups(username, password, libertadores, parallel = T)
# libertadores_lineups <- cleanlineups(libertadores_lineups)

jugs_ligamx <- player_season(username, password, season_id = 318, competition_id = 73)
jugs_ccl <- player_season(username, password, season_id = 316, competition_id = 101)
arg <- player_season(username, password, season_id = 316, competition_id = 81)
jugs_arg <- player_season(username, password, season_id = 315, competition_id = 81)
jugs_brasil <- player_season(username, password, season_id = 315, competition_id = 71)
jugs_colombia <- player_season(username, password, season_id = 316, competition_id = 74)
# jugs_colombia <- player_season(username, password, season_id = 315, competition_id = 74)
jugs_ecuador <- player_season(username, password, season_id = 315, competition_id = 105)
jugs_chile <- player_season(username, password, season_id = 315, competition_id = 103)
jugs_paraguay <- player_season(username, password, season_id = 315, competition_id = 247)
jugs_uruguay <- player_season(username, password, season_id = 315, competition_id = 111)
jugs_mls <- player_season(username, password, season_id = 315, competition_id = 44)
jugs_premier <- player_season(username, password, season_id = 318, competition_id = 2)
jugs_championship <- player_season(username, password, season_id = 318, competition_id = 3)
jugs_laliga <- player_season(username, password, season_id = 318, competition_id = 11)
jugs_laliga_2 <- player_season(username, password, season_id = 318, competition_id = 42)
jugs_serie_a <- player_season(username, password, season_id = 318, competition_id = 12)
jugs_serie_b <- player_season(username, password, season_id = 318, competition_id = 84)
jugs_bundesliga <- player_season(username, password, season_id = 318, competition_id = 9)
jugs_bundesliga_2 <- player_season(username, password, season_id = 318, competition_id = 10)
jugs_ligue_1 <- player_season(username, password, season_id = 318, competition_id = 7)
jugs_eredivisie <- player_season(username, password, season_id = 318, competition_id = 6)
jugs_belgica <- player_season(username, password, season_id = 318, competition_id = 46)
jugs_portugal <- player_season(username, password, season_id = 318, competition_id = 13)
jugs_turquia <- player_season(username, password, season_id = 318, competition_id = 85)
jugs_escocia <- player_season(username, password, season_id = 318, competition_id = 51)
jugs_champions <- player_season(username, password, season_id = 318, competition_id = 16)
jugs_libertadores <- player_season(username, password, season_id = 315, competition_id = 102)
jugs_uel <- player_season(username, password, season_id = 318, competition_id = 35)

goles_ligamx <- all_player_matches(username, password, ligamx)
goles_ccl <- all_player_matches(username, password, ccl)
goles_arg <- all_player_matches(username, password, arg)
goles_brasil <- all_player_matches(username, password, brasil)
goles_colombia <- all_player_matches(username, password, colombia)
goles_ecuador <- all_player_matches(username, password, ecuador)
goles_chile <- all_player_matches(username, password, chile)
goles_paraguay <- all_player_matches(username, password, paraguay)
goles_uruguay <- all_player_matches(username, password, uruguay)
goles_mls <- all_player_matches(username, password, mls)
goles_premier <- all_player_matches(username, password, premier)
goles_championship <- all_player_matches(username, password, championship)
goles_laliga <- all_player_matches(username, password, laliga)
goles_laliga_2 <- all_player_matches(username, password, laliga_2)
goles_serie_a <- all_player_matches(username, password, serie_a)
goles_serie_b <- all_player_matches(username, password, serie_b)
goles_bundesliga <- all_player_matches(username, password, bundesliga)
goles_bundesliga_2 <- all_player_matches(username, password, bundesliga_2)
goles_ligue_1 <- all_player_matches(username, password, ligue_1)
goles_eredivisie <- all_player_matches(username, password, eredivisie)
goles_belgica <- all_player_matches(username, password, belgica)
goles_portugal <- all_player_matches(username, password, portugal)
goles_turquia <- all_player_matches(username, password, turquia)
goles_escocia <- all_player_matches(username, password, escocia)
goles_champions <- all_player_matches(username, password, champions)
goles_libertadores <- all_player_matches(username, password, libertadores)
goles_uel <- all_player_matches(username, password, europa_league)

glimpse(jugs_ligamx)

distinct_positions <- jugs_ligamx |> 
  distinct(primary_position) |> 
  arrange(primary_position)

# LIGA MX ----

# pos_ligamx <- ligamxlineups |>
#   unnest(positions, keep_empty = TRUE) |>
#   filter(!is.na(position), position != "") |>
#   mutate(
#     from_hms = suppressWarnings(hms(from)),
#     to_hms   = suppressWarnings(hms(to)),
#     mins     = as.numeric(to_hms - from_hms) / 60
#   ) |>
#   group_by(player_id, position) |>
#   summarise(
#     matches = n_distinct(match_id),     # in how many matches this position appeared
#     stints  = n(),                      # how many position stints
#     minutes = sum(replace_na(mins, 0)), # total minutes in that position
#     .groups = "drop"
#   ) |>
#   arrange(player_id, desc(matches), desc(stints), desc(minutes)) |>
#   group_by(player_id, player_name) |>
#   slice(1) |>                          # pick the winner per player
#   ungroup() |>
#   transmute(player_id, primary_position = position)

jugs_ligamx <- jugs_ligamx |>
  mutate(position_group = case_when(
           primary_position == "Goalkeeper" ~ "Portero",
           primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
           primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
           primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
           primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
           primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
           primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
           TRUE ~ NA_character_
         ))

goles_ligamx <- goles_ligamx |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  ) |>
  arrange(desc(season_goals))

jugs_ligamx <- jugs_ligamx |>
  left_join(goles_ligamx, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

## Delanteros ----

### Goleadores ----

del_goleadores <- jugs_ligamx |>
  summarise(
    season_goals = season_goals,
    season_goals_90 = player_season_goals_90, 
    np_goals = player_season_npg_90,
    np_xg_90 = player_season_np_xg_90,
    goals_vs_xg = season_goals_90 / np_xg_90,
    .by = c(player_name , team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Delantero") |>
  # mutate(p_rank = dplyr::percent_rank(season_goals)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_goals))

del_goleadores |>
  ggplot(aes(x = season_goals_90, y = np_xg_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Goles de temporada",
    y = "xG (sin penales) de temporada",
    title = "Goles vs xG (sin penales)",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

goleadores_bar <- del_goleadores |>
  arrange(desc(season_goals)) |>
  slice_head(n = 10) |>
  mutate(player_name = fct_reorder(player_name, season_goals)) |>
  ggplot(aes(x = season_goals, y = player_name)) +
  geom_col(fill = "blue3") +
  scale_x_continuous(
    breaks = seq(2, 10, by = 2),
    # limits = c(0, 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Goles esta temporada",
    y = NULL,
    title = "Top 10 goleadores",
    subtitle = "Cada barra representa a un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Asistencias ----

del_asistencias <- jugs_ligamx |>
  summarise(
    season_assists = season_assists,
    season_assists_90 = player_season_assists_90,
    xa_90 = player_season_xa_90,
    assists_vs_xa = season_assists_90 / xa_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Delantero") |>
  # mutate(p_rank = dplyr::percent_rank(season_assists)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_assists))

del_asistencias |>
  ggplot(aes(x = season_assists_90, y = xa_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Asistencias de temporada (por 90 mins jugados)",
    y = "xA de temporada (por 90 mins jugados)",
    title = "Mejores Asistidores en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Delanteros Peligrosos ----

del_peligrosos <- jugs_ligamx |>
  summarise(
    obv_90 = player_season_obv_90,
    shots_key_passes = player_season_shots_key_passes_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Delantero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(shots_key_passes))

del_peligrosos |>
  ggplot(aes(x = obv_90, y = shots_key_passes)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "On-ball Value (por 90 mins jugados)",
    y = "Tiros y Pases Clave (por 90 mins jugados)",
    title = "Jugadores más Peligrosos en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Juego Aéreo ----

aereo_ligamx <- jugs_ligamx |>
  summarise(
    aerial_ratio = player_season_aerial_ratio,
    aerial_wins = player_season_aerial_wins_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Delantero") |>
  # mutate(p_rank = dplyr::percent_rank(aerial_wins)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(aerial_wins))

aereo_ligamx |>
  ggplot(aes(x = aerial_ratio, y = aerial_wins)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "Número de Balones Aéreos Ganados (por 90 mins jugados)",
    x = "% de Balones Aéreos Ganados",
    title = "Jugadores con Mejor Juego Aéreo en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Delanteros Defensivamente ----

defensa_delanteros_ligamx <- jugs_ligamx |>
  summarise(
    presiones_campo_rival = player_season_fhalf_pressures_90,
    recuperaciones_balon = player_season_ball_recoveries_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
    filter(position_group == "Delantero") |>
  # mutate(p_rank = dplyr::percent_rank(recuperaciones_balon)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(recuperaciones_balon))

defensa_delanteros_ligamx |>
  ggplot(aes(x = recuperaciones_balon, y = presiones_campo_rival)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "Número de  Balones Recuperados (por 90 mins jugados)",
    x = "Número de Presiones en Campo Rival (por 90 mins jugados)",
    title = "Balones recuperados vs Presiones en Campo Rival",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Positive Outcomes ----

del_outcomes <- jugs_ligamx |>
  summarise(
    outcome_90 = player_season_positive_outcome_90,
    outcome_score = player_season_positive_outcome_score,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Delantero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(outcome_score))

del_outcomes |>
  ggplot(aes(x = outcome_90, y = outcome_score)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Participación en Posesiones con Resultado Positivo (por 90 mins jugados)",
    y = "Qué tan frecuentemente tiene participación en posesiones positivas?",
    title = "Participación en Posesiones con Resultado Positivo",
    subtitle = "Cada punto es un jugador",
    caption = "Posesiones Positivas se refiere a una posesión que resulta en un tiro, tiro libre en campo contratio, o corner."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Plot de Radar ----

df_del <- jugs_ligamx |> 
  filter(position_group == "Delantero") |> 
  select(player_name, 
         team_name,
         birth_date,
         primary_position,
         player_season_90s_played,
         goals_90 = player_season_goals_90,
         xg_90 = player_season_np_xg_90,
         shots_90 = player_season_np_shots_90,
         shot_obv = player_season_obv_shot_90,
         pass_obv = player_season_obv_pass_90,
         op_xga = player_season_op_xa_90,
         padj_pressures = player_season_padj_pressures_90,
         aerial_wins = player_season_aerial_wins_90,
         turnovers_90 = player_season_turnovers_90,
         d_c_obv_90 = player_season_obv_dribble_carry_90,
         tib_90 = player_season_touches_inside_box_90,
         xg_per_shot = player_season_np_xg_per_shot) |> 
  distinct()

del_radar_clean <- function(player,
                            compare_to = NULL,
                            df = df_del,
                            seg = 6,
                            palette = c("#d62728", "#2ca02c"),
                            border_alpha = 1,
                            fill_alpha = 0.28,
                            label_cex = 0.95,
                            ring_label_cex = 0.85) {
  stopifnot(player %in% df$player_name)
  
  kpis <- c("goals_90","xg_90","shots_90","shot_obv","pass_obv",
            "op_xga","padj_pressures","aerial_wins","turnovers_90",
            "d_c_obv_90","tib_90","xg_per_shot")
  kpi_labels <- c("Goals/90","xG","Shots","Shot OBV","Pass OBV",
                  "Open Play xG Assisted","PAdj Pressures","Aerial Wins/90",
                  "Turnovers (↓)","Dribble & Carry OBV","Touches in Box","xG/Shot")
  
  one_decimal <- c("shots_90","tib_90")
  invert_vars <- c("turnovers_90")
  
  players <- c(player, compare_to); players <- players[!is.na(players)]
  stopifnot(all(players %in% df$player_name))
  
  mins_raw <- df %>% summarise(across(all_of(kpis), ~min(.x, na.rm = TRUE)))
  maxs_raw <- df %>% summarise(across(all_of(kpis), ~max(.x, na.rm = TRUE)))
  mins <- mins_raw; maxs <- maxs_raw
  for (nm in invert_vars) { maxs[[nm]] <- maxs_raw[[nm]][1] - mins_raw[[nm]][1]; mins[[nm]] <- 0 }
  
  df_sel <- df %>% filter(player_name %in% players) %>% select(player_name, all_of(kpis))
  for (nm in invert_vars) df_sel[[nm]] <- maxs_raw[[nm]][1] - df_sel[[nm]]
  
  radar_df <- rbind(maxs %>% select(all_of(kpis)),
                    mins %>% select(all_of(kpis)),
                    df_sel %>% select(all_of(kpis)))
  rownames(radar_df) <- c("max","min", df_sel$player_name)
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE); par(mar = c(1.2,1.2,2,1.2))
  n_pl <- nrow(df_sel)
  cols_border <- scales::alpha(palette[seq_len(n_pl)], border_alpha)
  cols_fill   <- scales::alpha(palette[seq_len(n_pl)], fill_alpha)
  
  fmsb::radarchart(radar_df, axistype = 1, seg = seg,
                   vlabels = kpi_labels, vlcex = label_cex,
                   pcol = cols_border, pfcol = cols_fill, plwd = 2.2, plty = 1,
                   cglcol = "grey85", cglty = 1, cglwd = 1.1,
                   caxislabels = rep("", seg), centerzero = FALSE)
  
  nvar <- length(kpis); theta <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar]
  max_vals <- as.numeric(maxs %>% select(all_of(kpis)) %>% as.matrix())
  xs <- 1.12 * sin(theta); ys <- 1.12 * cos(theta)
  fmt_val <- function(nm, x) {
    if (nm %in% one_decimal) format(round(x, 1), nsmall = 1, trim = TRUE) else
      if (nm %in% c("pass_obv","shot_obv","d_c_obv_90","op_xga","xg_per_shot"))
        format(round(x, 3), nsmall = 3, trim = TRUE) else
          format(round(x, 2), nsmall = 2, trim = TRUE)
  }
  for (i in seq_len(nvar)) text(xs[i], ys[i], labels = fmt_val(kpis[i], max_vals[i]),
                                cex = ring_label_cex, col = "grey30", xpd = NA)
  legend("topright", legend = df_sel$player_name, bty = "n",
         pch = 20, col = palette[seq_len(n_pl)], text.col = "black", pt.cex = 1.6, cex = 0.9)
  title(main = paste0("Radar — Delantero: ", player), cex.main = 1.1)
}

del_radar_clean("Paulinho")

del_radar_clean("Paulinho", compare_to = "João Pedro")

## Centrales ----

### Goles ---

cent_goleadores_ligamx <- jugs_ligamx |>
  summarise(
    season_goals = season_goals,
    season_goals_90 = player_season_goals_90, 
    np_goals = player_season_npg_90,
    np_xg_90 = player_season_np_xg_90,
    goals_vs_xg = season_goals_90 / np_xg_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Central") |>
  # mutate(p_rank = dplyr::percent_rank(season_goals)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_goals))

cent_goleadores_ligamx |>
  ggplot(aes(x = season_goals_90, y = np_xg_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Goles de temporada (por 90 mins jugados)",
    y = "xG (sin penales) de temporada (por 90 mins jugados)",
    title = "Goles vs xG (sin penales)",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Asistencias ----

cent_asistencias_ligamx <- jugs_ligamx |>
  summarise(
    season_assists = season_assists,
    season_assists_90 = player_season_assists_90,
    xa_90 = player_season_xa_90,
    assists_vs_xa = season_assists_90 / xa_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Central") |>
  # mutate(p_rank = dplyr::percent_rank(season_assists)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_assists))

cent_asistencias_ligamx |>
  ggplot(aes(x = season_assists_90, y = xa_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Asistencias de temporada (por 90 mins jugados)",
    y = "xA de temporada (por 90 mins jugados)",
    title = "Mejores Asistidores en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Juego Aéreo ----

cent_aereo_ligamx <- jugs_ligamx |>
  summarise(
    aerial_ratio = player_season_aerial_ratio,
    aerial_wins = player_season_aerial_wins_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Central") |>
  # mutate(p_rank = dplyr::percent_rank(aerial_wins)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(aerial_wins))

cent_aereo_ligamx |>
  ggplot(aes(x = aerial_ratio, y = aerial_wins)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "Número de Balones Aéreos Ganados (por 90 mins jugados)",
    x = "% de Balones Aéreos Ganados",
    title = "Jugadores con Mejor Juego Aéreo en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Acciones agresivas ----

cent_ag <- jugs_ligamx |>
  summarise(
    ag_ac = player_season_aggressive_actions_90,
    tack_int_90 = player_season_tackles_and_interceptions_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Central") |>
  # mutate(p_rank = dplyr::percent_rank(aerial_wins)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(tack_int_90))

cent_ag |>
  ggplot(aes(y = tack_int_90, x = ag_ac)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "Entradas o Intercepciones (por 90 mins jugados)",
    x = "Acciones Agresivas (por 90 mins jugados)",
    title = "Agresividad de Jugadores de Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### OBV x Pases Clave ----

cent_peligrosos_ligamx <- jugs_ligamx |>
  summarise(
    obv_90 = player_season_obv_90,
    shots_key_passes = player_season_shots_key_passes_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Central") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(shots_key_passes))

cent_peligrosos_ligamx |>
  ggplot(aes(x = obv_90, y = shots_key_passes)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "On-ball Value (por 90 mins jugados)",
    y = "Tiros y Pases Clave (por 90 mins jugados)",
    title = "Jugadores más Peligrosos en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Forward Pass x LB Pass ----

cent_pases <- jugs_ligamx |>
  summarise(
    fwd_pases = player_season_forward_pass_proportion,
    lbp_90 = player_season_lbp_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Central") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(fwd_pases))

cent_pases |>
  ggplot(aes(x = fwd_pases, y = lbp_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "% de Pases Hacia el Frente (por 90 mins jugados)",
    y = "Pases que Rompen una Línea Defensiva (por 90 mins jugados)",
    title = "Jugadores más frontales ",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Positive Outcomes ----

cent_outcomes <- jugs_ligamx |>
  summarise(
    outcome_90 = player_season_positive_outcome_90,
    outcome_score = player_season_positive_outcome_score,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Central") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(outcome_score))

cent_outcomes |>
  ggplot(aes(x = outcome_90, y = outcome_score)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Participación en Posesiones con Resultado Positivo (por 90 mins jugados)",
    y = "Qué tan frecuentemente tiene participación en posesiones positivas?",
    title = "Participación en Posesiones con Resultado Positivo",
    subtitle = "Cada punto es un jugador",
    caption = "Posesiones Positivas se refiere a una posesión que resulta en un tiro, tiro libre en campo contratio, o corner."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Plot de Radar ----

df_cent <- jugs_ligamx |> 
  filter(position_group == "Central") |> 
  select(player_name, 
         team_name,
         birth_date,
         primary_position,
         player_season_90s_played,
         goals_90 = player_season_goals_90,
         pressure_pass_ratio = player_season_change_in_passing_ratio,
         obv_90 = player_season_obv_90,
         pass_obv = player_season_obv_pass_90,
         d_c_obv_90 = player_season_obv_dribble_carry_90,
         blocks_per_shot = player_season_blocks_per_shot,
         aerial_wins_90 = player_season_aerial_wins_90,
         aerial_ratio = player_season_aerial_ratio,
         padj_clearances_90 = player_season_padj_clearances_90,
         fouls_90 = player_season_fouls_90,
         challenge_ratio = player_season_challenge_ratio,
         da_obv = player_season_obv_defensive_action_90) |> 
  distinct()

cent_radar_clean <- function(player,
                             compare_to = NULL,
                             df = df_cent,
                             seg = 6,
                             palette = c("#d62728", "#2ca02c"),
                             border_alpha = 1,
                             fill_alpha = 0.28,
                             label_cex = 0.95,
                             ring_label_cex = 0.85) {
  stopifnot(player %in% df$player_name)
  
  kpis <- c("goals_90","pressure_pass_ratio","obv_90","pass_obv","d_c_obv_90",
            "blocks_per_shot","aerial_wins_90","aerial_ratio","padj_clearances_90",
            "fouls_90","challenge_ratio","da_obv")
  kpi_labels <- c("Goals/90","Pressure Pass Ratio","OBV/90","Pass OBV","Dribble & Carry OBV",
                  "Blocks per Shot","Aerial Wins/90","Aerial Win %","PAdj Clearances/90",
                  "Fouls/90","Challenge Ratio","DA OBV")
  
  invert_vars <- character(0)  # none by default
  
  players <- c(player, compare_to); players <- players[!is.na(players)]
  stopifnot(all(players %in% df$player_name))
  
  mins_raw <- df %>% summarise(across(all_of(kpis), ~min(.x, na.rm = TRUE)))
  maxs_raw <- df %>% summarise(across(all_of(kpis), ~max(.x, na.rm = TRUE)))
  mins <- mins_raw; maxs <- maxs_raw
  # (no inversions)
  
  df_sel <- df %>% filter(player_name %in% players) %>% select(player_name, all_of(kpis))
  
  radar_df <- rbind(maxs %>% select(all_of(kpis)),
                    mins %>% select(all_of(kpis)),
                    df_sel %>% select(all_of(kpis)))
  rownames(radar_df) <- c("max","min", df_sel$player_name)
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE); par(mar = c(1.2,1.2,2,1.2))
  n_pl <- nrow(df_sel)
  cols_border <- scales::alpha(palette[seq_len(n_pl)], border_alpha)
  cols_fill   <- scales::alpha(palette[seq_len(n_pl)], fill_alpha)
  
  fmsb::radarchart(radar_df, axistype = 1, seg = seg,
                   vlabels = kpi_labels, vlcex = label_cex,
                   pcol = cols_border, pfcol = cols_fill, plwd = 2.2, plty = 1,
                   cglcol = "grey85", cglty = 1, cglwd = 1.1,
                   caxislabels = rep("", seg), centerzero = FALSE)
  
  nvar <- length(kpis); theta <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar]
  max_vals <- as.numeric(maxs %>% select(all_of(kpis)) %>% as.matrix())
  xs <- 1.12 * sin(theta); ys <- 1.12 * cos(theta)
  fmt_val <- function(nm, x) {
    if (nm %in% c("pass_obv","obv_90","d_c_obv_90","da_obv"))
      format(round(x, 3), nsmall = 3, trim = TRUE) else
        format(round(x, 2), nsmall = 2, trim = TRUE)
  }
  for (i in seq_len(nvar)) text(xs[i], ys[i], labels = fmt_val(kpis[i], max_vals[i]),
                                cex = ring_label_cex, col = "grey30", xpd = NA)
  legend("topright", legend = df_sel$player_name, bty = "n",
         pch = 20, col = palette[seq_len(n_pl)], text.col = "black", pt.cex = 1.6, cex = 0.9)
  title(main = paste0("Radar — Central: ", player), cex.main = 1.1)
}

cent_radar_clean("Israel Reyes")

cent_radar_clean("Israel Reyes", compare_to = "Willer Ditta")

## Laterales/Carrileros ----

### Goles ----

lat_goleadores_ligamx <- jugs_ligamx |>
  summarise(
    season_goals = season_goals,
    season_goals_90 = player_season_goals_90, 
    np_goals = player_season_npg_90,
    np_xg_90 = player_season_np_xg_90,
    goals_vs_xg = season_goals_90 / np_xg_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(season_goals)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_goals))

lat_goleadores_ligamx |>
  ggplot(aes(x = season_goals_90, y = np_xg_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Goles de temporada (por 90 mins jugados)",
    y = "xG (sin penales) de temporada (por 90 mins jugados)",
    title = "Goles vs xG (sin penales)",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Asistencias ----

lat_asistencias_ligamx <- jugs_ligamx |>
  summarise(
    season_assists = season_assists,
    season_assists_90 = player_season_assists_90,
    xa_90 = player_season_xa_90,
    assists_vs_xa = season_assists_90 / xa_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(season_assists)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_assists))

cent_asistencias_ligamx |>
  ggplot(aes(x = season_assists_90, y = xa_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Asistencias de temporada (por 90 mins jugados)",
    y = "xA de temporada (por 90 mins jugados)",
    title = "Mejores Asistidores en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Acciones agresivas ----

lat_ag <- jugs_ligamx |>
  summarise(
    ag_ac = player_season_aggressive_actions_90,
    tack_int_90 = player_season_tackles_and_interceptions_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(aerial_wins)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(tack_int_90))

lat_ag |>
  ggplot(aes(y = tack_int_90, x = ag_ac)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "Entradas o Intercepciones (por 90 mins jugados)",
    x = "Acciones Agresivas (por 90 mins jugados)",
    title = "Agresividad de Jugadores de Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### OBV x Pases Clave ----

lat_peligrosos_ligamx <- jugs_ligamx |>
  summarise(
    obv_90 = player_season_obv_90,
    shots_key_passes = player_season_shots_key_passes_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(shots_key_passes))

lat_peligrosos_ligamx |>
  ggplot(aes(x = obv_90, y = shots_key_passes)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "On-ball Value (por 90 mins jugados)",
    y = "Tiros y Pases Clave (por 90 mins jugados)",
    title = "Jugadores más Peligrosos en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Forward Pass x LB Pass ----

lat_pases <- jugs_ligamx |>
  summarise(
    fwd_pases = player_season_forward_pass_proportion,
    lbp_90 = player_season_lbp_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(fwd_pases))

lat_pases |>
  ggplot(aes(x = fwd_pases, y = lbp_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "% de Pases Hacia el Frente (por 90 mins jugados)",
    y = "Pases que Rompen una Línea Defensiva (por 90 mins jugados)",
    title = "Jugadores más frontales ",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Centros x Pases Clave ----

lat_centros <- jugs_ligamx |>
  summarise(
    cross_ratio = player_season_box_cross_ratio,
    pases_area = player_season_passes_into_box_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(cross_ratio))

lat_centros |>
  ggplot(aes(x = pases_area, y = cross_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "% de pases al área que son centros",
    x = "Pases al Área (por 90 mins jugados)",
    title = "Laterales Que Más Tiran Centros",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Positive Outcomes ----

lat_outcomes <- jugs_ligamx |>
  summarise(
    outcome_90 = player_season_positive_outcome_90,
    outcome_score = player_season_positive_outcome_score,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(outcome_score))

lat_outcomes |>
  ggplot(aes(x = outcome_90, y = outcome_score)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Participación en Posesiones con Resultado Positivo (por 90 mins jugados)",
    y = "Qué tan frecuentemente tiene participación en posesiones positivas?",
    title = "Participación en Posesiones con Resultado Positivo",
    subtitle = "Cada punto es un jugador",
    caption = "Posesiones Positivas se refiere a una posesión que resulta en un tiro, tiro libre en campo contratio, o corner."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Plot de Radar ----

df_lat <- jugs_ligamx |> 
  filter(position_group == "Lateral/Carrilero") |> 
  select(player_name, 
         team_name,
         birth_date,
         primary_position,
         player_season_90s_played,
         goals_90 = player_season_goals_90,
         padj_ti_90 = player_season_tackles_and_interceptions_90,
         deep_progressions_90 = player_season_deep_progressions_90,
         pass_obv = player_season_obv_pass_90,
         op_xga = player_season_op_xa_90,
         d_c_obv_90 = player_season_obv_dribble_carry_90,
         turnovers_90 = player_season_turnovers_90,
         aerial_ratio = player_season_aerial_ratio,
         padj_pressures_90 = player_season_padj_pressures_90,
         fouls_90 = player_season_fouls_90,
         challenge_ratio = player_season_challenge_ratio,
         da_obv = player_season_obv_defensive_action_90) |> 
  distinct()

lat_radar_clean <- function(player,
                            compare_to = NULL,
                            df = df_lat,
                            seg = 6,
                            palette = c("#d62728", "#2ca02c"),
                            border_alpha = 1,
                            fill_alpha = 0.28,
                            label_cex = 0.95,
                            ring_label_cex = 0.85) {
  stopifnot(player %in% df$player_name)
  
  kpis <- c("goals_90","padj_ti_90","deep_progressions_90","pass_obv","op_xga",
            "d_c_obv_90","turnovers_90","aerial_ratio","padj_pressures_90",
            "fouls_90","challenge_ratio","da_obv")
  kpi_labels <- c("Goals/90","PAdj T+I/90","Deep Prog/90","Pass OBV","Open Play xGA",
                  "Dribble & Carry OBV","Turnovers (↓)","Aerial Win %","PAdj Pressures/90",
                  "Fouls/90","Challenge Ratio","DA OBV")
  
  invert_vars <- c("turnovers_90")
  
  players <- c(player, compare_to); players <- players[!is.na(players)]
  stopifnot(all(players %in% df$player_name))
  
  mins_raw <- df %>% summarise(across(all_of(kpis), ~min(.x, na.rm = TRUE)))
  maxs_raw <- df %>% summarise(across(all_of(kpis), ~max(.x, na.rm = TRUE)))
  mins <- mins_raw; maxs <- maxs_raw
  for (nm in invert_vars) { maxs[[nm]] <- maxs_raw[[nm]][1] - mins_raw[[nm]][1]; mins[[nm]] <- 0 }
  
  df_sel <- df %>% filter(player_name %in% players) %>% select(player_name, all_of(kpis))
  for (nm in invert_vars) df_sel[[nm]] <- maxs_raw[[nm]][1] - df_sel[[nm]]
  
  radar_df <- rbind(maxs %>% select(all_of(kpis)),
                    mins %>% select(all_of(kpis)),
                    df_sel %>% select(all_of(kpis)))
  rownames(radar_df) <- c("max","min", df_sel$player_name)
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE); par(mar = c(1.2,1.2,2,1.2))
  n_pl <- nrow(df_sel)
  cols_border <- scales::alpha(palette[seq_len(n_pl)], border_alpha)
  cols_fill   <- scales::alpha(palette[seq_len(n_pl)], fill_alpha)
  
  fmsb::radarchart(radar_df, axistype = 1, seg = seg,
                   vlabels = kpi_labels, vlcex = label_cex,
                   pcol = cols_border, pfcol = cols_fill, plwd = 2.2, plty = 1,
                   cglcol = "grey85", cglty = 1, cglwd = 1.1,
                   caxislabels = rep("", seg), centerzero = FALSE)
  
  nvar <- length(kpis); theta <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar]
  max_vals <- as.numeric(maxs %>% select(all_of(kpis)) %>% as.matrix())
  xs <- 1.12 * sin(theta); ys <- 1.12 * cos(theta)
  fmt_val <- function(nm, x) {
    if (nm %in% c("pass_obv","d_c_obv_90","op_xga","da_obv"))
      format(round(x, 3), nsmall = 3, trim = TRUE) else
        format(round(x, 2), nsmall = 2, trim = TRUE)
  }
  for (i in seq_len(nvar)) text(xs[i], ys[i], labels = fmt_val(kpis[i], max_vals[i]),
                                cex = ring_label_cex, col = "grey30", xpd = NA)
  legend("topright", legend = df_sel$player_name, bty = "n",
         pch = 20, col = palette[seq_len(n_pl)], text.col = "black", pt.cex = 1.6, cex = 0.9)
  title(main = paste0("Radar — Lateral/Carrilero: ", player), cex.main = 1.1)
}

lat_radar_clean("Kevin Álvarez")

lat_radar_clean("Kevin Álvarez", compare_to = "Cristian Borja")

## Medios de Contención ----

### Goles ----

mc_goleadores_ligamx <- jugs_ligamx |>
  summarise(
    season_goals = season_goals,
    season_goals_90 = player_season_goals_90, 
    np_goals = player_season_npg_90,
    np_xg_90 = player_season_np_xg_90,
    goals_vs_xg = season_goals_90 / np_xg_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Medio de Contención") |>
  # mutate(p_rank = dplyr::percent_rank(season_goals)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_goals))

mc_goleadores_ligamx |>
  ggplot(aes(x = season_goals_90, y = np_xg_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Goles de temporada (por 90 mins jugados)",
    y = "xG (sin penales) de temporada (por 90 mins jugados)",
    title = "Goles vs xG (sin penales)",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Asistencias ----

mc_asistencias_ligamx <- jugs_ligamx |>
  summarise(
    season_assists = season_assists,
    season_assists_90 = player_season_assists_90,
    xa_90 = player_season_xa_90,
    assists_vs_xa = season_assists_90 / xa_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Medio de Contención") |>
  # mutate(p_rank = dplyr::percent_rank(season_assists)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_assists))

mc_asistencias_ligamx |>
  ggplot(aes(x = season_assists_90, y = xa_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Asistencias de temporada (por 90 mins jugados)",
    y = "xA de temporada (por 90 mins jugados)",
    title = "Mejores Asistidores en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Juego Aéreo ----

mc_aereo <- jugs_ligamx |>
  summarise(
    aerial_ratio = player_season_aerial_ratio,
    aerial_wins = player_season_aerial_wins_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Medio de Contención") |>
  # mutate(p_rank = dplyr::percent_rank(aerial_wins)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(aerial_wins))

mc_aereo |>
  ggplot(aes(x = aerial_ratio, y = aerial_wins)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "Número de Balones Aéreos Ganados (por 90 mins jugados)",
    x = "% de Balones Aéreos Ganados",
    title = "Jugadores con Mejor Juego Aéreo en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Acciones agresivas ----

mc_ag <- jugs_ligamx |>
  summarise(
    ag_ac = player_season_aggressive_actions_90,
    tack_int_90 = player_season_tackles_and_interceptions_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Medio de Contención") |>
  # mutate(p_rank = dplyr::percent_rank(aerial_wins)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(tack_int_90))

mc_ag |>
  ggplot(aes(y = tack_int_90, x = ag_ac)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "Entradas o Intercepciones (por 90 mins jugados)",
    x = "Acciones Agresivas (por 90 mins jugados)",
    title = "Agresividad de Jugadores de Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### OBV x Pases Clave ----

mc_peligrosos <- jugs_ligamx |>
  summarise(
    obv_90 = player_season_obv_90,
    shots_key_passes = player_season_shots_key_passes_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Medio de Contención") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(shots_key_passes))

mc_peligrosos |>
  ggplot(aes(x = obv_90, y = shots_key_passes)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "On-ball Value (por 90 mins jugados)",
    y = "Tiros y Pases Clave (por 90 mins jugados)",
    title = "Jugadores más Peligrosos en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Forward Pass x LB Pass ----

mc_pases <- jugs_ligamx |>
  summarise(
    fwd_pases = player_season_forward_pass_proportion,
    lbp_90 = player_season_lbp_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Medio de Contención") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(fwd_pases))

lat_pases |>
  ggplot(aes(x = fwd_pases, y = lbp_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "% de Pases Hacia el Frente (por 90 mins jugados)",
    y = "Pases que Rompen una Línea Defensiva (por 90 mins jugados)",
    title = "Jugadores más frontales ",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Carry Ratio x Deep Progressions ----

mc_acarreos <- jugs_ligamx |>
  summarise(
    carry_ratio = player_season_carry_ratio,
    deep_prog_90 = player_season_deep_progressions_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Medio de Contención") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(deep_prog_90))

mc_acarreos |>
  ggplot(aes(x = deep_prog_90, y = carry_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Pases y Acarreos que Entran al Tercio Ofensivo (por 90 mins jugados)",
    y = "% de Acarreos Exitosos",
    title = "Jugadores más frontales ",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Positive Outcomes ----

mc_outcomes <- jugs_ligamx |>
  summarise(
    outcome_90 = player_season_positive_outcome_90,
    outcome_score = player_season_positive_outcome_score,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Medio de Contención") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(outcome_score))

mc_outcomes |>
  ggplot(aes(x = outcome_90, y = outcome_score)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Participación en Posesiones con Resultado Positivo (por 90 mins jugados)",
    y = "Qué tan frecuentemente tiene participación en posesiones positivas?",
    title = "Participación en Posesiones con Resultado Positivo",
    subtitle = "Cada punto es un jugador",
    caption = "Posesiones Positivas se refiere a una posesión que resulta en un tiro, tiro libre en campo contratio, o corner."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Plot de Radar ----

df_mc <- jugs_ligamx |> 
  filter(position_group == "Medio de Contención") |> 
  select(player_name, 
         team_name,
         birth_date,
         primary_position,
         player_season_90s_played,
         goals_90 = player_season_goals_90,
         padj_ti_90 = player_season_tackles_and_interceptions_90,
         deep_progressions_90 = player_season_deep_progressions_90,
         obv_90 = player_season_obv_90,
         pass_obv = player_season_obv_pass_90,
         op_xga = player_season_op_xa_90,
         d_c_obv_90 = player_season_obv_dribble_carry_90,
         turnovers_90 = player_season_turnovers_90,
         padj_pressures_90 = player_season_padj_pressures_90,
         fouls_won_90 = player_season_fouls_won_90,
         challenge_ratio = player_season_challenge_ratio,
         da_obv = player_season_obv_defensive_action_90) |> 
  distinct()

mc_radar_clean <- function(player,
                           compare_to = NULL,
                           df = df_mc,
                           seg = 6,
                           palette = c("#d62728", "#2ca02c"),
                           border_alpha = 1,
                           fill_alpha = 0.28,
                           label_cex = 0.95,
                           ring_label_cex = 0.85) {
  stopifnot(player %in% df$player_name)
  
  kpis <- c("goals_90","padj_ti_90","deep_progressions_90","obv_90",
            "pass_obv","op_xga","d_c_obv_90","turnovers_90",
            "padj_pressures_90","fouls_won_90","challenge_ratio","da_obv")
  kpi_labels <- c("Goals/90","PAdj T+I/90","Deep Prog/90","OBV/90","Pass OBV",
                  "Open Play xGA","Dribble & Carry OBV","Turnovers (↓)",
                  "PAdj Pressures/90","Fouls Won/90","Challenge Ratio","DA OBV")
  
  one_decimal <- c("goals_90","deep_progressions_90")
  invert_vars <- c("turnovers_90")
  
  players <- c(player, compare_to); players <- players[!is.na(players)]
  stopifnot(all(players %in% df$player_name))
  
  mins_raw <- df %>% summarise(across(all_of(kpis), ~min(.x, na.rm = TRUE)))
  maxs_raw <- df %>% summarise(across(all_of(kpis), ~max(.x, na.rm = TRUE)))
  mins <- mins_raw; maxs <- maxs_raw
  for (nm in invert_vars) { maxs[[nm]] <- maxs_raw[[nm]][1] - mins_raw[[nm]][1]; mins[[nm]] <- 0 }
  
  df_sel <- df %>% filter(player_name %in% players) %>% select(player_name, all_of(kpis))
  for (nm in invert_vars) df_sel[[nm]] <- maxs_raw[[nm]][1] - df_sel[[nm]]
  
  radar_df <- rbind(maxs %>% select(all_of(kpis)),
                    mins %>% select(all_of(kpis)),
                    df_sel %>% select(all_of(kpis)))
  rownames(radar_df) <- c("max","min", df_sel$player_name)
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE); par(mar = c(1.2,1.2,2,1.2))
  n_pl <- nrow(df_sel)
  cols_border <- scales::alpha(palette[seq_len(n_pl)], border_alpha)
  cols_fill   <- scales::alpha(palette[seq_len(n_pl)], fill_alpha)
  
  fmsb::radarchart(radar_df, axistype = 1, seg = seg,
                   vlabels = kpi_labels, vlcex = label_cex,
                   pcol = cols_border, pfcol = cols_fill, plwd = 2.2, plty = 1,
                   cglcol = "grey85", cglty = 1, cglwd = 1.1,
                   caxislabels = rep("", seg), centerzero = FALSE)
  
  nvar <- length(kpis); theta <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar]
  max_vals <- as.numeric(maxs %>% select(all_of(kpis)) %>% as.matrix())
  xs <- 1.12 * sin(theta); ys <- 1.12 * cos(theta)
  fmt_val <- function(nm, x) {
    if (nm %in% one_decimal) format(round(x, 1), nsmall = 1, trim = TRUE) else
      if (nm %in% c("pass_obv","obv_90","d_c_obv_90","op_xga","da_obv"))
        format(round(x, 3), nsmall = 3, trim = TRUE) else
          format(round(x, 2), nsmall = 2, trim = TRUE)
  }
  for (i in seq_len(nvar)) text(xs[i], ys[i], labels = fmt_val(kpis[i], max_vals[i]),
                                cex = ring_label_cex, col = "grey30", xpd = NA)
  legend("topright", legend = df_sel$player_name, bty = "n",
         pch = 20, col = palette[seq_len(n_pl)], text.col = "black", pt.cex = 1.6, cex = 0.9)
  title(main = paste0("Radar — Medio de Contención: ", player), cex.main = 1.1)
}

# mc_radar_clean("Erik Lira")
# 
# mc_radar_clean("Erik Lira", compare_to = "Álvaro Fidalgo")

## Interiores ----

### Goles ----

int_goleadores <- jugs_ligamx |>
  summarise(
    season_goals = season_goals,
    season_goals_90 = player_season_goals_90, 
    np_goals = player_season_npg_90,
    np_xg_90 = player_season_np_xg_90,
    goals_vs_xg = season_goals_90 / np_xg_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Interior/Mediapunta") |>
  # mutate(p_rank = dplyr::percent_rank(season_goals)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_goals))

int_goleadores |>
  ggplot(aes(x = season_goals_90, y = np_xg_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Goles de temporada (por 90 mins jugados)",
    y = "xG (sin penales) de temporada (por 90 mins jugados)",
    title = "Goles vs xG (sin penales)",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Asistencias ----

int_asistencias <- jugs_ligamx |>
  summarise(
    season_assists = season_assists,
    season_assists_90 = player_season_assists_90,
    xa_90 = player_season_xa_90,
    assists_vs_xa = season_assists_90 / xa_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Interior/Mediapunta") |>
  # mutate(p_rank = dplyr::percent_rank(season_assists)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_assists))

int_asistencias |>
  ggplot(aes(x = season_assists_90, y = xa_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Asistencias de temporada (por 90 mins jugados)",
    y = "xA de temporada (por 90 mins jugados)",
    title = "Mejores Asistidores en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### OBV x Pases Clave ----

int_peligrosos <- jugs_ligamx |>
  summarise(
    obv_90 = player_season_obv_90,
    shots_key_passes = player_season_shots_key_passes_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Interior/Mediapunta") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(shots_key_passes))

int_peligrosos |>
  ggplot(aes(x = obv_90, y = shots_key_passes)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "On-ball Value (por 90 mins jugados)",
    y = "Tiros y Pases Clave (por 90 mins jugados)",
    title = "Jugadores más Peligrosos en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Forward Pass x LB Pass ----

int_pases <- jugs_ligamx |>
  summarise(
    fwd_pases = player_season_forward_pass_proportion,
    lbp_90 = player_season_lbp_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Interior/Mediapunta") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(fwd_pases))

int_pases |>
  ggplot(aes(x = fwd_pases, y = lbp_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "% de Pases Hacia el Frente (por 90 mins jugados)",
    y = "Pases que Rompen una Línea Defensiva (por 90 mins jugados)",
    title = "Jugadores más frontales ",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Passes into Box x Through Balls ----

int_pases2 <- jugs_ligamx |>
  summarise(
    pases_area_90 = player_season_passes_into_box_90,
    through_balls_90 = player_season_through_balls_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Interior/Mediapunta") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(pases_area_90))

int_pases2 |>
  ggplot(aes(x = pases_area_90, y = through_balls_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Pases al Área (por 90 mins jugados)",
    y = "Pases Filtrados (por 90 mins jugados)",
    title = "Jugadores con Pases más Agresivos",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Dribbles ----

int_drib <- jugs_ligamx |>
  summarise(
    dribble_ratio = player_season_dribble_ratio,
    dribbles_90 = player_season_dribbles_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Interior/Mediapunta") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(dribble_ratio))

int_drib |>
  ggplot(aes(x = dribbles_90, y = dribble_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Dribles Exitosos (por 90 mins jugados)",
    y = "% de Dribles Exitosos",
    title = "Jugadores más Dribladores",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Defensivamente ----

int_def <- jugs_ligamx |>
  summarise(
    def_actions_90 = player_season_defensive_actions_90,
    press_90 = player_season_pressures_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Interior/Mediapunta") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(def_actions_90))

int_def |>
  ggplot(aes(x = def_actions_90, y = press_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Número de Acciones Defensivos (por 90 mins jugados)",
    y = "Número de Presiones (por 90 mins jugados)",
    title = "Jugadores más Dribladores",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Positive Outcomes ----

int_outcomes <- jugs_ligamx |>
  summarise(
    outcome_90 = player_season_positive_outcome_90,
    outcome_score = player_season_positive_outcome_score,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Interior/Mediapunta") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(outcome_score))

int_outcomes |>
  ggplot(aes(x = outcome_90, y = outcome_score)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Participación en Posesiones con Resultado Positivo (por 90 mins jugados)",
    y = "Qué tan frecuentemente tiene participación en posesiones positivas?",
    title = "Participación en Posesiones con Resultado Positivo",
    subtitle = "Cada punto es un jugador",
    caption = "Posesiones Positivas se refiere a una posesión que resulta en un tiro, tiro libre en campo contratio, o corner."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Plot de Radar ----

df_int <- jugs_ligamx |> 
  filter(position_group == "Interior/Mediapunta") |> 
  select(player_name, 
         team_name,
         birth_date,
         primary_position,
         player_season_90s_played,
         goals_90 = player_season_goals_90,
         xg_90 = player_season_np_xg_90,
         shots_90 = player_season_np_shots_90,
         shot_obv = player_season_obv_shot_90,
         pass_obv = player_season_obv_pass_90,
         op_xga = player_season_op_xa_90,
         padj_pressures = player_season_padj_pressures_90,
         fouls_won_90 = player_season_fouls_won_90,
         turnovers_90 = player_season_turnovers_90,
         d_c_obv_90 = player_season_obv_dribble_carry_90,
         tib_90 = player_season_touches_inside_box_90,
         xg_per_shot = player_season_np_xg_per_shot) |> 
  distinct()

int_radar_clean <- function(player,
                            compare_to = NULL,
                            df = df_int,
                            seg = 6,
                            palette = c("#d62728", "#2ca02c"),
                            border_alpha = 1,
                            fill_alpha = 0.28,
                            label_cex = 0.95,
                            ring_label_cex = 0.85) {
  stopifnot(player %in% df$player_name)
  
  kpis <- c("xg_90","shots_90","shot_obv","padj_pressures",
            "pass_obv","op_xga","d_c_obv_90","fouls_won_90",
            "turnovers_90","tib_90","xg_per_shot")
  kpi_labels <- c("xG","Shots","Shot OBV","PAdj Pressures",
                  "Pass OBV","Open Play xG Assisted","Dribble & Carry OBV",
                  "Fouls Won","Turnovers (↓)","Touches in Box","xG/Shot")
  
  as_integer  <- character(0)
  one_decimal <- c("shots_90","tib_90")
  invert_vars <- c("turnovers_90")
  
  players <- c(player, compare_to); players <- players[!is.na(players)]
  stopifnot(all(players %in% df$player_name))
  
  mins_raw <- df %>% summarise(across(all_of(kpis), ~min(.x, na.rm = TRUE)))
  maxs_raw <- df %>% summarise(across(all_of(kpis), ~max(.x, na.rm = TRUE)))
  
  mins <- mins_raw; maxs <- maxs_raw
  for (nm in invert_vars) { maxs[[nm]] <- maxs_raw[[nm]][1] - mins_raw[[nm]][1]; mins[[nm]] <- 0 }
  
  df_sel <- df %>% filter(player_name %in% players) %>% select(player_name, all_of(kpis))
  for (nm in invert_vars) df_sel[[nm]] <- maxs_raw[[nm]][1] - df_sel[[nm]]
  
  radar_df <- rbind(maxs %>% select(all_of(kpis)),
                    mins %>% select(all_of(kpis)),
                    df_sel %>% select(all_of(kpis)))
  rownames(radar_df) <- c("max","min", df_sel$player_name)
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE); par(mar = c(1.2,1.2,2,1.2))
  n_pl <- nrow(df_sel)
  cols_border <- scales::alpha(palette[seq_len(n_pl)], border_alpha)
  cols_fill   <- scales::alpha(palette[seq_len(n_pl)], fill_alpha)
  
  fmsb::radarchart(radar_df, axistype = 1, seg = seg,
                   vlabels = kpi_labels, vlcex = label_cex,
                   pcol = cols_border, pfcol = cols_fill, plwd = 2.2, plty = 1,
                   cglcol = "grey85", cglty = 1, cglwd = 1.1,
                   caxislabels = rep("", seg), centerzero = FALSE)
  
  nvar <- length(kpis); theta <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar]
  max_vals <- as.numeric(maxs %>% select(all_of(kpis)) %>% as.matrix())
  xs <- 1.12 * sin(theta); ys <- 1.12 * cos(theta)
  fmt_val <- function(nm, x) {
    if (nm %in% one_decimal) format(round(x, 1), nsmall = 1, trim = TRUE) else
      if (nm %in% c("pass_obv","shot_obv","d_c_obv_90","op_xga","xg_per_shot"))
        format(round(x, 3), nsmall = 3, trim = TRUE) else
          format(round(x, 2), nsmall = 2, trim = TRUE)
  }
  for (i in seq_len(nvar)) text(xs[i], ys[i], labels = fmt_val(kpis[i], max_vals[i]),
                                cex = ring_label_cex, col = "grey30", xpd = NA)
  legend("topright", legend = df_sel$player_name, bty = "n",
         pch = 20, col = palette[seq_len(n_pl)], text.col = "black", pt.cex = 1.6, cex = 0.9)
  title(main = paste0("Radar — Interior/Mediapunta: ", player), cex.main = 1.1)
}

# int_radar_clean("Ángel Correa")
# 
# int_radar_clean("Ángel Correa", compare_to = "José Paradela")

## Volantes/Extremos ----

### Goles ----

vol_goleadores <- jugs_ligamx |>
  summarise(
    season_goals = season_goals,
    season_goals_90 = player_season_goals_90, 
    np_goals = player_season_npg_90,
    np_xg_90 = player_season_np_xg_90,
    goals_vs_xg = season_goals_90 / np_xg_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Volante/Extremo") |>
  # mutate(p_rank = dplyr::percent_rank(season_goals)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_goals))

vol_goleadores |>
  ggplot(aes(x = season_goals_90, y = np_xg_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Goles de temporada (por 90 mins jugados)",
    y = "xG (sin penales) de temporada (por 90 mins jugados)",
    title = "Goles vs xG (sin penales)",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Asistencias ----

vol_asistencias <- jugs_ligamx |>
  summarise(
    season_assists = season_assists,
    season_assists_90 = player_season_assists_90,
    xa_90 = player_season_xa_90,
    assists_vs_xa = season_assists_90 / xa_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Volante/Extremo") |>
  # mutate(p_rank = dplyr::percent_rank(season_assists)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(season_assists))

vol_asistencias |>
  ggplot(aes(x = season_assists_90, y = xa_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Asistencias de temporada (por 90 mins jugados)",
    y = "xA de temporada (por 90 mins jugados)",
    title = "Mejores Asistidores en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### OBV x Pases Clave ----

vol_peligrosos <- jugs_ligamx |>
  summarise(
    obv_90 = player_season_obv_90,
    shots_key_passes = player_season_shots_key_passes_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Volante/Extremo") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(shots_key_passes))

vol_peligrosos |>
  ggplot(aes(x = obv_90, y = shots_key_passes)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "On-ball Value (por 90 mins jugados)",
    y = "Tiros y Pases Clave (por 90 mins jugados)",
    title = "Jugadores más Peligrosos en Liga MX",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Centros x Pases Clave ----

vol_centros <- jugs_ligamx |>
  summarise(
    cross_ratio = player_season_box_cross_ratio,
    pases_area = player_season_passes_into_box_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(cross_ratio))

vol_centros |>
  ggplot(aes(x = pases_area, y = cross_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "% de pases al área que son centros",
    x = "Pases al Área (por 90 mins jugados)",
    title = "Laterales Que Más Tiran Centros",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Centros ----

vol_centros2 <- jugs_ligamx |>
  summarise(
    cross_90 = player_season_crosses_90,
    cross_ratio = player_season_crossing_ratio,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Lateral/Carrilero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(cross_90))

vol_centros2 |>
  ggplot(aes(x = cross_90, y = cross_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "% de Centros Exitosos",
    x = "Centros Tirados (por 90 mins jugados)",
    title = "Laterales Que Más Tiran Centros",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Dribbles ----

vol_drib <- jugs_ligamx |>
  summarise(
    dribble_ratio = player_season_dribble_ratio,
    dribbles_90 = player_season_dribbles_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Volante/Extremo") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(dribble_ratio))

vol_drib |>
  ggplot(aes(x = dribbles_90, y = dribble_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Dribles Exitosos (por 90 mins jugados)",
    y = "% de Dribles Exitosos",
    title = "Jugadores más Dribladores",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Defensivamente ----

vol_def <- jugs_ligamx |>
  summarise(
    def_actions_90 = player_season_defensive_actions_90,
    press_90 = player_season_pressures_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Volante/Extremo") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(def_actions_90))

vol_def |>
  ggplot(aes(x = def_actions_90, y = press_90)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Número de Acciones Defensivos (por 90 mins jugados)",
    y = "Número de Presiones (por 90 mins jugados)",
    title = "Jugadores más Dribladores",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Positive Outcomes ----

vol_outcomes <- jugs_ligamx |>
  summarise(
    outcome_90 = player_season_positive_outcome_90,
    outcome_score = player_season_positive_outcome_score,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Volante/Extremo") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(outcome_score))

vol_outcomes |>
  ggplot(aes(x = outcome_90, y = outcome_score)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Participación en Posesiones con Resultado Positivo (por 90 mins jugados)",
    y = "Qué tan frecuentemente tiene participación en posesiones positivas?",
    title = "Participación en Posesiones con Resultado Positivo",
    subtitle = "Cada punto es un jugador",
    caption = "Posesiones Positivas se refiere a una posesión que resulta en un tiro, tiro libre en campo contratio, o corner."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Plot de Radar ----

df_vol <- jugs_ligamx |> 
  filter(position_group == "Volante/Extremo") |> 
  select(player_name, 
         team_name,
         birth_date,
         primary_position,
         player_season_90s_played,
         goals_90 = player_season_goals_90,
         xg_90 = player_season_np_xg_90,
         shots_90 = player_season_np_shots_90,
         shot_obv = player_season_obv_shot_90,
         pass_obv = player_season_obv_pass_90,
         op_xga = player_season_op_xa_90,
         padj_pressures = player_season_padj_pressures_90,
         fouls_won_90 = player_season_fouls_won_90,
         turnovers_90 = player_season_turnovers_90,
         d_c_obv_90 = player_season_obv_dribble_carry_90,
         tib_90 = player_season_touches_inside_box_90,
         xg_per_shot = player_season_np_xg_per_shot) |> 
  distinct()

vol_radar_clean <- function(player,
                            compare_to = NULL,
                            df = df_vol,
                            seg = 6,
                            palette = c("#d62728", "#2ca02c"),  # red, green
                            border_alpha = 1,
                            fill_alpha = 0.28,
                            label_cex = 0.95,
                            ring_label_cex = 0.85) {
  stopifnot(player %in% df$player_name)
  
  # 1) KPIs in clockwise order (as on the figure)
  kpis <- c("xg_90", "shots_90", "shot_obv", "padj_pressures",
            "pass_obv", "op_xga", "d_c_obv_90", "fouls_won_90",
            "turnovers_90", "tib_90", "xg_per_shot")
  
  # 2) Labels for spokes
  kpi_labels <- c("xG", "Shots", "Shot OBV", "PAdj Pressures", "Pass OBV",
                  "Open Play xG Assisted", "Dribble & Carry OBV", "Fouls Won",
                  "Turnovers (↓ better)", "Touches in Box", "xG/Shot")
  
  # 3) Formatting rules
  as_integer   <- c("padj_pressures")
  one_decimal  <- c("shots_90", "tib_90")
  percent_vars <- character(0)
  invert_vars  <- c("turnovers_90")
  
  # which players to draw (single or compare)
  players <- c(player, compare_to)
  players <- players[!is.na(players)]
  stopifnot(all(players %in% df$player_name))
  
  # -------- global bounds from full cohort (keeps axes consistent) --------
  mins_raw <- df %>% summarise(across(all_of(kpis), ~min(.x, na.rm = TRUE)))
  maxs_raw <- df %>% summarise(across(all_of(kpis), ~max(.x, na.rm = TRUE)))
  
  # invert limits so bigger = better on “turnovers”
  mins <- mins_raw; maxs <- maxs_raw
  for (nm in invert_vars) {
    max_v <- maxs_raw[[nm]][1]; min_v <- mins_raw[[nm]][1]
    maxs[[nm]] <- max_v - min_v
    mins[[nm]] <- 0
  }
  
  # player rows + invert turnovers
  df_sel <- df %>%
    filter(player_name %in% players) %>%
    select(player_name, all_of(kpis))
  for (nm in invert_vars) {
    max_v <- maxs_raw[[nm]][1]
    df_sel[[nm]] <- max_v - df_sel[[nm]]
  }
  
  # -------- fmsb input: MAX, MIN, players --------
  radar_df <- rbind(
    maxs %>% select(all_of(kpis)),
    mins %>% select(all_of(kpis)),
    df_sel %>% select(all_of(kpis))
  )
  rownames(radar_df) <- c("max", "min", df_sel$player_name)
  
  # -------- draw --------
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  par(mar = c(1.2, 1.2, 2, 1.2))
  
  # >>> USE REQUESTED COLORS <<<
  n_pl <- nrow(df_sel)
  cols_border <- scales::alpha(palette[seq_len(n_pl)], border_alpha)
  cols_fill   <- scales::alpha(palette[seq_len(n_pl)], fill_alpha)
  
  fmsb::radarchart(
    radar_df,
    axistype = 1,
    seg = seg,
    vlabels = kpi_labels,
    vlcex = label_cex,
    pcol = cols_border,
    pfcol = cols_fill,
    plwd = 2.2,
    plty = 1,
    cglcol = "grey85",
    cglty = 1,
    cglwd = 1.1,
    caxislabels = rep("", seg),  # hide inner ring numbers
    centerzero = FALSE
  )
  
  # Clean outer tick labels (max per spoke), formatted nicely
  nvar <- length(kpis)
  theta <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar]
  max_vals <- as.numeric(maxs %>% select(all_of(kpis)) %>% as.matrix())
  r <- 1.12
  xs <- r * sin(theta)
  ys <- r * cos(theta)
  
  fmt_val <- function(nm, x) {
    if (nm %in% as_integer) {
      format(round(x, 0), nsmall = 0, trim = TRUE, scientific = FALSE)
    } else if (nm %in% one_decimal) {
      format(round(x, 1), nsmall = 1, trim = TRUE, scientific = FALSE)
    } else if (nm %in% c("pass_obv","shot_obv","d_c_obv_90","op_xga")) {
      format(round(x, 3), nsmall = 3, trim = TRUE, scientific = FALSE)
    } else {
      format(round(x, 2), nsmall = 2, trim = TRUE, scientific = FALSE)
    }
  }
  
  for (i in seq_len(nvar)) {
    text(xs[i], ys[i],
         labels = fmt_val(kpis[i], max_vals[i]),
         cex = ring_label_cex, col = "grey30", xpd = NA)
  }
  
  legend("topright", legend = df_sel$player_name, bty = "n",
         pch = 20, col = palette[seq_len(n_pl)], text.col = "black",
         pt.cex = 1.6, cex = 0.9)
  
  title(main = paste0("Radar — Volante/Extremo: ", player), cex.main = 1.1)
}

vol_radar_clean("Alejandro Zendejas")

vol_radar_clean("Alejandro Zendejas", compare_to = "Sergio Canales")

## Porteros ----

### Goles Recibidos x xS ----

por_goles <- jugs_ligamx |>
  summarise(
    goles_recibidos = player_season_goals_faced_90,
    xg_en_contra = player_season_np_xg_faced_90,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Portero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(goles_recibidos))

por_goles |>
  ggplot(aes(x = goles_recibidos, y = xg_en_contra)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Goles Recibidos (por 90 mins jugados)",
    y = "xG en Contra (por 90 mins jugados)",
    title = "Goles recibidos vs cuantos goles debió haber recibido",
    subtitle = "Cada punto es un jugador"
    ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Save ratio vs xS ratio ----

por_save <- jugs_ligamx |>
  summarise(
    xs_ratio = player_season_xs_ratio,
    save_ratio = player_season_save_ratio,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Portero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(save_ratio))

por_save |>
  ggplot(aes(x = save_ratio, y = xs_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "% de Tiros Parados",
    y = "Expectativa de Tiros Parados",
    title = "Tiros Parados vs cuantos Tiros Debió Haber Parado",
    subtitle = "Cada punto es un jugador"
    ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### GSAA ----

por_gsaa <- jugs_ligamx |>
  summarise(
    gsaa = player_season_gsaa_90,
    gsaa_ratio = player_season_gsaa_ratio,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Portero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(gsaa))

por_gsaa |>
  ggplot(aes(x = gsaa, y = gsaa_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Goles prevenidos vs expectativa (PSxG enfrentado)",
    y = "% de goles prevenidos sobre la media (de tiros enfrentados)",
    title = "Goals Saved Above Average",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Pases ----

por_pases <- jugs_ligamx |>
  summarise(
    lg = player_season_long_balls_90,
    lg_ratio = player_season_long_ball_ratio,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Portero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(lg_ratio))

por_pases |>
  ggplot(aes(x = lg, y = lg_ratio)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    x = "Número de Pases Largos (por 90 mins jugados)",
    y = "% de Pases Largos Completos",
    title = "Que Tan Exitosos Son Los Pases Largos del Portero?",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Acciones Defensivas (Portero cortando centros y saliendo de su área) ----

por_def <- jugs_ligamx |>
  summarise(
    clcaa = player_season_clcaa,
    da_ag = player_season_da_aggressive_distance,
    .by = c(player_name, team_name, primary_position, position_group)
  ) |>
  filter(position_group == "Portero") |>
  # mutate(p_rank = dplyr::percent_rank(shots_key_passes)) |>
  # filter(p_rank >= 0.6666666) |>   # keep top 33% (>= 67th percentile)
  # select(-p_rank) |>
  arrange(desc(clcaa))

por_def |>
  ggplot(aes(x = clcaa, y = da_ag)) +
  geom_point(size = 2.6, alpha = 0.75, color = "blue3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey45") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    y = "Que tan lejos de su portería llega el portero para hacer una acción defensiva (en metros)",
    x = "CLCAA% (intentos de salir por pases/centros sobre la media)",
    title = "Efectividad de Acciones Defensivas de Porteros",
    subtitle = "Cada punto es un jugador"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 12, 10, 12)
  )

### Plot de Radar ----

df_por <- jugs_ligamx |> 
  filter(position_group == "Portero") |> 
  select(player_name, 
         team_name,
         birth_date,
         primary_position,
         player_season_90s_played,
         gk_obv_90 = player_season_obv_gk_90,
         long_ball_ratio = player_season_long_ball_ratio,
         pass_danger_ratio = player_season_pass_into_danger_ratio,
         clcaa = player_season_clcaa,
         gk_agg_dist = player_season_da_aggressive_distance,
         positioning_error = player_season_np_optimal_gk_dlength,
         save_ratio = player_season_save_ratio) |> 
  distinct()

por_radar_clean <- function(player,
                            compare_to = NULL,
                            df = df_por,
                            seg = 6,
                            palette = c("#d62728", "#2ca02c"),
                            border_alpha = 1,
                            fill_alpha = 0.28,
                            label_cex = 0.95,
                            ring_label_cex = 0.85) {
  stopifnot(player %in% df$player_name)
  
  # Clockwise KPIs
  kpis <- c("gk_obv_90", "long_ball_ratio", "pass_danger_ratio",
            "clcaa", "gk_agg_dist", "positioning_error", "save_ratio")
  
  # Labels
  kpi_labels <- c("GK OBV/90", "Long Ball Ratio", "Pass into Danger Ratio",
                  "CLCAA", "Aggressive Distance", "Positioning Error (↓)", "Save Ratio")
  
  # Formatting & inversions
  as_integer  <- character(0)
  one_decimal <- character(0)
  invert_vars <- c("positioning_error")  # lower is better
  
  players <- c(player, compare_to); players <- players[!is.na(players)]
  stopifnot(all(players %in% df$player_name))
  
  mins_raw <- df %>% summarise(across(all_of(kpis), ~min(.x, na.rm = TRUE)))
  maxs_raw <- df %>% summarise(across(all_of(kpis), ~max(.x, na.rm = TRUE)))
  
  mins <- mins_raw; maxs <- maxs_raw
  for (nm in invert_vars) {
    max_v <- maxs_raw[[nm]][1]; mins[[nm]] <- 0; maxs[[nm]] <- max_v - mins_raw[[nm]][1]
  }
  
  df_sel <- df %>% filter(player_name %in% players) %>% select(player_name, all_of(kpis))
  for (nm in invert_vars) df_sel[[nm]] <- maxs_raw[[nm]][1] - df_sel[[nm]]
  
  radar_df <- rbind(maxs %>% select(all_of(kpis)),
                    mins %>% select(all_of(kpis)),
                    df_sel %>% select(all_of(kpis)))
  rownames(radar_df) <- c("max", "min", df_sel$player_name)
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  par(mar = c(1.2, 1.2, 2, 1.2))
  
  n_pl <- nrow(df_sel)
  cols_border <- scales::alpha(palette[seq_len(n_pl)], border_alpha)
  cols_fill   <- scales::alpha(palette[seq_len(n_pl)], fill_alpha)
  
  fmsb::radarchart(radar_df,
                   axistype = 1, seg = seg,
                   vlabels = kpi_labels, vlcex = label_cex,
                   pcol = cols_border, pfcol = cols_fill, plwd = 2.2, plty = 1,
                   cglcol = "grey85", cglty = 1, cglwd = 1.1,
                   caxislabels = rep("", seg), centerzero = FALSE)
  
  # Outer labels
  nvar <- length(kpis)
  theta <- seq(0, 2*pi, length.out = nvar + 1)[1:nvar]
  max_vals <- as.numeric(maxs %>% select(all_of(kpis)) %>% as.matrix())
  r <- 1.12; xs <- r * sin(theta); ys <- r * cos(theta)
  fmt_val <- function(nm, x) format(round(x, 2), nsmall = 2, trim = TRUE, scientific = FALSE)
  for (i in seq_len(nvar)) text(xs[i], ys[i], labels = fmt_val(kpis[i], max_vals[i]),
                                cex = ring_label_cex, col = "grey30", xpd = NA)
  
  legend("topright", legend = df_sel$player_name, bty = "n",
         pch = 20, col = palette[seq_len(n_pl)], text.col = "black", pt.cex = 1.6, cex = 0.9)
  title(main = paste0("Radar — Portero: ", player), cex.main = 1.1)
}

por_radar_clean("Keylor Navas")

por_radar_clean("Keylor Navas", compare_to = "Camilo Vargas")


# CONCACAF CHAMPIONS LEAGUE ----

jugs_ccl <- jugs_ccl|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_ccl <- goles_ccl |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_ccl <- jugs_ccl |>
  # left_join(pos_ccl, by = c("player_id")) |>
  left_join(goles_ccl, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA ARGENTINA ----

jugs_arg <- jugs_arg|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_arg <- goles_arg |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_arg <- jugs_arg |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_arg, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# BRASILEIRAO SERIE A ----

jugs_brasil <- jugs_brasil|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_brasil <- goles_brasil |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_brasil <- jugs_brasil |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_brasil, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA COLOMBIANA ----

jugs_colombia <- jugs_colombia|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_colombia <- goles_colombia |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_colombia <- jugs_colombia |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_colombia, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA ECUATORIANA ----

jugs_ecuador <- jugs_ecuador |>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_ecuador <- goles_ecuador |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_ecuador <- jugs_ecuador |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_ecuador, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA CHILENA ----

jugs_chile <- jugs_chile|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_chile <- goles_chile |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_chile <- jugs_chile |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_chile, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA PARAGUAYA ----

jugs_paraguay <- jugs_paraguay|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_paraguay <- goles_paraguay |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_paraguay <- jugs_paraguay |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_paraguay, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA URUGUAYA ----

jugs_uruguay <- jugs_uruguay|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_uruguay <- goles_uruguay |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_uruguay <- jugs_uruguay |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_uruguay, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# MLS ----

jugs_mls <- jugs_mls|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_mls <- goles_mls |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_mls <- jugs_mls |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_mls, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# PREMIER LEAGUE ----

jugs_premier <- jugs_premier|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_premier <- goles_premier |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_premier <- jugs_premier |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_premier, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# EFL CHAMPIONSHIP ----

jugs_championship <- jugs_championship|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_championship <- goles_championship |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_championship <- jugs_championship |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_championship, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LA LIGA ----

jugs_laliga <- jugs_laliga|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_laliga <- goles_laliga |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_laliga <- jugs_laliga |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_laliga, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LA LIGA 2 ----

jugs_laliga_2 <- jugs_laliga_2|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_laliga_2 <- goles_laliga_2 |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_laliga_2 <- jugs_laliga_2 |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_laliga_2, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# SERIE A ----

jugs_serie_a <- jugs_serie_a|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_serie_a <- goles_serie_a |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_serie_a <- jugs_serie_a |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_serie_a, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# SERIE B ----

jugs_serie_b <- jugs_serie_b|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_serie_b <- goles_serie_b |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_serie_b <- jugs_serie_b |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_serie_b, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# BUNDESLIGA ----

jugs_bundesliga <- jugs_bundesliga|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_bundesliga <- goles_bundesliga |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_bundesliga <- jugs_bundesliga |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_bundesliga, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)
 
# BUNDESLIGA 2 ----

jugs_bundesliga_2 <- jugs_bundesliga_2|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_bundesliga_2 <- goles_bundesliga_2 |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_bundesliga_2 <- jugs_bundesliga_2 |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_bundesliga_2, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGUE 1 ----

jugs_ligue_1 <- jugs_ligue_1|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_ligue_1 <- goles_ligue_1 |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_ligue_1 <- jugs_ligue_1 |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_ligue_1, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# EREDIVISIE ----

jugs_eredivisie <- jugs_eredivisie|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_eredivisie <- goles_eredivisie |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_eredivisie <- jugs_eredivisie |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_eredivisie, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA BELGA (JUPILER PRO LEAGUE) ----

jugs_belgica <- jugs_belgica|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_belgica <- goles_belgica |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_belgica <- jugs_belgica |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_belgica, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA PORTUGUESA (PRIMEIRA LIGA) ----

jugs_portugal <- jugs_portugal|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_portugal <- goles_portugal |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_portugal <- jugs_portugal |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_portugal, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA TURCA (SUPER LIG) ----

jugs_turquia <- jugs_turquia|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_turquia <- goles_turquia |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_turquia <- jugs_turquia |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_turquia, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# LIGA ESCOCESA (PREMIERSHIP) ----

jugs_escocia <- jugs_escocia|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_escocia <- goles_escocia |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_escocia <- jugs_escocia |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_escocia, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# UEFA CHAMPIONS LEAGUE ----

jugs_champions <- jugs_champions|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_champions <- goles_champions |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_champions <- jugs_champions |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_champions, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# COPA LIBERTADORES ----

jugs_libertadores <- jugs_libertadores|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_libertadores <- goles_libertadores |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_libertadores <- jugs_libertadores |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_libertadores, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# UEFA EUROPA LEAGUE ----

jugs_uel <- jugs_uel|>
  mutate(position_group = case_when(
    primary_position == "Goalkeeper" ~ "Portero",
    primary_position %in% c("Centre Back","Right Centre Back","Left Centre Back") ~ "Central",
    primary_position %in% c("Left Back","Left Wing Back","Right Back","Right Wing Back") ~ "Lateral/Carrilero",
    primary_position %in% c("Centre Defensive Midfielder","Right Defensive Midfielder","Left Defensive Midfielder") ~ "Medio de Contención",
    primary_position %in% c("Centre Attacking Midfielder","Left Attacking Midfielder","Left Centre Midfielder","Right Attacking Midfielder","Right Centre Midfielder") ~ "Interior/Mediapunta",
    primary_position %in% c("Left Midfielder","Left Wing","Right Midfielder","Right Wing") ~ "Volante/Extremo",
    primary_position %in% c("Centre Forward","Left Centre Forward","Right Centre Forward") ~ "Delantero",
    TRUE ~ NA_character_
  ))

goles_uel <- goles_uel |>
  group_by(player_id, player_name) |>
  summarise(
    season_goals = sum(player_match_goals, na.rm = FALSE),
    season_assists = sum(player_match_assists, na.rm = FALSE)
  )

jugs_uel <- jugs_uel |>
  # left_join(pos_ligamx, by = c("player_id")) |>
  left_join(goles_uel, by = c("player_id")) |>
  select(-player_name.x) |>
  rename(player_name = player_name.y)

# HELPERS ----

LEAGUE_CATALOG <- list(
  "Liga MX"                    = jugs_ligamx,
  "Concacaf Champions Cup"     = jugs_ccl,
  "Argentina"                  = jugs_arg,
  "Brasil – Série A"           = jugs_brasil,
  "Colombia"                   = jugs_colombia,
  "Ecuador"                    = jugs_ecuador,
  "Chile"                      = jugs_chile,
  "Paraguay"                   = jugs_paraguay,
  "Uruguay"                    = jugs_uruguay,
  "MLS"                        = jugs_mls,
  "Premier League"             = jugs_premier,
  "Championship (EFL)"         = jugs_championship,
  "LaLiga"                     = jugs_laliga,
  "LaLiga 2"                   = jugs_laliga_2,
  "Serie A"                    = jugs_serie_a,
  "Serie B"                    = jugs_serie_b,
  "Bundesliga"                 = jugs_bundesliga,
  "2. Bundesliga"              = jugs_bundesliga_2,
  "Ligue 1"                    = jugs_ligue_1,
  "Eredivisie"                 = jugs_eredivisie,
  "Bélgica"                    = jugs_belgica,
  "Portugal – Primeira Liga"   = jugs_portugal,
  "Turquía – Süper Lig"        = jugs_turquia,
  "Escocia – Premiership"      = jugs_escocia,
  "UEFA Champions League"      = jugs_champions,
  "CONMEBOL Libertadores"      = jugs_libertadores,
  "UEFA Europa League"         = jugs_uel
)

# 3) Position groups menu (consistent labels with your mutate() mapping):
POSITION_GROUPS <- c(
  "Delantero",
  "Volante/Extremo",
  "Interior/Mediapunta",
  "Medio de Contención",
  "Lateral/Carrilero",
  "Central",
  "Portero"
)

# --- CACHE: bundle all data frames into one RDS ------------------------------
# Create /data if it doesn't exist
if (!dir.exists("data")) dir.create("data", recursive = TRUE)

# Put *only* what the app needs into a named list (avoid huge intermediate objs)
scout_data <- list(
  jugs_ligamx       = jugs_ligamx,
  jugs_ccl          = jugs_ccl,
  jugs_arg          = jugs_arg,
  jugs_brasil       = jugs_brasil,
  jugs_colombia     = jugs_colombia,
  jugs_ecuador      = jugs_ecuador,
  jugs_chile        = jugs_chile,
  jugs_paraguay     = jugs_paraguay,
  jugs_uruguay      = jugs_uruguay,
  jugs_mls          = jugs_mls,
  jugs_premier      = jugs_premier,
  jugs_championship = jugs_championship,
  jugs_laliga       = jugs_laliga,
  jugs_laliga_2     = jugs_laliga_2,
  jugs_serie_a      = jugs_serie_a,
  jugs_serie_b      = jugs_serie_b,
  jugs_bundesliga   = jugs_bundesliga,
  jugs_bundesliga_2 = jugs_bundesliga_2,
  jugs_ligue_1      = jugs_ligue_1,
  jugs_eredivisie   = jugs_eredivisie,
  jugs_belgica      = jugs_belgica,
  jugs_portugal     = jugs_portugal,
  jugs_turquia      = jugs_turquia,
  jugs_escocia      = jugs_escocia,
  jugs_champions    = jugs_champions,
  jugs_libertadores = jugs_libertadores,
  jugs_uel          = jugs_uel
)

# Optional: strip attributes you don’t need to shrink size
# scout_data <- lapply(scout_data, function(df) { attributes(df)$spec <- NULL; df })

# Save one compact file. gzip is broadly compatible and decent size/speed tradeoff.

saveRDS(
  scout_data,
  file = "data/scout_data.rds",
  compress = "gzip"
)

message(
  sprintf("Wrote cache: data/scout_data.rds (%.1f MB)",
          file.info("data/scout_data.rds")$size / 1024^2)
)

liga_mx_final <- scout_data[[1]]

# ligamx_scout_data <- list(jugs_ligamx = jugs_ligamx)
# 
# saveRDS(
#   ligamx_scout_data,
#   file = "/Users/mateorodriguez/Desktop/ligamx_scout_data.rds",
#   compress = "gzip"
# )