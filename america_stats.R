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

comps <- competitions(username,password)

# version = "v4" explícito, a propósito distinto del "v7" que usan los
# pulls de jugador (ver safe_player_season() en dashboard_scout.R) --
# team_season() de StatsBombR por default pide "v5", pero acá se pide "v4"
# puntualmente. Explícito en las dos puntas para que ninguna quede en el
# default del paquete sin que nadie lo haya decidido.
pachuca_21_22 <- team_season(username, password, season_id = 108, competition_id = 73, version = "v4") |>
  filter(team_name == "Pachuca")

pachuca_22_23 <- team_season(username, password, season_id = 235, competition_id = 73, version = "v4") |>
  filter(team_name == "Pachuca")

pachuca_23_24 <- team_season(username, password, season_id = 281, competition_id = 73, version = "v4") |>
  filter(team_name == "Pachuca")

pachuca_24_25 <- team_season(username, password, season_id = 317, competition_id = 73, version = "v4") |>
  filter(team_name == "Pachuca")

ame_26_27 <- team_season(username, password, season_id = 351, competition_id = 73, version = "v4") |>
  filter(team_name == "América")

equipos_alamada <- bind_rows(
  pachuca_21_22,
  pachuca_22_23,
  pachuca_23_24,
  pachuca_24_25,
  ame_26_27
)

