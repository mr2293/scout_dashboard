# ============================================================
# EXTRACT ALL PLAYER NAMES + TEAMS (SB + SC) INTO CSVs
# - Reads: scout_data.rds (StatsBomb lists)
# - Reads: scout_skill_corner.rds$fis_data (SkillCorner lists)
# - Outputs (in data_dir):
#   1) SB_names_by_list.csv
#   2) SC_names_by_list.csv
#   3) ALL_statsbomb_names_teams.csv
#   4) ALL_skillcorner_names_teams.csv
#   5) ALL_combined_name_team_universe.csv
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(readr)
})

data_dir <- Sys.getenv("DATA_DIR", unset = "data")

sb_path  <- file.path(data_dir, "scout_data.rds")
sc_path  <- file.path(data_dir, "scout_skill_corner.rds")

scout_data <- readRDS(sb_path)
sc_scout_data <- readRDS(sc_path)

if (!("fis_data" %in% names(sc_scout_data))) {
  stop("Expected sc_scout_data to contain $fis_data")
}
fis_data <- sc_scout_data$fis_data

# -------------------------------------------------------
# Helper: pick first existing column name from candidates
# -------------------------------------------------------
pick_col <- function(df, candidates) {
  candidates <- candidates[candidates %in% names(df)]
  if (length(candidates) == 0) return(NA_character_)
  candidates[[1]]
}

# -------------------------------------------------------
# Helper: extract names + teams safely from a list of dfs
# -------------------------------------------------------
extract_names_teams_from_list <- function(lst, source_label) {
  map_dfr(names(lst), function(list_name) {
    df <- lst[[list_name]]
    
    name_col <- pick_col(df, c("player_known_name", "player_name", "Jugador", "player"))
    team_col <- pick_col(df, c("team_name", "team", "team_short_name", "squad", "equipo",
                               "team.team_name", "team_name_short", "team_name_long"))
    
    if (is.na(name_col)) return(NULL)
    
    out <- df |>
      transmute(
        league_list_name = list_name,
        source = source_label,
        player_name = as.character(.data[[name_col]]),
        team_name = if (!is.na(team_col)) as.character(.data[[team_col]]) else NA_character_
      ) |>
      filter(!is.na(player_name), player_name != "") |>
      distinct()
    
    out
  })
}

# -------------------------------------------------------
# Extract (by list)
# -------------------------------------------------------
sb_names_teams <- extract_names_teams_from_list(scout_data, "statsbomb")
sc_names_teams <- extract_names_teams_from_list(fis_data, "skillcorner")

# -------------------------------------------------------
# Master unique lists (player + team)
# Note: players can appear multiple times if they have multiple teams
# We'll keep the distinct pairs (player_name, team_name).
# -------------------------------------------------------
sb_unique <- sb_names_teams |>
  select(player_name, team_name) |>
  distinct() |>
  arrange(player_name, team_name)

sc_unique <- sc_names_teams |>
  select(player_name, team_name) |>
  distinct() |>
  arrange(player_name, team_name)

all_combined <- bind_rows(
  sb_unique |> mutate(source = "statsbomb"),
  sc_unique |> mutate(source = "skillcorner")
) |>
  arrange(source, player_name, team_name)

# -------------------------------------------------------
# Save CSVs
# -------------------------------------------------------
write_csv(sb_names_teams, file.path(data_dir, "SB_names_by_list.csv"))
write_csv(sc_names_teams, file.path(data_dir, "SC_names_by_list.csv"))

write_csv(sb_unique, file.path(data_dir, "ALL_statsbomb_names_teams.csv"))
write_csv(sc_unique, file.path(data_dir, "ALL_skillcorner_names_teams.csv"))
write_csv(all_combined, file.path(data_dir, "ALL_combined_name_team_universe.csv"))

cat("Saved:\n")
cat("- SB_names_by_list.csv\n")
cat("- SC_names_by_list.csv\n")
cat("- ALL_statsbomb_names_teams.csv\n")
cat("- ALL_skillcorner_names_teams.csv\n")
cat("- ALL_combined_name_team_universe.csv\n")

# -------------------------------------------------------
# Quick diagnostics
# -------------------------------------------------------
cat("\nDiagnostics:\n")
cat("SB distinct (player,team) pairs:", nrow(sb_unique), "\n")
cat("SC distinct (player,team) pairs:", nrow(sc_unique), "\n")
cat("SB rows by list:", nrow(sb_names_teams), "\n")
cat("SC rows by list:", nrow(sc_names_teams), "\n")

# Puro data set combinado ----

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)
})

data_dir <- Sys.getenv("DATA_DIR", unset = "data")

sb_csv <- file.path(data_dir, "ALL_statsbomb_names_teams.csv")
sc_csv <- file.path(data_dir, "ALL_skillcorner_names_teams.csv")

output_path <- file.path(data_dir, "ALL_combined_name_team_universe.csv")

# ------------------------------------------------------------
# Base cleaning (lower, remove accents, keep letters/spaces)
# ------------------------------------------------------------
clean_text <- function(x) {
  x <- as.character(x)
  x <- str_to_lower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[^a-z\\s]", " ")
  x <- str_squish(x)
  x
}

# ------------------------------------------------------------
# Team normalization: remove generic tokens (club, fc, cf, etc.)
# ------------------------------------------------------------
normalize_team <- function(x) {
  x <- clean_text(x)
  
  # remove common "generic" org words (whole-word)
  # add/remove tokens as you see examples in your data
  drop <- c(
    "club","deportivo","sporting","atletico","athletic",
    "futbol","football","futebol",
    "fc","cf","sc","ac","cd","ca","cda","sad","u d","ud","unione","ssc",
    "real","union"  # optional: comment out if it causes false merges
  )
  
  # turn vector into regex: \b(word1|word2|...)\b
  drop_regex <- paste0("\\b(", paste(drop, collapse = "|"), ")\\b")
  
  x <- str_replace_all(x, drop_regex, " ")
  x <- str_squish(x)
  
  # Optional: remove very short leftovers (e.g., single-letter tokens)
  x <- str_replace_all(x, "\\b[a-z]\\b", " ")
  x <- str_squish(x)
  
  x
}

# ------------------------------------------------------------
# Load both universes
# ------------------------------------------------------------
sb <- read_csv(sb_csv, show_col_types = FALSE) |>
  transmute(
    sb_player_name = as.character(player_name),
    team_name      = as.character(team_name),
    team_clean     = normalize_team(team_name),
    name_clean     = clean_text(player_name)
  )

sc <- read_csv(sc_csv, show_col_types = FALSE) |>
  transmute(
    sc_player_name = as.character(player_name),
    team_name      = as.character(team_name),
    team_clean     = normalize_team(team_name),
    name_clean     = clean_text(player_name)
  )

# ------------------------------------------------------------
# FULL JOIN on cleaned name + normalized team
# ------------------------------------------------------------
combined <- full_join(
  sb,
  sc,
  by = c("name_clean", "team_clean"),
  suffix = c("_sb", "_sc")
)

combined <- combined |>
  mutate(
    team_name = coalesce(team_name_sb, team_name_sc)
  ) |>
  select(
    sc_player_name,
    sb_player_name,
    team_name
  ) |>
  arrange(team_name, sb_player_name, sc_player_name)

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------
write_csv(combined, output_path)
cat("Saved:", output_path, "\n")