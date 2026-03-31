library(tidyverse)
library(readr)
library(devtools)
library(stats)
library(gt)
library(grid)
library(scales)
library(fmsb)

## Portero ----

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

## Central ----

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


## Lateral/Carrilero ----

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

## Contención ----

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

## Interior/Mediapunta ----

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

## Volante/Extremo ----

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

## Delantero ----

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