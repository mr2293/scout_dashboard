# ============================================================
# transfer_delta_analysis.R
#
# Estimates lambda (Competition Strength's coefficient in DataScore_previo
# = DataScore_Base + lambda x (CompetitionStrength - reference)) from real
# transfer deltas -- doc S8.2/S12.3/S21.8's own preferred calibration
# source ("transferencias entre ligas... es la evidencia más cercana a
# transferibilidad real"), as opposed to picking lambda by intuition.
#
# Logic: for a player who moved leagues, compare their role-relative
# DataScore_Base (BEFORE Competition Strength is applied) in the OLD
# league vs. the NEW league. If a player's true ability were constant,
# moving to a STRONGER league should suppress DataScore_Base (harder
# competition drags the role-relative percentile down) by an amount
# proportional to how much stronger the new league is -- regressing
# Delta(DataScore_Base) on Delta(CompetitionStrength) across many such
# transfers recovers that proportionality constant; lambda is defined to
# ADD BACK exactly that suppression, so lambda = -slope.
#
# Transfer-pair detection duplicates league_transition_gate.R's own
# chronology/continental-exclusion logic (SEASON_CHRONO_ORDER,
# CONTINENTAL_LEAGUES) rather than refactoring that function to share it
# -- apply_transition_gate() is already wired into the live pipeline
# (datascore_v2.R, confidence_shrinkage.R, ame_score_gate.R), and this is
# a standalone, one-off analysis script, not a new permanent pipeline
# stage; duplicating ~20 lines here is lower-risk than touching
# already-validated production code for an exploratory tool. Same
# precedent as role_eligibility_match_based.R's own duplicated mapping.
#
# Reliability filter: >=600 minutes on BOTH sides of the move (same
# MINUTES_THRESHOLD already used for the league-transition gate itself,
# for consistency) -- a transfer where either side's DataScore_Base is
# itself a small-sample fluke would just add noise to the regression,
# not signal.
#
# KNOWN LIMITATIONS, not solved by this analysis or the doc itself:
#   - Survivorship bias: players who move to STRONGER leagues are often
#     selected because they were already overperforming, not a random
#     sample -- this could understate the true suppression effect.
#   - Confounded with genuine development: a player's underlying ability
#     can change around the same time as the move (young players
#     improving, veterans declining) independent of league difficulty.
# Treat the output as a starting estimate with a confidence interval,
# not a precise final number -- look at the scatter before trusting the
# slope.
#
# Standalone script, not wired into app.R -- same convention as every
# other analysis file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(ggplot2)
}))

source("ame_score_plus.R")  # everything upstream (via its own source() chain)

MIN_RELIABLE_MINUTES <- MINUTES_THRESHOLD  # 600 -- from league_transition_gate.R
N_BOOTSTRAP <- 2000

SEASON_CHRONO_ORDER_LOCAL <- c("315" = 1, "318" = 2, "316" = 3, "351" = 4)
CONTINENTAL_LEAGUES_LOCAL <- c(
  "UEFA Champions League", "UEFA Europa League",
  "Copa Libertadores", "CONCACAF Champions Cup"
)

# ---- Transfer-pair detection (duplicated from league_transition_gate.R,
# see file header for why) -- returns ONE ROW PER TRANSFERRED PLAYER with
# both sides' DataScore_Base/minutes/CompetitionStrength, not just the
# gate's single resolved "current" score. ----
find_transfer_pairs <- function(rows) {
  rows <- rows |>
    dplyr::filter(!is.na(DataScore_previo)) |>
    dplyr::mutate(chrono_rank = unname(SEASON_CHRONO_ORDER_LOCAL[as.character(season_id)]))

  rows <- rows |>
    dplyr::group_by(player_id) |>
    dplyr::filter(!(league %in% CONTINENTAL_LEAGUES_LOCAL & any(!league %in% CONTINENTAL_LEAGUES_LOCAL))) |>
    dplyr::ungroup()

  rows |>
    dplyr::group_by(player_id) |>
    dplyr::group_modify(function(df, key) {
      df <- df |> dplyr::arrange(dplyr::desc(chrono_rank), dplyr::desc(player_season_minutes))
      current <- df[1, ]
      prior_candidates <- df |> dplyr::filter(chrono_rank < current$chrono_rank)
      if (!nrow(prior_candidates)) return(tibble::tibble())
      prior <- prior_candidates[1, ]
      if (identical(prior$league, current$league)) return(tibble::tibble())

      tibble::tibble(
        player_name = current$player_name,
        old_league = prior$league, new_league = current$league,
        old_role = prior$role_group_matchbased, new_role = current$role_group_matchbased,
        old_minutes = prior$player_season_minutes, new_minutes = current$player_season_minutes,
        old_ds_base = prior$DataScore_Base, new_ds_base = current$DataScore_Base,
        old_strength = prior$CompetitionStrength, new_strength = current$CompetitionStrength
      )
    }) |>
    dplyr::ungroup()
}

# ============================================================
# Main
# ============================================================

tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
dat <- load_scout_data()
rows <- build_scored_rows(dat, tiers)

pairs <- find_transfer_pairs(rows)
message(sprintf("Total transfer pairs found: %d", nrow(pairs)))

reliable <- pairs |>
  dplyr::filter(
    old_minutes >= MIN_RELIABLE_MINUTES, new_minutes >= MIN_RELIABLE_MINUTES,
    !is.na(old_ds_base), !is.na(new_ds_base), !is.na(old_strength), !is.na(new_strength)
  ) |>
  dplyr::mutate(
    delta_ds = new_ds_base - old_ds_base,
    delta_strength = new_strength - old_strength
  )
message(sprintf("Reliable pairs (>=%d min both sides): %d", MIN_RELIABLE_MINUTES, nrow(reliable)))

if (nrow(reliable) < 20) {
  message("Too few reliable pairs for a trustworthy regression (<20) -- stopping here. Re-run once more transfers clear the minutes threshold.")
} else {

  message(sprintf("\nDelta_strength range: [%.0f, %.0f], mean=%.1f", min(reliable$delta_strength), max(reliable$delta_strength), mean(reliable$delta_strength)))
  message(sprintf("Delta_DataScore_Base range: [%.1f, %.1f], mean=%.1f", min(reliable$delta_ds), max(reliable$delta_ds), mean(reliable$delta_ds)))

  fit <- lm(delta_ds ~ delta_strength, data = reliable)
  coefs <- summary(fit)$coefficients
  r_squared <- summary(fit)$r.squared

  message("\n=== Regression: Delta(DataScore_Base) ~ Delta(CompetitionStrength) ===")
  print(round(coefs, 4))
  message(sprintf("R-squared = %.3f", r_squared))

  slope <- coefs["delta_strength", "Estimate"]
  lambda_estimate <- -slope

  # Bootstrap CI on the slope -- more robust than the OLS standard error
  # alone given the modest sample size.
  set.seed(42)
  boot_slopes <- replicate(N_BOOTSTRAP, {
    idx <- sample(nrow(reliable), nrow(reliable), replace = TRUE)
    coef(lm(delta_ds ~ delta_strength, data = reliable[idx, ]))["delta_strength"]
  })
  boot_ci <- stats::quantile(boot_slopes, c(0.025, 0.975))

  message(sprintf(
    "\n=== Lambda estimate ===\nlambda = -slope = %.4f  (bootstrap 95%% CI: [%.4f, %.4f])",
    lambda_estimate, -boot_ci[2], -boot_ci[1]
  ))
  message(sprintf(
    "For comparison, the doc's own illustrative worked example (S12.3, explicitly NOT a real parameter) used lambda=0.25, currently in use in competition_strength.R."
  ))

  intercept <- coefs["(Intercept)", "Estimate"]
  message(sprintf(
    "\nIntercept = %.2f -- %s",
    intercept,
    if (abs(intercept) < 2) "close to 0, consistent with 'no systematic drift absent a strength change' (as the model assumes)."
    else "notably non-zero -- players who transfer leagues may be drifting for reasons OTHER than league strength (form, aging, survivorship), worth investigating before trusting lambda at face value."
  ))

  # ---- Scatter plot ----
  p <- ggplot(reliable, aes(x = delta_strength, y = delta_ds)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
    labs(
      title = "Transfer delta: Delta(DataScore_Base) vs. Delta(Competition Strength)",
      subtitle = sprintf("n=%d reliable transfer pairs (>=%d min both sides) | lambda estimate = %.3f",
                          nrow(reliable), MIN_RELIABLE_MINUTES, lambda_estimate),
      x = "Delta Competition Strength (new league - old league)",
      y = "Delta DataScore_Base (new league - old league)"
    ) +
    theme_minimal()
  ggsave("data/transfer_delta_scatter.png", p, width = 8, height = 6, dpi = 150)
  message("\nWrote data/transfer_delta_scatter.png")

  saveRDS(reliable, "data/transfer_delta_pairs.rds")
  message(sprintf("Wrote data/transfer_delta_pairs.rds (%d pairs)", nrow(reliable)))

  message("\n=== Spot check: largest strength increases (moved to a much stronger league) ===")
  print(reliable |> dplyr::arrange(dplyr::desc(delta_strength)) |>
          dplyr::select(player_name, old_league, new_league, delta_strength, old_ds_base, new_ds_base, delta_ds) |>
          head(10))

  message("\n=== Spot check: largest strength decreases (moved to a much weaker league) ===")
  print(reliable |> dplyr::arrange(delta_strength) |>
          dplyr::select(player_name, old_league, new_league, delta_strength, old_ds_base, new_ds_base, delta_ds) |>
          head(10))
}
