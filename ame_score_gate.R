# ============================================================
# ame_score_gate.R
#
# AmeScore's structural gate/ceiling (doc S9's "Principio de
# compensación" and S12.4): "Una fortaleza diferencial puede compensar
# una debilidad. No debería borrar una incompatibilidad estructural."
#
#   AmeScore final = mínimo(AmeScore Base, Techo estructural)
#
# Doc is explicit this must NOT be automatic for any weakness -- "Solo
# tendría sentido en capacidades que el rol realmente necesita para
# funcionar en el modelo" (S12.4) -- and that the threshold/ceiling are
# both "por calibrar" (S12.7/S12.8), so this is a placeholder mechanism,
# same framing as every other number in this pipeline.
#
# GATE_CAPACITY choice: "Presión posicional" (the AmeScore-only capacity
# built in capacity_subscores.R, fhalf_pressures_ratio et al.) for EVERY
# implemented role, not a different capacity per role. Reasoning: the
# doc's own synthesis of Almada's identity (S4) sequences it as
# "PRESIONAR -> RECUPERAR -> SOSTENER ALTURA -> PROGRESAR -> ATACAR LA
# VENTAJA" -- pressing positioning is the FIRST, foundational link in
# that chain for every outfield role. A player who can't press
# positionally doesn't meet the model's starting requirement, regardless
# of how good their progression/creation/finishing is -- which is
# exactly the "doesn't erase a structural incompatibility" principle.
# Reusing the same capacity everywhere (rather than picking a different
# "structural" capacity per role) also keeps the gate consistent and
# avoids 6 separate, harder-to-defend judgment calls.
#
# GATE_THRESHOLD = 20 (percentile within role pool) -- "carencia
# estructural EXTREMA" per the doc's own wording, so this should be a
# genuinely bottom-of-the-pool cutoff, not merely below average.
#
# Techo estructural = that role's mean AmeScore_Base (Techo_rol) -- reuses
# the same "role mean" anchor confidence_shrinkage.R already established
# for DataScore, rather than inventing an unrelated magic number. A
# structurally gated player can never be read as better than "average
# fit" for the role, no matter how high their raw weighted composite is.
#
# Standalone script, not wired into app.R yet -- same convention as
# every other file in this pipeline this session.
# ============================================================

suppressWarnings(suppressMessages({
  library(dplyr)
}))

source("confidence_shrinkage.R")  # apply_confidence_shrinkage(),
                                   # build_scored_rows(), apply_transition_gate(),
                                   # everything upstream (via its own source() chain)

GATE_CAPACITY <- "Presión posicional"
GATE_THRESHOLD <- 20  # percentile within role pool; por calibrar

# ---- Core function ------------------------------------------------------
# ame_base:        numeric vector -- AmeScore_Base
# gate_capacity_v: numeric vector, same length -- the player's value in
#                  GATE_CAPACITY (0-100 percentile, NA if that capacity
#                  itself didn't meet ITS OWN min-coverage gate)
# role:            character vector, same length -- role_group_matchbased
# techo_rol:       named numeric vector, names = role, values = that
#                  role's mean AmeScore_Base
# Returns a data.frame(gate_active, techo, AmeScore_final). NA
# gate_capacity_v values are NEVER gated -- missing data is a coverage
# problem, not a confirmed structural weakness, and this gate only fires
# on a CONFIRMED extreme value.
apply_ame_score_gate <- function(ame_base, gate_capacity_v, role, techo_rol,
                                  threshold = GATE_THRESHOLD) {
  techo <- unname(techo_rol[role])
  gate_active <- !is.na(gate_capacity_v) & gate_capacity_v < threshold
  ame_final <- ifelse(gate_active, pmin(ame_base, techo), ame_base)

  data.frame(gate_active = gate_active, techo = round(techo, 1), AmeScore_final = round(ame_final, 1))
}

# ============================================================
# Validation harness -- only runs when executed directly.
# ============================================================
if (sys.nframe() == 0) {

  tiers <- read.csv("data/competition_strength_tiers.csv", stringsAsFactors = FALSE)
  dat <- load_scout_data()

  rows <- build_scored_rows(dat, tiers)

  # Techo_rol -- mean AmeScore_Base per role, global pool (same scope as
  # DataScore's Media_rol).
  techo_rol <- rows |>
    dplyr::filter(!is.na(AmeScore_Base)) |>
    dplyr::group_by(role_group_matchbased) |>
    dplyr::summarise(mean_ame = mean(AmeScore_Base), .groups = "drop") |>
    tibble::deframe()
  message("=== Techo_rol per role (mean AmeScore_Base) ===")
  print(round(techo_rol, 1))

  gate <- apply_ame_score_gate(rows$AmeScore_Base, rows[[GATE_CAPACITY]], rows$role_group_matchbased, techo_rol)
  result <- rows |>
    dplyr::select(var_name, player_id, player_name, role_group_matchbased,
                  !!GATE_CAPACITY, AmeScore_Base) |>
    dplyr::bind_cols(gate)

  out_path <- "data/amescore_final_by_role.rds"
  saveRDS(result, out_path)
  message(sprintf("\nWrote %s (%d rows)", out_path, nrow(result)))

  message(sprintf(
    "\n=== Gate coverage: %d / %d rows have a valid %s value, %d gated (%.1f%% of those) ===",
    sum(!is.na(result[[GATE_CAPACITY]])), nrow(result), GATE_CAPACITY,
    sum(result$gate_active), 100 * mean(result$gate_active[!is.na(result[[GATE_CAPACITY]])])
  ))

  message("\n=== Spot check: gated players (AmeScore capped by Presión posicional deficiency) ===")
  print(
    result |>
      dplyr::filter(gate_active) |>
      dplyr::arrange(dplyr::desc(AmeScore_Base)) |>
      dplyr::select(player_name, role_group_matchbased, `Presión posicional`, AmeScore_Base, techo, AmeScore_final) |>
      head(10)
  )

  message("\n=== Spot check: players UNGATED despite a strong AmeScore_Base, for comparison ===")
  print(
    result |>
      dplyr::filter(!gate_active) |>
      dplyr::arrange(dplyr::desc(AmeScore_Base)) |>
      dplyr::select(player_name, role_group_matchbased, `Presión posicional`, AmeScore_Base, techo, AmeScore_final) |>
      head(10)
  )
}
