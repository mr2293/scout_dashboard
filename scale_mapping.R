# ============================================================
# scale_mapping.R
#
# Final 0-100 scale mapping (doc S12.7/S12.8, S21.16): "Mapeo final
# estable a escala 0-100... Calibrar contra una distribución histórica
# estable; no forzar que siempre haya un 99." Explicitly NOT the old
# .rescale_to_1_99() approach (per-run min/max stretch, which makes
# whoever is best in THIS run always hit 99 regardless of whether
# they're actually elite) -- a FIXED lo/hi pair instead, so a 90+ means
# something stable across runs, not "best of whoever happened to be in
# this pull."
#
# DataScore and AmeScore get SEPARATE bounds, not shared: confidence
# shrinkage (DataScore only) pulls every value toward its role mean on
# purpose, so DataScore's natural spread is much tighter than
# AmeScore's (which only gets a structural ceiling, no shrinkage). A
# shared rescale would make DataScore look artificially less
# differentiated than AmeScore purely from sharing a window neither was
# built to fill the same way.
#
# Same bounds originally derived in ca_scouting_dashboard/lib/
# scoreScale.js (2026-10-09, from the observed combined DataScore_final/
# AmeScore_final distribution across all 7 roles) -- migrated HERE so
# the actual STORED/SYNCED values are correctly scaled, not just their
# on-screen display. ca_scouting_dashboard's own rescale is being
# neutralized in the same pass (double-transforming already-scaled
# values would otherwise silently produce wrong numbers).
#
# lo/hi are illustrative/placeholder like everything else in this
# pipeline -- "calibrar contra una distribución histórica estable" is
# itself still an open item; these just bracket the CURRENT observed
# range with some headroom, not a true historical calibration.
#
# Sourced by datascore_v2.R, applied as the LAST step before
# DataScore/DataScoreAmerica are returned to callers (app.R, Mongo).
# ============================================================

DATASCORE_SCALE_LO <- 15
DATASCORE_SCALE_HI <- 90
AMESCORE_SCALE_LO <- 5
AMESCORE_SCALE_HI <- 92

scale_to_0_100 <- function(raw, lo, hi) {
  pct <- (raw - lo) / (hi - lo) * 100
  pmax(0, pmin(100, pct))
}

scale_datascore <- function(raw) scale_to_0_100(raw, DATASCORE_SCALE_LO, DATASCORE_SCALE_HI)
scale_amescore <- function(raw) scale_to_0_100(raw, AMESCORE_SCALE_LO, AMESCORE_SCALE_HI)
