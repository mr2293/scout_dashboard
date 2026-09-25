"""
V2-compatible replacement for skillcornerviz.utils.skillcorner_game_intelligence_utils.

skillcornerviz's gi_utils was written against SkillCorner's V1 Game
Intelligence field names (e.g. count_runs_per_match, runs_threat_per_match)
and the V1 per-run-type breakdown (overlap_runs, underlap_runs, ...). The V2
Game Intelligence "intelligent endpoints" no longer serve that per-run-type
breakdown (SkillCorner streamlined the schema to breakdowns primarily by
phase of play - see the API migration guide, section 6.1) and use different,
flatter field names, so gi_utils.add_run_normalisations() silently adds 0
columns against V2 data.

This module adds the equivalent normalisations for the V2 field names, based
on the actual fields returned by:
  - get_metrics_gi_ip_off_ball_runs
  - get_metrics_gi_ip_passes
  - get_metrics_gi_ip_player_possessions

Field names were confirmed against live API responses (2026-08) - per the
migration guide (section 6.3, "Metrics JSON key"), always re-check a field's
definition rather than relying solely on its name if SkillCorner changes it.

V2's `average_per` query param already computes per-match / per-30-TIP /
per-30-OTIP averages server-side, and many ratio/percentage fields (e.g.
pass_pct_completed) are already computed server-side too. What V2 does NOT
provide is a per-90 normalisation, so that's the main thing these helpers add
- plus the handful of ratios that aren't already in the response (mainly for
off-ball runs, which V1 also had a richer normalisation for).
"""

import pandas as pd


def _safe_ratio(df: pd.DataFrame, numerator: str, denominator: str) -> pd.Series:
    """Returns numerator / denominator * 100 if both columns exist, else None (column omitted)."""
    return (df[numerator] / df[denominator]) * 100


def add_per_90_metrics(df: pd.DataFrame, count_columns, minutes_col: str = "minutes") -> list:
    """
    Adds a `<col>_per_90` column for every column in `count_columns` that is
    present in `df`, using `minutes_col` (default: V2's "minutes" field) as
    the playing-time base. V2 only offers match/p30tip/p30otip averaging
    server-side (via the average_per query param), not per-90, so this fills
    that gap client-side.

    Parameters:
        df (DataFrame): The DataFrame to mutate in place.
        count_columns (list[str]): Candidate count-metric column names.
        minutes_col (str): Column holding minutes played to normalise against.

    Returns:
        list: Names of the newly added per-90 columns.
    """
    metrics = []
    if minutes_col not in df.columns:
        return metrics

    for col in count_columns:
        if col not in df.columns:
            continue
        new_col = f"{col}_per_90"
        df[new_col] = df[col] / (df[minutes_col] / 90)
        metrics.append(new_col)

    return metrics


# Off-Ball Runs (get_metrics_gi_ip_off_ball_runs) count fields, per the V2 schema.
OFF_BALL_RUN_COUNT_FIELDS = [
    "offballrun_count",
    "offballrun_count_targeted",
    "offballrun_count_received",
    "offballrun_count_dangerous",
    "offballrun_count_dangerous_targeted",
    "offballrun_count_dangerous_received",
    "offballrun_count_shotwithin10s",
    "offballrun_count_goalwithin10s",
    "offballrun_count_abovehsr",
    "offballrun_count_penaltyarea",
]


def add_run_normalisations(df: pd.DataFrame, minutes_col: str = "minutes") -> list:
    """
    V2 equivalent of gi_utils.add_run_normalisations() for the Off-Ball Runs
    intelligent endpoint (get_metrics_gi_ip_off_ball_runs).

    Adds per-90 counts plus the target/receive/serve/dangerous percentages
    that the V1 helper computed (V2 doesn't compute these server-side).
    Note V2 no longer breaks runs down by run type (overlap, underlap,
    support, etc.) or provides a "threat" field for off-ball runs, so those
    V1 metrics have no V2 equivalent here.

    Parameters:
        df (DataFrame): DataFrame from get_metrics_gi_ip_off_ball_runs.
        minutes_col (str): Column holding minutes played (default "minutes").

    Returns:
        list: Names of the newly added columns.
    """
    metrics = []

    metrics += add_per_90_metrics(df, OFF_BALL_RUN_COUNT_FIELDS, minutes_col)

    ratio_specs = [
        ("offballrun_target_percentage", "offballrun_count_targeted", "offballrun_count"),
        ("offballrun_receive_percentage", "offballrun_count_received", "offballrun_count_targeted"),
        ("offballrun_serve_percentage", "offballrun_count_received", "offballrun_count"),
        ("offballrun_dangerous_percentage", "offballrun_count_dangerous", "offballrun_count"),
        ("offballrun_dangerous_target_percentage", "offballrun_count_dangerous_targeted", "offballrun_count_dangerous"),
        ("offballrun_dangerous_receive_percentage", "offballrun_count_dangerous_received", "offballrun_count_dangerous_targeted"),
        ("offballrun_dangerous_serve_percentage", "offballrun_count_dangerous_received", "offballrun_count_dangerous"),
        ("offballrun_shotwithin10s_percentage", "offballrun_count_shotwithin10s", "offballrun_count"),
        ("offballrun_goalwithin10s_percentage", "offballrun_count_goalwithin10s", "offballrun_count"),
        ("offballrun_abovehsr_percentage", "offballrun_count_abovehsr", "offballrun_count"),
        ("offballrun_penaltyarea_percentage", "offballrun_count_penaltyarea", "offballrun_count"),
    ]

    for new_col, numerator, denominator in ratio_specs:
        if numerator in df.columns and denominator in df.columns:
            df[new_col] = _safe_ratio(df, numerator, denominator)
            metrics.append(new_col)

    return metrics


# Passes (get_metrics_gi_ip_passes) count fields, per the V2 schema.
# (pass_pct_* ratios are already computed server-side, so only per-90 is added here.)
PASS_COUNT_FIELDS = [
    "passopportunity_count",
    "pass_count_attempted",
    "pass_count_attempted_nopressure",
    "pass_count_attempted_lowpressure",
    "pass_count_attempted_mediumpressure",
    "pass_count_attempted_highpressure",
    "pass_count_attempted_veryhighpressure",
    "pass_count_completed",
    "pass_count_completed_nopressure",
    "pass_count_completed_lowpressure",
    "pass_count_completed_mediumpressure",
    "pass_count_completed_highpressure",
    "pass_count_completed_veryhighpressure",
    "pass_count_shotwithin10s",
    "pass_count_goalwithin10s",
    "pass_count_longrange_attempted",
    "pass_count_onetouch_attempted",
    "pass_count_quickpass_attempted",
    "pass_count_onetouch_dangerous_attempted",
    "pass_count_quickpass_dangerous_attempted",
    "passopportunity_count_linebreak",
    "pass_count_linebreak_attempted",
    "pass_count_linebreak_completed",
    "passopportunity_count_torun",
    "pass_count_torun_attempted",
    "pass_count_torun_completed",
    "pass_count_torun_shotwithin10s",
    "pass_count_torun_goalwithin10s",
    "passopportunity_count_dangerous",
    "pass_count_dangerous_attempted",
    "pass_count_dangerous_completed",
    "pass_count_difficultpass_attempted",
]


def add_pass_normalisations(df: pd.DataFrame, minutes_col: str = "minutes") -> list:
    """
    V2 equivalent of gi_utils.add_pass_normalisations() for the Passes
    intelligent endpoint (get_metrics_gi_ip_passes). V2 already computes the
    completion-ratio fields (pass_pct_completed, pass_pct_torun_completed,
    pass_pct_dangerous_completed, ...) server-side, so this only adds the
    per-90 counts that V2 doesn't offer.

    Parameters:
        df (DataFrame): DataFrame from get_metrics_gi_ip_passes.
        minutes_col (str): Column holding minutes played (default "minutes").

    Returns:
        list: Names of the newly added columns.
    """
    return add_per_90_metrics(df, PASS_COUNT_FIELDS, minutes_col)


# Player Possessions (get_metrics_gi_ip_player_possessions) count fields, per the V2 schema.
# (possession/reception/longcarry _pct_* ratios are already computed server-side.)
PLAYER_POSSESSION_COUNT_FIELDS = [
    "possession_count",
    "possession_count_retained",
    "possession_count_shotwithin10s",
    "possession_count_goalwithin10s",
    "possession_count_forwardmomentum",
    "possession_count_beatbymovement",
    "possession_count_beatbypossession",
    "possession_count_drawnpressure",
    "possession_count_drawnpressure_retained",
    "possession_count_escapedpressure",
    "possession_count_intensepressure_progressed",
    "possession_count_intensepressure_dangercreated",
    "reception_count",
    "reception_count_highpressure",
    "reception_count_mediumpressure",
    "reception_count_lowpressure",
    "reception_count_nopressure",
    "reception_count_veryhighpressure",
    "reception_count_intensepressure",
    "reception_count_retained",
    "reception_count_intensepressure_tightspace",
    "reception_count_intensepressure_tightspace_retained",
    "reception_count_intensepressure_constrainedspace",
    "reception_count_inspace",
    "longcarry_count",
    "longcarry_count_retained",
    "longcarry_count_forwardtrajectory",
    "longcarry_count_forwardtrajectory_retained",
    "longcarry_count_abovehsr",
    "longcarry_count_forwardtrajectory_abovehsr",
    "longcarry_count_forwardtrajectory_abovehsr_retained",
    "giveandgo_count",
]


def add_player_possession_normalisations(df: pd.DataFrame, minutes_col: str = "minutes") -> list:
    """
    V2 equivalent of gi_utils.add_playing_under_pressure_normalisations() for
    the Player Possessions intelligent endpoint
    (get_metrics_gi_ip_player_possessions), which is where V2 moved most of
    the "playing under pressure" metrics that used to live in the now-removed
    V1 on_ball_pressures endpoint. V2 already computes the retention/escape
    ratio fields (possession_pct_retained, reception_pct_retained,
    possession_pct_escapedpressure, ...) server-side, so this only adds the
    per-90 counts that V2 doesn't offer.

    Parameters:
        df (DataFrame): DataFrame from get_metrics_gi_ip_player_possessions.
        minutes_col (str): Column holding minutes played (default "minutes").

    Returns:
        list: Names of the newly added columns.
    """
    return add_per_90_metrics(df, PLAYER_POSSESSION_COUNT_FIELDS, minutes_col)
