# ============================================================
# chatbot.R — Scouting chatbot (natural-language player search)
#
# Pipeline per question:
#   1. EXTRACTION call to Claude: turn the scout's free-text question into a
#      structured filter spec (position, age, foot, league, and the exact
#      metric columns relevant to the question), using the metrics glossary
#      in scout_chat_system_prompt.md as the system prompt.
#   2. R computes a composite percentile score across those metrics for the
#      filtered player pool (all 27 leagues) and sorts the whole pool.
#   3. The top page (10 players) is rendered as a table. If the scout asks
#      for more ("más", "otros", ...), the next page is sliced from the same
#      sorted pool with no extra API call.
#
# The markdown dictionary is read once at source time and kept in
# SCOUT_CHAT_SYSTEM_PROMPT for the life of the app process -- it's the system
# prompt on the extraction call, so metric selection stays grounded in that
# reference.
# ============================================================

# Runtime correction appended to the dictionary (not edited into the source
# file itself): the dictionary implies SkillCorner's `position_group`/`group`
# categories and Game Intelligence vars (off-ball runs, passing-to-runs,
# pressure resistance -- i.e. most `count_*_per_30_tip` / `*_percentage`
# columns) are broadly available, but in the actual data they only exist for
# Liga MX. SkillCorner physical vars (distance, HSR, sprint, accel/decel,
# COD, psv99) cover 13 leagues. Every `player_season_*` StatsBomb column
# covers all 27. Without this note the model reaches for Liga MX-only
# columns by default and results collapse to Liga MX even for searches meant
# to span all 27 leagues.
SCOUT_CHAT_COVERAGE_NOTE <- paste(
  "\n\n---\n\n",
  "## Nota de cobertura por liga (correccion en tiempo de ejecucion)\n\n",
  "Las columnas `position_group` y `group` (categorias SkillCorner en ingles como \"Wide Attacker\") ",
  "y las variables de Game Intelligence de SkillCorner (carreras sin balon, pases a carreras, ",
  "resistencia a la presion -- en general cualquier columna `count_*_per_30_tip` o su `*_percentage` ",
  "asociado) SOLO existen para Liga MX; no existen en absoluto en las otras 26 ligas.\n\n",
  "Las variables fisicas de SkillCorner (distancia, HSR, sprint, aceleraciones/desaceleraciones, ",
  "cambios de direccion, psv99) cubren 13 ligas.\n\n",
  "Las columnas `player_season_*` de StatsBomb cubren las 27 ligas.\n\n",
  "Por defecto la busqueda debe considerar las 27 ligas por igual. Por lo tanto: no uses ",
  "`position_groups` salvo que el scout pida explicitamente un sub-perfil SkillCorner y acepte ",
  "limitarse a Liga MX; y prefiere metricas `player_season_*` (o fisicas SkillCorner de las 13 ligas) ",
  "sobre variables de Game Intelligence, salvo que el scout mencione explicitamente Liga MX o pida ",
  "algo que solo Game Intelligence puede responder (en ese caso está bien usarlas, pero acláralo en 'notes').",
  collapse = ""
)

# Transfermarkt fields (market value, contract expiry) aren't in the
# dictionary at all -- it only documents StatsBomb/SkillCorner performance
# metrics. This note tells the extraction model those fields exist, how to
# express them in the JSON spec, and that coverage is partial (only players
# Transfermarkt could be matched to).
SCOUT_CHAT_TRANSFERMARKT_NOTE <- paste0(
  "\n\n---\n\n",
  "## Datos de mercado (Transfermarkt, fuera del diccionario original)\n\n",
  "Ademas de las metricas de rendimiento hay dos campos de mercado disponibles:\n",
  "- Valor de mercado en euros (numero exacto: \"5 millones\"/\"5M\" = 5000000, \"500k\"/\"medio millon\" = 500000).\n",
  "- Fecha de fin de contrato (se filtra por año).\n\n",
  "Cobertura parcial: estos datos solo existen para los jugadores que Transfermarkt pudo emparejar. ",
  "Un jugador sin dato no aparecera en busquedas filtradas por valor de mercado o contrato.\n\n",
  "Hoy es ", format(Sys.Date(), "%Y-%m-%d"), ". Usa esto para expresiones relativas como ",
  "\"contrato por vencer\" o \"le queda poco contrato\" (interpreta como dentro de los proximos 12 meses).\n\n",
  "Cuando el scout mencione valor de mercado o contrato, completa los campos market_value_max_eur, ",
  "market_value_min_eur, contract_year_min y/o contract_year_max del JSON de salida (ver esquema)."
)

SCOUT_CHAT_SYSTEM_PROMPT <- paste0(
  paste(readLines("scout_chat_system_prompt.md", warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
  SCOUT_CHAT_COVERAGE_NOTE,
  SCOUT_CHAT_TRANSFERMARKT_NOTE
)

SCOUT_CHAT_MODEL <- "claude-sonnet-5"
SCOUT_CHAT_PAGE_SIZE <- 10
SCOUT_CHAT_MAX_RESULTS <- 30
# A metric only counts toward the composite score if at least this fraction
# of the (already position/age/foot/league-filtered) pool has data for it.
# Without this, metrics that only exist for a handful of leagues -- e.g. the
# dictionary notes SkillCorner Game Intelligence vars are Liga MX-only --
# give every non-Liga-MX player an undefined (NaN) composite score, which
# gets dropped, so results collapse to almost entirely Liga MX regardless of
# how broadly the scout searched.
SCOUT_CHAT_MIN_METRIC_COVERAGE <- 0.15

# ---- Low-level API call ----------------------------------------------------
# temperature is pinned low (not 0 -- Anthropic doesn't guarantee bit-identical
# output even at 0) so that the same question reliably extracts the same
# metric/filter spec run to run. At the previous default (unset -> 1.0), the
# same free-text prompt could have the model pick a different subset of the
# "3 a 8" scoring metrics on each call, silently producing a different
# composite score and ranking for what looked like an identical query.
SCOUT_CHAT_TEMPERATURE <- 0.1

call_claude_chat <- function(system_text, user_text, max_tokens = 1000) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(trimws(api_key)) == 0) {
    return(list(ok = FALSE, text = "Error: la variable ANTHROPIC_API_KEY no está configurada."))
  }

  resp <- tryCatch(
    httr::POST(
      url  = "https://api.anthropic.com/v1/messages",
      httr::add_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model       = SCOUT_CHAT_MODEL,
        max_tokens  = max_tokens,
        temperature = SCOUT_CHAT_TEMPERATURE,
        system      = system_text,
        messages    = list(list(role = "user", content = user_text))
      ), auto_unbox = TRUE),
      encode = "raw"
    ),
    error = function(e) e
  )

  if (inherits(resp, "error") || inherits(resp, "condition")) {
    return(list(ok = FALSE, text = paste0("Error de conexión: ", conditionMessage(resp))))
  }
  if (httr::status_code(resp) != 200) {
    err <- tryCatch(httr::content(resp, as = "parsed"), error = function(e) NULL)
    msg <- err$error$message
    if (is.null(msg)) msg <- "desconocido"
    return(list(ok = FALSE, text = paste0("Error de API (HTTP ", httr::status_code(resp), "): ", msg)))
  }

  parsed <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  # content can include a leading "thinking" block before the "text" block --
  # concatenate all text-type blocks rather than assuming content[[1]].
  text_blocks <- Filter(function(b) identical(b$type, "text"), parsed$content)
  text <- paste(vapply(text_blocks, function(b) b$text %||% "", character(1)), collapse = "\n")
  list(ok = TRUE, text = text)
}

# ---- Stage 1: question -> structured filter spec ---------------------------
scout_build_extraction_prompt <- function(question) {
  paste0(
    "Un scout de fútbol del Club América hizo esta pregunta:\n\n\"", question, "\"\n\n",
    "Tu tarea: usando el diccionario de arriba, traducir la pregunta a un filtro estructurado ",
    "para consultar una base de datos de jugadores. Responde ÚNICAMENTE con un objeto JSON ",
    "(sin texto adicional, sin backticks ni bloques de markdown), con esta forma exacta:\n\n",
    "{\n",
    '  "primary_positions": [],\n',
    '  "position_groups": [],\n',
    '  "age_min": null,\n',
    '  "age_max": null,\n',
    '  "foot": null,\n',
    '  "leagues": [],\n',
    '  "metrics": [],\n',
    '  "lower_is_better_metrics": [],\n',
    '  "market_value_max_eur": null,\n',
    '  "market_value_min_eur": null,\n',
    '  "contract_year_min": null,\n',
    '  "contract_year_max": null,\n',
    '  "hispanohablante": null,\n',
    '  "top5_europa": null,\n',
    '  "sudamerica_principal": null,\n',
    '  "scout_persona": null,\n',
    '  "n_results": 10,\n',
    '  "notes": ""\n',
    "}\n\n",
    "Donde:\n",
    "- primary_positions: strings EXACTOS de `primary_position` mencionados o implicados (o [] si no aplica).\n",
    "- position_groups: strings EXACTOS de `position_group` (SkillCorner) (o [] si no aplica).\n",
    "- age_min / age_max: números si el scout pide 'joven' (usar el corte de 23 años), 'experimentado' (24+), ",
    "o un rango explícito; null si no aplica.\n",
    "- foot: uno de \"zurdo\", \"diestro\", \"ambidiestro\", o null si no se menciona el pie.\n",
    "- leagues: nombres EXACTOS de `competition_name` si el scout menciona una liga específica; [] si busca en todas ",
    "las 27 ligas disponibles (comportamiento por defecto -- no restrinjas a Liga MX salvo que se pida explícitamente).\n",
    "- metrics: entre 3 y 8 nombres EXACTOS de columnas del diccionario, las más relevantes para evaluar la pregunta ",
    "(usa el mapeo de palabras clave y los perfiles de posición del diccionario).\n",
    "- lower_is_better_metrics: el subconjunto de `metrics` donde menor = mejor, según las reglas de interpretación ",
    "direccional del diccionario.\n",
    "- market_value_max_eur / market_value_min_eur: números en euros (ver nota de datos de mercado) si el scout ",
    "menciona valor de mercado; null si no aplica.\n",
    "- contract_year_min / contract_year_max: años (enteros) si el scout menciona fin de contrato; null si no aplica.\n",
    "- hispanohablante: true si el scout usa \"hispanohablante\", \"habla español\", \"hispano\" o equivalente ",
    "(ver sección del diccionario); null en cualquier otro caso.\n",
    "- top5_europa: true si el scout pide las 5 grandes ligas europeas (ver sección \"Grupos de ligas por región\" ",
    "del diccionario); null en cualquier otro caso.\n",
    "- sudamerica_principal: true si el scout pide las ligas principales de Sudamérica (ver misma sección); null ",
    "en cualquier otro caso.\n",
    "- scout_persona: uno de \"nacho\", \"ferrat\", \"jaime\" si el scout se identifica como esa persona o pide ",
    "explícitamente las ligas de esa persona (ver sección \"Ligas por scout\" del diccionario); null en cualquier ",
    "otro caso. Cuando se usa scout_persona, IGNORA leagues/top5_europa/sudamerica_principal -- las ligas de la ",
    "persona reemplazan cualquier otro filtro de liga o región.\n",
    "- n_results: entero, 10 por defecto; si el scout pide explícitamente un número distinto de jugadores, úsalo ",
    "(máximo 30).\n",
    "- notes: una frase breve en español explicando tu interpretación de la pregunta.\n\n",
    "Usa EXCLUSIVAMENTE nombres de columnas que aparezcan en el diccionario de arriba. No inventes columnas."
  )
}

scout_parse_json_response <- function(text) {
  cleaned <- gsub("```json|```", "", text)
  m <- regmatches(cleaned, regexpr("\\{[\\s\\S]*\\}", cleaned, perl = TRUE))
  if (length(m) == 0 || !nzchar(m)) return(NULL)
  tryCatch(jsonlite::fromJSON(m, simplifyVector = TRUE), error = function(e) NULL)
}

.as_chr_vec <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(character(0))
  as.character(x)
}

# ---- "Give me more" detection (no API call needed) --------------------------
# Deliberately a whitelist of short canonical phrasings rather than "contains
# the word más anywhere" -- a real question like "busco un central más
# físico que técnico" also contains "más" but is a brand new search, not a
# request to paginate the previous one.
.SCOUT_MORE_PHRASES <- c(
  "mas", "más", "dame mas", "dame más", "quiero mas", "quiero más",
  "muestrame mas", "muéstrame más", "otros", "otras", "mas jugadores",
  "más jugadores", "mas opciones", "más opciones", "siguientes",
  "dame mas opciones", "dame más opciones", "ver mas", "ver más",
  "mas por favor", "más por favor", "more", "give me more", "show me more",
  "muestra mas", "muestra más", "amplia", "amplía", "ampliar"
)
scout_is_more_request <- function(question) {
  q <- tolower(trimws(question))
  q <- gsub("[¿?¡!.]", "", q)
  q %in% .SCOUT_MORE_PHRASES
}

# ---- Player pool used by the chatbot, with Transfermarkt fields joined in --
# get_all_players_df() (defined in app.R) is the StatsBomb/SkillCorner pool
# shared with the rest of the dashboard; left-joining tm_crosswalk here
# (rather than inside get_all_players_df itself) keeps that change scoped to
# the chatbot instead of affecting the radar/similarity features that also
# use get_all_players_df(). Cached once per app process, same pattern as
# get_all_players_df().
.scout_chat_pool_cache <- NULL
get_scout_chat_player_pool <- function() {
  if (is.null(.scout_chat_pool_cache)) {
    base <- get_all_players_df()
    has_tm <- exists("tm_crosswalk", inherits = TRUE) &&
      is.data.frame(get("tm_crosswalk", inherits = TRUE)) &&
      nrow(get("tm_crosswalk", inherits = TRUE)) > 0
    if (has_tm) {
      tm <- get("tm_crosswalk", inherits = TRUE) |>
        dplyr::transmute(
          player_name, team_name,
          market_value_eur,
          contract_expires_date = as.Date(contract_expires, format = "%d/%m/%Y"),
          player_agent
        )
      base <- dplyr::left_join(base, tm, by = c("player_name", "team_name"))
    } else {
      base$market_value_eur <- NA_real_
      base$contract_expires_date <- as.Date(NA)
      base$player_agent <- NA_character_
    }

    # Nationality, via the same country_id -> name lookup app.R uses for the
    # "Nacionalidad" filter dropdown (built once here instead of per-session).
    if ("country_id" %in% names(base) && exists("country_id_names", inherits = TRUE)) {
      id_str <- as.character(base$country_id)
      cin <- get("country_id_names", inherits = TRUE)
      mapped <- cin[id_str]
      mapped[is.na(mapped)] <- paste0("Otro (", id_str[is.na(mapped)], ")")
      base$player_country <- mapped
    } else {
      base$player_country <- NA_character_
    }

    .scout_chat_pool_cache <<- base
  }
  .scout_chat_pool_cache
}

fmt_market_value_eur <- function(x) {
  ifelse(is.na(x), "–",
    ifelse(x >= 1e6, sprintf("€%.2fm", x / 1e6), sprintf("€%.0fk", x / 1e3)))
}

# See scout_chat_system_prompt.md "### Hispanohablante" -- El Salvador is
# listed there too but has no known country_id (no Salvadoran club appears
# anywhere in the 27-league dataset to infer it from), so it's omitted here;
# a real Salvadoran player would just never match this filter today.
HISPANIC_NATIONS <- c(
  "México", "España", "Colombia", "Venezuela", "Ecuador", "Perú", "Bolivia",
  "Argentina", "Uruguay", "Paraguay", "Costa Rica", "Panamá", "Guatemala",
  "Honduras", "República Dominicana", "Nicaragua"
)

# See scout_chat_system_prompt.md "### Grupos de ligas por región". These
# are `.league_label` values (the top-level 27-league catalog names set in
# all_players_df_from_cache / league_map in app.R) -- NOT the `competition_name`
# column values documented earlier in the dictionary, which is a different,
# narrower field scoped within a single league's rows.
TOP5_EUROPA_LEAGUES <- c("Premier League", "LaLiga", "Bundesliga", "Serie A", "Ligue 1")
SUDAMERICA_PRINCIPAL_LEAGUES <- c("Argentina", "Brasil", "Colombia", "Uruguay", "Ecuador", "Paraguay", "Chile")

# See scout_chat_system_prompt.md "### Ligas por scout (Nacho / Ferrat / Jaime)".
# "Ligas Europeas" here means ALL European leagues in the dataset (domestic +
# UCL/UEL), not just the top-5 group above -- a broader remit than the generic
# region filter.
ALL_EUROPA_LEAGUES <- c(
  "Premier League", "Championship", "LaLiga", "LaLiga 2", "Serie A", "Serie B",
  "Bundesliga", "2. Bundesliga", "Ligue 1", "Eredivisie", "Bélgica", "Portugal",
  "Turquía", "Escocia", "UEFA Champions League", "UEFA Europa League"
)
NACHO_LEAGUES  <- c("Argentina", "Uruguay", "Paraguay", "Chile", ALL_EUROPA_LEAGUES)
FERRAT_LEAGUES <- c("Brasil", "Colombia", "Ecuador", ALL_EUROPA_LEAGUES)
JAIME_LEAGUES  <- c("MLS", "Liga MX", ALL_EUROPA_LEAGUES)
# Venezuela and Perú have no domestic league in this dataset, so Jaime's
# remit for those two countries is expressed as player nationality instead
# (unioned with JAIME_LEAGUES, not intersected) -- see scout_rank_players_full.
JAIME_NATIONALITIES <- c("Venezuela", "Perú")

# ---- Stage 2: filter spec -> full ranked player pool ------------------------
# df must be the combined all-leagues player pool (get_scout_chat_player_pool()).
# Returns the FULL sorted pool (not just one page) so pagination is free.
scout_rank_players_full <- function(df, spec, all_cols) {
  d <- df

  primary_positions <- .as_chr_vec(spec$primary_positions)
  if (length(primary_positions)) {
    d <- d |> dplyr::filter(
      primary_position %in% primary_positions | secondary_position %in% primary_positions
    )
  }

  # NOTE: the markdown dictionary documents SkillCorner's English position
  # groups (e.g. "Wide Attacker") under the name `position_group`, but in
  # the actual data those English labels live in the `group` column --
  # `position_group` holds a separate Spanish taxonomy. Worse, `group` is
  # only populated for Liga MX -- it doesn't exist at all for the other 26
  # leagues -- so applying it as a hard filter silently collapses the whole
  # search to Liga MX. Only apply it when it actually has broad-enough
  # coverage in the current (position-filtered) pool; otherwise skip it and
  # rely on `primary_position`, which StatsBomb populates for all 27 leagues.
  position_groups <- .as_chr_vec(spec$position_groups)
  if (length(position_groups)) {
    candidate_cols <- intersect(c("position_group", "group"), names(d))
    usable_col <- Filter(function(col) {
      mean(d[[col]] %in% position_groups) >= SCOUT_CHAT_MIN_METRIC_COVERAGE
    }, candidate_cols)
    if (length(usable_col)) {
      match_any <- Reduce(`|`, lapply(usable_col, function(col) d[[col]] %in% position_groups))
      d <- d[match_any, , drop = FALSE]
    }
  }

  d$.age <- compute_age_years(d$birth_date)
  if (!is.null(spec$age_min) && !is.na(spec$age_min)) d <- d |> dplyr::filter(.age >= as.numeric(spec$age_min))
  if (!is.null(spec$age_max) && !is.na(spec$age_max)) d <- d |> dplyr::filter(.age <= as.numeric(spec$age_max))

  foot <- spec$foot
  if (!is.null(foot) && !is.na(foot) && foot %in% c("zurdo", "diestro", "ambidiestro") &&
      "player_season_left_foot_ratio" %in% names(d)) {
    ratio <- suppressWarnings(as.numeric(d$player_season_left_foot_ratio))
    keep <- switch(foot,
      "zurdo"       = ratio > 0.6,
      "diestro"     = ratio < 0.4,
      "ambidiestro" = ratio >= 0.4 & ratio <= 0.6
    )
    keep[is.na(keep)] <- FALSE
    d <- d[keep, , drop = FALSE]
  }

  # scout_persona (Nacho/Ferrat/Jaime) replaces any other league/region
  # filter rather than combining with it -- see scout_chat_system_prompt.md
  # "### Ligas por scout".
  persona <- spec$scout_persona
  persona_key <- if (!is.null(persona) && !is.na(persona)) tolower(trimws(persona)) else NA_character_

  if (!is.na(persona_key) && persona_key %in% c("nacho", "ferrat", "jaime") && ".league_label" %in% names(d)) {
    persona_leagues <- switch(persona_key,
      "nacho"  = NACHO_LEAGUES,
      "ferrat" = FERRAT_LEAGUES,
      "jaime"  = JAIME_LEAGUES
    )
    match_league <- d$.league_label %in% persona_leagues
    if (persona_key == "jaime" && "player_country" %in% names(d)) {
      match_league <- match_league | d$player_country %in% JAIME_NATIONALITIES
    }
    d <- d[match_league, , drop = FALSE]
  } else {
    leagues <- .as_chr_vec(spec$leagues)
    if (isTRUE(spec$top5_europa)) leagues <- union(leagues, TOP5_EUROPA_LEAGUES)
    if (isTRUE(spec$sudamerica_principal)) leagues <- union(leagues, SUDAMERICA_PRINCIPAL_LEAGUES)
    if (length(leagues) && ".league_label" %in% names(d)) {
      d <- d |> dplyr::filter(.league_label %in% leagues)
    }
  }

  # Transfermarkt fields have partial coverage (only matched players) --
  # filtering on them necessarily drops unmatched players, same as any other
  # column with missing data.
  show_market_value <- FALSE
  if ("market_value_eur" %in% names(d)) {
    if (!is.null(spec$market_value_max_eur) && !is.na(spec$market_value_max_eur)) {
      d <- d |> dplyr::filter(!is.na(market_value_eur), market_value_eur <= as.numeric(spec$market_value_max_eur))
      show_market_value <- TRUE
    }
    if (!is.null(spec$market_value_min_eur) && !is.na(spec$market_value_min_eur)) {
      d <- d |> dplyr::filter(!is.na(market_value_eur), market_value_eur >= as.numeric(spec$market_value_min_eur))
      show_market_value <- TRUE
    }
  }
  show_contract <- FALSE
  if ("contract_expires_date" %in% names(d)) {
    contract_year <- as.integer(format(d$contract_expires_date, "%Y"))
    if (!is.null(spec$contract_year_min) && !is.na(spec$contract_year_min)) {
      d <- d[!is.na(contract_year) & contract_year >= as.integer(spec$contract_year_min), , drop = FALSE]
      contract_year <- as.integer(format(d$contract_expires_date, "%Y"))
      show_contract <- TRUE
    }
    if (!is.null(spec$contract_year_max) && !is.na(spec$contract_year_max)) {
      d <- d[!is.na(contract_year) & contract_year <= as.integer(spec$contract_year_max), , drop = FALSE]
      show_contract <- TRUE
    }
  }

  if (isTRUE(spec$hispanohablante) && "player_country" %in% names(d)) {
    d <- d[d$player_country %in% HISPANIC_NATIONS, , drop = FALSE]
  }

  # Minimum minutes so single-match spikes don't dominate the ranking.
  if ("player_season_minutes" %in% names(d)) {
    d <- d |> dplyr::filter(is.na(player_season_minutes) | player_season_minutes >= 450)
  }

  metrics <- intersect(.as_chr_vec(spec$metrics), all_cols)
  if (!length(metrics) || nrow(d) == 0) {
    return(list(table = NULL, metrics = character(0), dropped_metrics = character(0), pool_n = nrow(d)))
  }

  # Drop metrics with too little coverage in this pool (see constant comment
  # above) so scoring doesn't silently collapse to whichever leagues happen
  # to have that metric populated.
  coverage <- sapply(metrics, function(col) {
    mean(!is.na(suppressWarnings(as.numeric(d[[col]]))))
  })
  kept_metrics <- names(coverage)[coverage >= SCOUT_CHAT_MIN_METRIC_COVERAGE]
  dropped_metrics <- setdiff(metrics, kept_metrics)
  metrics <- kept_metrics
  if (!length(metrics)) {
    return(list(table = NULL, metrics = character(0), dropped_metrics = dropped_metrics, pool_n = nrow(d)))
  }

  lower_better <- intersect(.as_chr_vec(spec$lower_is_better_metrics), metrics)

  pct_mat <- sapply(metrics, function(col) {
    x <- suppressWarnings(as.numeric(d[[col]]))
    n_valid <- sum(!is.na(x))
    if (n_valid == 0) return(rep(NA_real_, length(x)))
    if (n_valid == 1) {
      # Nothing to rank against -- the lone value is neither high nor low.
      out <- rep(NA_real_, length(x))
      out[!is.na(x)] <- 50
      return(out)
    }
    r <- rank(x, na.last = "keep", ties.method = "average")
    p <- (r - 1) / (n_valid - 1) * 100
    if (col %in% lower_better) p <- 100 - p
    p
  })
  if (is.null(dim(pct_mat))) pct_mat <- matrix(pct_mat, ncol = length(metrics), dimnames = list(NULL, metrics))

  d$.composite_score <- rowMeans(pct_mat, na.rm = TRUE)
  d <- d[is.finite(d$.composite_score), , drop = FALSE]
  if (nrow(d) == 0) return(list(table = NULL, metrics = metrics, dropped_metrics = dropped_metrics, pool_n = 0))

  d <- d |> dplyr::arrange(dplyr::desc(.composite_score))

  out <- d |> dplyr::transmute(
    Jugador  = player_name,
    Equipo   = team_name,
    Liga     = .league_label,
    Posición = primary_position,
    Edad     = round(.age),
    Score    = round(.composite_score, 1)
  )
  for (col in metrics) out[[col]] <- round(suppressWarnings(as.numeric(d[[col]])), 3)
  if (show_market_value) out[["Valor de Mercado"]] <- fmt_market_value_eur(d$market_value_eur)
  if (show_contract) out[["Fin de Contrato"]] <- ifelse(is.na(d$contract_expires_date), "–",
    format(d$contract_expires_date, "%d/%m/%Y"))
  if (isTRUE(spec$hispanohablante)) out[["Nacionalidad"]] <- d$player_country

  list(table = out, metrics = metrics, dropped_metrics = dropped_metrics, pool_n = nrow(d), lower_better = lower_better)
}

scout_slice_page <- function(full_table, skip, n) {
  if (is.null(full_table)) return(NULL)
  total <- nrow(full_table)
  if (skip >= total) return(NULL)
  full_table[(skip + 1):min(skip + n, total), , drop = FALSE]
}

# ---- Entry point used by the server -----------------------------------------
# `prior` is the previous query's result list (or NULL), used to serve
# "give me more" follow-ups without another extraction call.
scout_chat_query <- function(question, prior = NULL) {
  if (!is.null(prior) && !is.null(prior$full_table) && scout_is_more_request(question)) {
    page <- scout_slice_page(prior$full_table, prior$shown, SCOUT_CHAT_PAGE_SIZE)
    if (is.null(page)) {
      return(list(
        kind = "no_more", spec = prior$spec, metrics = prior$metrics,
        dropped_metrics = prior$dropped_metrics, pool_n = prior$pool_n,
        lower_better = prior$lower_better,
        table = NULL, shown = prior$shown, full_table = prior$full_table
      ))
    }
    return(list(
      kind = "more", spec = prior$spec, metrics = prior$metrics,
      dropped_metrics = prior$dropped_metrics, pool_n = prior$pool_n,
      lower_better = prior$lower_better,
      table = page, shown = prior$shown + nrow(page), full_table = prior$full_table
    ))
  }

  df <- get_scout_chat_player_pool()
  all_cols <- names(df)

  extraction_resp <- call_claude_chat(
    SCOUT_CHAT_SYSTEM_PROMPT,
    scout_build_extraction_prompt(question),
    max_tokens = 3000
  )
  if (!extraction_resp$ok) return(list(kind = "error", text = extraction_resp$text))

  spec <- scout_parse_json_response(extraction_resp$text)
  if (is.null(spec)) {
    return(list(
      kind = "error",
      text = "No pude interpretar la pregunta como una búsqueda de jugadores. ¿Puedes reformularla con más detalle (posición, edad, características)?"
    ))
  }

  rank_result <- scout_rank_players_full(df, spec, all_cols)
  if (is.null(rank_result$table)) {
    return(list(
      kind = "empty", spec = spec, metrics = rank_result$metrics,
      dropped_metrics = rank_result$dropped_metrics, pool_n = rank_result$pool_n,
      lower_better = rank_result$lower_better
    ))
  }

  n_first <- suppressWarnings(as.integer(spec$n_results %||% SCOUT_CHAT_PAGE_SIZE))
  if (is.na(n_first)) n_first <- SCOUT_CHAT_PAGE_SIZE
  n_first <- max(3, min(SCOUT_CHAT_MAX_RESULTS, n_first))

  page <- scout_slice_page(rank_result$table, 0, n_first)

  list(
    kind = "results", spec = spec, metrics = rank_result$metrics,
    dropped_metrics = rank_result$dropped_metrics, pool_n = rank_result$pool_n,
    lower_better = rank_result$lower_better,
    table = page, shown = nrow(page), full_table = rank_result$table
  )
}

# ---- Rendering a query result as UI -----------------------------------------
scout_html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  header <- tags$tr(lapply(names(df), function(nm) tags$th(nm)))
  body_rows <- lapply(seq_len(nrow(df)), function(i) {
    tags$tr(lapply(names(df), function(nm) tags$td(as.character(df[[nm]][i]))))
  })
  tags$div(
    class = "scout-chat-table-wrap",
    tags$table(class = "scout-chat-table", tags$thead(header), tags$tbody(body_rows))
  )
}

# Prominent, always-shown explanation of how the Score column was built --
# scouts kept treating Score as if it meant the same thing across different
# league filters (it doesn't: it's a percentile rank within THAT query's
# pool, not an absolute rating -- see SCOUT_CHAT_MIN_METRIC_COVERAGE comment
# above and scout_rank_players_full). Listing the exact metrics with their
# direction (mayor/menor = mejor) up front, instead of burying them in a
# one-line caption, lets a scout sanity-check or challenge a ranking instead
# of treating Score as an opaque number.
scout_methodology_box <- function(res) {
  if (!length(res$metrics)) return(NULL)
  metric_items <- lapply(res$metrics, function(m) {
    arrow <- if (m %in% res$lower_better) "menor = mejor" else "mayor = mejor"
    tags$li(tags$code(m), sprintf(" (%s)", arrow))
  })
  tags$div(
    class = "scout-chat-methodology",
    tags$div(
      class = "scout-chat-methodology-title",
      "Cómo se calculó este ranking"
    ),
    tags$div(
      class = "scout-chat-methodology-body",
      sprintf(
        "Score = promedio simple del percentil de cada métrica (0-100), calculado únicamente entre los %d jugadores del universo filtrado por esta pregunta. Todas las métricas pesan igual. Un mismo jugador puede tener un Score distinto en otra búsqueda si cambia el universo (ej. otro grupo de ligas), aunque sus estadísticas no cambien -- el número solo es comparable dentro de esta tabla.",
        res$pool_n
      )
    ),
    tags$ul(class = "scout-chat-methodology-metrics", metric_items)
  )
}

scout_render_result <- function(res) {
  switch(res$kind,
    "error" = tags$div(class = "scout-chat-note scout-chat-note-error", res$text),
    "empty" = tags$div(
      class = "scout-chat-note",
      "No se encontraron jugadores con los filtros interpretados",
      if (!is.null(res$spec$notes) && nzchar(res$spec$notes)) paste0(" (", res$spec$notes, ")"),
      ". Prueba ampliando la edad, los minutos jugados o las ligas.",
      if (length(res$dropped_metrics)) {
        tags$div(
          class = "scout-chat-meta", style = "margin-top: 6px;",
          sprintf(
            "Nota: las métricas más relevantes para esta pregunta (%s) solo tienen datos en Liga MX, así que no se pudieron evaluar en el resto de las ligas. Menciona \"en Liga MX\" si quieres buscar solo ahí con estas métricas.",
            paste(res$dropped_metrics, collapse = ", ")
          )
        )
      }
    ),
    "no_more" = tags$div(class = "scout-chat-note", "Ya se mostraron todos los jugadores que cumplen estos filtros."),
    "results" = ,
    "more" = tagList(
      if (!is.null(res$spec$notes) && nzchar(res$spec$notes)) {
        tags$div(class = "scout-chat-caption", res$spec$notes)
      },
      scout_methodology_box(res),
      tags$div(
        class = "scout-chat-meta",
        sprintf(
          "%d jugadores mostrados (de %d en el universo filtrado)%s",
          res$shown, res$pool_n,
          if (length(res$dropped_metrics)) {
            paste0(" · excluidas por baja cobertura fuera de su liga: ", paste(res$dropped_metrics, collapse = ", "))
          } else ""
        )
      ),
      scout_html_table(res$table),
      if (res$shown < res$pool_n) {
        tags$div(class = "scout-chat-hint", "Escribe \"más\" para ver más resultados.")
      }
    )
  )
}
