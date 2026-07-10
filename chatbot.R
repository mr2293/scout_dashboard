# ============================================================
# chatbot.R — Scouting chatbot (natural-language player search)
#
# Pipeline per question:
#   1. EXTRACTION call to Claude: turn the scout's free-text question into a
#      structured filter spec (position, age, foot, league, and the exact
#      metric columns relevant to the question), using the metrics glossary
#      in scout_chat_system_prompt.md as the system prompt.
#   2. R computes a composite percentile score across those metrics for the
#      filtered player pool (all 27 leagues) and takes the top N players.
#   3. ANSWER call to Claude: turn that computed table into a natural-language
#      Spanish response that names specific players with their numbers.
#
# The markdown dictionary is read once at source time and kept in
# SCOUT_CHAT_SYSTEM_PROMPT for the life of the app process -- it's the system
# prompt on both calls, so metric selection and interpretation ("alto"/"bajo",
# context-dependent metrics, etc.) stay grounded in that reference.
# ============================================================

SCOUT_CHAT_SYSTEM_PROMPT <- paste(
  readLines("scout_chat_system_prompt.md", warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

SCOUT_CHAT_MODEL <- "claude-sonnet-5"

# ---- Low-level API call ----------------------------------------------------
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
        model      = SCOUT_CHAT_MODEL,
        max_tokens = max_tokens,
        system     = system_text,
        messages   = list(list(role = "user", content = user_text))
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
    '  "n_results": 5,\n',
    '  "notes": ""\n',
    "}\n\n",
    "Donde:\n",
    "- primary_positions: strings EXACTOS de `primary_position` mencionados o implicados (o [] si no aplica).\n",
    "- position_groups: strings EXACTOS de `position_group` (SkillCorner) (o [] si no aplica).\n",
    "- age_min / age_max: números si el scout pide 'joven' (usar el corte de 23 años), 'experimentado' (24+), ",
    "o un rango explícito; null si no aplica.\n",
    "- foot: uno de \"zurdo\", \"diestro\", \"ambidiestro\", o null si no se menciona el pie.\n",
    "- leagues: nombres EXACTOS de `competition_name` si el scout menciona una liga específica; [] si busca en todas.\n",
    "- metrics: entre 3 y 8 nombres EXACTOS de columnas del diccionario, las más relevantes para evaluar la pregunta ",
    "(usa el mapeo de palabras clave y los perfiles de posición del diccionario).\n",
    "- lower_is_better_metrics: el subconjunto de `metrics` donde menor = mejor, según las reglas de interpretación ",
    "direccional del diccionario.\n",
    "- n_results: entero entre 3 y 10.\n",
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

# ---- Stage 2: filter spec -> ranked player table ---------------------------
# df must be the combined all-leagues player pool (get_all_players_df()).
scout_rank_players <- function(df, spec, all_cols) {
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
  # `position_group` holds a separate Spanish taxonomy. Match on whichever
  # column actually has them until the dictionary is corrected.
  position_groups <- .as_chr_vec(spec$position_groups)
  if (length(position_groups)) {
    has_pg <- "position_group" %in% names(d)
    has_gp <- "group" %in% names(d)
    if (has_pg && has_gp) {
      d <- d |> dplyr::filter(position_group %in% position_groups | group %in% position_groups)
    } else if (has_pg) {
      d <- d |> dplyr::filter(position_group %in% position_groups)
    } else if (has_gp) {
      d <- d |> dplyr::filter(group %in% position_groups)
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

  leagues <- .as_chr_vec(spec$leagues)
  if (length(leagues) && ".league_label" %in% names(d)) {
    d <- d |> dplyr::filter(.league_label %in% leagues)
  }

  # Minimum minutes so single-match spikes don't dominate the ranking.
  if ("player_season_minutes" %in% names(d)) {
    d <- d |> dplyr::filter(is.na(player_season_minutes) | player_season_minutes >= 450)
  }

  metrics <- intersect(.as_chr_vec(spec$metrics), all_cols)
  if (!length(metrics) || nrow(d) == 0) {
    return(list(table = NULL, metrics = metrics, pool_n = nrow(d)))
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
  if (nrow(d) == 0) return(list(table = NULL, metrics = metrics, pool_n = 0))

  n_results <- suppressWarnings(as.integer(spec$n_results %||% 5))
  if (is.na(n_results)) n_results <- 5
  n_results <- max(3, min(10, n_results))

  top <- d |> dplyr::arrange(dplyr::desc(.composite_score)) |> dplyr::slice_head(n = n_results)

  out <- top |> dplyr::transmute(
    Jugador  = player_name,
    Equipo   = team_name,
    Liga     = .league_label,
    Posición = primary_position,
    Edad     = round(.age),
    Score    = round(.composite_score, 1)
  )
  for (col in metrics) out[[col]] <- round(suppressWarnings(as.numeric(top[[col]])), 3)

  list(table = out, metrics = metrics, pool_n = nrow(d))
}

# ---- Stage 3: ranked table -> natural-language answer -----------------------
scout_build_answer_prompt <- function(question, spec, rank_result) {
  notes <- spec$notes
  if (is.null(notes) || is.na(notes)) notes <- "N/A"

  if (is.null(rank_result$table) || nrow(rank_result$table) == 0) {
    return(paste0(
      "Un scout preguntó: \"", question, "\"\n\n",
      "Interpretación aplicada: ", notes, "\n",
      "La búsqueda con esos filtros no encontró jugadores (o no hay suficientes minutos jugados / datos).\n\n",
      "Responde en español, brevemente, explicando que no se encontraron resultados y sugiere cómo ampliar ",
      "la búsqueda (edad, liga, minutos, posición). No inventes jugadores ni cifras."
    ))
  }

  tbl_txt <- paste(capture.output(print(rank_result$table, row.names = FALSE)), collapse = "\n")

  paste0(
    "Un scout del Club América preguntó: \"", question, "\"\n\n",
    "Interpretación aplicada de la pregunta: ", notes, "\n",
    "Métricas evaluadas: ", paste(rank_result$metrics, collapse = ", "), "\n",
    "Tamaño del universo de jugadores tras aplicar filtros: ", rank_result$pool_n, "\n\n",
    "Jugadores mejor evaluados (Score = percentil promedio compuesto de las métricas evaluadas, 0-100, ",
    "ya calculado dentro del universo filtrado):\n\n",
    tbl_txt, "\n\n",
    "Con esta información, escribe una respuesta en español para el scout. Reglas:\n",
    "- Menciona a los jugadores por nombre con su equipo y liga.\n",
    "- Usa los valores numéricos exactos de la tabla; no inventes cifras ni jugadores fuera de la tabla.\n",
    "- Interpreta los valores usando las reglas de direccionalidad e interpretación contextual del diccionario ",
    "(por ejemplo, no digas simplemente 'alto es bueno' en métricas contexto-dependientes; aclara el contexto).\n",
    "- Sé conciso: un párrafo breve de contexto, seguido de un jugador por línea con una justificación corta.\n",
    "- No repitas la tabla completa ni respondas en JSON."
  )
}

# ---- Entry point used by the server ----------------------------------------
scout_chat_answer <- function(question) {
  df <- get_all_players_df()
  all_cols <- names(df)

  extraction_resp <- call_claude_chat(
    SCOUT_CHAT_SYSTEM_PROMPT,
    scout_build_extraction_prompt(question),
    max_tokens = 700
  )
  if (!extraction_resp$ok) return(extraction_resp$text)

  spec <- scout_parse_json_response(extraction_resp$text)
  if (is.null(spec)) {
    return("No pude interpretar la pregunta como una búsqueda de jugadores. ¿Puedes reformularla con más detalle (posición, edad, características)?")
  }

  rank_result <- scout_rank_players(df, spec, all_cols)

  answer_resp <- call_claude_chat(
    SCOUT_CHAT_SYSTEM_PROMPT,
    scout_build_answer_prompt(question, spec, rank_result),
    max_tokens = 900
  )
  if (!answer_resp$ok) return(answer_resp$text)

  answer_resp$text
}
