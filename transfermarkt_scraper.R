# Scrapes market value, contract expiry, and player agent from Transfermarkt
# for players in data/sb_players_list.csv (or any player_name/team_name df).
#
# Two Transfermarkt fetches per player: schnellsuche (name search) to find the
# profile URL, then the profile page itself. Both are cached to disk so
# re-running the script never re-hits pages it already has.
#
# Transfermarkt's ToS disallow automated scraping -- this is throttled
# (3-6s between requests) and meant for small, personal-use pulls, not a
# scheduled bulk job.

library(rvest)
library(httr)
library(xml2)

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
CACHE_DIR <- "data/transfermarkt_cache"
CROSSWALK_FILE <- "data/transfermarkt_crosswalk.csv"
SLEEP_RANGE <- c(3, 6)

CROSSWALK_COLS <- c(
  "player_name", "team_name", "status", "tm_player_id", "tm_profile_url",
  "tm_matched_team", "match_score", "top_candidates", "market_value_raw",
  "market_value_eur", "market_value_last_update", "contract_expires",
  "player_agent", "joined", "scraped_at"
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

normalize_team <- function(x) {
  if (is.na(x)) return("")
  x <- tolower(x)
  converted <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  if (is.na(converted)) {
    # iconv silently returns NA for some decomposed-accent sequences
    # (e.g. "e" + U+0301 combining acute, as opposed to precomposed "é").
    # Fall back to stripping combining diacritical marks directly.
    converted <- gsub("[\u0300-\u036f]", "", x, perl = TRUE)
  }
  x <- gsub("[^a-z0-9]+", " ", converted)
  trimws(x)
}

# Team-name club boilerplate that shouldn't count toward token overlap
# (e.g. "Club Deportivo Guadalajara" vs "CD Guadalajara" should match on
# "guadalajara", not partly on the fact both happen to say "club"/"cd").
TEAM_STOP_TOKENS <- c(
  "fc", "cf", "cd", "sd", "sc", "ac", "ca", "the", "club", "de", "futbol",
  "fútbol", "deportivo", "atletico", "atlético", "real", "united", "city",
  "association", "football"
)

# Builds one or more search queries for a player name. StatsBomb sometimes
# abbreviates the first name to an initial ("M.Vera"), which Transfermarkt's
# search handles poorly -- stripping to the surname and searching again
# turns up candidates the literal query misses.
build_search_queries <- function(name) {
  queries <- name
  if (grepl("^[A-Za-z]\\.\\s*\\S", name)) {
    surname <- trimws(sub("^[A-Za-z]\\.\\s*", "", name))
    if (nzchar(surname) && !identical(surname, name)) queries <- c(queries, surname)
  }
  unique(queries)
}

# Token-based team-name similarity: splits both names into words, drops
# common club boilerplate, and scores by Jaccard overlap of the remaining
# tokens. Falls back to whole-string character similarity (discounted)
# for cases like "UANL" vs "Universidad Autonoma de Nuevo Leon" where the
# two names share no tokens at all but are clearly the same club.
team_similarity <- function(a, b) {
  if (is.na(a) || is.na(b)) return(0)
  a <- normalize_team(a)
  b <- normalize_team(b)
  if (a == "" || b == "") return(0)

  ta <- strsplit(a, "\\s+")[[1]]
  tb <- strsplit(b, "\\s+")[[1]]
  ta_f <- setdiff(ta, TEAM_STOP_TOKENS); if (length(ta_f) == 0) ta_f <- ta
  tb_f <- setdiff(tb, TEAM_STOP_TOKENS); if (length(tb_f) == 0) tb_f <- tb
  jaccard <- length(intersect(ta_f, tb_f)) / length(union(ta_f, tb_f))

  dist <- utils::adist(a, b)[1, 1]
  charsim <- 1 - dist / max(nchar(a), nchar(b), 1)

  max(jaccard, charsim * 0.9)
}

slugify <- function(x) gsub("[^a-z0-9]+", "_", tolower(x))

# Tracks consecutive network failures across calls so a long unattended run
# backs off (and eventually stops) instead of hammering Transfermarkt if it
# starts blocking/rate-limiting requests partway through.
.tm_failures <- new.env()
.tm_failures$n <- 0
CIRCUIT_BREAKER_LIMIT <- 20

fetch_cached <- function(url, cache_file) {
  if (file.exists(cache_file)) {
    return(paste(readLines(cache_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  resp <- tryCatch(httr::GET(url, httr::user_agent(UA), httr::timeout(20)), error = function(e) e)
  ok <- !inherits(resp, "error") && httr::status_code(resp) == 200
  if (!ok) {
    .tm_failures$n <- .tm_failures$n + 1
    if (.tm_failures$n %% 5 == 0) {
      message("  ", .tm_failures$n, " consecutive request failures -- backing off for 3 minutes")
      Sys.sleep(180)
    }
    reason <- if (inherits(resp, "error")) conditionMessage(resp) else paste("HTTP", httr::status_code(resp))
    stop("Request failed (", reason, "): ", url)
  }
  .tm_failures$n <- 0
  html_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(html_txt, cache_file, useBytes = TRUE)
  Sys.sleep(stats::runif(1, SLEEP_RANGE[1], SLEEP_RANGE[2]))
  html_txt
}

# Searches Transfermarkt's quick search and returns a data.frame of candidate
# players (name, team, profile_url, player_id).
tm_search_player <- function(query) {
  cache_file <- file.path(CACHE_DIR, "search", paste0(slugify(query), ".html"))
  url <- paste0(
    "https://www.transfermarkt.com/schnellsuche/ergebnis/schnellsuche?query=",
    utils::URLencode(query, reserved = TRUE)
  )
  html_txt <- fetch_cached(url, cache_file)
  page <- rvest::read_html(html_txt)

  links <- rvest::html_elements(
    page,
    xpath = "//table[contains(@class,'inline-table')]//a[contains(@href,'/profil/spieler/')]"
  )
  if (length(links) == 0) return(NULL)

  rows <- lapply(links, function(l) {
    href <- xml2::xml_attr(l, "href")
    name <- trimws(xml2::xml_text(l))
    team_node <- xml2::xml_find_first(l, "ancestor::table[1]//tr[2]/td[1]")
    team <- if (!inherits(team_node, "xml_missing")) trimws(xml2::xml_text(team_node)) else NA_character_
    id <- sub(".*/spieler/(\\d+).*", "\\1", href)
    data.frame(name = name, team = team, profile_url = href, player_id = id, stringsAsFactors = FALSE)
  })
  unique(do.call(rbind, rows))
}

# Picks the best candidate by fuzzy-matching team_name against each
# candidate's club. Flags ties or weak matches as "ambiguous" rather than
# guessing. Returns candidates sorted by score (best first) so callers can
# log runner-up candidates for manual review.
pick_best_match <- function(candidates, team_name) {
  if (is.null(candidates) || nrow(candidates) == 0) {
    return(list(status = "not_found", row = NULL, scored = NULL))
  }

  scored <- candidates
  scored$sim <- vapply(scored$team, team_similarity, numeric(1), b = team_name)
  scored <- scored[order(-scored$sim), ]
  rownames(scored) <- NULL

  if (nrow(scored) == 1) {
    return(list(status = "matched", row = scored[1, ], scored = scored))
  }

  top_gap <- scored$sim[1] - scored$sim[2]
  if (scored$sim[1] < 0.4 || top_gap < 0.15) {
    return(list(status = "ambiguous", row = scored[1, ], scored = scored))
  }
  list(status = "matched", row = scored[1, ], scored = scored)
}

# Formats the top N scored candidates as a compact string for manual
# review, e.g. "Luciano (Sao Paulo) score=0.71 | Luciano (Gremio) score=0.58"
format_top_candidates <- function(scored, n = 3) {
  if (is.null(scored) || nrow(scored) == 0) return(NA_character_)
  k <- min(n, nrow(scored))
  paste(
    sprintf("%s (%s) score=%.2f", scored$name[seq_len(k)], scored$team[seq_len(k)], scored$sim[seq_len(k)]),
    collapse = " | "
  )
}

parse_market_value <- function(raw) {
  if (is.null(raw) || is.na(raw) || raw %in% c("-", "")) return(NA_real_)
  x <- gsub("[€$£]", "", raw)
  mult <- 1
  if (grepl("m$", x, ignore.case = TRUE)) {
    mult <- 1e6
    x <- sub("[Mm]$", "", x)
  } else if (grepl("th\\.?$", x, ignore.case = TRUE)) {
    mult <- 1e3
    x <- sub("[Tt]h\\.?$", "", x)
  } else if (grepl("k$", x, ignore.case = TRUE)) {
    mult <- 1e3
    x <- sub("[Kk]$", "", x)
  }
  num <- suppressWarnings(as.numeric(x))
  if (is.na(num)) return(NA_real_)
  num * mult
}

# Fetches a player's profile page and extracts market value, contract
# expiry, player agent, and the market-value last-update date.
tm_scrape_profile <- function(profile_url, player_id) {
  cache_file <- file.path(CACHE_DIR, "profile", paste0(player_id, ".html"))
  url <- paste0("https://www.transfermarkt.com", profile_url)
  html_txt <- fetch_cached(url, cache_file)
  page <- rvest::read_html(html_txt)

  mv_node <- rvest::html_element(page, "a.data-header__market-value-wrapper")
  market_value_raw <- NA_character_
  last_update <- NA_character_
  if (!is.na(mv_node)) {
    update_node <- rvest::html_element(mv_node, "p.data-header__last-update")
    if (!is.na(update_node)) {
      last_update <- trimws(sub("(?i)last update:\\s*", "", rvest::html_text2(update_node), perl = TRUE))
      xml2::xml_remove(update_node)
    }
    market_value_raw <- trimws(rvest::html_text2(mv_node))
  }

  info_node <- rvest::html_element(page, "div.info-table--right-space")
  if (is.na(info_node)) info_node <- rvest::html_element(page, "div.info-table")

  facts <- list()
  if (!is.na(info_node)) {
    spans <- rvest::html_elements(info_node, xpath = "./span[contains(@class,'info-table__content')]")
    classes <- xml2::xml_attr(spans, "class")
    texts <- trimws(rvest::html_text2(spans))
    i <- 1
    n <- length(texts)
    while (i < n) {
      if (grepl("info-table__content--regular", classes[i])) {
        label <- sub(":$", "", texts[i])
        facts[[label]] <- texts[i + 1]
        i <- i + 2
      } else {
        i <- i + 1
      }
    }
  }

  list(
    market_value_raw = market_value_raw,
    market_value_eur = parse_market_value(market_value_raw),
    market_value_last_update = last_update,
    contract_expires = facts[["Contract expires"]] %||% NA_character_,
    player_agent = facts[["Player agent"]] %||% NA_character_,
    joined = facts[["Joined"]] %||% NA_character_
  )
}

# Loads the persistent player -> Transfermarkt crosswalk, creating an empty
# one with the right columns if it doesn't exist yet.
load_crosswalk <- function() {
  if (file.exists(CROSSWALK_FILE)) {
    df <- read.csv(CROSSWALK_FILE, stringsAsFactors = FALSE, colClasses = "character")
    df$match_score <- suppressWarnings(as.numeric(df$match_score))
    df$market_value_eur <- suppressWarnings(as.numeric(df$market_value_eur))
    df
  } else {
    df <- as.data.frame(matrix(nrow = 0, ncol = length(CROSSWALK_COLS)), stringsAsFactors = FALSE)
    names(df) <- CROSSWALK_COLS
    df$match_score <- numeric(0)
    df$market_value_eur <- numeric(0)
    df
  }
}

# Builds one crosswalk row from a match result (and profile data, if any)
# and inserts/replaces it in the crosswalk by player_name+team_name key.
upsert_crosswalk <- function(crosswalk, key, player_name, team_name, status,
                              row = NULL, scored = NULL, profile = NULL) {
  new_row <- data.frame(
    player_name = player_name,
    team_name = team_name,
    status = status,
    tm_player_id = if (!is.null(row)) row$player_id else NA_character_,
    tm_profile_url = if (!is.null(row)) row$profile_url else NA_character_,
    tm_matched_team = if (!is.null(row)) (row$team %||% NA_character_) else NA_character_,
    match_score = if (!is.null(scored)) scored$sim[1] else NA_real_,
    top_candidates = if (!is.null(scored)) format_top_candidates(scored) else NA_character_,
    market_value_raw = if (!is.null(profile)) profile$market_value_raw else NA_character_,
    market_value_eur = if (!is.null(profile)) profile$market_value_eur else NA_real_,
    market_value_last_update = if (!is.null(profile)) profile$market_value_last_update else NA_character_,
    contract_expires = if (!is.null(profile)) profile$contract_expires else NA_character_,
    player_agent = if (!is.null(profile)) profile$player_agent else NA_character_,
    joined = if (!is.null(profile)) profile$joined else NA_character_,
    scraped_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  existing_idx <- match(key, paste(crosswalk$player_name, crosswalk$team_name, sep = "||"))
  if (!is.na(existing_idx)) {
    crosswalk[existing_idx, names(new_row)] <- new_row
  } else {
    crosswalk <- rbind(crosswalk, new_row)
  }
  crosswalk
}

# Runs the full pipeline (search -> match -> profile scrape) over a
# player_name/team_name data.frame, reading from and writing to the
# persistent crosswalk at CROSSWALK_FILE.
#
# Reuse rules on each row:
#  - already "matched" with a market value on file: skipped (no network).
#  - "ambiguous"/"not_found"/"profile_fetch_failed" on file: skipped unless
#    force_refresh = TRUE, OR unless someone has manually filled in
#    tm_player_id in the crosswalk CSV -- that's treated as a human
#    resolving the case, and the profile is fetched directly using that id.
scrape_transfermarkt <- function(players_df, force_refresh = FALSE) {
  crosswalk <- load_crosswalk()

  for (i in seq_len(nrow(players_df))) {
    if (.tm_failures$n >= CIRCUIT_BREAKER_LIMIT) {
      message(sprintf(
        "Circuit breaker tripped (%d consecutive request failures) -- stopping early at row %d/%d. Saving progress; re-run the script later to resume (already-scraped players are skipped).",
        .tm_failures$n, i, nrow(players_df)
      ))
      break
    }
    if (i %% 25 == 0) write.csv(crosswalk, CROSSWALK_FILE, row.names = FALSE)

    player_name <- players_df$player_name[i]
    team_name <- players_df$team_name[i]
    key <- paste(player_name, team_name, sep = "||")
    message(sprintf("[%d/%d] %s (%s)", i, nrow(players_df), player_name, team_name))

    existing_idx <- match(key, paste(crosswalk$player_name, crosswalk$team_name, sep = "||"))
    existing <- if (!is.na(existing_idx)) crosswalk[existing_idx, ] else NULL

    manually_resolved <- !is.null(existing) && !is.na(existing$tm_player_id) &&
      nzchar(existing$tm_player_id) && is.na(existing$market_value_eur)

    if (!manually_resolved && !is.null(existing) && !force_refresh) {
      if (existing$status == "matched" && !is.na(existing$market_value_eur)) {
        message("  already scraped, skipping")
        next
      }
      if (existing$status %in% c("ambiguous", "not_found", "profile_fetch_failed")) {
        message("  previously flagged as '", existing$status, "', skipping (set force_refresh=TRUE to retry, or fill tm_player_id in the crosswalk to resolve manually)")
        next
      }
    }

    if (manually_resolved) {
      message("  manual tm_player_id found in crosswalk, fetching profile directly")
      row <- data.frame(player_id = existing$tm_player_id, profile_url = paste0("/manual/profil/spieler/", existing$tm_player_id), team = NA_character_, stringsAsFactors = FALSE)
      scored <- NULL
      status <- "matched"
    } else {
      candidates <- NULL
      for (q in build_search_queries(player_name)) {
        candidates <- tryCatch(tm_search_player(q), error = function(e) {
          message("  search failed: ", conditionMessage(e))
          NULL
        })
        if (!is.null(candidates) && nrow(candidates) > 0) break
      }
      match <- pick_best_match(candidates, team_name)
      status <- match$status
      row <- match$row
      scored <- match$scored
    }

    if (status %in% c("ambiguous", "not_found")) {
      crosswalk <- upsert_crosswalk(crosswalk, key, player_name, team_name, status, row = row, scored = scored)
      next
    }

    profile <- tryCatch(tm_scrape_profile(row$profile_url, row$player_id), error = function(e) {
      message("  profile fetch failed: ", conditionMessage(e))
      NULL
    })
    if (is.null(profile)) {
      crosswalk <- upsert_crosswalk(crosswalk, key, player_name, team_name, "profile_fetch_failed", row = row, scored = scored)
      next
    }

    crosswalk <- upsert_crosswalk(crosswalk, key, player_name, team_name, "matched", row = row, scored = scored, profile = profile)
  }

  write.csv(crosswalk, CROSSWALK_FILE, row.names = FALSE)
  crosswalk
}

if (sys.nframe() == 0) {
  players <- read.csv("data/sb_players_list.csv", stringsAsFactors = FALSE)

  crosswalk <- scrape_transfermarkt(players)
  result <- crosswalk[match(
    paste(players$player_name, players$team_name, sep = "||"),
    paste(crosswalk$player_name, crosswalk$team_name, sep = "||")
  ), ]

  message(sprintf(
    "Done. Matched: %d, ambiguous: %d, not found: %d, other: %d",
    sum(result$status == "matched", na.rm = TRUE),
    sum(result$status == "ambiguous", na.rm = TRUE),
    sum(result$status == "not_found", na.rm = TRUE),
    sum(!result$status %in% c("matched", "ambiguous", "not_found"), na.rm = TRUE)
  ))
}
