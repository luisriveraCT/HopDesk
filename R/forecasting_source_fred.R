# =============================================================================
# R/forecasting_source_fred.R
# FRED (St. Louis Fed) adapter — official source for US indicators.
# =============================================================================

fcs_fetch_fred <- function(metric, date_from, date_to) {
  serie <- metric$fetch_params[[1]]$serie %||% ""
  if (!nzchar(serie)) {
    return(list(status = "parse_error", rows = NULL, error_msg = "missing serie"))
  }

  api_key <- Sys.getenv("FRED_API_KEY", "")
  if (!nzchar(api_key)) {
    return(list(status = "http_error", rows = NULL,
                error_msg = "FRED_API_KEY not configured"))
  }

  url <- sprintf(
    paste0("https://api.stlouisfed.org/fred/series/observations",
           "?series_id=%s&api_key=%s&file_type=json",
           "&observation_start=%s&observation_end=%s"),
    serie, api_key,
    format(as.Date(date_from), "%Y-%m-%d"),
    format(as.Date(date_to),   "%Y-%m-%d")
  )

  resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
  if (is.null(resp)) {
    return(list(status = "timeout", rows = NULL, error_msg = "no response from FRED"))
  }
  if (httr::status_code(resp) != 200L) {
    return(list(status = "http_error", rows = NULL,
                error_msg = sprintf("HTTP %d", httr::status_code(resp))))
  }

  body <- tryCatch(
    jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(body)) {
    return(list(status = "parse_error", rows = NULL, error_msg = "JSON parse failed"))
  }

  obs <- tryCatch(body$observations, error = function(e) NULL)
  if (is.null(obs) || !length(obs) || !nrow(obs)) {
    return(list(status = "no_data", rows = NULL, error_msg = NULL))
  }

  rows <- tibble::tibble(
    metric_id        = metric$metric_id,
    fecha            = as.Date(obs$date),
    value            = suppressWarnings(as.numeric(obs$value)),
    source_id        = "fred",
    observation_type = "official",
    fetched_at       = Sys.time(),
    fetched_by       = "system",
    notes            = NA_character_
  )
  rows <- rows[!is.na(rows$value) & !is.na(rows$fecha), , drop = FALSE]

  list(
    status    = if (nrow(rows)) "ok" else "no_data",
    rows      = rows,
    error_msg = NULL
  )
}

# INEGI — placeholder (adapter not implemented in 6.1)
fcs_fetch_inegi <- function(metric, date_from, date_to) {
  list(status = "no_data", rows = NULL,
       error_msg = "INEGI adapter not implemented in 6.1")
}

# Manual — pass-through; UI constructs the row and calls the dispatcher.
fcs_fetch_manual <- function(metric, date_from, date_to) {
  list(status = "no_data", rows = NULL, error_msg = "manual adapter: use UI entry")
}
