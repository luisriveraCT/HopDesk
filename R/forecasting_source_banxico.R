# =============================================================================
# R/forecasting_source_banxico.R
# Banxico SIE adapter — official source for Mexican legal metrics.
# =============================================================================

fcs_fetch_banxico <- function(metric, date_from, date_to) {
  serie <- metric$fetch_params[[1]]$serie %||% ""
  if (!nzchar(serie)) {
    return(list(status = "parse_error", rows = NULL,
                error_msg = "missing serie in fetch_params"))
  }

  token <- Sys.getenv("BANXICO_TOKEN", "")
  if (!nzchar(token)) {
    return(list(status = "http_error", rows = NULL,
                error_msg = "BANXICO_TOKEN not configured"))
  }

  url <- sprintf(
    "https://www.banxico.org.mx/SieAPIRest/service/v1/series/%s/datos/%s/%s",
    serie,
    format(as.Date(date_from), "%Y-%m-%d"),
    format(as.Date(date_to),   "%Y-%m-%d")
  )

  resp <- tryCatch(
    httr::GET(url, httr::add_headers(`Bmx-Token` = token), httr::timeout(30)),
    error = function(e) NULL
  )

  if (is.null(resp)) {
    return(list(status = "timeout", rows = NULL, error_msg = "no response from Banxico"))
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

  datos <- tryCatch(body$bmx$series$datos[[1]], error = function(e) NULL)
  if (is.null(datos) || !length(datos) || !nrow(datos)) {
    return(list(status = "no_data", rows = NULL, error_msg = NULL))
  }

  rows <- tibble::tibble(
    metric_id        = metric$metric_id,
    fecha            = as.Date(datos$fecha, format = "%d/%m/%Y"),
    value            = suppressWarnings(as.numeric(gsub(",", "", as.character(datos$dato)))),
    source_id        = "banxico",
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
