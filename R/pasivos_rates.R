# =============================================================================
# R/pasivos_rates.R
# Fetch reference rates for the pasivos wizard variable-rate widget.
#   .fetch_tiie28() — BANXICO SIE REST API, series SF43783 (TIIE 28 días)
#   .fetch_sofr()   — FRED REST API, series SOFR
# Both return:
#   list(today, yesterday, date_today, date_yesterday)  on success
#   list(today=NA, yesterday=NA, error="...")            on failure
# Requires: httr, jsonlite (both loaded via global.R)
# =============================================================================

.RATE_MAX_LOOKBACK_DAYS <- 14L

# ── TIIE28 ─────────────────────────────────────────────────────────────────────
# token: Sys.getenv("BANXICO_TOKEN") — register at https://www.banxico.org.mx/SieAPIRest
.fetch_tiie28 <- function(token = Sys.getenv("BANXICO_TOKEN")) {
  if (!nzchar(token))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = "BANXICO_TOKEN no configurado"))

  fecha_fin    <- format(Sys.Date(), "%Y-%m-%d")
  fecha_inicio <- format(Sys.Date() - .RATE_MAX_LOOKBACK_DAYS, "%Y-%m-%d")
  url <- sprintf(
    "https://www.banxico.org.mx/SieAPIRest/service/v1/series/SF43783/datos/%s/%s",
    fecha_inicio, fecha_fin
  )

  resp <- tryCatch(
    httr::GET(url,
              httr::add_headers("Bmx-Token" = token),
              httr::timeout(20)),
    error = function(e) e
  )
  if (inherits(resp, "error"))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = conditionMessage(resp)))
  if (httr::status_code(resp) != 200L)
    return(list(today = NA_real_, yesterday = NA_real_,
                error = sprintf("BANXICO HTTP %d", httr::status_code(resp))))

  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyDataFrame = TRUE),
    error = function(e) e
  )
  if (inherits(parsed, "error"))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = conditionMessage(parsed)))

  datos <- tryCatch(parsed$bmx$series$datos[[1]], error = function(e) NULL)
  if (is.null(datos) || !is.data.frame(datos) || !nrow(datos))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = "Sin datos en respuesta BANXICO"))

  dates  <- as.Date(datos$fecha, format = "%d/%m/%Y")
  values <- suppressWarnings(as.numeric(gsub(",", ".", datos$dato)))
  valid  <- !is.na(dates) & !is.na(values)
  dates  <- dates[valid]; values <- values[valid]
  ord    <- order(dates, decreasing = TRUE)
  dates  <- dates[ord];  values <- values[ord]

  if (!length(values))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = "Sin observaciones válidas"))

  list(
    today          = values[1],
    yesterday      = if (length(values) >= 2L) values[2L] else NA_real_,
    date_today     = dates[1],
    date_yesterday = if (length(dates) >= 2L) dates[2L] else as.Date(NA)
  )
}

# ── SOFR ───────────────────────────────────────────────────────────────────────
# api_key: Sys.getenv("FRED_API_KEY") — register at https://fred.stlouisfed.org/docs/api/api_key.html
.fetch_sofr <- function(api_key = Sys.getenv("FRED_API_KEY")) {
  if (!nzchar(api_key))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = "FRED_API_KEY no configurado"))

  url <- sprintf(paste0(
    "https://api.stlouisfed.org/fred/series/observations",
    "?series_id=SOFR&api_key=%s&sort_order=desc&limit=5&file_type=json"
  ), api_key)

  resp <- tryCatch(
    httr::GET(url, httr::timeout(10)),
    error = function(e) e
  )
  if (inherits(resp, "error"))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = conditionMessage(resp)))
  if (httr::status_code(resp) != 200L)
    return(list(today = NA_real_, yesterday = NA_real_,
                error = sprintf("FRED HTTP %d", httr::status_code(resp))))

  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                       simplifyDataFrame = TRUE),
    error = function(e) e
  )
  if (inherits(parsed, "error"))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = conditionMessage(parsed)))

  obs <- tryCatch(parsed$observations, error = function(e) NULL)
  if (is.null(obs) || !is.data.frame(obs) || !nrow(obs))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = "Sin observaciones en respuesta FRED"))

  dates  <- as.Date(obs$date)
  values <- suppressWarnings(as.numeric(obs$value))
  valid  <- !is.na(dates) & !is.na(values)
  dates  <- dates[valid]; values <- values[valid]
  ord    <- order(dates, decreasing = TRUE)
  dates  <- dates[ord];  values <- values[ord]

  if (!length(values))
    return(list(today = NA_real_, yesterday = NA_real_,
                error = "Sin observaciones SOFR válidas"))

  list(
    today          = values[1],
    yesterday      = if (length(values) >= 2L) values[2L] else NA_real_,
    date_today     = dates[1],
    date_yesterday = if (length(dates) >= 2L) dates[2L] else as.Date(NA)
  )
}
