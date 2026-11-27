# =============================================================================
# R/forecasting_source_ecb.R -- T116 F3: the European Central Bank (Data
# Portal API, data-api.ecb.europa.eu). Public, no key.
#
#   fetch_params = list(flujo = "EXR", llave = "D.USD.EUR.SP00.A")   # USD per euro
#                  list(flujo = "EST", llave = "B.EU000A2X2A25.WT")  # €STR
#                  list(flujo = "EXR", llave = "D.USD.EUR.SP00.A",
#                       divide = "D.GBP.EUR.SP00.A")                  # USD per GBP, crossed
#
# A cross divides two series of the same flow, only on the dates both have
# (never one day by another's); its rows say "cruzado" in notes.
#
# The reference rates are "for information purposes only" (the ECB): shown
# with "BCE" as their source, never used to convert a real payment here.
# Fails closed like fcs_fetch_banxico: HTTP != 200, an empty or malformed
# body, or no numeric value ("NaN" on a day without data) give no rows.
# Confirmed by the Coordinador's read-only script, 2026-11-27 (HTTP 200).
# =============================================================================

FCS_ECB_URL <- "https://data-api.ecb.europa.eu"

# The base URL. HD_ECB_URL overrides it ONLY towards this machine (the tests'
# fake server, tests/helpers/mercado_falso.R): a misconfigured variable can
# never send the request anywhere else.
.fcs_url_local <- function(variable, oficial) {
  u <- Sys.getenv(variable, "")
  if (nzchar(u) && grepl("^http://127\\.0\\.0\\.1(:[0-9]+)?/?$", u)) sub("/$", "", u) else oficial
}

# One ECB series: list(status, datos = data.frame(fecha, valor), error_msg).
.fcs_ecb_serie <- function(flujo, llave, date_from, date_to) {
  url <- sprintf("%s/service/data/%s/%s", .fcs_url_local("HD_ECB_URL", FCS_ECB_URL), flujo, llave)
  resp <- tryCatch(httr::GET(url, query = list(startPeriod = format(as.Date(date_from)),
                                               endPeriod   = format(as.Date(date_to)),
                                               format      = "csvdata"),
                             httr::timeout(20)),
                   error = function(e) NULL)
  if (is.null(resp)) return(list(status = "timeout", datos = NULL, error_msg = "no response from the ECB"))
  if (httr::status_code(resp) != 200L)
    return(list(status = "http_error", datos = NULL, error_msg = sprintf("HTTP %d", httr::status_code(resp))))
  texto <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
  if (!nzchar(trimws(texto %||% ""))) return(list(status = "no_data", datos = NULL, error_msg = NULL))
  d <- tryCatch(utils::read.csv(text = texto, stringsAsFactors = FALSE, colClasses = "character"),
                error = function(e) NULL)
  if (is.null(d) || !all(c("TIME_PERIOD", "OBS_VALUE") %in% names(d)))
    return(list(status = "parse_error", datos = NULL, error_msg = "no TIME_PERIOD/OBS_VALUE columns"))
  datos <- data.frame(fecha = as.Date(d$TIME_PERIOD, optional = TRUE),
                      valor = suppressWarnings(as.numeric(d$OBS_VALUE)), stringsAsFactors = FALSE)
  datos <- datos[!is.na(datos$fecha) & is.finite(datos$valor) & datos$valor > 0, , drop = FALSE]
  if (!nrow(datos)) return(list(status = "no_data", datos = NULL, error_msg = NULL))
  list(status = "ok", datos = datos[order(datos$fecha), , drop = FALSE], error_msg = NULL)
}

fcs_fetch_ecb <- function(metric, date_from, date_to) {
  p <- metric$fetch_params[[1]] %||% list()
  flujo <- as.character(p$flujo %||% "")[1]; llave <- as.character(p$llave %||% "")[1]
  if (!nzchar(flujo) || !nzchar(llave))
    return(list(status = "parse_error", rows = NULL, error_msg = "missing flujo/llave in fetch_params"))
  a <- .fcs_ecb_serie(flujo, llave, date_from, date_to)
  if (!identical(a$status, "ok")) return(list(status = a$status, rows = NULL, error_msg = a$error_msg))
  datos <- a$datos; nota <- NA_character_
  divide <- as.character(p$divide %||% "")[1]
  if (!is.na(divide) && nzchar(divide)) {
    b <- .fcs_ecb_serie(flujo, divide, date_from, date_to)
    if (!identical(b$status, "ok")) return(list(status = b$status, rows = NULL, error_msg = b$error_msg))
    j <- merge(datos, b$datos, by = "fecha", suffixes = c("_a", "_b"))   # only the dates both have
    if (!nrow(j)) return(list(status = "no_data", rows = NULL, error_msg = "no common dates to cross"))
    datos <- data.frame(fecha = j$fecha, valor = j$valor_a / j$valor_b)
    nota <- "cruzado"
  }
  rows <- tibble::tibble(
    metric_id        = as.character(metric$metric_id %||% NA_character_),
    fecha            = datos$fecha,
    value            = datos$valor,
    source_id        = "ecb",
    observation_type = "official",
    fetched_at       = Sys.time(),
    fetched_by       = "system",
    notes            = nota)
  list(status = "ok", rows = rows, error_msg = NULL)
}
