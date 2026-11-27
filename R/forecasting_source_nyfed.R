# =============================================================================
# R/forecasting_source_nyfed.R -- T116 F3: the Federal Reserve Bank of New
# York's reference rates (Markets Data API, markets.newyorkfed.org). Public,
# no key.
#
#   fetch_params = list(tipo = "secured",   tasa = "sofr")   # SOFR
#                  list(tipo = "unsecured", tasa = "effr")   # EFFR (fed funds)
#
# GET /api/rates/<tipo>/<tasa>/search.json?startDate=&endDate=, reading
# refRates[].effectiveDate and refRates[].percentRate (confirmed by the
# Coordinador's read-only script, 2026-11-27).
#
# Terms of use (newyorkfed.org/privacy/termsofuse): use and display for
# business purposes allowed, with their attribution line and never implying
# the New York Fed endorses the use -- the screens that show these rates carry
# that line (comun.mercado.atribucion.nyfed). Fails closed like the others.
# Its base URL: FCS_NYFED_URL; HD_NYFED_URL overrides it only towards
# 127.0.0.1 (the tests' fake server), as in forecasting_source_ecb.R.
# =============================================================================

FCS_NYFED_URL <- "https://markets.newyorkfed.org"

fcs_fetch_nyfed <- function(metric, date_from, date_to) {
  p <- metric$fetch_params[[1]] %||% list()
  tipo <- as.character(p$tipo %||% "")[1]; tasa <- as.character(p$tasa %||% "")[1]
  if (!grepl("^[a-z]+$", tipo %||% "") || !grepl("^[a-z]+$", tasa %||% ""))
    return(list(status = "parse_error", rows = NULL, error_msg = "missing tipo/tasa in fetch_params"))
  base <- if (exists(".fcs_url_local", mode = "function")) .fcs_url_local("HD_NYFED_URL", FCS_NYFED_URL) else FCS_NYFED_URL
  url <- sprintf("%s/api/rates/%s/%s/search.json", base, tipo, tasa)
  resp <- tryCatch(httr::GET(url, query = list(startDate = format(as.Date(date_from)), endDate = format(as.Date(date_to))),
                             httr::timeout(20)),
                   error = function(e) NULL)
  if (is.null(resp)) return(list(status = "timeout", rows = NULL, error_msg = "no response from the New York Fed"))
  if (httr::status_code(resp) != 200L)
    return(list(status = "http_error", rows = NULL, error_msg = sprintf("HTTP %d", httr::status_code(resp))))
  body <- tryCatch(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE),
                   error = function(e) NULL)
  if (is.null(body)) return(list(status = "parse_error", rows = NULL, error_msg = "JSON parse failed"))
  rr <- body$refRates
  if (is.null(rr) || (is.list(rr) && !length(rr))) return(list(status = "no_data", rows = NULL, error_msg = NULL))
  if (!is.data.frame(rr) || !all(c("effectiveDate", "percentRate") %in% names(rr)))
    return(list(status = "parse_error", rows = NULL, error_msg = "no refRates effectiveDate/percentRate"))
  fecha <- as.Date(as.character(rr$effectiveDate), optional = TRUE)
  valor <- suppressWarnings(as.numeric(rr$percentRate))
  ok <- !is.na(fecha) & is.finite(valor)
  if (!any(ok)) return(list(status = "no_data", rows = NULL, error_msg = NULL))
  o <- order(fecha[ok])
  rows <- tibble::tibble(
    metric_id        = as.character(metric$metric_id %||% NA_character_),
    fecha            = fecha[ok][o],
    value            = valor[ok][o],
    source_id        = "nyfed",
    observation_type = "official",
    fetched_at       = Sys.time(),
    fetched_by       = "system",
    notes            = NA_character_)
  list(status = "ok", rows = rows, error_msg = NULL)
}
