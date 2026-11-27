# =============================================================================
# tests/helpers/mercado_falso.R -- T116 F3: a fake ECB Data Portal API and a
# fake New York Fed Markets Data API on 127.0.0.1 (the sap_falso.R pattern:
# httpuv in a callr::r_bg process). Invented figures only; never a real API.
#
# mercado_falso_iniciar(config) -> list(url, config(lista), log(), log_borrar(),
# parar()). Point the adapters at it with
#   Sys.setenv(HD_ECB_URL = f$url, HD_NYFED_URL = f$url)
# (they accept an override only towards 127.0.0.1).
#
# As the real APIs do, as far as the adapters use them:
#   GET /service/data/<flujo>/<llave>?startPeriod&endPeriod&format=csvdata
#       -> CSV with KEY, FREQ, TIME_PERIOD, OBS_VALUE
#   GET /api/rates/<tipo>/<tasa>/search.json?startDate&endDate
#       -> {"refRates": [{"effectiveDate", "type", "percentRate"}]}
# config: list(
#   ecb   = list(modo = "ok"|"falla500"|"vacio"|"malformado"|"nan",
#                fechas = c("2027-11-24", ...),
#                valores = list(USD = 1.0975, GBP = 0.83755, ..., "B.EU000A2X2A25.WT" = 2.445),
#                fechas_de = list(GBP = c(...))),          # optional, per series
#   nyfed = list(modo = ..., fechas = c(...), valores = list(sofr = 3.94, effr = 3.88)))
# A series without a value answers 404 (as the ECB does for an unknown key).
# log(): one row per GET -- ruta, consulta.
# =============================================================================

mercado_falso_iniciar <- function(config) {
  dir <- tempfile("mercado_falso_"); dir.create(dir)
  cfg <- file.path(dir, "config.json"); log <- file.path(dir, "log.tsv")
  jsonlite::write_json(config, cfg, auto_unbox = TRUE)
  puerto <- httpuv::randomPort()
  proc <- callr::r_bg(function(puerto, cfg, log) {
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
    responder <- function(status, tipo, cuerpo) list(status = status, headers = list("Content-Type" = tipo), body = cuerpo)
    app <- list(call = function(req) {
      ruta <- req$PATH_INFO; consulta <- sub("^\\?", "", req$QUERY_STRING %||% "")
      cat(paste(ruta, consulta, sep = "\t"), "\n", file = log, append = TRUE, sep = "")
      conf <- jsonlite::read_json(cfg, simplifyVector = TRUE)
      partes <- strsplit(sub("^/", "", ruta), "/")[[1]]
      if (length(partes) == 4L && partes[1] == "service" && partes[2] == "data") {
        e <- conf$ecb; flujo <- partes[3]; llave <- partes[4]
        serie <- if (flujo == "EXR") strsplit(llave, ".", fixed = TRUE)[[1]][2] else llave
        if (identical(e$modo, "falla500")) return(responder(500L, "text/plain", "error ficticio"))
        if (identical(e$modo, "malformado")) return(responder(200L, "text/html", "<html>no es un csv</html>"))
        v <- e$valores[[serie]]
        if (is.null(v)) return(responder(404L, "text/plain", "No results found."))
        cab <- "KEY,FREQ,TIME_PERIOD,OBS_VALUE"
        if (identical(e$modo, "vacio")) return(responder(200L, "text/csv", paste0(cab, "\n")))
        fechas <- e$fechas_de[[serie]] %||% e$fechas
        valor <- if (identical(e$modo, "nan")) "NaN" else format(v, digits = 10)
        filas <- sprintf("%s.%s,D,%s,%s", flujo, llave, fechas, valor)
        return(responder(200L, "text/csv", paste(c(cab, filas), collapse = "\n")))
      }
      if (length(partes) == 5L && partes[1] == "api" && partes[2] == "rates" && partes[5] == "search.json") {
        n <- conf$nyfed; tasa <- partes[4]
        if (identical(n$modo, "falla500")) return(responder(500L, "application/json", "{\"error\":\"ficticio\"}"))
        if (identical(n$modo, "malformado")) return(responder(200L, "application/json", "{no es json"))
        v <- n$valores[[tasa]]
        if (is.null(v) || identical(n$modo, "vacio")) return(responder(200L, "application/json", "{\"refRates\":[]}"))
        rr <- lapply(n$fechas, function(f) list(effectiveDate = f, type = toupper(tasa), percentRate = v))
        return(responder(200L, "application/json", as.character(jsonlite::toJSON(list(refRates = rr), auto_unbox = TRUE))))
      }
      responder(404L, "text/plain", "no existe")
    })
    s <- httpuv::startServer("127.0.0.1", puerto, app)
    repeat httpuv::service(100)
  }, args = list(puerto = puerto, cfg = cfg, log = log), supervise = TRUE)
  url <- sprintf("http://127.0.0.1:%d", puerto)
  listo <- FALSE
  for (i in 1:100) {
    ok <- tryCatch({ httr::GET(paste0(url, "/listo"), httr::timeout(1)); TRUE }, error = function(e) FALSE)
    if (ok) { listo <- TRUE; break }
    Sys.sleep(0.1)
  }
  if (!listo) { proc$kill(); stop("mercado_falso: the fake server did not start: ", paste(proc$read_error(), collapse = " ")) }
  unlink(log)   # the readiness probe is not a request of the test
  list(
    url = url, puerto = puerto,
    config = function(lista) jsonlite::write_json(lista, cfg, auto_unbox = TRUE),
    log = function() {
      if (!file.exists(log)) return(data.frame(ruta = character(), consulta = character(), stringsAsFactors = FALSE))
      x <- utils::read.delim(log, header = FALSE, colClasses = "character", quote = "")
      data.frame(ruta = x$V1, consulta = x$V2, stringsAsFactors = FALSE)
    },
    log_borrar = function() unlink(log),
    parar = function() proc$kill()
  )
}
