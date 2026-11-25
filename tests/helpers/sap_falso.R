# =============================================================================
# tests/helpers/sap_falso.R -- T103: a fake SAP B1 Service Layer on 127.0.0.1
#
# sap_falso_iniciar(config) starts it in a background R process (callr) and
# returns list(url_de = function(ini), config = function(lista), log = function(),
# parar = function()). Each company answers under /<INI>/b1s/v1/... so
# SAP_<INI>_URL picks it. Invented data only (FICTICIA), never a real ERP.
#
# As the real Service Layer does, as far as the app uses it:
#   POST /Login                       -> 200 + cookie B1SESSION
#   GET  /Invoices, /PurchaseInvoices -> {"value": [...], "@odata.nextLink"?}
#        page size = the `Prefer: odata.maxpagesize=N` header (at most 1000),
#        20 without it (the Service Layer's default PageSize); $skip honoured.
# config: list(<INI> = list(AR = n, AP = n, modo = "ok"|"falla500"|"lento_una_vez"))
#   falla500       every invoice GET answers HTTP 500 (a company that FAILS)
#   lento_una_vez  the first GET with $skip > 0 answers 5 s late, without
#                  blocking the server (a slow late page)
#   lento_siempre  every GET with $skip > 0 answers 5 s late
# log(): one row per invoice GET -- empresa, endpoint, skip, prefer, filas.
# =============================================================================

sap_falso_iniciar <- function(config) {
  dir  <- tempfile("sap_falso_"); dir.create(dir)
  cfg  <- file.path(dir, "config.json"); log <- file.path(dir, "log.tsv")
  jsonlite::write_json(config, cfg, auto_unbox = TRUE)
  puerto <- httpuv::randomPort()
  proc <- callr::r_bg(function(puerto, cfg, log) {
    lentos <- new.env()
    filas <- function(ini, libro, desde, n) {
      if (n <= 0) return(list())
      lapply(seq_len(n) + desde, function(k) list(
        DocNum = 100000L + k, NumAtCard = sprintf("F-%s-%s-%05d", ini, libro, k),
        CardCode = sprintf("C%04d", k %% 37), CardName = sprintf("CLIENTE FICTICIO %d SA", k %% 37),
        FederalTaxID = "XAXX010101000", DocDate = "2026-11-01", DocDueDate = "2026-12-01",
        DocCurrency = "MXN", DocTotal = 1000 + k, DocTotalFc = 0, PaidToDate = 0))
    }
    app <- list(call = function(req) {
      partes <- strsplit(sub("^/", "", req$PATH_INFO), "/")[[1]]
      ini <- partes[1]; recurso <- utils::tail(partes, 1)
      conf <- jsonlite::read_json(cfg)
      c_ini <- conf[[ini]]
      if (identical(recurso, "Login"))
        return(list(status = 200L, headers = list("Content-Type" = "application/json",
                                                  "Set-Cookie" = "B1SESSION=sesion-ficticia; Path=/"),
                    body = "{}"))
      if (is.null(c_ini)) return(list(status = 404L, headers = list(), body = "{}"))
      q <- req$QUERY_STRING %||% ""
      pares <- strsplit(strsplit(sub("^\\?", "", q), "&")[[1]], "=")
      qs <- stats::setNames(lapply(pares, function(p) utils::URLdecode(p[2] %||% "")),
                            vapply(pares, function(p) utils::URLdecode(p[1]), ""))
      skip <- as.integer(qs[["$skip"]] %||% 0L)
      prefer <- req$HTTP_PREFER %||% ""
      tam <- if (grepl("odata.maxpagesize=", prefer)) min(1000L, as.integer(sub(".*odata.maxpagesize=([0-9]+).*", "\\1", prefer))) else 20L
      top <- as.integer(qs[["$top"]] %||% tam); tam <- min(tam, top)
      libro <- if (identical(recurso, "Invoices")) "AR" else "AP"
      total <- as.integer(c_ini[[libro]] %||% 0L)
      if (identical(c_ini$modo, "falla500")) {
        cat(paste(ini, recurso, skip, prefer, -1L, sep = "\t"), "\n", file = log, append = TRUE, sep = "")
        return(list(status = 500L, headers = list("Content-Type" = "application/json"), body = "{\"error\":\"falla ficticia\"}"))
      }
      n <- max(0L, min(tam, total - skip))
      cuerpo <- list(value = filas(ini, libro, skip, n))
      if (skip + n < total) cuerpo[["@odata.nextLink"]] <- sprintf("%s?$skip=%d", recurso, skip + n)
      resp <- list(status = 200L, headers = list("Content-Type" = "application/json"),
                   body = as.character(jsonlite::toJSON(cuerpo, auto_unbox = TRUE)))
      # The slow page answers 5 s later WITHOUT blocking the server (a promise:
      # httpuv keeps serving the client's retry meanwhile, as a real server does).
      if (skip > 0L && (identical(c_ini$modo, "lento_siempre") ||
                         (identical(c_ini$modo, "lento_una_vez") && is.null(lentos[[ini]])))) {
        assign(ini, TRUE, envir = lentos)
        cat(paste(ini, recurso, skip, prefer, -2L, sep = "\t"), "\n", file = log, append = TRUE, sep = "")
        return(promises::promise(function(resolve, reject) later::later(function() resolve(resp), 5)))
      }
      cat(paste(ini, recurso, skip, prefer, n, sep = "\t"), "\n", file = log, append = TRUE, sep = "")
      resp
    })
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
    s <- httpuv::startServer("127.0.0.1", puerto, app)
    repeat httpuv::service(100)
  }, args = list(puerto = puerto, cfg = cfg, log = log), supervise = TRUE)
  # Wait until it answers.
  url_de <- function(ini) sprintf("http://127.0.0.1:%d/%s/b1s/v1", puerto, ini)
  listo <- FALSE
  for (i in 1:100) {
    ok <- tryCatch({ httr::POST(paste0(url_de("X"), "/Login"), httr::timeout(1)); TRUE }, error = function(e) FALSE)
    if (ok) { listo <- TRUE; break }
    Sys.sleep(0.1)
  }
  if (!listo) { proc$kill(); stop("sap_falso: the fake server did not start: ", paste(proc$read_error(), collapse = " ")) }
  list(
    url_de = url_de, puerto = puerto,
    config = function(lista) jsonlite::write_json(lista, cfg, auto_unbox = TRUE),
    log = function() {
      if (!file.exists(log)) return(data.frame(empresa = character(), endpoint = character(), skip = integer(),
                                               prefer = character(), filas = integer()))
      x <- utils::read.delim(log, header = FALSE, colClasses = "character", quote = "")
      stats::setNames(data.frame(x$V1, x$V2, as.integer(x$V3), x$V4, as.integer(x$V5), stringsAsFactors = FALSE),
                      c("empresa", "endpoint", "skip", "prefer", "filas"))
    },
    log_borrar = function() unlink(log),
    parar = function() proc$kill()
  )
}
