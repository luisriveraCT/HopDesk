# =============================================================================
# scripts/diagnose_t106_sap_campos_readonly.R -- T106 2c: does SAP keep the
# CFDI's fiscal UUID, and under which field name?  READ ONLY.
#
#   Rscript --vanilla scripts/diagnose_t106_sap_campos_readonly.R --cliente networks --produccion
#   (one click: scripts/diagnose_t106_sap_campos.bat; inside the VPN, like
#   the ERP refresher -- same environment checks, same credentials)
#
# For each company of the client: ONE GET of Invoices and ONE of
# PurchaseInvoices, $top=1, no $select. It prints FIELD NAMES ONLY -- never a
# value: how many fields the invoice has, the ones whose name looks fiscal
# (UUID, Folio, EDoc, CFDI, Timbre, SAT, user fields U_*) saying only "con
# valor" / "vacío", and the fields whose VALUE has the shape of a UUID
# (8-4-4-4-12 hex) -- the name is printed, the value never. Nothing is
# written anywhere: no S3 write, no SAP write, no log entry.
# Exit codes: 0 done · 1 setup · 2 no connection to the ERP (VPN?).
# =============================================================================

.RAIZ_ERP <- "scripts/actualizar_erp.R"

# The names of one invoice's fields, classified; never their values.
# fila: a named list (one record of the Service Layer's JSON).
diag_campos_resumen <- function(fila) {
  if (!length(fila)) return(data.frame(campo = character(), con_valor = logical(), fiscal = logical(),
                                       forma_uuid = logical(), stringsAsFactors = FALSE))
  nombres <- names(fila)
  valor_txt <- vapply(fila, function(v) if (is.null(v) || (length(v) == 1L && is.na(v)) || is.list(v)) "" else as.character(v)[1], "")
  data.frame(
    campo      = nombres,
    con_valor  = nzchar(trimws(valor_txt)),
    fiscal     = grepl("uuid|folio|fiscal|edoc|cfdi|timbre|sat|^U_", nombres, ignore.case = TRUE),
    forma_uuid = grepl("^[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}$", trimws(valor_txt)),
    stringsAsFactors = FALSE)
}

# The lines printed for one endpoint of one company: names only.
diag_lineas <- function(ini, endpoint, res) {
  if (!nrow(res)) return(sprintf("  %-5s %-17s sin registros (nada que revisar)", ini, endpoint))
  fis <- res[res$fiscal, , drop = FALSE]
  c(sprintf("  %-5s %-17s %d campos", ini, endpoint, nrow(res)),
    if (nrow(fis)) sprintf("        nombre fiscal: %s", paste(sprintf("%s (%s)", fis$campo, ifelse(fis$con_valor, "con valor", "vacío")), collapse = ", "))
    else "        nombre fiscal: ninguno",
    if (any(res$forma_uuid)) sprintf("        VALOR CON FORMA DE UUID en: %s", paste(res$campo[res$forma_uuid], collapse = ", "))
    else "        valor con forma de UUID: ninguno")
}

diag_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (!file.exists(.RAIZ_ERP)) { cat("Córrelo desde la raíz del checkout (donde está scripts/).\n"); return(invisible(1L)) }
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
  assign("%||%", `%||%`, envir = globalenv())
  sys.source(.RAIZ_ERP, envir = globalenv())   # erp_args/erp_entorno/erp_cargar/erp_empresas, nothing runs
  a <- tryCatch(erp_args(c(setdiff(args, "--prueba"), "--prueba")), error = function(e) e)   # --prueba: no --usuario needed
  if (inherits(a, "error")) { cat("Uso:", conditionMessage(a), "\n"); return(invisible(1L)) }
  bucket <- tryCatch(erp_entorno(a), error = function(e) e)
  if (inherits(bucket, "error")) { cat("Entorno:", conditionMessage(bucket), "\n"); return(invisible(1L)) }
  suppressMessages(erp_cargar())
  cat(sprintf("Campos fiscales en SAP (solo nombres, nunca valores) · cliente %s\n", a$cliente))
  cmap <- tryCatch(suppressMessages(erp_empresas(a$cliente)), error = function(e) e)
  if (inherits(cmap, "error")) { cat("Empresas:", conditionMessage(cmap), "\n"); return(invisible(1L)) }
  for (ini in names(cmap)) {
    url <- tryCatch(suppressMessages(.sap_url_configurada(ini, client_id = a$cliente)), error = function(e) "")
    if (!nzchar(url)) { cat(sprintf("  %-5s sin credenciales de ERP: se omite\n", ini)); next }
    ok <- tryCatch({ suppressMessages(sap_login(ini, client_id = a$cliente)); TRUE },
                   sap_connection_error = function(e) e, error = function(e) e)
    if (inherits(ok, "sap_connection_error")) {
      cat(sprintf("No hay conexión con el ERP de %s: ¿está la VPN arriba? (%s)\n", ini, .sin_url(conditionMessage(ok))))
      return(invisible(2L))
    }
    if (inherits(ok, "error")) { cat(sprintf("  %-5s el ERP rechazó el inicio de sesión: %s\n", ini, .sin_url(conditionMessage(ok)))); next }
    s <- .sap_sessions[[ini]]
    for (ep in c("Invoices", "PurchaseInvoices")) {
      r <- tryCatch(httr::GET(paste0(s$base_url, "/", ep), query = list(`$top` = 1),
                              httr::timeout(60), httr::add_headers(Cookie = s$cookie, Prefer = "odata.maxpagesize=1"),
                              httr::config(ssl_verifypeer = s$ssl_verify, connecttimeout = 10L)),
                    error = function(e) e)
      if (inherits(r, "error") || httr::http_error(r)) {
        cat(sprintf("  %-5s %-17s no se pudo leer (%s)\n", ini, ep,
                    if (inherits(r, "error")) .sin_url(conditionMessage(r)) else paste("HTTP", httr::status_code(r))))
        next
      }
      cuerpo <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)
      fila <- if (length(cuerpo$value)) cuerpo$value[[1]] else list()
      cat(diag_lineas(ini, ep, diag_campos_resumen(fila)), sep = "\n")
    }
    tryCatch(suppressMessages(sap_logout(ini)), error = function(e) NULL)
  }
  cat("Listo. Copia este texto y mándalo: trae nombres de campos, nunca valores.\n")
  invisible(0L)
}

if (sys.nframe() == 0L) quit(status = as.integer(diag_main()), save = "no")
