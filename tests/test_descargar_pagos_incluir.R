# =============================================================================
# tests/test_descargar_pagos_incluir.R
# T21b (Luis): in the "Descargar todos" preview each payment without an
# account (no alias or no CLABE) starts in "Omitir" and can be switched to
# "Incluir sin cuenta"; those go in a separate _002 file per company and
# currency, so the _001 never changes. On the real pagarHoyServer over the
# Agenda fixture (tests/helpers/agenda_fixture_app.R) and a fixed date:
#   1. with three included, the _001 files are still byte-identical to the
#      reference of test_descargar_pagos_zip_bytes; the _002 lines: 384 chars,
#      CLABE in zeros, bank 000, SPI / SPD by currency, alias blank, the
#      amount, the trailer's count and total, consecutive 002;
#   2. the button and the pills count what goes in plus what is included;
#   3. the USD _002 takes the USD origin account (the currency changes it);
#   4. a payment that got its account between preview and download goes in
#      _001 only, never in both; one taken out of the Agenda goes nowhere;
#   5. every opening starts in "Omitir" (a choice from an earlier one drops);
#   6. a company and currency with only included payments gets a _002 alone;
#   7. who included what is logged; nothing is logged with all in "Omitir";
#      a failed log is shown to the user.
#
# Run from project root:  Rscript tests/test_descargar_pagos_incluir.R
# =============================================================================
cat("=== test_descargar_pagos_incluir ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

invisible(capture.output(source("tests/helpers/agenda_fixture_app.R")))
Sys.Date <- function() as.Date("2026-11-05")
catalogo_zip <- dplyr::bind_rows(catalogo, tibble::tibble(
  nombre = "SERVICIOS BAJIO SA", alias = "SERVBAJIO", clabe = "030180003333333333",
  medio_pago = "SPEI", banco = "030", Empresa = NA_character_))
agenda_zip <- dplyr::bind_rows(agenda0, pago("P7", "SERVICIOS BAJIO SA", "4007", 2500.75, "2026-11-04"))
# The _001 reference of test_descargar_pagos_zip_bytes (taken on 03638ae).
REF_001 <- c(
  "pagos_Networks Trucking Services_MXN_05112026_001.txt" = "88ccbd2565271b0503d6a35c73f8a760",
  "pagos_Networks Trucking Services_USD_05112026_001.txt" = "859d61c30e95a4c7bd4dc8427d1bd8d5",
  "pagos_Otra Empresa SA_MXN_05112026_001.txt"            = "bc8470924a0986306a121970f843de4e")

LOGS <- list(); AVISOS <- character()
log_action <- function(user, module, action, description = "", ...) {
  LOGS[[length(LOGS) + 1L]] <<- list(user = user, action = action, args = list(...)); invisible(NULL)
}
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
get_cuenta_ppl_fixture <- get_cuenta_ppl

# Opens the preview, applies `antes(session)` (choices, changes), downloads;
# returns the files (name -> lines), the MD5s, the outputs' text.
correr <- function(antes = function(session, shared) NULL, agenda = agenda_zip, catalogo_v = catalogo_zip) {
  LOGS <<- list(); AVISOS <<- character()
  res <- list()
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP, OTR = OTR)), proveedores_db = reactiveVal(catalogo_v),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda),
    # The same SAP rows as test_descargar_pagos_zip_bytes (the Referencia lookup).
    sap_data = reactiveVal(list(AP = data.frame(Empresa = c("NTS", "NTS", "OTR"), Documento = c("4001", "4002", "4001"),
                                                Factura = c("F-4001", "F-4002", "F-OTRA"),
                                                Parte = c("ACEROS MONTERREY SA", "BODEGAS DEL NORTE SA", "OTRA EMPRESA"),
                                                check.names = FALSE))),
    sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis", home_client_id = function() "dev")
  error <- tryCatch({
    invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
      session$userData$anota <- function(...) invisible(NULL)
      session$flushReact()
      session$setInputs(preview_all = 1)
      antes(session, shared)
      res$salidas <<- paste(as.character(output$descargar_resumen$html), as.character(output$descargar_boton$html))
      res$incluidos <<- .descargar_incluidos()
      dir <- tempfile(); dir.create(dir)
      utils::unzip(output$download_all, exdir = dir)
      a <- sort(list.files(dir))
      res$md5 <<- stats::setNames(unname(tools::md5sum(file.path(dir, a))), a)
      res$archivos <<- stats::setNames(lapply(file.path(dir, a), readLines, warn = FALSE), a)
    }))); NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(error)) message("  ", error)
  res$error <- error
  res
}
incluir <- function(...) { ids <- paste0(c(...), "#1")
  function(session, shared) session$setInputs(`descargar_t-opciones` = list(ids = ids, valores = rep("incluir", length(ids)))) }
f002 <- function(emp, mon) sprintf("pagos_%s_%s_05112026_002.txt", emp, mon)
campo <- function(l, a, b) substr(l, a + 1L, b)   # 0-based [a:b) as in ppl_generator.R

# ── 1-3. Three included: P2 (MXN, sin CLABE), P3 (MXN, sin alias), P6 (USD, sin CLABE)
r <- correr(incluir("P2", "P3", "P6"))
ok("se incluyen los tres, por id", is.null(r$error) && setequal(r$incluidos, c("P2", "P3", "P6")))
ok("los _001 siguen idénticos byte a byte a la referencia, aunque haya incluidos",
   all(names(REF_001) %in% names(r$md5)) && identical(unname(r$md5[names(REF_001)]), unname(REF_001)))
ok("los incluidos van en un _002 por empresa y moneda (NTS MXN y NTS USD), y en nada más",
   setequal(setdiff(names(r$md5), names(REF_001)), c(f002(EMP, "MXN"), f002(EMP, "USD"))))
mxn <- r$archivos[[f002(EMP, "MXN")]]; usd <- r$archivos[[f002(EMP, "USD")]]
det <- grep("^02", mxn, value = TRUE)
ok("_002 MXN: encabezado con consecutivo 002 y dos líneas de 384", startsWith(mxn[1], "01") &&
     endsWith(mxn[1], "002") && length(det) == 2L && all(nchar(det) == 384L))
ok("la línea sin cuenta: CLABE en ceros, banco 000, SPI, alias en blanco",
   length(det) == 2L && all(campo(det, 68, 86) == "000000000000000000") && all(campo(det, 35, 38) == "000") &&
     all(campo(det, 61, 64) == "SPI") && all(campo(det, 95, 110) == strrep(" ", 15)))
ok("los importes en centavos (8200 y 3100) y la cola cuenta 2 y suma 11300.00",
   setequal(campo(det, 38, 53), c("000000000820000", "000000000310000")) &&
     identical(mxn[length(mxn)], paste0("09", "0000004", "0000002", formatC(1130000, width = 18, flag = "0", format = "d"))))
det_u <- grep("^02", usd, value = TRUE)
ok("_002 USD: SPD, CLABE en ceros, 300.00", length(det_u) == 1L && campo(det_u, 61, 64) == "SPD" &&
     campo(det_u, 68, 86) == "000000000000000000" && campo(det_u, 38, 53) == "000000000030000")
ok("botón: 8 pagos (5 que entran + 3 sin cuenta), con su aviso",
   grepl(tr("tablas.pagos.descargar_zip_sin_cuenta", n = 8, m = 3), r$salidas, fixed = TRUE) &&
     grepl(tr("tablas.pagos.aviso_sin_cuenta", m = 3), r$salidas, fixed = TRUE))
ok("píldoras: MXN 4 que entran + 2 sin cuenta ($11,300.00); USD 1 + 1 sin cuenta ($300.00)",
   grepl(tr("tablas.pagos.pildora_sin_cuenta", n = 2, total = fmt_money(11300)), r$salidas, fixed = TRUE) &&
     grepl(tr("tablas.pagos.pildora_sin_cuenta", n = 1, total = fmt_money(300)), r$salidas, fixed = TRUE) &&
     grepl(tr("tablas.pagos.pildora", n = 4, moneda = "MXN", total = fmt_money(18600.75)), r$salidas, fixed = TRUE))
inc_log <- Filter(function(l) identical(l$action, "incluir_sin_cuenta"), LOGS)
ok("auditoría: un incluir_sin_cuenta de luis con los tres pagos",
   length(inc_log) == 1L && identical(inc_log[[1]]$user, "luis") &&
     setequal(vapply(inc_log[[1]]$args$metadata$pagos, function(p) p$id, ""), c("P2", "P3", "P6")))

# The USD origin account: the currency changes it.
get_cuenta_ppl <- function(emp, cur, shared) if (toupper(cur) == "USD") "013386156002" else "073543010201"
r <- correr(incluir("P6"))
det_u <- grep("^02", r$archivos[[f002(EMP, "USD")]], value = TRUE)
ok("el _002 USD lleva la cuenta origen en dólares (la moneda cambia la cuenta)",
   is.null(r$error) && length(det_u) == 1L && campo(det_u, 19, 31) == "013386156002")
get_cuenta_ppl <- get_cuenta_ppl_fixture

# ── 4. Never in both; out of the Agenda, nowhere
con_clabe <- catalogo_zip; con_clabe$clabe[con_clabe$nombre == "BODEGAS DEL NORTE SA"] <- "014180009999999999"
r <- correr(function(session, shared) { incluir("P2")(session, shared); shared$proveedores_db(con_clabe) })
lin_001 <- grep("^02", r$archivos[[sprintf("pagos_%s_MXN_05112026_001.txt", EMP)]], value = TRUE)
ok("P2 ganó su CLABE entre abrir y descargar: va solo en el _001, nunca también en el _002",
   is.null(r$error) && !f002(EMP, "MXN") %in% names(r$md5) && any(campo(lin_001, 68, 86) == "014180009999999999"))
r <- correr(function(session, shared) { incluir("P3")(session, shared); shared$pagar_hoy_db(dplyr::filter(agenda_zip, id != "P3")) })
ok("P3 salió de la Agenda antes de descargar: no va en ningún archivo",
   is.null(r$error) && !f002(EMP, "MXN") %in% names(r$md5))

# ── 5. Every opening starts in "Omitir"
r <- correr(function(session, shared) { incluir("P3")(session, shared); session$setInputs(preview_all = 2) })
ok("al reabrir, la decisión anterior no cuenta: nada incluido y el ZIP como siempre",
   is.null(r$error) && !length(r$incluidos) && identical(unname(r$md5), unname(REF_001)) &&
     grepl(tr("tablas.pagos.descargar_zip", n = 5), r$salidas, fixed = TRUE))

# ── 6. Only included payments in a company and currency: a _002 alone
agenda_q2 <- dplyr::bind_rows(agenda_zip, dplyr::mutate(pago("Q2", "TRANSPORTES DESCONOCIDOS", "5001", 990, "2026-11-04",
                                                            moneda = "USD"), Empresa = OTR))
r <- correr(incluir("Q2"), agenda = agenda_q2)
ok("OTR USD sin pagos con cuenta: sale su _002 solo, con ese nombre",
   is.null(r$error) && f002(OTR, "USD") %in% names(r$md5) &&
     !sprintf("pagos_%s_USD_05112026_001.txt", OTR) %in% names(r$md5))

# ── 7. Nothing included, nothing logged; a failed log is shown
r <- correr()
ok("todo en Omitir: ningún incluir_sin_cuenta en la auditoría",
   is.null(r$error) && !any(vapply(LOGS, function(l) identical(l$action, "incluir_sin_cuenta"), logical(1))))
log_action <- function(user, module, action, ...) if (identical(action, "incluir_sin_cuenta")) stop("S3 no disponible") else invisible(NULL)
r <- correr(incluir("P3"))
ok("si la auditoría falla, el usuario lo ve (y el archivo sale)",
   is.null(r$error) && f002(EMP, "MXN") %in% names(r$md5) &&
     any(grepl(tr("tablas.pagos.auditoria_falla", error = "S3 no disponible"), AVISOS, fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
