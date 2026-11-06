# =============================================================================
# tests/test_descargar_pagos_zip_bytes.R
# Characterization of "Descargar todos los pagos" (T21): the payment files
# inside the ZIP, byte by byte. The real pagarHoyServer's download_all over a
# fixed Agenda (tests/helpers/agenda_fixture_app.R's data and stubs, plus a
# BanBajío supplier) and a fixed date; each TXT's name and MD5 must match the
# reference taken on master 03638ae, before the preview moved to the engine.
# The .zip container itself is not compared: it carries file times.
#
# Covered: two companies, MXN and USD, a supplier without CLABE and one not
# in the catalog (both left out), an abono, a BanBajío CLABE (BCO), the
# Referencia by the SAP lookup (Factura) and by Documento.
#
# Run from project root:  Rscript tests/test_descargar_pagos_zip_bytes.R
# ZIP_REFERENCIA=1 prints the current names and hashes instead (to take a
# new reference, only when the files are meant to change).
# =============================================================================
cat("=== test_descargar_pagos_zip_bytes ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

invisible(capture.output(source("tests/helpers/agenda_fixture_app.R")))
Sys.Date <- function() as.Date(Sys.getenv("ZIP_FECHA", "2026-11-05"))   # the module's functions live here too
catalogo_zip <- dplyr::bind_rows(catalogo, tibble::tibble(
  nombre = "SERVICIOS BAJIO SA", alias = "SERVBAJIO", clabe = "030180003333333333",
  medio_pago = "SPEI", banco = "030", Empresa = NA_character_))
agenda_zip <- dplyr::bind_rows(agenda0, pago("P7", "SERVICIOS BAJIO SA", "4007", 2500.75, "2026-11-04"))

descargar <- function() {
  dir <- tempfile(); dir.create(dir)
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP, OTR = OTR)), proveedores_db = reactiveVal(catalogo_zip),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda_zip),
    sap_data = reactiveVal(list(AP = data.frame(Empresa = c("NTS", "NTS", "OTR"), Documento = c("4001", "4002", "4001"),
                                                Factura = c("F-4001", "F-4002", "F-OTRA"),
                                                Parte = c("ACEROS MONTERREY SA", "BODEGAS DEL NORTE SA", "OTRA EMPRESA"),
                                                check.names = FALSE))),
    sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis", home_client_id = function() "dev")
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    # Unzipped here: the downloaded file goes with the session.
    utils::unzip(output$download_all, exdir = dir)
  })))
  archivos <- sort(list.files(dir))
  stats::setNames(unname(tools::md5sum(file.path(dir, archivos))), archivos)
}

h <- tryCatch(descargar(), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
if (identical(Sys.getenv("ZIP_REFERENCIA"), "1")) { print(h); quit(status = 0L) }

REFERENCIA <- c(
  "pagos_Networks Trucking Services_MXN_05112026_001.txt" = "88ccbd2565271b0503d6a35c73f8a760",
  "pagos_Networks Trucking Services_USD_05112026_001.txt" = "859d61c30e95a4c7bd4dc8427d1bd8d5",
  "pagos_Otra Empresa SA_MXN_05112026_001.txt"            = "bc8470924a0986306a121970f843de4e")

ok("el ZIP trae los mismos archivos, con los mismos nombres", identical(names(h), names(REFERENCIA)))
for (a in names(REFERENCIA))
  ok(sprintf("%s: idéntico byte a byte (MD5)", a), identical(unname(h[a]), unname(REFERENCIA[a])))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
