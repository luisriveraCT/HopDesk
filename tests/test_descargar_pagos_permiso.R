# =============================================================================
# tests/test_descargar_pagos_permiso.R
# "Descargar todos los pagos" is can_register_payments (Luis, 2026-11-05).
# Without it: no button, the preview builds nothing, the ZIP is not built
# and nothing is downloaded. With it, the same three work (control).
# The real pagarHoyServer over tests/helpers/agenda_fixture_app.R's Agenda.
#
# Run from project root:  Rscript tests/test_descargar_pagos_permiso.R
# =============================================================================
cat("=== test_descargar_pagos_permiso ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

invisible(capture.output(source("tests/helpers/agenda_fixture_app.R")))
# The fixture stubs puede_o_avisa() to TRUE; this test needs the real check.
for (f in c("R/tier_registry.R", "R/permisos.R")) sys.source(f, envir = globalenv())

# The module calls showModal() unqualified: count the modals it opens.
modales <- 0L
showModal <- function(...) modales <<- modales + 1L

probar <- function(con_permiso) {
  modales <<- 0L
  r <- list()
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP, OTR = OTR)), proveedores_db = reactiveVal(catalogo),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda0),
    sap_data = reactiveVal(list(AP = data.frame(Empresa = "NTS", Documento = "4001", Factura = "F-4001",
                                                Parte = "ACEROS MONTERREY SA", check.names = FALSE))),
    sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list(can_register_payments = con_permiso))),
    home_client_id = function() "dev")
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    r$boton <<- grepl("preview_all", paste(unlist(output$preview_all_ui), collapse = " "), fixed = TRUE)
    session$setInputs(preview_all = 1)
    r$modales <<- modales
    r$zip <<- tryCatch({ f <- output$download_all; file.exists(f) && file.size(f) > 0 },
                       error = function(e) FALSE)
  })))
  r
}

sin <- probar(FALSE)
ok("sin permiso: no hay botón", identical(sin$boton, FALSE))
ok("sin permiso: la vista previa no abre", identical(sin$modales, 0L))
ok("sin permiso: no se arma ni se descarga el ZIP", identical(sin$zip, FALSE))

con <- probar(TRUE)
ok("con permiso: hay botón", isTRUE(con$boton))
ok("con permiso: la vista previa abre", identical(con$modales, 1L))
ok("con permiso: el ZIP se descarga", isTRUE(con$zip))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
