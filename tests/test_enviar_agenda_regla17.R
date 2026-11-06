# =============================================================================
# tests/test_enviar_agenda_regla17.R
# T23 · 6: Vencidos and the lupa send invoices to the Agenda through
# handle_invoice_action() (R/search_module.R, stage_all / stage_selected).
# Regla 17: save -> if it fails, say only the error and leave the session as
# it was -> show it in the session -> say it was sent. It used to put the
# rows in the session before saving and, on a failed save, say the error AND
# "✓ N factura(s) enviadas"; and a failed fresh read fell back to the
# session's copy and saved on it.
#   1. the read fails: nothing saved, only the error, the session untouched;
#   2. the save fails: only the error (no "enviadas"), the session untouched;
#   3. it saves: the session shows what was written, and "✓ enviadas".
#
# Run from project root:  Rscript tests/test_enviar_agenda_regla17.R
# =============================================================================
cat("=== test_enviar_agenda_regla17 ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/search_module.R", "handle_invoice_action")
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", "fmt_money")
suppressMessages(source("R/i18n.R"))

# Stand-ins: what it reads and writes, and what it says.
AVISOS <- character(); GUARDADOS <- list(); FALLA_LEER <- FALSE; FALLA_GUARDAR <- FALSE
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
puede_o_avisa <- function(...) TRUE
log_action <- function(...) invisible(NULL)
pasivos_filter_out_provisions <- function(k) k
facturas_filas_agenda <- function(keys_df, new_rows, ...) new_rows
upsert_pagar_hoy <- function(db, nuevas) dplyr::bind_rows(db, nuevas)
AGENDA_S3 <- data.frame(id = "a0", ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = "9000",
                        stringsAsFactors = FALSE)
safe_load_pagar_hoy <- function(...) if (FALLA_LEER) stop("S3 no responde") else AGENDA_S3
save_pagar_hoy <- function(df, ...) {
  if (FALLA_GUARDAR) stop("escritura rechazada")
  GUARDADOS[[length(GUARDADOS) + 1L]] <<- df
  invisible(df)
}

fila <- list(ledger = "AP", empresa = "NTS", moneda = "MXN", documento = "4001", source = "sap", inv_id = "i1",
             parte = "ACEROS", codigo = "", importe = 1500, fecha = "05/11/2026", tipo = "", fecha_factura = "",
             confirmed = FALSE)
SESION <- data.frame(id = "s0", ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = "8000",
                     stringsAsFactors = FALSE)
correr <- function(leer_falla = FALSE, guardar_falla = FALSE) {
  AVISOS <<- character(); GUARDADOS <<- list(); FALLA_LEER <<- leer_falla; FALLA_GUARDAR <<- guardar_falla
  ph <- reactiveVal(SESION)
  shared <- list(current_user = function() "luis", effective_client_id = function() "acme",
                 home_client_id = function() "acme", pagar_hoy_db = ph,
                 bancos_confirmados = function() NULL)
  error <- tryCatch({ isolate(handle_invoice_action(list(action = "stage_selected", rows = list(fila)), shared)); NULL },
                    error = function(e) conditionMessage(e))
  if (!is.null(error)) message("  ", error)
  list(error = error, sesion = isolate(ph()), avisos = AVISOS, guardados = GUARDADOS)
}
enviadas <- function(av) any(grepl("enviadas a", av, fixed = TRUE))

r <- correr(leer_falla = TRUE)
ok("lectura fallida: no se guarda nada (antes guardaba sobre la copia de la sesión)", is.null(r$error) && !length(r$guardados))
ok("lectura fallida: solo el error, sin “✓ enviadas”",
   any(grepl("No se pudo leer la Agenda", r$avisos, fixed = TRUE)) && !enviadas(r$avisos))
ok("lectura fallida: la sesión queda como estaba", identical(r$sesion, SESION))

r <- correr(guardar_falla = TRUE)
ok("guardado fallido: solo el error, sin “✓ enviadas”",
   is.null(r$error) && any(grepl("No se pudo guardar en la Agenda", r$avisos, fixed = TRUE)) && !enviadas(r$avisos))
ok("guardado fallido: la sesión queda como estaba (no muestra lo que no se guardó)", identical(r$sesion, SESION))

r <- correr()
ok("guardado bien: la sesión muestra lo escrito (lo de S3 + la nueva), no la copia vieja",
   is.null(r$error) && length(r$guardados) == 1L && identical(r$sesion, r$guardados[[1]]) &&
     setequal(r$sesion$Documento, c("9000", "4001")))
ok("guardado bien: avisa “✓ 1 factura(s) enviadas” y ningún error",
   enviadas(r$avisos) && !any(grepl("No se pudo", r$avisos, fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
