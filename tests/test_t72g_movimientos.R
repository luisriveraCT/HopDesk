# =============================================================================
# tests/test_t72g_movimientos.R
# T72-G (movimientos). Moving or restoring a date from the Lupa / Vencidos
# (handle_invoice_action, R/search_module.R) logged mover_fecha /
# restaurar_fecha right away, BEFORE the deferred write ran: if it then
# failed, the log already said "movida" and no _fallo_guardado was left
# (the Calendario's .apply_move() logs after the write). Now the Lupa hands
# its entries (one per ledger, as .log_ledger_action wrote them) to
# .save_moves_deferred(log_info =), which writes them once the write worked,
# or as <action>_fallo_guardado if it never did. Editing an ERP invoice
# with a new date: the date's write leaves a staff-only
# editar_factura_sap/parte, or editar_factura_sap_fallo_guardado.
# Real code: handle_invoice_action(), .save_moves_deferred() and
# R/persistence.R over the in-memory S3 (tests/helpers/s3_memoria.R).
# Run from project root:  Rscript --vanilla tests/test_t72g_movimientos.R
# =============================================================================
cat("=== test_t72g_movimientos ===\n\n")
suppressPackageStartupMessages(library(shiny))
source("tests/helpers/s3_memoria.R")
.app_data_cache <- list()
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".cache_get", ".cache_set", ".cache_scoped_key"))
    eval(e, globalenv())
for (f in c("R/ledger_module.R", "R/search_module.R"))
  for (e in parse(f, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% c(".moves_read_live", ".save_moves_deferred", "handle_invoice_action",
                                    ".fecha_factura_de_llaves"))
      eval(e, globalenv())
for (f in c("R/data_pipeline.R", "R/policy_engine.R"))
  for (e in parse(f, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% c("politica_facturas", ".politica_normalizar_facturas", ".politica_fecha",
                                    ".politica_doc", ".politica_doc_unica", ".mudanza_doc", ".mudanza_fecha",
                                    ".mudanza_docs_unicos", ".mudanza_viejas", "mudanzas_mover", "mudanzas_restaurar"))
      eval(e, globalenv())

showNotification  <- function(...) invisible(NULL)
tr                <- function(clave, ...) clave
bump_sync_version <- function(...) invisible(NULL)
LOGS <- list()
log_action <- function(user, module, action, description = "", target_id = NULL, metadata = NULL, ...)
  { LOGS[[length(LOGS) + 1L]] <<- list(user = user, module = module, action = action, description = description,
                                       target_id = target_id, metadata = metadata); invisible(NULL) }

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"; MK <- paste0(CID, "/", S3_KEYS$moves)
mov <- function(docs, fecha, quien, ledger = "AP") .normalize(tibble::tibble(
  ledger = ledger, Empresa = "NCS", Moneda = "MXN", Documento = docs,
  FechaVenc_Proyectada = as.Date(fecha), moved_by = quien, last_updated = Sys.time()), .schema_moves)
sesion <- function(usuario, copia) {
  moves <- copia
  list(moves_db = function(v) if (missing(v)) moves else moves <<- v,
       current_user = function() usuario, effective_client_id = function() CID,
       home_client_id = function() CID)
}
fila <- function(doc, ledger = "AP") list(ledger = ledger, empresa = "NCS", moneda = "MXN", documento = doc,
                                         source = "sap", inv_id = doc, parte = "PROVEEDOR", importe = 100,
                                         fecha = "2026-11-20")
s3 <- function() get(MK, envir = .mem)
correr <- function() for (i in 1:5) later::run_now()
esperar_reintento <- function() { for (i in 1:3) { later::run_now(); Sys.sleep(1.1); later::run_now(); Sys.sleep(1.1) }; later::run_now() }
inicio <- function() {
  s3_memoria_reset(); LOGS <<- list()
  assign(MK, dplyr::bind_rows(mov(c("A1", "A2", "A3"), "2026-11-25", "inicio"),
                              mov(c("R1", "R2"), "2026-11-25", "inicio", "AR")), envir = .mem)
  sesion("ana", s3())
}
# save_moves() writes without an event (T72_DEUDA_EVENTO): its sin_evento entry is the write layer's, not this test's.
acciones <- function() setdiff_orden(vapply(LOGS, `[[`, "", "action"))
setdiff_orden <- function(x) x[x != "sin_evento"]

# ── 1. Lupa: Mover, AP and AR rows at once ───────────────────────────────
s <- inicio()
handle_invoice_action(list(action = "move", move_to = "2026-12-15",
                           rows = list(fila("A1"), fila("A2"), fila("R1", "AR"))), s)
ok("1 nothing is logged before the deferred write ran", !length(acciones()))
correr()
ok("1 control: the move reached S3", all(as.character(s3()$FechaVenc_Proyectada[s3()$Documento %in% c("A1", "A2", "R1")]) == "2026-12-15"))
ap <- Filter(function(x) identical(x$module, "ledger_AP"), LOGS); ar <- Filter(function(x) identical(x$module, "ledger_AR"), LOGS)
ok("1 after it worked: one mover_fecha row per ledger (AP and AR), nothing else",
   identical(acciones(), c("mover_fecha", "mover_fecha")) && length(ap) == 1L && length(ar) == 1L)
ok("1 …each with the words, target and metadata .log_ledger_action wrote",
   identical(ap[[1]]$description, "2 factura(s) movida(s) a 15/12/2026") && identical(ap[[1]]$target_id, "A1, A2") &&
     identical(ap[[1]]$metadata, list(n = 2L)) && identical(ap[[1]]$user, "ana") &&
     identical(ar[[1]]$description, "1 factura(s) movida(s) a 15/12/2026") && identical(ar[[1]]$target_id, "R1"))

# ── 2. Lupa: Restaurar ───────────────────────────────────────────────────
s <- inicio()
handle_invoice_action(list(action = "restore", rows = list(fila("A1"))), s)
ok("2 restore: nothing logged before the write", !length(acciones()))
correr()
ok("2 restore: one restaurar_fecha row after it worked, its words of always",
   identical(acciones(), "restaurar_fecha") &&
     identical(Filter(function(x) x$action == "restaurar_fecha", LOGS)[[1]]$description, "1 factura(s) restaurada(s) a su fecha original") &&
     !"A1" %in% s3()$Documento)

# ── 3. Lupa: the write never works ───────────────────────────────────────
s <- inicio(); .mem_falla_escritura <- MK
handle_invoice_action(list(action = "move", move_to = "2026-12-15",
                           rows = list(fila("A1"), fila("R1", "AR"))), s)
esperar_reintento()
.mem_falla_escritura <- character()
ok("3 a failed write: no mover_fecha (it never moved), one mover_fecha_fallo_guardado per ledger",
   identical(sort(acciones()), c("mover_fecha_fallo_guardado", "mover_fecha_fallo_guardado")))
ok("3 …saying it was not applied", all(startsWith(vapply(Filter(function(x) x$action != "sin_evento", LOGS), `[[`, "", "description"), "FALLO AL GUARDAR")))

# ── 4. Editar factura: the date's part ───────────────────────────────────
parte <- list(user = "ana", module = "ledger_AP", action = "editar_factura_sap/parte",
              description = "Factura A1: fecha 15/12/2026", target_id = "A1",
              metadata = list(empresa = "NCS", tabla = S3_KEYS$moves), client_id = CID, viewer_home_client_id = CID)
s <- inicio()
.save_moves_deferred(mov("A1", "2026-12-15", "ana"), client_id = CID, log_info = parte,
                     rebase = function(fresh) mudanzas_mover(fresh, mov("A1", "2026-12-15", "ana"), NULL))
correr()
ok("4 editar factura: the date's write leaves editar_factura_sap/parte (staff only) once it worked",
   identical(acciones(), "editar_factura_sap/parte") && identical(Filter(function(x) x$action == "editar_factura_sap/parte", LOGS)[[1]]$metadata$tabla, S3_KEYS$moves))
s <- inicio(); .mem_falla_escritura <- MK
.save_moves_deferred(mov("A1", "2026-12-15", "ana"), client_id = CID, log_info = parte,
                     rebase = function(fresh) mudanzas_mover(fresh, mov("A1", "2026-12-15", "ana"), NULL))
esperar_reintento()
.mem_falla_escritura <- character()
ok("4 …and if it never works, editar_factura_sap_fallo_guardado (the client sees it), no /parte",
   identical(acciones(), "editar_factura_sap_fallo_guardado"))

# ── 5. The Calendario's single entry, as before ──────────────────────────
s <- inicio()
.save_moves_deferred(mov("A1", "2026-12-15", "ana"), client_id = CID,
                     log_info = list(user = "ana", module = "ledger_AP", action = "mover_fecha",
                                     description = "1 factura(s) movida(s) a 15/12/2026", target_id = "A1",
                                     metadata = list(n = 1L), client_id = CID, viewer_home_client_id = CID),
                     rebase = function(fresh) mudanzas_mover(fresh, mov("A1", "2026-12-15", "ana"), NULL))
correr()
ok("5 one log_info entry (the Calendario's) is written once, as before", identical(acciones(), "mover_fecha"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
