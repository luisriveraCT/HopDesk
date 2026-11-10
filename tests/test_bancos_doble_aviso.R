# =============================================================================
# tests/test_bancos_doble_aviso.R
# CM19 (4), 2026-11-06 -- rule 17: a failed save is never announced as
# success. Two Bancos handlers told "Error al guardar" and then, anyway,
# showed their success notice and logged the action as done:
#   - .bnc_deshacer_abono (also put the voided copy in shared$abonos_db
#     before saving it);
#   - do_delete_conf_confirm (two saves: the linked movement and the
#     confirmation).
# Now each save goes through .bnc_guardar(): one error notice, <accion>_fallido
# in the log, no success notice or log, the session's copy untouched.
# On the real bancosServer in testServer over the in-memory S3 (the harness
# of test_bancos_guardados.R).
#
# Run from project root:  Rscript tests/test_bancos_doble_aviso.R
# =============================================================================
cat("=== test_bancos_doble_aviso ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
.falla_escritura <- character(0)
local({
  guardar_mem <- get("s3saveRDS", asNamespace("aws.s3"))
  utils::assignInNamespace("s3saveRDS", function(x, object, bucket, ...) {
    if (object %in% .falla_escritura) stop("simulated S3 write failure")
    guardar_mem(x, object, bucket, ...)
  }, "aws.s3")
  # A refused write refuses .s3_write's fallback too (put_object), as a down
  # S3 would: s3_memoria now stores put_object in memory, where it used to
  # reach the real aws.s3 -- the network -- and fail there.
  poner_mem <- get("put_object", asNamespace("aws.s3"))
  utils::assignInNamespace("put_object", function(file, object, bucket, ...) {
    if (object %in% .falla_escritura) stop("simulated S3 write failure")
    poner_mem(file, object, bucket, ...)
  }, "aws.s3")
})
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", c("fmt_money", "is_erp_sourced"))
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", "load_ctas_bancos"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/bancos_parser.R", "R/bancos_detect.R", "R/bancos_persistence.R", "R/bancos_formatos.R",
            "R/bancos_module.R"))
  suppressMessages(source(f))

LOG <- character(0); AVISOS <- character(0)
log_action <- function(user, module, action, description = "", ...) LOG <<- c(LOG, action)
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, paste0(type, ": ", as.character(ui))); invisible("id") }
removeModal <- function(...) invisible(NULL)
showModal <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede <- function(...) TRUE
puede_o_avisa <- function(...) TRUE
pasivos_provision_revive <- function(...) invisible(NULL)
limpiar <- function() { LOG <<- character(0); AVISOS <<- character(0) }
exito <- function() any(grepl("^message: ", AVISOS))

CID <- "acme"
MK <- paste0(CID, "/", S3_KEYS$bancos_movimientos)
CK <- paste0(CID, "/", S3_KEYS$bancos_confirmados)
AK <- paste0(CID, "/", S3_KEYS$abonos)
mov <- function(id) tibble::tibble(id = id, cuenta_id = "c1", fecha = as.Date("2026-10-30"), hora = "10:00:00",
                                   recibo = id, descripcion_raw = id, tipo = "spei_out", parte = "PROV", rfc = NA_character_,
                                   referencia = NA_character_, clave_rastreo = NA_character_, concepto = "c", cargo = 100,
                                   abono = 0, saldo_banco = 1000, conciliado = TRUE, doc_vinculado = NA_character_,
                                   agenda_id = NA_character_, importado_at = Sys.time(), fuente = "txt", eliminado = FALSE,
                                   eliminado_por = NA_character_, eliminado_at = as.POSIXct(NA), notas = NA_character_)
movs0 <- .normalize(mov("m1"), .schema_bancos_movimientos)
conf0 <- .normalize(tibble::tibble(confirmacion_id = "k1", agenda_item_id = "ag1", empresa = "NTS", parte = "PROV",
                                   documento = "D1", importe = 100, moneda = "MXN", cuenta_id = "c1",
                                   fecha = as.Date("2026-10-30"), tipo = "pago", mov_id = "m1",
                                   confirmado_at = Sys.time(), eliminado = FALSE), .schema_bancos_confirmados)
ab0 <- .normalize(tibble::tibble(id = "a1", ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = "D1",
                                 Parte = "PROV", importe = 50, fecha_abono = as.Date("2026-10-30"), notas = "",
                                 created_by = "t", created_at = Sys.time(), status = "active"), .schema_abonos)
sembrar <- function() {
  s3_memoria_reset(); limpiar()
  assign(MK, movs0, envir = .mem); assign(CK, conf0, envir = .mem); assign(AK, ab0, envir = .mem)
  MOVS(movs0); CONF(conf0); ABON(ab0)
}
MOVS <- reactiveVal(movs0); CONF <- reactiveVal(conf0); ABON <- reactiveVal(ab0)
shared <- list(
  effective_client_id = function() CID, home_client_id = function() CID, current_user = function() "luis",
  usuario = reactive("luis"), company_map = reactiveVal(c(NTS = "Networks Trucking")),
  visible_initials = reactiveVal(NULL), bancos_movimientos = MOVS, bancos_confirmados = CONF,
  conciliacion_rv = reactiveVal(tibble::tibble()), ctas_cuentas = reactiveVal(tibble::tibble()),
  papelera_rv = reactiveVal(NULL), abonos_db = ABON, manual_inv = reactiveVal(NULL),
  pasivos_provisions_db = reactiveVal(NULL), sap_data = reactiveVal(list()), empresas_db = reactiveVal(NULL),
  suppress_ledger_prov_refresh = reactiveVal(FALSE))

testServer(bancosServer, args = list(shared = shared), {
  session$flushReact()
  borrar_conf <- function() {
    session$userData[[paste0(session$ns("pending_delete_conf_id"))]] <- "k1"
    session$setInputs(do_delete_conf_confirm = runif(1))
  }

  # 1. Deshacer abono, S3 refuses the save
  sembrar(); .falla_escritura <<- AK
  .bnc_deshacer_abono("a1")
  ok("deshacer abono, failed save: no success notice", !exito())
  ok("deshacer abono, failed save: told once", sum(grepl("^error: ", AVISOS)) == 1L && !any(grepl("^warning: ", AVISOS)))
  ok("deshacer abono, failed save: no success log, deshacer_abono_fallido logged",
     !"deshacer_abono" %in% LOG && "deshacer_abono_fallido" %in% LOG)
  ok("deshacer abono, failed save: the session's abonos untouched", identical(ABON()$status, "active"))
  ok("deshacer abono, failed save: S3 untouched", identical(get(AK, envir = .mem)$status, "active"))
  .falla_escritura <<- character(0); limpiar()
  .bnc_deshacer_abono("a1")
  ok("deshacer abono, S3 back: voided in S3 and in the session, success told and logged",
     identical(get(AK, envir = .mem)$status, "voided") && identical(ABON()$status, "voided") &&
       exito() && "deshacer_abono" %in% LOG)

  # 2. Eliminar confirmación, S3 refuses the linked movement's save
  sembrar(); .falla_escritura <<- MK
  borrar_conf()
  ok("delete conf, movement save fails: no success notice, told once",
     !exito() && sum(grepl("^error: ", AVISOS)) == 1L && !any(grepl("^warning: ", AVISOS)))
  ok("delete conf, movement save fails: no success log", !"eliminar_confirmacion" %in% LOG)
  ok("delete conf, movement save fails: confirmation and movement untouched (session and S3)",
     !CONF()$eliminado && !MOVS()$eliminado && !get(CK, envir = .mem)$eliminado && !get(MK, envir = .mem)$eliminado)

  # 3. Eliminar confirmación, S3 refuses the confirmation's save
  sembrar(); .falla_escritura <<- CK
  borrar_conf()
  ok("delete conf, confirmation save fails: no success notice, told once",
     !exito() && sum(grepl("^error: ", AVISOS)) == 1L && !any(grepl("^warning: ", AVISOS)))
  ok("delete conf, confirmation save fails: no success log, eliminar_confirmacion_fallido logged",
     !"eliminar_confirmacion" %in% LOG && "eliminar_confirmacion_fallido" %in% LOG)
  ok("delete conf, confirmation save fails: the session's confirmation untouched", !CONF()$eliminado)

  # 4. S3 back: both saved, success told once and logged
  .falla_escritura <<- character(0); sembrar()
  borrar_conf()
  ok("delete conf, S3 back: confirmation and movement deleted in S3",
     isTRUE(get(CK, envir = .mem)$eliminado) && isTRUE(get(MK, envir = .mem)$eliminado))
  ok("delete conf, S3 back: success told and logged", exito() && "eliminar_confirmacion" %in% LOG)
})

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
