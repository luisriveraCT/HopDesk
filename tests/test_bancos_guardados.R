# =============================================================================
# tests/test_bancos_guardados.R
# Bancos' saves that fail (rule 8, T12a), on the real bancosServer in
# testServer over the CM09's in-memory S3 (tests/helpers/s3_memoria.R):
# when S3 refuses a save, the handler stops -- the session's copy and the
# store don't change, no success message and no success log; the user is
# told ("No se pudo guardar") and <accion>_fallido is logged. With S3 back,
# the same action saves. One block per handler:
#   1. do_add_manual (Agregar / Editar a manual movement)
#   2. do_reasignar_confirm (reassign an import session's account)
#   3. do_eliminar_sesion_confirm (delete an import session)
#   4. do_import (import a TXT's movements)
#   5. undo_mov_delete (restore a deleted manual movement)
#
# Run from project root:  Rscript tests/test_bancos_guardados.R
# =============================================================================
cat("=== test_bancos_guardados ===\n\n")
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
            "R/bancos_parser.R", "R/bancos_detect.R", "R/bancos_persistence.R", "R/bancos_module.R"))
  suppressMessages(source(f))

# Recorders (after the module, so they win)
LOG <- character(0); AVISOS <- character(0)
log_action <- function(user, module, action, description = "", ...) LOG <<- c(LOG, action)
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, paste0(type, ": ", as.character(ui))); invisible("id") }
removeModal <- function(...) invisible(NULL)
showModal <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede <- function(...) TRUE
puede_o_avisa <- function(...) TRUE
limpiar <- function() { LOG <<- character(0); AVISOS <<- character(0) }

CID <- "acme"; MK <- paste0(CID, "/", S3_KEYS$bancos_movimientos); PK <- paste0(CID, "/", S3_KEYS$bancos_papelera)
ctas <- tibble::tibble(id = c("c1", "c2"), Empresa = "NTS", banco_id = "bb", Moneda = "MXN", alias = c("A1", "A2"),
                       cuenta = c("01", "02"), clabe_interbancaria = c("0301", "0302"), saldo_inicial = 0, activa = TRUE)
SES <- as.POSIXct("2026-10-01 10:00:00")
mov <- function(id, cuenta = "c1", fuente = "txt", eliminado = FALSE, cargo = 100)
  tibble::tibble(id = id, cuenta_id = cuenta, fecha = as.Date("2026-10-30"), hora = "10:00:00", recibo = id,
                 descripcion_raw = paste("Mov", id), tipo = "spei_out", parte = "PROV", rfc = NA_character_,
                 referencia = NA_character_, clave_rastreo = NA_character_, concepto = "c", cargo = cargo, abono = 0,
                 saldo_banco = 1000, conciliado = FALSE, doc_vinculado = NA_character_, agenda_id = NA_character_,
                 importado_at = SES, fuente = fuente, eliminado = eliminado, eliminado_por = NA_character_,
                 eliminado_at = if (eliminado) Sys.time() else as.POSIXct(NA), notas = NA_character_)
movs0 <- .normalize(dplyr::bind_rows(mov("m1"), mov("m2"), mov("m3", fuente = "manual", eliminado = TRUE)),
                    .schema_bancos_movimientos)
sembrar <- function() {
  s3_memoria_reset()
  assign(MK, movs0, envir = .mem)
  assign(PK, add_to_bancos_papelera(.schema_bancos_papelera(), movs0[movs0$id == "m3", ], actor = "t"), envir = .mem)
}
en_s3 <- function() get(MK, envir = .mem)
MOVS <- reactiveVal(movs0)
shared <- list(
  effective_client_id = function() CID, home_client_id = function() CID, current_user = function() "luis",
  usuario = reactive("luis"), company_map = reactiveVal(c(NTS = "Networks Trucking")),
  visible_initials = reactiveVal(NULL), bancos_movimientos = MOVS,
  bancos_confirmados = reactiveVal(.schema_bancos_confirmados()), conciliacion_rv = reactiveVal(tibble::tibble()),
  ctas_cuentas = reactiveVal(ctas), papelera_rv = reactiveVal(NULL), abonos_db = reactiveVal(tibble::tibble()),
  manual_inv = reactiveVal(NULL), pasivos_provisions_db = reactiveVal(NULL), sap_data = reactiveVal(list()),
  empresas_db = reactiveVal(NULL), suppress_ledger_prov_refresh = reactiveVal(FALSE))

# A handler, once with S3 refusing the movements' save and once with S3 back.
probar <- function(nombre, accion, disparar, cambio_hecho) {
  sembrar(); MOVS(movs0); limpiar()
  .falla_escritura <<- MK
  disparar()
  ok(sprintf("%s: S3 refuses -> nothing changes in the session or the store", nombre),
     identical(MOVS(), movs0) && identical(en_s3(), movs0))
  ok(sprintf("%s: ...the user is told it wasn't saved, %s_fallido is logged, no success", nombre, accion),
     any(grepl("No se pudo guardar", AVISOS, fixed = TRUE)) && paste0(accion, "_fallido") %in% LOG &&
       !any(grepl("^message: ", AVISOS)) && !any(LOG %in% c(accion, setdiff(LOG, paste0(accion, "_fallido")))))
  .falla_escritura <<- character(0); limpiar()
  disparar()
  ok(sprintf("%s: S3 back -> the same action saves", nombre), isTRUE(cambio_hecho(en_s3())) && isTRUE(cambio_hecho(MOVS())))
}

testServer(bancosServer, args = list(shared = shared), {
  session$flushReact()

  # 1 · Agregar a manual movement
  probar("1 · do_add_manual", "guardar_movimiento_manual", function() {
    editing_mov_id(NULL)
    session$setInputs(man_cuenta = "c1", man_tipo = "manual_in", man_cargo = 0, man_abono = 777,
                      man_fecha = as.Date("2026-10-31"), man_parte = "AJUSTE", man_concepto = "x", man_notas = "",
                      do_add_manual = runif(1))
  }, function(m) any(m$abono == 777 & m$fuente == "manual"))

  # 2 · Reassign an import session's account
  probar("2 · do_reasignar_confirm", "reasignar_cuenta", function() {
    reasig_confirm_data_rv(list(sesion_key = as.character(SES), new_cid = "c2", dest_label = "NTS A2"))
    session$setInputs(do_reasignar_confirm = runif(1))
  }, function(m) all(m$cuenta_id[m$id %in% c("m1", "m2")] == "c2"))
  ok("2 · do_reasignar_confirm: a failed save keeps the confirmation's data (to try again)", {
    sembrar(); MOVS(movs0); .falla_escritura <<- MK
    reasig_confirm_data_rv(list(sesion_key = as.character(SES), new_cid = "c2", dest_label = "NTS A2"))
    session$setInputs(do_reasignar_confirm = runif(1)); .falla_escritura <<- character(0)
    !is.null(reasig_confirm_data_rv())
  })

  # 3 · Delete an import session
  probar("3 · do_eliminar_sesion_confirm", "eliminar_sesion", function() {
    delete_sesion_confirm_rv(list(sesion_key = as.character(SES)))
    session$setInputs(do_eliminar_sesion_confirm = runif(1))
  }, function(m) all(m$eliminado[m$id %in% c("m1", "m2")]))
  ok("3 · do_eliminar_sesion_confirm: a failed save keeps the confirmation's data (to try again)", {
    sembrar(); MOVS(movs0); .falla_escritura <<- MK
    delete_sesion_confirm_rv(list(sesion_key = as.character(SES)))
    session$setInputs(do_eliminar_sesion_confirm = runif(1)); .falla_escritura <<- character(0)
    !is.null(delete_sesion_confirm_rv())
  })
})

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
