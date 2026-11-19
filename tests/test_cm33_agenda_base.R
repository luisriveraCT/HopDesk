# =============================================================================
# tests/test_cm33_agenda_base.R
# CM33. Three Agenda writes in R/pagar_hoy_module.R still save the WHOLE
# table with no `base` (regla 15; test_s3_red_guardian's ratchet 3):
#   1  Confirmar pagos (do_confirm_ap_<emp>) and Confirmar cobros
#      (do_confirm_ar_<emp>): they read the Agenda, drop the confirmed ids and
#      write that copy back. Whatever another session staged or removed
#      between that read and the write is undone: its new row is lost, the
#      row it removed comes back. If the read fails they fall back to this
#      session's copy and write THAT over S3.
#   2  Vaciar agenda (do_clear_all): same whole write, and a failed read falls
#      back to the session's copy (regla 16).
# Each case runs the real pagarHoyServer (testServer) on the real
# persistence (save_pagar_hoy, modificar_bancos_confirmados) over the
# in-memory S3; "the other session" is S3 changing right after this
# session's read. Controls: confirming still closes the invoice
# (bancos_confirmados gets it), and a failed save never says it worked.
#
# Run from project root:  Rscript tests/test_cm33_agenda_base.R
# =============================================================================
cat("=== test_cm33_agenda_base ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
source("tests/helpers/s3_memoria.R")          # R/persistence.R + S3_KEYS
source("R/bancos_persistence.R")              # modificar_bancos_confirmados
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

for (e in parse("R/erp_connector_registry.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "SAP_B1_ERP_TYPE"))
    eval(e, globalenv())
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "is_erp_sourced"))
    eval(e, globalenv())
for (e in parse("R/data_pipeline.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".politica_doc_unica"))
    eval(e, globalenv())
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/monedas.R",
            "R/zona.R", "R/ppl_generator.R"))
  source(f)
for (e in parse("R/search_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".datos_sap_filas", ".datos_sap_columnas", "datos_sap_tabla", "datos_sap_visor"))
    eval(e, globalenv())
source("R/pagar_hoy_module.R")

# Stubs (after the module): no UI, no audit, no sync bus.
LOGS <- list(); AVISOS <- character()
log_action <- function(user, module, action, ...) { LOGS[[length(LOGS) + 1L]] <<- action; invisible(NULL) }
avisos_actividad <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede_o_avisa <- function(...) TRUE
puede <- function(...) TRUE
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
.log_pago_generado <- function(...) invisible(NULL)

CID <- "dev"; EMP <- "Networks"
KEY <- paste0(CID, "/", S3_KEYS$pagar_hoy_sync)
fila <- function(id, doc, ledger = "AP", emp = EMP, moneda = "MXN")
  tibble::tibble(id = id, ledger = ledger, Empresa = emp, Moneda = moneda, Documento = doc,
                 Parte = "ACME SA", Codigo = "", Importe = 100, FechaVenc = as.Date("2026-11-20"),
                 status = "pending", tipo_item = NA_character_, source = SAP_B1_ERP_TYPE,
                 provision_id = NA_character_, fecha_factura = as.Date("2026-11-01"))
agenda_s3 <- function() .normalize(get(KEY, envir = .mem), .schema_pagar_hoy)
ids_s3    <- function() sort(agenda_s3()$id)

# The Agenda in S3 (shared, sync on) and in this session: X1 (to confirm)
# and Y1, a row of another company this action does not touch.
sembrar <- function(ledger = "AP") {
  s3_memoria_reset()
  assign(paste0(CID, "/", S3_KEYS$agenda_sync_config),
         .normalize(tibble::tibble(is_enabled = TRUE), .schema_agenda_sync_config), envir = .mem)
  ag <- dplyr::bind_rows(fila("X1", "D1", ledger), fila("Y1", "D9", ledger, emp = "Otra"))
  assign(KEY, .normalize(ag, .schema_pagar_hoy), envir = .mem)
  ag
}

# The other session, right after this one's read of the Agenda.
OTRA <- NULL
safe_load_real <- safe_load_pagar_hoy
safe_load_pagar_hoy <- function(...) {
  x <- safe_load_real(...)
  if (is.function(OTRA)) { f <- OTRA; OTRA <<- NULL; f() }
  x
}
otra_agrega <- function(ledger = "AP") function()
  assign(KEY, .normalize(dplyr::bind_rows(agenda_s3(), fila("B9", "D7", ledger, emp = "Otra")), .schema_pagar_hoy), envir = .mem)
otra_quita <- function() function()
  assign(KEY, .normalize(dplyr::filter(agenda_s3(), id != "Y1"), .schema_pagar_hoy), envir = .mem)

correr <- function(accion, agenda, ledger = "AP") {
  LOGS <<- list(); AVISOS <<- character()
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP, OTR = "Otra")), proveedores_db = reactiveVal(NULL),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda),
    sap_data = reactiveVal(list(AP = NULL, AR = NULL)), sap_ov_db = reactiveVal(NULL),
    manual_inv = reactiveVal(NULL), bancos_confirmados = reactiveVal(tibble::tibble()),
    abonos_db = reactiveVal(NULL),
    effective_client_id = function() CID, current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list())),
    home_client_id = function() CID)
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    if (accion == "vaciar") {
      session$setInputs(do_clear_all = 1)
    } else {
      l <- tolower(ledger)
      do.call(session$setInputs, setNames(list("MXN", "2026-11-19"),
                                          c(paste0("bal_cur_", EMP), paste0("conf_fecha_", l, "_", EMP))))
      do.call(session$setInputs, setNames(list(list(nonce = 1)), paste0("do_confirm_", l, "_", EMP)))
    }
  })))
}
confirmadas <- function() {
  k <- paste0(CID, "/", S3_KEYS$bancos_confirmados)
  if (!exists(k, envir = .mem)) return(character())
  get(k, envir = .mem)$agenda_item_id
}
exito_confirmar <- function() any(LOGS %in% c("confirmar_pago", "confirmar_cobro"))

for (ledger in c("AP", "AR")) {
  # ── Control: alone, the confirm closes X1 and leaves Y1 ──────────────────
  ag <- sembrar(ledger)
  correr("confirmar", ag, ledger)
  ok(sprintf("%s control: confirming alone takes X1 out and closes it (bancos_confirmados), Y1 stays", ledger),
     identical(ids_s3(), "Y1") && "X1" %in% confirmadas())

  # ── 1. Another session stages B9 between the read and the write ─────────
  ag <- sembrar(ledger); OTRA <- otra_agrega(ledger)
  correr("confirmar", ag, ledger)
  ok(sprintf("%s 1 two sessions: the row another session staged meanwhile (B9) survives the confirm", ledger),
     identical(ids_s3(), c("B9", "Y1")))
  ok(sprintf("%s 1 …and X1 is still confirmed and out of the Agenda", ledger),
     !"X1" %in% ids_s3() && "X1" %in% confirmadas())

  # ── 2. Another session removes Y1 between the read and the write ────────
  ag <- sembrar(ledger); OTRA <- otra_quita()
  correr("confirmar", ag, ledger)
  ok(sprintf("%s 2 two sessions: the row another session removed meanwhile (Y1) does not come back", ledger),
     identical(ids_s3(), character()))

  # ── 3. The read fails: never this session's copy written over S3 ────────
  ag <- sembrar(ledger)
  assign(KEY, .normalize(dplyr::bind_rows(agenda_s3(), fila("B9", "D7", ledger, emp = "Otra")), .schema_pagar_hoy), envir = .mem)
  OTRA <- NULL
  safe_load_pagar_hoy <- function(...) stop("simulated read failure")
  n <- .mem_escrituras
  correr("confirmar", ag, ledger)
  safe_load_pagar_hoy <- function(...) {
    x <- safe_load_real(...)
    if (is.function(OTRA)) { f <- OTRA; OTRA <<- NULL; f() }
    x
  }
  ok(sprintf("%s 3 the Agenda read fails: B9 (in S3, not in this session's copy) is not lost", ledger),
     "B9" %in% ids_s3())
  ok(sprintf("%s 3 …and nothing is written anywhere: X1 stays in the Agenda and is NOT closed in bancos_confirmados, no success logged", ledger),
     identical(.mem_escrituras, n) && identical(ids_s3(), c("B9", "X1", "Y1")) &&
       !"X1" %in% confirmadas() && !exito_confirmar() &&
       any(grepl(tr("comun.reintentar"), AVISOS, fixed = TRUE)))

  # ── 4. The Agenda write fails: no success is announced ──────────────────
  ag <- sembrar(ledger)
  .mem_falla_escritura <- KEY
  correr("confirmar", ag, ledger)
  .mem_falla_escritura <- character()
  ok(sprintf("%s 4 control: the Agenda write fails -> the error notice, no success logged, X1 still in S3", ledger),
     !exito_confirmar() && "X1" %in% ids_s3() &&
       any(grepl(substr(tr("tablas.agenda.aviso.confirmada_agenda_no", n = "3"), 1, 20), AVISOS, fixed = TRUE)))
}

# ── 5. Vaciar agenda ──────────────────────────────────────────────────────
ag <- sembrar("AP")
correr("vaciar", ag)
ok("5 control: Vaciar alone empties the pending rows", identical(ids_s3(), character()))

ag <- sembrar("AP"); OTRA <- otra_agrega("AP")
correr("vaciar", ag)
ok("5 two sessions: a row another session staged after this read (B9, never seen here) survives Vaciar",
   identical(ids_s3(), "B9"))

ag <- sembrar("AP")
safe_load_pagar_hoy <- function(...) stop("simulated read failure")
n <- .mem_escrituras
correr("vaciar", ag)
ok("5 the Agenda read fails: Vaciar writes nothing and says it could not",
   identical(.mem_escrituras, n) && identical(ids_s3(), c("X1", "Y1")) &&
     !any(LOGS %in% "vaciar_agenda") &&
     !any(grepl(tr("tablas.agenda.aviso.vaciada"), AVISOS, fixed = TRUE)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
