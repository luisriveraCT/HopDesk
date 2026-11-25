# =============================================================================
# tests/test_t72f_confirmar.R
# T72-F. Confirmar pago (do_confirm_ap_<emp>) and Confirmar cobro
# (do_confirm_ar_<emp>) of R/pagar_hoy_module.R pass
# evento_parte_de("confirmar_pago" | "confirmar_cobro") to each of their
# writes: a write that worked leaves a staff-only "<accion>/parte" entry.
# The rule (Coordinador, 2026-11-23): nothing changes in WHAT is written nor
# in which ORDER, only in how it is logged. So, on the real pagarHoyServer
# (testServer) over the in-memory S3, with an Agenda that touches every
# write (an ERP invoice, a plain manual entry, a provision's entry, an
# abono):
#   1 the S3 write sequence (key, order, object) is identical with the
#     events and with evento_parte_de() answering NULL (= the wrappers' calls
#     of before, word for word); the client's log too;
#   2 each write that worked leaves one /parte, with its table;
#   3 save_pagar_hoy_with_retry: an attempt that fails leaves no /parte,
#     only the one that wrote;
#   4 (a) the abono's save fails: the summary no longer says "aplicado", the
#     user sees it, and the log says why (abono_error);
#   5 (b) the archive link or save_manual fails: <accion>_fallido in the log
#     and a notice, instead of a console message (the link) or a notice alone.
# uuid and Sys.time are fixed, so two runs write the same objects.
#
# Run from project root:  Rscript tests/test_t72f_confirmar.R
# =============================================================================
cat("=== test_t72f_confirmar ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
source("tests/helpers/s3_memoria.R")          # R/persistence.R + S3_KEYS
source("R/bancos_persistence.R")              # modificar_bancos_confirmados, enlazar_archivo_confirmaciones
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
            "R/zona.R", "R/ppl_generator.R", "R/pagos_layouts.R"))
  source(f)
for (e in parse("R/search_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".datos_sap_filas", ".datos_sap_columnas", "datos_sap_tabla", "datos_sap_visor"))
    eval(e, globalenv())
for (e in parse("R/ui_components.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c("HD_NUMERO_FORMATOS", ".hd_numero_decimales", "hd_numero_texto", "hd_numero_deps",
                                  "hd_numero_input", "hd_numero_update", "hd_numero_revisar"))
    eval(e, globalenv())
source("R/agenda_filas.R")
source("R/pagar_hoy_module.R")
source("tests/helpers/cuenta_origen_t104.R")   # T104: a confirmation carries its origin account
CTAS_NTS <- transform(CTAS_T104[1, ], id = "CT1", Empresa = "NTS", alias = "NTSBB")
CUENTA_AP <- "CT1"   # what the modal sends (tests/test_t104_confirmar.R changes it)

# ── Stubs (after the module) ──────────────────────────────────────────────
LOGS <- list(); AVISOS <- character()
log_action <- function(user, module, action, description = "", target_id = NULL, metadata = NULL, ...)
  { LOGS[[length(LOGS) + 1L]] <<- list(action = action, description = description, target_id = target_id,
                                       metadata = metadata); invisible(NULL) }
avisos_actividad <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede_o_avisa <- function(...) TRUE
puede <- function(...) TRUE
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
.log_pago_generado <- function(...) invisible(NULL)
load_ctas_bancos <- function(...) BANCOS_T104   # T104: the account's bank, for its name

# Fixed time and ids: two runs write the same objects.
Sys.time <- function() as.POSIXct("2026-11-23 10:00:00", tz = "UTC")
.uuid_n <- 0L
.uuid_orig <- uuid::UUIDgenerate
utils::assignInNamespace("UUIDgenerate", function(...) { .uuid_n <<- .uuid_n + 1L; sprintf("u-%04d", .uuid_n) }, "uuid")

# Every write that reached the store, in order: key and object. FALLA: a
# key's n-th writes fail (list(key = , n = ), n a vector: .s3_write's
# put_object fallback is the next one); n = 0: all of them.
SEQ <- list(); FALLA <- NULL; .n_llave <- list()
.mem_escribir <- function(object, valor) {
  .n_llave[[object]] <<- (.n_llave[[object]] %||% 0L) + 1L
  if (!is.null(FALLA) && identical(object, FALLA$key) && (identical(FALLA$n, 0L) || .n_llave[[object]] %in% FALLA$n))
    stop("simulated S3 write failure")
  .mem_escrituras <<- .mem_escrituras + 1L; assign(object, valor, envir = .mem)
  SEQ[[length(SEQ) + 1L]] <<- list(key = object, obj = valor)
  invisible(TRUE)
}

CID <- "dev"; EMP <- "Networks"
k <- function(s) paste0(CID, "/", S3_KEYS[[s]])
fila <- function(id, doc, ledger, source = SAP_B1_ERP_TYPE, tipo = NA_character_, prov = NA_character_)
  tibble::tibble(id = id, ledger = ledger, Empresa = EMP, Moneda = "MXN", Documento = doc,
                 Parte = "ACME SA", Codigo = "", Importe = 100, FechaVenc = as.Date("2026-11-20"),
                 status = "pending", tipo_item = tipo, source = source,
                 provision_id = prov, fecha_factura = as.Date("2026-11-01"))
manual <- function(id, doc, ledger, prov = NA_character_)
  tibble::tibble(id = id, ledger = ledger, Empresa = EMP, Moneda = "MXN", Documento = doc, Factura = doc,
                 Parte = "ACME SA", Codigo = "", Importe = 100, `Abono futuro` = 0,
                 `Fecha de vencimiento` = as.Date("2026-11-20"), Notas = "", created_by = "luis",
                 created_at = Sys.time(), updated_at = Sys.time(), provision_id = prov,
                 liability_id = NA_character_, referencia = NA_character_)

# The Agenda: an ERP invoice (X1), a plain manual entry (M1), a provision's
# entry (P1, its manual row MP1) and an abono (A1): every write of Confirmar.
# Z1, a manual row of another company, stays: without it the provision's
# save_manual would empty the table and [S3_RED] refuses it (found here,
# reported in T72_F_LISTO; not this task's).
sembrar <- function(ledger) {
  s3_memoria_reset()
  SEQ <<- list(); .n_llave <<- list(); .uuid_n <<- 0L
  assign(k("agenda_sync_config"), .normalize(tibble::tibble(is_enabled = TRUE), .schema_agenda_sync_config), envir = .mem)
  ag <- dplyr::bind_rows(fila("X1", "D1", ledger), fila("M1", "D2", ledger, source = "manual"),
                         fila("P1", "D3", ledger, source = "manual", prov = "PV1"),
                         fila("A1", "D4", ledger, tipo = "abono"))
  assign(k("pagar_hoy_sync"), .normalize(ag, .schema_pagar_hoy), envir = .mem)
  assign(k("manual"), .normalize(dplyr::bind_rows(manual("M1", "D2", ledger), manual("MP1", "D3", ledger, "PV1"),
                                                 dplyr::mutate(manual("Z1", "D8", ledger), Empresa = "Otra")),
                                 .schema_manual), envir = .mem)
  SEQ <<- list(); .n_llave <<- list()
  .normalize(ag, .schema_pagar_hoy)
}

correr <- function(ledger) {
  agenda <- sembrar(ledger)
  LOGS <<- list(); AVISOS <<- character()
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP)), proveedores_db = reactiveVal(NULL),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda),
    sap_data = reactiveVal(list(AP = NULL, AR = NULL)), sap_ov_db = reactiveVal(NULL),
    manual_inv = reactiveVal(.normalize(get(k("manual"), envir = .mem), .schema_manual)),
    papelera_rv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() CID, current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list())),
    home_client_id = function() CID, ctas_cuentas = reactiveVal(CTAS_NTS))
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    l <- tolower(ledger)
    do.call(session$setInputs, setNames(list("MXN", "2026-11-19"),
                                        c(paste0("bal_cur_", EMP), paste0("conf_fecha_", l, "_", EMP))))
    # T104: the payment's origin account, chosen in the modal (AP only).
    if (ledger == "AP" && !is.null(CUENTA_AP))
      do.call(session$setInputs, setNames(list(CUENTA_AP), paste0("conf_cuenta_ap_", EMP)))
    do.call(session$setInputs, setNames(list(list(nonce = 1)), paste0("do_confirm_", l, "_", EMP)))
  })))
  list(seq = SEQ, logs = LOGS, avisos = AVISOS)
}
accion_de <- function(ledger) if (ledger == "AP") "confirmar_pago" else "confirmar_cobro"
partes  <- function(r, ledger) Filter(function(x) identical(x$action, paste0(accion_de(ledger), "/parte")), r$logs)
cliente <- function(r) Filter(function(x) !grepl("/parte$", x$action) && !identical(x$action, "sin_evento"), r$logs)
datos   <- function(r) Filter(function(w) !grepl("(^|/)(app_audit|sync_versions)|_backup_", w$key), r$seq)
final   <- function(r, ledger) Filter(function(x) identical(x$action, accion_de(ledger)), r$logs)

evento_parte_real <- evento_parte_de
for (ledger in c("AP", "AR")) {
  a <- accion_de(ledger)
  # ── 1 + 2: with the events, and without them ────────────────────────────
  con <- correr(ledger)
  evento_parte_de <- function(...) NULL
  sin <- correr(ledger)
  evento_parte_de <- evento_parte_real

  ok(sprintf("%s control: the confirmation ran end to end (its one client row, 9 data writes)", ledger),
     length(final(con, ledger)) == 1L && length(datos(con)) == 9L)
  ok(sprintf("%s 1 the S3 write sequence (key, order, object) is identical with and without the events", ledger),
     identical(lapply(con$seq, `[[`, "key"), lapply(sin$seq, `[[`, "key")) &&
       identical(lapply(con$seq, `[[`, "obj"), lapply(sin$seq, `[[`, "obj")))
  ok(sprintf("%s 1 …and the client's log is the same, word for word (action, description, metadata)", ledger),
     identical(cliente(con), cliente(sin)))
  ok(sprintf("%s 1 …and its notices", ledger), identical(con$avisos, sin$avisos))
  ok(sprintf("%s 2 each write that worked leaves one %s/parte, with its table, in the write's order", ledger, a),
     identical(vapply(partes(con, ledger), function(x) x$metadata$tabla, ""),
               vapply(datos(con), function(w) sub(paste0("^", CID, "/"), "", w$key), "")))
  ok(sprintf("%s 2 …the five tables of Confirmar are there", ledger),
     all(unlist(S3_KEYS[c("bancos_confirmados", "pagar_hoy_sync", "abonos", "papelera", "manual")]) %in%
           vapply(partes(con, ledger), function(x) x$metadata$tabla, "")))
  ok(sprintf("%s 2 …and none without the events (no /parte, the writes of before)", ledger),
     !length(partes(sin, ledger)))

  # ── 3: the Agenda's first attempt fails, the second writes ──────────────
  FALLA <- list(key = k("pagar_hoy_sync"), n = 1L)
  r <- correr(ledger); FALLA <- NULL
  ag_partes <- Filter(function(x) identical(x$metadata$tabla, S3_KEYS$pagar_hoy_sync), partes(r, ledger))
  ok(sprintf("%s 3 save_pagar_hoy_with_retry: the failed attempt leaves no /parte, the one that wrote leaves one", ledger),
     length(ag_partes) == 1L && length(final(r, ledger)) == 1L)

  # ── 4 (a): the abono's save fails ───────────────────────────────────────
  FALLA <- list(key = k("abonos"), n = 0L)
  r <- correr(ledger); FALLA <- NULL
  f <- final(r, ledger)
  ok(sprintf("%s 4 (a) control: the abono failed -> its error notice, as before", ledger),
     any(startsWith(r$avisos, substr(tr("tablas.agenda.aviso.error_abono", error = ""), 1, 20))))
  ok(sprintf("%s 4 (a) the log no longer says 'aplicado': it says the abono was not applied", ledger),
     length(f) == 1L && !grepl("aplicado", f[[1]]$description) &&
       grepl(tr("tablas.agenda.log.abono_no_aplicado", n = 1L), f[[1]]$description, fixed = TRUE))
  ok(sprintf("%s 4 (a) …the user sees it in the summary notice", ledger),
     any(grepl(tr("tablas.agenda.log.abono_no_aplicado", n = 1L), r$avisos, fixed = TRUE)) &&
       !any(grepl("abono(s) aplicado(s)", r$avisos, fixed = TRUE)))
  ok(sprintf("%s 4 (a) …and the log's metadata says why (abono_error); no abonos /parte", ledger),
     grepl("simulated S3 write failure", f[[1]]$metadata$abono_error %||% "", fixed = TRUE) &&
       !any(vapply(partes(r, ledger), function(x) identical(x$metadata$tabla, S3_KEYS$abonos), NA)))
  ok(sprintf("%s 4 (a) control: when it worked, no abono_error key (the success row of always)", ledger),
     is.null(final(con, ledger)[[1]]$metadata$abono_error) && grepl("abono(s) aplicado(s)", final(con, ledger)[[1]]$description, fixed = TRUE))

  # ── 5 (b): the archive link fails (bancos_confirmados' 2nd write and its fallback)
  FALLA <- list(key = k("bancos_confirmados"), n = 2:3)
  r <- correr(ledger); FALLA <- NULL
  fl <- Filter(function(x) identical(x$action, "enlazar_archivo_fallido"), r$logs)
  ok(sprintf("%s 5 (b) the archive link fails: enlazar_archivo_fallido in the log, with the action it belonged to", ledger),
     length(fl) == 1L && identical(fl[[1]]$metadata$accion, a) && identical(fl[[1]]$target_id, EMP))
  ok(sprintf("%s 5 (b) …and the user is told (comun.error_guardar), not only the console", ledger),
     tr("comun.error_guardar") %in% r$avisos)
  ok(sprintf("%s 5 (b) …the confirmation itself still logs once", ledger), length(final(r, ledger)) == 1L)

  # ── 5 (b): save_manual fails ────────────────────────────────────────────
  FALLA <- list(key = k("manual"), n = 0L)
  r <- correr(ledger); FALLA <- NULL
  fl <- Filter(function(x) identical(x$action, "guardar_manual_fallido"), r$logs)
  ok(sprintf("%s 5 (b) save_manual fails: guardar_manual_fallido in the log for each of its two saves", ledger),
     length(fl) == 2L && all(vapply(fl, function(x) identical(x$metadata$accion, a), NA)))
  ok(sprintf("%s 5 (b) …and the user keeps its notice of before (error_eliminar_manual)", ledger),
     sum(startsWith(r$avisos, substr(tr("tablas.agenda.aviso.error_eliminar_manual", error = ""), 1, 20))) == 2L)
}

utils::assignInNamespace("UUIDgenerate", .uuid_orig, "uuid")
s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
