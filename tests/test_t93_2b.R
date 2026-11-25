# =============================================================================
# tests/test_t93_2b.R
# T93 (2b): each payment of a generated PPL file remembers its Agenda row.
#   1  btr_beneficiarios(): an entry written BEFORE 2b (5 fields) reads the
#      same 5 columns as parse_pago_beneficiarios(), and NA in the 3 new ones;
#      one written after carries agenda_item_id, documento, fecha_factura
#   2  the real .log_pago_generado() writes those 3 fields in the JSON (and
#      the 5 old ones exactly as before)
#   3  the real pagarHoyServer: the one-company file and the ZIP log each
#      payment with ITS Agenda row (id, Documento), and the ZIP's _002 too
#   4  .ph_confirmar_ap() exists at the file's top level (the Agenda's
#      Confirmar, moved untouched -- its behaviour is pinned by
#      test_confirmar_cierra, test_t72f_confirmar, test_t85_quita and
#      _run_confirmed_logic, run against master)
#
# Run from project root:  Rscript tests/test_t93_2b.R
# =============================================================================
cat("=== test_t93_2b ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
fmt_money <- function(x) paste0("$ ", formatC(x, format = "f", digits = 2, big.mark = ","))

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
for (e in parse("R/pagos_generados_persistence.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "parse_pago_beneficiarios"))
    eval(e, globalenv())
source("R/agenda_filas.R")
source("R/pagar_hoy_module.R")
source("R/bancos_respuesta.R")

# ── 1 · Old and new JSON ────────────────────────────────────────────────────
viejo <- as.character(jsonlite::toJSON(list(
  list(alias = "ACME", clabe = "012345678901234567", banco = "012", importe = 1000.5, concepto = "FAC-1"),
  list(alias = "BETA", clabe = "098765432109876543", banco = "072", importe = 2500, concepto = "FAC-2")),
  auto_unbox = TRUE))
b_viejo <- btr_beneficiarios(viejo)
ok("1 · JSON de antes: las 5 columnas idénticas a parse_pago_beneficiarios()",
   identical(b_viejo[, 1:5], parse_pago_beneficiarios(viejo)))
ok("1 · JSON de antes: las 3 nuevas en NA (nunca adivinadas)",
   all(is.na(unlist(b_viejo[, c("agenda_item_id", "documento", "fecha_factura")]))))
ok("1 · parse_pago_beneficiarios() sigue con sus 5 columnas",
   identical(names(parse_pago_beneficiarios(viejo)), c("alias", "clabe", "banco", "importe", "concepto")))
ok("1 · vacío y JSON roto: 0 renglones, las 8 columnas", nrow(btr_beneficiarios(NA_character_)) == 0L &&
     ncol(btr_beneficiarios("no{json")) == 8L)

# ── 2 · The real .log_pago_generado() ───────────────────────────────────────
REG <- NULL
add_pago_generado <- function(record, client_id = NULL, ...) { REG <<- record; invisible(list(ok = TRUE)) }
log_action <- function(...) invisible(NULL)
sh <- list(effective_client_id = function() "dev", home_client_id = function() "dev", current_user = function() "luis")
filas <- data.frame(id = c("A1", "A2"), Documento = c("D1", "D2"), fecha_factura = as.Date(c("2026-11-01", "2026-11-02")),
                    stringsAsFactors = FALSE)
pr <- data.frame(alias = c("ACME", "BETA"), clabe_dest = c("012345678901234567", "098765432109876543"),
                 banco_dest = c("012", "072"), medio_pago = "SPI", importe = c(1000.5, 2500), concepto = c("FAC-1", "FAC-2"),
                 stringsAsFactors = FALSE)
.log_pago_generado(sh, "Networks", "MXN", "073543010201", .ph_ppl_con_agenda(pr, filas), fname = "x.txt", source_type = "individual")
nuevo <- btr_beneficiarios(REG$beneficiarios)
ok("2 · el JSON lleva agenda_item_id, documento y fecha_factura de cada pago",
   identical(nuevo$agenda_item_id, c("A1", "A2")) && identical(nuevo$documento, c("D1", "D2")) &&
     identical(nuevo$fecha_factura, c("2026-11-01", "2026-11-02")))
ok("2 · las 5 columnas de siempre, iguales a las de un JSON de antes",
   identical(parse_pago_beneficiarios(REG$beneficiarios), parse_pago_beneficiarios(viejo)))
.log_pago_generado(sh, "Networks", "MXN", "073543010201", pr, fname = "x.txt", source_type = "individual")
ok("2 · un llamado sin las columnas nuevas (como antes) sigue registrando, con NA",
   nrow(btr_beneficiarios(REG$beneficiarios)) == 2L && all(is.na(btr_beneficiarios(REG$beneficiarios)$agenda_item_id)))

# ── 3 · The real server ─────────────────────────────────────────────────────
PPL <- list(); ultimo_modal <- NULL
.log_pago_generado <- function(shared, emp, moneda, cuenta_origen, ppl_rows, ..., source_type = NA_character_) {
  PPL[[length(PPL) + 1L]] <<- list(tipo = source_type, ids = ppl_rows$agenda_item_id, docs = ppl_rows$documento,
                                   ff = ppl_rows$fecha_factura)
  invisible(NULL)
}
avisos_actividad <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
hd_numero_update <- function(...) invisible(NULL)
puede_o_avisa <- function(...) TRUE
puede <- function(...) TRUE
showModal <- function(ui, ...) ultimo_modal <<- as.character(ui)
removeModal <- function(...) invisible(NULL)
showNotification <- function(...) invisible("id")
Sys.time <- function() as.POSIXct("2026-11-24 12:00:00", tz = "America/Mexico_City")
get_cuenta_ppl <- function(emp, cur, shared) "073543010201"
get_cuentas_ppl_data <- function(emp, cur, shared)
  data.frame(cuenta = "073543010201", banco = "030", alias = "Operativa", is_ppl_default = TRUE, stringsAsFactors = FALSE)
EMP <- "Networks"
catalogo <- tibble::tibble(nombre = c("ACME SA", "SIN CLABE SA"), alias = c("ACME", "SINCLABE"),
                           clabe = c("012345678901234567", NA), medio_pago = "SPEI", banco = c("012", NA), Empresa = NA_character_)
fila <- function(id, doc, parte = "ACME SA", imp = 100, ff = "2026-11-01")
  tibble::tibble(id = id, ledger = "AP", Empresa = EMP, Moneda = "MXN", Documento = doc, Parte = parte,
                 Codigo = "", Importe = imp, FechaVenc = as.Date("2026-11-25"), status = "pending",
                 tipo_item = NA_character_, source = SAP_B1_ERP_TYPE, provision_id = NA_character_,
                 fecha_factura = as.Date(ff))
AGENDA <- dplyr::bind_rows(fila("A1", "D1", imp = 100, ff = "2026-11-01"), fila("A2", "D2", imp = 200, ff = "2026-11-02"),
                           fila("A3", "D3", parte = "SIN CLABE SA", imp = 30, ff = "2026-11-03"))
SAP <- data.frame(Empresa = "NTS", Documento = c("D1", "D2", "D3"), Factura = c("F-1", "F-2", "F-3"),
                  Parte = c("ACME SA", "ACME SA", "SIN CLABE SA"), Moneda = "MXN",
                  `Fecha de contabilización` = as.Date("2026-11-01"), check.names = FALSE)
correr <- function(zip, incluir = character()) {
  PPL <<- list()
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP)), proveedores_db = reactiveVal(catalogo),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(AGENDA),
    sap_data = reactiveVal(list(AP = SAP)), sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list(can_register_payments = TRUE))),
    home_client_id = function() "dev")
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    if (!zip) {
      session$setInputs(dl_preview_Networks = 1)
      readLines(output$dl_ppl_Networks, warn = FALSE)
    } else {
      session$setInputs(preview_all = 1)
      if (length(incluir)) {
        ids <- paste0(incluir, "#1")
        session$setInputs(`descargar_t-opciones` = list(ids = ids, valores = rep("incluir", length(ids))))
      }
      dir <- tempfile(); dir.create(dir); utils::unzip(output$download_all, exdir = dir)
    }
  })))
  PPL
}
r <- correr(FALSE)
ok("3 · una empresa: cada pago registrado con su fila de la Agenda, su documento y su fecha de factura",
   length(r) == 1L && identical(sort(r[[1]]$ids), c("A1", "A2")) &&
     identical(r[[1]]$docs[order(r[[1]]$ids)], c("D1", "D2")) && identical(r[[1]]$ff[order(r[[1]]$ids)], c("2026-11-01", "2026-11-02")))
r <- correr(TRUE, incluir = "A3")
tipos <- vapply(r, `[[`, "", "tipo")
ok("3 · ZIP: el _001 con A1 y A2", identical(sort(r[[which(tipos == "zip_bulk")]]$ids), c("A1", "A2")))
ok("3 · ZIP: el _002 (incluido sin cuenta) con A3 y su documento",
   identical(r[[which(tipos == "zip_bulk_sin_cuenta")]]$ids, "A3") && identical(r[[which(tipos == "zip_bulk_sin_cuenta")]]$docs, "D3"))

# ── 4 · .ph_confirmar_ap() at top level ─────────────────────────────────────
ok("4 · .ph_confirmar_ap() es una función del archivo, con la firma que usa el servidor",
   is.function(.ph_confirmar_ap) &&
     identical(names(formals(.ph_confirmar_ap)), c("shared", "emp", "cur", "rows", "cuenta_sel", "fecha_pago",
                                                   ".parte_ap", ".ph_fallido", ".link_counter", "extra")))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
