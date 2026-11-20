# =============================================================================
# tests/test_t52g_ref_factura.R
# T52g (2026-11-18). The Agenda's Referencia -- the PPL's concepto, the text
# the supplier sees in its bank -- came from a lookup by Documento alone
# (.ph_ref_lookup()): with a reused number it could be ANOTHER invoice's
# Factura (another year, another currency, or a manual entry with the same
# number, which overrode SAP). Now each Agenda row takes its own invoice's:
#   - SAP: Moneda + Documento + "Fecha de contabilización" = the row's
#     fecha_factura; a row without fecha_factura only while the document is
#     one invoice (.politica_doc_unica(), the T52 rule);
#   - a manual row: its manual_inv row by id (stage_manual_row_to_agenda()
#     copies it); an old row with no id match, Moneda + Documento while that
#     has one Factura;
#   - source NA (old Agenda rows): SAP by invoice first, then the manual by
#     id; never a manual by number over a SAP invoice that matches;
#   - a manual no longer overrides a SAP row (integrator, 2026-11-18: the ✎
#     corrects a SAP invoice's Referencia).
# Over the real pagarHoyServer: the individual PPL, the ZIP's _001 and its
# _002 (included without an account), both previews; the table's call is
# checked in the source. With no reused number, the same concepto as before.
# PH_MODULE=<file> runs it on another pagar_hoy_module.R (the contraprueba).
#
# Run from project root:  Rscript tests/test_t52g_ref_factura.R
# =============================================================================
cat("=== test_t52g_ref_factura ===\n\n")
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
for (e in parse("R/data_pipeline.R", keep.source = FALSE, encoding = "UTF-8"))   # the T52 rule
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".politica_doc_unica"))
    eval(e, globalenv())
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/monedas.R",
            "R/zona.R", "R/ppl_generator.R"))
  source(f)
for (e in parse("R/search_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".datos_sap_filas", ".datos_sap_columnas", "datos_sap_tabla", "datos_sap_visor"))
    eval(e, globalenv())
MODULO <- Sys.getenv("PH_MODULE", "R/pagar_hoy_module.R")
source("R/agenda_filas.R")   # staged() reads agenda_pendientes() (T60)
source(MODULO)

# Stubs (after the module). Every payment file's rows are recorded.
PPL <- list(); ultimo_modal <- NULL
.log_pago_generado <- function(shared, emp, moneda, cuenta_origen, ppl_rows, ..., source_type = NA_character_) {
  PPL[[length(PPL) + 1L]] <<- list(tipo = source_type, moneda = moneda, concepto = ppl_rows$concepto)
  invisible(NULL)
}
log_action <- function(...) invisible(NULL)
avisos_actividad <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
puede_o_avisa <- function(...) TRUE
puede <- function(...) TRUE
showModal <- function(ui, ...) ultimo_modal <<- as.character(ui)
removeModal <- function(...) invisible(NULL)
get_cuentas_ppl_data <- function(emp, cur, shared)
  data.frame(cuenta = "012180001111111111", banco = "012", alias = "Operativa", is_ppl_default = TRUE, stringsAsFactors = FALSE)
get_cuenta_ppl <- function(emp, cur, shared) "012180001111111111"
Sys.time <- function() as.POSIXct("2026-11-18 12:00:00", tz = "America/Mexico_City")

EMP <- "Networks"
catalogo <- tibble::tibble(nombre = "ACME SA", alias = "ACME", clabe = "012345678901234567",
                           medio_pago = "SPEI", banco = "012", Empresa = NA_character_)
# An Agenda row. fecha: its invoice's posting date (agenda_filas_factura()).
fila <- function(id, doc, fecha = NA, moneda = "MXN", source = SAP_B1_ERP_TYPE, parte = "ACME SA", imp = 100)
  tibble::tibble(id = id, ledger = "AP", Empresa = EMP, Moneda = moneda, Documento = doc, Parte = parte,
                 Codigo = "", Importe = imp, FechaVenc = as.Date("2026-11-20"), status = "pending",
                 tipo_item = NA_character_, source = source, provision_id = NA_character_,
                 fecha_factura = as.Date(fecha))
sap <- function(doc, factura, fecha, moneda = "MXN")
  data.frame(Empresa = "NTS", Documento = doc, Factura = factura, Parte = "ACME SA", Moneda = moneda,
             `Fecha de contabilización` = as.Date(fecha), check.names = FALSE)
manual <- function(id, doc, factura, moneda = "MXN")
  tibble::tibble(id = id, ledger = "AP", Empresa = "NTS", Moneda = moneda, Documento = doc, Factura = factura)

# Runs every path on one Agenda; `incluir`: ids to send in the ZIP's _002.
correr <- function(agenda, sap_ap, man = NULL, incluir = character()) {
  PPL <<- list(); res <- list()
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP)), proveedores_db = reactiveVal(catalogo),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda),
    sap_data = reactiveVal(list(AP = sap_ap)), sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(man),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list(can_register_payments = TRUE))),
    home_client_id = function() "dev")
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    ultimo_modal <<- NULL
    session$setInputs(dl_preview_Networks = 1)
    res$vista_ind <<- ultimo_modal %||% ""
    invisible(output$dl_ppl_Networks)
    session$setInputs(preview_all = 1)
    v <- .descargar_vista()
    res$vista_zip <<- stats::setNames(as.character(v$Referencia), sub("#.*$", "", v$id))
    if (length(incluir)) {
      ids <- paste0(incluir, "#1")
      session$setInputs(`descargar_t-opciones` = list(ids = ids, valores = rep("incluir", length(ids))))
    }
    dir <- tempfile(); dir.create(dir)
    utils::unzip(output$download_all, exdir = dir)
  })))
  res$ppl <- PPL
  res
}
conceptos <- function(r, tipo, moneda = "MXN")
  unlist(lapply(Filter(function(p) identical(p$tipo, tipo) && identical(p$moneda, moneda), r$ppl), `[[`, "concepto"))

# ── 1. Reused numbers ─────────────────────────────────────────────────────────
SAP1 <- rbind(
  sap("D1", "F-VIEJA",  "2025-02-01"), sap("D1", "F-NUEVA", "2026-03-10"),   # D1 reused (2025 paid, 2026 open)
  sap("D1", "F-USD",    "2026-03-10", moneda = "USD"),                       # same number and date, in USD
  sap("D2", "F2-A",     "2025-05-05"), sap("D2", "F2-B",    "2026-05-05"),   # D2 reused
  sap("D3", "F-D3",     "2026-01-15"),                                       # D3 one invoice
  sap("D5", "F-SAP-D5", "2026-02-02"),                                       # D5 also a manual's number
  sap("D9", "F-SAP-D9", "2026-06-06"))
MAN1 <- dplyr::bind_rows(
  manual("man-1", "D4", "F-MAN-D4"),
  manual("man-x", "D5", "F-MAN-D5"),
  manual("m-7",   "D7", "F-D7"),                                             # old row: one Factura for D7
  manual("m-8a",  "D8", "F-D8-A"), manual("m-8b", "D8", "F-D8-B"),           # old row: two for D8
  manual("man-9", "D9", "F-MAN-D9"))
AG1 <- dplyr::bind_rows(
  fila("a", "D1", "2026-03-10"),
  fila("b", "D2"),                                   # no fecha_factura, reused
  fila("c", "D3"),                                   # no fecha_factura, one invoice
  fila("man-1", "D4", source = "manual"),
  fila("s5", "D5", "2026-02-02"),                    # a SAP invoice whose number a manual also has
  fila("o7", "D7", source = "manual"), fila("o8", "D8", source = "manual"),
  fila("r9", "D9", "2026-06-06", source = NA_character_),       # old row, source NA: SAP matches
  fila("u", "D1", "2026-03-10", moneda = "USD"),
  fila("n6", "D6", "2026-04-01", parte = "SIN CUENTA SA"))
SAP1 <- rbind(SAP1, sap("D6", "F6-VIEJA", "2024-04-01"), sap("D6", "F6-NUEVA", "2026-04-01"))
r <- tryCatch(correr(AG1, SAP1, MAN1, incluir = "n6"), error = function(e) { message("  ERROR: ", conditionMessage(e)); list() })
ind <- conceptos(r, "individual"); z1 <- conceptos(r, "zip_bulk"); z2 <- conceptos(r, "zip_bulk_sin_cuenta")
ESPERADO <- c(a = "F-NUEVA", b = "D2", c = "F-D3", "man-1" = "F-MAN-D4", s5 = "F-SAP-D5", o7 = "F-D7", o8 = "D8", r9 = "F-SAP-D9")
# The PPL's rows go by FechaVenc (all equal here), in Agenda order.
ok("individual PPL (MXN): each row's concepto is ITS invoice's Factura",
   identical(sort(unname(ind)), sort(unname(ESPERADO))))
ok("ZIP _001 MXN: the same", identical(sort(unname(z1)), sort(unname(ESPERADO))))
ok("ZIP _001 USD: D1 in USD takes the USD invoice (F-USD), not the MXN one of the same date",
   identical(unname(conceptos(r, "zip_bulk", "USD")), "F-USD"))
ok("ZIP _002 (included without an account): D6 reused takes its own invoice, F6-NUEVA",
   identical(unname(z2), "F6-NUEVA"))
ok("the reused D1 never says F-VIEJA, the reused D2 never picks one (it stays D2)",
   !any(c("F-VIEJA", "F2-A", "F2-B", "F6-VIEJA") %in% c(ind, z1, z2)))
ok("a manual no longer overrides a SAP invoice with its number (s5: F-SAP-D5; r9, source NA: F-SAP-D9)",
   !any(c("F-MAN-D5", "F-MAN-D9") %in% c(ind, z1)))
ok("the individual preview shows F-NUEVA and not F-VIEJA",
   grepl("F-NUEVA", r$vista_ind, fixed = TRUE) && !grepl("F-VIEJA", r$vista_ind, fixed = TRUE))
ok("the ZIP preview's Referencia, row by row",
   identical(unname(r$vista_zip[c(names(ESPERADO), "u", "n6")]), c(unname(ESPERADO), "F-USD", "F6-NUEVA")))

# ── 2. No reused number: the same concepto as before T52g ────────────────────
SAP2 <- rbind(sap("D3", "F-D3", "2026-01-15"), sap("E1", "F-E1", "2026-02-01"), sap("E2", "", "2026-02-02"))
MAN2 <- manual("man-1", "D4", "F-MAN-D4")
AG2  <- dplyr::bind_rows(fila("c", "D3"), fila("e1", "E1", "2026-02-01"), fila("e2", "E2", "2026-02-02"),
              fila("e3", "E3", "2026-02-03"), fila("man-1", "D4", source = "manual"))
r2 <- tryCatch(correr(AG2, SAP2, MAN2), error = function(e) { message("  ERROR: ", conditionMessage(e)); list() })
ANTES <- c("F-D3", "F-E1", "E2", "E3", "F-MAN-D4")   # what master writes (blank Factura -> Documento)
ok("no reuse: individual PPL, the same conceptos as before", identical(sort(unname(conceptos(r2, "individual"))), sort(ANTES)))
ok("no reuse: ZIP _001, the same", identical(sort(unname(conceptos(r2, "zip_bulk"))), sort(ANTES)))
# SAP without Moneda / posting date (the old fixtures): nothing filters on them.
r3 <- tryCatch(correr(fila("c", "D3"), data.frame(Empresa = "NTS", Documento = "D3", Factura = "F-D3")),
               error = function(e) { message("  ERROR: ", conditionMessage(e)); list() })
ok("SAP without Moneda and posting date: still found by Documento (F-D3)",
   identical(unname(conceptos(r3, "individual")), "F-D3"))

# ── 3. The source: no lookup by Documento left ───────────────────────────────
src <- paste(readLines(MODULO, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("pagar_hoy_module.R: .ph_ref_lookup() is gone (not left next to the new one)",
   !grepl(".ph_ref_lookup", src, fixed = TRUE))
ok("the 6 sites take .ph_referencias() (table with overrides = TRUE, 2 previews, 3 PPL)",
   lengths(regmatches(src, gregexpr(".ph_referencias(", src, fixed = TRUE))) == 6L &&
     grepl("\\.ph_referencias\\([^)]*overrides = TRUE", src))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
