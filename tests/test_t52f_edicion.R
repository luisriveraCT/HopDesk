# =============================================================================
# tests/test_t52f_edicion.R
# T52f: the ✎ saves only what the user changed, and the Agenda follows one
# invoice, not its number.
#
# 1. Before, the ✎ stored Parte, Código and Factura whenever they were not
#    empty -- prefilled with what was on screen, so moving only the date
#    froze SAP's supplier and Código. And Moneda / Importe were "changed"
#    when they differed from the row ON SCREEN, which already carried the
#    override: re-editing an invoice (only its date) silently dropped its
#    Importe_override / Moneda_override. Now each field keeps what the
#    invoice had unless the user changed it in this edit; emptying a field
#    goes back to SAP's value (NA).
# 2. .sync_staged() (ledger_module.R) updated the pending Agenda rows by
#    ledger + Empresa + Moneda + Documento: with two invoices of the same
#    number in the Agenda, the ✎ of one copied its Código (the bank account's
#    key) and Importe to the other, and moving one moved both. Now a key with
#    fecha_factura reaches its own invoice's rows; an Agenda row without the
#    date only while the document is one invoice (the T52 rule); a key
#    without the date (the manual entry in app.R) as before.
#
# Run from project root:  Rscript tests/test_t52f_edicion.R
# =============================================================================
cat("=== test_t52f_edicion ===\n\n")
source("tests/helpers/s3_memoria.R")          # R/persistence.R over an in-memory S3
.app_data_cache <- list()
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".cache_get", ".cache_set", ".cache_scoped_key", "is_erp_sourced"))
    eval(e, globalenv())
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("R/zona.R"); source("R/monedas.R"); source("R/empresas_identidad.R")
source("R/data_pipeline.R"); source("R/policy_engine.R")
cargar <- function(archivo, nombres)
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
cargar("R/ledger_module.R", c(".sap_ov_campos", ".sync_staged"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(suppressWarnings(expr))
s <- function(expr) tryCatch(q(expr), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })

# ── 1 · What the ✎ stores: .sap_ov_campos(prefill, entrada, existente) ───────
cat("-- 1. the ✎ stores only what changed --\n")
# What the modal showed (the override when there is one, else SAP's).
pre <- list(Parte = "HP", Codigo = "P-HP", Factura = "F-2026", Notas = "", Moneda = "MXN", Importe = 250)
igual <- pre
existente <- tibble::tibble(Parte_override = NA_character_, Codigo_override = NA_character_,
                            Factura_override = NA_character_, Notas_override = NA_character_,
                            Moneda_override = "MXN", Importe_override = 250)
r <- s(.sap_ov_campos(pre, igual, existente))
ok("c1: re-editing only the date keeps the invoice's Importe_override (the bug: it was dropped)",
   !is.null(r) && identical(r$campos$Importe_override, 250))
ok("c2: …and its Moneda_override", !is.null(r) && identical(r$campos$Moneda_override, "MXN"))
ok("c3: …and nothing counts as changed", !is.null(r) && !length(r$cambiados))
r <- s(.sap_ov_campos(pre, igual, NULL))
ok("c4: moving only the date on an invoice without override stores no Parte / Código / Factura",
   !is.null(r) && !length(r$cambiados) &&
     all(is.na(unlist(r$campos[c("Parte_override", "Codigo_override", "Factura_override")]))))
cambia <- modifyList(pre, list(Codigo = "P-HP-NUEVO"))
r <- s(.sap_ov_campos(pre, cambia, NULL))
ok("c5: changing only the Código stores only Codigo_override",
   !is.null(r) && identical(r$cambiados, "Codigo") && identical(r$campos$Codigo_override, "P-HP-NUEVO") &&
     is.na(r$campos$Parte_override) && is.na(r$campos$Factura_override) && is.na(r$campos$Importe_override))
ex2 <- modifyList(existente, list(Parte_override = "HP SA"))
r <- s(.sap_ov_campos(modifyList(pre, list(Parte = "HP SA")), modifyList(pre, list(Parte = "")), ex2))
ok("c6: emptying a field goes back to SAP's value (NA), the rest of the override stays",
   !is.null(r) && identical(r$cambiados, "Parte") && is.na(r$campos$Parte_override) &&
     identical(r$campos$Importe_override, 250))
r <- s(.sap_ov_campos(pre, modifyList(pre, list(Importe = 300)), existente))
ok("c7: a new amount is stored", !is.null(r) && identical(r$cambiados, "Importe") && identical(r$campos$Importe_override, 300))
r <- s(.sap_ov_campos(pre, modifyList(pre, list(Importe = NA_real_)), existente))
ok("c8: an emptied amount goes back to SAP's (NA)", !is.null(r) && is.na(r$campos$Importe_override))
r <- s(.sap_ov_campos(pre, modifyList(pre, list(Parte = "  HP  ")), NULL))
ok("c9: spaces around a value are not a change", !is.null(r) && !length(r$cambiados))

# ── 2 · .sync_staged(): the Agenda by invoice ─────────────────────────────────
cat("-- 2. the Agenda follows one invoice --\n")
CID <- "t52f"; USR <- "yo"
fila_ph <- function(id, fecha, codigo, importe, venc = "2026-11-10")
  tibble::tibble(id = id, ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "1001", Parte = "X",
                 Codigo = codigo, Importe = importe, FechaVenc = as.Date(venc), staged_by = "a",
                 staged_at = as.POSIXct("2026-11-01", tz = "UTC"), status = "pending",
                 fecha_factura = as.Date(fecha))
agenda0 <- q(.normalize(bind_rows(fila_ph("PH-2025", "2025-03-01", "P-DELL", 100),
                                  fila_ph("PH-2026", "2026-10-01", "P-HP", 200)), .schema_pagar_hoy))
dos <- tibble::tibble(ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "1001",
                      fecha_factura = as.Date(c("2025-03-01", "2026-10-01")))
una <- dos[2, ]
rv <- function(v) function(x) if (missing(x)) v else v <<- x
arranca <- function(ph) { rm(list = ls(.mem, all.names = TRUE), envir = .mem)
  q(save_pagar_hoy(ph, USR, client_id = CID)); rv(ph) }
lee <- function() q(safe_load_pagar_hoy(USR, client_id = CID))
llave <- function(fecha = NA) data.frame(Empresa = "NCS", Moneda = "MXN", Documento = "1001",
                                         fecha_factura = as.Date(fecha), stringsAsFactors = FALSE)

ph_rv <- arranca(agenda0)
s(.sync_staged(llave("2026-10-01"), "AP", ph_rv, new_date = as.Date("2026-12-01"), new_codigo = "P-HP-NUEVO",
               new_importe = 222, username = USR, client_id = CID, facturas = dos))
a <- lee()
ok("s1: the ✎ of the 2026 invoice changes its own Agenda row (Código, amount, date)",
   identical(a$Codigo[a$id == "PH-2026"], "P-HP-NUEVO") && identical(a$Importe[a$id == "PH-2026"], 222) &&
     identical(a$FechaVenc[a$id == "PH-2026"], as.Date("2026-12-01")))
ok("s2: …and never the 2025 one with the same number (its Código -- its bank account -- amount and date stay)",
   identical(a$Codigo[a$id == "PH-2025"], "P-DELL") && identical(a$Importe[a$id == "PH-2025"], 100) &&
     identical(a$FechaVenc[a$id == "PH-2025"], as.Date("2026-11-10")))

ph_rv <- arranca(agenda0)
s(.sync_staged(llave("2026-10-01"), "AP", ph_rv, new_date = as.Date("2026-12-05"), username = USR, client_id = CID,
               facturas = dos))
a <- lee()
ok("s3: moving the 2026 invoice moves only its Agenda row",
   identical(a$FechaVenc[a$id == "PH-2026"], as.Date("2026-12-05")) && identical(a$FechaVenc[a$id == "PH-2025"], as.Date("2026-11-10")))

vieja <- q(.normalize(fila_ph("PH-VIEJA", NA, "P-HP", 200), .schema_pagar_hoy))
ph_rv <- arranca(vieja)
s(.sync_staged(llave("2026-10-01"), "AP", ph_rv, new_date = as.Date("2026-12-07"), username = USR, client_id = CID, facturas = una))
ok("s4: an Agenda row without the date (from before) follows a one-invoice document",
   identical(lee()$FechaVenc, as.Date("2026-12-07")))
ph_rv <- arranca(vieja)
s(.sync_staged(llave("2026-10-01"), "AP", ph_rv, new_date = as.Date("2026-12-07"), new_codigo = "OTRO",
               username = USR, client_id = CID, facturas = dos))
ok("s5: …but not a reused number: nobody can tell whose it is, so it is left as it was",
   identical(lee()$FechaVenc, as.Date("2026-11-10")) && identical(lee()$Codigo, "P-HP"))

# A key without fecha_factura (the manual entry, app.R): every pending row of the number, as before.
ph_rv <- arranca(agenda0)
s(.sync_staged(llave(), "AP", ph_rv, new_date = as.Date("2026-12-09"), username = USR, client_id = CID))
ok("s6: a key without the date (manual entry) behaves as before: every pending row of the number",
   all(lee()$FechaVenc == as.Date("2026-12-09")))
ph_rv <- arranca(agenda0)
s(.sync_staged(data.frame(Empresa = "NCS", Moneda = "MXN", Documento = "1001", stringsAsFactors = FALSE), "AP", ph_rv,
               new_date = as.Date("2026-12-09"), username = USR, client_id = CID))
ok("s7: …and so does a key with no fecha_factura column at all", all(lee()$FechaVenc == as.Date("2026-12-09")))

detalle <- data.frame(Empresa = "NCS", Moneda = "MXN", Documento = "1001",
                      `Fecha de contabilización` = as.Date(c("2025-03-01", "2026-10-01")),
                      FechaVenc_Original = as.Date(c("2025-04-01", "2026-11-01")), check.names = FALSE)
ph_rv <- arranca(agenda0)
s(.sync_staged(llave("2026-10-01"), "AP", ph_rv, detail = detalle, username = USR, client_id = CID, facturas = dos))
a <- lee()
ok("s8: restoring the 2026 invoice takes ITS original date (not the 2025 one's), on its row only",
   identical(a$FechaVenc[a$id == "PH-2026"], as.Date("2026-11-01")) && identical(a$FechaVenc[a$id == "PH-2025"], as.Date("2026-11-10")))

# ── 3 · The ✎ handler's code ──────────────────────────────────────────────────
cat("-- 3. the ✎ handler --\n")
lm  <- readLines("R/ledger_module.R", warn = FALSE, encoding = "UTF-8")
lmc <- lm[!grepl("^\\s*#", lm)]
h0  <- grep("observeEvent\\(input\\$sap_edit_save", lmc)
h1  <- h0 + grep("^    \\}, ignoreInit|^    \\})", lmc[(h0 + 1):length(lmc)])[1]
hand <- if (length(h0) == 1L && !is.na(h1)) lmc[h0:h1] else character()
ok("h1: the handler decides what to store with .sap_ov_campos() (prefill kept when the modal opened)",
   any(grepl("\\.sap_ov_campos\\(", hand)) && any(grepl("pending_sap_edit_prefill", lmc)))
ok("h2: no more \"changed\" against the row on screen (orig_moneda / orig_importe)",
   !any(grepl("orig_importe|orig_moneda", hand)))
ok("h3: the handler's Agenda sync carries the invoice's date and the open invoices",
   any(grepl("fecha_factura", hand[grep("\\.sync_staged\\(", hand)[1] + 0:8])) &&
     any(grepl("facturas *= *facturas_mov", hand)))
ok("h4: the modal says, in one line, that an emptied field goes back to SAP's value",
   any(grepl("tablas\\.dia\\.sap_edit\\.vacio_es_sap", lm)) &&
     all(vapply(c("www/i18n/es.json", "www/i18n/en.json"), function(f)
       any(grepl("\"tablas.dia.sap_edit.vacio_es_sap\"", readLines(f, warn = FALSE, encoding = "UTF-8"), fixed = TRUE)), logical(1))))
ok("h5: move and restore pass the open invoices to the Agenda sync too",
   sum(grepl("\\.sync_staged\\(keys, ledger, shared\\$pagar_hoy_db", lmc)) == 2L &&
     all(grepl("facturas *=", lmc[grep("\\.sync_staged\\(keys, ledger, shared\\$pagar_hoy_db", lmc) + 1L]) |
         grepl("facturas *=", lmc[grep("\\.sync_staged\\(keys, ledger, shared\\$pagar_hoy_db", lmc)])))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
