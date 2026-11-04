# =============================================================================
# tests/test_confirmar_cierra.R
# Luis (2026-11-04): confirming a payment in the Agenda ALWAYS closes its
# invoice -- paid and locked, never sendable again.
#   - ERP invoices are never deleted by the app: confirmed, they stay in every
#     view, locked.
#   - HopDesk-generated rows (manual, provisions) disappear on confirmation;
#     they are not built here.
#   - Partial abonos: out of scope for now.
# The key: every path that sends an ERP invoice builds its Agenda row with
# agenda_filas_factura() (R/persistence.R), which carries the invoice's
# identity -- Empresa, Documento, Moneda and posting date (fecha_factura).
# The confirmation copies it and compute_confirmed_flags() closes on it, even
# when the ERP later moves the balance. Already-confirmed invoices are
# refused on the server.
# PENDIENTES: paths still building their rows by hand, owned by other fronts
# until they adopt the helper. A ratchet: it only goes down.
#
# Run from project root:  Rscript tests/test_confirmar_cierra.R
# =============================================================================
cat("=== test_confirmar_cierra ===\n\n")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
suppressPackageStartupMessages(library(dplyr))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
}
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
cargar("R/global.R", "is_erp_sourced")
cargar("R/persistence.R", c(".schema_pagar_hoy", "agenda_filas_factura", "confirmaciones_vigentes", ".normalize"))
cargar("R/bancos_persistence.R", ".schema_bancos_confirmados")
cargar("R/data_pipeline.R", "compute_confirmed_flags")

FC <- "Fecha de contabilización"
# Two ERP invoices of the ledger, as build_ledger_df() leaves them.
libro <- function(saldo = c(1000, 500), fecha = as.Date(c("2026-09-01", "2026-09-15"))) {
  d <- data.frame(Empresa = "Networks & Logistics", Moneda = "MXN", Documento = c("9001", "9002"),
                  Parte = "Acme", Codigo = "P001", Importe = saldo, `Saldo vencido` = saldo,
                  Saldo_original = saldo, FechaEff = as.Date("2026-11-10"), source = SAP_B1_ERP_TYPE,
                  confirmed = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
  d[[FC]] <- fecha
  d
}
# What pagar_hoy_module.R writes when the Agenda row is confirmed (AP).
confirmar <- function(filas) .normalize(data.frame(
  confirmacion_id = paste0("c", seq_len(nrow(filas))), agenda_item_id = filas$id,
  empresa = filas$Empresa, parte = filas$Parte, documento = filas$Documento, codigo = filas$Codigo,
  importe = filas$Importe, moneda = filas$Moneda, tipo = "pago", eliminado = FALSE,
  confirmado_at = Sys.time(), fecha_factura = filas$fecha_factura, stringsAsFactors = FALSE),
  .schema_bancos_confirmados)
cerradas <- function(df, conf) compute_confirmed_flags(df, "AP", conf, NULL)$confirmed

# ── 1. The row carries the invoice's identity ─────────────────────────────────
filas <- agenda_filas_factura(libro(), "luis", ledger = "AP")
ok("one Agenda row per invoice, tipo factura, pending", nrow(filas) == 2L && all(filas$tipo_item == "factura") &&
     all(filas$status == "pending"))
ok("... with its Empresa, Documento, Moneda, Codigo and balance",
   identical(filas$Documento, c("9001", "9002")) && all(filas$Codigo == "P001") && identical(filas$Importe, c(1000, 500)))
ok("... and its posting date (fecha_factura)", identical(filas$fecha_factura, as.Date(c("2026-09-01", "2026-09-15"))))
ok("the Agenda schema keeps fecha_factura", "fecha_factura" %in% names(.normalize(filas, .schema_pagar_hoy)) &&
     identical(.normalize(filas, .schema_pagar_hoy)$fecha_factura, filas$fecha_factura))
ok("the confirmations schema keeps it too", inherits(.schema_bancos_confirmados()$fecha_factura, "Date"))

# ── 2. Confirming closes it, whatever the ERP does to the balance ─────────────
conf <- confirmar(filas[1, ])
ok("confirming the row closes exactly its invoice", identical(cerradas(libro(), conf), c(TRUE, FALSE)))
ok("the ERP lowers the balance afterwards (partial application, credit note): still closed",
   identical(cerradas(libro(saldo = c(400, 500)), conf), c(TRUE, FALSE)))
ok("the ERP raises it: still closed", identical(cerradas(libro(saldo = c(1200, 500)), conf), c(TRUE, FALSE)))
ok("a DocNum reused later (another posting date) is NOT closed by the old confirmation",
   identical(cerradas(libro(fecha = as.Date(c("2029-01-05", "2026-09-15"))), conf), c(FALSE, FALSE)))
ok("sent for a different amount (Importe_override): still closed",
   identical(cerradas(libro(), confirmar(transform(filas[1, ], Importe = 123))), c(TRUE, FALSE)))
ok("the confirmed invoice is marked a paid ghost (stays in view)",
   isTRUE(compute_confirmed_flags(libro(), "AP", conf, NULL)$is_paid_ghost[1]))
ok("an undone confirmation (eliminado) no longer closes it",
   identical(cerradas(libro(), transform(conf, eliminado = TRUE)), c(FALSE, FALSE)))

# Older confirmations, written without fecha_factura, keep the amount key.
vieja <- transform(conf, fecha_factura = as.Date(NA))
ok("old confirmation (no fecha_factura): closes on the amount, as before",
   identical(cerradas(libro(), vieja), c(TRUE, FALSE)))
ok("old confirmation: a changed balance reopens it, as before (known limit)",
   identical(cerradas(libro(saldo = c(400, 500)), vieja), c(FALSE, FALSE)))
ok("manual and provision rows are never closed by this key (they disappear on their own path)", {
  m <- libro(); m$source <- c("manual", "provision")
  !any(cerradas(m, confirmar(transform(filas, fecha_factura = m[[FC]]))))
})

# ── 3. Never sendable again: refused on the server ────────────────────────────
marcado <- libro(); marcado$confirmed <- c(TRUE, FALSE)
r <- agenda_filas_factura(marcado, "luis", ledger = "AP")
ok("an invoice the view shows as confirmed is refused", identical(r$Documento, "9002") &&
     identical(attr(r, "rechazadas"), "9001"))
r <- agenda_filas_factura(libro(), "luis", ledger = "AP", confirmaciones = conf)
ok("... and so is a forged one (view says open, the confirmations say closed)",
   identical(r$Documento, "9002") && identical(attr(r, "rechazadas"), "9001"))
r <- agenda_filas_factura(libro(saldo = c(400, 500)), "luis", ledger = "AP", confirmaciones = conf)
ok("... also after the ERP moved its balance", identical(r$Documento, "9002"))
m <- libro(); m$source <- c("manual", "provision")
ok("manual and provision rows are not built here (own paths)", nrow(agenda_filas_factura(m, "luis", ledger = "AP")) == 0L)
m <- libro(); m$Documento[1] <- ""
ok("a row without Documento cannot be keyed: not sent", identical(agenda_filas_factura(m, "luis", ledger = "AP")$Documento, "9002"))
ok("the ledger can come per row (Vencidos mixes AP and AR)",
   identical(agenda_filas_factura(transform(libro(), ledger = c("AP", "AR")), "luis")$ledger, c("AP", "AR")))
ok("nothing to send: an empty Agenda table", nrow(agenda_filas_factura(NULL, "luis", ledger = "AP")) == 0L)

# ── 4. The confirmation in the Agenda copies the identity ─────────────────────
ph <- readLines("R/pagar_hoy_module.R", warn = FALSE, encoding = "UTF-8")
ok("confirming AP and AR in the Agenda writes fecha_factura (2 sites)",
   sum(grepl("fecha_factura   = as.Date(factura_rows$fecha_factura[i] %||% NA)", ph, fixed = TRUE)) == 2L)

# ── 5. Every path that sends an ERP invoice uses the helper (ratchet) ─────────
# Not ERP invoices, each with its own close: the Agenda itself, the manual
# entry (app.R), Pasivos' provisions, the partial abono (out of scope).
EXCEPCIONES <- c("R/persistence.R", "R/pagar_hoy_module.R", "app.R", "R/pasivos_module.R",
                 "R/staging_browse_module.R")
# Owned by other fronts until they adopt it: ejecutor 2 (Vencidos, lupa, day).
# Intercompany and the Mapa IC adopted it (T13).
PENDIENTES <- c("R/search_module.R", "R/ledger_module.R")
archivos <- setdiff(c(list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), "app.R"), EXCEPCIONES)
sin_helper <- Filter(function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8"); x <- x[!grepl("^\\s*#", x)]
  any(grepl("upsert_pagar_hoy(", x, fixed = TRUE)) && !any(grepl("agenda_filas_factura(", x, fixed = TRUE))
}, archivos)
for (f in setdiff(sin_helper, PENDIENTES)) message("  sin agenda_filas_factura(): ", f)
ok("no new path sends ERP invoices to the Agenda without agenda_filas_factura()",
   length(setdiff(sin_helper, PENDIENTES)) == 0L)
ok("PENDIENTES only goes down: a file that adopted the helper must leave the list",
   length(setdiff(PENDIENTES, sin_helper)) == 0L)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
