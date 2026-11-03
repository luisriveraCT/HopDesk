# =============================================================================
# tests/test_ic_mapa_llave.R
# A payment sent from the Mapa IC and confirmed in the Agenda must close the
# DEBTOR's AP invoice. It used to carry the creditor's AR DocNum and no
# Codigo, so confirming it closed nothing and the debt could be paid twice
# (T13 trace, 2026-11-03: 0 of 642 in dev). The key that crosses the two
# books is the folio: AR$Factura (creditor) == AP$Factura (debtor), same
# pair and currency (R/ic_mapa_llave.R).
#
# Run from project root:  Rscript tests/test_ic_mapa_llave.R
# =============================================================================
cat("=== test_ic_mapa_llave ===\n\n")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
}
source("R/ic_mapa_llave.R")
cargar("R/treasury_map_module.R", ".tm_parte_to_ini")
cargar("R/data_pipeline.R", "compute_confirmed_flags")
cargar("R/persistence.R", "confirmaciones_vigentes")

cmap <- list(NG = "Networks Group", NL = "Networks & Logistics", TR = "Transportes Rivera")

# The debtor's AP (as ic_invoices()$ap: .ini of the owner, Parte = supplier).
ap <- data.frame(
  .ini      = c("NL", "NL", "NL", "NL", "TR", "NL", "NL"),
  Empresa   = c("Networks & Logistics", "Networks & Logistics", "Networks & Logistics",
                "Networks & Logistics", "Transportes Rivera", "Networks & Logistics", "Networks & Logistics"),
  Parte     = c("Networks Group", "Networks Group", "Transportes Rivera", "Networks Group",
                "Networks Group", "Networks Group", "Networks Group"),
  Documento = c("9001", "9002", "9003", "9004", "9005", "9006", "9007"),
  Factura   = c("A-1578", "A-1600", "A-1578", "A-1578", "A-1578", "DUP", "dup "),
  Moneda    = c("MXN", "MXN", "MXN", "USD", "MXN", "MXN", "MXN"),
  `Código de proveedor` = "P-NG01",
  `Saldo vencido` = c(56475.60, 100, 1, 2, 3, 4, 5),
  `Fecha de vencimiento` = as.Date("2026-11-10"),
  check.names = FALSE, stringsAsFactors = FALSE)

item <- function(factura, from = "NL", to = "NG", moneda = "MXN")
  list(from_ini = from, to_ini = to, from_full = cmap[[from]], to_full = cmap[[to]],
       Documento = "1578", Factura = factura, Moneda = moneda, Importe = 56475.60)

# ── 1. The key ────────────────────────────────────────────────────────────────
r <- ic_mapa_ap_deudor(item("A-1578"), ap, cmap)
ok("finds the debtor's AP invoice by folio: its own DocNum, not the creditor's",
   identical(r$Documento, "9001"))
ok("... with the debtor's supplier code (Codigo was always empty)", identical(r$Codigo, "P-NG01"))
ok("... its amount and due date", r$Importe == 56475.60 && identical(r$FechaVenc, "2026-11-10"))
ok("same folio owed to another company is not it (pair)", identical(ic_mapa_ap_deudor(item("A-1578"), ap[3, ], cmap), NULL))
ok("same folio in another currency is not it", identical(ic_mapa_ap_deudor(item("A-1578", moneda = "USD"), ap, cmap)$Documento, "9004"))
ok("same folio in another debtor's books is not it", identical(ic_mapa_ap_deudor(item("A-1578", from = "TR"), ap, cmap)$Documento, "9005"))
ok("folio compared without case or spaces", identical(ic_mapa_ap_deudor(item(" a-1600"), ap, cmap)$Documento, "9002"))
ok("no AP with that folio (81 of 642 in dev): NULL, not sent",
   is.null(ic_mapa_ap_deudor(item("A-9999"), ap, cmap)))
ok("two AP invoices with that folio: NULL, never a guess", is.null(ic_mapa_ap_deudor(item("DUP"), ap, cmap)))
ok("an AR invoice without folio: NULL", is.null(ic_mapa_ap_deudor(item(NA_character_), ap, cmap)) &&
     is.null(ic_mapa_ap_deudor(item(""), ap, cmap)))
ok("no AP data: NULL", is.null(ic_mapa_ap_deudor(item("A-1578"), NULL, cmap)))
ok("the harmonized Codigo column wins when present",
   identical(ic_mapa_ap_deudor(item("A-1578"), transform(ap, Codigo = "PX"), cmap)$Codigo, "PX"))

# ── 2. End to end: confirming the row closes the debtor's invoice ─────────────
# The confirmation pagar_hoy_module.R writes for an AP row: empresa, documento,
# moneda, importe from the Agenda row; matched on Saldo_original.
libro <- data.frame(Empresa = "Networks & Logistics", Documento = c("9001", "9002"), Moneda = "MXN",
                    Saldo_original = c(56475.60, 100), source = "sap", stringsAsFactors = FALSE)
conf <- function(documento, importe) data.frame(
  confirmacion_id = "c1", empresa = "Networks & Logistics", documento = documento, moneda = "MXN",
  importe = importe, tipo = "pago", eliminado = FALSE, confirmado_at = Sys.time(), stringsAsFactors = FALSE)
antes <- compute_confirmed_flags(libro, "AP", conf("1578", 56475.60), NULL)
ok("before: the creditor's DocNum closes nothing", !any(antes$confirmed))
despues <- compute_confirmed_flags(libro, "AP", conf(r$Documento, r$Importe), NULL)
ok("after: confirming the Mapa row closes exactly the debtor's invoice", identical(despues$confirmed, c(TRUE, FALSE)))

# ── 3. The send uses it ───────────────────────────────────────────────────────
src <- paste(readLines("R/treasury_map_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
envio <- substr(src, regexpr("observeEvent(input$send_to_agenda", src, fixed = TRUE), nchar(src))
envio <- substr(envio, 1, regexpr("}, ignoreInit = TRUE)", envio, fixed = TRUE))
ok("the send resolves each item with ic_mapa_ap_deudor()", grepl("lapply(plan, ic_mapa_ap_deudor", envio, fixed = TRUE))
ok("the Agenda row carries the debtor's Documento and Codigo",
   grepl("Documento = a$Documento", envio, fixed = TRUE) && grepl("Codigo    = a$Codigo", envio, fixed = TRUE) &&
     !grepl("Documento = x$Documento", envio, fixed = TRUE))
ok("items without their AP stay in the plan, with a warning",
   grepl("payment_plan(plan[sin_ap])", envio, fixed = TRUE) && grepl('tr("comun.ic_sin_ap_deudor")', envio, fixed = TRUE))
ok("the plan carries the AR folio", grepl("Factura   = if (\"Factura\" %in% names(row))", src, fixed = TRUE))
ok("app.R loads the key before the Mapa",
   { a <- readLines("app.R", warn = FALSE); which(a == 'source("R/ic_mapa_llave.R")') < which(a == 'source("R/treasury_map_module.R")') })

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
