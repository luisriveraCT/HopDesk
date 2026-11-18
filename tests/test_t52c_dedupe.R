# =============================================================================
# tests/test_t52c_dedupe.R
# T52c: dedupe_invoices() (R/data_pipeline.R) kept one row per (Empresa,
# Moneda, Documento, due date). Two different invoices with the same number
# and due date -- a DocNum reused in another year, or another numbering
# series for another partner -- became one, and one balance was lost. The
# invoice is now keyed with its posting date and Parte too. The same invoice
# twice (a page SAP sends again) is still one; when its copies carry other
# balances, a [DEDUPE] line says how many (counts only).
#
# Run from project root:  Rscript --vanilla tests/test_t52c_dedupe.R
# =============================================================================
cat("=== test_t52c_dedupe ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
}
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
source("R/monedas.R"); source("R/empresas_identidad.R")
source(Sys.getenv("T52C_DATA_PIPELINE_R", "R/data_pipeline.R"))   # contraprueba: 71972d5's file
source("R/policy_engine.R")
cargar("R/persistence.R", c("active_abonos_summary", ".normalize", ".schema_policy_moves"))

fila <- function(doc, contab, venc, parte, saldo, Empresa = "Networks Realtors", Moneda = "MXN")
  tibble::tibble(Empresa = Empresa, Moneda = Moneda, Documento = doc, Parte = parte, Tipo = "AR",
                 `Fecha de contabilización` = as.Date(contab), FechaVenc_Original = as.Date(venc),
                 `Saldo vencido` = saldo)
silencio <- function(expr) { m <- character(); withCallingHandlers(expr, message = function(x) {
  m <<- c(m, conditionMessage(x)); invokeRestart("muffleMessage") }); m }

cat("-- two invoices stay two --\n")
dos <- dplyr::bind_rows(fila("500", "2025-12-15", "2026-01-30", "Cliente A", 1000),
                        fila("500", "2026-01-02", "2026-01-30", "Cliente B", 2500))
ok("same number and due date, other posting date: two rows", nrow(dedupe_invoices(dos)) == 2L)
ok("…and no balance is lost (3,500)", sum(dedupe_invoices(dos)$`Saldo vencido`) == 3500)
serie <- dplyr::bind_rows(fila("700", "2026-01-02", "2026-02-01", "Cliente A", 100),
                          fila("700", "2026-01-02", "2026-02-01", "Cliente B", 200))
ok("same number and dates, another partner (another series): two rows", nrow(dedupe_invoices(serie)) == 2L)

cat("\n-- what stays as it was --\n")
repetida <- dplyr::bind_rows(fila("900", "2026-01-05", "2026-02-04", "Cliente C", 50),
                             fila("900", "2026-01-05", "2026-02-04", "Cliente C", 50))
ok("the same invoice twice is one, and says nothing (same balance)",
   { m <- silencio(r <- dedupe_invoices(repetida)); nrow(r) == 1L && !any(grepl("[DEDUPE]", m, fixed = TRUE)) })
cuotas <- dplyr::bind_rows(fila("800", "2026-01-05", "2026-02-04", "Cliente C", 50),
                           fila("800", "2026-01-05", "2026-03-04", "Cliente C", 50))
ok("same document, two due dates (installments): two rows", nrow(dedupe_invoices(cuotas)) == 2L)
sin_doc <- fila(NA_character_, "2026-01-05", "2026-02-04", "X", 1)[c(1, 1), ]
ok("rows without a Documento are all kept", nrow(dedupe_invoices(sin_doc)) == 2L)
viejo <- dplyr::bind_rows(fila("600", NA, "2026-02-04", "Cliente D", 10), fila("600", NA, "2026-02-04", "Cliente D", 10))
ok("a snapshot without posting dates is keyed as before (one)", nrow(dedupe_invoices(viejo)) == 1L)
ok("empty in, empty out", nrow(dedupe_invoices(dos[0, ])) == 0L)

cat("\n-- the first row stays, but not in silence --\n")
pagada <- dplyr::bind_rows(fila("910", "2026-01-05", "2026-02-04", "Cliente C", 50),
                           fila("910", "2026-01-05", "2026-02-04", "Cliente C", 20),
                           fila("911", "2026-01-05", "2026-02-04", "Cliente E", 70),
                           fila("911", "2026-01-05", "2026-02-04", "Cliente E", 70))
m <- silencio(r <- dedupe_invoices(pagada))
ok("the same invoice with two balances: one row, the first (50)",
   nrow(r[r$Documento == "910", ]) == 1L && r$`Saldo vencido`[r$Documento == "910"] == 50)
ok("…and one [DEDUPE] line with the count (1), the ledger and no client data",
   length(grep("[DEDUPE]", m, fixed = TRUE)) == 1L && grepl("AR: 1 invoice(s)", m[grepl("[DEDUPE]", m, fixed = TRUE)], fixed = TRUE) &&
     !any(grepl("Cliente|910|Networks", m)))

cat("\n-- through build_ledger_df(): the ledger keeps both balances --\n")
raw <- tibble::tibble(
  Documento = c("1001", "1001"), Empresa = "NCS", Parte = c("Dell", "HP"), Moneda = "MXN",
  `Saldo vencido` = c(100, 200),
  `Fecha de contabilización` = as.Date(c("2025-03-01", "2026-03-01")),
  `Fecha de vencimiento`     = as.Date(c("2026-04-01", "2026-04-01")), check.names = FALSE)
df <- build_ledger_df(raw, "AP", empresa = NULL, moves_df = NULL, policy_moves_df = NULL)
ok("two AP invoices 1001 with the same due date: two rows, 300 in total",
   sum(df$Documento == "1001") == 2L && sum(df$`Saldo vencido`[df$Documento == "1001"]) == 300)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
