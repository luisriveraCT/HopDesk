# =============================================================================
# tests/test_t52d_tags.R
# T52d: the urgent / important tags (invoice_tags.rds) were kept and joined
# by document (ledger, Empresa, Moneda, Documento), without the invoice's
# posting date. The ERP reuses a DocNum in another year: a tag of a paid
# invoice reached the new one with its number, and tagging one of two open
# invoices with the same number rewrote the other's tags. Now a tag carries
# fecha_factura (its invoice). Closed rule for rows written before
# (fecha_factura NA; integrator and Coordinador, 2026-11-18):
#   1. a document that is one invoice: it reaches it, as in bb800d5, unless
#      the invoice is later than the tag (by day, in the client's zone);
#   2. a document with several invoices: it reaches none;
#   3. without tagged_at: rule 1 without the exception, rule 2 the same.
#
# Run from project root:  Rscript --vanilla tests/test_t52d_tags.R
# =============================================================================
cat("=== test_t52d_tags ===\n\n")
source("tests/helpers/s3_memoria.R")
.app_data_cache <- list()
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".cache_get", ".cache_set", ".cache_scoped_key"))
    eval(e, globalenv())
SAP_B1_ERP_TYPE <- "sap_b1_service_layer"
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("R/zona.R")
source("R/monedas.R"); source("R/empresas_identidad.R")
source(Sys.getenv("T52D_DATA_PIPELINE_R", "R/data_pipeline.R"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(expr)
FC <- "Fecha de contabilización"
etq <- function(doc, tag, fecha = NA, at = "2026-01-10 18:00:00", Empresa = "NCS")
  .normalize(tibble::tibble(ledger = "AR", Empresa = Empresa, Moneda = "MXN", Documento = doc, tag = tag,
                            tagged_by = "a", tagged_at = as.POSIXct(at, tz = "UTC"),
                            fecha_factura = as.Date(fecha)), .schema_tags)
fact <- function(doc, fc, Empresa = "NCS")
  tibble::tibble(Ledger = "AR", Tipo = "AR", Empresa = Empresa, Moneda = "MXN", Documento = doc,
                 `Fecha de contabilización` = as.Date(fc), check.names = FALSE)
dos  <- dplyr::bind_rows(fact("500", "2025-03-01"), fact("500", "2026-03-01"))   # a reused number, both open
una  <- fact("500", "2026-03-01")

cat("-- writing: set_invoice_tags() --\n")
viejas <- dplyr::bind_rows(etq("500", "urgent"), etq("500", "important", fecha = "2025-03-01"), etq("777", "urgent"))
r <- set_invoice_tags(viejas, "AR", "NCS", "MXN", "500", "urgent", tagged_by = "yo",
                      fecha_factura = "2026-03-01", facturas = dos)
ok("a reused number: tagging the 2026 invoice keeps the undated row (it could be the 2025 one's)",
   sum(is.na(r$fecha_factura) & r$Documento == "500") == 1L)
ok("…keeps the 2025 invoice's own tag, and adds the 2026 one with its date",
   any(r$fecha_factura %in% as.Date("2025-03-01") & r$tag == "important") &&
     any(r$fecha_factura %in% as.Date("2026-03-01") & r$tag == "urgent" & r$tagged_by == "yo"))
ok("…and never touches another document (777)", sum(r$Documento == "777") == 1L)
r1 <- set_invoice_tags(viejas[viejas$Documento != "500" | is.na(viejas$fecha_factura), ], "AR", "NCS", "MXN", "500",
                       "important", fecha_factura = "2026-03-01", facturas = una)
ok("one invoice: its undated row is replaced (one row per invoice, not the old one beside it)",
   !any(is.na(r1$fecha_factura) & r1$Documento == "500") &&
     identical(r1$tag[r1$Documento == "500"], "important"))
r2 <- set_invoice_tags(r, "AR", "NCS", "MXN", "500", character(0), fecha_factura = "2026-03-01", facturas = dos)
ok("clearing the 2026 invoice's tags leaves the 2025 one and the undated row",
   !any(r2$fecha_factura %in% as.Date("2026-03-01")) && nrow(r2) == nrow(r) - 1L)
r3 <- set_invoice_tags(viejas, "AR", "NCS", "MXN", "500", "urgent")
ok("without fecha_factura, as before: every row of the document is replaced",
   sum(r3$Documento == "500") == 1L && is.na(r3$fecha_factura[r3$Documento == "500"]))

cat("\n-- writing: two sessions, one number --\n")
TK <- "acme/invoice_tags.rds"
s3_memoria_reset(); assign(TK, .schema_tags(), envir = .mem)
base <- .schema_tags()
a <- set_invoice_tags(base, "AR", "NCS", "MXN", "500", "urgent", tagged_by = "A", fecha_factura = "2025-03-01", facturas = dos)
b <- set_invoice_tags(base, "AR", "NCS", "MXN", "500", "important", tagged_by = "B", fecha_factura = "2026-03-01", facturas = dos)
q(save_tags(a, client_id = "acme", base = base))
q(save_tags(b, client_id = "acme", base = base))
s3 <- get(TK, envir = .mem)
ok("session A tags the 2025 invoice, session B the 2026 one (same number): both stay",
   nrow(s3) == 2L && setequal(paste(s3$tagged_by, format(s3$fecha_factura)), c("A 2025-03-01", "B 2026-03-01")))
s3_memoria_reset(); assign(TK, dplyr::bind_rows(etq("500", "urgent", fecha = "2025-03-01"), etq("777", "urgent")), envir = .mem)
base <- get(TK, envir = .mem)
q(save_tags(set_invoice_tags(base, "AR", "NCS", "MXN", "500", character(0), fecha_factura = "2026-03-01", facturas = dos),
            client_id = "acme", base = base))
ok("clearing the 2026 invoice's (none) loses no row of S3", nrow(get(TK, envir = .mem)) == 2L)

cat("\n-- the two callers pass the invoice's date --\n")
llamadas <- list()
mirar <- function(x, f) {
  if (is.call(x)) {
    if (identical(x[[1]], as.name("set_invoice_tags"))) llamadas[[length(llamadas) + 1L]] <<- list(f = f, args = names(as.list(x)))
    for (a in as.list(x)[-1]) if (!missing(a)) mirar(a, f)
  }
}
for (f in c("app.R", list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE)))
  for (e in parse(f, keep.source = FALSE)) mirar(e, f)
ok("set_invoice_tags() is called from ledger_module.R and search_module.R",
   setequal(basename(vapply(llamadas, `[[`, "", "f")), c("ledger_module.R", "search_module.R")))
ok("…and every call passes fecha_factura and facturas",
   all(vapply(llamadas, function(l) all(c("fecha_factura", "facturas") %in% l$args), logical(1))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
