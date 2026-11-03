# =============================================================================
# tests/test_treasury_map_add_to_plan.R
# Mapa IC: the edge's invoice table is sorted by due date, but "Agregar al
# plan" used to apply the table's selected positions to the UNSORTED list, so
# the plan received different invoices than the ones marked. The table also
# resolved companies with the global COMPANY_MAP while the button used the
# tenant's company_map(), so the two could even list different invoices.
#
# Drives the real treasuryMapServer with shiny::testServer: three invoices
# with due dates out of order; mark the one drawn first; the plan must get
# exactly that one.
# Run from project root:  Rscript tests/test_treasury_map_add_to_plan.R
# =============================================================================

suppressPackageStartupMessages(library(shiny))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
# Global map deliberately differs from the tenant's: the table must use the
# tenant map, like the button does.
COMPANY_MAP <- c(ZZZ = "Otra Empresa")
# The edge's table is on the hd_table engine (T13): its rows keyed by
# facturas_ids(), and "Agregar al plan" reads the engine's selection.
cargar <- function(archivo, nombre) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && identical(as.character(e[[2]]), nombre)) eval(e, envir = globalenv())
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/vencidos_module.R", "facturas_ids")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
.i18n_dir <- file.path(getwd(), "www", "i18n")
puede <- function(usuario, permiso) TRUE
source("R/treasury_map_module.R")

.pass <- 0L; .fail <- 0L
.chk <- function(ok, label) {
  if (isTRUE(ok)) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else            { cat(sprintf("  FAIL  %s\n", label)); .fail <<- .fail + 1L }
}

ar <- data.frame(
  .ini                   = "BBB",
  Parte                  = "Empresa A",
  Documento              = c("D-LATE", "D-EARLY", "D-MID"),
  `Fecha de vencimiento` = as.Date(c("2026-10-20", "2026-10-05", "2026-10-12")),
  `Saldo vencido`        = c(300, 100, 200),
  Moneda                 = "MXN",
  check.names = FALSE, stringsAsFactors = FALSE
)
shared <- list(
  company_map  = function() c(AAA = "Empresa A", BBB = "Empresa B"),
  current_user = function() "tester"
)
ic_rv <- reactive(list(ar = ar))

plan_docs <- function(plan) vapply(plan, function(x) x$Documento, character(1))

shiny::testServer(treasuryMapServer,
                  args = list(shared = shared, ic_invoices_rv = ic_rv), {
  session$setInputs(map_cur = "MXN")
  selected_edge(list(from_ini = "AAA", to_ini = "BBB"))
  session$flushReact()
  # The rows the engine draws (a datos() failure here means it drew none).
  filas <- tryCatch(.arista_tabla(), error = function(e) NULL)
  .chk(is.data.frame(filas) && nrow(filas) == 3L, "the edge's table gets its 3 invoices")
  docs <- if (is.data.frame(filas)) filas$Documento else "(none)"
  cat("  table order:", paste(docs, collapse = ", "), "\n")
  .chk(identical(docs, c("D-EARLY", "D-MID", "D-LATE")),
       "table lists the edge's 3 invoices by due date (tenant company map)")
  id_de <- function(doc) filas$id[match(doc, filas$Documento)]

  # Mark D-EARLY (by its id, as the engine sends it) and add it.
  session$setInputs(`arista_t-sel` = id_de("D-EARLY"), add_to_plan = 1L)
  cat("  plan after marking D-EARLY:", paste(plan_docs(payment_plan()), collapse = ", "), "\n")
  .chk(identical(plan_docs(payment_plan()), "D-EARLY"), "plan gets exactly the marked invoice (D-EARLY)")
  .chk(identical(payment_plan()[[1]]$Importe, 100), "with its own amount (100)")

  # Mark D-MID and D-LATE and add them too.
  session$setInputs(`arista_t-sel` = id_de(c("D-MID", "D-LATE")), add_to_plan = 2L)
  cat("  plan after marking rows 2,3:", paste(plan_docs(payment_plan()), collapse = ", "), "\n")
  .chk(setequal(plan_docs(payment_plan()), c("D-EARLY", "D-MID", "D-LATE")) &&
         length(payment_plan()) == 3L, "marking D-MID and D-LATE adds them, once each")
})

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
