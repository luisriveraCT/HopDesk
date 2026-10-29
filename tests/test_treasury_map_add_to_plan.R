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
  # Render the table the user sees (a req() failure here means it drew no rows).
  rendered <- tryCatch({ invisible(output$edge_inv_tbl); TRUE }, error = function(e) FALSE)
  .chk(rendered, "invoice table renders rows for the edge")
  keys <- tryCatch(edge_tbl_keys(), error = function(e) "(edge_tbl_keys missing)")
  cat("  table order:", paste(keys, collapse = ", "), "\n")
  .chk(identical(keys, c("D-EARLY", "D-MID", "D-LATE")),
       "table lists the edge's 3 invoices by due date (tenant company map)")

  # Mark the row drawn first (D-EARLY) and add it.
  session$setInputs(edge_inv_tbl_rows_selected = 1L, add_to_plan = 1L)
  cat("  plan after marking row 1:", paste(plan_docs(payment_plan()), collapse = ", "), "\n")
  .chk(identical(plan_docs(payment_plan()), "D-EARLY"), "plan gets exactly the marked invoice (D-EARLY)")
  .chk(identical(payment_plan()[[1]]$Importe, 100), "with its own amount (100)")

  # Mark rows 2 and 3 (D-MID, D-LATE) and add them too.
  session$setInputs(edge_inv_tbl_rows_selected = c(2L, 3L), add_to_plan = 2L)
  cat("  plan after marking rows 2,3:", paste(plan_docs(payment_plan()), collapse = ", "), "\n")
  .chk(setequal(plan_docs(payment_plan()), c("D-EARLY", "D-MID", "D-LATE")) &&
         length(payment_plan()) == 3L, "rows 2 and 3 add D-MID and D-LATE, once each")
})

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
