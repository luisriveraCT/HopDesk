# =============================================================================
# tests/test_t36_mapa_plan_total.R
# Mapa IC, "Plan de pago": each item keeps its invoice's Moneda, but the
# Total summed every item and labelled the sum with the currency the selector
# showed at that moment. Add MXN invoices, switch to USD, add USD ones: the
# Total mixed pesos and dollars under one code (T36, P4).
#
# Now: one Total per currency. A plan in one currency looks as before.
# Drives the real treasuryMapServer with shiny::testServer.
# Run from project root:  Rscript tests/test_t36_mapa_plan_total.R
# Contraprueba: TMAP_R=<old treasury_map_module.R> must fail.
# =============================================================================

suppressPackageStartupMessages(library(shiny))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
cargar <- function(archivo, nombre) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && identical(as.character(e[[2]]), nombre)) eval(e, envir = globalenv())
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/vencidos_module.R", "facturas_ids")
for (f in c("R/i18n.R", "R/monedas.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
.i18n_dir <- file.path(getwd(), "www", "i18n")
puede <- function(usuario, permiso) TRUE
source(Sys.getenv("TMAP_R", "R/treasury_map_module.R"))

.pass <- 0L; .fail <- 0L
.chk <- function(ok, label) {
  if (isTRUE(ok)) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else            { cat(sprintf("  FAIL  %s\n", label)); .fail <<- .fail + 1L }
}

ar <- data.frame(
  .ini                   = "BBB",
  Parte                  = "Empresa A",
  Documento              = c("M-1", "M-2", "U-1"),
  `Fecha de vencimiento` = as.Date(c("2026-10-05", "2026-10-12", "2026-10-20")),
  `Saldo vencido`        = c(1000, 2500, 300),
  Moneda                 = c("MXN", "MXN", "USD"),
  check.names = FALSE, stringsAsFactors = FALSE
)
shared <- list(
  company_map  = function() c(AAA = "Empresa A", BBB = "Empresa B"),
  current_user = function() "tester"
)
ic_rv <- reactive(list(ar = ar))

# The plan's totals as the user reads them: "<code> <amount>" of each
# .tm-plan-total row, and the amount of each item.
leer <- function(html) {
  tot <- regmatches(html, gregexpr('<div class="tm-plan-total">\\s*<span>Total</span>\\s*<span>[^<]*</span>', html))[[1]]
  tot <- sub('.*<span>([^<]*)</span>$', "\\1", tot)
  amt <- regmatches(html, gregexpr('<span class="tm-plan-amt">[^<]*</span>', html))[[1]]
  list(totales = gsub(" ", " ", tot), montos = gsub(" ", " ", sub('.*>([^<]*)</span>$', "\\1", amt)))
}

shiny::testServer(treasuryMapServer,
                  args = list(shared = shared, ic_invoices_rv = ic_rv), {
  html <- function() as.character(output$payment_plan_ui$html)

  session$setInputs(map_cur = "MXN")
  selected_edge(list(from_ini = "AAA", to_ini = "BBB"))
  session$flushReact()
  filas <- .arista_tabla()
  session$setInputs(`arista_t-sel` = filas$id[match(c("M-1", "M-2"), filas$Documento)], add_to_plan = 1L)

  # One currency: as before ("Total" + "MXN 3,500", items without a code).
  v <- leer(html())
  cat("  MXN only:", paste(v$totales, collapse = " | "), "/", paste(v$montos, collapse = ", "), "\n")
  .chk(identical(v$totales, "MXN 3,500"), "one currency: a single Total, MXN 3,500 (as before)")
  .chk(identical(v$montos, c("1,000", "2,500")), "one currency: items show the bare amount (as before)")

  # Switch to USD and add the USD invoice.
  session$setInputs(map_cur = "USD")
  session$flushReact()
  filas <- .arista_tabla()
  session$setInputs(`arista_t-sel` = filas$id[match("U-1", filas$Documento)], add_to_plan = 2L)
  .chk(identical(vapply(payment_plan(), function(x) x$Moneda, ""), c("MXN", "MXN", "USD")),
       "each plan item keeps its own invoice's currency")

  v <- leer(html())
  cat("  MXN + USD:", paste(v$totales, collapse = " | "), "/", paste(v$montos, collapse = ", "), "\n")
  .chk(!"USD 3,800" %in% v$totales, "the Total no longer adds pesos to dollars (it said USD 3,800)")
  .chk(identical(v$totales, c("MXN 3,500", "USD 300")), "one Total per currency, in the plan's order: MXN 3,500 and USD 300")
  .chk(identical(v$montos, c("MXN 1,000", "MXN 2,500", "USD 300")), "with two currencies each item says its own")

  # Back on MXN the totals don't depend on the selector.
  session$setInputs(map_cur = "MXN")
  .chk(identical(leer(html())$totales, c("MXN 3,500", "USD 300")), "the totals don't follow the selector")
})

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
