# =============================================================================
# tests/test_t55b_montos.R
# T55b: amounts outside the tables in monto_moneda()'s format ("1,234.56 MXN",
# no "$", the true minus sign, the catalog's decimals, the code after; the
# number alone where the currency is right there).
#   C1  fmt_money() in the files without an owner: Bancos, Pasivos, the
#       Reporte's screen, the Lupa (the Lupa's own test: test_t55b_lupa.R);
#       montos_por_moneda(), one total per currency, never summed.
#
# Run from project root:  Rscript --vanilla tests/test_t55b_montos.R
# =============================================================================
cat("=== test_t55b_montos ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("R/monedas.R")
source("R/i18n.R")
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (e in parse("R/ui_components.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("montos_por_moneda"))) eval(e, globalenv())
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
MENOS <- intToUtf8(0x2212L); PUNTO <- intToUtf8(0xB7L)

cat("-- C1: fmt_money() out of the files without an owner --\n")
libres <- c("R/bancos_module.R", "R/pasivos_module.R", "R/pasivos_table_module.R", "R/reporte_module.R", "R/search_module.R")
ok("no call to fmt_money() left in Bancos, Pasivos, Pasivos' table, the Reporte or the Lupa",
   !any(vapply(libres, function(f) grepl("fmt_money(", leer(f), fixed = TRUE), TRUE)))
bn <- leer("R/bancos_module.R")
ok("Bancos, an account's card and the ticker: the number alone, the true minus sign (the currency is beside it)",
   grepl("bal_lbl <- monto_moneda(saldo, ct$moneda, codigo = FALSE)", bn, fixed = TRUE) &&
     grepl("monto_moneda(saldo, mon, codigo = FALSE)", bn, fixed = TRUE) && !grepl('paste0("- ", fmt_money', bn, fixed = TRUE) &&
     identical(monto_moneda(-1234.5, "MXN", codigo = FALSE), paste0(MENOS, "1,234.50")))
ok("Bancos, deleting movements: cargos and abonos one total per currency, never summed",
   grepl("txt_cargo <- montos_por_moneda(rows$cargo[es_cargo], mon_fila[es_cargo])", bn, fixed = TRUE) &&
     grepl("txt_abono <- montos_por_moneda(rows$abono[es_abono], mon_fila[es_abono])", bn, fixed = TRUE) &&
     identical(montos_por_moneda(c(100, 20, 5.5), c("MXN", "USD", "MXN")), paste("105.50 MXN", PUNTO, "20.00 USD")))
ok("Bancos, deleting a confirmation: its amount with its currency",
   grepl('paste("Importe:", monto_moneda(row$importe, row$moneda))', bn, fixed = TRUE))
pm <- leer("R/pasivos_module.R")
ok("Pasivos, a provision's amount: the code after (\"1,234.56 MXN\"), the ≈ kept",
   grepl('"≈ ", monto_moneda(ax$importe_aprox, moneda)', pm, fixed = TRUE) && grepl("else monto_moneda(amount, moneda)", pm, fixed = TRUE))
ok("Pasivos' table, \"Próximas\": the amount with its code, the currency no longer before it (es and en)",
   grepl('tr("tablas.pasivos.resumen.proximas", monto = monto_moneda(total, cur), n = n)', leer("R/pasivos_table_module.R"), fixed = TRUE) &&
     identical(tr("tablas.pasivos.resumen.proximas", monto = monto_moneda(52000, "MXN"), n = 3, idioma = "es"),
               paste("52,000.00 MXN", PUNTO, "3 próx.")) &&
     identical(tr("tablas.pasivos.resumen.proximas", monto = monto_moneda(52000, "USD"), n = 3, idioma = "en"),
               paste("52,000.00 USD", PUNTO, "3 due")))
ok("the Reporte's screen, \"Total MXN\": the number alone (the label above names the currency)",
   grepl("monto_moneda(sum(bals_sel$saldo[bals_sel$moneda == m], na.rm = TRUE), m, codigo = FALSE)", leer("R/reporte_module.R"), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
