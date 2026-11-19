# =============================================================================
# tests/test_t55b_lupa.R
# T55b: the Lupa's "sent to the Agenda" notice added every invoice's amount
# into one total, whatever its currency: pesos and dollars summed, shown as
# "MXN/USD $ 1,050.00". Now one total per currency, in order, never summed:
# "1,000.00 MXN · 50.00 USD". Fails on b400502 (one sum, no helper).
#
# Run from project root:  Rscript --vanilla tests/test_t55b_lupa.R
# =============================================================================
cat("=== test_t55b_lupa ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("R/monedas.R")
source("R/i18n.R")
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (e in parse("R/search_module.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".lupa_totales_texto"))) eval(e, globalenv())
PUNTO <- intToUtf8(0xB7L)

ok("one total per currency, in order, never summed: 1,000 MXN and 50 USD are not 1,050",
   identical(.lupa_totales_texto(c(1000, 50), c("MXN", "USD")), paste("1,000.00 MXN", PUNTO, "50.00 USD")))
ok("…the same currency adds up, whatever its spelling (\"mxn \")",
   identical(.lupa_totales_texto(c(1000, 50, 234.56), c("MXN", "USD", "mxn ")), paste("1,234.56 MXN", PUNTO, "50.00 USD")))
ok("…each with its own decimals (JPY whole)",
   identical(.lupa_totales_texto(c(1500.4, 10), c("JPY", "USD")), paste("1,500 JPY", PUNTO, "10.00 USD")))
ok("…a single currency: one total",
   identical(.lupa_totales_texto(c(100, 200.5), c("USD", "USD")), "300.50 USD"))
ok("…rows without a currency add up apart, without a code; a missing amount counts as nothing",
   identical(.lupa_totales_texto(c(5, 7, NA), c(NA, "MXN", "MXN")), paste("5.00", PUNTO, "7.00 MXN")))

src <- paste(readLines("R/search_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("the notice uses it, and nothing in the Lupa sums Importe across currencies or calls fmt_money",
   grepl(".lupa_totales_texto(new_rows$Importe, new_rows$Moneda)", src, fixed = TRUE) &&
     !grepl("sum(new_rows$Importe", src, fixed = TRUE) && !grepl("fmt_money(", src, fixed = TRUE))
t_es <- tr("tablas.lupa.aviso.enviadas", n = "2", ledger = "Pagos", total = .lupa_totales_texto(c(1000, 50), c("MXN", "USD")), idioma = "es")
t_en <- tr("tablas.lupa.aviso.enviadas", n = "2", ledger = "Payments", total = "1,000.00 MXN", idioma = "en")
ok("the notice in both languages: the totals once, no currency before them",
   endsWith(t_es, paste0("Pagos ", intToUtf8(0x2014L), " 1,000.00 MXN ", PUNTO, " 50.00 USD.")) &&
     endsWith(t_en, paste0("Payments ", intToUtf8(0x2014L), " 1,000.00 MXN.")) && !grepl("{moneda}", t_es, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
