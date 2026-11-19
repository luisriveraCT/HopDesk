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

cat("-- every file T55b touches still parses (the checks below read text) --\n")
tocados <- c("R/bancos_module.R", "R/pasivos_module.R", "R/pasivos_table_module.R", "R/reporte_module.R",
             "R/search_module.R", "R/ui_components.R", "R/pagar_hoy_module.R", "R/global.R", "R/cashflow_export_module.R")
no_parsean <- tocados[!vapply(tocados, function(f) !inherits(tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"),
                                                                       error = function(e) e), "error"), TRUE)]
ok("the 9 files parse", { if (length(no_parsean)) message("  no parsea: ", paste(no_parsean, collapse = ", ")); !length(no_parsean) })

cat("\n-- C1: fmt_money() out of the files without an owner --\n")
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

cat("\n-- C2: the Agenda (pagar_hoy_module.R, integrator's permission) --\n")
ph <- leer("R/pagar_hoy_module.R")
ok("the Agenda: every amount on screen with monto_moneda() and its currency, none with the code before it",
   all(vapply(c("monto_moneda(out, cur)", "monto_moneda(in_, cur)", "monto_moneda(net, cur)",
                "monto_moneda(bal_val, cur_sel)", "monto_moneda(tp, cur_sel)", "monto_moneda(tc, cur_sel)",
                "monto_moneda(net, cur_sel)", "monto_moneda(ok$Importe[i], cur_sel)", "monto_moneda(skip$Importe[i], cur_sel)",
                "monto_moneda(sum(e$Importe, na.rm = TRUE), m)", "monto_moneda(sum(i$Importe, na.rm = TRUE), m)"),
               function(s) grepl(s, ph, fixed = TRUE), TRUE)) &&
     lengths(regmatches(ph, gregexpr("monto_moneda(sum(rows$Importe, na.rm = TRUE), cur)", ph, fixed = TRUE))) == 2L &&
     !grepl('cur, " ", fmt_money', ph, fixed = TRUE) && !grepl('cur_sel, " ", fmt_money', ph, fixed = TRUE))
ok("the payments pill in both languages: the total with its code, no currency before it",
   identical(tr("tablas.pagos.pildora", n = 3, total = monto_moneda(1500, "USD"), idioma = "es"), "3 pago(s) 1,500.00 USD") &&
     identical(tr("tablas.pagos.pildora", n = 3, total = monto_moneda(1500, "USD"), idioma = "en"), "3 payment(s) 1,500.00 USD"))

cat("\n-- the guardian: fmt_money() only for the activity log --\n")
llamadas <- unlist(lapply(c("app.R", list.files(c("R", "scripts"), "\\.R$", full.names = TRUE, recursive = TRUE)), function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  k <- grep("fmt_money(", x, fixed = TRUE)
  k <- k[!grepl("^\\s*#", x[k]) & !grepl("fmt_money <- function", x[k], fixed = TRUE)]
  if (length(k)) paste0(f, ":", k, ": ", trimws(x[k]))
}))
ok("one call to fmt_money() left: the Agenda's log_action text for the opening balance (what the log says stays)",
   length(llamadas) == 1L && grepl("^R/pagar_hoy_module.R:", llamadas) && grepl("Saldo de apertura", llamadas))
if (length(llamadas) != 1L) cat(llamadas, sep = "\n")
ok("currency_formatter() (no callers, \"$ € £\" by hand) is gone",
   !any(grepl("currency_formatter", unlist(lapply(c("app.R", list.files("R", "\\.R$", full.names = TRUE)), leer)), fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
