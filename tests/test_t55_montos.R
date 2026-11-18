# =============================================================================
# tests/test_t55_montos.R
# T55: one format for every amount, monto_moneda() ("1,234.56 MXN", no "$").
# Part 1: monto_moneda() moves to R/monedas.R (the engine of tables uses it
# too, and everything loads that file), with decimales= and codigo=, and
# formats per group of decimals. With its defaults it is identical() to the
# T46 version.
#
# Run from project root:  Rscript --vanilla tests/test_t55_montos.R
# =============================================================================
cat("=== test_t55_montos ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("R/monedas.R")
MENOS <- "−"; RAYA <- "—"

# The T46 version (R/ui_components.R at 3ec07df + ae31eb7), as the reference.
t46 <- function(x, moneda) {
  n <- max(length(x), length(moneda))
  if (!n) return(character())
  x <- rep_len(suppressWarnings(as.numeric(x)), n)
  moneda <- rep_len(toupper(trimws(as.character(moneda))), n)
  dec <- moneda_decimales(moneda); dec[is.na(dec)] <- 2L
  cifra <- vapply(seq_len(n), function(i) if (is.na(x[i])) "" else
    formatC(abs(round(x[i], dec[i])), format = "f", digits = dec[i], big.mark = ","), character(1))
  signo <- ifelse(!is.na(x) & round(x, dec) < 0, "−", "")
  codigo <- ifelse(is.na(moneda) | !nzchar(moneda), "", paste0(" ", moneda))
  ifelse(is.na(x), "—", paste0(signo, cifra, codigo))
}

cat("-- one definition, in R/monedas.R --\n")
defs <- unlist(lapply(c("app.R", list.files("R", "\\.R$", full.names = TRUE, recursive = TRUE)), function(f)
  if (any(grepl("^monto_moneda <- function", readLines(f, warn = FALSE)))) f))
ok("monto_moneda() is defined once, in R/monedas.R", identical(defs, "R/monedas.R"))
src <- paste(readLines("R/monedas.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("its minus sign and dash are escaped (\\u2212, \\u2014), not literal characters",
   grepl('"\\u2212"', src, fixed = TRUE) && grepl('"\\u2014"', src, fixed = TRUE) &&
     !grepl("−|—", sub("(?s)^.*monto_moneda <- function", "", src, perl = TRUE)))

cat("\n-- with its defaults: identical() to the T46 version --\n")
x   <- c(842300, -10950296, 0, -0.004, 1234.4, 1.5, NA, 2500.5, 999999.995, 0.5, -0.5, 12.345, 1e9)
mon <- c("MXN", "MXN", "USD", "USD", "JPY", "KWD", "EUR", "usd ", "MXN", "JPY", "JPY", "ZZZ", NA)
ok("a mixed vector (MXN, USD, JPY, KWD, NA amount, a code outside the catalog, no code, negatives, rounding edges)",
   identical(monto_moneda(x, mon), t46(x, mon)))
ok("recycling and empty input, as before",
   identical(monto_moneda(c(1, 2.5), "MXN"), t46(c(1, 2.5), "MXN")) &&
     identical(monto_moneda(numeric(), character()), character()))
ok("NA is still the dash, negative the true minus",
   identical(monto_moneda(NA, "MXN"), RAYA) && identical(monto_moneda(-5, "MXN"), paste0(MENOS, "5.00 MXN")))
set.seed(55)
xx <- round(runif(3000, -2e7, 2e7), sample(0:3, 3000, TRUE)); mm <- sample(c("MXN", "USD", "JPY", "KWD", "CLP", NA), 3000, TRUE)
xx[sample(3000, 50)] <- NA
ok("3,000 random amounts in six currencies: identical() to the T46 version", identical(monto_moneda(xx, mm), t46(xx, mm)))

cat("\n-- decimales= and codigo= --\n")
ok("decimales = 0: whole units for every currency (the Calendar's cells)",
   identical(monto_moneda(c(432152.4, 1387735.5, -7673.2), "MXN", decimales = 0),
             c("432,152 MXN", "1,387,736 MXN", paste0(MENOS, "7,673 MXN"))))
ok("codigo = FALSE: the number alone, with the catalog's decimals",
   identical(monto_moneda(c(2550363.49, 1234, 1.5), c("MXN", "JPY", "KWD"), codigo = FALSE),
             c("2,550,363.49", "1,234", "1.500")))
ok("both together", identical(monto_moneda(598265.2, "MXN", decimales = 0, codigo = FALSE), "598,265"))
ok("NA stays the dash with either", identical(monto_moneda(NA, "MXN", decimales = 0, codigo = FALSE), RAYA))

cat("\n-- part A: the engine of tables (hd_columnas.R, hdtable.js) --\n")
suppressPackageStartupMessages(library(shiny))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
.i18n_dir <- file.path(getwd(), "www", "i18n")
cd <- col_dinero("Importe")
v <- cd$valores(c(1234.5, -0.5, 1500, NA, 1e6), c("MXN", "MXN", "JPY", "MXN", "KWD"))
ok("a money cell: no \"$\", the catalog's decimals of its row's currency, the true minus sign, empty when missing",
   identical(v$mostrar, c("1,234.50", paste0(MENOS, "0.50"), "1,500", "", "1,000,000.000")))
ok("…what sorts and what Ctrl+C copies don't change (numbers, no format)",
   identical(v$orden, c(1234.5, -0.5, 1500, NA, 1e6)) && identical(v$copiar, c("1234.5", "-0.5", "1500", "", "1000000")))
ok("without its rows' currency (no such column, or a direct call): 2 decimals, as before, no \"$\"",
   identical(cd$valores(c(1234.5, 1500))$mostrar, c("1,234.50", "1,500.00")))
ent <- col_dinero("x", formato = "entero")$valores(c(5000120.5, -463209.4), "MXN")
cmp <- col_dinero("x", formato = "compacto")$valores(c(4495962, -802000, 950, -0.4), "MXN")
ok("formato entero and compacto: no \"$\", the true minus sign",
   identical(ent$mostrar, c("5,000,121", paste0(MENOS, "463,209"))) &&
     identical(cmp$mostrar, c("4.5M", paste0(MENOS, "802K"), "950", "0")))
datos <- data.frame(id = c("a", "b"), Importe = c(10, 2000), Moneda = c("MXN", "JPY"), stringsAsFactors = FALSE)
fl <- hd_filas(datos, list(col_texto("id"), col_dinero("Importe")))
ok("the payload: each cell by its row's currency, and the decimals of each currency and its field for the JS",
   identical(unclass(fl$celdas[[2]]$mostrar), c("10.00", "2,000")) &&
     identical(fl$decimales, list(MXN = 2L, JPY = 0L)) && identical(fl$campo_moneda, "Moneda"))
sin_mon <- hd_filas(datos[, c("id", "Importe")], list(col_texto("id"), col_dinero("Importe")))
ok("a table whose data has no currency column still works (2 decimals)",
   identical(unclass(sin_mon$celdas[[2]]$mostrar), c("10.00", "2,000.00")))
source("R/pasivos_edit_confirm_module.R")
ok("the direct call in Pasivos' conflicts: the amount with its currency (never a figure without it)",
   identical(.pec_fecha_monto(as.Date(NA), c(150, NA), "MXN"), c(paste("—", "·", "150.00 MXN"), paste("—", "·", RAYA))))

cat("\n-- exports keep the number --\n")
xl <- tempfile(fileext = ".xlsx")
hd_excel_escribir(datos, list(col_texto("id"), col_dinero("Importe")), ids = c("a", "b"), id_col = "id", archivo = xl)
leido <- openxlsx::read.xlsx(xl)
ok("Excel: the money column is numeric, the amounts as they are", is.numeric(leido[[2]]) && identical(leido[[2]], c(10, 2000)))

cat("\n-- the JS writes what R writes --\n")
js <- paste(readLines("www/hdtable.js", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
pieza <- regmatches(js, regexpr("(?s)  var FMT_N = new Intl.NumberFormat\\('es-MX'\\);.*?\\n  function celdaLlevaCodigo", js, perl = TRUE))
nodo <- Sys.which("node")
if (!nzchar(nodo)) nodo <- "node"
prog <- tempfile(fileext = ".js")
set.seed(5)
azar <- c(round(runif(3000, -2e7, 2e7), sample(0:4, 3000, TRUE)),
          cumsum(round(runif(3000, -1e4, 1e4), 2)))      # totals: sums with their float error
casos <- list(x = c(842300, -10950296, 0, -0.004, 1234.4, 1.5, 2500.5, 0.5, -0.5, 2.5, -2.5, 0.125, 2.675, 999999.995, azar),
              m = c("MXN", "MXN", "USD", "USD", "JPY", "KWD", "", "JPY", "JPY", "JPY", "JPY", "MXN", "MXN", "MXN",
                    sample(c("MXN", "USD", "JPY", "KWD", ""), length(azar), TRUE)))
writeLines(c(sub("\\n  function celdaLlevaCodigo$", "", pieza),
             "DEC = { MXN: 2, USD: 2, JPY: 0, KWD: 3 };",
             sprintf("var x = %s, m = %s;", paste0("[", paste(sprintf("%.17g", casos$x), collapse = ","), "]"),jsonlite::toJSON(casos$m)),
             "console.log(JSON.stringify(x.map(function (v, i) { return dinero(v, m[i]); })));"), prog, useBytes = TRUE)
salida <- tryCatch(suppressWarnings(system2(nodo, prog, stdout = TRUE, stderr = TRUE)), error = function(e) "sin node")
de_js <- tryCatch(jsonlite::fromJSON(paste(salida, collapse = "")), error = function(e) NULL)
de_r  <- monto_moneda(casos$x, ifelse(nzchar(casos$m), casos$m, NA))
if (!identical(de_js, de_r)) { k <- which(de_js != de_r); print(head(data.frame(x = casos$x[k], m = casos$m[k], js = de_js[k], r = de_r[k]))) }
ok("dinero() in hdtable.js gives exactly monto_moneda()'s text (decimals, minus, code after, ties to even), 6,014 amounts and sums",
   identical(de_js, de_r))

cat("\n-- the JS: when a cell carries its code --\n")
ok("a cell says its currency only when no band and no shown column does (celdaLlevaCodigo())",
   grepl("function celdaLlevaCodigo(st)", js, fixed = TRUE) &&
     grepl("s.campo === cm || s.campo === '.moneda'", js, fixed = TRUE) &&
     grepl("st.cols[j].campo === cm", js, fixed = TRUE) &&
     grepl("if (c.tipo === 'dinero' && v && celdaLlevaCodigo(st)) v = conCodigo(v, st.moneda[i] || '')", js, fixed = TRUE))
ok("totals: the code after, always (fichas, bands, footer); no \"$\" left",
   !grepl("+ '$'", js, fixed = TRUE) && !grepl("mons.length > 1 && m", js, fixed = TRUE) &&
     grepl("dinero(grupos[tipo][mon], mon)", js, fixed = TRUE) &&
     grepl("return mons.map(function (m) { return dinero(s[m], m); })", js, fixed = TRUE))

# A group's sums sit in the column: the cell's rule (Coordinador, T55). The
# real sumaColumna() and celdaLlevaCodigo(), run in node on a table with and
# without a currency band.
funcion <- function(nombre) regmatches(js, regexpr(paste0("(?s)\\n  function ", nombre, "\\(.*?\\n  }\\n"), js, perl = TRUE))
prog2 <- tempfile(fileext = ".js")
writeLines(c("var MENOS = '−', DEC = { MXN: 2, JPY: 0 }, FMT_DEC = {}, FMT_N = new Intl.NumberFormat('es-MX');",
             "function t(k, o) { return (k === 'tablas.dinero.miles' ? '{n}K' : '{n}M').replace('{n}', o.n); }",
             vapply(c("fmtDec", "decimalesDe", "conCodigo", "redondear", "dinero", "celdaLlevaCodigo", "sumaColumna",
                      "entero", "compacto"), funcion, ""),
             "function tabla(conBanda, formato) { return { cols: [{ campo: 'Importe', formato: formato }, { campo: 'Moneda' }],",
             "  celdas: [{ orden: [1500.5, 2500, 300] }], moneda: ['MXN', 'MXN', 'JPY'], ids: ['a', 'b', 'c'], ed: {}, colEd: -1,",
             "  campoMoneda: 'Moneda', ver: [0], secciones: conBanda ? [{ campo: 'Moneda' }] : [] }; }",
             "console.log(JSON.stringify([sumaColumna(tabla(true), 0, [0, 1]), sumaColumna(tabla(true), 0, [2]),",
             "  sumaColumna(tabla(false), 0, [0, 1, 2]), sumaColumna(tabla(true, 'compacto'), 0, [0, 1]),",
             "  sumaColumna(tabla(false, 'compacto'), 0, [0, 1])]));"), prog2, useBytes = TRUE)
sumas <- tryCatch(jsonlite::fromJSON(paste(suppressWarnings(system2(nodo, prog2, stdout = TRUE, stderr = TRUE)), collapse = "")),
                  error = function(e) NULL)
ok("a group's sums under a currency band: the number alone, with the currency's decimals (JPY whole)",
   identical(sumas[1:2], c("4,000.50", "300")))
ok("…in a table that mixes currencies with no band or column of it: each sum with its code",
   identical(sumas[3], paste0("300 JPY", " · ", "4,000.50 MXN")))   # the currencies in order, like the footer
ok("…the compact format, the same rule", identical(sumas[4:5], c("4K", "4K MXN")))
ok("the group's total in its sum column: the cell's rule too (dinero(s[mo], mo, !codG))",
   grepl("codG = celdaLlevaCodigo(st)", js, fixed = TRUE) && grepl("return dinero(s[mo], mo, !codG);", js, fixed = TRUE))
ok("editing reads back what the table shows (code, true minus)",
   grepl(".replace(/[A-Za-z]{3}\\s*$/, '')", js, fixed = TRUE))

cat("\n-- part B: the Calendar's month cells and the day's pill --\n")
gl <- readLines("R/global.R", warn = FALSE, encoding = "UTF-8")
ini <- grep("^calendar_html <- function", gl); fin <- grep("^[A-Za-z_.][A-Za-z0-9_.]* <- function", gl)
cal <- paste(gl[ini:(min(c(fin[fin > ini], length(gl) + 1L)) - 1L)], collapse = "\n")   # its source, as written
ok("calendar_html(): no \"$\" and no formatter of its own; monto_moneda() with the currency of `currency`, whole units",
   !grepl("'$ '", cal, fixed = TRUE) && !grepl("formatC(", cal, fixed = TRUE) &&
     grepl("monto_moneda(v, cur, decimales = 0L, codigo = codigo)", cal, fixed = TRUE))
lm <- paste(readLines("R/ledger_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("the day's \"In the Agenda\" pill: the total with its currency (monto_moneda(total, ctx$cur))",
   grepl("tags$strong(monto_moneda(total, ctx$cur))", lm, fixed = TRUE) && !grepl("tags$strong(fmt_money(total))", lm, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
