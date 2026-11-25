# =============================================================================
# tests/test_t84_importador_dorado.R
# T84 A1. Until now no test ran a whole BanBajío TXT through the parser. This
# one freezes what parse_banbajio_txt() gives for an INVENTED statement
# (tests/fixtures/t84/banbajio_movimientos.txt: names, RFC, accounts and
# tracking keys are all fictitious -- never real data in the repo) that walks
# every branch of .classify_tipo() and of the .extract_*():
#   SPEI enviado (with "Beneficiario:" and the old "Beneficiario ... Aut."),
#   its Comisión and IVA Comisión at 0 sharing the recibo, SPEI recibido
#   (with and without "Cuenta Ordenante"), TEF in and out, Retiro/Entrega de
#   Recursos, Devolución de SPEI, nómina, domiciliación, Pago de Servicios,
#   POS, cambios, plazo, Gasto admon, the English wires/fees/transfers/
#   payroll, an unknown text, a comma inside a description, a broken line
#   and a blank one.
# The expected rows (banbajio_movimientos_esperado.R) were written by
# master 6dd2eeff's own parser and reviewed row by row. The importers'
# registry (T84 A2) must give exactly these rows.
#   1  the rows, their order and their column types are exactly the frozen ones;
#   2  the account number of line 1 travels as the "numero_cuenta" attribute;
#   3  the same file with Windows line ends, or as one string, gives the same rows;
#   4  the columns are the parser's own empty schema, in its order;
#   5  every type the classifier knows appears at least once;
#   6  the fixture is invented (its company says FICTICIA).
#
# Run from project root:  Rscript tests/test_t84_importador_dorado.R
# =============================================================================
cat("=== test_t84_importador_dorado ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
source("R/bancos_parser.R")

DIR       <- "tests/fixtures/t84"
CUENTA    <- "cta-ficticia-01"
lineas    <- readLines(file.path(DIR, "banbajio_movimientos.txt"), encoding = "UTF-8", warn = FALSE)
esperado  <- eval(parse(file.path(DIR, "banbajio_movimientos_esperado.R"), encoding = "UTF-8"))
sin_attr  <- function(x) { attr(x, "numero_cuenta") <- NULL; x }

r <- parse_banbajio_txt(lineas, CUENTA)

cat("== 1. Las filas congeladas ==\n")
ok("1.1 26 filas: 27 líneas de datos menos la rota y la vacía", nrow(r) == 26L && nrow(esperado) == 26L)
ok("1.2 las filas, su orden y sus tipos son exactamente los congelados", identical(sin_attr(r), esperado),
   paste(capture.output(print(all.equal(sin_attr(r), esperado))), collapse = " / "))
ok("1.3 a igual fecha y hora (recibo 5001), el orden de entrada se respeta: SPEI, Comisión, IVA",
   identical(r$tipo[r$recibo == "5001"], c("spei_out", "comision", "comision")) &&
     grepl("^Comisi", r$descripcion_raw[r$recibo == "5001"][2]) && grepl("^IVA", r$descripcion_raw[r$recibo == "5001"][3]))
ok("1.4 la coma dentro de la descripción no rompe la fila (recibo 5002)",
   identical(r$concepto[r$recibo == "5002"], "SERVICIO, MANTENIMIENTO Y SOPORTE") && r$cargo[r$recibo == "5002"] == 2300)

cat("== 2. El encabezado ==\n")
ok("2.1 numero_cuenta sale de la línea 1", identical(attr(r, "numero_cuenta"), "0009876543"))

cat("== 3. Otras formas de la misma entrada ==\n")
ok("3.1 con fin de línea de Windows (\\r\\n) da las mismas filas", identical(sin_attr(parse_banbajio_txt(paste0(lineas, "\r"), CUENTA)), esperado))
ok("3.2 como un solo texto da las mismas filas", identical(sin_attr(parse_banbajio_txt(paste(lineas, collapse = "\n"), CUENTA)), esperado))

cat("== 4. El esquema ==\n")
vacio <- .empty_movimientos_parsed()
ok("4.1 las columnas son las del esquema vacío del parser, en su orden", identical(names(r), names(vacio)))
ok("4.2 y con sus mismas clases", identical(lapply(r, class), lapply(vacio, class)))

cat("== 5. Cobertura del clasificador ==\n")
tipos <- c("spei_out", "spei_in", "comision", "traspaso", "nomina", "domiciliacion", "otro", "pos", "cambio", "plazo")
ok("5.1 cada tipo que conoce .classify_tipo() aparece al menos una vez", all(tipos %in% r$tipo),
   paste("faltan:", paste(setdiff(tipos, r$tipo), collapse = ", ")))

cat("== 6. Solo datos inventados ==\n")
ok("6.1 el encabezado del fixture dice FICTICIA", grepl("FICTICIA", lineas[1], fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
