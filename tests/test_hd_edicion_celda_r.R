# hd_table engine, piece edicion_celda (T14), R side: the contract of
# col_dinero(editable), the checks hd_table_server makes, hd_ediciones()
# (what t$ediciones() gives), the Excel with what was typed, and that a table
# without `editable` sends exactly what it sent before.
# Run from the repo root: Rscript tests/test_hd_edicion_celda_r.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)

facturas <- data.frame(
  id = c("a", "b", "c", "d"), Referencia = c("F1", "F2", "F3", "F4"), Moneda = "MXN",
  Saldo = c(1000, 500, 250, 80), Importe = c(1000, 500, 250, 80),
  .bloqueada = c(FALSE, FALSE, FALSE, TRUE), stringsAsFactors = FALSE, check.names = FALSE)
cols_abono <- function(editable = list(min = 0, max = "Saldo")) list(
  col_texto("Referencia", "Referencia"),
  col_dinero("Saldo", "Saldo", sumar = FALSE),
  col_dinero("Importe", "Importe", editable = editable))
PIEZAS <- c("casillas", "edicion_celda")

# ── The contract of col_dinero(editable) ────────────────────────────────────
test_that("editable: list(min, max, paso, aviso); min/max a number or a field", {
  col <- col_dinero("Importe", editable = list(min = 0, max = "Saldo", aviso = "tablas.edicion.mayor"))
  expect_identical(col$editable, list(min = 0, max = "Saldo", paso = 0.01, aviso = "tablas.edicion.mayor"))
  expect_null(col_dinero("Importe")$editable)
  expect_error(col_dinero("Importe", editable = TRUE), "must be list")
  expect_error(col_dinero("Importe", editable = list(maximo = 3)), "unknown field")
  expect_error(col_dinero("Importe", editable = list(max = c(1, 2))), "one number or the name")
  expect_error(col_dinero("Importe", editable = list(min = 5, max = 1)), "min is greater")
  expect_error(col_dinero("Importe", editable = list(paso = 0)), "positive")
})

test_that("only col_dinero has editable (P4)", {
  expect_error(col_numero("Importe", editable = list(max = 1)), "unused argument")
  expect_error(col_texto("Referencia", editable = list(max = 1)), "unused argument")
})

# ── What hd_table_server checks ─────────────────────────────────────────────
test_that("an editable column needs the piece, is the only one and is the one that sums", {
  expect_identical(hd_columna_editable(cols_abono(), "abono_parcial", hd_piezas_de("abono_parcial", PIEZAS))$campo,
                   "Importe")
  expect_null(hd_columna_editable(cols_abono(NULL), "abono_parcial", hd_piezas_de("abono_parcial")))
  expect_error(hd_columna_editable(cols_abono(), "abono_parcial", hd_piezas_de("abono_parcial")),
               "doesn't have the 'edicion_celda' piece")
  dos <- list(col_dinero("Saldo", editable = list(max = 1)), col_dinero("Importe", editable = list(max = 1)))
  expect_error(hd_columna_editable(dos, "abono_parcial", PIEZAS), "only one editable column")
  suman_dos <- list(col_dinero("Saldo"), col_dinero("Importe", editable = list(max = "Saldo")))
  expect_error(hd_columna_editable(suman_dos, "abono_parcial", PIEZAS), "must be the only one with sumar = TRUE")
  no_suma <- list(col_dinero("Saldo"), col_dinero("Importe", sumar = FALSE, editable = list(max = "Saldo")))
  expect_error(hd_columna_editable(no_suma, "abono_parcial", PIEZAS), "must be the only one with sumar = TRUE")
  # At creation, not on the first data (fixed columns).
  expect_error(hd_table_server("t", tabla = "abono_parcial", datos = reactive(facturas), columnas = cols_abono()),
               "edicion_celda")
})

test_that("a bound that names a field not in the data stops (validated with the data)", {
  expect_error(hd_validar_columnas(cols_abono(list(max = "SaldoX")), facturas), "names 'SaldoX'")
})

# ── Row payload and column metadata ─────────────────────────────────────────
test_that("the editable column carries each row's bounds and its warnings; other columns nothing new", {
  f <- hd_filas(facturas, cols_abono(), "id")
  expect_identical(as.numeric(f$celdas[[3]]$max), c(1000, 500, 250, 80))
  expect_identical(as.numeric(f$celdas[[3]]$min), c(0, 0, 0, 0))
  expect_null(f$celdas[[2]]$max)
  m <- hd_columnas_meta(cols_abono())
  expect_identical(m[[3]]$editable$paso, 0.01)
  expect_identical(m[[3]]$editable$mayor, tr("tablas.edicion.mayor"))
  expect_match(m[[3]]$editable$mayor, "{max}", fixed = TRUE)
  expect_null(m[[2]]$editable)
  # A table's own warning replaces both.
  m2 <- hd_columnas_meta(cols_abono(list(max = "Saldo", aviso = "tablas.error_datos")))
  expect_identical(m2[[3]]$editable$mayor, tr("tablas.error_datos"))
  expect_identical(m2[[3]]$editable$menor, tr("tablas.error_datos"))
})

test_that("without editable, the message is byte for byte what it was (no table changes)", {
  sin <- list(col_texto("Referencia", "Referencia"), col_dinero("Saldo", "Saldo", sumar = FALSE),
              col_dinero("Importe", "Importe"))
  antes <- '{"campo":"Importe","titulo":"Importe","tipo":"dinero","alinear":"derecha","recorte":false,"sumar":true,"opcional":false,"visible":true,"ancho":null}'
  expect_identical(as.character(shiny:::toJSON(hd_columnas_meta(sin)[[3]])), antes)
  expect_identical(names(hd_filas(facturas, sin, "id")$celdas[[3]]), c("mostrar", "orden", "copiar"))
})

# ── hd_ediciones(): what t$ediciones() gives ────────────────────────────────
ed <- function(ids, valores) list(ids = as.list(ids), valores = as.list(valores), nonce = 1)
col <- cols_abono()[[3]]

test_that("every amount typed, valid or not, checked against the row's own bounds (P2)", {
  r <- hd_ediciones(ed(c("a", "b", "c"), c(400, 600, 250.004)), facturas, col, "id")
  expect_identical(r$id, c("a", "b", "c"))
  expect_identical(r$valor, c(400, 600, 250))
  expect_identical(r$valido, c(TRUE, FALSE, TRUE))      # b: 600 > its Saldo 500
})

test_that("a negative amount is invalid unless min is below 0; NA bounds are no bound", {
  expect_false(hd_ediciones(ed("a", -1), facturas, col, "id")$valido)
  sin_min <- cols_abono(list(max = "Saldo"))[[3]]
  expect_false(hd_ediciones(ed("a", -1), facturas, sin_min, "id")$valido)
  neg <- cols_abono(list(min = -100))[[3]]
  expect_true(hd_ediciones(ed("a", -50), facturas, neg, "id")$valido)
  f2 <- facturas; f2$Saldo[1] <- NA
  expect_true(hd_ediciones(ed("a", 99999), f2, col, "id")$valido)
})

test_that("ids no longer in the data, locked rows, repeats and non-numbers drop out", {
  r <- hd_ediciones(ed(c("a", "zz", "d", "a", "b"), c(10, 10, 10, 20, NA)), facturas, col, "id")
  expect_identical(r$id, "a")
  expect_identical(r$valor, 10)
})

test_that("new data (another session) re-checks against the new bounds", {
  e <- ed(c("a", "b"), c(400, 450))
  expect_identical(hd_ediciones(e, facturas, col, "id")$valido, c(TRUE, TRUE))
  f2 <- facturas; f2$Saldo[2] <- 300                        # someone paid part of b
  expect_identical(hd_ediciones(e, f2, col, "id")$valido, c(TRUE, FALSE))
  f3 <- facturas[facturas$id != "a", ]                      # a is gone
  expect_identical(hd_ediciones(e, f3, col, "id")$id, "b")
})

test_that("nothing typed (or no editable column): an empty data.frame with the three columns", {
  for (r in list(hd_ediciones(NULL, facturas, col, "id"), hd_ediciones(ed(character(), numeric()), facturas, col, "id"),
                 hd_ediciones(ed("a", 1), facturas, NULL, "id"), hd_ediciones(ed("a", 1), NULL, col, "id"))) {
    expect_identical(names(r), c("id", "valor", "valido"))
    expect_identical(nrow(r), 0L)
  }
})

# ── The server: t$ediciones(), t$limpiar_ediciones() ────────────────────────
test_that("t$ediciones() follows the browser's ediciones and the data", {
  datos <- reactiveVal(facturas)
  testServer(function(input, output, session) {
    t <- hd_table_server("tab", tabla = "abono_parcial", datos = datos, columnas = cols_abono(), piezas = PIEZAS)
    session$userData$t <- t
  }, {
    t <- session$userData$t
    expect_identical(nrow(t$ediciones()), 0L)
    session$setInputs(`tab-ediciones` = ed(c("a", "b"), c(300, 900)))
    expect_identical(t$ediciones()$valido, c(TRUE, FALSE))
    f2 <- facturas; f2$.bloqueada[1] <- TRUE                # a got paid in another session
    datos(f2)
    session$flushReact()
    expect_identical(t$ediciones()$id, "b")
    msgs <- list()
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- list(type = type, message = message)
    t$limpiar_ediciones()
    expect_identical(msgs[[1]]$type, "hdt-ediciones")
    expect_true(isTRUE(msgs[[1]]$message$limpiar))
  })
})

test_that("a table without the piece: t$ediciones() is empty whatever comes", {
  testServer(function(input, output, session) {
    session$userData$t <- hd_table_server("tab", tabla = "abono_parcial", datos = reactive(facturas),
                                          columnas = cols_abono(NULL))
  }, {
    session$setInputs(`tab-ediciones` = ed("a", 300))
    expect_identical(nrow(session$userData$t$ediciones()), 0L)
  })
})

# ── Excel: what the screen shows (P3) ───────────────────────────────────────
test_that("the Excel carries the amounts typed in the editable column, the rest as the data", {
  skip_if_not_installed("openxlsx")
  archivo <- tempfile(fileext = ".xlsx")
  on.exit(unlink(archivo), add = TRUE)
  hd_excel_escribir(facturas, cols_abono(), c("a", "b", "c"), "id", archivo,
                    ediciones = data.frame(id = c("b", "zz"), valor = c(123.45, 1), valido = TRUE))
  x <- openxlsx::read.xlsx(archivo)
  expect_identical(x$Importe, c(1000, 123.45, 250))
  expect_identical(x$Saldo, c(1000, 500, 250))
  hd_excel_escribir(facturas, cols_abono(), c("a", "b"), "id", archivo)    # without: as before
  expect_identical(openxlsx::read.xlsx(archivo)$Importe, c(1000, 500))
})
