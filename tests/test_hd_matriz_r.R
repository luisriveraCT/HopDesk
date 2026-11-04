# hd_table engine, the matrix base (T17a), R side: columna_fija
# (col_*(fija = TRUE)), tono_celda (col_dinero(tono, destacar)) and
# dinero_compacto (col_dinero(formato = "compacto", cero = "vacio")): their
# values, the checks hd_table_server makes, the Excel, and that a table
# without them sends exactly what it sent before T17a (byte for byte against
# tests/fixtures/hd_matriz_antes.json, built with the code of 57cfbce).
# Run from the repo root: Rscript tests/test_hd_matriz_r.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
source("tests/helpers/hd_matriz_muestra.R")

PIEZAS <- c("tono_celda", "dinero_compacto")   # columna_fija comes with class F

# ── Nothing changes for a table that doesn't ask ────────────────────────────
test_that("without the new options, columns and rows are byte for byte what the engine sent before T17a", {
  m <- hd_matriz_muestra()
  ahora <- as.character(jsonlite::toJSON(list(meta = hd_columnas_meta(m$columnas), filas = hd_filas(m$datos, m$columnas)),
                                         auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
  antes <- readLines("tests/fixtures/hd_matriz_antes.json", warn = FALSE, encoding = "UTF-8")
  expect_identical(enc2utf8(ahora), enc2utf8(paste(antes, collapse = "\n")))
})

test_that("no table outside class F gets the new pieces, and in F they are optional", {
  for (tabla in names(hd_registro)) {
    if (hd_registro[[tabla]]$clase == "F") next
    p <- hd_piezas_motor(tabla, hd_piezas_de(tabla))
    expect_false(any(c("columna_fija", "tono_celda", "dinero_compacto") %in% p), info = tabla)
  }
  expect_true(all(c("tono_celda", "dinero_compacto") %in% hd_clases$F$opcional))
  expect_false(any(c("tono_celda", "dinero_compacto") %in% hd_piezas_de("flujo_caja")))
  expect_true(all(c("columna_fija", "tono_celda", "dinero_compacto") %in% HD_PIEZAS_MOTOR))
})

# ── dinero_compacto ─────────────────────────────────────────────────────────
test_that("compacto: $4.5M / $802K / $950, unit by what it rounds to; the full amount in the bubble, copy and sort", {
  x <- c(4495962, 802000, 999600, 999499, 999.6, 950, -1500, 1234567890, 0, NA)
  v <- col_dinero("v", formato = "compacto")$valores(x)
  expect_identical(v$mostrar, c("$4.5M", "$802K", "$1.0M", "$999K", "$1K", "$950", "-$2K", "$1234.6M", "$0", ""))
  # Halves round up, never to even: 1,500 and 2,500 don't both read "$2K".
  expect_identical(col_dinero("v", formato = "compacto")$valores(c(1500, 2500, 4450000, 4550000, 2.5, 3.5))$mostrar,
                   c("$2K", "$3K", "$4.5M", "$4.6M", "$3", "$4"))
  expect_identical(v$completo, c("$4,495,962.00", "$802,000.00", "$999,600.00", "$999,499.00", "$999.60", "$950.00",
                                 "-$1,500.00", "$1,234,567,890.00", "$0.00", ""))
  expect_identical(v$copiar, c("4495962", "802000", "999600", "999499", "999.6", "950", "-1500", "1234567890", "0", ""))
  expect_identical(v$orden, c(4495962, 802000, 999600, 999499, 999.6, 950, -1500, 1234567890, 0, NA))
})

test_that("cero = \"vacio\": a 0 paints nothing (it still copies and sorts as 0); other amounts as always", {
  v <- col_dinero("v", cero = "vacio")$valores(c(0, -0.001, 12.5, NA))
  expect_identical(v$mostrar, c("", "", "$12.50", ""))
  expect_identical(v$copiar, c("0", "0", "12.5", ""))
  expect_identical(v$orden, c(0, 0, 12.5, NA))
  expect_null(v$completo)
  w <- col_dinero("v", formato = "compacto", cero = "vacio")$valores(c(0, 2500))
  expect_identical(w$mostrar, c("", "$3K"))
  expect_identical(w$completo, c("", "$2,500.00"))
})

test_that("the compact units come from the dictionary, in each language", {
  dic <- function(lang) jsonlite::fromJSON(file.path("www", "i18n", paste0(lang, ".json")))
  for (lang in c("es", "en")) {
    expect_identical(dic(lang)[["tablas.dinero.millones"]], "{n}M")
    expect_identical(dic(lang)[["tablas.dinero.miles"]], "{n}K")
  }
})

# ── tono_celda ──────────────────────────────────────────────────────────────
test_that("tono = \"signo\": green above 0, red below, grey at 0, none without a value", {
  v <- col_dinero("v", tono = "signo")$valores(c(5, -0.2, 0, -0.001, NA))
  expect_identical(v$tono, c("exito", "peligro", "neutro", "neutro", ""))
})

test_that("tono = \"<campo>\": each row's field, only tones of the closed list (anything else: none)", {
  d <- data.frame(id = c("a", "b", "c", "d"), v = 1:4, t = c("aviso", "rojo\" onclick=x", NA, "morado"),
                  stringsAsFactors = FALSE)
  f <- hd_filas(d, list(col_dinero("v", tono = "t")))
  expect_identical(unclass(f$celdas[[1]]$tono), c("aviso", "", "", "morado"))
  expect_error(hd_filas(d, list(col_dinero("v", tono = "nada"))), "tono names 'nada', which is not in the data")
})

test_that("the column metadata says fija / destacar / tono only when asked", {
  m <- hd_columnas_meta(list(col_texto("a", fija = TRUE), col_dinero("b", tono = "signo", destacar = TRUE),
                             col_dinero("c", tono = "t"), col_dinero("d")))
  expect_true(m[[1]]$fija)
  expect_true(m[[2]]$destacar)
  expect_identical(m[[2]]$tono, "signo")
  expect_identical(m[[3]]$tono, "estado")
  expect_false(any(c("fija", "destacar", "tono", "formato", "cero") %in% names(m[[4]])))
  expect_false("fija" %in% names(m[[2]]))
})

# ── The contract's checks ───────────────────────────────────────────────────
test_that("bad values of the new arguments stop with a clear message", {
  expect_error(col_dinero("v", formato = "corto"), "`formato` must be")
  expect_error(col_dinero("v", cero = "nada"), "`cero` must be")
  expect_error(col_dinero("v", tono = c("a", "b")), "`tono` must be")
  expect_error(col_dinero("v", tono = ""), "`tono` must be")
  expect_error(col_dinero("v", destacar = NA), "`destacar` must be")
  expect_error(col_texto("v", fija = "si"), "`fija` must be TRUE or FALSE")
  expect_error(col_empresa("v", fija = NA), "`fija` must be TRUE or FALSE")
})

test_that("a column that asks for a piece the table doesn't have stops (never dropped silently)", {
  expect_error(hd_columnas_piezas(list(col_texto("a", fija = TRUE)), "x", character()), "'columna_fija' piece")
  expect_error(hd_columnas_piezas(list(col_dinero("a", tono = "signo")), "x", character()), "'tono_celda' piece")
  expect_error(hd_columnas_piezas(list(col_dinero("a", destacar = TRUE)), "x", character()), "'tono_celda' piece")
  expect_error(hd_columnas_piezas(list(col_dinero("a", formato = "compacto")), "x", character()), "'dinero_compacto' piece")
  expect_error(hd_columnas_piezas(list(col_dinero("a", cero = "vacio")), "x", character()), "'dinero_compacto' piece")
  expect_silent(hd_columnas_piezas(list(col_dinero("a")), "x", character()))
})

test_that("fixed columns are the first ones, one after the other, and never optional", {
  p <- "columna_fija"
  expect_silent(hd_columnas_piezas(list(col_texto("a", fija = TRUE), col_empresa("b", fija = TRUE), col_dinero("c")), "x", p))
  expect_error(hd_columnas_piezas(list(col_texto("a"), col_texto("b", fija = TRUE)), "x", p), "must be the first ones")
  expect_error(hd_columnas_piezas(list(col_texto("a", fija = TRUE), col_texto("b"), col_texto("c", fija = TRUE)), "x", p),
               "must be the first ones")
  expect_error(hd_columnas_piezas(list(col_texto("a", fija = TRUE, opcional = TRUE)), "x", p), "both fija and opcional")
})

test_that("hd_table_server checks it when the table is made, and for reactive columns when they come", {
  d <- data.frame(id = "a", Nombre = "x", v = 1, stringsAsFactors = FALSE)
  expect_error(hd_table_server("t", tabla = "flujo_caja", datos = reactive(d),
                               columnas = list(col_texto("Nombre"), col_dinero("v", tono = "signo"))),
               "'tono_celda' piece")
  expect_error(hd_table_server("t", tabla = "evidencia_excel", datos = reactive(d),
                               columnas = list(col_texto("Nombre", fija = TRUE))),
               "'columna_fija' piece")
  # Reactive columns: the load fails, the session doesn't (the table says so).
  testServer(function(input, output, session) {
    hd_table_server("t", tabla = "flujo_caja", datos = reactive(d), piezas = PIEZAS,
                    columnas = reactive(list(col_texto("Nombre"), col_texto("v", fija = TRUE))))
  }, {
    msgs <- capture_messages(session$flushReact())
    expect_true(any(grepl("the datos failed, the table shows a notice: .*must be the first ones", msgs)))
  })
})

test_that("a class F table with all three sends its columns and rows", {
  d <- data.frame(id = c("a", "b"), Nombre = c("x", "y"), Empresa = "NTS", hoy = c(4495962, 0),
                  est = c(5, -5), Estado = c("aviso", "nada"), stringsAsFactors = FALSE)
  cols <- list(col_texto("Nombre", fija = TRUE), col_empresa("Empresa", fija = TRUE),
               col_dinero("hoy", formato = "compacto", cero = "vacio", tono = "signo", destacar = TRUE),
               col_dinero("est", tono = "Estado", sumar = FALSE))
  expect_silent(hd_columnas_piezas(cols, "flujo_caja", hd_piezas_de("flujo_caja", PIEZAS)))
  f <- hd_filas(d, cols)
  expect_identical(unclass(f$celdas[[3]]$mostrar), c("$4.5M", ""))
  expect_identical(unclass(f$celdas[[3]]$tono), c("exito", "neutro"))
  expect_identical(unclass(f$celdas[[4]]$tono), c("aviso", ""))
})

# ── Excel: the full amounts, never the compact text ─────────────────────────
test_that("the Excel of a compact column has the full numbers (and 0, not empty)", {
  skip_if_not_installed("openxlsx")
  d <- data.frame(id = c("a", "b", "c"), Nombre = c("x", "y", "z"), v = c(4495962.25, 0, -1500),
                  stringsAsFactors = FALSE)
  archivo <- tempfile(fileext = ".xlsx")
  on.exit(unlink(archivo), add = TRUE)
  hd_excel_escribir(d, list(col_texto("Nombre", fija = TRUE),
                            col_dinero("v", formato = "compacto", cero = "vacio", tono = "signo")),
                    c("a", "b", "c"), "id", archivo)
  x <- openxlsx::read.xlsx(archivo)
  expect_identical(x$v, c(4495962.25, 0, -1500))
})
