# hd_table engine, casillas_columna (T68), R side:
# - col_casilla(): a yes/no cell, several per table;
# - the piece is optional in class E only, and only avisos_de has it;
# - a table without it sends byte for byte what it sent before;
# - hd_casillas() / t$casillas(): every cell now, the data's unless the
#   person ticked it otherwise, checked again on the server.
# The browser half: tests/test_hd_casillas_columna_chromote.R.
# Run from the repo root: Rscript tests/test_hd_casillas_columna_r.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
source("tests/helpers/hd_matriz_muestra.R")

test_that("a table without col_casilla sends byte for byte what it sent before", {
  m <- hd_matriz_muestra()
  f <- hd_filtros_validar(list(list(campo = "Estado"), list(campo = "Moneda", titulo = "Divisa")))
  ahora <- as.character(jsonlite::toJSON(list(meta = hd_columnas_meta(m$columnas), filas = hd_filas(m$datos, m$columnas),
                                              filtros = hd_filtros_payload(f, m$datos, m$columnas)),
                                         auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
  antes <- readLines("tests/fixtures/hd_opcion_ayuda_antes.json", warn = FALSE, encoding = "UTF-8")
  expect_identical(enc2utf8(ahora), enc2utf8(paste(antes, collapse = "\n")))
})

test_that("casillas_columna: in the engine, optional in class E only, and only avisos_de has it", {
  expect_true("casillas_columna" %in% HD_PIEZAS_MOTOR)
  expect_true("casillas_columna" %in% hd_clases$E$opcional)
  for (cl in setdiff(names(hd_clases), "E")) expect_false("casillas_columna" %in% c(hd_clases[[cl]]$siempre, hd_clases[[cl]]$opcional))
  for (tabla in setdiff(names(hd_registro), "avisos_de")) expect_false("casillas_columna" %in% hd_piezas_de(tabla), info = tabla)
  p <- hd_piezas_de("avisos_de")
  expect_true(all(c("casillas_columna", "expandir", "buscar", "encabezado_fijo") %in% p))
  expect_false(any(c("copiar_celda", "sel_fila", "casillas") %in% p))   # a click on a box never selects nor copies
})

test_that("col_casilla: empty text, sorted 0/1, Sí/No to copy, the logical value for the browser; NA is FALSE", {
  v <- col_casilla("req")$valores(c(TRUE, FALSE, NA))
  expect_identical(v$mostrar, c("", "", ""))
  expect_identical(v$orden, c(1L, 0L, 0L))
  expect_identical(v$copiar, c(tr("tablas.casillas.si"), tr("tablas.casillas.no"), tr("tablas.casillas.no")))
  expect_identical(v$valor, c(TRUE, FALSE, FALSE))
  m <- hd_columnas_meta(list(col_casilla("req", "Requisiciones")))[[1]]
  expect_identical(m$tipo, "casilla"); expect_identical(m$alinear, "centro")
})

D <- data.frame(id = c("u1", "u2", "u3"), nombre = c("Ana", "Beto", "Caro"),
                req = c(TRUE, FALSE, FALSE), bancos = c(FALSE, FALSE, TRUE),
                .bloqueada = c(FALSE, FALSE, TRUE), stringsAsFactors = FALSE)
COLS <- list(col_texto("nombre"), col_casilla("req", "Requisiciones"), col_casilla("bancos", "Bancos"))

test_that("the engine asks for the piece", {
  expect_error(hd_columnas_piezas(COLS, "importar_txt", character()), "col_casilla\\(\\) but the table doesn't have the 'casillas_columna' piece")
  expect_silent(hd_columnas_piezas(COLS, "avisos_de", hd_piezas_de("avisos_de")))
})

test_that("hd_casillas: the data's cells, then the person's valid changes (row exists, not locked, a casilla field)", {
  base <- hd_casillas(NULL, D, COLS, "id")
  expect_identical(base, data.frame(id = c("u1", "u2", "u3"), req = c(TRUE, FALSE, FALSE), bancos = c(FALSE, FALSE, TRUE),
                                    stringsAsFactors = FALSE))
  o <- list(ids = list("u1", "u2", "zz", "u3", "u2", "u2"), campos = list("req", "bancos", "req", "req", "nombre", "bancos"),
            valores = list(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
  r <- hd_casillas(o, D, COLS, "id")
  expect_identical(r$req, c(FALSE, FALSE, FALSE))      # u1 off; zz unknown; u3 locked; "nombre" is not a box
  expect_identical(r$bancos, c(FALSE, FALSE, TRUE))     # u2|bancos came twice: the last one (FALSE) wins
  expect_identical(hd_casillas(list(ids = list("u1"), campos = list(), valores = list(TRUE)), D, COLS, "id"), base)
  sin <- hd_casillas(NULL, D[, c("id", "nombre", "req")], COLS, "id")   # a field the data lacks: all FALSE
  expect_identical(sin$bancos, c(FALSE, FALSE, FALSE))
  vacia <- hd_casillas(NULL, NULL, COLS, "id")
  expect_identical(names(vacia), c("id", "req", "bancos")); expect_identical(nrow(vacia), 0L)
})

test_that("t$casillas() follows the browser, through the server's check", {
  testServer(function(input, output, session) {
    session$userData$t <- hd_table_server("t", tabla = "avisos_de", datos = reactive(D), columnas = COLS)
  }, {
    t <- session$userData$t
    expect_identical(t$casillas()$req, c(TRUE, FALSE, FALSE))
    session$setInputs(`t-casillas` = list(ids = list("u2", "u3"), campos = list("req", "req"), valores = list(TRUE, TRUE)))
    expect_identical(t$casillas()$req, c(TRUE, TRUE, FALSE))   # u3 is locked
  })
})
