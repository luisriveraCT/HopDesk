# hd_table engine, two small pieces Luis asked for (T18 §6), R side:
# - filtros[[k]]$ayuda: a title per option in a filter's menu;
# - opcion_fila: col_opcion(), one choice per row, t$opciones() /
#   t$poner_opcion(), and its tie to edicion_celda (al_escribir).
# A table without them sends byte for byte what it sent before (against
# tests/fixtures/hd_opcion_ayuda_antes.json, built with the code of 1e35d41).
# Run from the repo root: Rscript tests/test_hd_opcion_ayuda_r.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
source("tests/helpers/hd_matriz_muestra.R")

# ── Nothing changes for a table that doesn't ask ────────────────────────────
test_that("without ayuda or col_opcion, columns, rows and filters are byte for byte what they were", {
  m <- hd_matriz_muestra()
  f <- hd_filtros_validar(list(list(campo = "Estado"), list(campo = "Moneda", titulo = "Divisa")))
  ahora <- as.character(jsonlite::toJSON(list(meta = hd_columnas_meta(m$columnas), filas = hd_filas(m$datos, m$columnas),
                                              filtros = hd_filtros_payload(f, m$datos, m$columnas)),
                                         auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
  antes <- readLines("tests/fixtures/hd_opcion_ayuda_antes.json", warn = FALSE, encoding = "UTF-8")
  expect_identical(enc2utf8(ahora), enc2utf8(paste(antes, collapse = "\n")))
})
test_that("opcion_fila is optional in class E only, and no table has it unless it asks", {
  expect_true("opcion_fila" %in% hd_clases$E$opcional)
  expect_true("opcion_fila" %in% HD_PIEZAS_MOTOR)
  for (cl in setdiff(names(hd_clases), "E")) expect_false("opcion_fila" %in% c(hd_clases[[cl]]$siempre, hd_clases[[cl]]$opcional))
  for (tabla in names(hd_registro)) expect_false("opcion_fila" %in% hd_piezas_de(tabla), info = tabla)
})

# ── ayuda ───────────────────────────────────────────────────────────────────
D <- data.frame(id = c("a", "b", "c", "d"), cat = c("regular", "tarjeta", "regular", NA), stringsAsFactors = FALSE)
COLS <- list(col_texto("cat"))
test_that("ayuda: c(<value> = <dictionary key>); bad shapes and missing keys stop", {
  expect_error(hd_filtros_validar(list(list(campo = "cat", ayuda = "tablas.vacio"))), "`ayuda` must be c(", fixed = TRUE)
  expect_error(hd_filtros_validar(list(list(campo = "cat", ayuda = c(regular = 1)))), "`ayuda` must be c(", fixed = TRUE)
  expect_error(hd_filtros_validar(list(list(campo = "cat", ayuda = c(regular = "no.existe.esta")))),
               "ayuda key\\(s\\) not in the dictionary: no.existe.esta")
  expect_error(hd_filtros_validar(list(list(campo = "cat", ayuda = c(a = "tablas.vacio", a = "tablas.vacio")))), "`ayuda` must be c(", fixed = TRUE)
})
test_that("ayuda goes only to the options that have one, in the active language", {
  f <- hd_filtros_validar(list(list(campo = "cat", ayuda = c(regular = "tablas.vacio", otra = "tablas.vacio"))))
  op <- hd_filtros_payload(f, D, COLS)[[1]]$opciones
  expect_identical(vapply(op, `[[`, character(1), "valor"), c("regular", "tarjeta", ""))
  expect_identical(op[[1]]$ayuda, tr("tablas.vacio"))
  expect_null(op[[2]]$ayuda)
  expect_null(op[[3]]$ayuda)
})

# ── col_opcion ──────────────────────────────────────────────────────────────
OPC <- c(aplicar = "Nuevo", mantener = "Mantener", personalizar = "Otro")
conf <- data.frame(id = c("p1", "p2", "p3"), decision = c("mantener", "mantener", "aplicar"),
                   antes = c(100, 200, 300), nuevo = c(150, 250, 350), importe = c(0, 0, 0),
                   .bloqueada = c(FALSE, FALSE, TRUE), stringsAsFactors = FALSE)
cols_conf <- function(al = "personalizar") list(
  col_dinero("antes", sumar = FALSE), col_dinero("nuevo", sumar = FALSE),
  col_opcion("decision", "Decisión", opciones = OPC, al_escribir = al, burbuja = c(aplicar = "nuevo")),
  col_dinero("importe", editable = list(min = 0)))
P <- c("opcion_fila", "edicion_celda")

test_that("col_opcion: 2 to 4 named options; al_escribir and burbuja among them", {
  expect_error(col_opcion("d", opciones = c(a = "A")), "2 to 4")
  expect_error(col_opcion("d", opciones = c(a = "A", b = "B", c = "C", d = "D", e = "E")), "2 to 4")
  expect_error(col_opcion("d", opciones = c("A", "B")), "2 to 4")
  expect_error(col_opcion("d", opciones = OPC, al_escribir = "otro"), "`al_escribir` must be one of")
  expect_error(col_opcion("d", opciones = OPC, burbuja = c(nada = "nuevo")), "`burbuja` must be")
})
test_that("its cells: the option's text, sorted in the options' order, the raw value for the browser", {
  v <- col_opcion("decision", opciones = OPC)$valores(c("mantener", "aplicar"))
  expect_identical(v, list(mostrar = c("Mantener", "Nuevo"), orden = c(2L, 1L), copiar = c("Mantener", "Nuevo"),
                           valor = c("mantener", "aplicar")))
  expect_error(col_opcion("decision", opciones = OPC)$valores(c("mantener", "x", NA)),
               "value\\(s\\) x, NA not among its opciones")
})
test_that("its metadata: the options in order, al_escribir and the burbuja's field", {
  m <- hd_columnas_meta(cols_conf())[[3]]
  expect_identical(m$opciones, list(list(valor = "aplicar", texto = "Nuevo", burbuja = "nuevo"),
                                    list(valor = "mantener", texto = "Mantener"), list(valor = "personalizar", texto = "Otro")))
  expect_identical(m$al_escribir, "personalizar")
})
test_that("the engine checks it: the piece, only one, al_escribir needs an editable column, burbuja a column", {
  expect_error(hd_columna_opcion(cols_conf(), "conflictos_pasivo", "edicion_celda"), "needs the 'opcion_fila' piece")
  expect_error(hd_columna_opcion(c(cols_conf(), list(col_opcion("otra", opciones = OPC))), "conflictos_pasivo", P),
               "only one col_opcion")
  expect_error(hd_columna_opcion(list(col_opcion("decision", opciones = OPC, al_escribir = "personalizar")),
                                 "conflictos_pasivo", "opcion_fila"), "al_escribir needs an editable column")
  expect_error(hd_columna_opcion(list(col_opcion("decision", opciones = OPC, burbuja = c(aplicar = "zz"))),
                                 "conflictos_pasivo", "opcion_fila"), "burbuja names a field that isn't a column: zz")
  expect_identical(hd_columna_opcion(cols_conf(), "conflictos_pasivo", P)$campo, "decision")
  expect_error(hd_table_server("t", tabla = "conflictos_pasivo", datos = reactive(conf), columnas = cols_conf(),
                               piezas = "edicion_celda"),
               "needs the 'opcion_fila' piece")
})
test_that("hd_opciones: every row, the data's choice unless the person's is valid (row exists, not locked, an option)", {
  col <- hd_columna_opcion(cols_conf(), "conflictos_pasivo", P)
  expect_identical(hd_opciones(NULL, conf, col, "id"),
                   data.frame(id = c("p1", "p2", "p3"), opcion = c("mantener", "mantener", "aplicar"), stringsAsFactors = FALSE))
  o <- list(ids = list("p1", "zz", "p2", "p3", "p1"), valores = list("aplicar", "aplicar", "otra", "mantener", "personalizar"))
  expect_identical(hd_opciones(o, conf, col, "id")$opcion, c("aplicar", "mantener", "aplicar"))
  expect_identical(hd_opciones(list(ids = list("p1"), valores = list()), conf, col, "id")$opcion,
                   c("mantener", "mantener", "aplicar"))
})

test_that("t$opciones() follows the browser; an amount typed counts only on the al_escribir option", {
  testServer(function(input, output, session) {
    t <- hd_table_server("t", tabla = "conflictos_pasivo", datos = reactive(conf), columnas = cols_conf(), piezas = P)
    session$userData$t <- t
  }, {
    t <- session$userData$t
    expect_identical(t$opciones()$opcion, c("mantener", "mantener", "aplicar"))
    session$setInputs(`t-opciones` = list(ids = list("p1", "p2"), valores = list("personalizar", "aplicar")),
                      `t-ediciones` = list(ids = list("p1", "p2"), valores = list(120, 130)))
    expect_identical(t$opciones()$opcion, c("personalizar", "aplicar", "aplicar"))
    e <- t$ediciones()
    expect_identical(e$valido[match(c("p1", "p2"), e$id)], c(TRUE, FALSE))
    expect_error(t$poner_opcion("nada"), "takes one of the col_opcion\\(\\) options")
    expect_silent(t$poner_opcion("mantener", ids = c("p1", "p2")))
  })
})
test_that("without al_escribir, what was typed counts whatever the choice", {
  testServer(function(input, output, session) {
    session$userData$t <- hd_table_server("t", tabla = "conflictos_pasivo", datos = reactive(conf),
                                          columnas = cols_conf(al = NULL), piezas = P)
  }, {
    session$setInputs(`t-opciones` = list(ids = list("p1"), valores = list("aplicar")),
                      `t-ediciones` = list(ids = list("p1"), valores = list(120)))
    expect_true(session$userData$t$ediciones()$valido)
  })
})
test_that("the Excel has each row's choice as on screen", {
  skip_if_not_installed("openxlsx")
  archivo <- tempfile(fileext = ".xlsx")
  on.exit(unlink(archivo), add = TRUE)
  hd_excel_escribir(conf, cols_conf(), c("p1", "p2", "p3"), "id", archivo,
                    opciones = data.frame(id = c("p1", "p2", "p3"), opcion = c("personalizar", "mantener", "aplicar")))
  expect_identical(openxlsx::read.xlsx(archivo)$Decisión, c("Otro", "Mantener", "Nuevo"))
})
