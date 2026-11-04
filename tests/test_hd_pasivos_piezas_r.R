# hd_table engine, T17b step 2, R side: the three pieces Pasivos · Tabla
# needs (class F) -- accion_celda (col_dinero(accion = TRUE) with its
# <campo>.id / .marca / .boton / .n, hd_table_server(accion_celda =
# list(clic)), t$accion()'s celda and columna), semaforo (.semaforo) and
# sangria (.nivel). Nothing changes for a table that doesn't ask: the byte
# for byte snapshots are in test_hd_matriz_r and test_hd_opcion_ayuda_r,
# which run on this same code.
# Run from the repo root: Rscript tests/test_hd_pasivos_piezas_r.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)

D <- data.frame(id = c("r1", "r2", "r3", "r4"), Nombre = c("Renta", "Comisión", "Crédito", "Luz"),
                d1 = c(1000, 250, 700, 0), `d1.id` = c("p1", "p2", "", "p4"),
                `d1.marca` = c("pendiente", "otra", NA, "confirmada"), `d1.boton` = c("provision", "comprobante", "x", ""),
                `d1.n` = c(1, 3, NA, -2), .semaforo = c("vencida", "", "pronto", "roja"), .nivel = c(0, 1, NA, 2),
                check.names = FALSE, stringsAsFactors = FALSE)
COLS <- list(col_texto("Nombre", fija = TRUE), col_dinero("d1", sumar = FALSE, accion = TRUE))

test_that("the three pieces exist in the engine and are only F's: semaforo always, accion_celda and sangria optional", {
  expect_true(all(c("semaforo", "accion_celda", "sangria") %in% HD_PIEZAS_MOTOR))
  expect_true(all(c("accion_celda", "sangria") %in% hd_clases$F$opcional))
  for (tabla in names(hd_registro)) {
    if (hd_registro[[tabla]]$clase == "F") next
    expect_false(any(c("accion_celda", "sangria", "semaforo") %in% hd_piezas_motor(tabla, hd_piezas_de(tabla))), info = tabla)
  }
})

# ── accion_celda ────────────────────────────────────────────────────────────
test_that("col_dinero(accion): TRUE or FALSE; only an accion column says so in its metadata", {
  expect_error(col_dinero("d1", accion = "si"), "`accion` must be TRUE or FALSE")
  m <- hd_columnas_meta(list(col_dinero("d1", accion = TRUE), col_dinero("d2")))
  expect_true(m[[1]]$accion)
  expect_false("accion" %in% names(m[[2]]))
})
test_that("each cell: its id, mark, button and count from <campo>.*, only the closed lists", {
  c1 <- hd_filas(D, COLS)$celdas[[2]]
  expect_identical(unclass(c1$cid), c("p1", "p2", "", "p4"))
  expect_identical(unclass(c1$marca), c("pendiente", "", "", "confirmada"))
  expect_identical(unclass(c1$boton), c("provision", "comprobante", "", ""))
  expect_identical(unclass(c1$n), c(1L, 3L, 0L, 0L))
})
test_that("a missing companion field means none (no id: the cell doesn't respond)", {
  c1 <- hd_filas(D[, c("id", "Nombre", "d1")], COLS)$celdas[[2]]
  expect_identical(unclass(c1$cid), rep("", 4)); expect_identical(unclass(c1$boton), rep("", 4))
  expect_identical(unclass(c1$n), rep(0L, 4))
})
test_that("a column without accion carries none of it", {
  c1 <- hd_filas(D, list(col_dinero("d1", sumar = FALSE)))$celdas[[1]]
  expect_false(any(c("cid", "marca", "boton", "n") %in% names(c1)))
})
test_that("accion = TRUE needs the piece, never dropped silently", {
  expect_error(hd_columnas_piezas(COLS, "pasivos_tabla", "columna_fija"), "'accion_celda' piece")
  expect_silent(hd_columnas_piezas(COLS, "pasivos_tabla", c("columna_fija", "accion_celda")))
})
test_that("accion_celda = list(clic): an action name; it needs the piece; marks and buttons go in the language", {
  m <- hd_accion_celda_meta(list(clic = "editar_celda"), "pasivos_tabla", "accion_celda")
  expect_identical(m$clic, "editar_celda")
  expect_identical(names(m$marcas), c("pendiente", "vencida", "confirmada"))
  expect_identical(m$botones$provision, list(icono = "⚡", titulo = tr("tablas.accion.provision")))
  expect_identical(m$botones$comprobante$titulo, tr("tablas.accion.comprobante"))
  expect_null(hd_accion_celda_meta(NULL, "flujo_caja", character()))
  expect_null(hd_accion_celda_meta(list(clic = "a")$x, "flujo_caja", "semaforo"))
  expect_identical(hd_accion_celda_meta(NULL, "pasivos_tabla", "accion_celda")$clic, NULL)
  expect_error(hd_accion_celda_meta(list(clic = "Editar"), "pasivos_tabla", "accion_celda"), "must be list\\(clic")
  expect_error(hd_accion_celda_meta(list(clic = "a", otra = 1), "pasivos_tabla", "accion_celda"), "must be list\\(clic")
  expect_error(hd_accion_celda_meta(list(clic = "a"), "flujo_caja", character()), "needs the 'accion_celda' piece")
  expect_error(hd_table_server("t", tabla = "pasivos_tabla", datos = reactive(D), columnas = COLS,
                               accion_celda = list(clic = "editar_celda")), "'accion_celda' piece")
})
test_that("t$accion() from a cell carries its id and column; a hidden action never reaches the module", {
  testServer(function(input, output, session) {
    session$userData$t <- hd_table_server("t", tabla = "pasivos_tabla", datos = reactive(D), columnas = COLS,
                                          piezas = "accion_celda", accion_celda = list(clic = "editar_celda"),
                                          acciones_ocultas = reactive("comprobante"))
  }, {
    t <- session$userData$t
    session$setInputs(`t-accion` = list(accion = "provision", ids = list("r1"), nonce = 1, de = "celda",
                                        celda = "p1", columna = "d1"))
    a <- t$accion()
    expect_identical(a[c("accion", "ids", "de", "celda", "columna")],
                     list(accion = "provision", ids = "r1", de = "celda", celda = "p1", columna = "d1"))
    session$setInputs(`t-accion` = list(accion = "comprobante", ids = list("r2"), nonce = 2, de = "celda",
                                        celda = "p2", columna = "d1"))
    expect_null(t$accion())
    session$setInputs(`t-accion` = list(accion = "editar", ids = list("r1"), nonce = 3))
    expect_null(t$accion()$celda)
  })
})

# ── semaforo, sangria ───────────────────────────────────────────────────────
test_that(".semaforo: vencida / muy_pronto / pronto, anything else none; absent, not sent", {
  expect_identical(unclass(hd_filas(D, COLS)$semaforo), c("vencida", "", "pronto", ""))
  expect_null(hd_filas(D[, setdiff(names(D), ".semaforo")], COLS)$semaforo)
})
test_that(".nivel: 1 = a child of the row above, anything else 0; absent, not sent", {
  expect_identical(unclass(hd_filas(D, COLS)$nivel), c(0L, 1L, 0L, 1L))
  expect_null(hd_filas(D[, setdiff(names(D), ".nivel")], COLS)$nivel)
})
test_that("the dictionary has the new keys in both languages, the plural with .uno/.otros", {
  for (lang in c("es", "en")) {
    d <- jsonlite::fromJSON(file.path("www", "i18n", paste0(lang, ".json")))
    expect_true(all(c("tablas.accion.comprobante", "tablas.celda.pendiente", "tablas.celda.vencida",
                      "tablas.celda.confirmada", "tablas.celda.varias.uno", "tablas.celda.varias.otros") %in% names(d)),
                info = lang)
  }
})
