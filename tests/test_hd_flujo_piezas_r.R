# hd_table engine, T17c step 1, R side: the pieces the Flujo de caja needs
# (class F) -- sumas_columnas, filas_resumen (.resumen) and edicion_celda's
# additions (editable$expresion, editable$filas, and "if it sums, it's the
# only one"). Nothing changes for a table that doesn't ask: the byte for
# byte snapshots of test_hd_matriz_r, test_hd_opcion_ayuda_r and
# test_hd_edicion_celda_r run on this same code.
# Run from the repo root: Rscript tests/test_hd_flujo_piezas_r.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)

# A small Flujo: two periods, a Día 0 column typed only on the position row.
D <- data.frame(id = c("a1", "a2", "p1", "neto", "acum"), Contraparte = c("ALFA", "BETA", "UNO", "FLUJO NETO", "POS. ACUMULADA"),
                Moneda = "MXN", Tipo = c("Cobros", "Cobros", "Pagos", "", ""),
                Dia0 = c(0, 0, 0, 0, 1500), c1 = c(100, 50, 30, 120, 1620), c2 = c(0, 10, 5, 5, 1625),
                .resumen = c("", "", "", "neto", "acumulada"), .dia0 = c(FALSE, FALSE, FALSE, FALSE, TRUE),
                stringsAsFactors = FALSE, check.names = FALSE)
COLS <- list(col_texto("Contraparte", fija = TRUE),
             col_dinero("Dia0", sumar = FALSE, editable = list(expresion = TRUE, filas = ".dia0", min = -1e15)),
             col_dinero("c1", formato = "compacto", cero = "vacio"), col_dinero("c2", formato = "compacto", cero = "vacio"))
P <- c("sumas_columnas", "filas_resumen", "edicion_celda", "dinero_compacto")

test_that("the pieces exist in the engine and are optional in class F only (edicion_celda too, for Día 0)", {
  expect_true(all(c("sumas_columnas", "filas_resumen") %in% HD_PIEZAS_MOTOR))
  expect_true(all(c("sumas_columnas", "filas_resumen", "edicion_celda") %in% hd_clases$F$opcional))
  for (cl in setdiff(names(hd_clases), "F")) expect_false(any(c("sumas_columnas", "filas_resumen") %in% hd_clases[[cl]]$opcional))
  for (tabla in names(hd_registro)) expect_false(any(c("sumas_columnas", "filas_resumen") %in% hd_piezas_de(tabla)), info = tabla)
})

# ── edicion_celda: expresion, filas, and the rule ───────────────────────────
test_that("editable takes expresion = TRUE and filas = <field>; bad values stop", {
  e <- col_dinero("Dia0", sumar = FALSE, editable = list(expresion = TRUE, filas = ".dia0"))$editable
  expect_identical(c(e$expresion, e$filas), c("TRUE", ".dia0"))
  expect_null(col_dinero("x", editable = list(max = 1))$editable$expresion)
  expect_error(col_dinero("x", editable = list(expresion = "si")), "expresion must be TRUE or FALSE")
  expect_error(col_dinero("x", editable = list(filas = 3)), "filas must be the name of a logical field")
  expect_error(hd_filas(D, list(col_dinero("Dia0", editable = list(filas = "nada")))), "editable\\$filas names 'nada'")
})
test_that("only then the metadata says expresion, and the rows say which can be typed in", {
  m <- hd_columnas_meta(COLS)[[2]]
  expect_true(m$editable$expresion)
  expect_false("expresion" %in% names(hd_columnas_meta(list(col_dinero("x", editable = list(max = 1))))[[1]]$editable))
  expect_identical(unclass(hd_filas(D, COLS)$celdas[[2]]$edf), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_false("edf" %in% names(hd_filas(D, list(col_dinero("Dia0", editable = list(max = 1))))$celdas[[1]]))
})
test_that("if the editable column sums it's the only one that does; one that doesn't can sit among summed ones", {
  expect_identical(hd_columna_editable(COLS, "flujo_caja", P)$campo, "Dia0")
  dos <- list(col_dinero("c1"), col_dinero("Dia0", editable = list(max = 1)))
  expect_error(hd_columna_editable(dos, "flujo_caja", P), "must be the only one with sumar = TRUE")
})
test_that("the server takes what was typed only on rows that allow it (a negative with min below 0)", {
  col <- hd_columna_editable(COLS, "flujo_caja", P)
  e <- hd_ediciones(list(ids = list("acum", "a1"), valores = list(-250, 9)), D, col, "id")
  expect_identical(e, data.frame(id = "acum", valor = -250, valido = TRUE, stringsAsFactors = FALSE))
})

# ── filas_resumen ───────────────────────────────────────────────────────────
test_that(".resumen travels only when the data has it; anything but a-z and _ is a data row", {
  f <- hd_filas(D, COLS)
  expect_identical(unclass(f$resumen), c("", "", "", "neto", "acumulada"))
  d <- D; d$.resumen[1] <- "<b>"
  expect_identical(unclass(hd_filas(d, COLS)$resumen)[1], "")
  expect_null(hd_filas(D[, setdiff(names(D), ".resumen")], COLS)$resumen)
})

# ── sumas_columnas ──────────────────────────────────────────────────────────
test_that("a compact column says so in its metadata (its group sums read the same); a plain one doesn't", {
  m <- hd_columnas_meta(COLS)
  expect_identical(c(m[[3]]$formato, m[[3]]$cero), c("compacto", "vacio"))
  expect_false(any(c("formato", "cero") %in% names(hd_columnas_meta(list(col_dinero("x")))[[1]])))
})
test_that("sumas_columnas can't come with a single total: no fichas de monto", {
  creada <- FALSE
  testServer(function(input, output, session) {
    hd_table_server("t", tabla = "flujo_caja", datos = reactive(D), columnas = COLS, piezas = P,
                    secciones = "Moneda", grupo = "Tipo")
    creada <<- TRUE
  }, session$flushReact())
  expect_true(creada)
  expect_error(hd_table_server("t", tabla = "flujo_caja", datos = reactive(D), columnas = COLS, piezas = P,
                               fichas = list(campo = "Tipo", orden = c("Cobros", "Pagos"), mostrar = "monto")),
               "'sumas_columnas' adds up every summed column")
})
test_that("the dictionary has the expression's message in both languages", {
  for (lang in c("es", "en"))
    expect_true("tablas.edicion.no_expresion" %in% names(jsonlite::fromJSON(file.path("www", "i18n", paste0(lang, ".json")))))
})

# ── Step 2 additions (T17c): the Total's whole pesos, summary rows, the group's name ──
test_that("formato = \"entero\": whole pesos, halves up; the bubble and Copiar keep the cents; only then in the metadata", {
  v <- col_dinero("Total", formato = "entero")$valores(c(5000120.5, -463208.5, 802000, 0.4, -0.4, NA))
  expect_identical(v$mostrar, c("$5,000,121", "-$463,209", "$802,000", "$0", "$0", ""))
  expect_identical(v$completo[1], "$5,000,120.50")
  expect_identical(v$copiar[1], "5000120.5")
  expect_identical(hd_columnas_meta(list(col_dinero("x", formato = "entero")))[[1]]$formato, "entero")
  expect_error(col_dinero("x", formato = "redondo"), "completo\\\", \\\"compacto\\\" or \\\"entero")
})
test_that("with .resumen: a summary row keeps its 0 in a compact cero-vacio column, and only summary rows take the sign's tone", {
  cols <- list(col_texto("Contraparte"), col_dinero("c1", formato = "compacto", cero = "vacio", tono = "signo"))
  d <- data.frame(id = c("a", "n"), Contraparte = c("A", "NETO"), c1 = c(0, 0), .resumen = c("", "neto"),
                  stringsAsFactors = FALSE)
  f <- hd_filas(d, cols)$celdas[[2]]
  expect_identical(unclass(f$mostrar), c("", "$0"))
  expect_identical(unclass(f$tono), c("", "neutro"))
  # Without .resumen nothing changes: a 0 is empty and every row has its tone.
  f0 <- hd_filas(d[, c("id", "Contraparte", "c1")], cols)$celdas[[2]]
  expect_identical(unclass(f0$mostrar), c("", ""))
  expect_identical(unclass(f0$tono), c("neutro", "neutro"))
})
test_that("the group's row says its name (the last grupo field) only with sumas_columnas", {
  d <- data.frame(id = c("a", "b"), Moneda = "MXN", Tipo = c("Cobros", "Cobros"), Contraparte = c("A", "B"),
                  stringsAsFactors = FALSE)
  cols <- list(col_texto("Contraparte"))
  expect_identical(unclass(hd_grupo_payload(c("Moneda", "Tipo"), d, cols, titulos = TRUE)$titulos), c("Cobros", "Cobros"))
  expect_false("titulos" %in% names(hd_grupo_payload(c("Moneda", "Tipo"), d, cols)))
})
