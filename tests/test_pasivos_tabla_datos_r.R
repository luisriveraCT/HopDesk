# T17b step 3, first part: what Pasivos · Tabla will hand the table engine,
# from the same wide pivot it paints today -- pasivos_tabla_datos(),
# pasivos_tabla_columnas() and pasivos_tabla_ids_celda(), plus the pivot now
# counting what each cell adds up (cell_n, cell_ids; Luis P4). Pure, no
# Shiny, with tests/helpers/pasivos_datos_prueba.R.
# Run from the repo root: Rscript tests/test_pasivos_tabla_datos_r.R
suppressPackageStartupMessages({ library(testthat); library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", "strip_accents")
cargar("R/persistence.R", "confirmaciones_vigentes")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/pasivos_table_pivot.R")) source(f)
source("tests/helpers/pasivos_datos_prueba.R")

D <- pasivos_datos_prueba()
HOY <- D$hoy
LIABS <- D$liabilities[D$liabilities$empresa %in% D$visibles, ]
FILTROS <- list(empresa = character(0), categorias = c("regular", "financiero", "tarjeta"), subcategorias = character(0),
                currency = "All", search = "", granularity = "day", provision_type = "todas")
LONG <- suppressWarnings(pasivos_table_build_long(D$provisions[D$provisions$empresa %in% D$visibles, ], D$manual_inv,
                                                  confirmaciones_vigentes(D$bancos_confirmados), LIABS, FILTROS))
ancho <- function(g = "day") pasivos_table_pivot_wide(LONG, g, HOY - 7L, HOY + 53L)
lbl <- function(dias) as.character(HOY + dias)
campo <- function(dias) .pasivos_campo(lbl(dias))
W <- ancho(); X <- pasivos_tabla_datos(W, LIABS)
fila <- function(id) X[X$id == id, , drop = FALSE]

test_that("the pivot counts what each cell adds up and keeps all its provisions (one day, one week)", {
  c20 <- W$cells[["lib1"]][[lbl(20)]]
  expect_identical(c(c20$cell_n, c20$cell_ids), c("2", "p1e,p1h"))
  expect_identical(W$cells[["lib1"]][[lbl(5)]]$cell_n, 1L)
  s <- ancho("week")$cells[["lib3"]][[format(D$lunes, "%G-W%V")]]
  expect_identical(s$cell_n, 2L)
  expect_identical(sort(strsplit(s$cell_ids, ",")[[1]]), c("p3a", "p3b"))
})
test_that("rows: one per liability, orphan or fee, in the pivot's order, with what the engine needs", {
  expect_identical(X$id, c("orphan_p0", "lib1", "lib2", "lib2_fee_1", "lib2_fee_2", "lib3", "lib'4"))
  expect_false("lib6" %in% X$id)   # archived (estado "deleted"): out, as in the Calendario (T17b (a))
  expect_identical(fila("lib2_fee_1")[, c("Pasivo", "Parte", ".nivel", ".semaforo", ".acciones")],  # Parte: its liability
                   data.frame(Pasivo = "Comisión por apertura", Parte = "Crédito Banorte", .nivel = 1L, .semaforo = "", .acciones = "",
                              row.names = 4L, check.names = FALSE))
  expect_identical(fila("lib1")$.semaforo, "vencida")
  expect_identical(fila("orphan_p0")$.semaforo, "pronto")
  expect_identical(X$.acciones[X$id %in% c("lib1", "orphan_p0", "lib'4")], c("", "editar", "editar"))
})
test_that("cells: the provision's id, mark, button, count, hand change and tone, by what the cell is", {
  cel <- function(id, dias, sufijo) fila(id)[[paste0(campo(dias), sufijo)]]
  expect_identical(c(cel("lib1", 5, ".id"), cel("lib1", 5, ".marca"), cel("lib1", 5, ".boton"), cel("lib1", 5, ".tono")),
                   c("p1d", "", "provision", "morado"))
  expect_identical(c(cel("lib1", -3, ".boton"), cel("lib1", -3, ".tono")), c("provision", "aviso"))
  expect_identical(c(cel("lib2", 3, ".marca"), cel("lib2", 3, ".boton"), cel("lib2", 3, ".tono")), c("pendiente", "comprobante", "neutro"))
  expect_identical(c(cel("lib2", -1, ".marca"), cel("lib2", -1, ".tono")), c("vencida", "peligro"))
  expect_true(is.na(fila("lib2")[[campo(-2)]]))   # paid (a current confirmation): out (T17b (b))
  expect_true(cel("lib1", 30, ".mano")); expect_false(cel("lib1", 5, ".mano"))
  expect_identical(c(cel("lib1", 2, ".id"), cel("lib1", 2, ".boton")), c("", ""))
  expect_true(is.na(fila("lib1")[[campo(2)]]))
  expect_identical(fila("lib'4")[[paste0(campo(3), ".id")]], "p4")
})
test_that("a cell that adds up several: agg:<row>\\t<label>, its count, no button; its ids come back", {
  id20 <- fila("lib1")[[paste0(campo(20), ".id")]]
  expect_identical(id20, paste0("agg:lib1\t", lbl(20)))
  expect_identical(fila("lib1")[[paste0(campo(20), ".n")]], 2L)
  expect_identical(fila("lib1")[[paste0(campo(20), ".boton")]], "")
  expect_identical(fila("lib1")[[campo(20)]], 1950)
  expect_identical(pasivos_tabla_ids_celda(W, id20), c("p1e", "p1h"))
  expect_identical(pasivos_tabla_ids_celda(W, "p1d"), "p1d")
  expect_identical(pasivos_tabla_ids_celda(W, "agg:nada\tx"), character(0))
})
test_that("columns: the liability fixed at the left; one money column per period, today's shaded; the engine accepts them", {
  cols <- pasivos_tabla_columnas(W, HOY, "day")
  expect_identical(vapply(cols, `[[`, character(1), "campo")[1:3], c("Pasivo", "Parte", "Empresa"))
  expect_true(cols[[1]]$fija)
  expect_identical(length(cols), 3L + length(W$date_cols))
  hoy <- Filter(function(c) isTRUE(c$destacar), cols)
  expect_identical(vapply(hoy, `[[`, character(1), "campo"), campo(0))
  expect_true(all(vapply(cols[-(1:3)], function(c) isTRUE(c$accion), logical(1))))
  expect_silent(hd_columnas_piezas(cols, "pasivos_tabla", hd_piezas_de("pasivos_tabla", c("accion_celda", "tono_celda", "sangria"))))
  f <- hd_filas(X, cols)
  expect_identical(length(f$ids), nrow(X))
  expect_identical(unclass(f$nivel), X$.nivel)
})

test_that("P4: the list behind a cell of several -- one row per provision, its amount and state, ✎ and ⚡ or ↩", {
  ids <- pasivos_tabla_ids_celda(W, paste0("agg:lib1\t", lbl(20)))
  l <- pasivos_tabla_lista_celda(ids, D$provisions, LIABS, D$manual_inv, HOY)
  expect_identical(l$id, c("p1e", "p1h"))
  expect_identical(l$Pasivo, c("Renta oficina", "Renta oficina"))
  expect_identical(l$Importe, c(1450, 500))          # the override; the converted one's item
  expect_identical(l$Fecha, rep(HOY + 20L, 2))
  expect_identical(l$Estado, c(tr("tablas.pasivos.estado.provision"), tr("tablas.pasivos.estado.pendiente")))
  expect_identical(l$.acciones, c("editar,provision", "editar,comprobante"))
  expect_identical(l$.provision, c(TRUE, FALSE))
  v <- pasivos_tabla_lista_celda(c("p2v", "nada"), D$provisions, LIABS, D$manual_inv, HOY)
  expect_identical(c(v$id, v$Estado), c("p2v", tr("tablas.pasivos.estado.vencido")))
  expect_identical(nrow(pasivos_tabla_lista_celda(character(0), D$provisions)), 0L)
  expect_silent(hd_columnas_piezas(list(col_fecha("Fecha"), col_texto("Pasivo"), col_dinero("Importe", sumar = FALSE),
                                        col_texto("Estado")), "provisiones_celda", hd_piezas_de("provisiones_celda", "acc_fila")))
  expect_true(all(c("editar", "provision", "comprobante") %in% hd_acciones_fila_validar(c("editar", "provision", "comprobante"))))
})
