# hd_table engine, R side (no browser): pieces, columns, row payload.
# Run from the repo root: Rscript tests/test_hd_table_r.R
suppressPackageStartupMessages(library(testthat))
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
source("R/i18n.R")
source("R/hd_registro.R")
source("R/hd_piezas.R")
source("R/hd_columnas.R")

# ── Pieces ──────────────────────────────────────────────────────────────────
test_that("every registered table resolves to valid pieces: siempre - quitar + agregar", {
  for (tabla in names(hd_registro)) {
    def <- hd_registro[[tabla]]
    cl  <- hd_clases[[def$clase]]
    p   <- hd_piezas_de(tabla)
    expect_true(all(p %in% HD_PIEZAS_VALIDAS), info = tabla)
    expect_setequal(p, union(setdiff(cl$siempre, def$quitar), def$agregar))
    expect_false(any(def$quitar %in% p), info = tabla)
    expect_false(anyDuplicated(p) > 0, info = tabla)
  }
})

test_that("a caller can turn on optional pieces of the class", {
  expect_true("densidad" %in% hd_piezas_de("vencidos", "densidad"))
  expect_true("casillas" %in% hd_piezas_de("vencidos", c("densidad", "casillas")))
  # already on: allowed, no duplicate
  expect_identical(hd_piezas_de("vencidos", "buscar"), hd_piezas_de("vencidos"))
})

test_that("unknown tables and pieces stop with a clear message", {
  expect_error(hd_piezas_de("no_existe"), "unknown table 'no_existe'")
  expect_error(hd_piezas_de("vencidos", "volar"), "unknown piece\\(s\\): volar")
  # multilinea exists but class A neither has it nor may turn it on
  expect_error(hd_piezas_de("vencidos", "multilinea"),
               "class A.*multilinea are not optional")
})

test_that("a piece the table removes can't be turned back on by the caller", {
  quitan <- Filter(function(d) length(d$quitar) > 0, hd_registro)
  skip_if(!length(quitan), "no table removes pieces")
  tabla <- names(quitan)[1]
  pieza <- quitan[[1]]$quitar[1]
  expect_false(pieza %in% hd_piezas_de(tabla))
  if (!pieza %in% hd_clases[[quitan[[1]]$clase]]$opcional)
    expect_error(hd_piezas_de(tabla, pieza), "not optional")
})

test_that("pieces not built yet are dropped with one message per table", {
  p <- hd_piezas_de("vencidos")
  expect_message(m <- hd_piezas_motor("vencidos", p), "not built yet")
  expect_true(all(m %in% HD_PIEZAS_MOTOR))
  expect_silent(hd_piezas_motor("vencidos", p))
})

test_that("core rules exist and aren't pieces", {
  expect_true(length(HD_REGLAS_NUCLEO) >= 5)
  expect_false(any(names(HD_REGLAS_NUCLEO) %in% HD_PIEZAS_VALIDAS))
})

test_that("counter noun: class default, per-table exception, key exists", {
  expect_identical(hd_contador_clave("vencidos"), "tablas.contador.facturas")
  expect_identical(hd_contador_clave("libro_banco"), "tablas.contador.movimientos")
  expect_identical(hd_contador_clave("proveedores"), "tablas.contador.registros")
  for (tabla in names(hd_registro))
    expect_false(identical(tr_n(hd_contador_clave(tabla), 2), paste0(hd_contador_clave(tabla), ".otros")),
                 info = tabla)
})

# ── Columns: the three values of every cell ─────────────────────────────────
test_that("col_dinero: shown in Spanish money format, sorted by number, copied raw", {
  v <- col_dinero("Importe")$valores(c(1234.5, -0.5, 1000, NA, 1e6))
  expect_identical(v$mostrar, c("$1,234.50", "-$0.50", "$1,000.00", "", "$1,000,000.00"))
  expect_identical(v$orden, c(1234.5, -0.5, 1000, NA, 1e6))
  expect_identical(v$copiar, c("1234.5", "-0.5", "1000", "", "1000000"))
})

test_that("col_fecha: Date, POSIXct and ISO text; shown short, sorted and copied ISO", {
  v <- col_fecha("Vence")$valores(as.Date(c("2026-10-29", NA, "2026-01-05")))
  expect_identical(v$mostrar, c("29 oct 2026", "", "5 ene 2026"))
  expect_identical(v$orden, c("2026-10-29", NA, "2026-01-05"))
  expect_identical(v$copiar, c("2026-10-29", "", "2026-01-05"))
  expect_identical(col_fecha("f")$valores("2026-12-01")$mostrar, "1 dic 2026")
  expect_identical(col_fecha("f")$valores(as.POSIXct("2026-03-02 15:00", tz = "UTC"))$copiar, "2026-03-02")
  expect_identical(col_fecha("f")$valores("no es fecha")$mostrar, "")
})

test_that("col_fecha follows the language's date template", {
  old <- options(hopdesk.idioma = "en"); on.exit(options(old))
  expect_identical(col_fecha("f")$valores(as.Date("2026-10-29"))$mostrar, "Oct 29, 2026")
})

test_that("col_texto keeps text literal (escaping happens when painting) and sorts accent-free", {
  evil <- '<b>Renta</b> & "Bodega"'
  v <- col_texto("Parte")$valores(c(evil, "Árbol", NA))
  expect_identical(v$mostrar, c(evil, "Árbol", ""))
  expect_identical(v$copiar,  c(evil, "Árbol", ""))
  expect_identical(v$orden[2], "arbol")
  expect_true(col_texto("x")$recorte)
  expect_false(col_texto("x", recorte = FALSE)$recorte)
})

test_that("col_insignia: label from the map, sorted by the map's order, tone per value", {
  col <- col_insignia("Estado", "Estado", mapa = list(
    pendiente  = list(texto = "Pendiente",  tono = "aviso"),
    confirmada = list(texto = "Confirmada", tono = "exito")))
  v <- col$valores(c("confirmada", "pendiente", "otro"))
  expect_identical(v$mostrar, c("Confirmada", "Pendiente", "otro"))
  expect_identical(v$orden, c(2L, 1L, 3L))
  expect_identical(v$copiar, c("confirmada", "pendiente", "otro"))
  expect_identical(v$tono, c("exito", "aviso", "neutro"))
  expect_error(col_insignia("x", mapa = list("sin nombre")), "named list")
})

test_that("col_empresa and col_etiqueta are typed text columns", {
  expect_identical(col_empresa()$campo, "Empresa")
  expect_identical(col_empresa()$tipo, "empresa")
  v <- col_etiqueta("Etiqueta")$valores(c("Urgente", ""))
  expect_identical(v$orden, c("urgente", NA))
})

test_that("titles: dictionary key or plain text", {
  meta <- hd_columnas_meta(list(col_texto("a", "tablas.buscar"), col_texto("b", "Proveedor"),
                                col_texto("c")))
  expect_identical(vapply(meta, `[[`, "", "titulo"), c("Buscar", "Proveedor", "c"))
})

# ── Row payload ─────────────────────────────────────────────────────────────
cols <- list(col_empresa(), col_texto("Parte", "Proveedor"),
             col_dinero("Importe", "Importe"), col_fecha("Vence", "Vence"))
df <- data.frame(
  id = c("a", "b", "c"), Empresa = "NTS",
  Parte = c('<b>Renta</b> & "Bodega"', "Beta", "Gamma"),
  Importe = c(10, 20.5, 30), Vence = as.Date("2026-10-29") + 0:2,
  Moneda = c("MXN", "USD", "MXN"), stringsAsFactors = FALSE)

test_that("ids: missing column, empty or repeated ids stop with a clear message", {
  expect_error(hd_filas(df, cols, id_col = "folio"), "id column 'folio' is not in the data")
  d2 <- df; d2$id[2] <- NA
  expect_error(hd_filas(d2, cols), "1 empty value")
  d3 <- df; d3$id[3] <- "a"
  expect_error(hd_filas(d3, cols), "repeats: a")
  expect_error(hd_filas(df, list(col_texto("NoHay"))), "not in the data: NoHay")
  expect_error(hd_filas(df, list()), "non-empty list")
})

test_that("payload is column by column, with control columns and defaults", {
  f <- hd_filas(df, cols)
  expect_identical(f$ids, c("a", "b", "c"))
  expect_identical(f$bloqueada, c(FALSE, FALSE, FALSE))
  expect_identical(f$moneda, c("MXN", "USD", "MXN"))     # from col_dinero's `moneda`
  expect_true(all(is.na(f$tipo)))
  expect_length(f$celdas, 4)
  expect_identical(f$celdas[[3]]$mostrar, c("$10.00", "$20.50", "$30.00"))

  d2 <- df; d2$.bloqueada <- c(NA, TRUE, FALSE); d2$.moneda <- "EUR"
  d2$.tipo <- c("cobro", "pago", "otro")
  f2 <- hd_filas(d2, cols)
  expect_identical(f2$bloqueada, c(FALSE, TRUE, FALSE))
  expect_identical(f2$moneda, rep("EUR", 3))
  expect_identical(f2$tipo, c("cobro", "pago", NA))
})

test_that("<b> travels to the browser as literal text, never as markup", {
  json <- jsonlite::toJSON(hd_filas(df, cols), auto_unbox = TRUE, na = "null")
  back <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  expect_identical(back$celdas$mostrar[[2]][1], '<b>Renta</b> & "Bodega"')
  expect_false(grepl("&lt;", json, fixed = TRUE))   # not pre-escaped: the JS uses textContent
})

test_that("5,000 rows build quickly", {
  n  <- 5000
  big <- data.frame(id = sprintf("r%05d", 1:n), Empresa = "NTS", Parte = paste("Proveedor", 1:n),
                    Importe = round(runif(n, -1e5, 1e6), 2), Vence = Sys.Date() + (1:n %% 90),
                    Moneda = sample(c("MXN", "USD"), n, TRUE), stringsAsFactors = FALSE)
  t <- system.time(f <- hd_filas(big, cols))[["elapsed"]]
  cat(sprintf("\n  hd_filas(5,000 rows): %.3f s\n", t))
  expect_length(f$ids, n)
  expect_lt(t, 1)
})
