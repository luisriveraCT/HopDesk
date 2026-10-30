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
source("R/hd_table.R")

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

test_that("col_dinero: near-zero amounts never show or copy a minus sign", {
  v <- col_dinero("Importe")$valores(c(-0.001, 0.004, -0.005, -0.006))
  expect_identical(v$mostrar[1:2], c("$0.00", "$0.00"))
  expect_identical(v$copiar[1:2], c("0", "0"))
  expect_false(any(v$mostrar %in% c("-$0.00", "-0")) || any(v$copiar %in% c("-0", "-0.00")))
  expect_identical(v$mostrar[4], "-$0.01")
})

test_that("col_fecha: a POSIXct keeps its own time zone's date", {
  x <- as.POSIXct("2026-10-29 20:00", tz = "America/Mexico_City")
  v <- col_fecha("f")$valores(x)
  expect_identical(v$copiar, "2026-10-29")
  expect_identical(v$mostrar, "29 oct 2026")
  y <- as.POSIXct("2026-10-30 01:30", tz = "UTC")
  expect_identical(col_fecha("f")$valores(y)$copiar, "2026-10-30")
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
  expect_identical(unclass(f$ids), c("a", "b", "c"))
  expect_identical(unclass(f$bloqueada), c(FALSE, FALSE, FALSE))
  expect_identical(unclass(f$moneda), c("MXN", "USD", "MXN"))     # from col_dinero's `moneda`
  expect_true(all(is.na(f$tipo)))
  expect_length(f$celdas, 4)
  expect_identical(unclass(f$celdas[[3]]$mostrar), c("$10.00", "$20.50", "$30.00"))

  d2 <- df; d2$.bloqueada <- c(NA, TRUE, FALSE); d2$.moneda <- "EUR"
  d2$.tipo <- c("cobro", "pago", "otro")
  f2 <- hd_filas(d2, cols)
  expect_identical(unclass(f2$bloqueada), c(FALSE, TRUE, FALSE))
  expect_identical(unclass(f2$moneda), rep("EUR", 3))
  expect_identical(unclass(f2$tipo), c("cobro", "pago", NA))
})

# What the browser receives: Shiny's own serializer (auto_unbox = TRUE).
.json <- function(x) as.character(shiny:::toJSON(x))

test_that("<b> travels to the browser as literal text, never as markup", {
  json <- .json(hd_filas(df, cols))
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_identical(back$celdas[[2]]$mostrar[[1]], '<b>Renta</b> & "Bodega"')
  expect_false(grepl("&lt;", json, fixed = TRUE))   # not pre-escaped: the JS uses textContent
})

test_that("with 1 row every per-row value is still an array in the JSON", {
  back <- jsonlite::fromJSON(.json(hd_filas(df[1, ], cols)), simplifyVector = FALSE)
  for (k in c("ids", "bloqueada", "moneda", "tipo"))
    expect_true(is.list(back[[k]]) && length(back[[k]]) == 1L, info = k)
  for (j in seq_along(back$celdas))
    for (k in c("mostrar", "orden", "copiar"))
      expect_true(is.list(back$celdas[[j]][[k]]) && length(back$celdas[[j]][[k]]) == 1L,
                  info = paste(j, k))
  expect_match(.json(hd_filas(df[1, ], cols)), '"ids":["a"]', fixed = TRUE)
})

test_that("with 0 rows every per-row value is an empty array in the JSON", {
  json <- .json(hd_filas(df[0, ], cols))
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  for (k in c("ids", "bloqueada", "moneda", "tipo"))
    expect_identical(back[[k]], list(), info = k)
  expect_length(back$celdas, 4)
  for (j in seq_along(back$celdas))
    expect_identical(back$celdas[[j]]$mostrar, list(), info = j)
  expect_match(json, '"ids":[]', fixed = TRUE)
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


# ── T03a: arguments, control columns, export ───────────────────────────────
test_that("row actions: catalog order, crear_provision first and as text, unknown names stop", {
  expect_identical(hd_acciones_fila_validar(c("eliminar", "editar", "crear_provision")),
                   c("crear_provision", "editar", "eliminar"))
  expect_identical(names(HD_ACCIONES_FILA)[1], "crear_provision")
  expect_identical(HD_ACCIONES_FILA$crear_provision$estilo, "texto")
  expect_true(all(vapply(HD_ACCIONES_FILA[-1], function(a) identical(a$estilo, "icono"), logical(1))))
  expect_identical(utf8ToInt(HD_ACCIONES_FILA$provision$icono), 0x26a1L)
  expect_true(HD_ACCIONES_FILA$eliminar$confirmar)
  expect_error(hd_acciones_fila_validar(c("editar", "volar")), "unknown row action\\(s\\): volar")
  expect_error(hd_acciones_fila_validar(character(0)), "character vector")
  expect_identical(hd_acciones_fila_meta("crear_provision")[[1]]$titulo, tr("tablas.accion.crear_provision"))
})

test_that("icons in R/hd_table.R are written as escapes, not raw characters", {
  txt <- readLines("R/hd_table.R", encoding = "UTF-8", warn = FALSE)
  lineas <- grep("icono = \"", txt, value = TRUE)
  expect_true(length(lineas) >= 6)
  expect_false(any(vapply(lineas, function(l) any(utf8ToInt(l) > 127L), logical(1))))
})

test_that("fichas: shape validated, field must exist, labels from the col_insignia map", {
  expect_error(hd_fichas_validar(list(orden = "a")), "fichas` must be list")
  expect_error(hd_fichas_validar(list(campo = "x", ordne = "a")), "unknown `fichas` field\\(s\\): ordne")
  expect_error(hd_fichas_validar(list(campo = "x", mostrar = "suma")), "conteo")
  f <- hd_fichas_validar(list(campo = "Estado", orden = c("b", "a")))
  d <- data.frame(id = c("1", "2", "3"), Estado = c("a", "c", NA), Importe = 1:3, stringsAsFactors = FALSE)
  expect_error(hd_fichas_payload(hd_fichas_validar(list(campo = "Nope")), d, list(col_texto("Estado"))),
               "fichas\\$campo 'Nope' is not in the data")
  expect_error(hd_fichas_payload(hd_fichas_validar(list(campo = "Estado", mostrar = "monto")), d,
                                 list(col_texto("Estado"))), "needs a col_dinero")
  cols_f <- list(col_insignia("Estado", mapa = list(a = list(texto = "Alfa"))))
  pl <- hd_fichas_payload(f, d, cols_f)
  expect_identical(vapply(pl$opciones, `[[`, "", "valor"), c("b", "a", "c"))
  expect_identical(vapply(pl$opciones, `[[`, "", "texto"), c("b", "Alfa", "c"))
  expect_identical(unclass(pl$valores), c("a", "c", ""))
})

test_that("bar actions: validated, unique, 'detalle' reserved", {
  ok <- hd_acciones_barra_validar(list(list(accion = "x", texto = "tablas.accion.papelera", confirmar = TRUE)))
  expect_true(ok[[1]]$confirmar)
  expect_false(ok[[1]]$sin_seleccion)
  expect_error(hd_acciones_barra_validar(list(list(texto = "t"))), "needs `accion`")
  expect_error(hd_acciones_barra_validar(list(list(accion = "x"))), "needs `texto`")
  expect_error(hd_acciones_barra_validar(list(list(accion = "x", texto = "t", color = 1))), "unknown field\\(s\\) color")
  expect_error(hd_acciones_barra_validar(list(list(accion = "x", texto = "t"), list(accion = "x", texto = "u"))),
               "repeats 'x'")
  expect_error(hd_acciones_barra_validar(list(list(accion = "detalle", texto = "t"))), "reserved")
  expect_identical(hd_acciones_barra_meta(ok)[[1]]$texto, tr("tablas.accion.papelera"))
})

test_that("asking for confirmation on a table without the confirmar piece stops", {
  reg <- hd_registro
  on.exit(assign("hd_registro", reg, envir = globalenv()))
  hd_registro$sin_conf <<- list(nombre = "x", clase = "A", archivo = "x", agregar = character(),
                                quitar = "confirmar", notas = character())
  cols1 <- list(col_texto("Parte"))
  expect_error(hd_table_server("t", "sin_conf", shiny::reactive(NULL), cols1, acciones_fila = "eliminar"),
               "row action 'eliminar' asks for confirmation")
  expect_error(hd_table_server("t", "sin_conf", shiny::reactive(NULL), cols1,
                               acciones_barra = list(list(accion = "borrar", texto = "t", confirmar = TRUE))),
               "bar action 'borrar' asks for confirmation")
})

test_that(".acciones (list or comma text, fails closed) and .provision travel per row", {
  d <- data.frame(id = c("a", "b", "c", "d"), Parte = "p", stringsAsFactors = FALSE)
  d$.acciones <- list(c("editar", "detalle"), character(0), NA, "agregar, eliminar")
  d$.provision <- c(TRUE, NA, FALSE, TRUE)
  f <- hd_filas(d, list(col_texto("Parte")))
  expect_identical(unclass(f$acciones), c("editar,detalle", "", "", "agregar,eliminar"))
  expect_identical(unclass(f$provision), c(TRUE, FALSE, FALSE, TRUE))
  expect_null(hd_filas(d[, c("id", "Parte")], list(col_texto("Parte")))$acciones)
  expect_match(.json(hd_filas(d[1, ], list(col_texto("Parte")))), '"acciones":["editar,detalle"]', fixed = TRUE)
})

test_that("neutralize: text a spreadsheet would run as a formula gets an apostrophe", {
  tab <- intToUtf8(9L); cr <- intToUtf8(13L)
  expect_identical(hd_neutralizar(c("=1+1", "+52", "-3", "@x", paste0(tab, "A"), paste0(cr, "B"), "ok", NA)),
                   c("'=1+1", "'+52", "'-3", "'@x", paste0("'", tab, "A"), paste0("'", cr, "B"), "ok", NA))
})

xl_cols <- list(col_texto("Parte", "Proveedor"), col_dinero("Importe", "Importe"), col_fecha("Vence", "Vence"),
                col_insignia("Etapa", "Etapa", mapa = list(a = list(texto = "Aprobada"))))
xl_df <- function(n) data.frame(id = sprintf("R%03d", seq_len(n)),
                                Parte = c("=1+1", sprintf("P%d", seq_len(n)))[seq_len(n)],
                                Importe = seq_len(n) * 1.5, Vence = as.Date("2026-10-01") + seq_len(n) - 1,
                                Etapa = rep("a", n), stringsAsFactors = FALSE)
leer <- function(f) openxlsx::read.xlsx(f, detectDates = TRUE)

test_that("Excel, 300 rows: only the view's rows, in the view's order, typed, formulas neutralized", {
  d <- xl_df(300)
  vista <- rev(d$id[seq(1, 300, 3)])          # a filter and a reversed sort
  f <- tempfile(fileext = ".xlsx"); on.exit(unlink(f))
  hd_excel_escribir(d, xl_cols, vista, "id", f)
  x <- leer(f)
  expect_identical(nrow(x), length(vista))
  expect_identical(names(x), c("Proveedor", "Importe", "Vence", "Etapa"))
  esperado <- d[match(vista, d$id), ]
  expect_identical(x$Proveedor, hd_neutralizar(esperado$Parte))
  expect_true(is.numeric(x$Importe))
  expect_equal(x$Importe, esperado$Importe)
  expect_s3_class(x$Vence, "Date")
  expect_identical(x$Vence, esperado$Vence)
  expect_identical(x$Etapa, rep("a", nrow(x)))                     # raw value, not the label
  expect_true("'=1+1" %in% x$Proveedor)                            # written as text
})

test_that("Excel with 1 and with 0 rows opens", {
  d <- xl_df(5)
  f1 <- tempfile(fileext = ".xlsx"); f0 <- tempfile(fileext = ".xlsx"); on.exit(unlink(c(f1, f0)))
  hd_excel_escribir(d, xl_cols, "R002", "id", f1)
  x1 <- leer(f1)
  expect_identical(nrow(x1), 1L)
  expect_identical(x1$Proveedor, "P1")   # R002 is the 2nd row: "P1"
  hd_excel_escribir(d, xl_cols, character(0), "id", f0)
  x0 <- openxlsx::read.xlsx(f0)
  expect_identical(names(x0), c("Proveedor", "Importe", "Vence", "Etapa"))
})

test_that("filtros: shape validated, field must exist, options from the data with the col_insignia map", {
  expect_error(hd_filtros_validar(list(campo = "Moneda")), "filtros` must be a list of list")
  expect_error(hd_filtros_validar(list(list(titulo = "x"))), "filtros[[1]] needs `campo`", fixed = TRUE)
  expect_error(hd_filtros_validar(list(list(campo = "Moneda", orden = "a"))), "filtro 'Moneda': unknown field(s) orden", fixed = TRUE)
  expect_error(hd_filtros_validar(list(list(campo = "Moneda", titulo = 3))), "`titulo` must be")
  expect_error(hd_filtros_validar(list(list(campo = "Moneda"), list(campo = "Moneda"))), "repeats 'Moneda'")
  f <- hd_filtros_validar(list(list(campo = "Estado"), list(campo = "Moneda", titulo = "Divisa"),
                               list(campo = "Empresa")))
  d <- data.frame(id = as.character(1:5), Estado = c("c", "a", NA, "b", "a"),
                  Moneda = c("USD", "MXN", "MXN", "eur", "USD"), Empresa = "NTS", stringsAsFactors = FALSE)
  cols_f <- list(col_insignia("Estado", "Etapa", mapa = list(b = list(texto = "Beta"), a = list(texto = "Alfa"))),
                 col_texto("Moneda"))
  expect_error(hd_filtros_payload(hd_filtros_validar(list(list(campo = "Nope"))), d, cols_f),
               "filtro 'Nope' is not a column in the data")
  pl <- hd_filtros_payload(f, d, cols_f)
  # Map order and labels first, then the rest; "(Sin valor)" last when a row has none.
  expect_identical(vapply(pl[[1]]$opciones, `[[`, "", "valor"), c("b", "a", "c", ""))
  expect_identical(vapply(pl[[1]]$opciones, `[[`, "", "texto"), c("Beta", "Alfa", "c", tr("tablas.filtros.vacio")))
  expect_identical(unclass(pl[[1]]$valores), c("c", "a", "", "b", "a"))
  # Without a map: alphabetical, case- and accent-free.
  expect_identical(vapply(pl[[2]]$opciones, `[[`, "", "valor"), c("eur", "MXN", "USD"))
  # Title: the filter's own, else its column's, else the field.
  expect_identical(vapply(pl, `[[`, "", "titulo"), c("Etapa", "Divisa", "Empresa"))
  # One value, one row: still arrays in the JSON.
  j <- .json(hd_filtros_payload(f[3], d[1, ], cols_f))
  expect_match(j, '"valores":["NTS"]', fixed = TRUE)
  expect_match(j, '"opciones":[{"valor":"NTS","texto":"NTS"}]', fixed = TRUE)
})

test_that("filtros on a table without the piece are ignored with a message", {
  reg <- hd_registro
  on.exit(assign("hd_registro", reg, envir = globalenv()))
  hd_registro$sin_filtros <<- list(nombre = "x", clase = "A", archivo = "x", agregar = character(),
                                   quitar = "filtros", notas = character())
  f <- hd_filtros_validar(list(list(campo = "Parte")))
  expect_message(x <- .hd_sin_pieza("sin_filtros", "filtros", "filtros", f, hd_piezas_de("sin_filtros")),
                 "`filtros` ignored, the table doesn't have the 'filtros' piece")
  expect_null(x)
  expect_identical(.hd_sin_pieza("vencidos", "filtros", "filtros", f, hd_piezas_de("vencidos")), f)
})
