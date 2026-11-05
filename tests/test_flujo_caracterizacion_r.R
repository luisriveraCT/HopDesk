# =============================================================================
# tests/test_flujo_caracterizacion_r.R
# The Flujo de caja preview as it is today (T17c, step 0): the safety net
# before it moves to the table engine. The real aggregation
# (build_cashflow_export_data, what the exports use too) and today's painting
# (render_cashflow_preview_html), read through tests/helpers/flujo_adaptador.R
# -- texts as the person reads them and the exact numbers -- and the real
# cashflowPreviewServer() under testServer() for the toolbar (never S3:
# build_export_combined_df is replaced by fixed rows, as in flujo_fixture_app).
# Case numbers F1.. are those of reportes/T17c_paso0.md.
#
# Records TODAY's behaviour, rounding quirks included (F6). The whole grid of
# two scenarios is also compared with tests/fixtures/flujo_antes.json
# (positions, not dates). FL_GENERAR=1 rewrites it -- only on master, never
# to make a test pass.
#
# Run from the repo root: Rscript tests/test_flujo_caracterizacion_r.R
# =============================================================================
suppressPackageStartupMessages({ library(testthat); library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/persistence.R", "tag_label")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/cashflow_export_module.R", "R/cashflow_preview_module.R")) source(f)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
source("tests/helpers/flujo_datos_prueba.R")
source("tests/helpers/flujo_adaptador.R")

LUNES <- lubridate::floor_date(Sys.Date(), "week", week_start = 1)
D <- flujo_datos_prueba(LUNES)
datos <- function(desde = LUNES, hasta = LUNES + 6L, agrupacion = "auto", modo = "separate", base = "MXN", fx = c())
  build_cashflow_export_data(D$df, D$tags, D$empresas, desde, hasta, agrupacion, modo, base, fx)
ver <- function(ed, expandido = FALSE, day0 = list())
  fl_rejilla(render_cashflow_preview_html(ed, expanded = expandido, day0_vals = day0), ed, day0)
ED <- datos()
R0 <- ver(ED, day0 = list(MXN = 1500))
celda <- function(x, k) x$celdas[[paste0("c", k)]]
textos <- function(x) unname(vapply(x$celdas, `[[`, character(1), "texto"))
valores <- function(x) unname(vapply(x$celdas, `[[`, numeric(1), "valor"))

# ── Columns and blocks ──────────────────────────────────────────────────────
test_that("F1: Contraparte, Día 0, one column per period, Total; daily up to 14 days, weekly above, or as chosen", {
  expect_identical(R0$encabezado[c(1, 2, length(R0$encabezado))], c("Contraparte", "Día 0", "Total"))
  expect_identical(length(R0$columnas), 7L)
  expect_identical(datos(hasta = LUNES + 13L)$grouping_used, "daily")
  expect_identical(datos(hasta = LUNES + 14L)$grouping_used, "weekly")
  expect_identical(datos(agrupacion = "weekly")$grouping_used, "weekly")
  expect_identical(length(datos(hasta = LUNES + 20L)$bucket_labels), 3L)
  expect_identical(datos(hasta = LUNES + 20L, agrupacion = "daily")$grouping_used, "daily")
})
test_that("F2: one block per currency, in order, each with COBROS then PAGOS", {
  expect_identical(vapply(R0$bloques, `[[`, character(1), "moneda"), c("MXN", "USD"))
  expect_identical(names(fl_bloque(R0, "MXN")$secciones), c("cobros", "pagos"))
  expect_identical(fl_bloque(R0, "MXN")$secciones$cobros$titulo, "COBROS — MXN")
  expect_identical(fl_bloque(R0, "USD")$secciones$pagos$titulo, "PAGOS — USD")
})
test_that("F3: a row per counterparty, alphabetical; a provision is its own row, marked; tags marked", {
  cob <- fl_bloque(R0, "MXN")$secciones$cobros$filas
  expect_identical(vapply(cob, `[[`, character(1), "parte"), c("CLIENTE ALFA", "CLIENTE BETA", "CLIENTE GAMA"))
  expect_identical(vapply(cob, `[[`, logical(1), "provision"), c(FALSE, FALSE, TRUE))
  expect_identical(vapply(cob, `[[`, logical(1), "urgente"), c(TRUE, FALSE, FALSE))
  expect_identical(vapply(cob, `[[`, logical(1), "importante"), c(FALSE, TRUE, FALSE))
})
test_that("F4: out of the range and other companies never show", {
  partes <- unlist(lapply(R0$bloques, function(b) unlist(lapply(b$secciones, function(s) vapply(s$filas, `[[`, "", "parte")))))
  expect_false(any(c("FUERA ANTES", "FUERA DESPUES", "OTRA EMPRESA") %in% partes))
})
test_that("F5: counterparties start hidden; expanded, they show; subtotals, net and position always", {
  expect_false(any(unlist(lapply(fl_bloque(R0, "MXN")$secciones, function(s) vapply(s$filas, `[[`, logical(1), "visible")))))
  r <- ver(ED, expandido = TRUE)
  expect_true(all(unlist(lapply(fl_bloque(r, "MXN")$secciones, function(s) vapply(s$filas, `[[`, logical(1), "visible")))))
  expect_false(is.null(fl_bloque(R0, "MXN")$secciones$cobros$subtotal))
})

# ── Amounts ─────────────────────────────────────────────────────────────────
test_that("F6: cells compact ($4.5M / $802K / $950, empty for 0); Total full; TODAY's rounding, recorded as is", {
  alfa <- fl_fila(R0, "MXN", "cobros", "CLIENTE ALFA")
  expect_identical(textos(alfa), c("$4.5M", "", "$40K", "", "", "", ""))
  expect_identical(alfa$total$texto, "$4,535,962")
  expect_identical(textos(fl_fila(R0, "MXN", "cobros", "CLIENTE GAMA"))[4], "$950")
  # Halves go to even today (formatC): 120.5 reads "$120" but its Total "$5,000,121";
  # 2,500 reads "$2K" like 1,500 (the Día 0 below). T17c must not change a number
  # silently: any change here stops for the coordinator.
  uno <- fl_fila(R0, "MXN", "pagos", "PROVEEDOR UNO")
  expect_identical(c(textos(uno)[5], uno$total$texto), c("$120", "$5,000,121"))
  expect_identical(celda(uno, 5)$valor, 120.5)
  expect_identical(textos(fl_fila(R0, "USD", "pagos", "VENDOR USA"))[3], "$2K")
})
test_that("F7: each section's subtotal adds up its rows per column; its Total in full", {
  s <- fl_bloque(R0, "MXN")$secciones$cobros$subtotal
  expect_identical(s$titulo, "SUBTOTAL COBROS — MXN")
  expect_identical(valores(s), c(4495962, 802000, 40000, 950, 0, 0, 0))
  expect_identical(textos(s), c("$4.5M", "$802K", "$40K", "$950", "", "", ""))
  expect_identical(s$total$texto, "$5,338,912")
})
test_that("F8: FLUJO NETO = cobros - pagos per column, a 0 shows $0, green / red / grey by sign; Total in full", {
  n <- fl_bloque(R0, "MXN")$neto
  expect_identical(n$titulo, "FLUJO NETO MXN")
  expect_identical(valores(n), c(-504038, 0, 40000, 950, -120.5, 0, 0))
  expect_identical(textos(n), c("−$504K", "$0", "$40K", "$950", "−$120", "$0", "$0"))
  expect_identical(unname(vapply(n$celdas, `[[`, "", "tono")), c("peligro", "neutro", "exito", "exito", "peligro", "neutro", "neutro"))
  expect_identical(c(n$total$texto, n$total$tono), c("−$463,209", "peligro"))
})
test_that("F9: POS. ACUMULADA = Día 0 + running net; the last one is its Total", {
  a <- fl_bloque(R0, "MXN")$acumulada
  expect_identical(a$titulo, "POS. ACUMULADA MXN")
  expect_identical(a$dia0, list(texto = "$2K", valor = 1500))
  expect_identical(valores(a), 1500 + cumsum(c(-504038, 0, 40000, 950, -120.5, 0, 0)))
  expect_identical(a$total$texto, "−$461,709")
  expect_identical(a$total$valor, valores(a)[7])
})
test_that("F10: Día 0 is per currency: MXN's doesn't move USD's", {
  u <- fl_bloque(R0, "USD")$acumulada
  expect_identical(u$dia0$valor, 0)
  expect_identical(valores(u), cumsum(c(0, 10000, -2500, 0, 0, 0, 0)))
})
test_that("F11: Base (fused): one block in the base currency, the others converted at their rate", {
  e <- datos(modo = "fused", base = "MXN", fx = c(USD = 20))
  expect_identical(e$currencies, "MXN")
  r <- ver(e)
  expect_identical(length(r$bloques), 1L)
  expect_identical(celda(fl_fila(r, "MXN", "cobros", "CLIENT USA"), 2)$valor, 200000)
  expect_identical(valores(fl_bloque(r, "MXN")$neto)[2], 0 + 200000)
})
test_that("F12: nothing in the range: no data (the module then says 'Sin movimientos ...')", {
  expect_null(datos(desde = LUNES + 30L, hasta = LUNES + 36L))
})

# ── The module: toolbar, Día 0, saved state, export ─────────────────────────
nuevo_shared <- function() list(
  cf_preview_open = reactiveVal(TRUE), cf_preview_prefill = reactiveVal(NULL),
  currency_choices = list(AR = function() c("MXN", "USD"), AP = function() c("MXN", "USD")),
  ic_mode = list(AR = function() "exclude"), tags_db = function() D$tags, empresa_sel = function() D$empresas)
assign("build_export_combined_df", function(shared, ic_mode_val) D$df, envir = globalenv())
modulo <- function(cuerpo) {
  sh <- nuevo_shared()
  suppressWarnings(testServer(cashflowPreviewServer, args = list(shared = sh), {
    session$setInputs(date_range = c(as.character(LUNES), as.character(LUNES + 6L)))
    cuerpo(session, input, output, sh, rv)
  }))
}
tabla_modulo <- function(output) as.character(output$preview_body$html)

test_that("F13: the grouping button cycles Auto → Diario → Semanal → Auto", {
  modulo(function(session, input, output, sh, rv) {
    vistos <- character(0)
    for (k in 1:3) { session$setInputs(btn_grouping = k); vistos <- c(vistos, rv$grouping) }
    expect_identical(vistos, c("daily", "weekly", "auto"))
  })
})
test_that("F14: the currency button toggles Separadas ↔ Base; the FX row only in Base, without the base currency", {
  modulo(function(session, input, output, sh, rv) {
    expect_null(output$fx_section_ui)
    session$setInputs(btn_curmode = 1)
    expect_identical(rv$currency_mode, "fused")
    fx <- as.character(output$fx_section_ui$html)
    expect_true(grepl("fx_rate_usd", fx, fixed = TRUE)); expect_false(grepl("fx_rate_mxn", fx, fixed = TRUE))
    session$setInputs(fx_rate_usd_blur = 19.5); expect_identical(rv$fx_rates[["USD"]], 19.5)
    session$setInputs(fx_rate_usd_blur = 0);    expect_identical(rv$fx_rates[["USD"]], 19.5)   # 0 or less: ignored
    session$setInputs(btn_curmode = 2); expect_identical(rv$currency_mode, "separate")
  })
})
test_that("F15: Día 0 takes a number per currency (what the dialog sends); anything else is ignored", {
  modulo(function(session, input, output, sh, rv) {
    session$setInputs(cf_day0_submit = list(currency = "MXN", value = 1500))
    expect_true(grepl("data-valor=\"1500\"", tabla_modulo(output), fixed = TRUE))
    session$setInputs(cf_day0_submit = list(currency = "MXN", value = "x"))
    expect_true(grepl("data-valor=\"1500\"", tabla_modulo(output), fixed = TRUE))
  })
})
test_that("F16: the expand button shows and hides every counterparty", {
  modulo(function(session, input, output, sh, rv) {
    expect_true(grepl("display:none", tabla_modulo(output), fixed = TRUE))
    session$setInputs(btn_expand = 1); expect_false(grepl("display:none", tabla_modulo(output), fixed = TRUE))
    session$setInputs(btn_expand = 2); expect_true(grepl("display:none", tabla_modulo(output), fixed = TRUE))
  })
})
test_that("F17: a saved state comes back (range, grouping, currency mode, base, rates); a bad one is ignored", {
  modulo(function(session, input, output, sh, rv) {
    session$setInputs(stored_state = list(date_from = as.character(LUNES + 7L), date_to = as.character(LUNES + 13L),
                                          grouping = "weekly", currency_mode = "fused", base_cur = "USD",
                                          fx_rates = list(MXN = 0.05)))
    expect_identical(list(rv$date_from, rv$date_to, rv$grouping, rv$currency_mode, rv$base_cur, rv$fx_rates[["MXN"]]),
                     list(LUNES + 7L, LUNES + 13L, "weekly", "fused", "USD", 0.05))
    session$setInputs(stored_state = list(date_from = as.character(LUNES + 13L), date_to = as.character(LUNES), grouping = "x"))
    expect_identical(list(rv$date_from, rv$grouping), list(LUNES + 7L, "weekly"))
  })
})
test_that("F18: Exportar hands the export window the preview's settings", {
  modulo(function(session, input, output, sh, rv) {
    session$setInputs(btn_grouping = 1, btn_curmode = 1, fx_rate_usd_blur = 18)
    session$setInputs(btn_export = 1)
    p <- sh$cf_preview_prefill()
    expect_identical(p[c("date_from", "date_to", "grouping", "currency_mode", "base_cur")],
                     list(date_from = LUNES, date_to = LUNES + 6L, grouping = "daily", currency_mode = "fused", base_cur = "MXN"))
    expect_identical(p$fx_rates[["USD"]], 18)
  })
})
test_that("F19: the preview shows the same grid as the pure painting (the module adds nothing to the numbers)", {
  modulo(function(session, input, output, sh, rv) {
    r <- fl_rejilla(tabla_modulo(output), ED)
    expect_identical(r$bloques, ver(ED)$bloques)
  })
})
test_that("F20: no rows in the range: 'Sin movimientos en el período seleccionado'", {
  modulo(function(session, input, output, sh, rv) {
    session$setInputs(date_range = c(as.character(LUNES + 30L), as.character(LUNES + 36L)))
    session$elapse(500)   # the data is debounced 400 ms
    expect_true(grepl("Sin movimientos en el período seleccionado", tabla_modulo(output), fixed = TRUE))
  })
})

# ── The whole grid, against the reference ───────────────────────────────────
test_that("the whole grid of two scenarios is the reference's (texts and exact numbers)", {
  ahora <- list(diario = ver(ED, expandido = TRUE, day0 = list(MXN = 1500, USD = -250)),
                semanal_base = ver(datos(hasta = LUNES + 20L, modo = "fused", fx = c(USD = 20)), expandido = TRUE))
  ahora <- lapply(ahora, function(r) r[c("bloques")])     # labels move with the date; positions don't
  json <- jsonlite::toJSON(ahora, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE)
  archivo <- "tests/fixtures/flujo_antes.json"
  if (identical(Sys.getenv("FL_GENERAR"), "1")) writeLines(json, archivo, useBytes = TRUE)
  expect_identical(as.character(json), paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
})
