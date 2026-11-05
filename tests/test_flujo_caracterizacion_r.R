# =============================================================================
# tests/test_flujo_caracterizacion_r.R
# The Flujo de caja preview, before and after it moved to the table engine
# (T17c). Step 0 recorded today's preview (render_cashflow_preview_html, read
# through tests/helpers/flujo_adaptador.R) in tests/fixtures/flujo_antes.json;
# since step 2 the same cases read the engine's payload
# (cashflow_tabla_datos / _columnas through hd_filas, adapter
# tests/helpers/flujo_adaptador_motor.R) and the real cashflowPreviewServer()
# under testServer() (never S3: build_export_combined_df is replaced by fixed
# rows, as in flujo_fixture_app). Case numbers F1.. are those of
# reportes/T17c_paso0.md.
#
# Every exact number is the reference's. What the person reads changes only
# where the coordinator approved it, each marked [CAMBIA] with its reason:
#   F6   halves round up ("$121" for 120.5, "$3K" for 2,500), the engine's rule
#   T    the Total in whole pesos with the same rule (formato "entero")
#   S    the minus is the engine's "-" (it was "−")
#   TIT  "COBROS — MXN" / "SUBTOTAL COBROS — MXN" are the band (MXN) and the
#        group's row ("Cobros"), which is also its subtotal
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
source("tests/helpers/flujo_adaptador_motor.R")

LUNES <- lubridate::floor_date(Sys.Date(), "week", week_start = 1)
D <- flujo_datos_prueba(LUNES)
datos <- function(desde = LUNES, hasta = LUNES + 6L, agrupacion = "auto", modo = "separate", base = "MXN", fx = c())
  build_cashflow_export_data(D$df, D$tags, D$empresas, desde, hasta, agrupacion, modo, base, fx)
ver <- function(ed, expandido = FALSE, day0 = list()) fl_rejilla_motor(ed, day0 = day0, expandido = expandido)
fl_bloque <- function(r, moneda) { for (b in r$bloques) if (identical(b$moneda, moneda)) return(b); NULL }
fl_fila <- function(r, moneda, seccion, parte) {
  for (f in fl_bloque(r, moneda)$secciones[[seccion]]$filas) if (identical(f$parte, parte)) return(f)
  NULL
}
ED <- datos()
R0 <- ver(ED, day0 = list(MXN = 1500))
celda <- function(x, k) x$celdas[[paste0("c", k)]]
textos <- function(x) unname(vapply(x$celdas, `[[`, character(1), "texto"))
valores <- function(x) unname(vapply(x$celdas, `[[`, numeric(1), "valor"))

# ── Columns and blocks ──────────────────────────────────────────────────────
test_that("F1: Contraparte, (its tag), Día 0, one column per period, Total; daily up to 14 days, weekly above, or as chosen", {
  expect_identical(R0$encabezado[c(1, 3, length(R0$encabezado))], c("Contraparte", "Día 0", "Total"))
  expect_identical(length(R0$columnas), 7L)
  expect_identical(R0$encabezado[4:10], R0$columnas)
  expect_identical(datos(hasta = LUNES + 13L)$grouping_used, "daily")
  expect_identical(datos(hasta = LUNES + 14L)$grouping_used, "weekly")
  expect_identical(datos(agrupacion = "weekly")$grouping_used, "weekly")
  expect_identical(length(datos(hasta = LUNES + 20L)$bucket_labels), 3L)
  expect_identical(datos(hasta = LUNES + 20L, agrupacion = "daily")$grouping_used, "daily")
})
test_that("F2: one block per currency, in order, each with COBROS then PAGOS [CAMBIA TIT: the band and the group say it]", {
  expect_identical(vapply(R0$bloques, `[[`, character(1), "moneda"), c("MXN", "USD"))
  expect_identical(names(fl_bloque(R0, "MXN")$secciones), c("cobros", "pagos"))
  # [CAMBIA TIT] was "COBROS — MXN" / "PAGOS — USD": now band MXN / USD, group Cobros / Pagos.
  expect_identical(fl_bloque(R0, "MXN")$secciones$cobros$titulo, "Cobros")
  expect_identical(fl_bloque(R0, "USD")$secciones$pagos$titulo, "Pagos")
  expect_identical(unique(R0$datos$Moneda), c("MXN", "USD"))
})
test_that("F3: a row per counterparty, alphabetical; a provision is its own row, marked; tags marked", {
  cob <- fl_bloque(R0, "MXN")$secciones$cobros$filas
  expect_identical(vapply(cob, `[[`, character(1), "parte"), c("CLIENTE ALFA", "CLIENTE BETA", "CLIENTE GAMA"))
  expect_identical(vapply(cob, `[[`, logical(1), "provision"), c(FALSE, FALSE, TRUE))
  expect_identical(vapply(cob, `[[`, logical(1), "urgente"), c(TRUE, FALSE, FALSE))
  expect_identical(vapply(cob, `[[`, logical(1), "importante"), c(FALSE, TRUE, FALSE))
})
test_that("F4: out of the range and other companies never show", {
  expect_false(any(c("FUERA ANTES", "FUERA DESPUES", "OTRA EMPRESA") %in% R0$datos$Contraparte))
})
test_that("F5: the counterparties are a group (Moneda + Tipo) that starts closed; net and position are summary rows", {
  # Closed / open is the browser's (chromote B2); here, that the rows make the groups.
  d <- R0$datos
  expect_identical(unique(paste(d$Moneda, d$Tipo)[!nzchar(d$.resumen)]), c("MXN Cobros", "MXN Pagos", "USD Cobros", "USD Pagos"))
  expect_identical(d$.resumen[nzchar(d$.resumen)], c("neto", "acumulada", "neto", "acumulada"))
})

# ── Amounts ─────────────────────────────────────────────────────────────────
test_that("F6: cells compact ($4.5M / $802K / $950, empty for 0); Total in whole pesos [CAMBIA F6: halves up]", {
  alfa <- fl_fila(R0, "MXN", "cobros", "CLIENTE ALFA")
  expect_identical(textos(alfa), c("$4.5M", "", "$40K", "", "", "", ""))
  expect_identical(alfa$total$texto, "$4,535,962")
  expect_identical(textos(fl_fila(R0, "MXN", "cobros", "CLIENTE GAMA"))[4], "$950")
  # [CAMBIA F6] (coordinator, approved): halves go UP now, the engine's rule.
  # Before: 120.5 read "$120" (formatC to even) next to its Total "$5,000,121",
  # and 2,500 read "$2K" like 1,500. The exact number doesn't change.
  uno <- fl_fila(R0, "MXN", "pagos", "PROVEEDOR UNO")
  expect_identical(c(textos(uno)[5], uno$total$texto), c("$121", "$5,000,121"))
  expect_identical(celda(uno, 5)$valor, 120.5)
  expect_identical(textos(fl_fila(R0, "USD", "pagos", "VENDOR USA"))[3], "$3K")
})
test_that("F7: each group's row adds up its rows per column; its Total in whole pesos", {
  s <- fl_bloque(R0, "MXN")$secciones$cobros$subtotal
  expect_identical(s$titulo, "Cobros")   # [CAMBIA TIT] was "SUBTOTAL COBROS — MXN"
  expect_identical(valores(s), c(4495962, 802000, 40000, 950, 0, 0, 0))
  expect_identical(textos(s), c("$4.5M", "$802K", "$40K", "$950", "", "", ""))
  expect_identical(s$total$texto, "$5,338,912")
})
test_that("F8: FLUJO NETO = cobros - pagos per column, a 0 shows $0, green / red / grey by sign; Total whole [CAMBIA S]", {
  n <- fl_bloque(R0, "MXN")$neto
  expect_identical(n$titulo, "FLUJO NETO MXN")
  expect_identical(valores(n), c(-504038, 0, 40000, 950, -120.5, 0, 0))
  # [CAMBIA S] the minus was "−"; [CAMBIA F6] -120.5 read "−$120".
  expect_identical(textos(n), c("-$504K", "$0", "$40K", "$950", "-$121", "$0", "$0"))
  expect_identical(unname(vapply(n$celdas, `[[`, "", "tono")), c("peligro", "neutro", "exito", "exito", "peligro", "neutro", "neutro"))
  expect_identical(c(n$total$texto, n$total$tono), c("-$463,209", "peligro"))
})
test_that("F9: POS. ACUMULADA = Día 0 + running net; the last one is its Total", {
  a <- fl_bloque(R0, "MXN")$acumulada
  expect_identical(a$titulo, "POS. ACUMULADA MXN")
  expect_identical(a$dia0, list(texto = "$2K", valor = 1500))
  expect_identical(valores(a), 1500 + cumsum(c(-504038, 0, 40000, 950, -120.5, 0, 0)))
  expect_identical(a$total$texto, "-$461,709")
  expect_identical(a$total$valor, valores(a)[7])
})
test_that("F10: Día 0 is per currency: MXN's doesn't move USD's; only the position rows can be typed in", {
  u <- fl_bloque(R0, "USD")$acumulada
  expect_identical(u$dia0$valor, 0)
  expect_identical(u$dia0$texto, "$0")
  expect_identical(valores(u), cumsum(c(0, 10000, -2500, 0, 0, 0, 0)))
  expect_identical(R0$datos$id[R0$datos$.dia0], c("acum|MXN", "acum|USD"))
  expect_identical(unclass(R0$filas$celdas[[3]]$edf), R0$datos$.dia0)
})
test_that("F11: Base (fused): one block in the base currency, the others converted at their rate", {
  e <- datos(modo = "fused", base = "MXN", fx = c(USD = 20))
  expect_identical(e$currencies, "MXN")
  r <- ver(e)
  expect_identical(length(r$bloques), 1L)
  expect_identical(celda(fl_fila(r, "MXN", "cobros", "CLIENT USA"), 2)$valor, 200000)
  expect_identical(valores(fl_bloque(r, "MXN")$neto)[2], 0 + 200000)
})
test_that("F12: nothing in the range: no data, the table has no rows (it then says 'Sin movimientos ...')", {
  expect_null(datos(desde = LUNES + 30L, hasta = LUNES + 36L))
  expect_identical(nrow(cashflow_tabla_datos(NULL)), 0L)
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
    session$elapse(500)   # the data is debounced 400 ms
    cuerpo(session, input, output, sh, rv)
  }))
}
devuelto <- function(session) session$getReturned()

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
test_that("F15: Día 0 is typed in the position row, per currency; what isn't the position row is ignored", {
  modulo(function(session, input, output, sh, rv) {
    r <- devuelto(session)
    session$setInputs(`tabla-ediciones` = list(ids = list("acum|MXN"), valores = list(1500), nonce = 1))
    expect_identical(r$day0()[["MXN"]], 1500)
    d <- r$datos()
    expect_identical(d$Dia0[d$id == "acum|MXN"], 1500)
    expect_identical(d$b1[d$id == "acum|MXN"], 1500 + d$b1[d$id == "neto|MXN"])
    session$setInputs(`tabla-ediciones` = list(ids = list("neto|USD", "MXN|ar|1"), valores = list(9, 9), nonce = 2))
    expect_identical(names(r$day0()), "MXN")
    expect_identical(r$day0()[["MXN"]], 1500)
  })
})
test_that("F16: opening and closing every group is the engine's (expandir); the module's own button is gone", {
  modulo(function(session, input, output, sh, rv) {
    expect_true("expandir" %in% hd_piezas_de("flujo_caja", c("sumas_columnas", "filas_resumen", "edicion_celda",
                                                              "dinero_compacto", "tono_celda", "expandir", "etiquetas")))
  })
  expect_false(grepl("btn_expand", paste(as.character(cashflowPreviewUI("cf")), collapse = ""), fixed = TRUE))
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
test_that("F19: the module gives the table the same rows as the pure function (it adds nothing to the numbers)", {
  modulo(function(session, input, output, sh, rv) {
    expect_identical(devuelto(session)$datos(), cashflow_tabla_datos(ED))
    expect_identical(vapply(devuelto(session)$columnas(), `[[`, "", "titulo"), R0$encabezado)
  })
})
test_that("F20: no rows in the range: the table gets none, and says 'Sin movimientos en el período seleccionado'", {
  modulo(function(session, input, output, sh, rv) {
    session$setInputs(date_range = c(as.character(LUNES + 30L), as.character(LUNES + 36L)))
    session$elapse(500)
    expect_identical(nrow(devuelto(session)$datos()), 0L)
  })
  expect_identical(tr("tablas.flujo.vacio"), "Sin movimientos en el período seleccionado")
})

# ── The whole grid, against the reference ───────────────────────────────────
# flujo_antes.json is step 0's (today's preview), never rewritten. The
# engine's grid must be it, with only the approved [CAMBIA] applied to it.
test_that("the whole grid of two scenarios is the reference's, but for the approved [CAMBIA] (texts; every exact number)", {
  ahora <- list(diario = ver(ED, expandido = TRUE, day0 = list(MXN = 1500, USD = -250)),
                semanal_base = ver(datos(hasta = LUNES + 20L, modo = "fused", fx = c(USD = 20)), expandido = TRUE))
  ahora <- lapply(ahora, function(r) r[c("bloques")])
  nuevo <- jsonlite::fromJSON(jsonlite::toJSON(ahora, auto_unbox = TRUE, null = "null", na = "null", digits = NA),
                              simplifyVector = FALSE)
  antes <- jsonlite::fromJSON("tests/fixtures/flujo_antes.json", simplifyVector = FALSE)
  # The approved [CAMBIA], applied to the reference.
  cambia <- function(x, ruta = character(0)) {
    if (is.list(x)) {
      for (nm in names(x)) x[[nm]] <- cambia(x[[nm]], c(ruta, nm))
      if (!is.null(names(x)) && "titulo" %in% names(x) && length(ruta) && ruta[length(ruta)] %in% c("cobros", "pagos", "subtotal"))
        x$titulo <- sub("^(SUBTOTAL )?(COBROS|PAGOS) — .*$", "\\2", x$titulo) |>
          (\(t) if (t == "COBROS") "Cobros" else if (t == "PAGOS") "Pagos" else t)()          # [CAMBIA TIT]
      if (is.null(names(x))) x <- lapply(seq_along(x), function(i) cambia(x[[i]], c(ruta, i)))
      return(x)
    }
    if (is.character(x)) x <- gsub("−", "-", x, fixed = TRUE)                             # [CAMBIA S]
    x
  }
  antes <- cambia(antes)
  # [CAMBIA F6] the only texts the new rounding moves in these two scenarios (halves up):
  f6 <- function(r) { r$texto <- c("$120" = "$121", "-$120" = "-$121", "$2K" = "$3K", "-$2K" = "-$3K")[[r$texto]]; r }
  b <- antes$diario$bloques
  b[[1]]$secciones$pagos$filas[[1]]$celdas$c5 <- f6(b[[1]]$secciones$pagos$filas[[1]]$celdas$c5)
  b[[1]]$secciones$pagos$subtotal$celdas$c5 <- f6(b[[1]]$secciones$pagos$subtotal$celdas$c5)
  b[[1]]$neto$celdas$c5 <- f6(b[[1]]$neto$celdas$c5)
  b[[2]]$secciones$pagos$filas[[1]]$celdas$c3 <- f6(b[[2]]$secciones$pagos$filas[[1]]$celdas$c3)
  b[[2]]$secciones$pagos$subtotal$celdas$c3 <- f6(b[[2]]$secciones$pagos$subtotal$celdas$c3)
  b[[2]]$neto$celdas$c3 <- f6(b[[2]]$neto$celdas$c3)
  antes$diario$bloques <- b
  expect_identical(nuevo, antes)
})

# ── Step 3 [CAMBIA] ─────────────────────────────────────────────────────────
test_that("[CAMBIA] Día 0 is kept in the browser per currency until it changes (it lasted the session)", {
  # Before: cf_day0_rv only, gone on reload. Now: written to localStorage
  # (hopdesk_cf_dia0) when typed and read back when the preview opens.
  modulo(function(session, input, output, sh, rv) {
    r <- devuelto(session)
    session$setInputs(dia0_guardado = list(MXN = 2700, USD = "x", mxn = 5, EUR = NULL))
    expect_identical(r$day0(), list(MXN = 2700))
    expect_identical(r$datos()$Dia0[r$datos()$id == "acum|MXN"], 2700)
    # What was typed in this session wins over what the browser kept.
    session$setInputs(`tabla-ediciones` = list(ids = list("acum|MXN"), valores = list(-750), nonce = 1))
    session$setInputs(dia0_guardado = list(MXN = 2700))
    expect_identical(r$day0()[["MXN"]], -750)
  })
  src <- paste(readLines("R/cashflow_preview_module.R", warn = FALSE), collapse = "\n")
  expect_true(grepl("localStorage.setItem('%s',%s)", src, fixed = TRUE) && grepl("\"hopdesk_cf_dia0\"", src, fixed = TRUE))
})
test_that("[CAMBIA] an Esc that a field or the date picker took doesn't close the preview (it did, B7)", {
  # The browser's (chromote B7, inverted); here, that the listener asks first.
  src <- paste(readLines("R/cashflow_preview_module.R", warn = FALSE), collapse = "\n")
  expect_true(grepl("if(tomada){ tomada = false; return; }", src, fixed = TRUE))
  expect_true(grepl(".air-datepicker.-active-", src, fixed = TRUE))
})
