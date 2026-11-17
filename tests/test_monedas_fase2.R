# =============================================================================
# tests/test_monedas_fase2.R -- monedas fase 2: Flujo's Exportar and preview
# with propuesta A (Luis, 2026-11-16; requisiciones/flujo_monedas_fase2_paso0.md).
#
#   1. the catalog checked against ISO (BGN out, version b);
#   2. fx_tipo(): typed > Banxico (FIX for USD) > crossed > none; never made up;
#   3. typed rates: per user, per base, today only, last one wins, ✕ undoes,
#      the table never shrinks;
#   4. the 1.0 bug: a currency without a rate stops the report (counter-test
#      against 6368b5d, where it was multiplied by 1.0);
#   5. Banxico's other currencies: own cache (never the FIX's), fresh only,
#      a failed Banxico invents nothing;
#   6. the preview converts with the FIX when nothing was typed (it used to
#      convert with 1.0), and a new base drops the old base's rates;
#   7. Exportar's rows: origin of each rate, "cambiar", typed, ✕.
# Pure: an in-memory table store, never S3, never Banxico.
# Run from project root:  Rscript tests/test_monedas_fase2.R
# =============================================================================
suppressPackageStartupMessages({ library(testthat); library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/persistence.R", c("tag_label", ".normalize"))
for (f in c("R/i18n.R", "R/monedas.R", "R/fx_banxico.R", "R/fx_tipos.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/cashflow_export_module.R", "R/cashflow_preview_module.R")) source(f)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
source("tests/helpers/flujo_datos_prueba.R")

# ── In-memory store (local to this test; aws.s3 is never touched) ───────────
ALMACEN <- new.env()
modificar_tabla <- function(key, client_id, schema, cambio, permitir_reduccion = FALSE) {
  if (is.null(client_id) || !nzchar(client_id)) stop("modificar_tabla: sin client_id", call. = FALSE)
  k <- paste(client_id, key)
  fresco <- .normalize(ALMACEN[[k]], schema)
  nuevo  <- .normalize(cambio(fresco), schema)
  if (!isTRUE(permitir_reduccion) && nrow(nuevo) < nrow(fresco)) stop("reducción no permitida", call. = FALSE)
  ALMACEN[[k]] <- nuevo
  invisible(nuevo)
}
.s3_read_with <- function(key, client_id = NULL) ALMACEN[[paste(client_id, key)]]

HOY <- .fx_hoy()
DIV <- data.frame(fecha  = as.Date(c("2026-11-13", "2026-11-13", "2026-11-13", "2026-11-12", "2026-11-12", "2026-11-12")),
                  moneda = c("USD", "EUR", "GBP", "USD", "EUR", "JPY"),
                  valor  = c(18.5525, 20.5085, 24.5515, 18.5, 20.4, 0.1150), stringsAsFactors = FALSE)
FIX <- list(valor = 18.5525, fecha = as.Date("2026-11-13"))

# ── 1. Catalog ──────────────────────────────────────────────────────────────
test_that("el catálogo, cotejado contra ISO: sin BGN, versión 2026-11-16b, 155 monedas", {
  expect_identical(MONEDAS_VERSION, "2026-11-16b")
  expect_false("BGN" %in% monedas_catalogo())
  expect_identical(length(monedas_catalogo()), 155L)
  cab <- readLines(.MONEDAS_ARCHIVO, n = 3, encoding = "UTF-8")
  expect_true(any(grepl("2026-09-17", cab, fixed = TRUE)))
})

# ── 2. fx_tipo ──────────────────────────────────────────────────────────────
test_that("fx_tipo: la base es 1; lo escrito gana; USD/MXN es el FIX; las demás, Banxico", {
  expect_identical(fx_tipo("EUR", "EUR")[c("valor", "origen")], list(valor = 1, origen = "base"))
  t <- fx_tipo("EUR", "MXN", escritos = c(EUR = 21), fix = FIX, divisas = DIV)
  expect_identical(t[c("valor", "origen")], list(valor = 21, origen = "escrito"))
  for (malo in list(c(EUR = 0), c(EUR = NA), c(EUR = -3)))
    expect_identical(fx_tipo("EUR", "MXN", escritos = malo, divisas = DIV)$origen, "banxico")
  expect_identical(fx_tipo("USD", "MXN", fix = FIX, divisas = DIV),
                   list(valor = 18.5525, origen = "banxico_fix", fecha = as.Date("2026-11-13")))
  # USD/MXN is the FIX only: without one, none (SF46405 is for crossing)
  expect_identical(fx_tipo("USD", "MXN", fix = NULL, divisas = DIV)$origen, "sin_fuente")
  expect_identical(fx_tipo("EUR", "MXN", fix = FIX, divisas = DIV),
                   list(valor = 20.5085, origen = "banxico", fecha = as.Date("2026-11-13")))
  expect_identical(fx_tipo("JPY", "MXN", divisas = DIV)$fecha, as.Date("2026-11-12"))
})
test_that("fx_tipo: con base distinta del peso, cruzado con dos datos de Banxico del mismo día", {
  t <- fx_tipo("EUR", "USD", fix = FIX, divisas = DIV)
  expect_identical(t$origen, "cruzado"); expect_identical(t$fecha, as.Date("2026-11-13"))
  expect_equal(t$valor, 20.5085 / 18.5525)
  j <- fx_tipo("JPY", "USD", divisas = DIV)                 # JPY only on 12/11: crossed with USD of 12/11
  expect_identical(j$fecha, as.Date("2026-11-12")); expect_equal(j$valor, 0.1150 / 18.5)
  m <- fx_tipo("MXN", "USD", divisas = DIV)
  expect_identical(m$origen, "cruzado"); expect_equal(m$valor, 1 / 18.5525)
})
test_that("fx_tipo: sin fuente es NA, nunca un número inventado (ni 1.0 ni los pares viejos)", {
  for (par in list(c("CHF", "MXN"), c("EUR", "CHF"), c("SEK", "USD"), c("AUD", "MXN")))
    expect_identical(fx_tipo(par[1], par[2], fix = FIX, divisas = DIV)$valor, NA_real_)
  expect_identical(fx_tipo("EUR", "MXN", fix = FIX, divisas = NULL)$origen, "sin_fuente")
  expect_identical(fx_tipo(NA, "MXN")$origen, "sin_fuente")
  expect_identical(fx_tipo("eur", "mxn", divisas = DIV)$origen, "banxico")    # case and spaces don't matter
  t <- fx_tipos(c("USD", "EUR", "CHF"), "MXN", fix = FIX, divisas = DIV)
  expect_identical(t$origen, c("banxico_fix", "banxico", "sin_fuente"))
  expect_identical(fx_tipos_valores(t), c(USD = 18.5525, EUR = 20.5085))
})
test_that("el origen se dice desde el diccionario, con la fecha", {
  expect_identical(fx_origen_texto("banxico", as.Date("2026-11-13"), "es"), "Banxico · 13 nov")
  expect_identical(fx_origen_texto("cruzado", as.Date("2026-11-13"), "en"), "Banxico (crossed) · 13 Nov")
  expect_identical(fx_origen_texto("escrito", NA, "es"), "Escrito por ti · hoy")
  expect_identical(fx_origen_texto("sin_fuente", NA, "es"), "Sin fuente: escríbelo")
})

# ── 3. Typed rates ──────────────────────────────────────────────────────────
test_that("lo escrito: por usuario, por base, solo hoy; el último gana; ✕ lo quita; la tabla nunca se encoge", {
  rm(list = ls(ALMACEN), envir = ALMACEN)
  fx_escrito_guardar("cli", "ana", "EUR", "MXN", 21)
  fx_escrito_guardar("cli", "ana", "SEK", "MXN", 1.95)
  tabla <- fx_escritos_leer("cli")
  expect_identical(fx_escritos_de(tabla, "ana", "MXN"), c(EUR = 21, SEK = 1.95))
  expect_length(fx_escritos_de(tabla, "beto", "MXN"), 0L)                       # another user
  expect_length(fx_escritos_de(tabla, "ana", "USD"), 0L)                        # another base
  expect_length(fx_escritos_de(tabla, "ana", "MXN", ahora = Sys.time() + 86400 * 2), 0L)   # another day
  expect_length(fx_escritos_de(fx_escritos_leer("otro_cliente"), "ana", "MXN"), 0L)        # another client
  fx_escrito_guardar("cli", "ana", "EUR", "MXN", 22)
  expect_identical(fx_escritos_de(fx_escritos_leer("cli"), "ana", "MXN")[["EUR"]], 22)
  fx_escrito_guardar("cli", "ana", "EUR", "MXN", NA)                            # the ✕
  expect_identical(fx_escritos_de(fx_escritos_leer("cli"), "ana", "MXN"), c(SEK = 1.95))
  expect_identical(nrow(fx_escritos_leer("cli")), 4L)                           # only appended
})
test_that("guardar rechaza lo inválido (y no escribe)", {
  rm(list = ls(ALMACEN), envir = ALMACEN)
  expect_error(fx_escrito_guardar(NULL, "ana", "EUR", "MXN", 21))
  expect_error(fx_escrito_guardar("cli", "", "EUR", "MXN", 21))
  expect_error(fx_escrito_guardar("cli", "ana", "EUR", "EUR", 21))
  expect_error(fx_escrito_guardar("cli", "ana", "EURO", "MXN", 21))
  expect_error(fx_escrito_guardar("cli", "ana", "EUR", "MXN", 0))
  expect_error(fx_escrito_guardar("cli", "ana", "EUR", "MXN", Inf))
  expect_length(ls(ALMACEN), 0L)
  expect_length(fx_escritos_de(fx_escritos_leer(NULL), "ana", "MXN"), 0L)       # no client: none
})

# ── 4. The 1.0 bug ──────────────────────────────────────────────────────────
df_sek <- tibble::tibble(Moneda = c("MXN", "SEK", "USD"), `Saldo vencido` = c(1000, 100000, 10))
test_that("una moneda sin tipo detiene el reporte y dice cuál; con tipo se convierte", {
  e <- tryCatch(.apply_fx_conversion(df_sek, "MXN", c(USD = 18)), error = function(e) conditionMessage(e))
  expect_identical(e, tr("tablas.flujo.aviso.tc_faltante", monedas = "SEK/MXN"))
  expect_error(.apply_fx_conversion(df_sek, "MXN", c(USD = 18, SEK = 0)))
  expect_error(.apply_fx_conversion(df_sek, "MXN", list(USD = 18, SEK = NA)))
  r <- .apply_fx_conversion(df_sek, "MXN", list(USD = 18, SEK = 1.95))
  expect_identical(r$`Saldo vencido`, c(1000, 195000, 180))
  expect_identical(r$.is_converted, c(FALSE, TRUE, TRUE))
  expect_identical(unique(r$Moneda), "MXN")
  expect_identical(.apply_fx_conversion(df_sek[1, ], "MXN", c())$`Saldo vencido`, 1000)   # nothing to convert
})
test_that("contraprueba: en 6368b5d la SEK se sumaba por 1.0, sin avisar", {
  viejo <- tryCatch(system2("git", c("show", "6368b5d:R/cashflow_export_module.R"), stdout = TRUE, stderr = FALSE),
                    error = function(e) NULL, warning = function(w) NULL)
  skip_if(is.null(viejo) || !length(viejo), "git no disponible")
  env <- new.env(parent = globalenv())
  for (e in parse(text = viejo, keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".apply_fx_conversion")) eval(e, env)
  r <- env$.apply_fx_conversion(df_sek, "MXN", list(USD = 18))
  expect_identical(unname(r$`Saldo vencido`[2]), 100000)                         # 100,000 SEK became 100,000 MXN
})

# ── 5. Banxico's other currencies ───────────────────────────────────────────
test_that("divisas de Banxico: caché propio, solo vigentes, y un Banxico caído no inventa nada", {
  viejo_token <- Sys.getenv("BANXICO_TOKEN", NA)
  on.exit(if (is.na(viejo_token)) Sys.unsetenv("BANXICO_TOKEN") else Sys.setenv(BANXICO_TOKEN = viejo_token), add = TRUE)
  Sys.setenv(BANXICO_TOKEN = "prueba")
  ahora <- as.POSIXct("2026-11-16 15:00", tz = FX_FIX_TZ)                       # Monday after the FIX: expects 16/11
  CACHE <- data.frame(fecha = as.Date(c("2026-11-13", "2026-11-13", "2026-11-05")), moneda = c("EUR", "XYZ", "GBP"),
                      serie = "s", valor = c(20.5085, 9, 24), obtenido_en = "", stringsAsFactors = FALSE)
  llaves <- character(); pedidas <- character()
  assign("leer_hd_admin", function(key, schema_fn) { llaves <<- c(llaves, key); CACHE }, envir = globalenv())
  assign("modificar_hd_admin", function(key, schema_fn, cambio) { llaves <<- c(llaves, key); CACHE <<- cambio(CACHE); CACHE },
         envir = globalenv())
  # Banxico down
  assign("fcs_fetch_banxico", function(metric, desde, hasta) { pedidas <<- c(pedidas, metric$fetch_params[[1]]$serie)
    list(status = "http_error", rows = NULL) }, envir = globalenv())
  .fx_divisas_estado$intento <- NULL
  d <- .fx_divisas_calcular(ahora)
  expect_identical(sort(pedidas), sort(unname(FX_DIVISAS_SERIES)))               # asked for every series once
  expect_identical(d$moneda, "EUR")                                             # 13/11 still fresh; 05/11 too old; XYZ never
  expect_true(all(llaves == FX_DIVISAS_LLAVE))                                  # never the FIX's cache
  expect_false(FX_DIVISAS_LLAVE == FX_FIX_LLAVE)
  # within the hour: no new request
  pedidas <- character(); invisible(.fx_divisas_calcular(ahora + 600)); expect_length(pedidas, 0L)
  # Banxico answers (one "N/E" figure): the new rows are added, the cache only grows
  assign("fcs_fetch_banxico", function(metric, desde, hasta) {
    s <- metric$fetch_params[[1]]$serie
    v <- if (s == FX_DIVISAS_SERIES[["GBP"]]) NA else 20.6
    list(status = "ok", rows = tibble::tibble(fecha = as.Date("2026-11-16"), value = v)) }, envir = globalenv())
  .fx_divisas_estado$intento <- NULL
  d <- .fx_divisas_calcular(ahora)
  expect_true(all(c("USD", "EUR", "JPY", "CNY", "CAD") %in% d$moneda[d$fecha == as.Date("2026-11-16")]))
  expect_false("GBP" %in% d$moneda)
  expect_identical(nrow(CACHE), 3L + 5L)
  # no token: no request, no figure made up
  Sys.unsetenv("BANXICO_TOKEN"); CACHE <- CACHE[0, ]; pedidas <- character(); .fx_divisas_estado$intento <- NULL
  assign("fcs_fetch_banxico", function(...) { pedidas <<- c(pedidas, "x"); stop("no debería llamarse") }, envir = globalenv())
  expect_identical(nrow(.fx_divisas_calcular(ahora)), 0L); expect_length(pedidas, 0L)
  # an unreadable cache: nothing (and fx_tipo says none)
  assign("leer_hd_admin", function(key, schema_fn) stop("S3 caído"), envir = globalenv())
  expect_identical(nrow(.fx_divisas_calcular(ahora)), 0L)
})

# ── 6. The preview ──────────────────────────────────────────────────────────
D <- flujo_datos_prueba(lubridate::floor_date(Sys.Date(), "week", week_start = 1))
assign("build_export_combined_df", function(shared, ic_mode_val) D$df, envir = globalenv())
assign("fx_fix", function(...) FIX, envir = globalenv())
assign("fx_divisas", function(...) DIV[0, ], envir = globalenv())
llamadas <- list()
original_bced <- build_cashflow_export_data
assign("build_cashflow_export_data", function(...) { a <- list(...); llamadas[[length(llamadas) + 1L]] <<- a[c("base_cur", "fx_rates", "currency_mode")]
  original_bced(...) }, envir = globalenv())
nuevo_shared <- function() list(
  cf_preview_open = reactiveVal(TRUE), cf_preview_prefill = reactiveVal(NULL),
  currency_choices = list(AR = function() c("MXN", "USD"), AP = function() c("MXN", "USD")),
  ic_mode = list(AR = function() "exclude"), tags_db = function() D$tags, empresa_sel = function() D$empresas,
  effective_client_id = reactiveVal("cliente_a"), current_user = function() "ana")
test_that("vista previa: sin escribir nada convierte con el FIX (antes: 1.0); lo escrito queda del día; otra base no reusa tipos", {
  rm(list = ls(ALMACEN), envir = ALMACEN)
  suppressWarnings(testServer(cashflowPreviewServer, args = list(shared = nuevo_shared()), {
    session$setInputs(date_range = c(as.character(D$lunes), as.character(D$lunes + 6L)))
    session$elapse(500)
    llamadas <<- list()
    session$setInputs(btn_curmode = 1); session$elapse(500)
    ultima <- llamadas[[length(llamadas)]]
    expect_identical(ultima$currency_mode, "fused")
    expect_identical(ultima$fx_rates, c(USD = 18.5525))                         # 6368b5d: c() -> USD at 1.0
    fx <- as.character(output$fx_section_ui$html)
    expect_true(grepl("Banxico FIX", fx, fixed = TRUE))
    session$setInputs(fx_rate_usd_blur = 19); session$elapse(500)
    expect_identical(llamadas[[length(llamadas)]]$fx_rates, c(USD = 19))
    expect_identical(fx_escritos_de(fx_escritos_leer("cliente_a"), "ana", "MXN"), c(USD = 19))   # kept for the day
    expect_true(grepl("Escrito por ti", as.character(output$fx_section_ui$html), fixed = TRUE))
    session$setInputs(base_cur = "USD"); session$elapse(500)
    expect_identical(rv$base_cur, "USD"); expect_length(rv$fx_rates, 0L)        # the MXN-base rate is dropped
    ultima <- llamadas[[length(llamadas)]]
    expect_identical(ultima$base_cur, "USD")
    expect_false("MXN" %in% names(ultima$fx_rates))                              # no Banxico USD here: none, never 1/19
    expect_true(grepl("Sin fuente", as.character(output$fx_section_ui$html), fixed = TRUE))
  }))
})

# ── 7. Exportar ─────────────────────────────────────────────────────────────
exportar <- function(id, shared) moduleServer(id, function(input, output, session)
  setup_cashflow_export_server(input, output, session, shared))
sh_exp <- function() list(
  sap_data = function() list(AR = data.frame(Empresa = "NETWORKS TRANS", Moneda = c("MXN", "USD", "SEK", "X');alert(2);//")),
                             AP = data.frame(Empresa = "OTRA SA", Moneda = "CHF")),
  manual_inv = function() NULL, empresa_sel = function() "NETWORKS TRANS", company_map = function() c(NT = "NETWORKS TRANS"),
  effective_client_id = function() "cliente_a", current_user = function() "ana",
  cf_preview_prefill = reactiveVal(NULL))
test_that("Exportar: una fila por moneda de los datos con su origen; escribir, quitar y cambiar", {
  rm(list = ls(ALMACEN), envir = ALMACEN)
  suppressWarnings(testServer(exportar, args = list(shared = sh_exp()), {
    # ignoreInit: the first value may be taken as the start; a second click then toggles
    session$setInputs(btn_toggle_cur_mode = 1)
    if (is.null(output$export_fx_section_ui)) session$setInputs(btn_toggle_cur_mode = 2)
    h <- as.character(output$export_fx_section_ui$html)
    expect_true(grepl("USD/MXN", h, fixed = TRUE) && grepl("Banxico FIX", h, fixed = TRUE))
    expect_true(grepl("SEK/MXN", h, fixed = TRUE) && grepl("Sin fuente", h, fixed = TRUE))
    expect_true(grepl("export_fx_SEK_MXN", h, fixed = TRUE))                     # its field, asking
    expect_false(grepl("export_fx_USD_MXN", h, fixed = TRUE))                    # Banxico's: shown, not a field
    expect_false(grepl("CHF", h, fixed = TRUE))                                  # another company's currency
    expect_false(grepl("alert", h, fixed = TRUE))                                # a non-code "currency" in the data: no row
    expect_true(grepl("MXN — Peso mexicano", h, fixed = TRUE))                   # the base, from the client (MXN by default)
    session$setInputs(export_fx_cmd = list(cur = "SEK", base = "MXN", accion = "escribir", v = 1.95, t = 1))
    h <- as.character(output$export_fx_section_ui$html)
    expect_true(grepl("Escrito por ti", h, fixed = TRUE) && grepl("1.9500", h, fixed = TRUE))
    expect_identical(fx_escritos_de(fx_escritos_leer("cliente_a"), "ana", "MXN"), c(SEK = 1.95))
    session$setInputs(export_fx_cmd = list(cur = "SEK", accion = "quitar", t = 2))
    expect_true(grepl("Sin fuente", as.character(output$export_fx_section_ui$html), fixed = TRUE))
    session$setInputs(export_fx_cmd = list(cur = "USD", accion = "cambiar", t = 3))
    expect_true(grepl("export_fx_USD_MXN", as.character(output$export_fx_section_ui$html), fixed = TRUE))
    # a value typed against another base is not taken
    session$setInputs(export_fx_cmd = list(cur = "SEK", base = "USD", accion = "escribir", v = 0.1, t = 4))
    expect_length(fx_escritos_de(fx_escritos_leer("cliente_a"), "ana", "USD"), 0L)
    # the HTML carries no script built from data: currency codes are 3 letters
    session$setInputs(export_fx_cmd = list(cur = "X');alert(1);//", accion = "cambiar", t = 5))
    expect_false(grepl("alert", as.character(output$export_fx_section_ui$html), fixed = TRUE))   # neither from input nor from data
  }))
})
