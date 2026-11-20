# =============================================================================
# tests/test_hd_numero_input.R
# T66: hd_numero_input() (R/ui_components.R + www/hd_numero.js), without a
# browser:
#   - the markup: a text field (inputmode, no type="number"), the same id,
#     the initial value in its format, formato required;
#   - hd_numero_texto(): monto with separator and its decimals, tasa and
#     entero with neither;
#   - hd_numero_update(): the message (value, NA empties, label, min, max);
#   - www/hd_numero.js's reading, run in node when node is installed:
#     "608,763.35" -> 608763.35, "1,5" / "abc" / "" -> null, rounding.
# The browser half: tests/test_hd_numero_input_chromote.R.
# Run from the repo root:  Rscript tests/test_hd_numero_input.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/i18n.R", "R/ui_components.R")) suppressMessages(source(f))
html <- function(x) paste(as.character(x), collapse = "")

test_that("el campo es de texto, con el mismo id y sin flechas", {
  h <- html(hd_numero_input("pwiz_tasa_spread", "Sobretasa (bps)", "entero", value = 200, min = 0))
  expect_match(h, 'id="pwiz_tasa_spread"', fixed = TRUE)
  expect_match(h, 'type="text"', fixed = TRUE)
  expect_match(h, 'inputmode="numeric"', fixed = TRUE)
  expect_match(h, 'class="form-control hd-numero"', fixed = TRUE)
  expect_match(h, 'value="200"', fixed = TRUE)
  expect_match(h, 'data-formato="entero"', fixed = TRUE)
  expect_match(h, 'data-min="0"', fixed = TRUE)
  expect_false(grepl('type="number"', h, fixed = TRUE))
  expect_false(grepl("shiny-input-number", h, fixed = TRUE))   # never Shiny's own binding
  expect_match(html(hd_numero_input("m", "Monto", "monto")), 'inputmode="decimal"', fixed = TRUE)
})

test_that("el valor inicial se ve en su formato", {
  expect_match(html(hd_numero_input("m", "Monto", "monto", value = 608763.35)), 'value="608,763.35"', fixed = TRUE)
  expect_match(html(hd_numero_input("m", "Monto", "monto", value = NA)), 'value=""', fixed = TRUE)
  expect_match(html(hd_numero_input("t", "Tasa", "tasa", value = 12.5)), 'value="12.5"', fixed = TRUE)
  expect_match(html(hd_numero_input("t", "Tasa", "tasa", value = 0.1234)), 'value="0.1234"', fixed = TRUE)
})

test_that("formato es obligatorio y solo monto, tasa o entero", {
  expect_error(hd_numero_input("x", "X"), "formato")
  expect_error(hd_numero_input("x", "X", 5), "formato")          # a value passed where formato goes
  expect_error(hd_numero_input("x", "X", "moneda"), "formato")
  expect_error(hd_numero_input("x", "X", "tasa", decimales = -1), "decimales")
})

test_that("sin etiqueta, como numericInput(label = NULL)", {
  h <- html(hd_numero_input("x", NULL, "tasa"))
  expect_match(h, "shiny-label-null", fixed = TRUE)
})

test_that("hd_numero_texto(): monto con separador; tasa y entero sin él", {
  expect_identical(hd_numero_texto(1234567.891, "monto"), "1,234,567.89")
  expect_identical(hd_numero_texto(-1234.5, "monto"), "-1,234.50")
  expect_identical(hd_numero_texto(5, "monto", 0), "5")
  expect_identical(hd_numero_texto(1234.5, "tasa"), "1234.5")
  expect_identical(hd_numero_texto(0.12345, "tasa", 2), "0.12")
  expect_identical(hd_numero_texto(45, "entero"), "45")
  expect_identical(hd_numero_texto(NA, "tasa"), "")
  expect_identical(hd_numero_texto(NULL, "monto"), "")
})

test_that("hd_numero_update() manda el número, o vaciar con NA", {
  enviados <- list()
  ses <- list(sendInputMessage = function(id, msg) enviados[[length(enviados) + 1L]] <<- list(id = id, msg = msg))
  hd_numero_update(ses, "bal_NRS", value = 1234.5)
  hd_numero_update(ses, "bal_NRS", value = NA)
  hd_numero_update(ses, "bal_NRS", label = "Saldo", min = 0, max = NA)
  expect_identical(enviados[[1]], list(id = "bal_NRS", msg = list(value = 1234.5)))
  expect_identical(enviados[[2]]$msg, list(vaciar = TRUE))
  expect_identical(enviados[[3]]$msg, list(label = "Saldo", min = "0", max = ""))
})

test_that("www/hd_numero.js lee los números como dice el paso 0 (node)", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "node no está instalado")
  js <- tempfile(fileext = ".js")
  writeLines(c(
    "global.window = {};",
    sprintf("eval(require('fs').readFileSync(%s, 'utf8'));", jsonlite::toJSON(normalizePath("www/hd_numero.js", winslash = "/"), auto_unbox = TRUE)),
    "var I = window.hdNumero.interpretar, T = window.hdNumero.texto;",
    "var casos = [['608,763.35','monto',2],['608763.35','monto',2],['32,000.50','monto',2],['-5','monto',2],",
    "  ['1,5','monto',2],['abc','monto',2],['','monto',2],['12,34','monto',2],['1.005','monto',2],",
    "  ['0.125','tasa',2],['12.5','tasa',2],['5.5','entero',0],['7','entero',0],['1,234,567','entero',0],['-0','monto',2]];",
    "console.log(JSON.stringify({ lee: casos.map(function (c) { return I(c[0], c[1], c[2]); }),",
    "  texto: [T(608763.35,'monto',2), T(0.1234,'tasa',4), T(200,'tasa',2), T(null,'monto',2)] }));"), js)
  r <- jsonlite::fromJSON(system2(node, js, stdout = TRUE), simplifyVector = FALSE)   # null stays NULL
  expect_identical(r$lee[1:4], list(608763.35, 608763.35, 32000.5, -5L))
  expect_true(all(vapply(r$lee[5:8], is.null, logical(1))))          # "1,5", "abc", "", "12,34"
  expect_equal(r$lee[[9]], 1.01)                                     # a monto rounds (exactly)
  expect_null(r$lee[[10]])                                           # a tasa with too many decimals
  expect_equal(r$lee[[11]], 12.5)
  expect_null(r$lee[[12]])                                           # an entero with decimals
  expect_equal(r$lee[[13]], 7)
  expect_equal(r$lee[[14]], 1234567)
  expect_equal(r$lee[[15]], 0)
  expect_identical(unlist(r$texto), c("608,763.35", "0.1234", "200", ""))
})
