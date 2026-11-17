# =============================================================================
# tests/test_motor_secciones_orden.R
# T33 (monedas fase 3): a section's `orden` may be a function, called at each
# payload, so a table can put the client's base currency first even when the
# base arrives after the server started. A plain vector works as before.
# Run from project root:  Rscript tests/test_motor_secciones_orden.R
# HD_TABLE_R = another copy of R/hd_table.R (contraprueba).
# =============================================================================
suppressPackageStartupMessages(library(testthat))
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}
source("R/i18n.R")
source("R/hd_registro.R")
source("R/hd_piezas.R")
source("R/hd_columnas.R")
source(Sys.getenv("HD_TABLE_R", "R/hd_table.R"))

d <- data.frame(Moneda = c("EUR", "MXN", "USD", "MXN"), stringsAsFactors = FALSE)
valores <- function(pl) vapply(pl[[1]]$opciones, `[[`, character(1), "valor")

test_that("orden as a function is accepted and read at each payload", {
  primero <- "MXN"
  s <- hd_secciones_validar(list(list(campo = "Moneda", orden = function() primero)))
  expect_identical(valores(hd_secciones_payload(s, d, list())), c("MXN", "EUR", "USD"))
  primero <- c("USD", "EUR")          # the base changed between two payloads
  expect_identical(valores(hd_secciones_payload(s, d, list())), c("USD", "EUR", "MXN"))
  primero <- NULL                      # nothing first: alphabetical
  expect_identical(valores(hd_secciones_payload(s, d, list())), c("EUR", "MXN", "USD"))
})

test_that("orden as a vector works as before", {
  s <- hd_secciones_validar(list(list(campo = "Moneda", orden = "USD")))
  expect_identical(s[[1]]$orden, "USD")
  expect_identical(valores(hd_secciones_payload(s, d, list())), c("USD", "EUR", "MXN"))
  expect_identical(hd_secciones_validar("Moneda")[[1]]$orden, character(0))
})

test_that("orden as a function follows a reactive inside a reactive context", {
  suppressPackageStartupMessages(library(shiny))
  base <- reactiveVal("MXN")
  s <- hd_secciones_validar(list(list(campo = "Moneda", orden = function() base())))
  pl <- reactive(valores(hd_secciones_payload(s, d, list())))
  expect_identical(isolate(pl()), c("MXN", "EUR", "USD"))
  base("USD")
  expect_identical(isolate(pl()), c("USD", "EUR", "MXN"))
})
