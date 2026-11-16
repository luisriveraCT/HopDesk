# =============================================================================
# tests/test_monedas.R -- the currency catalog and the client's base currency
# (monedas fase 1, R/monedas.R). Pure: no S3, no network.
# Run from project root:  Rscript tests/test_monedas.R
# =============================================================================
library(testthat)
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/monedas.R")

test_that("el catálogo es ISO 4217 bien formado y versionado", {
  m <- .monedas_leer()
  expect_identical(names(m), c("codigo", "numero", "decimales", "nombre_es", "nombre_en"))
  expect_true(all(grepl("^[A-Z]{3}$", m$codigo)))
  expect_true(all(grepl("^[0-9]{3}$", m$numero)))          # leading zeros kept (008, 012...)
  expect_false(anyDuplicated(m$codigo) > 0)
  expect_false(anyDuplicated(m$numero) > 0)
  expect_true(all(m$decimales %in% c(0L, 2L, 3L)))
  expect_true(all(nzchar(m$nombre_es)) && all(nzchar(m$nombre_en)))
  expect_identical(m$codigo, sort(m$codigo))
  expect_gt(nrow(m), 150)
  # no funds, metals or units (decided in the file's header)
  expect_length(intersect(m$codigo, c("MXV", "CLF", "XAU", "XDR", "XXX", "USN")), 0)
  # the version in the file's header is MONEDAS_VERSION
  cab <- readLines(.MONEDAS_ARCHIVO, n = 3, encoding = "UTF-8")
  expect_true(any(grepl(paste0("Version: ", MONEDAS_VERSION), cab, fixed = TRUE)))
})

test_that("las monedas que hoy usa la app están en el catálogo, con sus decimales", {
  viejas <- c("MXN", "USD", "EUR", "GBP", "CAD", "JPY", "CHF", "AUD", "CNY", "BRL")
  expect_true(all(moneda_valida(viejas)))
  expect_identical(moneda_decimales(c("MXN", "JPY", "KWD", "ZZZ")), c(2L, 0L, 3L, NA))
  expect_identical(moneda_nombre(c("MXN", "ZZZ"), "es"), c("Peso mexicano", "ZZZ"))
  expect_identical(moneda_nombre("EUR", "en"), "Euro")
  expect_identical(moneda_valida(c("mxn", NA, "", "MXN")), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("opciones: primero las indicadas, luego el catálogo; con nombre en el idioma", {
  o <- monedas_opciones(c("USD", "MXN", "ZZZ"), "es")
  expect_identical(unname(o[1:2]), c("USD", "MXN"))
  expect_identical(length(o), length(monedas_catalogo()))
  expect_identical(names(o)[2], "MXN — Peso mexicano")
  expect_identical(names(monedas_opciones("MXN", "en"))[1], "MXN — Mexican peso")
})

test_that("monedas en los datos: por número de documentos, la base primero", {
  d <- c("USD", "MXN", "MXN", " mxn ", "EUR", NA, "", "USD", "MXN")
  expect_identical(monedas_en_datos(d), c("MXN", "USD", "EUR"))
  expect_identical(monedas_en_datos(d, base = "EUR"), c("EUR", "MXN", "USD"))
  # a code outside the catalog stays (data is data), last
  expect_identical(monedas_en_datos(c("XYZ", "XYZ", "XYZ", "MXN")), c("MXN", "XYZ"))
  expect_identical(monedas_en_datos(character()), character())
})

test_that("moneda base: ajuste > datos > ninguna, y nunca inventa una", {
  datos <- c("USD", "USD", "MXN")
  expect_identical(moneda_base_resolver("EUR", datos), list(moneda = "EUR", origen = "ajuste"))
  expect_identical(moneda_base_resolver(" eur ", datos), list(moneda = "EUR", origen = "ajuste"))
  expect_identical(moneda_base_resolver(NA, datos), list(moneda = "USD", origen = "datos"))
  expect_identical(moneda_base_resolver(NULL, datos), list(moneda = "USD", origen = "datos"))
  expect_identical(moneda_base_resolver("ZZZ", datos), list(moneda = "USD", origen = "datos"))
  # by documents, not by amount: many yen would not win over the peso
  expect_identical(moneda_base_resolver(NA, c("MXN", "MXN", "JPY"))$moneda, "MXN")
  # a code outside the catalog in the data is never the base
  expect_identical(moneda_base_resolver(NA, c("XYZ", "XYZ", "MXN"))$moneda, "MXN")
  expect_identical(moneda_base_resolver(NA, character()), list(moneda = NA_character_, origen = "ninguna"))
})

test_that("monedas_de_sap junta AR y AP y tolera lo que falte", {
  sap <- list(AR = data.frame(Moneda = c("MXN", "USD")), AP = data.frame(Moneda = "EUR"))
  expect_identical(monedas_de_sap(sap), c("MXN", "USD", "EUR"))
  expect_identical(monedas_de_sap(list(AR = NULL, AP = data.frame(x = 1))), character())
  expect_identical(monedas_de_sap(NULL), character())
})

test_that("la configuración del grupo trae moneda_base vacía por omisión", {
  src <- readLines("R/persistence.R", encoding = "UTF-8")
  expect_true(any(grepl("moneda_base = NA_character_", src, fixed = TRUE)))
})
