# CM49 (2): the vendors' table shows the currency as the catalog's ISO code.
# Vendors saved before 9baa492 hold "Pesos"/"Dólares"; the table showed them
# raw and its currency filter split them from MXN/USD.
# Run from the repo root:  Rscript tests/test_cm49_proveedores_moneda.R
suppressPackageStartupMessages(library(testthat))
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/monedas.R")
for (f in c("R/ppl_generator.R", "R/pagos_layouts.R", "R/proveedores_module.R"))
  for (e in parse(f, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
        as.character(e[[2]]) %in% c("ppl_moneda_palabra", ".prov_moneda_mostrar"))
      eval(e, globalenv())
stopifnot(exists(".prov_moneda_mostrar"))

test_that("the old words become ISO codes, as the PPL reads them", {
  expect_identical(.prov_moneda_mostrar(c("Pesos", "Dólares", "DLLS", "M.N.", "pesos mexicanos")),
                   c("MXN", "USD", "USD", "MXN", "MXN"))
})

test_that("ISO codes stay, legacy codes and case are normalized, empty stays empty", {
  expect_identical(.prov_moneda_mostrar(c("MXN", "usd", "EUR", "MXP", NA, "", "  ")),
                   c("MXN", "USD", "EUR", "MXN", "", "", ""))
})

test_that("the catalog column uses .prov_moneda_mostrar()", {
  x <- sub("#.*$", "", readLines("R/proveedores_module.R", warn = FALSE, encoding = "UTF-8"))
  expect_true(any(grepl('moneda       = .prov_moneda_mostrar(col("moneda"))', x, fixed = TRUE)))
})
