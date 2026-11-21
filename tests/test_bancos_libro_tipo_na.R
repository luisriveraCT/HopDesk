# =============================================================================
# tests/test_bancos_libro_tipo_na.R
# Bancos' Libro: the Tipo badge of a movement with no tipo (NA). Before
# 2026-11-21 `tipo == "comision"` gave NA, ifelse() kept it, and a reconciled
# movement with no tipo lost its "Conciliado" badge (the engine shows NA as
# no badge). .libro_tipo() is what libro_datos uses.
# Run from the repo root:  Rscript tests/test_bancos_libro_tipo_na.R
# =============================================================================
library(testthat)
for (e in parse("R/bancos_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".libro_tipo"))) eval(e, globalenv())

test_that("a reconciled movement with no tipo is 'conciliado'", {
  expect_identical(.libro_tipo(NA_character_, TRUE), "conciliado")
})
test_that("the rest as before: comision first, then conciliado, else none; NA reconciled is not", {
  expect_identical(.libro_tipo(c("comision", "comision", "spei_out", "spei_out", NA, NA),
                               c(TRUE, FALSE, TRUE, NA, NA, FALSE)),
                   c("comision", "comision", "conciliado", "", "", ""))
})
test_that("libro_datos uses it (no ifelse on tipo == 'comision' left)", {
  src <- paste(readLines("R/bancos_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("Tipo        = .libro_tipo(movs$tipo, movs$conciliado)", src, fixed = TRUE))
  expect_false(grepl('ifelse(movs$tipo == "comision"', src, fixed = TRUE))
})
