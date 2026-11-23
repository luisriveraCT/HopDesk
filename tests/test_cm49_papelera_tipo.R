# CM49 (1): Bancos > Papelera, the Tipo of a Calendario papelera row. Rows
# the Search modal wrote before ce8f4fa say ledger "MIXED"; they showed as
# "pago" even when they were cobros. Now their own ledger is taken from
# original_data, and without it the Tipo stays empty.
# Run from the repo root:  Rscript tests/test_cm49_papelera_tipo.R
suppressPackageStartupMessages(library(testthat))
for (e in parse("R/bancos_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".bnc_pap_tipo"))
    eval(e, globalenv())
stopifnot(exists(".bnc_pap_tipo"))

test_that("AR is cobro and AP is pago, as before", {
  expect_identical(.bnc_pap_tipo(c("AR", "AP", " ar ")), c("cobro", "pago", "cobro"))
})

test_that("a MIXED row takes its own ledger from original_data", {
  od <- list(list(ledger = "AR", Documento = "F-1"), list(ledger = "AP"), list(Documento = "F-3"), NULL)
  expect_identical(.bnc_pap_tipo(c("MIXED", "MIXED", "MIXED", "MIXED"), od), c("cobro", "pago", NA, NA))
})

test_that("a MIXED or empty ledger with nothing to go on is empty, never 'pago'", {
  expect_identical(.bnc_pap_tipo(c("MIXED", NA, "")), c(NA_character_, NA_character_, NA_character_))
})

test_that("the Calendario papelera uses .bnc_pap_tipo(), with no hand ifelse left", {
  x <- sub("#.*$", "", readLines("R/bancos_module.R", warn = FALSE, encoding = "UTF-8"))
  expect_true(any(grepl(".bnc_pap_tipo(cal$ledger, cal$original_data)", x, fixed = TRUE)))
  expect_false(any(grepl('ifelse(cal$ledger %in% "AR", "cobro", "pago")', x, fixed = TRUE)))
})
