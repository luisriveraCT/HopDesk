# =============================================================================
# tests/test_flujo_dias.R
# The day letters of the cash-flow buckets come from the dictionary, Monday
# first: L M X J V S D (Wednesday was a second "M"). .bucket_labels() feeds
# the preview and every export (build_cashflow_export_data()$bucket_labels).
#
# Run from project root:  Rscript tests/test_flujo_dias.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages(library(dplyr))
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres) eval(e, envir = globalenv())
}
cargar("R/cashflow_export_module.R", c(".bucket_labels", ".attach_tags_to_df", ".aggregate_to_wide",
                                       ".build_summary", "build_cashflow_export_data"))

lunes <- as.Date("2026-11-02")   # a Monday
semana <- seq(lunes, lunes + 6, by = "day")

test_that("a daily week reads L M X J V S D, Wednesday is X", {
  et <- .bucket_labels(semana, "daily")
  expect_equal(substr(et, 1, 1), c("L", "M", "X", "J", "V", "S", "D"))
  expect_equal(et[3], "X 04 Nov")
  expect_false(anyDuplicated(et) > 0)
})

test_that("in English the letters follow the dictionary too", {
  et <- .bucket_labels(semana, "daily")
  en <- vapply(1:7, function(d) tr(paste0("tablas.flujo.dia.", d), idioma = "en"), character(1))
  expect_equal(en, c("M", "T", "W", "T", "F", "S", "S"))
  expect_equal(substr(et, 1, 1)[3], tr("tablas.flujo.dia.3", idioma = "es"))
})

test_that("the export data carries the same labels (preview and exports)", {
  df <- tibble::tibble(Tipo = c("AR", "AP"), Parte = c("ALFA", "UNO"), Empresa = "NT", Moneda = "MXN",
                       FechaEff = lunes + c(2, 3), `Saldo vencido` = c(100, 40), source = "sap")
  ed <- build_cashflow_export_data(df, tags_df = NULL, emp_sel = "NT", date_from = lunes, date_to = lunes + 6,
                                   grouping = "daily")
  expect_equal(ed$bucket_labels[3], "X 04 Nov")
  expect_true("X 04 Nov" %in% names(ed$data[[1]]$ar_wide))
  expect_equal(ed$data[[1]]$ar_wide[["X 04 Nov"]], 100)
})
