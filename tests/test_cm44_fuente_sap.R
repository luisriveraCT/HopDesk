# CM44: es_fuente_sap() -- "this stored row came from SAP" accepts both the
# bare "sap" written before 2364f4c (2026-09-14) and SAP_B1_ERP_TYPE written
# since. Before, the papelera-ghost source of compute_confirmed_flags compared
# `== SAP_B1_ERP_TYPE` only: a SAP invoice trashed before 09-14 came back as
# open (counted in totals, payable again). Run from the repo root:
#   Rscript tests/test_cm44_fuente_sap.R
suppressPackageStartupMessages({ library(testthat); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/erp_connector_registry.R")
# Only function definitions from these files (their top level needs the app).
solo_funciones <- function(f, nombres) {
  for (e in parse(f, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
}
solo_funciones("R/persistence.R", "confirmaciones_vigentes")
solo_funciones("R/data_pipeline.R", "compute_confirmed_flags")
stopifnot(exists("es_fuente_sap"), exists("compute_confirmed_flags"))

test_that("es_fuente_sap: 'sap' and SAP_B1_ERP_TYPE are SAP; NA, blank, manual, provision and other ERPs are not", {
  expect_identical(es_fuente_sap(c("sap", SAP_B1_ERP_TYPE, NA, "", "manual", "provision", "quickbooks")),
                   c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(es_fuente_sap(character()), logical())
})

factura <- function(doc, source = SAP_B1_ERP_TYPE)
  tibble(source = source, Empresa = "ACME", Moneda = "MXN", Documento = doc,
         `Saldo vencido` = 100, Saldo_original = 100, confirmed = FALSE)
papelera <- function(doc, source)
  tibble(ledger = "AP", source = source, Empresa = "ACME", Moneda = "MXN", Documento = doc)

test_that("a SAP invoice trashed BEFORE 2026-09-14 (papelera source 'sap') is still a ghost: confirmed, out of totals", {
  r <- compute_confirmed_flags(factura("F-VIEJA"), "AP", NULL, papelera("F-VIEJA", "sap"))
  expect_true(r$confirmed[1])
  expect_true(r$is_ghost[1])
})

test_that("a SAP invoice trashed since then (SAP_B1_ERP_TYPE) is still a ghost", {
  r <- compute_confirmed_flags(factura("F-NUEVA"), "AP", NULL, papelera("F-NUEVA", SAP_B1_ERP_TYPE))
  expect_true(r$confirmed[1])
  expect_true(r$is_ghost[1])
})

test_that("a papelera row of unknown or manual origin does not ghost a SAP invoice (unchanged)", {
  for (s in c(NA_character_, "manual", "quickbooks")) {
    r <- compute_confirmed_flags(factura("F-X"), "AP", NULL, papelera("F-X", s))
    expect_false(r$confirmed[1], info = s)
  }
})

test_that("a manual invoice that reuses the key of a trashed 'sap' invoice is NOT ghosted (the !is_manual guard)", {
  r <- compute_confirmed_flags(factura("F-REUSO", source = "manual"), "AP", NULL, papelera("F-REUSO", "sap"))
  expect_equal(nrow(r), 1L)
  expect_false(r$confirmed[1])
})

test_that("no stored-source check compares with SAP_B1_ERP_TYPE or 'sap' by hand: es_fuente_sap() everywhere", {
  archivos <- c("app.R", list.files("R", "[.]R$", full.names = TRUE))
  for (f in archivos) {
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    x <- sub("#.*$", "", x)                         # code only
    x[grepl("^es_fuente_sap <- function", x)] <- ""  # the one definition
    malos <- grep('[=!]=\\s*SAP_B1_ERP_TYPE|%in%\\s*c\\("sap",\\s*SAP_B1_ERP_TYPE\\)|[=!]=\\s*"sap"\\b', x)
    expect_identical(malos, integer(), info = f)
  }
})
