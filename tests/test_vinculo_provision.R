# Provision -> item -> payment link: an item converted from a provision must
# keep its provision_id through every edit.
# Run from the repo root: Rscript tests/test_vinculo_provision.R
library(testthat)
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/persistence.R")

guardado <- tibble(
  id = "m1", ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "PROV-7",
  Parte = "Renta", Importe = 1000, `Abono futuro` = 200,
  created_by = "ana", created_at = as.POSIXct("2026-10-01", tz = "UTC"),
  provision_id = "p-7", liability_id = "l-3", referencia = "Contrato 12")

formulario <- tibble(
  id = "m1", ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "PROV-7",
  Parte = "Renta oficina", Importe = 1200, created_by = "ana",
  created_at = as.POSIXct("2026-10-01", tz = "UTC"), updated_at = Sys.time())

test_that("editar un item conserva su vínculo con la provisión", {
  out <- editar_manual(guardado, formulario)
  expect_equal(out$provision_id, "p-7")
  expect_equal(out$liability_id, "l-3")
  expect_equal(out$referencia, "Contrato 12")
  expect_equal(out$`Abono futuro`, 200)
  # what the form sends wins
  expect_equal(out$Parte, "Renta oficina")
  expect_equal(out$Importe, 1200)
})

test_that("tras upsert la fila guardada sigue vinculada", {
  df <- upsert_manual(guardado, editar_manual(guardado, formulario))
  expect_equal(nrow(df), 1)
  expect_equal(df$provision_id, "p-7")
})

test_that("sin fila previa, la edición es la fila del formulario", {
  expect_identical(editar_manual(guardado[0, ], formulario), formulario)
  expect_identical(editar_manual(NULL, formulario), formulario)
})
