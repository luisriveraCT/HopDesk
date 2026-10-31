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

source("R/pasivos_persistence.R")
provs <- tibble(id = c("p1", "p2", "p3", "p4", NA),
                estado = c("provisional", "converted", "item_confirmed", "closed", "item_confirmed"))

test_that("una provisión pagada se reconoce como pagada", {
  expect_setequal(provisiones_pagadas(provs, c("p1", "p2", "p3", "p4")), c("p3", "p4"))
  expect_length(provisiones_pagadas(provs, c("p1", "p2")), 0)
  expect_length(provisiones_pagadas(provs, NA), 0)
  expect_length(provisiones_pagadas(NULL, "p3"), 0)
  expect_length(provisiones_pagadas(provs, character()), 0)
})

lineas <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("borrar desde Calendario deja fuera las pagadas y relee S3", {
  src <- lineas("R/ledger_module.R")
  expect_match(src, "provisiones_pagadas(provs_db", fixed = TRUE)
  expect_match(src, "provs_db <- if (\"provision_id\" %in% names(rows_to_delete))\n        tryCatch(load_pasivos_provisions(",
               fixed = TRUE)
  # the in-memory copy is no longer the source for the estado write
  expect_false(grepl("provs_db <- tryCatch(shared$pasivos_provisions_db()", src, fixed = TRUE))
})

test_that("deshacer conversión rechaza en el servidor una provisión pagada", {
  src <- lineas("R/pasivos_module.R")
  undo <- regmatches(src, regexpr("observeEvent\\(input\\$pcm_do_undo_conversion[\\s\\S]*?provs\\$estado\\[idx\\]\\s+<- \"provisional\"", src, perl = TRUE))
  expect_length(undo, 1)
  expect_match(undo, "provisiones_pagadas(", fixed = TRUE)
  expect_lt(regexpr("provisiones_pagadas(", undo, fixed = TRUE),
            regexpr("save_manual(", undo, fixed = TRUE))   # checked before anything is removed
})
