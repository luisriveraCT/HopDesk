# CM46: undoing a manual confirmation brings the invoice back to manual_inv
# with its SAME id (restore_from_papelera adds a "recovered" row). The
# papelera anti-join of the Calendario (ledger_module.R) and the Flujo
# (cashflow_export_module.R) took every "manual" papelera row and kept the
# invoice hidden while it was pending again. ids_manuales_archivados()
# (data_pipeline.R) respects "recovered". Real papelera functions throughout.
# Run from the repo root:  Rscript tests/test_cm46_manual_recuperado.R
suppressPackageStartupMessages({ library(testthat); library(dplyr); library(tibble); library(uuid) })
`%||%` <- function(a, b) if (is.null(a)) b else a
solo <- function(f, nombres) for (e in parse(f, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% nombres) eval(e, globalenv())
solo("R/persistence.R", c(".schema_papelera", ".normalize", "add_to_papelera", "restore_from_papelera"))
solo("R/data_pipeline.R", "ids_manuales_archivados")
stopifnot(exists("ids_manuales_archivados"), exists("restore_from_papelera"))

factura <- function(id, ledger_tag = "AP")
  tibble(id = id, source = "manual", Empresa = "ACME", Moneda = "MXN", Documento = paste0("F-", id),
         Importe = 500, Parte = "P")
archivar <- function(pap, id, ledger) add_to_papelera(pap, factura(id), ledger, deleted_by = "ana", disposition = "confirmed")
ultimo_archivado <- function(pap, id) tail(pap$event_id[pap$action %in% "archived" & pap$id %in% id], 1)
recuperar <- function(pap, id) restore_from_papelera(pap, ultimo_archivado(pap, id), actor = "ana")$papelera_df
oculta <- function(pap, id, ledger = "AP") id %in% ids_manuales_archivados(pap, ledger)

test_that("archived -> hidden", {
  p <- archivar(.schema_papelera(), "M-1", "AP")
  expect_true(oculta(p, "M-1"))
})

test_that("archived -> recovered -> visible (the CM46 bug: master keeps it hidden)", {
  p <- recuperar(archivar(.schema_papelera(), "M-1", "AP"), "M-1")
  expect_identical(p$action, c("archived", "recovered"))
  expect_false(oculta(p, "M-1"))
})

test_that("archived -> recovered -> archived again -> hidden", {
  p <- archivar(recuperar(archivar(.schema_papelera(), "M-1", "AP"), "M-1"), "M-1", "AP")
  expect_true(oculta(p, "M-1"))
  # ...and recovered a second time -> visible again
  expect_false(oculta(recuperar(p, "M-1"), "M-1"))
})

test_that("an orphan 'recovered' row (its archived event is not there) -> visible", {
  p <- recuperar(archivar(.schema_papelera(), "M-1", "AP"), "M-1")
  huerfana <- p[p$action %in% "recovered", , drop = FALSE]
  expect_false(oculta(huerfana, "M-1"))
})

test_that("two ledgers do not mix; MIXED counts for both", {
  p <- archivar(archivar(.schema_papelera(), "M-AR", "AR"), "M-AP", "AP")
  expect_true(oculta(p, "M-AR", "AR")); expect_false(oculta(p, "M-AR", "AP"))
  expect_true(oculta(p, "M-AP", "AP")); expect_false(oculta(p, "M-AP", "AR"))
  pm <- archivar(.schema_papelera(), "M-MX", "AP"); pm$ledger <- "MIXED"
  expect_true(oculta(pm, "M-MX", "AR")); expect_true(oculta(pm, "M-MX", "AP"))
})

test_that("rows from before the papelera had actions (action NA) still hide; SAP rows never enter", {
  p <- archivar(.schema_papelera(), "M-1", "AP"); p$action <- NA_character_; p$event_id <- NA_character_
  expect_true(oculta(p, "M-1"))
  s <- archivar(.schema_papelera(), "S-1", "AP"); s$source <- "sap"
  expect_false(oculta(s, "S-1"))
  expect_identical(ids_manuales_archivados(NULL, "AP"), character())
  expect_identical(ids_manuales_archivados(.schema_papelera(), "AP"), character())
})

test_that("the Calendario and the Flujo use ids_manuales_archivados(), with no hand anti-join left", {
  for (f in c("R/ledger_module.R", "R/cashflow_export_module.R")) {
    x <- sub("#.*$", "", readLines(f, warn = FALSE, encoding = "UTF-8"))
    expect_true(any(grepl("ids_manuales_archivados\\(", x)), info = f)
    expect_false(any(grepl('\\[\\["source"\\]\\]\\s*==\\s*"manual"', x) & grepl("pap", x)), info = f)
  }
})
