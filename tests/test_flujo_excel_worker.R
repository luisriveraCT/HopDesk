# =============================================================================
# tests/test_flujo_excel_worker.R
# The cash-flow Excel export runs .cf_excel_worker() in a separate R process
# (callr::r_bg in the app), which only has what it sources. It must load the
# dictionary and the tag map itself: without them the export stopped at the
# first tag ("object 'HD_ETIQUETAS' not found") and, once the day letters came
# from the dictionary, at tr().
#
# Runs the worker exactly as the app does -- a clean process via callr --
# over a small snapshot, and reads the workbook back.
# Run from project root:  Rscript tests/test_flujo_excel_worker.R
# FLU_RAIZ=<older checkout> runs it against that checkout's R/.
# =============================================================================
library(testthat)
suppressPackageStartupMessages(library(dplyr))
raiz <- normalizePath(Sys.getenv("FLU_RAIZ", "."), winslash = "/")

lunes <- as.Date("2026-11-02")   # a Monday
snapshot <- function(lang) list(
  combined_df = tibble::tibble(Tipo = c("AR", "AP", "AR"), Parte = c("CLIENTE ALFA", "PROVEEDOR UNO", "CLIENTE BETA"),
                               Empresa = "NT", Moneda = "MXN", FechaEff = lunes + c(2, 3, 2),
                               `Saldo vencido` = c(1500, 400, 250), source = "sap", Documento = c("F1", "F2", "F3")),
  tags_df = NULL, emp_sel = "NT", date_from = lunes, date_to = lunes + 6, grouping = "daily",
  currency_mode = "separate", base_cur = "MXN", fx_rates = c(),
  ic_label = "Sin IC", user_name = "luis", lang_primary = lang, lang_secondary = NULL,
  group_name = "Grupo", logo_path = NULL)
correr_worker <- function(lang = "es") {
  snap <- tempfile(fileext = ".rds"); out <- tempfile(fileext = ".xlsx")
  saveRDS(snapshot(lang), snap)
  e <- new.env()
  for (ex in parse(file.path(raiz, "R/cashflow_export_module.R")))
    if (is.call(ex) && identical(as.character(ex[[2]])[1], ".cf_excel_worker")) eval(ex, envir = e)
  err <- tryCatch({ callr::r(e$.cf_excel_worker, args = list(snapshot_path = snap, output_path = out, app_dir = raiz)); NULL },
                  error = function(x) conditionMessage(x))
  list(err = err, out = out)
}
celdas <- function(path) {
  wb <- openxlsx::loadWorkbook(path)
  unlist(lapply(openxlsx::sheets(wb), function(h)
    unlist(openxlsx::read.xlsx(path, sheet = h, colNames = FALSE, skipEmptyRows = TRUE), use.names = FALSE)))
}

test_that("the worker writes the workbook in its own process", {
  r <- correr_worker("es")
  expect_null(r$err)
  expect_true(file.exists(r$out))
})
# (The "Wednesday is X" check comes with T11, which adds the letters.)
