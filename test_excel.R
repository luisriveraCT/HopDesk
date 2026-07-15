# Excel export self-test — run with: Rscript test_excel.R
library(openxlsx)
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(shinyWidgets)
library(officer)
library(flextable)
library(htmltools)
library(base64enc)

`%||%` <- function(a, b) if (!is.null(a)) a else b
build_ledger_df          <- function(...) stop("not in unit test")
apply_ic_filter          <- function(df, ...) df
build_ic_fullcodes       <- function(...) character()
forecasting_get_estimate <- function(...) NA_real_
CURRENCIES  <- c("MXN","USD","EUR","GBP","CAD","JPY","CHF","AUD","CNY","BRL")
COMPANY_MAP <- c(NG="Networks Group", NTS="Networks Trucking",
                 NCS="Networks Crossdocking", NL="Networks & Logistics",
                 NRS="Networks Realtors", PL="Paragon Logistics")

source("R/cashflow_export_module.R")
cat("[ ] source OK\n")

df_test <- tibble::tibble(
  Empresa   = rep("Networks Group", 5),
  Moneda    = c("MXN","MXN","MXN","USD","USD"),
  Tipo      = c("AR","AR","AP","AR","AP"),
  Parte     = c("TETRA PAK","GEODIS MEXICO","AUTO CONSUMO","PRINTPACK","TOKA"),
  Documento = c("F001","F002","F003","F004","F005"),
  source    = c("sap","sap","sap","sap","provision"),
  FechaEff  = as.Date(c("2026-06-04","2026-06-11","2026-06-05","2026-06-04","2026-06-12")),
  `Saldo vencido` = c(4495962, 802346, 520608, 24115, 4570),
  tag_urgent    = c(FALSE, TRUE, FALSE, FALSE, FALSE),
  tag_important = c(FALSE, FALSE, TRUE, FALSE, FALSE)
)

ed <- build_cashflow_export_data(
  df=df_test, tags_df=NULL, emp_sel="Networks Group",
  date_from=as.Date("2026-06-02"), date_to=as.Date("2026-06-14"),
  grouping="daily", currency_mode="separate", base_cur="MXN", fx_rates=c())
cat("[x] export_data built:", length(ed$currencies), "currencies\n")

# ── Test 1: separate currencies ───────────────────────────────────────────────
wb <- tryCatch(
  generate_cashflow_excel_wb(ed, "NG", "Networks Group", "Sin IC", "Test User", "es"),
  error = function(e) { cat("FAIL separate:", conditionMessage(e), "\n"); NULL })
stopifnot(!is.null(wb))
tmp <- tempfile(fileext = ".xlsx")
openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
stopifnot(file.exists(tmp) && file.size(tmp) > 5000)
cat("[x] separate currencies: OK (", round(file.size(tmp)/1024, 1), "KB)\n")

# ── Test 2: fused mode ────────────────────────────────────────────────────────
ed2 <- build_cashflow_export_data(
  df=df_test, tags_df=NULL, emp_sel="Networks Group",
  date_from=as.Date("2026-06-02"), date_to=as.Date("2026-06-14"),
  grouping="daily", currency_mode="fused", base_cur="MXN", fx_rates=c(USD=18.5))
wb2 <- tryCatch(
  generate_cashflow_excel_wb(ed2, "NG", "Networks Group", "Sin IC", "Test User", "es"),
  error = function(e) { cat("FAIL fused:", conditionMessage(e), "\n"); NULL })
stopifnot(!is.null(wb2))
tmp2 <- tempfile(fileext = ".xlsx")
openxlsx::saveWorkbook(wb2, tmp2, overwrite = TRUE)
stopifnot(file.size(tmp2) > 5000)
cat("[x] fused mode: OK (", round(file.size(tmp2)/1024, 1), "KB)\n")

# ── Test 3: bilingual ─────────────────────────────────────────────────────────
wb3 <- tryCatch(
  generate_cashflow_excel_wb(ed, "NG", "Networks Group", "Sin IC", "Test User",
    primary_lang="es", secondary_lang="en"),
  error = function(e) { cat("FAIL bilingual:", conditionMessage(e), "\n"); NULL })
stopifnot(!is.null(wb3))
tmp3 <- tempfile(fileext = ".xlsx")
openxlsx::saveWorkbook(wb3, tmp3, overwrite = TRUE)
stopifnot(file.size(tmp3) > 5000)
cat("[x] bilingual: OK (", round(file.size(tmp3)/1024, 1), "KB)\n")

# ── Test 4: weekly grouping ───────────────────────────────────────────────────
ed4 <- build_cashflow_export_data(
  df=df_test, tags_df=NULL, emp_sel="Networks Group",
  date_from=as.Date("2026-06-01"), date_to=as.Date("2026-07-31"),
  grouping="weekly", currency_mode="separate", base_cur="MXN", fx_rates=c())
wb4 <- tryCatch(
  generate_cashflow_excel_wb(ed4, "NG", "Networks Group", "Sin IC", "Test User", "es"),
  error = function(e) { cat("FAIL weekly:", conditionMessage(e), "\n"); NULL })
stopifnot(!is.null(wb4))
tmp4 <- tempfile(fileext = ".xlsx")
openxlsx::saveWorkbook(wb4, tmp4, overwrite = TRUE)
stopifnot(file.size(tmp4) > 5000)
cat("[x] weekly grouping: OK (", round(file.size(tmp4)/1024, 1), "KB)\n")

# ── Test 5: logo embedding ────────────────────────────────────────────────────
minimal_png_b64 <- "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
logo_raw <- base64enc::base64decode(minimal_png_b64)
wb5 <- tryCatch(
  generate_cashflow_excel_wb(ed, "NG", "Networks Group", "Sin IC", "Test User",
    "es", group_logo_raw=logo_raw, group_logo_ext="png"),
  error = function(e) { cat("FAIL logo:", conditionMessage(e), "\n"); NULL })
stopifnot(!is.null(wb5))
tmp5 <- tempfile(fileext = ".xlsx")
openxlsx::saveWorkbook(wb5, tmp5, overwrite = TRUE)
stopifnot(file.size(tmp5) > 5000)
cat("[x] logo embedding: OK (", round(file.size(tmp5)/1024, 1), "KB)\n")

# Cleanup
invisible(lapply(c(tmp, tmp2, tmp3, tmp4, tmp5), function(f) {
  if (!is.null(f) && file.exists(f)) unlink(f)
}))

cat("\n=== Excel self-test: ALL PASS ===\n")
