# PDF generation self-test — run with: Rscript test_pdf.R
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
COMPANY_MAP <- c(NG = "Networks Group", NTS = "Networks Trucking",
                 NCS = "Networks Crossdocking", NL = "Networks & Logistics",
                 NRS = "Networks Realtors", PL = "Paragon Logistics")

source("R/cashflow_export_module.R")
cat("[ ] source OK\n")

df_test <- tibble::tibble(
  Empresa   = c("Networks Group","Networks Group","Networks Group",
                "Networks Group","Networks Group"),
  Moneda    = c("MXN","MXN","MXN","USD","USD"),
  Tipo      = c("AR","AR","AP","AR","AP"),
  Parte     = c("TETRA PAK QUERETARO","GEODIS MEXICO",
                "AUTO CONSUMO DEL GOLFO","PRINTPACK PACKAGING",
                "TOKA INTERNACIONAL"),
  Documento = c("F001","F002","F003","F004","F005"),
  source    = c("sap","sap","sap","sap","provision"),
  FechaEff  = as.Date(c("2026-06-04","2026-06-11","2026-06-05",
                         "2026-06-04","2026-06-12")),
  `Saldo vencido` = c(4495962, 802346, 520608, 24115, 4570),
  tag_urgent    = c(FALSE, TRUE, FALSE, FALSE, FALSE),
  tag_important = c(FALSE, FALSE, TRUE, FALSE, FALSE)
)

export_data <- build_cashflow_export_data(
  df            = df_test,
  tags_df       = NULL,
  emp_sel       = "Networks Group",
  date_from     = as.Date("2026-06-02"),
  date_to       = as.Date("2026-06-14"),
  grouping      = "daily",
  currency_mode = "separate",
  base_cur      = "MXN",
  fx_rates      = c()
)
cat("[x] export_data built:", length(export_data$currencies), "currencies\n")

# ── Test: generate_cashflow_html_doc returns a valid HTML file ────────────────
html_path <- tryCatch(
  generate_cashflow_html_doc(
    export_data    = export_data,
    emp_initials   = "NG",
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es"
  ),
  error = function(e) { cat("FAIL html_doc ES:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(html_path))
stopifnot(file.exists(html_path))
stopifnot(file.size(html_path) > 5000)
cat("[x] generate_cashflow_html_doc ES: HTML file created (",
    round(file.size(html_path)/1024, 1), "KB)\n")

# ── Test: chrome_print converts HTML to PDF ───────────────────────────────────
pdf_path <- tempfile(fileext = ".pdf")
chrome_ok <- tryCatch({
  pagedown::chrome_print(
    input   = html_path,
    output  = pdf_path,
    timeout = 120,
    options = list(printBackground = TRUE, preferCSSPageSize = TRUE,
                   paperWidth = 11.69, paperHeight = 8.27,
                   marginTop = 0.59, marginBottom = 0.59,
                   marginLeft = 0.59, marginRight = 0.59)
  )
  TRUE
}, error = function(e) { cat("FAIL chrome_print:", conditionMessage(e), "\n"); FALSE })
stopifnot(isTRUE(chrome_ok))
stopifnot(file.exists(pdf_path))
stopifnot(file.size(pdf_path) > 5000)
cat("[x] chrome_print: PDF created (", round(file.size(pdf_path)/1024, 1), "KB)\n")

# ── Test: bilingual HTML ──────────────────────────────────────────────────────
html_bi <- tryCatch(
  generate_cashflow_html_doc(
    export_data    = export_data,
    emp_initials   = "NG",
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es",
    secondary_lang = "en"
  ),
  error = function(e) { cat("FAIL html bilingual:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(html_bi))
stopifnot(file.size(html_bi) > 5000)
cat("[x] generate_cashflow_html_doc bilingual: OK (",
    round(file.size(html_bi)/1024, 1), "KB)\n")

# ── Test: fused currency HTML ─────────────────────────────────────────────────
export_fused <- build_cashflow_export_data(
  df            = df_test,
  tags_df       = NULL,
  emp_sel       = "Networks Group",
  date_from     = as.Date("2026-06-02"),
  date_to       = as.Date("2026-06-14"),
  grouping      = "daily",
  currency_mode = "fused",
  base_cur      = "MXN",
  fx_rates      = c(USD = 18.5)
)
html_fused <- tryCatch(
  generate_cashflow_html_doc(
    export_data  = export_fused,
    emp_initials = "NG",
    emp_full     = "Networks Group",
    ic_label     = "Sin IC",
    user_name    = "Test User",
    primary_lang = "es"
  ),
  error = function(e) { cat("FAIL html fused:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(html_fused))
stopifnot(file.size(html_fused) > 5000)
cat("[x] generate_cashflow_html_doc fused: OK (",
    round(file.size(html_fused)/1024, 1), "KB)\n")

# Cleanup
invisible(lapply(c(html_path, pdf_path, html_bi, html_fused), function(f) {
  if (!is.null(f) && file.exists(f)) unlink(f)
}))

cat("\n=== PDF self-test: ALL PASS ===\n")
