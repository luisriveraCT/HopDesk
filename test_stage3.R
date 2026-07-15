# Stage 3 self-test — run with: Rscript test_stage3.R
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(shinyWidgets)
library(officer)
library(flextable)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Stubs for pipeline functions not needed in this test
build_ledger_df     <- function(...) stop("not in unit test")
apply_ic_filter     <- function(df, ...) df
build_ic_fullcodes  <- function(...) character()
forecasting_get_estimate <- function(...) NA_real_
CURRENCIES <- c("MXN","USD","EUR","GBP","CAD","JPY","CHF","AUD","CNY","BRL")
COMPANY_MAP <- c(NG = "Networks Group", NTS = "Networks Trucking",
                 NCS = "Networks Crossdocking", NL = "Networks & Logistics",
                 NRS = "Networks Realtors", PL = "Paragon Logistics")

source("R/cashflow_export_module.R")
cat("[ ] source OK\n")

# ── Build a representative export_data object ─────────────────────────────────
df_test <- tibble::tibble(
  Empresa   = c("Networks Group","Networks Group","Networks Group",
                "Networks Group","Networks Group"),
  Moneda    = c("MXN","MXN","MXN","USD","USD"),
  Tipo      = c("AR","AR","AP","AR","AP"),
  Parte     = c("TETRA PAK QUERÉTARO","GEODIS MEXICO",
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

tags_test <- tibble::tibble(
  ledger    = c("AR", "AP"),
  Empresa   = "Networks Group",
  Moneda    = "MXN",
  Documento = c("F002","F003"),
  tag       = c("urgent","important")
)

export_data <- build_cashflow_export_data(
  df            = df_test,
  tags_df       = tags_test,
  emp_sel       = "Networks Group",
  date_from     = as.Date("2026-06-02"),
  date_to       = as.Date("2026-06-14"),
  grouping      = "daily",
  currency_mode = "separate",
  base_cur      = "MXN",
  fx_rates      = c()
)
stopifnot(!is.null(export_data))
cat("[x] export_data built:", length(export_data$currencies), "currencies\n")

# ── Test .cf_lbl ──────────────────────────────────────────────────────────────
stopifnot(.cf_lbl("title", "es") == "FLUJO DE CAJA PROYECTADO")
stopifnot(.cf_lbl("title", "en") == "PROJECTED CASH FLOW")
stopifnot(.cf_lbl("unknown_key", "es") == "unknown_key")  # fallback
cat("[x] .cf_lbl: ES/EN labels and fallback work\n")

# ── Test .cf_fmt ──────────────────────────────────────────────────────────────
stopifnot(.cf_fmt(1234567)    == "$1,234,567")
stopifnot(.cf_fmt(0)          == "$0")
stopifnot(.cf_fmt(NA_real_)   == "—")
stopifnot(.cf_fmt("bad")      == "—")
cat("[x] .cf_fmt: number formatting and NA fallback\n")

# ── Generate ES document ──────────────────────────────────────────────────────
doc_es <- tryCatch(
  generate_cashflow_word_doc(
    export_data    = export_data,
    emp_initials   = c("NG"),
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es",
    secondary_lang = NULL
  ),
  error = function(e) { cat("FAIL ES doc:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(doc_es))
cat("[x] generate_cashflow_word_doc ES: document created\n")

# Write to temp file and verify it's a valid docx (has content)
tmp_es <- tempfile(fileext = ".docx")
print(doc_es, target = tmp_es)
stopifnot(file.exists(tmp_es))
stopifnot(file.size(tmp_es) > 5000)   # >5 KB means real content
cat("[x] ES .docx saved:", round(file.size(tmp_es)/1024, 1), "KB\n")

# ── Generate EN document ──────────────────────────────────────────────────────
doc_en <- tryCatch(
  generate_cashflow_word_doc(
    export_data    = export_data,
    emp_initials   = c("NG"),
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "en",
    secondary_lang = NULL
  ),
  error = function(e) { cat("FAIL EN doc:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(doc_en))
tmp_en <- tempfile(fileext = ".docx")
print(doc_en, target = tmp_en)
stopifnot(file.size(tmp_en) > 5000)
cat("[x] EN .docx saved:", round(file.size(tmp_en)/1024, 1), "KB\n")

# ── Generate bilingual document ───────────────────────────────────────────────
doc_bi <- tryCatch(
  generate_cashflow_word_doc(
    export_data    = export_data,
    emp_initials   = c("NG"),
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es",
    secondary_lang = "en"
  ),
  error = function(e) { cat("FAIL bilingual doc:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(doc_bi))
tmp_bi <- tempfile(fileext = ".docx")
print(doc_bi, target = tmp_bi)
stopifnot(file.size(tmp_bi) > 5000)
cat("[x] Bilingual .docx saved:", round(file.size(tmp_bi)/1024, 1), "KB\n")

# ── Generate fused-currency document ─────────────────────────────────────────
export_fused <- build_cashflow_export_data(
  df            = df_test,
  tags_df       = tags_test,
  emp_sel       = "Networks Group",
  date_from     = as.Date("2026-06-02"),
  date_to       = as.Date("2026-06-14"),
  grouping      = "daily",
  currency_mode = "fused",
  base_cur      = "MXN",
  fx_rates      = c(USD = 18.5)
)
doc_fused <- tryCatch(
  generate_cashflow_word_doc(
    export_data    = export_fused,
    emp_initials   = c("NG"),
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es"
  ),
  error = function(e) { cat("FAIL fused doc:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(doc_fused))
tmp_fused <- tempfile(fileext = ".docx")
print(doc_fused, target = tmp_fused)
stopifnot(file.size(tmp_fused) > 5000)
cat("[x] Fused .docx saved:", round(file.size(tmp_fused)/1024, 1), "KB\n")

# ── Generate weekly-grouping document ────────────────────────────────────────
export_weekly <- build_cashflow_export_data(
  df            = df_test,
  tags_df       = tags_test,
  emp_sel       = "Networks Group",
  date_from     = as.Date("2026-06-02"),
  date_to       = as.Date("2026-07-05"),
  grouping      = "weekly",
  currency_mode = "separate"
)
doc_wk <- tryCatch(
  generate_cashflow_word_doc(
    export_data    = export_weekly,
    emp_initials   = c("NG"),
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es"
  ),
  error = function(e) { cat("FAIL weekly doc:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(doc_wk))
tmp_wk <- tempfile(fileext = ".docx")
print(doc_wk, target = tmp_wk)
stopifnot(file.size(tmp_wk) > 5000)
cat("[x] Weekly .docx saved:", round(file.size(tmp_wk)/1024, 1), "KB\n")

# Cleanup
invisible(lapply(c(tmp_es, tmp_en, tmp_bi, tmp_fused, tmp_wk), file.remove))

cat("\n=== Stage 3 self-test: ALL PASS ===\n")
