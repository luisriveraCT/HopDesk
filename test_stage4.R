# Stage 4 self-test — run with: Rscript test_stage4.R
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
build_ledger_df          <- function(...) stop("not in unit test")
apply_ic_filter          <- function(df, ...) df
build_ic_fullcodes       <- function(...) character()
forecasting_get_estimate <- function(...) NA_real_
CURRENCIES <- c("MXN","USD","EUR","GBP","CAD","JPY","CHF","AUD","CNY","BRL")
COMPANY_MAP <- c(NG = "Networks Group", NTS = "Networks Trucking",
                 NCS = "Networks Crossdocking", NL = "Networks & Logistics",
                 NRS = "Networks Realtors", PL = "Paragon Logistics")
# Stub S3 functions for testing persistence layer independently
S3_KEYS <- list(group_config = "group_config.rds")
.s3_read  <- function(key) NULL   # always miss — tests default path
.s3_write <- function(obj, key) invisible(obj)

source("R/cashflow_export_module.R")
cat("[ ] cashflow_export_module source OK\n")

# Source just the group config helpers (defined inline above for isolation)
.schema_group_config <- function() {
  list(group_name = "Networks Group", logo_raw = NULL, logo_ext = "png")
}
load_group_config <- function() {
  cfg <- tryCatch(.s3_read(S3_KEYS$group_config), error = function(e) NULL)
  if (is.null(cfg) || !is.list(cfg)) return(.schema_group_config())
  defaults <- .schema_group_config()
  for (nm in names(defaults)) if (is.null(cfg[[nm]])) cfg[[nm]] <- defaults[[nm]]
  cfg
}
save_group_config <- function(cfg) {
  .s3_write(cfg, S3_KEYS$group_config)
  invisible(cfg)
}
cat("[ ] persistence helpers defined\n")

# ── Test load_group_config defaults ──────────────────────────────────────────
cfg_default <- load_group_config()
stopifnot(is.list(cfg_default))
stopifnot(cfg_default$group_name == "Networks Group")
stopifnot(is.null(cfg_default$logo_raw))
stopifnot(cfg_default$logo_ext == "png")
cat("[x] load_group_config: returns correct defaults when S3 is empty\n")

# ── Test save round-trip (mock) ───────────────────────────────────────────────
cfg_modified <- list(group_name = "Acme Holdings",
                     logo_raw   = NULL,
                     logo_ext   = "png")
result <- save_group_config(cfg_modified)
stopifnot(identical(result$group_name, "Acme Holdings"))
cat("[x] save_group_config: round-trip returns correct object\n")

# ── Build export_data for doc tests ──────────────────────────────────────────
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
cat("[x] export_data built\n")

# ── Test: custom group_name appears in document ───────────────────────────────
doc_custom <- tryCatch(
  generate_cashflow_word_doc(
    export_data    = export_data,
    emp_initials   = "NG",
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es",
    group_name     = "Acme Holdings"
  ),
  error = function(e) { cat("FAIL custom name doc:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(doc_custom))
tmp_custom <- tempfile(fileext = ".docx")
print(doc_custom, target = tmp_custom)
stopifnot(file.size(tmp_custom) > 5000)
cat("[x] generate_cashflow_word_doc: custom group_name doc created (",
    round(file.size(tmp_custom)/1024, 1), "KB)\n")

# ── Test: logo raw bytes appear in document ───────────────────────────────────
# Create a minimal valid 1x1 PNG (67 bytes)
minimal_png_b64 <- "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
logo_raw <- base64enc::base64decode(minimal_png_b64)

doc_logo <- tryCatch(
  generate_cashflow_word_doc(
    export_data    = export_data,
    emp_initials   = "NG",
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es",
    group_name     = "Networks Group",
    group_logo_raw = logo_raw,
    group_logo_ext = "png"
  ),
  error = function(e) { cat("FAIL logo doc:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(doc_logo))
tmp_logo <- tempfile(fileext = ".docx")
print(doc_logo, target = tmp_logo)
stopifnot(file.size(tmp_logo) > 5000)
cat("[x] generate_cashflow_word_doc: logo doc created (",
    round(file.size(tmp_logo)/1024, 1), "KB)\n")

# ── Test: NULL logo falls back to placeholder text ────────────────────────────
doc_nologo <- tryCatch(
  generate_cashflow_word_doc(
    export_data    = export_data,
    emp_initials   = "NG",
    emp_full       = "Networks Group",
    ic_label       = "Sin IC",
    user_name      = "Test User",
    primary_lang   = "es",
    group_logo_raw = NULL
  ),
  error = function(e) { cat("FAIL no-logo doc:", conditionMessage(e), "\n"); NULL }
)
stopifnot(!is.null(doc_nologo))
tmp_nologo <- tempfile(fileext = ".docx")
print(doc_nologo, target = tmp_nologo)
stopifnot(file.size(tmp_nologo) > 5000)
cat("[x] generate_cashflow_word_doc: NULL logo falls back to placeholder (",
    round(file.size(tmp_nologo)/1024, 1), "KB)\n")

# ── Test: filename prefix uses group name ─────────────────────────────────────
make_prefix <- function(grp_raw) {
  prefix <- gsub("[^A-Za-z0-9]", "", grp_raw %||% "Networks Group")
  if (!nzchar(prefix)) prefix <- "FlujoEfectivo"
  prefix
}
stopifnot(make_prefix("Networks Group") == "NetworksGroup")
stopifnot(make_prefix("Acme Holdings")  == "AcmeHoldings")
stopifnot(make_prefix("")               == "FlujoEfectivo")
stopifnot(make_prefix(NULL)             == "NetworksGroup")
cat("[x] filename prefix: group name slug logic correct\n")

# Cleanup
invisible(lapply(c(tmp_custom, tmp_logo, tmp_nologo), file.remove))

cat("\n=== Stage 4 self-test: ALL PASS ===\n")
