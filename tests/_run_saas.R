# =============================================================================
# tests/_run_saas.R
# Runner for all HopDesk SaaS feature tests.
# Run from project root:  source("tests/_run_saas.R")
# No live S3 credentials needed — all S3 I/O is mocked in-memory.
# =============================================================================

options(warn = 1)
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(jsonlite)
  library(uuid)
  library(aws.s3)    # must load before patching its namespace
  library(openssl)   # Stage 3: ERP secret encryption
  library(shiny)     # Stage 4: audit_log_viewer_module.R + testServer()
  library(bslib)
  library(DT)
  library(shinyWidgets)
})

# ── In-memory S3 mock ─────────────────────────────────────────────────────────
# Each object stored by its full S3 key (e.g. "networks/usuarios.rds").
.mock_s3_store <- new.env(parent = emptyenv())

mock_s3readRDS <- function(object, bucket, ...) {
  obj <- .mock_s3_store[[object]]
  if (is.null(obj)) stop("NoSuchKey: ", object)
  obj
}
mock_s3saveRDS <- function(x, object, bucket, ...) {
  .mock_s3_store[[object]] <- x
  invisible(x)
}
mock_s3getbucket <- function(bucket, prefix = "", max = 200L, ...) {
  all_keys <- ls(.mock_s3_store)
  matched  <- all_keys[startsWith(all_keys, prefix)]
  lapply(matched, function(k) list(Key = k))
}

# Patch aws.s3 namespace so every aws.s3::s3readRDS / s3saveRDS / get_bucket
# call (including those with :: inside sourced files) hits the mock.
suppressWarnings({
  utils::assignInNamespace("s3readRDS",  mock_s3readRDS,   "aws.s3")
  utils::assignInNamespace("s3saveRDS",  mock_s3saveRDS,   "aws.s3")
  utils::assignInNamespace("get_bucket", mock_s3getbucket, "aws.s3")
})

# ── Env vars ──────────────────────────────────────────────────────────────────
Sys.setenv(CLIENT_ID     = "networks",
           S3_BUCKET     = "mock-bucket",
           AWS_ACCESS_KEY_ID     = "mock",
           AWS_SECRET_ACCESS_KEY = "mock",
           AWS_DEFAULT_REGION    = "us-east-1",
           RESEND_API_KEY        = "sandbox",
           RESEND_FROM_EMAIL     = "noreply@test.hopdesk.com",
           HOPDESK_SECRETS_KEY   = openssl::base64_encode(openssl::rand_bytes(32)))

# ── S3 stubs for persistence.R internal helpers ──────────────────────────────
.s3_read  <- function(key)      mock_s3readRDS(paste0("networks/", key), "mock-bucket")
.s3_write <- function(obj, key) mock_s3saveRDS(obj, paste0("networks/", key), "mock-bucket")

# ── Minimal S3_KEYS (superset of all keys used by tested code) ───────────────
S3_KEYS <- list(
  usuarios           = "usuarios.rds",
  username_index     = "username_index.rds",
  pending_invites    = "pending_invites.rds",
  app_audit          = "app_audit.rds",
  sync_versions      = "sync_versions.rds",
  bancos             = "bancos.rds",
  bancos_cuentas     = "bancos_cuentas.rds",
  bancos_movimientos = "bancos_movimientos.rds",
  bancos_confirmados = "bancos_confirmados.rds",
  empresas           = "empresas.rds",
  grupos             = "grupos.rds",
  proveedores        = "proveedores.rds",
  proveedores_inactivos = "proveedores_inactivos.rds",
  moves              = "invoice_moves.rds",
  notes              = "notes.rds",
  tags               = "tags.rds",
  papelera           = "papelera.rds",
  ctas_cuentas       = "ctas_cuentas.rds",
  conciliacion       = "conciliacion.rds",
  parte_alias_map    = "parte_alias_map.rds",
  policy_catalog     = "policy_catalog.rds",
  partner_policies   = "partner_policies.rds",
  policy_moves       = "policy_moves.rds",
  holiday_overrides  = "holiday_overrides.rds",
  client_registry    = "hd-admin/client_registry.rds",
  pasivos_liabilities = "pasivos_liabilities.rds",
  pasivos_provisions  = "pasivos_provisions.rds",
  abonos             = "abonos.rds",
  sap_overrides      = "sap_overrides.rds",
  erp_connections    = "erp_connections.rds",
  manual             = "manual.rds",
  pagar_hoy          = "pagar_hoy.rds",
  pagar_hoy_sync     = "pagar_hoy_sync.rds",
  interco            = "interco.rds"
)

"%||%" <- function(a, b) if (!is.null(a)) a else b

source("R/persistence.R",    local = FALSE)
source("R/tier_registry.R",  local = FALSE)
source("R/sap_cache.R",      local = FALSE)
source("R/jump_logic.R",     local = FALSE)
source("R/tiers_tab_config.R", local = FALSE)
source("R/auth.R",           local = FALSE)
source("R/app_audit.R",      local = FALSE)
source("R/audit_log_viewer_module.R", local = FALSE)
source("R/email_service.R",  local = FALSE)
source("R/secrets_encryption.R",     local = FALSE)
source("R/erp_connector_registry.R", local = FALSE)
source("R/sap_api.R",                local = FALSE)

# ── Override hd-admin direct-read helpers (they bypass .s3_key()) ─────────────
# Re-point them at the mock store so tests can plant data there.
.read_username_index <- function() {
  tryCatch(mock_s3readRDS("hd-admin/username_index.rds", "mock-bucket"),
           error = function(e) .schema_username_index())
}
.write_username_index <- function(df) {
  mock_s3saveRDS(df, "hd-admin/username_index.rds", "mock-bucket")
  invisible(TRUE)
}
read_emergency_lock <- function() {
  tryCatch(mock_s3readRDS("hd-admin/emergency_lock.rds", "mock-bucket"),
           error = function(e) NULL)
}
read_pending_invites <- function() {
  tryCatch(mock_s3readRDS("hd-admin/pending_invites.rds", "mock-bucket"),
           error = function(e) .schema_pending_invites())
}
write_pending_invites <- function(df) {
  mock_s3saveRDS(df, "hd-admin/pending_invites.rds", "mock-bucket")
  invisible(TRUE)
}
read_client_registry <- function() {
  tryCatch(mock_s3readRDS("hd-admin/client_registry.rds", "mock-bucket"),
           error = function(e) .schema_client_registry())
}
write_client_registry <- function(df) {
  mock_s3saveRDS(df, "hd-admin/client_registry.rds", "mock-bucket")
  invisible(TRUE)
}
auth_load_usuarios <- function(client_id = NULL) {
  cid <- client_id %||% Sys.getenv("CLIENT_ID")
  tryCatch(mock_s3readRDS(paste0(tolower(cid), "/usuarios.rds"), "mock-bucket"),
           error = function(e) auth_schema_usuarios())
}
auth_save_usuarios <- function(df, client_id = NULL) {
  cid <- client_id %||% Sys.getenv("CLIENT_ID")
  mock_s3saveRDS(df, paste0(tolower(cid), "/usuarios.rds"), "mock-bucket")
  invisible(TRUE)
}

# ── Run all test modules ──────────────────────────────────────────────────────
.total_pass <- 0L
.total_fail <- 0L

.run_module <- function(file) {
  e <- new.env(parent = globalenv())
  e$.pass <- 0L
  e$.fail <- 0L
  tryCatch(source(file, local = e), error = function(err) {
    cat(sprintf("  ERROR loading %s: %s\n", basename(file), err$message))
    e$.fail <- e$.fail + 1L
  })
  .total_pass <<- .total_pass + e$.pass
  .total_fail <<- .total_fail + e$.fail
}

cat("\n====================================================\n")
cat("  HopDesk SaaS Test Suite\n")
cat("====================================================\n\n")

.run_module("tests/test_saas_isolation.R")
.run_module("tests/test_saas_erp_secrets.R")
.run_module("tests/test_saas_erp_isolation.R")
.run_module("tests/test_saas_erp_tiers.R")
.run_module("tests/test_saas_erp_fallback.R")
.run_module("tests/test_saas_perms.R")
.run_module("tests/test_saas_sap_cache.R")
.run_module("tests/test_saas_home_jump.R")
.run_module("tests/test_saas_stage2_tabs.R")
.run_module("tests/test_saas_invites.R")
.run_module("tests/test_saas_audit.R")
.run_module("tests/test_saas_audit_viewer.R")
.run_module("tests/test_saas_notifications.R")
.run_module("tests/test_saas_limit_change.R")

cat("\n====================================================\n")
cat(sprintf("  TOTAL: %d passed, %d failed\n", .total_pass, .total_fail))
cat("====================================================\n\n")

if (.total_fail > 0L) stop(sprintf("%d test(s) failed", .total_fail))
invisible(.total_pass)
