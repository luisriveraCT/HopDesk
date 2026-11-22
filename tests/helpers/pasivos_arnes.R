# =============================================================================
# tests/helpers/pasivos_arnes.R
# The common harness of the Pasivos tests (2026-11-22, paso 0
# hopdesk_coord/tablas/reportes/PASIVOS_PRUEBAS_paso0.md). Before it, 9 of
# them stopped before checking anything (CLIENT_ID unset, an old-signature
# .s3_write mock without client_id, functions that were never loaded): the
# Pasivos engine had no active test (CM02, 2026-10-31).
#
# What it gives a test that sources it at the top (from the repo root):
#   - the real persistence over an in-memory S3 (tests/helpers/s3_memoria.R):
#     .s3_read_with / .s3_write / modificar_tabla* run as in the app, with
#     client_id, base and the row-count guards; nothing reaches the network;
#   - ONE tenant, the test's: .s3_key() is the bare key, so `.mock_store` is
#     that store and `.mock_store[["pasivos_provisions.rds"]]` is what the app
#     reads and writes -- the tests seed and inspect it as they always did;
#     PASIVOS_CID ("acme") is the client_id to pass where the app requires one;
#   - .reset_mock(): empties the store and every persistence cache;
#   - the files the Pasivos code needs (monedas, zona, i18n, schemas,
#     persistence, audit, capabilities, forecasting, policies, engine), loaded
#     once; strip_accents() from R/global.R (without starting the app);
#   - stubs with no I/O: bump_sync_version(), log_action(), and puede()
#     that allows (a test that tests permissions sources R/permisos.R after).
# A test keeps its own checks; it only drops its old mock lines for this.
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(lubridate); library(jsonlite); library(uuid)
})
source("tests/helpers/s3_memoria.R")          # sources R/persistence.R with S3_KEYS; CLIENT_ID = acme
Sys.setenv(CLIENT_ID = "acme")
PASIVOS_CID <- "acme"

# One tenant: the bare key (the tests seed .mock_store[["pasivos_*.rds"]]).
.s3_key <- function(key, client_id = NULL) key
.S3_MISSING_CACHE_TTL <- 0                     # a seed after a miss is seen at once
.mock_store <- .mem
.reset_mock <- function() s3_memoria_reset()

# From R/global.R without starting the app: the app's %||% (NULL, length 0 or
# a single NA -> y; s3_memoria.R's only takes NULL) and strip_accents().
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c("%||%", "strip_accents"))
    eval(e, globalenv())

for (f in c("R/monedas.R", "R/zona.R", "R/i18n.R", "R/pasivos_schemas.R", "R/bancos_persistence.R",
            "R/pasivos_persistence.R", "R/pasivos_capabilities.R", "R/pasivos_audit.R",
            "R/forecasting_resolve.R", "R/policy_engine.R", "R/pasivos_engine.R"))
  suppressMessages(source(f))
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

bump_sync_version <- function(...) invisible(TRUE)
log_action <- function(...) invisible(TRUE)        # the Actividad log (R/app_audit.R), not under test here
# The capability checks as the Pasivos tests were written: a stub that allows.
# A test that tests permissions sources R/permisos.R itself, which replaces it.
puede <- function(usuario, permiso, ...) TRUE

# What tests/_run_confirmed_logic.R gives the tests that run under it: one
# top-level function of an R/ file, by name.
.extract_fn <- function(file, fn_name) {
  for (e in parse(file, keep.source = FALSE))
    if (is.call(e) && length(e) >= 2 && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(fn_name))) {
      assign(fn_name, eval(e[[3]], envir = globalenv()), envir = globalenv())
      return(invisible(TRUE))
    }
  stop(sprintf("%s not found in %s", fn_name, file))
}

# ── The two hygiene guardians, one rule each (engine G3/G8, stage2 7.1/7.2) ──
# cotizado_en: the files that may read it. The first four were the
# guardian's; the rest read it on purpose since, and none converts an amount
# outside the engine (2026-11-22, reviewed line by line).
PASIVOS_COTIZADO_PERMITIDOS <- c(
  "pasivos_engine.R", "pasivos_schemas.R", "pasivos_persistence.R", "pasivos_module.R",
  "fx_banxico.R",            # the "≈" of a quoted amount, with Banxico's FIX, at display
  "pasivos_calendar_glue.R", # carries the field to the calendar row; asks fx_monto_aprox()
  "pasivos_table_pivot.R",   # asks fx_monto_aprox() for the "≈"
  "pasivos_table_module.R",  # the manual provision's quoted currency
  "pasivos_list_module.R",   # the Lista's currency column (T33)
  "pasivos_deuda.R",         # T78: the debt's currency is the quoted one
  "pasivos_wizard_module.R", # the wizard's quoted fields; converts through the engine
  "pasivos_wizard_steps.R")  # the wizard's quoted inputs
# isTRUE() on a data frame column ($ inside its parentheses) is FALSE for any
# column longer than one: the bug these guardians look for. Not flagged,
# because they are scalars (reviewed): an element taken with [1], a Shiny
# input, and the one-value lists the Lista's saves return (r$cambio) and
# fx_monto_aprox() returns (ax$aprox). Comments don't count. Returns the lines.
pasivos_isTRUE_columna <- function(lineas) {
  sin_com <- sub("#.*$", "", lineas)
  escalar <- gsub("\\$[A-Za-z_.]+\\[1\\]|input\\$[A-Za-z_.]+|\\br\\$cambio\\b|\\bax\\$aprox\\b", "", sin_com)
  lineas[grepl("isTRUE\\([^)]*\\$", escalar)]
}
