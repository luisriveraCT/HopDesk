# =============================================================================
# tests/test_pasivos_borrado_lectores.R
# T18: a deleted liability keeps its provisional provisions on S3 (hidden with
# it). The readers that could still show them or move money:
#   B. build_ledger_df() without provisions passed (the Calendar's df_base)
#      used to load them from S3 itself -- no client_id, no liabilities, so
#      with one tenant's CLIENT_ID every provision showed (twice) and a deleted
#      liability's too.
# In-memory S3; each case creates the liability, deletes it, and checks.
# Run from project root:  Rscript tests/test_pasivos_borrado_lectores.R
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")                 # CLIENT_ID = "acme": one tenant
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, globalenv())
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/global.R", c("fmt_money", "CURRENCIES"))
for (f in c("R/i18n.R", "R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_service.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_calendar_glue.R", "R/data_pipeline.R"))
  suppressMessages(source(f))
has_capability <- function(user, cap) TRUE

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"
liab0 <- pasivos_wizard_draft_to_liability(list(
  categoria = "regular", nombre = "Renta", empresa = "NTS", parte = "Inmobiliaria",
  moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
  fecha_inicio = Sys.Date(), amount_default = 100), user = "ana")
liab0$id <- "L1"
provs0 <- pasivos_generate_provisions(liab0, window_start = Sys.Date())
borrado <- liab0; borrado$estado <- "deleted"
sembrar <- function(liab) {
  s3_memoria_reset()
  save_pasivos_liabilities(liab, client_id = CID)
  .s3_write(provs0, S3_KEYS$pasivos_provisions, client_id = CID)
}

# ── B: build_ledger_df takes only the provisions it is given ─────────────────
sembrar(borrado)
d <- build_ledger_df(raw_df = NULL, ledger = "AP", empresa = NULL, moves_df = NULL,
                     provs_df = NULL, liabs_df = NULL)
ok(!sum(d$source %in% "provision"),
   "B1: without provs_df (the Calendar's df_base), no provision rows -- not even from S3")
d <- build_ledger_df(raw_df = NULL, ledger = "AP", empresa = NULL, moves_df = NULL,
                     provs_df = provs0, liabs_df = borrado)
ok(!sum(d$source %in% "provision"), "B2: given the provisions of a deleted liability, none shows")
d <- build_ledger_df(raw_df = NULL, ledger = "AP", empresa = NULL, moves_df = NULL,
                     provs_df = provs0, liabs_df = liab0)
ok(sum(d$source %in% "provision") == nrow(provs0), "B3: (and of an active one, all of them, once)")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
