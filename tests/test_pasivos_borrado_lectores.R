# =============================================================================
# tests/test_pasivos_borrado_lectores.R
# T18: a deleted liability keeps its provisional provisions on S3 (hidden with
# it). The readers that could still show them or move money:
#   B. build_ledger_df() without provisions passed (the Calendar's df_base)
#      used to load them from S3 itself -- no client_id, no liabilities, so
#      with one tenant's CLIENT_ID every provision showed (twice) and a deleted
#      liability's too.
#   V. the ⚡ conversion: a modal opened before another session deleted the
#      liability must not turn its provision into an item, or into a payment
#      in today's Agenda.
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
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_calendar_glue.R", "R/data_pipeline.R",
            "R/pasivos_module.R"))
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

# ── V: the ⚡ conversion refuses a deleted liability's provision ─────────────
ESCRITURAS <- character(0)
load_manual         <- function(...) tibble::tibble()
save_manual         <- function(...) { ESCRITURAS <<- c(ESCRITURAS, "manual"); invisible(TRUE) }
safe_load_pagar_hoy <- function(...) tibble::tibble()
save_pagar_hoy      <- function(...) { ESCRITURAS <<- c(ESCRITURAS, "agenda"); invisible(TRUE) }
pasivos_log_audit   <- function(...) invisible(NULL)
bump_sync_version   <- function(...) invisible(NULL)
shared <- list(effective_client_id = function() CID, current_user = function() "ana",
               company_map = function() c(NTS = "Networks Trucking"),
               manual_inv = reactiveVal(tibble::tibble()), pagar_hoy_db = reactiveVal(tibble::tibble()))
entrada <- list(pcm_empresa = "NTS", pcm_parte = "Inmobiliaria", pcm_documento = "F-1",
                pcm_importe = 100, pcm_moneda = "MXN", pcm_fecha = as.character(Sys.Date() + 10))
pid <- provs0$id[1]
convertir <- function() {
  res <- NULL
  testServer(function(input, output, session) {
    res <<- .pasivos_perform_conversion(entrada, shared, pid, stage_to_agenda = TRUE, user = "ana")
  }, {})
  res
}

# The modal was opened while L1 was active; another session deleted L1 on S3.
sembrar(borrado); ESCRITURAS <- character(0)
res <- tryCatch(convertir(), error = function(e) list(ok = NA, msg = conditionMessage(e)))
p <- load_pasivos_provisions(client_id = CID)
ok(!isTRUE(res$ok) && !length(ESCRITURAS),
   "V1: converting (and staging to the Agenda) a deleted liability's provision is refused; nothing is written")
ok(identical(p$estado[p$id == pid], "provisional"), "V1: the provision stays provisional, hidden with its liability")
ok(identical(res$msg, tr("tablas.pasivos.convertir_borrado")), "V1: and the user is told why")

# Control: with L1 active the same conversion goes ahead.
sembrar(liab0); ESCRITURAS <- character(0)
res <- tryCatch(convertir(), error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
ok("manual" %in% ESCRITURAS, "V2: (with the liability active, the conversion writes its item)")

# V3: S3 does not answer for the liabilities: it can't be checked, so it is
# refused too (fail closed: it is money).
sembrar(liab0); ESCRITURAS <- character(0)
.mem_falla <- paste0(CID, "/", S3_KEYS$pasivos_liabilities)
res <- tryCatch(convertir(), error = function(e) list(ok = NA, msg = conditionMessage(e)))
.mem_falla <- character(0)
ok(!isTRUE(res$ok) && !length(ESCRITURAS),
   "V3: with the liabilities unreadable, the conversion is refused and nothing is written")
ok(identical(res$msg, tr("tablas.pasivos.convertir_no_verificado")), "V3: saying it could not check")
.mem_falla <- paste0(CID, "/", S3_KEYS$pasivos_liabilities)
ok(is.na(pasivos_provision_de_borrado(provs0[1, ], CID)), "V3: the check itself says NA (not FALSE) when S3 fails")
.mem_falla <- character(0)
ok(isFALSE(pasivos_provision_de_borrado(provs0[1, ], CID)), "V3: and FALSE once the table reads well")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
