# =============================================================================
# tests/test_pasivos_conflictos.R
# T18a. Editing a liability whose future provision was edited by hand (an
# override the new terms would change) opens the edit-confirm panel. Until
# 2026-11-04 the wizard save left that provision out of what it wrote, so it
# was gone from S3 before the panel opened, and every decision -- "Mantener"
# included -- found nothing to act on. Real wizard save and real decisions,
# on in-memory S3 (tests/helpers/s3_memoria.R).
# Run from project root:  Rscript tests/test_pasivos_conflictos.R
# =============================================================================

suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
if (!exists("CURRENCIES")) CURRENCIES <- c("MXN", "USD", "EUR")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_service.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_edit_confirm_module.R"))
  suppressMessages(source(f))
has_capability    <- function(user, cap) TRUE
bump_sync_version <- function(...) invisible(NULL)
fmt_money         <- function(x) format(round(x, 2), nsmall = 2)
pasivos_log_audit <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"
liab0 <- pasivos_wizard_draft_to_liability(list(
  categoria = "regular", nombre = "Renta bodega", empresa = "NG", parte = "Inmobiliaria",
  moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
  fecha_inicio = Sys.Date(), amount_default = 100), mode = "create", user = "ana")
liab0$id <- "L1"
provs0 <- pasivos_generate_provisions(liab0, window_start = Sys.Date())
P1 <- provs0$id[3]                                   # a future one, edited by hand
provs0$amount_pago_override[provs0$id == P1] <- 150

# The session's copies, as reactive values; the wizard reads and writes them.
nuevo_shared <- function() {
  L <- reactiveVal(liab0); P <- reactiveVal(provs0)
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       company_map = function() list(NG = "Nuevo Grupo"),
       holiday_overrides_db = function() list(),
       effective_client_id = function() CID, current_user = function() "ana",
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
sembrar <- function() {
  s3_memoria_reset()
  save_pasivos_liabilities(liab0, client_id = CID)
  .s3_write(provs0, S3_KEYS$pasivos_provisions, client_id = CID)
}
en_s3 <- function() load_pasivos_provisions(client_id = CID)

# Save the wizard on L1 with a new amount (120); capture what the panel gets.
abierto <- NULL
pasivos_edit_confirm_open <- function(session, conflicts, replacement, shared, liability_id)
  abierto <<- list(conflicts = conflicts, replacement = replacement, liability_id = liability_id)
guardar_asistente <- function(shared) {
  draft <- pasivos_liability_to_draft(liab0); draft$amount_default <- 120
  wiz <- list(mode = "edit", edit_target_id = "L1", draft = draft)
  res <- NULL
  testServer(function(input, output, session) {
    res <<- .pasivos_wizard_save(wiz, shared, session, "ana")
  }, {})
  res
}

# ── A1: the save keeps the conflict as it was ─────────────────────────────────
sembrar(); abierto <- NULL
sh <- nuevo_shared()
res <- guardar_asistente(sh)
ok(isTRUE(res$ok), "A1: the wizard save succeeds")
ok(identical(abierto$conflicts$id, P1), "A1: the panel opens with P1 as the one conflict")
s3 <- en_s3()
ok(P1 %in% s3$id, "A1: P1 is still on S3 after the wizard save")
ok(isTRUE(s3$amount_pago_override[s3$id == P1] == 150), "A1: P1 keeps its hand-edited 150")
ok(P1 %in% isolate(sh$pasivos_provisions_db())$id, "A1: and in the session's copy")
otra <- s3[s3$id == provs0$id[4], ]
ok(isTRUE(otra$amount_pago == 120), "A1: a provision without override takes the new 120")

# ── A2: the panel's suggested value is P1's generated row ─────────────────────
rep <- abierto$replacement
ok(identical(rep$id, P1), "A2: the replacement carries P1's id")
ok(isTRUE(rep$amount_pago == 120), "A2: and the generated amount, 120")

decidir <- function(dec, custom = NULL) {
  sembrar(); abierto <<- NULL
  sh <- nuevo_shared(); guardar_asistente(sh)
  d <- setNames(list(dec), P1)
  if (!is.null(custom)) d[[paste0(P1, "_custom")]] <- custom
  .pec_apply_decisions(abierto$conflicts, abierto$replacement, d, sh, "ana")
  r <- en_s3(); r[r$id == P1, , drop = FALSE]
}

# ── A3: each decision ─────────────────────────────────────────────────────────
r <- decidir("mantener")
ok(nrow(r) == 1L && isTRUE(r$amount_pago_override == 150) && isTRUE(r$amount_pago == 100),
   "A3: Mantener leaves P1 exactly as it was (override 150)")
r <- decidir("aplicar")
ok(nrow(r) == 1L && is.na(r$amount_pago_override) && isTRUE(r$amount_pago == 120),
   "A3: Aplicar gives P1 the generated 120 and clears the override")
r <- decidir("personalizar", custom = "135")
ok(nrow(r) == 1L && isTRUE(r$amount_pago_override == 135),
   "A3: Personalizar 135 sets P1's override to 135")
# Cancelar writes nothing: after the save alone P1 is as it was (A1).

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
