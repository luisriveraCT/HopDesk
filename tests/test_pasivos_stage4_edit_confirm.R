# =============================================================================
# tests/test_pasivos_stage4_edit_confirm.R
# Stage 4: Edit-confirmation panel decision logic.
# Run from project root:  source("tests/test_pasivos_stage4_edit_confirm.R")
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(uuid)
})

source("R/persistence.R")
source("R/pasivos_schemas.R")

if (!exists("S3_KEYS")) {
  S3_KEYS <- list(
    pasivos_liabilities   = "pasivos_liabilities.rds",
    pasivos_provisions    = "pasivos_provisions.rds",
    pasivos_modifiers     = "pasivos_modifiers.rds",
    pasivos_estimates     = "pasivos_estimates.rds",
    pasivos_card_expenses = "pasivos_card_expenses.rds",
    pasivos_audit         = "pasivos_audit.rds",
    bancos_cuentas        = "bancos_cuentas.rds",
    bancos_movimientos    = "bancos_movimientos.rds",
    bancos_confirmados    = "bancos_confirmados.rds",
    policy_catalog        = "policy_catalog.rds",
    partner_policies      = "partner_policies.rds",
    policy_moves          = "policy_moves.rds",
    holiday_overrides     = "holiday_overrides.rds"
  )
}
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
if (!exists("CURRENCIES")) CURRENCIES <- c("MXN","USD","EUR","GBP","CAD","JPY","CHF","AUD","CNY","BRL")

source("R/bancos_persistence.R")
source("R/pasivos_persistence.R")
source("R/pasivos_capabilities.R")
source("R/pasivos_audit.R")
source("R/forecasting_service.R")
source("R/policy_engine.R")
source("R/pasivos_engine.R")
source("R/pasivos_edit_confirm_module.R")

.pass <- 0L; .fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(tryCatch(all.equal(actual, expected, check.attributes = FALSE),
                        error = function(e) FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                     label, deparse(expected), deparse(actual))); .fail <<- .fail + 1L }
}
.chk_true <- function(expr, label) .chk(isTRUE(expr), TRUE, label)

.mock_store <- new.env(parent = emptyenv())
.s3_read  <- function(key) .mock_store[[key]]
.s3_write <- function(obj, key) { .mock_store[[key]] <- obj; invisible(TRUE) }
.reset_mock <- function() rm(list = ls(.mock_store), envir = .mock_store)

# Minimal mock shared
.mock_shared <- function(provs) {
  pdb <- provs
  list(
    pasivos_provisions_db = function(x) { if (missing(x)) pdb else pdb <<- x },
    current_user = function() "test"
  )
}

.make_prov <- function(id, amount_pago = 5000, override = NA_real_) {
  tibble::tibble(
    id = id, liability_id = "L1", origin = "rule",
    occurrence_index = 1L, estado = "provisional",
    fecha_calculada = Sys.Date() + 30L, fecha_efectiva = Sys.Date() + 30L,
    policy_ids = NA_character_,
    empresa = "NCS", parte = "P", codigo_parte = "",
    moneda_pago = "MXN", cotizado_en = NA_character_,
    amount_pago = amount_pago, amount_cotizado = NA_real_, fx_rate_used = NA_real_,
    componente_capital = NA_real_, componente_interes = NA_real_,
    componente_fees = NA_real_, componente_iva = NA_real_,
    amount_pago_override = override, amount_cotizado_override = NA_real_,
    fecha_efectiva_override = as.Date(NA),
    documento = "", referencia = "", notas = "",
    manual_inv_id = NA_character_, pagar_hoy_id = NA_character_,
    bancos_conf_id = NA_character_, reverted_count = 0L,
    generated_by = "test", generated_at = Sys.time(),
    last_edited_by = "test", last_edited_at = Sys.time()
  )
}

# =============================================================================
# Group E1 — Decision application
# =============================================================================
cat("── E1. Decision application ──────────────────────────────────────────────\n")
.reset_mock()

conf_pA <- .make_prov("pA", amount_pago = 5000, override = 4500)
conf_pB <- .make_prov("pB", amount_pago = 5000, override = 6000)
conf_pC <- .make_prov("pC", amount_pago = 5000, override = 7000)

conflicts <- dplyr::bind_rows(conf_pA, conf_pB, conf_pC)

rep_pA <- .make_prov("pA", amount_pago = 5800)
rep_pB <- .make_prov("pB", amount_pago = 5800)
rep_pC <- .make_prov("pC", amount_pago = 5800)
replacement <- dplyr::bind_rows(rep_pA, rep_pB, rep_pC)

all_provs <- dplyr::bind_rows(conflicts)
save_pasivos_provisions(all_provs)
shared <- .mock_shared(all_provs)

# "Aplicar nuevo" on pA → override cleared, generated value applied
decisions_test1 <- list(pA = "aplicar", pB = "mantener", pC = "personalizar",
                         pC_custom = "9000")
.pec_apply_decisions(conflicts, replacement, decisions_test1, shared, "test")
result1 <- load_pasivos_provisions()

.chk_true(is.na(result1$amount_pago_override[result1$id == "pA"]),
          "E1: 'aplicar' clears override on pA")
.chk(result1$amount_pago[result1$id == "pA"], 5800,
     "E1: 'aplicar' sets generated value on pA")

.chk(result1$amount_pago_override[result1$id == "pB"], 6000,
     "E1: 'mantener' preserves override on pB")

.chk(result1$amount_pago_override[result1$id == "pC"], 9000,
     "E1: 'personalizar' sets custom value 9000 on pC")

# =============================================================================
# Group E2 — Mass actions
# =============================================================================
cat("── E2. Mass actions ──────────────────────────────────────────────────────\n")
.reset_mock()

conf2 <- dplyr::bind_rows(
  .make_prov("q1", override = 100),
  .make_prov("q2", override = 200),
  .make_prov("q3", override = 300)
)
repl2 <- dplyr::bind_rows(
  .make_prov("q1", amount_pago = 999),
  .make_prov("q2", amount_pago = 999),
  .make_prov("q3", amount_pago = 999)
)
save_pasivos_provisions(conf2)
shared2 <- .mock_shared(conf2)

# "Aplicar todos: Mantener" → all rows default to mantener
dec_all_mantener <- setNames(
  as.list(rep("mantener", 3)),
  c("q1", "q2", "q3")
)
.pec_apply_decisions(conf2, repl2, dec_all_mantener, shared2, "test")
r2 <- load_pasivos_provisions()
.chk(r2$amount_pago_override[r2$id == "q1"], 100, "E2: mantener all keeps override q1=100")
.chk(r2$amount_pago_override[r2$id == "q2"], 200, "E2: mantener all keeps override q2=200")

# "Aplicar todos: Nuevo valor" → all cleared
save_pasivos_provisions(conf2)
shared3 <- .mock_shared(conf2)
dec_all_nuevo <- setNames(
  as.list(rep("aplicar", 3)),
  c("q1", "q2", "q3")
)
.pec_apply_decisions(conf2, repl2, dec_all_nuevo, shared3, "test")
r3 <- load_pasivos_provisions()
.chk_true(is.na(r3$amount_pago_override[r3$id == "q1"]), "E2: nuevo all clears override q1")
.chk_true(is.na(r3$amount_pago_override[r3$id == "q2"]), "E2: nuevo all clears override q2")
.chk(r3$amount_pago[r3$id == "q1"], 999, "E2: nuevo all sets generated value q1")

# =============================================================================
# Group E3 — Cancel behavior
# =============================================================================
cat("── E3. Cancel behavior ───────────────────────────────────────────────────\n")

# Cancel = leave everything as-is (no writes) → provisions unchanged
.reset_mock()
conf3 <- dplyr::bind_rows(.make_prov("c1", override = 111))
save_pasivos_provisions(conf3)

# Simulating cancel = treat as "mantener all"
dec_cancel <- list(c1 = "mantener")
shared_c <- .mock_shared(conf3)
.pec_apply_decisions(conf3, NULL, dec_cancel, shared_c, "test")
r4 <- load_pasivos_provisions()
.chk(r4$amount_pago_override[r4$id == "c1"], 111, "E3: cancel (mantener) preserves override")

# ── Results ─────────────────────────────────────────────────────────────────
cat(sprintf("\nStage 4 Edit-Confirm: %d PASS  /  %d FAIL\n\n", .pass, .fail))
if (.fail > 0) stop(sprintf("%d test(s) failed.", .fail))
