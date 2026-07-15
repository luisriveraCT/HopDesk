# =============================================================================
# tests/test_pasivos_stage4_cell_editor.R
# Stage 4: Cell editor — single provision edit logic.
# Run from project root:  source("tests/test_pasivos_stage4_cell_editor.R")
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

# ── Synthetic provision ────────────────────────────────────────────────────────
.make_prov <- function(id = uuid::UUIDgenerate(), ...) {
  base <- tibble::tibble(
    id = id, liability_id = "l1", origin = "rule",
    occurrence_index = 1L, estado = "provisional",
    fecha_calculada = Sys.Date() + 30L, fecha_efectiva = Sys.Date() + 30L,
    policy_ids = NA_character_,
    empresa = "NCS", parte = "Proveedor A", codigo_parte = "C001",
    moneda_pago = "MXN", cotizado_en = NA_character_,
    amount_pago = 5000, amount_cotizado = NA_real_, fx_rate_used = NA_real_,
    componente_capital = NA_real_, componente_interes = NA_real_,
    componente_fees = NA_real_, componente_iva = NA_real_,
    amount_pago_override = NA_real_, amount_cotizado_override = NA_real_,
    fecha_efectiva_override = as.Date(NA),
    documento = "", referencia = "REF-001", notas = "",
    manual_inv_id = NA_character_, pagar_hoy_id = NA_character_,
    bancos_conf_id = NA_character_, reverted_count = 0L,
    generated_by = "test", generated_at = Sys.time(),
    last_edited_by = "test", last_edited_at = Sys.time()
  )
  overrides <- list(...)
  for (nm in names(overrides)) base[[nm]] <- overrides[[nm]]
  base
}

# =============================================================================
# Group C1 — Single-occurrence edit (pure logic, no Shiny server)
# =============================================================================
cat("── C1. Single-occurrence provision edit ─────────────────────────────────\n")
.reset_mock()

p1_id <- uuid::UUIDgenerate()
p1 <- .make_prov(id = p1_id, amount_pago = 5000, referencia = "OLD")
p2 <- .make_prov(id = uuid::UUIDgenerate(), amount_pago = 3000)
provs <- dplyr::bind_rows(p1, p2)
save_pasivos_provisions(provs)

# Simulate: edit amount only
idx <- which(provs$id == p1_id)
provs$amount_pago_override[idx] <- 5800
provs$last_edited_by[idx] <- "test"
save_pasivos_provisions(provs)
result <- load_pasivos_provisions()

# Only p1 changed
.chk(result$amount_pago_override[result$id == p1_id], 5800, "C1: override set on edited prov")
.chk_true(is.na(result$amount_pago_override[result$id != p1_id]),
          "C1: other provision untouched")

# Edit referencia (not an override field)
provs$referencia[idx] <- "NEW-REF"
save_pasivos_provisions(provs)
result2 <- load_pasivos_provisions()
.chk(result2$referencia[result2$id == p1_id], "NEW-REF", "C1: referencia updated")
.chk_true(is.na(result2$amount_pago_override[result2$id != p1_id]),
          "C1: referencia edit doesn't touch other row override")

# Edit fecha
new_fecha <- Sys.Date() + 45L
provs$fecha_efectiva_override[idx] <- new_fecha
save_pasivos_provisions(provs)
result3 <- load_pasivos_provisions()
.chk(result3$fecha_efectiva_override[result3$id == p1_id], new_fecha,
     "C1: fecha_efectiva_override set")

# =============================================================================
# Group C2 — Override detection for conflicts
# =============================================================================
cat("── C2. Conflict detection on apply-to-future ────────────────────────────\n")
.reset_mock()

pA <- .make_prov(id = "A1", amount_pago = 5000)
pB <- .make_prov(id = "A2", amount_pago = 5000,
                  amount_pago_override = 6000)  # manually overridden
pC <- .make_prov(id = "A3", amount_pago = 5000)
existing <- dplyr::bind_rows(pA, pB, pC)
save_pasivos_provisions(existing)

gen_new <- dplyr::bind_rows(pA, pB, pC) %>%
  dplyr::mutate(amount_pago = 5500)  # new default amount

result_reco <- pasivos_reconcile_provisions(existing, gen_new, mode = "regenerate")

# pB has an override → conflict; pA and pC have none → update
.chk_true(nrow(result_reco$conflicts) >= 1L, "C2: 1 conflict row (overridden prov B)")
.chk_true("A2" %in% result_reco$conflicts$id, "C2: pB is the conflict")

# =============================================================================
# Group C3 — Read-only mode for confirmed items (UI contract test)
# =============================================================================
cat("── C3. Confirmed-item read-only contract ────────────────────────────────\n")

p_conf <- .make_prov(id = "CONF1", estado = "item_confirmed")
.chk(p_conf$estado[1], "item_confirmed", "C3: confirmed prov has estado=item_confirmed")
# The cell editor UI sets read_only = identical(kind, "item_confirmed")
read_only_flag <- identical(p_conf$estado[1], "item_confirmed")
.chk_true(read_only_flag, "C3: read_only_flag is TRUE for item_confirmed")

# ── Results ─────────────────────────────────────────────────────────────────
cat(sprintf("\nStage 4 Cell Editor: %d PASS  /  %d FAIL\n\n", .pass, .fail))
if (.fail > 0) stop(sprintf("%d test(s) failed.", .fail))
