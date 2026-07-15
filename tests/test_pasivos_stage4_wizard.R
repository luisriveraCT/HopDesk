# =============================================================================
# tests/test_pasivos_stage4_wizard.R
# Stage 4: Wizard validation, draft-to-liability conversion, and save flow.
# Run from project root:  source("tests/test_pasivos_stage4_wizard.R")
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(lubridate)
  library(jsonlite); library(uuid)
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

if (!exists("CURRENCIES")) {
  CURRENCIES <- c("MXN","USD","EUR","GBP","CAD","JPY","CHF","AUD","CNY","BRL")
}
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b

source("R/bancos_persistence.R")
source("R/pasivos_persistence.R")
source("R/pasivos_capabilities.R")
source("R/pasivos_audit.R")
source("R/forecasting_service.R")
source("R/policy_engine.R")
source("R/pasivos_engine.R")
source("R/pasivos_wizard_validate.R")
source("R/pasivos_wizard_module.R")

# ── Test runner ────────────────────────────────────────────────────────────────
.pass <- 0L; .fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(tryCatch(all.equal(actual, expected, check.attributes = FALSE),
                        error = function(e) FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                     label, deparse(expected), deparse(actual))); .fail <<- .fail + 1L }
}
.chk_true  <- function(expr, label) .chk(isTRUE(expr), TRUE, label)
.chk_error <- function(expr, label) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) e$message)
  if (!is.null(e)) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s  (no error thrown)\n", label)); .fail <<- .fail + 1L }
}

# S3 mock
.mock_store    <- new.env(parent = emptyenv())
.s3_read  <- function(key) .mock_store[[key]]
.s3_write <- function(obj, key) { .mock_store[[key]] <- obj; invisible(TRUE) }
.reset_mock <- function() rm(list = ls(.mock_store), envir = .mock_store)

# ── Helper: minimal draft ──────────────────────────────────────────────────────
.draft <- function(...) {
  base <- list(
    categoria = "regular", flavor = NA_character_,
    nombre = "Test regular", empresa = "NCS", parte = "Proveedor A",
    codigo_parte = "", subcategoria = "servicios",
    moneda_pago = "MXN", cotizado_en = "",
    tarjeta_closing_day = NA_integer_, tarjeta_due_day = NA_integer_,
    tarjeta_credit_limit = NA_real_,
    recurrence_type = "monthly_day",
    recurrence_params = list(day = 15L),
    fecha_inicio = as.Date("2026-06-01"),
    amount_default = 1500, amount_default_cot = NA_real_,
    tarjeta_provision_source = NA_character_,
    principal_original = NA_real_, saldo_capital = NA_real_,
    tasa_actual = NA_real_, tasa_tipo = "fija", tasa_spread = 0,
    tasa_estimate_method = "", plazo_meses = NA_integer_,
    dia_pago = NA_integer_, metodo_amortizacion = NA_character_,
    schedule_csv_raw = "", cargos_iniciales = NULL, valor_residual = NULL
  )
  overrides <- list(...)
  for (nm in names(overrides)) base[[nm]] <- overrides[[nm]]
  base
}

# =============================================================================
# Group W1 — Validation
# =============================================================================
cat("── W1. Validation ────────────────────────────────────────────────────────\n")

# Empty nombre fails
v1 <- pasivos_wizard_validate_final(.draft(nombre = ""),
                                     empresa_choices = c("NCS","NTS"))
.chk_true(!v1$ok, "W1: empty nombre fails")
.chk_true("nombre" %in% names(v1$errors), "W1: errors has 'nombre' key")

# Nonexistent empresa fails
v2 <- pasivos_wizard_validate_final(.draft(empresa = "DOES_NOT_EXIST"),
                                     empresa_choices = c("NCS","NTS"))
.chk_true(!v2$ok, "W1: bad empresa fails")

# Tarjeta closing day == due day → warning, not error
d_tarjeta <- .draft(
  categoria = "tarjeta", recurrence_type = NA_character_,
  tarjeta_closing_day = 15L, tarjeta_due_day = 15L,
  tarjeta_provision_source = "manual", amount_default = 5000
)
v3_warn <- pasivos_wizard_step_warnings(d_tarjeta, "detalles")
.chk_true(length(v3_warn) > 0, "W1: tarjeta same closing/due day → warning produced")
v3_det  <- pasivos_wizard_validate_detalles(d_tarjeta, c("NCS"))
.chk_true(v3_det$ok, "W1: tarjeta same closing/due day → detalles still OK")

# Financiero principal 0 fails
d_fin0 <- .draft(categoria = "financiero", flavor = "credito_simple",
                  principal_original = 0, tasa_actual = 5, plazo_meses = 60L,
                  dia_pago = 28L, fecha_inicio = as.Date("2026-06-01"),
                  metodo_amortizacion = "francesa",
                  recurrence_type = NA_character_)
v4 <- pasivos_wizard_validate_terminos(d_fin0)
.chk_true(!v4$ok, "W1: principal 0 fails terminos")
.chk_true("principal_original" %in% names(v4$errors), "W1: errors has 'principal_original'")

# Negative tasa fails
d_neg_t <- .draft(categoria = "financiero", flavor = "credito_simple",
                   principal_original = 1000000, tasa_actual = -1,
                   plazo_meses = 60L, dia_pago = 28L,
                   fecha_inicio = as.Date("2026-06-01"),
                   metodo_amortizacion = "francesa",
                   recurrence_type = NA_character_)
v5 <- pasivos_wizard_validate_terminos(d_neg_t)
.chk_true(!v5$ok && "tasa_actual" %in% names(v5$errors), "W1: negative tasa fails")

# Plazo < 1 fails
d_bad_pl <- .draft(categoria = "financiero", flavor = "credito_simple",
                    principal_original = 1e6, tasa_actual = 10, plazo_meses = 0L,
                    dia_pago = 28L, fecha_inicio = as.Date("2026-06-01"),
                    metodo_amortizacion = "francesa",
                    recurrence_type = NA_character_)
v6 <- pasivos_wizard_validate_terminos(d_bad_pl)
.chk_true(!v6$ok && "plazo_meses" %in% names(v6$errors), "W1: plazo 0 fails")

# Dia de pago out of range fails
d_bad_dp <- .draft(categoria = "financiero", flavor = "credito_simple",
                    principal_original = 1e6, tasa_actual = 10, plazo_meses = 60L,
                    dia_pago = 32L, fecha_inicio = as.Date("2026-06-01"),
                    metodo_amortizacion = "francesa",
                    recurrence_type = NA_character_)
v7 <- pasivos_wizard_validate_terminos(d_bad_dp)
.chk_true(!v7$ok && "dia_pago" %in% names(v7$errors), "W1: dia_pago 32 fails")

# Custom amortization with malformed CSV fails
d_bad_csv <- .draft(categoria = "financiero", flavor = "credito_simple",
                     principal_original = 1e6, tasa_actual = 10, plazo_meses = 60L,
                     dia_pago = 28L, fecha_inicio = as.Date("2026-06-01"),
                     metodo_amortizacion = "custom",
                     schedule_csv_raw = "wrong_col1,wrong_col2\n1,2\n3,4",
                     recurrence_type = NA_character_)
v8 <- pasivos_wizard_validate_terminos(d_bad_csv)
.chk_true(!v8$ok, "W1: bad CSV fails terminos")

# All valid regular draft passes
v9 <- pasivos_wizard_validate_final(.draft(), empresa_choices = c("NCS","NTS"))
.chk_true(v9$ok, "W1: valid regular draft passes final validation")

# All valid financiero draft passes
d_valid_fin <- .draft(
  categoria = "financiero", flavor = "credito_simple",
  principal_original = 1e6, tasa_actual = 10, plazo_meses = 60L,
  dia_pago = 28L, fecha_inicio = as.Date("2026-06-01"),
  metodo_amortizacion = "francesa",
  recurrence_type = NA_character_,
  amount_default = NA_real_
)
v10 <- pasivos_wizard_validate_final(d_valid_fin, empresa_choices = c("NCS","NTS"))
.chk_true(v10$ok, "W1: valid financiero draft passes final validation")

# =============================================================================
# Group W2 — Draft to liability conversion
# =============================================================================
cat("── W2. Draft to liability conversion ────────────────────────────────────\n")
.reset_mock()

# Pago regular
d_reg <- .draft()
liab_reg <- pasivos_wizard_draft_to_liability(d_reg, mode = "create", user = "test")
.chk(liab_reg$categoria[1], "regular", "W2: regular categoria")
.chk(liab_reg$recurrence_type[1], "monthly_day", "W2: regular recurrence_type")
.chk(liab_reg$recurrence_params[[1]]$day, 15L, "W2: regular recurrence_params day=15")
.chk(liab_reg$amount_default[1], 1500, "W2: regular amount_default")
.chk_true(!is.na(liab_reg$id[1]) && nzchar(liab_reg$id[1]), "W2: id generated")

# Financiero / credito_simple (no imported schedule)
d_fin <- .draft(
  categoria = "financiero", flavor = "credito_simple",
  principal_original = 1e6, tasa_actual = 10, plazo_meses = 60L,
  dia_pago = 28L, fecha_inicio = as.Date("2026-06-01"),
  metodo_amortizacion = "francesa",
  recurrence_type = NA_character_,
  amount_default = NA_real_
)
liab_fin <- pasivos_wizard_draft_to_liability(d_fin, mode = "create", user = "test")
.chk(liab_fin$categoria[1], "financiero", "W2: financiero categoria")
.chk(liab_fin$flavor[1], "credito_simple", "W2: financiero flavor")
.chk_true(is.null(liab_fin$schedule_imported[[1]]), "W2: schedule_imported NULL for non-custom")

# Financiero / otro with pasted CSV
valid_csv <- "periodo,fecha,capital,interes,fees\n1,2026-07-28,3000,1500,0\n2,2026-08-28,3030,1470,0"
d_fin_csv <- .draft(
  categoria = "financiero", flavor = "otro",
  principal_original = 1e6, tasa_actual = 10, plazo_meses = 60L,
  dia_pago = 28L, fecha_inicio = as.Date("2026-06-01"),
  metodo_amortizacion = "custom",
  schedule_csv_raw = valid_csv,
  recurrence_type = NA_character_,
  amount_default = NA_real_
)
liab_csv <- pasivos_wizard_draft_to_liability(d_fin_csv, mode = "create", user = "test")
.chk_true(!is.null(liab_csv$schedule_imported[[1]]), "W2: schedule_imported populated for custom")
.chk(nrow(liab_csv$schedule_imported[[1]]), 2L, "W2: schedule has 2 rows")

# Tarjeta provision_source = average_12m → amount_default NA
d_tdc_avg <- .draft(
  categoria = "tarjeta",
  tarjeta_closing_day = 28L, tarjeta_due_day = 15L,
  tarjeta_provision_source = "average_12m",
  recurrence_type = NA_character_,
  amount_default = NA_real_
)
liab_avg <- pasivos_wizard_draft_to_liability(d_tdc_avg, mode = "create", user = "test")
.chk(liab_avg$tarjeta_provision_source[1], "average_12m", "W2: tarjeta provision_source average_12m")
.chk_true(is.na(liab_avg$amount_default[1]), "W2: tarjeta average_12m amount_default is NA")

# Tarjeta provision_source = manual → amount_default set
d_tdc_man <- .draft(
  categoria = "tarjeta",
  tarjeta_closing_day = 28L, tarjeta_due_day = 15L,
  tarjeta_provision_source = "manual",
  recurrence_type = NA_character_,
  amount_default = 8000
)
liab_man <- pasivos_wizard_draft_to_liability(d_tdc_man, mode = "create", user = "test")
.chk(liab_man$tarjeta_provision_source[1], "manual", "W2: tarjeta provision_source manual")
.chk(liab_man$amount_default[1], 8000, "W2: tarjeta manual amount_default = 8000")

# Stage 4.5: wizard validation rejects variable_otro as new value
d_var_otro <- .draft(
  categoria = "financiero", flavor = "credito_simple",
  principal_original = 1e6, tasa_actual = 10, plazo_meses = 60L,
  dia_pago = 28L, fecha_inicio = as.Date("2026-06-01"),
  metodo_amortizacion = "francesa",
  tasa_tipo = "variable_otro",
  recurrence_type = NA_character_, amount_default = NA_real_
)
v_var <- pasivos_wizard_validate_terminos(d_var_otro)
.chk_true(length(v_var$errors) > 0,
          "W2: validate_terminos rejects tasa_tipo variable_otro")
.chk_true(grepl("tasa_tipo", paste(v_var$errors, collapse = " ")),
          "W2: validate_terminos error mentions tasa_tipo")

# Stage 4.5: engine still accepts variable_otro for existing liabilities
liab_var <- pasivos_wizard_draft_to_liability(.draft(
  categoria = "financiero", flavor = "credito_simple",
  principal_original = 500000, tasa_actual = 8.5, plazo_meses = 36L,
  dia_pago = 15L, fecha_inicio = as.Date("2025-01-01"),
  metodo_amortizacion = "francesa",
  tasa_tipo = "variable_otro",
  recurrence_type = NA_character_, amount_default = NA_real_
), mode = "create", user = "test")
liab_var$tasa_tipo[1] <- "variable_otro"  # force the legacy value past wizard save
sched_var <- tryCatch(pasivos_generate_schedule(liab_var), error = function(e) NULL)
.chk_true(!is.null(sched_var) && nrow(sched_var) > 0,
          "W2: engine generates schedule for variable_otro liability")
.chk_true(all(sched_var$interes >= 0, na.rm = TRUE),
          "W2: engine variable_otro schedule has non-negative interest (fallback to tasa_actual)")

# =============================================================================
# Group W3 — Save flow (mocked)
# =============================================================================
cat("── W3. Save flow (mocked engine) ────────────────────────────────────────\n")
.reset_mock()

# Pre-seed empty stores
save_pasivos_liabilities(.schema_pasivos_liability())
save_pasivos_provisions(.schema_pasivos_provision())

# Mock pasivos_generate_provisions to return 12 simple rows
.gen_prov <- function(liab_row, ...) {
  n <- 12L
  base_date <- Sys.Date()
  tibble::tibble(
    id = replicate(n, uuid::UUIDgenerate()),
    liability_id = liab_row$id[1], origin = "rule",
    occurrence_index = seq_len(n), estado = "provisional",
    fecha_calculada = base_date + seq_len(n) * 30L,
    fecha_efectiva  = base_date + seq_len(n) * 30L,
    policy_ids = NA_character_,
    empresa = liab_row$empresa[1], parte = liab_row$parte[1],
    codigo_parte = liab_row$codigo_parte[1],
    moneda_pago = liab_row$moneda_pago[1], cotizado_en = NA_character_,
    amount_pago = liab_row$amount_default[1], amount_cotizado = NA_real_,
    fx_rate_used = NA_real_, componente_capital = NA_real_,
    componente_interes = NA_real_, componente_fees = NA_real_,
    componente_iva = NA_real_,
    amount_pago_override = NA_real_, amount_cotizado_override = NA_real_,
    fecha_efectiva_override = as.Date(NA),
    documento = "", referencia = "", notas = "",
    manual_inv_id = NA_character_, pagar_hoy_id = NA_character_,
    bancos_conf_id = NA_character_, reverted_count = 0L,
    generated_by = "test", generated_at = Sys.time(),
    last_edited_by = "test", last_edited_at = Sys.time()
  )
}

.orig_gen <- pasivos_generate_provisions
pasivos_generate_provisions <- .gen_prov

# W3a: Create flow → extend reconcile → only inserts
d_create <- .draft()
liab_new <- pasivos_wizard_draft_to_liability(d_create, mode = "create", user = "test")
save_pasivos_liabilities(liab_new)
gen <- pasivos_generate_provisions(liab_new)
existing_empty <- .schema_pasivos_provision()
result_create <- pasivos_reconcile_provisions(existing_empty, gen, mode = "extend")
.chk(nrow(result_create$insert),    12L, "W3: create → 12 inserts")
.chk(nrow(result_create$conflicts),  0L, "W3: create → 0 conflicts")

# W3b: Edit with no overrides → regenerate → all updated, 0 conflicts
gen2 <- pasivos_generate_provisions(liab_new)
result_edit <- pasivos_reconcile_provisions(gen, gen2, mode = "regenerate")
.chk(nrow(result_edit$conflicts), 0L, "W3: edit no overrides → 0 conflicts")

# W3c: Edit with 3 overrides → regenerate → 3 conflicts
# Manually simulate the conflict condition:
# Existing rows have override set; generated rows have a DIFFERENT amount_pago
existing_overridden <- gen
existing_overridden$amount_pago_override[1:3] <- 9999  # rows 1-3 have manual overrides

# gen_changed: same occurrence_index but different amount_pago (5500 vs 1500)
gen_changed <- gen
gen_changed$amount_pago <- 5500  # the new computed value differs from stored

result_with_conf <- pasivos_reconcile_provisions(existing_overridden, gen_changed,
                                                  mode = "regenerate")
.chk(nrow(result_with_conf$conflicts), 3L, "W3: edit 3 overridden → 3 conflicts")

# Restore original
pasivos_generate_provisions <- .orig_gen

# =============================================================================
# Group W4 — pasivos_liability_to_draft round-trip
# =============================================================================
cat("── W4. Liability to draft round-trip ─────────────────────────────────────\n")

# Build a liability row and convert to draft, then back
liab_rt <- pasivos_wizard_draft_to_liability(.draft(
  nombre = "Round Trip Test",
  empresa = "NTS",
  recurrence_type = "monthly_day",
  recurrence_params = list(day = 20L)
), mode = "create", user = "test")

draft_rt <- pasivos_liability_to_draft(liab_rt)
.chk(draft_rt$nombre,            "Round Trip Test", "W4: nombre round-trips")
.chk(draft_rt$empresa,           "NTS",             "W4: empresa round-trips")
.chk(draft_rt$recurrence_type,   "monthly_day",     "W4: recurrence_type round-trips")
.chk(draft_rt$recurrence_params$day, 20L,           "W4: recurrence_params day round-trips")

# ── Results ─────────────────────────────────────────────────────────────────
cat(sprintf("\nStage 4 Wizard: %d PASS  /  %d FAIL\n\n", .pass, .fail))
if (.fail > 0) stop(sprintf("%d test(s) failed.", .fail))
