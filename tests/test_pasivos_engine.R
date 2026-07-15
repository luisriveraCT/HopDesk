# =============================================================================
# tests/test_pasivos_engine.R
# Stage 1 engine tests.  Run from project root:
#   source("tests/test_pasivos_engine.R")
# No live S3 credentials required — S3 mocked with in-memory stubs.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(jsonlite)
  library(uuid)
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

source("R/bancos_persistence.R")
source("R/pasivos_persistence.R")
source("R/pasivos_capabilities.R")
source("R/pasivos_audit.R")
source("R/forecasting_service.R")
source("R/policy_engine.R")
source("R/pasivos_engine.R")

# ── Test runner ────────────────────────────────────────────────────────────────
.pass <- 0L; .fail <- 0L

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) {
    cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L
  } else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true  <- function(expr, label) .chk(isTRUE(expr), TRUE, label)
.chk_warn  <- function(expr, label) {
  w <- tryCatch({ expr; NULL }, warning = function(w) w$message)
  if (!is.null(w)) { cat(sprintf("  PASS  %s  [warn: %s]\n", label, w)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s  (no warning emitted)\n", label)); .fail <<- .fail + 1L }
}
.chk_error <- function(expr, label) {
  e <- tryCatch({ expr; NULL }, error = function(e) e$message)
  if (!is.null(e)) { cat(sprintf("  PASS  %s  [err: %s]\n", label, e)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s  (no error thrown)\n", label)); .fail <<- .fail + 1L }
}

# ── S3 mock ────────────────────────────────────────────────────────────────────
.mock_store    <- new.env(parent = emptyenv())
.real_s3_read  <- .s3_read
.real_s3_write <- .s3_write
.s3_read  <- function(key) .mock_store[[key]]
.s3_write <- function(obj, key) { .mock_store[[key]] <- obj; invisible(TRUE) }
.reset_mock <- function() rm(list = ls(.mock_store), envir = .mock_store)

# ── Synthetic liability builder ────────────────────────────────────────────────
.make_liability <- function(...) {
  base <- tibble::tibble(
    id = "l1", categoria = "regular", subcategoria = "servicios",
    flavor = NA_character_, nombre = "Test", empresa = "NG", parte = "Proveedor A",
    codigo_parte = "C001", referencia_default = "REF", documento_template = NA_character_,
    moneda_pago = "MXN", cotizado_en = NA_character_,
    recurrence_type = "monthly_day", recurrence_params = list(list(day = 15L)),
    amount_default = 1500, amount_default_cot = NA_real_,
    tarjeta_provision_source = NA_character_,
    tarjeta_closing_day = NA_integer_, tarjeta_due_day = NA_integer_,
    tarjeta_credit_limit = NA_real_,
    principal_original = NA_real_, saldo_capital = NA_real_,
    tasa_actual = NA_real_, tasa_tipo = "fija", tasa_spread = NA_real_,
    tasa_estimate_method = NA_character_,
    fecha_inicio = as.Date(NA), fecha_vence = as.Date(NA),
    plazo_meses = NA_integer_, dia_pago = NA_integer_,
    metodo_amortizacion = NA_character_,
    schedule_imported = list(NULL), cargos_iniciales = list(NULL),
    valor_residual = list(NULL), modifier_ids = list(character(0)),
    estado = "active", notas = NA_character_,
    created_by = "dev", created_at = Sys.time(),
    updated_by = "dev", updated_at = Sys.time()
  )
  overrides <- list(...)
  for (nm in names(overrides)) base[[nm]] <- overrides[[nm]]
  base
}

.make_financiero <- function(
    principal = 1200000, tasa = 12, n = 60,
    fecha_inicio = as.Date("2026-05-01"),
    fecha_vence  = as.Date("2031-05-01"),
    dia_pago = 1L, metodo = "francesa",
    saldo_capital = NA_real_,
    cargos = list(NULL), residual = list(NULL),
    tasa_tipo = "fija"
) {
  .make_liability(
    categoria = "financiero", flavor = "credito_simple",
    recurrence_type = NA_character_, recurrence_params = list(NULL),
    amount_default = NA_real_,
    principal_original = principal, saldo_capital = saldo_capital,
    tasa_actual = tasa, tasa_tipo = tasa_tipo, tasa_spread = 0,
    fecha_inicio = fecha_inicio, fecha_vence = fecha_vence,
    plazo_meses = as.integer(n), dia_pago = as.integer(dia_pago),
    metodo_amortizacion = metodo,
    cargos_iniciales = cargos, valor_residual = residual
  )
}

# =============================================================================
# Group 1 — Recurrence expansion
# =============================================================================
cat("── 1. Recurrence expansion ──────────────────────────────────────────────\n")

# monthly_day day=15, from=2026-05-04, n=24
r1 <- pasivos_expand_recurrence("monthly_day", list(day = 15L),
                                  as.Date("2026-05-04"), 24L)
.chk(length(r1), 24L, "G1: monthly_day n=24 returns 24 dates")
.chk(r1[1],  as.Date("2026-05-15"), "G1: monthly_day first = 2026-05-15")
.chk(r1[24], as.Date("2028-04-15"), "G1: monthly_day last = 2028-04-15")
.chk_true(all(lubridate::day(r1) == 15L), "G1: all dates are 15th")

# monthly_day day=31, from=2026-01-01, n=12 — Feb=28, Apr/Jun/Sep/Nov=30
r2 <- pasivos_expand_recurrence("monthly_day", list(day = 31L),
                                  as.Date("2026-01-01"), 12L)
.chk(r2[1],  as.Date("2026-01-31"), "G1: day=31 Jan=31")
.chk(r2[2],  as.Date("2026-02-28"), "G1: day=31 Feb=28 (non-leap)")
.chk(r2[4],  as.Date("2026-04-30"), "G1: day=31 Apr=30")
.chk(r2[6],  as.Date("2026-06-30"), "G1: day=31 Jun=30")
.chk(r2[9],  as.Date("2026-09-30"), "G1: day=31 Sep=30")
.chk(r2[11], as.Date("2026-11-30"), "G1: day=31 Nov=30")

# monthly_day day=31, from=2024-01-01, n=2 — Jan 31, Feb 29 (leap)
r3 <- pasivos_expand_recurrence("monthly_day", list(day = 31L),
                                  as.Date("2024-01-01"), 2L)
.chk(r3[1], as.Date("2024-01-31"), "G1: leap Jan=31")
.chk(r3[2], as.Date("2024-02-29"), "G1: leap Feb=29")

# monthly_nth_weekday 2nd Tuesday
r4 <- pasivos_expand_recurrence("monthly_nth_weekday", list(nth = 2L, wday = 2L),
                                  as.Date("2026-05-04"), 3L)
.chk(r4[1], as.Date("2026-05-12"), "G1: 2nd Tue May 2026 = 2026-05-12")
.chk(r4[2], as.Date("2026-06-09"), "G1: 2nd Tue Jun 2026 = 2026-06-09")
.chk(r4[3], as.Date("2026-07-14"), "G1: 2nd Tue Jul 2026 = 2026-07-14")

# monthly_nth_weekday last Monday (nth=-1) — find a 5-Monday month
# October 2026: Oct 5,12,19,26 — only 4 Mondays.  March 2026: Mar 2,9,16,23,30 — 5 Mondays
r5 <- pasivos_expand_recurrence("monthly_nth_weekday", list(nth = -1L, wday = 1L),
                                  as.Date("2026-03-01"), 1L)
.chk(r5[1], as.Date("2026-03-30"), "G1: last Mon March 2026 = 30th (5th Monday)")

# biweekly, anchor 2025-01-01, from=2026-05-04, n=3
r6 <- pasivos_expand_recurrence("biweekly", list(anchor_date = as.Date("2025-01-01")),
                                  as.Date("2026-05-04"), 3L)
.chk_true(all(r6 >= as.Date("2026-05-04")), "G1: biweekly all dates >= from")
.chk_true(all(diff(as.integer(r6)) == 14L),  "G1: biweekly exactly 14 days apart")

# weekly wday=5 (Fri), from Wednesday 2026-05-06, n=3
r7 <- pasivos_expand_recurrence("weekly", list(wday = 5L),
                                  as.Date("2026-05-06"), 3L)  # 2026-05-06 is a Wednesday
.chk(r7[1], as.Date("2026-05-08"), "G1: weekly Fri from Wed = 2026-05-08")
.chk_true(all(diff(as.integer(r7)) == 7L), "G1: weekly 7 days apart")

# quarterly anchor_month=2, day=15, from=2026-01-01, n=4
r8 <- pasivos_expand_recurrence("quarterly", list(anchor_month = 2L, day = 15L),
                                  as.Date("2026-01-01"), 4L)
.chk(r8[1], as.Date("2026-02-15"), "G1: quarterly q1=2026-02-15")
.chk(r8[2], as.Date("2026-05-15"), "G1: quarterly q2=2026-05-15")
.chk(r8[3], as.Date("2026-08-15"), "G1: quarterly q3=2026-08-15")
.chk(r8[4], as.Date("2026-11-15"), "G1: quarterly q4=2026-11-15")

# yearly month=12, day=31, n=3
r9 <- pasivos_expand_recurrence("yearly", list(month = 12L, day = 31L),
                                  as.Date("2026-01-01"), 3L)
.chk(r9[1], as.Date("2026-12-31"), "G1: yearly 2026")
.chk(r9[2], as.Date("2027-12-31"), "G1: yearly 2027")
.chk(r9[3], as.Date("2028-12-31"), "G1: yearly 2028")

# custom with 3 dates, n=5 requested → returns 3 (no error)
r10 <- pasivos_expand_recurrence(
  "custom",
  list(dates = as.Date(c("2026-06-01","2026-07-01","2026-08-01"))),
  as.Date("2026-05-01"), 5L
)
.chk(length(r10), 3L, "G1: custom returns only available dates (3 not 5)")

# =============================================================================
# Group 2 — Schedule generation
# =============================================================================
cat("── 2. Schedule generation ───────────────────────────────────────────────\n")
.reset_mock()

# 2a Francesa fixed-rate 60m
lia_f60 <- .make_financiero(principal = 1200000, tasa = 12, n = 60,
                              fecha_inicio = as.Date("2026-05-01"),
                              fecha_vence  = as.Date("2031-05-01"),
                              dia_pago = 1L, metodo = "francesa")
sched_f60 <- pasivos_generate_schedule(lia_f60, today = as.Date("2026-05-01"))
reg_rows <- sched_f60[sched_f60$origin == "regular", ]
.chk(nrow(reg_rows), 60L, "G2: francesa 60 regular rows")
total_capital <- sum(reg_rows$capital)
.chk_true(abs(total_capital - 1200000) < 1, "G2: sum(capital) == principal")
.chk_true(sched_f60$saldo_post[nrow(sched_f60)] <= 0.01, "G2: final saldo_post = 0")

# 2b Variable-rate francesa — period payments not constant
.reset_mock()
forecasting_set_estimate("sofr", as.Date("2026-01-01"), 5.0)
forecasting_set_estimate("sofr", as.Date("2027-01-01"), 5.5)
forecasting_set_estimate("sofr", as.Date("2028-01-01"), 6.0)
lia_var <- .make_financiero(principal = 1000000, tasa = 5, n = 36,
                              fecha_inicio = as.Date("2026-05-01"),
                              fecha_vence  = as.Date("2029-05-01"),
                              dia_pago = 1L, metodo = "francesa",
                              tasa_tipo = "variable_sofr")
sched_var <- suppressWarnings(pasivos_generate_schedule(lia_var, today = as.Date("2026-05-01")))
reg_var <- sched_var[sched_var$origin == "regular", ]
.chk_true(length(unique(round(reg_var$total, 2))) > 1L,
          "G2: variable-rate produces non-constant payments")
.chk_true(abs(sum(reg_var$capital) - 1000000) < 5, "G2: variable-rate total capital == principal")

# 2c Half-way within 5% — no re-amortization
.reset_mock()
# 24 months in: compute expected saldo for francesa 12%/12 at period 24
r_f <- 12 / 12 / 100
pmt_f <- 1200000 * r_f * (1 + r_f)^60 / ((1 + r_f)^60 - 1)
saldo_at_24 <- 1200000
for (k in 1:24) {
  int_k <- saldo_at_24 * r_f
  cap_k <- pmt_f - int_k
  saldo_at_24 <- saldo_at_24 - cap_k
}
# Declare saldo within 5%
lia_halfok <- .make_financiero(
  principal = 1200000, tasa = 12, n = 60,
  fecha_inicio = as.Date("2024-05-01"),
  fecha_vence  = as.Date("2029-05-01"),
  dia_pago = 1L, metodo = "francesa",
  saldo_capital = round(saldo_at_24 * 1.03, 2)  # 3% off — within 5%
)
sched_halfok <- pasivos_generate_schedule(lia_halfok, today = as.Date("2026-05-05"))
.chk_true(!any(sched_halfok$re_amortized, na.rm = TRUE),
          "G2: half-way within 5% → no re-amortization")

# 2d Half-way >5% off — re-amortizes future rows
lia_halfre <- .make_financiero(
  principal = 1200000, tasa = 12, n = 60,
  fecha_inicio = as.Date("2024-05-01"),
  fecha_vence  = as.Date("2029-05-01"),
  dia_pago = 1L, metodo = "francesa",
  saldo_capital = round(saldo_at_24 * 0.88, 2)  # 12% off — beyond 5%
)
sched_halfre <- pasivos_generate_schedule(lia_halfre, today = as.Date("2026-05-05"))
hist_rows_re <- sched_halfre[sched_halfre$historico, ]
fut_rows_re  <- sched_halfre[!sched_halfre$historico & sched_halfre$origin == "regular", ]
.chk_true(!any(hist_rows_re$re_amortized), "G2: historico rows not re_amortized")
.chk_true(all(fut_rows_re$re_amortized),   "G2: future rows re_amortized")
.chk_true(abs(sum(fut_rows_re$capital) - round(saldo_at_24 * 0.88, 2)) < 5,
          "G2: re-amortized capital sums to declared saldo")

# 2e Initial fees
lia_fees <- .make_financiero(
  principal = 1000000, tasa = 10, n = 12,
  fecha_inicio = as.Date("2026-06-01"),
  fecha_vence  = as.Date("2027-06-01"),
  dia_pago = 1L, metodo = "francesa",
  cargos = list(list(
    list(descripcion = "Apertura", monto = 5000, moneda = "MXN", fecha = "2026-06-01"),
    list(descripcion = "Notarial",  monto = 3000, moneda = "MXN", fecha = "2026-06-15")
  ))
)
sched_fees <- pasivos_generate_schedule(lia_fees, today = as.Date("2026-05-01"))
fee_rows <- sched_fees[sched_fees$origin == "initial_fee", ]
.chk(nrow(fee_rows), 2L, "G2: two initial_fee rows")
.chk_true(all(fee_rows$fees %in% c(5000, 3000)), "G2: fee amounts correct")

# 2f Residual replace_last
lia_resl <- .make_financiero(
  principal = 1000000, tasa = 10, n = 12,
  fecha_inicio = as.Date("2026-06-01"),
  fecha_vence  = as.Date("2027-06-01"),
  dia_pago = 1L, metodo = "francesa",
  residual = list(list(behavior = "replace_last", monto = 200000))
)
sched_resl <- pasivos_generate_schedule(lia_resl, today = as.Date("2026-05-01"))
last_reg <- sched_resl[sched_resl$origin == "regular", ]
last_row  <- last_reg[nrow(last_reg), ]
.chk(last_row$total, 200000, "G2: residual replace_last sets total=200000")
.chk_true(abs(last_row$capital + last_row$interes + last_row$fees - last_row$total) < 0.01,
          "G2: residual replace_last row still balances")

# 2g Residual add_same_date
lia_ress <- .make_financiero(
  principal = 1000000, tasa = 10, n = 12,
  fecha_inicio = as.Date("2026-06-01"),
  fecha_vence  = as.Date("2027-06-01"),
  dia_pago = 1L, metodo = "francesa",
  residual = list(list(behavior = "add_same_date", monto = 50000))
)
sched_ress <- pasivos_generate_schedule(lia_ress, today = as.Date("2026-05-01"))
res_row <- sched_ress[sched_ress$origin == "residual", ]
reg_last <- sched_ress[sched_ress$origin == "regular", ]
.chk(nrow(res_row), 1L,           "G2: add_same_date has one residual row")
.chk(res_row$total, 50000,         "G2: residual monto correct")
.chk(res_row$fecha, reg_last$fecha[nrow(reg_last)], "G2: residual on same date as last regular")

# 2h Residual add_later_date
lia_resd <- .make_financiero(
  principal = 1000000, tasa = 10, n = 12,
  fecha_inicio = as.Date("2026-06-01"),
  fecha_vence  = as.Date("2027-06-01"),
  dia_pago = 1L, metodo = "francesa",
  residual = list(list(behavior = "add_later_date", monto = 50000, fecha = "2028-01-01"))
)
sched_resd <- pasivos_generate_schedule(lia_resd, today = as.Date("2026-05-01"))
res_row_d <- sched_resd[sched_resd$origin == "residual", ]
.chk(res_row_d$fecha, as.Date("2028-01-01"), "G2: add_later_date residual fecha correct")

# 2i Imported schedule wins
imp_sched <- tibble::tibble(
  periodo = 1:3, fecha = as.Date(c("2026-06-01","2026-07-01","2026-08-01")),
  capital = c(100,100,100), interes = c(10,9,8), fees = c(0,0,0),
  total = c(110,109,108), saldo_post = c(900,800,700)
)
lia_imp <- .make_financiero(
  n = 3, fecha_inicio = as.Date("2026-05-01"), fecha_vence = as.Date("2026-08-01"),
  metodo = "custom"
)
lia_imp$schedule_imported <- list(imp_sched)
sched_imp <- pasivos_generate_schedule(lia_imp, today = as.Date("2026-05-01"))
.chk(nrow(sched_imp), 3L, "G2: imported schedule — 3 rows returned as-is")

# 2j Custom without imported → error
lia_nomp <- .make_financiero(n = 3, metodo = "custom")
lia_nomp$schedule_imported <- list(NULL)
.chk_error(pasivos_generate_schedule(lia_nomp), "G2: custom without schedule_imported throws")

# 2k Variable-rate with NA forecast → falls back, warns once
.reset_mock()
# no estimates → forecasting returns NA → should warn once
lia_varNA <- .make_financiero(
  principal = 600000, tasa = 8, n = 6,
  fecha_inicio = as.Date("2026-05-01"),
  fecha_vence  = as.Date("2026-11-01"),
  dia_pago = 1L, metodo = "francesa",
  tasa_tipo = "variable_sofr"
)
.chk_warn(pasivos_generate_schedule(lia_varNA, today = as.Date("2026-05-01")),
          "G2: variable NA forecast emits warning")

# =============================================================================
# Group 2e — .periods_per_year (frecuencia_pago)
# =============================================================================
cat("── 2e. .periods_per_year ────────────────────────────────────────────────\n")
.reset_mock()

.chk(.periods_per_year("mensual"),   12L, "G2e: mensual → 12 periods/yr")
.chk(.periods_per_year("anual"),      1L, "G2e: anual → 1 period/yr")
.chk(.periods_per_year("semanal"),   52L, "G2e: semanal → 52 periods/yr")
.chk(.periods_per_year("diaria"),   365L, "G2e: diaria → 365 periods/yr")
.chk(.periods_per_year("quincenal"), 12L, "G2e: unknown frequency defaults to 12")

# =============================================================================
# Group 2f — .add_payment_period (frecuencia_pago)
# =============================================================================
cat("── 2f. .add_payment_period ──────────────────────────────────────────────\n")
.reset_mock()

# mensual — month-end clamping and plain advance
.chk(.add_payment_period(as.Date("2026-01-31"), 1L, "mensual", 28L),
     as.Date("2026-02-28"), "G2f: mensual dia_pago=28 Jan31+1mo → Feb28")
.chk(.add_payment_period(as.Date("2026-01-15"), 3L, "mensual", 15L),
     as.Date("2026-04-15"), "G2f: mensual Jan15 +3mo → Apr15")

# anual — preserves month/day; obeys mes_pago override
.chk(.add_payment_period(as.Date("2026-05-01"), 1L, "anual", 1L,
                          dia_semana_pago = NA_integer_, mes_pago = NA_integer_),
     as.Date("2027-05-01"), "G2f: anual +1yr preserves month/day")
.chk(.add_payment_period(as.Date("2026-01-01"), 2L, "anual", 15L,
                          dia_semana_pago = NA_integer_, mes_pago = 3L),
     as.Date("2028-03-15"), "G2f: anual +2yr with mes_pago=3 dia_pago=15 → 2028-03-15")

# semanal — no weekday snap vs. explicit snap
# 2026-06-01 is a Monday; anchor k=1 lands on 2026-06-08 (also Monday)
.chk(.add_payment_period(as.Date("2026-06-01"), 1L, "semanal", NA_integer_,
                          dia_semana_pago = NA_integer_),
     as.Date("2026-06-08"), "G2f: semanal no dia_semana_pago → plain +7d")
.chk(.add_payment_period(as.Date("2026-06-01"), 1L, "semanal", NA_integer_,
                          dia_semana_pago = 5L),
     as.Date("2026-06-12"), "G2f: semanal dia_semana_pago=5 (Fri) → nearest Friday")

# diaria — plain +k days
.chk(.add_payment_period(as.Date("2026-06-01"), 7L, "diaria", NA_integer_),
     as.Date("2026-06-08"), "G2f: diaria k=7 → +7 days")

# =============================================================================
# Group 2g — pasivos_generate_schedule with non-mensual frequencies
# =============================================================================
cat("── 2g. Non-mensual schedule generation ──────────────────────────────────\n")
.reset_mock()

# 2g.1 Semanal — 12 weekly payments; dia_pago=NA so dates are exactly +7d each
lia_sem <- .make_financiero(
  principal    = 120000, tasa = 12, n = 12L,
  fecha_inicio = as.Date("2026-06-01"),
  fecha_vence  = as.Date("2026-08-24"),
  dia_pago     = NA_integer_, metodo = "francesa"
)
lia_sem$frecuencia_pago <- "semanal"
sched_sem <- pasivos_generate_schedule(lia_sem, today = as.Date("2026-06-01"))
reg_sem   <- sched_sem[sched_sem$origin == "regular", ]
.chk(nrow(reg_sem), 12L, "G2g: semanal 12 periods → 12 rows")
.chk_true(abs(sum(reg_sem$capital) - 120000) < 1, "G2g: semanal sum(capital) == principal")
.chk(reg_sem$fecha[1], as.Date("2026-06-08"),      "G2g: semanal first payment = base + 7d")
.chk_true(all(as.integer(diff(reg_sem$fecha)) == 7L),
          "G2g: semanal consecutive payment dates all 7 days apart")

# 2g.2 Anual — 3 annual payments; dates must land on same day one year apart
lia_an <- .make_financiero(
  principal    = 300000, tasa = 12, n = 3L,
  fecha_inicio = as.Date("2026-01-01"),
  fecha_vence  = as.Date("2029-01-01"),
  dia_pago     = 1L, metodo = "francesa"
)
lia_an$frecuencia_pago <- "anual"
sched_an <- pasivos_generate_schedule(lia_an, today = as.Date("2026-01-01"))
reg_an   <- sched_an[sched_an$origin == "regular", ]
.chk(nrow(reg_an), 3L, "G2g: anual 3 periods → 3 rows")
.chk_true(abs(sum(reg_an$capital) - 300000) < 1, "G2g: anual sum(capital) == principal")
.chk(reg_an$fecha[1], as.Date("2027-01-01"), "G2g: anual first payment one year after base")
.chk(reg_an$fecha[3], as.Date("2029-01-01"), "G2g: anual third payment three years after base")

# 2g.3 Diaria — 30 daily payments; all consecutive gaps exactly 1 day
lia_day <- .make_financiero(
  principal    = 30000, tasa = 12, n = 30L,
  fecha_inicio = as.Date("2026-06-01"),
  fecha_vence  = as.Date("2026-07-01"),
  dia_pago     = NA_integer_, metodo = "francesa"
)
lia_day$frecuencia_pago <- "diaria"
sched_day <- pasivos_generate_schedule(lia_day, today = as.Date("2026-06-01"))
reg_day   <- sched_day[sched_day$origin == "regular", ]
.chk(nrow(reg_day), 30L, "G2g: diaria 30 periods → 30 rows")
.chk_true(abs(sum(reg_day$capital) - 30000) < 1, "G2g: diaria sum(capital) == principal")
.chk_true(all(as.integer(diff(reg_day$fecha)) == 1L),
          "G2g: diaria consecutive payment dates all 1 day apart")

# =============================================================================
# Group 3 — Currency wiring
# =============================================================================
cat("── 3. Currency wiring ───────────────────────────────────────────────────\n")
.reset_mock()

# pasivos_pago_currency NA → throws
.chk_error(pasivos_pago_currency(list(id = "x", moneda_pago = NA_character_)),
           "G3: pago_currency NA throws")

# pasivos_pago_currency MXN → "MXN"
.chk(pasivos_pago_currency(list(id = "x", moneda_pago = "MXN")), "MXN",
     "G3: pago_currency returns MXN")

# pasivos_pago_amount with override
.chk(pasivos_pago_amount(list(amount_pago_override = 5000, amount_pago = 4800)), 5000,
     "G3: pago_amount returns override")

# pasivos_pago_amount without override
.chk(pasivos_pago_amount(list(amount_pago_override = NA_real_, amount_pago = 4800)), 4800,
     "G3: pago_amount returns amount_pago when no override")

# recompute with cotizado_en = NA → returns amount unchanged
res_na <- pasivos_recompute_pago_from_cotizado(1000, NA, "MXN", Sys.Date())
.chk(res_na$amount_pago,   1000,    "G3: recompute NA cot → amount_pago unchanged")
.chk_true(is.na(res_na$fx_rate_used), "G3: recompute NA cot → fx_rate_used NA")

# recompute with cotizado_en == moneda_pago → same
res_eq <- pasivos_recompute_pago_from_cotizado(1000, "MXN", "MXN", Sys.Date())
.chk(res_eq$amount_pago, 1000, "G3: recompute same currency → unchanged")

# recompute with USD→MXN, FX=19.50
forecasting_set_estimate("fx_usd_mxn", Sys.Date(), 19.50)
res_fx <- pasivos_recompute_pago_from_cotizado(100, "USD", "MXN", Sys.Date())
.chk(res_fx$amount_pago,  1950, "G3: recompute USD→MXN = 100*19.5=1950")
.chk(res_fx$fx_rate_used, 19.5, "G3: fx_rate_used = 19.5")

# recompute with forecasting NA → returns NA, warns
.reset_mock()
.chk_warn(
  pasivos_recompute_pago_from_cotizado(100, "USD", "MXN", Sys.Date()),
  "G3: recompute with NA FX warns"
)
res_na_fx <- suppressWarnings(
  pasivos_recompute_pago_from_cotizado(100, "USD", "MXN", Sys.Date())
)
.chk_true(is.na(res_na_fx$amount_pago), "G3: recompute NA FX returns NA amount_pago")

# Static analysis: no cotizado_en reads outside allowlist
cat("  [static] scanning R/ for cotizado_en outside allowlist...\n")
allowlist <- c("pasivos_engine.R", "pasivos_schemas.R", "pasivos_persistence.R",
               "pasivos_module.R")
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
violations <- character(0)
for (f in r_files) {
  bn <- basename(f)
  if (bn %in% allowlist) next
  lines <- readLines(f, warn = FALSE)
  hits  <- grep("cotizado_en", lines)
  if (length(hits)) {
    violations <- c(violations, paste0(f, ":", hits))
  }
}
.chk(length(violations), 0L,
     sprintf("G3: static analysis — no cotizado_en outside allowlist (found %d: %s)",
             length(violations), paste(violations, collapse = ", ")))

# =============================================================================
# Group 4 — Provision generation
# =============================================================================
cat("── 4. Provision generation ──────────────────────────────────────────────\n")
.reset_mock()

# 4a Regular monthly, 24-month horizon
lia_reg <- .make_liability(
  recurrence_type = "monthly_day",
  recurrence_params = list(list(day = 15L)),
  amount_default = 1500, moneda_pago = "MXN"
)
provs_reg <- pasivos_generate_provisions(
  lia_reg,
  window_start = as.Date("2026-05-01"),
  window_end   = as.Date("2028-04-30")
)
.chk_true(nrow(provs_reg) >= 23L && nrow(provs_reg) <= 25L,
          "G4: regular 24m generates ~24 provisions")
.chk_true(all(provs_reg$estado == "provisional"), "G4: all estado=provisional")
.chk_true(all(provs_reg$amount_pago == 1500),     "G4: all amount_pago=1500")

# 4b Regular with cotizado_en=USD
.reset_mock()
forecasting_set_estimate("fx_usd_mxn", as.Date("2026-05-01"), 19.50)
lia_usd <- .make_liability(
  moneda_pago = "MXN", cotizado_en = "USD",
  amount_default = NA_real_, amount_default_cot = 100,
  recurrence_type = "monthly_day",
  recurrence_params = list(list(day = 15L))
)
provs_usd <- suppressWarnings(pasivos_generate_provisions(
  lia_usd,
  window_start = as.Date("2026-05-01"),
  window_end   = as.Date("2026-07-31")
))
if (nrow(provs_usd) > 0) {
  .chk(provs_usd$amount_cotizado[1], 100,  "G4: amount_cotizado=100")
  .chk(provs_usd$amount_pago[1],    1950,  "G4: amount_pago=1950")
  .chk(provs_usd$fx_rate_used[1],   19.5,  "G4: fx_rate_used=19.5")
} else {
  .chk(FALSE, TRUE, "G4: USD provisions generated at least one row")
}

# 4c Financiero, started 24m ago → future provisions start at occurrence_index 25
.reset_mock()
lia_fin_past <- .make_financiero(
  principal = 6000000, tasa = 10, n = 60,
  fecha_inicio = as.Date("2024-05-01"),
  fecha_vence  = as.Date("2029-05-01"),
  dia_pago = 1L, metodo = "francesa"
)
provs_fin <- pasivos_generate_provisions(
  lia_fin_past,
  window_start = as.Date("2026-05-05")
)
.chk_true(nrow(provs_fin) > 0, "G4: financiero past generates future provisions")
.chk_true(min(provs_fin$occurrence_index) >= 25L,
          "G4: financiero past — occurrence_index starts >= 25")

# 4d Empty policies → fecha_efectiva == fecha_calculada
.reset_mock()
provs_nopol <- pasivos_generate_provisions(
  lia_reg,
  window_start = as.Date("2026-05-01"),
  window_end   = as.Date("2026-07-31"),
  policies_for_liability = list()
)
.chk_true(all(provs_nopol$fecha_efectiva == provs_nopol$fecha_calculada),
          "G4: empty policies → fecha_efectiva == fecha_calculada")

# =============================================================================
# Group 5 — Reconciliation
# =============================================================================
cat("── 5. Reconciliation ────────────────────────────────────────────────────\n")

.make_prov_row <- function(i, estado = "provisional",
                             ov = NA_real_, ov_cot = NA_real_,
                             ov_fecha = as.Date(NA),
                             liability_id = "l1",
                             origin = "rule",
                             amount_pago = 1500) {
  tibble::tibble(
    id = paste0("p", i), liability_id = liability_id,
    origin = origin, occurrence_index = as.integer(i),
    estado = estado,
    fecha_calculada = as.Date("2026-06-01") + months(i - 1L),
    fecha_efectiva  = as.Date("2026-06-01") + months(i - 1L),
    policy_ids = "", empresa = "NG", parte = "A", codigo_parte = "C001",
    moneda_pago = "MXN", cotizado_en = NA_character_,
    amount_pago = amount_pago, amount_cotizado = NA_real_, fx_rate_used = NA_real_,
    componente_capital = NA_real_, componente_interes = NA_real_,
    componente_fees = NA_real_, componente_iva = NA_real_,
    amount_pago_override = ov, amount_cotizado_override = ov_cot,
    fecha_efectiva_override = ov_fecha,
    documento = NA_character_, referencia = NA_character_, notas = NA_character_,
    manual_inv_id = NA_character_, pagar_hoy_id = NA_character_,
    bancos_conf_id = NA_character_, reverted_count = 0L,
    generated_by = "system", generated_at = Sys.time(),
    last_edited_by = NA_character_, last_edited_at = as.POSIXct(NA)
  )
}

# 5a extend: existing 24, generated 36 → keep=24, insert=12
existing_24  <- dplyr::bind_rows(lapply(1:24, .make_prov_row))
generated_36 <- dplyr::bind_rows(lapply(1:36, function(i)
  .make_prov_row(i, amount_pago = 1600)))  # changed amount to test extend doesn't update
rec_ext <- pasivos_reconcile_provisions(existing_24, generated_36, mode = "extend")
.chk(nrow(rec_ext$keep),   24L, "G5: extend keep=24")
.chk(nrow(rec_ext$update),  0L, "G5: extend update=0")
.chk(nrow(rec_ext$insert), 12L, "G5: extend insert=12")
.chk(nrow(rec_ext$conflicts), 0L, "G5: extend conflicts=0")

# 5b regenerate, no overrides → all 24 update
existing_24b <- dplyr::bind_rows(lapply(1:24, .make_prov_row))
gen_24b      <- dplyr::bind_rows(lapply(1:24, function(i)
  .make_prov_row(i, amount_pago = 1600)))
rec_reg <- pasivos_reconcile_provisions(existing_24b, gen_24b, mode = "regenerate")
.chk(nrow(rec_reg$update),    24L, "G5: regenerate no-override → update=24")
.chk(nrow(rec_reg$keep),       0L, "G5: regenerate no-override → keep=0 (rule rows)")
.chk(nrow(rec_reg$insert),     0L, "G5: regenerate no-override → insert=0")
.chk(nrow(rec_reg$conflicts),  0L, "G5: regenerate no-override → conflicts=0")

# 5c regenerate, 3 of 24 with amount_pago_override → 21 update, 3 conflict
existing_24c <- dplyr::bind_rows(c(
  lapply(1:21, .make_prov_row),
  lapply(22:24, function(i) .make_prov_row(i, ov = 9999))
))
gen_24c <- dplyr::bind_rows(lapply(1:24, function(i)
  .make_prov_row(i, amount_pago = 1600)))
rec_ov <- pasivos_reconcile_provisions(existing_24c, gen_24c, mode = "regenerate")
.chk(nrow(rec_ov$update),    21L, "G5: override → update=21")
.chk(nrow(rec_ov$conflicts),  3L, "G5: override → conflicts=3")

# 5d regenerate, 5 converted → those 5 kept
existing_conv <- dplyr::bind_rows(c(
  lapply(1:19, .make_prov_row),
  lapply(20:24, function(i) .make_prov_row(i, estado = "converted"))
))
gen_conv <- dplyr::bind_rows(lapply(1:24, function(i)
  .make_prov_row(i, amount_pago = 1600)))
rec_conv <- pasivos_reconcile_provisions(existing_conv, gen_conv, mode = "regenerate")
converted_kept <- rec_conv$keep[rec_conv$keep$estado == "converted", ]
.chk(nrow(converted_kept), 5L, "G5: converted rows go to keep")

# 5e regenerate, 2 item_confirmed → kept
existing_ic <- dplyr::bind_rows(c(
  lapply(1:22, .make_prov_row),
  lapply(23:24, function(i) .make_prov_row(i, estado = "item_confirmed"))
))
gen_ic <- dplyr::bind_rows(lapply(1:24, function(i)
  .make_prov_row(i, amount_pago = 1600)))
rec_ic <- pasivos_reconcile_provisions(existing_ic, gen_ic, mode = "regenerate")
ic_kept <- rec_ic$keep[rec_ic$keep$estado == "item_confirmed", ]
.chk(nrow(ic_kept), 2L, "G5: item_confirmed rows go to keep")

# 5f Orphan manual provisions always in keep
orphan <- .make_prov_row(99L, origin = "manual")
orphan$liability_id <- NA_character_
existing_orph <- dplyr::bind_rows(dplyr::bind_rows(lapply(1:5, .make_prov_row)), orphan)
gen_orph <- dplyr::bind_rows(lapply(1:5, function(i)
  .make_prov_row(i, amount_pago = 1600)))
rec_orph <- pasivos_reconcile_provisions(existing_orph, gen_orph, mode = "regenerate")
orph_kept <- rec_orph$keep[is.na(rec_orph$keep$liability_id), ]
.chk(nrow(orph_kept), 1L, "G5: orphan manual provision in keep regardless of mode")

# 5g rate_update: only componente_interes + derived amount_pago changes; capital unchanged
existing_ru <- dplyr::bind_rows(lapply(1:5, function(i) {
  r <- .make_prov_row(i)
  r$componente_capital   <- 10000
  r$componente_interes   <- 500
  r$componente_fees      <- 0
  r$amount_pago          <- 10500
  r
}))
gen_ru <- dplyr::bind_rows(lapply(1:5, function(i) {
  r <- .make_prov_row(i)
  r$componente_capital   <- 10000
  r$componente_interes   <- 600   # rate went up
  r$componente_fees      <- 0
  r$amount_pago          <- 10600
  r
}))
rec_ru <- pasivos_reconcile_provisions(existing_ru, gen_ru, mode = "rate_update")
.chk_true(all(rec_ru$update$componente_capital == 10000), "G5: rate_update capital unchanged")
.chk_true(all(rec_ru$update$componente_interes == 600),   "G5: rate_update interes updated")
.chk_true(all(rec_ru$update$amount_pago == 10600),        "G5: rate_update amount_pago recomputed")

# 5h rate_update skips rows with amount_pago_override
existing_ru_ov <- dplyr::bind_rows(c(
  lapply(1:4, function(i) {
    r <- .make_prov_row(i)
    r$componente_capital <- 10000; r$componente_interes <- 500
    r$componente_fees <- 0; r$amount_pago <- 10500; r
  }),
  list({
    r <- .make_prov_row(5L)
    r$componente_capital <- 10000; r$componente_interes <- 500
    r$componente_fees <- 0; r$amount_pago <- 10500
    r$amount_pago_override <- 99999; r
  })
))
gen_ru_ov <- gen_ru
rec_ru_ov <- pasivos_reconcile_provisions(existing_ru_ov, gen_ru_ov, mode = "rate_update")
ov_row <- rec_ru_ov$keep[!is.na(rec_ru_ov$keep$amount_pago_override), ]
.chk(nrow(ov_row), 1L,   "G5: rate_update skips override row → in keep")
.chk(nrow(rec_ru_ov$update), 4L, "G5: rate_update updates only non-override rows")

# =============================================================================
# Group 6 — Lifecycle
# =============================================================================
cat("── 6. Lifecycle ─────────────────────────────────────────────────────────\n")
.reset_mock()

seed_prov <- dplyr::bind_rows(lapply(1:3, function(i) .make_prov_row(i)))
save_pasivos_provisions(seed_prov)

# convert → item_confirmed → close
pasivos_provision_convert("p1", manual_inv_id = "m1", pagar_hoy_id = "ph1", user = "dev")
pasivos_provision_item_confirmed("p1", bancos_conf_id = "bc1", user = "dev")
pasivos_provision_close("p1", user = "dev")
final_p1 <- load_pasivos_provisions()
final_p1 <- final_p1[final_p1$id == "p1", ]
.chk(final_p1$estado, "closed", "G6: convert→confirm→close final estado=closed")

audit_log <- load_pasivos_audit()
p1_audit  <- audit_log[!is.na(audit_log$target_id) & audit_log$target_id == "p1", ]
.chk_true(nrow(p1_audit) >= 3L, "G6: at least 3 audit entries for p1")

# convert → revive (p2 is already in the store from the initial seed)
pasivos_provision_convert("p2", "m2", "ph2", "dev")
pasivos_provision_revive("p2", "dev")
final_p2 <- load_pasivos_provisions()
final_p2 <- final_p2[final_p2$id == "p2", ]
.chk(final_p2$estado, "provisional",     "G6: after revive → provisional")
.chk_true(is.na(final_p2$manual_inv_id), "G6: revive clears manual_inv_id")
.chk_true(is.na(final_p2$pagar_hoy_id),  "G6: revive clears pagar_hoy_id")
.chk(final_p2$reverted_count, 1L,         "G6: reverted_count=1 after revive")

# re-revive (already provisional) → warns, no-op
.chk_warn(pasivos_provision_revive("p2", "dev"),
          "G6: re-revive warns and no-ops")
final_p2b <- load_pasivos_provisions()
final_p2b <- final_p2b[final_p2b$id == "p2", ]
.chk(final_p2b$reverted_count, 1L, "G6: re-revive does not increment reverted_count")

# audit round-trip via jsonlite
audit_after <- load_pasivos_audit()
last_entry  <- audit_after[nrow(audit_after), ]
parsed_before <- tryCatch(jsonlite::fromJSON(last_entry$payload_before), error = function(e) NULL)
.chk_true(!is.null(parsed_before), "G6: audit payload_before round-trips via fromJSON")

# convert on non-provisional (already closed) → throws
.chk_error(pasivos_provision_convert("p1", "m_x", "ph_x", "dev"),
           "G6: convert on closed provision throws")

# reverted_count increments correctly on repeated revives (p3 already in store)
pasivos_provision_convert("p3", "m3", "ph3", "dev")
pasivos_provision_revive("p3", "dev")
pasivos_provision_convert("p3", "m3b", "ph3b", "dev")
pasivos_provision_revive("p3", "dev")
final_p3 <- load_pasivos_provisions()
final_p3 <- final_p3[final_p3$id == "p3", ]
.chk(final_p3$reverted_count, 2L, "G6: reverted_count=2 after two revives")

# =============================================================================
# Group 7 — Modifiers
# =============================================================================
cat("── 7. Modifiers ─────────────────────────────────────────────────────────\n")
.reset_mock()

.make_mod <- function(id, scope_type = "global", scope_id = NA_character_,
                       type = "fx_rate", target_field = "amount_pago",
                       frozen_value = NA_real_, estimate_method = NA_character_,
                       enabled = TRUE,
                       created_at = as.POSIXct("2026-01-01")) {
  tibble::tibble(
    id = id, scope_type = scope_type, scope_id = scope_id,
    type = type, target_field = target_field,
    estimate_method = estimate_method, frozen_value = frozen_value,
    enabled = enabled, display_label = id,
    created_by = "dev", created_at = created_at,
    updated_at = created_at
  )
}

base_prov <- list(
  id = "prov_x", liability_id = "l1",
  moneda_pago = "MXN", cotizado_en = "USD",
  fecha_efectiva = as.Date("2026-06-15"),
  amount_pago = 1000, amount_cotizado = 50,
  fx_rate_used = 19.5,
  componente_capital = 800, componente_interes = 150,
  componente_fees = 50, componente_iva = NA_real_,
  amount_pago_override = NA_real_,
  amount_cotizado_override = NA_real_,
  fecha_efectiva_override = as.Date(NA)
)

# 7a single fx_rate multiplier frozen=19.5
mods_fx <- .make_mod("m1", frozen_value = 19.5)
res_fx <- pasivos_apply_modifiers(as.list(base_prov), mods_fx)
.chk(res_fx$amount_pago, 1000 * 19.5, "G7: fx_rate modifier multiplies amount_pago")

# 7b two modifiers (FX × inflation), multiplicative
mods_2 <- dplyr::bind_rows(
  .make_mod("m_fx",  frozen_value = 1.05, type = "fx_rate",
            target_field = "amount_pago",
            created_at = as.POSIXct("2026-01-01")),
  .make_mod("m_inf", frozen_value = 1.03, type = "inflation_index",
            target_field = "amount_pago",
            created_at = as.POSIXct("2026-01-02"))
)
res_2 <- pasivos_apply_modifiers(as.list(base_prov), mods_2)
expected_2 <- 1000 * 1.05 * 1.03
.chk_true(abs(res_2$amount_pago - expected_2) < 0.01, "G7: two multiplicative modifiers compose")

# 7c disabled modifier → ignored
mods_dis <- .make_mod("m_off", frozen_value = 99, enabled = FALSE)
res_dis <- pasivos_apply_modifiers(as.list(base_prov), mods_dis)
.chk(res_dis$amount_pago, 1000, "G7: disabled modifier ignored")

# 7d modifier scoped to different liability_id → ignored
mods_scope <- .make_mod("m_other", scope_type = "liability", scope_id = "other_l",
                          frozen_value = 99)
res_scope <- pasivos_apply_modifiers(as.list(base_prov), mods_scope)
.chk(res_scope$amount_pago, 1000, "G7: wrong liability scope → ignored")

# 7e provision-scoped modifier matching this provision → applied
mods_ps <- .make_mod("m_ps", scope_type = "provision", scope_id = "prov_x",
                       frozen_value = 2)
res_ps <- pasivos_apply_modifiers(as.list(base_prov), mods_ps)
.chk(res_ps$amount_pago, 1000 * 2, "G7: provision-scoped modifier applied")

# 7f frozen value beats live estimate
forecasting_set_estimate("fx_usd_mxn", as.Date("2026-06-15"), 25.0)
mods_frozen <- .make_mod("m_fz", frozen_value = 19.5, estimate_method = "spot",
                           type = "fx_rate", target_field = "amount_pago")
res_fz <- pasivos_apply_modifiers(as.list(base_prov), mods_frozen)
.chk(res_fz$amount_pago, 1000 * 19.5, "G7: frozen value beats live estimate")

# 7g interest_rate_sofr modifier recomputes interes and amount_pago
mods_ir <- .make_mod("m_ir", type = "interest_rate_sofr",
                       target_field = "componente_interes",
                       frozen_value = 5.5)  # 5.5% pa → 5.5/12/100 per period
res_ir <- pasivos_apply_modifiers(as.list(base_prov), mods_ir)
expected_int <- base_prov$componente_capital * (5.5 / 12 / 100)
.chk_true(abs(res_ir$componente_interes - expected_int) < 0.001,
          "G7: interest_rate_sofr recomputes interes")
.chk_true(abs(res_ir$amount_pago - (base_prov$componente_capital + expected_int + base_prov$componente_fees)) < 0.001,
          "G7: amount_pago recomputed from new interes")

# 7h provision with amount_pago_override → all modifiers skipped
prov_ov <- base_prov
prov_ov$amount_pago_override <- 5000
res_ov <- pasivos_apply_modifiers(as.list(prov_ov), mods_fx)
.chk(res_ov$amount_pago, 1000, "G7: amount_pago_override → modifiers skipped, value unchanged")

# =============================================================================
# Group 8 — Hygiene
# =============================================================================
cat("── 8. Hygiene ───────────────────────────────────────────────────────────\n")

# isTRUE( grep across pasivos files
pasivos_files <- list.files("R", pattern = "^pasivos", full.names = TRUE)
itrue_violations <- character(0)
for (f in pasivos_files) {
  lines <- readLines(f, warn = FALSE)
  hits  <- grep("isTRUE\\(", lines)
  for (h in hits) {
    line_txt <- trimws(lines[h])
    # Flag any isTRUE applied to a data frame column access ($ pattern)
    if (grepl("isTRUE\\(.*\\$", line_txt)) {
      itrue_violations <- c(itrue_violations, sprintf("%s:%d: %s", f, h, line_txt))
    }
  }
}
if (length(itrue_violations) == 0L) {
  cat("  PASS  G8: no isTRUE(df$col) patterns found in pasivos files\n")
  .pass <- .pass + 1L
} else {
  cat(sprintf("  FAIL  G8: isTRUE(df$col) found:\n%s\n",
              paste(itrue_violations, collapse = "\n")))
  .fail <- .fail + 1L
}

# Stage 0 regression — run in a subprocess to avoid .GlobalEnv contamination
# (Stage 0 sources its R files without local=TRUE, so they land in .GlobalEnv;
#  running it as a subprocess gives it a pristine .GlobalEnv with the mock intact.)
cat("── Regression: Stage 0 tests ────────────────────────────────────────────\n")
rscript_bin <- file.path(R.home("bin"), "Rscript")
out <- tryCatch(
  system2(rscript_bin, args = "tests/test_pasivos_stage0.R",
          stdout = TRUE, stderr = TRUE),
  error = function(e) { attr(structure(character(0), status = 1L), "status"); character(0) }
)
exit_ok <- is.null(attr(out, "status")) || attr(out, "status") == 0L
if (exit_ok) {
  cat("  PASS  Stage 0 regression (subprocess)\n"); .pass <- .pass + 1L
} else {
  fails <- grep("FAIL|Error", out, value = TRUE)
  cat(sprintf("  FAIL  Stage 0 regression: %s\n", paste(fails, collapse = " | ")))
  .fail <- .fail + 1L
}

# ── Summary ────────────────────────────────────────────────────────────────────
cat(sprintf(
  "\n── Results: %d passed, %d failed ────────────────────────────────────────\n",
  .pass, .fail
))
if (.fail > 0L) stop(sprintf("%d test(s) failed.", .fail))

.s3_read  <- .real_s3_read
.s3_write <- .real_s3_write
