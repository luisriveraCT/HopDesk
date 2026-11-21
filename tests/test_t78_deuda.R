# =============================================================================
# tests/test_t78_deuda.R
# T78: Pasivos · "Costo de la deuda" (R/pasivos_deuda.R), pure functions.
#   A  a French loan worked out by hand: 120,000 MXN at 12 % effective a year,
#      12 monthly payments, one already paid: EAR = the captured rate,
#      nominal = 12 ((1 + EAR)^(1/12) - 1), balance, interest in 12 months,
#      share due, next payment; XIRR = EAR (no fees, first payment one month
#      after the start);
#   B  the same with an opening fee: the XIRR goes above the EAR, and equals
#      an IRR worked out here with its own bisection;
#   C  a card: debt = its pending provisions (overrides win), no rate, out of
#      the weighted cost, no interest;
#   D  a regular payment and a pure lease are not debt; the regular one is
#      only in the footer's cost per year;
#   E  two currencies are never added (summary and service);
#   F  the 12 months' service: capital and interest by month, per currency.
# Run from the repo root:  Rscript tests/test_t78_deuda.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/monedas.R", "R/i18n.R", "R/zona.R", "R/pasivos_schemas.R", "R/forecasting_resolve.R", "R/policy_engine.R",
            "R/pasivos_engine.R", "R/pasivos_wizard_validate.R", "R/pasivos_wizard_module.R", "R/pasivos_deuda.R"))
  suppressMessages(source(f))
forecasting_get_estimate <- function(...) NA_real_

HOY    <- as.Date("2026-11-21")
INICIO <- as.Date("2026-10-05")          # first payment 2026-11-05 (already paid), last 2027-10-05
credito <- function(id = "L1", ...) {
  d <- modifyList(list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito T78", empresa = "NG",
                       parte = "Banco Uno", moneda_pago = "MXN", principal_original = 120000, tasa_tipo = "fija",
                       tasa_actual = 12, plazo_meses = 12L, frecuencia_pago = "mensual", dia_pago = 5L,
                       fecha_inicio = INICIO, metodo_amortizacion = "francesa", periodo_gracia = 0L), list(...))
  l <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"); l$id <- id; l$estado <- "active"; l
}
r   <- (1 + 0.12)^(1 / 12) - 1           # the engine's effective-to-monthly rate
pago <- 120000 * r / (1 - (1 + r)^-12)
int1 <- 120000 * r; cap1 <- pago - int1

test_that("A: a French loan, by hand: rates, balance, 12 months, next payment; XIRR = EAR", {
  f <- pasivos_deuda_pasivo(credito(), hoy = HOY)
  expect_identical(nrow(f), 1L)
  expect_identical(f$grupo, "financiamiento"); expect_identical(f$moneda, "MXN"); expect_identical(f$acreedor, "Banco Uno")
  expect_equal(f$ear, 12)
  expect_equal(f$nominal, 12 * ((1.12)^(1 / 12) - 1) * 100, tolerance = 1e-9)   # 11.3866 %
  expect_equal(round(f$nominal, 4), 11.3866)
  expect_equal(f$m, 12)
  expect_equal(f$saldo, 120000 - cap1, tolerance = 0.01)                        # one payment already made
  expect_equal(f$int12, 11 * pago - (120000 - cap1), tolerance = 0.05)          # the 11 left fall in 12 months
  expect_equal(f$cap12, f$saldo, tolerance = 0.01)
  expect_equal(f$prox_fecha, as.Date("2026-12-05"))
  expect_equal(f$prox_monto, pago, tolerance = 0.01)
  expect_equal(f$plazo_meses, round(as.numeric(as.Date("2027-10-05") - HOY) / 30.4375))
  # Dated flows: months are 28-31 days, so the XIRR is the EAR within a few
  # hundredths of a point, not to the cent.
  expect_equal(f$costo_efectivo, 12, tolerance = 0.1 / 12)
  d <- pasivos_deuda(credito(), hoy = HOY)
  expect_equal(d$resumen$pct12, 100, tolerance = 1e-6)
  expect_equal(d$resumen$ponderado, 12)
})

# An IRR with its own bisection on the same dated flows (not .deuda_xirr()).
tir_a_mano <- function(fechas, flujos) {
  t <- as.numeric(fechas - min(fechas)) / 365
  lo <- 0; hi <- 1
  for (i in 1:200) { mid <- (lo + hi) / 2; if (sum(flujos / (1 + mid)^t) > 0) lo <- mid else hi <- mid }
  mid
}

test_that("B: with an opening fee the XIRR goes above the EAR, and is the IRR worked out by hand", {
  l <- credito(cargos_iniciales = list(list(desc = "Apertura", monto = 2400, fecha = INICIO, moneda = "MXN")))
  f <- pasivos_deuda_pasivo(l, hoy = HOY)
  expect_equal(f$ear, 12)                                    # the rate does not change
  expect_gt(f$costo_efectivo, 12 + 3)                        # 2 % fee on a 1-year loan: about +3.9 points
  fechas <- c(INICIO, INICIO, seq(as.Date("2026-11-05"), by = "1 month", length.out = 12))
  flujos <- c(120000, -2400, rep(-pago, 12))
  expect_equal(f$costo_efectivo, 100 * tir_a_mano(fechas, flujos), tolerance = 1e-4)
  # taxes never enter: IVA on the interest leaves the XIRR where it was
  li <- credito(cargos_iniciales = list(list(desc = "Apertura", monto = 2400, fecha = INICIO, moneda = "MXN")),
                impuestos = list(list(nombre = "IVA", tipo = "porcentual", valor = 16, base = "intereses")))
  expect_equal(pasivos_deuda_pasivo(li, hoy = HOY)$costo_efectivo, f$costo_efectivo, tolerance = 1e-8)
})

tarjeta <- function(id = "T1") {
  d <- list(categoria = "tarjeta", nombre = "Amex", empresa = "NG", parte = "Amex", moneda_pago = "MXN",
            tarjeta_provision_source = "manual", tarjeta_closing_day = 10L, tarjeta_due_day = 30L, fecha_inicio = INICIO)
  l <- tryCatch(pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"),
                error = function(e) tibble::tibble(categoria = "tarjeta", nombre = "Amex", parte = "Amex", moneda_pago = "MXN"))
  l$id <- id; l$estado <- "active"; l
}
provs <- data.frame(liability_id = c("T1", "T1", "T1", "T1"), estado = c("provisional", "provisional", "confirmed", "provisional"),
                    amount_pago = c(5000, 3000, 9999, 4000), amount_pago_override = c(NA, 3500, NA, NA),
                    fecha_efectiva = as.Date(c("2026-11-30", "2026-12-30", "2026-10-30", "2028-01-30")),
                    fecha_efectiva_override = as.Date(c(NA, NA, NA, NA)), moneda_pago = "MXN", stringsAsFactors = FALSE)

test_that("C: a card: debt = what its pending provisions owe; no rate, out of the weighted cost", {
  f <- pasivos_deuda_pasivo(tarjeta(), provs, hoy = HOY)
  expect_identical(f$grupo, "tarjeta")
  expect_equal(f$saldo, 5000 + 3500 + 4000)                  # the override wins; the confirmed one is paid
  expect_true(is.na(f$ear) && is.na(f$nominal) && is.na(f$costo_efectivo))
  expect_equal(f$int12, 0)
  expect_equal(f$cap12, 8500)                                # the 2028 one is beyond 12 months
  expect_equal(f$prox_fecha, as.Date("2026-11-30")); expect_equal(f$prox_monto, 5000)
  d <- pasivos_deuda(dplyr::bind_rows(credito(), tarjeta()), provs, hoy = HOY)
  expect_equal(d$resumen$ponderado, 12)                      # only the loan weighs
  expect_equal(d$resumen$deuda_tar, 12500)
  expect_equal(d$resumen$deuda, d$resumen$deuda_fin + 12500)
})

regular <- function(id = "R1") {
  d <- list(categoria = "regular", nombre = "Renta", empresa = "NG", parte = "Inmobiliaria", moneda_pago = "MXN",
            recurrence_type = "monthly_day", recurrence_params = list(day = 15L), fecha_inicio = HOY, amount_default = 10000)
  l <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"); l$id <- id; l$estado <- "active"; l
}

test_that("D: a regular payment and a pure lease are not debt; the regular one is in the footer", {
  expect_true(is.na(pasivos_deuda_grupo(regular())))
  expect_true(is.na(pasivos_deuda_grupo(data.frame(categoria = "financiero", flavor = "arrendamiento_puro"))))
  expect_identical(pasivos_deuda_grupo(data.frame(categoria = "financiero", flavor = "arrendamiento")), "financiamiento")
  d <- pasivos_deuda(dplyr::bind_rows(credito(), regular()), hoy = HOY)
  expect_identical(d$pasivos$id, "L1")
  expect_equal(d$regulares$moneda, "MXN"); expect_equal(d$regulares$anual, 120000)
  # paused or deleted liabilities are not counted
  p <- credito(); p$estado <- "paused"
  expect_identical(nrow(pasivos_deuda(p, hoy = HOY)$pasivos), 0L)
})

test_that("E: two currencies are never added", {
  usd <- credito(id = "L2", moneda_pago = "USD", principal_original = 50000, tasa_actual = 8, parte = "Bank Two")
  d <- pasivos_deuda(dplyr::bind_rows(credito(), usd), hoy = HOY)
  expect_setequal(d$resumen$moneda, c("MXN", "USD"))
  expect_equal(d$resumen$ponderado[d$resumen$moneda == "USD"], 8)
  expect_equal(d$resumen$ponderado[d$resumen$moneda == "MXN"], 12)
  expect_lt(d$resumen$deuda[d$resumen$moneda == "USD"], 50000)
  expect_setequal(unique(d$servicio$datos$Moneda), c("MXN", "USD"))
  expect_identical(.deuda_monedas(c("USD", "MXN", "USD"), "MXN"), c("MXN", "USD"))   # the base first
})

test_that("F: the 12 months' service: capital and interest by month, per currency", {
  s <- pasivos_deuda_servicio(dplyr::bind_rows(credito(), tarjeta()), provs, hoy = HOY)
  expect_equal(s$meses[1], as.Date("2026-11-01")); expect_length(s$meses, 12L)
  cap <- s$datos[s$datos$id == "MXN|capital", ]; int <- s$datos[s$datos$id == "MXN|interes", ]
  expect_equal(cap$b1, 5000)                                 # Nov: the card's 30th; the loan's 5th was paid
  expect_equal(cap$b2, 3500 + (pago - (120000 - cap1) * r), tolerance = 0.01)   # Dec: card + loan's 2nd capital
  expect_equal(int$b2, (120000 - cap1) * r, tolerance = 0.01)
  expect_equal(int$b1, 0)
  # the loan ends 2027-10-05 (b12); the card's 2028 provision is out
  expect_equal(cap$Total, (120000 - cap1) + 8500, tolerance = 0.01)
  expect_equal(cap$Total, sum(unlist(cap[paste0("b", 1:12)])), tolerance = 1e-6)
})
