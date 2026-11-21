# =============================================================================
# tests/test_t77_costeo.R
# T77: what a liability costs (pasivos_costeo(), R/pasivos_engine.R), from
# the engine, by currency, never adding currencies.
#   A  a French loan whose total interest can be worked out by hand:
#      120,000 MXN at 12 % a year (effective), 12 monthly payments; the cost
#      is the interest (never the capital), all of it in the next 12 months;
#   B  the same with an opening fee and IVA 16 % on the interest;
#   C  a regular payment with no end: 10,000 a month -> 120,000 a year,
#      10,000 a month, no total; with an end date on 2027-04-10, 4 payments;
#   D  a fee in another currency stays in its own row.
# Run from the repo root:  Rscript tests/test_t77_costeo.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/monedas.R", "R/i18n.R", "R/zona.R", "R/pasivos_schemas.R", "R/forecasting_resolve.R", "R/policy_engine.R",
            "R/pasivos_engine.R", "R/pasivos_wizard_validate.R", "R/pasivos_wizard_module.R"))
  suppressMessages(source(f))
forecasting_get_estimate <- function(...) NA_real_

HOY <- as.Date("2026-11-21")
credito <- function(...) {
  d <- modifyList(list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito T77", empresa = "NG",
                       parte = "Banco", moneda_pago = "MXN", principal_original = 120000, tasa_tipo = "fija",
                       tasa_actual = 12, plazo_meses = 12L, frecuencia_pago = "mensual", dia_pago = 5L,
                       fecha_inicio = HOY, metodo_amortizacion = "francesa", periodo_gracia = 0L), list(...))
  l <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"); l$id <- "L1"; l
}

test_that("A: a 12-month French loan: the cost is its interest, worked out by hand", {
  r <- (1 + 0.12)^(1 / 12) - 1                       # the engine's effective-to-monthly rate
  pago <- 120000 * r / (1 - (1 + r)^-12)
  a_mano <- 12 * pago - 120000
  c <- pasivos_costeo(credito(), hoy = HOY)
  expect_identical(c$categoria, "financiero")
  expect_identical(c$filas$moneda, "MXN")
  expect_equal(c$filas$intereses, a_mano, tolerance = 0.05)
  expect_equal(c$filas$total, c$filas$intereses)    # no fees, no taxes: the cost is the interest
  expect_equal(c$filas$cargos, 0); expect_equal(c$filas$impuestos, 0)
  expect_equal(c$filas$anual, c$filas$total, tolerance = 0.01)   # all 12 payments fall in the next 12 months
  expect_true(c$filas$total < 120000 * 0.12)        # never the capital
})

test_that("B: an opening fee and IVA 16 % on the interest add to the cost, with their breakdown", {
  l <- credito(cargos_iniciales = list(list(desc = "Apertura", monto = 1500, fecha = HOY + 1, moneda = "MXN")),
               impuestos = list(list(nombre = "IVA", tipo = "porcentual", valor = 16, base = "intereses")))
  c <- pasivos_costeo(l, hoy = HOY)
  base <- pasivos_costeo(credito(), hoy = HOY)$filas
  expect_equal(c$filas$cargos, 1500)
  expect_equal(c$filas$impuestos, round(base$intereses * 0.16, 2), tolerance = 0.12)   # rounded per payment
  expect_equal(c$filas$total, c$filas$intereses + c$filas$cargos + c$filas$impuestos)
})

test_that("C: a regular payment: by month and by year; a total only with an end date", {
  d <- list(categoria = "regular", nombre = "Renta", empresa = "NG", parte = "Inmobiliaria", moneda_pago = "MXN",
            recurrence_type = "monthly_day", recurrence_params = list(day = 15L), fecha_inicio = HOY, amount_default = 10000)
  l <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"); l$id <- "R1"
  c <- pasivos_costeo(l, hoy = HOY)
  expect_identical(c$categoria, "regular")
  expect_equal(c$filas$anual, 120000)
  expect_equal(c$filas$mes, 10000)
  expect_true(is.na(c$filas$total))                 # no end: no total
  l$fecha_vence <- HOY + 140                         # T70: ends 2027-04-10
  c2 <- pasivos_costeo(l, hoy = HOY)
  expect_equal(c2$filas$total, 40000)                # the 15th of Dec, Jan, Feb, Mar
  expect_equal(c2$filas$anual, 40000)                # nothing after the end
})

test_that("C2: a payment with no amount yet (no exchange rate) makes the cost unknown, never 0", {
  d <- list(categoria = "regular", nombre = "Licencias", empresa = "NG", parte = "X", moneda_pago = "MXN", cotizado_en = "USD",
            recurrence_type = "monthly_day", recurrence_params = list(day = 15L), fecha_inicio = HOY, amount_default = 250)
  l <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"); l$id <- "Q1"
  c <- pasivos_costeo(l, hoy = HOY)
  if (any(is.na(pasivos_generate_provisions(l, window_start = HOY)$amount_pago))) expect_true(all(is.na(c$filas$anual)))
  else expect_true(all(c$filas$anual > 0))
})

test_that("D: a fee in another currency keeps its own row; currencies are never added", {
  l <- credito(cargos_iniciales = list(list(desc = "Avalúo", monto = 200, fecha = HOY + 1, moneda = "USD")))
  c <- pasivos_costeo(l, hoy = HOY)
  expect_identical(sort(c$filas$moneda), c("MXN", "USD"))
  expect_equal(c$filas$cargos[c$filas$moneda == "USD"], 200)
  expect_equal(c$filas$intereses[c$filas$moneda == "USD"], 0)
  expect_equal(c$filas$cargos[c$filas$moneda == "MXN"], 0)
})

test_that("the Revisión's lines: financiero cost and breakdown; regular per month, per year and up to the end", {
  d <- list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito T77", empresa = "NG",
            parte = "Banco", moneda_pago = "MXN", principal_original = 120000, tasa_tipo = "fija",
            tasa_actual = 12, plazo_meses = 12L, frecuencia_pago = "mensual", dia_pago = 5L,
            fecha_inicio = HOY, metodo_amortizacion = "francesa", periodo_gracia = 0L)
  f <- .wiz_costeo_filas(d)
  etiquetas <- vapply(f, `[`, character(1), 1L)
  expect_identical(etiquetas, c(tr("tablas.pasivos.costeo.total_financiamiento"), tr("tablas.pasivos.costeo.desglose_titulo"),
                                tr("tablas.pasivos.costeo.por_ano")))
  expect_true(grepl("MXN", f[[1]][2], fixed = TRUE) && !grepl("$", f[[1]][2], fixed = TRUE))
  r <- .wiz_costeo_filas(list(categoria = "regular", nombre = "Renta", empresa = "NG", parte = "X", moneda_pago = "MXN",
                              recurrence_type = "monthly_day", recurrence_params = list(day = 15L), fecha_inicio = HOY,
                              amount_default = 10000, fecha_fin = HOY + 140))
  expect_identical(vapply(r, `[`, character(1), 1L), c(tr("tablas.pasivos.costeo.por_mes"), tr("tablas.pasivos.costeo.por_ano"),
                                                       tr("tablas.pasivos.costeo.hasta_fin")))
  expect_identical(.wiz_costeo_filas(list(categoria = "")), list())    # nothing to cost yet: no lines
})

