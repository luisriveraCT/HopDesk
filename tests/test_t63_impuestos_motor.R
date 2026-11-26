# =============================================================================
# tests/test_t63_impuestos_motor.R
# T63: taxes on each payment of a liability (pasivos_impuestos_pago(), the one
# rule): a percentage of the whole payment (default) or of its interest only,
# or a fixed amount per payment, in the payment's currency.
#   - a liability WITHOUT taxes gives exactly the schedule and provisions it
#     gave before T63 (the engine of coord/tanda48, 399813c, read from git);
#   - with taxes: componente_iva carries the tax and amount_pago includes it
#     (so the Agenda row, built from amount_pago, includes it too); the
#     schedule gains impuestos and total_pago; initial fees carry none;
#   - a record saved before T63 (no impuestos column) has no taxes.
# Run from the repo root:  Rscript tests/test_t63_impuestos_motor.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
source("R/persistence.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_observers.R",
            "R/pasivos_wizard_validate.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R", "R/pasivos_wizard_module.R"))
  suppressMessages(source(f))
forecasting_get_estimate <- function(...) NA_real_

HOY <- as.Date("2026-11-19")
credito <- list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito", empresa = "NTS",
                parte = "Banco", moneda_pago = "MXN", principal_original = 608763.35, saldo_capital = NA_real_,
                tasa_tipo = "fija", tasa_actual = 12, fecha_inicio = HOY + 10, plazo_meses = 12L,
                frecuencia_pago = "mensual", dia_pago = 5L, periodo_gracia = 0L, metodo_amortizacion = "alemana",
                cargos_iniciales = list(list(desc = "Apertura", monto = 1000, moneda = "MXN", fecha = HOY + 10)))
renta <- list(categoria = "regular", nombre = "Renta", empresa = "NTS", parte = "Inmobiliaria",
              moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
              fecha_inicio = HOY, amount_default = 100)
liab <- function(d) pasivos_wizard_draft_to_liability(d, mode = "create", user = "t")
sin_ruido <- function(df) as.data.frame(df[, setdiff(names(df), c("id", "liability_id", "generated_at"))])

# The engine before T63, from git, in its own environment.
antes <- new.env(parent = globalenv())
src <- system2("git", c("show", "399813c:R/pasivos_engine.R"), stdout = TRUE)
for (e in parse(text = paste(src, collapse = "\n"), keep.source = FALSE)) eval(e, antes)

test_that("sin impuestos: calendario y provisiones idénticos a los de antes de la T63", {
  for (d in list(credito, renta)) {
    l <- liab(d)
    if (identical(d$categoria, "financiero")) {
      # T115 added two columns (dias, tasa_periodo) and nothing else: the
      # columns that already existed must be identical.
      s1 <- pasivos_generate_schedule(l, today = HOY); s0 <- antes$pasivos_generate_schedule(l, today = HOY)
      expect_identical(setdiff(names(s1), names(s0)), c("dias", "tasa_periodo"))
      expect_identical(s1[, names(s0)], s0)
    }
    p1 <- pasivos_generate_provisions(l, window_start = HOY); p0 <- antes$pasivos_generate_provisions(l, window_start = HOY)
    expect_identical(sin_ruido(p1), sin_ruido(p0))
    expect_true(all(is.na(p1$componente_iva)))
  }
})

test_that("un registro sin la columna impuestos (guardado antes) no tiene impuestos", {
  l <- liab(renta); l$impuestos <- NULL
  expect_identical(pasivos_impuestos_de(l), list())
  expect_true(all(is.na(pasivos_generate_provisions(l, window_start = HOY)$componente_iva)))
})

test_that("la regla: porcentaje del pago, solo de intereses, o fijo por pago; redondeo a centavos", {
  iva  <- list(list(nombre = "IVA", tipo = "porcentual", valor = 16, base = "pago"))
  ivai <- list(list(nombre = "IVA", tipo = "porcentual", valor = 16, base = "intereses"))
  fijo <- list(list(nombre = "Derecho", tipo = "fijo", valor = 50))
  expect_equal(pasivos_impuestos_pago(iva, c(100, 250.5)), c(16, 40.08))
  expect_equal(pasivos_impuestos_pago(ivai, 1000, 33.33), 5.33)
  expect_equal(pasivos_impuestos_pago(fijo, c(10, 20)), c(50, 50))
  expect_equal(pasivos_impuestos_pago(c(iva, fijo), 100), 66)
  expect_equal(pasivos_impuestos_pago(list(), 100), 0)
  expect_equal(pasivos_impuestos_pago(list(list(nombre = "x", tipo = "raro", valor = 5)), 100), 0)   # malformed: ignored
})

test_that("renta con IVA 16 %: cada provisión lleva el IVA y amount_pago lo incluye", {
  l <- liab(renta); l$impuestos <- list(list(list(nombre = "IVA", tipo = "porcentual", valor = 16, base = "pago")))
  p <- pasivos_generate_provisions(l, window_start = HOY)
  expect_true(nrow(p) > 0)
  expect_equal(unique(p$componente_iva), 16); expect_equal(unique(p$amount_pago), 116)
})

test_that("crédito con IVA sobre intereses: por pago, en el calendario y en las provisiones; la comisión inicial sin impuesto", {
  l <- liab(credito); l$impuestos <- list(list(list(nombre = "IVA", tipo = "porcentual", valor = 16, base = "intereses")))
  s <- pasivos_generate_schedule(l, today = HOY)
  reg <- s$origin != "initial_fee"
  expect_equal(s$impuestos[reg], round(s$interes[reg] * 0.16, 2))
  expect_equal(s$impuestos[!reg], rep(0, sum(!reg)))
  expect_equal(s$total_pago, s$total + s$impuestos)
  p <- pasivos_generate_provisions(l, window_start = HOY)
  fee <- p$origin %in% "initial_fee"
  expect_true(all(is.na(p$componente_iva[fee])))
  expect_equal(p$componente_iva[!fee], round(p$componente_interes[!fee] * 0.16, 2))
  sin <- pasivos_generate_provisions(liab(credito), window_start = HOY)
  expect_equal(p$amount_pago[!fee], sin$amount_pago[!sin$origin %in% "initial_fee"] + p$componente_iva[!fee])
})
