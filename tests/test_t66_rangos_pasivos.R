# =============================================================================
# tests/test_t66_rangos_pasivos.R
# T66: with hd_numero_input() the browser no longer suggests min = 0, so the
# server checks it. The Pasivos wizard's amounts that no validator checked:
# the credit limit, the quoted amount, the principal balance, the spread, each
# fee and the residual value. Empty (NA) is still fine: they are optional.
# Each error names its field (pwiz_<clave>) for the T63 error box.
# Run from the repo root:  Rscript tests/test_t66_rangos_pasivos.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/i18n.R", "R/monedas.R", "R/pasivos_schemas.R", "R/pasivos_wizard_validate.R", "R/pasivos_wizard_steps.R"))
  suppressMessages(source(f))
source("tests/helpers/hd_numero.R")

terminos <- list(categoria = "financiero", flavor = "credito_simple", principal_original = 1000, tasa_tipo = "fija",
                 tasa_actual = 10, plazo_meses = 12L, frecuencia_pago = "mensual", dia_pago = 5L, fecha_inicio = Sys.Date())

test_that("saldo de capital: negativo no, vacío sí", {
  e <- pasivos_wizard_validate_terminos(c(terminos, list(saldo_capital = -1)))$errors
  expect_identical(e$saldo_capital, tr("tablas.pasivos.asistente.saldo_negativo"))
  expect_null(pasivos_wizard_validate_terminos(c(terminos, list(saldo_capital = NA_real_)))$errors$saldo_capital)
  expect_true(pasivos_wizard_validate_terminos(c(terminos, list(saldo_capital = 500)))$ok)
})

test_that("la sobretasa puede ser negativa (TIIE - 25 bps); la tasa total no", {
  tiie <- modifyList(terminos, list(tasa_tipo = "variable_tiie28", tasa_actual = 8.25))
  expect_true(pasivos_wizard_validate_terminos(c(tiie, list(tasa_spread = -25)))$ok)
  expect_true(pasivos_wizard_validate_terminos(c(tiie, list(tasa_spread = -825)))$ok)        # exactly 0 %
  e <- pasivos_wizard_validate_terminos(c(tiie, list(tasa_spread = -900)))$errors
  expect_identical(e$tasa_spread, tr("tablas.pasivos.asistente.tasa_total_negativa"))
  expect_true(pasivos_wizard_validate_terminos(c(tiie, list(tasa_spread = 150)))$ok)
})

test_that("un cargo de -500 y un residual negativo no pasan", {
  d <- list(cargos_iniciales = list(list(concepto = "Apertura", monto = -500, fecha = Sys.Date())),
            valor_residual = list(monto = -1, behavior = "last_payment"))
  e <- pasivos_wizard_validate_cargos(d)$errors
  expect_identical(e$cargo_monto_1, tr("tablas.pasivos.asistente.cargo_negativo", n = "1"))
  expect_identical(e$residual_monto, tr("tablas.pasivos.asistente.residual_negativo"))
  d$cargos_iniciales[[1]]$monto <- 500; d$valor_residual$monto <- NA_real_
  expect_true(pasivos_wizard_validate_cargos(d)$ok)
})

test_that("cada error apunta a su campo en la caja (data-campos)", {
  for (clave in c("saldo_capital", "tasa_spread", "cargo_monto_1", "residual_monto", "tarjeta_credit_limit", "amount_cotizado"))
    expect_identical(.wiz_error_campos(clave), paste0("pwiz_", clave))
})

test_that("las claves nuevas existen en los dos idiomas", {
  for (idioma in c("es", "en")) {
    d <- jsonlite::fromJSON(file.path("www/i18n", paste0(idioma, ".json")))
    for (k in c("limite_negativo", "cotizado_negativo", "saldo_negativo", "tasa_total_negativa", "cargo_negativo", "residual_negativo"))
      expect_true(nzchar(d[[paste0("tablas.pasivos.asistente.", k)]] %||% ""), info = paste(idioma, k))
  }
})
