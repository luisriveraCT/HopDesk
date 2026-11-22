# =============================================================================
# tests/test_t63_prime.R
# T63: "Variable PRIME" (US Prime Rate, FRED series DPRIME), the same way as
# SOFR: the wizard offers it, the validator accepts it, the rate is fetched
# from FRED with series_id=DPRIME, and the engine asks Forecasting for metric
# "prime" -- falling back to tasa_actual + spread when Forecasting has none.
# FRED is never called: httr::GET is mocked inside this process only.
# Run from the repo root:  Rscript tests/test_t63_prime.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
source("R/persistence.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_observers.R",
            "R/pasivos_wizard_validate.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/monedas.R", "R/zona.R", "R/hd_columnas.R", "R/hd_table.R", "R/pasivos_wizard_module.R",
            "R/pasivos_wizard_steps.R", "R/pasivos_rates.R"))
  suppressMessages(source(f))
source("tests/helpers/hd_numero.R")   # T66: hd_numero_input()

credito <- list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito", empresa = "NTS",
                parte = "Banco", moneda_pago = "USD", principal_original = 120000, saldo_capital = NA_real_,
                tasa_tipo = "variable_prime", tasa_actual = 7.5, tasa_spread = 200, fecha_inicio = Sys.Date(),
                plazo_meses = 12L, frecuencia_pago = "mensual", dia_pago = 15L, periodo_gracia = 0L,
                metodo_amortizacion = "francesa")

test_that("FRED: .fetch_prime() pide la serie DPRIME y devuelve las dos últimas observaciones", {
  pedida <- NULL
  json <- '{"observations":[{"date":"2026-11-18","value":"7.50"},{"date":"2026-11-17","value":"7.50"},{"date":"2026-11-14","value":"."}]}'
  local_mocked_bindings(GET = function(url, ...) { pedida <<- url
    structure(list(status_code = 200L, content = charToRaw(json), headers = list(`content-type` = "application/json")),
              class = "response") }, .package = "httr")
  r <- .fetch_prime(api_key = "k")
  expect_true(grepl("series_id=DPRIME", pedida, fixed = TRUE))
  expect_identical(r$today, 7.5); expect_identical(r$date_today, as.Date("2026-11-18"))
  expect_null(r$error)
  r2 <- .fetch_sofr(api_key = "k")                     # SOFR still asks for its own series
  expect_true(grepl("series_id=SOFR&", pedida, fixed = TRUE))
  expect_identical(.fetch_prime(api_key = "")$error, "FRED_API_KEY no configurado")
})

test_that("el asistente ofrece 'Variable PRIME' y el validador la acepta", {
  ui <- paste(as.character(pasivos_step_ui_terminos(list(categoria = "financiero", tasa_tipo = "variable_prime"), list())),
              collapse = "")
  expect_true(grepl('value="variable_prime"', ui, fixed = TRUE)); expect_true(grepl("Variable PRIME", ui, fixed = TRUE))
  v <- pasivos_wizard_validate_terminos(c(credito, list(principal_original = 120000)))
  expect_null(v$errors$tasa_tipo)
})

test_that("el motor pide la métrica 'prime' a Forecasting, y sin ella usa tasa_actual + margen", {
  pedidas <- character()
  assign("forecasting_get_estimate", function(metric, fecha, ...) { pedidas <<- c(pedidas, metric); 8 }, envir = globalenv())
  s_fc <- pasivos_generate_schedule(credito)
  expect_true(all(pedidas == "prime") && length(pedidas) > 0)
  assign("forecasting_get_estimate", function(metric, fecha, ...) NA_real_, envir = globalenv())
  s_na <- suppressWarnings(pasivos_generate_schedule(credito))
  # Without the index: tasa_actual + 200 bps = 9.5 % nominal annual, Act/360
  # with the period's real days (tasas variables, 2026-11-21: it used to be
  # taken as an effective rate, the same as a fixed one).
  dias1 <- as.numeric(s_na$fecha[1] - as.Date(credito$fecha_inicio))
  expect_equal(s_na$interes[1], 120000 * 0.095 * dias1 / 360, tolerance = 1e-6)
  dias2 <- as.numeric(s_na$fecha[2] - s_na$fecha[1])
  expect_equal(s_na$interes[2], s_na$saldo_post[1] * 0.095 * dias2 / 360, tolerance = 1e-6)
  expect_true(s_fc$interes[1] > s_na$interes[1])            # 8 % + 200 bps > 7.5 % + 200 bps
})
