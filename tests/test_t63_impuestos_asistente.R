# =============================================================================
# tests/test_t63_impuestos_asistente.R
# T63: taxes in the Pasivos wizard (Nuevo and Editar), "Cargos" step.
#   - the step shows the taxes section; a row has a free name with the "IVA"
#     placeholder, percentage by default, and "solo intereses" only for loans;
#   - the typed rows are read (.wiz_leer_impuestos), validated (name, value
#     >= 0, a percentage up to 100), saved in the liability (impuestos) and
#     read back on Editar;
#   - the Revisión previews show each payment's tax and the total to pay.
# Run from the repo root:  Rscript tests/test_t63_impuestos_asistente.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
source("R/persistence.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_observers.R",
            "R/pasivos_wizard_validate.R", "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R", "R/pasivos_wizard_module.R",
            "R/pasivos_wizard_steps.R"))
  suppressMessages(source(f))
forecasting_get_estimate <- function(...) NA_real_
html <- function(x) paste(as.character(x), collapse = "")

credito <- list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito", empresa = "NTS",
                parte = "Banco", moneda_pago = "MXN", principal_original = 608763.35, saldo_capital = NA_real_,
                tasa_tipo = "fija", tasa_actual = 12, fecha_inicio = Sys.Date() + 10, plazo_meses = 12L,
                frecuencia_pago = "mensual", dia_pago = 5L, periodo_gracia = 0L, metodo_amortizacion = "alemana")
renta <- list(categoria = "regular", nombre = "Renta", empresa = "NTS", parte = "Inmobiliaria",
              moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
              fecha_inicio = Sys.Date(), amount_default = 100)
IVA16 <- list(nombre = "IVA", tipo = "porcentual", valor = 16, base = "intereses")
FIJO  <- list(nombre = "Derecho", tipo = "fijo", valor = 50, base = "pago")

test_that("el paso Cargos trae la sección de impuestos; una fila: nombre libre con 'IVA' en gris, porcentual por omisión", {
  ui <- html(pasivos_step_ui_cargos(list(categoria = "financiero"), list(), list()))
  expect_true(grepl("pwiz_impuestos_table", ui, fixed = TRUE) && grepl("pwiz_add_impuesto", ui, fixed = TRUE))
  fila <- html(pasivos_impuesto_row_ui(1L, list(), financiero = TRUE))
  expect_true(grepl('placeholder="IVA"', fila, fixed = TRUE))
  expect_true(grepl('<option value="porcentual" selected>', fila, fixed = TRUE))
  expect_true(grepl("pwiz_imp_base_1", fila, fixed = TRUE))
  expect_false(grepl("pwiz_imp_base_1", html(pasivos_impuesto_row_ui(1L, list(), financiero = FALSE)), fixed = TRUE))
})

test_that("lo tecleado se lee; 'solo intereses' solo en financieros; una fila vacía se descarta", {
  input <- list(pwiz_imp_nombre_1 = " IVA ", pwiz_imp_tipo_1 = "porcentual", pwiz_imp_valor_1 = 16, pwiz_imp_base_1 = "intereses",
                pwiz_imp_nombre_2 = "", pwiz_imp_tipo_2 = "porcentual", pwiz_imp_valor_2 = NA)
  fin <- .wiz_leer_impuestos(input, 2L, "financiero")
  expect_identical(length(fin), 1L); expect_identical(fin[[1]]$nombre, "IVA"); expect_identical(fin[[1]]$base, "intereses")
  expect_identical(.wiz_leer_impuestos(input, 2L, "regular")[[1]]$base, "pago")
  expect_identical(length(.wiz_leer_impuestos(input, 2L, "financiero", todas = TRUE)), 2L)
})

test_that("validación: nombre obligatorio; valor 0 o mayor, porcentaje hasta 100", {
  ok <- pasivos_wizard_validate_cargos(list(impuestos = list(IVA16, FIJO)))
  expect_true(ok$ok)
  e <- pasivos_wizard_validate_cargos(list(impuestos = list(list(nombre = "", tipo = "porcentual", valor = 16),
                                                            list(nombre = "X", tipo = "porcentual", valor = 160),
                                                            list(nombre = "Y", tipo = "fijo", valor = -1))))$errors
  expect_setequal(names(e), c("impuesto_nombre_1", "impuesto_valor_2", "impuesto_valor_3"))
})

test_that("se guardan en el pasivo y vuelven al editar (Nuevo -> Editar)", {
  l <- pasivos_wizard_draft_to_liability(c(credito, list(impuestos = list(IVA16, FIJO))), mode = "create", user = "t")
  expect_identical(length(l$impuestos[[1]]), 2L)
  d <- pasivos_liability_to_draft(l)
  expect_identical(d$impuestos[[1]]$nombre, "IVA"); expect_identical(d$impuestos[[2]]$tipo, "fijo")
  sin <- pasivos_wizard_draft_to_liability(credito, mode = "create", user = "t")
  expect_null(sin$impuestos[[1]])
  expect_identical(pasivos_impuestos_de(sin), list())
})

test_that("Revisión: cada pago con su impuesto y el total a pagar; sin impuestos, 0 y el mismo total", {
  a <- pasivos_vista_amortizacion(c(credito, list(impuestos = list(IVA16))))
  expect_equal(a$impuestos, round(a$interes * 0.16, 2)); expect_equal(a$total_pago, a$total + a$impuestos)
  a0 <- pasivos_vista_amortizacion(credito)
  expect_true(all(a0$impuestos == 0)); expect_equal(a0$total_pago, a0$total)
  p <- pasivos_vista_provisiones(c(renta, list(impuestos = list(FIJO))))
  expect_equal(unique(p$impuestos), 50); expect_equal(unique(p$monto), 150)
})
