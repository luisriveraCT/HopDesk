# =============================================================================
# tests/test_t63_caja_errores.R
# T63: Luis typed 608763.35 as the principal in "Nuevo pasivo › Términos" and
# the red box still said "El principal debe ser mayor que cero" and "La tasa
# no puede ser negativa": the box of an earlier Siguiente (with both empty),
# never cleared while he typed; and an empty rate read as "negative".
#   - each line of the box names its fields' input ids (data-campos) and a
#     script hides the line when one of them changes (no server redraw, which
#     would wipe what is being typed);
#   - an empty fixed rate says "Captura la tasa anual"; an unfetched variable
#     one says so; only a negative one says "no puede ser negativa".
# Run from the repo root:  Rscript tests/test_t63_caja_errores.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/i18n.R", "R/monedas.R", "R/pasivos_schemas.R", "R/pasivos_wizard_validate.R", "R/pasivos_wizard_steps.R")) suppressMessages(source(f))
html <- function(x) paste(as.character(x), collapse = "")
base <- list(categoria = "financiero", flavor = "credito_simple", principal_original = 608763.35, tasa_tipo = "fija",
             plazo_meses = 45L, frecuencia_pago = "mensual", dia_pago = 5L)

test_that("la tasa vacía dice que falta; solo una negativa dice 'negativa'", {
  e <- pasivos_wizard_validate_terminos(c(base, list(tasa_actual = NA_real_)))$errors
  expect_identical(e$tasa_actual, tr("tablas.pasivos.asistente.tasa_falta"))
  expect_null(e$principal_original)                       # 608763.35 is fine
  e <- pasivos_wizard_validate_terminos(modifyList(base, list(tasa_tipo = "variable_prime", tasa_actual = NA_real_)))$errors
  expect_identical(e$tasa_actual, tr("tablas.pasivos.asistente.tasa_variable_falta"))
  e <- pasivos_wizard_validate_terminos(c(base, list(tasa_actual = -1)))$errors
  expect_identical(e$tasa_actual, tr("tablas.pasivos.asistente.tasa_negativa"))
  expect_null(pasivos_wizard_validate_terminos(c(base, list(tasa_actual = 0)))$errors$tasa_actual)   # 0 % is valid
})

test_that("cada línea de la caja nombra sus campos; el script la oculta al cambiarlos", {
  errs <- list(principal_original = "P", tasa_actual = "T", impuesto_valor_2 = "I")
  h <- html(.wiz_error_box(errs))
  expect_true(grepl('data-campos="pwiz_principal_original"', h, fixed = TRUE))
  expect_true(grepl('data-campos="pwiz_tasa_anual pwiz_tasa_periodo pwiz_tasa_tipo pwiz_tasa_spread pwiz_tasa_fetched"', h, fixed = TRUE))
  expect_true(grepl('data-campos="pwiz_imp_valor_2"', h, fixed = TRUE))   # the tax row's real input id
  expect_true(grepl("on('input.pwizerr change.pwizerr'", h, fixed = TRUE) && grepl(".hide()", h, fixed = TRUE))
  expect_true(grepl("pwiz-errores", h, fixed = TRUE))
  expect_null(.wiz_error_box(list()))
})

test_that("el paso Términos dibuja la caja con sus campos (el principal y la tasa)", {
  h <- html(pasivos_step_ui_terminos(base, list(principal_original = "P", tasa_actual = "T")))
  expect_true(grepl('data-campos="pwiz_principal_original"', h, fixed = TRUE))
  expect_true(grepl('id="pwiz_principal_original"', h, fixed = TRUE))   # the id a change reports
})
