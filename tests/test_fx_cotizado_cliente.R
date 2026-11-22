# =============================================================================
# tests/test_fx_cotizado_cliente.R
# A payment quoted in another currency (cotizado_en) gets its amount from the
# client's fx_* estimate: pasivos_recompute_pago_from_cotizado() read it with
# no client_id (pasivos_engine.R), the same bug the variable rates had; on the
# shared deployment that read has no tenant and the amount stayed NA.
#   - two clients with different USD->MXN: each provision with its own rate;
#   - no client: NA, never a guessed rate (as before);
#   - the session's client is the default.
# Estimates come from an in-memory store local to this test (never S3).
# Contraprueba: on ba1ae3c (before) this test fails.
# Run from the repo root:  Rscript tests/test_fx_cotizado_cliente.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/monedas.R", "R/i18n.R", "R/zona.R", "R/pasivos_schemas.R", "R/forecasting_resolve.R", "R/policy_engine.R",
            "R/pasivos_engine.R", "R/pasivos_wizard_validate.R", "R/pasivos_wizard_module.R"))
  suppressMessages(source(f))

fx <- function(v) tibble::tibble(metric = "fx_usd_mxn", fecha = as.Date("2026-01-01"), value = v,
                                 source_method = "manual", is_frozen = FALSE, updated_by = "t", updated_at = Sys.time())
ALMACEN <- list(a = fx(18), b = fx(20))
LEIDOS <- character()
load_pasivos_estimates <- function(client_id = NULL) {
  LEIDOS <<- c(LEIDOS, client_id %||% "<sin cliente>")
  if (is.null(client_id)) stop("[.s3_key] refusing to guess a tenant")
  ALMACEN[[client_id]] %||% ALMACEN$a[0, ]
}

HOY <- as.Date("2026-11-22")
d <- list(categoria = "regular", nombre = "Licencias", empresa = "NG", parte = "X", moneda_pago = "MXN", cotizado_en = "USD",
          recurrence_type = "monthly_day", recurrence_params = list(day = 15L), fecha_inicio = HOY, amount_default = 100)
lic <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"); lic$id <- "Q1"
lic$amount_default_cot <- 100   # a quoted payment's amount is in amount_default_cot (USD)

test_that("each client's exchange rate; with no client, NA (never a guessed rate)", {
  expect_equal(pasivos_recompute_pago_from_cotizado(100, "USD", "MXN", HOY, client_id = "a")$amount_pago, 1800)
  expect_equal(pasivos_recompute_pago_from_cotizado(100, "USD", "MXN", HOY, client_id = "b")$amount_pago, 2000)
  expect_true(is.na(suppressWarnings(pasivos_recompute_pago_from_cotizado(100, "USD", "MXN", HOY, client_id = NULL))$amount_pago))
})

test_that("the provisions take the client's rate", {
  pa <- suppressWarnings(pasivos_generate_provisions(lic, window_start = HOY, client_id = "a"))
  pb <- suppressWarnings(pasivos_generate_provisions(lic, window_start = HOY, client_id = "b"))
  expect_true(nrow(pa) > 0)
  expect_true(all(pa$amount_pago == 1800)); expect_true(all(pb$amount_pago == 2000))
  p0 <- suppressWarnings(pasivos_generate_provisions(lic, window_start = HOY, client_id = NULL))
  expect_true(all(is.na(p0$amount_pago)))
})

test_that("the session's client is the default", {
  ses <- shiny::MockShinySession$new()
  ses$userData$hd_cliente <- function() "b"
  r <- shiny::withReactiveDomain(ses, pasivos_recompute_pago_from_cotizado(100, "USD", "MXN", HOY))
  expect_equal(r$amount_pago, 2000)
})
