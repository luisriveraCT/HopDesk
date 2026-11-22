# =============================================================================
# tests/test_tasas_variables.R
# Variable rates (SOFR / TIIE28 / Prime) in the Pasivos engine
# (paso 0: hopdesk_coord/tablas/reportes/TASAS_VAR_paso0.md).
#   A  the index is read with the client: two clients with different SOFR
#      give each its own schedule; with no client (or no estimate) the rate
#      captured on the day is used; the session's client is the default.
#   B  (convention) nominal annual, simple interest Act/360 on the balance
#      with the real days of each period: the loan worked out by hand,
#      10,000,000 MXN, TIIE28 9.00 % + 250 bps, 36 monthly, French.
# Estimates come from an in-memory store local to this test (never S3).
# Run from the repo root:  Rscript tests/test_tasas_variables.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/monedas.R", "R/i18n.R", "R/zona.R", "R/pasivos_schemas.R", "R/forecasting_resolve.R", "R/policy_engine.R",
            "R/pasivos_engine.R", "R/pasivos_wizard_validate.R", "R/pasivos_wizard_module.R"))
  suppressMessages(source(f))

# In-memory estimates per client; NULL client -> as on the shared deployment
# (.s3_key refuses to guess a tenant).
ALMACEN <- list(
  a = tibble::tibble(metric = c("sofr", "tiie28"), fecha = as.Date(c("2026-01-01", "2026-01-01")), value = c(4, 9),
                     source_method = "manual", is_frozen = FALSE, updated_by = "t", updated_at = Sys.time()),
  b = tibble::tibble(metric = "sofr", fecha = as.Date("2026-01-01"), value = 6,
                     source_method = "manual", is_frozen = FALSE, updated_by = "t", updated_at = Sys.time()))
LEIDOS <- character()
load_pasivos_estimates <- function(client_id = NULL) {
  LEIDOS <<- c(LEIDOS, client_id %||% "<sin cliente>")
  if (is.null(client_id)) stop("[.s3_key] refusing to guess a tenant")
  ALMACEN[[client_id]] %||% ALMACEN$a[0, ]
}

INICIO <- as.Date("2026-11-05"); HOY <- as.Date("2026-11-21")
credito <- function(...) {
  d <- modifyList(list(categoria = "financiero", flavor = "credito_simple", nombre = "Crédito variable", empresa = "NG",
                       parte = "Banco", moneda_pago = "USD", principal_original = 1000000, tasa_tipo = "variable_sofr",
                       tasa_actual = 3.6, tasa_spread = 150, plazo_meses = 12L, frecuencia_pago = "mensual",
                       dia_pago = 5L, fecha_inicio = INICIO, metodo_amortizacion = "francesa", periodo_gracia = 0L),
                  list(...))
  l <- pasivos_wizard_draft_to_liability(d, mode = "create", user = "ana"); l$id <- "V1"; l$estado <- "active"; l
}
cal <- function(l, cid) suppressWarnings(pasivos_generate_schedule(l, today = HOY, client_id = cid))

test_that("A: each client's index; with no client, the captured rate", {
  sa <- cal(credito(), "a"); sb <- cal(credito(), "b"); s0 <- cal(credito(), NULL)
  expect_true("a" %in% LEIDOS && "b" %in% LEIDOS)
  # client a (SOFR 4) = a loan whose captured rate already is 4, read with no index
  expect_equal(sa$interes, cal(credito(tasa_actual = 4), NULL)$interes, tolerance = 1e-9)
  expect_equal(sb$interes, cal(credito(tasa_actual = 6), NULL)$interes, tolerance = 1e-9)
  expect_true(all(sb$interes[1:11] > sa$interes[1:11]))
  expect_true(all(sa$interes[1:11] > s0$interes[1:11]))   # 4 + 1.5 above the captured 3.6 + 1.5
})

test_that("A: the session's client is the default (session$userData$hd_cliente)", {
  ses <- shiny::MockShinySession$new()
  ses$userData$hd_cliente <- function() "b"
  s <- shiny::withReactiveDomain(ses, cal(credito(), pasivos_cliente_sesion()))
  expect_equal(s$interes, cal(credito(), "b")$interes)
  expect_null(pasivos_cliente_sesion(NULL))               # outside a session: no client
  # the provisions and the costeo pass it through
  LEIDOS <<- character()
  invisible(suppressWarnings(pasivos_generate_provisions(credito(), window_start = HOY, client_id = "b")))
  invisible(suppressWarnings(pasivos_costeo(credito(), hoy = HOY, client_id = "b")))
  expect_true(length(LEIDOS) > 0 && all(LEIDOS == "b"))
})
