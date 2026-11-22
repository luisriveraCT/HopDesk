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

# ── B: the convention, by hand ───────────────────────────────────────────────
# 10,000,000 MXN, TIIE28 9.00 % (client a) + 250 bps = 11.50 % nominal,
# 36 monthly payments from 2026-12-05, French.
tiie <- function(...) do.call(credito, modifyList(list(moneda_pago = "MXN", tasa_tipo = "variable_tiie28", tasa_actual = 8,
  tasa_spread = 250, principal_original = 1e7, plazo_meses = 36L, fecha_inicio = as.Date("2026-12-05")), list(...)))
a_mano <- function(fechas, tasa_de) {           # tasa_de(fecha): nominal annual, decimal
  s <- 1e7; n <- length(fechas); prev <- as.Date("2026-12-05"); out <- data.frame(interes = numeric(n), pago = numeric(n))
  for (k in seq_len(n)) {
    t <- tasa_de(fechas[k]); r <- t / 12; rem <- n - k + 1
    pago <- s * r * (1 + r)^rem / ((1 + r)^rem - 1)
    int <- s * t * as.numeric(fechas[k] - prev) / 360
    cap <- if (k == n) s else pago - int
    out[k, ] <- c(int, cap + int); s <- s - cap; prev <- fechas[k]
  }
  out
}

test_that("B: nominal annual, simple interest Act/360 with real days: the loan by hand", {
  s <- cal(tiie(), "a")
  expect_identical(nrow(s), 36L)
  expect_equal(s$fecha[1], as.Date("2027-01-05"))
  expect_equal(s$interes[1], 1e7 * 0.115 * 31 / 360, tolerance = 1e-6)       # 99,027.78
  expect_equal(round(s$interes[1], 2), 99027.78)
  expect_equal(s$total[1], 329760.06, tolerance = 0.01)                      # the payment on 11.50 % / 12
  # February has 31 days before it (Jan 5 -> Feb 5) and March 28
  expect_equal(s$interes[3], s$saldo_post[2] * 0.115 * 28 / 360, tolerance = 1e-6)
  h <- a_mano(s$fecha, function(f) 0.115)
  expect_equal(s$interes, h$interes, tolerance = 1e-6)
  expect_equal(sum(s$interes), sum(h$interes), tolerance = 1e-6)
  expect_equal(tail(s$saldo_post, 1), 0)
  # what the paso 0 measured: about 1.905 M of interest, above the old 1.775 M
  expect_true(abs(sum(s$interes) - 1905551.55) / 1905551.55 < 0.002)
  expect_gt(sum(s$interes), 1774853.41 + 120000)
})

test_that("B: the index changes half-way: from its date on, the new rate, and the payment is worked out again", {
  ALMACEN$a <<- dplyr::bind_rows(ALMACEN$a, tibble::tibble(metric = "tiie28", fecha = as.Date("2027-06-01"), value = 10,
    source_method = "manual", is_frozen = FALSE, updated_by = "t", updated_at = Sys.time()))
  on.exit(ALMACEN$a <<- ALMACEN$a[!(ALMACEN$a$metric == "tiie28" & ALMACEN$a$fecha == as.Date("2027-06-01")), ])
  s <- cal(tiie(), "a")
  h <- a_mano(s$fecha, function(f) if (f >= as.Date("2027-06-01")) 0.125 else 0.115)
  expect_equal(s$interes, h$interes, tolerance = 1e-6)
  expect_equal(s$total, h$pago, tolerance = 1e-6)
  expect_gt(s$total[7], s$total[5])                                          # June: payment goes up
})

test_that("B: no index (no client): the captured rate, the same convention", {
  s <- cal(tiie(), NULL)                                                      # 8 + 2.5 = 10.5 %
  expect_equal(s$interes[1], 1e7 * 0.105 * 31 / 360, tolerance = 1e-6)
  v <- pasivos_tasa_variable_vigente(tiie(), as.Date("2026-12-05"), client_id = NULL)
  expect_equal(v$nominal, 10.5); expect_false(v$del_indice)
  v <- pasivos_tasa_variable_vigente(tiie(), as.Date("2026-12-05"), client_id = "a")
  expect_equal(v$nominal, 11.5); expect_true(v$del_indice)
  expect_null(pasivos_tasa_variable_vigente(tiie(tasa_tipo = "fija")))
  expect_equal(pasivos_ear_act360(11.5, 12), 100 * ((1 + 0.115 * 365 / 360 / 12)^12 - 1))
  expect_equal(round(pasivos_ear_act360(11.5, 12), 2), 12.30)
})

test_that("B: a fixed rate does not change (effective, as before)", {
  s <- cal(tiie(tasa_tipo = "fija", tasa_actual = 11.5, tasa_spread = 0), "a")
  r <- (1.115)^(1 / 12) - 1
  expect_equal(s$interes[1], 1e7 * r, tolerance = 1e-6)
  pago <- 1e7 * r / (1 - (1 + r)^-36)
  expect_equal(s$total[1], pago, tolerance = 1e-6)
  expect_equal(sum(s$interes), 36 * pago - 1e7, tolerance = 0.01)
})

test_that("B: the re-amortization from a declared balance uses the same convention", {
  l <- tiie(fecha_inicio = as.Date("2026-06-05")); l$saldo_capital <- 8e6                         # far from the computed one
  s <- cal(l, "a"); f <- s[!s$historico, ]
  prev <- s$fecha[max(which(s$historico))]
  expect_true(all(f$re_amortized))
  expect_equal(f$interes[1], 8e6 * 0.115 * as.numeric(f$fecha[1] - prev) / 360, tolerance = 1e-6)
  expect_equal(tail(f$saldo_post, 1), 0)
})

test_that("the dead modifier is gone (it computed valor/12 on the period's capital, and nothing called it)", {
  expect_false(exists("pasivos_apply_modifiers"))
  expect_false(exists(".metric_for_modifier_type"))
  src <- paste(readLines("R/pasivos_engine.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl(".get_period_rate", src, fixed = TRUE))
})
