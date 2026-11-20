# =============================================================================
# tests/test_t63_forecasting_prime.R
# T63: PRIME (FRED DPRIME) in the Forecasting catalog. The catalog is per
# client and was only seeded when empty, so a client seeded before PRIME never
# got it. forecasting_completar_catalogo() adds the seed's missing metrics and
# their global-default subscription, with the Coordinator's conditions:
#   only adds (a metric there, edited by the user or not, is never changed or
#   removed); idempotent; strict re-read right before writing; a failed read
#   writes nothing; two sessions one after the other never duplicate.
# In-memory S3 (aws.s3 is never patched).
# Run from the repo root:  Rscript tests/test_t63_forecasting_prime.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.tomar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% nombres) eval(e, globalenv())
.tomar("R/global.R", "S3_KEYS")
.tomar("R/persistence.R", c(".normalize", ".s3_leer_para_escribir"))
source("R/forecasting_schemas.R"); source("R/forecasting_persistence.R"); source("R/forecasting_metric_registry.R")
log_action <- function(...) invisible(NULL)

.mem <- new.env(); .s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.fallar <- FALSE; .escrituras <- 0L
.s3_key <- function(key, client_id = NULL) paste0(tolower(client_id %||% "x"), "/", key)
.s3_read_with <- function(key, client_id = NULL) if (.fallar) NULL else .mem[[.s3_key(key, client_id)]]
.s3_existe <- function(key, client_id) if (.fallar) NA else exists(.s3_key(key, client_id), envir = .mem, inherits = FALSE)
.s3_write <- function(obj, key, client_id = NULL, ...) { .escrituras <<- .escrituras + 1L; assign(.s3_key(key, client_id), obj, envir = .mem) }
CID <- "luis"
reiniciar <- function() { rm(list = ls(.mem), envir = .mem); .fallar <<- FALSE; .escrituras <<- 0L }
# A client seeded before T63: today's seed without "prime", one metric edited by the user.
viejo <- function() {
  m <- .seed_forecasting_metrics()
  m <- m[m$metric_id != "prime", , drop = FALSE]
  m$label[m$metric_id == "sofr"] <- "SOFR (mi nombre)"
  m$active[m$metric_id == "cad_mxn"] <- FALSE
  save_forecasting_metrics(m, client_id = CID)
  s <- .seed_forecasting_global_subscriptions(m)
  s$method_id[s$metric_id == "usd_mxn"] <- "spot"            # a subscription the user changed
  save_forecasting_subscriptions(s, client_id = CID)
  .escrituras <<- 0L
  invisible(m)
}

test_that("la semilla trae PRIME de FRED (DPRIME), tasa anual", {
  s <- .seed_forecasting_metrics()
  p <- s[s$metric_id == "prime", ]
  expect_identical(nrow(p), 1L)
  expect_identical(p$primary_source_id, "fred"); expect_identical(p$fetch_params[[1]]$serie, "DPRIME")
  expect_identical(p$unit, "percent_annual"); expect_identical(p$category, "interest_rate")
})

test_that("un cliente sembrado antes recibe PRIME y su suscripción; lo demás queda intacto", {
  reiniciar(); antes <- viejo()
  subs_antes <- load_forecasting_subscriptions(client_id = CID)
  expect_identical(forecasting_completar_catalogo(CID), "prime")
  m <- load_forecasting_metrics(client_id = CID)
  expect_identical(nrow(m), nrow(antes) + 1L)
  expect_identical(m$label[m$metric_id == "sofr"], "SOFR (mi nombre)")   # the user's edit stays
  expect_false(m$active[m$metric_id == "cad_mxn"])
  comunes <- intersect(names(antes), names(m))
  expect_equal(as.data.frame(m[m$metric_id != "prime", comunes]), as.data.frame(antes[, comunes]),
               ignore_attr = TRUE)
  s <- load_forecasting_subscriptions(client_id = CID)
  expect_identical(sum(s$metric_id == "prime" & s$consumer_type == "global_default"), 1L)
  expect_identical(s$method_id[s$metric_id == "usd_mxn"], "spot")        # the user's change stays
  expect_identical(nrow(s), nrow(subs_antes) + 1L)
})

test_that("idempotente: la segunda vez no agrega ni escribe nada", {
  reiniciar(); viejo()
  forecasting_completar_catalogo(CID); n <- .escrituras
  expect_identical(forecasting_completar_catalogo(CID), character())
  expect_identical(.escrituras, n)
  expect_identical(sum(load_forecasting_metrics(client_id = CID)$metric_id == "prime"), 1L)
})

test_that("dos sesiones, una tras otra: la segunda relee y no duplica", {
  reiniciar(); viejo()
  a <- forecasting_completar_catalogo(CID)    # session A
  b <- forecasting_completar_catalogo(CID)    # session B, after A wrote
  expect_identical(a, "prime"); expect_identical(b, character())
  expect_identical(sum(load_forecasting_metrics(client_id = CID)$metric_id == "prime"), 1L)
  expect_identical(sum(load_forecasting_subscriptions(client_id = CID)$metric_id == "prime"), 1L)
})

test_that("una lectura fallida no escribe nada (nunca 'catálogo vacío')", {
  reiniciar(); viejo()
  .fallar <<- TRUE
  expect_error(forecasting_completar_catalogo(CID))
  .fallar <<- FALSE
  expect_identical(.escrituras, 0L)
  expect_false("prime" %in% load_forecasting_metrics(client_id = CID)$metric_id)
})

test_that("un catálogo vacío no se completa aquí (se siembra entero arriba)", {
  reiniciar()
  expect_identical(forecasting_completar_catalogo(CID), character())
  expect_identical(.escrituras, 0L)
})
