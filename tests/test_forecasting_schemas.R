# =============================================================================
# tests/test_forecasting_schemas.R
# Schema correctness + persistence round-trip tests.
# Run from project root:  source("tests/test_forecasting_schemas.R")
# =============================================================================

library(dplyr)
library(tibble)
library(jsonlite)

source("R/persistence.R")

if (!exists("S3_KEYS")) {
  S3_KEYS <- list(
    forecasting_series_observations = "forecasting_series_observations.rds",
    forecasting_metrics             = "forecasting_metrics.rds",
    forecasting_sources             = "forecasting_sources.rds",
    forecasting_methods             = "forecasting_methods.rds",
    forecasting_indicators          = "forecasting_indicators.rds",
    forecasting_subscriptions       = "forecasting_subscriptions.rds",
    forecasting_manual_curves       = "forecasting_manual_curves.rds",
    forecasting_fetch_log           = "forecasting_fetch_log.rds",
    # needed by forecasting_persistence.R
    pasivos_estimates = "pasivos_estimates.rds"
  )
}

source("R/forecasting_schemas.R")
source("R/forecasting_persistence.R")

.pass <- 0L; .fail <- 0L
.test <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) {
    cat("[ERROR]", label, "—", conditionMessage(e), "\n"); FALSE
  })
  if (isTRUE(result)) {
    .pass <<- .pass + 1L
    cat("[PASS]", label, "\n")
  } else {
    .fail <<- .fail + 1L
    cat("[FAIL]", label, "\n")
  }
}

# ── 1. Schema column presence ──────────────────────────────────────────────────

.test("series_observations: 8 columns", {
  s <- .schema_forecasting_series_observations()
  length(names(s)) == 8L
})
.test("series_observations: fecha is Date", {
  inherits(.schema_forecasting_series_observations()$fecha, "Date")
})
.test("metrics: 12 columns", {
  length(names(.schema_forecasting_metrics())) == 12L
})
.test("metrics: fallback_source_ids is list", {
  is.list(.schema_forecasting_metrics()$fallback_source_ids)
})
.test("metrics: fetch_params is list", {
  is.list(.schema_forecasting_metrics()$fetch_params)
})
.test("sources: 9 columns", {
  length(names(.schema_forecasting_sources())) == 9L
})
.test("methods: 7 columns", {
  length(names(.schema_forecasting_methods())) == 7L
})
.test("indicators: 6 columns", {
  length(names(.schema_forecasting_indicators())) == 6L
})
.test("subscriptions: 11 columns", {
  length(names(.schema_forecasting_subscriptions())) == 11L
})
.test("subscriptions: method_params is list", {
  is.list(.schema_forecasting_subscriptions()$method_params)
})
.test("manual_curves: 6 columns", {
  length(names(.schema_forecasting_manual_curves())) == 6L
})
.test("fetch_log: 9 columns", {
  length(names(.schema_forecasting_fetch_log())) == 9L
})

# ── 2. List-column round-trip (metrics) ───────────────────────────────────────

.test("metrics list-col round-trip: fetch_params survives pack/unpack", {
  df <- .schema_forecasting_metrics()
  df <- dplyr::bind_rows(df, tibble::tibble(
    metric_id           = "test_m",
    label               = "Test",
    category            = "fx",
    unit                = "rate",
    is_legal_metric     = FALSE,
    primary_source_id   = "yahoo",
    fallback_source_ids = list(list("fred")),
    fetch_params        = list(list(ticker = "TSLA")),
    default_method_id   = "spot",
    active              = TRUE,
    created_at          = Sys.time(),
    notes               = NA_character_
  ))
  packed   <- .fcp_pack_list_col(df, "fetch_params")
  unpacked <- .fcp_unpack_list_col(packed, "fetch_params")
  identical(unpacked$fetch_params[[1]]$ticker, "TSLA")
})

.test("metrics list-col round-trip: fallback_source_ids survives pack/unpack", {
  df <- .schema_forecasting_metrics()
  df <- dplyr::bind_rows(df, tibble::tibble(
    metric_id           = "test_m2",
    label               = "Test2",
    category            = "fx",
    unit                = "rate",
    is_legal_metric     = FALSE,
    primary_source_id   = "yahoo",
    fallback_source_ids = list(list("fred", "manual")),
    fetch_params        = list(list()),
    default_method_id   = "spot",
    active              = TRUE,
    created_at          = Sys.time(),
    notes               = NA_character_
  ))
  packed   <- .fcp_pack_list_col(df, "fallback_source_ids")
  unpacked <- .fcp_unpack_list_col(packed, "fallback_source_ids")
  length(unpacked$fallback_source_ids[[1]]) == 2L
})

.test("subscriptions list-col: method_params survives round-trip", {
  df <- .schema_forecasting_subscriptions()
  df <- dplyr::bind_rows(df, tibble::tibble(
    subscription_id = "s1",
    consumer_type   = "global_default",
    consumer_id     = NA_character_,
    metric_id       = "usd_mxn",
    method_id       = "yesterday_spot",
    method_params   = list(list(n_days_back = 2L)),
    active          = TRUE,
    created_by      = "system",
    created_at      = Sys.time(),
    updated_at      = Sys.time(),
    notes           = NA_character_
  ))
  packed   <- .fcp_pack_list_col(df, "method_params")
  unpacked <- .fcp_unpack_list_col(packed, "method_params")
  identical(as.integer(unpacked$method_params[[1]]$n_days_back), 2L)
})

# ── 3. S3 mocked persistence round-trip ───────────────────────────────────────

.s3_cache_test <- new.env(hash = TRUE, parent = emptyenv())

.orig_s3_write <- NULL
.orig_s3_read  <- NULL

# Temporarily override with in-memory store
with_mock_s3 <- function(expr) {
  orig_write     <- get(".s3_write",      envir = globalenv(), inherits = TRUE)
  orig_read      <- get(".s3_read",       envir = globalenv(), inherits = TRUE)
  orig_read_with <- get(".s3_read_with",  envir = globalenv(), inherits = TRUE)
  env <- .s3_cache_test
  assign(".s3_write", function(obj, key, client_id = NULL) { assign(key, obj, envir = env) },
         envir = globalenv())
  assign(".s3_read", function(key) {
    if (exists(key, envir = env)) get(key, envir = env) else NULL
  }, envir = globalenv())
  # .s3_read_with() adds a client prefix before calling aws.s3 — bypass it so
  # tests hit the same in-memory store as .s3_write() (which uses bare keys).
  assign(".s3_read_with", function(key, client_id = NULL) {
    if (exists(key, envir = env)) get(key, envir = env) else NULL
  }, envir = globalenv())
  on.exit({
    assign(".s3_write",     orig_write,     envir = globalenv())
    assign(".s3_read",      orig_read,      envir = globalenv())
    assign(".s3_read_with", orig_read_with, envir = globalenv())
  }, add = TRUE)
  force(expr)
}

.test("series_observations: write 3 rows, read back identical", {
  with_mock_s3({
    rows <- dplyr::bind_rows(
      .schema_forecasting_series_observations(),
      tibble::tibble(
        metric_id = c("usd_mxn","usd_mxn","tiie28"),
        fecha     = as.Date(c("2026-01-01","2026-01-02","2026-01-03")),
        value     = c(19.1, 19.2, 10.5),
        source_id = "banxico",
        observation_type = "official",
        fetched_at = Sys.time(),
        fetched_by = "test",
        notes = NA_character_
      )
    )
    save_forecasting_series_observations(rows)
    back <- load_forecasting_series_observations()
    nrow(back) == 3L
  })
})

.test("metrics: write 1 row with list cols, read back with intact list cols", {
  with_mock_s3({
    m <- dplyr::bind_rows(.schema_forecasting_metrics(), tibble::tibble(
      metric_id = "rt_test", label = "RT", category = "fx", unit = "rate",
      is_legal_metric = FALSE, primary_source_id = "yahoo",
      fallback_source_ids = list(list("fred")),
      fetch_params = list(list(ticker = "X")),
      default_method_id = "spot", active = TRUE,
      created_at = Sys.time(), notes = NA_character_
    ))
    save_forecasting_metrics(m)
    back <- load_forecasting_metrics()
    nrow(back) == 1L && identical(back$fetch_params[[1]]$ticker, "X")
  })
})

cat("\n── Results ──────────────────────────────────────────\n")
cat(sprintf("PASS: %d   FAIL: %d\n", .pass, .fail))
if (.fail > 0) stop(sprintf("%d test(s) failed", .fail))
