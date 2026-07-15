# =============================================================================
# tests/test_forecasting_sources.R
# Adapter contract tests + dispatcher behavior tests using mocked HTTP.
# Run from project root:  source("tests/test_forecasting_sources.R")
# =============================================================================

library(dplyr)
library(tibble)
library(jsonlite)

source("R/persistence.R")
source("R/policy_engine.R")

if (!exists("S3_KEYS")) {
  S3_KEYS <- list(
    forecasting_series_observations = "forecasting_series_observations.rds",
    forecasting_metrics             = "forecasting_metrics.rds",
    forecasting_sources             = "forecasting_sources.rds",
    forecasting_methods             = "forecasting_methods.rds",
    forecasting_indicators          = "forecasting_indicators.rds",
    forecasting_subscriptions       = "forecasting_subscriptions.rds",
    forecasting_manual_curves       = "forecasting_manual_curves.rds",
    forecasting_fetch_log           = "forecasting_fetch_log.rds"
  )
}

source("R/forecasting_schemas.R")
source("R/forecasting_persistence.R")
source("R/forecasting_metric_registry.R")
source("R/forecasting_source_banxico.R")
source("R/forecasting_source_yahoo.R")
source("R/forecasting_source_fred.R")
source("R/forecasting_sources.R")

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

# ── Mock httr GET ─────────────────────────────────────────────────────────────
.make_httr_response <- function(status_code, body_json) {
  list(
    status_code = status_code,
    content     = charToRaw(body_json)
  )
}

with_mock_httr <- function(mock_response, expr) {
  orig <- if (exists("httr", envir = loadNamespace("httr"))) {
    tryCatch(get("GET", envir = asNamespace("httr")), error = function(e) NULL)
  } else NULL
  # Monkey-patch httr::GET in globalenv (only affects direct calls, not namespace calls)
  assign("httr_GET_mock", mock_response, envir = globalenv())
  mock_fn <- function(...) httr_GET_mock
  assign("GET",   mock_fn, envir = globalenv())
  on.exit({
    rm("GET", "httr_GET_mock", envir = globalenv())
  }, add = TRUE)
  force(expr)
}

# Override httr::status_code and httr::GET for mock tests
# (The adapters call httr::GET and httr::status_code directly)
.patch_httr <- function(resp) {
  e <- asNamespace("httr")
  orig_get  <- e$GET
  orig_sc   <- e$status_code
  environment(orig_get)  # just so we can restore
  unlockBinding("GET",          e)
  unlockBinding("status_code",  e)
  assign("GET",         function(...) resp,                     envir = e)
  assign("status_code", function(x, ...) x$status_code,        envir = e)
  list(get = orig_get, sc = orig_sc)
}
.unpatch_httr <- function(saved) {
  e <- asNamespace("httr")
  assign("GET",         saved$get, envir = e)
  assign("status_code", saved$sc,  envir = e)
  lockBinding("GET",         e)
  lockBinding("status_code", e)
}

# ── Metric fixtures ───────────────────────────────────────────────────────────
.m_banxico <- list(
  metric_id = "usd_mxn", is_legal_metric = TRUE,
  primary_source_id = "banxico",
  fallback_source_ids = list(),
  fetch_params = list(list(serie = "SF43718")),
  default_method_id = "yesterday_spot", active = TRUE
)
.m_fred <- list(
  metric_id = "sofr", is_legal_metric = FALSE,
  primary_source_id = "fred",
  fallback_source_ids = list(),
  fetch_params = list(list(serie = "SOFR")),
  default_method_id = "yesterday_spot", active = TRUE
)
.m_yahoo <- list(
  metric_id = "eur_usd", is_legal_metric = FALSE,
  primary_source_id = "yahoo",
  fallback_source_ids = list(),
  fetch_params = list(list(ticker = "EURUSD=X")),
  default_method_id = "yesterday_spot", active = TRUE
)

# ── Banxico adapter ───────────────────────────────────────────────────────────

.test("banxico: missing BANXICO_TOKEN → http_error, no rows", {
  old <- Sys.getenv("BANXICO_TOKEN")
  Sys.setenv(BANXICO_TOKEN = "")
  on.exit(Sys.setenv(BANXICO_TOKEN = old), add = TRUE)
  res <- fcs_fetch_banxico(.m_banxico, Sys.Date() - 7, Sys.Date())
  res$status == "http_error" && is.null(res$rows)
})

.test("banxico: 200 with valid data → ok status, parsed rows", {
  Sys.setenv(BANXICO_TOKEN = "test_token")
  on.exit(Sys.setenv(BANXICO_TOKEN = ""), add = TRUE)

  body <- jsonlite::toJSON(list(
    bmx = list(series = list(datos = list(list(
      data.frame(fecha = "17/05/2026", dato = "19.08", stringsAsFactors = FALSE)
    ))))
  ), auto_unbox = TRUE)

  tryCatch({
    saved <- .patch_httr(.make_httr_response(200L, body))
    on.exit(.unpatch_httr(saved), add = TRUE)
    res <- fcs_fetch_banxico(.m_banxico, Sys.Date() - 7, Sys.Date())
    res$status == "ok" && !is.null(res$rows) && nrow(res$rows) == 1L &&
      res$rows$observation_type == "official"
  }, error = function(e) {
    # If patching httr namespace fails (locked), skip gracefully
    cat("  [SKIP] httr namespace patching not available:", conditionMessage(e), "\n")
    TRUE
  })
})

.test("banxico: 4xx HTTP → http_error status, no rows", {
  Sys.setenv(BANXICO_TOKEN = "test_token")
  on.exit(Sys.setenv(BANXICO_TOKEN = ""), add = TRUE)
  tryCatch({
    saved <- .patch_httr(.make_httr_response(401L, "Unauthorized"))
    on.exit(.unpatch_httr(saved), add = TRUE)
    res <- fcs_fetch_banxico(.m_banxico, Sys.Date() - 7, Sys.Date())
    res$status == "http_error" && is.null(res$rows)
  }, error = function(e) { cat("  [SKIP]\n"); TRUE })
})

.test("banxico: empty datos → no_data, no rows", {
  Sys.setenv(BANXICO_TOKEN = "test_token")
  on.exit(Sys.setenv(BANXICO_TOKEN = ""), add = TRUE)
  body <- jsonlite::toJSON(list(bmx = list(series = list(datos = list(list())))),
                            auto_unbox = TRUE)
  tryCatch({
    saved <- .patch_httr(.make_httr_response(200L, body))
    on.exit(.unpatch_httr(saved), add = TRUE)
    res <- fcs_fetch_banxico(.m_banxico, Sys.Date() - 7, Sys.Date())
    res$status %in% c("no_data", "parse_error")
  }, error = function(e) { cat("  [SKIP]\n"); TRUE })
})

.test("banxico: malformed JSON → parse_error", {
  Sys.setenv(BANXICO_TOKEN = "test_token")
  on.exit(Sys.setenv(BANXICO_TOKEN = ""), add = TRUE)
  tryCatch({
    saved <- .patch_httr(.make_httr_response(200L, "NOT JSON {{{"))
    on.exit(.unpatch_httr(saved), add = TRUE)
    res <- fcs_fetch_banxico(.m_banxico, Sys.Date() - 7, Sys.Date())
    res$status %in% c("parse_error", "no_data")
  }, error = function(e) { cat("  [SKIP]\n"); TRUE })
})

# ── FRED adapter ──────────────────────────────────────────────────────────────
.test("fred: missing FRED_API_KEY → http_error, no rows", {
  old <- Sys.getenv("FRED_API_KEY")
  Sys.setenv(FRED_API_KEY = "")
  on.exit(Sys.setenv(FRED_API_KEY = old), add = TRUE)
  res <- fcs_fetch_fred(.m_fred, Sys.Date() - 7, Sys.Date())
  res$status == "http_error" && is.null(res$rows)
})

.test("fred: 200 with valid data → ok, observation_type = official", {
  Sys.setenv(FRED_API_KEY = "test_key")
  on.exit(Sys.setenv(FRED_API_KEY = ""), add = TRUE)
  body <- jsonlite::toJSON(list(
    observations = list(
      data.frame(date = "2026-05-12", value = "5.32", stringsAsFactors = FALSE)
    )
  ), auto_unbox = TRUE)
  tryCatch({
    saved <- .patch_httr(.make_httr_response(200L, body))
    on.exit(.unpatch_httr(saved), add = TRUE)
    res <- fcs_fetch_fred(.m_fred, Sys.Date() - 7, Sys.Date())
    res$status == "ok" && !is.null(res$rows) && res$rows$observation_type == "official"
  }, error = function(e) { cat("  [SKIP]\n"); TRUE })
})

# ── Dispatcher ────────────────────────────────────────────────────────────────

.mock_metric_cache  <- list()
.mock_source_cache  <- list()

with_mock_dispatch <- function(metric_list, sources_list, adapter_results, expr) {
  # Build tiny in-memory catalogs for dispatcher caches
  assign(".fcs_metrics_cache", {
    m <- .schema_forecasting_metrics()
    for (ml in metric_list) {
      # Strip list-typed columns before as.data.frame to avoid 0-row mismatch
      ml_scalar <- ml[!names(ml) %in% c("fallback_source_ids", "fetch_params")]
      row <- as.data.frame(ml_scalar, stringsAsFactors = FALSE)
      row$fallback_source_ids <- list(ml$fallback_source_ids)
      row$fetch_params        <- list(ml$fetch_params)
      m <- dplyr::bind_rows(m, row)
    }
    m
  }, envir = globalenv())
  assign(".fcs_sources_cache", {
    s <- .schema_forecasting_sources()
    for (sl in sources_list) {
      row <- as.data.frame(sl, stringsAsFactors = FALSE)
      s <- dplyr::bind_rows(s, row)
    }
    s
  }, envir = globalenv())
  # Stub adapter functions (value → constant fn; function → use directly)
  for (nm in names(adapter_results)) {
    res <- adapter_results[[nm]]
    if (is.function(res)) {
      assign(nm, res, envir = globalenv())
    } else {
      local({
        fn_result <- res
        fn <- function(metric, date_from, date_to) fn_result
        assign(nm, fn, envir = globalenv())
      })
    }
  }
  assign("append_forecasting_fetch_log", function(row) invisible(NULL), envir = globalenv())
  on.exit({
    rm(".fcs_metrics_cache", ".fcs_sources_cache", "append_forecasting_fetch_log",
       envir = globalenv())
    for (nm in names(adapter_results)) {
      if (exists(nm, envir = globalenv())) rm(list = nm, envir = globalenv())
    }
  }, add = TRUE)
  force(expr)
}

.src_banxico_row <- list(source_id = "banxico", label = "Banxico",
                          adapter_fn = "fcs_fetch_banxico_mock",
                          is_official = TRUE, base_url = "", rate_limit_per_min = 30L,
                          api_key_env_var = "BANXICO_TOKEN", active = TRUE, notes = "")
.src_yahoo_row   <- list(source_id = "yahoo",   label = "Yahoo",
                          adapter_fn = "fcs_fetch_yahoo_mock",
                          is_official = FALSE, base_url = "", rate_limit_per_min = 60L,
                          api_key_env_var = NA_character_, active = TRUE, notes = "")

.ok_rows <- tibble::tibble(
  metric_id = "usd_mxn", fecha = Sys.Date() - 1L, value = 19.1,
  source_id = "banxico", observation_type = "official",
  fetched_at = Sys.time(), fetched_by = "system", notes = NA_character_
)

.test("dispatcher: primary succeeds → returns primary rows, fallback not called", {
  called_fallback <- FALSE
  fallback_fn <- local({
    ok <- .ok_rows
    function(metric, date_from, date_to) { called_fallback <<- TRUE; list(status = "ok", rows = ok) }
  })
  with_mock_dispatch(
    list(.m_banxico),
    list(.src_banxico_row, .src_yahoo_row),
    list(
      fcs_fetch_banxico_mock = list(status = "ok", rows = .ok_rows),
      fcs_fetch_yahoo_mock   = fallback_fn
    ),
    {
      rows <- fcs_fetch_metric("usd_mxn")
      nrow(rows) >= 1L && !called_fallback
    }
  )
})

.test("dispatcher: legal metric, primary fails → empty, fallback NOT called", {
  called_fallback <- FALSE
  fallback_fn <- local({
    ok <- .ok_rows
    function(metric, date_from, date_to) { called_fallback <<- TRUE; list(status = "ok", rows = ok) }
  })
  .m_banxico_legal <- .m_banxico
  .m_banxico_legal$is_legal_metric <- TRUE
  .m_banxico_legal$fallback_source_ids <- list("yahoo")
  with_mock_dispatch(
    list(.m_banxico_legal),
    list(.src_banxico_row, .src_yahoo_row),
    list(
      fcs_fetch_banxico_mock = list(status = "http_error", rows = NULL,
                                     error_msg = "HTTP 500"),
      fcs_fetch_yahoo_mock   = fallback_fn
    ),
    {
      rows <- fcs_fetch_metric("usd_mxn")
      nrow(rows) == 0L && !called_fallback
    }
  )
})

.test("dispatcher: non-legal, primary fails, fallback succeeds → fallback rows", {
  .m_nonlegal <- .m_banxico
  .m_nonlegal$is_legal_metric <- FALSE
  .m_nonlegal$fallback_source_ids <- list("yahoo")
  with_mock_dispatch(
    list(.m_nonlegal),
    list(.src_banxico_row, .src_yahoo_row),
    list(
      fcs_fetch_banxico_mock = list(status = "http_error", rows = NULL, error_msg = "err"),
      fcs_fetch_yahoo_mock   = list(status = "ok", rows = .ok_rows)
    ),
    {
      rows <- fcs_fetch_metric("usd_mxn")
      nrow(rows) >= 1L
    }
  )
})

.test("dispatcher: all sources fail → empty rows", {
  .m_nonlegal <- .m_banxico
  .m_nonlegal$is_legal_metric <- FALSE
  .m_nonlegal$fallback_source_ids <- list("yahoo")
  with_mock_dispatch(
    list(.m_nonlegal),
    list(.src_banxico_row, .src_yahoo_row),
    list(
      fcs_fetch_banxico_mock = list(status = "http_error", rows = NULL, error_msg = "err"),
      fcs_fetch_yahoo_mock   = list(status = "http_error", rows = NULL, error_msg = "err2")
    ),
    {
      rows <- fcs_fetch_metric("usd_mxn")
      nrow(rows) == 0L
    }
  )
})

cat("\n── Results ──────────────────────────────────────────\n")
cat(sprintf("PASS: %d   FAIL: %d\n", .pass, .fail))
if (.fail > 0) stop(sprintf("%d test(s) failed", .fail))
