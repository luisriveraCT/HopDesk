# =============================================================================
# tests/test_forecasting_resolve.R
# Tests for subscription resolution and forecasting_get_estimate().
# Run from project root:  source("tests/test_forecasting_resolve.R")
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
    forecasting_fetch_log           = "forecasting_fetch_log.rds",
    pasivos_estimates               = "pasivos_estimates.rds",
    pasivos_audit                   = "pasivos_audit.rds"
  )
}

source("R/forecasting_schemas.R")
source("R/forecasting_persistence.R")
source("R/forecasting_metric_registry.R")
source("R/forecasting_methods.R")
source("R/forecasting_resolve.R")

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

# ── Mock S3 helpers ───────────────────────────────────────────────────────────
.s3_env <- new.env(hash = TRUE, parent = emptyenv())

with_mock_data <- function(metrics, methods, subs, obs, expr) {
  # In-memory overrides for all load functions used by the resolver
  # (client_id accepted-and-ignored to match the real load_forecasting_*() signatures)
  assign("load_forecasting_metrics",       function(client_id = NULL) metrics, envir = globalenv())
  assign("load_forecasting_methods",       function(client_id = NULL) methods, envir = globalenv())
  assign("load_forecasting_subscriptions", function(client_id = NULL) subs,    envir = globalenv())
  assign("load_forecasting_series_observations", function(client_id = NULL) obs, envir = globalenv())
  assign("load_forecasting_manual_curves", function(client_id = NULL) .schema_forecasting_manual_curves(),
         envir = globalenv())
  on.exit({
    rm(list = c("load_forecasting_metrics","load_forecasting_methods",
                "load_forecasting_subscriptions","load_forecasting_series_observations",
                "load_forecasting_manual_curves"),
       envir = globalenv())
  }, add = TRUE)
  force(expr)
}

# ── Seed data ─────────────────────────────────────────────────────────────────
.methods_seed <- .seed_forecasting_methods()
.metrics_seed <- tibble::tibble(
  metric_id = "usd_mxn", label = "USD/MXN FIX", category = "fx",
  unit = "currency_per_currency", is_legal_metric = TRUE,
  primary_source_id = "banxico",
  fallback_source_ids = list(list()),
  fetch_params = list(list(serie = "SF43718")),
  default_method_id = "yesterday_spot",
  active = TRUE, created_at = Sys.time(), notes = NA_character_
)
.obs_seed <- tibble::tibble(
  metric_id = "usd_mxn",
  fecha     = .prev_business_day_mx(Sys.Date(), 1L),
  value     = 19.08,
  source_id = "banxico",
  observation_type = "official",
  fetched_at = Sys.time(),
  fetched_by = "system",
  notes = NA_character_
)
.subs_empty <- .schema_forecasting_subscriptions()

# ── 1. Legacy name normalization ──────────────────────────────────────────────
.test("legacy metric: fx_usd_mxn → usd_mxn", {
  identical(.normalize_legacy_metric_id("fx_usd_mxn"), "usd_mxn")
})
.test("legacy metric: tiie28 → tiie28 (unchanged)", {
  identical(.normalize_legacy_metric_id("tiie28"), "tiie28")
})
.test("legacy metric: sofr → sofr (unchanged)", {
  identical(.normalize_legacy_metric_id("sofr"), "sofr")
})

# ── 2. Resolution order ───────────────────────────────────────────────────────

# a) Explicit method override wins
.test("explicit method override wins over all subs", {
  global_sub <- tibble::tibble(
    subscription_id = "g1", consumer_type = "global_default",
    consumer_id = NA_character_, metric_id = "usd_mxn",
    method_id = "yesterday_spot",
    method_params = list(list()),
    active = TRUE, created_by = "test",
    created_at = Sys.time(), updated_at = Sys.time(), notes = NA_character_
  )
  with_mock_data(.metrics_seed, .methods_seed, global_sub, .obs_seed, {
    r <- .resolve_method("usd_mxn", "spot", NULL, NULL)
    identical(r$method$method_id, "spot")
  })
})

# b) Consumer-specific wins over global_default
.test("consumer-specific sub wins over global_default", {
  specific_sub <- tibble::tibble(
    subscription_id = "c1", consumer_type = "pasivos_liability",
    consumer_id = "LID_001", metric_id = "usd_mxn",
    method_id = "spot",
    method_params = list(list()),
    active = TRUE, created_by = "test",
    created_at = Sys.time(), updated_at = Sys.time(), notes = NA_character_
  )
  global_sub <- tibble::tibble(
    subscription_id = "g1", consumer_type = "global_default",
    consumer_id = NA_character_, metric_id = "usd_mxn",
    method_id = "yesterday_spot",
    method_params = list(list()),
    active = TRUE, created_by = "test",
    created_at = Sys.time(), updated_at = Sys.time(), notes = NA_character_
  )
  subs <- dplyr::bind_rows(specific_sub, global_sub)
  with_mock_data(.metrics_seed, .methods_seed, subs, .obs_seed, {
    r <- .resolve_method("usd_mxn", NULL, "pasivos_liability", "LID_001")
    identical(r$method$method_id, "spot")
  })
})

# c) Global default wins over metric default
.test("global_default wins over metric default_method_id", {
  global_sub <- tibble::tibble(
    subscription_id = "g1", consumer_type = "global_default",
    consumer_id = NA_character_, metric_id = "usd_mxn",
    method_id = "spot",
    method_params = list(list()),
    active = TRUE, created_by = "test",
    created_at = Sys.time(), updated_at = Sys.time(), notes = NA_character_
  )
  with_mock_data(.metrics_seed, .methods_seed, global_sub, .obs_seed, {
    r <- .resolve_method("usd_mxn", NULL, NULL, NULL)
    identical(r$method$method_id, "spot")  # not "yesterday_spot" (the metric default)
  })
})

# d) Metric default is final fallback
.test("metric default_method_id is final fallback when no subs", {
  with_mock_data(.metrics_seed, .methods_seed, .subs_empty, .obs_seed, {
    r <- .resolve_method("usd_mxn", NULL, NULL, NULL)
    identical(r$method$method_id, "yesterday_spot")  # metric's default
  })
})

# e) Inactive subscriptions are ignored
.test("inactive sub is ignored; falls back to metric default", {
  inactive_sub <- tibble::tibble(
    subscription_id = "g1", consumer_type = "global_default",
    consumer_id = NA_character_, metric_id = "usd_mxn",
    method_id = "spot",
    method_params = list(list()),
    active = FALSE,   # <── inactive
    created_by = "test", created_at = Sys.time(),
    updated_at = Sys.time(), notes = NA_character_
  )
  with_mock_data(.metrics_seed, .methods_seed, inactive_sub, .obs_seed, {
    r <- .resolve_method("usd_mxn", NULL, NULL, NULL)
    identical(r$method$method_id, "yesterday_spot")
  })
})

# ── 3. forecasting_get_estimate end-to-end ────────────────────────────────────

.test("get_estimate: returns numeric value when obs exist", {
  with_mock_data(.metrics_seed, .methods_seed, .subs_empty, .obs_seed, {
    est <- forecasting_get_estimate("usd_mxn", Sys.Date())
    is.numeric(est) && !is.na(est)
  })
})

.test("get_estimate: legacy name fx_usd_mxn resolves via .normalize", {
  with_mock_data(.metrics_seed, .methods_seed, .subs_empty, .obs_seed, {
    est <- forecasting_get_estimate("fx_usd_mxn", Sys.Date())
    is.numeric(est) && !is.na(est)
  })
})

.test("get_estimate: unknown metric → NA without throwing", {
  with_mock_data(.metrics_seed, .methods_seed, .subs_empty, .obs_seed, {
    est <- forecasting_get_estimate("no_such_metric", Sys.Date())
    is.na(est)
  })
})

.test("get_estimate: no observations → NA", {
  with_mock_data(.metrics_seed, .methods_seed, .subs_empty,
                 .schema_forecasting_series_observations(), {
    est <- forecasting_get_estimate("usd_mxn", Sys.Date())
    is.na(est)
  })
})

cat("\n── Results ──────────────────────────────────────────\n")
cat(sprintf("PASS: %d   FAIL: %d\n", .pass, .fail))
if (.fail > 0) stop(sprintf("%d test(s) failed", .fail))
