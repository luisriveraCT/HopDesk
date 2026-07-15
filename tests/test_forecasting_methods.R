# =============================================================================
# tests/test_forecasting_methods.R
# Unit tests for all three deterministic forecasting methods.
# Run from project root:  source("tests/test_forecasting_methods.R")
# =============================================================================

library(dplyr)
library(tibble)

source("R/persistence.R")
source("R/policy_engine.R")   # for get_holidays() used by .is_business_day_mx

if (!exists("S3_KEYS")) {
  S3_KEYS <- list(
    forecasting_series_observations = "forecasting_series_observations.rds",
    forecasting_manual_curves       = "forecasting_manual_curves.rds"
  )
}

source("R/forecasting_schemas.R")
source("R/forecasting_persistence.R")
source("R/forecasting_methods.R")

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

# ── Helpers ───────────────────────────────────────────────────────────────────
.obs <- function(metric, fecha, value, type = "official") {
  tibble::tibble(
    metric_id = metric, fecha = as.Date(fecha), value = as.numeric(value),
    source_id = "banxico", observation_type = type,
    fetched_at = Sys.time(), fetched_by = "test", notes = NA_character_
  )
}
.empty_obs <- .schema_forecasting_series_observations()

# ── fcs_method_spot ────────────────────────────────────────────────────────────

.test("spot: empty observations → NA", {
  is.na(fcs_method_spot("usd_mxn", Sys.Date(), list(), .empty_obs))
})
.test("spot: all observations after target_date → NA", {
  obs <- .obs("usd_mxn", Sys.Date() + 5, 19.1)
  is.na(fcs_method_spot("usd_mxn", Sys.Date(), list(), obs))
})
.test("spot: single observation on target_date → that value", {
  obs <- .obs("usd_mxn", Sys.Date(), 19.5)
  identical(fcs_method_spot("usd_mxn", Sys.Date(), list(), obs), 19.5)
})
.test("spot: observation before target_date → most recent", {
  obs <- dplyr::bind_rows(
    .obs("usd_mxn", Sys.Date() - 5, 18.9),
    .obs("usd_mxn", Sys.Date() - 2, 19.1),
    .obs("usd_mxn", Sys.Date() - 1, 19.3)
  )
  identical(fcs_method_spot("usd_mxn", Sys.Date(), list(), obs), 19.3)
})
.test("spot: official wins over scraped on same date", {
  obs <- dplyr::bind_rows(
    .obs("usd_mxn", Sys.Date() - 1, 19.1, "scraped"),
    .obs("usd_mxn", Sys.Date() - 1, 19.5, "official")
  )
  identical(fcs_method_spot("usd_mxn", Sys.Date(), list(), obs), 19.5)
})
.test("spot: scraped wins over manual_entry", {
  obs <- dplyr::bind_rows(
    .obs("usd_mxn", Sys.Date() - 1, 19.1, "manual_entry"),
    .obs("usd_mxn", Sys.Date() - 1, 19.7, "scraped")
  )
  identical(fcs_method_spot("usd_mxn", Sys.Date(), list(), obs), 19.7)
})

# ── .is_business_day_mx ───────────────────────────────────────────────────────

.test(".is_business_day_mx: Monday is business day", {
  # Find a Monday that's not a holiday
  d <- as.Date("2026-02-09")  # Monday Feb 9 2026 (not a holiday)
  isTRUE(.is_business_day_mx(d))
})
.test(".is_business_day_mx: Saturday is not business day", {
  d <- as.Date("2026-05-09")  # Saturday
  !isTRUE(.is_business_day_mx(d))
})
.test(".is_business_day_mx: Mexican holiday (Sep 16) is not business day", {
  d <- as.Date("2026-09-16")
  !isTRUE(.is_business_day_mx(d))
})
.test(".is_business_day_mx: Jan 1 is not business day", {
  !isTRUE(.is_business_day_mx(as.Date("2026-01-01")))
})

# ── fcs_method_yesterday_spot ─────────────────────────────────────────────────

.test("yesterday_spot: n=1, observation on day-before-target → that value", {
  target <- as.Date("2026-05-12")  # Tuesday
  prev   <- .prev_business_day_mx(target, 1L)  # should be 2026-05-11 (Monday)
  obs    <- .obs("tiie28", prev, 11.0)
  res    <- fcs_method_yesterday_spot("tiie28", target, list(n_days_back = 1L), obs)
  identical(res, 11.0)
})
.test("yesterday_spot: no obs within last 7 days → falls back to most-recent-prior", {
  target <- as.Date("2026-05-12")
  obs    <- .obs("tiie28", as.Date("2026-04-01"), 10.5)
  res    <- fcs_method_yesterday_spot("tiie28", target, list(), obs)
  identical(res, 10.5)
})
.test("yesterday_spot: across Mexican holiday (Sep 16 2026 = Wed): prev of Sep 17 is Sep 15", {
  # Sep 17 (Thu); Sep 16 = Independencia holiday; so prev business day = Sep 15 (Tue)
  target <- as.Date("2026-09-17")
  prev   <- .prev_business_day_mx(target, 1L)
  identical(prev, as.Date("2026-09-15"))
})

# ── fcs_method_manual_curve ───────────────────────────────────────────────────

# Mock load_forecasting_manual_curves
.mock_curves <- NULL
.orig_load_curves <- NULL

with_mock_curves <- function(curves_df, expr) {
  old <- if (exists("load_forecasting_manual_curves", envir = globalenv()))
    get("load_forecasting_manual_curves", envir = globalenv()) else NULL
  assign("load_forecasting_manual_curves", function() curves_df, envir = globalenv())
  on.exit({
    if (is.null(old)) rm("load_forecasting_manual_curves", envir = globalenv())
    else assign("load_forecasting_manual_curves", old, envir = globalenv())
  }, add = TRUE)
  force(expr)
}

.test("manual_curve: missing subscription_id → NA", {
  with_mock_curves(.schema_forecasting_manual_curves(), {
    is.na(fcs_method_manual_curve("usd_mxn", Sys.Date() + 10, list(), .empty_obs))
  })
})
.test("manual_curve: past date with observation → spot value", {
  obs  <- .obs("usd_mxn", Sys.Date() - 2, 19.1)
  curves <- tibble::tibble(
    subscription_id = "s1",
    fecha = Sys.Date() - 2,
    value = 99.0,   # should NOT be used for past dates
    set_by = "test", set_at = Sys.time(), notes = NA_character_
  )
  with_mock_curves(curves, {
    res <- fcs_method_manual_curve("usd_mxn", Sys.Date() - 1,
                                   list(subscription_id = "s1"), obs)
    identical(res, 19.1)
  })
})
.test("manual_curve: future exact date → curve value", {
  fut <- Sys.Date() + 10
  curves <- tibble::tibble(
    subscription_id = "s1",
    fecha = fut,
    value = 20.5,
    set_by = "test", set_at = Sys.time(), notes = NA_character_
  )
  with_mock_curves(curves, {
    res <- fcs_method_manual_curve("usd_mxn", fut, list(subscription_id = "s1"), .empty_obs)
    identical(res, 20.5)
  })
})
.test("manual_curve: future interpolation between two points", {
  d1 <- Sys.Date() + 10
  d2 <- Sys.Date() + 20
  curves <- tibble::tibble(
    subscription_id = c("s1","s1"),
    fecha = c(d1, d2),
    value = c(20.0, 22.0),
    set_by = "test", set_at = Sys.time(), notes = NA_character_
  )
  mid <- Sys.Date() + 15
  with_mock_curves(curves, {
    res <- fcs_method_manual_curve("usd_mxn", mid, list(subscription_id = "s1"), .empty_obs)
    abs(res - 21.0) < 0.001  # midpoint interpolation = 21.0
  })
})
.test("manual_curve: future date, no curve entries → NA", {
  with_mock_curves(.schema_forecasting_manual_curves(), {
    res <- fcs_method_manual_curve("usd_mxn", Sys.Date() + 5,
                                   list(subscription_id = "s_no_entries"), .empty_obs)
    is.na(res)
  })
})

cat("\n── Results ──────────────────────────────────────────\n")
cat(sprintf("PASS: %d   FAIL: %d\n", .pass, .fail))
if (.fail > 0) stop(sprintf("%d test(s) failed", .fail))
