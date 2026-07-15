# =============================================================================
# R/forecasting_methods.R
# Deterministic forecasting methods for Stage 6.1.
# Statistical methods (ARIMA, ETS, GARCH) are deferred to Stage 6.3.
# =============================================================================

# ── Business-day helper ───────────────────────────────────────────────────────

# Returns TRUE if `d` is a Mexican business day (Mon–Fri, not a Mexican holiday).
# Reuses get_holidays("MX") from policy_engine.R.
.is_business_day_mx <- function(d) {
  d  <- as.Date(d)
  wd <- as.integer(format(d, "%u"))  # 1=Mon … 7=Sun
  if (wd >= 6L) return(FALSE)        # weekend
  yr <- as.integer(format(d, "%Y"))
  holidays <- tryCatch(get_holidays("MX", years = yr), error = function(e) as.Date(character()))
  !d %in% holidays
}

# Walk back n business days from target_date.
.prev_business_day_mx <- function(target_date, n = 1L) {
  d     <- as.Date(target_date)
  steps <- 0L
  while (steps < n) {
    d <- d - 1L
    if (.is_business_day_mx(d)) steps <- steps + 1L
  }
  d
}

# ── Observation priority resolver ─────────────────────────────────────────────

.resolve_obs_priority <- function(obs) {
  # official > scraped > manual_entry; within same type, latest fecha then latest fetched_at
  prios <- c(official = 1L, scraped = 2L, manual_entry = 3L)
  obs$prio <- prios[obs$observation_type %||% "scraped"]
  obs$prio[is.na(obs$prio)] <- 99L
  obs <- obs[order(obs$prio, -as.numeric(obs$fecha), -as.numeric(obs$fetched_at),
                   na.last = TRUE), , drop = FALSE]
  obs
}

# ── fcs_method_spot ───────────────────────────────────────────────────────────

#' Most-recent observation on or before target_date.
fcs_method_spot <- function(metric_id, target_date, params = list(), observations) {
  target_date <- as.Date(target_date)
  past <- observations[
    !is.na(observations$value) & !is.na(observations$fecha) &
    observations$fecha <= target_date, , drop = FALSE
  ]
  if (!nrow(past)) return(NA_real_)
  past <- .resolve_obs_priority(past)
  past$value[1]
}

# ── fcs_method_yesterday_spot ─────────────────────────────────────────────────

#' Observation as of (target_date − n business days). Default n = 1.
fcs_method_yesterday_spot <- function(metric_id, target_date, params = list(),
                                       observations) {
  n  <- as.integer(params$n_days_back %||% 1L)
  d  <- .prev_business_day_mx(target_date, n)
  fcs_method_spot(metric_id, d, list(), observations)
}

# ── fcs_method_manual_curve ───────────────────────────────────────────────────

#' User-defined forecast values for future dates; past uses spot.
fcs_method_manual_curve <- function(metric_id, target_date, params = list(),
                                     observations) {
  target_date <- as.Date(target_date)
  sub_id <- params$subscription_id %||% NULL
  if (is.null(sub_id) || is.na(sub_id) || !nzchar(as.character(sub_id))) {
    return(NA_real_)
  }

  if (target_date <= Sys.Date()) {
    return(fcs_method_spot(metric_id, target_date, list(), observations))
  }

  curves <- tryCatch(load_forecasting_manual_curves(), error = function(e) NULL)
  if (is.null(curves) || !nrow(curves)) return(NA_real_)

  sub_curves <- curves[curves$subscription_id == sub_id, , drop = FALSE]
  if (!nrow(sub_curves)) return(NA_real_)

  # Exact match
  exact <- sub_curves[!is.na(sub_curves$fecha) & sub_curves$fecha == target_date, , drop = FALSE]
  if (nrow(exact)) return(exact$value[1])

  # Linear interpolation between nearest bracketing points
  before <- sub_curves[!is.na(sub_curves$fecha) & sub_curves$fecha < target_date, , drop = FALSE]
  after  <- sub_curves[!is.na(sub_curves$fecha) & sub_curves$fecha > target_date, , drop = FALSE]

  if (!nrow(before) && !nrow(after)) return(NA_real_)
  if (!nrow(before)) return(after$value[order(after$fecha)][1])
  if (!nrow(after))  return(before$value[order(before$fecha, decreasing = TRUE)][1])

  b <- before[order(before$fecha, decreasing = TRUE), , drop = FALSE][1, ]
  a <- after[order(after$fecha), , drop = FALSE][1, ]
  if (identical(a$fecha, b$fecha)) return(a$value)

  frac <- as.numeric(target_date - b$fecha) / as.numeric(a$fecha - b$fecha)
  b$value + frac * (a$value - b$value)
}
