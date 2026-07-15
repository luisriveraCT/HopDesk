# =============================================================================
# R/forecasting_sources.R
# Fetch dispatcher: routes metric fetch requests to the correct adapter.
# Adapter functions are resolved by name from globalenv at runtime.
# =============================================================================

# ── In-memory caches (populated at startup from seeded stores) ────────────────

.fcs_metrics_cache  <- NULL
.fcs_sources_cache  <- NULL

.lookup_metric <- function(metric_id, client_id = NULL) {
  if (is.null(.fcs_metrics_cache)) {
    .fcs_metrics_cache <<- load_forecasting_metrics(client_id = client_id)
  }
  m <- .fcs_metrics_cache[.fcs_metrics_cache$metric_id == metric_id, , drop = FALSE]
  if (!nrow(m)) return(NULL)
  as.list(m[1, ])
}

.lookup_source <- function(source_id) {
  if (is.null(.fcs_sources_cache)) {
    .fcs_sources_cache <<- load_forecasting_sources()
  }
  s <- .fcs_sources_cache[.fcs_sources_cache$source_id == source_id, , drop = FALSE]
  if (!nrow(s)) return(NULL)
  as.list(s[1, ])
}

# Flush caches (call after saving sources/metrics).
fcs_flush_caches <- function() {
  .fcs_metrics_cache <<- NULL
  .fcs_sources_cache <<- NULL
  invisible(NULL)
}

.empty_observations <- function() .schema_forecasting_series_observations()

.fetch_succeeded <- function(result) {
  isTRUE(result$status == "ok") && !is.null(result$rows) && nrow(result$rows) > 0L
}

.log_fetch <- function(metric_id, source_id, result,
                        rows_added = 0L, triggered_by = "scheduled", note = NULL) {
  row <- tibble::tibble(
    fetch_id     = uuid::UUIDgenerate(),
    metric_id    = as.character(metric_id),
    source_id    = as.character(source_id),
    attempted_at = Sys.time(),
    status       = as.character(result$status %||% "unknown"),
    rows_added   = as.integer(rows_added),
    duration_ms  = as.integer(result$duration_ms %||% 0L),
    error_msg    = as.character(result$error_msg %||% note %||% NA_character_),
    triggered_by = as.character(triggered_by)
  )
  tryCatch(append_forecasting_fetch_log(row), error = function(e) NULL)
  invisible(NULL)
}

# ── .try_fetch ────────────────────────────────────────────────────────────────

.try_fetch <- function(source_id, metric, date_from, date_to, triggered_by) {
  source <- .lookup_source(source_id)
  if (is.null(source) || !isTRUE(source$active)) {
    return(list(status = "no_data", rows = NULL, error_msg = "source inactive",
                duration_ms = 0L))
  }
  adapter_fn_name <- source$adapter_fn %||% ""
  adapter_fn <- if (nzchar(adapter_fn_name) &&
                    exists(adapter_fn_name, envir = globalenv(), inherits = TRUE)) {
    get(adapter_fn_name, envir = globalenv(), inherits = TRUE)
  } else {
    NULL
  }
  if (is.null(adapter_fn) || !is.function(adapter_fn)) {
    return(list(status = "parse_error", rows = NULL,
                error_msg = sprintf("adapter function '%s' not found", adapter_fn_name),
                duration_ms = 0L))
  }

  t_start <- proc.time()[["elapsed"]]
  res <- tryCatch(
    adapter_fn(metric, date_from, date_to),
    error = function(e) list(status = "parse_error", rows = NULL,
                             error_msg = conditionMessage(e))
  )
  res$duration_ms <- as.integer((proc.time()[["elapsed"]] - t_start) * 1000)
  res
}

# ── Main dispatcher ───────────────────────────────────────────────────────────

#' Fetch observations for a metric from its preferred source.
#'
#' Returns a tibble matching .schema_forecasting_series_observations().
#' Deduplication (by metric_id + fecha + source_id) is the caller's responsibility.
fcs_fetch_metric <- function(metric_id,
                              date_from    = Sys.Date() - 30L,
                              date_to      = Sys.Date(),
                              triggered_by = "scheduled",
                              client_id    = NULL) {
  metric <- .lookup_metric(metric_id, client_id = client_id)
  if (is.null(metric)) {
    stop(sprintf("[forecasting] unknown metric_id: %s", metric_id))
  }

  # Primary attempt
  result <- .try_fetch(metric$primary_source_id, metric, date_from, date_to, triggered_by)
  if (.fetch_succeeded(result)) {
    .log_fetch(metric_id, metric$primary_source_id, result,
               rows_added = nrow(result$rows), triggered_by = triggered_by)
    return(result$rows)
  }

  # Legal metrics: no fallback allowed
  if (isTRUE(metric$is_legal_metric)) {
    .log_fetch(metric_id, metric$primary_source_id, result,
               triggered_by = triggered_by,
               note = "legal metric — fallback disabled by policy")
    return(.empty_observations())
  }

  # Non-legal: try fallbacks in order
  # fallback_source_ids comes from as.list(tibble_row), so it's list(actual_ids) — unwrap once
  fb_raw     <- metric$fallback_source_ids %||% list()
  fb_sources <- if (length(fb_raw) && is.list(fb_raw[[1]])) fb_raw[[1]] else fb_raw
  if (is.character(fb_sources)) fb_sources <- as.list(fb_sources)
  for (fb_source_id in fb_sources) {
    result <- .try_fetch(fb_source_id, metric, date_from, date_to, triggered_by)
    if (.fetch_succeeded(result)) {
      .log_fetch(metric_id, fb_source_id, result,
                 rows_added = nrow(result$rows), triggered_by = triggered_by)
      return(result$rows)
    }
  }

  # All sources failed
  .log_fetch(metric_id, metric$primary_source_id, result,
             triggered_by = triggered_by, note = "all sources failed")
  .empty_observations()
}

# Fetch and APPEND to the observations store (dedup by metric_id + fecha + source_id).
fcs_fetch_and_store <- function(metric_id,
                                 date_from    = Sys.Date() - 30L,
                                 date_to      = Sys.Date(),
                                 triggered_by = "manual_ui",
                                 user         = "system",
                                 client_id    = NULL) {
  new_rows <- tryCatch(
    fcs_fetch_metric(metric_id, date_from, date_to, triggered_by, client_id = client_id),
    error = function(e) {
      warning("[forecasting] fcs_fetch_metric error: ", conditionMessage(e))
      .empty_observations()
    }
  )
  new_rows$fetched_by <- user

  if (!nrow(new_rows)) return(invisible(0L))

  existing <- load_forecasting_series_observations(client_id = client_id)

  # Dedup key: metric_id + fecha + source_id (keep latest fetched_at)
  dedup_key <- function(df) paste(df$metric_id, df$fecha, df$source_id, sep = "\x01")
  new_keys  <- dedup_key(new_rows)
  existing  <- existing[!dedup_key(existing) %in% new_keys, , drop = FALSE]

  updated <- dplyr::bind_rows(existing, new_rows)
  save_forecasting_series_observations(updated, client_id = client_id)
  invisible(nrow(new_rows))
}
