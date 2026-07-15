# =============================================================================
# R/forecasting_service.R
# Public stub API for the Forecasting service.
# This is the ONLY way other modules read or write estimate values.
# Internal implementation will be replaced in a future Forecasting module;
# the public function signatures are frozen.
#
# Behavior contract:
#   forecasting_get_estimate() always returns numeric scalar or NA. Never throws.
#   Frozen values override everything.
#   Never blocks on I/O for more than one S3 read.
# =============================================================================

# Get an estimate for a metric on a specific date.
# @param metric  character: "fx_usd_mxn" | "sofr" | "tiie28" | future
# @param fecha   Date: the date this estimate applies to
# @param method  character or NULL: method override; ignored in Stage 0 stub
# @return numeric scalar; NA_real_ if no estimate available
forecasting_get_estimate <- function(metric, fecha, method = NULL, client_id = NULL) {
  est <- tryCatch(load_pasivos_estimates(client_id = client_id), error = function(e) NULL)
  if (is.null(est) || !nrow(est)) return(NA_real_)

  matches <- est[est$metric == metric, , drop = FALSE]
  if (!nrow(matches)) return(NA_real_)

  # Frozen wins over everything
  frozen <- matches[!is.na(matches$is_frozen) & matches$is_frozen, , drop = FALSE]
  if (nrow(frozen)) return(frozen$value[1])

  fecha <- as.Date(fecha)

  # Exact date match
  exact <- matches[!is.na(matches$fecha) & matches$fecha == fecha, , drop = FALSE]
  if (nrow(exact)) return(exact$value[1])

  # Most recent value on or before fecha
  past <- matches[!is.na(matches$fecha) & matches$fecha <= fecha, , drop = FALSE]
  if (nrow(past)) return(past$value[order(past$fecha, decreasing = TRUE)[1]])

  # No data on or before fecha; return earliest known value as placeholder
  ord <- order(matches$fecha)
  matches$value[ord[1]]
}

# Set or update an estimate. Upserts by (metric, fecha).
# @param metric        character
# @param fecha         Date
# @param value         numeric
# @param source_method character; default "manual"
# @param is_frozen     logical; default FALSE
# @param user          character; default "system"
# @return invisible(TRUE)
forecasting_set_estimate <- function(metric, fecha, value,
                                      source_method = "manual",
                                      is_frozen     = FALSE,
                                      user          = "system",
                                      client_id     = NULL) {
  fecha   <- as.Date(fecha)
  current <- tryCatch(load_pasivos_estimates(client_id = client_id), error = function(e) .schema_pasivos_estimates())

  existing_idx <- which(current$metric == metric & !is.na(current$fecha) &
                          current$fecha == fecha)

  new_row <- tibble::tibble(
    metric        = as.character(metric),
    fecha         = fecha,
    value         = as.numeric(value),
    source_method = as.character(source_method),
    is_frozen     = isTRUE(is_frozen),
    updated_by    = as.character(user),
    updated_at    = Sys.time()
  )

  if (length(existing_idx)) {
    current[existing_idx[1], ] <- new_row
    updated <- current
  } else {
    updated <- dplyr::bind_rows(current, new_row)
  }

  save_pasivos_estimates(updated, client_id = client_id)

  pasivos_log_audit(
    action_type = "estimate.changed",
    user        = user,
    target_kind = "estimate",
    target_id   = paste0(metric, "@", as.character(fecha)),
    after       = list(metric = metric, fecha = as.character(fecha),
                       value = value, source_method = source_method,
                       is_frozen = is_frozen),
    client_id   = client_id
  )

  invisible(TRUE)
}

# Pin a metric to a single hypothetical value (all dates share this frozen value).
# @param metric  character
# @param value   numeric
# @param user    character
forecasting_freeze_metric <- function(metric, value, user = "system", client_id = NULL) {
  current <- tryCatch(load_pasivos_estimates(client_id = client_id), error = function(e) .schema_pasivos_estimates())

  # Unfreeze any existing rows for this metric, then insert a synthetic frozen row.
  current$is_frozen[current$metric == metric] <- FALSE

  frozen_row <- tibble::tibble(
    metric        = as.character(metric),
    fecha         = Sys.Date(),
    value         = as.numeric(value),
    source_method = "manual",
    is_frozen     = TRUE,
    updated_by    = as.character(user),
    updated_at    = Sys.time()
  )
  updated <- dplyr::bind_rows(current, frozen_row)
  save_pasivos_estimates(updated, client_id = client_id)

  pasivos_log_audit(
    action_type = "estimate.frozen",
    user        = user,
    target_kind = "estimate",
    target_id   = metric,
    after       = list(metric = metric, frozen_value = value),
    client_id   = client_id
  )

  invisible(TRUE)
}

# Remove the freeze on a metric so live/interpolated values are used again.
# @param metric  character
# @param user    character
forecasting_unfreeze_metric <- function(metric, user = "system", client_id = NULL) {
  current <- tryCatch(load_pasivos_estimates(client_id = client_id), error = function(e) .schema_pasivos_estimates())

  current$is_frozen[current$metric == metric] <- FALSE
  save_pasivos_estimates(current, client_id = client_id)

  pasivos_log_audit(
    action_type = "estimate.unfrozen",
    user        = user,
    target_kind = "estimate",
    target_id   = metric,
    client_id   = client_id
  )

  invisible(TRUE)
}

# List all known metrics in the estimates store.
# @return character vector (may be empty)
forecasting_list_metrics <- function(client_id = NULL) {
  est <- tryCatch(load_pasivos_estimates(client_id = client_id), error = function(e) NULL)
  if (is.null(est) || !nrow(est)) return(character(0))
  unique(est$metric)
}
