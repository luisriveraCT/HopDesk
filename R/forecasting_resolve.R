# =============================================================================
# R/forecasting_resolve.R
# Subscription-aware forecasting resolver. Replaces forecasting_service.R.
# Public signature is backward-compatible with Stage 0.
# =============================================================================

# ── Legacy metric ID normalization ────────────────────────────────────────────

.fcs_legacy_map <- c(
  fx_usd_mxn  = "usd_mxn",
  fx_eur_usd  = "eur_usd",
  fx_eur_mxn  = "eur_mxn",
  fx_gbp_usd  = "gbp_usd",
  fx_gbp_mxn  = "gbp_mxn",
  fx_jpy_mxn  = "jpy_mxn",
  fx_cad_mxn  = "cad_mxn"
  # Rate metrics (sofr, tiie28, etc.) already match their canonical IDs.
)

.normalize_legacy_metric_id <- function(metric) {
  mapped <- .fcs_legacy_map[metric]
  if (!is.na(mapped)) unname(mapped) else metric
}

# ── Method resolution ─────────────────────────────────────────────────────────

.resolve_method <- function(metric_id, method_override, consumer_type, consumer_id,
                             client_id = NULL) {
  methods_catalog <- tryCatch(load_forecasting_methods(),
                               error = function(e) .schema_forecasting_methods())

  # a) Explicit override
  if (!is.null(method_override) && nzchar(method_override %||% "")) {
    m <- methods_catalog[methods_catalog$method_id == method_override, , drop = FALSE]
    if (nrow(m)) {
      return(list(method = m[1, ], params = list(), subscription_id = NA_character_))
    }
  }

  subs <- tryCatch(
    {
      s <- load_forecasting_subscriptions(client_id = client_id)
      s[!is.na(s$active) & s$active, , drop = FALSE]
    },
    error = function(e) .schema_forecasting_subscriptions()
  )

  # b) Consumer-specific subscription
  if (!is.null(consumer_type) && !is.null(consumer_id) &&
      nzchar(consumer_type %||% "") && nzchar(consumer_id %||% "")) {
    s <- subs[
      !is.na(subs$consumer_type) & subs$consumer_type == consumer_type &
      !is.na(subs$consumer_id)   & subs$consumer_id   == consumer_id &
      !is.na(subs$metric_id)     & subs$metric_id     == metric_id,
      , drop = FALSE
    ]
    if (nrow(s)) {
      m <- methods_catalog[methods_catalog$method_id == s$method_id[1], , drop = FALSE]
      if (nrow(m)) {
        return(list(method          = m[1, ],
                    params          = s$method_params[[1]] %||% list(),
                    subscription_id = s$subscription_id[1]))
      }
    }
  }

  # c) Global-default subscription
  s <- subs[
    !is.na(subs$consumer_type) & subs$consumer_type == "global_default" &
    !is.na(subs$metric_id)     & subs$metric_id     == metric_id,
    , drop = FALSE
  ]
  if (nrow(s)) {
    m <- methods_catalog[methods_catalog$method_id == s$method_id[1], , drop = FALSE]
    if (nrow(m)) {
      return(list(method          = m[1, ],
                  params          = s$method_params[[1]] %||% list(),
                  subscription_id = s$subscription_id[1]))
    }
  }

  # d) Metric default_method_id
  metrics <- tryCatch(load_forecasting_metrics(client_id = client_id), error = function(e) .schema_forecasting_metrics())
  metric_row <- metrics[metrics$metric_id == metric_id, , drop = FALSE]
  if (!nrow(metric_row)) return(NULL)
  m <- methods_catalog[methods_catalog$method_id == metric_row$default_method_id[1], , drop = FALSE]
  if (!nrow(m)) return(NULL)
  list(method = m[1, ], params = list(), subscription_id = NA_character_)
}

# ── Public API ────────────────────────────────────────────────────────────────

#' Get a forecast estimate for a metric at a specific date.
#'
#' Backward-compatible with Stage 0 signature. New: subscription-aware resolution.
#'
#' @param metric        Metric ID (e.g. "usd_mxn") or legacy name (e.g. "fx_usd_mxn").
#' @param fecha         Date for the estimate.
#' @param method        Optional method override; NULL = use subscription resolver.
#' @param consumer_type Optional consumer type for per-consumer subscriptions.
#' @param consumer_id   Optional consumer ID for per-consumer subscriptions.
#' @return numeric scalar; NA_real_ if no estimate available.
forecasting_get_estimate <- function(metric, fecha,
                                      method        = NULL,
                                      consumer_type = NULL,
                                      consumer_id   = NULL,
                                      client_id     = NULL) {
  tryCatch({
    metric_id <- .normalize_legacy_metric_id(metric)
    fecha     <- as.Date(fecha)

    resolved <- .resolve_method(metric_id, method, consumer_type, consumer_id,
                                client_id = client_id)
    if (is.null(resolved)) return(NA_real_)

    obs_all <- tryCatch(load_forecasting_series_observations(client_id = client_id),
                        error = function(e) .schema_forecasting_series_observations())
    obs <- obs_all[!is.na(obs_all$metric_id) & obs_all$metric_id == metric_id, , drop = FALSE]

    apply_fn <- get(resolved$method$apply_fn, envir = globalenv(), inherits = TRUE)
    apply_fn(
      metric_id   = metric_id,
      target_date = fecha,
      params      = c(resolved$params, list(subscription_id = resolved$subscription_id)),
      observations = obs
    )
  }, error = function(e) {
    warning("[forecasting] forecasting_get_estimate error: ", conditionMessage(e))
    NA_real_
  })
}

# ── Backward-compatible helpers from Stage 0 ─────────────────────────────────
# These shim the old pasivos_estimates.rds interface so Stage 0-5 code continues
# to work. In 6.2, pasivos_wizard_module.R will be updated to use the new store.

forecasting_set_estimate <- function(metric, fecha, value,
                                      source_method = "manual",
                                      is_frozen     = FALSE,
                                      user          = "system",
                                      client_id     = NULL) {
  metric_id <- .normalize_legacy_metric_id(metric)
  obs <- tibble::tibble(
    metric_id        = metric_id,
    fecha            = as.Date(fecha),
    value            = as.numeric(value),
    source_id        = "manual",
    observation_type = "manual_entry",
    fetched_at       = Sys.time(),
    fetched_by       = as.character(user),
    notes            = as.character(source_method)
  )
  tryCatch({
    existing <- load_forecasting_series_observations(client_id = client_id)
    # Upsert: remove same (metric_id, fecha, source_id) then add new
    existing <- existing[
      !(existing$metric_id == metric_id &
        !is.na(existing$fecha) & existing$fecha == obs$fecha &
        existing$source_id == "manual"), , drop = FALSE
    ]
    updated <- dplyr::bind_rows(existing, obs)
    save_forecasting_series_observations(updated, client_id = client_id)
    # Also maintain old pasivos_estimates store for Stage 0-5 backward compat
    old_est <- tryCatch(load_pasivos_estimates(client_id = client_id), error = function(e) .schema_pasivos_estimates())
    idx <- which(old_est$metric == metric & !is.na(old_est$fecha) & old_est$fecha == as.Date(fecha))
    new_row <- tibble::tibble(
      metric = as.character(metric), fecha = as.Date(fecha),
      value = as.numeric(value), source_method = as.character(source_method),
      is_frozen = isTRUE(is_frozen), updated_by = as.character(user),
      updated_at = Sys.time()
    )
    if (length(idx)) {
      old_est[idx[1], ] <- new_row
    } else {
      old_est <- dplyr::bind_rows(old_est, new_row)
    }
    save_pasivos_estimates(old_est, client_id = client_id)
  }, error = function(e) warning("[forecasting] forecasting_set_estimate: ", conditionMessage(e)))
  invisible(TRUE)
}

forecasting_freeze_metric <- function(metric, value, user = "system", client_id = NULL) {
  forecasting_set_estimate(metric, Sys.Date(), value, "manual", is_frozen = TRUE, user = user,
                           client_id = client_id)
  invisible(TRUE)
}

forecasting_unfreeze_metric <- function(metric, user = "system", client_id = NULL) {
  tryCatch({
    current <- load_pasivos_estimates(client_id = client_id)
    current$is_frozen[current$metric == metric] <- FALSE
    save_pasivos_estimates(current, client_id = client_id)
  }, error = function(e) NULL)
  invisible(TRUE)
}

forecasting_list_metrics <- function(client_id = NULL) {
  m <- tryCatch(load_forecasting_metrics(client_id = client_id), error = function(e) NULL)
  if (!is.null(m) && nrow(m)) return(m$metric_id)
  character(0)
}
