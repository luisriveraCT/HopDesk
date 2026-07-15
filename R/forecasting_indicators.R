# =============================================================================
# R/forecasting_indicators.R
# Indicators catalog — empty registry in 6.1.
# Indicators differ from methods: they transform a series -> series.
# Implementations come in Stage 6.5.
# =============================================================================

# Empty catalog is loaded from persistence; this file provides the interface.

# Returns the loaded indicators catalog tibble.
fcs_indicators_catalog <- function() {
  tryCatch(load_forecasting_indicators(),
           error = function(e) .schema_forecasting_indicators())
}

# Apply an indicator transformation.
# indicator_id: string from catalog
# observations: tibble matching .schema_forecasting_series_observations()
# params: named list of indicator-specific params
# Returns a tibble of the same schema (transformed series).
fcs_apply_indicator <- function(indicator_id, observations, params = list()) {
  cat_row <- fcs_indicators_catalog()
  cat_row <- cat_row[cat_row$indicator_id == indicator_id, , drop = FALSE]
  if (!nrow(cat_row)) {
    warning("[forecasting] unknown indicator_id: ", indicator_id)
    return(observations)
  }
  apply_fn <- tryCatch(get(cat_row$apply_fn[1], envir = globalenv(), inherits = TRUE),
                       error = function(e) NULL)
  if (!is.function(apply_fn)) {
    warning("[forecasting] indicator apply_fn not found: ", cat_row$apply_fn[1])
    return(observations)
  }
  tryCatch(apply_fn(observations, params), error = function(e) observations)
}
