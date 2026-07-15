# =============================================================================
# R/forecasting_schemas.R
# Column-typed empty tibbles for every Forecasting table.
# =============================================================================

.schema_forecasting_series_observations <- function() tibble::tibble(
  metric_id        = character(),
  fecha            = as.Date(character()),
  value            = numeric(),
  source_id        = character(),
  observation_type = character(),   # "official" | "scraped" | "manual_entry"
  fetched_at       = as.POSIXct(character()),
  fetched_by       = character(),
  notes            = character()
)

.schema_forecasting_metrics <- function() tibble::tibble(
  metric_id           = character(),
  label               = character(),
  category            = character(),   # "fx" | "interest_rate" | "inflation" | "commodity" | "equity" | "other"
  unit                = character(),
  is_legal_metric     = logical(),
  primary_source_id   = character(),
  fallback_source_ids = list(),
  fetch_params        = list(),
  default_method_id   = character(),
  active              = logical(),
  created_at          = as.POSIXct(character()),
  notes               = character()
)

.schema_forecasting_sources <- function() tibble::tibble(
  source_id          = character(),
  label              = character(),
  adapter_fn         = character(),
  is_official        = logical(),
  base_url           = character(),
  rate_limit_per_min = integer(),
  api_key_env_var    = character(),
  active             = logical(),
  notes              = character()
)

.schema_forecasting_methods <- function() tibble::tibble(
  method_id     = character(),
  label         = character(),
  kind          = character(),   # "deterministic_rule" | "statistical"
  apply_fn      = character(),
  description   = character(),
  params_schema = list(),
  active        = logical()
)

.schema_forecasting_indicators <- function() tibble::tibble(
  indicator_id  = character(),
  label         = character(),
  apply_fn      = character(),
  category      = character(),   # "trend" | "momentum" | "volatility" | "volume" | "other"
  params_schema = list(),
  active        = logical()
)

.schema_forecasting_subscriptions <- function() tibble::tibble(
  subscription_id = character(),
  consumer_type   = character(),   # "pasivos_liability" | "global_default"
  consumer_id     = character(),
  metric_id       = character(),
  method_id       = character(),
  method_params   = list(),
  active          = logical(),
  created_by      = character(),
  created_at      = as.POSIXct(character()),
  updated_at      = as.POSIXct(character()),
  notes           = character()
)

.schema_forecasting_manual_curves <- function() tibble::tibble(
  subscription_id = character(),
  fecha           = as.Date(character()),
  value           = numeric(),
  set_by          = character(),
  set_at          = as.POSIXct(character()),
  notes           = character()
)

.schema_forecasting_fetch_log <- function() tibble::tibble(
  fetch_id     = character(),
  metric_id    = character(),
  source_id    = character(),
  attempted_at = as.POSIXct(character()),
  status       = character(),   # "ok"|"no_data"|"http_error"|"parse_error"|"rate_limited"|"timeout"
  rows_added   = integer(),
  duration_ms  = integer(),
  error_msg    = character(),
  triggered_by = character()   # "scheduled"|"manual_ui"|"first_use"
)
