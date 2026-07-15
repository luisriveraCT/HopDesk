# =============================================================================
# R/forecasting_metric_registry.R
# Seed data for metrics and sources catalogs.
# Called on first app start if the stores are empty.
# =============================================================================

.seed_forecasting_sources <- function() {
  tibble::tibble(
    source_id          = c("banxico", "yahoo", "fred", "inegi", "manual"),
    label              = c("Banxico SIE", "Yahoo Finance", "FRED (St. Louis Fed)",
                           "INEGI", "Entrada manual"),
    adapter_fn         = c("fcs_fetch_banxico", "fcs_fetch_yahoo", "fcs_fetch_fred",
                           "fcs_fetch_inegi",   "fcs_fetch_manual"),
    is_official        = c(TRUE,  FALSE, TRUE,  TRUE,  FALSE),
    base_url           = c("https://www.banxico.org.mx/SieAPIRest/service/v1/",
                           "https://finance.yahoo.com",
                           "https://api.stlouisfed.org/fred/",
                           "https://www.inegi.org.mx/",
                           NA_character_),
    rate_limit_per_min = c(30L, 60L, 120L, 10L, 0L),
    api_key_env_var    = c("BANXICO_TOKEN", NA_character_, "FRED_API_KEY",
                           NA_character_, NA_character_),
    active             = c(TRUE, TRUE, TRUE, FALSE, TRUE),
    notes              = c(NA_character_, NA_character_, NA_character_,
                           "INEGI adapter not implemented in 6.1",
                           NA_character_)
  )
}

.seed_forecasting_methods <- function() {
  tibble::tibble(
    method_id     = c("spot", "yesterday_spot", "manual_curve"),
    label         = c("Spot (más reciente)", "Spot del día anterior", "Curva manual"),
    kind          = c("deterministic_rule", "deterministic_rule", "deterministic_rule"),
    apply_fn      = c("fcs_method_spot", "fcs_method_yesterday_spot", "fcs_method_manual_curve"),
    description   = c(
      "Valor observado más reciente en o antes de la fecha objetivo.",
      "Valor observado el día hábil anterior a la fecha objetivo.",
      "Valores futuros definidos por el usuario; pasado usa spot."
    ),
    params_schema = list(list(), list(n_days_back = 1L), list(subscription_id = NA_character_)),
    active        = c(TRUE, TRUE, TRUE)
  )
}

.seed_forecasting_metrics <- function() {
  now <- Sys.time()
  tibble::tibble(
    metric_id = c(
      "usd_mxn", "tiie28", "tiie91", "tiie182", "inpc", "udis",
      "sofr",
      "eur_usd", "eur_mxn", "gbp_usd", "gbp_mxn", "jpy_mxn", "cad_mxn"
    ),
    label = c(
      "USD/MXN FIX", "TIIE 28d", "TIIE 91d", "TIIE 182d", "INPC", "UDIS",
      "SOFR",
      "EUR/USD", "EUR/MXN", "GBP/USD", "GBP/MXN", "JPY/MXN", "CAD/MXN"
    ),
    category = c(
      "fx", "interest_rate", "interest_rate", "interest_rate",
      "inflation", "inflation",
      "interest_rate",
      "fx", "fx", "fx", "fx", "fx", "fx"
    ),
    unit = c(
      "currency_per_currency", "percent_annual", "percent_annual", "percent_annual",
      "index", "currency_per_currency",
      "percent_annual",
      "currency_per_currency", "currency_per_currency", "currency_per_currency",
      "currency_per_currency", "currency_per_currency", "currency_per_currency"
    ),
    is_legal_metric = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    primary_source_id = c(
      "banxico", "banxico", "banxico", "banxico", "banxico", "banxico",
      "fred",
      "yahoo", "yahoo", "yahoo", "yahoo", "yahoo", "yahoo"
    ),
    fallback_source_ids = list(
      list(), list(), list(), list(), list(), list(),
      list(),
      list(), list(), list(), list(), list(), list()
    ),
    fetch_params = list(
      list(serie = "SF43718"),   # usd_mxn
      list(serie = "SF43783"),   # tiie28
      list(serie = "SF43878"),   # tiie91
      list(serie = "SF111916"),  # tiie182
      list(serie = "SP1"),       # inpc
      list(serie = "SP68257"),   # udis
      list(serie = "SOFR"),      # sofr (FRED)
      list(ticker = "EURUSD=X"),
      list(ticker = "EURMXN=X"),
      list(ticker = "GBPUSD=X"),
      list(ticker = "GBPMXN=X"),
      list(ticker = "JPYMXN=X"),
      list(ticker = "CADMXN=X")
    ),
    default_method_id = c(
      "yesterday_spot", "yesterday_spot", "yesterday_spot", "yesterday_spot",
      "spot", "spot",
      "yesterday_spot",
      "yesterday_spot", "yesterday_spot", "yesterday_spot",
      "yesterday_spot", "yesterday_spot", "yesterday_spot"
    ),
    active     = rep(TRUE, 13L),
    created_at = rep(now, 13L),
    notes      = rep(NA_character_, 13L)
  )
}

# Seed global-default subscriptions (one per metric, method = metric's default).
.seed_forecasting_global_subscriptions <- function(metrics) {
  now <- Sys.time()
  tibble::tibble(
    subscription_id = vapply(seq_len(nrow(metrics)), function(i) uuid::UUIDgenerate(),
                             character(1)),
    consumer_type   = rep("global_default", nrow(metrics)),
    consumer_id     = rep(NA_character_,    nrow(metrics)),
    metric_id       = metrics$metric_id,
    method_id       = metrics$default_method_id,
    method_params   = rep(list(list()), nrow(metrics)),
    active          = rep(TRUE, nrow(metrics)),
    created_by      = rep("system", nrow(metrics)),
    created_at      = rep(now, nrow(metrics)),
    updated_at      = rep(now, nrow(metrics)),
    notes           = rep(NA_character_, nrow(metrics))
  )
}

# Called during app startup if stores are empty; idempotent.
forecasting_seed_if_empty <- function() {
  metrics_db <- load_forecasting_metrics()
  if (!nrow(metrics_db)) {
    message("[forecasting] seeding metrics catalog")
    save_forecasting_metrics(.seed_forecasting_metrics())
    metrics_db <- load_forecasting_metrics()
  }

  sources_db <- load_forecasting_sources()
  if (!nrow(sources_db)) {
    message("[forecasting] seeding sources catalog")
    save_forecasting_sources(.seed_forecasting_sources())
  }

  methods_db <- load_forecasting_methods()
  if (!nrow(methods_db)) {
    message("[forecasting] seeding methods catalog")
    save_forecasting_methods(.seed_forecasting_methods())
  }

  subs_db <- load_forecasting_subscriptions()
  if (!nrow(subs_db)) {
    message("[forecasting] seeding global-default subscriptions")
    subs <- .seed_forecasting_global_subscriptions(metrics_db)
    save_forecasting_subscriptions(subs)
  }

  invisible(NULL)
}
