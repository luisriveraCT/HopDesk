# Antiguedad_App — Stage 6.1: Forecasting Module Foundation

**Audience:** Claude Code (fresh session)
**Project:** Hopdesk (Antiguedad_App, R/Shiny)
**Prerequisite:** Stages 0–5 complete and green. App is operationally stable.
**Scope:** Build the Forecasting module's three-layer foundation: series store, methods + indicators catalogs, subscription resolution. Plus a thin UI that lets a user view series, pick methods, and inspect subscriptions. **No statistical models yet, no sandbox yet, no indicators UI yet.** Those are 6.3, 6.5, and later.

This is the largest greenfield design effort since Stage 1.

---

## 0. Operating instructions

1. **Architectural separation is the deliverable, not just the features.** If you find yourself tempted to read a series directly from a module that should consume through `forecasting_get_estimate()`, stop and surface it.
2. **No new Pasivos features.** Pasivos's only change in this stage is a backwards-compatible upgrade to `forecasting_get_estimate()` consumers — the function signature stays the same, but its resolution path is now subscription-aware. Pasivos liabilities created before this stage continue to work without modification.
3. **Fetch layer and store layer are strictly separated.** Adapters know how to talk to Banxico, Yahoo, FRED, etc. The store knows how to persist and query observations. Nothing in the store directly knows what an "exchange rate" is — it just sees rows keyed by `(metric, fecha)`.
4. **Each numbered task in §1–§8 ends with: stop, summarize, propose test plan, wait for user approval, then run tests.**
5. **No new schema columns on existing Pasivos tables.** The forecasting module adds its own tables.
6. **Recurring bug patterns:** `isTRUE(df$col)` → `!is.na(df$col) & df$col`; `jsonlite::toJSON()` cast `as.character()`.
7. **No user-facing strings reference "S3" / "AWS" / "cloud."**
8. **No reference to financial certifications anywhere** — code, comments, UI text, error messages. The module is for treasury professionals; we don't name credentials.

---

## 1. New file structure

```
R/
├── forecasting_schemas.R           # Schemas for all new tables
├── forecasting_persistence.R       # load/save + sync bus registration
├── forecasting_sources.R           # Fetch dispatcher + per-source adapter registry
├── forecasting_source_banxico.R    # Banxico SIE adapter (legal-data source)
├── forecasting_source_yahoo.R      # Yahoo Finance adapter (via quantmod)
├── forecasting_source_fred.R       # FRED adapter (US indicators)
├── forecasting_methods.R           # Methods catalog: Spot, Yesterday-spot, Manual curve
├── forecasting_indicators.R        # Indicators catalog: empty registry + interface (6.5 fills it)
├── forecasting_resolve.R           # Subscription resolution: forecasting_get_estimate v2
├── forecasting_module.R            # Shiny module: series viewer + method picker + subscriptions
└── forecasting_metric_registry.R   # Metric definitions: id, label, unit, source preference, fetch params
```

The existing `R/forecasting_service.R` (Stage 0 stub) is **replaced** by `R/forecasting_resolve.R`, which provides the same `forecasting_get_estimate(metric, fecha, method = NULL)` signature for backward compatibility, but with the new resolution path. Delete the old stub *after* the new resolver is verified.

Tests:

```
tests/test_forecasting_schemas.R
tests/test_forecasting_methods.R
tests/test_forecasting_resolve.R
tests/test_forecasting_sources.R         # Adapter contract tests with mocked HTTP
```

---

## 2. Schemas (`R/forecasting_schemas.R`)

### 2.1 `.schema_forecasting_series_observations()`

Time-indexed observations. The fact table.

```r
.schema_forecasting_series_observations <- function() tibble(
  metric_id     = character(),   # FK to forecasting_metrics; e.g. "usd_mxn", "tiie28"
  fecha         = as.Date(character()),
  value         = numeric(),
  source_id     = character(),   # FK to forecasting_sources; e.g. "banxico"
  observation_type = character(),# "official" | "scraped" | "manual_entry"
  fetched_at    = as.POSIXct(character()),
  fetched_by    = character(),   # user_id, or "system" for scheduled fetches
  notes         = character()
)
```

The same `(metric_id, fecha)` pair may appear multiple times if observed from different sources or re-fetched at different times. Resolution functions in §4 pick the canonical value per `observation_type` priority: `official > scraped > manual_entry`. Within the same type, latest `fetched_at` wins.

### 2.2 `.schema_forecasting_metrics()`

Metric definitions. Defines what we track.

```r
.schema_forecasting_metrics <- function() tibble(
  metric_id           = character(),   # short ID: "usd_mxn", "tiie28", "sofr", "inpc"
  label               = character(),   # display name: "USD/MXN FIX"
  category            = character(),   # "fx" | "interest_rate" | "inflation" | "commodity" | "equity" | "other"
  unit                = character(),   # "rate" | "percent_annual" | "index" | "currency_per_currency" | etc.
  is_legal_metric     = logical(),     # TRUE = uses only official source, fallback disabled
  primary_source_id   = character(),   # FK to forecasting_sources
  fallback_source_ids = list(),         # ordered list of fallback source IDs; ignored if is_legal_metric
  fetch_params        = list(),         # source-specific: list(serie = "SF43718") for Banxico, list(ticker = "MXN=X") for Yahoo
  default_method_id   = character(),   # which method new subscribers default to
  active              = logical(),     # FALSE = don't auto-fetch, keep historical data
  created_at          = as.POSIXct(character()),
  notes               = character()
)
```

The `is_legal_metric` flag is the architectural enforcement of "Banxico-only for Mexican legal data." When TRUE, `fallback_source_ids` is ignored at runtime; a fetch failure on the primary source returns NA rather than retrying elsewhere. This prevents accidental violation of legal sourcing rules.

### 2.3 `.schema_forecasting_sources()`

Source definitions. Where data comes from.

```r
.schema_forecasting_sources <- function() tibble(
  source_id      = character(),  # "banxico", "yahoo", "fred", "inegi", "manual"
  label          = character(),  # display name
  adapter_fn     = character(),  # name of the adapter function in R (e.g. "fcs_fetch_banxico")
  is_official    = logical(),    # TRUE for govt sources used in legal contexts
  base_url       = character(),  # for documentation/diagnostics
  rate_limit_per_min = integer(),# 0 = unknown
  api_key_env_var = character(), # name of env var holding API key; NA if no key needed
  active         = logical(),
  notes          = character()
)
```

Sources are configured once at install. The `adapter_fn` field is a string name; the dispatcher looks up the actual R function by name. This indirection lets us hot-swap adapters without restarting.

### 2.4 `.schema_forecasting_methods()`

Methods catalog: forecasting rules and statistical models.

```r
.schema_forecasting_methods <- function() tibble(
  method_id     = character(),  # "spot" | "yesterday_spot" | "manual_curve" | future: "arima_011" | etc.
  label         = character(),
  kind          = character(),  # "deterministic_rule" | "statistical"
  apply_fn      = character(),  # name of the apply function in R
  description   = character(),
  params_schema = list(),        # parameter definitions (e.g. for "n_days_ago_spot", n)
  active        = logical()
)
```

For 6.1 the catalog ships with three deterministic rules:

| method_id          | kind                  | apply_fn                       | description                                          |
|--------------------|------------------------|--------------------------------|------------------------------------------------------|
| `spot`             | deterministic_rule    | `fcs_method_spot`              | Most recent observed value, held flat into future.  |
| `yesterday_spot`   | deterministic_rule    | `fcs_method_yesterday_spot`    | Observed value as of (target_date − 1 business day). |
| `manual_curve`     | deterministic_rule    | `fcs_method_manual_curve`      | User-defined `(fecha, value)` pairs for future dates. |

Statistical methods (`arima`, `ets`, `garch`, etc.) come in 6.3.

### 2.5 `.schema_forecasting_indicators()`

Indicators catalog. **Empty registry in 6.1.** Defined for architectural completeness.

```r
.schema_forecasting_indicators <- function() tibble(
  indicator_id  = character(),  # "sma_20", "bbands_20_2", "rsi_14", etc. (filled in 6.5+)
  label         = character(),
  apply_fn      = character(),  # transforms series → series
  category      = character(),  # "trend" | "momentum" | "volatility" | "volume" | "other"
  params_schema = list(),
  active        = logical()
)
```

Indicators differ from methods: methods produce a *forecast value at a future date*; indicators produce a *transformed time series* useful for visualization and as inputs to statistical methods. Both registries exist side by side; consumers query them separately.

### 2.6 `.schema_forecasting_subscriptions()`

Who uses what.

```r
.schema_forecasting_subscriptions <- function() tibble(
  subscription_id    = character(),    # uuid
  consumer_type      = character(),    # "pasivos_liability" | "global_default" | future: "scenario", "report"
  consumer_id        = character(),    # FK to the consumer; e.g. liability_id; NA_character_ for global
  metric_id          = character(),    # FK to forecasting_metrics
  method_id          = character(),    # FK to forecasting_methods
  method_params      = list(),         # method-specific params, e.g. list(n = 1) for n-days-ago variants
  active             = logical(),
  created_by         = character(),
  created_at         = as.POSIXct(character()),
  updated_at         = as.POSIXct(character()),
  notes              = character()
)
```

Resolution order when answering "what method does liability L use for metric M?":

1. Active subscription where `consumer_type = "pasivos_liability"` AND `consumer_id = L` AND `metric_id = M`. If found, use it.
2. Active subscription where `consumer_type = "global_default"` AND `metric_id = M`. If found, use it.
3. The metric's `default_method_id` from §2.2.

This three-step resolution is the heart of the architecture. It lets a treasurer override the default for one specific BAJIO loan without affecting any other loan, and lets them override a global default without touching individual loans.

### 2.7 `.schema_forecasting_manual_curves()`

Storage for `manual_curve` method values. One row per `(subscription_id, fecha)`.

```r
.schema_forecasting_manual_curves <- function() tibble(
  subscription_id = character(),
  fecha           = as.Date(character()),
  value           = numeric(),
  set_by          = character(),
  set_at          = as.POSIXct(character()),
  notes           = character()
)
```

When a subscription uses `manual_curve` method, the resolver pulls future values from here. Past observations come from `forecasting_series_observations`.

### 2.8 `.schema_forecasting_fetch_log()`

Operational log of fetches. Critical for diagnosing source failures.

```r
.schema_forecasting_fetch_log <- function() tibble(
  fetch_id     = character(),
  metric_id    = character(),
  source_id    = character(),
  attempted_at = as.POSIXct(character()),
  status       = character(),   # "ok" | "no_data" | "http_error" | "parse_error" | "rate_limited" | "timeout"
  rows_added   = integer(),
  duration_ms  = integer(),
  error_msg    = character(),
  triggered_by = character()    # "scheduled" | "manual_ui" | "first_use"
)
```

This is append-only. The UI inspector reads it to show "last successful fetch for USD/MXN: 2026-05-11 06:00 from Banxico."

---

## 3. Persistence (`R/forecasting_persistence.R`)

Mirror the discipline from `R/pasivos_persistence.R`. One `load_*` and one `save_*` per table. List-column tables (metrics, sources, methods, subscriptions — they all have `fetch_params` / `params_schema` / `fallback_source_ids` / `method_params`) use the custom list-column round-trip helper.

S3 keys to add in `S3_KEYS`:

```r
forecasting_series_observations = "forecasting_series_observations.rds",
forecasting_metrics             = "forecasting_metrics.rds",
forecasting_sources             = "forecasting_sources.rds",
forecasting_methods             = "forecasting_methods.rds",
forecasting_indicators          = "forecasting_indicators.rds",
forecasting_subscriptions       = "forecasting_subscriptions.rds",
forecasting_manual_curves       = "forecasting_manual_curves.rds",
forecasting_fetch_log           = "forecasting_fetch_log.rds"
```

**Sync bus registration** (in `app.R`, alongside the Stage 5 registrations):

```r
register_synced("forecasting_series_observations_db", S3_KEYS$forecasting_series_observations,
                load_forecasting_series_observations)
register_synced("forecasting_metrics_db",             S3_KEYS$forecasting_metrics,
                load_forecasting_metrics)
register_synced("forecasting_subscriptions_db",       S3_KEYS$forecasting_subscriptions,
                load_forecasting_subscriptions)
register_synced("forecasting_manual_curves_db",       S3_KEYS$forecasting_manual_curves,
                load_forecasting_manual_curves)
```

`forecasting_sources`, `forecasting_methods`, and `forecasting_indicators` are **static configuration** loaded once at app start. They don't need sync (they don't change during normal operation). `forecasting_fetch_log` is append-only and per-session diagnostic; it doesn't need sync either.

Add `bump_sync_version()` calls to every save function for the four synced keys.

---

## 4. Sources & adapters

### 4.1 `R/forecasting_sources.R` — dispatcher

```r
# Fetch dispatcher. Routes requests to the right adapter based on the metric's
# primary_source_id, with conditional fallback for non-legal metrics.
#
# Returns a tibble matching .schema_forecasting_series_observations() with new
# observations to APPEND (caller dedupes by metric_id + fecha + source_id).
fcs_fetch_metric <- function(metric_id,
                              date_from = Sys.Date() - 30,
                              date_to   = Sys.Date(),
                              triggered_by = "scheduled") {
  metric  <- .lookup_metric(metric_id)
  if (is.null(metric)) {
    stop(sprintf("[forecasting] unknown metric_id: %s", metric_id))
  }

  # Primary attempt
  result <- .try_fetch(metric$primary_source_id, metric, date_from, date_to, triggered_by)
  if (.fetch_succeeded(result)) return(result$rows)

  # Legal metrics: no fallback allowed
  if (isTRUE(metric$is_legal_metric)) {
    .log_fetch(metric_id, metric$primary_source_id, result,
               note = "legal metric — fallback disabled by policy")
    return(.empty_observations())
  }

  # Non-legal: try fallbacks in order
  for (fb_source in (metric$fallback_source_ids %||% list())) {
    result <- .try_fetch(fb_source, metric, date_from, date_to, triggered_by)
    if (.fetch_succeeded(result)) return(result$rows)
  }

  # All sources failed
  .log_fetch(metric_id, metric$primary_source_id, result,
             note = "all sources failed")
  .empty_observations()
}

.try_fetch <- function(source_id, metric, date_from, date_to, triggered_by) {
  source <- .lookup_source(source_id)
  if (is.null(source) || !isTRUE(source$active)) {
    return(list(status = "no_data", rows = NULL, error_msg = "source inactive"))
  }
  adapter_fn <- get(source$adapter_fn, envir = globalenv(), inherits = TRUE)
  t_start <- Sys.time()
  res <- tryCatch(
    adapter_fn(metric, date_from, date_to),
    error = function(e) list(status = "parse_error", rows = NULL, error_msg = conditionMessage(e))
  )
  res$duration_ms <- as.integer(difftime(Sys.time(), t_start, units = "secs") * 1000)
  .log_fetch(metric$metric_id, source_id, res, triggered_by = triggered_by)
  res
}
```

The dispatcher is the only place that decides which source to try. Adapters are dumb — given a metric and a date range, return rows or an error.

### 4.2 Adapter contract

Every adapter implements this signature and contract:

```r
# Contract:
# Input:  metric (one row of .schema_forecasting_metrics), date_from, date_to
# Output: list(status = "ok" | "no_data" | "http_error" | "parse_error" | "rate_limited" | "timeout",
#              rows = tibble matching .schema_forecasting_series_observations() OR NULL,
#              error_msg = character() OR NULL)
# Side effects: network call; no S3 writes; no logging (dispatcher logs).
# Time budget: must return within 30 seconds or set status = "timeout".
fcs_fetch_<source_id> <- function(metric, date_from, date_to) { ... }
```

### 4.3 `R/forecasting_source_banxico.R` — Banxico SIE adapter

Uses Banxico's SIE API (https://www.banxico.org.mx/SieAPIRest/service/v1/). Each metric specifies its `serie` code in `fetch_params`. Known series:

- USD/MXN FIX: `SF43718`
- TIIE 28d: `SF43783`
- TIIE 91d: `SF43878`
- TIIE 182d: `SF111916`
- INPC: `SP1`
- UDIS: `SP68257`

The Banxico API returns JSON; parse it into the observations schema. Required header: `Bmx-Token` (free, register at https://www.banxico.org.mx/SieAPIRest/service/v1/token).

```r
fcs_fetch_banxico <- function(metric, date_from, date_to) {
  serie <- metric$fetch_params[[1]]$serie
  token <- Sys.getenv("BANXICO_TOKEN", "")
  if (!nzchar(token)) {
    return(list(status = "http_error", rows = NULL,
                error_msg = "Banxico API token not configured"))
  }

  url <- sprintf("https://www.banxico.org.mx/SieAPIRest/service/v1/series/%s/datos/%s/%s",
                  serie, format(date_from, "%Y-%m-%d"), format(date_to, "%Y-%m-%d"))

  resp <- tryCatch(
    httr::GET(url, httr::add_headers(`Bmx-Token` = token), httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(resp)) return(list(status = "timeout", rows = NULL, error_msg = "no response"))
  if (resp$status_code != 200) {
    return(list(status = "http_error", rows = NULL,
                error_msg = sprintf("HTTP %d", resp$status_code)))
  }

  parsed <- jsonlite::fromJSON(rawToChar(resp$content))
  datos  <- parsed$bmx$series$datos[[1]]
  if (is.null(datos) || !length(datos)) {
    return(list(status = "no_data", rows = NULL, error_msg = NULL))
  }

  rows <- tibble::tibble(
    metric_id        = metric$metric_id,
    fecha            = as.Date(datos$fecha, format = "%d/%m/%Y"),
    value            = suppressWarnings(as.numeric(gsub(",", "", datos$dato))),
    source_id        = "banxico",
    observation_type = "official",
    fetched_at       = Sys.time(),
    fetched_by       = "system",
    notes            = NA_character_
  )
  rows <- rows[!is.na(rows$value), , drop = FALSE]
  list(status = if (nrow(rows)) "ok" else "no_data", rows = rows, error_msg = NULL)
}
```

### 4.4 `R/forecasting_source_yahoo.R` — Yahoo Finance adapter

Via `quantmod::getSymbols(..., src = "yahoo", auto.assign = FALSE)`. Each metric specifies its `ticker` in `fetch_params`. Known tickers for cross-pairs:

- EUR/USD: `EURUSD=X`
- EUR/MXN: `EURMXN=X`
- GBP/USD: `GBPUSD=X`
- JPY/USD: `JPYUSD=X` (or inverse `USDJPY=X` and we transform — but per architectural rule, direct only)

```r
fcs_fetch_yahoo <- function(metric, date_from, date_to) {
  ticker <- metric$fetch_params[[1]]$ticker
  if (!nzchar(ticker)) {
    return(list(status = "parse_error", rows = NULL, error_msg = "missing ticker"))
  }

  data <- tryCatch(
    quantmod::getSymbols(ticker, src = "yahoo", from = date_from, to = date_to,
                          auto.assign = FALSE, warnings = FALSE),
    error = function(e) NULL
  )
  if (is.null(data) || !nrow(data)) {
    return(list(status = "no_data", rows = NULL, error_msg = "no quantmod data"))
  }

  # Use Adjusted close as canonical
  adj_col <- grep("\\.Adjusted$", colnames(data), value = TRUE)
  if (!length(adj_col)) adj_col <- grep("\\.Close$", colnames(data), value = TRUE)
  if (!length(adj_col)) return(list(status = "parse_error", rows = NULL, error_msg = "no Close column"))

  rows <- tibble::tibble(
    metric_id        = metric$metric_id,
    fecha            = as.Date(zoo::index(data)),
    value            = as.numeric(data[, adj_col[1]]),
    source_id        = "yahoo",
    observation_type = "scraped",
    fetched_at       = Sys.time(),
    fetched_by       = "system",
    notes            = NA_character_
  )
  rows <- rows[!is.na(rows$value), , drop = FALSE]
  list(status = if (nrow(rows)) "ok" else "no_data", rows = rows, error_msg = NULL)
}
```

Note `observation_type = "scraped"` — Yahoo is *not* an official source. This labeling matters for resolution priority (§5).

### 4.5 `R/forecasting_source_fred.R` — FRED adapter

For US indicators: SOFR, DGS10, CPIAUCSL, etc. Via the FRED API (https://api.stlouisfed.org/fred/series/observations). Requires free API key.

```r
fcs_fetch_fred <- function(metric, date_from, date_to) {
  serie <- metric$fetch_params[[1]]$serie
  api_key <- Sys.getenv("FRED_API_KEY", "")
  if (!nzchar(api_key)) {
    return(list(status = "http_error", rows = NULL, error_msg = "FRED API key not configured"))
  }

  url <- sprintf("https://api.stlouisfed.org/fred/series/observations?series_id=%s&api_key=%s&file_type=json&observation_start=%s&observation_end=%s",
                  serie, api_key, format(date_from, "%Y-%m-%d"), format(date_to, "%Y-%m-%d"))

  resp <- tryCatch(httr::GET(url, httr::timeout(30)), error = function(e) NULL)
  if (is.null(resp) || resp$status_code != 200) {
    return(list(status = "http_error", rows = NULL,
                error_msg = sprintf("HTTP %s", if (is.null(resp)) "no-resp" else resp$status_code)))
  }

  parsed <- jsonlite::fromJSON(rawToChar(resp$content))
  obs <- parsed$observations
  if (is.null(obs) || !length(obs)) return(list(status = "no_data", rows = NULL, error_msg = NULL))

  rows <- tibble::tibble(
    metric_id        = metric$metric_id,
    fecha            = as.Date(obs$date),
    value            = suppressWarnings(as.numeric(obs$value)),
    source_id        = "fred",
    observation_type = "official",
    fetched_at       = Sys.time(),
    fetched_by       = "system",
    notes            = NA_character_
  )
  rows <- rows[!is.na(rows$value), , drop = FALSE]
  list(status = if (nrow(rows)) "ok" else "no_data", rows = rows, error_msg = NULL)
}
```

### 4.6 Adapters NOT built in 6.1

- INEGI — placeholder source row in the registry (`active = FALSE`); adapter stub returns "no_data" with error_msg "INEGI adapter not implemented in 6.1".
- Manual — adapter handles user-entered observations. **Built**, but trivial: pass through whatever the UI submits.

### 4.7 Initial metric registry (seed data)

`R/forecasting_metric_registry.R` provides a `.seed_forecasting_metrics()` function called on first app start (if `forecasting_metrics.rds` is empty). Seeds:

| metric_id   | label              | category       | unit            | is_legal | primary  | fallbacks         |
|-------------|--------------------|-----------------|-----------------|----------|----------|-------------------|
| `usd_mxn`   | USD/MXN FIX        | fx             | currency_per_currency | TRUE | banxico | (none)            |
| `tiie28`    | TIIE 28d           | interest_rate  | percent_annual  | TRUE     | banxico  | (none)            |
| `tiie91`    | TIIE 91d           | interest_rate  | percent_annual  | TRUE     | banxico  | (none)            |
| `tiie182`   | TIIE 182d          | interest_rate  | percent_annual  | TRUE     | banxico  | (none)            |
| `inpc`      | INPC               | inflation      | index           | TRUE     | banxico  | (none)            |
| `udis`      | UDIS               | inflation      | currency_per_currency | TRUE | banxico | (none)            |
| `sofr`      | SOFR               | interest_rate  | percent_annual  | FALSE    | fred     | (none for now)    |
| `eur_usd`   | EUR/USD            | fx             | currency_per_currency | FALSE | yahoo | (none for now)    |
| `eur_mxn`   | EUR/MXN            | fx             | currency_per_currency | FALSE | yahoo | (none for now)    |
| `gbp_usd`   | GBP/USD            | fx             | currency_per_currency | FALSE | yahoo | (none for now)    |
| `gbp_mxn`   | GBP/MXN            | fx             | currency_per_currency | FALSE | yahoo | (none for now)    |
| `jpy_mxn`   | JPY/MXN            | fx             | currency_per_currency | FALSE | yahoo | (none for now)    |
| `cad_mxn`   | CAD/MXN            | fx             | currency_per_currency | FALSE | yahoo | (none for now)    |

`default_method_id` for every FX and rate metric: `yesterday_spot` (legal default for valuation).
`default_method_id` for INPC and UDIS: `spot` (they don't typically need previous-day mechanics).

---

## 5. Methods (`R/forecasting_methods.R`)

Three deterministic rules for 6.1. Each has the same interface:

```r
# Contract:
# Input:  metric_id, target_date, params (named list), observations (tibble for this metric)
# Output: numeric scalar OR NA
fcs_method_<id> <- function(metric_id, target_date, params, observations) { ... }
```

### 5.1 `fcs_method_spot`

Most recent observation on-or-before `target_date`.

```r
fcs_method_spot <- function(metric_id, target_date, params = list(), observations) {
  past <- observations[!is.na(observations$value) & observations$fecha <= target_date, , drop = FALSE]
  if (!nrow(past)) return(NA_real_)
  past <- past[order(past$fecha, decreasing = TRUE), , drop = FALSE]

  # Resolve observation_type priority: official > scraped > manual_entry
  priorities <- c("official" = 1L, "scraped" = 2L, "manual_entry" = 3L)
  past$prio <- priorities[past$observation_type %||% "scraped"] %||% 99L
  past <- past[order(past$prio, decreasing = FALSE,
                       past$fecha, decreasing = TRUE), , drop = FALSE]
  past$value[1]
}
```

### 5.2 `fcs_method_yesterday_spot`

Observation as of (`target_date` − 1 business day). Mexican business days for legal metrics; calendar days otherwise.

```r
fcs_method_yesterday_spot <- function(metric_id, target_date, params = list(), observations) {
  # n_days_back defaults to 1, can be overridden via params for the n-days-ago variants
  n <- as.integer(params$n_days_back %||% 1L)

  # Walk back n business days from target_date.
  # For Mexican legal metrics, use Mexico's business calendar.
  # Reuse existing holidays/policy_engine helpers from R/policy_engine.R if available.
  d <- target_date
  steps_taken <- 0L
  while (steps_taken < n) {
    d <- d - 1L
    if (.is_business_day_mx(d)) steps_taken <- steps_taken + 1L
  }

  # Now find the observation on `d`. If no observation exactly on `d` (e.g., late
  # publication), fall back to most-recent-on-or-before `d`.
  fcs_method_spot(metric_id, d, list(), observations)
}
```

`.is_business_day_mx` is a small helper that consults Mexican holidays. Reuse `policy_engine.R`'s holiday catalog if it has one; otherwise build a minimal version that returns TRUE for Mon-Fri and excludes known fixed Mexican holidays. The full holiday list is a deferred concern — for 6.1, weekday-only is acceptable, with a known-holiday list as a stretch.

### 5.3 `fcs_method_manual_curve`

User-defined forecast curve for future dates. Past dates fall back to spot.

```r
fcs_method_manual_curve <- function(metric_id, target_date, params = list(),
                                       observations) {
  # subscription_id must be in params (passed by the resolver)
  sub_id <- params$subscription_id
  if (is.null(sub_id)) return(NA_real_)

  if (target_date <= Sys.Date()) {
    # Past: use observations
    return(fcs_method_spot(metric_id, target_date, list(), observations))
  }

  # Future: look up in forecasting_manual_curves
  curves <- load_forecasting_manual_curves()
  match <- curves[curves$subscription_id == sub_id & curves$fecha == target_date, , drop = FALSE]
  if (nrow(match)) return(match$value[1])

  # No exact match: interpolate between nearest curve points (linear)
  before <- curves[curves$subscription_id == sub_id & curves$fecha <= target_date, , drop = FALSE]
  after  <- curves[curves$subscription_id == sub_id & curves$fecha >= target_date, , drop = FALSE]
  if (!nrow(before) && !nrow(after)) return(NA_real_)
  if (!nrow(before)) return(after$value[order(after$fecha)][1])
  if (!nrow(after))  return(before$value[order(before$fecha, decreasing = TRUE)][1])

  b <- before[order(before$fecha, decreasing = TRUE), , drop = FALSE][1, ]
  a <- after[order(after$fecha), , drop = FALSE][1, ]
  if (a$fecha == b$fecha) return(a$value)

  # Linear interpolation
  frac <- as.numeric(target_date - b$fecha) / as.numeric(a$fecha - b$fecha)
  b$value + frac * (a$value - b$value)
}
```

Manual curve is the escape hatch: when no statistical model fits, the user types in their expected values and the resolver uses them.

---

## 6. Subscription resolution (`R/forecasting_resolve.R`)

This file replaces the Stage 0 stub. Same public signature; subscription-aware resolution.

```r
#' Get an estimate for a metric at a specific date
#'
#' Backward-compatible with Stage 0's signature. New behavior: uses subscription
#' resolution when consumer_id is provided.
#'
#' @param metric Metric ID (e.g., "usd_mxn") OR legacy Stage 0 metric (e.g., "fx_usd_mxn")
#' @param fecha Date for the estimate
#' @param method Optional method override; if NULL, uses the subscription resolver
#' @param consumer_type Optional consumer type for subscription resolution
#' @param consumer_id Optional consumer id for subscription resolution
#'
#' @return numeric scalar; NA if no estimate available
forecasting_get_estimate <- function(metric, fecha,
                                       method = NULL,
                                       consumer_type = NULL,
                                       consumer_id = NULL) {
  # 1. Normalize legacy metric names ("fx_usd_mxn" -> "usd_mxn", etc.)
  metric_id <- .normalize_legacy_metric_id(metric)

  # 2. Resolve the method:
  #    a) explicit method param wins;
  #    b) liability-specific subscription;
  #    c) global default subscription;
  #    d) metric's default_method_id.
  resolved <- .resolve_method(metric_id, method, consumer_type, consumer_id)
  if (is.null(resolved)) return(NA_real_)

  # 3. Load observations for this metric.
  obs_all <- shared$forecasting_series_observations_db() %||% load_forecasting_series_observations()
  obs <- obs_all[obs_all$metric_id == metric_id, , drop = FALSE]

  # 4. Apply the method.
  apply_fn <- get(resolved$method$apply_fn, envir = globalenv(), inherits = TRUE)
  apply_fn(metric_id = metric_id,
           target_date = fecha,
           params = c(resolved$params, list(subscription_id = resolved$subscription_id)),
           observations = obs)
}

.resolve_method <- function(metric_id, method_override, consumer_type, consumer_id) {
  methods_catalog <- load_forecasting_methods()

  # a) explicit override
  if (!is.null(method_override) && nzchar(method_override)) {
    m <- methods_catalog[methods_catalog$method_id == method_override, , drop = FALSE]
    if (nrow(m)) {
      return(list(method = m[1, ], params = list(), subscription_id = NA_character_))
    }
  }

  subs <- load_forecasting_subscriptions() %>% dplyr::filter(active)

  # b) consumer-specific
  if (!is.null(consumer_type) && !is.null(consumer_id) &&
      nzchar(consumer_type) && nzchar(consumer_id)) {
    s <- subs[subs$consumer_type == consumer_type &
              subs$consumer_id   == consumer_id &
              subs$metric_id     == metric_id, , drop = FALSE]
    if (nrow(s)) {
      m <- methods_catalog[methods_catalog$method_id == s$method_id[1], , drop = FALSE]
      if (nrow(m)) {
        return(list(method = m[1, ],
                    params = s$method_params[[1]] %||% list(),
                    subscription_id = s$subscription_id[1]))
      }
    }
  }

  # c) global default
  s <- subs[subs$consumer_type == "global_default" & subs$metric_id == metric_id, , drop = FALSE]
  if (nrow(s)) {
    m <- methods_catalog[methods_catalog$method_id == s$method_id[1], , drop = FALSE]
    if (nrow(m)) {
      return(list(method = m[1, ],
                  params = s$method_params[[1]] %||% list(),
                  subscription_id = s$subscription_id[1]))
    }
  }

  # d) metric default
  metric_row <- load_forecasting_metrics()
  metric_row <- metric_row[metric_row$metric_id == metric_id, , drop = FALSE]
  if (!nrow(metric_row)) return(NULL)
  m <- methods_catalog[methods_catalog$method_id == metric_row$default_method_id[1], , drop = FALSE]
  if (!nrow(m)) return(NULL)
  list(method = m[1, ], params = list(), subscription_id = NA_character_)
}
```

### 6.1 Backward compatibility for Pasivos

Pasivos's `pasivos_recompute_pago_from_cotizado()` currently calls `forecasting_get_estimate(metric, fecha)` where `metric` is `"fx_<cot>_<pag>"`. The `.normalize_legacy_metric_id` helper translates:

- `"fx_usd_mxn"` → `"usd_mxn"`
- `"sofr"`       → `"sofr"`  (already matches)
- `"tiie28"`     → `"tiie28"`
- etc.

No Pasivos code change needed in 6.1. The translation table covers all metric IDs Pasivos currently uses.

Pasivos liabilities will gain *liability-specific subscriptions* in 6.2 when we expose the subscription editor in the wizard. For 6.1, Pasivos uses global defaults via the metric_default fallback path. This means: BAJIO loans will pick up `yesterday_spot` for TIIE28 as the global default behavior — which is exactly what the legal previous-day rule requires.

---

## 7. Thin UI (`R/forecasting_module.R`)

A new top-level tab "Forecasting" alongside the other modules. Three sub-tabs.

### 7.1 Sub-tab: Series viewer

Pick a metric → see a line chart of its observations + a small summary card.

```
[Metric: USD/MXN FIX ▼]  [Source: banxico]  [Last fetch: 2026-05-11 06:00 ✓]   [Refresh now]
─────────────────────────────────────────────────────────────────────────────
[                                                                            ]
[                                line chart (recent 90d default)             ]
[                                                                            ]
─────────────────────────────────────────────────────────────────────────────
Latest value: 19.0847   on 2026-05-10
30d avg:      18.9521
30d range:    18.78 — 19.21
Number of obs: 21
```

Chart: simple `ggplot2 + plotly` or `dygraphs` line. No indicators overlaid in 6.1 (that's 6.5).

Refresh button calls `fcs_fetch_metric(metric_id, date_from = Sys.Date() - 30, triggered_by = "manual_ui")` and saves the resulting rows to `forecasting_series_observations` (with dedup). Capability gate: `forecasting.refresh_metric`.

### 7.2 Sub-tab: Methods & subscriptions

Two side-by-side panels.

**Left panel — Methods catalog:** read-only list of installed methods with descriptions. (Method authoring comes in 6.3.)

**Right panel — Subscriptions:** a `DT` table of all active subscriptions:

| Consumer type           | Consumer        | Metric    | Method            | Actions |
|-------------------------|------------------|-----------|--------------------|---------|
| global_default          | —                | usd_mxn   | yesterday_spot     | Edit ⏸  |
| global_default          | —                | tiie28    | yesterday_spot     | Edit ⏸  |
| pasivos_liability       | BAJIO 11778274   | tiie28    | spot               | Edit ⏸  |
| pasivos_liability       | LAND ROVER (..)  | usd_mxn   | yesterday_spot     | Edit ⏸  |

"Edit" opens a modal:

```
Editar suscripción
────────────────────
Consumer:  BAJIO 11778274  (pasivos_liability)
Metric:    TIIE 28d

Método actual: spot
Método nuevo: [yesterday_spot ▼]

[Si manual_curve seleccionado]:
  Valores futuros:
  Fecha          Valor
  2026-06-15     [11.50]   ✕
  2026-09-15     [11.75]   ✕
  [+ Agregar fecha]
────────────────────
[Cancelar]                       [Guardar]
```

On save: update the subscription row; if method changed, run a recompute trigger for the consumer (see §7.4).

A button "+ Suscripción global" creates new global-default subscriptions for metrics that don't have one yet.

### 7.3 Sub-tab: Fetch log

A `DT` table of the last 200 fetches with metric, source, status, duration, rows added, error message. Diagnostic tool.

Top of the panel: a "Fetch all active metrics now" button (capability `forecasting.refresh_all`).

### 7.4 Recompute trigger

When a subscription is changed (method swap, manual curve edit, etc.), Pasivos provisions subscribed via that subscription should refresh. For 6.1, the simplest reliable approach:

1. On subscription save, identify consumers affected:
   - For `consumer_type = "pasivos_liability"`: the specific liability.
   - For `consumer_type = "global_default"`: all liabilities whose metric_id matches AND who don't have a specific override.
2. For each affected liability, call `pasivos_reconcile_provisions(mode = "rate_update")` (Stage 1 §2.6) using freshly generated provisions.
3. Surface conflicts via the existing edit-confirmation panel.

This reuses the entire Stage 4 edit-propagation machinery. No new code in Pasivos.

Note: this can be a lot of work if a global subscription changes (could affect dozens of liabilities). Show a progress toast: "Recalculando provisiones para N pasivos…"

Capability gate: `forecasting.edit_subscription`.

---

## 8. Tests

### 8.1 `tests/test_forecasting_schemas.R`

- Every `.schema_forecasting_*()` produces correct columns + types.
- Round-trip persistence for each (write 3 rows, read, compare).
- List-column round-trip for `forecasting_metrics` (`fetch_params`, `fallback_source_ids`).

### 8.2 `tests/test_forecasting_methods.R`

For each of the three methods:

- **Spot:**
  - Empty observations → NA.
  - Observations only after `target_date` → NA.
  - One observation on `target_date` → that value.
  - Multiple observations same date, different types → `official` wins over `scraped` wins over `manual_entry`.
- **Yesterday-spot:**
  - `n_days_back = 1`, observation on `(target_date - 1)` → that value.
  - No business-day observation in last 7 days → falls back to most-recent prior.
  - Across a Mexican holiday (e.g., Sept 16): yesterday is the previous *business* day, not just `target_date - 1`.
- **Manual curve:**
  - Subscription has no curve entries → NA for future, spot for past.
  - Exact date match → returns that value.
  - Between two curve points → linear interpolation correct.
  - Past target with no observation but curve has past entries → returns spot, not curve.

### 8.3 `tests/test_forecasting_resolve.R`

- Explicit method override wins over all subscriptions.
- Consumer-specific subscription wins over global_default.
- Global_default wins over metric.default_method_id.
- Metric.default_method_id is final fallback.
- Inactive subscriptions are ignored.
- Legacy metric name `fx_usd_mxn` resolves to `usd_mxn`.

### 8.4 `tests/test_forecasting_sources.R`

For each of the three real adapters (Banxico, Yahoo, FRED), use HTTP mocking:

- Successful response → ok status, parsed rows match schema.
- 4xx HTTP → http_error status, no rows.
- Empty response → no_data status, no rows.
- Timeout → timeout status.
- Malformed JSON → parse_error status.

For dispatcher:

- Primary succeeds → returns primary rows, no fallback called.
- Primary fails, legal metric → returns empty, fallback NOT called.
- Primary fails, non-legal metric, fallback succeeds → returns fallback rows.
- All sources fail → empty rows, fetch log has all attempts.

### 8.5 Integration tests

Synthesize a small `forecasting_series_observations` store. Create a Pasivos liability subscribed (globally) to `yesterday_spot` for TIIE28. Generate provisions across a Mexican holiday weekend. Verify the resulting `componente_interes` uses TIIE28 from the appropriate business day.

This is the most important test: it confirms Pasivos and Forecasting actually integrate end-to-end.

### 8.6 Manual smoke checklist

1. Open Forecasting tab. Confirm metric registry shows the 13 seeded metrics.
2. Click Series viewer for USD/MXN. Confirm the chart renders with historical data (assuming Banxico fetch worked — see §9 for credentials).
3. Click "Refresh now". Confirm new observation rows appear in the chart, fetch log shows the attempt.
4. Open Subscriptions sub-tab. Confirm global_default subscriptions exist for all 13 metrics.
5. Change one global_default's method (e.g., `usd_mxn` from yesterday_spot to spot). Confirm subscription saves and the recompute toast appears for affected liabilities.
6. Verify a BAJIO loan's next interest installment recomputes with the new method.

---

## 9. Configuration / credentials

The Banxico and FRED adapters need API tokens. **These are not committed to code.** Add to the deployment environment:

- `BANXICO_TOKEN` — register free at https://www.banxico.org.mx/SieAPIRest/service/v1/token
- `FRED_API_KEY` — register free at https://fred.stlouisfed.org/docs/api/api_key.html

Yahoo via quantmod needs no key.

Document this in a new `docs/forecasting_setup.md` file:

```markdown
# Forecasting module — environment setup

## Required environment variables

- `BANXICO_TOKEN` — Banxico SIE API token (free registration)
- `FRED_API_KEY`  — FRED API key (free registration)

On shinyapps.io: set via the Dashboard → app settings → environment variables.

Without these, fetches for legal metrics return errors. The module still functions
(historical observations and manual_curve methods work) but no new data is pulled.
```

---

## 10. Stage 6.1 completion criteria

1. All schema, persistence, methods, resolver, and sources tests green.
2. The integration test (§8.5) passes end-to-end.
3. Manual smoke checklist runs cleanly.
4. Pasivos `forecasting_get_estimate` calls continue to work (no regression in Stage 0-4.5 tests).
5. The 13 seeded metrics fetch successfully from their primary sources (assuming API tokens are configured).
6. The Forecasting tab is usable for picking subscriptions and viewing series.
7. Hygiene grep clean: `grep -n "isTRUE(" R/forecasting_*.R`.

---

## 11. What's NOT in 6.1

- **Statistical methods** — ARIMA, ETS, GARCH, etc. Stage 6.3.
- **Indicators UI** — moving averages, Bollinger, RSI display. Stage 6.5.
- **Sandbox for exploratory analysis** — the registered-method/exploratory-method distinction is architecturally present (only catalog methods are subscribable) but the sandbox UI is 6.5.
- **Cross-source pair comparison / arbitrage view** — Stage 6.4 or later.
- **Scheduled fetches** — fetches in 6.1 are manual via the UI button. Cron-style scheduling is a deployment concern, addressed when the soak surfaces the need.
- **Indicators registry implementations** — empty registry exists.
- **Derivatives sub-module** — duration, Macaulay, option pricing, etc. Stage 6.6+.
- **OCC/CAPEX module** — separate module, not Forecasting. Future.
- **Modifiers UI in Pasivos** — Stage 7.

---

## 12. Hand-off to next stages

When 6.1 is green:

- **6.2** Subscription editor in Pasivos wizard (per-liability override of metric methods, with manual curve entry).
- **6.3** Statistical methods (ARIMA initially, then expand based on need).
- **6.4** Cross-source comparison view.
- **6.5** Indicators registry implementation + sandbox.
- **6.6** Derivatives sub-module.

The order beyond 6.2 will be re-prioritized based on actual usage. The architect (me) wants soak data before committing to that order.

---

**End of Stage 6.1 specification.**
