# =============================================================================
# R/forecasting_metric_registry.R
# Seed data for metrics and sources catalogs.
# Called on first app start if the stores are empty.
# =============================================================================

.seed_forecasting_sources <- function() {
  tibble::tibble(
    source_id          = c("banxico", "yahoo", "fred", "inegi", "manual", "ecb", "nyfed"),
    label              = c("Banxico SIE", "Yahoo Finance", "FRED (St. Louis Fed)",
                           "INEGI", "Entrada manual", "BCE (Banco Central Europeo)",
                           "Fed de Nueva York"),
    adapter_fn         = c("fcs_fetch_banxico", "fcs_fetch_yahoo", "fcs_fetch_fred",
                           "fcs_fetch_inegi",   "fcs_fetch_manual", "fcs_fetch_ecb",
                           "fcs_fetch_nyfed"),
    is_official        = c(TRUE,  FALSE, TRUE,  TRUE,  FALSE, TRUE, TRUE),
    base_url           = c("https://www.banxico.org.mx/SieAPIRest/service/v1/",
                           "https://finance.yahoo.com",
                           "https://api.stlouisfed.org/fred/",
                           "https://www.inegi.org.mx/",
                           NA_character_,
                           "https://data-api.ecb.europa.eu",
                           "https://markets.newyorkfed.org"),
    rate_limit_per_min = c(30L, 60L, 120L, 10L, 0L, 60L, 60L),
    api_key_env_var    = c("BANXICO_TOKEN", NA_character_, "FRED_API_KEY",
                           NA_character_, NA_character_, NA_character_, NA_character_),
    # yahoo (T116 F3): retired, never called again -- an unofficial source
    # without a licence. The row stays: old observations still name it.
    active             = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
    notes              = c(NA_character_, FCS_YAHOO_RETIRADA, NA_character_,
                           "INEGI adapter not implemented in 6.1",
                           NA_character_, NA_character_, NA_character_)
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

# One row per metric. fetch_params carries the keys of EVERY source the
# metric may use (primary and fallbacks): each adapter reads its own
# (banxico/fred: serie; ecb: flujo, llave, divide; nyfed: tipo, tasa).
# T116 F3: the six that Yahoo gave now come from Banxico SIE (pesos per unit,
# the same series as fx_divisas()) or the ECB (dollars per euro / pound);
# fed_funds, estr and cetes28 are new. Confirmed by the Coordinador's
# read-only script, 2026-11-27.
.FCS_METRICAS_SEMILLA <- list(
  #        id           label          category         unit                     legal  primary    fallbacks  fetch_params                                                    method
  list("usd_mxn",   "USD/MXN FIX", "fx",            "currency_per_currency", TRUE,  "banxico", list(),    list(serie = "SF43718"),                                         "yesterday_spot"),
  list("tiie28",    "TIIE 28d",    "interest_rate", "percent_annual",        TRUE,  "banxico", list(),    list(serie = "SF43783"),                                         "yesterday_spot"),
  list("tiie91",    "TIIE 91d",    "interest_rate", "percent_annual",        TRUE,  "banxico", list(),    list(serie = "SF43878"),                                         "yesterday_spot"),
  list("tiie182",   "TIIE 182d",   "interest_rate", "percent_annual",        TRUE,  "banxico", list(),    list(serie = "SF111916"),                                        "yesterday_spot"),
  list("inpc",      "INPC",        "inflation",     "index",                 TRUE,  "banxico", list(),    list(serie = "SP1"),                                             "spot"),
  list("udis",      "UDIS",        "inflation",     "currency_per_currency", TRUE,  "banxico", list(),    list(serie = "SP68257"),                                         "spot"),
  list("sofr",      "SOFR",        "interest_rate", "percent_annual",        FALSE, "fred",    list("nyfed"), list(serie = "SOFR", tipo = "secured", tasa = "sofr"),       "yesterday_spot"),
  list("prime",     "PRIME",       "interest_rate", "percent_annual",        FALSE, "fred",    list(),    list(serie = "DPRIME"),                                          "yesterday_spot"),
  list("eur_usd",   "EUR/USD",     "fx",            "currency_per_currency", FALSE, "ecb",     list(),    list(flujo = "EXR", llave = "D.USD.EUR.SP00.A"),                 "yesterday_spot"),
  list("eur_mxn",   "EUR/MXN",     "fx",            "currency_per_currency", FALSE, "banxico", list(),    list(serie = "SF46410"),                                         "yesterday_spot"),
  list("gbp_usd",   "GBP/USD",     "fx",            "currency_per_currency", FALSE, "ecb",     list(),    list(flujo = "EXR", llave = "D.USD.EUR.SP00.A", divide = "D.GBP.EUR.SP00.A"), "yesterday_spot"),
  list("gbp_mxn",   "GBP/MXN",     "fx",            "currency_per_currency", FALSE, "banxico", list(),    list(serie = "SF46407"),                                         "yesterday_spot"),
  list("jpy_mxn",   "JPY/MXN",     "fx",            "currency_per_currency", FALSE, "banxico", list(),    list(serie = "SF46406"),                                         "yesterday_spot"),
  list("cad_mxn",   "CAD/MXN",     "fx",            "currency_per_currency", FALSE, "banxico", list(),    list(serie = "SF60632"),                                         "yesterday_spot"),
  list("fed_funds", "EFFR",        "interest_rate", "percent_annual",        FALSE, "nyfed",   list("fred"), list(serie = "EFFR", tipo = "unsecured", tasa = "effr"),      "yesterday_spot"),
  list("estr",      "€STR",        "interest_rate", "percent_annual",        FALSE, "ecb",     list(),    list(flujo = "EST", llave = "B.EU000A2X2A25.WT"),                "yesterday_spot"),
  list("cetes28",   "CETES 28d",   "interest_rate", "percent_annual",        FALSE, "banxico", list(),    list(serie = "SF43936"),                                         "spot")
)

.seed_forecasting_metrics <- function() {
  now <- Sys.time()
  col <- function(k) lapply(.FCS_METRICAS_SEMILLA, `[[`, k)
  n <- length(.FCS_METRICAS_SEMILLA)
  tibble::tibble(
    metric_id           = unlist(col(1)),
    label               = unlist(col(2)),
    category            = unlist(col(3)),
    unit                = unlist(col(4)),
    is_legal_metric     = unlist(col(5)),
    primary_source_id   = unlist(col(6)),
    fallback_source_ids = col(7),
    fetch_params        = col(8),
    default_method_id   = unlist(col(9)),
    active              = rep(TRUE, n),
    created_at          = rep(now, n),
    notes               = rep(NA_character_, n)
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

# T63: adds the seed's metrics this client's catalog lacks (by metric_id), and a
# global-default subscription for each one that has none. Never changes or
# removes a metric or a subscription that is there. Strict: a failed read
# stop()s before anything is written. Returns the metric_ids it added.
forecasting_completar_catalogo <- function(client_id) {
  semilla <- .seed_forecasting_metrics()
  nuevas <- NULL
  # T72-C: a system write, logged as such; the metric_ids added go in the
  # entry's difference. An unchanged catalog is not written, nor logged.
  ev <- if (exists("evento_sistema", mode = "function"))
    evento_sistema("forecasting", "sembrar_catalogo", "Catálogo de pronósticos: se agregaron métricas nuevas de la siembra")
  modificar_forecasting_metrics(client_id, evento = ev, function(m) {
    if (!nrow(m)) return(m)          # an empty catalog is seeded whole above, never here
    falta <- semilla[!semilla$metric_id %in% m$metric_id, , drop = FALSE]
    if (!nrow(falta)) return(m)
    nuevas <<- falta
    dplyr::bind_rows(m, falta)
  })
  if (is.null(nuevas)) return(character())
  modificar_forecasting_subscriptions(client_id, evento = ev, function(subs) {
    tiene <- subs$metric_id[subs$consumer_type %in% "global_default"]
    falta <- nuevas[!nuevas$metric_id %in% tiene, , drop = FALSE]
    if (!nrow(subs) || !nrow(falta)) return(subs)
    dplyr::bind_rows(subs, .seed_forecasting_global_subscriptions(falta))
  })
  nuevas$metric_id
}

# ── T116 F3: Yahoo retired ───────────────────────────────────────────────────
# Yahoo Finance was an unofficial source without a licence; nothing shown to a
# client may come from it. Three layers:
#   1. forecasting_completar_fuentes(): the deployment's sources table gets the
#      new sources (ecb, nyfed) and yahoo becomes inactive -- .try_fetch()
#      never calls an inactive source, even for a metric that still names it.
#      The row stays: old observations name it.
#   2. forecasting_migrar_yahoo(client_id): each metric still exactly as the
#      old seed left it (yahoo + its ticker) moves to its new source. One the
#      user edited is not touched: it stays without an active source, and
#      Forecasting says so (forecasting_metricas_sin_fuente()).
#   3. R/forecasting_source_yahoo.R is gone.
FCS_YAHOO_RETIRADA <- "Retirada (T116 F3): fuente no oficial y sin licencia; ya no se consulta"
.FCS_YAHOO_SEMILLA <- c(eur_usd = "EURUSD=X", eur_mxn = "EURMXN=X", gbp_usd = "GBPUSD=X",
                        gbp_mxn = "GBPMXN=X", jpy_mxn = "JPYMXN=X", cad_mxn = "CADMXN=X")

# The deployment's sources table: adds the seed's missing sources and retires
# yahoo. Strict read (a failed one stop()s, nothing written); an unchanged
# table is neither written nor logged. Returns the source_ids it changed.
forecasting_completar_fuentes <- function() {
  semilla <- .seed_forecasting_sources()
  cambiadas <- character()
  ev <- if (exists("evento_sistema", mode = "function"))
    evento_sistema("forecasting", "sembrar_catalogo", "Catálogo de fuentes: BCE y Fed de NY agregadas, Yahoo retirada")
  modificar_forecasting_sources(evento = ev, function(f) {
    if (!nrow(f)) return(f)              # an empty table is seeded whole elsewhere
    falta <- semilla[!semilla$source_id %in% f$source_id, , drop = FALSE]
    y <- which(f$source_id %in% "yahoo" & !(f$active %in% FALSE))
    if (!nrow(falta) && !length(y)) return(f)
    if (length(y)) { f$active[y] <- FALSE; f$notes[y] <- FCS_YAHOO_RETIRADA }
    cambiadas <<- c(falta$source_id, if (length(y)) "yahoo")
    dplyr::bind_rows(f, falta)
  })
  cambiadas
}

# TRUE for each metric row still exactly as the old seed left it on Yahoo.
.fcs_es_yahoo_de_semilla <- function(m) {
  tk <- vapply(seq_len(nrow(m)), function(i) {
    fp <- m$fetch_params[[i]]
    if (is.list(fp) && length(fp) && is.list(fp[[1]])) fp <- fp[[1]]
    as.character((if (is.list(fp)) fp$ticker else NULL) %||% NA_character_)[1]
  }, character(1))
  m$primary_source_id %in% "yahoo" & m$metric_id %in% names(.FCS_YAHOO_SEMILLA) &
    !is.na(tk) & tk == unname(.FCS_YAHOO_SEMILLA[m$metric_id])
}

# The client's catalog: each metric still as the old seed left it on Yahoo
# takes the new seed's source, fallbacks and fetch_params. Strict read; an
# unchanged catalog is neither written nor logged. Idempotent. Returns the
# metric_ids it moved.
forecasting_migrar_yahoo <- function(client_id) {
  semilla <- .seed_forecasting_metrics()
  movidas <- character()
  ev <- if (exists("evento_sistema", mode = "function"))
    evento_sistema("forecasting", "sembrar_catalogo", "Catálogo de pronósticos: métricas de Yahoo pasan a Banxico o al BCE")
  modificar_forecasting_metrics(client_id, evento = ev, function(m) {
    if (!nrow(m)) return(m)
    i <- which(.fcs_es_yahoo_de_semilla(m))
    if (!length(i)) return(m)
    for (k in i) {
      s <- semilla[semilla$metric_id == m$metric_id[k], , drop = FALSE]
      m$primary_source_id[k]   <- s$primary_source_id
      m$fallback_source_ids[k] <- s$fallback_source_ids
      m$fetch_params[k]        <- s$fetch_params
      m$notes[k]               <- "T116 F3: antes Yahoo"
    }
    movidas <<- m$metric_id[i]
    m
  })
  movidas
}

# The client's active metrics whose primary source and every fallback are
# inactive or unknown (a metric the user edited on Yahoo, after its
# retirement): Forecasting says so, never silently. data.frame(metric_id,
# label, primary_source_id).
forecasting_metricas_sin_fuente <- function(metrics, sources) {
  vacio <- data.frame(metric_id = character(), label = character(), primary_source_id = character(),
                      stringsAsFactors = FALSE)
  if (!is.data.frame(metrics) || !nrow(metrics)) return(vacio)
  activas <- if (is.data.frame(sources)) sources$source_id[sources$active %in% TRUE] else character()
  sin <- vapply(seq_len(nrow(metrics)), function(i) {
    if (!isTRUE(metrics$active[i])) return(FALSE)
    fb <- metrics$fallback_source_ids[[i]]
    if (is.list(fb) && length(fb) && is.list(fb[[1]])) fb <- fb[[1]]
    !any(c(metrics$primary_source_id[i], unlist(fb)) %in% activas)
  }, logical(1))
  out <- metrics[sin, c("metric_id", "label", "primary_source_id"), drop = FALSE]
  as.data.frame(out, stringsAsFactors = FALSE)
}

# Called during app startup if stores are empty; idempotent.
# client_id (2026-09-25): metrics and subscriptions are TENANT-scoped -- each
# client has its own forecasting_metrics.rds / forecasting_subscriptions.rds.
# They used to be called with no client_id at all, which on the shared
# deployment resolved to hd-admin's own folder: every client's first run
# read, and seeded, the control plane's catalog instead of its own. sources /
# methods / indicators below stay deployment-level on purpose -- they are
# static definitions shared by every tenant, not per-client data.
forecasting_seed_if_empty <- function(client_id = NULL) {
  # T72-C: each write is a system event (it used to be one hand-written
  # "seed_catalog" entry after them all); nothing written, nothing logged.
  ev <- function(que) if (exists("evento_sistema", mode = "function"))
    evento_sistema("forecasting", "sembrar_catalogo", paste("Catálogo de pronósticos:", que))

  metrics_db <- load_forecasting_metrics(client_id = client_id)
  if (!nrow(metrics_db)) {
    message("[forecasting] seeding metrics catalog")
    save_forecasting_metrics(.seed_forecasting_metrics(), client_id = client_id, evento = ev("métricas iniciales"))
    metrics_db <- load_forecasting_metrics(client_id = client_id)
  }

  sources_db <- load_forecasting_sources()
  if (!nrow(sources_db)) {
    message("[forecasting] seeding sources catalog")
    save_forecasting_sources(.seed_forecasting_sources(), evento = ev("fuentes iniciales"))
  }

  methods_db <- load_forecasting_methods()
  if (!nrow(methods_db)) {
    message("[forecasting] seeding methods catalog")
    save_forecasting_methods(.seed_forecasting_methods(), evento = ev("métodos iniciales"))
  }

  # Seeded only on a table read as really empty (T26): a failed read stop()s
  # here -- it used to look empty and the defaults were written over the
  # client's subscriptions.
  modificar_forecasting_subscriptions(client_id, evento = ev("suscripciones iniciales"), function(subs_db) {
    if (nrow(subs_db)) return(subs_db)
    message("[forecasting] seeding global-default subscriptions")
    .seed_forecasting_global_subscriptions(metrics_db)
  })

  # T63: a metric the seed has and this client's catalog lacks (PRIME, for a
  # client seeded before it existed) is added, with its global-default
  # subscription. Only added: a metric already there -- the user's edits
  # too -- is never changed or removed. Strict re-read right before each
  # write; a failed read writes nothing. Idempotent.
  tryCatch(forecasting_completar_catalogo(client_id), error = function(e) {
    message("[forecasting] no se pudo completar el catálogo"); character()
  })
  # T116 F3: Yahoo retired -- the sources table, then this client's metrics.
  # Same rules: strict reads, written only when something changes.
  tryCatch(forecasting_completar_fuentes(), error = function(e) {
    message("[forecasting] no se pudo completar el catálogo de fuentes"); character()
  })
  tryCatch(forecasting_migrar_yahoo(client_id), error = function(e) {
    message("[forecasting] no se pudo migrar las métricas de Yahoo"); character()
  })

  invisible(NULL)
}
