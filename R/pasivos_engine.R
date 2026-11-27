# =============================================================================
# R/pasivos_engine.R
# Stage 1: Provision engine — pure functions only, no reactives, no S3 I/O.
# Callers fetch data and pass it in; this file only computes.
# =============================================================================

# ── §1 Currency guardrails ─────────────────────────────────────────────────────

# THE ONLY WAY to ask "what currency is this paid in"
pasivos_pago_currency <- function(row) {
  cur <- row$moneda_pago
  if (is.null(cur) || is.na(cur) || !nzchar(cur)) {
    stop("[pasivos] row has no moneda_pago — refusing to guess. id=",
         row$id %||% "<unknown>")
  }
  cur
}

# THE ONLY WAY to ask "what is the cash amount that will leave the bank"
pasivos_pago_amount <- function(row) {
  if (!is.na(row$amount_pago_override)) return(row$amount_pago_override)
  row$amount_pago
}

# Recompute moneda_pago amount from cotizado_en via the client's fx_*
# estimate. Returns list(amount_pago = numeric, fx_rate_used = numeric).
# client_id: whose estimates (default: the session's client). Without it the
# read had no tenant, as the variable rates had (tasas variables, 2026-11-21).
pasivos_recompute_pago_from_cotizado <- function(amount_cotizado, cotizado_en,
                                                  moneda_pago, fecha,
                                                  modifiers = list(),
                                                  client_id = pasivos_cliente_sesion()) {
  no_conversion <- is.null(cotizado_en) || is.na(cotizado_en) ||
    !nzchar(cotizado_en) || identical(cotizado_en, moneda_pago)
  if (no_conversion) {
    return(list(amount_pago = amount_cotizado, fx_rate_used = NA_real_))
  }
  metric <- paste0("fx_", tolower(cotizado_en), "_", tolower(moneda_pago))
  rate <- tryCatch(
    forecasting_get_estimate(metric, fecha, client_id = client_id),
    error = function(e) NA_real_
  )
  if (is.na(rate)) {
    warning("[pasivos] FX rate not available for ", metric, " on ", fecha,
            " — amount_pago set to NA")
    return(list(amount_pago = NA_real_, fx_rate_used = NA_real_))
  }
  list(amount_pago = amount_cotizado * rate, fx_rate_used = rate)
}

# ── §2.1 Recurrence expansion ──────────────────────────────────────────────────

# Clamp day to actual days in the given year/month.
.clamp_day <- function(year, month, day) {
  max_day <- lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month)))
  min(as.integer(day), as.integer(max_day))
}

# Build date for year/month with day clamped to month-end.
.ym_date <- function(year, month, day) {
  as.Date(sprintf("%04d-%02d-%02d", year, month, .clamp_day(year, month, day)))
}

# Generate the next n occurrence dates for a recurrence rule, starting on or after `from`.
pasivos_expand_recurrence <- function(recurrence_type, recurrence_params, from, n) {
  from <- as.Date(from)
  n    <- as.integer(n)

  switch(recurrence_type,

    monthly_day = {
      day <- as.integer(recurrence_params$day)
      # Start from the month containing `from`; expand forward
      year0  <- lubridate::year(from)
      month0 <- lubridate::month(from)
      results <- Date(0)
      m <- 0L
      while (length(results) < n) {
        yr <- year0 + (month0 + m - 1L) %/% 12L
        mo <- ((month0 + m - 1L) %% 12L) + 1L
        d  <- .ym_date(yr, mo, day)
        if (d >= from) results <- c(results, d)
        m <- m + 1L
      }
      results[seq_len(n)]
    },

    monthly_nth_weekday = {
      nth  <- as.integer(recurrence_params$nth)
      wday <- as.integer(recurrence_params$wday)
      year0  <- lubridate::year(from)
      month0 <- lubridate::month(from)
      results <- Date(0)
      m <- 0L
      while (length(results) < n) {
        yr <- year0 + (month0 + m - 1L) %/% 12L
        mo <- ((month0 + m - 1L) %% 12L) + 1L
        d  <- .nth_weekday(yr, mo, nth, wday)
        if (d >= from) results <- c(results, d)
        m <- m + 1L
      }
      results[seq_len(n)]
    },

    # "Quincenal" = two fixed calendar dates per month (day1/day2), not a
    # rolling 14-day interval — real quincena pay dates don't drift. The old
    # rolling behavior now lives under interval_days below. day2=31 clamps
    # to real month-end via .ym_date().
    biweekly = {
      day1 <- as.integer(recurrence_params$day1 %||% 15L)
      day2 <- as.integer(recurrence_params$day2 %||% 31L)
      year0  <- lubridate::year(from)
      month0 <- lubridate::month(from)
      results <- Date(0)
      m <- 0L
      while (length(results) < n) {
        yr <- year0 + (month0 + m - 1L) %/% 12L
        mo <- ((month0 + m - 1L) %% 12L) + 1L
        cand <- sort(unique(c(.ym_date(yr, mo, day1), .ym_date(yr, mo, day2))))
        results <- c(results, cand[cand >= from])
        m <- m + 1L
      }
      results[seq_len(n)]
    },

    # "Cada N días" — the rolling-interval behavior biweekly used to have,
    # generalized from a hardcoded 14 to any interval.
    interval_days = {
      anchor   <- as.Date(recurrence_params$anchor_date)
      interval <- as.integer(recurrence_params$interval_days %||% 30L)
      delta    <- as.integer(from - anchor)
      k_start  <- ceiling(delta / interval)
      if (k_start < 0L) k_start <- 0L
      results <- Date(0)
      k <- k_start
      while (length(results) < n) {
        d <- anchor + k * interval
        if (d >= from) results <- c(results, d)
        k <- k + 1L
      }
      results[seq_len(n)]
    },

    weekly = {
      wday <- as.integer(recurrence_params$wday)
      wd_from <- lubridate::wday(from, week_start = 1L)
      days_fwd <- (wday - wd_from) %% 7L
      first <- from + days_fwd
      seq.Date(first, by = 7L, length.out = n)
    },

    quarterly = {
      anchor_month <- as.integer(recurrence_params$anchor_month)
      day          <- as.integer(recurrence_params$day)
      # Quarter offsets from anchor_month: 0, 3, 6, 9 months
      year0  <- lubridate::year(from)
      month0 <- lubridate::month(from)
      results <- Date(0)
      q <- 0L
      while (length(results) < n * 8L && length(results) < n) {
        # Generate all quarters for several years
        for (yr_off in 0L:6L) {
          yr <- year0 - 1L + yr_off
          for (q_off in c(0L, 3L, 6L, 9L)) {
            mo <- ((anchor_month - 1L + q_off) %% 12L) + 1L
            extra_yr <- (anchor_month - 1L + q_off) %/% 12L
            d <- .ym_date(yr + extra_yr, mo, day)
            if (d >= from) results <- c(results, d)
          }
        }
        break
      }
      results <- sort(unique(results))
      if (length(results) < n) {
        # extend further if needed
        last_yr <- lubridate::year(results[length(results)])
        for (yr_off in 1L:20L) {
          yr <- last_yr + yr_off
          for (q_off in c(0L, 3L, 6L, 9L)) {
            mo <- ((anchor_month - 1L + q_off) %% 12L) + 1L
            extra_yr <- (anchor_month - 1L + q_off) %/% 12L
            d <- .ym_date(yr + extra_yr, mo, day)
            results <- c(results, d)
          }
          if (length(results) >= n) break
        }
        results <- sort(unique(results))
      }
      results[seq_len(n)]
    },

    # "Bimestral" — same shape as quarterly above, 2-month offsets instead of 3.
    bimestral = {
      anchor_month <- as.integer(recurrence_params$anchor_month)
      day          <- as.integer(recurrence_params$day)
      year0  <- lubridate::year(from)
      results <- Date(0)
      for (yr_off in 0L:6L) {
        yr <- year0 - 1L + yr_off
        for (b_off in c(0L, 2L, 4L, 6L, 8L, 10L)) {
          mo <- ((anchor_month - 1L + b_off) %% 12L) + 1L
          extra_yr <- (anchor_month - 1L + b_off) %/% 12L
          d <- .ym_date(yr + extra_yr, mo, day)
          if (d >= from) results <- c(results, d)
        }
      }
      results <- sort(unique(results))
      if (length(results) < n) {
        last_yr <- lubridate::year(results[length(results)])
        for (yr_off in 1L:20L) {
          yr <- last_yr + yr_off
          for (b_off in c(0L, 2L, 4L, 6L, 8L, 10L)) {
            mo <- ((anchor_month - 1L + b_off) %% 12L) + 1L
            extra_yr <- (anchor_month - 1L + b_off) %/% 12L
            d <- .ym_date(yr + extra_yr, mo, day)
            results <- c(results, d)
          }
          if (length(results) >= n) break
        }
        results <- sort(unique(results))
      }
      results[seq_len(n)]
    },

    yearly = {
      month <- as.integer(recurrence_params$month)
      day   <- as.integer(recurrence_params$day)
      year0 <- lubridate::year(from)
      results <- Date(0)
      yr <- year0
      while (length(results) < n) {
        d <- .ym_date(yr, month, day)
        if (d >= from) results <- c(results, d)
        yr <- yr + 1L
      }
      results[seq_len(n)]
    },

    custom = {
      dates <- sort(as.Date(recurrence_params$dates))
      dates[dates >= from][seq_len(min(n, sum(dates >= from)))]
    },

    stop("[pasivos] unknown recurrence_type: ", recurrence_type)
  )
}

# PV-1 (2026-11-22): the dates of a regular / tarjeta liability counted from
# its fecha_inicio, up to `hasta`: occurrence k is the k-th date on or after
# the start, whatever day it is generated. They used to be counted from the
# day they were generated (window_start), so the same date had another index
# on another day, and editing the liability later moved its pending
# provisions to other months and left months without one (the reconciler
# matches by index). Empty without a fecha_inicio or a recurrence.
pasivos_recurrencia_fechas <- function(liability, hasta) {
  vacio <- as.Date(character(0))
  ini   <- suppressWarnings(as.Date((liability$fecha_inicio %||% NA)[1]))
  hasta <- suppressWarnings(as.Date(hasta))
  tipo  <- (liability$recurrence_type %||% NA_character_)[1]
  if (is.na(ini) || length(hasta) != 1L || is.na(hasta) || hasta < ini || is.na(tipo) || !nzchar(tipo))
    return(vacio)
  params <- liability$recurrence_params[[1]] %||% list()
  n <- 64L
  repeat {
    d <- pasivos_expand_recurrence(tipo, params, ini, n)
    d <- d[!is.na(d)]
    if (!length(d) || length(d) < n || max(d) > hasta || n >= 50000L) break
    n <- n * 4L
  }
  d[d <= hasta]
}

# ── §2.2 Amortization schedule generator ──────────────────────────────────────

# Add k months to base date, snapping to dia_pago, clamping to month-end.
.add_payment_month <- function(base, k, dia_pago) {
  yr  <- lubridate::year(base)
  mo  <- lubridate::month(base)
  total_mo <- mo + as.integer(k)
  new_yr <- yr + (total_mo - 1L) %/% 12L
  new_mo <- ((total_mo - 1L) %% 12L) + 1L
  .ym_date(new_yr, new_mo, dia_pago)
}

# ── T115: day-count conventions ───────────────────────────────────────────────
# How a financiero's period interest comes from its annual rate t:
#   efectiva  t is EFFECTIVE annual: every period pays (1 + t)^(1/m) - 1,
#             whatever its days -- what every fixed rate did before T115;
#   30_360    nominal, 30E/360: months of 30 days, / 360;
#   act_360   nominal, real days / 360 -- what every variable rate did before;
#   act_365   nominal, real days / 365;
#   act_act   nominal, ISDA: each piece of the period over its year's 365/366.
# The days run from payment date to payment date as scheduled (the calculated
# date; a payment moved by a holiday is T116's).
# A liability keeps its own (convencion_dias); NA = the one it had before
# T115, by its rate type (pasivos_convencion()): nothing saved changes. The
# client's default (group_config$convencion_dias) is copied into a NEW
# liability by the wizard, never read live -- changing it moves no saved
# liability, and the variable rates' refresh rewrites nothing for it.
PASIVOS_CONVENCIONES <- c("efectiva", "30_360", "act_360", "act_365", "act_act")
PASIVOS_CONVENCION_OMISION <- "act_360"   # a client that never chose one

pasivos_convencion_valida <- function(x)
  is.character(x) && length(x) == 1L && !is.na(x) && x %in% PASIVOS_CONVENCIONES

pasivos_convencion <- function(liability) {
  cv <- if ("convencion_dias" %in% names(liability)) as.character(liability$convencion_dias[1]) else NA_character_
  if (pasivos_convencion_valida(cv)) return(cv)
  tipo <- if ("tasa_tipo" %in% names(liability)) as.character(liability$tasa_tipo[1]) else NA_character_
  if (!is.na(tipo) && startsWith(tipo, "variable_")) "act_360" else "efectiva"
}

# The client's default for a new liability (group_config$convencion_dias).
pasivos_convencion_cliente <- function(x) if (pasivos_convencion_valida(x)) x else PASIVOS_CONVENCION_OMISION

# The days of the period (desde, hasta] the convention counts: 30E/360 counts
# months of 30; NA for efectiva (its periods have no days).
pasivos_dias_periodo <- function(conv, desde, hasta) {
  desde <- as.Date(desde); hasta <- as.Date(hasta)
  if (identical(conv, "efectiva")) return(NA_real_)
  if (identical(conv, "30_360")) {
    p <- function(d, f) as.integer(format(d, f))
    return(360 * (p(hasta, "%Y") - p(desde, "%Y")) + 30 * (p(hasta, "%m") - p(desde, "%m")) +
             (min(p(hasta, "%d"), 30L) - min(p(desde, "%d"), 30L)))
  }
  as.numeric(hasta - desde)
}

# Act/Act ISDA: each piece of (desde, hasta] over the days of its own year.
.pasivos_fraccion_act_act <- function(desde, hasta) {
  if (hasta <= desde) return(0)
  enero <- function(a) as.Date(sprintf("%d-01-01", a))
  anos <- as.integer(format(desde, "%Y")):as.integer(format(hasta, "%Y"))
  sum(vapply(anos, function(a) {
    ini <- max(desde, enero(a)); fin <- min(hasta, enero(a + 1L))
    max(0, as.numeric(fin - ini)) / as.numeric(enero(a + 1L) - enero(a))
  }, numeric(1)))
}

# A period's rates, annual rate t as a fraction (0.0875), m periods a year:
# r for the level payment, f what the balance pays as interest from desde to
# hasta. The operations keep the pre-T115 order for efectiva and act_360, so
# a saved liability gives exactly the same figures.
pasivos_tasas_convencion <- function(conv, t, m, desde, hasta) {
  switch(conv,
    efectiva = { r <- (1 + t)^(1 / m) - 1; list(r = r, f = r) },
    act_360  = list(r = t / m, f = t * as.numeric(as.Date(hasta) - as.Date(desde)) / 360),
    act_365  = list(r = t / m, f = t * as.numeric(as.Date(hasta) - as.Date(desde)) / 365),
    `30_360` = list(r = t / m, f = t * pasivos_dias_periodo("30_360", desde, hasta) / 360),
    act_act  = list(r = t / m, f = t * .pasivos_fraccion_act_act(as.Date(desde), as.Date(hasta))),
    stop("[pasivos] unknown convencion_dias: ", conv))
}

# The EAR (%) of a nominal annual rate (%) paid m times a year, by the
# convention: Act/360 as T78 (365/360 per year); efectiva is already the EAR.
pasivos_ear_convencion <- function(nominal, m, conv) {
  switch(conv,
    efectiva = nominal,
    act_360  = pasivos_ear_act360(nominal, m),
    100 * ((1 + nominal / 100 / m)^m - 1))
}

# Periods per year for a given payment frequency.
.periods_per_year <- function(frecuencia) {
  switch(frecuencia,
    mensual = 12L,
    anual   = 1L,
    semanal = 52L,
    diaria  = 365L,
    12L
  )
}

# Frequency-aware date advancement: k-th payment from base date.
.add_payment_period <- function(base, k, frecuencia, dia_pago,
                                 dia_semana_pago = NA_integer_,
                                 mes_pago = NA_integer_) {
  switch(frecuencia %||% "mensual",
    mensual = .add_payment_month(base, k, dia_pago),
    anual   = {
      yr  <- lubridate::year(base) + as.integer(k)
      mo  <- if (!is.na(mes_pago)) as.integer(mes_pago) else lubridate::month(base)
      day <- if (!is.na(dia_pago)) as.integer(dia_pago) else lubridate::day(base)
      .ym_date(yr, mo, day)
    },
    semanal = {
      anchor <- base + as.integer(k) * 7L
      if (!is.na(dia_semana_pago)) {
        wd  <- lubridate::wday(anchor, week_start = 1L)
        fwd <- (as.integer(dia_semana_pago) - wd) %% 7L
        anchor + fwd
      } else anchor
    },
    diaria  = base + as.integer(k),
    .add_payment_month(base, k, dia_pago)
  )
}

# Generate amortization schedule for a financiero liability.
# ── Impuestos (T63) ────────────────────────────────────────────────────────────
# A liability's taxes, from a tibble row (list-column) or a draft (plain list):
# list of list(nombre, tipo = "porcentual" | "fijo", valor, base = "pago" |
# "intereses"). Missing, NULL or malformed entries -> none.
pasivos_impuestos_de <- function(liability) {
  v <- tryCatch(liability[["impuestos"]], error = function(e) NULL)
  if (is.null(v) || !length(v)) return(list())
  lst <- if (is.list(v[[1]]) && !is.null(v[[1]][["tipo"]])) v else if (is.list(v[[1]])) v[[1]] else list()
  ok <- vapply(lst, function(t) is.list(t) && isTRUE(t[["tipo"]] %in% c("porcentual", "fijo")) &&
                 is.numeric(suppressWarnings(as.numeric(t[["valor"]]))) &&
                 !is.na(suppressWarnings(as.numeric(t[["valor"]]))) && as.numeric(t[["valor"]]) >= 0, logical(1))
  lst[ok]
}

# The tax of each payment, in the payment's own currency: a percentage of the
# whole payment (base "pago", the default) or of its interest only (base
# "intereses", for loans), or a fixed amount per payment. Vectorized over
# payments; 0 when there are no taxes. Rounded to cents.
pasivos_impuestos_pago <- function(impuestos, total, interes = 0) {
  total <- suppressWarnings(as.numeric(total)); total[is.na(total)] <- 0
  interes <- rep_len(suppressWarnings(as.numeric(interes)), length(total)); interes[is.na(interes)] <- 0
  imps <- pasivos_impuestos_de(list(impuestos = impuestos))
  out <- rep(0, length(total))
  for (t in imps) {
    v <- as.numeric(t[["valor"]])
    out <- out + if (identical(t[["tipo"]], "fijo")) v
                 else (if (identical(t[["base"]], "intereses")) interes else total) * v / 100
  }
  round(out, 2)
}

# today: the client's day (T75b, CM27). Sys.Date() was the server's: on UTC,
# from 18:00 in Mexico a payment due today already counted as historico.
# client_id: whose estimates (SOFR / TIIE28 / Prime) a variable rate reads.
# Without it the read had no tenant: on the shared deployment .s3_key()
# refused it, so a variable rate never followed its index and stayed at
# the rate captured on the day (prod, 2026-11-21: SOFR captured at
# 3.55-3.62, the day's 3.91). Default: the session's client.
# usar_indice = FALSE: a variable rate takes tasa_actual (the wizard's
# preview: the rate just fetched), same convention, no estimates read.
pasivos_generate_schedule <- function(liability, today = hoy_en(), client_id = pasivos_cliente_sesion(),
                                      usar_indice = TRUE) {
  today  <- as.Date(today)
  cat_ok <- !is.null(liability$categoria) && !is.na(liability$categoria) &&
    liability$categoria == "financiero"
  if (!cat_ok) stop("[pasivos] pasivos_generate_schedule requires categoria == 'financiero'")

  metodo   <- liability$metodo_amortizacion
  n_amort  <- as.integer(liability$plazo_meses)   # amortization periods only
  pg       <- as.integer(liability$periodo_gracia %||% 0L)
  if (is.na(pg) || pg < 0L) pg <- 0L
  n        <- n_amort + pg                         # total periods (grace + amortization)
  base         <- as.Date(liability$fecha_inicio)
  dia_pago     <- as.integer(liability$dia_pago)
  tasa_tipo    <- liability$tasa_tipo %||% "fija"
  frecuencia   <- liability$frecuencia_pago %||% "mensual"
  ppy          <- .periods_per_year(frecuencia)
  mes_pago_val <- as.integer(liability$mes_pago %||% NA_integer_)

  # ── Imported schedule wins ─────────────────────────────────────────────────
  imp <- liability$schedule_imported[[1]]
  if (!is.null(imp) && length(imp) > 0 && (is.data.frame(imp) || is.list(imp))) {
    if (is.data.frame(imp)) {
      sched <- imp
    } else {
      sched <- as.data.frame(imp)
    }
    sched$origin      <- "regular"
    sched$historico   <- as.Date(sched$fecha) < today
    sched$re_amortized <- FALSE
    return(sched)
  }
  if (identical(metodo, "custom")) {
    stop("[pasivos] metodo_amortizacion = 'custom' requires schedule_imported to be non-empty")
  }

  principal   <- as.numeric(liability$principal_original)
  tasa        <- as.numeric(liability$tasa_actual)
  spread_bps  <- as.numeric(liability$tasa_spread %||% 0)
  if (is.na(spread_bps)) spread_bps <- 0
  monto_gracia_custom <- as.numeric(liability$monto_gracia %||% NA_real_)  # NA = auto

  # ── A period's rates ───────────────────────────────────────────────────────
  # list(r, f): r is the rate per period the level (French) payment is worked
  # out with; f is what the balance pays as interest in this period.
  #   fija: the captured rate is effective annual, r = f = (1 + t)^(1/m) - 1
  #     (unchanged; Luis decides on the fixed ones apart).
  #   variable (SOFR / TIIE28 / Prime): the market quotes NOMINAL annual,
  #     simple interest Act/360: t = index + spread in points (bps / 100);
  #     f = t * days of the period / 360, r = t / m. The index is the one in
  #     force on the payment date (client's estimates); none -> the rate
  #     captured on the day, the same convention.
  # T115: by the liability's day-count convention (pasivos_convencion(): its
  # own, or NA = the one before T115 -- efectiva for fixed, act_360 for
  # variable -- so the figures of a saved liability do not change).
  es_variable <- startsWith(tasa_tipo, "variable_")
  convencion  <- pasivos_convencion(liability)
  variable_na_warned <- FALSE
  .tasas_periodo <- function(fecha_k, fecha_prev) {
    if (!es_variable)
      return(pasivos_tasas_convencion(convencion, tasa / 100, ppy, fecha_prev, fecha_k))
    est <- if (!isTRUE(usar_indice)) tasa else
      tryCatch(forecasting_get_estimate(pasivos_metrica_tasa(tasa_tipo), fecha_k, client_id = client_id),
               error = function(e) NA_real_)
    if (is.na(est)) {
      if (!variable_na_warned) {
        warning("[pasivos] variable rate NA for some periods — falling back to tasa_actual")
        variable_na_warned <<- TRUE
      }
      est <- tasa
    }
    t <- (est + spread_bps / 100) / 100
    pasivos_tasas_convencion(convencion, t, ppy, fecha_prev, fecha_k)
  }

  # ── Build regular schedule ─────────────────────────────────────────────────
  saldo <- principal
  sched <- vector("list", n)
  fecha_prev <- base

  for (k in seq_len(n)) {
    fecha_k <- .add_payment_period(base, k, frecuencia, dia_pago,
                                    dia_semana_pago = dia_pago,
                                    mes_pago        = mes_pago_val)
    tp      <- .tasas_periodo(fecha_k, fecha_prev)
    r_k     <- tp$r
    f_k     <- tp$f
    dias_k  <- pasivos_dias_periodo(convencion, fecha_prev, fecha_k)   # T115: the table's Días
    fecha_prev <- fecha_k

    if (k <= pg) {
      # ── Grace period ─────────────────────────────────────────────────────────
      # Custom fixed amount overrides auto; auto = interest-only (= $0 at 0% tasa)
      cap_k  <- 0
      int_k  <- if (!is.na(monto_gracia_custom) && monto_gracia_custom >= 0)
                  monto_gracia_custom
                else
                  saldo * f_k
      fees_k   <- 0
      origin_k <- "grace"
    } else {
      # ── Amortization period ──────────────────────────────────────────────────
      k_a <- k - pg            # 1-based index within amortization phase
      rem <- n_amort - k_a + 1L  # remaining amortization periods

      if (identical(metodo, "francesa")) {
        # For variable-rate: recompute level payment using remaining saldo + remaining periods
        if (r_k == 0) {
          pmt <- saldo / rem
        } else {
          pmt <- saldo * r_k * (1 + r_k)^rem / ((1 + r_k)^rem - 1)
        }
        int_k  <- saldo * f_k
        cap_k  <- pmt - int_k
        fees_k <- 0
      } else if (identical(metodo, "alemana")) {
        cap_k  <- principal / n_amort
        int_k  <- saldo * f_k
        fees_k <- 0
      } else if (identical(metodo, "americana")) {
        cap_k  <- if (k_a == n_amort) saldo else 0
        int_k  <- principal * f_k
        fees_k <- 0
      } else {
        stop("[pasivos] unknown metodo_amortizacion: ", metodo)
      }
      origin_k <- "regular"
    }

    # Guard against floating-point overshoot on final period
    if (k == n) cap_k <- saldo
    saldo_post <- max(0, saldo - cap_k)

    sched[[k]] <- list(
      periodo    = k,
      fecha      = fecha_k,
      capital    = cap_k,
      interes    = int_k,
      fees       = fees_k,
      total      = cap_k + int_k + fees_k,
      saldo_post = saldo_post,
      origin     = origin_k,
      historico  = fecha_k < today,
      re_amortized = FALSE,
      dias       = dias_k,          # T115: days the convention counts (NA: efectiva)
      tasa_periodo = f_k * 100      # T115: the rate this period paid, %
    )
    saldo <- saldo_post
  }

  sched_df <- dplyr::bind_rows(lapply(sched, tibble::as_tibble))

  # ── §2.2.3 Half-way-through re-amortization ───────────────────────────────
  saldo_declarado <- as.numeric(liability$saldo_capital)
  if (!is.na(saldo_declarado) && as.Date(liability$fecha_inicio) < today) {
    hist_rows <- sched_df[sched_df$historico, , drop = FALSE]
    if (nrow(hist_rows) > 0) {
      computed_saldo <- hist_rows$saldo_post[nrow(hist_rows)]
      delta <- abs(saldo_declarado - computed_saldo) / principal
      if (delta > 0.05) {
        future_idx <- which(!sched_df$historico)

        # Separate future grace periods from future amortization periods
        grace_future_idx <- future_idx[sched_df$periodo[future_idx] <= pg]
        amort_future_idx <- future_idx[sched_df$periodo[future_idx] >  pg]
        n_rem_amort      <- length(amort_future_idx)
        s_rem            <- saldo_declarado

        # Re-state future grace rows against the declared saldo
        for (idx in grace_future_idx) {
          fecha_k <- sched_df$fecha[idx]
          fecha_prev <- if (sched_df$periodo[idx] <= 1L) base else
                          sched_df$fecha[sched_df$periodo == sched_df$periodo[idx] - 1L][1]
          f_k     <- .tasas_periodo(fecha_k, fecha_prev)$f
          int_k   <- if (!is.na(monto_gracia_custom) && monto_gracia_custom >= 0)
                       monto_gracia_custom
                     else
                       s_rem * f_k
          sched_df$capital[idx]      <- 0
          sched_df$interes[idx]      <- int_k
          sched_df$total[idx]        <- int_k + sched_df$fees[idx]
          sched_df$saldo_post[idx]   <- s_rem
          sched_df$re_amortized[idx] <- TRUE
        }

        # Re-amortize future amortization rows from the declared saldo
        for (i in seq_along(amort_future_idx)) {
          idx     <- amort_future_idx[i]
          fecha_k <- sched_df$fecha[idx]
          fecha_prev <- if (sched_df$periodo[idx] <= 1L) base else
                          sched_df$fecha[sched_df$periodo == sched_df$periodo[idx] - 1L][1]
          tp      <- .tasas_periodo(fecha_k, fecha_prev)
          r_k     <- tp$r
          f_k     <- tp$f

          if (identical(metodo, "francesa")) {
            rem <- n_rem_amort - i + 1L
            if (r_k == 0) pmt <- s_rem / rem else
              pmt <- s_rem * r_k * (1 + r_k)^rem / ((1 + r_k)^rem - 1)
            int_k <- s_rem * f_k
            cap_k <- pmt - int_k
          } else if (identical(metodo, "alemana")) {
            cap_k <- saldo_declarado / n_rem_amort
            int_k <- s_rem * f_k
          } else {  # americana
            cap_k <- if (i == n_rem_amort) s_rem else 0
            int_k <- saldo_declarado * f_k
          }
          if (i == n_rem_amort) cap_k <- s_rem
          s_post <- max(0, s_rem - cap_k)

          sched_df$capital[idx]      <- cap_k
          sched_df$interes[idx]      <- int_k
          sched_df$total[idx]        <- cap_k + int_k + sched_df$fees[idx]
          sched_df$saldo_post[idx]   <- s_post
          sched_df$re_amortized[idx] <- TRUE
          s_rem <- s_post
        }
      }
    }
  }

  # ── §2.2.4 Initial fees ───────────────────────────────────────────────────
  fees_list <- liability$cargos_iniciales[[1]]
  fee_rows  <- tibble::tibble(
    periodo = integer(0), fecha = Date(0), capital = numeric(0),
    interes = numeric(0), fees = numeric(0), total = numeric(0),
    saldo_post = numeric(0), origin = character(0),
    historico = logical(0), re_amortized = logical(0),
    moneda = character(0), fee_desc = character(0)
  )
  if (!is.null(fees_list) && length(fees_list) > 0) {
    fee_rows <- dplyr::bind_rows(lapply(seq_along(fees_list), function(fi) {
      f <- fees_list[[fi]]
      tibble::tibble(
        periodo    = as.integer(-(fi + 1000L)),
        fecha      = as.Date(f$fecha),
        capital    = 0,
        interes    = 0,
        fees       = as.numeric(f$monto),
        total      = as.numeric(f$monto),
        saldo_post = principal,
        origin     = "initial_fee",
        historico  = as.Date(f$fecha) < today,
        re_amortized = FALSE,
        moneda     = f$moneda %||% (liability$moneda_pago[1] %||% NA_character_),
        fee_desc   = f$desc   %||% ""
      )
    }))
  }

  # ── §2.2.5 Residual ───────────────────────────────────────────────────────
  vr <- liability$valor_residual[[1]]
  if (!is.null(vr) && length(vr) > 0) {
    behavior <- vr$behavior
    monto    <- as.numeric(vr$monto)
    last_idx <- max(which(sched_df$origin == "regular"))

    if (identical(behavior, "replace_last")) {
      orig_int  <- sched_df$interes[last_idx]
      orig_fees <- sched_df$fees[last_idx]
      sched_df$total[last_idx]   <- monto
      sched_df$capital[last_idx] <- monto - orig_int - orig_fees
    } else {
      last_fecha <- sched_df$fecha[last_idx]
      res_fecha  <- if (identical(behavior, "separate") && !is.null(vr$fecha) && !is.na(vr$fecha))
                     as.Date(vr$fecha)
                   else last_fecha
      res_row    <- tibble::tibble(
        periodo = -1L, fecha = res_fecha, capital = monto, interes = 0,
        fees = 0, total = monto, saldo_post = 0,
        origin = "residual", historico = res_fecha < today, re_amortized = FALSE
      )
      sched_df <- dplyr::bind_rows(sched_df, res_row)
    }
  }

  # Combine and sort
  out <- dplyr::bind_rows(fee_rows, sched_df) |>
    dplyr::arrange(.data$fecha)
  # T63: with taxes, each scheduled payment's tax and the payment with it (the
  # same rule as the provisions; initial fees carry none). Without taxes the
  # schedule is exactly what it was.
  imps <- pasivos_impuestos_de(liability)
  if (length(imps) && nrow(out)) {
    es_fee <- out$origin %in% "initial_fee"
    out$impuestos  <- ifelse(es_fee, 0, pasivos_impuestos_pago(imps, out$total, out$interes))
    out$total_pago <- out$total + out$impuestos
  }
  out
}

# T70: a regular or tarjeta liability's end date (fecha_vence), or NA (no end).
# A financiero's fecha_vence is its term's end, computed by the wizard.
pasivos_fecha_fin <- function(liability) {
  v <- if ("fecha_vence" %in% names(liability)) liability$fecha_vence[1] else NA
  tryCatch(as.Date(v), error = function(e) as.Date(NA))
}

# T71: the liability's note, or NA when it has none.
pasivos_nota_de <- function(liability) {
  n <- if ("notas" %in% names(liability)) as.character(liability$notas[1]) else NA_character_
  if (!length(n) || is.na(n) || !nzchar(trimws(n))) NA_character_ else trimws(n)
}

# ── T120: a liability's payment policies ──────────────────────────────────────
# A liability carries its own policies (policy_ids: catalog ids in the order
# they apply, comma-joined; NA/"" = none). Only they move its provisions'
# payment date (fecha_efectiva); a Parte's policies never do. The policy moves
# the PAYMENT date, never the interest days (T115: interest runs between the
# calculated dates).
pasivos_ids_politicas <- function(liability) {
  v <- if ("policy_ids" %in% names(liability)) as.character(liability$policy_ids[1]) else NA_character_
  if (!length(v) || is.na(v) || !nzchar(trimws(v))) return(character())
  x <- trimws(strsplit(v, ",", fixed = TRUE)[[1]])
  unique(x[nzchar(x)])
}

# The policies to hand pasivos_generate_provisions(): list of
# list(id, type, params, roll_direction) in the liability's order, from the
# client's catalog; an id the catalog no longer has is left out. The holiday
# cache travels with them (attribute "festivos", festivos_cache() with the
# staff's overrides), so every generator moves dates the same way. EVERY call
# of pasivos_generate_provisions() takes its policies from here
# (tests/test_t120_politicas_pasivos.R guards it).
pasivos_politicas_de <- function(liability, catalog, overrides = NULL) {
  ids <- pasivos_ids_politicas(liability)
  vacio <- structure(list(), festivos = list())
  if (!length(ids) || is.null(catalog) || !is.data.frame(catalog) || !nrow(catalog)) return(vacio)
  pols <- Filter(Negate(is.null), lapply(ids, function(pid) {
    i <- which(catalog$id == pid)
    if (!length(i)) return(NULL)
    i <- i[1]
    list(id = pid, type = as.character(catalog$type[[i]]),
         params = catalog$params[[i]] %||% list(),
         roll_direction = as.character(catalog$roll_direction[[i]] %||% "forward"))
  }))
  if (!length(pols)) return(vacio)
  structure(pols, festivos = festivos_cache(pols, overrides, years = .pasivos_anios_politicas(liability)))
}

# The years a liability's provisions can span (T120 review): from its start
# (or today) to its end (or two years out), one more each side for rolls
# across New Year. get_holidays() alone covers only the server's years.
.pasivos_anios_politicas <- function(liability) {
  hoy <- hoy_en()   # the client's day (CM27)
  ini <- tryCatch(as.Date(liability$fecha_inicio[1]), error = function(e) as.Date(NA))
  fin <- pasivos_fecha_fin(liability)
  if (is.na(ini) || ini > hoy) ini <- min(c(ini, hoy), na.rm = TRUE)
  if (is.na(fin) || fin < hoy) fin <- hoy + 365L * 2L
  a <- as.integer(format(ini, "%Y")) - 1L
  b <- min(as.integer(format(fin, "%Y")) + 1L, a + 60L)
  seq.int(a, b)
}

# T120 review: a future payment never lands in the past -- a backward roll
# that would cross today takes the next valid day from today instead.
.pasivos_fecha_politica <- function(fecha_calc, policies, holidays_cache, hoy) {
  f <- tryCatch(compose_policies(fecha_calc, policies, holidays_cache), error = function(e) fecha_calc)
  if (length(policies) && !is.na(f) && !is.na(fecha_calc) && fecha_calc >= hoy && f < hoy) {
    # From today, only what makes a day valid (business days, holidays),
    # forward: an offset or a month day applied again would go back again.
    filtros <- Filter(function(p) identical(p$type, "weekdays") || identical(p$type, "skip_holidays"), policies)
    filtros <- lapply(filtros, function(p) { p$roll_direction <- "forward"; p })
    f <- tryCatch(compose_policies(hoy, filtros, holidays_cache), error = function(e) hoy)
    if (is.na(f) || f < hoy) f <- hoy
  }
  as.Date(f)
}

# The catalog and the overrides read strictly, for a write (the daily
# refresh): a failed read stop()s -- never "no policies", which would send
# every moved provision back to its calculated date. A missing file is empty.
# The client's policy catalog read strictly (a failed read stop()s; a
# missing file is empty), normalized as load_policy_catalog() does.
.pasivos_catalogo_estricto <- function(client_id) {
  cat_raw <- .s3_leer_para_escribir(S3_KEYS$policy_catalog, client_id)
  if (is.null(cat_raw) || !is.data.frame(cat_raw) || !nrow(cat_raw)) return(.schema_policy_catalog())
  s <- .schema_policy_catalog()
  for (col in setdiff(names(s), "params")) if (!col %in% names(cat_raw)) cat_raw[[col]] <- s[[col]][NA_integer_]
  if (!"params" %in% names(cat_raw)) cat_raw[["params"]] <- vector("list", nrow(cat_raw))
  cat_raw
}

.pasivos_politicas_contexto <- function(client_id) {
  catalogo <- .pasivos_catalogo_estricto(client_id)
  # T120 review: a provision paid ahead may still be "provisional": what has
  # a live payment (bancos_confirmados, confirmaciones_vigentes()) never moves.
  conf <- leer_tabla_para_escribir(S3_KEYS$bancos_confirmados, client_id, .schema_bancos_confirmados)
  vivas <- confirmaciones_vigentes(conf)
  pagadas <- if (is.data.frame(vivas) && nrow(vivas) && "provision_id" %in% names(vivas))
    unique(as.character(vivas$provision_id[!is.na(vivas$provision_id)])) else character()
  list(catalogo = catalogo,
       overrides = leer_tabla_para_escribir(S3_KEYS$holiday_overrides, client_id, .schema_holiday_overrides),
       pagadas = pagadas)
}

# T70: the pending provisions of a liability that fall after its end date:
# editing the end date earlier sends them to the Papelera. Converted, paid or
# deleted ones are never touched. Returns their rows (none when no end).
pasivos_provisiones_tras_fin <- function(existing, liability) {
  fin <- pasivos_fecha_fin(liability)
  if (is.na(fin) || !nrow(existing) || !(liability$categoria[1] %in% c("regular", "tarjeta")))
    return(existing[0, , drop = FALSE])
  f <- if ("fecha_efectiva" %in% names(existing)) as.Date(existing$fecha_efectiva) else as.Date(NA)
  existing[!is.na(existing$liability_id) & existing$liability_id == liability$id[1] &
             existing$estado %in% "provisional" & !is.na(f) & f > fin, , drop = FALSE]
}

# ── T77: what a liability costs ────────────────────────────────────────────────
# Worked out from the engine each time (never stored), by currency, never
# adding currencies (Luis, Coordinador 2026-11-21):
#   financiero: the cost of the financing = interest + charges (initial fees
#     and any fee in the payments) + taxes over the whole term -- never the
#     capital -- with its breakdown, and the same over the next 12 months
#     ("por año"). The schedule's currency is the quoted one (cotizado_en)
#     when there is one, as the engine computes it; a fee keeps its own.
#     A variable rate is taken at its current value.
#   regular / tarjeta: what is paid over the next 12 months ("por año"), a
#     twelfth of it ("por mes") and, with an end date (T70), everything up
#     to it ("total"); taxes included (amount_pago carries them, T63).
# Returns list(categoria, filas = data.frame(moneda, intereses, cargos,
# impuestos, total, anual, mes, impuestos_faltan, total_faltan)); NA where it
# does not apply (total without an end date; intereses/cargos/impuestos and
# the *_faltan for regular and tarjeta).
# T109, one rule for every figure, all from the SAME schedule:
#   "todo el plazo" = every payment of the contract (total, intereses,
#     cargos, impuestos), past ones included;
#   "por pagar"     = the payments dated today or later (the client's day;
#     the schedule's historico == FALSE): impuestos_faltan, total_faltan;
#   "próximos 12 meses" = [hoy, hoy + 1 year): anual (and mes = anual / 12).
# usar_indice: as pasivos_generate_schedule(); the wizard passes FALSE and the
# client's day, like its amortization table, so both show the same schedule.
# politicas (T120): the session's policy catalog and holiday overrides,
# list(catalogo, overrides); NULL = none (the 12-month window may then be a
# day or two off for a liability with policies; the amounts are the same).
pasivos_costeo <- function(liability, hoy = hoy_en(), client_id = pasivos_cliente_sesion(), usar_indice = TRUE,
                           politicas = NULL) {
  hoy <- as.Date(hoy)
  un_ano <- seq(hoy, by = "1 year", length.out = 2L)[2]
  cat <- as.character(liability$categoria[1] %||% "")
  vacio <- data.frame(moneda = character(), intereses = numeric(), cargos = numeric(), impuestos = numeric(),
                      total = numeric(), anual = numeric(), mes = numeric(),
                      impuestos_faltan = numeric(), total_faltan = numeric(), stringsAsFactors = FALSE)
  por_moneda <- function(m, f) { if (!length(m)) return(numeric()); vapply(split(f, m), sum, numeric(1)) }

  if (identical(cat, "financiero")) {
    s <- pasivos_generate_schedule(liability, today = hoy, client_id = client_id, usar_indice = usar_indice)
    if (!nrow(s)) return(list(categoria = cat, filas = vacio))
    base_m <- as.character(liability$cotizado_en[1] %||% NA_character_)
    if (is.na(base_m) || !nzchar(base_m)) base_m <- as.character(liability$moneda_pago[1] %||% NA_character_)
    m <- if ("moneda" %in% names(s)) as.character(s$moneda) else rep(NA_character_, nrow(s))
    m[is.na(m) | !nzchar(m)] <- base_m
    num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }
    int <- num(s$interes); car <- num(s$fees)
    imp <- if ("impuestos" %in% names(s)) num(s$impuestos) else rep(0, nrow(s))
    costo <- int + car + imp
    prox <- as.Date(s$fecha) >= hoy & as.Date(s$fecha) < un_ano
    monedas <- sort(unique(m))
    filas <- data.frame(moneda = monedas,
      intereses = unname(por_moneda(m, int)[monedas]), cargos = unname(por_moneda(m, car)[monedas]),
      impuestos = unname(por_moneda(m, imp)[monedas]), total = unname(por_moneda(m, costo)[monedas]),
      anual = vapply(monedas, function(x) sum(costo[prox & m == x]), numeric(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE)
    filas$mes <- filas$anual / 12
    falta <- as.Date(s$fecha) >= hoy   # T109: "por pagar" (= !historico)
    filas$impuestos_faltan <- vapply(monedas, function(x) sum(imp[falta & m == x]), numeric(1), USE.NAMES = FALSE)
    filas$total_faltan     <- vapply(monedas, function(x) sum(costo[falta & m == x]), numeric(1), USE.NAMES = FALSE)
    return(list(categoria = cat, filas = filas))
  }

  if (cat %in% c("regular", "tarjeta")) {
    fin <- pasivos_fecha_fin(liability)
    p <- pasivos_generate_provisions(liability, window_start = hoy, client_id = client_id,
                                     policies_for_liability = pasivos_politicas_de(liability, politicas$catalogo, politicas$overrides),
                                     window_end = if (!is.na(fin)) max(fin, un_ano) else un_ano)
    if (!nrow(p)) return(list(categoria = cat, filas = vacio))
    m <- as.character(p$moneda_pago); m[is.na(m)] <- as.character(liability$moneda_pago[1] %||% NA_character_)
    f <- as.Date(p$fecha_efectiva)
    monto <- suppressWarnings(as.numeric(p$amount_pago))
    prox <- f >= hoy & f < un_ano
    monedas <- sort(unique(m))
    # A payment with no amount yet (quoted in another currency without an
    # exchange rate) makes the sum unknown: NA, never a false 0.
    suma <- function(k) if (any(is.na(monto[k]))) NA_real_ else sum(monto[k])
    anual <- vapply(monedas, function(x) suma(prox & m == x), numeric(1), USE.NAMES = FALSE)
    total <- if (is.na(fin)) rep(NA_real_, length(monedas))
             else vapply(monedas, function(x) suma(f <= fin & m == x), numeric(1), USE.NAMES = FALSE)
    return(list(categoria = cat, filas = data.frame(moneda = monedas, intereses = NA_real_, cargos = NA_real_,
      impuestos = NA_real_, total = total, anual = anual, mes = anual / 12,
      impuestos_faltan = NA_real_, total_faltan = NA_real_, stringsAsFactors = FALSE)))
  }
  list(categoria = cat, filas = vacio)
}

# The session's client (session$userData$hd_cliente(), set in app.R), or
# NULL outside a session (scripts, the emails job): the default client_id of
# the engine's estimate reads. Per session, never a global.
# The estimates' metric of a variable tasa_tipo ("variable_sofr" -> "sofr").
pasivos_metrica_tasa <- function(tasa_tipo) {
  m <- c(variable_sofr = "sofr", variable_tiie28 = "tiie28", variable_prime = "prime", variable_otro = "otro")
  tasa_tipo <- as.character(tasa_tipo %||% "")
  if (tasa_tipo %in% names(m)) unname(m[[tasa_tipo]]) else sub("^variable_", "", tasa_tipo)
}

# A variable rate in force on `fecha`, nominal annual in % (index + spread),
# and whether it is the index (TRUE) or the rate captured on the day (FALSE).
# NULL for a fixed rate.
pasivos_tasa_variable_vigente <- function(liability, fecha = hoy_en(), client_id = pasivos_cliente_sesion()) {
  tipo <- as.character(liability$tasa_tipo[1] %||% "fija")
  if (!startsWith(tipo, "variable_")) return(NULL)
  est <- tryCatch(forecasting_get_estimate(pasivos_metrica_tasa(tipo), as.Date(fecha), client_id = client_id),
                  error = function(e) NA_real_)
  spread <- suppressWarnings(as.numeric(liability$tasa_spread[1] %||% 0)); if (is.na(spread)) spread <- 0
  base <- if (is.na(est)) suppressWarnings(as.numeric(liability$tasa_actual[1])) else est
  list(nominal = base + spread / 100, del_indice = !is.na(est))
}

# The effective annual rate (EAR, %) of a nominal annual rate in % paid m
# times a year, simple interest Act/360: (1 + t * 365/360 / m)^m - 1.
pasivos_ear_act360 <- function(nominal, m) 100 * ((1 + nominal / 100 * 365 / 360 / m)^m - 1)

pasivos_cliente_sesion <- function(session = shiny::getDefaultReactiveDomain()) {
  cid <- tryCatch(shiny::isolate(session$userData$hd_cliente()), error = function(e) NULL)
  if (is.character(cid) && length(cid) == 1L && !is.na(cid) && nzchar(cid)) cid else NULL
}

# ── §2.3 Provision generation ──────────────────────────────────────────────────

# window_start / today: the client's day by default (T75b, CM27); `today`
# is what the financiero schedule calls historico (it used to take the
# server's day whatever the caller passed).
pasivos_generate_provisions <- function(liability,
                                         window_start = hoy_en(),
                                         window_end   = NULL,
                                         policies_for_liability = list(),
                                         holidays_cache = list(),
                                         today = window_start,
                                         client_id = pasivos_cliente_sesion()) {
  window_start <- as.Date(window_start)
  # T120: the holidays come with the policies (pasivos_politicas_de()).
  if (!length(holidays_cache) && length(attr(policies_for_liability, "festivos")))
    holidays_cache <- attr(policies_for_liability, "festivos")
  cat <- liability$categoria

  if (is.null(window_end)) {
    window_end <- switch(cat,
      regular    = window_start + lubridate::years(2),
      financiero = as.Date(liability$fecha_vence),
      tarjeta    = window_start + lubridate::years(2),
      window_start + lubridate::years(2)
    )
  }
  window_end <- as.Date(window_end)
  if (is.na(window_end)) window_end <- window_start + lubridate::years(2)
  # T70: a regular or tarjeta liability may have an end date (fecha_vence; NA =
  # no end): nothing is generated after it.
  if (cat %in% c("regular", "tarjeta")) {
    fin <- pasivos_fecha_fin(liability)
    if (!is.na(fin) && fin < window_end) window_end <- fin
  }
  nota_pasivo <- pasivos_nota_de(liability)   # T71: every provision is born with it

  schema <- .schema_pasivos_provision()
  imps   <- pasivos_impuestos_de(liability)   # T63: taxes on each payment (none -> as before)

  # ── Build raw date+amount stream ─────────────────────────────────────────
  if (identical(cat, "financiero")) {
    sched <- pasivos_generate_schedule(liability, today = today, client_id = client_id)
    # Filter to non-historico rows in window
    stream <- sched[!sched$historico &
                      sched$fecha >= window_start &
                      sched$fecha <= window_end, , drop = FALSE]
    if (!nrow(stream)) return(schema)

    n_rows <- nrow(stream)
    rows <- lapply(seq_len(n_rows), function(i) {
      row_s <- stream[i, , drop = FALSE]

      fecha_calc <- row_s$fecha
      # Apply policies
      fecha_ef <- .pasivos_fecha_politica(fecha_calc, policies_for_liability, holidays_cache, as.Date(today))
      pol_ids <- if (length(policies_for_liability)) {
        paste(vapply(policies_for_liability, function(p) p$id %||% "", character(1)),
              collapse = ",")
      } else { "" }

      # Currency — fee rows use their own moneda; regular rows use liability's
      amt_cot     <- row_s$total
      fee_moneda  <- row_s[["moneda"]][[1L]]
      is_fee_row  <- identical(as.character(row_s$origin[[1L]]), "initial_fee") &&
                       !is.null(fee_moneda) && !is.na(fee_moneda) && nzchar(fee_moneda)
      if (is_fee_row) {
        row_moneda_pago <- fee_moneda
        cot_en          <- NA_character_
        fx_res          <- list(amount_pago = amt_cot, fx_rate_used = NA_real_)
      } else {
        row_moneda_pago <- liability$moneda_pago
        cot_en <- liability$cotizado_en
        cot_en <- if (is.null(cot_en) || length(cot_en) == 0) NA_character_ else cot_en
        fx_res  <- pasivos_recompute_pago_from_cotizado(
          amt_cot, cot_en, row_moneda_pago, fecha_ef, client_id = client_id
        )
      }

      # Documento substitution
      tmpl <- liability$documento_template
      tmpl <- if (is.null(tmpl) || length(tmpl) == 0 || is.na(tmpl)) NA_character_ else {
        tmpl <- gsub("\\{YYYY\\}", format(fecha_ef, "%Y"), tmpl)
        tmpl <- gsub("\\{MM\\}",   format(fecha_ef, "%m"), tmpl)
        tmpl <- gsub("\\{DD\\}",   format(fecha_ef, "%d"), tmpl)
        tmpl
      }

      # T63: the liability's taxes on each scheduled payment, in its payment
      # currency (after FX); "intereses" takes the interest converted the same
      # way. Initial fees (their own currency and amount) carry none. With no
      # taxes nothing changes: componente_iva NA, amount_pago as before.
      imp_k <- NA_real_
      if (!is_fee_row && length(imps)) {
        conv  <- if (!is.na(amt_cot) && amt_cot != 0) fx_res$amount_pago / amt_cot else 1
        imp_k <- pasivos_impuestos_pago(imps, fx_res$amount_pago, (row_s$interes %||% 0) * conv)
        fx_res$amount_pago <- fx_res$amount_pago + imp_k
      }

      tibble::tibble(
        id                   = uuid::UUIDgenerate(),
        liability_id         = liability$id,
        origin               = row_s$origin,
        occurrence_index     = as.integer(row_s$periodo),
        estado               = "provisional",
        fecha_calculada      = fecha_calc,
        fecha_efectiva       = fecha_ef,
        policy_ids           = pol_ids,
        empresa              = liability$empresa,
        parte                = liability$parte,
        codigo_parte         = liability$codigo_parte,
        moneda_pago          = row_moneda_pago,
        cotizado_en          = cot_en,
        amount_pago          = fx_res$amount_pago,
        amount_cotizado      = amt_cot,
        fx_rate_used         = fx_res$fx_rate_used,
        componente_capital   = row_s$capital,
        componente_interes   = row_s$interes,
        componente_fees      = row_s$fees,
        componente_iva       = imp_k,
        amount_pago_override     = NA_real_,
        amount_cotizado_override = NA_real_,
        fecha_efectiva_override  = as.Date(NA),
        documento            = tmpl,
        referencia           = if (is_fee_row) (row_s[["fee_desc"]][[1L]] %||% "") else (liability$referencia_default %||% NA_character_),
        notas                = nota_pasivo,
        manual_inv_id        = NA_character_,
        pagar_hoy_id         = NA_character_,
        bancos_conf_id       = NA_character_,
        reverted_count       = 0L,
        generated_by         = "system",
        generated_at         = Sys.time(),
        last_edited_by       = NA_character_,
        last_edited_at       = as.POSIXct(NA)
      )
    })

  } else {
    # regular or tarjeta
    # Expand enough recurrence dates to cover the window
    # PV-1: with a fecha_inicio, each date's index is its place counted from
    # the start (pasivos_recurrencia_fechas()): the same date keeps the same
    # index whatever day it is generated, and nothing is generated before the
    # start. Without one (older liabilities), as before: counted from
    # window_start.
    ini <- suppressWarnings(as.Date((liability$fecha_inicio %||% NA)[1]))
    if (!is.na(ini)) {
      todas <- tryCatch(pasivos_recurrencia_fechas(liability, window_end),
                        error = function(e) as.Date(character(0)))
      en_ventana <- todas >= window_start & todas <= window_end
      dates   <- todas[en_ventana]
      indices <- which(en_ventana)
    } else {
      n_approx <- as.integer(ceiling(as.numeric(window_end - window_start) / 14)) + 60L
      dates <- tryCatch(
        pasivos_expand_recurrence(
          liability$recurrence_type,
          liability$recurrence_params[[1]] %||% list(),
          window_start, n_approx
        ),
        error = function(e) as.Date(character(0))
      )
      dates   <- dates[dates >= window_start & dates <= window_end]
      indices <- seq_along(dates)
    }
    if (!length(dates)) return(schema)

    cot_en <- liability$cotizado_en
    cot_en <- if (is.null(cot_en) || length(cot_en) == 0) NA_character_ else cot_en
    use_cotizado <- !is.na(cot_en) && nzchar(cot_en)
    amt_default <- if (use_cotizado) {
      as.numeric(liability$amount_default_cot)
    } else {
      as.numeric(liability$amount_default)
    }

    rows <- lapply(seq_along(dates), function(i) {
      fecha_calc <- dates[i]
      fecha_ef <- .pasivos_fecha_politica(fecha_calc, policies_for_liability, holidays_cache, as.Date(today))
      pol_ids <- if (length(policies_for_liability)) {
        paste(vapply(policies_for_liability, function(p) p$id %||% "", character(1)),
              collapse = ",")
      } else { "" }

      fx_res <- pasivos_recompute_pago_from_cotizado(
        amt_default, cot_en, liability$moneda_pago, fecha_ef, client_id = client_id
      )
      # T63: taxes on the whole payment (no interest here), in its currency.
      imp_k <- NA_real_
      if (length(imps)) {
        imp_k <- pasivos_impuestos_pago(imps, fx_res$amount_pago)
        fx_res$amount_pago <- fx_res$amount_pago + imp_k
      }

      tmpl <- liability$documento_template
      tmpl <- if (is.null(tmpl) || length(tmpl) == 0 || is.na(tmpl)) NA_character_ else {
        tmpl <- gsub("\\{YYYY\\}", format(fecha_ef, "%Y"), tmpl)
        tmpl <- gsub("\\{MM\\}",   format(fecha_ef, "%m"), tmpl)
        tmpl <- gsub("\\{DD\\}",   format(fecha_ef, "%d"), tmpl)
        tmpl
      }

      tibble::tibble(
        id                   = uuid::UUIDgenerate(),
        liability_id         = liability$id,
        origin               = "rule",
        occurrence_index     = as.integer(indices[i]),
        estado               = "provisional",
        fecha_calculada      = fecha_calc,
        fecha_efectiva       = fecha_ef,
        policy_ids           = pol_ids,
        empresa              = liability$empresa,
        parte                = liability$parte,
        codigo_parte         = liability$codigo_parte,
        moneda_pago          = liability$moneda_pago,
        cotizado_en          = cot_en,
        amount_pago          = fx_res$amount_pago,
        amount_cotizado      = if (use_cotizado) amt_default else NA_real_,
        fx_rate_used         = fx_res$fx_rate_used,
        componente_capital   = NA_real_,
        componente_interes   = NA_real_,
        componente_fees      = NA_real_,
        componente_iva       = imp_k,
        amount_pago_override     = NA_real_,
        amount_cotizado_override = NA_real_,
        fecha_efectiva_override  = as.Date(NA),
        documento            = tmpl,
        referencia           = liability$referencia_default,
        notas                = nota_pasivo,
        manual_inv_id        = NA_character_,
        pagar_hoy_id         = NA_character_,
        bancos_conf_id       = NA_character_,
        reverted_count       = 0L,
        generated_by         = "system",
        generated_at         = Sys.time(),
        last_edited_by       = NA_character_,
        last_edited_at       = as.POSIXct(NA)
      )
    })
  }

  dplyr::bind_rows(rows)
}

# ── §2.4 Reconciliation ────────────────────────────────────────────────────────

# Overrideable fields checked for conflicts.
.PROVISION_OVERRIDEABLE_FIELDS <- c(
  "amount_pago", "amount_cotizado", "fecha_efectiva",
  "componente_capital", "componente_interes", "componente_fees",
  "documento", "referencia"
)

# Maps overrideable field to its override column name.
.override_col <- function(field) {
  switch(field,
    amount_pago     = "amount_pago_override",
    amount_cotizado = "amount_cotizado_override",
    fecha_efectiva  = "fecha_efectiva_override",
    NA_character_
  )
}

# A provision's note (notas) is the person's: editing the liability used to
# copy the engine's NA over it, so every note written by hand on a pending
# provision vanished (found 2026-11-20, T70/T71 paso 0). It is replaced only
# when the provision has none, or still has the liability's previous note
# (`nota_anterior`): a note of its own is kept.
.nota_propia <- function(nota, nota_anterior = NULL) {
  n <- as.character(nota %||% NA_character_)[1]
  if (is.na(n) || !nzchar(trimws(n))) return(FALSE)
  a <- as.character(nota_anterior %||% NA_character_)[1]
  !(!is.na(a) && identical(trimws(n), trimws(a)))
}

# PV-1: the index each existing row of a regular / tarjeta liability is
# matched by: its date's place in the recurrence counted from fecha_inicio
# (the same count pasivos_generate_provisions() gives the generated rows).
# The stored index can be the old one, counted from the day the row was
# generated; the date is what is trusted. `antes` are the terms the rows were
# generated with (the liability before an edit), `despues` the new ones: the
# k-th date of the old terms becomes the k-th of the new (as a financial
# liability's period k). A date off the recurrence takes the nearest one
# within half the shortest gap; farther, none (NA: never matched, kept as it
# is). Without a fecha_inicio (older liabilities) the generated rows are
# counted from the day they are generated: a row takes the index of the
# generated row with its very date. Two rows on one place: the one that is no
# longer pending, then the nearest, keeps it. Rows of other liabilities keep
# theirs.
.pasivos_claves_recurrencia <- function(existing, antes, despues, generated) {
  clave <- existing$occurrence_index
  mia <- !is.na(existing$liability_id) & existing$liability_id == despues$id[1]
  if (!any(mia)) return(clave)
  f <- suppressWarnings(as.Date(existing$fecha_calculada))
  sin_f <- is.na(f)
  f[sin_f] <- suppressWarnings(as.Date(existing$fecha_efectiva[sin_f]))
  nueva <- rep(NA_integer_, nrow(existing))
  dist  <- rep(Inf, nrow(existing))
  ini_a <- suppressWarnings(as.Date((antes$fecha_inicio %||% NA)[1]))
  ini_d <- suppressWarnings(as.Date((despues$fecha_inicio %||% NA)[1]))
  if (!is.na(ini_a) && !is.na(ini_d)) {
    hasta <- if (any(mia & !is.na(f))) max(f[mia & !is.na(f)]) + 31 else ini_a
    ref <- tryCatch(pasivos_recurrencia_fechas(antes, hasta), error = function(e) as.Date(character(0)))
    if (length(ref)) {
      tol <- if (length(ref) > 1L) max(1, floor(min(as.numeric(diff(ref))) / 2)) else 3
      for (j in which(mia & !is.na(f))) {
        d <- abs(as.numeric(ref - f[j]))
        k <- which.min(d)
        if (d[k] < tol) { nueva[j] <- k; dist[j] <- d[k] }
      }
    }
  } else if (!is.null(generated) && nrow(generated)) {
    m <- match(f, suppressWarnings(as.Date(generated$fecha_calculada)))
    ok <- mia & !is.na(m)
    nueva[ok] <- as.integer(generated$occurrence_index[m[ok]])
    dist[ok]  <- 0
  }
  orden <- order(existing$estado %in% "provisional", dist)
  repetida <- rep(FALSE, nrow(existing))
  repetida[orden] <- duplicated(nueva[orden], incomparables = NA)
  nueva[repetida] <- NA_integer_
  clave[mia] <- nueva[mia]
  clave
}

# `liability` (PV-1): the liability the rows belong to, with its new terms,
# and `liability_antes` the terms they were generated with (the liability
# before an edit; by default the same). For a regular / tarjeta one the rows
# are matched by date (.pasivos_claves_recurrencia()), so a month with no
# provision gets one again, none is duplicated, and a pending one is never
# moved to another month. Without it, by the stored index, as before.
pasivos_reconcile_provisions <- function(existing, generated, mode = "regenerate", nota_anterior = NULL,
                                         liability = NULL, liability_antes = liability) {
  empty <- existing[0L, , drop = FALSE]

  if (!nrow(existing)) {
    return(list(keep = empty, update = empty, insert = generated,
                conflicts = empty))
  }

  # PV-1: matched by the date's place; the rows this call keeps go back with
  # their stored index (a paid or converted row is not rewritten), the ones it
  # changes (update, conflicts) take the absolute one.
  indice_guardado <- NULL
  if (!is.null(liability) && NROW(liability) &&
      isTRUE(liability$categoria[1] %in% c("regular", "tarjeta"))) {
    indice_guardado <- stats::setNames(existing$occurrence_index, existing$id)
    existing$occurrence_index <- .pasivos_claves_recurrencia(
      existing, if (!is.null(liability_antes) && NROW(liability_antes)) liability_antes else liability,
      liability, generated)
  }

  # Orphans always go to keep
  orphans    <- existing[is.na(existing$liability_id), , drop = FALSE]
  rule_exist <- existing[!is.na(existing$liability_id), , drop = FALSE]

  # Non-provisional always kept (immutability rule 1)
  immutable  <- rule_exist[rule_exist$estado != "provisional", , drop = FALSE]
  mutable    <- rule_exist[rule_exist$estado == "provisional", , drop = FALSE]

  keep_rows     <- dplyr::bind_rows(orphans, immutable)
  update_rows   <- empty
  insert_rows   <- empty
  conflict_rows <- empty

  # Occurrences that already exist in ANY estado. Inserts used to be checked
  # against the provisional ones only, so regenerating (or extending past) a
  # converted, paid or deleted occurrence created it again as a new
  # provisional row: a duplicate, or a deleted one coming back (found
  # 2026-11-01).
  .occ <- function(df) paste(df$liability_id, df$occurrence_index, sep = "\r")
  ya_existe <- .occ(rule_exist)

  if (identical(mode, "extend")) {
    max_idx  <- if (nrow(mutable)) max(mutable$occurrence_index, na.rm = TRUE) else -Inf
    max_date <- if (nrow(mutable)) max(mutable$fecha_efectiva, na.rm = TRUE) else as.Date(-Inf)
    insert_rows <- generated[
      generated$occurrence_index > max_idx |
        (!is.na(generated$fecha_efectiva) & generated$fecha_efectiva > max_date),
      , drop = FALSE
    ]
    insert_rows <- insert_rows[!.occ(insert_rows) %in% ya_existe, , drop = FALSE]
    keep_rows <- dplyr::bind_rows(keep_rows, mutable)

  } else if (identical(mode, "rate_update")) {
    # Only update componente_interes (and derived amount_pago) on mutable non-overridden rows
    matched <- dplyr::inner_join(
      mutable[, c("id", "liability_id", "occurrence_index"), drop = FALSE],
      generated[, c("liability_id", "occurrence_index", "componente_interes",
                    "componente_capital", "componente_fees", "amount_pago"), drop = FALSE],
      by = c("liability_id", "occurrence_index")
    )
    for (i in seq_len(nrow(mutable))) {
      row_ex <- mutable[i, , drop = FALSE]
      has_override <- !is.na(row_ex$amount_pago_override)
      if (has_override) {
        keep_rows <- dplyr::bind_rows(keep_rows, row_ex)
        next
      }
      m <- matched[matched$id == row_ex$id, , drop = FALSE]
      if (!nrow(m)) {
        keep_rows <- dplyr::bind_rows(keep_rows, row_ex)
        next
      }
      row_ex$componente_interes <- m$componente_interes[1]
      row_ex$amount_pago <- m$componente_capital[1] + m$componente_interes[1] + m$componente_fees[1]
      update_rows <- dplyr::bind_rows(update_rows, row_ex)
    }

  } else {
    # regenerate mode
    for (i in seq_len(nrow(mutable))) {
      row_ex <- mutable[i, , drop = FALSE]
      g_match <- generated[
        !is.na(generated$liability_id) &
          generated$liability_id == row_ex$liability_id &
          !is.na(generated$occurrence_index) &
          generated$occurrence_index == row_ex$occurrence_index,
        , drop = FALSE
      ]
      if (!nrow(g_match)) {
        # No longer in generated — keep (deletions are explicit in Stage 4)
        keep_rows <- dplyr::bind_rows(keep_rows, row_ex)
        next
      }
      g <- g_match[1, , drop = FALSE]

      # Check for conflicts
      in_conflict <- FALSE
      for (fld in .PROVISION_OVERRIDEABLE_FIELDS) {
        ov_col <- .override_col(fld)
        if (!is.na(ov_col) && ov_col %in% names(row_ex)) {
          has_override <- !is.na(row_ex[[ov_col]])
          if (!has_override) next
          # Override is set — check if generated wants to change the field
          if (fld %in% names(g) && fld %in% names(row_ex)) {
            ex_val <- row_ex[[fld]]
            gn_val <- g[[fld]]
            different <- !isTRUE(all.equal(ex_val, gn_val, check.attributes = FALSE))
            if (different) { in_conflict <- TRUE; break }
          }
        }
      }

      if (in_conflict) {
        conflict_rows <- dplyr::bind_rows(conflict_rows, row_ex)
      } else {
        # Update non-overridden fields from generated
        updated_row <- row_ex
        for (fld in setdiff(names(g), c("id", "liability_id", "occurrence_index",
                                          "estado", "generated_by", "generated_at",
                                          "manual_inv_id", "pagar_hoy_id",
                                          "bancos_conf_id", "reverted_count",
                                          "last_edited_by", "last_edited_at"))) {
          # The hand edits themselves (*_override) are the person's, never the
          # engine's: a generated row has them NA, and copying it erased every
          # amount or date edited by hand on the liability's pending
          # provisions whenever the wizard saved without changing that field
          # (renaming it was enough; found 2026-11-22, PV-1).
          if (grepl("_override$", fld)) next
          ov_col <- .override_col(fld)
          # Skip if this field is protected by a non-NA override
          if (!is.na(ov_col) && ov_col %in% names(row_ex) && !is.na(row_ex[[ov_col]])) next
          if (identical(fld, "notas") && .nota_propia(row_ex$notas, nota_anterior)) next
          if (fld %in% names(updated_row)) updated_row[[fld]] <- g[[fld]]
        }
        update_rows <- dplyr::bind_rows(update_rows, updated_row)
      }
    }

    # Rows in generated not in existing (any estado) → insert
    insert_rows <- dplyr::bind_rows(insert_rows,
                                    generated[!.occ(generated) %in% ya_existe, , drop = FALSE])
  }

  if (!is.null(indice_guardado) && nrow(keep_rows)) {
    k <- keep_rows$id %in% names(indice_guardado)
    keep_rows$occurrence_index[k] <- unname(indice_guardado[keep_rows$id[k]])
  }

  list(keep = keep_rows, update = update_rows,
       insert = insert_rows, conflicts = conflict_rows)
}

# ── §2.5 Lifecycle state machine ───────────────────────────────────────────────

# Helpers that load/mutate/save/audit a single provision by id.
# All functions return the updated provision row invisibly.

.load_provision <- function(provision_id, client_id = NULL) {
  all_provs <- load_pasivos_provisions(client_id = client_id)
  row <- all_provs[all_provs$id == provision_id, , drop = FALSE]
  if (!nrow(row)) stop("[pasivos] provision not found: ", provision_id)
  row[1L, , drop = FALSE]  # guard against duplicate ids
}

.save_provision_row <- function(row, client_id = NULL) {
  all_provs <- load_pasivos_provisions(client_id = client_id)
  provs_base <- all_provs   # CM09: only this row is applied on S3 as it is now
  idx <- which(all_provs$id == row$id)
  if (length(idx)) {
    all_provs[idx, ] <- row
  } else {
    all_provs <- dplyr::bind_rows(all_provs, row)
  }
  save_pasivos_provisions(all_provs, client_id = client_id, base = provs_base)
  bump_sync_version("pasivos_provisions_db", client_id = client_id)
  invisible(TRUE)
}

pasivos_provision_convert <- function(provision_id, manual_inv_id, pagar_hoy_id, user,
                                       client_id = NULL, content_diff = NULL) {
  row <- .load_provision(provision_id, client_id = client_id)
  if (row$estado != "provisional")
    stop("[pasivos] cannot convert provision with estado=", row$estado,
         "; must be 'provisional'")
  before <- as.list(row)
  row$estado        <- "converted"
  row$manual_inv_id <- as.character(manual_inv_id)
  row$pagar_hoy_id  <- as.character(pagar_hoy_id)
  .save_provision_row(row, client_id = client_id)
  # content_diff (2026-07-30): the provision row itself only captures the
  # estado transition + linkage ids -- it has no fields comparable to the
  # empresa/moneda/documento/importe/etc. the user can edit on the review
  # form before finalizing the conversion. Flagged as critical: "who
  # approved what, when, how, because they could've made edits ... either
  # correctly or incorrectly." Caller (.pasivos_perform_conversion) computes
  # this field-by-field diff, since it's the only place both the
  # provision's original implied values and the form's final values are in
  # scope together. Merged into `after` so one audit entry shows both the
  # state transition and exactly what content changed, if anything.
  after_payload <- as.list(row)
  if (!is.null(content_diff)) after_payload$content_diff <- content_diff
  pasivos_log_audit(
    action_type = "provision.converted_to_item",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = after_payload,
    client_id   = client_id
  )
  invisible(row)
}

pasivos_provision_item_confirmed <- function(provision_id, bancos_conf_id, user,
                                              client_id = NULL) {
  row <- .load_provision(provision_id, client_id = client_id)
  if (row$estado != "converted")
    stop("[pasivos] cannot confirm provision with estado=", row$estado,
         "; must be 'converted'")
  before <- as.list(row)
  row$estado         <- "item_confirmed"
  row$bancos_conf_id <- as.character(bancos_conf_id)
  .save_provision_row(row, client_id = client_id)
  pasivos_log_audit(
    action_type = "provision.item_confirmed",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row),
    client_id   = client_id
  )
  invisible(row)
}

# The item's confirmation was undone in Bancos. undo_conf() restores the
# item itself, so the provision is converted again -- not paid, and not
# provisional either (the item still exists). Until 2026-10-31 nothing made
# this transition and the provision stayed "item_confirmed" after the undo.
pasivos_provision_item_reversed <- function(provision_id, user, client_id = NULL) {
  row <- .load_provision(provision_id, client_id = client_id)
  if (row$estado != "item_confirmed") return(invisible(row))
  before <- as.list(row)
  row$estado         <- "converted"
  row$bancos_conf_id <- NA_character_
  .save_provision_row(row, client_id = client_id)
  pasivos_log_audit(
    action_type = "provision.item_reversed",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row),
    client_id   = client_id
  )
  invisible(row)
}

pasivos_provision_close <- function(provision_id, user, client_id = NULL) {
  row <- .load_provision(provision_id, client_id = client_id)
  if (row$estado != "item_confirmed")
    stop("[pasivos] cannot close provision with estado=", row$estado,
         "; must be 'item_confirmed'")
  before <- as.list(row)
  row$estado <- "closed"
  .save_provision_row(row, client_id = client_id)
  pasivos_log_audit(
    action_type = "provision.item_confirmed",  # reuse closest available action
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row),
    notes       = "closed",
    client_id   = client_id
  )
  invisible(row)
}

pasivos_provision_cancel <- function(provision_id, user, client_id = NULL) {
  row <- .load_provision(provision_id, client_id = client_id)
  if (row$estado == "closed")
    return(invisible(row))  # already closed, no-op
  before <- as.list(row)
  row$estado <- "closed"
  .save_provision_row(row, client_id = client_id)
  pasivos_log_audit(
    action_type = "provision.cancelled",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row),
    client_id   = client_id
  )
  invisible(row)
}

pasivos_provision_revive <- function(provision_id, user, client_id = NULL) {
  row <- .load_provision(provision_id, client_id = client_id)
  if (row$estado == "provisional") {
    warning("[pasivos] provision ", provision_id, " is already provisional — no-op")
    return(invisible(row))
  }
  if (!row$estado %in% c("converted", "item_confirmed", "closed"))
    stop("[pasivos] cannot revive provision with estado=", row$estado)
  before <- as.list(row)
  row$estado         <- "provisional"
  row$manual_inv_id  <- NA_character_
  row$pagar_hoy_id   <- NA_character_
  row$bancos_conf_id <- NA_character_
  row$reverted_count <- as.integer(row$reverted_count) + 1L
  .save_provision_row(row, client_id = client_id)
  pasivos_log_audit(
    action_type = "provision.revived",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row),
    client_id   = client_id
  )
  invisible(row)
}

# ── §2.6 Modifiers ─────────────────────────────────────────────────────────────
# pasivos_apply_modifiers() and .metric_for_modifier_type() were removed
# (2026-11-21, tasas variables): nothing called them, and their
# interest_rate_* branch used valor/12 on the period's capital, not on the
# balance. A variable rate is the schedule's job (.tasas_periodo() above).
# The pasivos_modifiers table and its loaders stay; nothing writes it.


# ── PV-2: provisiones vivas (2026-11-22) ───────────────────────────────────────
# The stored provisions were only generated when the wizard saved, and then
# froze: a variable-rate liability kept the index of the day it was saved,
# and a recurring one with no end stopped two years after it. Once a day per
# client (pasivos_module.R, when the session's client is known) they are
# brought up to date:
#   - variable rate (financiero, variable_*, no imported table): the pending
#     future provisions take the index in force, when one of them moves more
#     than 0.5 % or 1 unit (not rewritten for cents);
#   - regular / tarjeta: the dates from today to the horizon with no provision
#     get one (the two years ahead, and the months an old edit left empty,
#     PV-1). Nothing else of theirs changes (amounts, FX: as saved).
# Never touched: paid, converted or confirmed provisions, the ones edited by
# hand, the past, other liabilities, paused or deleted liabilities, fixed-rate
# ones. One write, modificar_tabla() over S3 as it is now: two sessions at once
# do not duplicate (the second finds nothing to do).
PASIVOS_REFRESCO_UMBRAL <- c(relativo = 0.005, absoluto = 1)

# TRUE for the provisions edited by hand (any *_override set).
.pasivos_con_override <- function(p) {
  if (is.null(p) || !nrow(p)) return(logical(0))
  cols <- vapply(.PROVISION_OVERRIDEABLE_FIELDS, .override_col, character(1))
  cols <- intersect(cols[!is.na(cols)], names(p))
  if (!length(cols)) return(rep(FALSE, nrow(p)))
  Reduce(`|`, lapply(cols, function(cl) !is.na(p[[cl]])))
}

# What depends on the payment date (T120 review): the exchange rate at that
# date and what it converts, and the document number built from the date.
PASIVOS_CAMPOS_FECHA_PAGO <- c("amount_pago", "fx_rate_used", "componente_capital", "componente_interes",
                               "componente_fees", "componente_iva", "documento")

# Pure: the provisions table brought up to date, and what changed (one row
# per liability: motivo "tasa" | "horizonte" | "politica", actualizadas,
# nuevas, reubicadas).
# T120: politicas = list(catalogo, overrides) (.pasivos_politicas_contexto()).
# A liability with policies -- or whose provisions a policy moved before --
# has the PAYMENT date of its provisional, future, non-overridden provisions
# put where its policies say today (a new holiday, an edited or removed
# policy). Converted, confirmed, paid, closed or hand-edited ones never move;
# nor does one no policy ever moved.
pasivos_refresco_calcular <- function(liabs, provs, hoy, client_id, politicas = NULL) {
  hoy <- as.Date(hoy)
  cambios <- data.frame(liability_id = character(), nombre = character(), motivo = character(),
                        actualizadas = integer(), nuevas = integer(), reubicadas = integer(),
                        stringsAsFactors = FALSE)
  if (is.null(liabs) || !nrow(liabs) || is.null(provs)) return(list(tabla = provs, cambios = cambios))
  activos <- liabs[liabs$estado %in% "active", , drop = FALSE]
  tabla <- provs
  for (i in seq_len(nrow(activos))) {
    L <- activos[i, , drop = FALSE]
    categoria <- L$categoria[1]
    importada <- "schedule_imported" %in% names(L) && length(L$schedule_imported[[1]]) > 0
    variable <- identical(categoria, "financiero") && !importada &&
      isTRUE(startsWith(as.character(L$tasa_tipo[1] %||% ""), "variable_"))
    recurrente <- isTRUE(categoria %in% c("regular", "tarjeta"))
    existentes <- tabla[!is.na(tabla$liability_id) & tabla$liability_id == L$id, , drop = FALSE]
    pid_prev <- if ("policy_ids" %in% names(existentes)) as.character(existentes$policy_ids) else rep(NA_character_, nrow(existentes))
    movida_antes <- !is.na(pid_prev) & nzchar(pid_prev)
    con_politica <- length(pasivos_ids_politicas(L)) > 0L || any(movida_antes)
    if (!variable && !recurrente && !con_politica) next
    g <- tryCatch(pasivos_generate_provisions(L, window_start = hoy, client_id = client_id,
                                              policies_for_liability = pasivos_politicas_de(L, politicas$catalogo, politicas$overrides)),
                  error = function(e) { message("[pasivos] refresco ", L$id, ": ", conditionMessage(e)); NULL })
    if (is.null(g) || !nrow(g)) next
    r <- tryCatch(pasivos_reconcile_provisions(existentes, g, mode = "regenerate", liability = L),
                  error = function(e) { message("[pasivos] refresco ", L$id, ": ", conditionMessage(e)); NULL })
    if (is.null(r)) next
    nuevas <- r$insert
    nuevas <- nuevas[!is.na(nuevas$fecha_efectiva) & as.Date(nuevas$fecha_efectiva) >= hoy, , drop = FALSE]
    if (!variable && !recurrente) nuevas <- nuevas[0L, , drop = FALSE]   # T120: a policy only re-places
    actualizadas <- existentes[0L, , drop = FALSE]
    if (variable && nrow(r$update)) {
      u <- r$update[r$update$estado %in% "provisional" & !.pasivos_con_override(r$update) &
                      !is.na(r$update$fecha_efectiva) & as.Date(r$update$fecha_efectiva) >= hoy, , drop = FALSE]
      antes <- existentes[match(u$id, existentes$id), , drop = FALSE]
      viejo <- suppressWarnings(as.numeric(antes$amount_pago))
      d <- abs(suppressWarnings(as.numeric(u$amount_pago)) - viejo)
      umbral <- pmax(PASIVOS_REFRESCO_UMBRAL[["relativo"]] * abs(viejo), PASIVOS_REFRESCO_UMBRAL[["absoluto"]])
      if (any(!is.na(d) & !is.na(umbral) & d > umbral)) actualizadas <- u
    }
    # T120: the payment date of what may move, matched to today's generation
    # by its calculated date (the same occurrence).
    reubicadas <- existentes[0L, , drop = FALSE]
    if (con_politica && nrow(existentes)) {
      fc <- as.Date(existentes$fecha_calculada)
      puede_mover <- existentes$estado %in% "provisional" & !.pasivos_con_override(existentes) &
        !is.na(fc) & fc >= hoy & (length(pasivos_ids_politicas(L)) > 0L | movida_antes) &
        !(existentes$id %in% (politicas$pagadas %||% character()))   # a live payment: never moved
      mov <- existentes[puede_mover, , drop = FALSE]
      if (nrow(mov)) {
        k <- match(paste(mov$origin, as.Date(mov$fecha_calculada)), paste(g$origin, as.Date(g$fecha_calculada)))
        f_nueva <- as.Date(g$fecha_efectiva[k]); f_vieja <- as.Date(mov$fecha_efectiva)
        p_nueva <- as.character(g$policy_ids[k])
        cambia <- !is.na(k) & !is.na(f_nueva) & (is.na(f_vieja) | f_nueva != f_vieja)
        if (any(cambia)) {
          reubicadas <- mov[cambia, , drop = FALSE]
          kk <- k[cambia]
          reubicadas$fecha_efectiva <- f_nueva[cambia]
          reubicadas$policy_ids     <- p_nueva[cambia]
          # the exchange rate and the document at the new date, from the
          # generator itself (the wizard's save gives the same row)
          for (cl in intersect(PASIVOS_CAMPOS_FECHA_PAGO, intersect(names(g), names(reubicadas))))
            reubicadas[[cl]] <- g[[cl]][kk]
        }
      }
    }
    if (!nrow(nuevas) && !nrow(actualizadas) && !nrow(reubicadas)) next
    if (nrow(actualizadas)) {
      k <- match(actualizadas$id, tabla$id)
      for (cl in intersect(names(tabla), names(actualizadas))) tabla[[cl]][k] <- actualizadas[[cl]]
    }
    if (nrow(reubicadas)) {
      k <- match(reubicadas$id, tabla$id)
      for (cl in intersect(c("fecha_efectiva", "policy_ids", PASIVOS_CAMPOS_FECHA_PAGO), names(tabla)))
        tabla[[cl]][k] <- reubicadas[[cl]]
    }
    if (nrow(nuevas)) tabla <- dplyr::bind_rows(tabla, nuevas)
    cambios <- rbind(cambios, data.frame(
      liability_id = L$id, nombre = as.character(L$nombre[1] %||% NA_character_),
      motivo = if (nrow(reubicadas) && !nrow(actualizadas) && !nrow(nuevas)) "politica"
               else if (variable) "tasa" else "horizonte",
      actualizadas = nrow(actualizadas), nuevas = nrow(nuevas), reubicadas = nrow(reubicadas),
      stringsAsFactors = FALSE))
  }
  list(tabla = tabla, cambios = cambios)
}

# The day each client was last brought up to date, in this process. Keyed by
# client (never one global value for every session). A new process refreshes
# again once: harmless, the write is idempotent (nothing to do = no write).
# On S3 it would be a logged write per client every day (T72) to save only
# the computation.
.PASIVOS_REFRESCO_HECHO <- new.env(parent = emptyenv())

# Brings the client's provisions up to date once a day. Reads the
# liabilities and the provisions strictly (a failed read stops it: never
# taken as "nothing there"); with nothing to change it returns before any
# write. Otherwise it writes with modificar_tabla() over S3 as it is now (the
# change is computed again on that copy: another session's write meanwhile
# is kept, and one that already did it leaves nothing to do), and each
# liability brought up to date gets its line in the Pasivos history
# (pasivos_log_audit, which also reaches Actividad).
# Returns list(hecho, cambios, tabla): `tabla` is what S3 holds after it
# (NULL when nothing was written).
pasivos_refrescar_provisiones <- function(client_id, hoy = hoy_en(), forzar = FALSE) {
  if (is.null(client_id) || !nzchar(client_id))
    stop("pasivos_refrescar_provisiones: sin client_id", call. = FALSE)
  hoy <- as.Date(hoy)
  cid <- tolower(trimws(client_id))
  if (!isTRUE(forzar) && identical(.PASIVOS_REFRESCO_HECHO[[cid]], hoy))
    return(list(hecho = FALSE, cambios = NULL, tabla = NULL))
  liabs <- leer_tabla_para_escribir(S3_KEYS$pasivos_liabilities, client_id, .schema_pasivos_liability)
  provs <- leer_tabla_para_escribir(S3_KEYS$pasivos_provisions, client_id, .schema_pasivos_provision)
  # T120: the policies only when a liability or a provision has any; read
  # strictly (a failed read stops the refresh: never "no policies").
  usa <- function(df) !is.null(df) && "policy_ids" %in% names(df) && any(!is.na(df$policy_ids) & nzchar(df$policy_ids))
  politicas <- if (usa(liabs) || usa(provs)) .pasivos_politicas_contexto(client_id) else NULL
  previo <- pasivos_refresco_calcular(liabs, provs, hoy, client_id, politicas)
  if (!nrow(previo$cambios)) {
    assign(cid, hoy, envir = .PASIVOS_REFRESCO_HECHO)
    return(list(hecho = TRUE, cambios = previo$cambios, tabla = NULL))
  }
  cambios <- previo$cambios
  tabla <- modificar_tabla(
    S3_KEYS$pasivos_provisions, client_id, .schema_pasivos_provision,
    function(fresco) {
      r <- pasivos_refresco_calcular(liabs, fresco, hoy, client_id, politicas)
      cambios <<- r$cambios
      if (!nrow(r$cambios)) return(fresco)
      iniciales_vigentes(r$tabla, "pasivos_provisions", client_id)   # CM29, as every provisions save
    },
    evento = evento_sistema("pasivos_refresco", "provisiones_refrescadas",
                            tr("tablas.pasivos.refresco.evento")))
  assign(cid, hoy, envir = .PASIVOS_REFRESCO_HECHO)
  # One history line per liability brought up to date (none when another
  # session had already done it: `cambios` is then empty).
  for (i in seq_len(nrow(cambios))) {
    tryCatch(pasivos_log_audit(
      action_type = if (cambios$motivo[i] %in% c("tasa", "politica")) "provision.edited" else "provision.generated",
      user        = "sistema:pasivos_refresco",
      target_kind = "liability",
      target_id   = cambios$liability_id[i],
      after       = list(motivo = cambios$motivo[i], actualizadas = cambios$actualizadas[i],
                         nuevas = cambios$nuevas[i], reubicadas = cambios$reubicadas[i]),
      notes       = tr("tablas.pasivos.refresco.evento"),
      client_id   = client_id),
      error = function(e) message("[pasivos] refresco: history line failed: ", conditionMessage(e)))
  }
  if (nrow(cambios))
    message(sprintf("[pasivos] refresco %s: %s", client_id,
                    paste(sprintf("%s (%s: %d al día, %d nuevas)", cambios$nombre, cambios$motivo,
                                  cambios$actualizadas, cambios$nuevas), collapse = "; ")))
  list(hecho = TRUE, cambios = cambios, tabla = if (nrow(cambios)) tabla else NULL)
}
