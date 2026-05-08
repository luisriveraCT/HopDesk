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

# Recompute moneda_pago amount from cotizado_en via the active FX modifier.
# Returns list(amount_pago = numeric, fx_rate_used = numeric).
pasivos_recompute_pago_from_cotizado <- function(amount_cotizado, cotizado_en,
                                                  moneda_pago, fecha,
                                                  modifiers = list()) {
  no_conversion <- is.null(cotizado_en) || is.na(cotizado_en) ||
    !nzchar(cotizado_en) || identical(cotizado_en, moneda_pago)
  if (no_conversion) {
    return(list(amount_pago = amount_cotizado, fx_rate_used = NA_real_))
  }
  metric <- paste0("fx_", tolower(cotizado_en), "_", tolower(moneda_pago))
  rate <- tryCatch(
    forecasting_get_estimate(metric, fecha),
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

    biweekly = {
      anchor <- as.Date(recurrence_params$anchor_date)
      # Find the smallest anchor + 14k >= from
      delta <- as.integer(from - anchor)
      k_start <- ceiling(delta / 14)
      if (k_start < 0L) k_start <- 0L
      results <- Date(0)
      k <- k_start
      while (length(results) < n) {
        d <- anchor + k * 14L
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
pasivos_generate_schedule <- function(liability, today = Sys.Date()) {
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

  # ── Variable-rate helpers ──────────────────────────────────────────────────
  .get_period_rate <- function(k, fecha_k) {
    if (startsWith(tasa_tipo, "variable_")) {
      metric_map <- c(
        variable_sofr   = "sofr",
        variable_tiie28 = "tiie28",
        variable_otro   = "otro"
      )
      metric <- metric_map[[tasa_tipo]]
      if (is.null(metric)) metric <- sub("^variable_", "", tasa_tipo)
      est <- tryCatch(forecasting_get_estimate(metric, fecha_k), error = function(e) NA_real_)
      if (is.na(est)) {
        NA_real_  # caller accumulates warning
      } else {
        (1 + (est + spread_bps / 100) / 100)^(1 / ppy) - 1
      }
    } else {
      (1 + tasa / 100)^(1 / ppy) - 1
    }
  }

  # ── Build regular schedule ─────────────────────────────────────────────────
  saldo <- principal
  sched <- vector("list", n)
  variable_na_warned <- FALSE

  for (k in seq_len(n)) {
    fecha_k <- .add_payment_period(base, k, frecuencia, dia_pago,
                                    dia_semana_pago = dia_pago,
                                    mes_pago        = mes_pago_val)
    r_k     <- .get_period_rate(k, fecha_k)

    if (is.na(r_k)) {
      if (!variable_na_warned) {
        warning("[pasivos] variable rate NA for some periods — falling back to tasa_actual")
        variable_na_warned <- TRUE
      }
      r_k <- (1 + (tasa + spread_bps / 100) / 100)^(1 / ppy) - 1
    }

    if (k <= pg) {
      # ── Grace period ─────────────────────────────────────────────────────────
      # Custom fixed amount overrides auto; auto = interest-only (= $0 at 0% tasa)
      cap_k  <- 0
      int_k  <- if (!is.na(monto_gracia_custom) && monto_gracia_custom >= 0)
                  monto_gracia_custom
                else
                  saldo * r_k
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
        int_k  <- saldo * r_k
        cap_k  <- pmt - int_k
        fees_k <- 0
      } else if (identical(metodo, "alemana")) {
        cap_k  <- principal / n_amort
        int_k  <- saldo * r_k
        fees_k <- 0
      } else if (identical(metodo, "americana")) {
        cap_k  <- if (k_a == n_amort) saldo else 0
        int_k  <- principal * r_k
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
      re_amortized = FALSE
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
          r_k     <- .get_period_rate(sched_df$periodo[idx], fecha_k)
          if (is.na(r_k)) r_k <- (1 + tasa / 100)^(1 / ppy) - 1
          int_k   <- if (!is.na(monto_gracia_custom) && monto_gracia_custom >= 0)
                       monto_gracia_custom
                     else
                       s_rem * r_k
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
          r_k     <- .get_period_rate(sched_df$periodo[idx], fecha_k)
          if (is.na(r_k)) r_k <- (1 + tasa / 100)^(1 / ppy) - 1

          if (identical(metodo, "francesa")) {
            rem <- n_rem_amort - i + 1L
            if (r_k == 0) pmt <- s_rem / rem else
              pmt <- s_rem * r_k * (1 + r_k)^rem / ((1 + r_k)^rem - 1)
            int_k <- s_rem * r_k
            cap_k <- pmt - int_k
          } else if (identical(metodo, "alemana")) {
            cap_k <- saldo_declarado / n_rem_amort
            int_k <- s_rem * r_k
          } else {  # americana
            cap_k <- if (i == n_rem_amort) s_rem else 0
            int_k <- saldo_declarado * r_k
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
        moneda     = f$moneda %||% "MXN",
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
  out
}

# ── §2.3 Provision generation ──────────────────────────────────────────────────

pasivos_generate_provisions <- function(liability,
                                         window_start = Sys.Date(),
                                         window_end   = NULL,
                                         policies_for_liability = list(),
                                         holidays_cache = list()) {
  window_start <- as.Date(window_start)
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

  schema <- .schema_pasivos_provision()

  # ── Build raw date+amount stream ─────────────────────────────────────────
  if (identical(cat, "financiero")) {
    sched <- pasivos_generate_schedule(liability)
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
      fecha_ef <- tryCatch(
        compose_policies(fecha_calc, policies_for_liability, holidays_cache),
        error = function(e) fecha_calc
      )
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
          amt_cot, cot_en, row_moneda_pago, fecha_ef
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
        componente_iva       = NA_real_,
        amount_pago_override     = NA_real_,
        amount_cotizado_override = NA_real_,
        fecha_efectiva_override  = as.Date(NA),
        documento            = tmpl,
        referencia           = if (is_fee_row) (row_s[["fee_desc"]][[1L]] %||% "") else (liability$referencia_default %||% NA_character_),
        notas                = NA_character_,
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
    n_approx <- as.integer(ceiling(as.numeric(window_end - window_start) / 14)) + 60L
    dates <- tryCatch(
      pasivos_expand_recurrence(
        liability$recurrence_type,
        liability$recurrence_params[[1]] %||% list(),
        window_start, n_approx
      ),
      error = function(e) as.Date(character(0))
    )
    dates <- dates[dates >= window_start & dates <= window_end]
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
      fecha_ef <- tryCatch(
        compose_policies(fecha_calc, policies_for_liability, holidays_cache),
        error = function(e) fecha_calc
      )
      pol_ids <- if (length(policies_for_liability)) {
        paste(vapply(policies_for_liability, function(p) p$id %||% "", character(1)),
              collapse = ",")
      } else { "" }

      fx_res <- pasivos_recompute_pago_from_cotizado(
        amt_default, cot_en, liability$moneda_pago, fecha_ef
      )

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
        occurrence_index     = as.integer(i),
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
        componente_iva       = NA_real_,
        amount_pago_override     = NA_real_,
        amount_cotizado_override = NA_real_,
        fecha_efectiva_override  = as.Date(NA),
        documento            = tmpl,
        referencia           = liability$referencia_default,
        notas                = NA_character_,
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

pasivos_reconcile_provisions <- function(existing, generated, mode = "regenerate") {
  empty <- existing[0L, , drop = FALSE]

  if (!nrow(existing)) {
    return(list(keep = empty, update = empty, insert = generated,
                conflicts = empty))
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

  if (identical(mode, "extend")) {
    max_idx  <- if (nrow(mutable)) max(mutable$occurrence_index, na.rm = TRUE) else -Inf
    max_date <- if (nrow(mutable)) max(mutable$fecha_efectiva, na.rm = TRUE) else as.Date(-Inf)
    insert_rows <- generated[
      generated$occurrence_index > max_idx |
        (!is.na(generated$fecha_efectiva) & generated$fecha_efectiva > max_date),
      , drop = FALSE
    ]
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
          ov_col <- .override_col(fld)
          # Skip if this field is protected by a non-NA override
          if (!is.na(ov_col) && ov_col %in% names(row_ex) && !is.na(row_ex[[ov_col]])) next
          if (fld %in% names(updated_row)) updated_row[[fld]] <- g[[fld]]
        }
        update_rows <- dplyr::bind_rows(update_rows, updated_row)
      }
    }

    # Rows in generated not in existing → insert
    existing_idxs <- mutable$occurrence_index
    for (i in seq_len(nrow(generated))) {
      g <- generated[i, , drop = FALSE]
      is_new <- !g$occurrence_index %in% existing_idxs
      if (is_new) insert_rows <- dplyr::bind_rows(insert_rows, g)
    }
  }

  list(keep = keep_rows, update = update_rows,
       insert = insert_rows, conflicts = conflict_rows)
}

# ── §2.5 Lifecycle state machine ───────────────────────────────────────────────

# Helpers that load/mutate/save/audit a single provision by id.
# All functions return the updated provision row invisibly.

.load_provision <- function(provision_id) {
  all_provs <- load_pasivos_provisions()
  row <- all_provs[all_provs$id == provision_id, , drop = FALSE]
  if (!nrow(row)) stop("[pasivos] provision not found: ", provision_id)
  row[1L, , drop = FALSE]  # guard against duplicate ids
}

.save_provision_row <- function(row) {
  all_provs <- load_pasivos_provisions()
  idx <- which(all_provs$id == row$id)
  if (length(idx)) {
    all_provs[idx, ] <- row
  } else {
    all_provs <- dplyr::bind_rows(all_provs, row)
  }
  save_pasivos_provisions(all_provs)
  invisible(TRUE)
}

pasivos_provision_convert <- function(provision_id, manual_inv_id, pagar_hoy_id, user) {
  row <- .load_provision(provision_id)
  if (row$estado != "provisional")
    stop("[pasivos] cannot convert provision with estado=", row$estado,
         "; must be 'provisional'")
  before <- as.list(row)
  row$estado        <- "converted"
  row$manual_inv_id <- as.character(manual_inv_id)
  row$pagar_hoy_id  <- as.character(pagar_hoy_id)
  .save_provision_row(row)
  pasivos_log_audit(
    action_type = "provision.converted_to_item",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row)
  )
  invisible(row)
}

pasivos_provision_item_confirmed <- function(provision_id, bancos_conf_id, user) {
  row <- .load_provision(provision_id)
  if (row$estado != "converted")
    stop("[pasivos] cannot confirm provision with estado=", row$estado,
         "; must be 'converted'")
  before <- as.list(row)
  row$estado         <- "item_confirmed"
  row$bancos_conf_id <- as.character(bancos_conf_id)
  .save_provision_row(row)
  pasivos_log_audit(
    action_type = "provision.item_confirmed",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row)
  )
  invisible(row)
}

pasivos_provision_close <- function(provision_id, user) {
  row <- .load_provision(provision_id)
  if (row$estado != "item_confirmed")
    stop("[pasivos] cannot close provision with estado=", row$estado,
         "; must be 'item_confirmed'")
  before <- as.list(row)
  row$estado <- "closed"
  .save_provision_row(row)
  pasivos_log_audit(
    action_type = "provision.item_confirmed",  # reuse closest available action
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row),
    notes       = "closed"
  )
  invisible(row)
}

pasivos_provision_cancel <- function(provision_id, user) {
  row <- .load_provision(provision_id)
  if (row$estado == "closed")
    return(invisible(row))  # already closed, no-op
  before <- as.list(row)
  row$estado <- "closed"
  .save_provision_row(row)
  pasivos_log_audit(
    action_type = "provision.cancelled",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row)
  )
  invisible(row)
}

pasivos_provision_revive <- function(provision_id, user) {
  row <- .load_provision(provision_id)
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
  .save_provision_row(row)
  pasivos_log_audit(
    action_type = "provision.revived",
    user        = user,
    target_kind = "provision",
    target_id   = provision_id,
    before      = before,
    after       = as.list(row)
  )
  invisible(row)
}

# ── §2.6 Modifier composition ──────────────────────────────────────────────────

.metric_for_modifier_type <- function(type, provision) {
  cot <- provision$cotizado_en %||% NA_character_
  pag <- provision$moneda_pago %||% NA_character_
  switch(type,
    fx_rate              = paste0("fx_", tolower(cot), "_", tolower(pag)),
    interest_rate_sofr   = "sofr",
    interest_rate_tiie28 = "tiie28",
    inflation_index      = "inflation",
    custom_multiplier    = NA_character_,
    NA_character_
  )
}

pasivos_apply_modifiers <- function(provision, all_modifiers, fecha = NULL) {
  if (is.null(fecha) || is.na(fecha))
    fecha <- provision$fecha_efectiva

  # Skip all modifiers if top-level amount_pago_override is set
  has_top_override <- !is.na(provision$amount_pago_override)
  if (has_top_override) return(provision)

  # Filter to applicable modifiers (enabled + scope matches)
  prov_id   <- provision$id
  liab_id   <- provision$liability_id

  mods <- all_modifiers[
    !is.na(all_modifiers$enabled) & all_modifiers$enabled &
      (
        all_modifiers$scope_type == "global" |
          (all_modifiers$scope_type == "liability" & !is.na(all_modifiers$scope_id) &
             all_modifiers$scope_id == liab_id) |
          (all_modifiers$scope_type == "provision"  & !is.na(all_modifiers$scope_id) &
             all_modifiers$scope_id == prov_id)
      ),
    , drop = FALSE
  ]

  if (!nrow(mods)) return(provision)

  # Sort by created_at
  mods <- mods[order(mods$created_at), , drop = FALSE]

  for (i in seq_len(nrow(mods))) {
    mod <- mods[i, , drop = FALSE]

    # Get effective value
    value <- if (!is.na(mod$frozen_value)) {
      mod$frozen_value
    } else if (!is.na(mod$estimate_method)) {
      metric <- .metric_for_modifier_type(mod$type, provision)
      if (is.na(metric)) NA_real_ else
        tryCatch(
          forecasting_get_estimate(metric, fecha, method = mod$estimate_method),
          error = function(e) NA_real_
        )
    } else {
      NA_real_
    }

    if (is.na(value)) next

    tgt <- mod$target_field
    typ <- mod$type

    if (typ %in% c("fx_rate", "inflation_index", "custom_multiplier")) {
      if (tgt %in% names(provision)) provision[[tgt]] <- provision[[tgt]] * value

    } else if (typ %in% c("interest_rate_sofr", "interest_rate_tiie28")) {
      # Recompute componente_interes using new rate; recalculate amount_pago
      # value is annual rate in %; period rate uses /12 (monthly assumption).
      # TODO: pass liability frecuencia_pago here to make modifiers frequency-aware.
      r <- value / 12 / 100
      # Need saldo (capital) — use componente_capital as proxy
      cap <- provision$componente_capital
      if (!is.na(cap)) {
        provision$componente_interes <- cap * r
        provision$amount_pago <- provision$componente_capital +
          provision$componente_interes +
          (provision$componente_fees %||% 0)
      }
    }
  }

  provision
}
