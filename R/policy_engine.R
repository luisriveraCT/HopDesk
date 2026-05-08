# =============================================================================
# R/policy_engine.R
# Payment policy algorithm functions.
# All functions are pure: no reactives, no S3 calls, no side effects.
# =============================================================================

# ── Easter algorithm (Gregorian / Western) ─────────────────────────────────────
.easter_sunday <- function(year) {
  y <- as.integer(year)
  a <- y %% 19L
  b <- y %/% 100L
  c <- y %% 100L
  d <- b %/% 4L
  e <- b %% 4L
  f <- (b + 8L) %/% 25L
  g <- (b - f + 1L) %/% 3L
  h <- (19L * a + b - d - g + 15L) %% 30L
  i <- c %/% 4L
  k <- c %% 4L
  l <- (32L + 2L * e + 2L * i - h - k) %% 7L
  m <- (a + 11L * h + 22L * l) %/% 451L
  mo  <- (h + l - 7L * m + 114L) %/% 31L
  day <- (h + l - 7L * m + 114L) %% 31L + 1L
  as.Date(sprintf("%04d-%02d-%02d", y, mo, day))
}

# ── Nth weekday in a month ─────────────────────────────────────────────────────
# wday uses lubridate week_start=1 convention (1=Mon ... 7=Sun).
# n=-1 returns the last occurrence in the month.
.nth_weekday <- function(year, month, n, wday) {
  if (n > 0L) {
    first     <- as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
    wd1       <- lubridate::wday(first, week_start = 1L)
    days_fwd  <- (as.integer(wday) - wd1) %% 7L
    first_hit <- first + days_fwd
    first_hit + (n - 1L) * 7L
  } else {
    last  <- as.Date(lubridate::ceiling_date(
      as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month))), "month"
    ) - 1L)
    wdl       <- lubridate::wday(last, week_start = 1L)
    days_back <- (wdl - as.integer(wday)) %% 7L
    last - days_back
  }
}

# ── US observed holiday (Sat → Fri, Sun → Mon) ────────────────────────────────
.us_observed <- function(date) {
  wd <- lubridate::wday(as.Date(date), week_start = 1L)
  if (wd == 6L) as.Date(date) - 1L       # Saturday → Friday
  else if (wd == 7L) as.Date(date) + 1L  # Sunday   → Monday
  else as.Date(date)
}

# ── Holiday presets ────────────────────────────────────────────────────────────
# Returns a sorted Date vector of banking-calendar holidays.
# Supported: "MX" (Mexico LFT + CNBV), "US" (federal), "FR" (France)
# Works for any year range, not just 2025–2027.

# overrides: optional data.frame with cols date (Date), country (chr), action ("add"|"remove")
get_holidays <- function(country = "MX", years = NULL, overrides = NULL) {
  if (is.null(years)) {
    cur <- as.integer(format(Sys.Date(), "%Y"))
    years <- (cur - 1L):(cur + 1L)
  }
  country <- toupper(trimws(country))
  hols <- unlist(lapply(as.integer(years), function(y) {
    switch(country,
      MX = {
        easter <- .easter_sunday(y)
        c(
          as.Date(sprintf("%d-01-01", y)),        # Año Nuevo
          .nth_weekday(y,  2L,  1L, 1L),          # 1er lunes feb — Constitución
          .nth_weekday(y,  3L,  3L, 1L),          # 3er lunes mar — Juárez
          as.Date(sprintf("%d-05-01", y)),         # Día del Trabajo
          easter - 3L,                             # Jueves Santo (CNBV)
          easter - 2L,                             # Viernes Santo (LFT + CNBV)
          as.Date(sprintf("%d-09-16", y)),         # Independencia
          .nth_weekday(y, 11L,  3L, 1L),          # 3er lunes nov — Revolución
          as.Date(sprintf("%d-12-25", y))          # Navidad
        )
      },
      US = {
        c(
          .us_observed(sprintf("%d-01-01", y)),    # New Year's Day
          .nth_weekday(y,  1L,  3L, 1L),          # MLK Day  (3rd Mon Jan)
          .nth_weekday(y,  2L,  3L, 1L),          # Presidents' Day (3rd Mon Feb)
          .nth_weekday(y,  5L, -1L, 1L),          # Memorial Day (last Mon May)
          .us_observed(sprintf("%d-06-19", y)),    # Juneteenth
          .us_observed(sprintf("%d-07-04", y)),    # Independence Day
          .nth_weekday(y,  9L,  1L, 1L),          # Labor Day (1st Mon Sep)
          .nth_weekday(y, 10L,  2L, 1L),          # Columbus Day (2nd Mon Oct)
          .us_observed(sprintf("%d-11-11", y)),    # Veterans Day
          .nth_weekday(y, 11L,  4L, 4L),          # Thanksgiving (4th Thu Nov)
          .us_observed(sprintf("%d-12-25", y))     # Christmas
        )
      },
      FR = {
        easter <- .easter_sunday(y)
        c(
          as.Date(sprintf("%d-01-01", y)),         # Nouvel An
          easter + 1L,                             # Lundi de Pâques
          as.Date(sprintf("%d-05-01", y)),         # Fête du Travail
          as.Date(sprintf("%d-05-08", y)),         # Victoire 1945
          easter + 39L,                            # Ascension
          easter + 50L,                            # Lundi de Pentecôte
          as.Date(sprintf("%d-07-14", y)),         # Fête Nationale
          as.Date(sprintf("%d-08-15", y)),         # Assomption
          as.Date(sprintf("%d-11-01", y)),         # Toussaint
          as.Date(sprintf("%d-11-11", y)),         # Armistice
          as.Date(sprintf("%d-12-25", y))          # Noël
        )
      },
      as.Date(character())  # unknown country → empty
    )
  }))
  result <- sort(unique(as.Date(hols)))

  if (!is.null(overrides) && is.data.frame(overrides) && nrow(overrides) > 0) {
    ov <- overrides[toupper(trimws(overrides$country)) == country, , drop = FALSE]
    if (nrow(ov)) {
      adds    <- as.Date(ov$date[ov$action == "add"])
      removes <- as.Date(ov$date[ov$action == "remove"])
      result  <- sort(unique(c(setdiff(result, removes), adds)))
    }
  }

  result
}

# ── Rolling helpers ─────────────────────────────────────────────────────────────

# Roll d in direction until it lands on an allowed weekday not in holidays.
# allowed_wdays: 1=Mon ... 5=Fri (lubridate week_start=1); NULL/empty = any weekday.
.roll_to_weekday <- function(d, direction = "forward",
                              allowed_wdays = NULL,
                              holidays = as.Date(character())) {
  if (is.null(allowed_wdays) || !length(allowed_wdays)) allowed_wdays <- 1L:5L
  delta <- if (identical(direction, "forward")) 1L else -1L
  for (i in seq_len(28L)) {
    wd <- lubridate::wday(d, week_start = 1L)
    if (wd %in% allowed_wdays && !d %in% holidays) return(d)
    d <- d + delta
  }
  warning("[policy_engine] .roll_to_weekday exceeded 28 iterations — check holiday data")
  d
}

# Roll d in direction until it is not in holidays (ignores weekday constraint).
.roll_past_holidays <- function(d, direction = "forward",
                                 holidays = as.Date(character())) {
  if (!length(holidays) || !d %in% holidays) return(d)
  delta <- if (identical(direction, "forward")) 1L else -1L
  for (i in seq_len(28L)) {
    if (!d %in% holidays) return(d)
    d <- d + delta
  }
  d
}

# ── Individual policy algorithm functions ──────────────────────────────────────

# Shift date by params$n calendar or business days, then roll to a valid weekday.
# params$unit: "naturales" (calendar days, default) | "habiles" (Mon–Fri only)
apply_offset_policy <- function(date, params = list(),
                                 roll_direction = "forward",
                                 holidays = as.Date(character())) {
  n    <- as.integer(if (!is.null(params$n) && length(params$n)) params$n else 0L)
  unit <- if (!is.null(params$unit) && nzchar(params$unit)) params$unit else "naturales"
  d    <- as.Date(date)

  result <- if (unit == "habiles" && n != 0L) {
    delta     <- if (n > 0L) 1L else -1L
    remaining <- abs(n)
    cur       <- d
    for (i in seq_len(abs(n) * 4L + 10L)) {
      cur <- cur + delta
      if (lubridate::wday(cur, week_start = 1L) %in% 1L:5L) {
        remaining <- remaining - 1L
        if (remaining == 0L) break
      }
    }
    cur
  } else {
    d + n
  }

  .roll_to_weekday(result, roll_direction, 1L:5L, holidays)
}

# Move to last calendar day of the month (no rolling — pure calendar boundary).
apply_last_day_policy <- function(date, params = list(),
                                   roll_direction = "forward",
                                   holidays = as.Date(character())) {
  d <- lubridate::floor_date(as.Date(date), "month")
  as.Date(lubridate::ceiling_date(d, "month") - 1L)
}

# Move to the scheduled day-of-month listed in params$days, then roll to a valid
# weekday.  Direction determines whether we look forward (next scheduled day ≥ d)
# or backward (most recent scheduled day ≤ d).
# Clamps to last day of month for short months (e.g. day 31 in Feb).
apply_month_days_policy <- function(date, params = list(),
                                     roll_direction = "forward",
                                     holidays = as.Date(character())) {
  d    <- as.Date(date)
  days <- sort(unique(as.integer(
    if (!is.null(params$days) && length(params$days)) params$days else c(15L, 30L)
  )))

  month_floor <- lubridate::floor_date(d, "month")
  month_end   <- as.Date(lubridate::ceiling_date(month_floor, "month") - 1L)
  max_day     <- lubridate::day(month_end)
  candidates  <- as.Date(paste0(
    format(month_floor, "%Y-%m-"),
    sprintf("%02d", pmin(days, max_day))
  ))

  target <- if (identical(roll_direction, "backward")) {
    past <- candidates[candidates <= d]
    if (length(past)) {
      max(past)
    } else {
      # All fixed days are ahead — go to last qualifying day in previous month
      prev_start   <- lubridate::floor_date(month_floor - 1L, "month")
      prev_end     <- as.Date(lubridate::ceiling_date(prev_start, "month") - 1L)
      prev_max_day <- lubridate::day(prev_end)
      target_day   <- max(pmin(days, prev_max_day))
      as.Date(paste0(format(prev_start, "%Y-%m-"), sprintf("%02d", target_day)))
    }
  } else {
    future <- candidates[candidates >= d]
    if (length(future)) {
      min(future)
    } else {
      # All targets already passed — go to first qualifying day in next month
      next_start   <- lubridate::ceiling_date(d, "month")
      next_end     <- as.Date(lubridate::ceiling_date(next_start, "month") - 1L)
      next_max_day <- lubridate::day(next_end)
      target_day   <- min(pmin(days, next_max_day))
      as.Date(paste0(format(as.Date(next_start), "%Y-%m-"), sprintf("%02d", target_day)))
    }
  }

  .roll_to_weekday(target, roll_direction, 1L:5L, holidays)
}

# Roll to an allowed weekday.
# params$days: integer vector of allowed wdays (1=Mon...5=Fri); NULL/empty = any weekday.
# Also skips holidays if provided (allows composing without a separate skip_holidays policy).
apply_weekday_policy <- function(date, params = list(),
                                  roll_direction = "forward",
                                  holidays = as.Date(character())) {
  allowed <- if (!is.null(params$days) && length(params$days)) {
    as.integer(params$days)
  } else {
    NULL  # .roll_to_weekday treats NULL as any weekday
  }
  .roll_to_weekday(as.Date(date), roll_direction, allowed, holidays)
}

# Roll past holidays (no weekday requirement).
# params$country is resolved to a Date vector by compose_policies.
apply_skip_holidays_policy <- function(date, params = list(),
                                        roll_direction = "forward",
                                        holidays = as.Date(character())) {
  .roll_past_holidays(as.Date(date), roll_direction, holidays)
}

# ── Composition internals ───────────────────────────────────────────────────────

.POLICY_TYPE_ORDER <- c(
  "offset_days"   = 1L,
  "last_day"      = 2L,
  "month_days"    = 2L,
  "weekdays"      = 3L,
  "skip_holidays" = 3L
)

.get_policy_holidays <- function(pol, holidays_cache) {
  country <- pol$params$country
  if (is.null(country) || !nzchar(country)) return(as.Date(character()))
  hc <- holidays_cache[[country]]
  if (!is.null(hc)) hc else tryCatch(get_holidays(country), error = function(e) as.Date(character()))
}

.apply_single_policy <- function(d, pol, holidays) {
  switch(pol$type,
    "offset_days"   = apply_offset_policy(d, pol$params, pol$roll_direction, holidays),
    "last_day"      = apply_last_day_policy(d, pol$params, pol$roll_direction, holidays),
    "month_days"    = apply_month_days_policy(d, pol$params, pol$roll_direction, holidays),
    "weekdays"      = apply_weekday_policy(d, pol$params, pol$roll_direction, holidays),
    "skip_holidays" = apply_skip_holidays_policy(d, pol$params, pol$roll_direction, holidays),
    d  # unknown type → pass through
  )
}

# ── Policy composition ──────────────────────────────────────────────────────────
# Apply a list of policies to a single date in spec-mandated order:
#   1. offset_days  → 2. last_day / month_days  → 3. weekdays / skip_holidays
#
# policies: list of named lists, each with: type (chr), params (list), roll_direction (chr)
# holidays_cache: named list  country_code → Date vector

compose_policies <- function(date, policies, holidays_cache = list()) {
  if (!length(policies) || is.na(date)) return(as.Date(date))

  filter_types <- c("weekdays", "skip_holidays")
  orders <- vapply(policies, function(p) {
    v <- .POLICY_TYPE_ORDER[p$type]
    if (is.na(v)) 9L else as.integer(v)
  }, integer(1))
  policies <- policies[order(orders, seq_along(policies))]

  non_filters <- Filter(function(p) !p$type %in% filter_types, policies)
  filters     <- Filter(function(p)  p$type %in% filter_types, policies)

  d <- as.Date(date)

  # Phase 1+2: offsets then transformations (single pass)
  for (pol in non_filters) {
    d <- .apply_single_policy(d, pol, .get_policy_holidays(pol, holidays_cache))
  }

  # Phase 3: filters — stability loop (Holy Week needs up to 3 passes)
  if (length(filters)) {
    for (pass in seq_len(4L)) {
      prev <- d
      for (pol in filters) {
        d <- .apply_single_policy(d, pol, .get_policy_holidays(pol, holidays_cache))
      }
      if (isTRUE(d == prev)) break
    }
  }

  d
}

# ── Application engine ──────────────────────────────────────────────────────────
# Compute FechaVenc_Politica for every invoice that has a matching policy assignment.
# Returns a tibble compatible with .schema_policy_moves() (applied_by / applied_at stamped here).
#
# invoices_df        must have: Empresa, Moneda, Documento, Parte, FechaVenc_Original
# partner_policies_df  schema: parte, policy_id, policy_order, ledger, ...
# policy_catalog_df    schema: id, type, params (list col), roll_direction, ...

compute_policy_moves <- function(invoices_df,
                                  partner_policies_df,
                                  policy_catalog_df,
                                  holidays_cache = list(),
                                  overrides_df   = NULL,
                                  ledger         = "AP",
                                  applied_by     = "system",
                                  applied_at     = NULL) {

  empty_out <- tibble::tibble(
    ledger             = character(),
    Empresa            = character(),
    Moneda             = character(),
    Documento          = character(),
    Parte              = character(),
    FechaVenc_Politica = as.Date(character()),
    policy_ids         = character(),
    applied_by         = character(),
    applied_at         = as.POSIXct(character())
  )

  if (is.null(invoices_df)       || !nrow(invoices_df)       ||
      is.null(partner_policies_df) || !nrow(partner_policies_df) ||
      is.null(policy_catalog_df) || !nrow(policy_catalog_df)) {
    return(empty_out)
  }

  if (is.null(applied_by) || !nzchar(applied_by %||% "")) applied_by <- "system"
  if (is.null(applied_at)) applied_at <- Sys.time()

  # Filter partner_policies to matching ledger ("", NA = both ledgers)
  pp <- partner_policies_df |>
    dplyr::filter(
      .data$ledger == !!ledger |
      !nzchar(.data$ledger %||% "") |
      is.na(.data$ledger)
    ) |>
    dplyr::arrange(.data$parte, .data$policy_order)

  if (!nrow(pp)) return(empty_out)

  # Build policy object lookup: id → list(type, params, roll_direction)
  pol_lookup <- setNames(
    lapply(seq_len(nrow(policy_catalog_df)), function(i) {
      list(
        type           = policy_catalog_df$type[[i]],
        params         = policy_catalog_df$params[[i]] %||% list(),
        roll_direction = policy_catalog_df$roll_direction[[i]] %||% "forward"
      )
    }),
    policy_catalog_df$id
  )

  # Pre-populate holidays_cache for all countries referenced in policies
  needed_countries <- unique(unlist(lapply(pol_lookup, function(p) {
    p$params$country %||% NULL
  })))
  for (ctry in needed_countries) {
    if (nzchar(ctry) && !ctry %in% names(holidays_cache)) {
      holidays_cache[[ctry]] <- tryCatch(
        get_holidays(ctry, overrides = overrides_df),
        error = function(e) as.Date(character())
      )
    }
  }

  # Normalise invoice columns — raw SAP data uses different column names than the
  # canonical schema (e.g. "Divisa" → "Moneda", "Nº de documento" → "Documento",
  # "Nombre del cliente" → "Parte").  Guards ensure we never overwrite a column
  # that is already present with the correct name (keeps unit tests stable).
  if (!"Parte" %in% names(invoices_df))
    invoices_df <- tryCatch(
      standardize_party(invoices_df, ledger),
      error = function(e) { invoices_df$Parte <- NA_character_; invoices_df }
    )

  if (!"Moneda" %in% names(invoices_df))
    invoices_df <- tryCatch(
      ensure_moneda(invoices_df),
      error = function(e) { invoices_df$Moneda <- "MXN"; invoices_df }
    )

  if (!"Documento" %in% names(invoices_df))
    invoices_df <- tryCatch(
      ensure_documento(invoices_df),
      error = function(e) { invoices_df$Documento <- NA_character_; invoices_df }
    )

  if (!"Empresa" %in% names(invoices_df))
    invoices_df$Empresa <- NA_character_

  if (!"FechaVenc_Original" %in% names(invoices_df)) {
    invoices_df <- tryCatch(ensure_dates(invoices_df), error = function(e) invoices_df)
    fv_col <- intersect(c("Fecha de vencimiento", "FechaVenc"), names(invoices_df))
    invoices_df$FechaVenc_Original <- if (length(fv_col))
      tryCatch(as.Date(invoices_df[[fv_col[[1L]]]]), error = function(e) as.Date(NA))
    else
      as.Date(NA)
  }

  # Join invoices → partner_policies on Parte (case-insensitive + trim)
  inv_keyed <- invoices_df |>
    dplyr::select(dplyr::any_of(
      c("Empresa", "Moneda", "Documento", "Parte", "FechaVenc_Original")
    )) |>
    dplyr::mutate(.pk = tolower(trimws(.data$Parte)))

  pp_keyed <- pp |>
    dplyr::select("parte", "policy_id", "policy_order") |>
    dplyr::mutate(.pk = tolower(trimws(.data$parte)))

  # relationship = "many-to-many" is required when a partner has multiple
  # stacked policy steps (pp_keyed has N rows per partner × M invoices per
  # partner in inv_keyed).  Without this flag, dplyr 1.1.1+ throws an error.
  joined <- inv_keyed |>
    dplyr::inner_join(pp_keyed, by = ".pk", relationship = "many-to-many") |>
    dplyr::select(-dplyr::any_of(c(".pk", "parte")))

  if (!nrow(joined)) return(empty_out)

  # Collapse policies per invoice (ordered by policy_order)
  invoice_policies <- joined |>
    dplyr::arrange(.data$Empresa, .data$Moneda, .data$Documento, .data$policy_order) |>
    dplyr::group_by(.data$Empresa, .data$Moneda, .data$Documento,
                    .data$Parte, .data$FechaVenc_Original) |>
    dplyr::summarise(
      policy_ids      = paste(.data$policy_id, collapse = ","),
      policy_ids_list = list(.data$policy_id),
      .groups         = "drop"
    )

  # Apply composed policies per invoice (vectorised via vapply)
  computed_int <- vapply(
    seq_len(nrow(invoice_policies)),
    function(i) {
      fecha <- invoice_policies$FechaVenc_Original[[i]]
      if (is.na(fecha)) return(NA_integer_)
      ids  <- invoice_policies$policy_ids_list[[i]]
      pols <- Filter(Negate(is.null), lapply(ids, function(id) pol_lookup[[id]]))
      if (!length(pols)) return(as.integer(fecha))
      as.integer(compose_policies(fecha, pols, holidays_cache))
    },
    integer(1)
  )

  out <- invoice_policies
  out$ledger             <- ledger
  out$FechaVenc_Politica <- as.Date(computed_int, origin = "1970-01-01")
  out$applied_by         <- applied_by
  out$applied_at         <- applied_at

  out <- out[!is.na(out$FechaVenc_Politica), , drop = FALSE]

  out[, c("ledger", "Empresa", "Moneda", "Documento", "Parte",
          "FechaVenc_Politica", "policy_ids", "applied_by", "applied_at"),
      drop = FALSE]
}
