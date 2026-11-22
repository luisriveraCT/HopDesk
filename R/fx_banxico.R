# =============================================================================
# R/fx_banxico.R
# The Banxico FIX (USD/MXN, serie SF43718, by date of determination) for
# Panorama's Cinta, Flujo's Exportar suggestion and the Pasivos wizard
# (banxico-fix, 2026-11-12).
#
# One figure for every client -- it is public data -- kept in one global
# cache, hd-admin/fx/banxico_fix.rds (fecha, valor, serie, obtenido_en), one
# row per business day, only growing. fx_fix() reads it; when the cache lacks
# the FIX it expects and BANXICO_TOKEN is set, it asks Banxico for the last
# days through the existing adapter (fcs_fetch_banxico, forecasting) -- at
# most once an hour per process -- and adds what comes back.
#
# Fails closed: no token, Banxico down or answering without a figure, an
# unreadable cache, or a newest FIX older than FX_FIX_MAX_DIAS business days
# all give NULL. Nothing is shown then; no figure is ever made up.
#
# fx_fix() -> NULL or list(valor, fecha, cambio_pct, serie, fuente), kept
# 10 minutes in memory per process (NULL too):
#   cambio_pct  % change against the previous FIX in the cache (NA if none).
# =============================================================================

FX_FIX_SERIE    <- "SF43718"
FX_FIX_LLAVE    <- "fx/banxico_fix.rds"
FX_FIX_MAX_DIAS <- 3L          # business days a FIX may be old and still shown
FX_FIX_HORA     <- "12:30"     # Banxico determines the FIX around 12:00 CDMX
FX_FIX_TZ       <- "America/Mexico_City"
FX_FIX_REINTENTO_SEG <- 3600   # one Banxico request an hour per process, at most
FX_FIX_MEMO_SEG      <- 600    # fx_fix() remembers its answer 10 minutes (NULL too)

.schema_fx_fix <- function() data.frame(fecha = as.Date(character()), valor = numeric(), serie = character(),
                                        obtenido_en = character(), stringsAsFactors = FALSE)
.fx_fix_estado <- new.env(parent = emptyenv())   # last Banxico attempt in this process

.fx_es_habil <- function(d) !format(d, "%u") %in% c("6", "7")

# The FIX that should exist now: today's once it is past FX_FIX_HORA on a
# business day, else the previous business day's.
fx_fix_esperado <- function(ahora = Sys.time()) {
  hoy  <- as.Date(format(ahora, tz = FX_FIX_TZ, "%Y-%m-%d"))
  hora <- format(ahora, tz = FX_FIX_TZ, "%H:%M")
  d <- if (.fx_es_habil(hoy) && hora >= FX_FIX_HORA) hoy else hoy - 1L
  while (!.fx_es_habil(d)) d <- d - 1L
  d
}

# Business days from a to b (a <= b), not counting a.
.fx_dias_habiles <- function(a, b) {
  if (b <= a) return(0L)
  sum(.fx_es_habil(seq(a + 1L, b, by = "day")))
}

.fx_fix_leer <- function() tryCatch({
  df <- leer_hd_admin(FX_FIX_LLAVE, .schema_fx_fix)
  df$fecha <- as.Date(df$fecha); df$valor <- suppressWarnings(as.numeric(df$valor))
  df[!is.na(df$fecha) & !is.na(df$valor) & df$valor > 0, , drop = FALSE]
}, error = function(e) NULL)

# Asks Banxico for the last 14 days and adds the new ones to the cache.
# Returns the cache after that, or NULL on any failure (nothing written).
.fx_fix_bajar <- function(ahora) {
  if (!nzchar(Sys.getenv("BANXICO_TOKEN", ""))) return(NULL)
  hoy <- as.Date(format(ahora, tz = FX_FIX_TZ, "%Y-%m-%d"))
  r <- tryCatch(fcs_fetch_banxico(list(metric_id = "usd_mxn", fetch_params = list(list(serie = FX_FIX_SERIE))),
                                  hoy - 14L, hoy),
                error = function(e) NULL)
  if (is.null(r) || !identical(r$status, "ok") || is.null(r$rows) || !nrow(r$rows)) return(NULL)
  nuevas <- data.frame(fecha = as.Date(r$rows$fecha), valor = as.numeric(r$rows$value), serie = FX_FIX_SERIE,
                       obtenido_en = format(ahora, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ"), stringsAsFactors = FALSE)
  nuevas <- nuevas[!is.na(nuevas$fecha) & !is.na(nuevas$valor) & nuevas$valor > 0 & nuevas$fecha <= hoy, , drop = FALSE]
  if (!nrow(nuevas)) return(NULL)
  # T72-C: a system write, logged as such -- the date, never a rate. Only when
  # a date is added: modificar_hd_admin() skips an unchanged table, so a
  # second call the same day neither writes nor logs.
  ev <- if (exists("evento_sistema", mode = "function"))
    evento_sistema("banxico", "fx_fix", sprintf("FIX de Banxico hasta el %s", format(max(nuevas$fecha))))
  tryCatch(modificar_hd_admin(FX_FIX_LLAVE, .schema_fx_fix, evento = ev, function(df) {
    df$fecha <- as.Date(df$fecha)
    faltan <- nuevas[!nuevas$fecha %in% df$fecha, , drop = FALSE]
    if (!nrow(faltan)) return(df)
    out <- rbind(df, faltan[, names(df), drop = FALSE])
    out[order(out$fecha), , drop = FALSE]
  }), error = function(e) NULL)
}

.fx_fix_calcular <- function(ahora) {
  esperado <- fx_fix_esperado(ahora)
  cache <- .fx_fix_leer()
  if (is.null(cache)) cache <- .schema_fx_fix()   # unreadable: try Banxico, never invent
  if (!any(cache$fecha >= esperado)) {
    ultimo <- .fx_fix_estado$intento
    if (is.null(ultimo) || as.numeric(difftime(ahora, ultimo, units = "secs")) >= FX_FIX_REINTENTO_SEG) {
      .fx_fix_estado$intento <- ahora
      bajado <- .fx_fix_bajar(ahora)
      if (!is.null(bajado)) {
        bajado$fecha <- as.Date(bajado$fecha); bajado$valor <- suppressWarnings(as.numeric(bajado$valor))
        cache <- bajado[!is.na(bajado$fecha) & !is.na(bajado$valor) & bajado$valor > 0, , drop = FALSE]
      }
    }
  }
  hoy <- as.Date(format(ahora, tz = FX_FIX_TZ, "%Y-%m-%d"))
  cache <- cache[cache$fecha <= hoy, , drop = FALSE]
  if (!nrow(cache)) return(NULL)
  cache <- cache[order(cache$fecha), , drop = FALSE]
  i <- nrow(cache)
  if (.fx_dias_habiles(cache$fecha[i], esperado) > FX_FIX_MAX_DIAS) return(NULL)   # too old: not shown
  previo <- if (i > 1L) cache$valor[i - 1L] else NA_real_
  list(valor      = cache$valor[i],
       fecha      = cache$fecha[i],
       cambio_pct = if (!is.na(previo) && previo > 0) (cache$valor[i] / previo - 1) * 100 else NA_real_,
       serie      = FX_FIX_SERIE,
       fuente     = "Banxico FIX")
}

# The last answer is kept FX_FIX_MEMO_SEG in this process, NULL too (the
# integrator, 2026-11-12): Panorama's Cinta redraws every minute in every
# session, and the FIX changes once a day -- a read of S3 per redraw was
# N reads a minute for nothing.
fx_fix <- function(ahora = Sys.time()) {
  m <- .fx_fix_estado$memo
  if (!is.null(m) && as.numeric(difftime(ahora, m$cuando, units = "secs")) < FX_FIX_MEMO_SEG &&
      as.numeric(difftime(ahora, m$cuando, units = "secs")) >= 0) return(m$valor)
  r <- .fx_fix_calcular(ahora)
  .fx_fix_estado$memo <- list(cuando = ahora, valor = r)
  r
}

# ── Other currencies (monedas fase 2: Exportar and Flujo's preview) ────────
# Banxico publishes daily, in pesos per unit, the currencies of the SDR
# basket and a few more. FX_DIVISAS_SERIES is Banxico's own table (checked
# against the SIE API by the integrator, 2026-11-16, figures of 13/11): it is
# a rule of the outside world, like the FIX, and this whole file is an
# exception of tests/test_monedas_guardian.R for that reason. A currency
# outside it has no source: its rate is typed by hand (R/fx_tipos.R).
# USD here is SF46405 (the same day's FIX figure), used only to cross two
# currencies when the base is not the peso; USD/MXN itself is always the FIX.
# Its own global cache, hd-admin/fx/banxico_divisas.rds (fecha, moneda, serie,
# valor, obtenido_en), one row per day and currency, only growing: the FIX's
# cache, in production, is never touched. Same rules as the FIX: one Banxico
# request an hour per process, fails closed, never invents a figure.
FX_BANXICO_EN     <- "MXN"     # Banxico quotes every series in pesos
FX_FIX_MONEDA     <- "USD"     # the FIX is pesos per US dollar
FX_DIVISAS_SERIES <- c(USD = "SF46405", EUR = "SF46410", GBP = "SF46407",
                       JPY = "SF46406", CNY = "SF290383", CAD = "SF60632")
FX_DIVISAS_LLAVE  <- "fx/banxico_divisas.rds"

.schema_fx_divisas <- function() data.frame(fecha = as.Date(character()), moneda = character(), serie = character(),
                                            valor = numeric(), obtenido_en = character(), stringsAsFactors = FALSE)
.fx_divisas_estado <- new.env(parent = emptyenv())

.fx_divisas_limpiar <- function(df) {
  df$fecha <- as.Date(df$fecha); df$valor <- suppressWarnings(as.numeric(df$valor))
  df[!is.na(df$fecha) & !is.na(df$valor) & df$valor > 0 & df$moneda %in% names(FX_DIVISAS_SERIES), , drop = FALSE]
}

.fx_divisas_leer <- function() tryCatch(.fx_divisas_limpiar(leer_hd_admin(FX_DIVISAS_LLAVE, .schema_fx_divisas)),
                                        error = function(e) NULL)

# Asks Banxico for the last 14 days of every series and adds the new rows to
# the cache. Returns the cache after that, or NULL on any failure. A series
# that fails (or says "N/E") is just missing; the others still count.
.fx_divisas_bajar <- function(ahora) {
  if (!nzchar(Sys.getenv("BANXICO_TOKEN", ""))) return(NULL)
  hoy <- as.Date(format(ahora, tz = FX_FIX_TZ, "%Y-%m-%d"))
  obtenido <- format(ahora, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
  partes <- lapply(names(FX_DIVISAS_SERIES), function(m) {
    s <- FX_DIVISAS_SERIES[[m]]
    r <- tryCatch(fcs_fetch_banxico(list(metric_id = paste0("fx_", tolower(m)), fetch_params = list(list(serie = s))),
                                    hoy - 14L, hoy),
                  error = function(e) NULL)
    if (is.null(r) || !identical(r$status, "ok") || is.null(r$rows) || !nrow(r$rows)) return(NULL)
    data.frame(fecha = as.Date(r$rows$fecha), moneda = m, serie = s, valor = suppressWarnings(as.numeric(r$rows$value)),
               obtenido_en = obtenido, stringsAsFactors = FALSE)
  })
  nuevas <- do.call(rbind, partes)
  if (is.null(nuevas)) return(NULL)
  nuevas <- .fx_divisas_limpiar(nuevas)
  nuevas <- nuevas[nuevas$fecha <= hoy, , drop = FALSE]
  if (!nrow(nuevas)) return(NULL)
  tryCatch(modificar_hd_admin(FX_DIVISAS_LLAVE, .schema_fx_divisas, function(df) {
    df$fecha <- as.Date(df$fecha)
    faltan <- nuevas[!paste(nuevas$fecha, nuevas$moneda) %in% paste(df$fecha, df$moneda), , drop = FALSE]
    if (!nrow(faltan)) return(df)
    out <- rbind(df, faltan[, names(df), drop = FALSE])
    out[order(out$fecha, out$moneda), , drop = FALSE]
  }), error = function(e) NULL)
}

.fx_divisas_calcular <- function(ahora) {
  esperado <- fx_fix_esperado(ahora)
  cache <- .fx_divisas_leer()
  if (is.null(cache)) cache <- .schema_fx_divisas()   # unreadable: try Banxico, never invent
  if (!any(cache$fecha >= esperado)) {
    ultimo <- .fx_divisas_estado$intento
    if (is.null(ultimo) || as.numeric(difftime(ahora, ultimo, units = "secs")) >= FX_FIX_REINTENTO_SEG) {
      .fx_divisas_estado$intento <- ahora
      bajado <- .fx_divisas_bajar(ahora)
      if (!is.null(bajado)) cache <- .fx_divisas_limpiar(bajado)
    }
  }
  hoy <- as.Date(format(ahora, tz = FX_FIX_TZ, "%Y-%m-%d"))
  cache <- cache[cache$fecha <= hoy, c("fecha", "moneda", "valor"), drop = FALSE]
  vigente <- vapply(seq_len(nrow(cache)), function(i) .fx_dias_habiles(cache$fecha[i], esperado) <= FX_FIX_MAX_DIAS,
                    logical(1))
  cache <- cache[vigente, , drop = FALSE]
  rownames(cache) <- NULL
  cache[order(cache$fecha, cache$moneda), , drop = FALSE]
}

# The figures still fresh enough to show (no older than FX_FIX_MAX_DIAS
# business days): data.frame(fecha, moneda, valor), pesos per unit; empty
# when there are none. Kept FX_FIX_MEMO_SEG in this process, like fx_fix().
fx_divisas <- function(ahora = Sys.time()) {
  m <- .fx_divisas_estado$memo
  if (!is.null(m) && as.numeric(difftime(ahora, m$cuando, units = "secs")) < FX_FIX_MEMO_SEG &&
      as.numeric(difftime(ahora, m$cuando, units = "secs")) >= 0) return(m$valor)
  r <- .fx_divisas_calcular(ahora)
  .fx_divisas_estado$memo <- list(cuando = ahora, valor = r)
  r
}

# ── Approximate amounts, for display only (banxico-fix 3/3, Luis) ──────────
# A provision quoted in USD and paid in MXN shows its amount converted with
# today's FIX, marked approximate ("≈"), wherever the converted amount is
# displayed (Pasivos' table and detail, Flujo's preview and Exportar). It is
# never written anywhere: the stored amount -- and Importe, part of the
# Agenda's key -- stay as they are. A hand-set amount (override) is not
# approximate. Without a FIX nothing is approximated: it shows as before.
#
# The FIX for display: NULL when fx_fix() is missing or fails.
.fx_para_mostrar <- function()
  if (exists("fx_fix", mode = "function")) tryCatch(fx_fix(), error = function(e) NULL) else NULL

.fx_valido <- function(fx) is.list(fx) && is.numeric(fx$valor) && length(fx$valor) == 1L &&
  !is.na(fx$valor) && fx$valor > 0

# Vectorised over provisions: list(aprox = logical, importe_aprox = numeric).
fx_monto_aprox <- function(amount_cotizado, cotizado_en, moneda_pago, override, fx) {
  cot <- toupper(as.character(cotizado_en)); pag <- toupper(as.character(moneda_pago))
  monto <- suppressWarnings(as.numeric(amount_cotizado))
  aprox <- .fx_valido(fx) & !is.na(cot) & cot == "USD" & !is.na(pag) & pag == "MXN" &
           is.na(suppressWarnings(as.numeric(override))) & !is.na(monto)
  aprox[is.na(aprox)] <- FALSE
  list(aprox = aprox, importe_aprox = ifelse(aprox, monto * (if (.fx_valido(fx)) fx$valor else NA_real_), NA_real_))
}

# "Aproximado: FIX del {fecha} ({valor})", for a tooltip next to the "≈".
fx_aprox_ayuda <- function(fx) if (!.fx_valido(fx)) "" else
  tr("tablas.fx.aprox_ayuda", fecha = format(as.Date(fx$fecha), "%d/%m/%Y"),
     valor = formatC(fx$valor, format = "f", digits = 4))

# ── The other screens (fix-aprox-pantallas): Calendario, Día, Vencidos, Lupa ─
# "Incluye montos aproximados (FIX del {fecha})": the help of a total that
# adds an approximate amount (the Calendar's bubble, Día's Resumen, the
# tables' footer sum).
fx_ayuda_total <- function(fx) if (!.fx_valido(fx)) "" else
  tr("tablas.fx.aprox_total", fecha = format(as.Date(fx$fecha), "%d/%m/%Y"))

# The amount to SHOW for rows of the ledger: the approximate one where the
# provision bridge marked it (aprox, importe_aprox) and there is a FIX, the
# real one elsewhere. list(ver, aprox = per-row help or "", aprox_total).
# Display only: actions, keys and the Agenda keep reading Importe.
fx_columna_ver <- function(importe, aprox, importe_aprox, fx = .fx_para_mostrar()) {
  n <- length(importe)
  ayuda <- fx_aprox_ayuda(fx)
  ap <- rep_len(as.logical(aprox %||% NA), n); ia <- rep_len(suppressWarnings(as.numeric(importe_aprox %||% NA)), n)
  ax <- ap %in% TRUE & !is.na(ia) & nzchar(ayuda)
  list(ver = ifelse(ax, ia, as.numeric(importe)), aprox = ifelse(ax, ayuda, ""),
       aprox_total = if (any(ax)) fx_ayuda_total(fx) else "")
}

# ── The FIX's pair (T33, monedas fase 3) ─────────────────────────────────────
# Banxico's FIX is pesos per US dollar and nothing else: FX_FIX_MONEDA and
# FX_BANXICO_EN (above, monedas fase 2) are its pair.

# TRUE when an amount quoted in `de` and paid in `a` converts with the FIX.
fx_fix_aplica <- function(de, a) {
  norm <- function(x) toupper(trimws(as.character(if (length(x)) x[1] else NA_character_)))
  isTRUE(norm(de) == FX_FIX_MONEDA) && isTRUE(norm(a) == FX_BANXICO_EN)
}
