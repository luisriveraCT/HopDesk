# =============================================================================
# R/mercado.R -- T116 F2: the market data layer's countries and the official
# non-working days the HopDesk staff capture.
#
# MERCADO_PAISES: the countries a client can operate in. `calendario` is the
# code get_holidays() (R/policy_engine.R, still the only door) takes. Rates
# and exchange rates per country come in phase 3.
#
# A client's countries: group_config$paises; empty = the country of its base
# currency (MXN -> MX, USD -> US, EUR -> EU), so nothing is written for the
# clients that already exist.
#
# The official calendar: hd-admin/mercado/inhabiles.rds, ONE table for every
# client (public data). Only grows: a wrong row is voided (anulado_*), never
# deleted. Two kinds of rows:
#   tipo "dia"   one non-working day (pais, fecha, motivo, fuente)
#   tipo "anio"  "the staff checked this country's year against the official
#                publication and it is complete" (pais, anio)
# Only a CONFIRMED year replaces the rule's dates in get_holidays(); a year
# half captured changes nothing. Captured only by HopDesk staff
# (can_capture_market_calendar, an internal key: R/settings/settings_mercado.R).
#
# In memory: .MERCADO_INHABILES. mercado_inhabiles_oficiales() only reads it
# (get_holidays() stays pure); mercado_inhabiles_refrescar() is the one that
# reads S3, at most once an hour per process (the fx_fix() pattern). A failed
# read keeps what was there (or nothing): never an empty table posing as the
# real one, and get_holidays() then gives the rule.
# =============================================================================

# tasas (T116 F3): the reference rates of each market shown in Panorama's
# «Tasas», by metric_id (R/forecasting_metric_registry.R, R/mercado_datos.R).
MERCADO_PAISES <- list(
  MX = list(nombre = "comun.mercado.pais.mx", moneda = "MXN", calendario = "MX",
            tasas = c("tiie28", "cetes28")),
  US = list(nombre = "comun.mercado.pais.us", moneda = "USD", calendario = "US",
            tasas = c("sofr", "fed_funds", "prime")),
  EU = list(nombre = "comun.mercado.pais.eu", moneda = "EUR", calendario = "EU",
            tasas = c("estr"))
)

# The market's quoting order (T116 F3): a pair is written "A/B" with A the one
# that comes first here -- EUR/USD, USD/MXN, EUR/MXN, GBP/USD -- and its value
# is B per one A. A currency outside the list goes after them all.
MERCADO_ORDEN_COTIZACION <- c("EUR", "GBP", "USD", "CAD", "CHF", "JPY", "CNY", "MXN")
MERCADO_BCE_EN <- "EUR"   # the ECB quotes every currency per euro

mercado_pais_valido <- function(x) {
  x <- toupper(trimws(as.character(x)))
  !is.na(x) & x %in% names(MERCADO_PAISES)
}

# The country whose currency is `moneda`; NA when none is.
mercado_pais_de_moneda <- function(moneda) {
  m <- toupper(trimws(as.character(moneda %||% NA_character_)[1]))
  hit <- names(MERCADO_PAISES)[vapply(MERCADO_PAISES, function(p) identical(p$moneda, m), logical(1))]
  if (length(hit)) hit[1] else NA_character_
}

# A client's countries from its group config: the chosen ones (valid codes
# only, in registry order), or else the country of its base currency (the
# default base when it has none). Never NA; empty when no country fits.
mercado_paises_cliente <- function(cfg) {
  p <- toupper(trimws(as.character(cfg$paises %||% character())))
  p <- p[mercado_pais_valido(p)]
  if (length(p)) return(intersect(names(MERCADO_PAISES), p))
  mb <- cfg$moneda_base
  if (is.null(mb) || length(mb) != 1L || is.na(mb) || !nzchar(trimws(mb)))
    mb <- if (exists("MONEDA_BASE_OMISION")) MONEDA_BASE_OMISION else NA_character_
  pais <- mercado_pais_de_moneda(mb)
  if (is.na(pais)) character() else pais
}

mercado_pais_nombre <- function(pais, idioma = NULL) {
  p <- MERCADO_PAISES[[toupper(trimws(pais))]]
  if (is.null(p)) return(pais)
  if (is.null(idioma)) tr(p$nombre) else tr(p$nombre, idioma = idioma)
}

# ── The official calendar ────────────────────────────────────────────────────
.MERCADO_INHABILES_KEY <- "mercado/inhabiles.rds"
.MERCADO_TIPOS <- c("dia", "anio")

.schema_mercado_inhabiles <- function() tibble::tibble(
  id            = character(),
  tipo          = character(),     # "dia" | "anio"
  pais          = character(),
  fecha         = as.Date(character()),   # NA on an "anio" row
  anio          = integer(),
  motivo        = character(),
  fuente        = character(),
  capturado_por = character(),
  capturado_en  = as.POSIXct(character(), tz = "UTC"),
  anulado_por   = character(),
  anulado_en    = as.POSIXct(character(), tz = "UTC")
)

.MERCADO_INHABILES <- new.env(parent = emptyenv())
.MERCADO_TTL_MIN <- 60

# What the process holds (NULL: never read, or never read well).
mercado_inhabiles_tabla <- function() .MERCADO_INHABILES$tabla

# Put a table in memory (the refresh, a save in this process, a test).
mercado_inhabiles_poner <- function(df) {
  .MERCADO_INHABILES$tabla <- if (is.null(df)) NULL else .hd_admin_completar(df, .schema_mercado_inhabiles)
  invisible(.MERCADO_INHABILES$tabla)
}

# Read hd-admin/mercado/inhabiles.rds into memory, at most once every
# .MERCADO_TTL_MIN minutes per process unless `forzar`. `leer` is the strict
# read (a test passes its own). Returns "ok", "falla" (this call read and
# failed) or "cache" (within the TTL, nothing read) -- so a failure is
# reported once, by the call that met it. Never stops, never prints (the
# job's log is public). A failure keeps the table already in memory.
mercado_inhabiles_refrescar <- function(forzar = FALSE,
                                        leer = function() leer_hd_admin(.MERCADO_INHABILES_KEY, .schema_mercado_inhabiles)) {
  ultimo <- .MERCADO_INHABILES$intento_en
  if (!forzar && !is.null(ultimo) &&
      as.numeric(difftime(Sys.time(), ultimo, units = "mins")) < .MERCADO_TTL_MIN)
    return(invisible("cache"))   # within the TTL: what the last read left, ok or not
  .MERCADO_INHABILES$intento_en <- Sys.time()
  r <- tryCatch({
    salida <- NULL
    utils::capture.output(salida <- suppressWarnings(suppressMessages(leer())))
    salida
  }, error = function(e) e)
  if (inherits(r, "error") || !is.data.frame(r)) {
    .MERCADO_INHABILES$estado <- "falla"
    return(invisible("falla"))
  }
  mercado_inhabiles_poner(r)
  .MERCADO_INHABILES$estado <- "ok"
  invisible("ok")
}

# Live rows (not voided) of a table.
.mercado_vivos <- function(df) df[is.na(df$anulado_en), , drop = FALSE]

# For get_holidays(): of `anios`, those confirmed for `pais`, and the live
# official days of those years. Memory only.
mercado_inhabiles_oficiales <- function(pais, anios) {
  vacio <- list(anios = integer(), fechas = as.Date(character()))
  df <- .MERCADO_INHABILES$tabla
  if (is.null(df) || !nrow(df)) return(vacio)
  pais <- toupper(trimws(pais))
  v <- .mercado_vivos(df)
  v <- v[v$pais %in% pais, , drop = FALSE]
  conf <- intersect(as.integer(anios), unique(v$anio[v$tipo %in% "anio"]))
  if (!length(conf)) return(vacio)
  dias <- v[v$tipo %in% "dia" & v$anio %in% conf & !is.na(v$fecha), , drop = FALSE]
  list(anios = conf, fechas = sort(unique(as.Date(dias$fecha))))
}

# The official name of a day when the staff captured it; NA otherwise.
mercado_inhabil_motivo <- function(pais, fecha) {
  df <- .MERCADO_INHABILES$tabla
  if (is.null(df) || !nrow(df)) return(NA_character_)
  v <- .mercado_vivos(df)
  v <- v[v$tipo %in% "dia" & v$pais %in% toupper(pais) & v$fecha %in% as.Date(fecha), , drop = FALSE]
  if (nrow(v) && !is.na(v$motivo[1]) && nzchar(v$motivo[1])) v$motivo[1] else NA_character_
}

# ── Capture (staff) ──────────────────────────────────────────────────────────
# Each one rereads S3 strictly and writes through modificar_hd_admin() with
# its T72 event; refuses with a mercado_error (clave = dictionary key).
mercado_error <- function(clave)
  structure(class = c("mercado_error", "error", "condition"), list(message = clave, call = NULL, clave = clave))

modificar_mercado_inhabiles <- function(cambio, evento) {
  df <- modificar_hd_admin(.MERCADO_INHABILES_KEY, .schema_mercado_inhabiles, cambio, evento = evento)
  mercado_inhabiles_poner(df)
  df
}

.mercado_fila <- function(tipo, pais, fecha, anio, motivo, fuente, por) tibble::tibble(
  id = uuid::UUIDgenerate(), tipo = tipo, pais = pais, fecha = as.Date(fecha), anio = as.integer(anio),
  motivo = motivo, fuente = fuente, capturado_por = por, capturado_en = as.POSIXct(Sys.time(), tz = "UTC"),
  anulado_por = NA_character_, anulado_en = as.POSIXct(NA, tz = "UTC"))

modificar_mercado_agregar_dia <- function(pais, fecha, motivo, fuente, por, evento = NULL) {
  pais <- toupper(trimws(pais %||% ""))
  if (!mercado_pais_valido(pais)) stop(mercado_error("tablas.ajustes.mercado.error_pais"))
  fecha <- tryCatch(as.Date(fecha), error = function(e) as.Date(NA))
  if (length(fecha) != 1L || is.na(fecha) || is.null(.festivos_anios(fecha)))
    stop(mercado_error("tablas.ajustes.mercado.error_fecha"))
  motivo <- trimws(motivo %||% "")
  if (!nzchar(motivo) || nchar(motivo) > 120) stop(mercado_error("tablas.ajustes.mercado.error_motivo"))
  fuente <- trimws(fuente %||% "")
  anio <- as.integer(format(fecha, "%Y"))
  modificar_mercado_inhabiles(function(fresco) {
    v <- .mercado_vivos(fresco)
    if (any(v$tipo %in% "dia" & v$pais %in% pais & v$fecha %in% fecha))
      stop(mercado_error("tablas.ajustes.mercado.error_repetido"))
    .hd_admin_agregar(fresco, .mercado_fila("dia", pais, fecha, anio, motivo,
                                            if (nzchar(fuente)) fuente else NA_character_, por))
  }, evento)
}

modificar_mercado_confirmar_anio <- function(pais, anio, fuente, por, evento = NULL) {
  pais <- toupper(trimws(pais %||% ""))
  if (!mercado_pais_valido(pais)) stop(mercado_error("tablas.ajustes.mercado.error_pais"))
  anio <- suppressWarnings(as.integer(anio))
  if (length(anio) != 1L || is.na(anio) || anio < .FESTIVOS_ANIOS_TOPE[1] || anio > .FESTIVOS_ANIOS_TOPE[2])
    stop(mercado_error("tablas.ajustes.mercado.error_anio"))
  fuente <- trimws(fuente %||% "")
  modificar_mercado_inhabiles(function(fresco) {
    v <- .mercado_vivos(fresco)
    if (any(v$tipo %in% "anio" & v$pais %in% pais & v$anio %in% anio))
      stop(mercado_error("tablas.ajustes.mercado.error_ya_confirmado"))
    if (!any(v$tipo %in% "dia" & v$pais %in% pais & v$anio %in% anio))
      stop(mercado_error("tablas.ajustes.mercado.error_anio_vacio"))
    .hd_admin_agregar(fresco, .mercado_fila("anio", pais, NA, anio, NA_character_,
                                            if (nzchar(fuente)) fuente else NA_character_, por))
  }, evento)
}

# Void a live row (a day, or a year's confirmation). Never deletes.
modificar_mercado_anular <- function(id, por, evento = NULL) {
  modificar_mercado_inhabiles(function(fresco) {
    i <- which(fresco$id %in% id & is.na(fresco$anulado_en))
    if (length(i) != 1L) stop(mercado_error("tablas.ajustes.mercado.error_no_existe"))
    fresco$anulado_por[i] <- por
    fresco$anulado_en[i]  <- as.POSIXct(Sys.time(), tz = "UTC")
    fresco
  }, evento)
}

# The screen's comparison for one country and year: every date in the
# official capture or the rule, with which has it.
#   estado: "coincide" | "solo_oficial" | "solo_regla"
mercado_comparar_vacio <- function()
  data.frame(id = character(), fecha = as.Date(character()), motivo = character(),
             estado = character(), fila_id = character(), stringsAsFactors = FALSE)

mercado_comparar <- function(pais, anio, df = .MERCADO_INHABILES$tabla) {
  pais <- toupper(trimws(pais %||% "")); anio <- suppressWarnings(as.integer(anio))
  if (!length(anio) || is.na(anio) || !mercado_pais_valido(pais)) return(mercado_comparar_vacio())
  regla <- .festivos_regla(MERCADO_PAISES[[pais]]$calendario, anio)
  v <- if (is.null(df) || !nrow(df)) .schema_mercado_inhabiles() else .mercado_vivos(df)
  of <- v[v$tipo %in% "dia" & v$pais %in% pais & v$anio %in% anio, , drop = FALSE]
  fechas <- sort(unique(c(regla$fecha, as.Date(of$fecha))))
  if (!length(fechas)) return(mercado_comparar_vacio())
  en_of <- fechas %in% as.Date(of$fecha); en_regla <- fechas %in% regla$fecha
  motivo <- vapply(seq_along(fechas), function(i) {
    m <- of$motivo[as.Date(of$fecha) == fechas[i]]
    if (length(m) && !is.na(m[1])) return(m[1])
    k <- regla$clave[regla$fecha == fechas[i]]
    if (length(k) && !is.na(k[1])) tr(k[1]) else ""
  }, character(1))
  data.frame(id = format(fechas), fecha = fechas, motivo = motivo,
             estado = ifelse(en_of & en_regla, "coincide", ifelse(en_of, "solo_oficial", "solo_regla")),
             fila_id = vapply(fechas, function(f) { x <- of$id[as.Date(of$fecha) == f]; if (length(x)) x[1] else NA_character_ },
                              character(1)),
             stringsAsFactors = FALSE)
}

# The live confirmation row of a country's year, or NULL.
mercado_anio_confirmado <- function(pais, anio, df = .MERCADO_INHABILES$tabla) {
  if (is.null(df) || !nrow(df)) return(NULL)
  v <- .mercado_vivos(df)
  r <- v[v$tipo %in% "anio" & v$pais %in% toupper(pais) & v$anio %in% as.integer(anio), , drop = FALSE]
  if (nrow(r)) r[1, , drop = FALSE] else NULL
}
