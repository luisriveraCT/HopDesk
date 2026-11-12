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
# fx_fix() -> NULL or list(valor, fecha, cambio_pct, serie, fuente):
#   cambio_pct  % change against the previous FIX in the cache (NA if none).
# =============================================================================

FX_FIX_SERIE    <- "SF43718"
FX_FIX_LLAVE    <- "fx/banxico_fix.rds"
FX_FIX_MAX_DIAS <- 3L          # business days a FIX may be old and still shown
FX_FIX_HORA     <- "12:30"     # Banxico determines the FIX around 12:00 CDMX
FX_FIX_TZ       <- "America/Mexico_City"
FX_FIX_REINTENTO_SEG <- 3600   # one Banxico request an hour per process, at most

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
  tryCatch(modificar_hd_admin(FX_FIX_LLAVE, .schema_fx_fix, function(df) {
    df$fecha <- as.Date(df$fecha)
    faltan <- nuevas[!nuevas$fecha %in% df$fecha, , drop = FALSE]
    if (!nrow(faltan)) return(df)
    out <- rbind(df, faltan[, names(df), drop = FALSE])
    out[order(out$fecha), , drop = FALSE]
  }), error = function(e) NULL)
}

fx_fix <- function(ahora = Sys.time()) {
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
