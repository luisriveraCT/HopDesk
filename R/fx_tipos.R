# =============================================================================
# R/fx_tipos.R
# The exchange rate of each currency against the base, with where it came
# from, for Flujo's preview and its Exportar (monedas fase 2, Luis
# 2026-11-16: propuesta A, requisiciones/flujo_monedas_fase2_paso0.md).
#
# fx_tipo(moneda, base, escritos, fix, divisas) -> list(valor, origen, fecha)
#   origen "base"         moneda == base: 1
#          "escrito"      typed by this user today (escritos)
#          "banxico_fix"  the FIX (USD with the peso as base)
#          "banxico"      another currency Banxico publishes, the peso as base
#          "cruzado"      the base is not the peso: two Banxico figures of the
#                         same day, divided (Luis: yes, marked "cruzado")
#          "sin_fuente"   none: valor NA. Nothing is ever made up; the export
#                         stops and the preview says which one is missing.
#
# Typed rates (Luis: one day, per user, kept on the server): fx/escritos.rds
# of the client (fecha, usuario, moneda, base, valor, escrito_en). Rows are
# only appended: the last row of (fecha, usuario, moneda, base) wins, and one
# with valor NA undoes it (the ✕). Other days' rows are never read, and
# nothing is deleted, so a write never shrinks the table.
# =============================================================================

FX_ESCRITOS_LLAVE <- "fx/escritos.rds"

.schema_fx_escritos <- function() data.frame(fecha = as.Date(character()), usuario = character(), moneda = character(),
                                             base = character(), valor = numeric(), escrito_en = character(),
                                             stringsAsFactors = FALSE)

.fx_hoy <- function(ahora = Sys.time()) as.Date(format(ahora, tz = FX_FIX_TZ, "%Y-%m-%d"))

.fx_codigo <- function(x) {
  x <- toupper(trimws(as.character(x %||% NA_character_)))[1]
  if (!is.na(x) && grepl("^[A-Z]{3}$", x)) x else NA_character_
}

# The client's typed rates, every day (fx_escritos_de() keeps today's). A
# failed read gives none: the screen then shows Banxico's or asks, never a
# stale figure, and nothing is written from it.
fx_escritos_leer <- function(client_id) {
  if (is.null(client_id) || !nzchar(client_id)) return(.schema_fx_escritos())
  tryCatch(.normalize(.s3_read_with(FX_ESCRITOS_LLAVE, client_id = client_id), .schema_fx_escritos),
           error = function(e) .schema_fx_escritos())
}

# Today's typed rates of `usuario` against `base`: c(EUR = 20.1, ...).
fx_escritos_de <- function(tabla, usuario, base, ahora = Sys.time()) {
  vacio <- stats::setNames(numeric(), character())
  if (!is.data.frame(tabla) || !nrow(tabla) || !is.character(usuario) || length(usuario) != 1L ||
      is.na(usuario) || !nzchar(usuario)) return(vacio)
  t <- tabla[as.Date(tabla$fecha) %in% .fx_hoy(ahora) & tabla$usuario %in% usuario & tabla$base %in% base, ,
             drop = FALSE]
  if (!nrow(t)) return(vacio)
  t <- t[!duplicated(t$moneda, fromLast = TRUE), , drop = FALSE]   # rows are appended: the last one wins
  t <- t[!is.na(t$valor) & t$valor > 0, , drop = FALSE]
  stats::setNames(as.numeric(t$valor), t$moneda)
}

# Appends one typed rate (valor NA: undo today's). A failed read or write
# stop()s, so the caller can say it was not kept.
fx_escrito_guardar <- function(client_id, usuario, moneda, base, valor, ahora = Sys.time()) {
  moneda <- .fx_codigo(moneda); base <- .fx_codigo(base)
  v <- suppressWarnings(as.numeric(valor %||% NA))[1]
  if (is.na(moneda) || is.na(base) || identical(moneda, base)) stop("fx_escrito_guardar: moneda o base inválida", call. = FALSE)
  if (!is.character(usuario) || length(usuario) != 1L || is.na(usuario) || !nzchar(usuario))
    stop("fx_escrito_guardar: sin usuario", call. = FALSE)
  if (!is.na(v) && !(is.finite(v) && v > 0)) stop("fx_escrito_guardar: el tipo debe ser positivo", call. = FALSE)
  fila <- data.frame(fecha = .fx_hoy(ahora), usuario = usuario, moneda = moneda, base = base, valor = v,
                     escrito_en = format(ahora, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ"), stringsAsFactors = FALSE)
  modificar_tabla(FX_ESCRITOS_LLAVE, client_id, .schema_fx_escritos, function(df) rbind(df, fila[, names(df)]))
}

# ── The rate ─────────────────────────────────────────────────────────────────

.fx_sin_fuente <- function() list(valor = NA_real_, origen = "sin_fuente", fecha = as.Date(NA))

# Pesos per unit of `m` by date (data.frame(fecha, valor)); the peso itself
# is 1 on any date (NULL here, handled by the caller).
.fx_pesos <- function(m, divisas) {
  if (!is.data.frame(divisas) || !nrow(divisas)) return(divisas[0, c("fecha", "valor"), drop = FALSE])
  d <- divisas[divisas$moneda %in% m & !is.na(divisas$valor) & divisas$valor > 0, c("fecha", "valor"), drop = FALSE]
  d[order(d$fecha), , drop = FALSE]
}

fx_tipo <- function(moneda, base, escritos = numeric(), fix = NULL, divisas = NULL) {
  moneda <- .fx_codigo(moneda); base <- .fx_codigo(base)
  if (is.na(moneda) || is.na(base)) return(.fx_sin_fuente())
  if (identical(moneda, base)) return(list(valor = 1, origen = "base", fecha = as.Date(NA)))
  e <- if (moneda %in% names(escritos)) suppressWarnings(as.numeric(escritos[[moneda]])) else NA_real_
  if (!is.na(e) && is.finite(e) && e > 0) return(list(valor = e, origen = "escrito", fecha = as.Date(NA)))
  if (!is.data.frame(divisas)) divisas <- .schema_fx_divisas()[, c("fecha", "moneda", "valor")]

  if (identical(base, FX_BANXICO_EN)) {
    if (identical(moneda, FX_FIX_MONEDA))
      return(if (.fx_valido(fix)) list(valor = fix$valor, origen = "banxico_fix", fecha = as.Date(fix$fecha))
             else .fx_sin_fuente())
    d <- .fx_pesos(moneda, divisas)
    if (!nrow(d)) return(.fx_sin_fuente())
    i <- nrow(d)
    return(list(valor = d$valor[i], origen = "banxico", fecha = as.Date(d$fecha[i])))
  }

  # Crossed: (pesos per moneda) / (pesos per base), both of the same day.
  den <- .fx_pesos(base, divisas)
  if (!nrow(den)) return(.fx_sin_fuente())
  if (identical(moneda, FX_BANXICO_EN)) {
    i <- nrow(den)
    return(list(valor = 1 / den$valor[i], origen = "cruzado", fecha = as.Date(den$fecha[i])))
  }
  num <- .fx_pesos(moneda, divisas)
  dias <- intersect(as.character(num$fecha), as.character(den$fecha))
  if (!length(dias)) return(.fx_sin_fuente())
  dia <- max(as.Date(dias))
  list(valor  = num$valor[num$fecha == dia][1] / den$valor[den$fecha == dia][1],
       origen = "cruzado", fecha = dia)
}

# One row per currency: data.frame(moneda, valor, origen, fecha).
fx_tipos <- function(monedas, base, escritos = numeric(), fix = NULL, divisas = NULL) {
  monedas <- unique(as.character(monedas))
  filas <- lapply(monedas, function(m) {
    t <- fx_tipo(m, base, escritos, fix, divisas)
    data.frame(moneda = m, valor = t$valor, origen = t$origen, fecha = as.Date(t$fecha), stringsAsFactors = FALSE)
  })
  if (!length(filas)) return(data.frame(moneda = character(), valor = numeric(), origen = character(),
                                        fecha = as.Date(character()), stringsAsFactors = FALSE))
  do.call(rbind, filas)
}

# The figures of today, for a screen: the FIX and the other currencies, each
# NULL / empty when missing or failing (fx_fix() and fx_divisas() memoise).
fx_fuentes_hoy <- function() list(
  fix     = tryCatch(if (exists("fx_fix", mode = "function")) fx_fix() else NULL, error = function(e) NULL),
  divisas = tryCatch(if (exists("fx_divisas", mode = "function")) fx_divisas() else NULL, error = function(e) NULL)
)

# The rates of a fused (single-currency) report: c(USD = 18.55, ...), only
# those with a value. A currency missing here has no rate, and the report
# stops instead of converting it (.apply_fx_conversion).
fx_tipos_valores <- function(tipos) {
  ok <- !is.na(tipos$valor) & tipos$valor > 0
  stats::setNames(as.numeric(tipos$valor[ok]), tipos$moneda[ok])
}

# "Banxico FIX · 13 nov", "Escrito por ti · hoy", "Sin fuente: escríbelo"...
fx_origen_texto <- function(origen, fecha, idioma = hd_idioma()) {
  f <- as.Date(fecha)
  dia <- if (is.na(f)) "" else paste0(as.integer(format(f, "%d")), " ",
                                      tr(paste0("comun.mes.", as.integer(format(f, "%m"))), idioma = idioma))
  tr(paste0("tablas.flujo.fx.origen.", origen), fecha = dia, idioma = idioma)
}
