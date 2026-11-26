# =============================================================================
# R/avisos_inhabiles.R -- T116 F2: «Día inhábil bancario en México: martes
# 02/11/2027 (Día de Muertos)», an automatic aviso in Panorama.
#
# Published by the avisos job (scripts/avisos_job.R) on its Monday and Friday
# runs, for each of the client's countries (mercado_paises_cliente(): the
# chosen ones, or the base currency's), for the weekdays of the next
# INHABILES_HORIZONTE natural days. With two runs a week that announces every
# non-working day at least two working days before -- a Tuesday holiday on
# the Monday of the week before, and again on the Friday.
#
# One aviso per country and date: clave "inhabil:<pais>:<fecha>", published
# once (repetir = FALSE): the Friday run never makes it "new" again nor
# brings back one somebody discarded. Expires at the end of that day.
# Never prints and never stops the job (its log is public): it counts
# avisos_inhabiles, and inhabiles_regla_calculada when the staff's official
# calendar could not be read (the aviso then comes from the rule).
# =============================================================================

INHABILES_HORIZONTE <- 10L

# The client's non-working weekdays from `hoy` to hoy + dias, per country.
inhabiles_proximos <- function(paises, hoy, dias = INHABILES_HORIZONTE) {
  vacio <- data.frame(pais = character(), fecha = as.Date(character()), motivo = character(),
                      stringsAsFactors = FALSE)
  hoy <- as.Date(hoy)
  paises <- paises[mercado_pais_valido(paises)]
  if (!length(paises) || is.na(hoy)) return(vacio)
  fin <- hoy + as.integer(dias)
  anios <- unique(as.integer(format(c(hoy, fin), "%Y")))
  filas <- lapply(paises, function(p) {
    cal <- MERCADO_PAISES[[p]]$calendario
    f <- get_holidays(cal, years = anios)
    f <- f[f >= hoy & f <= fin & as.integer(format(f, "%u")) <= 5L]
    if (!length(f)) return(NULL)
    regla <- do.call(rbind, lapply(anios, function(y) .festivos_regla(cal, y)))
    motivo <- vapply(seq_along(f), function(i) {
      m <- mercado_inhabil_motivo(p, f[i])
      if (!is.na(m)) return(m)
      k <- regla$clave[regla$fecha == f[i]]
      if (length(k) && !is.na(k[1])) tr(k[1], idioma = "es") else ""
    }, character(1))
    data.frame(pais = p, fecha = f, motivo = motivo, stringsAsFactors = FALSE)
  })
  filas <- Filter(Negate(is.null), filas)
  if (!length(filas)) vacio else do.call(rbind, filas)
}

# The aviso of one row. Text in Spanish, as every stored aviso.
inhabiles_aviso <- function(fila) {
  f <- as.Date(fila$fecha)
  dia <- tr(paste0("comun.dia.", as.integer(format(f, "%u"))), idioma = "es")
  list(
    tipo      = "automatico",
    titulo    = tr("avisos.inhabil.titulo", idioma = "es", pais = mercado_pais_nombre(fila$pais, idioma = "es"),
                   dia = dia, fecha = format(f, "%d/%m/%Y")),
    cuerpo    = if (nzchar(fila$motivo %||% "")) tr("avisos.inhabil.cuerpo", idioma = "es", motivo = fila$motivo) else "",
    severidad = "info",
    niveles   = niveles_cliente(),
    clave     = paste("inhabil", fila$pais, format(f), sep = ":"),
    repetir   = FALSE,
    vence     = as.POSIXct(paste(format(f), "23:59:59"), tz = ZONA_OMISION))
}

# The job's step for one client. `leer_config` is the strict read of the
# client's group config (a failed read publishes nothing: never a Mexican
# aviso for a client whose countries could not be read). Returns the number
# of avisos written; never stops.
inhabiles_job <- function(cid, hoy, contar,
                          leer_config = function() .group_config_completar(.s3_leer_para_escribir(S3_KEYS$group_config, cid))) {
  tryCatch({
    if (identical(mercado_inhabiles_refrescar(), "falla")) contar("inhabiles_regla_calculada")
    cfg <- suppressWarnings(suppressMessages(leer_config()))
    filas <- inhabiles_proximos(mercado_paises_cliente(cfg), hoy)
    for (i in seq_len(nrow(filas)))
      suppressWarnings(suppressMessages(avisos_agregar(cid, inhabiles_aviso(filas[i, , drop = FALSE]), por = "sistema")))
    if (nrow(filas)) contar("avisos_inhabiles", nrow(filas))
    nrow(filas)
  }, error = function(e) 0L)
}
