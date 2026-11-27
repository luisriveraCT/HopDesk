# =============================================================================
# R/mercado_datos.R -- T116 F3: the market's exchange rates and reference
# rates for Panorama's «Tipos» and «Tasas».
#
# Public data, one figure for every client: kept in global caches of
# hd-admin that only grow, one row per date --
#   fx/banxico_fix.rds, fx/banxico_divisas.rds   (R/fx_banxico.R, unchanged)
#   mercado/bce.rds    fecha, moneda, valor (units per euro), obtenido_en
#   mercado/tasas.rds  fecha, metric_id, valor, fuente, obtenido_en
#
# Who fills them: mercado_datos_refrescar(), from app.R (the session's first
# flush and «Actualizar»), at most once an hour per process -- the fx_fix()
# pattern -- with the requests in a child process (never blocking a session). Panorama NEVER calls a source: its cards read the caches only
# (mercado_cache(), kept 10 minutes in memory per process).
#
# A figure older than its limit (business days of its own calendar: 3 for a
# daily one, 7 for CETES 28, weekly) is not shown: «Sin dato reciente
# (último: dd/mm)». Nothing is ever made up.
#
# Sources and their terms (T116 F3 paso 0, section 6): Banxico (FIX and the
# currencies it publishes), the ECB (reference rates, "for information
# purposes only"; the ECB is cited as the source), the New York Fed (SOFR,
# EFFR; its attribution line goes under the card) and FRED (Prime).
# =============================================================================

MERCADO_BCE_LLAVE    <- "mercado/bce.rds"
MERCADO_TASAS_LLAVE  <- "mercado/tasas.rds"
MERCADO_REINTENTO_SEG <- 3600   # one request per source an hour per process, at most
MERCADO_MEMO_SEG      <- 600    # the caches, kept 10 minutes in memory
MERCADO_MAX_DIAS      <- 3L     # business days a daily figure may be old and still shown
MERCADO_MAX_DIAS_TASA <- c(cetes28 = 7L)   # weekly auction
MERCADO_DIAS_ATRAS    <- 14L    # each request asks for the last 14 days

.schema_mercado_bce <- function() data.frame(fecha = as.Date(character()), moneda = character(), valor = numeric(),
                                             obtenido_en = character(), stringsAsFactors = FALSE)
.schema_mercado_tasas <- function() data.frame(fecha = as.Date(character()), metric_id = character(), valor = numeric(),
                                               fuente = character(), obtenido_en = character(), stringsAsFactors = FALSE)
.MERCADO_DATOS <- new.env(parent = emptyenv())

# The currencies the ECB quotes per euro that Panorama may need: every
# market's currency but the euro, plus the ones Banxico publishes.
.mercado_bce_monedas <- function()
  setdiff(unique(c(vapply(MERCADO_PAISES, `[[`, "", "moneda"), names(FX_DIVISAS_SERIES))), MERCADO_BCE_EN)

# Every rate any market shows.
.mercado_tasas_todas <- function() unique(unlist(lapply(MERCADO_PAISES, `[[`, "tasas")))

.mercado_limpiar <- function(df, schema) {
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$fecha <- as.Date(df$fecha); df$valor <- suppressWarnings(as.numeric(df$valor))
  df[!is.na(df$fecha) & is.finite(df$valor), names(schema()), drop = FALSE]
}

# ── Filling the caches ───────────────────────────────────────────────────────
# The requests run in a child R process (callr::r_bg), with the lessons of
# the Flujo's Excel (T74 F1):
#   - a child hides its errors behind a clean exit: the result AND its
#     stderr are checked, and every failure leaves one line in the app's
#     log, «mercado: refresco falló (<fuente>)» -- the source's name only,
#     never a URL, a header or the token;
#   - its working directory is R's temp folder: it is given absolute paths
#     (HD_APP_DIR, set when global.R loaded) and loads only what fetching
#     needs;
#   - it never touches S3: it returns rows, and the main process writes;
#   - one child at a time per process; one past MERCADO_HIJO_TOPE_SEG is
#     killed;
#   - BANXICO_TOKEN (and FRED_API_KEY) reach it through its environment,
#     inherited, never through its arguments (visible in the process list).
# Each piece returns list(filas, fallas): the rows (or NULL) and the names of
# what failed ("bce:USD", "tasa:sofr", "banxico:SF46410", "banxico:sin_token").

.mercado_bajar_banxico <- function(hoy, obtenido) {
  if (!nzchar(Sys.getenv("BANXICO_TOKEN", "")))
    return(list(fix = NULL, divisas = NULL, fallas = "banxico:sin_token"))
  fallas <- character()
  pedir <- function(id, serie) {
    r <- tryCatch(fcs_fetch_banxico(list(metric_id = id, fetch_params = list(list(serie = serie))),
                                    hoy - MERCADO_DIAS_ATRAS, hoy), error = function(e) NULL)
    if (is.null(r) || !identical(r$status, "ok") || is.null(r$rows) || !nrow(r$rows)) {
      fallas <<- c(fallas, paste0("banxico:", serie)); return(NULL)
    }
    data.frame(fecha = as.Date(r$rows$fecha), valor = as.numeric(r$rows$value), stringsAsFactors = FALSE)
  }
  f <- pedir("usd_mxn", FX_FIX_SERIE)
  fix <- if (is.null(f)) NULL else data.frame(fecha = f$fecha, valor = f$valor, serie = FX_FIX_SERIE,
                                              obtenido_en = obtenido, stringsAsFactors = FALSE)
  partes <- lapply(names(FX_DIVISAS_SERIES), function(m) {
    d <- pedir(paste0("fx_", tolower(m)), FX_DIVISAS_SERIES[[m]])
    if (is.null(d)) NULL else data.frame(fecha = d$fecha, moneda = m, serie = FX_DIVISAS_SERIES[[m]], valor = d$valor,
                                         obtenido_en = obtenido, stringsAsFactors = FALSE)
  })
  list(fix = fix, divisas = do.call(rbind, partes), fallas = fallas)
}

.mercado_bajar_bce <- function(hoy, obtenido) {
  fallas <- character()
  partes <- lapply(.mercado_bce_monedas(), function(m) {
    r <- tryCatch(.fcs_ecb_serie("EXR", sprintf("D.%s.%s.SP00.A", m, MERCADO_BCE_EN), hoy - MERCADO_DIAS_ATRAS, hoy),
                  error = function(e) NULL)
    if (is.null(r) || !identical(r$status, "ok")) { fallas <<- c(fallas, paste0("bce:", m)); return(NULL) }
    data.frame(fecha = r$datos$fecha, moneda = m, valor = r$datos$valor, obtenido_en = obtenido, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, partes)
  list(filas = if (is.null(out) || !nrow(out)) NULL else out, fallas = fallas)
}

# Each reference rate from the first of its sources (the seed's primary,
# then its fallbacks) that answers.
.mercado_bajar_tasas <- function(hoy, obtenido) {
  semilla <- .seed_forecasting_metrics()
  fallas <- character()
  partes <- lapply(.mercado_tasas_todas(), function(id) {
    m <- semilla[semilla$metric_id == id, , drop = FALSE]
    if (!nrow(m)) { fallas <<- c(fallas, paste0("tasa:", id)); return(NULL) }
    metrica <- as.list(m[1, ])
    for (fuente in c(metrica$primary_source_id, unlist(metrica$fallback_source_ids[[1]]))) {
      f <- paste0("fcs_fetch_", fuente)
      if (!exists(f, mode = "function")) next
      r <- tryCatch(get(f, mode = "function")(metrica, hoy - MERCADO_DIAS_ATRAS, hoy), error = function(e) NULL)
      if (is.null(r) || !identical(r$status, "ok") || is.null(r$rows) || !nrow(r$rows)) next
      return(data.frame(fecha = as.Date(r$rows$fecha), metric_id = id, valor = as.numeric(r$rows$value),
                        fuente = fuente, obtenido_en = obtenido, stringsAsFactors = FALSE))
    }
    fallas <<- c(fallas, paste0("tasa:", id))
    NULL
  })
  out <- do.call(rbind, partes)
  list(filas = if (is.null(out) || !nrow(out)) NULL else out, fallas = fallas)
}

# Everything: list(fix, divisas, bce, tasas, fallas).
.mercado_bajar_todo <- function(hoy, obtenido) {
  b <- .mercado_bajar_banxico(hoy, obtenido)
  e <- .mercado_bajar_bce(hoy, obtenido)
  t <- .mercado_bajar_tasas(hoy, obtenido)
  list(fix = b$fix, divisas = b$divisas, bce = e$filas, tasas = t$filas, fallas = c(b$fallas, e$fallas, t$fallas))
}

# What the child loads: absolute paths under the app's root (its own
# working directory is R's temp folder).
.mercado_archivos_hijo <- function(app_dir = get0("HD_APP_DIR", envir = globalenv(),
                                                  ifnotfound = normalizePath(getwd(), winslash = "/")))
  file.path(app_dir, c("R/forecasting_metric_registry.R", "R/forecasting_source_banxico.R",
                       "R/forecasting_source_fred.R", "R/forecasting_source_ecb.R",
                       "R/forecasting_source_nyfed.R", "R/fx_banxico.R", "R/mercado.R", "R/mercado_datos.R"))

# The child. Arguments: file paths, a date and a timestamp -- nothing secret.
# Never reads nor writes S3; never depends on its working directory.
.mercado_hijo <- function(archivos, hoy, obtenido) {
  assign("%||%", function(a, b) if (is.null(a)) b else a, envir = globalenv())
  for (f in archivos) sys.source(f, envir = globalenv())
  suppressWarnings(suppressMessages(.mercado_bajar_todo(as.Date(hoy), obtenido)))
}

# One line in the app's log per refresh that failed somewhere.
.mercado_log_falla <- function(fuentes) {
  fuentes <- unique(fuentes[nzchar(fuentes)])
  if (length(fuentes)) message("mercado: refresco falló (", paste(fuentes, collapse = ", "), ")")
  invisible(length(fuentes))
}

# Adds to a global cache only the rows whose (fecha, por) it lacks; an
# unchanged table is neither written nor logged (modificar_hd_admin). TRUE
# when nothing had to be added or it was saved; FALSE when the save failed.
.mercado_agregar <- function(llave, schema, nuevas, por, evento_texto) {
  if (is.null(nuevas) || !nrow(nuevas)) return(TRUE)
  llave_de <- function(df) if (is.null(por)) as.character(as.Date(df$fecha)) else paste(as.Date(df$fecha), df[[por]])
  ev <- if (exists("evento_sistema", mode = "function")) evento_sistema("mercado", "datos_mercado", evento_texto)
  tryCatch({
    modificar_hd_admin(llave, schema, evento = ev, function(df) {
      df$fecha <- as.Date(df$fecha)
      kn <- llave_de(nuevas)
      faltan <- nuevas[!kn %in% llave_de(df) & !duplicated(kn), , drop = FALSE]
      if (!nrow(faltan)) return(df)
      out <- rbind(df, faltan[, names(df), drop = FALSE])
      out[order(out$fecha, if (is.null(por)) out$fecha else out[[por]]), , drop = FALSE]
    })
    TRUE
  }, error = function(e) FALSE)
}

# What the fetch brought, into the caches (the main process). Logs what
# failed -- to fetch or to save. "ok" (everything fetched and saved) or
# "falla".
.mercado_guardar <- function(r) {
  if (!is.list(r)) { .mercado_log_falla("proceso hijo"); return("falla") }
  callado <- function(x) suppressWarnings(suppressMessages(x))
  guardado <- c(
    fix     = callado(.mercado_agregar(FX_FIX_LLAVE, .schema_fx_fix, r$fix, NULL, "FIX de Banxico")),
    divisas = callado(.mercado_agregar(FX_DIVISAS_LLAVE, .schema_fx_divisas, r$divisas, "moneda", "Divisas de Banxico")),
    bce     = callado(.mercado_agregar(MERCADO_BCE_LLAVE, .schema_mercado_bce, r$bce, "moneda", "Tipos de referencia del BCE")),
    tasas   = callado(.mercado_agregar(MERCADO_TASAS_LLAVE, .schema_mercado_tasas, r$tasas, "metric_id",
                                       "Tasas de referencia de mercado")))
  .MERCADO_DATOS$memo <- NULL            # the next read sees what was just added
  fallas <- c(as.character(r$fallas %||% character()), paste0("guardar:", names(guardado)[!guardado]))
  .mercado_log_falla(fallas)
  if (!length(fallas)) "ok" else "falla"
}

MERCADO_HIJO_TOPE_SEG <- 180   # a child still running after this is killed

# What a finished child left: its result, or NULL when it failed -- an error
# (get_result() stops), a non-zero exit, or an error line in its stderr
# (whose text is never copied to the log: only that it failed).
.mercado_resultado_hijo <- function(job) {
  r <- tryCatch(job$get_result(), error = function(e) NULL)
  err <- tryCatch(paste(job$read_all_error_lines(), collapse = "\n"), error = function(e) "")
  estado <- tryCatch(job$get_exit_status(), error = function(e) NA_integer_)
  if (is.null(r) || !is.list(r) || !identical(as.integer(estado), 0L) || grepl("(^|\n)Error", err)) NULL else r
}

# Fills the caches, at most once every MERCADO_REINTENTO_SEG per process
# (forzar skips that). en_fondo (the app): the requests go to a child and this
# returns "lanzado" at once; a later() tick every 2 s writes what it brought
# when it ends, or kills it past MERCADO_HIJO_TOPE_SEG. en_fondo = FALSE
# (tests, scripts): fetches and writes here; "ok" or "falla". Also "espera"
# (asked too recently) and "corriendo" (a child is alive: never two). Never
# stops; prints only the failure line.
mercado_datos_refrescar <- function(ahora = Sys.time(), forzar = FALSE, en_fondo = TRUE,
                                    archivos = .mercado_archivos_hijo()) {
  job <- .MERCADO_DATOS$job
  if (!is.null(job) && isTRUE(tryCatch(job$is_alive(), error = function(e) FALSE))) return("corriendo")
  ultimo <- .MERCADO_DATOS$intento
  if (!isTRUE(forzar) && !is.null(ultimo) &&
      as.numeric(difftime(ahora, ultimo, units = "secs")) < MERCADO_REINTENTO_SEG) return("espera")
  .MERCADO_DATOS$intento <- ahora
  hoy <- hoy_en(ZONA_OMISION, ahora)
  obtenido <- format(ahora, tz = "UTC", "%Y-%m-%dT%H:%M:%SZ")
  if (!isTRUE(en_fondo))
    return(tryCatch(.mercado_guardar(suppressWarnings(suppressMessages(.mercado_bajar_todo(hoy, obtenido)))),
                    error = function(e) { .mercado_log_falla("proceso"); "falla" }))
  tryCatch({
    job <- callr::r_bg(.mercado_hijo, args = list(archivos = archivos, hoy = format(hoy), obtenido = obtenido),
                       supervise = TRUE)       # the environment (and the tokens in it) is inherited
    .MERCADO_DATOS$job <- job
    inicio <- Sys.time()
    revisar <- function() {
      if (isTRUE(tryCatch(job$is_alive(), error = function(e) FALSE))) {
        if (as.numeric(difftime(Sys.time(), inicio, units = "secs")) <= MERCADO_HIJO_TOPE_SEG)
          return(invisible(later::later(revisar, delay = 2)))
        tryCatch(job$kill(), error = function(e) NULL)
        .MERCADO_DATOS$job <- NULL
        .mercado_log_falla("tiempo agotado")
        return(invisible())
      }
      .MERCADO_DATOS$job <- NULL
      tryCatch(.mercado_guardar(.mercado_resultado_hijo(job)), error = function(e) .mercado_log_falla("guardar"))
      invisible()
    }
    later::later(revisar, delay = 2)
    "lanzado"
  }, error = function(e) { .mercado_log_falla("proceso hijo"); "falla" })
}

# ── Reading the caches (never a source) ──────────────────────────────────────

# list(fix, divisas, bce, tasas): each the cache's rows, or NULL when it could
# not be read. Kept MERCADO_MEMO_SEG in this process.
mercado_cache <- function(ahora = Sys.time()) {
  m <- .MERCADO_DATOS$memo
  if (!is.null(m)) {
    d <- as.numeric(difftime(ahora, m$cuando, units = "secs"))
    if (d >= 0 && d < MERCADO_MEMO_SEG) return(m$valor)
  }
  leer <- function(llave, schema) tryCatch(.mercado_limpiar(leer_hd_admin(llave, schema), schema), error = function(e) NULL)
  divisas <- if (exists("FX_DIVISAS_LLAVE")) leer(FX_DIVISAS_LLAVE, .schema_fx_divisas) else NULL
  fix <- if (exists("FX_FIX_LLAVE")) tryCatch({
    f <- leer_hd_admin(FX_FIX_LLAVE, .schema_fx_fix)
    f <- data.frame(fecha = as.Date(f$fecha), valor = suppressWarnings(as.numeric(f$valor)))
    f[!is.na(f$fecha) & is.finite(f$valor) & f$valor > 0, , drop = FALSE]
  }, error = function(e) NULL) else NULL
  v <- list(fix = fix, divisas = divisas,
            bce = leer(MERCADO_BCE_LLAVE, .schema_mercado_bce),
            tasas = leer(MERCADO_TASAS_LLAVE, .schema_mercado_tasas))
  .MERCADO_DATOS$memo <- list(cuando = ahora, valor = v)
  v
}

# Business days of `calendario` after `fecha` up to `hoy` (0 when fecha >= hoy).
.mercado_dias_habiles <- function(fecha, hoy, calendario) {
  fecha <- as.Date(fecha); hoy <- as.Date(hoy)
  if (is.na(fecha) || is.na(hoy) || fecha >= hoy) return(0L)
  d <- seq(fecha + 1L, hoy, by = "day")
  fest <- tryCatch(get_holidays(calendario, years = unique(as.integer(format(d, "%Y")))), error = function(e) as.Date(character()))
  sum(as.integer(format(d, "%u")) <= 5L & !d %in% fest)
}

.mercado_filas_vacias <- function() data.frame(clave = character(), etiqueta = character(), tipo = character(),
  valor = numeric(), fecha = as.Date(character()), fuente = character(), cruzado = logical(), reciente = logical(),
  stringsAsFactors = FALSE)

# One card row: what it is, its value (NA when not shown), date, source, and
# whether it is fresh. `serie`: data.frame(fecha, valor) of that figure.
.mercado_cuadro_fila <- function(clave, etiqueta, tipo, serie, fuente, calendario, max_dias, hoy, cruzado = FALSE) {
  fila <- function(valor, fecha, reciente) data.frame(clave = clave, etiqueta = etiqueta, tipo = tipo, valor = valor,
                                                      fecha = as.Date(fecha), fuente = fuente, cruzado = cruzado,
                                                      reciente = reciente, stringsAsFactors = FALSE)
  if (is.null(serie) || !nrow(serie)) return(fila(NA_real_, NA, FALSE))
  serie <- serie[as.Date(serie$fecha) <= hoy, , drop = FALSE]
  if (!nrow(serie)) return(fila(NA_real_, NA, FALSE))
  i <- which.max(as.Date(serie$fecha))
  f <- as.Date(serie$fecha[i])
  reciente <- .mercado_dias_habiles(f, hoy, calendario) <= max_dias
  fila(if (reciente) serie$valor[i] else NA_real_, f, reciente)
}

# The calendar a figure's age is counted in: its publisher's (Banxico's FIX
# is not late on a US holiday, nor the ECB's rate on a Mexican one).
.mercado_calendario_fuente <- function(fuente)
  switch(fuente %||% "", banxico = , banxico_fix = "MX", ecb = "EU", nyfed = , fred = "US", "MX")

# The pair of two currencies as the market writes it: c(A, B), value B per A.
.mercado_par <- function(x, y) {
  r <- function(m) { k <- match(m, MERCADO_ORDEN_COTIZACION); if (is.na(k)) length(MERCADO_ORDEN_COTIZACION) + 1L else k }
  if (r(x) <= r(y)) c(x, y) else c(y, x)
}

# B per one A by date, from the caches. Banxico for a currency against the
# peso when the client's base is the peso, and always for USD/MXN (the FIX);
# else the ECB's per-euro figures (a client with base EUR reads the ECB),
# divided on the same date (crossed when neither is the euro).
# list(serie = data.frame(fecha, valor), fuente, cruzado).
.mercado_serie_par <- function(a, b, cache, base = b) {
  vacio <- data.frame(fecha = as.Date(character()), valor = numeric())
  if (identical(b, FX_BANXICO_EN) && identical(a, FX_FIX_MONEDA))
    return(list(serie = cache$fix %||% vacio, fuente = "banxico_fix", cruzado = FALSE))
  if (identical(b, FX_BANXICO_EN) && identical(base, FX_BANXICO_EN) && a %in% names(FX_DIVISAS_SERIES)) {
    d <- cache$divisas
    s <- if (is.null(d)) vacio else d[d$moneda %in% a, c("fecha", "valor"), drop = FALSE]
    return(list(serie = s, fuente = "banxico", cruzado = FALSE))
  }
  e <- cache$bce
  por_euro <- function(m) {
    if (identical(m, MERCADO_BCE_EN)) return(NULL)
    if (is.null(e)) return(vacio)
    e[e$moneda %in% m, c("fecha", "valor"), drop = FALSE]
  }
  pa <- por_euro(a); pb <- por_euro(b)
  s <- if (is.null(pa)) pb                                     # EUR/B: B per euro
       else if (is.null(pb)) data.frame(fecha = pa$fecha, valor = 1 / pa$valor)
       else { j <- merge(pa, pb, by = "fecha"); data.frame(fecha = j$fecha, valor = j$valor.y / j$valor.x) }
  list(serie = s, fuente = "ecb", cruzado = !is.null(pa) && !is.null(pb))
}

# «Tipos»: one row per currency of the client's markets that is not its base,
# as the market writes the pair with the base. Empty without markets.
mercado_tipos_cliente <- function(cfg, cache, ahora = Sys.time()) {
  paises <- mercado_paises_cliente(cfg)
  base <- toupper(trimws(as.character(cfg$moneda_base %||% NA_character_)[1]))
  if (is.na(base) || !nzchar(base)) base <- if (exists("MONEDA_BASE_OMISION")) MONEDA_BASE_OMISION else NA_character_
  if (!length(paises) || is.na(base)) return(.mercado_filas_vacias())
  hoy <- hoy_en(ZONA_OMISION, ahora)
  monedas <- setdiff(unique(vapply(MERCADO_PAISES[paises], `[[`, "", "moneda")), base)
  if (!length(monedas)) return(.mercado_filas_vacias())
  filas <- lapply(monedas, function(m) {
    par <- .mercado_par(m, base)
    s <- .mercado_serie_par(par[1], par[2], cache, base = base)
    .mercado_cuadro_fila(paste(par, collapse = "/"), paste(par, collapse = "/"), "tipo", s$serie, s$fuente,
                  .mercado_calendario_fuente(s$fuente), MERCADO_MAX_DIAS, hoy, cruzado = s$cruzado)
  })
  do.call(rbind, filas)
}

# «Tasas»: the reference rates of the client's markets. Empty without markets.
mercado_tasas_cliente <- function(cfg, cache, ahora = Sys.time()) {
  paises <- mercado_paises_cliente(cfg)
  if (!length(paises)) return(.mercado_filas_vacias())
  hoy <- hoy_en(ZONA_OMISION, ahora)
  t <- cache$tasas
  filas <- unlist(lapply(paises, function(p) lapply(MERCADO_PAISES[[p]]$tasas, function(id) {
    s <- if (is.null(t)) NULL else t[t$metric_id %in% id, , drop = FALSE]
    fuente <- if (!is.null(s) && nrow(s)) s$fuente[which.max(s$fecha)] else ""
    max_dias <- if (id %in% names(MERCADO_MAX_DIAS_TASA)) MERCADO_MAX_DIAS_TASA[[id]] else MERCADO_MAX_DIAS
    .mercado_cuadro_fila(id, tr(paste0("comun.mercado.tasa.", id)), "tasa",
                  if (is.null(s)) NULL else s[, c("fecha", "valor"), drop = FALSE], fuente,
                  MERCADO_PAISES[[p]]$calendario, max_dias, hoy)
  })), recursive = FALSE)
  if (!length(filas)) .mercado_filas_vacias() else do.call(rbind, filas)
}

# ── The card ─────────────────────────────────────────────────────────────────

.mercado_fecha_corta <- function(f) if (is.na(f)) "" else format(as.Date(f), "%d/%m")

# The source as shown next to a figure: «Banxico FIX 26/11», «BCE 26/11»,
# «BCE, cruzado 26/11», «Fed de NY 24/11», «FRED 24/11».
mercado_fuente_texto <- function(fuente, fecha, cruzado = FALSE)
  tr(paste0("panorama.mercado.fuente.", if (isTRUE(cruzado)) "cruzado" else (fuente %||% "")),
     fecha = .mercado_fecha_corta(fecha))

# One card («Tipos» or «Tasas») from its rows; NULL without rows. The New
# York Fed's attribution line and the ECB's note go under it when a row comes
# from them (their terms of use).
mercado_cuadro_ui <- function(filas, titulo) {
  if (is.null(filas) || !nrow(filas)) return(NULL)
  valor_txt <- function(i) {
    if (!filas$reciente[i])
      return(if (is.na(filas$fecha[i])) tr("panorama.mercado.sin_dato")
             else tr("panorama.mercado.sin_dato_reciente", fecha = .mercado_fecha_corta(filas$fecha[i])))
    if (identical(filas$tipo[i], "tasa")) paste0(formatC(filas$valor[i], format = "f", digits = 2), " %")
    else formatC(filas$valor[i], format = "f", digits = 4)
  }
  notas <- c(if (any(filas$fuente[filas$reciente] %in% "nyfed")) tr("comun.mercado.atribucion.nyfed",
                                                                    anio = format(hoy_en(), "%Y")),
             if (any(filas$fuente[filas$reciente] %in% "ecb")) tr("comun.mercado.atribucion.bce"))
  shiny::div(class = "pan-tarjeta pan-mercado",
    shiny::div(class = "pan-etiqueta", tr(titulo)),
    shiny::tags$dl(class = "pan-mercado-lista",
      lapply(seq_len(nrow(filas)), function(i) shiny::div(class = "pan-mercado-fila",
        shiny::tags$dt(filas$etiqueta[i]),
        shiny::tags$dd(class = if (!filas$reciente[i]) "pan-mercado-viejo",
          shiny::span(class = "pan-mercado-valor", valor_txt(i)),
          if (filas$reciente[i]) shiny::span(class = "pan-mercado-fuente",
                                             mercado_fuente_texto(filas$fuente[i], filas$fecha[i], filas$cruzado[i])))))),
    if (length(notas)) shiny::div(class = "pan-mercado-nota", lapply(notas, shiny::div)))
}
