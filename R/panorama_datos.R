# =============================================================================
# R/panorama_datos.R
# Panorama (front B): what the screen shows, computed from data the session
# already holds. Pure functions -- no Shiny, no S3 -- so each one is tested on
# its own (tests/test_panorama_datos.R). R/panorama_module.R only draws them.
#
# Amounts are never added across currencies: every total is one row per
# currency (Moneda, total, n), MXN first.
# =============================================================================

PANORAMA_DIAS_PASIVOS <- 3L     # "Pasivos en 3 días"
PANORAMA_SAP_VIEJO_H  <- 24     # SAP data older than this (or a snapshot) = stale
PANORAMA_DESTACADOS   <- 3L     # slides in Destacados

# Feed kinds (the chips): comunicado, solicitud, actividad, alerta.
PANORAMA_TIPOS_FEED <- c("com", "sol", "act", "ale")

.pan_num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }

# One row per currency, MXN first, then alphabetical.
.pan_por_moneda <- function(moneda, importe) {
  vacio <- data.frame(moneda = character(), total = numeric(), n = integer(), stringsAsFactors = FALSE)
  if (!length(moneda)) return(vacio)
  moneda <- toupper(trimws(ifelse(is.na(moneda) | !nzchar(trimws(moneda)), "MXN", moneda)))
  t <- tapply(.pan_num(importe), moneda, sum)
  n <- tapply(rep(1L, length(moneda)), moneda, sum)
  out <- data.frame(moneda = names(t), total = as.numeric(t), n = as.integer(n[names(t)]),
                    stringsAsFactors = FALSE, row.names = NULL)
  out[order(out$moneda != "MXN", out$moneda), , drop = FALSE]
}

# ── Hoy ──────────────────────────────────────────────────────────────────────

# Pagos de hoy: what the Agenda de Hoy holds to pay today, still pending.
panorama_pagos_hoy <- function(ph, hoy = Sys.Date()) {
  if (is.null(ph) || !is.data.frame(ph) || !nrow(ph)) return(.pan_por_moneda(character(), numeric()))
  f <- ph[ph$ledger %in% "AP" & ph$status %in% "pending" &
            (is.na(ph$tipo_item) | ph$tipo_item != "abono"), , drop = FALSE]
  f <- f[!is.na(f$FechaVenc) & as.Date(f$FechaVenc) <= hoy, , drop = FALSE]
  .pan_por_moneda(f$Moneda, abs(.pan_num(f$Importe)))
}

# Cobros que vencen hoy: the Calendar's own CxC rows (df_combined_AR, same
# filters as the Calendar and Vencidos) whose effective date is today.
panorama_cobros_hoy <- function(df_ar, hoy = Sys.Date()) {
  if (is.null(df_ar) || !is.data.frame(df_ar) || !nrow(df_ar) || !"FechaEff" %in% names(df_ar))
    return(.pan_por_moneda(character(), numeric()))
  f <- df_ar[!is.na(df_ar$FechaEff) & as.Date(df_ar$FechaEff) == hoy, , drop = FALSE]
  monto <- if ("Saldo vencido" %in% names(f)) f[["Saldo vencido"]] else rep(0, nrow(f))
  .pan_por_moneda(f$Moneda, abs(.pan_num(monto)))
}

# Pasivos en N días: provisions still open whose date (override first) falls
# from today to today + dias. Returns the totals plus the first date.
panorama_pasivos_pronto <- function(provs, hoy = Sys.Date(), dias = PANORAMA_DIAS_PASIVOS) {
  vacio <- list(totales = .pan_por_moneda(character(), numeric()), primera = as.Date(NA))
  if (is.null(provs) || !is.data.frame(provs) || !nrow(provs)) return(vacio)
  fecha <- as.Date(provs$fecha_efectiva_override)
  fecha[is.na(fecha)] <- as.Date(provs$fecha_efectiva[is.na(fecha)])
  monto <- suppressWarnings(as.numeric(provs$amount_pago_override))
  monto[is.na(monto)] <- .pan_num(provs$amount_pago[is.na(monto)])
  abiertas <- provs$estado %in% "provisional" & !is.na(fecha) & fecha >= hoy & fecha <= hoy + dias
  if (!any(abiertas)) return(vacio)
  list(totales = .pan_por_moneda(provs$moneda_pago[abiertas], monto[abiertas]),
       primera = min(fecha[abiertas]))
}

# ── Avisos ───────────────────────────────────────────────────────────────────

# A publicado written before `clase` existed is a comunicado.
avisos_clase <- function(df) {
  cl <- if ("clase" %in% names(df)) df$clase else rep(NA_character_, nrow(df))
  cl[is.na(cl) & df$tipo %in% "publicado"] <- "comunicado"
  cl
}

# Feed kind of each visible aviso:
#   solicitud (published or personal)            -> sol
#   automatic with severity atencion / urgente  -> ale (threshold alerts)
#   automatic info, comments and mentions        -> act
#   any other published one                      -> com
panorama_tipo_feed <- function(df) {
  if (!nrow(df)) return(character())
  cl <- avisos_clase(df)
  ifelse(cl %in% "solicitud", "sol",
  ifelse(df$tipo %in% "automatico" & df$severidad %in% c("atencion", "urgente"), "ale",
  ifelse(df$tipo %in% c("automatico", "personal"), "act", "com")))
}

.pan_hechos <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(character())
  tolower(tryCatch(as.character(unlist(jsonlite::fromJSON(s))), error = function(e) character()))
}

# The feed: visible avisos newest first, each with its kind and whether it is
# new since `ultima_visita` (POSIXct or NULL: first visit, nothing is new).
panorama_feed <- function(vis, ultima_visita = NULL) {
  if (is.null(vis) || !nrow(vis)) {
    return(data.frame(id = character(), tipo_feed = character(), nuevo = logical(),
                      stringsAsFactors = FALSE))
  }
  cuando <- vis$actualizado
  cuando[is.na(cuando)] <- vis$creado[is.na(cuando)]
  uv <- suppressWarnings(as.POSIXct(ultima_visita %||% NA, tz = "UTC"))
  out <- vis
  out$tipo_feed <- panorama_tipo_feed(vis)
  out$clase_ok  <- avisos_clase(vis)
  out$cuando    <- cuando
  out$nuevo     <- !is.na(uv) & !is.na(cuando) & cuando > uv
  out[order(out$cuando, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}

# Mis solicitudes: solicitudes addressed to this user (personal) that they
# have not marked done.
panorama_mis_solicitudes <- function(vis, usuario) {
  u <- tolower(usuario %||% "")
  if (is.null(vis) || !nrow(vis) || !nzchar(u)) return(vis[0, , drop = FALSE])
  es <- avisos_clase(vis) %in% "solicitud" & vis$tipo %in% "personal" &
    !vapply(vis$hechos, function(h) u %in% .pan_hechos(h), logical(1))
  s <- vis[es, , drop = FALSE]
  s[order(s$vence, na.last = TRUE), , drop = FALSE]
}

# Destacados: published comunicados, recordatorios and reconocimientos; the
# urgent ones first, then the newest.
panorama_destacados <- function(vis, n = PANORAMA_DESTACADOS) {
  if (is.null(vis) || !nrow(vis)) return(vis)
  cl <- avisos_clase(vis)
  d <- vis[vis$tipo %in% "publicado" & cl %in% c("comunicado", "recordatorio", "reconocimiento"), , drop = FALSE]
  if (!nrow(d)) return(d)
  d$clase <- avisos_clase(d)
  d <- d[order(!(d$severidad %in% "urgente"), -as.numeric(d$creado)), , drop = FALSE]
  utils::head(d, n)
}

# Tu equipo hoy: the team activity avisos of today, by who did them.
panorama_equipo_hoy <- function(vis, hoy = Sys.Date(), tz = "America/Mexico_City") {
  vacio <- list(personas = character(), acciones = 0L)
  if (is.null(vis) || !nrow(vis)) return(vacio)
  act <- vis[vis$tipo %in% "automatico" & !(vis$severidad %in% c("atencion", "urgente")) &
               !is.na(vis$actualizado) &
               as.Date(vis$actualizado, tz = tz) == hoy, , drop = FALSE]
  if (!nrow(act)) return(vacio)
  quien <- act$creado_por[!is.na(act$creado_por) & act$creado_por != "sistema"]
  list(personas = unique(quien), acciones = as.integer(sum(pmax(1L, act$contador %||% 1L), na.rm = TRUE)))
}

# ── SAP and Clima de hoy ─────────────────────────────────────────────────────

# list(ts, viejo): the oldest of the loaded ledgers (what the screen relies
# on), and whether it is a snapshot or older than PANORAMA_SAP_VIEJO_H hours.
panorama_sap_estado <- function(info, ahora = Sys.time()) {
  e <- Filter(function(x) !is.null(x) && !is.null(x$ts), list(info$AR, info$AP))
  if (!length(e)) return(list(ts = NULL, viejo = FALSE, cargado = FALSE))
  ts <- e[[which.min(vapply(e, function(x) as.numeric(x$ts), numeric(1)))]]$ts
  viejo <- any(!vapply(e, function(x) isTRUE(x$is_live), logical(1))) ||
    as.numeric(difftime(ahora, ts, units = "hours")) > PANORAMA_SAP_VIEJO_H
  list(ts = ts, viejo = viejo, cargado = TRUE)
}

# Clima de hoy, from what already exists:
#   neblina   SAP data is stale (the numbers may be wrong: that comes first)
#   nublado   at least one urgent aviso
#   medio     an alert, a solicitud pending, or a reminder ending by tomorrow
#   despejado none of that
# Returns list(clima, pendientes, urgentes, razones), razones being
# list(clave, params) for tr(), at most 3.
panorama_clima <- function(feed, n_solicitudes = 0L, n_pagos_hoy = 0L, sap = list(viejo = FALSE),
                           ahora = Sys.time()) {
  urg  <- if (nrow(feed)) sum(feed$severidad %in% "urgente") else 0L
  ale  <- if (nrow(feed)) sum(feed$tipo_feed %in% "ale") else 0L
  rec  <- if (nrow(feed)) sum(feed$clase_ok %in% "recordatorio" & !is.na(feed$vence) &
                                 as.numeric(difftime(feed$vence, ahora, units = "hours")) <= 36) else 0L
  pend <- as.integer(n_solicitudes + n_pagos_hoy)
  razones <- list()
  add <- function(clave, ...) razones[[length(razones) + 1L]] <<- list(clave = clave, params = list(...))
  clima <- if (isTRUE(sap$viejo)) "neblina" else if (urg > 0) "nublado" else
    if (ale > 0 || n_solicitudes > 0 || rec > 0) "medio" else "despejado"
  if (isTRUE(sap$viejo)) add("panorama.clima.razon.sap_viejo") else add("panorama.clima.razon.sap_al_dia")
  if (urg > 0) add("panorama.clima.razon.urgentes", n = urg) else add("panorama.clima.razon.sin_urgentes")
  if (ale > 0) add("panorama.clima.razon.alertas", n = ale)
  else if (rec > 0) add("panorama.clima.razon.recordatorio")
  else if (n_solicitudes > 0) add("panorama.clima.razon.solicitudes", n = n_solicitudes)
  else add("panorama.clima.razon.sin_alertas")
  list(clima = clima, pendientes = pend, urgentes = as.integer(urg), razones = utils::head(razones, 3))
}

# ── Formatting ───────────────────────────────────────────────────────────────

panorama_monto <- function(x) paste0("$", formatC(.pan_num(x), format = "f", digits = 2, big.mark = ","))

# "MIÉRCOLES 12 DE NOVIEMBRE": weekday and month from the dictionary.
panorama_fecha_larga <- function(fecha = Sys.Date(), idioma = hd_idioma()) {
  fecha <- as.Date(fecha)
  dia <- as.integer(format(fecha, "%u"))   # 1 = Monday
  tr("panorama.fecha_larga", idioma = idioma,
     dia_semana = tr(paste0("panorama.dia.", dia), idioma = idioma),
     dia = as.character(as.integer(format(fecha, "%d"))),
     mes = tr(paste0("tablas.periodo.mes_largo.", as.integer(format(fecha, "%m"))), idioma = idioma))
}

panorama_saludo <- function(hora) {
  hora <- as.integer(hora)
  if (hora < 12L) "panorama.saludo.dias" else if (hora < 19L) "panorama.saludo.tardes" else "panorama.saludo.noches"
}

# "hace 25 min", "hace 2 h", "Ayer", or the short date.
panorama_cuando <- function(t, ahora = Sys.time(), tz = "America/Mexico_City") {
  if (is.null(t) || is.na(t)) return("")
  min <- as.numeric(difftime(ahora, t, units = "mins"))
  if (min < 1) return(tr("panorama.cuando.ahora"))
  if (min < 60) return(tr("panorama.cuando.min", n = as.character(floor(min))))
  hoy <- as.Date(ahora, tz = tz); d <- as.Date(t, tz = tz)
  if (d == hoy) return(tr("panorama.cuando.horas", n = as.character(floor(min / 60))))
  if (d == hoy - 1) return(tr("panorama.cuando.ayer"))
  paste(as.integer(format(d, "%d")), tr(paste0("comun.mes.", as.integer(format(d, "%m")))))
}
