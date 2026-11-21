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
PANORAMA_DESTACADOS   <- 3L     # slides in Destacados

# SAP data is stale (Panorama's neblina, the job skips the client's email)
# when its date, in the client's time zone, is before today - SAP_DIAS_VIEJO:
# calendar days, so data refreshed on a Monday is still fine all of the next
# Monday. The one rule for both (T46; SAP is refreshed about once a week).
SAP_DIAS_VIEJO <- 7L
sap_viejo <- function(ts, ahora = Sys.time(), tz = "America/Mexico_City") {
  if (is.null(ts) || !length(ts) || is.na(ts[1])) return(TRUE)   # no date: never trusted
  dia <- function(t) as.Date(format(as.POSIXct(t, tz = "UTC"), "%Y-%m-%d", tz = tz))
  dia(ts[1]) < dia(ahora) - SAP_DIAS_VIEJO
}

# Feed kinds (the chips): comunicado, solicitud, actividad, requisiciones
# (T62), alerta.
PANORAMA_TIPOS_FEED <- c("com", "sol", "act", "req", "ale")

# ── Mis avisos de requisiciones (T62) ────────────────────────────────────────
# Each user chooses whose requisitions reach them as avisos, saved in their
# preferences (prefs_guardar(), key req_avisos):
#   ninguna  no avisos: they are only seen in Requisiciones
#   mias     (default) the ones I am part of: I created, commented or acted on it
#   mias_y   mine plus those of the people I choose (by user id, account_code)
#   todas    every requisition of the client
REQ_AVISOS_MODOS <- c("ninguna", "mias", "mias_y", "todas")

# A stored preference (anything: missing, old, hand-edited) as
# list(modo, personas): an unknown mode is the default; people only count
# with mias_y, as unique non-empty ids.
req_avisos_pref <- function(x) {
  modo <- if (is.list(x)) as.character(x$modo %||% "")[1] else ""
  if (is.na(modo) || !modo %in% REQ_AVISOS_MODOS) modo <- "mias"
  personas <- if (is.list(x)) as.character(unlist(x$personas %||% character())) else character()
  personas <- unique(trimws(personas[!is.na(personas)]))
  personas <- personas[nzchar(personas)]
  list(modo = modo, personas = if (identical(modo, "mias_y")) personas else character())
}

# The people one may choose (the client's live users with a client tier, by
# id; a HopDesk staff account is never listed), as data.frame(id, nombre).
req_avisos_personas <- function(u) {
  vacio <- data.frame(id = character(), nombre = character(), stringsAsFactors = FALSE)
  if (!is.data.frame(u) || !nrow(u) || !"account_code" %in% names(u)) return(vacio)
  vivo <- !(u$deleted %in% TRUE) & !(u$activo %in% FALSE) & u$tier %in% niveles_cliente() &
    !is.na(u$account_code) & nzchar(u$account_code %||% "")
  u <- u[vivo, , drop = FALSE]
  if (!nrow(u)) return(vacio)
  nombre <- if ("display_name" %in% names(u)) u$display_name else rep(NA_character_, nrow(u))
  out <- data.frame(id = u$account_code, nombre = ifelse(is.na(nombre) | !nzchar(nombre %||% ""), u$username, nombre),
                    stringsAsFactors = FALSE)
  out[order(tolower(out$nombre)), , drop = FALSE]
}

# ── De quién recibo avisos (T68) ─────────────────────────────────────────────
# One preference per person, on the side of who RECEIVES (T68_paso0.md):
# preferencias.rds, datos$avisos_de =
#   list(v = 1, mio = TRUE/FALSE,
#        canales = list(<canal> = list(base = "todos"|"nadie", cambia = <account_codes>)))
# `cambia`: the people who go against the base ("todos menos dos" is two
# codes), so whoever joins later gets what the column says. Channels: the
# requisitions (req) and the activity kinds of ALERTAS_ACCIONES (crear,
# archivo_pago, confirmar_pago, bancos). Nothing here reads S3.
AVISOS_DE_CANALES <- c("req", "crear", "archivo_pago", "confirmar_pago", "bancos")

# The actions of an activity channel (ALERTAS_ACCIONES, read when called).
avisos_de_acciones <- function(canal) {
  if (identical(canal, "req")) return(character())
  names(Filter(function(d) identical(d$columna, canal), ALERTAS_ACCIONES))
}

avisos_de_vacio <- function(mio = TRUE) {
  list(v = 1L, mio = isTRUE(mio),
       canales = stats::setNames(lapply(AVISOS_DE_CANALES, function(c) list(base = "nadie", cambia = character())),
                                 AVISOS_DE_CANALES))
}

# Any stored value (missing, old, hand-edited) as a clean preference; with
# `codigos`, people that no longer exist drop out of `cambia`.
avisos_de_normalizar <- function(x, codigos = NULL) {
  out <- avisos_de_vacio()
  if (!is.list(x)) return(out)
  if (!is.null(x$mio)) out$mio <- isTRUE(as.logical(unlist(x$mio))[1])
  for (c in AVISOS_DE_CANALES) {
    k <- if (is.list(x$canales)) x$canales[[c]] else NULL
    if (!is.list(k)) next
    base <- as.character(unlist(k$base))[1]
    if (is.na(base) || !base %in% c("todos", "nadie")) base <- "nadie"
    cam <- unique(trimws(as.character(unlist(k$cambia))))
    cam <- cam[!is.na(cam) & nzchar(cam)]
    if (!is.null(codigos)) cam <- intersect(cam, codigos)
    out$canales[[c]] <- list(base = base, cambia = cam)
  }
  out
}

# Does this preference take channel `canal` from the author with this code?
avisos_de_recibe <- function(pref, canal, autor_code) {
  k <- pref$canales[[canal]]
  if (is.null(k)) return(FALSE)
  dentro <- length(autor_code) == 1L && !is.na(autor_code) && autor_code %in% k$cambia
  if (identical(k$base, "todos")) !dentro else dentro
}

# Without avisos_de, it is worked out when read (nothing is written until the
# person saves): "lo mío" and requisitions from req_avisos (T62); each
# activity channel from the matrix of "Alertas del equipo" (the authors
# marked in that column; codigo_de maps username -> account_code).
avisos_de_migrar <- function(req_avisos, matriz, codigo_de) {
  pf <- req_avisos_pref(req_avisos)
  out <- avisos_de_vacio(mio = !identical(pf$modo, "ninguna"))
  out$canales$req <- switch(pf$modo,
    todas  = list(base = "todos", cambia = character()),
    mias_y = list(base = "nadie", cambia = pf$personas),
    list(base = "nadie", cambia = character()))
  if (is.data.frame(matriz) && nrow(matriz) && "usuario" %in% names(matriz)) {
    for (c in setdiff(AVISOS_DE_CANALES, "req")) {
      if (!c %in% names(matriz)) next
      us <- tolower(trimws(as.character(matriz$usuario[matriz[[c]] %in% TRUE])))
      cod <- unname(codigo_de[us])
      cod <- unique(cod[!is.na(cod) & nzchar(cod)])
      out$canales[[c]] <- list(base = "nadie", cambia = cod)
    }
  }
  out
}

# A person's preference as it stands: their avisos_de, or the migration.
# `datos`: their preferencias datos (a list).
avisos_de_de <- function(datos, matriz = NULL, codigo_de = character(), codigos = NULL) {
  if (is.list(datos) && is.list(datos$avisos_de)) return(avisos_de_normalizar(datos$avisos_de, codigos))
  avisos_de_normalizar(avisos_de_migrar(if (is.list(datos)) datos$req_avisos, matriz, codigo_de), codigos)
}

# The channels a reader may have (T68 condition: permission by ACTION): an
# activity channel if they hold the permission of at least one of its
# actions; requisitions with can_view_requisiciones.
avisos_de_canales_de <- function(usuario) {
  si <- function(p) isTRUE(tryCatch(puede(usuario, p), error = function(e) FALSE))
  ok <- vapply(AVISOS_DE_CANALES, function(c) {
    if (identical(c, "req")) return(si("can_view_requisiciones"))
    any(vapply(avisos_de_acciones(c), function(a) si(ALERTAS_ACCIONES[[a]]$permiso), logical(1)))
  }, logical(1))
  AVISOS_DE_CANALES[ok]
}

# Saving: a channel the reader may not have goes back to "nadie", whatever
# came from the browser.
avisos_de_recortar <- function(pref, canales) {
  for (c in setdiff(AVISOS_DE_CANALES, canales)) pref$canales[[c]] <- list(base = "nadie", cambia = character())
  pref
}

# From the grid (t$casillas(): data.frame(id = account_code, <canal> = logical))
# to base + exceptions: whichever needs fewer exceptions, so "todos menos
# dos" is stored as two codes and a newcomer gets "todos".
avisos_de_desde_tabla <- function(tab, canales, mio) {
  pref <- avisos_de_vacio(mio)
  for (c in intersect(canales, names(tab))) {
    si <- tab[[c]] %in% TRUE
    pref$canales[[c]] <- if (length(si) && sum(si) * 2L > length(si))
      list(base = "todos", cambia = as.character(tab$id[!si]))
    else list(base = "nadie", cambia = as.character(tab$id[si]))
  }
  pref
}

# For JSON: `cambia` always an array, even of one (auto_unbox would make it
# a string).
avisos_de_para_guardar <- function(pref) {
  pref$canales <- lapply(pref$canales, function(k) list(base = k$base, cambia = I(as.character(k$cambia))))
  pref
}

# Saving the panel: the server decides the channels again (permission by
# action), never the browser -- a channel this reader may not have is
# "nadie" whatever the grid says -- and the block is replaced whole
# (prefs_reemplazar(), R/avisos.R). `estado`: data.frame(id, <canal> = logical).
# casa: HopDesk staff save in THEIR home client (Coordinador, 2026-11-20),
# avisos_de_clientes[[<client seen>]], never in the client's folder; without
# requisitions nor "lo mio" (they are not one of the client's users).
avisos_de_guardar <- function(client_id, usuario, estado, mio, casa = NULL) {
  canales <- avisos_de_canales_de(usuario)
  if (!is.null(casa)) { canales <- setdiff(canales, "req"); mio <- FALSE }
  tab <- estado[, c("id", intersect(canales, names(estado))), drop = FALSE]
  pref <- avisos_de_recortar(avisos_de_desde_tabla(tab, canales, mio), canales)
  if (is.null(casa)) prefs_reemplazar(client_id, usuario$user, "avisos_de", avisos_de_para_guardar(pref))
  else prefs_reemplazar(casa, usuario$user, "avisos_de_clientes", avisos_de_para_guardar(pref), sub = tolower(client_id))
  invisible(pref)
}

# A staff member's preference for the client they see, from their home's
# datos: the saved block, or -- never saved -- every activity their
# permission gives them (as before T68).
avisos_de_staff_de <- function(datos_casa, client_id) {
  b <- if (is.list(datos_casa) && is.list(datos_casa$avisos_de_clientes)) datos_casa$avisos_de_clientes[[tolower(client_id)]]
  if (is.list(b)) return(avisos_de_normalizar(b))
  p <- avisos_de_vacio(mio = FALSE)
  for (c in setdiff(AVISOS_DE_CANALES, "req")) p$canales[[c]] <- list(base = "todos", cambia = character())
  p
}

# The grid's rows: the people one may choose (req_avisos_personas()) with
# their tier and job title for the groups, and one logical per channel
# from the preference.
avisos_de_filas <- function(u, pref, canales, yo_code = NA_character_) {
  p <- req_avisos_personas(u)
  if (!nrow(p)) return(data.frame(id = character(), nombre = character(), nivel = character(), puesto = character(),
                                  stringsAsFactors = FALSE))
  k <- match(p$id, u$account_code)
  p$nivel  <- vapply(as.character(u$tier[k]), function(t) tryCatch(nivel_nombre(t), error = function(e) t), character(1))
  pu <- if ("puesto" %in% names(u)) as.character(u$puesto[k]) else rep(NA_character_, nrow(p))
  pu <- trimws(gsub("\\s+", " ", pu))
  p$puesto <- ifelse(is.na(pu) | !nzchar(pu), tr("panorama.avisos_de.sin_puesto"), tools::toTitleCase(tolower(pu)))
  if (!is.na(yo_code)) p$nombre[p$id == yo_code] <- paste(p$nombre[p$id == yo_code], tr("panorama.avisos_de.tu"))
  for (c in canales) p[[c]] <- vapply(p$id, function(id) avisos_de_recibe(pref, c, id), logical(1), USE.NAMES = FALSE)
  rownames(p) <- NULL
  p
}

# The feed's activity, by the reader's preference: an "act" aviso
# (clave "act|<acción>|<autor>") stays only if the reader takes its channel
# from that author. Permission already filtered it (avisos_visibles());
# the preference never opens what the permission closed. Other avisos stay.
avisos_de_filtrar_feed <- function(df, pref, codigo_de) {
  if (!is.data.frame(df) || !nrow(df) || !"clave" %in% names(df)) return(df)
  cl <- as.character(df$clave)
  m <- regmatches(cl, regexec("^act\\|([a-z_]+)\\|(.+)$", cl))
  queda <- vapply(m, function(x) {
    if (length(x) != 3L) return(TRUE)
    def <- ALERTAS_ACCIONES[[x[2]]]
    if (is.null(def)) return(TRUE)
    avisos_de_recibe(pref, def$columna, unname(codigo_de[tolower(x[3])]))
  }, logical(1))
  df[queda, , drop = FALSE]
}

# The client's live readers (usuarios.rds as stored): username, tier,
# resolved permissions and account_code; staff never.
avisos_de_lectores <- function(usuarios, config = NULL) {
  if (!is.data.frame(usuarios) || !nrow(usuarios)) return(list())
  col <- function(n, def) if (n %in% names(usuarios)) usuarios[[n]] else rep(def, nrow(usuarios))
  user <- tolower(trimws(as.character(col("username", NA_character_))))
  tier <- as.character(col("tier", NA_character_))
  perm <- as.character(col("permisos", "{}"))
  code <- as.character(col("account_code", NA_character_))
  vivo <- !(col("deleted", FALSE) %in% TRUE) & !(col("activo", TRUE) %in% FALSE) &
    tier %in% niveles_cliente() & !is.na(user) & nzchar(user)
  lapply(which(vivo), function(i) list(
    user = user[i], tier = tier[i], code = code[i],
    permisos = permisos_resueltos(tier[i], if (is.na(perm[i])) "{}" else perm[i], config)))
}

# username -> account_code, for the authors of the feed and the matrix.
avisos_de_codigos <- function(usuarios) {
  if (!is.data.frame(usuarios) || !nrow(usuarios) || !all(c("username", "account_code") %in% names(usuarios)))
    return(character())
  stats::setNames(as.character(usuarios$account_code), tolower(trimws(as.character(usuarios$username))))
}

# The gate (T68, Coordinador 2026-11-20): an "act" is published when at
# least one other live user of the client holds the permission of its
# action -- permissions only, never preferences, so nothing outside the
# client is read and what anyone (staff included) chooses always finds it
# published; the preference filters when shown (avisos_de_filtrar_feed()).
# One aviso per action, never one per reader. Pure: the caller reads
# usuarios and config strictly.
avisos_de_alguien_puede <- function(usuarios, config, accion, autor) {
  def <- ALERTAS_ACCIONES[[accion %||% ""]]
  if (is.null(def)) return(FALSE)
  autor <- tolower(trimws(autor %||% ""))
  for (l in avisos_de_lectores(usuarios, config)) {
    if (identical(l$user, autor)) next
    if (isTRUE(tryCatch(puede(l, def$permiso), error = function(e) FALSE))) return(TRUE)
  }
  FALSE
}

.pan_num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }

# One row per currency, the client's base first, then alphabetical; a row
# without a currency is in the base (monedas fase 3, T39).
.pan_por_moneda <- function(moneda, importe, base = MONEDA_BASE_OMISION) {
  vacio <- data.frame(moneda = character(), total = numeric(), n = integer(), stringsAsFactors = FALSE)
  if (!length(moneda)) return(vacio)
  moneda <- toupper(trimws(ifelse(is.na(moneda) | !nzchar(trimws(moneda)), base, moneda)))
  t <- tapply(.pan_num(importe), moneda, sum)
  n <- tapply(rep(1L, length(moneda)), moneda, sum)
  out <- data.frame(moneda = names(t), total = as.numeric(t), n = as.integer(n[names(t)]),
                    stringsAsFactors = FALSE, row.names = NULL)
  out[order(out$moneda != base, out$moneda), , drop = FALSE]
}

# ── Hoy ──────────────────────────────────────────────────────────────────────

# Pagos de hoy: what the Agenda de Hoy holds to pay, still pending -- the twin
# of the Agenda's own "↓ Pagos" (T60): its rows through agenda_pendientes()
# (R/agenda_filas.R), ledger AP, Importe summed by currency with its sign,
# whatever the due date (it used to keep FechaVenc <= today, so an item due
# after today was in the Agenda and not here) and partial payments included.
# The Hoy card, the Clima's "N pagos" and the Cinta read this. `hoy` is kept
# for the callers; the breakdown below is what uses the day.
panorama_pagos_hoy <- function(ph, hoy = Sys.Date(), base = MONEDA_BASE_OMISION) {
  f <- agenda_pendientes(ph, "AP")
  if (is.null(f) || !nrow(f)) return(.pan_por_moneda(character(), numeric(), base))
  .pan_por_moneda(f$Moneda, .pan_num(f$Importe), base)
}

# Under the card (T60): of those same rows, the overdue ones (due before
# `hoy`, the user's day) and the ones paid ahead (due after it), each by
# currency, never added up across currencies.
panorama_pagos_desglose <- function(ph, hoy, base = MONEDA_BASE_OMISION) {
  vacio <- .pan_por_moneda(character(), numeric(), base)
  f <- agenda_pendientes(ph, "AP")
  if (is.null(f) || !nrow(f)) return(list(vencidos = vacio, anticipados = vacio))
  venc <- as.Date(f$FechaVenc)
  parte <- function(k) if (any(k)) .pan_por_moneda(f$Moneda[k], .pan_num(f$Importe[k]), base) else vacio
  list(vencidos    = parte(!is.na(venc) & venc < hoy),
       anticipados = parte(!is.na(venc) & venc > hoy))
}

# Cobros que vencen hoy: the Calendar's own CxC rows (df_combined_AR, same
# filters as the Calendar and Vencidos) whose effective date is today.
panorama_cobros_hoy <- function(df_ar, hoy = Sys.Date(), base = MONEDA_BASE_OMISION) {
  if (is.null(df_ar) || !is.data.frame(df_ar) || !nrow(df_ar) || !"FechaEff" %in% names(df_ar))
    return(.pan_por_moneda(character(), numeric(), base))
  f <- df_ar[!is.na(df_ar$FechaEff) & as.Date(df_ar$FechaEff) == hoy, , drop = FALSE]
  monto <- if ("Saldo vencido" %in% names(f)) f[["Saldo vencido"]] else rep(0, nrow(f))
  .pan_por_moneda(f$Moneda, abs(.pan_num(monto)), base)
}

# What "Por cobrar hoy" leaves out: the same CxC rows whose effective date
# already went by with no new one (the weekly email's "No incluye … ya
# vencidos sin fecha nueva", T45/T46b). `hoy` has no default: it is the
# client's day, which the caller knows (the server's Sys.Date() is not, CM27).
panorama_cobros_atrasados <- function(df_ar, hoy, base = MONEDA_BASE_OMISION) {
  if (is.null(df_ar) || !is.data.frame(df_ar) || !nrow(df_ar) || !"FechaEff" %in% names(df_ar))
    return(.pan_por_moneda(character(), numeric(), base))
  f <- df_ar[!is.na(df_ar$FechaEff) & as.Date(df_ar$FechaEff) < hoy, , drop = FALSE]
  monto <- if ("Saldo vencido" %in% names(f)) f[["Saldo vencido"]] else rep(0, nrow(f))
  .pan_por_moneda(f$Moneda, abs(.pan_num(monto)), base)
}

# Pasivos en N días: provisions still open whose date (override first) falls
# from today to today + dias. Returns the totals plus the first date.
panorama_pasivos_pronto <- function(provs, hoy = Sys.Date(), dias = PANORAMA_DIAS_PASIVOS,
                                    base = MONEDA_BASE_OMISION) {
  vacio <- list(totales = .pan_por_moneda(character(), numeric(), base), primera = as.Date(NA))
  if (is.null(provs) || !is.data.frame(provs) || !nrow(provs)) return(vacio)
  fecha <- as.Date(provs$fecha_efectiva_override)
  fecha[is.na(fecha)] <- as.Date(provs$fecha_efectiva[is.na(fecha)])
  monto <- suppressWarnings(as.numeric(provs$amount_pago_override))
  monto[is.na(monto)] <- .pan_num(provs$amount_pago[is.na(monto)])
  abiertas <- provs$estado %in% "provisional" & !is.na(fecha) & fecha >= hoy & fecha <= hoy + dias
  if (!any(abiertas)) return(vacio)
  list(totales = .pan_por_moneda(provs$moneda_pago[abiertas], monto[abiertas], base),
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
#   a requisition's event or comment (clave "requisiciones|..." or "req:...") -> req (T62)
#   automatic info, comments and mentions        -> act
#   any other published one                      -> com
panorama_tipo_feed <- function(df) {
  if (!nrow(df)) return(character())
  cl <- avisos_clase(df)
  ifelse(cl %in% "solicitud", "sol",
  ifelse(df$tipo %in% "automatico" & df$severidad %in% c("atencion", "urgente"), "ale",
  ifelse(grepl("^(requisiciones\\||req:)",df$clave %||% rep(NA_character_, nrow(df))), "req",
  ifelse(df$tipo %in% c("automatico", "personal"), "act", "com"))))
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

# list(ts, viejo, cargado): the oldest of the loaded ledgers (what the screen
# relies on) and sap_viejo() of it. Whether it came live or from the S3
# snapshot does not matter, only its date (T46: every session started from
# the snapshot, so the screen was always in neblina).
panorama_sap_estado <- function(info, ahora = Sys.time(), tz = "America/Mexico_City") {
  e <- Filter(function(x) !is.null(x) && !is.null(x$ts), list(info$AR, info$AP))
  if (!length(e)) return(list(ts = NULL, viejo = FALSE, cargado = FALSE))
  ts <- e[[which.min(vapply(e, function(x) as.numeric(x$ts), numeric(1)))]]$ts
  list(ts = ts, viejo = sap_viejo(ts, ahora, tz), cargado = TRUE)
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

# ── Publicar: the audience bar ───────────────────────────────────────────────
# The bar lists the client tiers highest first (niveles_cliente()). Its state
# is a range [desde, hasta] (positions in that list) plus the tiers skipped
# inside it. Plan "Audiencia por niveles" (Luis, 2026-10-28): no limit on
# skipped tiers, no Desde/Hasta sliders.
#   - a tier outside the range widens it up to that tier;
#   - an end of the range trims it by one (a range never goes empty);
#   - a tier inside toggles skipped.
# An end is never a skipped tier: trimming walks past skipped ones.

audiencia_estado <- function(sel, todos) {
  i <- which(todos %in% sel)
  if (!length(i)) return(list(desde = 1L, hasta = length(todos), omitidos = integer()))
  rango <- seq.int(min(i), max(i))
  list(desde = min(i), hasta = max(i), omitidos = setdiff(rango, i))
}

audiencia_click <- function(e, i) {
  i <- as.integer(i)
  if (is.na(i) || i < 1L) return(e)
  if (i < e$desde) e$desde <- i
  else if (i > e$hasta) e$hasta <- i
  else if (e$desde == e$hasta) return(e)                  # the only one: stays
  else if (i == e$desde) { e$desde <- i + 1L; while (e$desde %in% e$omitidos) e$desde <- e$desde + 1L }
  else if (i == e$hasta) { e$hasta <- i - 1L; while (e$hasta %in% e$omitidos) e$hasta <- e$hasta - 1L }
  else e$omitidos <- if (i %in% e$omitidos) setdiff(e$omitidos, i) else sort(c(e$omitidos, i))
  e$omitidos <- e$omitidos[e$omitidos > e$desde & e$omitidos < e$hasta]
  e
}

audiencia_seleccion <- function(e, todos) {
  i <- setdiff(seq.int(e$desde, e$hasta), e$omitidos)
  todos[i[i >= 1L & i <= length(todos)]]
}

# "Admin a Compras, sin Finanzas" -- names from the registry, words from the
# dictionary.
audiencia_resumen <- function(e, todos, idioma = hd_idioma()) {
  nom <- function(i) nivel_nombre(todos[i])
  rango <- if (e$desde == e$hasta) nom(e$desde) else
    tr("panorama.publicar.rango", idioma = idioma, desde = nom(e$desde), hasta = nom(e$hasta))
  if (!length(e$omitidos)) return(rango)
  tr("panorama.publicar.rango_sin", idioma = idioma, rango = rango,
     omitidos = paste(vapply(e$omitidos, nom, character(1)),
                      collapse = paste0(" ", tr("panorama.publicar.ni", idioma = idioma), " ")))
}

# ── Publicar: what gets written ──────────────────────────────────────────────
PANORAMA_TITULO_MAX  <- 80L
PANORAMA_MENSAJE_MAX <- 500L
# The kinds the form offers. "urgente" is a comunicado with severity urgente
# (Design's fourth card); the others are AVISOS_CLASES.
PANORAMA_TIPOS_PUBLICAR <- c("comunicado", "recordatorio", "solicitud", "reconocimiento", "urgente")
# Tabs a Destacado's button may open.
PANORAMA_ENLACES <- c("PH", "BNC", "PSV", "CAL", "VEN", "REQ")

# Checks the form and returns list(ok = TRUE, aviso = <for avisos_agregar or
# avisos_personales>, responsables) or list(ok = FALSE, error = <dictionary
# key>). `vivos`: usernames who may be given a solicitud; `niveles`: the
# tiers the bar chose; `vence`: Date (end of that day in `tz`) or NA.
panorama_publicacion <- function(tipo, titulo, mensaje, niveles, responsables = character(),
                                 vence = NA, enlace = "", vivos = character(),
                                 tz = "America/Mexico_City", ahora = Sys.time()) {
  mal <- function(clave) list(ok = FALSE, error = clave)
  if (!isTRUE(tipo %in% PANORAMA_TIPOS_PUBLICAR)) return(mal("panorama.publicar.error.tipo"))
  titulo  <- trimws(titulo %||% ""); mensaje <- trimws(mensaje %||% "")
  if (!nzchar(titulo)) return(mal("panorama.publicar.error.titulo"))
  if (nchar(titulo) > PANORAMA_TITULO_MAX) return(mal("panorama.publicar.error.titulo_largo"))
  if (nchar(mensaje) > PANORAMA_MENSAJE_MAX) return(mal("panorama.publicar.error.mensaje_largo"))
  enlace <- if (isTRUE(enlace %in% PANORAMA_ENLACES)) enlace else NA_character_
  fin <- if (length(vence) && !is.na(vence))
    as.POSIXct(paste(as.Date(vence), "23:59:59"), tz = tz) else as.POSIXct(NA)
  if (!is.na(fin) && fin <= ahora) return(mal("panorama.publicar.error.fecha_pasada"))
  if (tipo %in% c("recordatorio", "solicitud") && is.na(fin)) return(mal("panorama.publicar.error.fecha"))
  base <- list(titulo = titulo, cuerpo = mensaje, enlace = enlace,
               vence = if (is.na(fin)) NA else format(fin, tz = "UTC", usetz = TRUE),
               severidad = if (tipo == "urgente") "urgente" else "info",
               clase = if (tipo == "urgente") "comunicado" else tipo)
  if (tipo == "solicitud") {
    resp <- intersect(unique(tolower(responsables %||% character())), tolower(vivos))
    if (!length(resp)) return(mal("panorama.publicar.error.responsables"))
    return(list(ok = TRUE, aviso = c(base, list(tipo = "personal")), responsables = resp))
  }
  niveles <- intersect(niveles %||% character(), niveles_cliente())
  if (!length(niveles)) return(mal("panorama.publicar.error.niveles"))
  list(ok = TRUE, aviso = c(base, list(tipo = "publicado", niveles = niveles)), responsables = character())
}

# The Publicadas list: one row per publication. A solicitud is one aviso per
# person, grouped by its clave ("sol:<id>|<user>") into one row.
panorama_publicadas <- function(df, ahora = Sys.time()) {
  vacia <- data.frame(id = character(), tipo = character(), titulo = character(), para = character(),
                      por = character(), creado = as.POSIXct(character()), vence = as.POSIXct(character()),
                      ids = character(), stringsAsFactors = FALSE)
  if (is.null(df) || !nrow(df)) return(vacia)
  vig <- !is.na(df$id) & !(df$retirado %in% TRUE) & (is.na(df$vence) | df$vence > ahora)
  cl <- avisos_clase(df)
  pub <- df[vig & df$tipo %in% "publicado", , drop = FALSE]
  sol <- df[vig & df$tipo %in% "personal" & cl %in% "solicitud" &
              grepl("^sol:", df$clave %||% ""), , drop = FALSE]
  filas <- list()
  if (nrow(pub)) filas[[1]] <- data.frame(
    id = pub$id,
    tipo = ifelse(pub$severidad %in% "urgente", "urgente", avisos_clase(pub)),
    titulo = pub$titulo,
    para = vapply(pub$niveles, function(s) paste(vapply(.pan_hechos_raw(s), nivel_nombre, character(1)),
                                                 collapse = ", "), character(1)),
    por = pub$creado_por, creado = pub$creado, vence = pub$vence, ids = pub$id,
    stringsAsFactors = FALSE)
  if (nrow(sol)) {
    grupo <- sub("[|].*$", "", sol$clave)
    filas[[length(filas) + 1L]] <- do.call(rbind, lapply(split(seq_len(nrow(sol)), grupo), function(k) {
      s <- sol[k, , drop = FALSE]
      data.frame(id = sub("[|].*$", "", s$clave[1]), tipo = "solicitud", titulo = s$titulo[1],
                 para = paste(sort(unique(unlist(lapply(s$usuarios, .pan_hechos_raw)))), collapse = ", "),
                 por = s$creado_por[1], creado = min(s$creado), vence = s$vence[1],
                 ids = paste(s$id, collapse = ","), stringsAsFactors = FALSE)
    }))
  }
  if (!length(filas)) return(vacia)
  out <- do.call(rbind, filas)
  out[order(out$creado, decreasing = TRUE), , drop = FALSE]
}

.pan_hechos_raw <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(character())
  tryCatch(as.character(unlist(jsonlite::fromJSON(s))), error = function(e) character())
}
