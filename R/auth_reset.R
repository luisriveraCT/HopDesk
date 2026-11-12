# =============================================================================
# R/auth_reset.R
# T31: "¿Olvidaste tu contraseña?" -- request a link by e-mail, then choose a
# new password with it. The pages are in app.R (?olvide=, ?reset=); this
# file holds everything they do, so it can be tested without Shiny.
#
# Security (Luis and the integrator, 2026-11-12):
#   - the page never says whether an e-mail has an account: reset_solicitar()
#     returns a reason for the log and the tests only; every case shows the
#     same sentence, and app.R runs it after answering (later::later);
#   - the token is 32 bytes from openssl, in hex; only its sha256 is stored,
#     and a token is found by comparing hashes in constant time;
#   - one use, 60 minutes; a new request cancels the account's earlier
#     links, and a changed password cancels them all;
#   - limits: 3 requests per e-mail per hour, 5 links per account per day,
#     30 requests per 15 minutes in all of HopDesk (then alert);
#   - reads are strict: if password_resets.rds, the registry, a client's
#     users or the lock list cannot be read, nothing is sent (fail closed);
#   - the link is built with app_public_url(NULL): HOPDESK_APP_URL or
#     nothing, never the session's address;
#   - the log never holds the token, the password or the e-mail: only the
#     user name (when there is an account) and a short hash of the e-mail;
#   - alerts (avisos_agregar, not avisos_actividad: security alerts are not
#     the team's activity and cannot be switched off): HopDesk's staff in
#     hd-admin's Panorama; the client's Dev only for its own accounts, never
#     for a staff account and never naming HopDesk's tiers or permissions.
# password_resets.rds only grows (rows change state, none is removed).
# =============================================================================

RESET_MINUTOS <- 60L
RESET_LIMITES <- list(correo_hora = 3L, cuenta_dia = 5L, global_15 = 30L, invalidos_15 = 5L,
                      repetidas_hora = 3L)
.RESET_KEY <- "password_resets.rds"

.schema_password_resets <- function() data.frame(
  id          = character(),
  tipo        = character(),   # "solicitud" (one per request) | "token" (one per account e-mailed)
  token_hash  = character(),   # sha256 of the token; NA on a solicitud
  cuenta_id   = character(),   # the account's id (its user name if it has none)
  client_id   = character(),
  username    = character(),
  correo_hash = character(),   # sha256 of the e-mail, lowercase: counts requests without keeping it
  creado      = character(),   # ISO UTC
  expira      = character(),
  usado       = character(),
  estado      = character(),   # solicitud: "registrada" | "limitada"; token: pendiente | usado | anulado
  stringsAsFactors = FALSE)

.reset_iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
.reset_de_iso <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

reset_hash <- function(x) as.character(openssl::sha256(enc2utf8(x)))
reset_token_nuevo <- function() paste(as.character(openssl::rand_bytes(32)), collapse = "")
# Only what a real token looks like reaches the table: 64 lowercase hex.
reset_token_formato <- function(t)
  is.character(t) && length(t) == 1L && !is.na(t) && nchar(t, type = "bytes") == 64L &&
    grepl("^[0-9a-f]{64}$", t)

# Same time whatever the position of the first difference.
.igual_tiempo_constante <- function(a, b) {
  if (!is.character(a) || !is.character(b) || length(a) != 1L || length(b) != 1L ||
      is.na(a) || is.na(b) || nchar(a, "bytes") != nchar(b, "bytes")) return(FALSE)
  sum(bitwXor(as.integer(charToRaw(a)), as.integer(charToRaw(b)))) == 0L
}
# Index of the token row whose hash is `h`, or NA; looks at every row.
.reset_buscar <- function(df, h) {
  hallado <- NA_integer_
  for (i in seq_len(nrow(df))) {
    igual <- identical(df$tipo[i], "token") && .igual_tiempo_constante(df$token_hash[i], h)
    if (igual && is.na(hallado)) hallado <- i
  }
  hallado
}

.reset_leer <- function() leer_hd_admin(.RESET_KEY, .schema_password_resets)
.reset_modificar <- function(cambio) modificar_hd_admin(.RESET_KEY, .schema_password_resets, cambio)

# ── Accounts of an e-mail ────────────────────────────────────────────────────
# Every live account with this e-mail, in every active client and hd-admin:
# data.frame(cuenta_id, client_id, username, tier, cliente, bloqueada).
# Strict: a registry, a client's users or the lock list that cannot be read
# stop()s -- "no account" must never be a failed read.
reset_cuentas_por_correo <- function(correo) {
  reg <- leer_hd_admin("client_registry.rds", .schema_client_registry)
  activos <- reg[reg$status %in% "active", , drop = FALSE]
  clientes <- unique(c("hd-admin", as.character(activos$client_id)))
  lock <- read_emergency_lock()
  bloqueados <- if (is.data.frame(lock) && nrow(lock)) tolower(lock$username) else character()
  filas <- lapply(clientes, function(cid) {
    u <- .usuarios_leer_estricto(paste0(cid, "/usuarios.rds"))
    if (!is.data.frame(u) || !nrow(u) || !"email" %in% names(u)) return(NULL)
    vivo <- !(u$activo %in% FALSE) & !(u$deleted %in% TRUE)
    m <- u[vivo & !is.na(u$email) & tolower(trimws(u$email)) == correo, , drop = FALSE]
    if (cid == "hd-admin") m <- m[m$tier %in% niveles_staff(), , drop = FALSE]
    if (cid != "hd-admin") m <- m[!m$tier %in% niveles_staff(), , drop = FALSE]
    if (!nrow(m)) return(NULL)
    nombre_cliente <- if (cid == "hd-admin") "HopDesk" else {
      i <- match(cid, reg$client_id); if (is.na(i)) cid else as.character(reg$display_name[i]) }
    data.frame(cuenta_id = ifelse(is.na(m$id) | !nzchar(m$id %||% ""), m$username, m$id),
               client_id = cid, username = tolower(m$username), tier = m$tier,
               cliente = nombre_cliente, bloqueada = tolower(m$username) %in% bloqueados,
               stringsAsFactors = FALSE)
  })
  filas <- Filter(Negate(is.null), filas)
  if (!length(filas)) return(data.frame(cuenta_id = character(), client_id = character(), username = character(),
                                        tier = character(), cliente = character(), bloqueada = logical(),
                                        stringsAsFactors = FALSE))
  do.call(rbind, filas)
}

# ── Log and alerts ───────────────────────────────────────────────────────────
.reset_log <- function(accion, usuario = NA_character_, client_id = "hd-admin", detalle = list(),
                       descripcion = tr(paste0("cuentas.reset.log.", accion))) {
  tryCatch(log_action(user = if (is.na(usuario)) "anonimo" else usuario, module = "auth", action = paste0("reset_", accion),
                      description = descripcion, target_id = usuario, client_id = client_id,
                      viewer_home_client_id = client_id, metadata = detalle),
           error = function(e) message("[RESET] log failed: ", conditionMessage(e)))
}

# regla: a cuentas.reset.alerta.<regla> key pair (.titulo/.cuerpo).
# cuenta: one row of reset_cuentas_por_correo(), or NULL. Staff always hears
# it in hd-admin; the client's Dev only for an account of that client.
.reset_alertar <- function(regla, cuenta = NULL, severidad = "atencion", al_cliente = TRUE) {
  usuario <- if (is.null(cuenta)) "" else cuenta$username
  publicar <- function(cid, niveles, cliente) tryCatch(
    avisos_agregar(cid, list(
      tipo = "automatico", severidad = severidad, niveles = niveles,
      titulo = tr(paste0("cuentas.reset.alerta.", regla, ".titulo")),
      cuerpo = tr(paste0("cuentas.reset.alerta.", regla, ".cuerpo"), usuario = usuario, cliente = cliente),
      clave = paste("t31", regla, if (is.null(cuenta)) "todo" else cuenta$cuenta_id, sep = ":"),
      ventana_min = 60)),
    error = function(e) message("[RESET] alert failed (", cid, "): ", conditionMessage(e)))
  publicar("hd-admin", niveles_staff(), if (is.null(cuenta)) "" else cuenta$cliente)
  if (al_cliente && !is.null(cuenta) && !identical(cuenta$client_id, "hd-admin") &&
      !cuenta$tier %in% niveles_staff())
    publicar(cuenta$client_id, nivel_ti(), cuenta$cliente)
  invisible(NULL)
}

# ── The mail ─────────────────────────────────────────────────────────────────
# cuentas: data.frame(username, cliente, url). One button per account; the
# user name almost hidden (usuario_ocultar(): "l…s"), as Luis asked.
email_reset_html <- function(cuentas, minutos = RESET_MINUTOS) {
  bloques <- vapply(seq_len(nrow(cuentas)), function(i) paste0(
    '<p style="margin:14px 0 4px;font-size:14px;color:#07090d;">',
    .email_esc(tr("cuentas.olvide.correo.cuenta", usuario = usuario_ocultar(cuentas$username[i]),
                  cliente = cuentas$cliente[i])), "</p>",
    .email_button(cuentas$url[i], tr("cuentas.olvide.correo.boton")),
    .email_fallback_url(cuentas$url[i])), character(1))
  body <- paste0(
    '<p style="margin:0 0 14px;font-size:19px;font-weight:bold;color:#07090d;">',
    .email_esc(tr("cuentas.olvide.correo.hola")), "</p>",
    '<p style="margin:0 0 6px;">', .email_esc(tr("cuentas.olvide.correo.intro")), "</p>",
    paste(bloques, collapse = ""),
    '<p style="margin:18px 0 0;font-size:13px;color:#7b8794;">',
    .email_esc(tr("cuentas.olvide.correo.caduca", minutos = as.character(minutos))), "</p>")
  .email_shell(body, preheader = tr("cuentas.olvide.correo.caduca", minutos = as.character(minutos)),
               footer = .email_footer_default(tr("cuentas.olvide.correo.no_fuiste")))
}

# ── Request ──────────────────────────────────────────────────────────────────
# Does the whole request and returns list(motivo, enviados) -- for the log
# and the tests only: the page shows the same sentence in every case.
# motivo: "formato" | "sin_url" | "lectura" | "limite_correo" | "limite_global"
#         | "sin_cuenta" | "solo_bloqueadas" | "limite_cuenta" | "envio_fallido" | "enviado"
reset_solicitar <- function(correo, ahora = Sys.time(), base_url = app_public_url(NULL)) {
  c0 <- correo_normalizar(correo)
  if (!is.null(correo_error(c0))) {
    .reset_log("solicitado", detalle = list(motivo = "formato"))
    return(list(motivo = "formato", enviados = 0L))
  }
  ch <- reset_hash(c0)
  corto <- substr(ch, 1, 8)
  if (!nzchar(base_url %||% "")) {
    .reset_log("sin_url", detalle = list(correo = corto))
    .reset_alertar("sin_url")
    return(list(motivo = "sin_url", enviados = 0L))
  }
  cuentas <- tryCatch(reset_cuentas_por_correo(c0), error = function(e) {
    message("[RESET] lookup failed: ", conditionMessage(e)); NULL })
  if (is.null(cuentas)) {
    .reset_log("lectura_fallida", detalle = list(correo = corto, paso = "cuentas"))
    .reset_alertar("lectura")
    return(list(motivo = "lectura", enviados = 0L))
  }
  vivas <- cuentas[!cuentas$bloqueada, , drop = FALSE]
  tokens <- vapply(seq_len(nrow(vivas)), function(i) reset_token_nuevo(), character(1))
  motivo <- NULL; con_token <- logical(nrow(vivas)); repetidas <- logical(nrow(vivas))
  t_hora <- ahora - 3600; t_dia <- ahora - 86400; t_15 <- ahora - 900
  r <- tryCatch(.reset_modificar(function(df) {
    creado <- .reset_de_iso(df$creado)
    sol <- df$tipo %in% "solicitud"
    fila_sol <- data.frame(id = uuid::UUIDgenerate(), tipo = "solicitud", token_hash = NA_character_,
                           cuenta_id = NA_character_, client_id = NA_character_, username = NA_character_,
                           correo_hash = ch, creado = .reset_iso(ahora), expira = NA_character_,
                           usado = NA_character_, estado = "registrada", stringsAsFactors = FALSE)
    if (sum(sol & df$correo_hash %in% ch & creado >= t_hora, na.rm = TRUE) >= RESET_LIMITES$correo_hora)
      motivo <<- "limite_correo"
    else if (sum(sol & creado >= t_15, na.rm = TRUE) >= RESET_LIMITES$global_15)
      motivo <<- "limite_global"
    if (!is.null(motivo)) { fila_sol$estado <- "limitada"; return(.hd_admin_agregar(df, fila_sol)) }
    df <- .hd_admin_agregar(df, fila_sol)
    creado <- .reset_de_iso(df$creado)
    for (i in seq_len(nrow(vivas))) {
      de_cuenta <- df$tipo %in% "token" & df$cuenta_id %in% vivas$cuenta_id[i] & df$client_id %in% vivas$client_id[i]
      if (sum(de_cuenta & creado >= t_dia, na.rm = TRUE) >= RESET_LIMITES$cuenta_dia) next
      repetidas[i] <<- sum(de_cuenta & creado >= t_hora, na.rm = TRUE) + 1L >= RESET_LIMITES$repetidas_hora
      df$estado[de_cuenta & df$estado %in% "pendiente"] <- "anulado"   # only the newest link works
      df <- .hd_admin_agregar(df, data.frame(
        id = uuid::UUIDgenerate(), tipo = "token", token_hash = reset_hash(tokens[i]),
        cuenta_id = vivas$cuenta_id[i], client_id = vivas$client_id[i], username = vivas$username[i],
        correo_hash = ch, creado = .reset_iso(ahora), expira = .reset_iso(ahora + RESET_MINUTOS * 60),
        usado = NA_character_, estado = "pendiente", stringsAsFactors = FALSE))
      creado <- .reset_de_iso(df$creado)
      con_token[i] <<- TRUE
    }
    df
  }), error = function(e) { message("[RESET] password_resets: ", conditionMessage(e)); e })
  if (inherits(r, "error")) {
    .reset_log("lectura_fallida", detalle = list(correo = corto, paso = "password_resets"))
    .reset_alertar("lectura")
    return(list(motivo = "lectura", enviados = 0L))
  }
  # Alerts on who was asked for, whatever happens next.
  for (i in seq_len(nrow(cuentas))) {
    k <- cuentas[i, , drop = FALSE]
    if (k$bloqueada) .reset_alertar("bloqueada", k)
    else if (k$tier %in% niveles_staff()) .reset_alertar("staff", k, al_cliente = FALSE)
    else if (k$tier %in% nivel_ti()) .reset_alertar("ti", k)
  }
  for (i in which(repetidas)) .reset_alertar("repetidas", vivas[i, , drop = FALSE])
  if (!is.null(motivo)) {
    .reset_log("limite", detalle = list(correo = corto, limite = motivo))
    if (identical(motivo, "limite_global")) .reset_alertar("limite_global")
    return(list(motivo = motivo, enviados = 0L))
  }
  for (i in seq_len(nrow(cuentas)))
    .reset_log("solicitado", cuentas$username[i], cuentas$client_id[i], list(correo = corto))
  if (!nrow(cuentas)) {
    .reset_log("solicitado", detalle = list(correo = corto, motivo = "sin_cuenta"))
    return(list(motivo = "sin_cuenta", enviados = 0L))
  }
  if (!nrow(vivas)) return(list(motivo = "solo_bloqueadas", enviados = 0L))
  if (!any(con_token)) {
    .reset_log("limite", detalle = list(correo = corto, limite = "limite_cuenta"))
    return(list(motivo = "limite_cuenta", enviados = 0L))
  }
  env <- vivas[con_token, , drop = FALSE]
  env$url <- paste0(base_url, "/?reset=", tokens[con_token])
  enviado <- tryCatch(isTRUE(send_email(to = c0, subject = tr("cuentas.olvide.correo.asunto"),
                                        html = email_reset_html(env), queue_on_fail = FALSE)),
                      error = function(e) { message("[RESET] send failed: ", conditionMessage(e)); FALSE })
  for (i in seq_len(nrow(env)))
    .reset_log(if (enviado) "enviado" else "envio_fallido", env$username[i], env$client_id[i], list(correo = corto))
  list(motivo = if (enviado) "enviado" else "envio_fallido", enviados = if (enviado) nrow(env) else 0L)
}

# ── The link ─────────────────────────────────────────────────────────────────
# Opened links that lead nowhere, per process (a page view must not write to
# S3): 5 in 15 minutes is an alert.
.reset_invalidos <- new.env()
.reset_contar_invalido <- function(ahora) {
  v <- c(.reset_invalidos$t %||% numeric(), as.numeric(ahora))
  v <- v[v >= as.numeric(ahora) - 900]
  .reset_invalidos$t <- v
  if (length(v) == RESET_LIMITES$invalidos_15) .reset_alertar("invalidos")
}

# The pending, unexpired token row for `token`, or NULL. `registrar = FALSE`
# when the caller re-checks a token it already logged.
reset_resolver <- function(token, ahora = Sys.time(), registrar = TRUE) {
  if (!reset_token_formato(token)) {
    if (registrar) { .reset_log("enlace_invalido", detalle = list(estado = "formato")); .reset_contar_invalido(ahora) }
    return(NULL)
  }
  h <- reset_hash(token)
  df <- tryCatch(.reset_leer(), error = function(e) { message("[RESET] read failed: ", conditionMessage(e)); NULL })
  if (is.null(df)) { .reset_log("lectura_fallida", detalle = list(paso = "enlace")); return(NULL) }
  i <- .reset_buscar(df, h)
  estado <- if (is.na(i)) "desconocido" else if (!identical(df$estado[i], "pendiente")) df$estado[i]
            else if (is.na(.reset_de_iso(df$expira[i])) || .reset_de_iso(df$expira[i]) <= ahora) "caducado"
            else "vigente"
  if (!identical(estado, "vigente")) {
    if (registrar) {
      .reset_log("enlace_invalido", if (is.na(i)) NA_character_ else df$username[i],
                 if (is.na(i)) "hd-admin" else df$client_id[i], list(estado = estado, token = substr(h, 1, 8)))
      .reset_contar_invalido(ahora)
    }
    return(NULL)
  }
  df[i, , drop = FALSE]
}

# Sets the new password. Returns list(ok = TRUE, usuario_oculto) or
# list(ok = FALSE, error = <key>). The token is taken (one use) before the
# password is saved and given back if the save fails, so a retry works.
reset_aplicar <- function(token, contrasena, confirmar, ahora = Sys.time()) {
  err <- contrasena_error(contrasena)
  if (is.null(err) && !identical(contrasena, confirmar)) err <- "cuentas.error.contrasenas_distintas"
  if (!is.null(err)) return(list(ok = FALSE, error = err))
  fila <- reset_resolver(token, ahora)
  if (is.null(fila)) return(list(ok = FALSE, error = "cuentas.reset.enlace_invalido"))
  h <- reset_hash(token)
  tomado <- FALSE
  r <- tryCatch(.reset_modificar(function(df) {
    i <- .reset_buscar(df, h)
    if (is.na(i) || !identical(df$estado[i], "pendiente")) return(df)
    otros <- df$tipo %in% "token" & df$cuenta_id %in% df$cuenta_id[i] & df$client_id %in% df$client_id[i] &
      df$estado %in% "pendiente"
    df$estado[otros] <- "anulado"
    df$estado[i] <- "usado"; df$usado[i] <- .reset_iso(ahora)
    tomado <<- TRUE
    df
  }), error = function(e) e)
  if (inherits(r, "error") || !tomado) {
    if (inherits(r, "error")) message("[RESET] could not take the token: ", conditionMessage(r))
    return(list(ok = FALSE, error = if (inherits(r, "error")) "comun.reintentar" else "cuentas.reset.enlace_invalido"))
  }
  devolver <- function() tryCatch(.reset_modificar(function(df) {
    i <- .reset_buscar(df, h); if (!is.na(i)) { df$estado[i] <- "pendiente"; df$usado[i] <- NA_character_ }; df
  }), error = function(e) message("[RESET] could not give the token back: ", conditionMessage(e)))
  cid <- fila$client_id
  guardado <- tryCatch({
    usuarios <- auth_load_usuarios(client_id = cid)
    j <- which(usuarios$id %in% fila$cuenta_id)
    if (!length(j)) j <- which(tolower(usuarios$username) %in% fila$username)
    if (length(j) != 1L || usuarios$activo[j] %in% FALSE || usuarios$deleted[j] %in% TRUE) "sin_cuenta" else {
      nuevo <- usuarios
      nuevo$password_hash[j] <- hash_password(contrasena)
      nuevo$password_changed_at[j] <- .reset_iso(ahora)
      nuevo$requires_password_change[j] <- FALSE
      auth_save_usuarios(nuevo, client_id = cid, base = usuarios)
      "ok"
    }
  }, error = function(e) { message("[RESET] password not saved (", cid, "): ", conditionMessage(e)); "error" })
  if (identical(guardado, "sin_cuenta")) return(list(ok = FALSE, error = "cuentas.reset.enlace_invalido"))
  if (!identical(guardado, "ok")) { devolver(); return(list(ok = FALSE, error = "comun.reintentar")) }
  .reset_log("contrasena_cambiada", fila$username, cid)
  # Staff and the client's Dev see it (low severity); a HopDesk account
  # (hd-admin) is never told to a client: .reset_alertar() checks client_id.
  # A client folder holds no staff accounts (reset_cuentas_por_correo()).
  cuenta <- data.frame(cuenta_id = fila$cuenta_id, client_id = cid, username = fila$username,
                       tier = NA_character_, cliente = "", bloqueada = FALSE, stringsAsFactors = FALSE)
  .reset_alertar("cambiada", cuenta, severidad = "info")
  list(ok = TRUE, usuario_oculto = usuario_ocultar(fila$username))
}
