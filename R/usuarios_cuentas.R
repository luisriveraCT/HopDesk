# =============================================================================
# R/usuarios_cuentas.R
# T31: what a person types to get or recover an account -- user name, name,
# job title, e-mail, password -- checked on the server, one rule each, the
# same on every screen (invitation, Usuarios, "olvidé", reset).
#
# Every value arrives from a form, so it is untrusted:
#   - .campo() accepts exactly one valid UTF-8 string, cut by BYTES before
#     anything else touches it (a 5 MB field never reaches a regex, scrypt
#     or S3);
#   - the user name and the e-mail are matched against an allow-list
#     pattern; the user's value is never itself used as a pattern (lookups
#     compare with ==);
#   - name and job title refuse control characters, < > (they are printed
#     in pages and mails) and { } (tr() fills {placeholders} one after the
#     other, so a "{cliente}" inside a name would be filled by the next one).
# Each check returns NULL when the value is fine, or the dictionary key of
# the message to show.
#
# reservar_usuario(): a user name is unique in ALL of HopDesk, not only in
# its client -- production runs as hd-admin and shinymanager logs in by user
# name alone over every client's accounts (T31 paso 0, Luis 2026-11-12).
# =============================================================================

USUARIO_MIN    <- 3L
USUARIO_MAX    <- 30L
NOMBRE_MAX     <- 80L
PUESTO_MAX     <- 80L
CORREO_MAX     <- 254L
CONTRASENA_MIN <- 8L
CONTRASENA_MAX <- 128L

# One string or NA: NULL, a vector, a list, NA, invalid UTF-8 or more than
# max_chars * 4 bytes (the most a character can take) are all NA.
.campo <- function(x, max_chars) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(NA_character_)
  if (nchar(x, type = "bytes") > max_chars * 4L) return(NA_character_)
  if (!validUTF8(x)) return(NA_character_)
  x
}

.tiene_control <- function(x) grepl("[[:cntrl:]]", x, perl = TRUE)

# ── User name ────────────────────────────────────────────────────────────────
usuario_normalizar <- function(x) {
  x <- .campo(x, USUARIO_MAX + 10L)
  if (is.na(x)) return(NA_character_)
  tolower(trimws(x))
}

usuario_error <- function(x) {
  u <- usuario_normalizar(x)
  if (is.na(u) || !nzchar(u)) return("cuentas.error.usuario_obligatorio")
  if (grepl("@", u, fixed = TRUE)) return("cuentas.error.usuario_correo")
  if (!grepl(sprintf("^[a-z0-9_]{%d,%d}$", USUARIO_MIN, USUARIO_MAX), u))
    return("cuentas.error.usuario_formato")
  NULL
}

# What the "olvidé" mail shows instead of the user name (Luis): the first
# and the last letter -- "l…s" for luis --, only the first for short ones.
usuario_ocultar <- function(x) {
  u <- usuario_normalizar(x)
  if (is.na(u) || !nzchar(u)) return("…")
  n <- nchar(u)
  if (n <= 3L) paste0(substr(u, 1, 1), "…")
  else paste0(substr(u, 1, 1), "…", substr(u, n, n))
}

# ── Name and job title ───────────────────────────────────────────────────────
.texto_error <- function(x, max, obligatorio, largo) {
  v <- .campo(x, max)
  if (is.na(v)) return(if (is.character(x) && length(x) == 1L && !is.na(x)) largo else obligatorio)
  v <- trimws(v)
  if (!nzchar(v)) return(obligatorio)
  if (nchar(v) > max) return(largo)
  if (.tiene_control(v) || grepl("[<>{}]", v)) return("cuentas.error.caracteres")
  NULL
}
nombre_error <- function(x) .texto_error(x, NOMBRE_MAX, "cuentas.error.nombre_obligatorio",
                                         "cuentas.error.nombre_largo")
puesto_error <- function(x) .texto_error(x, PUESTO_MAX, "cuentas.error.puesto_obligatorio",
                                         "cuentas.error.puesto_largo")
texto_limpio <- function(x) trimws(.campo(x, NOMBRE_MAX))

# ── E-mail ───────────────────────────────────────────────────────────────────
correo_normalizar <- function(x) {
  x <- .campo(x, CORREO_MAX)
  if (is.na(x)) return(NA_character_)
  tolower(trimws(x))
}

correo_error <- function(x) {
  c0 <- correo_normalizar(x)
  if (is.na(c0) || !nzchar(c0)) return("cuentas.error.correo_formato")
  if (nchar(c0) > CORREO_MAX) return("cuentas.error.correo_formato")
  if (!grepl("^[a-z0-9._%+-]{1,64}@[a-z0-9-]+(\\.[a-z0-9-]+)*\\.[a-z]{2,}$", c0, perl = TRUE))
    return("cuentas.error.correo_formato")
  NULL
}

# ── Password ─────────────────────────────────────────────────────────────────
# One rule for every screen (Luis, 2026-11-12): 8 to 128 characters, at
# least one digit and one symbol. Existing passwords are not touched: this
# only runs when a password is set.
contrasena_error <- function(x) {
  p <- .campo(x, CONTRASENA_MAX)
  if (is.na(p)) return(if (is.character(x) && length(x) == 1L && !is.na(x))
    "cuentas.error.contrasena_larga" else "cuentas.error.contrasena_regla")
  if (nchar(p) > CONTRASENA_MAX) return("cuentas.error.contrasena_larga")
  if (.tiene_control(p)) return("cuentas.error.caracteres")
  if (nchar(p) < CONTRASENA_MIN || !grepl("[0-9]", p) || !grepl("[^A-Za-z0-9]", p))
    return("cuentas.error.contrasena_regla")
  NULL
}

# ── Unique user name ─────────────────────────────────────────────────────────
.usuario_ocupado <- function(u)
  structure(class = c("usuario_ocupado", "error", "condition"),
            list(message = paste0("[UINDEX] user name taken: ", u), call = NULL))

# Claims `username` in hd-admin/username_index.rds, or stop()s:
#   - class "usuario_ocupado" when an account already has it -- in the index
#     (read strictly, inside the same change that claims it) or in `ocupados`
#     (the logins loaded now and the client's own table: accounts older than
#     the index may never have been registered in it);
#   - any other error when the index cannot be read or written (never taken
#     for "free": a failed read is not an empty index, A2).
# The caller saves the account after this and, if that save fails, gives the
# name back with unregister_username().
reservar_usuario <- function(username, client_id, account_code, ocupados = character()) {
  u <- usuario_normalizar(username)
  if (!is.null(usuario_error(u))) stop("[UINDEX] invalid user name", call. = FALSE)
  if (u %in% tolower(trimws(as.character(ocupados)))) stop(.usuario_ocupado(u))
  modificar_hd_admin("username_index.rds", .schema_username_index, function(idx) {
    mismo <- tolower(idx$username) == u
    if (any(mismo & idx$active %in% TRUE)) stop(.usuario_ocupado(u))
    if (any(mismo)) {
      i <- which(mismo)[1]
      idx$active[i] <- TRUE; idx$home_folder[i] <- client_id; idx$account_code[i] <- account_code
      return(idx)
    }
    .hd_admin_agregar(idx, data.frame(username = u, home_folder = client_id, account_code = account_code,
                                      created_at = as.character(Sys.time()), active = TRUE,
                                      stringsAsFactors = FALSE))
  })
  invisible(u)
}

# Adds the account `fila` (one row) to the client's table `usuarios` (read
# strictly by the caller): reserve its user name, save, and give the name
# back if the save fails -- one path for the activation and for Usuarios'
# "Nueva cuenta". `ocupados`: see reservar_usuario(); the client's own user
# names are added here. Errors as reservar_usuario() and auth_save_usuarios().
guardar_cuenta_nueva <- function(usuarios, fila, client_id, ocupados = character()) {
  u <- reservar_usuario(fila$username, client_id, fila$account_code,
                        ocupados = c(as.character(usuarios$username), ocupados))
  tryCatch(auth_save_usuarios(.hd_admin_agregar(usuarios, fila), client_id = client_id, base = usuarios),
           error = function(e) {
             tryCatch(unregister_username(u), error = function(e2)
               message("[UINDEX] could not give back '", u, "': ", conditionMessage(e2)))
             stop(e)
           })
  invisible(u)
}

# The logins loaded now, as a safety net for reservar_usuario(); never the
# authority (a failed load is just "nothing more to add").
.usuarios_en_credenciales <- function()
  tryCatch(as.character(auth_get_credentials()$user), error = function(e) character())

# ── Activation (?invite=) ────────────────────────────────────────────────────
# campos: list(nombre, puesto, usuario, contrasena, confirmar) as typed.
# Returns list(ok = FALSE, errores = list(<campo> = <key>)) when a field is
# wrong or the user name is taken, or list(ok = TRUE, username, nombre,
# account_code) once the account is saved and the invitation used. A failed
# read or write stop()s: the page says "try again", nothing half-saved.
activar_invitacion <- function(invite, campos) {
  errores <- list()
  for (campo in c("nombre", "puesto", "usuario", "contrasena")) {
    e <- switch(campo, nombre = nombre_error(campos$nombre), puesto = puesto_error(campos$puesto),
                usuario = usuario_error(campos$usuario), contrasena = contrasena_error(campos$contrasena))
    if (!is.null(e)) errores[[campo]] <- e
  }
  if (is.null(errores$contrasena) && !identical(campos$contrasena, campos$confirmar))
    errores$confirmar <- "cuentas.error.contrasenas_distintas"
  if (length(errores)) return(list(ok = FALSE, errores = errores))

  cid <- invite$client_id
  usuarios <- auth_load_usuarios(client_id = cid)   # stop()s on a failed read
  ahora <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  fila <- data.frame(
    id = uuid::UUIDgenerate(), account_code = .next_account_code(usuarios),
    username = usuario_normalizar(campos$usuario), password_hash = hash_password(campos$contrasena),
    display_name = texto_limpio(campos$nombre), tier = invite$tier, client_id = cid,
    permisos = "{}", group_ids = "[]", allowed_clients = "[]", email = invite$email,
    requires_password_change = FALSE, puesto = texto_limpio(campos$puesto),
    password_changed_at = ahora, activo = TRUE, created_at = ahora, last_login = NA_character_,
    deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE)
  # The invitation is taken FIRST, only if it is still pending, inside one
  # change of pending_invites (the integrator's review): two tabs, or a
  # retry after a failure, can no longer make two accounts from one
  # invitation. It is given back if the account is not saved.
  if (!.tomar_invitacion(invite$token))
    return(list(ok = FALSE, errores = list(invitacion = "cuentas.activacion.enlace_ya_no")))
  r <- tryCatch(guardar_cuenta_nueva(usuarios, fila, cid, ocupados = .usuarios_en_credenciales()),
                usuario_ocupado = function(e) e,
                error = function(e) { .devolver_invitacion(invite$token); stop(e) })
  if (inherits(r, "usuario_ocupado")) {
    .devolver_invitacion(invite$token)
    return(list(ok = FALSE, errores = list(usuario = "cuentas.error.usuario_ocupado")))
  }
  list(ok = TRUE, username = fila$username, nombre = fila$display_name, account_code = fila$account_code)
}

# pending -> accepted on S3's current table; FALSE if it was not pending.
.tomar_invitacion <- function(token) {
  tomada <- FALSE
  modificar_pending_invites(function(inv) {
    i <- which(inv$token == token & inv$status %in% "pending")
    if (!length(i)) return(inv)
    inv$status[i[1]] <- "accepted"
    inv$accepted_at[i[1]] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    tomada <<- TRUE
    inv
  })
  tomada
}
.devolver_invitacion <- function(token)
  tryCatch(modificar_pending_invites(function(inv) {
    i <- which(inv$token == token & inv$status %in% "accepted")
    if (length(i)) { inv$status[i[1]] <- "pending"; inv$accepted_at[i[1]] <- NA_character_ }
    inv
  }), error = function(e) message("[INVITE] could not give the invitation back: ", conditionMessage(e)))

# A dictionary sentence with some values in bold, every piece escaped:
# tr_negritas("cuentas.activacion.saludo", nombre = x, cliente = y) gives
# "Hola <strong>x</strong>. ...". The values go in after the template is
# escaped, each escaped itself, so nothing typed by anyone becomes markup.
tr_negritas <- function(clave, ...) {
  vals <- list(...)
  marcas <- stats::setNames(as.list(sprintf("\u0001%d\u0001", seq_along(vals))), names(vals))
  txt <- htmltools::htmlEscape(do.call(tr, c(list(clave), marcas)))
  for (i in seq_along(vals)) {
    pieza <- paste0("<strong>", htmltools::htmlEscape(as.character(vals[[i]])), "</strong>")
    # gsub reads "\\" in a replacement even with fixed = TRUE: doubled, a
    # backslash someone typed stays one backslash.
    txt <- gsub(marcas[[i]], gsub("\\", "\\\\", pieza, fixed = TRUE), txt, fixed = TRUE)
  }
  htmltools::HTML(txt)
}
