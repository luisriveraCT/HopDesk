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
