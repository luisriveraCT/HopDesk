# =============================================================================
# R/avisos.R
# Avisos (Panorama) and per-user preferences: the A1 contracts every front
# uses. No front reads or writes avisos.rds / preferencias.rds directly.
#
# Every function takes client_id explicitly and fails closed without one.
# Every write re-reads S3 first and applies its change to that fresh copy
# (rebase), so two sessions writing at once never drop each other's rows —
# the lost-update family fixed for movimientos.
#
# Avisos come in three kinds:
#   publicado   written by a person with Publicar; reaches the tiers chosen
#   automatico  written by the app or the scheduled job; reaches every tier,
#               optionally only users holding `permiso`
#   personal    comments and mentions; reaches named users, never a tier
# =============================================================================

.AVISOS_KEY       <- "avisos.rds"
.PREFERENCIAS_KEY <- "preferencias.rds"
AVISOS_TIPOS      <- c("publicado", "automatico", "personal")

.schema_avisos <- function() tibble::tibble(
  id          = character(),
  tipo        = character(),
  titulo      = character(),
  cuerpo      = character(),
  severidad   = character(),   # info | atencion | urgente
  niveles     = character(),   # JSON array of tiers (publicado / automatico)
  usuarios    = character(),   # JSON array of usernames (personal)
  permiso     = character(),   # optional permission key the reader must hold
  clave       = character(),   # dedup key: same live clave updates, never duplicates
  contador    = integer(),     # times a deduplicated aviso was raised again
  enlace      = character(),   # tab or target the aviso opens
  creado_por  = character(),
  creado      = as.POSIXct(character(), tz = "UTC"),
  actualizado = as.POSIXct(character(), tz = "UTC"),
  vence       = as.POSIXct(character(), tz = "UTC"),
  retirado    = logical(),
  retirado_por = character(),
  retirado_en = as.POSIXct(character(), tz = "UTC"),
  hechos      = character()    # JSON array of usernames who marked it done
)

.schema_preferencias <- function() tibble::tibble(
  usuario     = character(),
  datos       = character(),   # JSON object
  actualizado = as.POSIXct(character(), tz = "UTC")
)

PREFS_DEFECTO <- list(inicio = NULL, idioma = "es", audiencia = NULL, ultima_visita = NULL)

.avisos_cid <- function(client_id) {
  cid <- tolower(trimws(client_id %||% ""))
  if (!nzchar(cid)) stop("[avisos] client_id es obligatorio", call. = FALSE)
  cid
}

.json <- function(x) as.character(jsonlite::toJSON(as.character(x %||% character()), auto_unbox = FALSE))
.de_json <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(character())
  tryCatch(as.character(unlist(jsonlite::fromJSON(s))), error = function(e) character())
}

.ahora <- function() as.POSIXct(Sys.time(), tz = "UTC")

# Live read: drops this key from the read caches so a write always rebases
# on what S3 holds now, not on what this process saw earlier.
.s3_leer_vivo <- function(key, client_id) {
  full_key <- tryCatch(.s3_key(key, client_id = client_id), error = function(e) NULL)
  if (!is.null(full_key)) {
    for (env in list(.s3_preload_cache, .s3_missing_cache))
      suppressWarnings(rm(list = intersect(full_key, ls(env)), envir = env))
  }
  .s3_read_with(key, client_id = client_id)
}

.normalizar <- function(df, schema) {
  s <- schema()
  if (is.null(df) || !is.data.frame(df)) return(s)
  for (col in setdiff(names(s), names(df))) df[[col]] <- rep(s[[col]][NA_integer_], nrow(df))
  tibble::as_tibble(df[, names(s), drop = FALSE])
}

# Read fresh, apply `cambio` to the fresh copy, write. One retry on failure,
# re-reading again, so a concurrent writer's rows survive.
.s3_modificar <- function(key, client_id, schema, cambio) {
  for (intento in 1:2) {
    fresco <- .normalizar(.s3_leer_vivo(key, client_id), schema)
    nuevo  <- cambio(fresco)
    ok <- tryCatch({ .s3_write(nuevo, key, client_id = client_id); TRUE },
                   error = function(e) { message("[avisos] escritura falló (", key, "): ", e$message); FALSE })
    if (ok) return(invisible(nuevo))
  }
  stop("[avisos] no se pudo guardar ", key, call. = FALSE)
}

# ── Avisos ───────────────────────────────────────────────────────────────────

avisos_leer <- function(client_id) {
  .normalizar(.s3_read_with(.AVISOS_KEY, client_id = .avisos_cid(client_id)), .schema_avisos)
}

.vigente <- function(df, ahora = .ahora()) {
  !is.na(df$id) & !(df$retirado %in% TRUE) & (is.na(df$vence) | df$vence > ahora)
}

# What `usuario` (list(user, tier, permisos), see usuario_sesion() in app.R)
# may see. Filtered here, on the server, never in the browser.
avisos_visibles <- function(client_id, usuario, df = NULL) {
  df <- df %||% avisos_leer(client_id)
  if (!nrow(df) || is.null(usuario)) return(df[0, ])
  user <- tolower(usuario$user %||% "")
  tier <- usuario$tier %||% ""
  ve <- vapply(seq_len(nrow(df)), function(i) {
    if (identical(df$tipo[i], "personal"))
      return(user %in% tolower(.de_json(df$usuarios[i])))
    if (!tier %in% .de_json(df$niveles[i])) return(FALSE)
    p <- df$permiso[i]
    is.na(p) || !nzchar(p) || puede(usuario, p)
  }, logical(1))
  df[.vigente(df) & ve, , drop = FALSE]
}

# `aviso`: list(tipo, titulo, cuerpo, severidad, niveles, usuarios, permiso,
#               clave, enlace, vence). Returns the aviso id.
# A live aviso with the same `clave` is refreshed (contador + 1) instead of
# duplicated: "3 comentarios nuevos en REQ-2026-0014" is one aviso.
avisos_agregar <- function(client_id, aviso, por = "sistema") {
  cid  <- .avisos_cid(client_id)
  tipo <- aviso$tipo %||% "automatico"
  if (!tipo %in% AVISOS_TIPOS) stop("[avisos] tipo desconocido: ", tipo, call. = FALSE)
  niveles <- aviso$niveles %||% (if (tipo == "automatico") niveles_cliente() else character())
  niveles <- intersect(niveles, niveles_todos())
  if (tipo == "publicado" && !length(niveles))
    stop("[avisos] un aviso publicado necesita al menos un nivel", call. = FALSE)
  id_nuevo <- uuid::UUIDgenerate()
  id_final <- id_nuevo
  ahora <- .ahora()
  .s3_modificar(.AVISOS_KEY, cid, .schema_avisos, function(df) {
    clave <- aviso$clave %||% NA_character_
    if (!is.na(clave)) {
      i <- which(df$clave %in% clave & .vigente(df, ahora))
      if (length(i)) {
        i <- i[1]
        df$contador[i]    <- (df$contador[i] %||% 1L) + 1L
        df$actualizado[i] <- ahora
        df$titulo[i]      <- aviso$titulo %||% df$titulo[i]
        df$cuerpo[i]      <- aviso$cuerpo %||% df$cuerpo[i]
        df$hechos[i]      <- .json(character())   # news again: nobody has it done
        id_final <<- df$id[i]
        return(df)
      }
    }
    fila <- tibble::tibble(
      id = id_nuevo, tipo = tipo,
      titulo = aviso$titulo %||% "", cuerpo = aviso$cuerpo %||% "",
      severidad = aviso$severidad %||% "info",
      niveles = .json(niveles), usuarios = .json(aviso$usuarios),
      permiso = aviso$permiso %||% NA_character_, clave = clave, contador = 1L,
      enlace = aviso$enlace %||% NA_character_, creado_por = por,
      creado = ahora, actualizado = ahora,
      vence = as.POSIXct(aviso$vence %||% NA, tz = "UTC"),
      retirado = FALSE, retirado_por = NA_character_,
      retirado_en = as.POSIXct(NA, tz = "UTC"), hechos = .json(character()))
    dplyr::bind_rows(df, fila)
  })
  id_final
}

avisos_retirar <- function(client_id, id, por) {
  cid <- .avisos_cid(client_id)
  .s3_modificar(.AVISOS_KEY, cid, .schema_avisos, function(df) {
    i <- which(df$id == id)
    df$retirado[i] <- TRUE; df$retirado_por[i] <- por; df$retirado_en[i] <- .ahora()
    df
  })
  invisible(TRUE)
}

avisos_marcar_hecho <- function(client_id, id, usuario) {
  u <- tolower(usuario %||% "")
  cid <- .avisos_cid(client_id)
  if (!nzchar(u)) return(invisible(FALSE))
  .s3_modificar(.AVISOS_KEY, cid, .schema_avisos, function(df) {
    i <- which(df$id == id)
    for (j in i) df$hechos[j] <- .json(union(.de_json(df$hechos[j]), u))
    df
  })
  invisible(TRUE)
}

# Comments and mentions: one personal aviso per recipient, grouped per
# `aviso$clave` (e.g. "req:<id>") until the recipient opens it. Never to the
# author (`aviso$autor`).
avisos_personales <- function(client_id, usuarios, aviso) {
  destino <- setdiff(unique(tolower(usuarios)), tolower(aviso$autor %||% ""))
  ids <- character()
  for (u in destino) {
    a <- aviso
    a$tipo <- "personal"; a$usuarios <- u
    a$clave <- if (!is.null(aviso$clave)) paste0(aviso$clave, "|", u) else NULL
    ids <- c(ids, avisos_agregar(client_id, a, por = aviso$autor %||% "sistema"))
  }
  invisible(ids)
}

# Hook for the team-activity alerts (front C). Called right after one of the
# tracked actions succeeds. It never throws: an alert failing must never
# make the user's action fail.
avisos_actividad <- function(client_id, accion, usuario, params = list()) {
  tryCatch({
    clave_i18n <- paste0("avisos.actividad.", accion)
    titulo <- params$titulo %||%
      (if (is.null(i18n_dict("es")[[clave_i18n]])) accion else tr(clave_i18n, usuario = usuario %||% ""))
    avisos_agregar(client_id, list(
      tipo = "automatico", titulo = titulo, cuerpo = params$cuerpo %||% "",
      severidad = "info", permiso = params$permiso %||% NA_character_,
      clave = params$clave %||% NULL, enlace = params$enlace %||% NA_character_),
      por = usuario %||% "sistema")
  }, error = function(e) {
    message("[avisos] avisos_actividad(", accion, ") falló: ", conditionMessage(e))
    invisible(NULL)
  })
}

# Client tiers with how many live users hold each, highest rank first: what
# the audience bar shows. A tier added to the registry shows up by itself.
audiencia_niveles <- function(client_id) {
  cid <- .avisos_cid(client_id)
  u <- tryCatch(auth_load_usuarios(client_id = cid), error = function(e) NULL)
  vivos <- if (is.data.frame(u) && nrow(u))
    u[(is.na(u$deleted) | !u$deleted) & (is.na(u$activo) | u$activo), , drop = FALSE] else NULL
  niv <- niveles_cliente()
  data.frame(
    nivel    = niv,
    nombre   = vapply(niv, nivel_nombre, character(1)),
    rango    = vapply(niv, tier_rank, integer(1)),
    personas = vapply(niv, function(t) if (is.null(vivos)) 0L else sum(vivos$tier == t), integer(1)),
    stringsAsFactors = FALSE, row.names = NULL)
}

# ── Preferencias ─────────────────────────────────────────────────────────────
# One file per client keyed by username: a username never becomes part of an
# S3 path.

prefs_leer <- function(client_id, usuario) {
  df <- .normalizar(.s3_read_with(.PREFERENCIAS_KEY, client_id = .avisos_cid(client_id)),
                    .schema_preferencias)
  i <- which(tolower(df$usuario) == tolower(usuario %||% ""))
  guardadas <- if (length(i)) tryCatch(jsonlite::fromJSON(df$datos[i[1]], simplifyVector = TRUE),
                                       error = function(e) list()) else list()
  utils::modifyList(PREFS_DEFECTO, as.list(guardadas))
}

prefs_guardar <- function(client_id, usuario, cambios) {
  cid <- .avisos_cid(client_id)
  u <- tolower(usuario %||% "")
  if (!nzchar(u)) stop("[prefs] usuario es obligatorio", call. = FALSE)
  .s3_modificar(.PREFERENCIAS_KEY, cid, .schema_preferencias, function(df) {
    i <- which(tolower(df$usuario) == u)
    actuales <- if (length(i)) tryCatch(jsonlite::fromJSON(df$datos[i[1]], simplifyVector = TRUE),
                                        error = function(e) list()) else list()
    datos <- as.character(jsonlite::toJSON(utils::modifyList(as.list(actuales), cambios),
                                           auto_unbox = TRUE, null = "null"))
    if (length(i)) { df$datos[i[1]] <- datos; df$actualizado[i[1]] <- .ahora() }
    else df <- dplyr::bind_rows(df, tibble::tibble(usuario = u, datos = datos, actualizado = .ahora()))
    df
  })
  invisible(TRUE)
}
