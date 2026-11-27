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

# Avisos live under <cliente>/avisos/ since D (2026-11): the scheduled job's
# AWS role may only write there. The old key is read until the new one
# exists, and its rows become the new file on the first write; it is never
# written again and never deleted (Luis: nothing is deleted; the role cannot).
.AVISOS_KEY       <- "avisos/avisos.rds"
.AVISOS_KEY_VIEJA <- "avisos.rds"
.PREFERENCIAS_KEY <- "preferencias.rds"
AVISOS_TIPOS      <- c("publicado", "automatico", "personal")
# What a person publishes (B, Luis 2026-11-12): each has its own tone and tag
# in Panorama. A solicitud also goes to named people (Mis solicitudes). Rows
# written before the column existed read as NA: a publicado with no clase is
# a comunicado (avisos_clase()).
AVISOS_CLASES     <- c("comunicado", "recordatorio", "reconocimiento", "solicitud")

.schema_avisos <- function() tibble::tibble(
  id          = character(),
  tipo        = character(),
  clase       = character(),   # AVISOS_CLASES; NA for automatic ones
                               # (and for comments/mentions)
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
  hechos      = character(),   # JSON array of usernames who marked it done
  descartados = character()    # T95: JSON array of usernames who took it off their own view (NA = nobody)
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

.normalizar <- function(df, schema) {
  s <- schema()
  if (is.null(df) || !is.data.frame(df)) return(s)
  for (col in setdiff(names(s), names(df))) df[[col]] <- rep(s[[col]][NA_integer_], nrow(df))
  tibble::as_tibble(df[, names(s), drop = FALSE])
}

# Read fresh, apply `cambio` to the fresh copy, write. One retry on failure,
# re-reading again, so a concurrent writer's rows survive.
# The read is strict (C): a read that fails stop()s, and only a file S3 says
# does not exist starts empty. It used .s3_leer_vivo(), where a failed read
# was an empty table: prefs_guardar() then rewrote preferencias.rds with the
# one user's row, and everyone else's preferences were gone.
.s3_modificar <- function(key, client_id, schema, cambio) {
  for (intento in 1:2) {
    leido  <- if (identical(key, .AVISOS_KEY)) .avisos_leer_para_escribir(client_id)
              else .s3_leer_para_escribir(key, client_id)
    fresco <- .normalizar(leido, schema)
    nuevo  <- cambio(fresco)
    # T119: nothing changed, nothing written (avisos_agregar with repetir =
    # FALSE rewrote avisos.rds as it was). Only after a read that WORKED: a
    # failed read stop()s above and never reaches here.
    if (identical(nuevo, fresco)) return(invisible(nuevo))
    ok <- tryCatch({ .s3_write(nuevo, key, client_id = client_id); TRUE },
                   error = function(e) { message("[avisos] escritura falló (", key, "): ", e$message); FALSE })
    if (ok) return(invisible(nuevo))
  }
  stop("[avisos] no se pudo guardar ", key, call. = FALSE)
}

# ── Avisos ───────────────────────────────────────────────────────────────────

# Strict read for a write: the new key; only when S3 says it does not exist,
# the old one (strict too). A failed read of either stop()s, so a first write
# never starts from an empty table while the old file holds avisos.
.avisos_leer_para_escribir <- function(client_id) {
  x <- .s3_leer_para_escribir(.AVISOS_KEY, client_id)
  if (is.null(x)) x <- .s3_leer_para_escribir(.AVISOS_KEY_VIEJA, client_id)
  x
}

avisos_leer <- function(client_id) {
  cid <- .avisos_cid(client_id)
  x <- .s3_read_with(.AVISOS_KEY, client_id = cid)
  # The old key only while the new one does not exist (S3 says so), never
  # because a read of the new one failed: that would show stale avisos.
  if (is.null(x) && identical(.s3_existe(.AVISOS_KEY, cid), FALSE))
    x <- .s3_read_with(.AVISOS_KEY_VIEJA, client_id = cid)
  .normalizar(x, .schema_avisos)
}

.vigente <- function(df, ahora = .ahora()) {
  !is.na(df$id) & !(df$retirado %in% TRUE) & (is.na(df$vence) | df$vence > ahora)
}

# What `usuario` (list(user, tier, permisos), see usuario_sesion() in app.R)
# may see. Filtered here, on the server, never in the browser.
#
# T91a, propios = TRUE (Panorama): the author ALSO sees what they published
# -- a publication (tipo "publicado"), a solicitud (clave "sol:<id>|<user>":
# one personal aviso per recipient, never to the author) and a public note
# (clave "nota|<id>") -- even when their tier or name is not in its
# audience: Luis asked "tesoreria" for something and could not see it. Only
# what a person published, never the automatic ones written in their name
# (activity: "Ana confirmó un pago"). Only in this client's avisos.rds: what
# staff published in another client is not here. Those rows come with
# propia = TRUE; a solicitud comes once per group with how many recipients
# marked it done (sol_atendidas / sol_total). The weekly email does not ask
# for them (propios = FALSE): someone's own posts are not their news.
avisos_visibles <- function(client_id, usuario, df = NULL, propios = FALSE) {
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
  vig <- .vigente(df)
  out <- df[vig & ve, , drop = FALSE]
  if (!isTRUE(propios)) return(out)
  out$propia <- rep(FALSE, nrow(out)); out$sol_atendidas <- NA_integer_; out$sol_total <- NA_integer_
  mio <- vig & nzchar(user) & tolower(trimws(df$creado_por %||% "")) %in% user
  clave <- df$clave %||% rep(NA_character_, nrow(df))
  es_pub  <- mio & df$tipo %in% "publicado"
  es_nota <- mio & startsWith(ifelse(is.na(clave), "", clave), "nota|")
  es_sol  <- mio & df$tipo %in% "personal" & startsWith(ifelse(is.na(clave), "", clave), "sol:")
  # Already in `out` (their tier was in the audience): mark them as theirs.
  out$propia[out$id %in% df$id[es_pub | es_nota]] <- TRUE
  extra <- df[(es_pub | es_nota) & !(df$id %in% out$id), , drop = FALSE]
  if (nrow(extra)) {
    extra$propia <- TRUE; extra$sol_atendidas <- NA_integer_; extra$sol_total <- NA_integer_
    out <- rbind(out, extra)
  }
  if (any(es_sol)) {
    s <- df[es_sol, , drop = FALSE]
    grupo <- sub("[|].*$", "", s$clave)
    filas <- lapply(split(seq_len(nrow(s)), grupo), function(k) {
      g <- s[k, , drop = FALSE]
      hecho <- vapply(seq_len(nrow(g)), function(j)
        any(tolower(.de_json(g$usuarios[j])) %in% tolower(.de_json(g$hechos[j]))), logical(1))
      r <- g[1, , drop = FALSE]
      r$usuarios <- .json(unique(unlist(lapply(g$usuarios, .de_json))))
      r$propia <- TRUE; r$sol_atendidas <- as.integer(sum(hecho)); r$sol_total <- as.integer(nrow(g))
      r
    })
    out <- rbind(out[!(out$id %in% s$id), , drop = FALSE], do.call(rbind, filas))
  }
  out
}

# `aviso`: list(tipo, titulo, cuerpo, severidad, niveles, usuarios, permiso,
#               clave, enlace, vence, ventana_min). Returns the aviso id.
# A live aviso with the same `clave` is refreshed (contador + 1) instead of
# duplicated: "3 comentarios nuevos en REQ-2026-0014" is one aviso.
# With `ventana_min` (C), only one updated in the last ventana_min minutes
# is refreshed: three payments confirmed in 10 minutes are one aviso, and
# one confirmed an hour later is a new one.
avisos_agregar <- function(client_id, aviso, por = "sistema") {
  cid  <- .avisos_cid(client_id)
  tipo <- aviso$tipo %||% "automatico"
  if (!tipo %in% AVISOS_TIPOS) stop("[avisos] tipo desconocido: ", tipo, call. = FALSE)
  niveles <- aviso$niveles %||% (if (tipo == "automatico") niveles_cliente() else character())
  niveles <- intersect(niveles, niveles_todos())
  if (tipo == "publicado" && !length(niveles))
    stop("[avisos] un aviso publicado necesita al menos un nivel", call. = FALSE)
  clase <- aviso$clase %||% (if (tipo == "publicado") "comunicado" else NA_character_)
  if (!is.na(clase) && !clase %in% AVISOS_CLASES)
    stop("[avisos] clase desconocida: ", clase, call. = FALSE)
  id_nuevo <- uuid::UUIDgenerate()
  id_final <- id_nuevo
  ahora <- .ahora()
  .s3_modificar(.AVISOS_KEY, cid, .schema_avisos, function(df) {
    clave <- aviso$clave %||% NA_character_
    if (!is.na(clave)) {
      # aviso$repetir = FALSE (T62): a clave already there -- live, retired or
      # expired -- is left as it is: an event published twice (two sessions)
      # never turns "new" again, nor comes back. Default TRUE: today's refresh.
      if (identical(aviso$repetir, FALSE) && any(df$clave %in% clave)) {
        id_final <<- df$id[which(df$clave %in% clave)[1]]
        return(df)
      }
      junta <- df$clave %in% clave & .vigente(df, ahora)
      if (!is.null(aviso$ventana_min))
        junta <- junta & !is.na(df$actualizado) &
          df$actualizado >= ahora - as.numeric(aviso$ventana_min) * 60
      i <- which(junta)
      if (length(i)) {
        i <- i[1]
        df$contador[i]    <- (df$contador[i] %||% 1L) + 1L
        df$actualizado[i] <- ahora
        df$titulo[i]      <- aviso$titulo %||% df$titulo[i]
        df$cuerpo[i]      <- aviso$cuerpo %||% df$cuerpo[i]
        df$hechos[i]      <- .json(character())   # news again: nobody has it done
        df$descartados[i] <- NA_character_        # T95: nor discarded (NA = nobody, as rows from before T95)
        id_final <<- df$id[i]
        return(df)
      }
    }
    fila <- tibble::tibble(
      id = id_nuevo, tipo = tipo, clase = clase,
      titulo =aviso$titulo %||% "", cuerpo = aviso$cuerpo %||% "",
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

# ── T95: each user's own view (descartar, restaurar, what leaves by itself) ──
# Descartar takes an aviso off ONE user's view (`descartados`, like `hechos`):
# everyone else still sees it, and it stays in that user's history. Nothing
# is deleted: retirar (for everyone) stays the author's or a moderator's.

# How many days after it came an aviso already SEEN leaves the default view,
# by its Panorama kind (panorama_tipo_feed()). A comunicado only without a
# vence (with one, the vence decides, as today). Solicitudes and alerts never
# leave by days: a solicitud for me waits for "Hecho", an alert for its vence
# or for being solved.
AVISOS_VENTANA_VISTO <- c(act = 3, req = 7, com = 14)

# Who has it discarded, one logical per row of `df` (lower-cased usernames).
avisos_descartado <- function(df, usuario) {
  u <- tolower(trimws(usuario %||% ""))
  if (!nrow(df) || !nzchar(u)) return(rep(FALSE, nrow(df)))
  d <- df$descartados %||% rep(NA_character_, nrow(df))
  vapply(d, function(s) u %in% tolower(.de_json(s)), logical(1), USE.NAMES = FALSE)
}

# What `usuario` may take off their view, one logical per row: everything but
# a solicitud addressed to them that they have not done (it goes when they
# mark it "Hecho", or when its author retires it). Checked on the server.
avisos_descartable <- function(df, usuario) {
  u <- tolower(trimws(usuario %||% ""))
  if (!nrow(df)) return(logical())
  para_mi <- df$tipo %in% "personal" &
    vapply(df$usuarios, function(s) u %in% tolower(.de_json(s)), logical(1), USE.NAMES = FALSE)
  hecha <- vapply(df$hechos, function(s) u %in% tolower(.de_json(s)), logical(1), USE.NAMES = FALSE)
  !(df$clase %in% "solicitud" & para_mi & !hecha)   # an old row without clase is never a solicitud
}

# Whether a SEEN aviso already left the default view by its days (it stays in
# the history). Seen = it came at or before the user's last visit to Panorama
# (preferencias ultima_visita): what was never seen never leaves by itself.
# Vectors of one length: tipo_feed (act | req | com | sol | ale), cuando (when
# it came or was refreshed), vence; ultima_visita one value or NULL.
avisos_fuera_de_vista <- function(tipo_feed, cuando, ultima_visita, vence = NULL,
                                  ahora = .ahora(), ventanas = AVISOS_VENTANA_VISTO) {
  n <- length(tipo_feed)
  if (!n) return(logical())
  uv <- suppressWarnings(as.POSIXct(ultima_visita %||% NA, tz = "UTC"))
  cuando <- as.POSIXct(cuando, tz = "UTC")
  vence <- if (is.null(vence)) rep(as.POSIXct(NA, tz = "UTC"), n) else as.POSIXct(vence, tz = "UTC")
  visto <- !is.na(uv) & !is.na(cuando) & cuando <= uv
  dias <- unname(ventanas[as.character(tipo_feed)])
  dias[tipo_feed %in% "com" & !is.na(vence)] <- NA   # a comunicado with a vence: the vence decides
  edad <- as.numeric(difftime(ahora, cuando, units = "days"))
  visto & !is.na(dias) & !is.na(edad) & edad >= dias
}

# The write of a user's view change: strict re-read, the change on the fresh
# copy, one retry -- as .s3_modificar() -- but always with its T72 event, so
# the log has who discarded what (the write logs itself once S3 has it).
modificar_avisos_vista <- function(cid, cambio, evento) {
  for (intento in 1:2) {
    fresco <- .normalizar(.avisos_leer_para_escribir(cid), .schema_avisos)
    nuevo  <- cambio(fresco)
    ok <- tryCatch({ .s3_write(nuevo, .AVISOS_KEY, client_id = cid, evento = evento); TRUE },
                   error = function(e) { message("[avisos] escritura falló (descartar): ", e$message); FALSE })
    if (ok) return(invisible(nuevo))
  }
  stop("[avisos] no se pudo guardar ", .AVISOS_KEY, call. = FALSE)
}

# Take avisos off `usuario`'s view. `accion`: "descartar_aviso" (one, the x
# or a swipe) or "limpiar_avisos" (Limpiar lo visto, many at once). What may
# not be discarded (avisos_descartable()) is refused here, whatever the
# screen sent. Returns list(descartados, rechazados): ids.
avisos_descartar <- function(client_id, ids, usuario, home = NULL,
                             accion = c("descartar_aviso", "limpiar_avisos")) {
  cid <- .avisos_cid(client_id)
  accion <- match.arg(accion)
  u <- tolower(trimws(usuario %||% ""))
  ids <- unique(as.character(ids %||% character()))
  res <- list(descartados = character(), rechazados = character())
  if (!nzchar(u) || !length(ids)) return(invisible(res))
  evento <- list(usuario = u, modulo = "panorama", accion = accion,
                 descripcion = tr("avisos.log.descartar", n = length(ids)), home = home)
  modificar_avisos_vista(cid, function(df) {
    i <- which(df$id %in% ids)
    puede <- avisos_descartable(df[i, , drop = FALSE], u)
    res$rechazados  <<- df$id[i[!puede]]
    res$descartados <<- df$id[i[puede]]
    for (j in i[puede]) df$descartados[j] <- .json(union(.de_json(df$descartados[j]), u))
    df
  }, evento = evento)
  invisible(res)
}

# Put discarded avisos back on `usuario`'s view (the history's "Volver a
# mostrar", and Deshacer). Returns the ids it changed.
avisos_restaurar <- function(client_id, ids, usuario, home = NULL) {
  cid <- .avisos_cid(client_id)
  u <- tolower(trimws(usuario %||% ""))
  ids <- unique(as.character(ids %||% character()))
  hechos <- character()
  if (!nzchar(u) || !length(ids)) return(invisible(hechos))
  evento <- list(usuario = u, modulo = "panorama", accion = "restaurar_aviso",
                 descripcion = tr("avisos.log.restaurar", n = length(ids)), home = home)
  modificar_avisos_vista(cid, function(df) {
    for (j in which(df$id %in% ids)) {
      antes <- .de_json(df$descartados[j])
      if (u %in% tolower(antes)) hechos <<- c(hechos, df$id[j])
      df$descartados[j] <- .json(setdiff(antes, u))
    }
    df
  }, evento = evento)
  invisible(hechos)
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
# make the user's action fail. It publishes only when the Alertas del equipo
# matrix marks this user for this action (alertas_preparar(), which also
# sets the aviso's permiso, clave and 10-minute window).
#
# al_responder = session (T108): publish AFTER the user has the answer --
# on the session's next flush, once the outputs went out -- instead of
# inside the save. Its S3 round trips (~1.2 s measured on a wizard save)
# leave the path the user waits on. The arguments are taken now, as the
# call made them; nothing is lost: it runs once, on that flush, and a
# failure is logged with "[avisos]" as always, never silently. With no
# session (scripts, persistence code), it publishes right away as before.
avisos_actividad <- function(client_id, accion, usuario, params = list(), al_responder = NULL) {
  if (!is.null(al_responder) && is.function(al_responder$onFlushed)) {
    cid <- tryCatch(client_id, error = function(e) e)
    pr  <- tryCatch(params,    error = function(e) e)
    us  <- tryCatch(usuario,   error = function(e) e)
    if (inherits(cid, "error") || inherits(pr, "error") || inherits(us, "error")) {
      message("[avisos] avisos_actividad(", accion, ") falló: ",
              conditionMessage(Filter(function(x) inherits(x, "error"), list(cid, pr, us))[[1]]))
      return(invisible(NULL))
    }
    al_responder$onFlushed(function() {
      tryCatch(avisos_actividad(cid, accion, us, pr),
               error = function(e) message("[avisos] avisos_actividad(", accion, ") diferido falló: ", conditionMessage(e)))
    }, once = TRUE)
    return(invisible(NULL))
  }
  tryCatch({
    params <- alertas_preparar(client_id, accion, usuario, params); if (is.null(params)) return(invisible(NULL))
    clave_i18n <- paste0("avisos.actividad.", accion)
    titulo <- params$titulo %||%
      (if (is.null(i18n_dict("es")[[clave_i18n]])) accion else tr(clave_i18n, usuario = usuario %||% ""))
    avisos_agregar(client_id, list(
      tipo = "automatico", titulo = titulo, cuerpo = params$cuerpo %||% "",
      severidad = "info", permiso = params$permiso %||% NA_character_,
      clave = params$clave %||% NULL, enlace = params$enlace %||% NA_character_,
      ventana_min = params$ventana_min %||% NULL),
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

# Preferences that are not logged (T72): they change on their own, by the
# volume of use, not by a decision -- like the "leído" marks (the
# Coordinador's call). Panorama writes ultima_visita on every visit.
PREFS_SIN_LOG <- c("ultima_visita")

# T68: one preference block REPLACED whole (datos[[clave]] <- valor), never
# merged: prefs_guardar() merges with modifyList(), which is recursive -- a
# channel taken out of avisos_de would survive and a NULL would delete the
# key. Same strict re-read as prefs_guardar() (.s3_modificar(): a failed read
# stops and writes nothing); only this user's `clave` changes. Wrap vectors
# that must stay arrays in I() (auto_unbox).
# sub: replace only datos[[clave]][[sub]] (a staff member's preference for
# one client, kept in their home client: avisos_de_clientes[[<client>]]);
# the block's other entries stay as they are.
prefs_reemplazar <- function(client_id, usuario, clave, valor, sub = NULL) {
  cid <- .avisos_cid(client_id)
  u <- tolower(usuario %||% "")
  if (!nzchar(u)) stop("[prefs] usuario es obligatorio", call. = FALSE)
  if (!is.character(clave) || length(clave) != 1L || !nzchar(clave)) stop("[prefs] clave es obligatoria", call. = FALSE)
  cambio_log <- list()
  .s3_modificar(.PREFERENCIAS_KEY, cid, .schema_preferencias, function(df) {
    i <- which(tolower(df$usuario) == u)
    actuales <- if (length(i)) tryCatch(jsonlite::fromJSON(df$datos[i[1]], simplifyVector = FALSE),
                                        error = function(e) list()) else list()
    actuales <- as.list(actuales)
    antes <- if (is.null(sub)) actuales[[clave]] else if (is.list(actuales[[clave]])) actuales[[clave]][[sub]]
    nombre <- if (is.null(sub)) clave else paste0(clave, "$", sub)
    # T72: logged as prefs_guardar() logs, only when something changed.
    cambio_log <<- list()
    if (!identical(jsonlite::toJSON(antes, auto_unbox = TRUE, null = "null"), jsonlite::toJSON(valor, auto_unbox = TRUE, null = "null")))
      cambio_log[[nombre]] <<- list(antes = antes, despues = valor)
    if (is.null(sub)) actuales[clave] <- list(valor)
    else {
      bloque <- actuales[[clave]]
      bloque <- if (is.list(bloque) && !is.null(names(bloque))) bloque else stats::setNames(list(), character())
      bloque[sub] <- list(valor)
      actuales[clave] <- list(bloque)
    }
    datos <- as.character(jsonlite::toJSON(actuales, auto_unbox = TRUE, null = "null"))
    if (length(i)) { df$datos[i[1]] <- datos; df$actualizado[i[1]] <- .ahora() }
    else df <- dplyr::bind_rows(df, tibble::tibble(usuario = u, datos = datos, actualizado = .ahora()))
    df
  })
  if (length(cambio_log))
    tryCatch(log_action(user = u, module = "preferencias", action = "guardar_preferencias",
                        description = tr("avisos.log.preferencias", claves = paste(names(cambio_log), collapse = ", ")),
                        target_id = u, metadata = list(cambios = cambio_log),
                        client_id = cid, viewer_home_client_id = cid),
             error = function(e) message("[prefs] log_action fall\u00f3: ", conditionMessage(e)))
  invisible(TRUE)
}

# home: the actor's own client (T72; staff jumping in), NULL = the client.
prefs_guardar <- function(client_id, usuario, cambios, home = NULL) {
  cid <- .avisos_cid(client_id)
  u <- tolower(usuario %||% "")
  if (!nzchar(u)) stop("[prefs] usuario es obligatorio", call. = FALSE)
  cambio_log <- list()
  .s3_modificar(.PREFERENCIAS_KEY, cid, .schema_preferencias, function(df) {
    i <- which(tolower(df$usuario) == u)
    actuales <- if (length(i)) tryCatch(jsonlite::fromJSON(df$datos[i[1]], simplifyVector = TRUE),
                                        error = function(e) list()) else list()
    # T72: only what really changed, with its antes and despues (computed
    # again on .s3_modificar's retry, on what it reads then).
    cambio_log <<- list()
    for (k in setdiff(names(cambios), PREFS_SIN_LOG))
      if (!identical(actuales[[k]], cambios[[k]]))
        cambio_log[[k]] <<- list(antes = actuales[[k]], despues = cambios[[k]])
    datos <- as.character(jsonlite::toJSON(utils::modifyList(as.list(actuales), cambios),
                                           auto_unbox = TRUE, null = "null"))
    if (length(i)) { df$datos[i[1]] <- datos; df$actualizado[i[1]] <- .ahora() }
    else df <- dplyr::bind_rows(df, tibble::tibble(usuario = u, datos = datos, actualizado = .ahora()))
    df
  })
  # After the write; a log that fails never fails the save (T72, category
  # "preferencias": avisos, "De quién recibo avisos", Panorama).
  if (length(cambio_log))
    tryCatch(log_action(user = u, module = "preferencias", action = "guardar_preferencias",
                        description = tr("avisos.log.preferencias", claves = paste(names(cambio_log), collapse = ", ")),
                        target_id = u, metadata = list(cambios = cambio_log),
                        client_id = cid, viewer_home_client_id = home %||% cid),
             error = function(e) message("[prefs] log_action fall\u00f3: ", conditionMessage(e)))
  invisible(TRUE)
}
