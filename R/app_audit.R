# =============================================================================
# R/app_audit.R — Application audit log with chunk rotation
#
# Layout per client:
#   <client_id>/app_audit.rds          ← current live chunk
#   <client_id>/app_audit_001.rds      ← archived (oldest)
#   <client_id>/app_audit_002.rds, …
#
# Rotation triggers when the live chunk reaches .MAX_AUDIT_ROWS (10,000).
# The live file is renamed to the next sequential index; a fresh file starts.
#
# Dual-write: when log_action() is called with client_id != CLIENT_ID env var,
# it also appends to the deployment's own log — capturing cross-client staff
# actions in both the target client's log and the home deployment's log.
# =============================================================================

.APP_AUDIT_SCHEMA <- function() tibble::tibble(
  id          = character(),
  ts          = as.POSIXct(character()),
  user        = character(),
  module      = character(),
  action      = character(),
  description = character(),
  target_id   = character(),
  s3_key      = character(),
  client_id   = character(),
  metadata    = character()
)

# A chunk or a batch with the schema's columns and types (T72). One chunk
# whose ts came back as character, or a column read back as logical, made
# every dplyr::bind_rows() with it fail: the flush retried and then dropped
# whole batches ("These entries are lost"), and read_audit_log() showed an
# EMPTY Actividad. Extra columns are kept; missing ones are added as NA.
.audit_normalizar <- function(df) {
  esq <- .APP_AUDIT_SCHEMA()
  if (is.null(df) || !is.data.frame(df)) return(esq)
  df <- tibble::as_tibble(df)
  n <- nrow(df)
  for (col in names(esq)) {
    if (identical(col, "ts")) {
      if (!col %in% names(df)) df$ts <- as.POSIXct(rep(NA_real_, n), tz = "UTC")
      else if (!inherits(df$ts, "POSIXct"))
        df$ts <- suppressWarnings(as.POSIXct(if (is.numeric(df$ts)) df$ts else as.character(df$ts), tz = "UTC"))
    } else if (!col %in% names(df)) {
      df[[col]] <- rep(NA_character_, n)
    } else if (!is.character(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  df
}

.MAX_AUDIT_ROWS <- 10000L

.audit_current_key <- function(cid) {
  paste0(tolower(trimws(cid)), "/app_audit.rds")
}

.audit_read_chunk <- function(key) {
  tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = key, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
}

# Read of the live chunk before appending to it (CM09). .audit_read_chunk()
# turns any read error into NULL, and the flush then appended the pending
# rows to an EMPTY table and wrote it over the live chunk -- up to
# .MAX_AUDIT_ROWS entries of the app's activity log gone on one transient S3
# error. This one returns NULL only when S3 says the chunk does not exist
# (a new client); otherwise a failed read stop()s, and the flush puts the rows
# back and retries. If S3 cannot answer, it assumes the chunk exists.
.audit_read_chunk_para_escribir <- function(key) {
  err <- NULL
  x <- tryCatch(
    suppressMessages(suppressWarnings(aws.s3::s3readRDS(object = key, bucket = .s3_bucket()))),
    error = function(e) { err <<- e; NULL })
  if (!is.null(x)) return(x)
  # TRUE / FALSE (only a 404) / NA (a 403, the network...): R/persistence.R.
  # A 403 used to read as "no chunk yet", and the flush started from empty.
  existe <- s3_objeto_existe(key, .s3_bucket())
  if (identical(existe, FALSE)) return(NULL)
  stop("[APP_AUDIT] could not read ", key, " before appending to it",
       if (!is.null(err)) paste0(": ", conditionMessage(err)), call. = FALSE)
}

.audit_write_chunk <- function(df, key) {
  aws.s3::s3saveRDS(df, object = key, bucket = .s3_bucket())
}

# ── Rotation ──────────────────────────────────────────────────────────────────
rotate_log_if_needed <- function(cid, current_df) {
  if (is.null(current_df) || nrow(current_df) < .MAX_AUDIT_ROWS)
    return(invisible(FALSE))

  prefix <- paste0(cid, "/app_audit_")
  existing_keys <- tryCatch({
    objs <- aws.s3::get_bucket(bucket = .s3_bucket(), prefix = prefix, max = 200L)
    vapply(objs, function(o) o[["Key"]], character(1))
  }, error = function(e) character(0))

  existing_nums <- suppressWarnings(as.integer(
    sub(paste0("^", cid, "/app_audit_(\\d+)\\.rds$"), "\\1", existing_keys)
  ))
  existing_nums <- existing_nums[!is.na(existing_nums)]
  next_idx      <- if (length(existing_nums)) max(existing_nums) + 1L else 1L
  archive_key   <- sprintf("%s/app_audit_%03d.rds", cid, next_idx)

  tryCatch({
    .audit_write_chunk(current_df, archive_key)
    .audit_write_chunk(.APP_AUDIT_SCHEMA(), .audit_current_key(cid))
    message(sprintf("[AUDIT] Rotated %d rows to %s", nrow(current_df), archive_key))
    invisible(TRUE)
  }, error = function(e) {
    message("[AUDIT] Rotation failed: ", e$message)
    invisible(FALSE)
  })
}

# ── Read ──────────────────────────────────────────────────────────────────────

# Read ALL audit chunks for a client (current + archived), newest first.
# since: optional POSIXct or character cutoff — filters ts >= since.
# until: optional POSIXct or character cutoff — filters ts <= until.
read_audit_log <- function(client_id = NULL, since = NULL, until = NULL) {
  cid    <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  prefix <- paste0(cid, "/app_audit")

  all_keys <- tryCatch({
    objs <- aws.s3::get_bucket(bucket = .s3_bucket(), prefix = prefix, max = 200L)
    vapply(objs, function(o) o[["Key"]], character(1))
  }, error = function(e) character(0))

  chunks <- if (length(all_keys)) Filter(Negate(is.null), lapply(all_keys, .audit_read_chunk)) else list()

  result <- if (length(chunks)) tryCatch(
    dplyr::bind_rows(lapply(chunks, function(ch) {
      .audit_normalizar(ch)[, names(.APP_AUDIT_SCHEMA()), drop = FALSE]
    })),
    error = function(e) {
      message("[APP_AUDIT] read: chunks could not be joined: ", conditionMessage(e))
      .APP_AUDIT_SCHEMA()
    }
  ) else .APP_AUDIT_SCHEMA()

  # Merge in anything queued but not yet flushed to S3 (see .audit_queue further
  # down) — otherwise an action taken moments ago could briefly appear missing
  # from Actividad/Bitácora while its entry sits in the buffer.
  pending <- tryCatch(.audit_pending[[cid]], error = function(e) NULL)
  if (!is.null(pending) && nrow(pending)) result <- dplyr::bind_rows(result, pending)

  if (!nrow(result)) return(.APP_AUDIT_SCHEMA())

  if (!is.null(since)) {
    since_ts <- tryCatch(as.POSIXct(since), error = function(e) NULL)
    if (!is.null(since_ts))
      result <- result[!is.na(result$ts) & result$ts >= since_ts, , drop = FALSE]
  }

  if (!is.null(until)) {
    until_ts <- tryCatch(as.POSIXct(until), error = function(e) NULL)
    if (!is.null(until_ts))
      result <- result[!is.na(result$ts) & result$ts <= until_ts, , drop = FALSE]
  }

  result[order(result$ts, decreasing = TRUE), , drop = FALSE]
}

# ── Scoped read — the only path the audit log viewer UI may use ─────────────
# Enforces the three visibility rules from ARCHITECTURE.md §6 / Stage 4 Part C.
# Throws loudly (stop()) on any attempt outside those rules — a UI that reaches
# this without the right permission is a bug worth surfacing, not a case to
# silently return empty for.
read_audit_log_scoped <- function(requested_client_id,
                                  viewer_is_staff,
                                  viewer_can_view_client_logs,
                                  viewer_can_view_staff_log,
                                  viewer_home_client_id,
                                  since = NULL,
                                  until = NULL) {
  requested_cid <- tolower(trimws(requested_client_id %||% ""))
  home_cid      <- tolower(trimws(viewer_home_client_id %||% ""))

  if (identical(requested_cid, "hd-admin")) {
    if (!isTRUE(viewer_can_view_staff_log))
      stop("read_audit_log_scoped: viewer lacks can_view_staff_audit_log — cannot view the hd-admin staff log")
  } else if (!isTRUE(viewer_is_staff)) {
    if (!identical(requested_cid, home_cid))
      stop("read_audit_log_scoped: a client session may only request its own client's log")
  } else if (!isTRUE(viewer_can_view_client_logs)) {
    stop("read_audit_log_scoped: viewer lacks can_view_client_audit_logs")
  }

  read_audit_log(client_id = requested_cid, since = since, until = until)
}

# Backward-compatible wrapper used by Actividad tab.
load_app_audit <- function(client_id = NULL) {
  read_audit_log(client_id)
}

# ── Write ─────────────────────────────────────────────────────────────────────
save_app_audit <- function(log_df, client_id = NULL) {
  cid <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  .audit_write_chunk(log_df, .audit_current_key(cid))
}

# ── Deferred write buffer ─────────────────────────────────────────────────────
# log_action() must never block its caller on an S3 round-trip — with 60+ call
# sites across the app (Stage 6 logging-completeness sweep), a synchronous
# read-rotate-reread-write on every single action was the dominant source of
# multi-second latency on routine actions. Entries are held in memory per
# client_id and flushed via one batched read-modify-write on a short delay
# (later::later(), same defer-the-write pattern already used for save_moves()
# in ledger_module.R) — a burst of actions in the same couple of seconds costs
# one S3 round-trip total, not one per action.
#
# Durability trade, made deliberately: if the R process crashes inside the
# ~2s window before a flush fires, whatever hasn't flushed yet is lost. This
# is not meaningfully worse than the previous behavior, where ANY transient S3
# write failure already silently dropped the entry with no retry — the
# difference is failures here DO retry (see .audit_flush_pending) instead of
# vanishing. Deliberately just an in-memory buffer, not a local-disk queue —
# it has no dependency on where this process is hosted (today's shinyapps.io
# containers, or a future self-hosted NAS), so it isn't something that needs
# to be redesigned at migration time.
.audit_pending          <- new.env(parent = emptyenv())
.audit_flush_scheduled  <- new.env(parent = emptyenv())
.audit_retry_count      <- new.env(parent = emptyenv())
.AUDIT_FLUSH_DELAY       <- 2    # seconds — batches a burst of actions together
.AUDIT_FLUSH_RETRY_DELAY <- 10   # seconds — backoff before retrying a failed flush
.AUDIT_MAX_RETRIES       <- 5    # ~50s of retrying before giving up and dropping the batch

# write_cid must be a real, non-empty client slug — env[[key]] assignment
# throws on "" and silently files NA under the literal string key "NA" (never
# reachable by any read path) instead of throwing. Both are caller bugs
# (log_action() invoked with no resolvable client context) that must not
# crash the shared later::later() flush loop or silently vanish forever.
.audit_queue <- function(write_cid, row) {
  if (is.na(write_cid) || !nzchar(write_cid)) {
    message(sprintf(
      "[APP_AUDIT] Dropping audit entry (action=%s) — invalid client_id (%s). This means log_action() was called with no resolvable client context; the caller needs fixing.",
      row$action[1] %||% "?", if (is.na(write_cid)) "NA" else "empty string"
    ))
    return(invisible(FALSE))
  }

  existing <- .audit_pending[[write_cid]]
  .audit_pending[[write_cid]] <- if (is.null(existing)) row else dplyr::bind_rows(existing, row)

  if (!isTRUE(.audit_flush_scheduled[[write_cid]])) {
    .audit_flush_scheduled[[write_cid]] <- TRUE
    later::later(function() .audit_flush_pending(write_cid), delay = .AUDIT_FLUSH_DELAY)
  }
}

# Flushes whatever is currently pending for one client_id in a single
# read-modify-write. Re-reads after rotation ONLY if rotation actually
# happened (the old code re-read unconditionally on every call — pure waste
# on the common, non-rotating path).
.audit_flush_pending <- function(write_cid) {
  pending <- .audit_pending[[write_cid]]
  .audit_pending[[write_cid]]         <- NULL
  .audit_flush_scheduled[[write_cid]] <- FALSE
  if (is.null(pending) || !nrow(pending)) return(invisible(TRUE))

  ok <- tryCatch({
    # Strict: a failed read stop()s here and the rows go back to the queue
    # (below), instead of being appended to an empty table over the live log.
    current <- .audit_normalizar(.audit_read_chunk_para_escribir(.audit_current_key(write_cid)))
    rotated <- isTRUE(rotate_log_if_needed(write_cid, current))
    if (rotated) {
      ch <- .audit_read_chunk_para_escribir(.audit_current_key(write_cid))
      current <- .audit_normalizar(ch)
    }
    .audit_write_chunk(dplyr::bind_rows(current, .audit_normalizar(pending)), .audit_current_key(write_cid))
    TRUE
  }, error = function(e) {
    message("[APP_AUDIT] flush failed (", write_cid, "), will retry: ", e$message)
    FALSE
  })

  if (ok) {
    .audit_retry_count[[write_cid]] <- NULL
  } else {
    # Bounded retry: an extended S3 outage must not grow this buffer (or the
    # crash-loss exposure window) without limit. After .AUDIT_MAX_RETRIES
    # failed attempts, give up loudly and drop the batch rather than retrying
    # forever — a silent indefinite retry loop is worse than a visible,
    # bounded loss with a clear log line pointing at S3 connectivity.
    attempt <- (.audit_retry_count[[write_cid]] %||% 0L) + 1L
    if (attempt > .AUDIT_MAX_RETRIES) {
      .audit_retry_count[[write_cid]]     <- NULL
      .audit_flush_scheduled[[write_cid]] <- FALSE
      .audit_rescatar(write_cid, pending)
      return(invisible(FALSE))
    }
    .audit_retry_count[[write_cid]] <- attempt

    # Put the rows back (prepended, in case new ones arrived meanwhile) and
    # retry after a longer backoff instead of losing them.
    still_pending <- .audit_pending[[write_cid]]
    .audit_pending[[write_cid]] <- if (is.null(still_pending)) pending
                                    else dplyr::bind_rows(pending, still_pending)
    .audit_flush_scheduled[[write_cid]] <- TRUE
    later::later(function() .audit_flush_pending(write_cid), delay = .AUDIT_FLUSH_RETRY_DELAY)
  }
  invisible(ok)
}

# T74 (sleep proposal, common part): flush every client's pending rows NOW,
# not on the 2 s timer -- called when a session ends, so the day's last
# entries (the logout itself) never depend on a later:: callback in a
# process the host may put to sleep 5-8 min after the last visit (measured
# on Connect Cloud, 2026-11-21). A client already in retry backoff is left
# to its own chain (flushing it here too would burn its retries twice as
# fast). A failure falls into the usual path: retries, then the T72 rescue.
# Never throws. Returns the client ids it flushed.
.audit_flush_ahora <- function() {
  hechos <- character(0)
  for (cid in ls(.audit_pending)) {
    pend <- .audit_pending[[cid]]
    if (is.null(pend) || !NROW(pend)) next
    if (!is.null(.audit_retry_count[[cid]])) next
    tryCatch({ .audit_flush_pending(cid); hechos <- c(hechos, cid) },
             error = function(e) message("[APP_AUDIT] flush on session end failed (", cid, "): ", conditionMessage(e)))
  }
  invisible(hechos)
}

# When a batch cannot be appended after the retries (T72): it is written to
# a key of its own, <cid>/app_audit_pendiente_<utc>_<rand>.rds -- a plain
# write, nothing read first -- which read_audit_log() already picks up (same
# "app_audit" prefix; rotation only counts the numbered chunks), so the
# entries still show in Actividad. Staff hear it in hd-admin's Panorama
# either way, once an hour per client (the t31 pattern); if even the rescue
# write fails, the aviso says the entries are lost. Never throws.
.audit_rescatar <- function(write_cid, pending) {
  n   <- nrow(pending)
  key <- sprintf("%s/app_audit_pendiente_%s_%s.rds", write_cid,
                 format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), substr(uuid::UUIDgenerate(), 1, 8))
  guardado <- tryCatch({ .audit_write_chunk(.audit_normalizar(pending), key); TRUE },
                       error = function(e) FALSE)
  message(sprintf("[APP_AUDIT] Gave up appending %d entr%s for %s after %d attempts; %s",
                  n, if (n == 1) "y" else "ies", write_cid, .AUDIT_MAX_RETRIES,
                  if (guardado) paste("rescued to", key) else "the rescue write failed too: these entries are lost"))
  tryCatch(avisos_agregar("hd-admin", list(
    tipo = "automatico", severidad = "atencion", niveles = niveles_staff(),
    titulo = tr("comun.actividad.alerta.titulo"),
    cuerpo = tr(if (guardado) "comun.actividad.alerta.rescatadas" else "comun.actividad.alerta.perdidas",
                n = as.character(n), cliente = write_cid, archivo = basename(key)),
    clave = paste("t72", "log", write_cid, sep = ":"),
    ventana_min = 60)),
    error = function(e) message("[APP_AUDIT] alert failed (", write_cid, "): ", conditionMessage(e)))
  invisible(guardado)
}

# ── Session-duration formatting ──────────────────────────────────────────────
# Shared by app.R's login/logout logging (Stage 1 of the Actividad audit
# overhaul, 2026-08-26) and the Actividad detail popup/PDF export (Stage 2/3)
# that render a logout entry's active-duration. Pulled out to a plain,
# app.R-independent function so it's unit-testable without booting the whole
# Shiny app, and so the popup/PDF can format the same duration_seconds value
# identically to how it was described at logout time.
format_session_duration <- function(secs) {
  # %||% only substitutes on NULL, not NA (same NA-safety gap documented in
  # log_action() above) -- an explicit NA (an unknown/unrecorded login time,
  # the common case for a logout with no matching login in this session) must
  # be normalized here too, or `if (h > 0)` below throws "missing value
  # where TRUE/FALSE needed" instead of the intended "0s" fallback.
  secs <- suppressWarnings(as.numeric(secs %||% 0))
  if (is.null(secs) || !length(secs) || is.na(secs)) secs <- 0
  secs <- max(0, secs)
  h <- secs %/% 3600L; m <- (secs %% 3600) %/% 60L; s <- round(secs %% 60)
  if (h > 0) sprintf("%dh %dmin", h, m)
  else if (m > 0) sprintf("%dmin %ds", m, s)
  else sprintf("%ds", s)
}

# ── log_action() ─────────────────────────────────────────────────────────────
# Fire-and-forget — never throws. Returns the new row invisibly. The S3 write
# itself is deferred (see above) — this function never blocks on network I/O.
#
# client_id             — target folder this entry is filed under, i.e. where
#                         the action actually happened (default: CLIENT_ID env
#                         var — legacy fallback, see below).
# viewer_home_client_id — the ACTING USER's own true home client (their login-
#                         time client_id; always "hd-admin" for staff). Default
#                         falls back to Sys.getenv("CLIENT_ID") for any call
#                         site that hasn't been updated to pass it explicitly.
# s3_key                — the S3 object that was created/modified by this action
#
# Dual-write: fires when client_id differs from the ACTOR's own home, i.e. a
# staff member acting inside a jumped client — never for a native client
# user's own actions, and never just because this is a single shared
# deployment. This app runs as ONE shared deployment (CLIENT_ID env var is
# always "hd-admin", ARCHITECTURE.md §3) — comparing client_id against that
# fixed env var instead of the actual actor's home meant EVERY native client
# user's own action (not just staff jump actions) was dual-written into
# hd-admin's own log, exactly the "CLIENT_ID as an is-this-session-staff
# stand-in" anti-pattern §3 warns against elsewhere. Fixed 2026-07-16 while
# testing Stage 4 (found via a client dev user's invoice-move entries leaking
# into Bitácora Global / Hopdesk's own log).
# ── T87: the wall between HopDesk staff and a client, in what is WRITTEN ─────
# A client's log lives in that client's S3 folder: whatever a row holds there
# is the client's, shown or not. When staff act inside a client, the client's
# copy of a row is written by class (Luis, 2026-11-24, T87_paso2_catalogo.md);
# hd-admin always keeps the whole row (real user, full action and metadata,
# plus actor_code):
#   S  the staff's own action          -> nothing in the client's folder
#   N  the subscription (seats)        -> a "hopdesk" row in neutral words
#   A  the staff's entry/exit          -> a signed row in neutral words
#   C  anything a client user can do   -> the row, signed "hd-staff:<code>"
#                                         with the first name, the user name
#                                         nowhere in it
# Every action is C unless listed here. tests/test_t87_catalogo.R fails on an
# action written anywhere that is not classified (S/N/A here, or its frozen
# list of known C ones). "<accion>/parte" and "sin_evento" are the write
# layer's own traces: S. "<accion>_fallido" takes its action's class.
ACCIONES_STAFF <- list(
  client_access           = list(clase = "S"),
  ver_como                = list(clase = "S"),
  ctx_load_fallido        = list(clase = "S"),
  hop_access_requested    = list(clase = "S"),
  hop_access_approved     = list(clase = "S"),
  hop_access_denied       = list(clase = "S"),
  hop_access_revoked      = list(clase = "S"),
  hop_access_direct_grant = list(clase = "S"),
  agregar_contacto        = list(clase = "S"),
  desactivar_contacto     = list(clase = "S"),
  solicitud_nuevo_cliente = list(clase = "S"),
  aprobacion_cliente      = list(clase = "S"),
  rechazo_cliente         = list(clase = "S"),
  emergency_lock          = list(clase = "S"),
  emergency_unlock        = list(clase = "S"),
  reintentar_cola_email   = list(clase = "S"),
  reintentar_email        = list(clase = "S"),
  consulta_bitacora       = list(clase = "S"),
  sin_evento              = list(clase = "S"),
  cambio_limite_usuarios  = list(clase = "N", texto = "tablas.actividad.neutral.cambio_limite",
                                 params = function(m) list(antes = m$old_max_users %||% m$old_limit %||% "?",
                                                           despues = m$new_max_users %||% m$new_limit %||% "?"),
                                 deja = c("old_max_users", "new_max_users")),
  user_deactivated_by_limit_reduction = list(clase = "N", texto = "tablas.actividad.neutral.cuenta_desactivada",
                                 params = function(m) list(nombre = m$nombre %||% m$cuenta %||% "?"),
                                 deja = c("nombre")),
  acceso_staff_entrada    = list(clase = "A", texto = "tablas.actividad.acceso.entrada"),
  acceso_staff_salida     = list(clase = "A", texto = "tablas.actividad.acceso.salida"))

# A staff jump, entry and exit (T87): `anterior` is the client being LEFT
# (the jump's previous value), `nuevo` the one entered (NULL/"" = home). The
# exit is logged for the client left, with its reason ("volver", "cambio",
# "cierre", "revocado", "vencido"); the entry for the client entered. Each
# goes whole to hd-admin and as a neutral signed row to that client (class A).
# Never throws.
t87_registrar_salto <- function(anterior, nuevo, user, home, motivo = NULL) {
  limpio <- function(x) { x <- tolower(trimws(as.character(x %||% "")[1])); if (is.na(x)) "" else x }
  a <- limpio(anterior); n <- limpio(nuevo)
  if (identical(a, n)) return(invisible(FALSE))
  tryCatch({
    if (nzchar(a)) {
      m <- motivo %||% (if (nzchar(n)) "cambio" else "volver")
      log_action(user = user, module = "clientes", action = "acceso_staff_salida",
                 description = sprintf("Staff left '%s' (%s)", a, m), target_id = a,
                 metadata = list(motivo = m, hacia = if (nzchar(n)) n else NA_character_),
                 client_id = a, viewer_home_client_id = home)
    }
    if (nzchar(n))
      log_action(user = user, module = "clientes", action = "acceso_staff_entrada",
                 description = sprintf("Staff entered '%s'", n), target_id = n,
                 metadata = list(desde = if (nzchar(a)) a else NA_character_),
                 client_id = n, viewer_home_client_id = home)
  }, error = function(e) message("[T87] jump not logged: ", conditionMessage(e)))
  invisible(TRUE)
}

# The class of an action: S, N, A or C (unlisted).
t87_clase_accion <- function(action) {
  a <- as.character(action %||% "")[1]
  if (is.na(a)) return("C")
  if (grepl("/parte$", a)) return("S")
  base <- sub("_fallido$", "", a)
  ACCIONES_STAFF[[a]]$clase %||% ACCIONES_STAFF[[base]]$clase %||% "C"
}

# Staff accounts live only in hd-admin (auth.R splits them by tier with
# niveles_staff()): a row is "staff inside a client" when the actor's home is
# hd-admin and the row goes to another client. The home is what the caller
# passed, or this session's own home (app.R: session$userData$hd_cliente_casa)
# when a write by event came without one -- so no staff write passes as a
# client user's, and every one reaches hd-admin.
.T87_CASA_STAFF <- "hd-admin"
.t87_casa_sesion <- function() {
  s <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (is.null(s)) return(NULL)
  f <- tryCatch(s$userData$hd_cliente_casa, error = function(e) NULL)
  if (!is.function(f)) return(NULL)
  h <- tryCatch(tolower(trimws(shiny::isolate(f()))), error = function(e) NULL)
  if (length(h) == 1L && !is.na(h) && nzchar(h)) h else NULL
}

# The staff directory: user -> account_code and display name, from
# hd-admin/usuarios.rds, read at most every 2 minutes per process (an account
# created or renamed shows within that). A read that fails gives nothing: the
# client's row is then plain "hd-staff", never the user name.
.T87_DIRECTORIO <- new.env(parent = emptyenv())
.T87_DIRECTORIO_TTL <- 120
t87_directorio_invalidar <- function() rm(list = ls(.T87_DIRECTORIO), envir = .T87_DIRECTORIO)
.t87_directorio <- function() {
  if (!is.null(.T87_DIRECTORIO$df) && !is.null(.T87_DIRECTORIO$leido) &&
      as.numeric(difftime(Sys.time(), .T87_DIRECTORIO$leido, units = "secs")) < .T87_DIRECTORIO_TTL)
    return(.T87_DIRECTORIO$df)
  df <- tryCatch(suppressMessages(auth_load_usuarios(client_id = .T87_CASA_STAFF)), error = function(e) NULL)
  if (!is.data.frame(df)) return(NULL)
  .T87_DIRECTORIO$df <- df; .T87_DIRECTORIO$leido <- Sys.time()
  df
}
# list(code, primer, nombre) of a staff user; NAs when unknown.
t87_staff_firma <- function(user) {
  vacio <- list(code = NA_character_, primer = NA_character_, nombre = NA_character_)
  u <- tolower(trimws(as.character(user %||% "")[1]))
  if (is.na(u) || !nzchar(u)) return(vacio)
  df <- .t87_directorio()
  if (is.null(df) || !nrow(df) || !"username" %in% names(df)) return(vacio)
  i <- which(tolower(trimws(as.character(df$username))) == u)[1]
  if (is.na(i)) return(vacio)
  nombre <- trimws(as.character(df$display_name[i] %||% NA_character_))
  primer <- if (is.na(nombre) || !nzchar(nombre)) NA_character_ else strsplit(nombre, "[[:space:]]+")[[1]][1]
  code <- as.character(df$account_code[i] %||% NA_character_)
  list(code = if (is.na(code) || !nzchar(code)) NA_character_ else code, primer = primer,
       nombre = if (is.na(nombre) || !nzchar(nombre)) NA_character_ else nombre)
}

# The user field of a staff row in a client's folder: "hd-staff:<code>", or
# "hd-staff" when the account is not known. Read back by the client's view.
t87_usuario_staff <- function(firma) if (is.na(firma$code)) "hd-staff" else paste0("hd-staff:", firma$code)

# Every text of the client's row with the staff's user name and full name
# put out of it (fixed text, never a pattern).
.t87_limpiar <- function(x, user, firma) {
  if (is.null(x) || !length(x)) return(x)
  x <- as.character(x)
  reemplazo <- if (is.na(firma$primer)) "HD Staff" else firma$primer
  for (q in unique(stats::na.omit(c(firma$nombre, user))))
    if (nzchar(q)) x <- gsub(q, reemplazo, x, fixed = TRUE)
  x
}

# The client's copy of a staff row of class N, A or C (S never gets here).
.t87_fila_cliente <- function(fila, clase, user, metadata, firma) {
  def <- ACCIONES_STAFF[[fila$action]] %||% ACCIONES_STAFF[[sub("_fallido$", "", fila$action)]]
  m <- as.list(metadata %||% list())
  m$home_client <- NULL
  if (identical(clase, "N")) {
    params <- tryCatch(def$params(m), error = function(e) list())
    fila$user        <- "hopdesk"
    fila$description <- tryCatch(do.call(tr, c(list(def$texto), params, list(idioma = "es"))), error = function(e) "")
    m <- c(m[intersect(names(m), def$deja %||% character())], list(texto_clave = def$texto))
  } else {
    fila$user <- t87_usuario_staff(firma)
    if (identical(clase, "A"))
      fila$description <- tryCatch(tr(def$texto, idioma = "es"), error = function(e) "")
    m$actor_primer <- if (is.na(firma$primer)) NULL else firma$primer
  }
  fila$description <- .t87_limpiar(fila$description, user, firma)
  fila$target_id   <- .t87_limpiar(fila$target_id, user, firma)
  json <- tryCatch(as.character(jsonlite::toJSON(m, auto_unbox = TRUE, na = "null")), error = function(e) "{}")
  fila$metadata    <- .t87_limpiar(json, user, firma)
  fila
}

log_action <- function(user,
                       module,
                       action,
                       description,
                       target_id             = NA_character_,
                       s3_key                = NA_character_,
                       metadata              = NULL,
                       client_id             = NULL,
                       viewer_home_client_id = NULL) {
  cid      <- tolower(trimws(client_id %||% Sys.getenv("CLIENT_ID")))
  home_cid <- tolower(trimws(viewer_home_client_id %||% Sys.getenv("CLIENT_ID")))
  # %||% only substitutes on NULL, not NA — an explicit client_id=NA_character_
  # (or a viewer_home_client_id resolving to NA via some upstream tryCatch)
  # survives to here unchanged. Normalize to "" so every downstream comparison
  # (including .audit_queue()'s guard below) has one single, non-NA definition
  # of "no resolvable client" instead of every call site needing its own
  # NA-safety check. Without this, `if (nzchar(home_cid) && home_cid != cid)`
  # evaluates to `if (NA)` and throws — log_action() must never throw.
  if (is.na(cid))      cid      <- ""
  if (is.na(home_cid)) home_cid <- ""
  # T87: a write by event can come without the actor's home; the session
  # knows it (staff's home is hd-admin).
  casa_sesion <- .t87_casa_sesion()
  if (identical(casa_sesion, .T87_CASA_STAFF) && nzchar(cid) && cid != .T87_CASA_STAFF)
    home_cid <- .T87_CASA_STAFF
  staff_dentro <- identical(home_cid, .T87_CASA_STAFF) && nzchar(cid) && cid != home_cid
  firma <- if (staff_dentro) tryCatch(t87_staff_firma(user), error = function(e)
    list(code = NA_character_, primer = NA_character_, nombre = NA_character_)) else NULL

  # When staff acts in a client context, stamp home_client in metadata so the
  # audit record is clearly marked as an outsider write, not a client-user write.
  metadata_propia <- metadata
  if (nzchar(home_cid) && home_cid != cid) {
    metadata <- c(as.list(metadata %||% list()), list(home_client = home_cid))
    # T87: hd-admin's copy names the account behind the row (Principal reads
    # code <-> full name there).
    if (staff_dentro && !is.na(firma$code)) metadata$actor_code <- firma$code
  }

  new_row <- tibble::tibble(
    id          = uuid::UUIDgenerate(),
    ts          = Sys.time(),
    user        = as.character(user        %||% "unknown"),
    module      = as.character(module),
    action      = as.character(action),
    description = as.character(description),
    target_id   = as.character(target_id   %||% NA_character_),
    s3_key      = as.character(s3_key      %||% NA_character_),
    client_id   = cid,
    # na = "null": jsonlite's own default (no na= argument) inconsistently
    # serializes a numeric NA (e.g. duration_seconds = NA_real_ for a logout
    # with no known login time) as the literal STRING "NA" instead of JSON
    # null -- confirmed empirically while building Stage 1 of the Actividad
    # audit overhaul (2026-08-26), where a bare NA_real_ round-tripped back
    # as the misleading string "NA" rather than a real, checkable NA/NULL.
    # Character NA already happened to serialize as null by default; this
    # makes every NA type in metadata behave the same, consistent way.
    metadata    = as.character(if (!is.null(metadata))
                    tryCatch(jsonlite::toJSON(metadata, auto_unbox = TRUE, na = "null"),
                             error = function(e) "{}")
                  else "{}")
  )

  # T87: staff inside a client -- hd-admin keeps the whole row; the client's
  # folder gets its row by class (nothing for S).
  if (staff_dentro) {
    .audit_queue(home_cid, new_row)
    clase <- t87_clase_accion(action)
    if (!identical(clase, "S")) {
      fila_cliente <- tryCatch(.t87_fila_cliente(new_row, clase, as.character(user %||% ""), metadata_propia, firma),
                               error = function(e) {
                                 message("[T87] client row not written (", action, "): ", conditionMessage(e))
                                 NULL })
      if (!is.null(fila_cliente)) .audit_queue(cid, fila_cliente)
    }
    return(invisible(new_row))
  }

  .audit_queue(cid, new_row)

  # Dual-write when a staff user acts in a different client's context
  if (nzchar(home_cid) && home_cid != cid)
    .audit_queue(home_cid, new_row)

  invisible(new_row)
}
