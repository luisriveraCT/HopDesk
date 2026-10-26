# =============================================================================
# R/email_service.R
# Thin wrapper around the Resend API for transactional email.
#
# Never throws — all failures are queued to hd-admin/email_queue.rds for
# manual retry via the "Cola de emails" panel in tiers_module.R.
#
# Environment variables:
#   RESEND_API_KEY    — a real key starts with "re_". The literal "sandbox"
#                       skips delivery and reports success (dev mode). An
#                       EMPTY or malformed value is NOT sandbox: it is a
#                       misconfiguration, and send_email() now reports
#                       failure so the caller can say so out loud instead of
#                       silently dropping real invitations on the floor.
#   RESEND_FROM_EMAIL — sender address on a domain verified in Resend,
#                       e.g. noreply@send.hopdesk.mx
#
# See docs/EMAIL_SERVICE_RUNBOOK.md for the full enable/pay/verify procedure.
# =============================================================================

.RESEND_ENDPOINT <- "https://api.resend.com/emails"
.EMAIL_QUEUE_KEY <- "hd-admin/email_queue.rds"

# ── Configuration status ──────────────────────────────────────────────────────
# Single source of truth for "is email actually live?". Used by send_email()
# and by the status badge in the Invitaciones tab so an operator can see the
# mode BEFORE sending an invitation rather than discovering it afterwards.
email_service_status <- function() {
  api_key   <- trimws(Sys.getenv("RESEND_API_KEY"))
  from_addr <- trimws(Sys.getenv("RESEND_FROM_EMAIL"))
  if (!nzchar(from_addr)) from_addr <- "noreply@hopdesk.com"

  mode <- if (!nzchar(api_key)) {
    "unconfigured"
  } else if (tolower(api_key) == "sandbox") {
    "sandbox"
  } else if (!grepl("^re_", api_key)) {
    "invalid"
  } else {
    "live"
  }

  list(
    mode    = mode,
    live    = identical(mode, "live"),
    from    = from_addr,
    label   = switch(mode,
      live         = "Envío de correo activo",
      sandbox      = "Modo sandbox — no se envía correo real",
      unconfigured = "Correo sin configurar",
      invalid      = "Clave de API inválida"
    ),
    detail  = switch(mode,
      live         = paste0("Remitente: ", from_addr),
      sandbox      = "RESEND_API_KEY=sandbox. Las invitaciones se registran en el log pero nadie las recibe.",
      unconfigured = "Falta RESEND_API_KEY en .Renviron. Las invitaciones se guardarán en la cola sin enviarse.",
      invalid      = "RESEND_API_KEY no empieza con 're_'. Revisa que sea la clave de Resend y no otro valor."
    )
  )
}

# ── Schema ───────────────────────────────────────────────────────────────────
.schema_email_queue <- function() {
  data.frame(
    id          = character(),
    to          = character(),
    subject     = character(),
    html        = character(),
    queued_at   = character(),
    last_tried  = character(),
    attempts    = integer(),
    last_error  = character(),
    status      = character(),   # "pending", "sent", "failed"
    stringsAsFactors = FALSE
  )
}

# ── Queue helpers ─────────────────────────────────────────────────────────────
read_email_queue <- function() {
  q <- tryCatch(
    suppressMessages(suppressWarnings(
      aws.s3::s3readRDS(object = .EMAIL_QUEUE_KEY, bucket = .s3_bucket())
    )),
    error = function(e) NULL
  )
  if (is.null(q) || !is.data.frame(q) || !nrow(q)) return(.schema_email_queue())
  for (col in names(.schema_email_queue()))
    if (!col %in% names(q)) q[[col]] <- .schema_email_queue()[[col]]
  q
}

write_email_queue <- function(q) {
  tryCatch(
    aws.s3::s3saveRDS(q, object = .EMAIL_QUEUE_KEY, bucket = .s3_bucket()),
    error = function(e) message("[EMAIL] Could not persist queue: ", e$message)
  )
  invisible(TRUE)
}

.enqueue_email <- function(to, subject, html, error = NA_character_) {
  q <- read_email_queue()
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  new_row <- data.frame(
    id         = paste0("eq_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                        substr(digest::digest(paste(to, subject, now)), 1, 6)),
    to         = to,
    subject    = subject,
    html       = html,
    queued_at  = now,
    last_tried = NA_character_,
    attempts   = 0L,
    last_error = error %||% NA_character_,
    status     = "pending",
    stringsAsFactors = FALSE
  )
  write_email_queue(rbind(q, new_row))
  message("[EMAIL] Queued (", to, "): ", subject)
}

# ── Core send ────────────────────────────────────────────────────────────────
# Returns TRUE on success, or FALSE carrying an "error" attribute on failure.
# Never throws. On failure, queues for retry.
#
# Callers that report to a human MUST branch on the return value. Before
# 2026-09-30 an empty RESEND_API_KEY was silently treated as sandbox, so the
# Invitaciones tab said "Invitación enviada" for a mail that was never sent
# and never queued. Only the literal "sandbox" claims success now.
send_email <- function(to, subject, html, queue_on_fail = TRUE) {
  status    <- email_service_status()
  api_key   <- trimws(Sys.getenv("RESEND_API_KEY"))
  from_addr <- status$from

  to <- tolower(trimws(to %||% ""))
  if (!grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", to)) {
    message("[EMAIL] Refused: invalid recipient '", to, "'")
    return(structure(FALSE, error = paste0("destinatario inválido: '", to, "'")))
  }

  # Sandbox mode: log and return success without hitting the API
  if (identical(status$mode, "sandbox")) {
    message("[EMAIL][SANDBOX] To: ", to, " | Subject: ", subject)
    return(TRUE)
  }

  # Not configured / malformed key: do NOT pretend. Queue so nothing is lost
  # and the operator can retry from the "Cola de emails" panel once the key
  # is in place.
  if (!identical(status$mode, "live")) {
    message("[EMAIL] Not sent (", status$mode, "): ", status$detail)
    if (queue_on_fail) .enqueue_email(to, subject, html, error = status$detail)
    return(structure(FALSE, error = status$detail))
  }

  body <- jsonlite::toJSON(list(
    from    = jsonlite::unbox(from_addr),
    to      = list(to),
    subject = jsonlite::unbox(subject),
    html    = jsonlite::unbox(html)
  ), auto_unbox = FALSE)

  result <- tryCatch({
    resp <- httr::POST(
      url    = .RESEND_ENDPOINT,
      httr::add_headers(
        Authorization  = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body   = body,
      encode = "raw"
    )
    # NOT named `status` — that would clobber the email_service_status() list
    # in this frame, since tryCatch evaluates its expr in the caller's env.
    http_status <- httr::status_code(resp)
    if (http_status %in% c(200L, 201L)) {
      message("[EMAIL] Sent to ", to, ": ", subject)
      TRUE
    } else {
      msg <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                      error = function(e) as.character(http_status))
      message("[EMAIL] API error (", http_status, "): ", msg)
      list(ok = FALSE, error = paste0("HTTP ", http_status, ": ", msg))
    }
  }, error = function(e) {
    message("[EMAIL] Network error: ", e$message)
    list(ok = FALSE, error = e$message)
  })

  if (isTRUE(result)) return(TRUE)

  err_msg <- if (is.list(result)) result$error else "error desconocido"
  if (queue_on_fail) .enqueue_email(to, subject, html, error = err_msg)
  structure(FALSE, error = err_msg)
}

# Human-readable reason for the last failure of a send_email() call.
email_error_of <- function(x) {
  e <- attr(x, "error", exact = TRUE)
  if (is.null(e) || !nzchar(e %||% "")) "error desconocido" else e
}

# ── Retry a queued email by id ────────────────────────────────────────────────
retry_queued_email <- function(queue_id) {
  q <- read_email_queue()
  idx <- which(q$id == queue_id)
  if (!length(idx)) return(FALSE)

  row <- q[idx, ]
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  q[idx, "last_tried"] <- now
  q[idx, "attempts"]   <- (q[idx, "attempts"] %||% 0L) + 1L

  ok <- send_email(row$to, row$subject, row$html, queue_on_fail = FALSE)

  q[idx, "status"]     <- if (isTRUE(ok)) "sent" else "failed"
  q[idx, "last_error"] <- if (isTRUE(ok)) NA_character_ else email_error_of(ok)
  write_email_queue(q)
  isTRUE(ok)
}

# ── Retry every pending/failed email in the queue ─────────────────────────────
# Used by the "Reintentar todo" button after the API key is first installed:
# everything that piled up while email was unconfigured goes out in one go.
# Returns list(sent = <int>, failed = <int>).
retry_all_queued_emails <- function(max_attempts = 5L) {
  q <- read_email_queue()
  idx <- which(!is.na(q$status) & q$status %in% c("pending", "failed") &
               (is.na(q$attempts) | q$attempts < max_attempts))
  if (!length(idx)) return(list(sent = 0L, failed = 0L))

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  sent <- 0L; failed <- 0L
  for (i in idx) {
    res <- send_email(q$to[i], q$subject[i], q$html[i], queue_on_fail = FALSE)
    q[i, "last_tried"] <- now
    q[i, "attempts"]   <- (q$attempts[i] %||% 0L) + 1L
    if (isTRUE(res)) {
      q[i, "status"] <- "sent"; q[i, "last_error"] <- NA_character_; sent <- sent + 1L
    } else {
      q[i, "status"] <- "failed"; q[i, "last_error"] <- email_error_of(res); failed <- failed + 1L
    }
  }
  write_email_queue(q)
  list(sent = sent, failed = failed)
}

# ── Template helpers ──────────────────────────────────────────────────────────
email_invite_html <- function(display_name, client_name, invite_url, expires_hours = 48) {
  sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:#c2185b">Bienvenido a HopDesk</h2>
  <p>Hola <strong>%s</strong>,</p>
  <p>Has sido invitado a unirte al equipo de <strong>%s</strong> en HopDesk.</p>
  <p style="margin:24px 0">
    <a href="%s"
       style="background:#c2185b;color:#fff;padding:12px 24px;
              border-radius:6px;text-decoration:none;font-weight:bold">
      Activar mi cuenta
    </a>
  </p>
  <p style="color:#666;font-size:13px">
    Este enlace expira en %d horas. Si no esperabas esta invitación, ignora este mensaje.
  </p>
</div>
', display_name, client_name, invite_url, expires_hours)
}

# Sent once the invitee sets their password. The username is DERIVED from the
# display name at activation time (app.R), so the invitee has no way to know
# it in advance — without this mail it only ever appeared in a transient
# showNotification that vanishes after 6 seconds.
email_welcome_html <- function(display_name, username, client_name, app_url) {
  sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:#c2185b">Tu cuenta de HopDesk está activa</h2>
  <p>Hola <strong>%s</strong>,</p>
  <p>Tu cuenta en <strong>%s</strong> quedó activada. Estos son tus datos de acceso:</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Usuario</td>
      <td style="padding:8px;border:1px solid #e5e7eb;font-weight:bold">%s</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Contrase&ntilde;a</td>
      <td style="padding:8px;border:1px solid #e5e7eb">La que acabas de crear</td>
    </tr>
  </table>
  <p style="margin:24px 0">
    <a href="%s"
       style="background:#c2185b;color:#fff;padding:12px 24px;
              border-radius:6px;text-decoration:none;font-weight:bold">
      Entrar a HopDesk
    </a>
  </p>
  <p style="color:#666;font-size:13px">
    Guarda este correo: tu nombre de usuario no es tu direcci&oacute;n de correo.
  </p>
</div>
', display_name, client_name, username, app_url)
}

email_password_reset_html <- function(display_name, reset_url, expires_hours = 2) {
  sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:#c2185b">Restablecer contraseña — HopDesk</h2>
  <p>Hola <strong>%s</strong>,</p>
  <p>Recibimos una solicitud para restablecer tu contraseña.</p>
  <p style="margin:24px 0">
    <a href="%s"
       style="background:#c2185b;color:#fff;padding:12px 24px;
              border-radius:6px;text-decoration:none;font-weight:bold">
      Restablecer contraseña
    </a>
  </p>
  <p style="color:#666;font-size:13px">
    Este enlace expira en %d horas. Si no solicitaste esto, ignora este mensaje.
  </p>
</div>
', display_name, reset_url, expires_hours)
}

email_user_limit_warning_html <- function(client_name, current_users, max_users) {
  remaining <- max_users - current_users
  sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:#f59e0b">&#9888; Aviso de capacidad — HopDesk</h2>
  <p>El cliente <strong>%s</strong> está acercándose a su límite de usuarios.</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Usuarios activos</td>
      <td style="padding:8px;border:1px solid #e5e7eb;font-weight:bold">%d</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Límite</td>
      <td style="padding:8px;border:1px solid #e5e7eb;font-weight:bold">%d</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Lugares disponibles</td>
      <td style="padding:8px;border:1px solid #e5e7eb;font-weight:bold;color:#f59e0b">%d</td>
    </tr>
  </table>
  <p style="color:#666;font-size:13px">
    Si necesitas ampliar el límite, contacta al administrador de HopDesk.
  </p>
</div>
', client_name, current_users, max_users, remaining)
}

email_user_limit_reached_html <- function(client_name, current_users, max_users) {
  sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:#dc2626">&#128274; Límite de usuarios alcanzado — HopDesk</h2>
  <p>El cliente <strong>%s</strong> ha alcanzado su límite máximo de usuarios.</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Usuarios activos</td>
      <td style="padding:8px;border:1px solid #e5e7eb;font-weight:bold">%d / %d</td>
    </tr>
  </table>
  <p>No se podrán crear nuevas cuentas hasta que se amplíe el límite o se desactiven usuarios existentes.</p>
  <p style="color:#666;font-size:13px">
    El cliente verá un botón de "Solicitar usuarios" para notificar al equipo HopDesk.
  </p>
</div>
', client_name, current_users, max_users)
}

email_user_request_html <- function(client_name, current_users, max_users, contact_email) {
  sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:#0a58ca">&#128101; Solicitud de más usuarios — HopDesk</h2>
  <p>El cliente <strong>%s</strong> ha solicitado más capacidad de usuarios.</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Usuarios activos</td>
      <td style="padding:8px;border:1px solid #e5e7eb;font-weight:bold">%d / %d</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Contacto del cliente</td>
      <td style="padding:8px;border:1px solid #e5e7eb">
        <a href="mailto:%s">%s</a>
      </td>
    </tr>
  </table>
  <p style="color:#666;font-size:13px">
    Para atender esta solicitud, aumenta el límite de usuarios en el panel de Clientes de HopDesk.
  </p>
</div>
', client_name, current_users, max_users, contact_email, contact_email)
}

# ── notify_user_limit() ───────────────────────────────────────────────────────
# Called after every user create/delete. Checks if the new count enters a
# warning zone and, if so, fires an in-app notification + emails.
# Fire-and-forget — never throws.

notify_user_limit <- function(client_id) {
  tryCatch({
    registry <- read_client_registry()
    row      <- registry[registry$client_id == client_id, , drop = FALSE]
    if (!nrow(row)) return(invisible(FALSE))

    cur  <- as.integer(row$current_users[1])
    max  <- as.integer(row$max_users[1])
    rem  <- max - cur
    dname <- row$display_name[1]

    if (rem > 2) return(invisible(FALSE))  # no alert needed

    type <- if (rem <= 0) "user_limit_reached" else "user_limit_warning"
    msg  <- if (rem <= 0)
      sprintf("'%s' ha alcanzado su límite (%d/%d usuarios).", dname, cur, max)
    else
      sprintf("'%s' tiene %d lugar(es) restante(s) (%d/%d).", dname, rem, cur, max)

    append_hd_notification(type = type, client_id = client_id, message_text = msg,
                           metadata = list(current_users = cur, max_users = max))

    recipients <- tryCatch(get_limit_alert_recipients(client_id), error = function(e) character(0))
    if (!length(recipients)) return(invisible(TRUE))

    html_fn <- if (rem <= 0) email_user_limit_reached_html else email_user_limit_warning_html
    html    <- if (rem <= 0)
      email_user_limit_reached_html(dname, cur, max)
    else
      email_user_limit_warning_html(dname, cur, max)

    subject <- if (rem <= 0)
      sprintf("[HopDesk] Límite alcanzado — %s", dname)
    else
      sprintf("[HopDesk] Aviso de capacidad — %s (%d lugar(es) restante(s))", dname, rem)

    for (addr in recipients) send_email(addr, subject, html)
    invisible(TRUE)
  }, error = function(e) {
    message("[LIMIT] notify_user_limit failed for '", client_id, "': ", e$message)
    invisible(FALSE)
  })
}

email_limit_change_html <- function(client_name, old_limit, new_limit,
                                    changed_by, deactivated = character(0)) {
  direction <- if (new_limit > old_limit) "aumentado" else "reducido"
  color     <- if (new_limit > old_limit) "#16a34a" else "#dc2626"
  deact_section <- if (length(deactivated)) {
    paste0(
      '<p><strong>Cuentas desactivadas como parte del cambio:</strong></p>',
      '<ul style="color:#6b7280;font-size:13px">',
      paste0('<li>', deactivated, '</li>', collapse = ""),
      '</ul>'
    )
  } else ""
  sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:%s">&#128101; Cambio de límite de usuarios — HopDesk</h2>
  <p>El límite de usuarios para <strong>%s</strong> ha sido %s.</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Límite anterior</td>
      <td style="padding:8px;border:1px solid #e5e7eb;font-weight:bold">%d usuarios</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Límite nuevo</td>
      <td style="padding:8px;border:1px solid #e5e7eb;font-weight:bold;color:%s">%d usuarios</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid #e5e7eb;color:#6b7280">Modificado por</td>
      <td style="padding:8px;border:1px solid #e5e7eb">%s</td>
    </tr>
  </table>
  %s
  <p style="color:#666;font-size:13px">Este es un aviso automático de auditoría.</p>
</div>
', color, client_name, direction, old_limit, color, new_limit, changed_by, deact_section)
}

# ── notify_limit_change_to_principals() ───────────────────────────────────────
# Called after a max_users change is confirmed. Notifies all principal-tier
# accounts in hd-admin via in-app notification + email.

notify_limit_change_to_principals <- function(client_id, client_name,
                                               old_limit, new_limit,
                                               changed_by,
                                               deactivated = character(0)) {
  tryCatch({
    msg <- sprintf("Límite de '%s' modificado de %d a %d por %s.",
                   client_name, old_limit, new_limit, changed_by)
    append_hd_notification(
      type         = "limit_change",
      client_id    = client_id,
      message_text = msg,
      metadata     = list(old_limit = old_limit, new_limit = new_limit,
                          changed_by = changed_by,
                          deactivated_count = length(deactivated))
    )

    hd_staff   <- tryCatch(auth_load_usuarios(client_id = "hd-admin"), error = function(e) NULL)
    if (is.null(hd_staff) || !nrow(hd_staff)) return(invisible(FALSE))
    principals <- hd_staff[
      (is.na(hd_staff$deleted) | hd_staff$deleted != TRUE) &
      hd_staff$tier == "principal" &
      !is.na(hd_staff$email) & nzchar(hd_staff$email %||% ""),
    , drop = FALSE]
    if (!nrow(principals)) return(invisible(FALSE))

    html <- email_limit_change_html(client_name, old_limit, new_limit,
                                    changed_by, deactivated)
    subj <- sprintf("[HopDesk] Límite modificado — %s (%d → %d)", client_name,
                    old_limit, new_limit)
    for (addr in principals$email) send_email(addr, subj, html)
    invisible(TRUE)
  }, error = function(e) {
    message("[LIMIT] notify_limit_change_to_principals failed: ", e$message)
    invisible(FALSE)
  })
}
