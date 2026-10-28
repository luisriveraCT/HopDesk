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
  if (!nzchar(from_addr)) from_addr <- "noreply@mail.hopdesk.app"

  # Gmail shows the local part of the address as the sender name when the From
  # header carries no display name -- the 2026-10-27 test landed in the inbox
  # as a bare "onboarding". A display name makes the invitation read as coming
  # from HopDesk, which is also what keeps people from reporting it as phishing.
  from_name <- trimws(Sys.getenv("RESEND_FROM_NAME"))
  if (!nzchar(from_name)) from_name <- "HopDesk"

  # NO Reply-To header, by decision (2026-10-28). Not an oversight, and not a
  # knob left at its default -- the mechanism is gone on purpose, so don't
  # "restore" it:
  #
  #   * hopdesk.app has no MX record at all (checked against 1.1.1.1), so no
  #     address on the domain can receive anything.
  #   * Pointing replies at a corporate mailbox exposes that mailbox to every
  #     invitee, which is what got reported.
  #   * Pointing them at the inviter publishes a staff address to people who
  #     are not users yet. These invitations go to employees who are being
  #     given controlled access; the mail already names who invited them, and
  #     they can reach that person through the channels they already use.
  #
  # If a reply address is ever wanted, it needs a mailbox that exists first:
  # Porkbun forwarding (MX fwd1/fwd2.porkbun.com at the apex) or Resend
  # inbound. See docs/EMAIL_SERVICE_RUNBOOK.md.

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
    mode        = mode,
    live        = identical(mode, "live"),
    from        = from_addr,
    from_name   = from_name,
    # RFC 5322 display-name form, which is what actually goes on the wire.
    from_header = sprintf('%s <%s>', from_name, from_addr),
    label   = switch(mode,
      live         = "Envío de correo activo",
      sandbox      = "Modo sandbox — no se envía correo real",
      unconfigured = "Correo sin configurar",
      invalid      = "Clave de API inválida"
    ),
    detail  = switch(mode,
      live         = paste0("Remitente: ", from_name, " <", from_addr, ">",
                            " · Sin dirección de respuesta"),
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

# ── Payload ──────────────────────────────────────────────────────────────────
# Builds the JSON body Resend expects. Split out of send_email() so it can be
# asserted in tests WITHOUT hitting the network -- the bug below was invisible
# to the whole suite precisely because sandbox mode never serialises anything.
#
# `to = list(to)` produced {"to":[["x@y.com"]]} -- a nested array -- because
# with auto_unbox = FALSE the inner length-1 character vector ALREADY becomes
# an array, and list() wrapped it a second time. Resend answers
# 422 "The `to` field must be a `string`" and NOTHING would have been
# delivered in production. I() marks the vector as-is, giving {"to":["x@y.com"]},
# which is what the API documents (string or array of strings).
.resend_body <- function(from_header, to, subject, html) {
  payload <- list(
    from    = jsonlite::unbox(from_header),
    to      = I(to),
    subject = jsonlite::unbox(subject),
    html    = jsonlite::unbox(html),
    # HTML-only messages score worse with spam filters than multipart ones,
    # and some corporate clients render the plain part only. Derived from the
    # HTML so the two can never drift apart.
    text    = jsonlite::unbox(.html_to_text(html))
  )
  # No reply_to key: see email_service_status() for why there is no Reply-To.
  jsonlite::toJSON(payload, auto_unbox = FALSE)
}

# ── HTML → texto plano ────────────────────────────────────────────────────────
# Deliberately small: these are our own four templates, not arbitrary HTML.
# The one thing it must not do is lose the link -- an invitation whose plain
# part has no URL is useless to anyone reading the text alternative.
.html_to_text <- function(html) {
  x <- html %||% ""
  # Surface each anchor's href next to its label before the tags are stripped.
  # (?s) is load-bearing: our templates put the href and the link label on
  # separate lines, and without it `.` stops at the newline, the match fails,
  # and the <a> tag is later stripped WITH the URL inside it -- a plain-text
  # invitation with no link at all.
  x <- gsub('(?s)<a[^>]*href="([^"]*)"[^>]*>(.*?)</a>', '\\2: \\1', x, perl = TRUE)
  # The captured label keeps the newlines the template indents it with, which
  # would otherwise orphan the URL on its own line under a stray colon:
  #   Activar mi cuenta
  #   : https://...
  x <- gsub('[[:space:]]*\n[[:space:]]*:[ \t]*', ': ', x, perl = TRUE)
  x <- gsub("<br[^>]*>|</p>|</div>|</tr>|</h[1-6]>", "\n", x, perl = TRUE)
  x <- gsub("</td>", " ", x, perl = TRUE)
  x <- gsub("<[^>]+>", "", x, perl = TRUE)
  # Entities our templates actually emit, plus the always-present ampersand.
  ents <- c("&nbsp;" = " ", "&aacute;" = "a", "&eacute;" = "e", "&iacute;" = "i",
            "&oacute;" = "o", "&uacute;" = "u", "&ntilde;" = "n", "&Aacute;" = "A",
            "&quot;" = '"', "&#39;" = "'", "&lt;" = "<", "&gt;" = ">", "&amp;" = "&")
  for (e in names(ents)) x <- gsub(e, ents[[e]], x, fixed = TRUE)
  # Drop the numeric entities used for the emoji/warning glyphs in the headers.
  x <- gsub("&#[0-9]+;", "", x, perl = TRUE)
  x <- gsub("[ \t]+", " ", x, perl = TRUE)
  x <- gsub(" *\n[ \n]*", "\n", x, perl = TRUE)
  trimws(x)
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

  body <- .resend_body(status$from_header, to, subject, html)

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
      # Resend devuelve un id por mensaje. Es la ÚNICA forma de correlacionar
      # "la app dice que lo mandó" con lo que el panel de Resend reporta como
      # entregado o rebotado: un 200 significa "lo acepté", no "llegó".
      # Sin esto, un "no me llegó nada" no se puede investigar.
      sent_id <- tryCatch(
        jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))$id,
        error = function(e) NULL
      )
      message("[EMAIL] Sent to ", to, " (id ", sent_id %||% "?", "): ", subject)
      structure(TRUE, resend_id = sent_id)
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

  # `result`, no TRUE: devolverlo pelón tiraba el atributo resend_id.
  if (isTRUE(result)) return(result)

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

# ── Public app URL ────────────────────────────────────────────────────────────
# Where a link that LEAVES the app must point.
#
# This used to be built, at both call sites, straight from the sender's own
# session$clientData. That is the wrong source: shinyapps.io hands every
# session a per-worker path segment (/Hopdesk/_w_<hex>/) and shinymanager
# appends ?token=/?language= to the address bar. A link assembled from that
# is a link into somebody else's worker, carrying somebody else's auth token
# -- it cannot be mailed to an invitee.
#
# HOPDESK_APP_URL pins the canonical address. It belongs in .Renviron, which
# IS bundled into the shinyapps.io deploy (unlike .Renviron.local). The
# session-derived value stays only as a fallback, now scrubbed.
.normalize_app_url <- function(url) {
  url <- trimws(url %||% "")
  if (!nzchar(url)) return("")
  url <- sub("[?#].*$", "", url)                 # drop query / fragment
  url <- gsub("/_w_[0-9a-fA-F]+(?=/|$)", "", url, perl = TRUE)  # drop worker segment
  sub("/+$", "", url)                            # callers append "/?invite=..."
}

app_public_url <- function(session = NULL) {
  raw <- trimws(Sys.getenv("HOPDESK_APP_URL"))
  # "auto" = derive from this session. It has to be a real word, not an empty
  # value: readRenviron() SKIPS a bare `NAME=` line, so .Renviron.local could
  # not blank out what .Renviron set -- verified 2026-10-27, the production
  # URL leaked into a local run exactly that way. Any override in that file
  # that means "undo this" needs a sentinel, never emptiness.
  pinned <- if (identical(tolower(raw), "auto")) "" else .normalize_app_url(raw)
  if (nzchar(pinned)) return(pinned)
  if (is.null(session)) return("")
  derived <- tryCatch({
    prot <- session$clientData$url_protocol
    host <- session$clientData$url_hostname
    port <- session$clientData$url_port
    path <- session$clientData$url_pathname
    if (!nzchar(host %||% "")) return("")
    if (nzchar(port %||% "") && !port %in% c("80", "443"))
      sprintf("%s//%s:%s%s", prot, host, port, path)
    else
      sprintf("%s//%s%s", prot, host, path)
  }, error = function(e) "")
  .normalize_app_url(derived)
}

# The address an invitee opens. Kept here, beside app_public_url(), so the
# activation page and the mail that advertises it cannot drift apart.
invite_activation_url <- function(token, session = NULL) {
  base <- app_public_url(session)
  if (!nzchar(base)) return("")
  paste0(base, "/?invite=", utils::URLencode(token, reserved = TRUE))
}

# ── Template helpers ──────────────────────────────────────────────────────────
#
# Design constraints these templates are built around, all of them learned the
# hard way by people who ship email:
#
#   * No <style> block, no classes. Several clients strip the <head> entirely
#     (Gmail's webmail keeps it, the Gmail mobile apps historically have not),
#     so every rule is an inline attribute on the element it styles.
#   * No <svg>. Gmail removes SVG outright, so the astrolabe from the login
#     screen cannot travel. It is NOT replaced by a hosted PNG either: images
#     are blocked by default in most clients, so an image-based logo renders
#     as a grey placeholder box on first open -- the worst possible first
#     impression for a message asking you to click a link and type a password.
#     The wordmark is live text, which always renders.
#   * Tables for layout. `display:flex` is not reliable in Outlook's Word
#     rendering engine, and neither is the `font:` shorthand, which is why
#     font-family/size/weight are always written out separately.
#   * Colours are the app's: #07090d near-black, #b49444 gold. The invitee
#     lands on the activation page seconds after clicking, and a pink email
#     leading to a black-and-gold page reads as two different products.
#     (The tagline's #283f58 from the login screen is deliberately dim against
#     that page's grid; at email sizes it is illegible, so it is lightened.)

# htmltools would do this, but these templates are also exercised by tests that
# source R/ without shiny attached, and an invitee's display name is attacker-
# adjacent input: it comes from a form, and whatever it contains is mailed from
# a domain the recipient's provider has been taught to trust.
.email_esc <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;",  x, fixed = TRUE)   # first, or it double-escapes
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.EMAIL_SANS <- "font-family:Helvetica,Arial,sans-serif;"

# A call to action. bgcolor= as well as background: because Outlook ignores the
# CSS one; the padding lives on the <a> so the whole block is clickable.
.email_button <- function(url, label) {
  sprintf(
    '<table role="presentation" cellpadding="0" cellspacing="0" border="0" style="margin:28px 0 20px;">
      <tr><td align="center" bgcolor="#b49444" style="background:#b49444;border-radius:4px;">
        <a href="%s" style="display:inline-block;padding:14px 32px;%sfont-size:14px;font-weight:bold;letter-spacing:0.08em;color:#07090d;text-decoration:none;">%s</a>
      </td></tr>
    </table>',
    .email_esc(url), .EMAIL_SANS, .email_esc(label))
}

# The button alone is not enough for an activation link: corporate gateways
# rewrite or strip anchors, and some clients render the table flat. The bare
# URL below it is plain text on purpose -- Gmail auto-links it, and leaving it
# un-anchored keeps .html_to_text() from emitting "url: url" twice.
.email_fallback_url <- function(url) {
  sprintf(
    '<p style="margin:0 0 4px;%sfont-size:12px;color:#7b8794;">Si el botón no funciona, copia esta dirección en tu navegador:</p>
     <p style="margin:0 0 8px;%sfont-size:12px;color:#4a5b6b;word-break:break-all;">%s</p>',
    .EMAIL_SANS, .EMAIL_SANS, .email_esc(url))
}

# `preheader` is the grey line the inbox list shows next to the subject. Left
# out, the client grabs the first words of the body instead.
.email_shell <- function(body, preheader = "", footer = "") {
  sprintf(
    '<div style="margin:0;padding:0;background:#f4f6f8;">
  <div style="display:none;max-height:0;overflow:hidden;font-size:1px;line-height:1px;color:#f4f6f8;">%s</div>
  <table role="presentation" width="100%%" cellpadding="0" cellspacing="0" border="0" style="background:#f4f6f8;">
    <tr><td align="center" style="padding:28px 12px;">
      <table role="presentation" width="100%%" cellpadding="0" cellspacing="0" border="0" style="max-width:560px;margin:0 auto;border-collapse:collapse;">

        <tr><td align="center" bgcolor="#07090d" style="background:#07090d;padding:32px 28px 24px;">
          <div style="%sfont-size:22px;font-weight:bold;letter-spacing:0.22em;color:#b49444;text-transform:uppercase;">HopDesk</div>
          <div style="%sfont-size:9px;letter-spacing:0.26em;color:#6b8299;text-transform:uppercase;padding-top:7px;">Treasury Intelligence Platform</div>
        </td></tr>

        <tr><td bgcolor="#b49444" style="background:#b49444;height:3px;line-height:3px;font-size:0;">&nbsp;</td></tr>

        <tr><td bgcolor="#ffffff" style="background:#ffffff;padding:34px 32px 28px;%sfont-size:15px;line-height:1.62;color:#23303d;">%s</td></tr>

        <tr><td bgcolor="#eef1f5" style="background:#eef1f5;padding:18px 32px;%sfont-size:11px;line-height:1.7;color:#7b8794;">%s</td></tr>

      </table>
    </td></tr>
  </table>
</div>',
    .email_esc(preheader), .EMAIL_SANS, .EMAIL_SANS, .EMAIL_SANS, body,
    .EMAIL_SANS, footer)
}

.email_footer_default <- function(extra = "") {
  paste0(extra, if (nzchar(extra)) "<br>" else "",
         "© ", format(Sys.Date(), "%Y"), " HopDesk · Mensaje automático.")
}

# `inviter_name` is the person who pressed Enviar, not a department. An
# invitation that says who sent it is the difference between "I'll click this"
# and "I'll ask someone first" -- and it is the honest answer to "who is
# emailing me". Empty when the inviter could not be resolved, in which case
# the sentence simply drops the name instead of inventing one.
email_invite_html <- function(display_name, client_name, invite_url,
                              expires_hours = 48, inviter_name = "") {
  who <- trimws(inviter_name %||% "")
  opening <- if (nzchar(who))
    sprintf("<strong>%s</strong> te invitó a unirte al equipo de <strong>%s</strong> en HopDesk.",
            .email_esc(who), .email_esc(client_name))
  else
    sprintf("Te invitaron a unirte al equipo de <strong>%s</strong> en HopDesk.",
            .email_esc(client_name))

  body <- sprintf(
    '<p style="margin:0 0 14px;font-size:19px;font-weight:bold;color:#07090d;">Hola %s,</p>
     <p style="margin:0 0 14px;">%s</p>
     <p style="margin:0 0 14px;">HopDesk es la plataforma de inteligencia en tesorería donde %s concentra su operación financiera: antigüedad de saldos, bancos, pagos y flujo proyectado, con la información del ERP ya conciliada y al día.</p>
     <p style="margin:0 0 4px;">Para entrar sólo tienes que elegir una contraseña. En esa misma página aparece tu <strong>nombre de usuario</strong>: anótalo, no es tu correo.</p>
     %s
     %s
     <p style="margin:18px 0 0;font-size:13px;color:#7b8794;">El enlace caduca en %d horas y sólo puede usarse una vez.</p>',
    .email_esc(display_name), opening, .email_esc(client_name),
    .email_button(invite_url, "Activar mi cuenta"),
    .email_fallback_url(invite_url),
    as.integer(expires_hours))

  .email_shell(
    body,
    preheader = sprintf("Activa tu cuenta y elige tu contraseña. El enlace caduca en %d horas.",
                        as.integer(expires_hours)),
    footer = .email_footer_default(
      "Si no esperabas esta invitación puedes ignorar este mensaje: sin la contraseña que tú elijas, la cuenta no se crea."))
}

# Sent once the invitee sets their password. The username is DERIVED from the
# display name at activation time (app.R), so the invitee has no way to know
# it in advance — without this mail it only ever appeared in a transient
# showNotification that vanishes after 6 seconds.
email_welcome_html <- function(display_name, username, client_name, app_url) {
  body <- sprintf(
    '<p style="margin:0 0 14px;font-size:19px;font-weight:bold;color:#07090d;">Listo, %s. Tu cuenta está activa.</p>
     <p style="margin:0 0 18px;">Ya formas parte del equipo de <strong>%s</strong> en HopDesk. Estos son tus datos de acceso:</p>
     <table role="presentation" width="100%%" cellpadding="0" cellspacing="0" border="0" style="border-collapse:collapse;margin:0 0 6px;">
       <tr>
         <td bgcolor="#f7f9fb" style="background:#f7f9fb;border:1px solid #e3e8ee;padding:12px 14px;%sfont-size:11px;letter-spacing:0.1em;text-transform:uppercase;color:#7b8794;width:38%%;">Usuario</td>
         <td bgcolor="#f7f9fb" style="background:#f7f9fb;border:1px solid #e3e8ee;padding:12px 14px;font-family:Consolas,Menlo,monospace;font-size:16px;font-weight:bold;letter-spacing:0.04em;color:#07090d;">%s</td>
       </tr>
       <tr>
         <td style="border:1px solid #e3e8ee;padding:12px 14px;%sfont-size:11px;letter-spacing:0.1em;text-transform:uppercase;color:#7b8794;">Contraseña</td>
         <td style="border:1px solid #e3e8ee;padding:12px 14px;%sfont-size:14px;color:#4a5b6b;">La que acabas de crear</td>
       </tr>
     </table>
     <p style="margin:0;font-size:13px;color:#4a5b6b;">Tu nombre de usuario <strong>no es tu correo</strong>. Guarda este mensaje.</p>
     %s',
    .email_esc(display_name), .email_esc(client_name),
    .EMAIL_SANS, .email_esc(username), .EMAIL_SANS, .EMAIL_SANS,
    .email_button(app_url, "Entrar a HopDesk"))

  .email_shell(
    body,
    preheader = sprintf("Tu usuario es %s. Guárdalo: no es tu correo.", username),
    footer = .email_footer_default())
}

email_password_reset_html <- function(display_name, reset_url, expires_hours = 2) {
  body <- sprintf(
    '<p style="margin:0 0 14px;font-size:19px;font-weight:bold;color:#07090d;">Hola %s,</p>
     <p style="margin:0 0 6px;">Recibimos una solicitud para restablecer tu contraseña de HopDesk.</p>
     %s
     %s
     <p style="margin:18px 0 0;font-size:13px;color:#7b8794;">El enlace caduca en %d horas y sólo puede usarse una vez.</p>',
    .email_esc(display_name),
    .email_button(reset_url, "Elegir una contraseña nueva"),
    .email_fallback_url(reset_url),
    as.integer(expires_hours))

  .email_shell(
    body,
    preheader = sprintf("Restablece tu contraseña. El enlace caduca en %d horas.",
                        as.integer(expires_hours)),
    footer = .email_footer_default(
      "Si no fuiste tú, ignora este mensaje: tu contraseña actual sigue funcionando y nadie puede cambiarla sin este enlace."))
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
