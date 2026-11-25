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
    label   = tr(paste0("niveles.correo.estado.", mode)),
    detail  = tr(paste0("niveles.correo.estado.", mode, ".detalle"), nombre = from_name, correo = from_addr)
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
# Strict (CM18): a failed read stop()s. It used to come back empty and the
# next save (s3saveRDS, outside the net) left a queue holding only its own
# row: every other pending invitation was lost.
read_email_queue <- function() leer_hd_admin("email_queue.rds", .schema_email_queue)

# cambio(queue) -> queue, on S3's current queue; a failed read or write stop()s.
modificar_email_queue <- function(cambio) modificar_hd_admin("email_queue.rds", .schema_email_queue, cambio)

# Applies `cambios` (list of list(id, status, last_error)) to the queue rows
# with those ids, on S3's current queue, one more attempt each.
.email_queue_marcar <- function(cambios, now) {
  modificar_email_queue(function(q) {
    for (x in cambios) {
      i <- which(q$id == x$id)
      if (!length(i)) next
      q$last_tried[i] <- now
      q$attempts[i]   <- ifelse(is.na(q$attempts[i]), 0L, q$attempts[i]) + 1L
      q$status[i]     <- x$status
      q$last_error[i] <- x$last_error
    }
    q
  })
}

.enqueue_email <- function(to, subject, html, error = NA_character_) {
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
  # The send already failed and the caller says so; a queue that can't be
  # saved is only logged (this function never throws).
  guardado <- tryCatch({ modificar_email_queue(function(q) .hd_admin_agregar(q, new_row)); TRUE },
                       error = function(e) { message("[EMAIL] Could not persist queue: ", e$message); FALSE })
  if (guardado) message("[EMAIL] Queued (", to, "): ", subject)
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
    return(structure(FALSE, error = tr("niveles.correo.error.destinatario", correo = to)))
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

  err_msg <- if (is.list(result)) result$error else tr("niveles.correo.error.desconocido")
  if (queue_on_fail) .enqueue_email(to, subject, html, error = err_msg)
  structure(FALSE, error = err_msg)
}

# Human-readable reason for the last failure of a send_email() call.
email_error_of <- function(x) {
  e <- attr(x, "error", exact = TRUE)
  if (is.null(e) || !nzchar(e %||% "")) tr("niveles.correo.error.desconocido") else e
}

# ── Retry a queued email by id ────────────────────────────────────────────────
retry_queued_email <- function(queue_id) {
  q <- read_email_queue()
  idx <- which(q$id == queue_id)
  if (!length(idx)) return(FALSE)

  row <- q[idx[1], ]
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  ok <- send_email(row$to, row$subject, row$html, queue_on_fail = FALSE)

  # The mail went out (or not) whatever happens to this save: a failed save is
  # logged, and the result is still the send's, so it is never re-sent as if
  # it had failed.
  tryCatch(.email_queue_marcar(list(list(
    id = queue_id, status = if (isTRUE(ok)) "sent" else "failed",
    last_error = if (isTRUE(ok)) NA_character_ else email_error_of(ok))), now),
    error = function(e) message("[EMAIL] Could not persist queue: ", e$message))
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
  sent <- 0L; failed <- 0L; cambios <- list()
  for (i in idx) {
    res <- send_email(q$to[i], q$subject[i], q$html[i], queue_on_fail = FALSE)
    if (isTRUE(res)) sent <- sent + 1L else failed <- failed + 1L
    cambios[[length(cambios) + 1L]] <- list(
      id = q$id[i], status = if (isTRUE(res)) "sent" else "failed",
      last_error = if (isTRUE(res)) NA_character_ else email_error_of(res))
  }
  # On S3's current queue: mail queued by another session meanwhile stays.
  tryCatch(.email_queue_marcar(cambios, now),
           error = function(e) message("[EMAIL] Could not persist queue: ", e$message))
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
#   * Colours are theme roles: every colour is a {{rol}} marker that
#     .email_pintar() resolves with the registry (R/tema_registro.R) when the
#     mail is built -- mail clients do not understand var(). Under Clásico they
#     are today's near-black and gold, so the invitee who lands on the
#     activation page seconds after clicking sees one product, not two. (The
#     login's tagline colour is deliberately dim against that page's grid; at
#     email sizes it is illegible, so the mail's is lighter.)

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

# Turns every {{rol}} marker of a finished mail into the theme's hex (mail
# clients do not understand var(), so the colour is resolved here, in R).
# `tema` NULL is the default theme: mail follows the CLIENT's theme, never a
# user's preference, and until clients have one (TEMA stage 5) that is Clásico.
# The one copy of that lives in the registry: tema_pintar().
.email_pintar <- function(html, tema = NULL) tema_pintar(html, tema)

# Dictionary text for a mail (niveles.correo.*): the values are escaped before
# they go in, so a name can never inject markup, and **this** is set in bold.
# `idioma` NULL is the active language (hd_idioma()); a caller that knows the
# recipient's language passes it.
.email_t <- function(clave, idioma = NULL, ...) {
  vars <- lapply(list(...), function(v) if (is.character(v)) .email_esc(v) else v)
  txt <- do.call(tr, c(list(paste0("niveles.correo.", clave)), vars, list(idioma = idioma %||% hd_idioma())))
  gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", txt)
}

# A call to action. bgcolor= as well as background: because Outlook ignores the
# CSS one; the padding lives on the <a> so the whole block is clickable.
.email_button <- function(url, label) {
  sprintf(
    '<table role="presentation" cellpadding="0" cellspacing="0" border="0" style="margin:28px 0 20px;">
      <tr><td align="center" bgcolor="{{acento}}" style="background:{{acento}};border-radius:4px;">
        <a href="%s" style="display:inline-block;padding:14px 32px;%sfont-size:14px;font-weight:bold;letter-spacing:0.08em;color:{{leg-texto-07090d}};text-decoration:none;">%s</a>
      </td></tr>
    </table>',
    .email_esc(url), .EMAIL_SANS, .email_esc(label))
}

# The button alone is not enough for an activation link: corporate gateways
# rewrite or strip anchors, and some clients render the table flat. The bare
# URL below it is plain text on purpose -- Gmail auto-links it, and leaving it
# un-anchored keeps .html_to_text() from emitting "url: url" twice.
.email_fallback_url <- function(url, idioma = NULL) {
  sprintf(
    '<p style="margin:0 0 4px;%sfont-size:12px;color:{{texto-2}};">%s</p>
     <p style="margin:0 0 8px;%sfont-size:12px;color:{{leg-texto-2-4a5b6b}};word-break:break-all;">%s</p>',
    .EMAIL_SANS, .email_t("boton_falla", idioma), .EMAIL_SANS, .email_esc(url))
}

# `preheader` is the grey line the inbox list shows next to the subject. Left
# out, the client grabs the first words of the body instead.
.email_shell <- function(body, preheader = "", footer = "", tema = NULL, idioma = NULL) {
  .email_pintar(sprintf(
    '<div style="margin:0;padding:0;background:{{superficie-2}};">
  <div style="display:none;max-height:0;overflow:hidden;font-size:1px;line-height:1px;color:{{superficie-2}};">%s</div>
  <table role="presentation" width="100%%" cellpadding="0" cellspacing="0" border="0" style="background:{{superficie-2}};">
    <tr><td align="center" style="padding:28px 12px;">
      <table role="presentation" width="100%%" cellpadding="0" cellspacing="0" border="0" style="max-width:560px;margin:0 auto;border-collapse:collapse;">

        <tr><td align="center" bgcolor="{{leg-encabezado-07090d}}" style="background:{{leg-encabezado-07090d}};padding:32px 28px 24px;">
          <div style="%sfont-size:22px;font-weight:bold;letter-spacing:0.22em;color:{{acento}};text-transform:uppercase;">HopDesk</div>
          <div style="%sfont-size:9px;letter-spacing:0.26em;color:{{leg-oscuro-texto-2-6b8299}};text-transform:uppercase;padding-top:7px;">%s</div>
        </td></tr>

        <tr><td bgcolor="{{acento}}" style="background:{{acento}};height:3px;line-height:3px;font-size:0;">&nbsp;</td></tr>

        <tr><td bgcolor="{{superficie}}" style="background:{{superficie}};padding:34px 32px 28px;%sfont-size:15px;line-height:1.62;color:{{leg-texto-23303d}};">%s</td></tr>

        <tr><td bgcolor="{{superficie-2}}" style="background:{{superficie-2}};padding:18px 32px;%sfont-size:11px;line-height:1.7;color:{{texto-2}};">%s</td></tr>

      </table>
    </td></tr>
  </table>
</div>',
    .email_esc(preheader), .EMAIL_SANS, .EMAIL_SANS, .email_t("lema", idioma), .EMAIL_SANS, body,
    .EMAIL_SANS, footer), tema)
}

.email_footer_default <- function(extra = "", idioma = NULL) {
  paste0(extra, if (nzchar(extra)) "<br>" else "",
         .email_t("pie", idioma, anio = format(Sys.Date(), "%Y")))
}

# `inviter_name` is the person who pressed Enviar, not a department. An
# invitation that says who sent it is the difference between "I'll click this"
# and "I'll ask someone first" -- and it is the honest answer to "who is
# emailing me". Empty when the inviter could not be resolved, in which case
# the sentence simply drops the name instead of inventing one.
email_invite_html <- function(display_name, client_name, invite_url,
                              expires_hours = 48, inviter_name = "", tema = NULL, idioma = NULL) {
  who <- trimws(inviter_name %||% "")
  opening <- if (nzchar(who))
    .email_t("invitacion.abre_con", idioma, quien = who, cliente = client_name)
  else
    .email_t("invitacion.abre_sin", idioma, cliente = client_name)
  horas <- as.integer(expires_hours)

  body <- sprintf(
    '<p style="margin:0 0 14px;font-size:19px;font-weight:bold;color:{{leg-texto-07090d}};">%s</p>
     <p style="margin:0 0 14px;">%s</p>
     <p style="margin:0 0 14px;">%s</p>
     <p style="margin:0 0 4px;">%s</p>
     %s
     %s
     <p style="margin:18px 0 0;font-size:13px;color:{{texto-2}};">%s</p>',
    .email_t("saludo", idioma, nombre = display_name), opening,
    .email_t("invitacion.que_es", idioma, cliente = client_name),
    .email_t("invitacion.entrar", idioma),
    .email_button(invite_url, tr("niveles.correo.invitacion.boton", idioma = idioma %||% hd_idioma())),
    .email_fallback_url(invite_url, idioma),
    .email_t("caduca", idioma, horas = horas))

  .email_shell(
    body,
    tema = tema,
    idioma = idioma,
    preheader = tr("niveles.correo.invitacion.preheader", horas = horas, idioma = idioma %||% hd_idioma()),
    footer = .email_footer_default(.email_t("invitacion.pie", idioma), idioma))
}

# Sent once the invitee sets their password. The username is DERIVED from the
# display name at activation time (app.R), so the invitee has no way to know
# it in advance — without this mail it only ever appeared in a transient
# showNotification that vanishes after 6 seconds.
email_welcome_html <- function(display_name, username, client_name, app_url, tema = NULL, idioma = NULL) {
  body <- sprintf(
    '<p style="margin:0 0 14px;font-size:19px;font-weight:bold;color:{{leg-texto-07090d}};">%s</p>
     <p style="margin:0 0 18px;">%s</p>
     <table role="presentation" width="100%%" cellpadding="0" cellspacing="0" border="0" style="border-collapse:collapse;margin:0 0 6px;">
       <tr>
         <td bgcolor="{{superficie-2}}" style="background:{{superficie-2}};border:1px solid {{borde}};padding:12px 14px;%sfont-size:11px;letter-spacing:0.1em;text-transform:uppercase;color:{{texto-2}};width:38%%;">%s</td>
         <td bgcolor="{{superficie-2}}" style="background:{{superficie-2}};border:1px solid {{borde}};padding:12px 14px;font-family:Consolas,Menlo,monospace;font-size:16px;font-weight:bold;letter-spacing:0.04em;color:{{leg-texto-07090d}};">%s</td>
       </tr>
       <tr>
         <td style="border:1px solid {{borde}};padding:12px 14px;%sfont-size:11px;letter-spacing:0.1em;text-transform:uppercase;color:{{texto-2}};">%s</td>
         <td style="border:1px solid {{borde}};padding:12px 14px;%sfont-size:14px;color:{{leg-texto-2-4a5b6b}};">%s</td>
       </tr>
     </table>
     <p style="margin:0;font-size:13px;color:{{leg-texto-2-4a5b6b}};">%s</p>
     %s',
    .email_t("bienvenida.titulo", idioma, nombre = display_name),
    .email_t("bienvenida.intro", idioma, cliente = client_name),
    .EMAIL_SANS, .email_t("bienvenida.usuario", idioma), .email_esc(username),
    .EMAIL_SANS, .email_t("bienvenida.contrasena", idioma),
    .EMAIL_SANS, .email_t("bienvenida.contrasena_valor", idioma),
    .email_t("bienvenida.nota", idioma),
    .email_button(app_url, tr("niveles.correo.bienvenida.boton", idioma = idioma %||% hd_idioma())))

  .email_shell(
    body,
    tema = tema,
    idioma = idioma,
    preheader = tr("niveles.correo.bienvenida.preheader", usuario = username, idioma = idioma %||% hd_idioma()),
    footer = .email_footer_default(idioma = idioma))
}

email_password_reset_html <- function(display_name, reset_url, expires_hours = 2, tema = NULL, idioma = NULL) {
  horas <- as.integer(expires_hours)
  body <- sprintf(
    '<p style="margin:0 0 14px;font-size:19px;font-weight:bold;color:{{leg-texto-07090d}};">%s</p>
     <p style="margin:0 0 6px;">%s</p>
     %s
     %s
     <p style="margin:18px 0 0;font-size:13px;color:{{texto-2}};">%s</p>',
    .email_t("saludo", idioma, nombre = display_name),
    .email_t("contrasena.intro", idioma),
    .email_button(reset_url, tr("niveles.correo.contrasena.boton", idioma = idioma %||% hd_idioma())),
    .email_fallback_url(reset_url, idioma),
    .email_t("caduca", idioma, horas = horas))

  .email_shell(
    body,
    tema = tema,
    idioma = idioma,
    preheader = tr("niveles.correo.contrasena.preheader", horas = horas, idioma = idioma %||% hd_idioma()),
    footer = .email_footer_default(.email_t("contrasena.pie", idioma), idioma))
}

email_user_limit_warning_html <- function(client_name, current_users, max_users, tema = NULL, idioma = NULL) {
  remaining <- max_users - current_users
  .email_pintar(sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:{{aviso-texto}}">&#9888; %s</h2>
  <p>%s</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}};font-weight:bold">%d</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}};font-weight:bold">%d</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}};font-weight:bold;color:{{aviso-texto}}">%d</td>
    </tr>
  </table>
  <p style="color:{{leg-texto-2-666666}};font-size:13px">
    %s
  </p>
</div>
', .email_t("aviso.titulo", idioma), .email_t("aviso.texto", idioma, cliente = client_name),
   .email_t("limite.activos", idioma), current_users, .email_t("limite.limite", idioma), max_users,
   .email_t("limite.disponibles", idioma), remaining, .email_t("aviso.nota", idioma)), tema)
}

email_user_limit_reached_html <- function(client_name, current_users, max_users, tema = NULL, idioma = NULL) {
  .email_pintar(sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:{{peligro}}">&#128274; %s</h2>
  <p>%s</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}};font-weight:bold">%d / %d</td>
    </tr>
  </table>
  <p>%s</p>
  <p style="color:{{leg-texto-2-666666}};font-size:13px">
    %s
  </p>
</div>
', .email_t("tope.titulo", idioma), .email_t("tope.texto", idioma, cliente = client_name),
   .email_t("limite.activos", idioma), current_users, max_users,
   .email_t("tope.bloqueo", idioma), .email_t("tope.nota", idioma)), tema)
}

email_user_request_html <- function(client_name, current_users, max_users, contact_email, tema = NULL, idioma = NULL) {
  .email_pintar(sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:{{primario}}">&#128101; %s</h2>
  <p>%s</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}};font-weight:bold">%d / %d</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}}">
        <a href="mailto:%s">%s</a>
      </td>
    </tr>
  </table>
  <p style="color:{{leg-texto-2-666666}};font-size:13px">
    %s
  </p>
</div>
', .email_t("solicitud.titulo", idioma), .email_t("solicitud.texto", idioma, cliente = client_name),
   .email_t("limite.activos", idioma), current_users, max_users,
   .email_t("solicitud.contacto", idioma), contact_email, contact_email,
   .email_t("solicitud.nota", idioma)), tema)
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
      tr("niveles.correo.notif.tope", cliente = dname, actuales = cur, tope = max)
    else
      tr_n("niveles.correo.notif.aviso", rem, cliente = dname, actuales = cur, tope = max)

    append_hd_notification(type = type, client_id = client_id, message_text = msg,
                           metadata = list(current_users = cur, max_users = max))

    recipients <- tryCatch(get_limit_alert_recipients(client_id), error = function(e) character(0))
    if (!length(recipients)) return(invisible(TRUE))

    html_fn <- if (rem <= 0) email_user_limit_reached_html else email_user_limit_warning_html
    tema_c  <- correo_tema(client_id)   # TEMA 6-9: the client's theme
    html    <- if (rem <= 0)
      email_user_limit_reached_html(dname, cur, max, tema = tema_c)
    else
      email_user_limit_warning_html(dname, cur, max, tema = tema_c)

    subject <- if (rem <= 0)
      tr("niveles.correo.asunto.tope", cliente = dname)
    else
      tr_n("niveles.correo.asunto.aviso", rem, cliente = dname)

    for (addr in recipients) send_email(addr, subject, html)
    invisible(TRUE)
  }, error = function(e) {
    message("[LIMIT] notify_user_limit failed for '", client_id, "': ", e$message)
    invisible(FALSE)
  })
}

email_limit_change_html <- function(client_name, old_limit, new_limit,
                                    changed_by, deactivated = character(0), tema = NULL, idioma = NULL) {
  sube  <- new_limit > old_limit
  color <- if (sube) "{{exito-texto}}" else "{{peligro}}"
  deact_section <- if (length(deactivated)) {
    paste0(
      '<p><strong>', .email_t("cambio.desactivadas", idioma), '</strong></p>',
      '<ul style="color:{{texto-2}};font-size:13px">',
      paste0('<li>', .email_esc(deactivated), '</li>', collapse = ""),
      '</ul>'
    )
  } else ""
  .email_pintar(sprintf('
<div style="font-family:sans-serif;max-width:520px;margin:auto;padding:32px">
  <h2 style="color:%s">&#128101; %s</h2>
  <p>%s</p>
  <table style="border-collapse:collapse;width:100%%;margin:16px 0">
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}};font-weight:bold">%s</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}};font-weight:bold;color:%s">%s</td>
    </tr>
    <tr>
      <td style="padding:8px;border:1px solid {{borde}};color:{{texto-2}}">%s</td>
      <td style="padding:8px;border:1px solid {{borde}}">%s</td>
    </tr>
  </table>
  %s
  <p style="color:{{leg-texto-2-666666}};font-size:13px">%s</p>
</div>
', color, .email_t("cambio.titulo", idioma),
   .email_t(if (sube) "cambio.texto.sube" else "cambio.texto.baja", idioma, cliente = client_name),
   .email_t("cambio.anterior", idioma), .email_t("cambio.usuarios", idioma, n = as.integer(old_limit)),
   .email_t("cambio.nuevo", idioma), color, .email_t("cambio.usuarios", idioma, n = as.integer(new_limit)),
   .email_t("cambio.por", idioma), .email_esc(changed_by), deact_section,
   .email_t("cambio.nota", idioma)), tema)
}

# ── notify_limit_change_to_principals() ───────────────────────────────────────
# Called after a max_users change is confirmed. Notifies all principal-tier
# accounts in hd-admin via in-app notification + email.

notify_limit_change_to_principals <- function(client_id, client_name,
                                               old_limit, new_limit,
                                               changed_by,
                                               deactivated = character(0)) {
  tryCatch({
    msg <- tr("niveles.correo.notif.cambio", cliente = client_name, antes = as.integer(old_limit),
              despues = as.integer(new_limit), quien = changed_by)
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
      hd_staff$tier == nivel_tope() &
      !is.na(hd_staff$email) & nzchar(hd_staff$email %||% ""),
    , drop = FALSE]
    if (!nrow(principals)) return(invisible(FALSE))

    html <- email_limit_change_html(client_name, old_limit, new_limit,
                                    changed_by, deactivated,
                                    tema = correo_tema(client_id))   # TEMA 6-9: about that client
    subj <- tr("niveles.correo.asunto.cambio", cliente = client_name,
               antes = as.integer(old_limit), despues = as.integer(new_limit))
    for (addr in principals$email) send_email(addr, subj, html)
    invisible(TRUE)
  }, error = function(e) {
    message("[LIMIT] notify_limit_change_to_principals failed: ", e$message)
    invisible(FALSE)
  })
}
