# =============================================================================
# tests/test_email_service_live.R
# Covers the 2026-09-30 "turn the email service on" hardening:
#   A. email_service_status() classifies every RESEND_API_KEY shape
#   B. send_email() no longer reports success when email is unconfigured,
#      and queues instead of dropping the message
#   C. recipient validation
#   D. retry_all_queued_emails() drains the queue
#   E. hd_staff_emails_with_perm() resolves the permission from the TIER
#      (the old code filtered on a column that does not exist, so every
#      limit-alert / user-request mail silently had zero recipients)
#   F. get_limit_alert_recipients() no longer collapses multi-row contact
#      tables to zero via isTRUE() on a vector
# Sourced by _run_saas.R.
# =============================================================================

cat("── Email service (live-mode hardening) ──────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# The runner sets RESEND_API_KEY=sandbox globally; restore it at the end so
# later modules see the environment they expect.
.orig_key <- Sys.getenv("RESEND_API_KEY")

# Point the queue helpers at the mock store.
read_email_queue <- function() {
  raw <- tryCatch(mock_s3readRDS("hd-admin/email_queue.rds", "mock-bucket"),
                  error = function(e) NULL)
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw)) return(.schema_email_queue())
  raw
}
write_email_queue <- function(q) {
  mock_s3saveRDS(q, "hd-admin/email_queue.rds", "mock-bucket")
  invisible(TRUE)
}
mock_s3saveRDS(.schema_email_queue(), "hd-admin/email_queue.rds", "mock-bucket")

# ── A. email_service_status() ────────────────────────────────────────────────

Sys.setenv(RESEND_API_KEY = "sandbox")
.chk(email_service_status()$mode, "sandbox", "status: 'sandbox' -> sandbox mode")
.chk(email_service_status()$live, FALSE,     "status: sandbox is not live")

Sys.setenv(RESEND_API_KEY = "")
.chk(email_service_status()$mode, "unconfigured", "status: empty key -> unconfigured (NOT sandbox)")

Sys.setenv(RESEND_API_KEY = "not-a-resend-key")
.chk(email_service_status()$mode, "invalid", "status: key without 're_' prefix -> invalid")

Sys.setenv(RESEND_API_KEY = "re_abc123FAKEKEYfortests")
.chk(email_service_status()$mode, "live", "status: 're_' key -> live")
.chk(email_service_status()$live, TRUE,   "status: live flag set")

# from address falls back when RESEND_FROM_EMAIL is blank
.saved_from <- Sys.getenv("RESEND_FROM_EMAIL")
Sys.setenv(RESEND_FROM_EMAIL = "")
.chk(email_service_status()$from, "noreply@mail.hopdesk.app", "status: blank from -> default sender")
Sys.setenv(RESEND_FROM_EMAIL = .saved_from)

# ── A2. .resend_body(): the JSON Resend actually receives ────────────────────
# Caught live on 2026-10-27 against the real API: `to = list(to)` serialised to
# {"to":[["x@y.com"]]} and EVERY send came back 422 "The `to` field must be a
# `string`". Nothing in the suite noticed, because sandbox mode returns before
# the payload is ever built. These assertions look at the bytes on the wire.

body <- as.character(.resend_body("HopDesk <noreply@mail.hopdesk.app>",
                                  "ana@empresa.com", "Asunto", "<p>hola</p>"))
.chk(grepl('"to":\\["ana@empresa.com"\\]', body, fixed = FALSE), TRUE,
     "payload: `to` is a flat array of strings, NOT a nested array")
.chk(grepl('"to":\\[\\[', body), FALSE,
     "payload: no double-wrapped `to` (the exact 422 bug)")
.chk(grepl('"from":"HopDesk <noreply@mail.hopdesk.app>"', body, fixed = TRUE), TRUE,
     "payload: `from` carries the RFC 5322 display name, as a bare string")
.chk(grepl('"subject":"Asunto"', body, fixed = TRUE), TRUE,
     "payload: `subject` is a bare string")
.chk(isTRUE(jsonlite::validate(body)), TRUE, "payload: is valid JSON")

parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
# Checked after parsing, not on the raw bytes: jsonlite escapes the forward
# slash (`<\/p>`), which is a legal JSON escape and round-trips correctly.
.chk(parsed$html, "<p>hola</p>", "payload: `html` round-trips to the exact HTML")
.chk(is.character(parsed$from) && length(parsed$from) == 1L, TRUE,
     "payload: parses back with a scalar `from`")
.chk(length(parsed$to) == 1L && is.character(parsed$to[[1]]), TRUE,
     "payload: parses back with exactly one recipient string")
.chk(is.null(parsed$reply_to), TRUE,
     "payload: reply_to omitted when not configured")

# Un TRUE con atributos debe seguir pasando isTRUE(), porque TODOS los call
# sites lo usan. Si alguien cambiara isTRUE por identical(TRUE, x), cada envío
# exitoso se leeria como fallo.
.chk(isTRUE(structure(TRUE, resend_id = "abc")), TRUE,
     "contract: isTRUE() tolera el atributo resend_id del valor de retorno")
.chk(parsed$text, "hola", "payload: carries a plain-text alternative")

body_rt <- as.character(.resend_body("HopDesk <noreply@mail.hopdesk.app>",
                                     "ana@empresa.com", "Asunto", "<p>hola</p>",
                                     reply_to = "soporte@empresa.com"))
.chk(jsonlite::fromJSON(body_rt, simplifyVector = FALSE)$reply_to,
     "soporte@empresa.com", "payload: reply_to included when configured")

# ── A3. .html_to_text() ──────────────────────────────────────────────────────
# A plain-text part that swallows the activation link is worse than none.
inv_html <- email_invite_html("Ana", "ACME",
                              "https://app.example.com/?invite=TOK123", 48)
txt <- .html_to_text(inv_html)
.chk(grepl("https://app.example.com/?invite=TOK123", txt, fixed = TRUE), TRUE,
     "html->text: the activation URL survives")
.chk(grepl("Activar mi cuenta: https://app.example.com/?invite=TOK123", txt,
           fixed = TRUE), TRUE,
     "html->text: label and URL stay on ONE line, not split by a stray colon")
.chk(grepl("(^|\n):", txt), FALSE,
     "html->text: no line begins with an orphaned colon")
.chk(grepl("<", txt, fixed = TRUE), FALSE, "html->text: no leftover tags")
.chk(grepl("&nbsp;|&#[0-9]+;|&aacute;", txt), FALSE,
     "html->text: entities decoded or dropped, none left raw")
.chk(grepl("Ana", txt, fixed = TRUE) && grepl("ACME", txt, fixed = TRUE), TRUE,
     "html->text: the substituted names survive")
.chk(nzchar(.html_to_text(NULL)), FALSE, "html->text: NULL is safe")

# The limit templates use numeric entities and nested tables -- the shape most
# likely to leave garbage behind.
lim_txt <- .html_to_text(email_user_limit_reached_html("ACME", 10L, 10L))
.chk(grepl("ACME", lim_txt, fixed = TRUE), TRUE,
     "html->text: table-based template still yields its content")
.chk(grepl("10 / 10", lim_txt, fixed = TRUE), TRUE,
     "html->text: table cells are separated, not glued together")

# ── B. send_email() tells the truth ──────────────────────────────────────────

Sys.setenv(RESEND_API_KEY = "sandbox")
res <- suppressMessages(send_email("ana@empresa.com", "Asunto", "<p>hola</p>"))
.chk(isTRUE(res), TRUE, "send: sandbox still reports success")
.chk(nrow(read_email_queue()), 0L, "send: sandbox does not queue")

Sys.setenv(RESEND_API_KEY = "")
res <- suppressMessages(send_email("ana@empresa.com", "Asunto", "<p>hola</p>"))
.chk(isTRUE(res), FALSE, "send: unconfigured reports FAILURE (was silently TRUE)")
q <- read_email_queue()
.chk(nrow(q), 1L, "send: unconfigured queues the message instead of dropping it")
.chk(q$to[1], "ana@empresa.com", "send: queued row keeps the recipient")
.chk(q$status[1], "pending", "send: queued row is pending")
.chk(!is.na(q$last_error[1]) && nzchar(q$last_error[1]), TRUE,
     "send: queued row records WHY it failed (last_error was never stored)")

Sys.setenv(RESEND_API_KEY = "no-prefix")
res <- suppressMessages(send_email("beto@empresa.com", "Asunto", "<p>hola</p>"))
.chk(isTRUE(res), FALSE, "send: malformed key reports failure")
.chk(nrow(read_email_queue()), 2L, "send: malformed key queues too")

# queue_on_fail = FALSE must not grow the queue
res <- suppressMessages(send_email("cira@empresa.com", "Asunto", "<p>hola</p>",
                                   queue_on_fail = FALSE))
.chk(isTRUE(res), FALSE, "send: queue_on_fail=FALSE still reports failure")
.chk(nrow(read_email_queue()), 2L, "send: queue_on_fail=FALSE does not queue")

# ── C. Recipient validation ──────────────────────────────────────────────────

Sys.setenv(RESEND_API_KEY = "sandbox")
for (bad in list("", "sin-arroba", "a@b", "a b@c.com", NULL)) {
  res <- suppressMessages(send_email(bad, "Asunto", "<p>x</p>"))
  .chk(isTRUE(res), FALSE,
       sprintf("send: rejects invalid recipient '%s'", bad %||% "<NULL>"))
}
.chk(nrow(read_email_queue()), 2L, "send: invalid recipients are not queued")

res <- suppressMessages(send_email("  Ana.Lopez@Empresa.COM ", "Asunto", "<p>x</p>"))
.chk(isTRUE(res), TRUE, "send: trims and lowercases a valid recipient")

# ── D. retry_all_queued_emails() ─────────────────────────────────────────────

Sys.setenv(RESEND_API_KEY = "sandbox")   # sandbox "delivers", so retries succeed
out <- suppressMessages(retry_all_queued_emails())
.chk(out$sent,   2L, "retry-all: drains both queued messages")
.chk(out$failed, 0L, "retry-all: none failed in sandbox")
q <- read_email_queue()
.chk(all(q$status == "sent"), TRUE, "retry-all: rows marked sent")
.chk(all(is.na(q$last_error)), TRUE, "retry-all: last_error cleared on success")
.chk(all(q$attempts == 1L), TRUE, "retry-all: attempts incremented")

out <- suppressMessages(retry_all_queued_emails())
.chk(out$sent, 0L, "retry-all: already-sent rows are not re-sent")

# max_attempts guard
q <- read_email_queue(); q$status <- "failed"; q$attempts <- 5L; write_email_queue(q)
out <- suppressMessages(retry_all_queued_emails(max_attempts = 5L))
.chk(out$sent + out$failed, 0L, "retry-all: skips rows at the attempt ceiling")

mock_s3saveRDS(.schema_email_queue(), "hd-admin/email_queue.rds", "mock-bucket")

# ── E. hd_staff_emails_with_perm() ───────────────────────────────────────────
# can_manage_invites is resolved from the tier, never stored as a column.

.staff_row <- function(username, tier, email, deleted = FALSE, activo = TRUE,
                       permisos = "{}") {
  data.frame(
    id = paste0("u_", username), account_code = toupper(username),
    username = username, password_hash = "x", display_name = username,
    tier = tier, client_id = "hd-admin", permisos = permisos,
    group_ids = "[]", allowed_clients = "[]", email = email,
    requires_password_change = FALSE, activo = activo,
    created_at = "2026-01-01T00:00:00Z", last_login = NA_character_,
    deleted = deleted, deleted_at = NA_character_,
    stringsAsFactors = FALSE
  )
}

staff <- rbind(
  .staff_row("mouse",  "principal", "mouse@hopdesk.com"),      # can_manage_invites TRUE
  .staff_row("sofia",  "hopdesk",   "sofia@hopdesk.com"),      # TRUE
  .staff_row("lector", "analysis",  "lector@hopdesk.com"),     # FALSE
  .staff_row("viejo",  "principal", "viejo@hopdesk.com", deleted = TRUE),
  .staff_row("baja",   "hopdesk",   "baja@hopdesk.com",  activo = FALSE),
  .staff_row("sinmail","principal", "")
)
auth_save_usuarios(staff, client_id = "hd-admin")

got <- sort(hd_staff_emails_with_perm("can_manage_invites"))
.chk(got, sort(c("mouse@hopdesk.com", "sofia@hopdesk.com")),
     "staff-perm: returns exactly the tiers that hold the permission")
.chk(length(got), 2L,
     "staff-perm: MORE THAN ONE recipient (old isTRUE(vector) returned zero)")

# an explicit permisos override must win over the tier default
staff2 <- staff
staff2$permisos[staff2$username == "lector"] <- '{"can_manage_invites":true}'
auth_save_usuarios(staff2, client_id = "hd-admin")
.chk(sort(hd_staff_emails_with_perm("can_manage_invites")),
     sort(c("mouse@hopdesk.com", "sofia@hopdesk.com", "lector@hopdesk.com")),
     "staff-perm: honours a per-user permisos override")
auth_save_usuarios(staff, client_id = "hd-admin")

.chk(hd_staff_emails_with_perm("can_approve_clients"), "mouse@hopdesk.com",
     "staff-perm: works for other permission keys too")

# ── F. get_limit_alert_recipients() with several contacts ────────────────────

# Planted in the mock store rather than overridden here: the module runs in
# its own env, so a local read_contacts() would be invisible to
# get_limit_alert_recipients(), which lives in globalenv.
mock_s3saveRDS(
  data.frame(
    id = c("c1", "c2", "c3"),
    name = c("CFO", "Tesorero", "Ex"),
    email = c("cfo@cliente.com", "tesoreria@cliente.com", "ex@cliente.com"),
    role = c("cfo", "tes", "x"),
    receives_limit_alerts    = c(TRUE, TRUE, TRUE),
    receives_audit_summaries = c(FALSE, FALSE, FALSE),
    active                   = c(TRUE, TRUE, FALSE),
    created_at = rep("2026-01-01T00:00:00Z", 3),
    stringsAsFactors = FALSE
  ),
  "acme/contacts.rds", "mock-bucket"
)

write_client_registry(rbind(.schema_client_registry(), data.frame(
  client_id     = "acme",
  display_name  = "ACME",
  max_users     = 10L,
  current_users = 9L,
  contact_email = "admin@cliente.com",
  status        = "active",
  created_at    = "2026-01-01T00:00:00Z",
  created_by    = "mouse",
  stringsAsFactors = FALSE
)))

recips <- sort(get_limit_alert_recipients("acme"))
.chk("cfo@cliente.com"       %in% recips, TRUE, "limit-recipients: includes first alert contact")
.chk("tesoreria@cliente.com" %in% recips, TRUE,
     "limit-recipients: includes SECOND alert contact (isTRUE(vector) dropped both)")
.chk("ex@cliente.com"        %in% recips, FALSE, "limit-recipients: excludes inactive contact")
.chk("admin@cliente.com"     %in% recips, TRUE, "limit-recipients: includes registry contact_email")
.chk("mouse@hopdesk.com"     %in% recips, TRUE, "limit-recipients: includes hd-admin staff")
.chk("lector@hopdesk.com"    %in% recips, FALSE, "limit-recipients: excludes staff without the perm")
.chk(anyDuplicated(recips), 0L, "limit-recipients: deduplicated")

Sys.setenv(RESEND_API_KEY = .orig_key)
