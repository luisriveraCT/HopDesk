# =============================================================================
# tests/test_grupo_invites_logging.R
#
# Grupo activity-logging audit (2026-09-04), Stage G2: R/tiers_module.R's
# "invites" sub-tab. All 3 handlers here previously had NO log_action()
# call at all -- an invite creates a usable signup token bound to a tier
# and a specific client, which is exactly the kind of credential-adjacent
# event that needs an audit trail, and had none.
#
#   1. btn_send_invite  -- client_id = client_in (the invite's TARGET
#      client, an arbitrary client chosen from a dropdown -- NOT the
#      acting staff session's own context).
#   2. inv_revoke_token -- client_id = the invite row's OWN client_id
#      (read before the row is mutated -- status flips to "revoked", but
#      client_id doesn't change, so before/after doesn't matter here, only
#      that we read a real column, not a session-context guess).
#   3. email_retry_id   -- client_id = "hd-admin" (a pure retry of
#      Hopdesk's own send queue, not a client-data mutation -- correctly
#      the one handler in this trio where "hd-admin" IS the right answer).
#
# Static-scan approach, matching test_grupo_clients_scoping.R's rationale.
#
# Sourced by _run_confirmed_logic.R.
# =============================================================================

cat("=== test_grupo_invites_logging ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

tm_txt <- readLines("R/tiers_module.R", warn = FALSE)
ok("R/tiers_module.R still parses without syntax errors",
   { p <- tryCatch({parse("R/tiers_module.R"); TRUE}, error = function(e) FALSE); p })

# Handler-body extractor: from an observeEvent(input$X, { start line
# through the next line matching end_pattern (each handler here has its
# own distinctive close -- verified individually below, not assumed).
.handler_block <- function(txt, input_id, end_pattern) {
  s <- grep(sprintf("observeEvent\\(input\\$%s,", input_id), txt)
  ok(sprintf("found observeEvent(input$%s, ...) to scan", input_id), length(s) >= 1)
  if (!length(s)) return(NA_character_)
  e <- grep(end_pattern, txt); e <- e[e > s[1]][1]
  ok(sprintf("found %s's closing line", input_id), !is.na(e))
  if (is.na(e)) return(NA_character_)
  paste(txt[s[1]:e], collapse = "\n")
}

# =============================================================================
# Part A -- the 3 previously-unlogged handlers now call log_action()
# =============================================================================
cat("\n-- Part A: btn_send_invite --\n")

b_send <- .handler_block(tm_txt, "btn_send_invite", "^\\s*\\}, ignoreInit = TRUE\\)\\s*$")
if (!is.na(b_send)) {
  ok("btn_send_invite calls log_action()", grepl("log_action\\(", b_send))
  ok("btn_send_invite: module=\"invites\"", grepl('module\\s*=\\s*"invites"', b_send))
  ok("btn_send_invite: action=\"enviar_invitacion\"", grepl('action\\s*=\\s*"enviar_invitacion"', b_send))
  ok("btn_send_invite: client_id is client_in (the CHOSEN target client), not hd-admin or a session-context helper",
     grepl("client_id\\s*=\\s*client_in,", b_send))
  ok("btn_send_invite: target_id is the generated token (traceable to the exact invite)",
     grepl("target_id\\s*=\\s*token,", b_send))
  ok("btn_send_invite: the log call comes AFTER create_invite() succeeds (only a real invite gets logged, not a validation-error early return)",
     grepl("create_invite\\(", b_send) &&
     which(grepl("create_invite\\(", strsplit(b_send, "\n")[[1]])) <
     which(grepl("log_action\\(", strsplit(b_send, "\n")[[1]])))
}

cat("\n-- Part B: inv_revoke_token --\n")

b_revoke <- .handler_block(tm_txt, "inv_revoke_token", "^\\s*\\}, ignoreInit = TRUE\\)\\s*$")
if (!is.na(b_revoke)) {
  ok("inv_revoke_token calls log_action()", grepl("log_action\\(", b_revoke))
  ok("inv_revoke_token: action=\"revocar_invitacion\"", grepl('action\\s*=\\s*"revocar_invitacion"', b_revoke))
  ok("inv_revoke_token: client_id is inv_client, read from the invite row's own client_id column",
     grepl("client_id\\s*=\\s*inv_client,", b_revoke))
  ok("inv_revoke_token: inv_client is captured from invites$client_id BEFORE the row is mutated",
     grepl("inv_client <- invites\\$client_id\\[idx\\[1\\]\\]", b_revoke))
  ok("inv_revoke_token: the log call is inside the `if (length(idx))` guard (never fires for a token that doesn't exist)",
     {
       lines <- strsplit(b_revoke, "\n")[[1]]
       if_line  <- which(grepl("if \\(length\\(idx\\)\\)", lines))[1]
       log_line <- which(grepl("log_action\\(", lines))[1]
       !is.na(if_line) && !is.na(log_line) && log_line > if_line
     })
}

cat("\n-- Part C: email_retry_id --\n")

b_retry <- .handler_block(tm_txt, "email_retry_id", "^\\s*\\}, ignoreInit = TRUE\\)\\s*$")
if (!is.na(b_retry)) {
  ok("email_retry_id calls log_action()", grepl("log_action\\(", b_retry))
  ok("email_retry_id: action=\"reintentar_email\"", grepl('action\\s*=\\s*"reintentar_email"', b_retry))
  ok("email_retry_id: client_id is \"hd-admin\" (correct here -- this retries Hopdesk's own send queue, not client data)",
     grepl('client_id\\s*=\\s*"hd-admin",', b_retry))
  ok("email_retry_id: the description records whether the retry actually succeeded, not just that a retry was attempted",
     grepl("if \\(ok\\) .exitoso. else .fallido.", b_retry))
}

# =============================================================================
# Part D -- every one of the 3 new calls passes viewer_home_client_id
# (regression guard: without it, log_action() defaults to Sys.getenv
# ("CLIENT_ID"), which happens to also be "hd-admin" for staff today, but
# would silently be WRONG the moment this app's single-shared-deployment
# assumption ever changes -- explicit is correct, implicit is fragile)
# =============================================================================
cat("\n-- Part D: all 3 pass viewer_home_client_id explicitly --\n")

for (nm in c("enviar_invitacion", "revocar_invitacion", "reintentar_email")) {
  al <- grep(sprintf('action\\s*=\\s*"%s"', nm), tm_txt)
  ok(sprintf("found action=\"%s\" to scan", nm), length(al) >= 1)
  if (length(al)) {
    start <- al[1]
    while (start > 1 && !grepl("log_action\\(", tm_txt[start])) start <- start - 1
    end <- start
    while (end < length(tm_txt) && !grepl("viewer_home_client_id", tm_txt[end])) end <- end + 1
    block <- paste(tm_txt[start:end], collapse = "\n")
    ok(sprintf("%s: passes viewer_home_client_id explicitly", nm),
       grepl("viewer_home_client_id\\s*=\\s*tryCatch\\(shared\\$home_client_id\\(\\)", block))
  }
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0) stop("Tests FAILED.")
