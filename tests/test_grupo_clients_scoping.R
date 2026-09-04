# =============================================================================
# tests/test_grupo_clients_scoping.R
#
# Grupo activity-logging audit (2026-09-04), Stage G1: R/tiers_module.R's
# "clients" sub-tab (client requests/approval/rejection/limit-changes/
# contacts). This is the highest-stakes area of the app -- staff routinely
# act on OTHER clients' data from a global admin screen WITHOUT ever
# jumping into that client's vacuum (unlike Bancos/Pasivos, where the
# acting session's own effective_client_id() IS always the real target).
# Getting log_action()'s client_id= wrong here doesn't just mislabel an
# entry -- it silently files the action under the WRONG TENANT's audit
# trail, or drops it from a client's own log entirely.
#
# A full inventory pass (background investigation, verified by hand against
# the real source before any fix) found 3 real scoping bugs in this
# sub-tab, all fixed here:
#   1. aprobacion_cliente (client approval) -- client_id was hard-coded to
#      "hd-admin" even though initialize_client_folder() had ALREADY
#      created the new client's own app_audit.rds moments earlier in the
#      same handler. No dual-write occurred (home_cid == cid == "hd-admin"),
#      so a client's very first possible log entry -- their own approval --
#      was invisible to them forever.
#   2. agregar_contacto / desactivar_contacto -- both hard-coded
#      client_id = "hd-admin" while s3_key correctly named the target
#      client's own file (paste0(cid, "/contacts.rds")) and add_contact()/
#      deactivate_contact() genuinely write there. Internally inconsistent:
#      the write went to the right client, the audit trail didn't.
#
# Everything else in this sub-tab (client requests, rejection, limit-change)
# was already independently verified correct and is left untouched -- see
# the "control" checks in Part B, which exist so a future regression in
# THOSE handlers doesn't slip through unnoticed just because this file only
# originally set out to check the 3 broken ones.
#
# Static-scan approach (not testServer()): tiers_module.R's handlers are
# deeply entangled with `shared` (a large reactive bag), shinyjs, and
# multiple S3-backed reactiveVals -- mocking a real functional harness for
# one handler at a time here would cost far more than it returns, when the
# actual defect class (a hard-coded literal string reaching a specific
# argument) is fully, reliably observable by reading the real source text.
#
# Sourced by _run_confirmed_logic.R (no shiny/AWS setup needed -- pure
# text/regex checks against the real source file).
# =============================================================================

cat("=== test_grupo_clients_scoping ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

tm_txt   <- readLines("R/tiers_module.R", warn = FALSE)
ok("R/tiers_module.R still parses without syntax errors",
   { p <- tryCatch({parse("R/tiers_module.R"); TRUE}, error = function(e) FALSE); p })

# Grabs from an action's log_action(...) opening line through the line
# holding its LAST parameter. Every single log_action() call in this file
# (verified: 20 calls, 20 viewer_home_client_id occurrences, a 1:1 match)
# passes viewer_home_client_id as its final argument -- anchoring on that
# line is far more reliable here than trying to detect a generic closing
# ")" line, since these calls close with "))" attached to their last
# argument's own line, not a standalone closing-paren line (confirmed by
# reading the real source -- a naive "^\\s*\\)\\s*$" search would run past
# the intended block with no bound).
.log_action_block <- function(txt, action_literal) {
  al <- grep(sprintf('action\\s*=\\s*"%s"', action_literal), txt)
  ok(sprintf("found action=\"%s\"'s log_action() call to scan", action_literal), length(al) >= 1)
  if (!length(al)) return(NA_character_)
  start <- al[1]
  while (start > 1 && !grepl("log_action\\(", txt[start])) start <- start - 1
  end <- start
  while (end < length(txt) && !grepl("viewer_home_client_id", txt[end])) end <- end + 1
  paste(txt[start:end], collapse = "\n")
}

# =============================================================================
# Part A -- the 3 fixed handlers now pass the correct client_id
# =============================================================================
cat("\n-- Part A: the 3 fixed handlers --\n")

b_approve <- .log_action_block(tm_txt, "aprobacion_cliente")
if (!is.na(b_approve)) {
  ok("aprobacion_cliente: client_id is now the NEW client (row$client_id_proposed), not hard-coded \"hd-admin\"",
     grepl('client_id\\s*=\\s*row\\$client_id_proposed', b_approve))
  ok("aprobacion_cliente: viewer_home_client_id is still the reviewer's real home (unchanged) -- this is what makes log_action()'s own dual-write actually fire",
     grepl("viewer_home_client_id\\s*=\\s*tryCatch\\(shared\\$home_client_id\\(\\)", b_approve))
  ok("aprobacion_cliente: target_id still names the new client (regression guard -- unrelated to this fix)",
     grepl('target_id\\s*=\\s*row\\$client_id_proposed', b_approve))
}

b_add_contact <- .log_action_block(tm_txt, "agregar_contacto")
if (!is.na(b_add_contact)) {
  ok("agregar_contacto: client_id is now cid (the target client), not hard-coded \"hd-admin\"",
     grepl("client_id\\s*=\\s*cid,", b_add_contact))
  ok("agregar_contacto: s3_key still correctly names <cid>/contacts.rds (regression guard -- this was already right)",
     grepl('s3_key\\s*=\\s*paste0\\(cid, "/contacts\\.rds"\\)', b_add_contact))
}

b_deact_contact <- .log_action_block(tm_txt, "desactivar_contacto")
if (!is.na(b_deact_contact)) {
  ok("desactivar_contacto: client_id is now cid (the target client), not hard-coded \"hd-admin\"",
     grepl("client_id\\s*=\\s*cid,", b_deact_contact))
}

# =============================================================================
# Part B -- control: the handlers that were ALREADY correct stay that way
# (a regression guard, not a re-test of Part A's fixes)
# =============================================================================
cat("\n-- Part B: control -- already-correct handlers unchanged --\n")

b_request <- .log_action_block(tm_txt, "solicitud_nuevo_cliente")
if (!is.na(b_request))
  ok("solicitud_nuevo_cliente: still correctly hd-admin (no client folder exists yet at request time)",
     grepl('client_id\\s*=\\s*"hd-admin"', b_request))

b_reject <- .log_action_block(tm_txt, "rechazo_cliente")
if (!is.na(b_reject))
  ok("rechazo_cliente: still correctly hd-admin (a rejected request never gets a folder)",
     grepl('client_id\\s*=\\s*"hd-admin"', b_reject))

# The limit-change handler deliberately makes TWO separate log_action()
# calls, BOTH literally named action="cambio_limite_usuarios" -- one for
# the registry-level record (hd-admin), one for the client's own log
# (cid) -- found already correct by the investigation and verified here
# directly. Anchored on each occurrence's own nearby lines rather than
# trying to bound the whole observer (this handler's closing brace is
# "}, ignoreInit = TRUE)", not a bare "})", so a generic closing-brace
# search would run past it into unrelated later code).
cl_occurrences <- grep('action\\s*=\\s*"cambio_limite_usuarios"', tm_txt)
ok("found both cambio_limite_usuarios log_action() occurrences", length(cl_occurrences) == 2L)
if (length(cl_occurrences) == 2L) {
  ctx1 <- paste(tm_txt[cl_occurrences[1]:(cl_occurrences[1] + 6)], collapse = "\n")
  ctx2 <- paste(tm_txt[cl_occurrences[2]:(cl_occurrences[2] + 6)], collapse = "\n")
  has_hd_admin <- function(ctx) grepl('client_id\\s*=\\s*"hd-admin"', ctx)
  has_cid      <- function(ctx) grepl("client_id\\s*=\\s*cid,", ctx)
  ok("cambio_limite_usuarios: one occurrence targets hd-admin (registry-level record)",
     has_hd_admin(ctx1) || has_hd_admin(ctx2))
  ok("cambio_limite_usuarios: the OTHER occurrence targets cid (the client's own log)",
     has_cid(ctx1) || has_cid(ctx2))
  ok("cambio_limite_usuarios: the two occurrences are NOT the same call counted twice (one hd-admin, one cid, not both the same)",
     xor(has_hd_admin(ctx1), has_hd_admin(ctx2)))
}

# =============================================================================
# Part C -- functional: log_action()'s own dual-write actually does what
# these fixes now depend on (client_id != viewer_home_client_id -> both
# logs get the entry). Extracts the REAL function, doesn't hand-copy it.
# =============================================================================
cat("\n-- Part C: functional -- log_action() dual-write, exercised the way the fixed calls now rely on it --\n")
{
  .extract_fn <- function(file, fn_name) {
    exprs <- parse(file, keep.source = FALSE)
    for (e in exprs) {
      if (is.call(e) && length(e) >= 2 &&
          identical(e[[1]], as.name("<-")) &&
          identical(e[[2]], as.name(fn_name))) {
        assign(fn_name, eval(e[[3]], envir = globalenv()), envir = globalenv())
        return(invisible(TRUE))
      }
    }
    stop(sprintf("%s not found in %s", fn_name, file))
  }
  assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())
  queued <- list()
  assign(".audit_queue", function(write_cid, row) { queued[[length(queued) + 1L]] <<- list(cid = write_cid, row = row) }, envir = globalenv())
  .extract_fn("R/app_audit.R", "log_action")

  # Mirrors aprobacion_cliente's exact call shape after the fix: a NEW
  # client as client_id, the reviewer's real home ("hd-admin") as
  # viewer_home_client_id.
  log_action(user = "principal_user", module = "clientes", action = "aprobacion_cliente",
            description = "test", target_id = "newclient",
            client_id = "newclient", viewer_home_client_id = "hd-admin")

  ok("dual-write fires exactly twice (once per log) when client_id != viewer_home_client_id",
     length(queued) == 2L)
  cids_written <- vapply(queued, function(q) q$cid, character(1))
  ok("the new client's OWN log receives the entry", "newclient" %in% cids_written)
  ok("hd-admin's log ALSO receives the entry (Hopdesk's own record of the approval)", "hd-admin" %in% cids_written)

  # Control: when they're the SAME (e.g. a client's own dev user acting in
  # their own vacuum), only one write happens -- dual-write must not fire
  # spuriously for a normal, non-cross-tenant action.
  queued <- list()
  log_action(user = "dev_user", module = "usuarios", action = "crear_usuario",
            description = "test", client_id = "networks", viewer_home_client_id = "networks")
  ok("no spurious dual-write when client_id == viewer_home_client_id (the common, non-staff case)",
     length(queued) == 1L)
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0) stop("Tests FAILED.")
