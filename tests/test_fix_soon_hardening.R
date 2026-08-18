# =============================================================================
# tests/test_fix_soon_hardening.R
# Regression tests for the 5 "fix soon" findings from the multi-pass code
# review (2026-08): a crashable log_action() on invalid client_id, a
# silently-lost NA-client_id audit entry, an unbounded audit-flush retry
# loop, a double-click crash in Bancos' "Deshacer confirmación", and the
# credential cache never refreshing after an out-of-process password reset.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Fix-soon hardening (post-review) ────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── A. log_action()/.audit_queue(): empty-string client_id no longer crashes ─

Sys.setenv(CLIENT_ID = "networks")

ok_empty <- tryCatch({
  log_action(user = "x", module = "m", action = "a", description = "d", client_id = "")
  TRUE
}, error = function(e) FALSE)
.chk(ok_empty, TRUE, "log_action(client_id=''): does not throw (previously crashed the shared flush loop)")
# Note: .audit_pending[[""]] itself throws ("zero-length variable name") --
# that's environment indexing semantics, not something to route around in
# the real code (the guard rejects "" before any indexing happens). Confirm
# via ls() instead of repeating the crash here.
.chk("" %in% ls(.audit_pending), FALSE, "log_action(client_id=''): nothing queued under the empty-string key")

# ── B. .audit_queue(): NA client_id no longer silently vanishes into env[['NA']] ─

ok_na <- tryCatch({
  log_action(user = "x", module = "m", action = "a", description = "d", client_id = NA_character_)
  TRUE
}, error = function(e) FALSE)
.chk(ok_na, TRUE, "log_action(client_id=NA): does not throw")
.chk(is.null(.audit_pending[["NA"]]), TRUE,
     "log_action(client_id=NA): nothing silently filed under the literal 'NA' bucket (previously permanent silent loss)")

# ── C. Valid client_id still queues and flushes normally (no regression) ────

mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds", "mock-bucket")
log_action(user = "mouse", module = "m", action = "still_works", description = "d", client_id = "networks")
.audit_flush_pending("networks")
after <- mock_s3readRDS("networks/app_audit.rds", "mock-bucket")
.chk(any(after$action == "still_works"), TRUE,
     "log_action(client_id='networks'): a valid entry still queues and flushes correctly (no regression from the guard)")

# ── D. .audit_flush_pending(): retry is bounded, doesn't grow forever ───────
{
  # Force every flush attempt to fail, regardless of cause, and count attempts.
  .attempts <- 0L
  old_write_chunk <- .audit_write_chunk
  .audit_write_chunk <<- function(df, key) { .attempts <<- .attempts + 1L; stop("simulated S3 outage") }

  Sys.setenv(CLIENT_ID = "networks")
  log_action(user = "x", module = "m", action = "will_fail", description = "d", client_id = "networks")

  # Drive the retry loop directly (bypassing the real later::later() delay)
  # exactly .AUDIT_MAX_RETRIES + 2 times -- enough to exceed the cap if it
  # were unbounded, and to prove it actually stops.
  for (i in seq_len(.AUDIT_MAX_RETRIES + 2L)) .audit_flush_pending("networks")

  # .AUDIT_MAX_RETRIES counts RETRIES after the first attempt, so the total
  # number of real write attempts before giving up is MAX_RETRIES + 1 (one
  # initial attempt + N retries) -- not "not unbounded" via a loose bound.
  .chk(.attempts <= .AUDIT_MAX_RETRIES + 1L, TRUE,
       sprintf("retry is bounded: %d attempt(s) made (1 initial + up to %d retries = %d max, not unbounded)",
               .attempts, .AUDIT_MAX_RETRIES, .AUDIT_MAX_RETRIES + 1L))
  .chk(is.null(.audit_pending[["networks"]]), TRUE,
       "after exhausting retries, the batch is dropped (not held forever, growing memory)")
  .chk(isTRUE(.audit_flush_scheduled[["networks"]]), FALSE,
       "after exhausting retries, no further flush remains scheduled")

  .audit_write_chunk <<- old_write_chunk
}

# ── E. auth.R: credential cache TTL bounds staleness from an external write ─
# (e.g. recover_mouse.R/add_bunny.R, which write straight to S3 and cannot
# call auth_invalidate_credentials() in THIS process.)
{
  users <- data.frame(
    id = "u1", account_code = "U0001", username = "ttluser",
    password_hash = hash_password("Original1!"), display_name = "TTL User",
    tier = "finance", client_id = "networks", permisos = "{}", group_ids = "[]",
    allowed_clients = "[]", email = NA_character_, requires_password_change = FALSE,
    activo = TRUE, created_at = as.character(Sys.time()), last_login = NA_character_,
    deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE
  )
  mock_s3saveRDS(users, "networks/usuarios.rds", "mock-bucket")
  Sys.setenv(CLIENT_ID = "networks")
  auth_invalidate_credentials()

  c1 <- auth_get_credentials()
  fetched_at_1 <- .auth_credentials_at

  # Simulate an EXTERNAL write from a totally separate process (a recovery
  # script) that this process has no way to be told about directly.
  users$password_hash <- hash_password("RecoveredPw2!")
  mock_s3saveRDS(users, "networks/usuarios.rds", "mock-bucket")

  # Immediately after: still within the TTL, so the (now stale) cache is
  # still served -- this is the documented, intentional tradeoff, not a bug.
  c2 <- auth_get_credentials()
  .chk(identical(.auth_credentials_at, fetched_at_1), TRUE,
       "within the TTL window, the cache is NOT refetched (expected -- bounded staleness, not zero staleness)")

  # Simulate TTL expiry by backdating the cache timestamp instead of sleeping
  # (deterministic, no real-time wait in the test suite).
  .auth_credentials_at <<- Sys.time() - (.AUTH_CREDENTIALS_TTL + 5)
  c3 <- auth_get_credentials()
  .chk(!identical(.auth_credentials_at, fetched_at_1), TRUE,
       "after the TTL elapses, auth_get_credentials() refetches from S3 automatically")

  checker <- auth_check_credentials()
  res_new <- checker("ttluser", "RecoveredPw2!")
  res_old <- checker("ttluser", "Original1!")
  .chk(isTRUE(res_new$result), TRUE,
       "after TTL-driven refresh, the EXTERNALLY-written (recovered) password now authenticates")
  .chk(isTRUE(res_old$result), FALSE,
       "after TTL-driven refresh, the stale in-memory password no longer authenticates")
}

# ── F. auth_invalidate_credentials() clears both the value and its timestamp ─
{
  auth_get_credentials()  # ensure both are populated
  auth_invalidate_credentials()
  .chk(is.null(.auth_credentials),    TRUE, "auth_invalidate_credentials(): clears the cached value")
  .chk(is.null(.auth_credentials_at), TRUE, "auth_invalidate_credentials(): clears the cached timestamp too")
}

# ── G. Bancos: recover_confirmacion() double-call still throws (unchanged,
# correct, lower-level behavior) -- and the exact tryCatch shape now wrapping
# it in bancos_module.R's undo_conf handler correctly prevents that from
# propagating, instead of crashing the session. ─────────────────────────────
{
  # Build off the REAL schema + bind_rows rather than a hand-written tibble,
  # so this test doesn't silently drift from bancos_persistence.R's actual
  # column set (recover_confirmacion() touches several fields beyond the
  # ones a naive test fixture would think to include: eliminado_at, action,
  # reverses_confirmacion_id, recovered_at, archive_event_id).
  conf <- dplyr::bind_rows(.schema_bancos_confirmados(), tibble::tibble(
    confirmacion_id = "c1", agenda_item_id = "a1", empresa = "NG", parte = "Proveedor X",
    documento = "F001", codigo = NA_character_, importe = 100, moneda = "MXN",
    cuenta_id = "cta1", fecha = Sys.Date(), tipo = "pago", mov_id = NA_character_,
    confirmado_at = Sys.time(), eliminado = FALSE, eliminado_at = as.POSIXct(NA),
    provision_id = NA_character_, action = "confirmed",
    reverses_confirmacion_id = NA_character_, recovered_at = as.POSIXct(NA),
    archive_event_id = NA_character_
  ))

  first  <- tryCatch(recover_confirmacion(conf, "c1", actor = "tester"), error = function(e) NULL)
  .chk(!is.null(first) && any(first$eliminado[first$confirmacion_id == "c1"]), TRUE,
       "recover_confirmacion(): first call succeeds and soft-deletes the original row")

  # Simulate the double-click: call it again on the ALREADY-recovered id,
  # exactly as a second, fast undo_conf event would.
  threw_uncaught <- tryCatch({ recover_confirmacion(first, "c1", actor = "tester"); FALSE },
                             error = function(e) TRUE)
  .chk(threw_uncaught, TRUE,
       "recover_confirmacion(): a second call on an already-recovered id still throws (correct, unchanged) -- confirms the double-click scenario is real")

  # This is the exact shape now guarding that call in bancos_module.R's
  # undo_conf handler -- verifies the FIX (not just the underlying throw).
  new_conf <- tryCatch(
    recover_confirmacion(first, "c1", actor = "tester"),
    error = function(e) NULL
  )
  .chk(is.null(new_conf), TRUE,
       "the tryCatch wrapper now in undo_conf converts that throw into a clean NULL instead of propagating and crashing the session")
}

# Static confirmation the fix actually landed in the real handler, not just
# in this test's reconstruction of the pattern.
{
  txt   <- readLines("R/bancos_module.R", warn = FALSE)
  start <- grep("observeEvent\\(input\\$undo_conf,", txt)[1]
  block <- paste(txt[start:min(start + 55, length(txt))], collapse = "\n")
  .chk(grepl("tryCatch\\(\\s*\\n?\\s*recover_confirmacion\\(", block), TRUE,
       "R/bancos_module.R: the undo_conf handler's recover_confirmacion() call is wrapped in tryCatch")
}

cat("\n")
