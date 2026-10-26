# =============================================================================
# tests/test_moves_save_failure_visibility.R
#
# CRITICAL fix (2026-09-01): live report "tesoreria's moves are gone" traced
# to real S3 data being fully intact (see scripts/diagnose_moves_readonly.R
# and scripts/diagnose_moves_join_readonly.R output from that day) while the
# actual bug was .save_moves_deferred() (R/ledger_module.R) treating its S3
# write as fire-and-forget: shared$moves_db(updated) always fires immediately
# (so the calendar visibly "applies" the move and the Activity log used to
# unconditionally record it as a fact), but the real S3 write happened later
# via later::later() with only a server-console warning() on failure -- never
# surfaced to the browser. A transient failure (S3 hiccup, clock skew --
# hopdesk_start.bat's own VPN/clock-resync step documents exactly this class
# of AWS failure risk on cold boot -- or a dropped ngrok tunnel, seen live in
# hopdesk_ngrok.log's own "heartbeat timeout / failed to reconnect" gap)
# meant the move looked successful and even got logged as one, but never
# reached S3 -- so the next fresh session reverted to the last real save and
# every edit made since silently vanished with no error anyone ever saw.
#
# This test exercises the real .save_moves_deferred() end-to-end (mocking
# only save_moves/bump_sync_version/log_action/showNotification) to confirm:
#   Part A: a transient failure that succeeds on retry logs the normal
#           success action once -- no user-facing error, no failure log entry.
#   Part B: a failure that persists through the retry (1) shows a persistent
#           (duration = NULL, so it won't auto-dismiss unnoticed), explicit
#           on-screen error via the caller's own session, and (2) logs a
#           distinctly-flagged "..._fallo_guardado" entry instead of a
#           misleading success one -- the Activity log must never claim a
#           save happened when it didn't.
#   Part C: every real call site that can trigger a move now threads a
#           session through so the error can actually reach someone.
#
# Sourced by _run_saas.R (S3 fully mocked there; this file mocks the
# higher-level save_moves/log_action/showNotification directly instead).
#
# R/ledger_module.R is a large Shiny UI module and is NOT in _run_saas.R's
# base source list (moduleServer()-wrapped code needs a live session to
# exercise meaningfully; the base list only carries pure-logic files). Its
# top-level helper functions (.save_moves_deferred/.apply_move/.clear_moves)
# sit outside any moduleServer() call, though, so Part A/B here extract and
# eval() just .save_moves_deferred()'s own definition out of the real file
# text -- the same "extract the real block, then execute it" approach this
# suite's own Phase 2 test (test_phase2_stale_preload_cache.R) already uses
# for app.R -- rather than sourcing the whole module or re-implementing the
# function's logic by hand (either of which could quietly drift from the
# real code this bug lives in).
# =============================================================================

cat("── Moves save-failure visibility (critical, 2026-09-01) ───────────────────\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)

ledger_src <- readLines("R/ledger_module.R", warn = FALSE)
fn_start   <- grep("^\\.save_moves_deferred <- function", ledger_src)
fn_end     <- grep("^\\.apply_move <- function", ledger_src)
.chk_true(length(fn_start) == 1 && length(fn_end) == 1 && fn_start < fn_end,
          "located exactly one .save_moves_deferred() block in R/ledger_module.R, ending before .apply_move()")
fn_txt <- paste(ledger_src[fn_start:(fn_end - 1)], collapse = "\n")
# Evaluated into THIS script's own environment (not globalenv) so
# .save_moves_deferred()'s lexical lookups for save_moves/bump_sync_version/
# log_action/showNotification resolve to the mocks defined below, not to
# whatever (if anything) those names happen to be bound to globally.
eval(parse(text = fn_txt), envir = environment())
.chk_true(exists(".save_moves_deferred", mode = "function"),
          ".save_moves_deferred() extracted and evaluated successfully from the real source file")

# =============================================================================
# Part A -- transient failure, succeeds on retry: logs success, no error UI
# =============================================================================
cat("\n-- Part A: retry succeeds -- logs success, shows no error --\n")

.calls_save    <- 0L
.logged        <- list()
.notifications <- list()

save_moves         <- function(df, client_id = NULL) {
  .calls_save <<- .calls_save + 1L
  if (.calls_save == 1L) stop("simulated transient S3 failure")
  invisible(NULL)
}
bump_sync_version  <- function(...) invisible(NULL)
log_action         <- function(...) { .logged[[length(.logged) + 1L]] <<- list(...); invisible(NULL) }
showNotification   <- function(...) { .notifications[[length(.notifications) + 1L]] <<- list(...); invisible(NULL) }

fake_session <- structure(list(), class = "ShinySession")

.save_moves_deferred(
  data.frame(x = 1),
  client_id = "networks",
  session   = fake_session,
  log_info  = list(user = "tester", module = "ledger_AP", action = "mover_fecha",
                    description = "1 factura(s) movida(s) a 29/08/2026",
                    target_id = "123", metadata = list(n = 1),
                    client_id = "networks", viewer_home_client_id = "networks")
)

later::run_now()                 # first attempt (delay=0) -- fails, schedules retry
.chk(.calls_save, 1L, "first attempt ran immediately and failed as simulated")
.chk_true(length(.logged) == 0 && length(.notifications) == 0,
          "no log entry or notification yet -- retry hasn't run")

Sys.sleep(2.2)
later::run_now()                 # retry attempt (delay=2) -- succeeds
.chk(.calls_save, 2L, "retry attempt ran after the delay and succeeded")
.chk(length(.logged), 1L, "exactly one log_action call after the retry succeeds")
if (length(.logged) == 1L) {
  .chk(.logged[[1]]$action, "mover_fecha", "logged action is the real success action, not a failure variant")
  .chk_false(grepl("FALLO", .logged[[1]]$description %||% "", fixed = TRUE),
             "logged description has no failure marker")
}
.chk(length(.notifications), 0L, "no error notification shown -- the retry saved it, nothing to warn about")

# =============================================================================
# Part B -- failure persists through the retry: visible error + honest log
# =============================================================================
cat("\n-- Part B: retry also fails -- persistent on-screen error, dishonest-success log avoided --\n")

.calls_save2    <- 0L
.logged2        <- list()
.notifications2 <- list()

save_moves        <- function(df, client_id = NULL) { .calls_save2 <<- .calls_save2 + 1L; stop("S3 unreachable") }
log_action        <- function(...) { .logged2[[length(.logged2) + 1L]] <<- list(...); invisible(NULL) }
showNotification  <- function(...) { .notifications2[[length(.notifications2) + 1L]] <<- list(...); invisible(NULL) }

.save_moves_deferred(
  data.frame(x = 1),
  client_id = "networks",
  session   = fake_session,
  log_info  = list(user = "tester", module = "ledger_AP", action = "restaurar_fecha",
                    description = "1 factura(s) restaurada(s) a su fecha original",
                    target_id = "123", metadata = list(n = 1),
                    client_id = "networks", viewer_home_client_id = "networks")
)

later::run_now()
Sys.sleep(2.2)
later::run_now()

.chk(.calls_save2, 2L, "both the first attempt and the retry ran (and both failed, as simulated)")
.chk(length(.notifications2), 1L, "exactly one on-screen notification after retries are exhausted")
if (length(.notifications2) == 1L) {
  nfy <- .notifications2[[1]]
  .chk(nfy$type, "error", "notification type is 'error' -- must not look like a routine info toast")
  .chk_true(is.null(nfy$duration), "notification duration is NULL -- persists until dismissed, not an easy-to-miss auto-fading toast")
  .chk_true(identical(nfy$session, fake_session),
            "notification is sent through the caller's own session (later::later() runs outside any reactive domain -- getDefaultReactiveDomain() would be NULL there)")
  ui_txt <- paste(unlist(nfy[!names(nfy) %in% c("type","duration","session")]), collapse = " ")
  .chk_true(grepl("NO qued.*guardad|no se pudo guardar", ui_txt, ignore.case = TRUE),
            "notification text tells the user the change was NOT actually saved")
}
.chk(length(.logged2), 1L, "exactly one log_action call -- a failure record, not silence")
if (length(.logged2) == 1L) {
  .chk(.logged2[[1]]$action, "restaurar_fecha_fallo_guardado",
       "logged action is distinctly flagged as a failed save, never the plain success action")
  .chk_true(grepl("^FALLO AL GUARDAR", .logged2[[1]]$description %||% ""),
            "logged description is explicitly flagged as a failure, not a misleading success record")
}

# =============================================================================
# Part C -- every real call site threads a session through
# =============================================================================
cat("\n-- Part C: real call sites forward session, not just this test's fixture --\n")

ledger_txt <- paste(readLines("R/ledger_module.R", warn = FALSE), collapse = "\n")
search_txt <- paste(readLines("R/search_module.R", warn = FALSE), collapse = "\n")
app_txt    <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

.chk_true(grepl(".apply_move <- function(keys, new_date, ledger, shared, session = NULL)", ledger_txt, fixed = TRUE),
          ".apply_move() accepts a session parameter")
.chk_true(grepl(".clear_moves <- function(keys, ledger, shared, session = NULL)", ledger_txt, fixed = TRUE),
          ".clear_moves() accepts a session parameter")
.chk_true(grepl(".apply_move(keys, new_date, ledger, shared, session = session)", ledger_txt, fixed = TRUE),
          "Calendario's do_move handler forwards its own session into .apply_move()")
.chk_true(grepl(".clear_moves(keys, ledger, shared, session = session)", ledger_txt, fixed = TRUE),
          "Calendario's do_restore handler forwards its own session into .clear_moves()")
# Trailing ")" dropped 2026-09-27: this call now also passes rebase=, so the
# argument list runs onto a second line. The invariant under test is that
# `session` reaches .save_moves_deferred() (so a failed save surfaces on
# screen instead of vanishing), not that it is the final argument.
.chk_true(grepl(".save_moves_deferred(updated_moves, client_id = shared$effective_client_id(), session = session",
                ledger_txt, fixed = TRUE),
          "the SAP-row-edit date-move save forwards session too")

.chk_true(grepl("handle_invoice_action <- function(payload, shared, session = NULL)", search_txt, fixed = TRUE),
          "handle_invoice_action() accepts a session parameter")
.chk_true(grepl("handle_search_action <- function(input, shared, session = NULL)", search_txt, fixed = TRUE),
          "handle_search_action() accepts and forwards a session parameter")
.chk_true(grepl("handle_vencidos_action <- function(input, shared, session = NULL)", search_txt, fixed = TRUE),
          "handle_vencidos_action() accepts and forwards a session parameter")
.chk_true(grepl("handle_invoice_action(input$search_action, shared, session = session)", search_txt, fixed = TRUE),
          "handle_search_action() forwards session into handle_invoice_action()")
.chk_true(grepl("handle_invoice_action(input$vencidos_action, shared, session = session)", search_txt, fixed = TRUE),
          "handle_vencidos_action() forwards session into handle_invoice_action()")
.chk(lengths(regmatches(search_txt, gregexpr(".save_moves_deferred(updated, client_id = shared$effective_client_id(), session = session)",
                                              search_txt, fixed = TRUE))),
     2L, "both move/restore branches in handle_invoice_action() forward session into .save_moves_deferred()")

.chk_true(grepl("handle_search_action(input, shared, session)", app_txt, fixed = TRUE),
          "app.R's search_action observer passes its real session")
.chk_true(grepl("handle_vencidos_action(input, shared, session)", app_txt, fixed = TRUE),
          "app.R's vencidos_action observer passes its real session")

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
