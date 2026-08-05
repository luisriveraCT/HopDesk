# =============================================================================
# tests/test_confirm_agenda_resilience.R
# Found live 2026-08-05: Mouse reported that in the Networks client,
# `tesoreria` sent items to Agenda de Hoy and confirmed them against bank
# movements, but (1) those confirm actions never appeared in Actividad for
# `dev`, and (2) the confirmed items still showed up in Agenda de Hoy. His
# own hypothesis: flaky connectivity might silently interrupt an action
# mid-save, and he asked for a safeguard.
#
# Root cause, confirmed by reading both confirm handlers in
# R/pagar_hoy_module.R (AP "Confirmar pagos" ~line 1364-1680, AR "Confirmar
# cobros" ~line 1672-2011) end to end -- both share one bug, byte-for-byte
# identical in shape:
#   1. Write to bancos_confirmados -- wrapped in its own tryCatch (a failure
#      here shows a warning and does not crash the observer).
#   2. Remove the item from Agenda (safe_load_pagar_hoy -> unstage_pagar_hoy
#      -> shared$pagar_hoy_db(ph); save_pagar_hoy(ph, ...)) -- this call had
#      NO error handling at all.
#   3. Abono activation, notification, log_action() -- all downstream of (2)
#      in the same unprotected flow.
# If (2) throws (a transient network blip during that exact S3 write is
# precisely what a flaky connection would cause), the observer halts
# uncaught: bancos_confirmados was already durably written (so "confirmed"
# genuinely exists somewhere), shared$pagar_hoy_db(ph) had ALREADY been
# optimistically updated in-memory one line earlier (so the acting user's
# own session shows the item as gone, no visible error), the actual S3 file
# was never updated (so any other session/fresh reload sees it still
# pending -- "dev sees it still in Agenda"), and log_action() never runs at
# all ("no Actividad entry"). One mechanism, both symptoms.
#
# Fix: save_pagar_hoy_with_retry() (R/persistence.R) gives step 2 a bounded,
# synchronous retry (not a later::later()-deferred one like log_action()'s
# own flush queue -- this call's caller needs a definitive yes/no before
# deciding whether to log success, activate abonos, or show an error, which
# a fire-and-forget retry can't provide). On total failure, the handler
# stops before abono activation, shows an explicit error, and -- new --
# logs the failure itself via log_action(), so the inconsistency is visible
# in Actividad instead of silent. The abono-activation/manual_inv-archiving
# block (which already individually tryCatch()es each of ITS OWN save_*()
# calls) is now wrapped in one outer tryCatch too, so an unrelated hiccup
# there can never also suppress the log entry for an otherwise-fully-
# succeeded confirm.
# =============================================================================
cat("=== test_confirm_agenda_resilience ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

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

# ── 1. Functional: the real save_pagar_hoy_with_retry(), synthetic failures ─
{
  .extract_fn("R/persistence.R", "save_pagar_hoy_with_retry")

  # Recovers from 2 transient failures, then succeeds on the 3rd attempt.
  attempt_count <- 0L
  assign("save_pagar_hoy", function(df, username = NULL, client_id = NULL) {
    attempt_count <<- attempt_count + 1L
    if (attempt_count < 3L) stop("simulated transient network error")
    invisible(TRUE)
  }, envir = globalenv())
  res <- save_pagar_hoy_with_retry(data.frame(x = 1), "u", "networks",
                                   max_attempts = 3, retry_delay = 0.05)
  ok("recovers after 2 transient failures (3rd attempt succeeds)", isTRUE(res$ok))
  ok("reports the actual number of attempts taken (3)", identical(res$attempts, 3L))
  ok("error is NULL once it eventually succeeds", is.null(res$error))

  # Total failure: exhausts max_attempts, reports ok=FALSE with the real error.
  attempt_count <- 0L
  assign("save_pagar_hoy", function(df, username = NULL, client_id = NULL)
    stop("persistent network error"), envir = globalenv())
  res2 <- save_pagar_hoy_with_retry(data.frame(x = 1), "u", "networks",
                                    max_attempts = 3, retry_delay = 0.05)
  ok("total failure after exhausting max_attempts is reported as ok=FALSE (not silently swallowed)",
     !isTRUE(res2$ok))
  ok("total failure still reports the attempt count actually taken", res2$attempts == 3)
  ok("total failure carries the real underlying error message, not a generic one",
     !is.null(res2$error) && grepl("persistent network error", conditionMessage(res2$error)))

  # Happy path: succeeds on the first attempt, no unnecessary retry/delay.
  assign("save_pagar_hoy", function(df, username = NULL, client_id = NULL)
    invisible(TRUE), envir = globalenv())
  res3 <- save_pagar_hoy_with_retry(data.frame(x = 1), "u", "networks")
  ok("happy path succeeds on the first attempt", isTRUE(res3$ok) && identical(res3$attempts, 1L))

  # Custom max_attempts is honored (not hardcoded to 3 internally).
  attempt_count <- 0L
  assign("save_pagar_hoy", function(df, username = NULL, client_id = NULL) {
    attempt_count <<- attempt_count + 1L
    stop("always fails")
  }, envir = globalenv())
  res4 <- save_pagar_hoy_with_retry(data.frame(x = 1), "u", "networks",
                                    max_attempts = 5, retry_delay = 0.01)
  ok("a custom max_attempts value is actually honored (5, not the default 3)",
     res4$attempts == 5 && attempt_count == 5L)
}

# ── 2. Static: both confirm handlers now use the retry wrapper ─────────────
{
  txt <- readLines("R/pagar_hoy_module.R", warn = FALSE)

  ap_start <- grep('paste0\\("do_confirm_ap_", emp\\)', txt)
  ok("found the AP confirm handler (do_confirm_ap_<emp>) to scan", length(ap_start) > 0)
  ar_start <- grep('paste0\\("do_confirm_ar_", emp\\)', txt)
  ok("found the AR confirm handler (do_confirm_ar_<emp>) to scan", length(ar_start) > 0)

  if (length(ap_start) && length(ar_start)) {
    ap_block <- paste(txt[ap_start[1]:(ar_start[1] - 1)], collapse = "\n")
    ar_end   <- grep("Interco map button", txt)
    ar_end   <- ar_end[ar_end > ar_start[1]][1]
    ar_block <- paste(txt[ar_start[1]:(ar_end %||% (ar_start[1] + 260))], collapse = "\n")

    for (blk in list(list(name = "AP", code = ap_block, action = "confirmar_pago"),
                     list(name = "AR", code = ar_block, action = "confirmar_cobro"))) {
      ok(sprintf("%s handler calls save_pagar_hoy_with_retry() (the fix)", blk$name),
         grepl("save_pagar_hoy_with_retry\\(", blk$code))
      ok(sprintf("%s handler no longer calls the bare, unguarded save_pagar_hoy(ph, ...) directly", blk$name),
         !grepl("shared\\$pagar_hoy_db\\(ph\\);\\s*save_pagar_hoy\\(ph,", blk$code))
      ok(sprintf("%s handler checks save_result$ok before proceeding", blk$name),
         grepl("isTRUE\\(save_result\\$ok\\)", blk$code))
      ok(sprintf("%s handler shows a distinct, clear error notification on total failure", blk$name),
         grepl("posible problema de conexión", blk$code))
      ok(sprintf("%s handler logs the failure itself via log_action() on total failure (the safeguard)", blk$name),
         grepl(sprintf('action = "%s_incompleto"', blk$action), blk$code))
      ok(sprintf("%s handler stops (return()) after a total failure -- does not fall through to abono activation or a false success log", blk$name),
         grepl("return\\(\\)\\s*\\n\\s*\\}", blk$code))
      ok(sprintf("%s handler still logs the normal success action (%s) on the happy path (regression guard)", blk$name, blk$action),
         grepl(sprintf('"pagar_hoy", "%s"', blk$action), blk$code))
      ok(sprintf("%s handler wraps abono/manual_inv secondary processing in an outer tryCatch (the decoupling fix)", blk$name),
         grepl("secondary_err_(ap|ar) <- tryCatch\\(\\{", blk$code))
      ok(sprintf("%s handler surfaces a secondary-processing failure without blocking the primary success log", blk$name),
         grepl("algo falló al procesar", blk$code))
    }

    # Regression guard: the bancos_confirmados write's OWN pre-existing
    # tryCatch (the part that was already correct) is untouched by this fix.
    ok("AP handler's bancos_confirmados write still has its own pre-existing tryCatch (unaffected regression guard)",
       grepl('save_bancos_confirmados\\(conf_db, client_id = shared\\$effective_client_id\\(\\)\\)\\s*\\n\\s*bump_sync_version\\("bancos_confirmados_db"\\)', ap_block))
  }
}

# ── 3. Static: save_pagar_hoy_with_retry() is documented in place ──────────
{
  txt <- readLines("R/persistence.R", warn = FALSE)
  ok("save_pagar_hoy_with_retry() is defined", any(grepl("^save_pagar_hoy_with_retry <- function", txt)))
  ok("the incident is documented in place (grep-able tone matching this codebase's own convention)",
     any(grepl("found 2026-08-05", txt)))
  ok("the function explicitly explains why it retries synchronously, not via later::later() (design rationale, not just what)",
     any(grepl("later::later\\(\\)-deferred", txt)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
