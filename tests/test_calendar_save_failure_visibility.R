# =============================================================================
# tests/test_calendar_save_failure_visibility.R
#
# CRITICAL fix (2026-09-03): closes the same silent-write-failure/dishonest-
# log bug class already fixed for .save_moves_deferred() (see
# test_moves_save_failure_visibility.R) in the Calendario/Search modal's
# sibling actions -- tagging an invoice, and deleting an invoice to papelera
# (both SAP-sourced and manual/provision-sourced), in both R/ledger_module.R
# (Calendario) and R/search_module.R (Vencidos/Search modal, a separate
# implementation of the same actions). Before this fix, each save's error
# handler only showed a transient warning toast (or, for the tag save in
# ledger_module.R, had no error handling at all -- an uncaught throw would
# crash the observer) without gating the success notification or the
# Activity-log write that followed -- a failed save still showed "saved" on
# screen and logged it as a fact.
#
# Fix: a new generic s3_save_with_retry() (R/persistence.R), the same
# small-bounded-synchronous-retry shape already proven correct by
# save_pagar_hoy_with_retry() in this codebase (see that function's own
# comment for the retry-vs-defer rationale) -- used because these actions,
# unlike a calendar drag, aren't latency-sensitive enough to need the
# later::later()-deferred shape .save_moves_deferred() uses.
#
# Part A tests s3_save_with_retry() itself behaviorally (extracted from the
# real source file, never hand-reimplemented). Parts B-F are static
# assertions on the real call sites, the same "call-site scoping (static)"
# style already used elsewhere in this suite (e.g.
# test_saas_log_action_scoping.R) -- these five handlers are observeEvent
# bodies deeply nested inside a moduleServer()/module function and cannot be
# meaningfully extracted and eval'd in isolation the way a top-level helper
# like s3_save_with_retry() itself can.
# =============================================================================

cat("── Calendar/Search save-failure visibility (critical, 2026-09-03) ────────\n")

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
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)

# =============================================================================
# Part A -- s3_save_with_retry() itself, extracted and eval'd for real
# =============================================================================
cat("\n-- Part A: s3_save_with_retry() behavior --\n")

persist_src <- readLines("R/persistence.R", warn = FALSE)
fn_start <- grep("^s3_save_with_retry <- function", persist_src)
.chk_true(length(fn_start) == 1, "located exactly one s3_save_with_retry() definition in R/persistence.R")
# Function body is a short, self-contained brace block -- find its closing brace.
depth <- 0L; fn_end <- NA_integer_
for (i in fn_start:length(persist_src)) {
  depth <- depth + lengths(regmatches(persist_src[i], gregexpr("\\{", persist_src[i]))) -
                    lengths(regmatches(persist_src[i], gregexpr("\\}", persist_src[i])))
  if (depth == 0L && i > fn_start) { fn_end <- i; break }
}
.chk_true(!is.na(fn_end), "located the end of s3_save_with_retry()'s definition")
eval(parse(text = paste(persist_src[fn_start:fn_end], collapse = "\n")), envir = environment())
.chk_true(exists("s3_save_with_retry", mode = "function"),
          "s3_save_with_retry() extracted and evaluated successfully from the real source file")

calls <- 0L
r1 <- s3_save_with_retry(function() { calls <<- calls + 1L; if (calls == 1L) stop("transient") },
                          max_attempts = 3, retry_delay = 0.01)
.chk_true(isTRUE(r1$ok), "succeeds on the 2nd attempt after one simulated transient failure")
.chk(r1$attempts, 2L, "reports the correct attempt count on eventual success")

calls2 <- 0L
r2 <- s3_save_with_retry(function() { calls2 <<- calls2 + 1L; stop("permanent") },
                          max_attempts = 3, retry_delay = 0.01)
.chk_true(!isTRUE(r2$ok), "reports failure when every attempt fails")
.chk(calls2, 3L, "made exactly max_attempts calls, no more")
.chk_true(grepl("permanent", r2$error$message), "the returned error is the real underlying error, not a generic message")

# =============================================================================
# Parts B-F -- static call-site assertions on the real, unmodified source
# =============================================================================
ledger_txt <- paste(readLines("R/ledger_module.R", warn = FALSE), collapse = "\n")
search_txt <- paste(readLines("R/search_module.R", warn = FALSE), collapse = "\n")

cat("\n-- Part B: Calendario delete (SAP-sourced invoices) --\n")
.chk_true(grepl("save_papelera(sap_pap, client_id = shared$effective_client_id()))",
                ledger_txt, fixed = TRUE) &&
          grepl("s3_save_with_retry(function()\n            save_papelera(sap_pap",
                ledger_txt, fixed = TRUE),
          "the SAP-invoice papelera save is wrapped in s3_save_with_retry()")
.chk_true(grepl('"eliminar_facturas_sap_fallo_guardado"', ledger_txt, fixed = TRUE),
          "a distinctly-flagged failure action exists for the SAP-invoice delete")

cat("\n-- Part C: Calendario delete (manual/provision-sourced invoices) --\n")
.chk_true(grepl("save_failures <- character(0)", ledger_txt, fixed = TRUE),
          "the manual/provision delete handler tracks per-piece save failures")
.chk_true(grepl('"eliminar_facturas_fallo_guardado"', ledger_txt, fixed = TRUE),
          "a distinctly-flagged failure action exists for the manual/provision delete")

cat("\n-- Part D: Calendario tag --\n")
.chk_true(grepl("save_tags(tags_db, client_id = shared$effective_client_id()))",
                ledger_txt, fixed = TRUE),
          "the Calendario tag save is wrapped in s3_save_with_retry()")
.chk_true(grepl('"etiquetar_fallo_guardado"', ledger_txt, fixed = TRUE),
          "a distinctly-flagged failure action exists for the Calendario tag save")

cat("\n-- Part E: Vencidos/Search modal tag --\n")
.chk_true(grepl("save_tags(tdb, client_id = shared$effective_client_id()))",
                search_txt, fixed = TRUE),
          "the Vencidos/Search tag save is wrapped in s3_save_with_retry()")
.chk_true(grepl('if (isTRUE(tag_result$ok)) "etiquetar" else "etiquetar_fallo_guardado"',
                search_txt, fixed = TRUE),
          "the Vencidos/Search tag log action name is gated on the real save outcome")

cat("\n-- Part F: Vencidos/Search modal delete --\n")
.chk_true(grepl("item_save_failures <- character(0)", search_txt, fixed = TRUE),
          "the Vencidos/Search delete handler tracks per-piece save failures")
.chk_true(grepl('"eliminar_facturas" else "eliminar_facturas_fallo_guardado"',
                search_txt, fixed = TRUE),
          "the Vencidos/Search delete log action name is gated on the real save outcome")

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
