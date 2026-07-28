# =============================================================================
# tests/test_saas_manual_entry_client_scoping.R
# Regression guard for a real, confirmed cross-tenant bug found 2026-07-23:
# app.R's me_save observer (the "Guardar" button on the new/edit manual
# invoice modal) called save_manual(df) in BOTH its insert and edit
# branches WITHOUT client_id -- which falls back to Sys.getenv("CLIENT_ID"),
# always "hd-admin" on the shared deployment, regardless of which client's
# session is actually saving. A real save from a "networks" session was
# confirmed (via a read-only S3 check) to have been written to
# hd-admin/manual_invoices.rds instead of networks/manual_invoices.rds --
# invisible to the saving user's own session on next reload, and a real
# cross-tenant data leak on the live shared deployment.
#
# This scans the actual production source file (not this test file) for
# every save_manual( call site and asserts client_id is present in its
# argument list -- same style as test_saas_log_action_scoping.R's guard
# against the analogous log_action() bug.
# =============================================================================

cat("── save_manual() call-site client_id scoping (static) ──────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

.PRODUCTION_FILES <- c("app.R", "R/pagar_hoy_module.R", "R/ledger_module.R",
                      "R/search_module.R", "R/staging_browse_module.R",
                      "R/manual_entry_handlers.R")

found_any <- FALSE
for (f in .PRODUCTION_FILES) {
  if (!file.exists(f)) next
  txt <- readLines(f, warn = FALSE)
  starts <- grep("save_manual\\(", txt)
  for (s in starts) {
    found_any <- TRUE
    # A save_manual( call's argument list is at most ~5 lines in this
    # codebase -- generous window without needing a real R parser.
    block <- paste(txt[s:min(s + 5, length(txt))], collapse = "\n")
    label_loc <- sprintf("%s:%d", f, s)
    .chk(grepl("client_id\\s*=", block), TRUE,
         sprintf("%s: save_manual() call passes client_id=", label_loc))
  }
}
.chk(found_any, TRUE, "at least one save_manual() call site was found to scan (guard against this test silently checking nothing)")

cat("\n")
