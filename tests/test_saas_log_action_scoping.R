# =============================================================================
# tests/test_saas_log_action_scoping.R
# Stage 4 (extended, 2026-07-16): static regression guard against the bug
# class found while manually testing — a log_action() call missing
# client_id= silently defaults to Sys.getenv("CLIENT_ID"), which is always
# "hd-admin" in this shared deployment, misfiling the entry into Hopdesk's
# own folder instead of the acting client's. Missing viewer_home_client_id=
# similarly breaks the dual-write's "is this actor outside their own home"
# check (see R/app_audit.R's log_action() doc comment).
#
# This scans the actual production source files (not this test file) for
# every log_action( call site and asserts both params are present in its
# argument list — cheap, durable, and catches ANY future call site that
# forgets either one, not just the ones fixed today.
# =============================================================================

cat("── log_action() call-site scoping (static) ────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

.PRODUCTION_FILES <- c(
  "app.R", "R/bancos_module.R", "R/pagar_hoy_module.R",
  "R/tiers_module.R", "R/ledger_module.R"
)

for (f in .PRODUCTION_FILES) {
  txt <- readLines(f, warn = FALSE)
  starts <- grep("log_action\\(", txt)
  for (s in starts) {
    # A log_action( call's argument list is at most ~15 lines in this codebase;
    # generous enough window without needing a real R parser.
    block <- paste(txt[s:min(s + 15, length(txt))], collapse = "\n")
    label_loc <- sprintf("%s:%d", f, s)
    .chk(grepl("client_id\\s*=", block), TRUE,
         sprintf("%s: log_action() call passes client_id=", label_loc))
    .chk(grepl("viewer_home_client_id\\s*=", block), TRUE,
         sprintf("%s: log_action() call passes viewer_home_client_id=", label_loc))
  }
}

cat("\n")
