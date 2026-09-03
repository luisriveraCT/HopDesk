# =============================================================================
# tests/test_refresh_variable_rates_client_scoping.R
#
# Fix (2026-09-03, found while investigating an unrelated production-session
# question -- unrelated to the SAP/ERP fetch mechanism, which is correct and
# untouched): app.R's .refresh_variable_rates() (fetches SOFR/TIIE28 and
# writes them via forecasting_set_estimate()) never passed client_id, so it
# silently defaulted to Sys.getenv("CLIENT_ID") ("hd-admin", the shared
# deployment's own env-level default) instead of the session's real
# effective_client_id() -- every session's rate refresh wrote into hd-admin's
# own pasivos_estimates.rds/pasivos_audit.rds regardless of which real client
# was using the app. Low severity (SOFR/TIIE28 are public market rates, not
# client financial data) but a real cross-tenant scoping bug, same anti-
# pattern class as the log_action()/save_manual() CLIENT_ID-as-stand-in bugs
# already fixed elsewhere in this codebase (see their own "static" tests,
# same style used here).
#
# Static text assertions on the real source file, same style as this
# codebase's other call-site-scoping tests (log_action(), save_manual()) --
# .refresh_variable_rates() is a closure nested inside app.R's giant server()
# function and cannot be meaningfully extracted/eval'd in isolation the way
# a top-level helper can.
# =============================================================================

cat("── .refresh_variable_rates() client_id scoping (static) ──────────────────\n")

.pass <- 0L
.fail <- 0L
.chk_true <- function(cond, label) {
  if (isTRUE(cond)) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s\n", label)); .fail <<- .fail + 1L }
}

app_src   <- readLines("app.R", warn = FALSE)
fn_start  <- grep("^\\s*\\.refresh_variable_rates <- function", app_src)
fn_end    <- grep("^\\s*session\\$onFlushed\\(function\\(\\) \\.refresh_variable_rates\\(\\)", app_src)
.chk_true(length(fn_start) == 1 && length(fn_end) == 1 && fn_start < fn_end,
          "located exactly one .refresh_variable_rates() definition, ending before its onFlushed registration")
fn_txt <- paste(app_src[fn_start[1]:fn_end[1]], collapse = "\n")

.chk_true(grepl("effective_client_id()", fn_txt, fixed = TRUE),
          ".refresh_variable_rates() reads the session's real effective_client_id()")
.chk_true(grepl("forecasting_set_estimate(job$metric, Sys.Date(), rate,\n                                 source_method = \"auto_refresh\", client_id = cid)",
                fn_txt, fixed = TRUE),
          "forecasting_set_estimate() is called with client_id = cid, not left to default to Sys.getenv(\"CLIENT_ID\")")

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
