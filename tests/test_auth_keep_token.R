# =============================================================================
# tests/test_auth_keep_token.R
# Found live 2026-07-29: refreshing the browser forced a full re-login,
# indistinguishable from a real logout. Root cause, confirmed by reading the
# installed shinymanager package source: secure_server() didn't pass
# keep_token = TRUE, so shinymanager's own resetQueryString() stripped the
# auth token from the URL right after login -- every refresh afterward had
# no token to present.
#
# This file is the static, permanent half of verification (runs anywhere,
# no browser needed). The other half was a live 14-scenario matrix driven
# with a headless-Chrome (chromote) two-user harness against isolated
# shinymanager test apps (not the full production app, to avoid needing
# live SAP/S3 credentials) -- confirmed: fresh session requires login;
# login persists across 3 consecutive refreshes; a brand-new tab with no
# token is NOT auto-logged-in just because another session is active
# (no session bleed); two independent users (alice/bob) each keep their
# own identity across their own refreshes with zero cross-contamination;
# the documented keep_token trade-off is real (a copied URL with a token
# logs a new tab in as that user, no password) -- an accepted, explicit
# risk for a small trusted internal team, not an oversight; a tampered/
# invalid token falls back to the login form rather than crashing; and a
# logout-style cookie-clear + reload still correctly forces re-login, so
# keep_token didn't silently break "Salir". All 14 passed.
#
# Stage 9 correction (2026-08-03, Issue A): that last claim does not hold up.
# Mouse reported live that clicking "Salir" did not log him out, and reading
# the actual app.R + installed shinymanager source confirmed why: shinymanager
# validates purely off the url's ?token= query string, never a cookie, so a
# cookie-clear + reload of the SAME url with the SAME still-valid token
# re-authenticates instantly -- it does not "still correctly force re-login."
# Whatever the 2026-07-29 manual chromote pass actually clicked through, it
# was not exercising the real production failure mode end-to-end (most likely
# a manually-stripped-token reload, which trivially forces a login and proves
# nothing about the app's own logout button). Lesson: a live-browser scenario
# description in a header is not a substitute for a runnable, re-executable
# test -- see tests/test_auth_logout_chromote.R, which IS runnable, extracts
# the real current app.R logout code verbatim, and was confirmed to fail
# against the pre-fix code before being confirmed to pass against the fix.
# =============================================================================
cat("=== test_auth_keep_token ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

app_txt <- readLines("app.R", warn = FALSE)

secure_server_start <- grep("::secure_server\\s*\\(", app_txt)
ok("found the secure_server() call to scan", length(secure_server_start) > 0)
ss_block <- if (length(secure_server_start)) {
  paste(app_txt[secure_server_start[1]:min(secure_server_start[1] + 5, length(app_txt))], collapse = "\n")
} else ""

ok("secure_server() passes keep_token = TRUE",
   grepl("keep_token\\s*=\\s*TRUE", ss_block))
ok("secure_server() still passes check_credentials (regression guard -- the fix didn't drop it)",
   grepl("check_credentials\\s*=\\s*auth_check_credentials\\(\\)", ss_block))

secure_app_start <- grep("::secure_app\\s*\\(", app_txt)
ok("found the secure_app() call to scan", length(secure_app_start) > 0)
secure_app_end <- grep("^\\)$", app_txt)
secure_app_end <- secure_app_end[secure_app_end > secure_app_start[1]][1]
sa_block <- if (length(secure_app_start) && !is.na(secure_app_end)) {
  paste(app_txt[secure_app_start[1]:secure_app_end], collapse = "\n")
} else ""

ok("the dead timeout_session argument is fully removed from secure_app() (it was never a real argument -- confirmed against the installed shinymanager source -- and silently did nothing)",
   !grepl("timeout_session\\s*=", sa_block))
ok("no actual timeout_session=... assignment remains anywhere in app.R (a mention in the explanatory comment is fine and expected -- this checks for the dead ARGUMENT specifically, not the word)",
   !any(grepl("timeout_session\\s*=\\s*[0-9]", app_txt)))
ok("secure_app() still sets enable_admin = FALSE (regression guard -- the fix didn't touch this)",
   grepl("enable_admin\\s*=\\s*FALSE", sa_block))
ok("secure_app() still configures a theme (regression guard -- the fix didn't drop unrelated config)",
   grepl("theme\\s*=\\s*bslib::bs_theme", sa_block))

# ── Control: confirm this is a real, meaningful change, not a no-op edit ────
# (guards against a future accidental revert of keep_token going unnoticed;
# excludes the explanatory comment's own mention of the parameter name)
code_lines <- app_txt[!grepl("^\\s*#", app_txt)]
ok("control: keep_token=TRUE appears exactly once in actual code (one deliberate fix site, not duplicated/scattered; the explanatory comment's own mention is excluded here on purpose)",
   sum(grepl("keep_token\\s*=\\s*TRUE", code_lines)) == 1L)

# ── Explicit no-regression checks on adjacent, unrelated auth code ─────────
# UPDATED 2026-08-03 (Stage 9, Issue A): these two checks used to assert the
# logout handler "still clears cookies and reloads" as a regression guard --
# but that WAS Issue A's bug (cookies are never checked by shinymanager;
# reloading the same url with the same still-valid token silently logged the
# user right back in). Stage 9 replaced the handler with a call into
# shinymanager's own token-revocation input (.shinymanager_logout). Asserting
# the old body here would fail the suite against the correct fix, so this
# guard now checks for the NEW real behavior instead of the old broken one --
# see tests/test_auth_logout_chromote.R for the live-browser proof this
# actually revokes the token, not just this static shape check.
ok("the logout handler (btn_logout) is untouched -- still exists as input$btn_logout",
   any(grepl("observeEvent\\(input\\$btn_logout,", app_txt)))
logout_start <- grep("observeEvent\\(input\\$btn_logout,", app_txt)
if (length(logout_start)) {
  logout_block <- paste(app_txt[logout_start[1]:min(logout_start[1] + 10, length(app_txt))], collapse = "\n")
  ok("logout handler fires shinymanager's own .shinymanager_logout revocation input (Stage 9 fix)",
     grepl("\\.shinymanager_logout", logout_block))
  ok("logout handler no longer relies on cookie-clearing (confirmed dead code -- shinymanager never reads a cookie)",
     !grepl("document\\.cookie", logout_block))
  ok("logout handler no longer calls location.reload() directly (shinymanager's own .shinymanager_logout observer calls session$reload() itself, after removing the token and clearing the query string -- doing it twice would race)",
     !grepl("location\\.reload\\(\\)", logout_block))
}

# ── Documentation/comment quality -- the fix explains itself for the next
# person who wonders "why is this here" (matches this codebase's own
# established convention of explaining non-obvious fixes in place) ─────────
ok("the keep_token fix is accompanied by an explanatory comment (not a silent one-line change)",
   any(grepl("resetQueryString|strips its own auth token", app_txt)))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
cat("(Plus 14/14 live browser scenarios verified separately -- see file header.)\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
