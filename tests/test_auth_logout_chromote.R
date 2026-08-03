# =============================================================================
# tests/test_auth_logout_chromote.R
# Stage 9, Issue A (2026-08-03): "Salir doesn't log out; a copied url logs you
# in forever" -- both symptoms traced to one root cause: app.R's btn_logout
# handler cleared cookies and called location.reload(), but shinymanager
# validates purely off the url's ?token= query string (confirmed by reading
# the installed shinymanager source -- getToken()/secure_app()'s request
# filter never look at a cookie). Reloading the same url with the same
# still-valid token just logged the user back in.
#
# Fix: btn_logout now fires Shiny.setInputValue('.shinymanager_logout', ...),
# which is shinymanager's OWN internal, previously-unwired revocation path --
# .tok$remove(token) (deletes the token from its process-wide valid-token
# registry, so it's dead everywhere, not just in the tab that logged out),
# then clearQueryString() + session$reload().
#
# This is a REAL, live-browser (chromote/headless-Chrome) test, not just
# static analysis -- run against a small isolated shinymanager fixture app
# (see tests/helpers/live_app_harness.R's header for why: app.R's own
# .Renviron points at live production S3, so the full app is never launched
# for automated testing). The fixture's logout observer body is EXTRACTED
# from the real, current app.R (not hand-copied) so this test breaks the
# moment the real fix drifts from what's tested here.
#
# Requires a local Chrome/Chromium (chromote::find_chrome()) and network
# loopback. Skips gracefully (not a hard failure) if neither is available,
# so this file still runs in restricted CI environments -- but a skip is
# reported loudly, not silently, since a live UI fix with no live UI test is
# exactly the gap this stage's convention exists to close.
# =============================================================================
cat("=== test_auth_logout_chromote ===\n\n")

.pass <- 0L; .fail <- 0L; .skip <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) {
    cat(" PASS:", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  }
}

.deps_ok <- all(vapply(
  c("chromote", "callr", "curl", "jsonlite", "shiny", "shinymanager", "shinyjs"),
  requireNamespace, logical(1), quietly = TRUE
))
.chrome_ok <- .deps_ok && !inherits(tryCatch(chromote::find_chrome(), error = function(e) e), "error")

if (!.chrome_ok) {
  cat(" SKIP: chromote/Chrome not available in this environment -- skipping live browser test.\n")
  cat(" (This is a skip, not a pass -- Issue A's live-browser verification did not run here.)\n")
  .skip <- 1L
  cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
  invisible(NULL)
} else {

source("tests/helpers/live_app_harness.R")

# ── Extract the REAL logout observer from app.R (not a hand-copied mirror) ──
app_txt <- readLines("app.R", warn = FALSE)
logout_start <- grep("Logout button", app_txt)
ok("found the '# ── Logout button' marker comment in app.R", length(logout_start) > 0)
block_start <- grep("observeEvent\\(input\\$btn_logout,", app_txt)
block_start <- block_start[block_start > logout_start[1]][1]
ok("found observeEvent(input$btn_logout, ...) after the marker", !is.na(block_start))
block_end <- grep("^\\s*\\}, ignoreInit = TRUE\\)\\s*$", app_txt)
block_end <- block_end[block_end >= block_start][1]
ok("found the closing '}, ignoreInit = TRUE)' of the logout observer", !is.na(block_end))
logout_block <- paste(app_txt[block_start:block_end], collapse = "\n")

ok("extracted logout block fires the .shinymanager_logout input (the actual fix under test)",
   grepl("\\.shinymanager_logout", logout_block))
ok("extracted logout block no longer clears cookies (dead code removed)",
   !grepl("document\\.cookie", logout_block))

# ── Build the isolated fixture app, embedding the real block verbatim ──────
app_code <- sprintf('
library(shiny)
library(shinymanager)
library(shinyjs)

creds <- data.frame(
  user = c("alice", "bob"),
  password = c("pw-alice", "pw-bob"),
  stringsAsFactors = FALSE
)

ui <- shinymanager::secure_app(
  fluidPage(
    shinyjs::useShinyjs(),
    actionButton("btn_logout", "Salir"),
    textOutput("who")
  ),
  enable_admin = FALSE
)

server <- function(input, output, session) {
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(creds),
    keep_token = TRUE
  )
  output$who <- renderText({
    req(res_auth$user)
    paste("Logged in as:", res_auth$user)
  })

%s
}

shinyApp(ui, server)
', logout_block)

PORT <- 38917L
app <- NULL
s1 <- NULL
s2 <- NULL
on.exit({
  try(if (!is.null(s1)) s1$close(), silent = TRUE)
  try(if (!is.null(s2)) s2$close(), silent = TRUE)
  stop_bg_app(app)
}, add = TRUE)

app <- tryCatch(start_bg_app(app_code, PORT), error = function(e) { message("startup error: ", e$message); NULL })
ok("fixture auth app started and became reachable", !is.null(app))

if (!is.null(app)) {
  base_url <- sprintf("http://127.0.0.1:%d/", PORT)

  login_as <- function(session, user, password) {
    session$Page$navigate(base_url, wait_ = TRUE)
    poll_until(function() grepl("Please authenticate|Username", body_text(session)))
    fill_input(session, "#auth-user_id", user)
    fill_input(session, "#auth-user_pwd", password)
    click_selector(session, "#auth-go_auth")
    poll_until(function() grepl(paste0("Logged in as: ", user), body_text(session)), timeout_s = 10)
  }

  s1 <- new_chromote_session()

  # ── (a) fresh session requires login, (b) login succeeds ─────────────────
  ok("session 1: shows the login form before authenticating", {
    s1$Page$navigate(base_url, wait_ = TRUE)
    poll_until(function() grepl("Please authenticate", body_text(s1)))
  })
  ok("session 1: logging in as alice succeeds", login_as(s1, "alice", "pw-alice"))

  # ── (c) copy the authenticated url (this is the token-in-url trade-off,
  # deliberately unchanged -- Stage 0's accepted risk, not this stage's bug) ─
  copied_url <- current_url(s1)
  ok("copied url contains a token query param (keep_token's documented trade-off, unchanged)",
     grepl("token=", copied_url))

  # ── (d) refresh survives (Stage 0 non-regression: refresh must NOT force
  # a re-login just because the fix touched nearby auth code) ───────────────
  ok("session 1: a plain refresh does NOT force a re-login (Stage 0 behavior preserved)", {
    s1$Page$reload(wait_ = TRUE)
    poll_until(function() grepl("Logged in as: alice", body_text(s1)))
  })

  # ── (e) click Salir -> the SAME tab must land back on the login screen ───
  ok("session 1: clicking Salir navigates back to the login form", {
    click_selector(s1, "#btn_logout")
    poll_until(function() grepl("Please authenticate", body_text(s1)), timeout_s = 10)
  })
  ok("session 1: after Salir, the url no longer carries the old token (clearQueryString ran)",
     !grepl(regmatches(copied_url, regexpr("token=[^&]+", copied_url)), current_url(s1), fixed = TRUE))

  # ── (f) the COPIED url, opened in a brand-new session/context, must NOT
  # auto-authenticate any more -- this is the actual security fix: the token
  # was revoked from shinymanager's process-wide store, not just hidden from
  # this one tab. ────────────────────────────────────────────────────────────
  s2 <- new_chromote_session()
  ok("session 2 (fresh context): navigating to the OLD copied url shows the login form, not auto-login", {
    s2$Page$navigate(copied_url, wait_ = TRUE)
    poll_until(function() grepl("Please authenticate", body_text(s2)), timeout_s = 10)
  })
  ok("session 2: confirms it did NOT land on the logged-in view via the dead token",
     !grepl("Logged in as: alice", body_text(s2)))

  # ── Control: a second, brand-new login (bob) still works after all of the
  # above -- proves the fix didn't break auth generally, only revoked the one
  # specific dead token. ─────────────────────────────────────────────────────
  ok("session 2: bob can still log in fresh after alice's token was revoked",
     login_as(s2, "bob", "pw-bob"))
  ok("session 2: bob's own session is unaffected by alice's logout (no cross-session bleed)",
     grepl("Logged in as: bob", body_text(s2)))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
}
