# =============================================================================
# tests/helpers/live_app_harness.R
# Stage 9 (2026-08-03): reusable infrastructure for chromote-driven live tests.
#
# Found while starting Stage 9: every prior "chromote" test in this repo (see
# test_auth_keep_token.R, test_live_refresh_observers.R, test_cart_inv_sel.R
# headers) only ever *narrates* a manual chromote session someone ran outside
# the repo -- none of them is runnable code. That's fine for a stage that
# already got its hands-on verification some other way, but Stage 9 needs
# actual live-browser proof for Issues A, B, and E (auth logout, modal
# reopening, cross-session isolation), so this file provides the first real,
# committed, reusable harness: launch a small standalone Shiny app in a
# background process and drive it with a real headless Chrome (chromote).
#
# Deliberately never launches the FULL production app.R -- app.R's own
# .Renviron points at live production S3/SAP credentials (antiguedad-rds-prod,
# real AWS keys). A concurrent-session or repeated-reload test against that
# would read AND WRITE real client data. Every Stage 9 chromote test instead
# builds a minimal, isolated fixture app that reproduces only the exact
# mechanism under test (mirrors the convention already described, in prose
# only, in test_auth_keep_token.R's header: "isolated shinymanager test apps,
# not the full production app, to avoid needing live SAP/S3 credentials").
# =============================================================================

# Launches `app_code` (a string of R source defining `ui`/`server`/`shinyApp(...)`)
# as a background Rscript process listening on `port`. Returns a list with
# $proc (the processx handle from callr) and $port. Call stop_bg_app() on it
# when done, always (even on test failure) -- an orphaned background Shiny
# process holds its port and breaks the next test run.
start_bg_app <- function(app_code, port, timeout_s = 20) {
  tmp <- tempfile(fileext = ".R")
  writeLines(app_code, tmp)

  proc <- callr::r_bg(
    function(app_file, port) {
      shiny::runApp(app_file, port = port, host = "127.0.0.1", launch.browser = FALSE)
    },
    args = list(app_file = tmp, port = port),
    stdout = tempfile(), stderr = tempfile()
  )

  ok <- FALSE
  deadline <- Sys.time() + timeout_s
  while (Sys.time() < deadline) {
    if (!proc$is_alive()) {
      stop("background app process died during startup. stderr:\n",
           paste(readLines(proc$get_error_file(), warn = FALSE), collapse = "\n"))
    }
    resp <- tryCatch(
      curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", port), handle = curl::new_handle(TIMEOUT = 2)),
      error = function(e) NULL
    )
    if (!is.null(resp) && resp$status_code < 500) { ok <- TRUE; break }
    Sys.sleep(0.3)
  }
  if (!ok) {
    proc$kill()
    stop("background app on port ", port, " never became reachable within ", timeout_s, "s")
  }
  list(proc = proc, port = port, app_file = tmp)
}

stop_bg_app <- function(app) {
  if (is.null(app)) return(invisible())
  try({ if (app$proc$is_alive()) app$proc$kill() }, silent = TRUE)
  try(unlink(app$app_file), silent = TRUE)
  invisible()
}

# Opens a fresh chromote session (== a fresh browser tab/context, no cookies
# or state shared with any other session created this way -- each is its own
# ChromoteSession, which is what lets Issue A's test prove a copied url in "a
# fresh session/context" does not auto-authenticate, and Issue E2's test
# prove two concurrent users don't bleed into each other).
new_chromote_session <- function() {
  b <- chromote::ChromoteSession$new()
  b$Page$enable()
  b
}

# Polls `expr` (a function of no args, evaluated in the caller's frame via
# force()) until it returns TRUE or `timeout_s` elapses. Shiny's reactive
# flush + chromote's own round-trip means UI state doesn't update
# instantaneously after an input event; tests must poll, not assume the DOM
# is already settled the instant a click/navigate call returns.
poll_until <- function(expr_fn, timeout_s = 10, interval_s = 0.25) {
  deadline <- Sys.time() + timeout_s
  repeat {
    if (isTRUE(expr_fn())) return(TRUE)
    if (Sys.time() >= deadline) return(FALSE)
    Sys.sleep(interval_s)
  }
}

# Reads the CURRENT browser-visible url (post any client-side
# history.replaceState/pushState shinymanager does on logout/login --
# session$clientData$url_* is a SERVER-side snapshot from page load and does
# NOT reflect those client-side rewrites, so tests must ask the browser
# directly via chromote, not trust server-side state).
current_url <- function(session) {
  session$Runtime$evaluate("window.location.href")$result$value
}

body_text <- function(session) {
  res <- session$Runtime$evaluate("document.body.innerText")
  res$result$value
}

set_input_value <- function(session, input_id, value) {
  session$Runtime$evaluate(sprintf(
    "Shiny.setInputValue('%s', %s, {priority: 'event'});",
    input_id, jsonlite::toJSON(value, auto_unbox = TRUE)
  ))
}

click_selector <- function(session, css_selector) {
  session$Runtime$evaluate(sprintf(
    "document.querySelector('%s').click();", css_selector
  ))
}

fill_input <- function(session, css_selector, text) {
  session$Runtime$evaluate(sprintf("
    (function(){
      var el = document.querySelector('%s');
      el.value = %s;
      el.dispatchEvent(new Event('input', {bubbles: true}));
      el.dispatchEvent(new Event('change', {bubbles: true}));
    })();
  ", css_selector, jsonlite::toJSON(text, auto_unbox = TRUE)))
}
