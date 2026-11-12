# =============================================================================
# tests/helpers/aislar_global.R
# For any test (or test app) that loads the WHOLE of R/global.R -- through
# shiny::loadSupport(), source("app.R"), runApp()/shinyAppDir() on app.R or
# source()/sys.source() of R/global.R. Source it BEFORE that load. It does two
# things to this process (and the callr/processx children it starts, which
# inherit the environment):
#
#  1. Its own single-instance lock. R/global.R takes
#     %LOCALAPPDATA%\HopDesk\instance_<CLIENT_ID>_<S3_BUCKET>.lock and refuses
#     to start if another process holds it. In a checkout with the dev
#     .Renviron files every test got hd-admin_antiguedad-rds-dev -- the very
#     lock of Luis's local dev app and of every other session's tests -- so
#     tests failed on the lock, not on the code, and a running test kept the
#     dev app from starting (candado, 2026-11-11). LOCALAPPDATA now points at
#     a folder of this process: the guard still runs, on a lock nobody else
#     shares. The real app never loads this file, so its guard is unchanged.
#     T15 (tests/helpers/arranque_app.R) did this first.
#
#  2. No network to AWS. Loading global.R with .Renviron.local copies the user
#     accounts from the prod bucket into dev (.hopdesk_sync_dev_users_from_prod:
#     reads prod, writes dev). The proxy is pointed at a dead local port, so
#     that copy (and anything else) fails to connect -- global.R treats a
#     failed copy as non-fatal. AWS_* are not relied on: global.R reads them
#     back from .Renviron. localhost stays reachable (Chrome, background apps).
#
# aislar_global_sin_red(): TRUE when a request to AWS (through curl) fails to
# CONNECT (the dead proxy), not with an answer from AWS. It asks for a bucket
# that does not exist, without credentials, so it never touches a real bucket
# either way. Call it after global.R loads (global.R runs readRenviron()).
# =============================================================================
local({
  dir <- file.path(tempdir(), "hopdesk_lock")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  Sys.setenv(LOCALAPPDATA = normalizePath(dir, winslash = "\\", mustWork = TRUE))
  muerto <- "http://127.0.0.1:9"
  Sys.setenv(http_proxy = muerto, https_proxy = muerto, HTTP_PROXY = muerto, HTTPS_PROXY = muerto,
             no_proxy = "127.0.0.1,localhost", NO_PROXY = "127.0.0.1,localhost")
  Sys.unsetenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN"))
})

aislar_global_sin_red <- function() {
  # Straight through curl (what aws.s3/httr use underneath), so a test that
  # replaced aws.s3's functions with in-memory fakes is checked all the same.
  r <- tryCatch({
    h <- curl::curl_fetch_memory("https://s3.amazonaws.com/hopdesk-candado-no-existe-0000",
                                 handle = curl::new_handle(connecttimeout = 5))
    paste("respondió HTTP", h$status_code)
  }, error = function(e) conditionMessage(e))
  # A dead proxy fails at the connection; AWS itself would answer (404/403).
  grepl("127\\.0\\.0\\.1|proxy|connect|Couldn't|Could not|resolve", r, ignore.case = TRUE) &&
    !grepl("respondió", r)
}
