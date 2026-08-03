# =============================================================================
# tests/test_sync_bus_timing_chromote.R
# Stage 9, Issue E1 (2026-08-03): "changing a provision or item from one date
# to another does update live and does log, but if I edit on window A, B gets
# whitewashed for like over 10 seconds and then it updates correctly. Slow
# af... it won't work in prod."
#
# R/sync_bus.R's poller runs invalidateLater(poll_ms, session) with
# poll_ms = 8000 (app.R:1294). The Stage 9 doc asked whether the observed
# ~10+ seconds is (a) just unlucky timing relative to the fixed 8s poll
# (nothing broken, an inherent property of the current design), (b) a slow
# .refresh_ctx_detail()/showModal() re-render on top of that, or (c) a
# genuine bug in the version-diffing logic causing a missed tick.
#
# This is a REAL, live chromote test: TWO concurrent chromote sessions
# against a minimal fixture app that sources the REAL, unmodified
# R/sync_bus.R (not a hand-copied mirror) against a local, in-memory fake of
# the S3 layer (never touches production S3/AWS -- see
# tests/helpers/live_app_harness.R's header for why the full app is never
# launched for automated testing). Measures actual end-to-end propagation
# time from one session's bump_sync_version() call to another session's
# reactiveVal actually updating, across several trials with randomized
# timing relative to the poll cycle.
#
# Finding: across real trials, propagation time is bounded by roughly
# [0, poll_ms] with a mean around poll_ms/2, exactly what pure poll-interval
# waiting predicts -- no trial ever exceeded poll_ms by more than one
# render/tick's worth of overhead, and no update was ever silently dropped.
# This rules out (c) -- there is no missed-tick bug -- and is strong
# evidence for (a): the dominant cost is inherent to the fixed 8s poll
# interval, not a bug. This harness cannot measure real S3 network latency
# or the real .refresh_ctx_detail()/showModal() render cost (both add ON TOP
# of what's measured here in production) -- (b) is not fully ruled in or out
# by this test alone; see this stage's final report for the full reasoning
# and an explicit (not unilateral) recommendation on tightening poll_ms,
# which the ledger-integrity master plan's own "Open items needing Mouse's
# input" section already flagged as his call to make, not this stage's.
# =============================================================================
cat("=== test_sync_bus_timing_chromote ===\n\n")

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
  c("chromote", "callr", "curl", "jsonlite", "shiny"),
  requireNamespace, logical(1), quietly = TRUE
))
.chrome_ok <- .deps_ok && !inherits(tryCatch(chromote::find_chrome(), error = function(e) e), "error")

if (!.chrome_ok) {
  cat(" SKIP: chromote/Chrome not available in this environment -- skipping live timing test.\n")
  .skip <- 1L
  cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
  invisible(NULL)
} else {

source("tests/helpers/live_app_harness.R")

POLL_MS <- 8000L  # matches app.R:1294's real production value exactly

app_code <- sprintf('
library(shiny)

# Local, in-memory fake of the S3 layer -- sync_bus.R never touches real S3.
.FAKE_STORE <- new.env()
S3_KEYS <- list(shared_val = "shared_val.rds", sync_versions = "sync_versions.rds")
.s3_missing_cache <- new.env()
.s3_preload_cache <- new.env()
.s3_key   <- function(key, client_id = NULL) paste0("test/", key)
.s3_read  <- function(key, client_id = NULL) {
  k <- .s3_key(key, client_id = client_id)
  if (exists(k, envir = .FAKE_STORE, inherits = FALSE)) get(k, envir = .FAKE_STORE) else NULL
}
.s3_write <- function(value, key, client_id = NULL) {
  assign(.s3_key(key, client_id = client_id), value, envir = .FAKE_STORE)
  invisible(TRUE)
}
`%%||%%` <- function(x, y) if (is.null(x)) y else x

# local = TRUE: keep sync_bus.R functions in THIS app-private environment,
# the same place the fakes above live -- shiny::runApp() on a single-file app
# evaluates the top level in its own env, not .GlobalEnv; local = FALSE would
# split sync_bus.R functions from the fakes into two environments and
# break their lexical lookup of .s3_write/.s3_read/etc (confirmed live during
# this stage investigation -- see docs/saas_rebuild/STAGE_9_ATTEMPT_LOG.md).
source("%s/R/sync_bus.R", local = TRUE)

load_shared_val <- function(client_id = NULL) .s3_read(S3_KEYS$shared_val)
save_shared_val <- function(value, client_id = NULL) .s3_write(value, S3_KEYS$shared_val)
register_synced("shared_val", S3_KEYS$shared_val, load_shared_val)

ui <- fluidPage(
  actionButton("bump", "Write + bump"),
  verbatimTextOutput("state")
)
server <- function(input, output, session) {
  # shared is a plain list of reactiveVal()s, matching app.R real shape
  # (app.R:1131) -- setup_sync_bus() calls shared[[name]] AS A FUNCTION.
  shared <- list(shared_val = reactiveVal(0))
  observeEvent(input$bump, {
    save_shared_val(isolate(shared$shared_val()) + 1000)
    bump_sync_version("shared_val")
  }, ignoreInit = TRUE)
  setup_sync_bus(session, shared, poll_ms = %d)
  output$state <- renderText(paste("shared_val:", shared$shared_val()))
}
shinyApp(ui, server)
', gsub("\\\\", "/", normalizePath(getwd())), POLL_MS)

PORT <- 38960L
app <- NULL
sA <- NULL
sB <- NULL
on.exit({
  try(if (!is.null(sA)) sA$close(), silent = TRUE)
  try(if (!is.null(sB)) sB$close(), silent = TRUE)
  stop_bg_app(app)
}, add = TRUE)

app <- tryCatch(start_bg_app(app_code, PORT), error = function(e) { message("startup error: ", e$message); NULL })
ok("fixture sync_bus timing app started and became reachable", !is.null(app))

if (!is.null(app)) {
  base_url <- sprintf("http://127.0.0.1:%d/", PORT)
  sA <- new_chromote_session()
  sB <- new_chromote_session()
  sA$Page$navigate(base_url, wait_ = TRUE)
  sB$Page$navigate(base_url, wait_ = TRUE)
  poll_until(function() grepl("shared_val: 0", body_text(sA)))
  poll_until(function() grepl("shared_val: 0", body_text(sB)))

  N_TRIALS <- 6
  deltas <- numeric(0)
  for (trial in seq_len(N_TRIALS)) {
    Sys.sleep(runif(1, 0, POLL_MS / 1000 * 0.75))  # randomize phase relative to B's own poll clock
    t0 <- Sys.time()
    click_selector(sA, "#bump")
    expected <- trial * 1000
    caught <- poll_until(function() grepl(paste0("shared_val: ", expected, "$"), trimws(body_text(sB))),
                         timeout_s = (POLL_MS / 1000) + 8, interval_s = 0.15)
    delta <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cat(sprintf("  trial %d: propagation = %.2fs (caught=%s)\n", trial, delta, caught))
    ok(sprintf("trial %d: session B eventually saw the update (no silent drop)", trial), caught)
    if (caught) deltas <- c(deltas, delta)
    Sys.sleep(0.3)
  }

  ok("at least 5 of 6 trials completed successfully (guard against a flaky harness silently reporting nothing)",
     length(deltas) >= 5)

  if (length(deltas) >= 2) {
    cat(sprintf("\n  Propagation delay across %d trials: min=%.2fs mean=%.2fs max=%.2fs (poll_ms=%dms)\n\n",
               length(deltas), min(deltas), mean(deltas), max(deltas), POLL_MS))
    ok("no trial's propagation delay exceeded poll_ms by more than a 3s render/tick margin (rules out hypothesis (c): a missed-tick/version-diffing bug -- a real bug would show delays clustering near 2x poll_ms or higher, or timeouts)",
       max(deltas) <= (POLL_MS / 1000) + 3)
    ok("mean propagation delay is consistent with 'waiting for the next poll tick', not some larger fixed penalty (roughly bounded by poll_ms, not poll_ms*2+)",
       mean(deltas) <= (POLL_MS / 1000) + 1)
  }
}

cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
}
