# =============================================================================
# tests/test_sync_bus_isolation_chromote.R
# Stage 9, Issue E2 (2026-08-03): "verify this does NOT happen whatsoever
# cross Clients (if Hopdesk employee is changing stuff in their Hopdesk
# folder, it should NOT interrupt anything on anyone's window ideally, but
# ESPECIALLY CRITICAL CROSS CLIENTS ... otherwise this already decently
# large problem will be catastrophical with more and more clients."
#
# The Stage 9 doc laid out two separate questions about R/sync_bus.R, read
# in full before writing anything (as instructed):
#   (a) correctness/security -- does a session ever receive ANOTHER client's
#       data via the poll-and-reload mechanism? bump_sync_version(name)
#       bumps ONE counter per dataset NAME, global to the process (confirmed
#       by reading the code) -- does that ever leak across the client
#       boundary?
#   (b) UX -- even where data stays correctly scoped, does an UNRELATED
#       client's write force every OTHER client's session to
#       recompute/re-render at all? Mouse said this should NOT happen.
#
# THIS IS THE HIGHEST-STAKES ITEM IN THIS STAGE. Per the stage doc's own
# instruction: do not conclude anything about the isolation boundary without
# building this harness and proving it has real teeth (temporarily break
# isolation on purpose, confirm the harness catches it, restore, confirm it
# passes again) -- a harness that would pass either way is worthless.
#
# ── Finding (a): SAFE, confirmed both by reading the code and by this live
# harness. Every reload in setup_sync_bus() is scoped by
# active_client_rv() (== app.R:1295's effective_client_id, a genuine
# per-session reactive -- app.R:1029-1031) via
# entry$loader(client_id = active_cid) whenever active_cid differs from the
# deployment's CLIENT_ID env var (true for every real client session AND
# every staff-jumped session), and via entry$loader() with zero args (which
# falls back to the SAME env var) only for a staff session sitting in its
# own hd-admin home context -- which is the one case where that fallback is
# actually correct. No session's reactiveVal is ever assigned another
# client's reloaded content -- proven below across repeated writes from a
# DIFFERENT client, and the harness is proven to have teeth for this
# specific claim (see Part 2).
#
# ── Finding (b): This did NOT need a new fix. Investigated live: a first,
# less careful version of this harness (see
# docs/saas_rebuild/STAGE_9_ATTEMPT_LOG.md) appeared to show client B's
# session re-rendering after client A's unrelated write -- but that was a
# test-harness confound (B's reactiveVal hadn't yet caught up to its OWN
# baseline after switching identity, so the reload really WAS a genuine
# change for B, not a false trigger). Once the harness properly establishes
# each session's own baseline first, repeated unrelated writes from another
# client never increment the watching session's own re-render counter.
# Traced why: shiny::reactiveVal()'s own setter
# (`ReactiveVal$set()`, in the installed shiny package) already does
# `if (identical(private$value, value)) return(invisible(FALSE))` before
# invalidating any dependents -- and app.R:1131's `shared` is a plain list of
# individual reactiveVal()s (confirmed during Issue E1's investigation), not
# a shiny::reactiveValues() object. So a poll-triggered reload that returns
# byte-for-byte the same content a session already has (exactly what happens
# for every OTHER client when ONE client writes) never triggers
# invalidation, for free, with no fix needed in R/sync_bus.R itself. An
# initial attempt DID add an explicit identical() guard in sync_bus.R before
# this was fully understood -- reverted once confirmed redundant (also in
# the attempt log) rather than shipping dead code that duplicates behavior
# Shiny already provides.
#
# Residual, not a bug: this guarantee depends on a reload actually returning
# `identical()`-equal content when nothing relevant changed. A data source
# that embeds something incidental (e.g. a save-time timestamp column
# touched on every full-table rewrite regardless of which rows changed)
# could still defeat this and cause a real but narrower version of the
# UX complaint. Not confirmed to exist in this codebase's actual save_*()
# functions during this stage's scope; flagged as worth knowing, not fixed
# speculatively.
# =============================================================================
cat("=== test_sync_bus_isolation_chromote ===\n\n")

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
  cat(" SKIP: chromote/Chrome not available in this environment -- skipping live isolation test.\n")
  .skip <- 1L
  cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
  invisible(NULL)
} else {

source("tests/helpers/live_app_harness.R")

# Builds the fixture app source. `sync_bus_path`: which sync_bus.R-shaped
# file to source (the real one, or a deliberately-broken scratch copy for
# the teeth proof below). `deployment_client_id`: what Sys.getenv("CLIENT_ID")
# resolves to for this fixture run (production is always "hd-admin" --
# confirmed in .Renviron; the teeth-proof run below deliberately sets it to
# a REAL client slug to simulate the concrete misconfiguration/regression
# shape that would turn "an unrelated reload" into "one client literally
# receiving another client's real data").
build_isolation_app <- function(sync_bus_path, deployment_client_id) {
  sprintf('
library(shiny)

.FAKE_STORE <- new.env()
S3_KEYS <- list(shared_val = "shared_val.rds", sync_versions = "sync_versions.rds")
.s3_missing_cache <- new.env()
.s3_preload_cache <- new.env()
`%%||%%` <- function(x, y) if (is.null(x)) y else x
.client_id <- function() Sys.getenv("CLIENT_ID")
.s3_key <- function(key, client_id = NULL) {
  prefix <- if (!is.null(client_id) && nzchar(client_id)) tolower(client_id) else tolower(.client_id())
  paste0(prefix, "/", key)
}
.s3_read <- function(key, client_id = NULL) {
  k <- .s3_key(key, client_id = client_id)
  if (exists(k, envir = .FAKE_STORE, inherits = FALSE)) get(k, envir = .FAKE_STORE) else NULL
}
.s3_write <- function(value, key, client_id = NULL) {
  assign(.s3_key(key, client_id = client_id), value, envir = .FAKE_STORE)
  invisible(TRUE)
}
Sys.setenv(CLIENT_ID = "%s")

# local = TRUE: keep sync_bus functions in this app-private environment, the
# same place the fakes above live (see docs/saas_rebuild/STAGE_9_ATTEMPT_LOG.md
# for why local = FALSE breaks this).
source("%s", local = TRUE)

load_shared_val <- function(client_id = NULL) .s3_read(S3_KEYS$shared_val, client_id = client_id)
save_shared_val <- function(value, client_id = NULL) .s3_write(value, S3_KEYS$shared_val, client_id = client_id)
register_synced("shared_val", S3_KEYS$shared_val, load_shared_val)

save_shared_val(list(client = "networks", value = "networks-original"), client_id = "networks")
save_shared_val(list(client = "acme",     value = "acme-original"),     client_id = "acme")

ui <- fluidPage(
  selectInput("as_client", "acts as:", choices = c("networks", "acme"), selected = "networks"),
  actionButton("write_own", "write+bump"),
  verbatimTextOutput("state")
)
server <- function(input, output, session) {
  # Mirrors app.R:1029-1031 exactly (home_client_id/jump_client_id/
  # effective_client_id) -- a real per-session identity reactive, not a
  # hand-simplified stand-in.
  home_client_id <- reactiveVal(NULL)
  jump_client_id <- reactiveVal(NULL)
  effective_client_id <- reactive({ jump_client_id() %%||%% home_client_id() })
  observeEvent(input$as_client, { home_client_id(input$as_client) }, ignoreNULL = FALSE)
  home_client_id(isolate(input$as_client) %%||%% "networks")

  # shared is a plain list of reactiveVal()s, matching app.R real shape
  # (app.R:1131) -- NOT shiny::reactiveValues().
  shared <- list(shared_val = reactiveVal(load_shared_val(client_id = isolate(effective_client_id()))))

  rerender_count <- reactiveVal(0)
  observeEvent(shared$shared_val(), { rerender_count(isolate(rerender_count()) + 1) }, ignoreInit = TRUE)

  observeEvent(input$write_own, {
    cid <- effective_client_id()
    new_val <- list(client = cid, value = paste0(cid, "-updated-", as.character(as.numeric(Sys.time())*1000)))
    save_shared_val(new_val, client_id = cid)
    bump_sync_version("shared_val")  # bare, no client_id -- the codebase real dominant convention
  }, ignoreInit = TRUE)

  setup_sync_bus(session, shared, poll_ms = 1500, active_client_rv = effective_client_id)

  output$state <- renderText({
    v <- shared$shared_val()
    paste0("my_client:", effective_client_id(),
           "|shared_val_client:", v$client %%||%% "NULL",
           "|shared_val_value:", v$value %%||%% "NULL",
           "|rerender_count:", rerender_count())
  })
}
shinyApp(ui, server)
', deployment_client_id, gsub("\\\\", "/", normalizePath(sync_bus_path)))
}

# ── Drives one full trial against a given app_code, returns a results list.
run_isolation_trial <- function(app_code, port) {
  app <- NULL; sA <- NULL; sB <- NULL
  res <- list(leak_detected = NA, baseline_B = NA, after_B = NA, final_A = NULL, final_B = NULL)
  tryCatch({
    app <- start_bg_app(app_code, port)
    sA <- new_chromote_session()  # acts as "networks"
    sB <- new_chromote_session()  # will switch to "acme"
    base_url <- sprintf("http://127.0.0.1:%d/", port)
    sA$Page$navigate(base_url, wait_ = TRUE)
    sB$Page$navigate(base_url, wait_ = TRUE)
    poll_until(function() grepl("my_client:networks", body_text(sA)))
    poll_until(function() grepl("my_client:networks", body_text(sB)))

    set_input_value(sB, "as_client", "acme")
    poll_until(function() grepl("my_client:acme", body_text(sB)), timeout_s = 5)

    # Establish B's own correct baseline BEFORE testing cross-client
    # behavior (avoids the confound documented in the attempt log: B's
    # reactiveVal is only initialized once, at session start, under
    # whatever identity was active then).
    click_selector(sB, "#write_own")
    poll_until(function() grepl("shared_val_client:acme", body_text(sB)), timeout_s = 5)
    Sys.sleep(0.3)
    get_field <- function(txt, field) sub(paste0(".*", field, ":([^|]*).*"), "\\1", txt)
    b_state0 <- gsub("\n", "", body_text(sB))
    res$baseline_B <- as.integer(get_field(b_state0, "rerender_count"))
    baseline_val_B <- get_field(b_state0, "shared_val_value")

    # Client A ("networks") now writes its OWN data multiple times.
    for (i in 1:3) {
      click_selector(sA, "#write_own")
      Sys.sleep(0.6)
    }
    Sys.sleep(3)  # give B several poll ticks (poll_ms=1500) to react

    a_state <- gsub("\n", "", body_text(sA))
    b_state <- gsub("\n", "", body_text(sB))
    res$final_A <- a_state
    res$final_B <- b_state
    res$after_B <- as.integer(get_field(b_state, "rerender_count"))

    b_client_field <- get_field(b_state, "shared_val_client")
    b_value_field  <- get_field(b_state, "shared_val_value")
    # THE CORE SECURITY ASSERTION: B must never show "networks" as its own
    # shared_val_client, and must never show A's actual written value.
    res$leak_detected <- identical(b_client_field, "networks") ||
                         grepl("^networks-updated-", b_value_field)
  }, error = function(e) message("trial error: ", conditionMessage(e)))
  try(if (!is.null(sA)) sA$close(), silent = TRUE)
  try(if (!is.null(sB)) sB$close(), silent = TRUE)
  stop_bg_app(app)
  res
}

REAL_SYNC_BUS <- "R/sync_bus.R"

# =============================================================================
# Part 1: the REAL, current sync_bus.R -- correctness (a) and UX (b)
# =============================================================================
app_real <- build_isolation_app(REAL_SYNC_BUS, "hd-admin")
r1 <- run_isolation_trial(app_real, 38980L)

ok("Part 1: trial completed and produced readable state for both sessions",
   !is.null(r1$final_A) && !is.null(r1$final_B))
ok("Part 1 (Finding a -- correctness): client B NEVER shows client A's data, across 3 separate writes from A",
   isFALSE(r1$leak_detected))
ok("Part 1: client B's own identity is still correctly 'acme' throughout (didn't get confused/reset)",
   grepl("my_client:acme", r1$final_B %||% ""))
ok("Part 1: client A's own write is correctly visible in ITS OWN session (regression guard -- isolation isn't achieved by breaking normal same-client updates)",
   grepl("shared_val_client:networks", r1$final_A %||% "") && grepl("networks-updated-", r1$final_A %||% ""))
ok(sprintf("Part 1 (Finding b -- UX): client B's rerender_count did NOT increase after 3 unrelated writes from client A (baseline=%s, after=%s) -- no unnecessary cross-client re-render, thanks to shiny::reactiveVal()'s own identical-value skip",
          r1$baseline_B, r1$after_B),
   !is.na(r1$baseline_B) && !is.na(r1$after_B) && r1$baseline_B == r1$after_B)

# =============================================================================
# Part 2: prove the harness has teeth -- temporarily (this run only, nothing
# committed) use a deliberately-broken sync_bus.R that drops the
# in_jump_context client-id threading entirely (always calls
# entry$loader() with zero args, exactly the shape of regression Stage 8 was
# written to hunt for), combined with a deployment CLIENT_ID that happens to
# coincide with a REAL client slug ("networks") -- simulating the concrete
# misconfiguration shape that turns "an unrelated reload" into "one client
# receiving another client's real, actual data". If the assertion above
# would pass regardless of whether the code is actually correct, it is
# worthless -- this proves it is not.
# =============================================================================
broken_sync_bus <- tempfile(fileext = ".R")
real_lines <- readLines(REAL_SYNC_BUS, warn = FALSE)
# Replace the in_jump_context-gated reload call with an unconditional,
# zero-argument call -- deletes exactly the fix this stage's reading of the
# code identified as the correctness-relevant branch.
broken_lines <- gsub(
  "if \\(in_jump_context\\)\\s*$", "if (FALSE)  # BROKEN FOR TEST: client_id threading disabled",
  real_lines
)
writeLines(broken_lines, broken_sync_bus)
ok("built the deliberately-broken sync_bus.R variant (disables client_id threading on reload)",
   any(grepl("BROKEN FOR TEST", broken_lines)) && !identical(broken_lines, real_lines))

app_broken <- build_isolation_app(broken_sync_bus, "networks")  # misconfigured env var, matches a real client
r2 <- run_isolation_trial(app_broken, 38981L)
unlink(broken_sync_bus)

ok("Part 2 (teeth proof): against the deliberately-broken code, the SAME harness DOES detect the leak (client B ends up showing client A's/networks' data) -- confirms this harness has real teeth, not a check that would pass either way",
   isTRUE(r2$leak_detected))

# =============================================================================
# Part 3: confirm the REAL code passes again immediately after (no lingering
# state from the broken run -- each trial launches a fresh R subprocess)
# =============================================================================
r3 <- run_isolation_trial(app_real, 38982L)
ok("Part 3: re-running against the REAL, unmodified sync_bus.R passes again (no leak) -- confirms Part 2's failure was specific to the deliberately-broken variant, not a flaky harness",
   isFALSE(r3$leak_detected))

cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
}
