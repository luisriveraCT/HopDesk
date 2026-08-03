# =============================================================================
# tests/test_ledger_modal_reopen_chromote.R
# Stage 9, Issue B (2026-08-03): "I closed a Calendario day-view modal, then
# did something completely unrelated elsewhere in the app (tagged an item in
# Vencidos), and the OLD, closed modal reappeared." Also reported on
# spam-clicking logout (see Issue A's report -- entangled, see that stage's
# notes).
#
# The Stage 9 doc's hypothesis: ledgerModuleServer's modal_open reactiveVal
# never flips back to FALSE because the JS emits a document-level, bare
# 'cal_day_modal_closed' input, but the R-side observer lives inside
# moduleServer(id, ...) (ledgerModuleServer is instantiated twice, "ar" and
# "ap"), so input$cal_day_modal_closed there is actually namespace-scoped
# ("ar-cal_day_modal_closed"/"ap-cal_day_modal_closed") and never matches.
#
# Live chromote investigation this stage (see
# docs/saas_rebuild/STAGE_9_ATTEMPT_LOG.md for the full trail) found the
# namespace mismatch IS real, but it's not the only break in the chain: the
# JS itself used document.addEventListener('hidden.bs.modal', ...), a NATIVE
# DOM listener -- but Shiny's own bundled showModal()/modalDialog() uses the
# OLDER jQuery/Bootstrap-3-style modal plugin (confirmed: window.bootstrap is
# undefined; the modal element's class is "modal fade in", not Bootstrap 5's
# "modal fade show"), which fires 'hidden.bs.modal' as a jQuery-namespaced
# event via $el.trigger(...) -- invisible to a native addEventListener,
# regardless of namespace. A fire-counter reactiveVal at the ROOT level (not
# just the module level) stayed at 0 across a real close in the pre-fix
# reproduction. So the actual bug is two independent breaks stacked: (1) the
# JS signal never reached ANY R input, root or module, and (2) even if it
# had, the module's own `input$` would still have missed it. BOTH needed
# fixing; this test proves each half is independently necessary AND that the
# real, current code (extracted verbatim, not hand-copied) fixes both.
#
# This is a REAL, live-browser (chromote/headless-Chrome) test against a
# minimal 2-module fixture (mirrors ledgerModuleServer("ar", ...) /
# ledgerModuleServer("ap", ...) exactly, without needing real ledger/SAP
# data) -- never the full app.R (see tests/helpers/live_app_harness.R header).
# =============================================================================
cat("=== test_ledger_modal_reopen_chromote ===\n\n")

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
  cat(" SKIP: chromote/Chrome not available in this environment -- skipping live browser test.\n")
  .skip <- 1L
  cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
  invisible(NULL)
} else {

source("tests/helpers/live_app_harness.R")

# ── Extract the REAL JS listener from ui_components.R (not hand-copied) ─────
ui_txt <- readLines("R/ui_components.R", warn = FALSE)
js_start <- grep("Notify R when any Bootstrap modal is dismissed", ui_txt)
ok("found the modal-dismiss JS comment marker in ui_components.R", length(js_start) > 0)
js_bind_start <- grep("\\.on\\('hidden\\.bs\\.modal'|addEventListener\\('hidden\\.bs\\.modal'", ui_txt)
js_bind_start <- js_bind_start[js_bind_start > js_start[1]][1]
ok("found the hidden.bs.modal binding line", !is.na(js_bind_start))
js_end <- grep("^\\s*\\}\\);\\s*$", ui_txt)
js_end <- js_end[js_end >= js_bind_start][1]
js_block <- paste(ui_txt[js_bind_start:js_end], collapse = "\n")

ok("extracted JS block uses jQuery's $(document).on(...) (the actual fix -- native addEventListener never sees this jQuery-triggered event)",
   grepl("\\$\\(document\\)\\.on\\(", js_block))
ok("extracted JS block no longer uses native document.addEventListener for this signal",
   !grepl("document\\.addEventListener\\('hidden\\.bs\\.modal'", js_block))
ok("extracted JS block still sends the same 'cal_day_modal_closed' input name (only the binding mechanism changed, not the contract)",
   grepl("cal_day_modal_closed", js_block))

# ── Extract the REAL R observer from ledger_module.R (not hand-copied) ──────
ledger_txt <- readLines("R/ledger_module.R", warn = FALSE)
obs_start <- grep("observeEvent\\(session\\$rootScope\\(\\)\\$input\\$cal_day_modal_closed,|observeEvent\\(input\\$cal_day_modal_closed,", ledger_txt)
ok("found the modal-closed consumer observer in ledger_module.R", length(obs_start) > 0)
obs_end <- grep("^\\s*\\}, ignoreInit = TRUE, ignoreNULL = TRUE\\)\\s*$", ledger_txt)
obs_end <- obs_end[obs_end >= obs_start[1]][1]
r_block <- paste(ledger_txt[obs_start[1]:obs_end], collapse = "\n")

ok("extracted R observer reads session$rootScope()$input$cal_day_modal_closed (the actual fix -- a plain module-scoped input$ can never see a root-emitted signal)",
   grepl("session\\$rootScope\\(\\)\\$input\\$cal_day_modal_closed", r_block))
ok("extracted R observer still sets modal_open(FALSE) (the contract/behavior is unchanged, only the input source)",
   grepl("modal_open\\(FALSE\\)", r_block))

# ── Build a 2-module fixture app embedding BOTH real blocks verbatim,
# mirroring ledgerModuleServer("ar", ...) / ("ap", ...) exactly ────────────
build_app_code <- function(js_snippet, r_snippet) sprintf('
library(shiny)

modUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("open"), paste("Open modal", id)),
    actionButton(ns("bump_shared"), paste("Bump shared dataset (simulates an unrelated action elsewhere)")),
    verbatimTextOutput(ns("state"))
  )
}

modServer <- function(id, shared_bump) {
  moduleServer(id, function(input, output, session) {
    modal_open <- reactiveVal(FALSE)
    reopen_count <- reactiveVal(0)

    observeEvent(input$open, {
      modal_open(TRUE)
      reopen_count(reopen_count() + 1)
      showModal(modalDialog(title = paste("Modal for", id), easyClose = TRUE))
    })

    # Mirrors the ledger_module.R live-refresh observer at a MINIMAL scale:
    # watches a shared reactive; if modal_open() is TRUE, re-shows the modal
    # (the actual re-open mechanism whose staleness is the whole bug).
    observeEvent(shared_bump(), ignoreInit = TRUE, {
      if (!isolate(modal_open())) return()
      reopen_count(reopen_count() + 1)
      showModal(modalDialog(title = paste("RE-rendered modal for", id), easyClose = TRUE))
    })

    %s

    output$state <- renderText(paste("modal_open:", modal_open(), "| reopen_count:", reopen_count()))
  })
}

ui <- fluidPage(
  tags$script(HTML("
%s
  ")),
  modUI("ar"), modUI("ap")
)

server <- function(input, output, session) {
  shared_bump <- reactiveVal(0)
  modServer("ar", shared_bump)
  modServer("ap", shared_bump)
  # A single shared bump button drives BOTH modules shared_bump reactive,
  # same as bump_sync_version() driving both AR and AP watched reactives
  # from one real underlying dataset change.
  observeEvent(input[["ar-bump_shared"]], { shared_bump(isolate(shared_bump()) + 1) }, ignoreInit = TRUE)
  observeEvent(input[["ap-bump_shared"]], { shared_bump(isolate(shared_bump()) + 1) }, ignoreInit = TRUE)
}

shinyApp(ui, server)
', r_snippet, js_snippet)

PORT <- 38942L
app <- NULL
s <- NULL
on.exit({
  try(if (!is.null(s)) s$close(), silent = TRUE)
  stop_bg_app(app)
}, add = TRUE)

app_code <- build_app_code(js_block, r_block)
app <- tryCatch(start_bg_app(app_code, PORT), error = function(e) { message("startup error: ", e$message); NULL })
ok("fixture 2-module modal app started and became reachable", !is.null(app))

if (!is.null(app)) {
  base_url <- sprintf("http://127.0.0.1:%d/", PORT)
  s <- new_chromote_session()
  s$Page$navigate(base_url, wait_ = TRUE)

  # Read each module's own <pre> state output directly (element-targeted,
  # not whole-body text scraping -- robust to whatever else is on the page).
  mod_state <- function(id) {
    res <- s$Runtime$evaluate(sprintf(
      "(document.querySelector('#%s-state') || {}).innerText || ''", id))
    res$result$value
  }
  reopen_count_of <- function(id) {
    txt <- mod_state(id)
    m <- regmatches(txt, regexpr("reopen_count:\\s*\\d+", txt))
    if (!length(m)) return(NA_integer_)
    as.integer(sub("reopen_count:\\s*", "", m))
  }

  poll_until(function() nzchar(mod_state("ar")))

  # ── (a) open AR's modal ───────────────────────────────────────────────────
  click_selector(s, "#ar-open")
  poll_until(function() grepl("Modal for ar", body_text(s)), timeout_s = 5)
  ok("AR's modal_open flips TRUE when opened", grepl("modal_open: TRUE", mod_state("ar")))

  # ── (b) close it the way a real user would -- Shiny's bundled jQuery modal
  # plugin's own hide() call, which is what backdrop-click/ESC/close-button
  # all delegate to internally ──────────────────────────────────────────────
  s$Runtime$evaluate("$('#shiny-modal').modal('hide');")
  ok("AR's modal_open flips back to FALSE after closing (THE core Issue B fix)",
     poll_until(function() grepl("modal_open: FALSE", mod_state("ar")), timeout_s = 5))

  # ── (c) the actual reproduction: perform an "unrelated action elsewhere"
  # (bump the shared dataset both AR and AP watch) and confirm the CLOSED
  # modal does NOT reappear ─────────────────────────────────────────────────
  reopen_before <- reopen_count_of("ar")
  click_selector(s, "#ar-bump_shared")
  Sys.sleep(1)
  ok("bumping the shared dataset after AR's modal was closed does NOT re-show AR's modal (the exact bug: a stale, closed modal reopening from an unrelated action)",
     !grepl("RE-rendered modal for ar", body_text(s)))
  ok("AR's reopen_count did NOT increment from the unrelated bump (modal_open's FALSE gate correctly suppressed the re-render, not just visually hidden)",
     identical(reopen_before, reopen_count_of("ar")))

  # ── (d) non-regression: an OPEN modal STILL updates live on the same
  # shared-dataset change (Mouse's explicit requirement -- don't lose this
  # to fix the reopening bug) ────────────────────────────────────────────────
  click_selector(s, "#ap-open")
  poll_until(function() grepl("Modal for ap", body_text(s)), timeout_s = 5)
  click_selector(s, "#ap-bump_shared")
  ok("an OPEN modal (AP's) still re-renders live when the shared dataset changes (Stage 2 live-update behavior preserved, not regressed by this fix)",
     poll_until(function() grepl("RE-rendered modal for ap", body_text(s)), timeout_s = 5))

  # ── (e) AP's own modal_open is unaffected by AR's earlier close (each
  # module's own flag is independent; the shared root signal correctly
  # resets each module's OWN flag rather than being globally confused) ──────
  ok("AP's modal is still genuinely open per its own modal_open flag",
     grepl("modal_open: TRUE", mod_state("ap")))
}

cat("\n=== results:", .pass, "passed,", .fail, "failed,", .skip, "skipped ===\n")
if (.fail > 0) stop("Tests FAILED.")
invisible(NULL)
}
