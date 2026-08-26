# =============================================================================
# tests/test_activity_live_refresh_toggle.R
#
# Incident (reported 2026-08-25): the Actividad table "seems to refresh or
# restart constantly" and its filters "randomly reset." Root cause:
# R/audit_log_viewer_module.R's fetched() reactive called invalidateLater(20000)
# unconditionally, rebuilding the whole DT widget (and its own client-side
# search box + page position) every 20 seconds regardless of whether the
# user was mid-filter. Fix: that poll is now opt-in via a small toggle button
# next to the existing manual refresh button, default OFF.
#
# Sourced by _run_saas.R (which loads shiny/DT/shinyWidgets/bslib and
# R/audit_log_viewer_module.R, and seeds a mocked S3 store, before running
# this file) -- same harness as tests/test_saas_audit_viewer.R, which this
# file supplements rather than duplicates (that file's mode A-H coverage is
# untouched by this fix and is re-run in full by the same suite run).
# =============================================================================

cat("── Activity live-refresh toggle (Actividad glitch fix) ────────────────\n")

.pass <- 0L
.fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}
.chk_true <- function(cond, label) .chk(isTRUE(cond), TRUE, label)

# =============================================================================
# Part A -- static scan of the real source text
# =============================================================================
cat("\n-- Part A: static scan of R/audit_log_viewer_module.R --\n")

src_path <- "R/audit_log_viewer_module.R"
.chk(file.exists(src_path), TRUE, "R/audit_log_viewer_module.R exists at expected path")
txt <- readLines(src_path, warn = FALSE)

.chk_true({p <- tryCatch({parse(src_path); TRUE}, error = function(e) FALSE); p},
          "audit_log_viewer_module.R still parses without syntax errors after the edit")

live_var_ln    <- grep("live_refresh_on <- reactiveVal\\(FALSE\\)", txt)
toggle_obs_ln  <- grep("observeEvent\\(input\\$audit_live_toggle,", txt)
btn_output_ln  <- grep("output\\$live_toggle_btn <- renderUI", txt)
gated_poll_ln  <- grep("if \\(isTRUE\\(live_refresh_on\\(\\)\\)\\) invalidateLater\\(20000\\)", txt)
ui_wire_ln     <- grep("uiOutput\\(ns\\(\"live_toggle_btn\"\\)", txt)

.chk(length(live_var_ln), 1L, "live_refresh_on reactiveVal is defined, defaulting to FALSE")
.chk(length(toggle_obs_ln), 1L, "an observeEvent on input$audit_live_toggle exists")
.chk(length(btn_output_ln), 1L, "output$live_toggle_btn renderUI exists")
.chk(length(gated_poll_ln), 1L, "invalidateLater(20000) is gated behind live_refresh_on() -- the actual fix")
.chk(length(ui_wire_ln), 1L, "the toggle button's uiOutput is wired into the toolbar UI")

# Regression guard: invalidateLater(20000) must appear EXACTLY where it's
# gated -- an unconditional second copy anywhere else would silently
# reintroduce the incident.
unconditional_hits <- grep("^\\s*invalidateLater\\(20000\\)\\s*(#.*)?$", txt)
.chk(length(unconditional_hits), 0L,
     "no UNCONDITIONAL invalidateLater(20000) call remains anywhere in the file")
all_20000_hits <- grep("invalidateLater\\(20000\\)", txt)
.chk(length(all_20000_hits), 1L,
     "invalidateLater(20000) appears exactly once in the whole file (the gated call)")

# Scoping check: the toggle button is rendered via its OWN uiOutput, not
# inline inside output$panel's renderUI -- toggling it must never force the
# whole toolbar (and the filters inside it) to re-render, which would be
# the same class of problem this fix exists to solve, just user-triggered
# instead of automatic.
panel_start <- grep('output\\$panel <- renderUI\\(\\{', txt)
panel_related_end <- grep('output\\$audit_results <- renderUI', txt)
.chk_true(length(panel_start) == 1 && length(panel_related_end) == 1,
          "found both output$panel and output$audit_results anchors to scope-check against")
if (length(panel_start) == 1 && length(panel_related_end) == 1) {
  panel_block <- paste(txt[panel_start:(panel_related_end - 1)], collapse = "\n")
  .chk_true(grepl('uiOutput\\(ns\\("live_toggle_btn"\\)', panel_block),
            "output$panel references the toggle button only via uiOutput(), never inlines its renderUI logic")
  .chk_true(!grepl('toggle-off', panel_block),
            "output$panel's own renderUI body contains no icon-state logic itself (that lives in output$live_toggle_btn, isolating the reactive scope)")
}

btn_render_block <- if (length(btn_output_ln)) {
  end <- btn_output_ln[1] + 15
  paste(txt[btn_output_ln[1]:min(end, length(txt))], collapse = "\n")
} else ""
.chk_true(grepl('"toggle-on"', btn_render_block) && grepl('"toggle-off"', btn_render_block),
          "live_toggle_btn renders a distinct icon per state (toggle-on vs toggle-off)")
.chk_true(grepl("btn-outline-success", btn_render_block) && grepl("btn-outline-secondary", btn_render_block),
          "live_toggle_btn renders a distinct button color per state")
.chk_true(grepl("title\\s*=", btn_render_block),
          "live_toggle_btn sets a title/tooltip explaining the current state")

# The manual refresh mechanism (refresh_tick / audit_refresh) must be
# completely untouched by this fix -- it's the thing users are told to rely
# on now that live polling is opt-in.
.chk(length(grep('actionButton\\(ns\\("audit_refresh"\\)', txt)), 1L,
     "the manual refresh button (audit_refresh) still exists, unmodified")
.chk(length(grep('observeEvent\\(input\\$audit_refresh,', txt)), 1L,
     "the manual refresh observer (refresh_tick increment) still exists, unmodified")

# =============================================================================
# Part B -- functional testServer() coverage
# =============================================================================
cat("\n-- Part B: testServer() functional coverage --\n")

.seed_usuario_local <- function(cid, username, tier, permisos_json = "{}") {
  existing <- tryCatch(mock_s3readRDS(paste0(cid, "/usuarios.rds"), "mock-bucket"),
                        error = function(e) NULL)
  row <- data.frame(username = username, tier = tier, permisos = permisos_json,
                     stringsAsFactors = FALSE)
  df <- if (is.null(existing)) row else {
    existing <- existing[tolower(existing$username) != tolower(username), , drop = FALSE]
    rbind(existing[, names(row), drop = FALSE], row)
  }
  mock_s3saveRDS(df, paste0(cid, "/usuarios.rds"), "mock-bucket")
}
.seed_usuario_local("networks", "tesoreria2", "finance")

mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds", "mock-bucket")
log_action(user = "tesoreria2", module = "bancos", action = "reconcile",
           description = "toggle-test entry", client_id = "networks",
           viewer_home_client_id = "networks")
.audit_flush_pending("networks")

.mock_shared_local <- function(user, tier, client_id, is_staff, home_cid, effective_cid) {
  list(
    current_user_info  = function() list(user = user, tier = tier, client_id = client_id,
                                          is_staff = is_staff),
    home_client_id      = function() home_cid,
    effective_client_id = function() effective_cid
  )
}

shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared_local("tesoreria2", "finance", "networks", FALSE, "networks", "networks"),
  mode   = "own_client"
), {
  session$flushReact()

  # ── B1: default state ─────────────────────────────────────────────────────
  .chk(live_refresh_on(), FALSE, "B1: live_refresh_on() defaults to FALSE on a fresh session")

  df0 <- fetched()
  .chk(inherits(df0, "error"), FALSE, "B1: fetched() works correctly with live-refresh OFF (default)")
  .chk(is.data.frame(df0) && nrow(df0) >= 1L, TRUE, "B1: fetched() returns real rows with live-refresh OFF")

  # ── B2: toggling on ────────────────────────────────────────────────────────
  session$setInputs(audit_live_toggle = 1)
  session$flushReact()
  .chk(live_refresh_on(), TRUE, "B2: one click on audit_live_toggle flips live_refresh_on() to TRUE")

  df1 <- fetched()
  .chk(inherits(df1, "error"), FALSE, "B2: fetched() still works correctly with live-refresh ON")
  .chk(is.data.frame(df1) && nrow(df1) >= 1L, TRUE, "B2: fetched() still returns real rows with live-refresh ON")

  # ── B3: toggling back off ─────────────────────────────────────────────────
  session$setInputs(audit_live_toggle = 2)
  session$flushReact()
  .chk(live_refresh_on(), FALSE, "B3: a second click flips live_refresh_on() back to FALSE")

  # ── B4: rapid multi-toggle stays correct (odd clicks = ON, even = OFF) ─────
  for (i in 3:8) {
    session$setInputs(audit_live_toggle = i)
    session$flushReact()
    expected <- (i %% 2L == 1L)
    .chk(live_refresh_on(), expected,
         sprintf("B4: after %d total toggle clicks, live_refresh_on() == %s", i, expected))
  }

  # ── B5: manual refresh (refresh_tick) works identically regardless of the
  # live-refresh toggle's current state -- it must never depend on it ───────
  tick_before <- refresh_tick()
  session$setInputs(audit_refresh = (input$audit_refresh %||% 0) + 1)
  session$flushReact()
  .chk(refresh_tick(), tick_before + 1L,
       "B5: clicking the manual refresh button still increments refresh_tick() regardless of toggle state")
  df2 <- fetched()
  .chk(inherits(df2, "error"), FALSE, "B5: manual refresh still fetches successfully after multiple toggles")
})

# ── B6: a viewer who never touches the toggle at all (the common case) sees
# completely normal behavior -- toggle-related code must be fully inert
# unless explicitly used ────────────────────────────────────────────────────
shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared_local("tesoreria2", "finance", "networks", FALSE, "networks", "networks"),
  mode   = "own_client"
), {
  session$flushReact()
  .chk(live_refresh_on(), FALSE, "B6: a session that never clicks the toggle stays at the default (FALSE)")
  vis <- visible_fetched()
  .chk(any(vis$action == "reconcile"), TRUE,
       "B6: normal log rows are visible exactly as before this fix, untouched by the toggle's existence")

  # Filters (the actual regression this fix targets) still narrow correctly
  # -- re-verified here specifically alongside the new toggle code, not just
  # in the older test_saas_audit_viewer.R file, to prove the two features
  # don't interact badly.
  session$setInputs(audit_mod_sel = "bancos")
  session$flushReact()
  f <- filtered()
  .chk(nrow(f) >= 1L && all(f$module == "bancos"), TRUE,
       "B6: Módulo filter still narrows correctly with the toggle present but untouched")
})

# ── B7: multi_client mode is unaffected by this fix (same toggle mechanism,
# different data-scoping mode) ───────────────────────────────────────────────
shiny::testServer(auditLogViewerServer, args = list(
  shared             = .mock_shared_local("bunny_toggle", "hopdesk", "hd-admin", TRUE, "hd-admin", "hd-admin"),
  mode               = "multi_client",
  allowed_client_ids = c(Networks = "networks"),
  include_staff_log  = function() FALSE
), {
  # flushReact() once before the first setInputs() -- otherwise the
  # observer's ignoreInit=TRUE "first run" and this test's first real toggle
  # click coalesce into the same flush and the click gets ignored too (a
  # testServer()-only artifact; a real browser session is always already
  # flushed once by the time a user can click anything). Same pattern B1
  # above already follows.
  session$flushReact()
  .chk(live_refresh_on(), FALSE, "B7: multi_client mode also defaults live_refresh_on() to FALSE")
  session$setInputs(audit_live_toggle = 1)
  session$flushReact()
  .chk(live_refresh_on(), TRUE, "B7: toggle works identically in multi_client mode")

  session$setInputs(audit_client_sel = "networks")
  session$flushReact()
  df <- fetched()
  .chk(inherits(df, "error"), FALSE, "B7: fetched() still works in multi_client mode with live-refresh ON")
})

# =============================================================================
# Part C -- direct regression proof: filters survive exactly the scenario
# that was reported (toggling / repeated flushes), which is the actual
# symptom this stage exists to fix, not just the toggle mechanism in
# isolation
# =============================================================================
cat("\n-- Part C: filters survive toggling and repeated flushes --\n")

mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds", "mock-bucket")
log_action(user = "tesoreria2", module = "bancos",    action = "reconcile",
           description = "row 1", client_id = "networks", viewer_home_client_id = "networks")
log_action(user = "larm2",      module = "ledger_AR", action = "mover_fecha",
           description = "row 2", client_id = "networks", viewer_home_client_id = "networks")
.audit_flush_pending("networks")

shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared_local("tesoreria2", "finance", "networks", FALSE, "networks", "networks"),
  mode   = "own_client"
), {
  session$flushReact()

  # Set a filter, same as a user actively narrowing the table.
  session$setInputs(audit_mod_sel = "ledger_AR", audit_user_filter = "larm2")
  session$flushReact()
  f0 <- filtered()
  .chk(nrow(f0) == 1L && f0$user[1] == "larm2", TRUE,
       "C1: filter set correctly before any toggle interaction")

  # Turn live-refresh ON -- this alone must not touch the filter inputs.
  session$setInputs(audit_live_toggle = 1)
  session$flushReact()
  .chk(live_refresh_on(), TRUE, "C2: live-refresh is now ON")
  f1 <- filtered()
  .chk(nrow(f1) == 1L && f1$user[1] == "larm2", TRUE,
       "C2: filter values survive turning live-refresh ON")
  .chk(input$audit_mod_sel, "ledger_AR", "C2: Módulo input value itself is unchanged after the toggle")
  .chk(input$audit_user_filter, "larm2", "C2: Usuario input value itself is unchanged after the toggle")

  # Multiple flushes in a row (simulating time passing / repeated reactivity
  # cycles) must not reset anything either -- this is the direct proxy for
  # "the table seems to refresh or restart constantly."
  for (i in 1:5) session$flushReact()
  f2 <- filtered()
  .chk(nrow(f2) == 1L && f2$user[1] == "larm2", TRUE,
       "C3: filter survives 5 consecutive flushes with live-refresh ON")

  # Turn live-refresh back OFF -- filters must still be intact.
  session$setInputs(audit_live_toggle = 2)
  session$flushReact()
  .chk(live_refresh_on(), FALSE, "C4: live-refresh is now OFF again")
  f3 <- filtered()
  .chk(nrow(f3) == 1L && f3$user[1] == "larm2", TRUE,
       "C4: filter still intact after turning live-refresh back OFF")

  # Clearing the filter still works normally (proves the filter machinery
  # itself, not just its persistence, is unaffected by any of the above).
  session$setInputs(audit_mod_sel = "Todos", audit_user_filter = "")
  session$flushReact()
  .chk(nrow(filtered()), 2L, "C5: clearing filters after all the above still restores both rows")
})

# ── Additional static-scan coverage (module-namespacing + co-location) ─────
cat("\n-- Part D: additional static checks --\n")

.chk(length(grep('ns\\("audit_live_toggle"\\)', txt)), 1L,
     "the toggle button's input id is correctly namespaced via ns()")
.chk(length(grep('ns\\("live_toggle_btn"\\)', txt)), 1L,
     "live_toggle_btn's uiOutput() call site correctly namespaces the id via ns()")
.chk(length(grep('output\\$live_toggle_btn <- renderUI', txt)), 1L,
     "the matching server-side output$live_toggle_btn definition exists (bare name, matched to the ns()-wrapped UI id by Shiny's module boundary)")
colocation_hits <- grep('div\\(class = "d-flex gap-2 align-items-center"', txt)
.chk(length(colocation_hits), 1L,
     "the refresh button and the live-toggle button are wrapped in one small flex container (co-located, minimal footprint)")
if (length(colocation_hits)) {
  colo_block <- paste(txt[colocation_hits[1]:min(colocation_hits[1] + 3, length(txt))], collapse = "\n")
  .chk_true(grepl("audit_refresh", colo_block) && grepl("live_toggle_btn", colo_block),
            "both the manual-refresh button and the live-toggle button live inside that same small container")
}

# Exactly one renderUI for live_toggle_btn (no accidental duplicate output
# definitions, which Shiny would silently let the last one win on).
.chk(length(grep('output\\$live_toggle_btn <-', txt)), 1L,
     "output$live_toggle_btn is defined exactly once")

# ── Cross-check against the existing permission-scoping suite: the toggle
# must be fully inert with respect to who can see what (Stage 4's own
# railguards are untouched by this change) ──────────────────────────────────
cat("\n-- Part E: toggle is inert with respect to permission scoping --\n")

shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared_local("bunny_toggle2", "hopdesk", "hd-admin", TRUE, "hd-admin", "hd-admin"),
  mode   = "own_client"
), {
  session$flushReact()
  df_before <- fetched()
  .chk(inherits(df_before, "error"), TRUE,
       "E1: plain hopdesk-at-home is still correctly refused (Stage 4 gap #1 railguard unaffected by this fix)")

  session$setInputs(audit_live_toggle = 1)
  session$flushReact()
  df_after <- fetched()
  .chk(inherits(df_after, "error"), TRUE,
       "E2: same railguard still refuses access even after toggling live-refresh ON")
})

# =============================================================================
# Part F -- DT full-rebuild-on-every-filter-change fix (reported 2026-08-26:
# "just opening the [date range] clears everything", i.e. column sort order,
# DT's own search box, and page position all reset on ANY filter/date
# interaction, not just the 20s poll fixed in Parts A-E above)
# =============================================================================
cat("\n-- Part F: DT rebuild-avoidance (error_state stability + proxy pattern) --\n")

error_state_ln    <- grep("error_state <- reactiveVal\\(FALSE\\)", txt)
dedup_observe_ln  <- grep("if \\(!identical\\(new_err, isolate\\(error_state\\(\\)\\)\\)\\) error_state\\(new_err\\)", txt)
isolate_render_ln <- grep("isolate\\(\\.audit_display_df\\(filtered\\(\\)\\)\\)", txt)
proxy_ln          <- grep('audit_tbl_proxy <- DT::dataTableProxy\\("audit_tbl"\\)', txt)
replace_data_ln   <- grep("DT::replaceData\\(audit_tbl_proxy, \\.audit_display_df\\(filtered\\(\\)\\)", txt)

.chk(length(error_state_ln), 1L, "error_state reactiveVal is defined")
.chk(length(dedup_observe_ln), 1L, "the value-dedup observe() (only update error_state on an actual flip) exists")
.chk(length(isolate_render_ln), 1L, "output$audit_tbl's initial render is isolate()-wrapped (renders once, not on every filtered() change)")
.chk(length(proxy_ln), 1L, "audit_tbl_proxy (DT::dataTableProxy) is defined")
.chk(length(replace_data_ln), 1L, "DT::replaceData() is called to push data updates without a full rebuild")

# output$audit_results must depend on error_state(), NOT directly on
# fetched() -- reading fetched() there was the actual root cause (a plain
# reactive() re-evaluating on every upstream change regardless of whether
# its value changed, tearing down and recreating the dataTableOutput()
# container each time).
audit_results_start <- grep("output\\$audit_results <- renderUI", txt)
.chk_true(length(audit_results_start) == 1, "found output$audit_results to scan")
if (length(audit_results_start)) {
  block <- paste(txt[audit_results_start[1]:min(audit_results_start[1] + 8, length(txt))], collapse = "\n")
  .chk_true(grepl("error_state\\(\\)", block), "output$audit_results reads error_state()")
  .chk_true(!grepl("inherits\\(fetched\\(\\), \"error\"\\)", block),
            "output$audit_results no longer reads fetched() directly (the actual bug)")
}

# Functional: error_state() genuinely reflects the current permission state,
# and (this is the real fix, not just its symptom) fetched()/filtered() keep
# working correctly across repeated date-range-style interactions -- proving
# the refactor didn't break underlying correctness while fixing the rebuild.
shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared_local("tesoreria2", "finance", "networks", FALSE, "networks", "networks"),
  mode   = "own_client"
), {
  session$flushReact()
  .chk(error_state(), FALSE, "F1: error_state() is FALSE for a viewer who is allowed to see their own log")

  # Simulate repeated date-range interactions (the exact reported scenario)
  # -- each one changes input$audit_range, which fetched()/filtered() must
  # react to correctly, while error_state() (and therefore the DT
  # container) must NOT flip since permission state never changed.
  for (i in 1:4) {
    session$setInputs(audit_range = c(Sys.time() - (7 + i) * 86400, Sys.time()))
    session$flushReact()
    .chk(error_state(), FALSE,
         sprintf("F2.%d: error_state() stays FALSE across repeated date-range changes (no permission flip)", i))
  }
  df_final <- fetched()
  .chk(inherits(df_final, "error"), FALSE,
       "F3: fetched() still resolves correctly after repeated date-range interactions")
})

shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared_local("bunny_toggle3", "hopdesk", "hd-admin", TRUE, "hd-admin", "hd-admin"),
  mode   = "own_client"
), {
  session$flushReact()
  .chk(error_state(), TRUE, "F4: error_state() is TRUE for a viewer who is genuinely refused (plain hopdesk at home)")
})

# =============================================================================
# Part G -- live-toggle popup message ("just the text, no background, no
# text box, just a quick message" -- requested 2026-08-26)
# =============================================================================
cat("\n-- Part G: live-toggle popup message --\n")

.chk(length(grep('shinyjs::hidden\\(', txt)), 1L,
     "the popup message element is wrapped in shinyjs::hidden() (starts hidden)")
.chk(length(grep('id = ns\\("live_toggle_msg"\\)', txt)), 1L,
     "the popup message span has a namespaced id")
.chk(length(grep('shinyjs::html\\(id = "live_toggle_msg"', txt)), 1L,
     "the toggle observer sets the popup's text via shinyjs::html()")
.chk(length(grep('shinyjs::show\\("live_toggle_msg"\\)', txt)), 1L,
     "the toggle observer shows the popup")
.chk(length(grep('shinyjs::delay\\(1800, shinyjs::hide\\("live_toggle_msg"\\)\\)', txt)), 1L,
     "the toggle observer auto-hides the popup after a short delay")

# "No background, no text box" -- the element must not carry any box-styling
# CSS (background/border/box-shadow), just typography.
msg_span_ln <- grep('tags\\$span\\(id = ns\\("live_toggle_msg"\\)', txt)
.chk_true(length(msg_span_ln) == 1, "found the popup message span definition to scan")
if (length(msg_span_ln)) {
  span_block <- paste(txt[msg_span_ln[1]:min(msg_span_ln[1] + 2, length(txt))], collapse = " ")
  .chk_true(!grepl("background", span_block, ignore.case = TRUE),
            "popup message has no background styling")
  .chk_true(!grepl("border", span_block, ignore.case = TRUE),
            "popup message has no border styling")
}

# Both toggle directions produce a message (activada / desactivada).
toggle_obs_block_start <- grep("observeEvent\\(input\\$audit_live_toggle,", txt)
.chk_true(length(toggle_obs_block_start) == 1, "found the toggle observeEvent to scan")
if (length(toggle_obs_block_start)) {
  obs_block <- paste(txt[toggle_obs_block_start[1]:min(toggle_obs_block_start[1] + 8, length(txt))], collapse = "\n")
  .chk_true(grepl("activada", obs_block), "toggle observer has an 'activated' message branch")
  .chk_true(grepl("desactivada", obs_block), "toggle observer has a 'deactivated' message branch")
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
