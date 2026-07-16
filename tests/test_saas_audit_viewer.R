# =============================================================================
# tests/test_saas_audit_viewer.R
# Stage 4 Part B/C: testServer coverage for R/audit_log_viewer_module.R —
# the shared component both Actividad and Bitácora Global are wired through.
# Sourced by _run_saas.R (which loads shiny/DT/shinyWidgets/bslib and
# R/audit_log_viewer_module.R before running this file).
# =============================================================================

cat("── Audit Log Viewer Module ─────────────────────────────────────────────\n")

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

# ── Seed usuarios for the viewer-own-perms lookup ────────────────────────────

.seed_usuario <- function(cid, username, tier, permisos_json = "{}") {
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

.seed_usuario("hd-admin", "bunny", "hopdesk")            # no override — tier defaults apply
.seed_usuario("hd-admin", "mouse", "principal")
.seed_usuario("networks", "tesoreria", "finance")

# Seed a couple of audit rows so fetched() has real data to filter.
Sys.setenv(CLIENT_ID = "networks")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "networks/app_audit.rds", "mock-bucket")
mock_s3saveRDS(.APP_AUDIT_SCHEMA(), "hd-admin/app_audit.rds", "mock-bucket")
log_action(user = "tesoreria", module = "bancos", action = "reconcile",
          description = "test entry", client_id = "networks")
# Jump/context-switch bookkeeping row — same shape app.R's context-switch
# observer writes into a client's own folder when staff jumps in.
log_action(user = "mouse", module = "clientes", action = "external_access",
          description = "HopDesk staff accessed this client's data (session context switch)",
          target_id = "networks", client_id = "networks")

.mock_shared <- function(user, tier, client_id, is_staff, home_cid, effective_cid) {
  list(
    current_user_info   = function() list(user = user, tier = tier, client_id = client_id,
                                          is_staff = is_staff),
    home_client_id       = function() home_cid,
    effective_client_id  = function() effective_cid
  )
}

# ── A. own_client mode: native client user at home — unaffected regression ──
# Also covers the newly-scoped fix: a native client viewer's own Actividad
# hides Hopdesk's jump/context-switch bookkeeping rows (module="clientes",
# action in {client_access, external_access}) but keeps substantive changes.

shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared("tesoreria", "finance", "networks", FALSE, "networks", "networks"),
  mode   = "own_client"
), {
  session$flushReact()
  df <- fetched()
  .chk(inherits(df, "error"), FALSE, "own_client: native client user's own log fetch does not error")
  .chk(is.data.frame(df) && nrow(df) >= 1L, TRUE, "own_client: native client user sees their own log rows")

  vis <- visible_fetched()
  .chk(any(vis$action == "reconcile"), TRUE,
       "own_client: native client viewer still sees substantive changes")
  .chk(any(vis$module == "clientes" & vis$action == "external_access"), FALSE,
       "own_client: native client viewer does not see jump/context-switch bookkeeping rows")
})

# ── A2. own_client mode: hopdesk staff MID-JUMP still sees bookkeeping rows ──
# The filter only applies to a native (non-staff) viewer's own view.

shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared("bunny", "hopdesk", "hd-admin", TRUE, "hd-admin", "networks"),
  mode   = "own_client"
), {
  session$flushReact()
  vis <- visible_fetched()
  .chk(any(vis$module == "clientes" & vis$action == "external_access"), TRUE,
       "own_client: staff mid-jump still sees jump/context-switch bookkeeping rows")
})

# ── B. own_client mode: hopdesk staff AT HOME — Stage 4 gap #1 fix ──────────
# effective_client_id == home_client_id == "hd-admin" (not jumped). A plain
# hopdesk viewer (no can_view_staff_audit_log override) must be refused.

shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared("bunny", "hopdesk", "hd-admin", TRUE, "hd-admin", "hd-admin"),
  mode   = "own_client"
), {
  session$flushReact()
  df <- fetched()
  .chk(inherits(df, "error"), TRUE,
       "own_client: plain hopdesk at home requesting hd-admin's own log -> error (gap #1 fixed)")
})

# ── C. own_client mode: hopdesk staff MID-JUMP — unchanged, correct behavior ─

shiny::testServer(auditLogViewerServer, args = list(
  shared = .mock_shared("bunny", "hopdesk", "hd-admin", TRUE, "hd-admin", "networks"),
  mode   = "own_client"
), {
  session$flushReact()
  df <- fetched()
  .chk(inherits(df, "error"), FALSE,
       "own_client: hopdesk mid-jump into networks sees that client's log without error")
})

# ── D. multi_client mode: plain hopdesk viewer never gets "Hopdesk (interno)" ─
# Doc's explicit automated test: assert absence from rendered choices, not
# just that selecting it would fail.

shiny::testServer(auditLogViewerServer, args = list(
  shared             = .mock_shared("bunny", "hopdesk", "hd-admin", TRUE, "hd-admin", "hd-admin"),
  mode               = "multi_client",
  allowed_client_ids = c(Networks = "networks"),
  include_staff_log  = function() {
    isTRUE(.audit_viewer_resolve_perms(
      .mock_shared("bunny", "hopdesk", "hd-admin", TRUE, "hd-admin", "hd-admin")
    )$can_view_staff_audit_log)
  }
), {
  choices <- client_choices()
  .chk("hd-admin" %in% choices, FALSE,
       "multi_client: plain hopdesk viewer's client_choices() never includes hd-admin")
  .chk("Hopdesk (interno)" %in% names(choices), FALSE,
       "multi_client: plain hopdesk viewer's client_choices() never labels 'Hopdesk (interno)'")
  .chk("networks" %in% choices, TRUE,
       "multi_client: plain hopdesk viewer still sees Networks as a selectable client")
})

# ── E. multi_client mode: principal viewer DOES get "Hopdesk (interno)" ─────

shiny::testServer(auditLogViewerServer, args = list(
  shared             = .mock_shared("mouse", "principal", "hd-admin", TRUE, "hd-admin", "hd-admin"),
  mode               = "multi_client",
  allowed_client_ids = c(Networks = "networks"),
  include_staff_log  = function() {
    isTRUE(.audit_viewer_resolve_perms(
      .mock_shared("mouse", "principal", "hd-admin", TRUE, "hd-admin", "hd-admin")
    )$can_view_staff_audit_log)
  }
), {
  choices <- client_choices()
  .chk("hd-admin" %in% choices, TRUE,
       "multi_client: principal viewer's client_choices() includes hd-admin")
  .chk("Hopdesk (interno)" %in% names(choices), TRUE,
       "multi_client: principal viewer's client_choices() labels it 'Hopdesk (interno)'")

  session$setInputs(audit_client_sel = "hd-admin")
  session$flushReact()
  df <- fetched()
  .chk(inherits(df, "error"), FALSE,
       "multi_client: principal selecting hd-admin succeeds (no railguard error)")
})

# ── F. multi_client mode: gate blocks a viewer without can_view_client_audit_logs ─

shiny::testServer(auditLogViewerServer, args = list(
  shared             = .mock_shared("tesoreria", "finance", "networks", FALSE, "networks", "networks"),
  mode               = "multi_client",
  allowed_client_ids = c(Networks = "networks"),
  include_staff_log  = function() FALSE
), {
  perms <- viewer_perms()
  .chk(isTRUE(perms$can_view_client_audit_logs), FALSE,
       "multi_client: finance-tier viewer has can_view_client_audit_logs=FALSE")
})

cat("\n")
