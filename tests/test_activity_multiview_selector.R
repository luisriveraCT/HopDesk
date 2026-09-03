# =============================================================================
# tests/test_activity_multiview_selector.R
#
# Stage 1 of the Bancos/Pasivos activity-logging extension (2026-09-03):
# generalizes the Activity tab's binary actividad<->pagos toggle
# (R/tiers_module.R) into a data-driven .ACTIVITY_VIEWS registry, so future
# stages (Bancos, Pasivos) can add a view by appending ONE list entry
# instead of restructuring the toggle/UI-dispatch logic again.
#
# Sourced by _run_saas.R.
# =============================================================================

cat("── Activity tab multi-view selector (Stage 1) ──────────────────────────\n")

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

source("R/pagos_generados_persistence.R", local = FALSE)
source("R/audit_log_viewer_module.R",     local = FALSE)  # .audit_viewer_resolve_perms(), auditLogViewerUI
source("R/pagos_audit_viewer_module.R",   local = FALSE)  # .payment_audit_can_view, pagosAuditViewerUI
source("R/tiers_module.R",                local = FALSE)  # .ACTIVITY_VIEWS

# =============================================================================
# Part A -- .ACTIVITY_VIEWS registry shape
# =============================================================================
cat("\n-- Part A: registry shape --\n")

.chk_true(exists(".ACTIVITY_VIEWS"), ".ACTIVITY_VIEWS is defined")
.chk_true(is.list(.ACTIVITY_VIEWS) && length(.ACTIVITY_VIEWS) >= 2,
          ".ACTIVITY_VIEWS has at least the 2 views that existed before this refactor (actividad, pagos)")

ids <- vapply(.ACTIVITY_VIEWS, `[[`, character(1), "id")
.chk(anyDuplicated(ids), 0L, "no two views share the same id (would collide on both the reactiveVal value and the button input id)")
.chk_true("actividad" %in% ids, "the 'actividad' view is registered")
.chk_true("pagos" %in% ids, "the 'pagos' view is registered")

for (v in .ACTIVITY_VIEWS) {
  .chk_true(is.character(v$id) && nzchar(v$id), sprintf("view '%s': id is a non-empty string", v$id %||% "?"))
  .chk_true(is.character(v$label) && nzchar(v$label), sprintf("view '%s': label is a non-empty string", v$id))
  .chk_true(is.character(v$icon) && nzchar(v$icon), sprintf("view '%s': icon is a non-empty string", v$id))
  .chk_true(is.function(v$can_view), sprintf("view '%s': can_view is a function", v$id))
  .chk_true(is.function(v$ui), sprintf("view '%s': ui is a function", v$id))
  .chk_true(length(formals(v$can_view)) == 1L, sprintf("view '%s': can_view takes exactly one argument (shared)", v$id))
  .chk_true(length(formals(v$ui)) == 1L, sprintf("view '%s': ui takes exactly one argument (ns)", v$id))
}

actividad_entry <- Find(function(v) identical(v$id, "actividad"), .ACTIVITY_VIEWS)
pagos_entry     <- Find(function(v) identical(v$id, "pagos"),     .ACTIVITY_VIEWS)

.chk(actividad_entry$can_view(list()), TRUE,
     "the 'actividad' view's can_view() is unconditionally TRUE (always available, matching pre-refactor behavior)")

# ── ui() functions actually call the real UI constructors, not a stand-in ──
ns_stub <- function(x) paste0("test-", x)
actividad_ui <- actividad_entry$ui(ns_stub)
pagos_ui     <- pagos_entry$ui(ns_stub)
.chk_true(!is.null(actividad_ui), "actividad's ui() returns something (auditLogViewerUI didn't error)")
.chk_true(!is.null(pagos_ui), "pagos' ui() returns something (pagosAuditViewerUI didn't error)")
.chk_true(!identical(actividad_ui, pagos_ui), "the two views' ui() outputs are genuinely different (not accidentally both wired to the same UI)")

# =============================================================================
# Part B -- can_view() behavior matches .payment_audit_can_view() exactly
# (regression coverage: the registry must not silently diverge from the
# permission function it wraps)
# =============================================================================
cat("\n-- Part B: pagos view's can_view() matches .payment_audit_can_view() --\n")

mk_shared <- function(is_staff, tier = "finance", permisos_json = "{}") {
  list(
    current_user_info = function() list(user = "u", tier = tier, client_id = "networks", is_staff = is_staff),
    home_client_id      = function() "networks",
    effective_client_id = function() "networks"
  )
}

mock_s3saveRDS(data.frame(username = "amv_test_dev", tier = "dev", permisos = "{}", stringsAsFactors = FALSE),
              "networks/usuarios.rds", "mock-bucket")

staff_shared <- mk_shared(TRUE)
.chk(pagos_entry$can_view(staff_shared), .payment_audit_can_view(staff_shared),
     "pagos view's can_view() agrees with .payment_audit_can_view() for a staff session")

finance_shared <- list(
  current_user_info = function() list(user = "amv_test_fin", tier = "finance", client_id = "networks", is_staff = FALSE),
  home_client_id      = function() "networks",
  effective_client_id = function() "networks"
)
mock_s3saveRDS(data.frame(username = "amv_test_fin", tier = "finance", permisos = "{}", stringsAsFactors = FALSE),
              "networks/usuarios.rds", "mock-bucket")
.chk(pagos_entry$can_view(finance_shared), .payment_audit_can_view(finance_shared),
     "pagos view's can_view() agrees with .payment_audit_can_view() for a non-staff finance session (both FALSE)")

# =============================================================================
# Part C -- functional: the real visible-filtering + fallback logic,
# extracted from the actual app.R/tiers_module.R source (not a hand-copied
# reimplementation) and evaluated against synthetic view lists
# =============================================================================
cat("\n-- Part C: visible-view filtering + fallback-to-actividad logic --\n")

tm_txt <- readLines("R/tiers_module.R", warn = FALSE)
.chk_true({p <- tryCatch({parse("R/tiers_module.R"); TRUE}, error = function(e) FALSE); p},
          "R/tiers_module.R still parses without syntax errors")

filter_start <- grep("visible <- Filter\\(function\\(v\\) isTRUE\\(tryCatch\\(v\\$can_view\\(shared\\)", tm_txt)
.chk_true(length(filter_start) == 1, "found the visible-filtering block to extract")
if (length(filter_start)) {
  fallback_end <- grep("view_entry <- Find\\(function\\(v\\) identical\\(v\\$id, view_id\\), \\.ACTIVITY_VIEWS\\)", tm_txt)
  block_text <- paste(tm_txt[filter_start[1]:fallback_end[1]], collapse = "\n")

  run_logic <- function(views, shared, current_view) {
    env <- new.env()
    env$.ACTIVITY_VIEWS <- views
    env$shared <- shared
    env$activity_view <- function() current_view
    eval(parse(text = block_text), envir = env)
    list(view_id = env$view_id, view_entry = env$view_entry, visible = env$visible)
  }

  fake_views <- list(
    list(id = "actividad", label = "Actividad", icon = "x", can_view = function(shared) TRUE, ui = function(ns) "UI-actividad"),
    list(id = "pagos",     label = "Pagos",     icon = "x", can_view = function(shared) TRUE, ui = function(ns) "UI-pagos"),
    list(id = "bancos",    label = "Bancos",    icon = "x", can_view = function(shared) FALSE, ui = function(ns) "UI-bancos")
  )

  r1 <- run_logic(fake_views, list(), "pagos")
  .chk(r1$view_id, "pagos", "C1: selecting a visible, permitted view keeps it selected")
  .chk(r1$view_entry$ui(NULL), "UI-pagos", "C1: the correct view's ui() is resolved")
  .chk(length(r1$visible), 2L, "C1: exactly the 2 permitted views (actividad, pagos) are visible -- bancos (can_view=FALSE) is excluded")

  r2 <- run_logic(fake_views, list(), "bancos")
  .chk(r2$view_id, "actividad",
       "C2: FALLBACK -- selecting a view whose can_view() is FALSE falls back to actividad, not a blank/broken view")
  .chk(r2$view_entry$ui(NULL), "UI-actividad", "C2: the fallback correctly resolves actividad's own ui()")

  r3 <- run_logic(fake_views, list(), "nonexistent_id")
  .chk(r3$view_id, "actividad",
       "C3: selecting an id that doesn't exist in the registry at all also falls back to actividad, not an error")

  all_hidden_views <- list(
    list(id = "actividad", label = "A", icon = "x", can_view = function(shared) TRUE, ui = function(ns) "UI-a")
  )
  r4 <- run_logic(all_hidden_views, list(), "actividad")
  .chk(length(r4$visible), 1L, "C4: with only one view ever visible, exactly one is returned")

  # A can_view() that itself throws must degrade to "not visible", never
  # crash the whole Activity tab render.
  throwing_views <- list(
    list(id = "actividad", label = "A", icon = "x", can_view = function(shared) TRUE, ui = function(ns) "UI-a"),
    list(id = "broken", label = "B", icon = "x", can_view = function(shared) stop("boom"), ui = function(ns) "UI-b")
  )
  r5 <- tryCatch(run_logic(throwing_views, list(), "broken"), error = function(e) e)
  .chk_true(!inherits(r5, "error"), "C5: a view whose can_view() throws does not crash the render")
  if (!inherits(r5, "error")) {
    .chk(length(r5$visible), 1L, "C5: the throwing view is excluded from visible (treated as not permitted, not shown broken)")
    .chk(r5$view_id, "actividad", "C5: falls back to actividad when the selected view's can_view() threw")
  }
}

cat(sprintf("\n  Subtotal: %d passed, %d failed\n", .pass, .fail))
