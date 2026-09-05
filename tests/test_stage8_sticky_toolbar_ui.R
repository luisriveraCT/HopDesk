# =============================================================================
# tests/test_stage8_sticky_toolbar_ui.R
#
# UI request (2026-08-27): the filter/toolbar bar on both audit tables
# (Actividad -- R/audit_log_viewer_module.R -- and "Ver archivos de pago" --
# R/pagos_audit_viewer_module.R) should stay pinned in view while scrolling
# a long table, mirroring R/vencidos_module.R's own sticky toolbar; the "Ver
# detalle" trigger on the payment-file table should sit in that sticky bar
# instead of below the table; and DT's built-in "Filtrar:" search label
# should become a plain icon baked into the input box.
#
# SCOPE NARROWED 2026-09-05: this originally ALSO tried to make DT's own
# column headers sticky (a --audit-toolbar-h CSS var, kept in sync via
# ResizeObserver/MutationObserver/a retry schedule). Three straight live
# attempts at that (2026-08-27, 2026-08-28, 2026-09-05) each either didn't
# hold up or made the symptom worse -- see R/audit_sticky_ui.R's own
# history comment for the full account. Confirmed directly (asked, chose
# this) rather than guessed a fourth time: only the toolbar is sticky now.
# Column headers scroll away normally, like a plain table.
#
# This is a structural/static test file (no visual rendering harness is
# available here) -- it verifies the shared CSS/JS helper's content and
# that both consuming modules actually wire it in and blank DT's search
# label, matching what a real browser needs to render correctly.
#
# Sourced by _run_saas.R (which now also sources R/audit_sticky_ui.R before
# R/audit_log_viewer_module.R).
# =============================================================================

cat("── Sticky toolbar/header UI (Actividad + Ver archivos de pago) ────────────\n")

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
.chk_true  <- function(cond, label) .chk(isTRUE(cond), TRUE, label)
.chk_false <- function(cond, label) .chk(isTRUE(cond), FALSE, label)

# =============================================================================
# Part A -- .AUDIT_STICKY_CSS content
# =============================================================================
cat("\n-- Part A: .AUDIT_STICKY_CSS --\n")

.chk_true(grepl("\\.audit-sticky-toolbar", .AUDIT_STICKY_CSS), "defines .audit-sticky-toolbar")
.chk_true(grepl("position:\\s*sticky", .AUDIT_STICKY_CSS), "toolbar rule uses position: sticky")
.chk_true(grepl("dataTables_filter", .AUDIT_STICKY_CSS), "styles DT's own .dataTables_filter search wrapper")

# Regression guard (scope narrowed 2026-09-05): DT's own column headers are
# NOT made sticky anymore -- three straight live attempts at that (see
# R/audit_sticky_ui.R's own history comment) either didn't hold up or made
# the symptom worse, because DT renders its <thead> on its own async timer
# that this code can't reliably observe. Only the toolbar (a plain
# `top: 0`, no JS measurement) is sticky now.
.chk_false(grepl("\\.audit-sticky-table thead th", .AUDIT_STICKY_CSS),
     "no longer defines a sticky rule for thead th -- column headers are not pinned")
.chk_false(grepl("var\\(--audit-toolbar-h", .AUDIT_STICKY_CSS),
     "no longer reads a --audit-toolbar-h custom property -- nothing needs the toolbar's height anymore")

# Regression guard (found 2026-08-28): the search icon used to be a plain
# emoji baked into CSS `content` on the OUTER .dataTables_filter div, which
# (a) doesn't match the app's Font Awesome icon set used everywhere else
# (icon('rotate'), icon('file-pdf'), ...), and (b) positioned relative to
# the wrong element -- DT's <label> wraps the actual <input> with its own
# box, so the icon rendered beside the search box, not inside it. Fixed
# with a real <i class="fas fa-magnifying-glass"> injected via JS into a
# purpose-built wrapper around the actual input (see Part B).
.chk_false(grepl("🔍", .AUDIT_STICKY_CSS, fixed = TRUE),
     "no emoji left in the CSS -- the search icon is a real Font Awesome glyph now")
.chk_true(grepl("\\.audit-search-wrap", .AUDIT_STICKY_CSS),
     "defines .audit-search-wrap -- a purpose-built wrapper around the actual <input>, not the outer filter div")
.chk_true(grepl("0d6efd", .AUDIT_STICKY_CSS, fixed = TRUE),
     "search icon color uses the app's own primary accent (#0d6efd, R/ui_components.R), not a default/emoji color")

# =============================================================================
# Part B -- audit_sticky_js()
# =============================================================================
cat("\n-- Part B: audit_sticky_js() --\n")

js_tag <- audit_sticky_js("myns-sticky_scope")
.chk_true(inherits(js_tag, "shiny.tag"), "audit_sticky_js() returns a tag (a <script> block)")
js_txt <- paste(as.character(js_tag), collapse = "\n")
.chk_true(grepl("myns-sticky_scope", js_txt, fixed = TRUE), "emitted script references the given scope id")

# Regression guard (scope narrowed 2026-09-05): no height/offset
# measurement logic survives in the JS anymore -- three straight live
# attempts at keeping DT's own <thead> pinned below the toolbar (wrong
# measurement 2026-08-27, DT-not-rendered-yet 2026-08-28, a scroll-heal
# that made it worse 2026-09-05) were all reverted rather than patched a
# fourth time; the toolbar's own `top: 0` needs none of this.
.chk_false(grepl("ResizeObserver", js_txt, fixed = TRUE), "no ResizeObserver -- nothing depends on toolbar height anymore")
.chk_false(grepl("--audit-toolbar-h", js_txt, fixed = TRUE), "no --audit-toolbar-h custom property is set")
.chk_false(grepl("addEventListener\\('resize'", js_txt), "no window resize listener -- no offset to recompute on resize")
.chk_false(grepl("table.dataTable", js_txt, fixed = TRUE), "no longer measures any gap up to the <table> element")
.chk_false(grepl("offsetTop", js_txt, fixed = TRUE), "no offsetTop-based offset measurement")
.chk_false(grepl("getBoundingClientRect", js_txt, fixed = TRUE),
     "no getBoundingClientRect() usage -- the scroll-heal attempt that used it was reverted")

# The search-icon injection has nothing to do with sticky positioning --
# DT auto-generates its own ".dataTables_filter" search box asynchronously,
# so this still needs a retry schedule + MutationObserver to catch that,
# independent of the (now-removed) offset logic above.
.chk_true(grepl("MutationObserver", js_txt, fixed = TRUE),
     "watches for the search box's own (async) appearance via MutationObserver, not just a fixed retry schedule")
.chk_true(grepl("childList: true, subtree: true", js_txt, fixed = TRUE),
     "MutationObserver watches the whole scope subtree (catches the search box appearing anywhere inside it)")
.chk_true(grepl("4000", js_txt, fixed = TRUE),
     "retry schedule extended to cover a slower/cold-started initial load, alongside the MutationObserver")

# Regression guard (found 2026-08-28): the search icon must be a real,
# correctly-positioned Font Awesome glyph, not an emoji floating outside
# the input box.
.chk_true(grepl("ensureAuditSearchIcon", js_txt, fixed = TRUE),
     "defines a dedicated function to inject the search icon")
.chk_true(grepl("fas fa-magnifying-glass", js_txt, fixed = TRUE),
     "injects the same Font Awesome icon class shiny::icon('magnifying-glass') itself produces in this app")
.chk_false(grepl("\U0001F50D", js_txt),
     "no emoji magnifying glass character anywhere in the script")
.chk_true(grepl("audit-search-wrap", js_txt, fixed = TRUE),
     "wraps the actual <input> (not DT's outer .dataTables_filter div) so the icon positions correctly inside the box")
.chk_true(grepl("classList.contains\\('audit-search-wrap'\\)", js_txt),
     "icon injection is idempotent -- checks whether the input is already wrapped before wrapping again")
.chk_true(length(gregexpr("ensureAuditSearchIcon", js_txt, fixed = TRUE)[[1]]) >= 3,
     "icon injection is wired into both the retry schedule AND the MutationObserver-driven retry")

# IDs are passed through jsonlite::toJSON for safe embedding -- confirm a
# value containing a single quote or other JS-special character can't break
# the generated script (defensive, since ns() ids are usually simple but
# this must never be assumed).
ok_special <- tryCatch({
  t <- audit_sticky_js("d'e\"f")
  is.character(paste(as.character(t), collapse = ""))
}, error = function(e) FALSE)
.chk_true(ok_special, "audit_sticky_js() does not error on ids containing quote characters")

# =============================================================================
# Part C -- both modules actually wire the helper in
# =============================================================================
cat("\n-- Part C: module wiring --\n")

audit_txt <- paste(readLines("R/audit_log_viewer_module.R", warn = FALSE), collapse = "\n")
pagos_txt <- paste(readLines("R/pagos_audit_viewer_module.R", warn = FALSE), collapse = "\n")

for (nm_txt in list(list(name = "audit_log_viewer_module.R", txt = audit_txt),
                    list(name = "pagos_audit_viewer_module.R", txt = pagos_txt))) {
  .chk_true(grepl("\\.AUDIT_STICKY_CSS", nm_txt$txt),
       sprintf("%s references .AUDIT_STICKY_CSS", nm_txt$name))
  .chk_true(grepl("audit_sticky_js\\(", nm_txt$txt),
       sprintf("%s calls audit_sticky_js()", nm_txt$name))
  .chk_true(grepl('ns\\("sticky_scope"\\)', nm_txt$txt),
       sprintf("%s defines a sticky_scope element", nm_txt$name))
  .chk_true(grepl('ns\\("sticky_toolbar"\\)', nm_txt$txt),
       sprintf("%s defines a sticky_toolbar element", nm_txt$name))
  .chk_true(grepl('class = "audit-sticky-table"', nm_txt$txt),
       sprintf("%s wraps its table output in .audit-sticky-table", nm_txt$name))
}

# DT's "Filtrar:" label must be blanked in both modules' actual DT options
# (the CSS icon-in-box treatment only makes sense once the text label is
# gone -- otherwise both would show together).
.chk_false(grepl('search\\s*=\\s*"Filtrar:"', audit_txt),
     "audit_log_viewer_module.R: no remaining search = \"Filtrar:\" in its DT options")
.chk_false(grepl('search\\s*=\\s*"Filtrar:"', pagos_txt),
     "pagos_audit_viewer_module.R: no remaining search = \"Filtrar:\" in its DT options")
.chk_true(grepl('search\\s*=\\s*""', audit_txt), "audit_log_viewer_module.R: search label explicitly blanked")
.chk_true(grepl('search\\s*=\\s*""', pagos_txt), "pagos_audit_viewer_module.R: search label explicitly blanked")

# The "Ver detalle" trigger must now live inside the sticky toolbar, not
# below the table -- checked structurally: detail_btn_ui's uiOutput() call
# must appear BEFORE the DT::dataTableOutput() call in the source (matches
# "moved into the toolbar" the way this file is laid out), not after it.
detail_pos <- regexpr('uiOutput\\(ns\\("detail_btn_ui"\\)', pagos_txt)
table_pos  <- regexpr('DT::dataTableOutput\\(ns\\("pagos_tbl"\\)\\)', pagos_txt)
.chk_true(detail_pos > 0 && table_pos > 0 && detail_pos < table_pos,
     "pagos_audit_viewer_module.R: the 'Ver detalle' trigger's UI call now appears before (i.e. inside the toolbar, above) the table output, not after it")

cat(sprintf("\n  Stage 8 subtotal: %d passed, %d failed\n", .pass, .fail))
