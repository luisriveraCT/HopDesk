# =============================================================================
# tests/test_stage8_sticky_toolbar_ui.R
#
# UI request (2026-08-27): the filter/toolbar bar and the DT column headers
# on both audit tables (Actividad -- R/audit_log_viewer_module.R -- and "Ver
# archivos de pago" -- R/pagos_audit_viewer_module.R) should stay pinned in
# view while scrolling a long table, mirroring R/vencidos_module.R's own
# sticky toolbar/header treatment; the "Ver detalle" trigger on the payment-
# file table should sit in that sticky bar instead of below the table; and
# DT's built-in "Filtrar:" search label should become a plain icon baked
# into the input box. Implemented via a new shared helper,
# R/audit_sticky_ui.R (.AUDIT_STICKY_CSS + audit_sticky_js()), since
# Vencidos itself renders a hand-built HTML table (no DT), so its exact
# code has no 1:1 equivalent to copy -- this reproduces the same visual
# effect against DT's own generated markup instead.
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
.chk_true(grepl("\\.audit-sticky-table thead th", .AUDIT_STICKY_CSS), "defines a sticky rule for thead th")
.chk_true(grepl("var\\(--audit-toolbar-h", .AUDIT_STICKY_CSS),
     "thead th's sticky offset reads the --audit-toolbar-h custom property (kept in sync by audit_sticky_js())")
.chk_true(grepl("dataTables_filter", .AUDIT_STICKY_CSS), "styles DT's own .dataTables_filter search wrapper")

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

js_tag <- audit_sticky_js("myns-sticky_toolbar", "myns-sticky_scope")
.chk_true(inherits(js_tag, "shiny.tag"), "audit_sticky_js() returns a tag (a <script> block)")
js_txt <- paste(as.character(js_tag), collapse = "\n")
.chk_true(grepl("myns-sticky_toolbar", js_txt, fixed = TRUE), "emitted script references the given toolbar id")
.chk_true(grepl("myns-sticky_scope", js_txt, fixed = TRUE), "emitted script references the given scope id")
.chk_true(grepl("ResizeObserver", js_txt, fixed = TRUE), "wires a ResizeObserver so a variable-height toolbar stays correctly offset")
.chk_true(grepl("--audit-toolbar-h", js_txt, fixed = TRUE), "sets the same CSS custom property the CSS rule reads")
.chk_true(grepl("addEventListener\\('resize'", js_txt),
     "recalculates on window resize (filters wrapping to a second line on a narrow viewport)")

# Regression guard (found 2026-08-27): the offset used to be just the
# toolbar's own height, ignoring DT's own auto-generated ".dataTables_filter"
# search box sitting between the toolbar and the <table> -- the thead then
# stuck too early and appeared to float/duplicate in the middle of the data
# as rows scrolled past underneath it. Fixed by measuring the actual gap up
# to the <table> itself, using offsetTop/offsetHeight (layout-based, NOT
# displaced by position:sticky's paint-time offset the way
# getBoundingClientRect() would be once an element is already stuck).
.chk_true(grepl("table.dataTable", js_txt, fixed = TRUE),
     "measures the gap up to the actual <table> element, not just the toolbar's own height")
.chk_true(grepl("offsetTop", js_txt, fixed = TRUE) && grepl("offsetHeight", js_txt, fixed = TRUE),
     "uses offsetTop/offsetHeight (layout-based) rather than getBoundingClientRect() (paint-based, corrupted once an element is sticky-stuck)")
.chk_false(grepl("getBoundingClientRect", js_txt, fixed = TRUE),
     "no longer uses getBoundingClientRect() for this measurement at all")

# Second regression guard (found 2026-08-28, live in production, after the
# offsetTop fix above still reproduced the same bug): the DT table renders
# ASYNCHRONOUSLY (its own data-dependent renderUI), so a fixed retry
# schedule can finish before the <table> exists in the DOM at all -- every
# retry then measures gap=0 and locks in the wrong offset, same visible bug
# for a different reason. Fixed with a MutationObserver reacting to the
# table actually appearing, the same "watch for it" principle
# R/vencidos_module.R's own re-render hook already uses.
.chk_true(grepl("MutationObserver", js_txt, fixed = TRUE),
     "watches for the table's own (async) appearance via MutationObserver, not just a fixed retry schedule")
.chk_true(grepl("childList: true, subtree: true", js_txt, fixed = TRUE),
     "MutationObserver watches the whole scope subtree (catches the table appearing anywhere inside it)")
.chk_true(grepl("4000", js_txt, fixed = TRUE),
     "retry schedule extended to cover a slower/cold-started initial load, alongside the MutationObserver")

# Third regression guard (found 2026-08-28, same live report): the search
# icon must be a real, correctly-positioned Font Awesome glyph, not an
# emoji floating outside the input box.
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
.chk_true(grepl("ensureAuditSearchIcon\\(\\);", js_txt) &&
     length(gregexpr("ensureAuditSearchIcon\\(\\)", js_txt)[[1]]) >= 3,
     "icon injection is wired into the same retry schedule AND the MutationObserver as the sticky-offset recalc (so it also survives the table's async render)")

# A scroll-triggered recalc (mirroring R/vencidos_module.R's own
# venRecalcStickyOffsets()) was tried here on 2026-09-05 and made the live
# symptom WORSE, not better -- reverted the same day rather than leaving
# an unverified guess in place. Left unresolved; see the module's own
# history above for what's already been tried and ruled out.

# IDs are passed through jsonlite::toJSON for safe embedding -- confirm a
# value containing a single quote or other JS-special character can't break
# the generated script (defensive, since ns() ids are usually simple but
# this must never be assumed).
ok_special <- tryCatch({
  t <- audit_sticky_js("a'b\"c", "d'e\"f")
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
