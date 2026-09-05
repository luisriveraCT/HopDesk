# =============================================================================
# R/audit_sticky_ui.R
#
# Shared sticky toolbar/header treatment for the two audit-log DT tables
# (R/audit_log_viewer_module.R's Actividad, R/pagos_audit_viewer_module.R's
# "Ver archivos de pago") -- added 2026-08-27 per explicit request: the
# filter bar and column headers should stay pinned in view while scrolling a
# long table, the same way R/vencidos_module.R's own toolbar/header already
# do (see that file's #ven_sticky_header + .ven-section-band/thead CSS for
# the pattern this mirrors). Vencidos renders a hand-built HTML table, not a
# DT one, so its exact CSS/JS doesn't transplant directly onto a
# DT::dataTableOutput() table -- this reproduces the same effect (sticky
# toolbar, sticky thead pinned just below it, via a CSS custom property kept
# in sync with the toolbar's live height) against DT's own generated markup.
#
# Usage, inside a module's output$panel <- renderUI({ ... }):
#   tagList(
#     tags$style(HTML(.AUDIT_STICKY_CSS)),
#     div(id = ns("sticky_scope"),
#       div(id = ns("sticky_toolbar"), class = "audit-sticky-toolbar", ...toolbar content...),
#       div(class = "audit-sticky-table", DT::dataTableOutput(ns("my_tbl")))
#     ),
#     audit_sticky_js(ns("sticky_toolbar"), ns("sticky_scope"))
#   )
# Also set language = list(search = "") in the table's DT options -- the CSS
# below replaces the "Filtrar:" text label with an icon baked into the input
# box itself, so the label text must be blanked at the source or both would
# show together.
# =============================================================================

.AUDIT_STICKY_CSS <- "
  .audit-sticky-toolbar {
    position: sticky;
    top: 0;
    z-index: 200;
    background: #fff;
    padding: 0.5rem 0 0.75rem;
    margin-bottom: 0.25rem;
    border-bottom: 1px solid #dee2e6;
  }
  .audit-sticky-table thead th {
    position: sticky;
    top: var(--audit-toolbar-h, 92px);
    background: #fff;
    z-index: 150;
    cursor: pointer;
  }
  .audit-sticky-table .dataTables_filter label {
    margin-bottom: 0;
  }
  /* Wraps the actual <input> (see ensureAuditSearchIcon() in
     audit_sticky_js() below) -- positioned relative to THIS wrapper, not
     DT's own .dataTables_filter div (which also contains the <label> and,
     depending on DT's markup, left the icon floating beside the box
     instead of inside it). */
  .audit-search-wrap {
    position: relative;
    display: inline-flex;
    align-items: center;
  }
  .audit-search-wrap input[type=search] {
    padding-left: 28px !important;
    min-width: 170px;
  }
  .audit-search-wrap .audit-search-icon {
    position: absolute;
    left: 9px;
    top: 50%;
    transform: translateY(-50%);
    font-size: 12px;
    /* App's own primary accent (R/ui_components.R's #0d6efd), muted -- a
       real Font Awesome glyph via JS (see ensureAuditSearchIcon()), not an
       emoji, matching every other icon in the app (icon('rotate'),
       icon('file-pdf'), ...). */
    color: rgba(13, 110, 253, 0.55);
    pointer-events: none;
  }
"

# Keeps --audit-toolbar-h (read by .audit-sticky-table thead th's `top`
# above) in sync with the actual gap between the toolbar and the table,
# via ResizeObserver plus a few staggered one-off retries (fonts/badges/
# pills can still be settling their layout a beat after first paint) and a
# window resize listener (filters wrap to a second line on a narrow
# viewport). toolbar_id/scope_id must already be ns()-qualified by the
# caller -- this file has no module of its own.
#
# BUG FIXED 2026-08-27: this used to set the CSS var to just the toolbar's
# OWN height. DT auto-generates its own ".dataTables_filter" search box as
# a sibling sitting BETWEEN our toolbar and the actual <table> -- not part
# of the toolbar element this measures -- so the thead's sticky `top` ended
# up too small by exactly that search box's height. The thead then "stuck"
# too early: as the user scrolled, it froze in place while still overlapping
# that not-yet-scrolled-away search box, and every row that scrolled up
# afterward appeared to pass BEHIND/UNDER the now-misplaced header --
# reported as the header appearing to "duplicate" or float in the middle of
# the data. Fixed by measuring the actual layout gap between the toolbar and
# the <table> itself (whatever DT puts in between), using offsetTop/
# offsetHeight rather than getBoundingClientRect(): the former reflect an
# element's position in normal layout flow and are NOT affected by
# position:sticky's paint-time displacement once an element is actually
# stuck, while the latter would report the STUCK (already-offset) position
# and silently corrupt this exact calculation for any recalc that fires
# after the user has already scrolled (a window resize mid-scroll, for one).
# SECOND FIX (found 2026-08-28, after the offsetTop/offsetHeight fix above
# still reproduced the same visual bug live): the DT table itself renders
# ASYNCHRONOUSLY -- output$audit_results (R/audit_log_viewer_module.R) is
# its own renderUI that only resolves once fetched()'s data pipeline
# completes, which can easily take longer than this script's whole
# [0..1000]ms retry schedule, especially against a just-cold-started
# server instance. When every retry fires before the <table> exists yet,
# `gap` is silently measured as 0 (querySelector finds nothing) and
# --audit-toolbar-h locks in at just the toolbar's height -- reproducing
# the exact original bug, just for a different reason (missing element,
# not a wrong measurement). Fixed by reacting to the table actually
# appearing via MutationObserver on the scope element, the same "watch for
# it, don't just guess a timeout" principle R/vencidos_module.R's own
# venRecalcStickyOffsets() re-render hook already uses (see that file's
# $(document).on('shiny:value', ...) call) -- generalized here to any DOM
# change inside our own scope, debounced, so it needs no knowledge of this
# specific output's name.
audit_sticky_js <- function(toolbar_id, scope_id) {
  tags$script(HTML(sprintf("
    (function() {
      var auditRecalcTimer = null;
      function recalcAuditSticky() {
        var t = document.getElementById(%s);
        var s = document.getElementById(%s);
        if (!t || !s) return;
        var toolbarH = Math.ceil(t.offsetHeight);
        var gap = 0;
        var table = s.querySelector('table.dataTable');
        if (table) {
          gap = Math.max(0, Math.ceil(table.offsetTop - (t.offsetTop + t.offsetHeight)));
        }
        var total = toolbarH + gap;
        if (total > 0) s.style.setProperty('--audit-toolbar-h', total + 'px');
      }
      // Wraps DT's own auto-generated search <input> in a positioned span
      // and injects a REAL Font Awesome icon (matching every other icon in
      // the app -- icon('rotate'), icon('file-pdf'), ...) as its sibling,
      // instead of a CSS ::before on the outer .dataTables_filter div (that
      // positioned relative to the wrong element -- DT's <label> wraps the
      // input with its own box, so the icon landed beside the search box,
      // not inside it). Idempotent (checks .audit-search-wrap first) so
      // re-running it on every recalc/mutation is a harmless no-op once done.
      function ensureAuditSearchIcon() {
        var s = document.getElementById(%s);
        if (!s) return;
        var input = s.querySelector('.dataTables_filter input[type=search]');
        if (!input || input.parentElement.classList.contains('audit-search-wrap')) return;
        var wrap = document.createElement('span');
        wrap.className = 'audit-search-wrap';
        input.parentNode.insertBefore(wrap, input);
        wrap.appendChild(input);
        var icon = document.createElement('i');
        icon.className = 'fas fa-magnifying-glass audit-search-icon';
        icon.setAttribute('aria-hidden', 'true');
        wrap.insertBefore(icon, input);
      }
      function scheduleAuditRecalc() {
        if (auditRecalcTimer) clearTimeout(auditRecalcTimer);
        auditRecalcTimer = setTimeout(function() {
          recalcAuditSticky();
          ensureAuditSearchIcon();
        }, 30);
      }
      [0, 60, 200, 500, 1000, 2000, 4000].forEach(function(ms) {
        setTimeout(function() { recalcAuditSticky(); ensureAuditSearchIcon(); }, ms);
      });
      var t0 = document.getElementById(%s);
      if (t0 && window.ResizeObserver) {
        new ResizeObserver(recalcAuditSticky).observe(t0);
      }
      var s0 = document.getElementById(%s);
      if (s0 && window.MutationObserver) {
        // Catches the DT table's own (async) first render, any later
        // replaceData()-driven DOM churn, and a permission-alert <-> table
        // swap -- all without needing to name a specific Shiny output.
        new MutationObserver(scheduleAuditRecalc).observe(s0, {childList: true, subtree: true});
      }
      window.addEventListener('resize', recalcAuditSticky);
      // Continuous self-heal (added 2026-09-05, ported from
      // R/vencidos_module.R's own venRecalcStickyOffsets() -- that table
      // never reproduces the 'header stuck too early / floats mid-table'
      // bug this file's history above describes fixing twice, and the one
      // thing its sticky mechanism does that this one didn't is recalc on
      // every scroll, not just on a fixed timer schedule + DOM mutations.
      // If EITHER of those retry mechanisms ever measures a wrong value
      // (e.g. the tab was still hidden/mid-transition when they fired --
      // Shiny suspends a hidden tab's renderUI, so the very first render
      // can land at an awkward moment relative to the tab's own
      // shown.bs.tab animation), nothing before this correction was ever
      // able to fix it afterward -- the wrong CSS var just persisted for
      // the rest of the session. Cheap (recalcAuditSticky() itself is
      // just a couple of offset reads) and rAF-throttled, so this costs
      // nothing during normal scrolling.
      var _auditScrollTicking = false;
      window.addEventListener('scroll', function() {
        if (_auditScrollTicking) return;
        _auditScrollTicking = true;
        requestAnimationFrame(function() { recalcAuditSticky(); _auditScrollTicking = false; });
      }, true);
    })();
  ", jsonlite::toJSON(toolbar_id, auto_unbox = TRUE),
     jsonlite::toJSON(scope_id, auto_unbox = TRUE),
     jsonlite::toJSON(scope_id, auto_unbox = TRUE),
     jsonlite::toJSON(toolbar_id, auto_unbox = TRUE),
     jsonlite::toJSON(scope_id, auto_unbox = TRUE))))
}
