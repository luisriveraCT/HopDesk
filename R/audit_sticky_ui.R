# =============================================================================
# R/audit_sticky_ui.R
#
# Shared sticky-toolbar treatment for the two audit-log DT tables
# (R/audit_log_viewer_module.R's Actividad, R/pagos_audit_viewer_module.R's
# "Ver archivos de pago") -- added 2026-08-27 per explicit request: the
# filter bar should stay pinned in view while scrolling a long table, the
# same way R/vencidos_module.R's own toolbar already does.
#
# SCOPE NARROWED 2026-09-05 (see the file-history note further down for the
# full story): this ONLY makes the filter toolbar sticky now, not DT's own
# column headers too. Column headers scroll away normally, like a plain
# table -- re-scroll up to see them again. That's a deliberate, confirmed
# trade-off (asked directly, chose this), not an oversight: three straight
# attempts at also pinning DT's own auto-generated <thead> all failed or
# made things worse live, because DT initializes that markup on its OWN
# async timer, entirely separate from Shiny's DOM insertion -- a race that
# simply doesn't exist for Vencidos' hand-built table (no DT widget to wait
# on at all), which is why "just copy Vencidos" doesn't transplant as
# directly as it sounds. The toolbar itself was NEVER the buggy part in any
# of those three attempts (it's a fixed `top: 0`, no JS measurement needed)
# -- only the thead sync was, so dropping just that piece removes the
# actual root cause instead of patching around it a fourth time.
#
# Usage, inside a module's output$panel <- renderUI({ ... }):
#   tagList(
#     tags$style(HTML(.AUDIT_STICKY_CSS)),
#     div(id = ns("sticky_scope"),
#       div(id = ns("sticky_toolbar"), class = "audit-sticky-toolbar", ...toolbar content...),
#       div(class = "audit-sticky-table", DT::dataTableOutput(ns("my_tbl")))
#     ),
#     audit_sticky_js(ns("sticky_scope"))
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

# HISTORY (kept for the next person who's tempted to re-add thead
# stickiness): this used to ALSO keep a --audit-toolbar-h CSS custom
# property in sync with the toolbar's height, so DT's own <thead> could be
# position:sticky too, pinned just below the toolbar. That went through
# three real attempts -- wrong height measurement (2026-08-27), DT's
# thead not existing yet when every retry fired because it renders on its
# OWN async timer, separate from Shiny's DOM insertion (2026-08-28), and a
# scroll-triggered recalc ported from Vencidos that made it WORSE live
# instead of better (2026-09-05) -- before landing on the actual fix:
# don't try to pin an element whose render timing this code can't observe
# reliably. Scope narrowed the same day to just the toolbar (a plain
# `top: 0`, no JS measurement, never the buggy part in any of the three
# attempts) -- confirmed directly rather than guessed a fourth time.
#
# What's left here is just the search-icon injection, which has nothing to
# do with sticky positioning: DT auto-generates its own ".dataTables_filter"
# search box asynchronously (same DT-render-timing fact as above), so
# injecting a real Font Awesome icon into it (instead of a CSS ::before,
# which positioned relative to the wrong wrapper element) still needs a
# retry schedule + MutationObserver to catch that async render -- it just
# no longer needs to also compute or react to anything about height/scroll.
audit_sticky_js <- function(scope_id) {
  tags$script(HTML(sprintf("
    (function() {
      // Wraps DT's own auto-generated search <input> in a positioned span
      // and injects a REAL Font Awesome icon (matching every other icon in
      // the app -- icon('rotate'), icon('file-pdf'), ...) as its sibling,
      // instead of a CSS ::before on the outer .dataTables_filter div (that
      // positioned relative to the wrong element -- DT's <label> wraps the
      // input with its own box, so the icon landed beside the search box,
      // not inside it). Idempotent (checks .audit-search-wrap first) so
      // re-running it on every retry/mutation is a harmless no-op once done.
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
      var iconRetryTimer = null;
      function scheduleIconRetry() {
        if (iconRetryTimer) clearTimeout(iconRetryTimer);
        iconRetryTimer = setTimeout(ensureAuditSearchIcon, 30);
      }
      [0, 60, 200, 500, 1000, 2000, 4000].forEach(function(ms) {
        setTimeout(ensureAuditSearchIcon, ms);
      });
      var s0 = document.getElementById(%s);
      if (s0 && window.MutationObserver) {
        // Catches the DT table's own (async) first render, any later
        // replaceData()-driven DOM churn, and a permission-alert <-> table
        // swap -- all without needing to name a specific Shiny output.
        new MutationObserver(scheduleIconRetry).observe(s0, {childList: true, subtree: true});
      }
    })();
  ", jsonlite::toJSON(scope_id, auto_unbox = TRUE),
     jsonlite::toJSON(scope_id, auto_unbox = TRUE))))
}
