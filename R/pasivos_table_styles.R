# =============================================================================
# R/pasivos_table_styles.R
# Stage 3: CSS class assignment per cell state + shared CSS for the Pasivos table.
# Pure functions — no I/O, no reactives.
# =============================================================================

# ── pasivos_cell_classes ──────────────────────────────────────────────────────
# Returns a character vector of CSS classes for a single cell payload.
#
# cell_payload : list with cell_kind, fecha, cell_has_override, cell_overdue_severity
# today        : Date (default Sys.Date())
pasivos_cell_classes <- function(cell_payload, today = Sys.Date()) {
  kind     <- cell_payload$cell_kind %||% ""
  fecha    <- tryCatch(as.Date(cell_payload$fecha), error = function(e) NA)
  override <- isTRUE_safe(cell_payload$cell_has_override)

  base <- "pasivos-cell"

  kind_cls <- switch(kind,
    provision = {
      cls <- c("pasivos-cell-provision")
      if (!is.na(fecha)) {
        if (fecha == today)      cls <- c(cls, "pasivos-cell-due-today")
      }
      cls
    },
    overdue_provision = c("pasivos-cell-provision", "pasivos-cell-overdue-amber"),
    manual_item = {
      cls <- c("pasivos-cell-item-pending")
      if (!is.na(fecha) && fecha == today) cls <- c(cls, "pasivos-cell-due-today")
      cls
    },
    overdue_manual    = c("pasivos-cell-item-pending", "pasivos-cell-overdue-red"),
    confirmed_item    = c("pasivos-cell-item-confirmed"),
    character(0)
  )

  classes <- c(base, kind_cls)
  if (override) classes <- c(classes, "pasivos-cell-has-override")
  classes
}


# ── pasivos_table_css ─────────────────────────────────────────────────────────
# Returns a <style> tag containing all Pasivos table CSS.
# Called once from pasivos_table_module_ui() or appended to app_styles().
pasivos_table_css <- function() {
  shiny::tags$style(shiny::HTML("
/* ── Pasivos table container ── */
.pasivos-tbl-col {
  display: flex;
  flex-direction: column;
  max-height: calc(100vh - 220px);
  overflow: hidden;
}
.pasivos-table-wrap {
  flex: 1 1 auto;
  min-height: 0;
  overflow-x: scroll;
  overflow-y: auto;
}
/* Hide native horizontal scrollbar — the .pasivos-scroll-bar below replaces it */
.pasivos-table-wrap::-webkit-scrollbar:horizontal { height: 0; }
.pasivos-scroll-bar {
  flex: 0 0 auto;
  overflow-x: scroll;
  overflow-y: hidden;
  height: 14px;
  background: #f8fafc;
  border-top: 1px solid #e5e7eb;
}
.pasivos-scroll-bar-inner { height: 1px; }
.pasivos-table {
  border-collapse: separate;
  border-spacing: 0;
  font-size: 11.5px;
}
.pasivos-table th,
.pasivos-table td {
  padding: 4px 7px;
  border-right: 1px solid #e5e7eb;
  border-bottom: 1px solid #e5e7eb;
  white-space: nowrap;
  vertical-align: top;
}
.pasivos-table thead th {
  position: sticky;
  top: 0;
  background: #f9fafb;
  z-index: 2;
}
.pasivos-table .pasivos-meta-col {
  position: sticky;
  left: 0;
  background: #ffffff;
  z-index: 1;
}
.pasivos-table thead th.pasivos-meta-col { z-index: 3; }

/* ── Row highlight for overdue rows ── */
.pasivos-table tr.has-overdue {
  background-color: rgba(220, 38, 38, 0.06);
}
.pasivos-table tr.has-overdue td.pasivos-meta-col {
  border-left: 4px solid #dc2626;
  background-color: #fdf2f2;
}

/* ── Row highlight for due-soon rows (due within 3–7 days) ── */
.pasivos-table tr.has-due-soon {
  background-color: rgba(245, 158, 11, 0.07);
}
.pasivos-table tr.has-due-soon td.pasivos-meta-col {
  border-left: 4px solid #f59e0b;
  background-color: #fef9f0;
}

/* ── Row highlight for very-soon rows (due today, tomorrow, or day-after) ── */
.pasivos-table tr.has-very-soon {
  background-color: rgba(245, 158, 11, 0.18);
}
.pasivos-table tr.has-very-soon td.pasivos-meta-col {
  border-left: 4px solid #d97706;
  background-color: #fdf3dc;
}

/* ── Cell base ── */
.pasivos-cell { font-variant-numeric: tabular-nums; }

/* ── Cell states ── */
.pasivos-cell-provision        { color: #6d28d9; font-style: italic; }
.pasivos-cell-item-pending     { color: #b45309; }
.pasivos-cell-item-confirmed   { color: #065f46; }
.pasivos-cell-due-today        { background: #fef3c7; font-weight: 600; }
.pasivos-cell-overdue-amber    { background: #fffbeb; color: #d97706; font-weight: 600; }
.pasivos-cell-overdue-red      { background: #fef2f2; color: #b91c1c; font-weight: 700; }
.pasivos-cell-has-override::after {
  content: '';
  position: absolute;
  top: 4px; right: 4px;
  width: 6px; height: 6px;
  border-radius: 50%;
  background: #6b7280;
}

/* ── Today column highlight ── */
.pasivos-table-today-col { background: #f0f9ff; }
.pasivos-table thead th.pasivos-table-today-col {
  background: #dbeafe;
  color: #1d4ed8;
  font-weight: 700;
  border-bottom: 2px solid #3b82f6;
}

/* ── Convert bolt button ── */
.pasivos-convert-btn {
  background: none;
  border: none;
  cursor: pointer;
  padding: 0 2px;
  font-size: 14px;
  line-height: 1;
}
.pasivos-convert-btn:hover { opacity: .7; }

/* ── Window extension buttons ── */
.pasivos-extend-btn {
  font-size: 12px;
  padding: 2px 8px;
}

/* ── Group expand caret ── */
.pasivos-caret {
  cursor: pointer;
  user-select: none;
  margin-right: 4px;
  font-size: 11px;
}

/* ── Stage 4: pencil row-edit button ── */
.pasivos-row-edit-btn {
  display: inline-block;
  width: 14px;
  height: 14px;
  font-size: 10px;
  background: transparent;
  color: #7c3aed;
  border: none;
  cursor: pointer;
  padding: 0;
  margin-right: 4px;
  opacity: 0.7;
  line-height: 1;
}
.pasivos-row-edit-btn:hover { opacity: 1; }

/* ── Fee sub-row ── */
.pasivos-fee-row td {
  background-color: #faf5ff;
  border-bottom: 1px dashed #e9d5ff;
}
.pasivos-fee-row td.pasivos-meta-col {
  background-color: #faf5ff;
}

/* ── Currency group separator row ── */
.pasivos-currency-group-row td {
  vertical-align: middle;
  border-bottom: 1px solid #e5e7eb;
}
.pasivos-currency-group-cell {
  background: #f8fafc !important;
  padding: 3px 10px !important;
}

/* ── Currency pill on each row ── */
.pasivos-cur-pill {
  display: inline-block;
  font-size: 9px;
  font-weight: 600;
  line-height: 1;
  padding: 2px 5px;
  border-radius: 3px;
  color: #fff;
  vertical-align: middle;
  margin-left: 4px;
  letter-spacing: .3px;
}
"))
}

# Returns a hex color for a given currency code (for badges, pills, separators).
.pasivos_cur_color <- function(cur) {
  switch(toupper(cur %||% ""),
    MXN = "#2563eb",
    USD = "#16a34a",
    EUR = "#7c3aed",
    "#6b7280"
  )
}

# Returns currencies sorted in canonical display order (MXN, USD, EUR, then rest).
.pasivos_cur_order <- function(currencies) {
  priority <- c("MXN", "USD", "EUR")
  up <- toupper(unique(currencies[!is.na(currencies)]))
  c(priority[priority %in% up], sort(up[!up %in% priority]))
}
