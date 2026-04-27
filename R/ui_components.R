# =============================================================================
# R/ui_components.R
# Shared UI building blocks used across the app.
# Pure functions that return Shiny tag objects — no reactives, no server logic.
# =============================================================================

# ── Notes panel ───────────────────────────────────────────────────────────────
# Renders the collapsible notes panel for a given ledger.
# Called from renderUI in notes_handlers.R

notes_panel_ui <- function(notes_df, ledger, ns_prefix) {
  if (!nrow(notes_df)) {
    return(div(
      class = "notes-panel p-3",
      tags$em(class = "text-muted", "— Sin notas para este mes —")
    ))
  }

  div(
    class = "notes-panel p-3",
    lapply(seq_len(nrow(notes_df)), function(i) {
      n <- notes_df[i, ]
      div(
        class = "note-item mb-2 p-2 border rounded",
        div(class = "d-flex justify-content-between align-items-start",
          tags$strong(n$title %||% "(Sin título)"),
          div(class = "d-flex gap-2",
            actionLink(paste0("edit_note_", ns_prefix, "_", n$id),
                       icon("pencil"), class = "text-muted small"),
            actionLink(paste0("delete_note_", ns_prefix, "_", n$id),
                       icon("trash"), class = "text-danger small")
          )
        ),
        if (nzchar(n$body %||% ""))
          tags$p(class = "mb-0 mt-1 small text-muted",
                 style = "white-space: pre-wrap;", n$body)
      )
    })
  )
}

# ── Note edit modal ────────────────────────────────────────────────────────────

note_edit_modal <- function(ledger, note_row = NULL, token = NULL) {
  is_new  <- is.null(note_row)
  ns_pre  <- tolower(ledger)
  token   <- token %||% uuid::UUIDgenerate()

  modalDialog(
    title     = if (is_new) "Nueva nota" else "Editar nota",
    easyClose = TRUE,
    footer    = tagList(
      actionButton(paste0("cancel_note_", ns_pre, "_", token), "Cancelar"),
      actionButton(paste0("save_note_",   ns_pre, "_", token), "Guardar",
                   class = "btn-primary")
    ),
    textInput(
      paste0("note_title_", ns_pre),
      "Título",
      value = if (!is_new) note_row$title %||% "" else ""
    ),
    textAreaInput(
      paste0("note_body_", ns_pre),
      "Detalle",
      rows  = 6,
      value = if (!is_new) note_row$body %||% "" else ""
    ),
    # Return token so caller can wire the save/cancel observers
    tags$input(type = "hidden", id = paste0("note_token_", ns_pre), value = token)
  )
}

# ── Manual entry modal ─────────────────────────────────────────────────────────

manual_entry_modal <- function(ledger, empresas, row = NULL, token = NULL) {
  is_new    <- is.null(row)
  ns_pre    <- tolower(ledger)
  token     <- token %||% uuid::UUIDgenerate()   # fallback if not passed
  party_lbl <- if (ledger == "AR") "Cliente" else "Proveedor"

  modalDialog(
    title     = if (is_new) paste("Nueva entrada manual –", ledger)
                else        paste("Editar entrada –", ledger),
    size      = "l",
    easyClose = TRUE,
    footer    = tagList(
      if (!is_new)
        actionButton(paste0("delete_manual_", ns_pre, "_", token), "Eliminar",
                     class = "btn btn-danger me-auto"),
      actionButton(paste0("cancel_manual_", ns_pre, "_", token), "Cancelar"),
      actionButton(paste0("save_manual_",   ns_pre, "_", token), "Guardar",
                   class = "btn-primary")
    ),
    fluidRow(
      column(6,
        selectInput(paste0("manual_empresa_", ns_pre), "Empresa",
                    choices  = empresas,
                    selected = if (!is_new) row$Empresa else empresas[1]),
        selectInput(paste0("manual_moneda_", ns_pre), "Moneda",
                    choices  = CURRENCIES,
                    selected = if (!is_new) row$Moneda else "MXN"),
        textInput(paste0("manual_documento_", ns_pre), "Documento (referencia única)",
                  value = if (!is_new) row$Documento %||% "" else ""),
        textInput(paste0("manual_factura_", ns_pre), "No. Factura",
                  value = if (!is_new) row$Factura %||% "" else "")
      ),
      column(6,
        textInput(paste0("manual_parte_", ns_pre), party_lbl,
                  value = if (!is_new) row$Parte %||% "" else ""),
        textInput(paste0("manual_codigo_", ns_pre), paste("Código de", party_lbl),
                  value = if (!is_new) row$Codigo %||% "" else ""),
        numericInput(paste0("manual_importe_", ns_pre), "Importe",
                     value = if (!is_new) row$Importe %||% 0 else 0, min = 0)
      )
    ),
    fluidRow(
      column(6,
        dateInput(paste0("manual_fecha_", ns_pre), "Fecha de vencimiento",
                  value    = if (!is_new) row$`Fecha de vencimiento` else Sys.Date(),
                  weekstart = 1, language = "es")
      ),
      column(6,
        textAreaInput(paste0("manual_notas_", ns_pre), "Notas",
                      rows  = 3,
                      value = if (!is_new) row$Notas %||% "" else "")
      )
    ),
    # Visual distinction badge
    div(class = "mt-2",
      tags$span(class = "badge bg-warning text-dark", "✎ Entrada manual"),
      tags$small(class = "text-muted ms-2",
                 "No se enviará a SAP. Solo visible en esta app.")
    )
  )
}

# ── Inline CSS (injected once into the UI head) ───────────────────────────────

app_styles <- function() {
  tags$style(HTML("

    /* ── Control bar ─────────────────────────────────────────────────────── */
    .control-bar {
      border-bottom: 1px solid #dee2e6;
      background: #fff;
      position: sticky;
      top: 0;
      z-index: 100;
    }
    .control-item { min-width: 120px; }

    /* ── Empresa toggle buttons ──────────────────────────────────────────── */
    .emp-toggle-btn {
      display: inline-flex; align-items: center;
      padding: 3px 12px;
      font-size: 0.78rem; font-weight: 500;
      border: 1px solid #c5d0e6;
      border-radius: 20px;
      background: #fff;
      color: #4a5568;
      cursor: pointer;
      transition: background 0.13s, border-color 0.13s, color 0.13s;
      white-space: nowrap;
      line-height: 1.6;
      user-select: none;
    }
    .emp-toggle-btn:hover {
      background: #f0f4ff;
      border-color: #0A58CA;
      color: #0A58CA;
    }
    .emp-toggle-btn.active {
      background: #e8f0fe;
      border-color: #0A58CA;
      color: #0A58CA;
      font-weight: 600;
    }
    .emp-toggle-btn.active.snapshot {
      background: #fff3e0;
      border-color: #e67e22;
      color: #e67e22;
      font-weight: 600;
    }

    /* ── Snapshot tooltip ────────────────────────────────────────────────── */
    .emp-tooltip-wrap {
      position: relative;
      display: inline-block;
    }
    .emp-tooltip {
      visibility: hidden;
      opacity: 0;
      pointer-events: none;
      position: absolute;
      top: calc(100% + 7px);
      left: 50%;
      transform: translateX(-50%);
      background: #fff8f0;
      border: 1px solid #f0a05a;
      color: #7a3d00;
      border-radius: 8px;
      padding: 7px 11px;
      font-size: 0.75rem;
      line-height: 1.5;
      white-space: nowrap;
      box-shadow: 0 3px 10px rgba(0,0,0,0.10);
      z-index: 9999;
      transition: opacity 0.15s;
    }
    .emp-tooltip::before {
      content: \"\";
      position: absolute;
      top: -6px; left: 50%;
      transform: translateX(-50%);
      border-width: 0 6px 6px;
      border-style: solid;
      border-color: transparent transparent #f0a05a;
    }
    .emp-tooltip::after {
      content: \"\";
      position: absolute;
      top: -5px; left: 50%;
      transform: translateX(-50%);
      border-width: 0 5px 5px;
      border-style: solid;
      border-color: transparent transparent #fff8f0;
    }
    .emp-tooltip-wrap:hover .emp-tooltip {
      visibility: visible;
      opacity: 1;
    }

    /* ── Calendar container fills remaining height ───────────────────────── */
    .calendar-container {
      width: 100%;
      height: calc(100vh - 120px);
      min-height: 500px;
      overflow-y: auto;
    }

    /* ── Notes toggle bar pinned to bottom ───────────────────────────────── */
    .notes-toggle-bar {
      position: fixed;
      bottom: 0; left: 0; right: 0;
      background: #f8f9fa;
      border-top: 1px solid #dee2e6;
      padding: 6px 16px;
      z-index: 100;
    }

    /* ── Notes panel slides up over the calendar ─────────────────────────── */
    .notes-panel-wrapper {
      position: fixed;
      bottom: 36px; left: 0; right: 0;
      max-height: 320px;
      overflow-y: auto;
      background: #fff;
      border-top: 1px solid #dee2e6;
      box-shadow: 0 -2px 8px rgba(0,0,0,.08);
      z-index: 99;
    }

    /* ── Modal summary bar ───────────────────────────────────────────────── */
    .modal-summary-bar {
      background: #f8f9fa;
      border-radius: 6px;
      padding: 8px 12px;
      margin-bottom: 8px;
    }

    /* ── Note items ──────────────────────────────────────────────────────── */
    .note-item { background: #fffdf0; border-color: #ffe58f !important; }

    /* ── Loading overlay ─────────────────────────────────────────────────── */
    .sap-loading-overlay {
      position: absolute; inset: 0;
      background: rgba(255,255,255,0.85);
      display: flex; flex-direction: column;
      align-items: center; justify-content: center;
      z-index: 20; font-size: 1.1rem; color: #0A58CA; gap: 12px;
    }

    /* ════════════════════════════════════════════════════════════════════════
       HTML CALENDAR GRID
    ════════════════════════════════════════════════════════════════════════ */

    /* Title bar */
    .cal-title-bar {
      display: flex; align-items: baseline;
      justify-content: space-between; gap: 12px;
      margin-bottom: 10px; flex-wrap: wrap;
    }
    .cal-title-left { display: flex; align-items: center; gap: 10px; }
    .cal-title {
      font-size: 1.25rem; font-weight: 700; color: #C0392B; margin: 0;
    }
    .cal-title-hint { font-size: 0.78rem; color: #888; white-space: nowrap; }
    .cal-ledger-badge {
      font-size: 0.7rem; font-weight: 700;
      padding: 2px 8px; border-radius: 12px;
      background: #0A58CA; color: white;
      letter-spacing: 0.04em; flex-shrink: 0;
    }

    /* Day-of-week header */
    .cal-dow-row {
      display: grid; grid-template-columns: repeat(7, 1fr); gap: 3px; margin-bottom: 3px;
    }
    .cal-dow {
      text-align: center; font-size: 0.72rem; font-weight: 700;
      color: #555; padding: 4px 0;
      text-transform: uppercase; letter-spacing: 0.05em;
    }
    .cal-dow--weekend { color: #9ba8b5; }

    /* Grid layout */
    .cal-grid-wrapper { width: 100%; }
    .cal-grid { display: flex; flex-direction: column; gap: 3px; }
    .cal-week { display: grid; grid-template-columns: repeat(7, 1fr); gap: 3px; }

    /* Day tiles */
    .cal-tile {
      min-height: 120px;
      border: 1px solid #D6E0F0;
      border-radius: 6px;
      padding: 7px 8px 6px;
      background: #fff;
      display: flex; flex-direction: column;
      box-sizing: border-box;
      transition: box-shadow 0.12s, border-color 0.12s;
      overflow: hidden;
    }
    .cal-tile--empty  { background: transparent; border-color: transparent; }
    .cal-tile--weekend { background: #F6F9FF; border-color: #DDE7F7; }
    .cal-tile--has-data { background: #FDFEFF; border-color: #9DB8E8; }
    .cal-tile--has-data:hover {
      box-shadow: 0 3px 14px rgba(10,88,202,0.15);
      border-color: #0A58CA; z-index: 2;
    }
    .cal-tile--today  { border: 2.5px solid #0A58CA !important; background: #EFF5FF; }
    .cal-tile--urgent    { background: #FFF5F5 !important; border-color: #E57373 !important; }
    .cal-tile--important { background: #FFFBF0 !important; border-color: #F0C040 !important; }
    .cal-tile--both      { background: #FFF3EC !important; border-color: #E8874A !important; }

    /* En proceso — tile has staged invoices — very light indicator only */
    .cal-tile--en-proceso {
      opacity: 0.93;
      border-color: #B8CCDE !important;
      border-style: dashed !important;
    }
    .cal-tile--en-proceso.cal-tile--today {
      border-style: solid !important;
    }
    .cal-badge--staged {
      background: #E8F4FD;
      color: #1a6cc4;
      border: 1px solid #9DB8E8;
      font-size: 0.6rem;
      font-weight: 700;
      padding: 2px 5px;
      border-radius: 8px;
      white-space: nowrap;
    }

    /* Tile header */
    .cal-tile-header {
      display: flex; align-items: flex-start;
      justify-content: space-between; margin-bottom: 4px; gap: 4px;
    }
    .cal-daynum { font-size: 1rem; font-weight: 700; color: #C0392B; line-height: 1; flex-shrink: 0; }
    .cal-tile--today .cal-daynum { color: #0A58CA; }
    .cal-tile--weekend .cal-daynum { color: #9ba8b5; }

    /* Tag badges */
    .cal-badge {
      font-size: 0.6rem; font-weight: 700;
      padding: 2px 5px; border-radius: 8px;
      white-space: nowrap; letter-spacing: 0.02em; line-height: 1.4;
    }
    .cal-badge--urgent    { background: #FDECEA; color: #C62828; border: 1px solid #E57373; }
    .cal-badge--important { background: #FFF8E1; color: #F57F17; border: 1px solid #F0C040; }
    .cal-badge--both      { background: #FFF0E6; color: #BF360C; border: 1px solid #E8874A; }

    /* Invoice rows */
    .cal-rows { flex: 1; display: flex; flex-direction: column; gap: 2px; overflow: hidden; min-height: 0; }
    .cal-row {
      display: flex; align-items: baseline;
      justify-content: space-between; gap: 4px;
      font-size: 0.7rem; line-height: 1.35; min-width: 0;
    }
    .cal-party {
      color: #2c3e50; font-weight: 500;
      flex: 1; min-width: 0;
      overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
    }
    .cal-amount {
      color: #1a6cc4; font-weight: 600;
      font-variant-numeric: tabular-nums;
      flex-shrink: 0; font-size: 0.68rem;
    }
    .cal-amount--partial { color: #6c8ebf; }
    .cal-abono-icon {
      font-size: 0.6rem; vertical-align: middle;
      margin-right: 2px; opacity: 0.75;
      cursor: default;
    }
    .cal-more { font-size: 0.63rem; color: #888; font-style: italic; margin-top: 1px; }

    /* Total row */
    .cal-divider { height: 1px; background: #d0dff7; margin: 4px 0 3px; }
    .cal-total {
      display: flex; justify-content: space-between;
      font-size: 0.7rem; font-weight: 700; color: #0A58CA;
    }

    /* Modal context pills */
    .modal-day-header { display: flex; flex-wrap: wrap; gap: 8px; align-items: center; margin-bottom: 10px; }
    .modal-day-pill {
      font-size: 0.75rem; padding: 3px 10px;
      border-radius: 12px; font-weight: 600;
    }
    .modal-day-pill--date   { background: #EFF5FF; color: #0A58CA; border: 1px solid #9DB8E8; }
    .modal-day-pill--ledger { background: #0A58CA; color: white; }
    .modal-day-pill--cur    { background: #f1f3f5; color: #333; border: 1px solid #ccc; }
    .modal-day-pill--saldo  { background: #e9f7ef; color: #155724; border: 1px solid #a3d9b3; }
    .modal-day-pill--abono  { background: #fff3cd; color: #856404; border: 1px solid #ffc107; }

    /* ── Cart modal styles ───────────────────────────────────────────────── */
    .cart-row {
      border-bottom: 1px solid #f0f0f0;
      padding: 8px 4px;
    }
    .cart-row:last-child { border-bottom: none; }
    .cart-party {
      font-weight: 500;
      color: #2c3e50;
      font-size: 0.9rem;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    .cart-empresa-badge {
      font-size: 0.65rem;
      font-weight: 700;
      padding: 1px 6px;
      border-radius: 8px;
      background: #EFF5FF;
      color: #0A58CA;
      border: 1px solid #9DB8E8;
      flex-shrink: 0;
    }
    .cart-amount {
      font-weight: 700;
      color: #1a6cc4;
      font-variant-numeric: tabular-nums;
      white-space: nowrap;
      flex-shrink: 0;
      min-width: 100px;
      text-align: right;
    }
    .cart-btn { min-width: 90px; flex-shrink: 0; }
    .cart-expand-btn { line-height: 1.2 !important; font-size: 0.75rem !important; color: #1a6cc4 !important; }
    .cart-expand-btn:hover { color: #0a3d7a !important; }
    .cart-expand-lbl { font-size: 0.72rem; color: #1a6cc4; }
    .cart-inv-list {
      background: #ffffff;
      border-left: 3px solid #9DB8E8;
      margin: -4px 0 6px 0;
      border-radius: 0 0 6px 6px;
      box-shadow: inset 0 2px 4px rgba(0,0,0,0.04);
    }
    .cart-inv-row {
      padding: 5px 0;
      border-bottom: 1px solid #f0f4fb;
      font-size: 0.8rem;
    }
    .cart-inv-row:last-child { border-bottom: none; }
    .cart-inv-doc { color: #2c3e50; font-weight: 500; font-variant-numeric: tabular-nums; min-width: 80px; }
    .cart-inv-amt { font-weight: 700; color: #0A58CA; font-variant-numeric: tabular-nums; white-space: nowrap; }
    .cart-inv-btn { min-width: 32px; padding: 1px 8px !important; font-size: 0.8rem !important; flex-shrink: 0; }
    .cart-btn.btn-success {
      background-color: #28a745;
      border-color: #28a745;
      color: white;
    }
    .cart-list { max-height: 420px; overflow-y: auto; }
    .cart-total-staged {
      background: #EFF5FF;
      border: 1px solid #9DB8E8;
      color: #0A58CA;
      font-size: 0.85rem;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    /* ── Pagar Hoy ───────────────────────────────────────────────────────── */
    .ph-container { overflow: hidden; }

    .ph-summary-bar {
      background: #fff;
      border-bottom: 1px solid #dee2e6;
      flex-shrink: 0;
    }

    .ph-company-panel {
      max-height: calc(100vh - 180px);
      overflow-y: auto;
    }

    .ph-balance-row {
      background: #f8f9fa;
      border: 1px solid #dee2e6;
    }

    .ph-balance-calc {
      min-width: 180px;
      padding-left: 1rem;
      border-left: 2px solid #dee2e6;
    }

    /* ── Agenda de Hoy — supplier catalog match coloring ─────────────────── */

    tr.ph-row-ok      { background-color: rgba(25, 135, 84,  0.06) !important; }
    tr.ph-row-partial { background-color: rgba(255, 193, 7,  0.10) !important; }
    tr.ph-row-nomatch { background-color: rgba(220, 53,  69, 0.07) !important; }

    .ph-dot {
      display: inline-block;
      width: 8px; height: 8px;
      border-radius: 50%;
      margin-right: 6px;
      vertical-align: middle;
      flex-shrink: 0;
    }
    .ph-dot-ok      { background-color: #198754; }
    .ph-dot-partial { background-color: #ffc107; }
    .ph-dot-nomatch { background-color: #dc3545; }

    .ph-alias-badge {
      font-size: 0.68rem;
      padding: 1px 5px;
      margin-left: 4px;
      vertical-align: middle;
      font-weight: 500;
      border-radius: 4px;
    }

    .ph-legend-dot {
      display: inline-block;
      width: 10px; height: 10px;
      border-radius: 50%;
      flex-shrink: 0;
    }
    .ph-ok      { background-color: #198754; }
    .ph-partial { background-color: #ffc107; }
    .ph-nomatch { background-color: #dc3545; }

    /* ── Settings hub ────────────────────────────────────────────────────── */

    .settings-hub { min-height: 420px; }

    .settings-sidebar .btn {
      justify-content: flex-start;
      text-align: left;
      font-size: 0.82rem;
    }
    .settings-sidebar .btn.active,
    .settings-sidebar .btn:focus {
      background-color: #e7f0ff;
      border-color: #0A58CA;
      color: #0A58CA;
    }

    .settings-content {
      max-height: 520px;
      overflow-y: auto;
    }

    .catalogo-form { border-color: #dee2e6 !important; }

    #cat_alias_counter { display: block; margin-top: 2px; }

    .font-monospace {
      font-family: 'Courier New', monospace;
      font-size: 0.78rem;
      letter-spacing: 0.03em;
    }

    /* ── Panel Tiers ── */
    .tiers-header {
      background: linear-gradient(120deg, #0d1b3e 0%, #1b3d7a 100%);
      color: #fff;
      padding: 20px 28px 16px;
      border-radius: 10px;
      margin-bottom: 20px;
      display: flex;
      align-items: center;
      gap: 14px;
    }
    .tiers-dev-badge {
      font-size: 0.65rem;
      font-weight: 800;
      letter-spacing: 1.5px;
      text-transform: uppercase;
      background: rgba(255,255,255,0.15);
      border: 1px solid rgba(255,255,255,0.3);
      padding: 3px 10px;
      border-radius: 20px;
    }
    .tiers-perm-row {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 7px 12px;
      border-bottom: 1px solid #f0f0f0;
      font-size: 0.88rem;
    }
    .tiers-perm-row:last-child { border-bottom: none; }
    .tiers-perm-default { color: #9ca3af; font-size: 0.75rem; }
    .tiers-perm-override { color: #0a58ca; font-weight: 600; font-size: 0.75rem; }

    /* Config. de Tiers cards — collapse shiny wrapper margins so rows stay compact */
    .tiers-cfg-body .form-group { margin-bottom: 0 !important; }
    .tiers-cfg-body .checkbox    { margin: 0 !important; padding: 0 !important; }

  "))
}

# Initializes Bootstrap 5 tooltips after every Shiny output update.
# Cal tiles use data-bs-toggle="tooltip" for the abono breakdown.
app_scripts <- function() {
  tags$script(HTML("
    $(document).on('shiny:value', function() {
      requestAnimationFrame(function() {
        document.querySelectorAll('[data-bs-toggle=\"tooltip\"]:not(.tt-init)')
          .forEach(function(el) {
            el.classList.add('tt-init');
            new bootstrap.Tooltip(el, {container: 'body', html: true});
          });
      });
    });
  "))
}