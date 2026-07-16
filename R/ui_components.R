# =============================================================================
# R/ui_components.R
# Shared UI building blocks used across the app.
# Pure functions that return Shiny tag objects — no reactives, no server logic.
# =============================================================================

# ── Notes panel ───────────────────────────────────────────────────────────────
# Renders the collapsible notes panel for a given ledger.
# Called from renderUI in notes_handlers.R

notes_panel_ui <- function(notes_df, current_user = "unknown", current_user_code = "") {
  if (!nrow(notes_df)) {
    return(div(
      class = "notes-panel p-3",
      tags$em(class = "text-muted", "— Sin notas para este mes —")
    ))
  }

  div(
    class = "notes-panel p-2",
    lapply(seq_len(nrow(notes_df)), function(i) {
      n           <- notes_df[i, ]
      vis         <- if (!is.na(n$visibility %||% NA_character_)) n$visibility else "public"
      is_personal <- identical(vis, "personal")
      item_cls    <- paste0("note-item mb-1 p-2 border rounded",
                            if (is_personal) " note-personal" else "")

      note_code <- { ac <- n$author_code %||% ""; if (is.na(ac)) "" else ac }
      note_auth <- { au <- n$author      %||% ""; if (is.na(au)) "" else au }
      is_author <-
        (nzchar(current_user_code) && nzchar(note_code) && current_user_code == note_code) ||
        (!nzchar(current_user_code) && !nzchar(note_code)) ||
        (nzchar(current_user) && current_user != "unknown" && nzchar(note_auth) &&
           note_auth == current_user)

      div(
        class = item_cls,
        div(class = "d-flex justify-content-between align-items-start",
          div(class = "d-flex align-items-center gap-1 flex-wrap",
            if (is_personal)
              tags$span(class = "badge note-badge-personal", "\U1F512 Personal")
            else
              tags$span(class = "badge note-badge-public",   "\U1F30D Pública"),
            tags$strong(class = "note-title", n$title %||% "(Sin título)")
          ),
          if (is_author)
            div(class = "d-flex gap-2",
              actionLink(paste0("edit_note_",   n$id),
                         icon("pencil"), class = "text-muted small"),
              actionLink(paste0("delete_note_", n$id),
                         icon("trash"), class = "text-danger small")
            )
          else
            NULL
        ),
        if (nzchar(n$body %||% ""))
          tags$p(class = "mb-0 mt-1 note-body",
                 style = "white-space: pre-wrap;", n$body),
        tags$div(class = "note-meta",
          paste0("Por: ", n$author %||% "?"))
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
                 "No se enviará al ERP. Solo visible en esta app")
    )
  )
}

# ── Inline CSS (injected once into the UI head) ───────────────────────────────

app_styles <- function() {
  tags$style(HTML("

    /* ── Navbar — compact height ─────────────────────────────────────────── */
    nav.navbar {
      min-height: unset !important;
      padding-top: 3px !important;
      padding-bottom: 3px !important;
    }
    nav.navbar .navbar-brand {
      font-size: 0.95rem;
      font-weight: 800;
      letter-spacing: 0.06em;
      text-transform: uppercase;
      padding-top: 2px;
      padding-bottom: 2px;
    }
    nav.navbar .nav-link {
      display: inline-flex !important;
      align-items: center;
      gap: 0.3em;
      padding-top: 5px !important;
      padding-bottom: 5px !important;
      white-space: nowrap;
    }
    nav.navbar .nav-link .svg-inline--fa {
      flex-shrink: 0;
      height: 1em;
      width: 1.25em;
    }

    /* ── Control bar — 3-zone grid ──────────────────────────────────────── */
    .control-bar {
      border-bottom: 1px solid #dce3ef;
      background: #fff;
      position: sticky;
      top: 0;
      z-index: 100;
    }
    .cb-row {
      display: grid;
      grid-template-columns: 1fr auto 1fr;
      align-items: center;
      padding: 7px 16px;
      gap: 20px;
    }

    /* Left: company pills */
    .cb-zone-emp { min-width: 0; overflow: visible; }
    .emp-strip-outer {
      min-width: 0;
      overflow: visible;
      position: relative;
    }
    .emp-strip-outer::after {
      content: '';
      position: absolute;
      right: 0; top: 0; bottom: 0;
      width: 28px;
      background: linear-gradient(to right, transparent, #fff);
      pointer-events: none;
      z-index: 2;
    }
    .emp-strip {
      overflow-x: auto;
      scrollbar-width: none;
      -ms-overflow-style: none;
      display: flex;
      align-items: center;
      gap: 5px;
      padding: 2px 2px;
    }
    .emp-strip::-webkit-scrollbar { display: none; }

    /* Center: controls card */
    .cb-zone-controls {
      display: flex;
      align-items: center;
      gap: 8px;
      background: #f4f7fc;
      border: 1px solid #dde4f0;
      border-radius: 9px;
      padding: 4px 12px;
      justify-self: center;
      white-space: nowrap;
    }
    /* Kill default form-group margins so the card hugs its contents */
    .cb-zone-controls .form-group,
    .cb-zone-controls .shiny-input-container {
      margin-bottom: 0 !important;
      margin-top: 0 !important;
    }
    .cb-field .form-control,
    .cb-field .form-select {
      height: 30px !important;
      padding: 3px 9px !important;
      font-size: 0.82rem !important;
      line-height: 1.3 !important;
      background: #fff !important;
      border-color: #d0d9ec !important;
    }
    .cb-field .form-select { padding-right: 26px !important; }
    /* Month input matches currency height */
    .cb-month .form-control,
    .cb-month input { height: 30px !important; }
    .cb-month { width: 102px; }
    .cb-currency { width: 90px; }
    /* Month/currency are driven by the calendar widget internally — hide from bar */
    .cb-month,
    .cb-currency { display: none !important; }

    /* Right: actions */
    .cb-zone-actions { justify-self: end; }
    .cb-act {
      width: 31px !important;
      height: 31px !important;
      padding: 0 !important;
      display: inline-flex !important;
      align-items: center !important;
      justify-content: center !important;
      flex-shrink: 0;
    }

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
      position: fixed;
      top: 0;
      left: 0;
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
      top: -6px; left: var(--caret-left, 50%);
      transform: translateX(-50%);
      border-width: 0 6px 6px;
      border-style: solid;
      border-color: transparent transparent #f0a05a;
    }
    .emp-tooltip::after {
      content: \"\";
      position: absolute;
      top: -5px; left: var(--caret-left, 50%);
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
      height: calc(100vh - 84px);
      min-height: 500px;
      overflow-y: auto;
    }

    /* ── Notes toggle button floating ───────────────────────────────────── */
    .notes-toggle-bar {
      position: fixed;
      bottom: 16px; left: 16px;
      z-index: 1000;
    }
    .notes-bar-btn {
      background: transparent;
      border: 1px solid #c5d0e6;
      border-radius: 20px;
      color: #4a6fa5;
      font-size: 0.78rem;
      font-weight: 500;
      padding: 3px 14px;
      cursor: pointer;
      transition: background 0.13s, border-color 0.13s, color 0.13s;
      text-decoration: none !important;
      display: inline-flex;
      align-items: center;
      gap: 5px;
    }
    .notes-bar-btn:hover {
      background: #eef2ff;
      border-color: #4a6fa5;
      color: #1a3a6b;
    }
    .notes-bar-btn.notes-open {
      background: #4a6fa5;
      border-color: #4a6fa5;
      color: #fff;
    }

    /* ── Notes panel — resizable, slides up from bottom ─────────────────── */
    .notes-panel-wrapper {
      position: fixed;
      bottom: 0; left: 0; right: 0;
      height: 300px;
      max-height: 80vh;
      min-height: 100px;
      background: #fff;
      border-top: 2px solid #4a6fa5;
      box-shadow: 0 -4px 16px rgba(74,111,165,0.12);
      z-index: 999;
      display: flex;
      flex-direction: column;
      overflow: hidden;
    }
    .notes-resize-handle {
      flex-shrink: 0;
      height: 8px;
      background: #f4f7fc;
      border-bottom: 1px solid #dde4f0;
      cursor: ns-resize;
      display: flex;
      align-items: center;
      justify-content: center;
      user-select: none;
    }
    .notes-resize-handle::after {
      content: '';
      width: 36px;
      height: 3px;
      background: #c5d0e6;
      border-radius: 2px;
    }
    .notes-panel-scroll {
      flex: 1;
      overflow-y: auto;
      min-height: 0;
    }
    .notes-panel-header {
      position: sticky;
      top: 0;
      background: #f4f7fc;
      border-bottom: 1px solid #dde4f0;
      padding: 6px 14px;
      display: flex;
      justify-content: space-between;
      align-items: center;
      z-index: 1;
    }

    /* ── Modal summary bar ───────────────────────────────────────────────── */
    .modal-summary-bar {
      background: #f8f9fa;
      border-radius: 6px;
      padding: 8px 12px;
      margin-bottom: 8px;
    }

    /* ── Note items ──────────────────────────────────────────────────────── */
    .note-item               { background: #fffdf0; border-color: #ffe58f !important; }
    .note-item.note-personal { background: #fff8f0; border-color: #f0a05a !important; }
    .note-badge-public   { background: #92400e; color: #fff; font-size: 0.60rem; padding: 1px 5px; }
    .note-badge-personal { background: #c05e0e; color: #fff; font-size: 0.60rem; padding: 1px 5px; }
    .note-title { font-size: 0.80rem; }
    .note-body  { font-size: 0.75rem; color: #6b7280; }
    .note-meta  { font-size: 0.63rem; color: #9ca3af; margin-top: 3px; }

    /* ── Refresh button animation states ────────────────────────────────── */
    @keyframes hop-spin {
      to { transform: rotate(360deg); }
    }
    #btn_refresh { transition: border-color .2s, color .2s, background .2s, box-shadow .2s; }
    #btn_refresh.refreshing i,
    #btn_refresh.refreshing svg {
      animation: hop-spin 0.7s linear infinite;
      transform-origin: center;
    }
    #btn_refresh.refreshing {
      border-color: #0d6efd !important;
      color: #0d6efd !important;
      background: rgba(13,110,253,0.06) !important;
    }
    #btn_refresh.refresh-done {
      border-color: #198754 !important;
      color: #198754 !important;
      background: rgba(25,135,84,0.07) !important;
    }
    /* ── Top-of-page progress bar (NProgress-style) ─────────────────────── */
    #app-refresh-bar {
      position: fixed; top: 0; left: 0;
      height: 2px; width: 0%;
      opacity: 0; z-index: 10000;
      pointer-events: none;
      background: linear-gradient(90deg, #0d6efd, #0dcaf0);
      border-radius: 0 1px 1px 0;
    }
    #app-refresh-bar.hop-loading {
      opacity: 1; width: 75%;
      transition: width 5s cubic-bezier(.08,.01,.18,1), opacity .15s ease;
    }
    #app-refresh-bar.hop-done {
      opacity: 1; width: 100% !important;
      background: #198754;
      transition: width .3s ease, background .2s ease;
    }
    #app-refresh-bar.hop-fade {
      opacity: 0;
      transition: opacity .4s ease .1s;
    }

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
      display: flex; align-items: center;
      justify-content: space-between; gap: 12px;
      margin-bottom: 10px; flex-wrap: wrap;
    }
    .cal-title-left { display: flex; align-items: center; gap: 10px; }
    .cal-title-hint { font-size: 0.78rem; color: #888; white-space: nowrap; }
    /* Cobros / Pagos toggle */
    .cal-ledger-toggle {
      display: inline-flex; border-radius: 8px; overflow: hidden;
      border: 1.5px solid #0A58CA; gap: 0; flex-shrink: 0;
    }
    .cal-toggle-btn {
      background: transparent; border: none; color: #0A58CA;
      font-size: 0.88rem; font-weight: 600;
      padding: 6px 18px; cursor: pointer;
      transition: background 0.15s, color 0.15s; line-height: 1.4;
    }
    .cal-toggle-btn:hover { background: rgba(10, 88, 202, 0.08); }
    .cal-toggle-active { background: #0A58CA !important; color: #fff !important; }
    /* Month label — interactive */
    .cal-period-label {
      font-size: 1.1rem; font-weight: 700; color: #0A58CA;
      cursor: pointer; position: relative; display: inline-flex;
      align-items: center; gap: 4px; user-select: none;
    }
    .cal-period-label:hover { color: #0344a8; }
    /* Currency badge */
    .cal-currency-badge {
      font-size: 0.75rem; font-weight: 800;
      padding: 1px 6px; border-radius: 3px;
      background: #e8f0fe; color: #0A58CA;
      letter-spacing: 0.06em; flex-shrink: 0;
    }
    .cal-currency-interactive {
      cursor: pointer; position: relative; display: inline-flex;
      align-items: center; gap: 3px; user-select: none;
    }
    .cal-currency-interactive:hover { }
    /* Chevron arrow shared by both pickers */
    .cal-pick-arr {
      width: 10px; height: 6px; flex-shrink: 0;
      transition: transform 0.18s;
    }
    /* Month picker dropdown */
    .cal-mpicker {
      display: none; position: absolute; top: calc(100% + 6px); left: 0;
      z-index: 1050; background: #fff; border: 1px solid #d0d9ec;
      border-radius: 10px; box-shadow: 0 6px 24px rgba(10,40,100,0.13);
      padding: 10px; min-width: 200px;
    }
    .cal-mpicker.open { display: block; }
    .cal-mp-nav {
      display: flex; align-items: center; justify-content: space-between;
      margin-bottom: 8px;
    }
    .cal-mp-navbtn {
      background: none; border: none; font-size: 1.3rem; color: #0A58CA;
      cursor: pointer; padding: 0 6px; line-height: 1;
    }
    .cal-mp-navbtn:hover { color: #0344a8; }
    .cal-mp-navlbl { font-weight: 700; font-size: 0.9rem; color: #222; }
    .cal-mp-months {
      display: grid; grid-template-columns: repeat(4,1fr); gap: 4px;
      margin-bottom: 6px;
    }
    .cal-mpm-btn {
      background: none; border: none; border-radius: 6px; padding: 4px 2px;
      font-size: 0.78rem; cursor: pointer; color: #333; text-align: center;
    }
    .cal-mpm-btn:hover { background: #e8f0fe; }
    .cal-mpm-sel { background: #0A58CA !important; color: #fff !important; font-weight: 700; }
    .cal-mp-years { display: flex; gap: 4px; flex-wrap: wrap; }
    .cal-mpy-btn {
      background: none; border: none; border-radius: 6px; padding: 3px 7px;
      font-size: 0.76rem; cursor: pointer; color: #555;
    }
    .cal-mpy-btn:hover { background: #e8f0fe; }
    .cal-mpy-sel { background: #e8f0fe; color: #0A58CA; font-weight: 700; }
    /* Currency picker dropdown */
    .cal-cur-picker {
      display: none; position: absolute; top: calc(100% + 6px); right: 0;
      z-index: 1050; background: #fff; border: 1px solid #d0d9ec;
      border-radius: 10px; box-shadow: 0 6px 24px rgba(10,40,100,0.13);
      padding: 6px; min-width: 90px;
    }
    .cal-cur-picker.open { display: block; }
    .cal-cur-opt {
      display: block; width: 100%; background: none; border: none;
      border-radius: 6px; padding: 5px 10px; font-size: 0.82rem;
      font-weight: 700; color: #333; cursor: pointer; text-align: left;
      letter-spacing: 0.04em;
    }
    .cal-cur-opt:hover { background: #e8f0fe; }
    .cal-cur-sel { color: #0A58CA; background: #f0f5ff; }
    /* AP theme picker overrides */
    .cal-theme-ap .cal-mp-navbtn { color: #7B4F2E; }
    .cal-theme-ap .cal-mpm-sel { background: #7B4F2E !important; }
    .cal-theme-ap .cal-cur-opt:hover { background: #F5EDE6; }
    .cal-theme-ap .cal-cur-sel { color: #7B4F2E; background: #EDE0D6; }

    /* ── Cobros (AR) — blue theme ─────────────────────────────── */
    .cal-theme-ar .cal-ledger-toggle { border-color: #0A58CA; }
    .cal-theme-ar .cal-toggle-btn { color: #0A58CA; }
    .cal-theme-ar .cal-toggle-btn:hover { background: rgba(10,88,202,0.08); }
    .cal-theme-ar .cal-toggle-active { background: #0A58CA !important; }
    .cal-theme-ar .cal-period-label { color: #0A58CA; }
    .cal-theme-ar .cal-currency-badge { background: #e8f0fe; color: #0A58CA; border-radius: 3px; }
    .cal-theme-ar .cal-tile { border-color: #D6E0F0; }
    .cal-theme-ar .cal-tile--weekend { background: #F6F9FF; border-color: #DDE7F7; }
    .cal-theme-ar .cal-tile--has-data { border-color: #9DB8E8; }
    .cal-theme-ar .cal-tile--has-data:hover { box-shadow: 0 3px 14px rgba(10,88,202,0.15); border-color: #0A58CA; }
    .cal-theme-ar .cal-tile--today { border-color: #0A58CA !important; background: #EFF5FF; }
    .cal-theme-ar .cal-daynum { color: #0A58CA; }
    .cal-theme-ar .cal-tile--today .cal-daynum { color: #0A58CA; }
    .cal-theme-ar .cal-tile--weekend .cal-daynum { color: #9ba8b5; }

    /* ── Pagos (AP) — brown theme ───────────────────────────────── */
    .cal-theme-ap .cal-ledger-toggle { border-color: #7B4F2E; }
    .cal-theme-ap .cal-toggle-btn { color: #7B4F2E; }
    .cal-theme-ap .cal-toggle-btn:hover { background: rgba(123,79,46,0.08); }
    .cal-theme-ap .cal-toggle-active { background: #7B4F2E !important; }
    .cal-theme-ap .cal-period-label { color: #7B4F2E; }
    .cal-theme-ap .cal-currency-badge { background: #F5EDE6; color: #7B4F2E; border-radius: 3px; }
    .cal-theme-ap .cal-tile { border-color: #EBD9CE; }
    .cal-theme-ap .cal-tile--weekend { background: #FDF8F5; border-color: #F0E0D5; }
    .cal-theme-ap .cal-tile--has-data { border-color: #D4A98C; }
    .cal-theme-ap .cal-tile--has-data:hover { box-shadow: 0 3px 14px rgba(123,79,46,0.15); border-color: #7B4F2E; }
    .cal-theme-ap .cal-tile--today { border-color: #7B4F2E !important; background: #FDF5EE; }
    .cal-theme-ap .cal-daynum { color: #7B4F2E; }
    .cal-theme-ap .cal-tile--today .cal-daynum { color: #7B4F2E; }
    .cal-theme-ap .cal-tile--weekend .cal-daynum { color: #9ba8b5; }

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
    .cal-daynum { font-size: 1rem; font-weight: 700; color: #7B4F2E; line-height: 1; flex-shrink: 0; }
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
    .cart-btn-provision { min-width: 90px; flex-shrink: 0; transition: background-color 0.15s, border-color 0.15s, color 0.15s; }
    .cart-btn-provision:hover { background-color: #6d28d9 !important; border-color: #6d28d9 !important; color: white !important; }
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
    tr.ph-row-confirmed td { text-decoration: line-through; opacity: 0.55; }

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

    /* ── Agenda de Hoy — company tab badges ─────────────────────────────── */

    .ph-tab-ini {
      font-weight: 600;
      letter-spacing: 0.02em;
    }

    .ph-tab-badge {
      display: inline-flex;
      align-items: center;
      gap: 3px;
      margin-left: 5px;
      vertical-align: middle;
    }

    .ph-ctr-pill {
      display: inline-block;
      font-size: 0.58em;
      font-weight: 700;
      line-height: 1;
      padding: 2px 5px;
      border-radius: 6px;
      letter-spacing: 0.02em;
      white-space: nowrap;
    }

    /* MXN = terracotta, USD = navy — same palette as Bancos card borders */
    .ph-ctr-mxn {
      background: #C0674A;
      color: #fff;
    }

    .ph-ctr-usd {
      background: #185FA5;
      color: #fff;
    }

    .ph-ctr-cur {
      font-size: 0.82em;
      font-weight: 500;
      opacity: 0.88;
    }

    .ph-ctr-other {
      background: #5a6373;
      color: #fff;
    }

    /* active tab: slightly deeper shade to keep contrast on lighter button bg */
    .nav-tabs .nav-link.active .ph-ctr-mxn   { background: #a8513a; }
    .nav-tabs .nav-link.active .ph-ctr-usd   { background: #134e8a; }
    .nav-tabs .nav-link.active .ph-ctr-other { background: #444b57; }

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

    /* ── Pasivos provisions ──────────────────────────────────────────────── */
    .pasivos-provision {
      border: 2px dashed #6b7280;
      opacity: 0.78;
      background-color: #f9fafb;
      position: relative;
    }
    .pasivos-provision::before {
      content: 'P';
      position: absolute;
      top: 2px; left: 4px;
      font-size: 10px; font-weight: 700;
      color: #6b7280;
      background: white;
      border: 1px solid #6b7280;
      border-radius: 50%;
      width: 14px; height: 14px;
      text-align: center; line-height: 12px;
    }
    .pasivos-convert-btn {
      background-color: #7c3aed;
      color: white; border: none;
      border-radius: 4px;
      padding: 2px 6px; font-size: 11px; cursor: pointer;
    }
    .pasivos-convert-btn:hover { background-color: #6d28d9; }

    /* ── Cashflow export modal: uniform btn-sm row in footer ─────────────── */
    /* Cancelar button: match btn-sm size */
    .cf-footer .btn { font-size:.875rem; padding:.25rem .5rem; line-height:1.5; }
    /* Selectize control: compact to match btn-sm height (~31px) */
    .cf-footer .shiny-input-container { margin-bottom:0; width:auto; }
    .cf-footer .selectize-control.single .selectize-input {
      padding: 3px 28px 3px 8px; /* right room for caret */
      min-height: 31px; height: 31px;
      font-size: .8rem; line-height: 1.5;
    }
    .cf-footer .selectize-dropdown { font-size: .8rem; }

    /* ── Language row: remove selectInput bottom margin ─────────────────── */
    .cf-lang-row .shiny-input-container { margin-bottom: 0 !important; }

  "))
}

# Initializes Bootstrap 5 tooltips after every Shiny output update.
# Cal tiles use data-bs-toggle="tooltip" for the abono breakdown.
app_scripts <- function() {
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      Shiny.setInputValue('client_tz', Intl.DateTimeFormat().resolvedOptions().timeZone);
    });

    /* ── Snapshot tooltip: position:fixed + synchronous viewport clamping ── */
    /* getBoundingClientRect() forces a layout reflow, giving us the real     */
    /* rendered rect (including transform) without needing requestAnimFrame.  */
    document.addEventListener('mouseenter', function(e) {
      var wrap = e.target.closest('.emp-tooltip-wrap');
      if (!wrap) return;
      var tip = wrap.querySelector('.emp-tooltip');
      if (!tip) return;
      var r  = wrap.getBoundingClientRect();
      var cx = r.left + r.width / 2;
      tip.style.top  = (r.bottom + 7) + 'px';
      tip.style.left = cx + 'px';
      /* Force reflow, then clamp so the tooltip never leaves the viewport. */
      var tr      = tip.getBoundingClientRect();
      var pad     = 8;
      var newLeft = cx;
      if (tr.right > window.innerWidth - pad) {
        newLeft = cx - (tr.right - (window.innerWidth - pad));
      } else if (tr.left < pad) {
        newLeft = cx + (pad - tr.left);
      }
      tip.style.left = newLeft + 'px';
      /* Repoint the caret at the pill centre regardless of how much the box shifted. */
      var caretPx  = cx - (newLeft - tr.width / 2);
      var caretPct = Math.max(8, Math.min(92, (caretPx / tr.width) * 100));
      tip.style.setProperty('--caret-left', caretPct + '%');
    }, true);

    /* ── Notify R when any Bootstrap modal is dismissed ─────────────── */
    /* ledger_module uses this to clear modal_open so the provisions     */
    /* observer doesn't re-open the calendar day modal after user leaves */
    document.addEventListener('hidden.bs.modal', function() {
      if (window.Shiny) Shiny.setInputValue('cal_day_modal_closed', Math.random(), {priority: 'event'});
    });

    /* ── Notes bar toggle: highlight button when panel is open ── */
    $(document).on('click', '#toggle_notes_ar, #toggle_notes_ap', function() {
      $(this).toggleClass('notes-open');
    });
    $(document).on('shiny:value', function() {
      requestAnimationFrame(function() {
        document.querySelectorAll('[data-bs-toggle=\"tooltip\"]:not(.tt-init)')
          .forEach(function(el) {
            el.classList.add('tt-init');
            new bootstrap.Tooltip(el, {container: 'body', html: true});
          });
      });
    });

    /* ── Calendar inline pickers ── */
    function _hopCloseAll(exceptEl) {
      document.querySelectorAll('.cal-mpicker.open, .cal-cur-picker.open').forEach(function(p) {
        if (p !== exceptEl) {
          p.classList.remove('open');
          var label = p.closest('.cal-period-label') || p.closest('.cal-currency-interactive');
          if (label) {
            var arr = label.querySelector('.cal-pick-arr');
            if (arr) arr.style.transform = '';
          }
        }
      });
    }

    // Opens the month picker — never closes it (only month selection or outside-click closes)
    function hopMPick(labelEl) {
      var picker = labelEl.querySelector('.cal-mpicker');
      if (!picker || picker.classList.contains('open')) return;
      _hopCloseAll(picker);
      picker.classList.add('open');
      var arr = labelEl.querySelector('.cal-pick-arr');
      if (arr) arr.style.transform = 'rotate(180deg)';
    }

    // Navigate displayed year in picker — does NOT close or fire Shiny
    function hopMPickNav(pickerId, dir) {
      var picker = document.getElementById(pickerId);
      if (!picker) return;
      var lbl = picker.querySelector('.cal-mp-navlbl');
      var yr = parseInt(lbl ? lbl.textContent : (picker.dataset.selYear || '2026'), 10);
      hopMPickYear(pickerId, yr + dir);
    }

    // Switch displayed year in picker — does NOT close or fire Shiny
    function hopMPickYear(pickerId, year) {
      var picker = document.getElementById(pickerId);
      if (!picker) return;
      picker.dataset.selYear = year;
      var lbl = picker.querySelector('.cal-mp-navlbl');
      if (lbl) lbl.textContent = year;
      picker.querySelectorAll('.cal-mpy-btn').forEach(function(btn) {
        btn.classList.toggle('cal-mpy-sel', parseInt(btn.textContent, 10) === year);
      });
    }

    // Select a month — closes picker and fires Shiny (no timezone bug: uses stored year + month int)
    function hopMPickSel(pickerId, monthNum) {
      var picker = document.getElementById(pickerId);
      if (!picker) return;
      var yr = picker.dataset.selYear || picker.querySelector('.cal-mp-navlbl').textContent;
      var mm = String(monthNum).padStart(2, '0');
      var newVal = yr + '-' + mm + '-01';
      _hopCloseAll(null);
      Shiny.setInputValue('month_sel', newVal, {priority: 'event'});
    }

    function hopCurPick(ledger, cur, badgeEl) {
      if (cur) {
        _hopCloseAll(null);
        var inputId = ledger === 'AR' ? 'cur_ar' : 'cur_ap';
        Shiny.setInputValue(inputId, cur, {priority: 'event'});
        return;
      }
      if (!badgeEl) return;
      var picker = badgeEl.querySelector('.cal-cur-picker');
      if (!picker) return;
      var isOpen = picker.classList.contains('open');
      _hopCloseAll(picker);
      picker.classList.toggle('open', !isOpen);
      var arr = badgeEl.querySelector('.cal-pick-arr');
      if (arr) arr.style.transform = !isOpen ? 'rotate(180deg)' : '';
    }

    document.addEventListener('click', function(e) {
      if (!e.target.closest('.cal-period-label') && !e.target.closest('.cal-currency-interactive')) {
        _hopCloseAll(null);
      }
    }, true);

    /* ── Navbar dropdown (e.g. \"Grupo\") ──────────────────────────────
       Plain open-on-click / close-on-outside-click / close-on-item-select,
       same as any ordinary dropdown. Bootstrap's own Dropdown component
       never reliably closes this one in this app (a binding/order clash
       with Shiny's own nav-link click handling on the nested tab items,
       most likely) — so it's managed by hand instead of fought with. */
    function _hopNavDropdowns() { return document.querySelectorAll('.navbar li.dropdown'); }
    function _hopCloseNavDropdowns(except) {
      _hopNavDropdowns().forEach(function(li) {
        if (li === except) return;
        li.classList.remove('show');
        var menu = li.querySelector('.dropdown-menu');
        if (menu) menu.classList.remove('show');
        var toggle = li.querySelector('[data-bs-toggle=\"dropdown\"]');
        if (toggle) toggle.setAttribute('aria-expanded', 'false');
      });
    }
    document.addEventListener('click', function(e) {
      var toggle = e.target.closest('.navbar li.dropdown > [data-bs-toggle=\"dropdown\"]');
      if (toggle) {
        e.preventDefault();
        e.stopPropagation();
        var li     = toggle.closest('li.dropdown');
        var isOpen = li.classList.contains('show');
        _hopCloseNavDropdowns(isOpen ? null : li);
        li.classList.toggle('show', !isOpen);
        var menu = li.querySelector('.dropdown-menu');
        if (menu) menu.classList.toggle('show', !isOpen);
        toggle.setAttribute('aria-expanded', String(!isOpen));
        return;
      }
      // Item selected inside an open menu, or click landed anywhere else —
      // close (item clicks still bubble through to Shiny's own tab switch).
      _hopCloseNavDropdowns(null);
    }, true);
    document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape') _hopCloseNavDropdowns(null);
    });

    /* ── Refresh button feedback ───────────────────────────────────── */
    (function() {
      var _rfBar;
      $(document).on('shiny:connected', function() {
        _rfBar = document.createElement('div');
        _rfBar.id = 'app-refresh-bar';
        document.body.appendChild(_rfBar);
      });

      Shiny.addCustomMessageHandler('refresh_start', function(x) {
        var btn = document.getElementById('btn_refresh');
        if (btn) { btn.classList.remove('refresh-done'); btn.classList.add('refreshing'); }
        if (_rfBar) {
          _rfBar.classList.remove('hop-loading', 'hop-done', 'hop-fade');
          _rfBar.style.width = '0%';
          _rfBar.style.background = '';
          void _rfBar.offsetWidth;
          _rfBar.classList.add('hop-loading');
        }
      });

      Shiny.addCustomMessageHandler('refresh_end', function(x) {
        var btn = document.getElementById('btn_refresh');
        if (btn) {
          btn.classList.remove('refreshing');
          btn.classList.add('refresh-done');
          setTimeout(function() { btn.classList.remove('refresh-done'); }, 2000);
        }
        if (_rfBar) {
          _rfBar.classList.remove('hop-loading');
          _rfBar.classList.add('hop-done');
          setTimeout(function() {
            _rfBar.classList.add('hop-fade');
            setTimeout(function() {
              _rfBar.className = '';
              _rfBar.style.width = '0%';
            }, 500);
          }, 350);
        }
      });
    })();
  "))
}
