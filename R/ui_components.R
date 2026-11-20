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

# ── Currencies to capture and to filter (monedas fase 3, T39) ─────────────────
# The client's base for code that has no `shared` (the manual-entry and abono
# modals): app.R leaves it in session$userData$moneda_base, as it does
# session$userData$usuario; without it, MONEDA_BASE_OMISION.
moneda_base_sesion <- function(session = shiny::getDefaultReactiveDomain())
  tryCatch(session$userData$moneda_base(), error = function(e) NULL) %||% MONEDA_BASE_OMISION

# Rule 1, a selector to capture a currency (a manual entry, an override): the
# data's currencies, the base first, then the rest of the catalog -- an option
# never disappears because no document uses it yet.
monedas_para_capturar <- function(datos = character(), base = MONEDA_BASE_OMISION)
  unique(c(monedas_en_datos(datos, base), monedas_catalogo()))

# Rule 2, a filter or an order over what is there: only the data's
# currencies, the base first when the data has it, then by documents.
monedas_para_filtrar <- function(datos = character(), base = MONEDA_BASE_OMISION) {
  d <- toupper(trimws(as.character(datos)))
  intersect(monedas_en_datos(d, base), d)
}

# ── One wording for amounts and for SAP's date (T46) ──────────────────────────
# Panorama and the weekly emails call these two; nobody keeps a local copy.

# monto_moneda() lives in R/monedas.R since T55 (the engine of tables uses
# it too, and every app and fixture loads monedas.R).

# What several amounts add up to, one total per currency in order --
# "1,234.56 MXN · 500.00 USD" -- never different currencies summed into one
# figure (T55b: the Lupa's notice said "MXN/USD $ 1,050.00"). Rows without a
# currency add up apart, without a code. Bancos' totals and the Lupa use it.
montos_por_moneda <- function(importe, moneda) {
  importe <- suppressWarnings(as.numeric(importe))
  moneda  <- toupper(trimws(as.character(moneda)))
  moneda[is.na(moneda)] <- ""
  if (!length(importe)) return(monto_moneda(0, NA))
  s <- tapply(importe, moneda, sum, na.rm = TRUE)
  s <- s[order(names(s))]
  paste(monto_moneda(as.numeric(s), ifelse(nzchar(names(s)), names(s), NA)), collapse = " \u00b7 ")
}

# "Datos de SAP al martes 17, 08:54": when SAP's data is from, in `tz`. The
# month is added once the date is past SAP_DIAS_VIEJO (sap_viejo(), R/
# panorama_datos.R): "martes 10 de nov, 08:54". No date: the "not loaded" line.
sap_al_texto <- function(ts, tz = "America/Mexico_City", idioma = hd_idioma(), ahora = Sys.time()) {
  if (is.null(ts) || !length(ts) || is.na(ts[1])) return(tr("comun.sap.sin_datos", idioma = idioma))
  ts <- as.POSIXct(ts[1], tz = "UTC")
  d <- as.Date(format(ts, "%Y-%m-%d", tz = tz))
  forma <- if (sap_viejo(ts, ahora, tz)) "comun.sap.fecha_mes" else "comun.sap.fecha_semana"
  fecha <- tr(forma, idioma = idioma,
              dia = tr(paste0("comun.dia.", as.integer(format(d, "%u"))), idioma = idioma),
              n = as.character(as.integer(format(d, "%d"))),
              mes = tr(paste0("comun.mes.", as.integer(format(d, "%m"))), idioma = idioma),
              hora = format(ts, "%H:%M", tz = tz))
  tr("comun.sap.al", idioma = idioma, fecha = fecha)
}

# ── Manual entry modal ─────────────────────────────────────────────────────────
# (No caller today; kept on the same rules as manual_entry_tab_content().)
manual_entry_modal <- function(ledger, empresas, row = NULL, token = NULL,
                               monedas = character(), base = moneda_base_sesion()) {
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
                    choices  = monedas_para_capturar(c(if (!is_new) row$Moneda, monedas), base),
                    selected = if (!is_new) row$Moneda else base),
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

    /* ── Barra en un renglón: marca · ☰ · pestaña activa (T51) ───────────────
       The links live in bslib's own .navbar-collapse, shown as a panel under
       the ☰ (barra_menu_ui() opens it). The fixed heights below the bar read
       --hd-barra-alto. Display rules on .nav-link are left alone: the tier
       and permission rules hide links with display:none !important. */
    /* 43 px: what the old one-row navbar measured at 1772x866 (43.2, Chrome on
       63c077c, T51b). 48 took 4.8 px from Vencidos' rows on wide screens. */
    :root { --hd-barra-alto: 43px; }
    nav.navbar { height: var(--hd-barra-alto); flex-wrap: nowrap !important; }
    nav.navbar > .container-fluid { flex-wrap: nowrap !important; height: 100%; position: relative; }
    nav.navbar .navbar-header { display: flex; align-items: center; gap: 14px; min-width: 0; height: 100%; }
    nav.navbar .navbar-brand { order: 0; flex: none; }
    nav.navbar .navbar-toggle, nav.navbar .navbar-toggler { display: none !important; }
    .hd-menu-btn {
      order: 1; flex: none; position: relative;
      display: inline-flex; align-items: center; justify-content: center;
      width: 40px; height: 36px; padding: 0;   /* a click area of 36 px or more (T51b) */
      background: transparent; color: #fff;
      border: 1px solid rgba(255,255,255,.55); border-radius: 8px;
      cursor: pointer;
    }
    .hd-menu-btn:hover { background: rgba(255,255,255,.12); }
    .hd-menu-btn:focus-visible { outline: 2px solid #fff; outline-offset: 2px; }
    .hd-menu-icono::before { content: '\\2630'; font-size: 1.15rem; line-height: 1; }
    nav.navbar.hd-menu-abierto .hd-menu-icono::before { content: '\\2715'; font-size: 1rem; }
    #hd-menu-globo { position: absolute; top: -3px; right: -10px; font-size: .6rem; }   /* inside the 43 px bar */
    #hd-barra-activa {
      order: 2; min-width: 0;
      display: inline-flex; align-items: center; gap: .35em;
      color: #fff; font-weight: 600; font-size: .95rem;
      white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
    }
    #hd-barra-activa .svg-inline--fa { flex-shrink: 0; height: 1em; width: 1.25em; }
    /* The panel: always laid out (Shiny draws the outputs in it), invisible
       until the menu opens; barra_menu_ui() sets display/visibility inline
       because bslib's own rules beat a stylesheet here. */
    nav.navbar .navbar-collapse {
      visibility: hidden; pointer-events: none;
      position: absolute !important; top: 100%; left: 12px; z-index: 1050;
      width: 300px; max-width: calc(100vw - 24px);
      max-height: calc(100vh - var(--hd-barra-alto) - 8px); overflow-y: auto;
      background: var(--hd-blanco); border: 1px solid var(--hd-linea);
      border-radius: var(--hd-radio-sm); box-shadow: var(--hd-sombra);
      padding: 6px 0 0;
    }
    nav.navbar.hd-menu-abierto .navbar-collapse { visibility: visible; pointer-events: auto; }
    nav.navbar .navbar-collapse .navbar-nav {
      flex-direction: column !important; align-items: stretch !important;
      margin: 0 !important; padding: 0 !important; gap: 0 !important;
    }
    nav.navbar .navbar-collapse .nav-link,
    nav.navbar .navbar-collapse .dropdown-item {
      width: 100%; justify-content: flex-start;
      color: var(--hd-tinta) !important; border: 0 !important;
      padding: 8px 18px !important; border-radius: 0;
    }
    nav.navbar .navbar-collapse .nav-link:hover,
    nav.navbar .navbar-collapse .dropdown-item:hover,
    nav.navbar .navbar-collapse .nav-link:focus-visible,
    nav.navbar .navbar-collapse .dropdown-item:focus-visible { background: var(--hd-crema); outline: none; }
    nav.navbar .navbar-collapse .nav-link.active,
    nav.navbar .navbar-collapse .dropdown-item.active {
      color: var(--hd-marino) !important; font-weight: 700;
      background: var(--hd-oro-velo); box-shadow: inset 3px 0 0 var(--hd-oro);
    }
    /* Grupo's items listed under it, always (the approved drawing; the
       active one may live there and must take the focus). */
    nav.navbar .navbar-collapse .dropdown-menu {
      display: block !important;
      position: static !important; float: none; transform: none !important;
      border: 0; box-shadow: none; margin: 0; padding: 0 0 0 22px; background: transparent;
    }
    nav.navbar .navbar-collapse .bslib-nav-spacer { display: none; }
    nav.navbar .navbar-collapse .dropdown-toggle::after,
    nav.navbar .navbar-collapse .dropdown-toggle .caret { display: none; }
    nav.navbar .navbar-collapse .dropdown-toggle { cursor: default; }
    /* The user and Salir: a navy foot (their own white text stays readable). */
    nav.navbar .navbar-collapse .bslib-nav-item {
      background: var(--hd-marino); padding: 6px 14px; margin: 0;
    }
    nav.navbar .navbar-collapse .bslib-nav-item:has(#navbar_user_badge) { margin-top: 6px; padding-top: 10px; }
    nav.navbar .navbar-collapse .bslib-nav-item:last-child { padding-bottom: 10px; }
    @media (max-width: 600px) {
      nav.navbar .navbar-brand { font-size: .8rem; }
      nav.navbar .navbar-collapse { left: 0; width: 100vw; max-width: 100vw; border-radius: 0; }
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
      /* the bar (T51) + the control bar and its margin: 72 px, measured in
         Chrome at 1293x632 (the old 84px took the control bar as 36 px, so
         the calendar ran 36 px past the window). */
      height: calc(100vh - var(--hd-barra-alto) - 72px);
      min-height: 500px;
      overflow-y: auto;
    }

    /* ── Notes toggle button floating ───────────────────────────────────── */
    .notes-toggle-bar {
      position: fixed;
      bottom: 16px; left: 16px;
      z-index: 1000;
    }
    /* T56: the button floats over the bottom-left corner of every tab, so
       each scrolling box that reaches the window's bottom ends with room for
       it: scrolled to the end, its last row always shows above the button.
       At rest nothing moves and no height is lost. 56px = 16 of margin + the
       button's ~27 + air. The table engine reads this same variable for its
       room below a table (www/hdtable.js, T65): one source, keep its name.
       Only a Panorama that shows its Cinta is left out: there the Cinta lifts
       the button (T49). Without it (nothing to show) Panorama gets the room
       too: measured at 1293x632, its last card ended under the button.
       The room is an empty block after the content, not padding: Bancos'
       and Intercompany's boxes carry Bootstrap's p-0 (padding 0 !important),
       which erased a padding-bottom (measured: Bancos' end stayed under it). */
    :root { --hd-notas-reserva: 56px; }
    .tab-pane > .overflow-auto:not(:has(.pan-cinta))::after,
    .ph-company-panel::after,
    .hd-scroll-pestana::after {
      content: \"\"; display: block; height: var(--hd-notas-reserva);
    }
    /* T56: on the Calendar the button lives in its header, next to the month
       (calendar_html(), R/global.R: .cal-notes-btn clicks #toggle_notes), and
       the floating one hides -- except with the Flujo preview slid in, where
       that header is out of view. The Calendar keeps its height: scrolled to
       the end its last week already ended clear (measured at 1293x632). */
    body:has(.tab-pane.active .cf-slide-wrapper:not(.cf-preview-active)) .notes-toggle-bar {
      display: none;
    }
    .cal-notes-btn { margin-left: 8px; flex: none; }
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
    /* T59: \"open\" while the notes panel is there (the server draws it into
       #notes_panel), on both buttons: the floating one and the Calendar
       header's, which is redrawn often and could not keep a class. No JS.
       #toggle_notes by id, so it outweighs panorama.css's opaque face (T49). */
    body:has(#notes_panel .notes-panel-wrapper) #toggle_notes,
    body:has(#notes_panel .notes-panel-wrapper) .cal-notes-btn {
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
    .modal-day-header { display: flex; flex-wrap: wrap; gap: 8px; align-items: center; }
    .modal-day-pill {
      font-size: 0.75rem; padding: 3px 10px;
      border-radius: 12px; font-weight: 600;
    }
    .modal-day-pill--date   { background: #EFF5FF; color: #0A58CA; border: 1px solid #9DB8E8; }
    .modal-day-pill--ledger { background: #0A58CA; color: white; }
    .modal-day-pill--cur    { background: #f1f3f5; color: #333; border: 1px solid #ccc; }
    .modal-day-pill--saldo  { background: #e9f7ef; color: #155724; border: 1px solid #a3d9b3; }
    .modal-day-pill--abono  { background: #fff3cd; color: #856404; border: 1px solid #ffc107; }

    /* ── Day modal: the Agenda total, a pill in the window's header (T09) ──── */
    .cart-total-staged {
      background: #EFF5FF;
      border: 1px solid #9DB8E8;
      color: #0A58CA;
      display: inline-flex;
      gap: 6px;
      align-items: center;
    }

    /* ── Pagar Hoy ───────────────────────────────────────────────────────── */
    .ph-container { overflow: hidden; }

    .ph-summary-bar {
      background: #fff;
      border-bottom: 1px solid #dee2e6;
      flex-shrink: 0;
    }

    /* T59: the company panel fills what is left under the Agenda's summary
       bar and its company tabs, whatever their height: a flex chain from
       .ph-container (a column, h-100, overflow hidden) down to the active
       tab, instead of max-height: calc(100vh - 180px), which guessed it.
       When the summary wrapped (the net position in several currencies), the
       panel ran past the window and its end could not be reached at all. */
    .ph-container > .shiny-html-output,
    .ph-container > .shiny-html-output > .tabbable {
      flex: 1 1 auto; min-height: 0;
      display: flex; flex-direction: column;
    }
    .ph-container .tabbable > .tab-content { flex: 1 1 auto; min-height: 0; }
    .ph-container .tabbable > .tab-content > .tab-pane.active { height: 100%; }
    .ph-company-panel {
      height: 100%;
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

    /* No scroll of its own (T09): the window scrolls a long panel, and the
       Excel review (alto = ventana) fills the window -- one scroll. */
    .settings-content { min-width: 0; }

    /* Reviewing an Excel (T09): what's only needed to choose the file steps
       aside so the review gets the window. Back as soon as there's no review. */
    .hd-importando .bp-sap-seccion,
    .hd-importando .bp-catalogo-titulo,
    .hd-importando .prov-solo-eleccion,
    .hd-importando .prov-eleccion .control-label,
    .hd-importando .prov-eleccion .form-label,
    .hd-importando .prov-eleccion .progress { display: none !important; }
    .hd-importando .prov-eleccion {
      display: flex; flex-wrap: wrap; align-items: center; column-gap: 16px; row-gap: 4px; margin-bottom: 4px;
    }
    .hd-importando .prov-eleccion > .row { flex: 0 1 auto; margin: 0; flex-wrap: nowrap; align-items: center; column-gap: 8px; }
    .hd-importando .prov-eleccion > .row > div { width: auto; flex: 0 0 auto; padding: 0; margin: 0; }
    .hd-importando .prov-eleccion > .row > .prov-archivo { width: 240px; }
    .hd-importando .prov-eleccion .selectize-control { width: 200px; }
    .hd-importando .prov-eleccion .selectize-input { white-space: nowrap; overflow: hidden; flex-wrap: nowrap; }
    .hd-importando .prov-eleccion .prov-desde-fecha { width: 130px !important; }
    .hd-importando .prov-eleccion .shiny-input-container:not(.shiny-input-container-inline) { width: auto; }
    .hd-importando .nav-tabs .nav-link { padding-top: 3px; padding-bottom: 3px; }
    .hd-importando .settings-content { padding-top: 2px !important; padding-bottom: 0 !important; }
    .hd-importando .prov-resumen { padding-top: 4px !important; }
    .hd-importando .prov-importar { padding-top: 4px !important; padding-bottom: 0 !important; }
    .hd-importando .prov-eleccion .form-group,
    .hd-importando .prov-eleccion .shiny-input-container,
    .hd-importando .prov-eleccion .checkbox { margin-bottom: 0 !important; margin-top: 0; }
    .hd-importando .prov-eleccion > div { margin-bottom: 0 !important; }

    /* The review's summary line and its column mapping (T09). */
    .prov-mapeo { display: flex; flex-wrap: wrap; align-items: baseline; column-gap: 12px; row-gap: 2px; margin-bottom: 4px; }
    .prov-mapeo > .shiny-html-output { display: contents; }
    .prov-mapeo details[open], .prov-mapeo .prov-mapeo-falta { flex-basis: 100%; }
    .prov-mapeo summary { cursor: pointer; }
    .prov-resumen-opciones .form-group { margin-bottom: 0; }

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
  tagList(barra_menu_ui(), tags$script(HTML("
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
    /* found 2026-08-03 (Stage 9, Issue B): this was document.addEventListener,
       a NATIVE DOM listener -- but Shiny's bundled modal dialog (showModal())
       uses the OLD jQuery/Bootstrap-3-style modal plugin (confirmed live:
       window.bootstrap is undefined in this app; the modal element is
       'modal fade in', the Bootstrap-3/4 jQuery-plugin class shape, not
       Bootstrap 5's 'modal fade show'). That plugin fires hidden.bs.modal
       as a jQuery-namespaced event via $el.trigger(...), which is invisible
       to addEventListener -- jQuery's own event system and the native DOM
       event system don't share events unless jQuery is told to dispatch a
       real one. Confirmed by a live chromote test: with addEventListener,
       closing a modal via jQuery's own $('#shiny-modal').modal('hide') (the
       exact call Shiny's bundled JS makes for a backdrop click, ESC, or the
       close button) never fired this handler at all, at ANY scope -- not
       just the module-namespace mismatch fixed alongside this in
       ledger_module.R, but the signal never reaching R in the first place.
       Switching to jQuery's own $(document).on(...) binding is what actually
       receives it. */
    $(document).on('hidden.bs.modal', function() {
      if (window.Shiny) Shiny.setInputValue('cal_day_modal_closed', Math.random(), {priority: 'event'});
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
  ")))
}

# ── The navbar as one row with a ☰ menu (T51) ─────────────────────────────────
# Luis, 2026-11-17: "pasar toda esa barra a un menú de hamburguesa… para
# ahorrar espacio vertical". The row keeps the brand, the ☰ (with Vencidos'
# count in a bubble) and the active tab's name; everything else (tabs, Grupo,
# the user and Salir) lives in a panel under the ☰. bslib's navbar stays as
# it is: the panel is its own .navbar-collapse, so the .nav-link elements and
# every rule that hides or shows them by tier and permission (app.R) are the
# same. The menu shows exactly the links that are visible today and adds no
# logic of its own. Styles in app_styles() ("Barra en un renglón").
# Keyboard: the ☰ is a <button> with aria-expanded/aria-controls; opening
# moves the focus to the active tab, Esc closes and gives the focus back,
# choosing a tab, clicking outside or tabbing out closes it.
barra_menu_ui <- function() {
  tagList(
    htmltools::singleton(tags$head(i18n_head())),   # hdT(), as hd_table() does
    tags$script(HTML("
    (function() {
      function t(k, v) { return window.hdT ? window.hdT(k, v) : k; }
      function nav()   { return document.querySelector('nav.navbar'); }
      function panel() { var n = nav(); return n ? n.querySelector('.navbar-collapse') : null; }
      function boton() { return document.getElementById('hd-menu-btn'); }
      function abierto() { var n = nav(); return !!(n && n.classList.contains('hd-menu-abierto')); }
      // Inline and !important: bslib's own navbar CSS shows .navbar-collapse
      // with !important rules that beat any stylesheet (seen in Chrome, T51).
      // Closed is invisible, not display:none: Shiny does not draw an output
      // it thinks hidden (the user's badge lives in the panel), and an
      // invisible panel still takes no clicks and no focus.
      function mostrarPanel(si) {
        var p = panel(); if (!p) return;
        p.style.setProperty('display', 'block', 'important');
        p.style.setProperty('visibility', si ? 'visible' : 'hidden', 'important');
        p.style.setProperty('pointer-events', si ? 'auto' : 'none', 'important');
      }
      // Shown = neither the link nor its <li> is display:none (the tier,
      // permission and admin-deployment rules hide one or the other).
      // An item of a dropdown (Grupo) is shown only if its dropdown is.
      function visible(el) {
        if (!el || getComputedStyle(el).display === 'none') return false;
        var li = el.closest('li');
        if (li && getComputedStyle(li).display === 'none') return false;
        var menu = el.closest('.dropdown-menu');
        if (menu) {
          var dd = menu.closest('li.dropdown');
          return !!dd && visible(dd.querySelector('.dropdown-toggle'));
        }
        return true;
      }
      function activo() {
        var p = panel(); if (!p) return null;
        return p.querySelector('.dropdown-item.active[data-value]') ||
               p.querySelector('.nav-link.active[data-value]:not(.dropdown-toggle)');
      }
      // The active tab's icon and name next to the ☰ (copied from its link,
      // already translated); nothing while it is hidden (the app loading).
      function pintarActiva() {
        var span = document.getElementById('hd-barra-activa'), p = panel();
        if (!span || !p) return;
        var a = activo();
        span.textContent = '';
        if (a && visible(a)) a.childNodes.forEach(function(n) {
          if (n.nodeType === 1 && n.id === 'ven_tab_badge') return;
          var c = n.cloneNode(true);
          if (c.nodeType === 1) c.removeAttribute('id');
          span.appendChild(c);
        });
        p.querySelectorAll('[data-value]').forEach(function(l) {
          if (l === a) l.setAttribute('aria-current', 'page'); else l.removeAttribute('aria-current');
        });
      }
      // Vencidos' count also outside the menu, in a bubble on the ☰.
      function pintarGlobo() {
        var g = document.getElementById('hd-menu-globo'), b = document.getElementById('ven_tab_badge');
        var p = panel(), btn = boton();
        if (!g || !p || !btn) return;
        var n = (b && b.style.display !== 'none') ? b.textContent.trim() : '';
        var ok = n !== '' && visible(p.querySelector('.nav-link[data-value=VEN]'));
        g.textContent = ok ? n : '';
        g.style.display = ok ? '' : 'none';
        var etiqueta = abierto() ? t('comun.menu.cerrar') : t('comun.menu.abrir');
        if (ok) etiqueta += ' · ' + t('comun.menu.vencidos', { n: n });
        btn.setAttribute('aria-label', etiqueta);
        btn.title = etiqueta;
      }
      function abrir() {
        var n = nav(), btn = boton(); if (!n || !btn) return;
        n.classList.add('hd-menu-abierto');
        mostrarPanel(true);
        btn.setAttribute('aria-expanded', 'true');
        pintarGlobo();
        var a = activo(), p = panel();
        var destino = (a && visible(a)) ? a :
          Array.from(p.querySelectorAll('.nav-link, button')).filter(visible)[0];
        if (destino) destino.focus();
      }
      function cerrar(devolverFoco) {
        var n = nav(), btn = boton(); if (!n || !btn || !abierto()) return;
        n.classList.remove('hd-menu-abierto');
        mostrarPanel(false);
        btn.setAttribute('aria-expanded', 'false');
        pintarGlobo();
        if (devolverFoco) btn.focus();
      }
      function iniciar() {
        var n = nav(), p = panel();
        if (!n || !p || boton()) return;
        var fila = n.querySelector('.navbar-header') || n.querySelector('.container-fluid');
        var btn = document.createElement('button');
        btn.type = 'button';
        btn.id = 'hd-menu-btn';
        btn.className = 'hd-menu-btn';
        btn.setAttribute('aria-expanded', 'false');
        btn.setAttribute('aria-controls', p.id);
        var icono = document.createElement('span');
        icono.className = 'hd-menu-icono';
        icono.setAttribute('aria-hidden', 'true');
        var globo = document.createElement('span');
        globo.id = 'hd-menu-globo';
        globo.className = 'badge rounded-pill bg-danger';
        globo.setAttribute('aria-hidden', 'true');
        globo.style.display = 'none';
        btn.appendChild(icono); btn.appendChild(globo);
        var activa = document.createElement('span');
        activa.id = 'hd-barra-activa';
        fila.appendChild(btn); fila.appendChild(activa);
        p.classList.add('hd-marca');                       // B's tokens inside the panel
        mostrarPanel(false);
        btn.addEventListener('click', function() { if (abierto()) cerrar(true); else abrir(); });
        // Choosing a tab (or a Grupo item) closes it.
        p.addEventListener('click', function(e) {
          if (e.target.closest('a[data-bs-toggle=tab], a[data-toggle=tab]')) cerrar(false);
        });
        document.addEventListener('click', function(e) {
          if (abierto() && !e.target.closest('nav.navbar')) cerrar(false);
        });
        document.addEventListener('keydown', function(e) {
          if (e.key === 'Escape' && abierto()) cerrar(true);
        });
        n.addEventListener('focusout', function(e) {
          if (abierto() && e.relatedTarget && !n.contains(e.relatedTarget)) cerrar(false);
        });
        $(document).on('shown.bs.tab', function(e) {
          if ($(e.target).closest('nav.navbar').length) { pintarActiva(); pintarGlobo(); }
        });
        // The server shows and hides links (tier, permission, the admin
        // deployment) and fills the Vencidos count: repaint on each change.
        new MutationObserver(function() { pintarActiva(); pintarGlobo(); })
          .observe(p, { subtree: true, childList: true, characterData: true,
                        attributes: true, attributeFilter: ['style', 'class'] });
        pintarActiva(); pintarGlobo();
      }
      $(iniciar);
      $(document).on('shiny:connected', iniciar);
    })();
    "))
  )
}

# ── hd_numero_input(): a number field with no spinner and no wheel (T66) ──────
# Luis scrolled down over "Sobretasa (bps)" and the number changed: a
# <input type="number"> takes the mouse wheel and the arrow keys. This is a
# text field (inputmode="decimal", the phone shows the number pad) that:
#   - never changes with the wheel or the arrow keys (the page scrolls);
#   - takes "608,763.35" or "608763.35" (the comma only as a well-placed
#     thousands separator: "1,5" is not a number);
#   - sends the server a NUMBER, or NA when empty or not a number, never text,
#     under the same inputId (type "shiny.number", as numericInput does), and
#     only when the field is left or on Enter, never on every key; the same
#     number is never sent twice;
#   - formato (required): "monto" (thousands separator when the field is left,
#     rounded to `decimales`, 2 by default: what is shown is what is sent),
#     "tasa" (bps, %: no separator, no padding zeros, 4 decimals by default)
#     or "entero" (no decimals). More decimals than a tasa or entero takes
#     make it NA.
# min, max and step are NOT enforced in the browser (numericInput only marked
# the field): the server validates the range, module by module.
# JS that reads one of these fields uses hdNumero.valor(el), never parseFloat
# on its text ("1,234" would be 1). www/hd_numero.js.
HD_NUMERO_FORMATOS <- c(monto = 2L, tasa = 4L, entero = 0L)

.hd_numero_decimales <- function(formato, decimales = NULL) {
  if (!is.character(formato) || length(formato) != 1L || !formato %in% names(HD_NUMERO_FORMATOS))
    stop("hd_numero_input(): formato must be one of ", paste(names(HD_NUMERO_FORMATOS), collapse = ", "), call. = FALSE)
  if (identical(formato, "entero")) return(0L)
  if (is.null(decimales)) return(HD_NUMERO_FORMATOS[[formato]])
  d <- suppressWarnings(as.integer(decimales))
  if (length(d) != 1L || is.na(d) || d < 0L || d > 10L) stop("hd_numero_input(): decimales must be 0..10", call. = FALSE)
  d
}

# The text a value shows: a monto with its thousands separator and exactly
# `decimales`; a tasa or entero with no separator and no padding zeros.
hd_numero_texto <- function(x, formato, decimales = NULL) {
  d <- .hd_numero_decimales(formato, decimales)
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || is.na(x[1]) || !is.finite(x[1])) return("")
  x <- x[1]
  if (identical(formato, "monto"))   # monto_moneda() (R/monedas.R), with the minus a person types
    return(sub("−", "-", monto_moneda(x, NA_character_, decimales = d, codigo = FALSE), fixed = TRUE))
  formatC(x, format = "f", digits = max(d, 0L), drop0trailing = TRUE)
}

hd_numero_deps <- function() {
  htmltools::singleton(htmltools::tags$head(
    htmltools::tags$script(src = "hd_numero.js?v=1")
  ))
}

hd_numero_input <- function(inputId, label, formato, value = NA, min = NA, max = NA, step = NA,
                            decimales = NULL, width = NULL, placeholder = NULL) {
  if (missing(formato)) stop("hd_numero_input(): formato is required (monto, tasa or entero)", call. = FALSE)
  d <- .hd_numero_decimales(formato, decimales)
  num_attr <- function(v) { v <- suppressWarnings(as.numeric(v)); if (length(v) && !is.na(v[1])) format(v[1], scientific = FALSE) else NULL }
  etiqueta <- htmltools::tags$label(class = "control-label", id = paste0(inputId, "-label"), `for` = inputId,
                                    class = if (is.null(label)) "shiny-label-null", label)
  htmltools::tagList(
    hd_numero_deps(),
    htmltools::div(
      class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width:", htmltools::validateCssUnit(width), ";"),
      etiqueta,
      htmltools::tags$input(
        id = inputId, type = "text", class = "form-control hd-numero",
        inputmode = if (d > 0L) "decimal" else "numeric",
        autocomplete = "off", spellcheck = "false",
        value = hd_numero_texto(value, formato, d),
        placeholder = placeholder,
        `data-formato` = formato, `data-decimales` = d,
        `data-min` = num_attr(min), `data-max` = num_attr(max), `data-step` = num_attr(step)
      )
    )
  )
}

# The updateNumericInput of hd_numero_input(): value = NA empties the field.
# The same number it already has changes nothing and sends nothing.
hd_numero_update <- function(session = shiny::getDefaultReactiveDomain(), inputId,
                             label = NULL, value = NULL, min = NULL, max = NULL) {
  msg <- list()
  if (!is.null(label)) msg$label <- as.character(label)
  if (!is.null(value)) {
    v <- suppressWarnings(as.numeric(value))
    if (length(v) && !is.na(v[1]) && is.finite(v[1])) msg$value <- v[1] else msg$vaciar <- TRUE
  }
  if (!is.null(min)) msg$min <- if (is.na(min)) "" else format(as.numeric(min), scientific = FALSE)
  if (!is.null(max)) msg$max <- if (is.na(max)) "" else format(as.numeric(max), scientific = FALSE)
  session$sendInputMessage(inputId, msg)
  invisible(msg)
}
