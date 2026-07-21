# =============================================================================
# R/reporte_module.R
# PDF Financial Summary Report — "Exportar" button in Bancos triggers this.
#
# Architecture:
#   - reporteUI("rpt")   → hidden nav_panel("RPT") in app.R
#   - reporteServer()    → wired in app.R after bancosServer()
#   - Triggered by actionButton("btn_export") in bancosUI toolbar
#   - Data pulled exclusively from shared$* reactiveVals (no extra S3 reads)
#   - PDF via pagedown::chrome_print() on a temp HTML file
#
# Future: tier="viewer" users see ONLY this panel.
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────
reporteUI <- function(id) {
  ns <- NS(id)

  # SVG icon helpers (inline, no external library)
  svg_back <- '<svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="15 18 9 12 15 6"/></svg>'
  svg_expand <- '<svg class="fs-expand" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="15 3 21 3 21 9"/><polyline points="9 21 3 21 3 15"/><line x1="21" y1="3" x2="14" y2="10"/><line x1="3" y1="21" x2="10" y2="14"/></svg>'
  svg_compress <- '<svg class="fs-compress" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round" style="display:none"><polyline points="4 14 10 14 10 20"/><polyline points="20 10 14 10 14 4"/><line x1="10" y1="14" x2="3" y2="21"/><line x1="21" y1="3" x2="14" y2="10"/></svg>'
  svg_gear <- '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="3"/><path d="M19.4 15a1.65 1.65 0 0 0 .33 1.82l.06.06a2 2 0 0 1-2.83 2.83l-.06-.06a1.65 1.65 0 0 0-1.82-.33 1.65 1.65 0 0 0-1 1.51V21a2 2 0 0 1-4 0v-.09A1.65 1.65 0 0 0 9 19.4a1.65 1.65 0 0 0-1.82.33l-.06.06a2 2 0 0 1-2.83-2.83l.06-.06A1.65 1.65 0 0 0 4.68 15a1.65 1.65 0 0 0-1.51-1H3a2 2 0 0 1 0-4h.09A1.65 1.65 0 0 0 4.6 9a1.65 1.65 0 0 0-.33-1.82l-.06-.06a2 2 0 0 1 2.83-2.83l.06.06A1.65 1.65 0 0 0 9 4.68a1.65 1.65 0 0 0 1-1.51V3a2 2 0 0 1 4 0v.09a1.65 1.65 0 0 0 1 1.51 1.65 1.65 0 0 0 1.82-.33l.06-.06a2 2 0 0 1 2.83 2.83l-.06.06A1.65 1.65 0 0 0 19.4 9a1.65 1.65 0 0 0 1.51 1H21a2 2 0 0 1 0 4h-.09a1.65 1.65 0 0 0-1.51 1z"/></svg>'
  svg_zoom_in  <- '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="8"/><line x1="21" y1="21" x2="16.65" y2="16.65"/><line x1="11" y1="8" x2="11" y2="14"/><line x1="8" y1="11" x2="14" y2="11"/></svg>'
  svg_zoom_out <- '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><circle cx="11" cy="11" r="8"/><line x1="21" y1="21" x2="16.65" y2="16.65"/><line x1="8" y1="11" x2="14" y2="11"/></svg>'

  NS_PREFIX <- ns("")   # e.g. "rpt-"

  tagList(
    # ── CSS ──────────────────────────────────────────────────────────────────
    tags$style(HTML("
      .rpt-fab-main {
        width:44px;height:44px;border-radius:50%;background:#f5f0e8;
        border:none;cursor:pointer;display:flex;align-items:center;
        justify-content:center;box-shadow:0 2px 10px rgba(0,0,0,0.20);
        color:#374151;transition:transform .15s ease,box-shadow .15s ease;
        flex-shrink:0;
      }
      .rpt-fab-main:hover { transform:scale(1.08); box-shadow:0 4px 16px rgba(0,0,0,0.26); }
      .rpt-fab-bubble {
        width:38px;height:38px;border-radius:50%;background:#f5f0e8;
        border:none;cursor:pointer;display:none;align-items:center;
        justify-content:center;box-shadow:0 2px 8px rgba(0,0,0,0.16);
        color:#374151;transition:transform .15s ease;flex-shrink:0;
      }
      .rpt-fab-bubble:hover { transform:scale(1.1); }
      .rpt-settings-panel {
        position:fixed;top:0;right:-340px;width:320px;height:100vh;
        background:#fff;z-index:10020;
        box-shadow:-4px 0 24px rgba(0,0,0,0.13);
        transition:right .28s cubic-bezier(.4,0,.2,1);
        overflow-y:auto;border-left:1px solid #e5e7eb;
      }
    ")),

    # ── Full-screen report iframe wrapper ────────────────────────────────────
    div(
      id    = ns("rpt_main_wrap"),
      style = "position:fixed;inset:0;background:#fff;z-index:500;overflow:hidden;display:none;",
      uiOutput(ns("report_viewer"))
    ),

    # ── Floating Action Button cluster ───────────────────────────────────────
    div(
      id    = ns("fab_root"),
      style = "position:fixed;bottom:24px;right:24px;z-index:10010;display:none;flex-direction:column;align-items:center;gap:10px;",

      # Bubble: Back (←)  — top of stack
      tags$button(id = ns("fab_back"), class = "rpt-fab-bubble",
                  title = "Volver a Bancos", HTML(svg_back)),

      # Bubble: Zoom In
      tags$button(id = ns("fab_zoom_in"), class = "rpt-fab-bubble",
                  title = "Acercar (+)", HTML(svg_zoom_in)),

      # Zoom level indicator
      tags$div(
        id    = ns("zoom_label"),
        style = paste0(
          "display:none;font-size:.68rem;font-weight:700;color:#374151;",
          "background:#f5f0e8;border-radius:10px;padding:2px 8px;",
          "box-shadow:0 1px 4px rgba(0,0,0,.12);text-align:center;",
          "min-width:38px;line-height:1.6;cursor:default;user-select:none;"
        ),
        "100%"
      ),

      # Bubble: Zoom Out
      tags$button(id = ns("fab_zoom_out"), class = "rpt-fab-bubble",
                  title = "Alejar (\u2212)", HTML(svg_zoom_out)),

      # Bubble: Fullscreen
      tags$button(id = ns("fab_fullscreen"), class = "rpt-fab-bubble",
                  title = "Pantalla completa",
                  HTML(paste0(svg_expand, svg_compress))),

      # Bubble: Settings (gear)
      tags$button(id = ns("fab_settings"), class = "rpt-fab-bubble",
                  title = "Configuración", HTML(svg_gear)),

      # Main FAB button — hamburger / ×
      tags$button(
        id = ns("fab_main"), class = "rpt-fab-main", title = "Acciones",
        HTML(paste0(
          '<span id="', ns("fab_lines"),
          '" style="display:flex;flex-direction:column;gap:3px;align-items:center;">',
          '<span style="display:block;width:16px;height:2px;background:currentColor;border-radius:1px;"></span>',
          '<span style="display:block;width:16px;height:2px;background:currentColor;border-radius:1px;"></span>',
          '<span style="display:block;width:16px;height:2px;background:currentColor;border-radius:1px;"></span>',
          '</span>',
          '<span id="', ns("fab_x"),
          '" style="display:none;font-size:22px;font-weight:300;line-height:1;">&#215;</span>'
        ))
      )
    ),

    # ── Settings panel (slides in from right) ────────────────────────────────
    div(
      id = ns("rpt_settings_panel"),
      class = "rpt-settings-panel",

      # Header
      div(
        style = "display:flex;align-items:center;justify-content:space-between;padding:16px 20px;border-bottom:1px solid #f0f0f0;position:sticky;top:0;background:#fff;z-index:1;",
        tags$span(style = "font-weight:600;font-size:.95rem;", "Configuración del reporte"),
        tags$button(
          id    = ns("rpt_settings_close"),
          style = "background:none;border:none;cursor:pointer;font-size:22px;color:#6b7280;padding:0 4px;line-height:1;",
          HTML("&#215;")
        )
      ),

      # Body
      div(
        style = "padding:20px;",

        tags$p(style = "font-size:.78rem;font-weight:600;text-transform:uppercase;letter-spacing:.05em;color:#6b7280;margin-bottom:8px;",
               "Período"),
        tags$label("Desde", class = "form-label small"),
        dateInput(ns("fecha_desde"), NULL, value = floor_date_month(),
                  format = "dd/mm/yyyy", language = "es", width = "100%"),
        tags$label("Hasta", class = "form-label small"),
        dateInput(ns("fecha_hasta"), NULL, value = Sys.Date(),
                  format = "dd/mm/yyyy", language = "es", width = "100%"),

        tags$hr(class = "my-3"),

        tags$p(style = "font-size:.78rem;font-weight:600;text-transform:uppercase;letter-spacing:.05em;color:#6b7280;margin-bottom:8px;",
               "Empresas"),
        uiOutput(ns("empresa_checks")),

        tags$hr(class = "my-3"),

        uiOutput(ns("preview_summary")),

        tags$hr(class = "my-3"),

        actionButton(ns("btn_apply"),
                     label = tagList(icon("rotate"), " Aplicar y generar"),
                     class = "btn btn-primary w-100 mb-3"),

        div(class = "d-flex gap-2",
          downloadButton(ns("dl_html"),
                         label = tagList(icon("code"), " HTML"),
                         class = "btn btn-sm btn-outline-secondary flex-fill"),
          downloadButton(ns("dl_pdf"),
                         label = tagList(icon("file-pdf"), " PDF"),
                         class = "btn btn-sm btn-outline-secondary flex-fill")
        ),

        tags$p(style = "font-size:.73rem;color:#9ca3af;margin-top:12px;margin-bottom:0;",
               tags$em("PDF requiere Chrome/Edge y ", tags$code("pagedown"), "."))
      )
    ),

    # ── JavaScript ───────────────────────────────────────────────────────────
    tags$script(HTML(paste0('
(function() {
  var NS = "', NS_PREFIX, '";
  function el(id)  { return document.getElementById(NS + id); }
  function qel(id) { return document.getElementById(id); }
  var fabOpen = false;

  function setDisplay(id, v) { var e=el(id); if(e) e.style.display=v; }

  function toggleFab() {
    fabOpen = !fabOpen;
    ["fab_back","fab_zoom_in","fab_zoom_out","fab_fullscreen","fab_settings"].forEach(function(id) {
      setDisplay(id, fabOpen ? "flex" : "none");
    });
    // zoom label uses inline-block to stay compact
    var zl = el("zoom_label");
    if (zl) zl.style.display = fabOpen ? "block" : "none";
    var lines = el("fab_lines"), x = el("fab_x");
    if (lines) lines.style.display = fabOpen ? "none" : "flex";
    if (x)     x.style.display     = fabOpen ? "block" : "none";
  }
  function closeFab() { if (fabOpen) toggleFab(); }

  function openSettings()  {
    var p = el("rpt_settings_panel"); if (p) p.style.right = "0";
  }
  function closeSettings() {
    var p = el("rpt_settings_panel"); if (p) p.style.right = "-340px";
  }

  // ── Zoom ────────────────────────────────────────────────────────────────────
  var zoomLevel = 1.0;
  var ZOOM_STEP = 0.15;
  var ZOOM_MIN  = 0.4;
  var ZOOM_MAX  = 2.5;

  function getIframe() {
    var wrap = el("rpt_main_wrap");
    return wrap ? wrap.querySelector("iframe") : null;
  }

  function applyZoom() {
    var iframe = getIframe();
    if (!iframe) return;
    try {
      var doc  = iframe.contentDocument || iframe.contentWindow.document;
      var body = doc && doc.body;
      if (body) {
        // zoom works in Chrome/Edge; transform fallback for Firefox
        body.style.zoom = "";
        body.style.transform = "";
        body.style.transformOrigin = "";
        body.style.width = "";
        if ("zoom" in body.style) {
          body.style.zoom = zoomLevel;
        } else {
          body.style.transform = "scale(" + zoomLevel + ")";
          body.style.transformOrigin = "0 0";
          body.style.width = (100 / zoomLevel) + "%";
        }
      }
    } catch(e) {}
    updateZoomLabel();
  }

  function updateZoomLabel() {
    var lbl = el("zoom_label");
    if (lbl) lbl.textContent = Math.round(zoomLevel * 100) + "%";
  }

  function zoomIn() {
    zoomLevel = Math.min(ZOOM_MAX, Math.round((zoomLevel + ZOOM_STEP) * 100) / 100);
    applyZoom();
  }
  function zoomOut() {
    zoomLevel = Math.max(ZOOM_MIN, Math.round((zoomLevel - ZOOM_STEP) * 100) / 100);
    applyZoom();
  }
  function resetZoom() {
    zoomLevel = 1.0;
    applyZoom();
  }

  // Reapply zoom whenever a new report is rendered into the iframe
  $(document).on("shiny:value", function(e) {
    if (e.name === NS + "report_viewer") {
      // iframe is re-created; wait for it to load then reapply
      setTimeout(function() { applyZoom(); }, 200);
    }
  });
  // ────────────────────────────────────────────────────────────────────────────

  function toggleFullscreen() {
    if (!document.fullscreenElement) {
      document.documentElement.requestFullscreen &&
        document.documentElement.requestFullscreen().catch(function(){});
    } else {
      document.exitFullscreen && document.exitFullscreen();
    }
  }
  document.addEventListener("fullscreenchange", function() {
    var inFs = !!document.fullscreenElement;
    var wrap = el("rpt_main_wrap");
    if (wrap) {
      wrap.querySelectorAll(".fs-expand").forEach(function(e){ e.style.display = inFs?"none":"block"; });
      wrap.querySelectorAll(".fs-compress").forEach(function(e){ e.style.display = inFs?"block":"none"; });
    }
    // Also update icons in the fab cluster itself
    var fab = el("fab_root");
    if (fab) {
      fab.querySelectorAll(".fs-expand").forEach(function(e){ e.style.display = inFs?"none":"block"; });
      fab.querySelectorAll(".fs-compress").forEach(function(e){ e.style.display = inFs?"block":"none"; });
    }
  });

  // Event delegation
  document.addEventListener("click", function(e) {
    var t = e.target;
    if (t.closest("#" + NS + "fab_main"))    { toggleFab();   return; }
    if (t.closest("#" + NS + "fab_back")) {
      closeFab();
      var bncTab = document.querySelector(".nav-link[data-value=BNC]");
      if (bncTab) bncTab.click();
      return;
    }
    if (t.closest("#" + NS + "fab_zoom_in"))    { zoomIn();  return; }
    if (t.closest("#" + NS + "fab_zoom_out"))   { zoomOut(); return; }
    if (t.closest("#" + NS + "fab_fullscreen")) { toggleFullscreen(); return; }
    if (t.closest("#" + NS + "fab_settings"))   { openSettings(); closeFab(); return; }
    if (t.closest("#" + NS + "rpt_settings_close")) { closeSettings(); return; }
  });

  // Messages from Shiny server
  Shiny.addCustomMessageHandler("rpt_close_settings", function(m) { closeSettings(); });
  Shiny.addCustomMessageHandler("rpt_reset_fab",       function(m) { closeFab(); closeSettings(); resetZoom(); });
})();
    ')))
  )
}

# Helper: first day of current month (used for dateInput default)
floor_date_month <- function() {
  d <- Sys.Date()
  as.Date(format(d, "%Y-%m-01"))
}

# ── Server ────────────────────────────────────────────────────────────────────
reporteServer <- function(id, shared, active_tab = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Applied settings (committed values used for rendering) ───────────────
    applied <- reactiveValues(
      desde    = floor_date_month(),
      hasta    = Sys.Date(),
      empresas = NULL,   # NULL → use all_empresas()
      trigger  = 0L
    )

    # ── Empresa checkboxes (derived from ctas_cuentas) ───────────────────────
    all_empresas <- reactive({
      cts <- tryCatch(shared$ctas_cuentas(), error = function(e) NULL)
      emps <- if (is.null(cts) || !nrow(cts)) sort(names(COMPANY_MAP))
              else sort(unique(trimws(cts$Empresa)))
      allowed <- tryCatch(shared$visible_initials(), error = function(e) NULL)
      if (!is.null(allowed)) emps <- intersect(emps, allowed)
      emps
    })

    output$empresa_checks <- renderUI({
      emps <- all_empresas()
      div(class = "d-flex flex-wrap gap-3",
        lapply(emps, function(e) {
          div(class = "form-check",
            tags$input(
              type = "checkbox", class = "form-check-input",
              id   = ns(paste0("chk_emp_", e)),
              checked = "checked"
            ),
            tags$label(class = "form-check-label small",
                        `for` = ns(paste0("chk_emp_", e)), e)
          )
        })
      )
    })

    # Collect selected empresas from checkboxes
    selected_empresas <- reactive({
      emps <- all_empresas()
      Filter(function(e) {
        isTRUE(input[[paste0("chk_emp_", e)]])
      }, emps)
    })

    # ── Auto-activate: trigger first render when RPT tab is visited ──────────
    .first_render_done <- FALSE
    if (!is.null(active_tab)) {
      observeEvent(active_tab(), {
        val <- active_tab()
        if (isTRUE(val == "RPT")) {
          session$sendCustomMessage("rpt_reset_fab", list())
          if (!.first_render_done) {
            .first_render_done <<- TRUE
            emps <- isolate(all_empresas())
            if (is.null(applied$empresas))
              applied$empresas <- if (length(emps)) emps else NULL
            applied$trigger <- isolate(applied$trigger) + 1L
          }
        }
      }, ignoreNULL = TRUE, ignoreInit = FALSE, priority = -1)
    }

    # ── Apply button: commit settings and re-render ──────────────────────────
    observeEvent(input$btn_apply, {
      applied$desde    <- input$fecha_desde
      applied$hasta    <- input$fecha_hasta
      applied$empresas <- selected_empresas()
      applied$trigger  <- isolate(applied$trigger) + 1L
      session$sendCustomMessage("rpt_close_settings", list())
    }, ignoreInit = TRUE)

    # ── FAB back button → switch to BNC ─────────────────────────────────────
    observeEvent(input$fab_back_click, {
      shiny::updateNavbarPage(
        session = shiny::getDefaultReactiveDomain(),
        inputId = "nav", selected = "BNC"
      )
    }, ignoreInit = TRUE)

    # ctas_bancos loaded once for bank-name resolution (not in shared) — mirrors
    # bancos_module.R's ctas_bancos_rv pattern. banco_id is a FK to ctas_bancos$id,
    # not a display name; without this join every account's bank falls back to "—".
    ctas_bancos_rv <- reactiveVal(NULL)
    observe({
      cid <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
      ctas_bancos_rv(tryCatch(
        load_ctas_bancos(client_id = cid), error = function(e) .schema_ctas_bancos()
      ))
    })

    # ── Balance computation (mirrors bancos_module balance logic) ────────────
    compute_balances <- reactive({
      cts_raw <- tryCatch(shared$ctas_cuentas(), error = function(e) NULL)
      movs    <- tryCatch(shared$bancos_movimientos(), error = function(e) NULL)
      if (is.null(cts_raw) || !nrow(cts_raw)) return(NULL)

      bancos_df <- ctas_bancos_rv()
      banco_map <- if (!is.null(bancos_df) && nrow(bancos_df))
        setNames(bancos_df$nombre, bancos_df$id) else character(0)

      movs_clean <- if (!is.null(movs) && nrow(movs))
        dplyr::filter(movs, !eliminado, fuente != "agenda")
      else
        tibble::tibble(cuenta_id = character(), cargo = numeric(),
                       abono = numeric(), fecha = as.Date(character()))

      saldos <- if (nrow(movs_clean)) {
        movs_clean |>
          dplyr::group_by(cuenta_id) |>
          dplyr::summarise(
            total_cargo = sum(cargo, na.rm = TRUE),
            total_abono = sum(abono, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        tibble::tibble(cuenta_id = character(),
                       total_cargo = numeric(), total_abono = numeric())
      }

      bals <- cts_raw |>
        dplyr::transmute(
          cuenta_id  = id,
          empresa    = Empresa,
          moneda     = Moneda,
          alias      = alias,
          banco      = dplyr::coalesce(banco_map[banco_id], "—"),
          saldo_inicial = dplyr::coalesce(as.numeric(saldo_inicial), 0)
        ) |>
        dplyr::left_join(saldos, by = "cuenta_id") |>
        dplyr::mutate(
          total_cargo = dplyr::coalesce(total_cargo, 0),
          total_abono = dplyr::coalesce(total_abono, 0),
          saldo       = saldo_inicial + total_abono - total_cargo
        )
      # Return both balances and clean movements for top-movements computation
      list(balances = bals, movimientos = movs_clean)
    })

    # ── Cash Flow Pulse — AP/AR forward cash flow by week ────────────────────
    compute_pulse <- reactive({
      sap_raw  <- tryCatch(shared$sap_data(), error = function(e) NULL)
      tags_raw <- tryCatch(shared$tags_db(),  error = function(e) NULL)
      if (is.null(sap_raw)) return(NULL)

      # normalize_sap(): processes a raw SAP snapshot frame into a clean tibble
      # with standardised column names that the pulse logic expects.
      # Uses the same pipeline helpers as build_ledger_df() (all in global scope).
      normalize_sap <- function(raw, ledger_type) {
        empty <- tibble::tibble(
          FechaEff       = as.Date(character()),
          Parte          = character(),
          Moneda         = character(),
          `Saldo vencido` = numeric(),
          Empresa        = character(),
          Documento      = character()
        )
        if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) return(empty)

        # Apply data-pipeline helpers (defined in R/data_pipeline.R, global scope)
        df <- tryCatch({
          df <- ensure_dates(raw)       # parses "Fecha de vencimiento" → Date
          df <- ensure_amounts(df)      # parses "Saldo vencido" → numeric
          df <- ensure_moneda(df)       # normalises/creates Moneda column
          df
        }, error = function(e) raw)

        # Helper: return first matching column value or a fallback vector
        pick <- function(cols, fallback)
          if (length(m <- Filter(function(c) c %in% names(df), cols)))
            df[[m[1]]] else fallback

        # Effective date — prefer already-processed FechaEff, else due-date column
        fecha_raw <- pick(c("FechaEff",
                             "Fecha de vencimiento",
                             "DocDueDate"), NA)
        fecha_eff <- tryCatch(as.Date(fecha_raw), error = function(e) as.Date(NA))

        # Amount — already made numeric by ensure_amounts
        importe <- abs(suppressWarnings(as.numeric(
          pick(c("Saldo vencido", "DocTotal", "Importe"), 0)
        )))

        # Currency — ensure_moneda should have created "Moneda"
        moneda_raw <- as.character(
          pick(c("Moneda", "Currency", "Divisa"), "MXN")
        )
        moneda_raw <- toupper(trimws(moneda_raw))
        moneda_raw <- ifelse(moneda_raw %in% c("MXP", "MXV"), "MXN", moneda_raw)

        # Party name
        ar_name_cols <- c("Parte","Nombre del cliente","Cliente",
                          "Customer","CardName","Socio de negocios")
        ap_name_cols <- c("Parte","Nombre de acreedor","Nombre del proveedor",
                          "Proveedor","Vendor","CardName")
        parte <- as.character(
          pick(if (ledger_type == "AR") ar_name_cols else ap_name_cols,
               NA_character_)
        )

        # Supporting columns for tag join
        empresa  <- as.character(pick(c("Empresa","Company"),  NA_character_))
        documento <- as.character(pick(c("Documento","Nº de documento",
                                         "DocNum","DocEntry"), NA_character_))

        tibble::tibble(
          FechaEff        = fecha_eff,
          Parte           = parte,
          Moneda          = moneda_raw,
          `Saldo vencido` = importe,
          Empresa         = empresa,
          Documento       = documento
        )
      }

      ar_all <- normalize_sap(sap_raw$AR, "AR")
      ap_all <- normalize_sap(sap_raw$AP, "AP")

      # Attach urgency / importance tags to AP rows
      if (!is.null(tags_raw) && is.data.frame(tags_raw) && nrow(tags_raw) > 0 &&
          nrow(ap_all) > 0 &&
          all(c("Empresa", "Moneda", "Documento") %in% names(ap_all))) {
        tag_sum <- tags_raw |>
          dplyr::filter(ledger == "AP") |>
          dplyr::group_by(Empresa, Moneda, Documento) |>
          dplyr::summarise(
            is_urgent    = any(tag == "urgent",    na.rm = TRUE),
            is_important = any(tag == "important", na.rm = TRUE),
            .groups = "drop"
          )
        ap_all <- dplyr::left_join(ap_all, tag_sum,
                                   by = c("Empresa", "Moneda", "Documento")) |>
          dplyr::mutate(
            is_urgent    = dplyr::coalesce(is_urgent,    FALSE),
            is_important = dplyr::coalesce(is_important, FALSE)
          )
      } else {
        if (nrow(ap_all) > 0)
          ap_all <- dplyr::mutate(ap_all, is_urgent = FALSE, is_important = FALSE)
      }

      today   <- Sys.Date()
      w0_cur  <- lubridate::floor_date(today, "week", week_start = 1)
      w1_cur  <- w0_cur + 6L
      w0_next <- w0_cur + 7L
      w1_next <- w0_next + 6L

      # Build daily timeline (7 rows) for one currency slice
      mk_daily <- function(ar_c, ap_c, days) {
        sv <- "Saldo vencido"
        d_ar <- if (nrow(ar_c) && sv %in% names(ar_c))
          dplyr::group_by(ar_c, date = FechaEff) |>
          dplyr::summarise(inflow  = sum(abs(as.numeric(.data[[sv]])), na.rm = TRUE),
                           .groups = "drop")
        else tibble::tibble(date = as.Date(character()), inflow = numeric())

        d_ap <- if (nrow(ap_c) && sv %in% names(ap_c))
          dplyr::group_by(ap_c, date = FechaEff) |>
          dplyr::summarise(outflow = sum(abs(as.numeric(.data[[sv]])), na.rm = TRUE),
                           .groups = "drop")
        else tibble::tibble(date = as.Date(character()), outflow = numeric())

        tibble::tibble(date = days) |>
          dplyr::left_join(d_ar, by = "date") |>
          dplyr::left_join(d_ap, by = "date") |>
          dplyr::mutate(
            inflow  = dplyr::coalesce(inflow,  0),
            outflow = dplyr::coalesce(outflow, 0),
            cum_net = cumsum(inflow - outflow)
          )
      }

      # Build data for one currency × one week
      mk_currency <- function(currency, wk_start, wk_end,
                               focus_start, focus_end, focus_label) {
        sv  <- "Saldo vencido"
        hd  <- "FechaEff"
        ar_wk <- if (nrow(ar_all) && hd %in% names(ar_all))
          dplyr::filter(ar_all, Moneda == currency, !is.na(FechaEff),
                        FechaEff >= wk_start, FechaEff <= wk_end)
        else tibble::tibble()
        ap_wk <- if (nrow(ap_all) && hd %in% names(ap_all))
          dplyr::filter(ap_all, Moneda == currency, !is.na(FechaEff),
                        FechaEff >= wk_start, FechaEff <= wk_end)
        else tibble::tibble()

        focus_ar <- if (nrow(ar_all) && hd %in% names(ar_all) && sv %in% names(ar_all))
          dplyr::filter(ar_all, Moneda == currency, !is.na(FechaEff),
                        FechaEff >= focus_start, FechaEff <= focus_end) |>
          dplyr::mutate(.a = abs(as.numeric(.data[[sv]]))) |>
          dplyr::arrange(dplyr::desc(.a)) |>
          dplyr::slice_head(n = 3)
        else tibble::tibble()

        focus_ap <- if (nrow(ap_all) && hd %in% names(ap_all) && sv %in% names(ap_all))
          dplyr::filter(ap_all, Moneda == currency, !is.na(FechaEff),
                        FechaEff >= focus_start, FechaEff <= focus_end) |>
          dplyr::mutate(.a = abs(as.numeric(.data[[sv]]))) |>
          dplyr::arrange(dplyr::desc(.a)) |>
          dplyr::slice_head(n = 5)
        else tibble::tibble()

        list(
          currency      = currency,
          inflow_total  = if (nrow(ar_wk) && sv %in% names(ar_wk))
            sum(abs(as.numeric(ar_wk[[sv]])), na.rm = TRUE) else 0,
          outflow_total = if (nrow(ap_wk) && sv %in% names(ap_wk))
            sum(abs(as.numeric(ap_wk[[sv]])), na.rm = TRUE) else 0,
          daily         = mk_daily(ar_wk, ap_wk,
                                   seq.Date(wk_start, wk_end, by = "day")),
          focus_ar      = focus_ar,
          focus_ap      = focus_ap,
          focus_label   = focus_label
        )
      }

      # Build one week panel (both currencies + other-currency detection)
      mk_week <- function(wk_start, wk_end, focus_start, focus_end, focus_label) {
        hd <- "FechaEff"
        ar_w <- if (nrow(ar_all) && hd %in% names(ar_all))
          dplyr::filter(ar_all, !is.na(FechaEff),
                        FechaEff >= wk_start, FechaEff <= wk_end)
        else tibble::tibble(Moneda = character())
        ap_w <- if (nrow(ap_all) && hd %in% names(ap_all))
          dplyr::filter(ap_all, !is.na(FechaEff),
                        FechaEff >= wk_start, FechaEff <= wk_end)
        else tibble::tibble(Moneda = character())

        list(
          mxn              = mk_currency("MXN", wk_start, wk_end,
                                         focus_start, focus_end, focus_label),
          usd              = mk_currency("USD", wk_start, wk_end,
                                         focus_start, focus_end, focus_label),
          other_currencies = setdiff(unique(c(ar_w$Moneda, ap_w$Moneda)),
                                     c("MXN", "USD")),
          wk_start         = wk_start,
          wk_end           = wk_end
        )
      }

      list(
        today     = today,
        current   = mk_week(w0_cur,  w1_cur,  today,   today,   "Hoy"),
        next_week = mk_week(w0_next, w1_next, w0_next, w1_next, "Sem. Pr\u00f3xima")
      )
    })

    # ── Report HTML (computed from applied settings only) ────────────────────
    report_html <- reactive({
      applied$trigger  # explicit dependency — only re-renders when applied changes
      isolate({
        emps  <- applied$empresas %||% all_empresas()
        desde <- applied$desde
        hasta <- applied$hasta
        res   <- compute_balances()
        if (is.null(res) || !length(emps)) return(NULL)
        bals  <- dplyr::filter(res$balances, empresa %in% emps)
        if (!nrow(bals)) return(NULL)
        movs  <- if (!is.null(res$movimientos) && nrow(res$movimientos))
          dplyr::filter(res$movimientos, cuenta_id %in% bals$cuenta_id)
        else NULL
        pulse <- compute_pulse()
        .build_report_html(bals, movs, desde, hasta, Sys.time(), pulse = pulse)
      })
    })

    # ── Report viewer (iframe with srcdoc) ───────────────────────────────────
    output$report_viewer <- renderUI({
      html <- report_html()
      if (is.null(html)) {
        div(
          style = "display:flex;align-items:center;justify-content:center;height:100vh;",
          div(class = "text-center text-muted",
            div(class = "spinner-border mb-3", role = "status"),
            tags$p("Generando reporte…")
          )
        )
      } else {
        tags$iframe(
          srcdoc    = html,
          style     = "width:100%;height:100vh;border:none;display:block;",
          scrolling = "yes"
        )
      }
    })

    # ── Preview summary in settings panel (reflects live inputs) ─────────────
    output$preview_summary <- renderUI({
      res   <- compute_balances()
      bals  <- if (is.null(res)) NULL else res$balances
      emps  <- selected_empresas()
      if (is.null(bals) || !length(emps)) return(NULL)

      bals_sel <- dplyr::filter(bals, empresa %in% emps)
      mxn_tot  <- sum(bals_sel$saldo[bals_sel$moneda == "MXN"], na.rm = TRUE)
      usd_tot  <- sum(bals_sel$saldo[bals_sel$moneda == "USD"], na.rm = TRUE)
      n_accts  <- nrow(bals_sel)

      div(class = "alert alert-light border mb-0",
        div(class = "d-flex flex-wrap gap-4",
          div(
            tags$small(class = "text-muted d-block", "Total MXN"),
            tags$strong(class = "fs-6", fmt_money(mxn_tot))
          ),
          div(
            tags$small(class = "text-muted d-block", "Total USD"),
            tags$strong(class = "fs-6", fmt_money(usd_tot))
          ),
          div(
            tags$small(class = "text-muted d-block", "Cuentas"),
            tags$strong(class = "fs-6", n_accts)
          ),
          div(
            tags$small(class = "text-muted d-block", "Período"),
            tags$strong(class = "fs-6",
              paste0(format(input$fecha_desde, "%d/%m/%Y"),
                     " — ", format(input$fecha_hasta, "%d/%m/%Y")))
          )
        )
      )
    })

    # ── HTML download — uses applied (committed) settings ────────────────────
    output$dl_html <- downloadHandler(
      filename = function() {
        paste0("reporte_financiero_", format(Sys.Date(), "%Y%m%d"), ".html")
      },
      content = function(file) {
        res   <- isolate(compute_balances())
        emps  <- isolate(applied$empresas %||% all_empresas())
        desde <- isolate(applied$desde)
        hasta <- isolate(applied$hasta)
        if (is.null(res) || !length(emps)) {
          writeLines("<p>No hay datos.</p>", file); return()
        }
        bals_sel <- dplyr::filter(res$balances, empresa %in% emps)
        movs_sel <- if (!is.null(res$movimientos) && nrow(res$movimientos))
          dplyr::filter(res$movimientos, cuenta_id %in% bals_sel$cuenta_id) else NULL
        pulse_data <- isolate(compute_pulse())
        writeLines(
          .build_report_html(bals_sel, movs_sel, desde, hasta, Sys.time(),
                             pulse = pulse_data),
          file, useBytes = FALSE
        )
      }
    )

    # ── PDF download — uses applied (committed) settings ─────────────────────
    output$dl_pdf <- downloadHandler(
      filename = function() {
        paste0("reporte_financiero_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        res   <- isolate(compute_balances())
        emps  <- isolate(applied$empresas %||% all_empresas())
        desde <- isolate(applied$desde)
        hasta <- isolate(applied$hasta)

        if (is.null(res) || !length(emps)) {
          writeLines("No hay datos para exportar.", file); return()
        }

        bals_sel <- dplyr::filter(res$balances, empresa %in% emps)
        movs_sel <- if (!is.null(res$movimientos) && nrow(res$movimientos))
          dplyr::filter(res$movimientos, cuenta_id %in% bals_sel$cuenta_id) else NULL
        pulse_data   <- isolate(compute_pulse())
        html_content <- .build_report_html(
          balances    = bals_sel,
          movimientos = movs_sel,
          desde       = desde,
          hasta       = hasta,
          generated   = Sys.time(),
          pulse       = pulse_data
        )

        tmp_html <- tempfile(fileext = ".html")
        on.exit(unlink(tmp_html), add = TRUE)
        writeLines(html_content, tmp_html, useBytes = FALSE)

        # ── pagedown installed? ──────────────────────────────────────────────
        if (!requireNamespace("pagedown", quietly = TRUE)) {
          writeLines(html_content, file)
          showNotification(
            tagList(
              tags$strong("Instala pagedown primero:"),
              tags$br(),
              tags$code('install.packages("pagedown")'),
              tags$br(),
              tags$small("Se descargo el reporte como HTML mientras tanto.")
            ),
            type = "warning", duration = 15
          )
          return()
        }

        tryCatch({
          # pagedown::chrome_print uses actual browser print CSS — reliable multi-page
          html_url <- paste0("file:///", gsub("\\\\", "/", normalizePath(tmp_html, winslash="/")))
          pagedown::chrome_print(
            input   = html_url,
            output  = file,
            options = list(
              printBackground  = TRUE,
              paperWidth       = 8.5,
              paperHeight      = 11,
              marginTop        = 0,
              marginBottom     = 0,
              marginLeft       = 0,
              marginRight      = 0,
              preferCSSPageSize = TRUE
            ),
            wait    = 3,
            verbose = 0
          )
          showNotification(
            "\u2713 PDF generado correctamente.",
            type = "message", duration = 3
          )
        }, error = function(e) {
          # Chrome found but print failed — deliver HTML and show error
          writeLines(html_content, file)
          showNotification(
            tagList(
              tags$strong("Error al generar PDF: "),
              tags$br(),
              conditionMessage(e),
              tags$br(),
              tags$small("Se descargo el reporte como HTML.")
            ),
            type = "error", duration = 10
          )
        })
      }
    )
  })
}

# =============================================================================
# .brand_inject() — replace static branding strings with dynamic group values.
# Called on the assembled HTML before returning from .build_report_html().
# Reads COMPANY_MAP from the global environment so it always reflects the
# latest state (empresas_module broadcasts updates on every save).
# =============================================================================
.brand_inject <- function(html) {
  cmap         <- tryCatch(get("COMPANY_MAP", envir = .GlobalEnv),
                           error = function(e) c(NG = "Networks Group"))
  grp_ini      <- names(cmap)[1]
  grp_name     <- unname(cmap[1])
  brand_name   <- paste0(grp_ini, " Finances")
  brand_letter <- substr(grp_ini, 1, 1)

  html <- gsub("NG Finances",    brand_name, html, fixed = TRUE)
  html <- gsub('class="rpt-logo">N<',
               paste0('class="rpt-logo">', brand_letter, '<'), html, fixed = TRUE)
  html <- gsub("Networks Group &mdash; Reporte Financiero",
               paste0(htmltools::htmlEscape(grp_name), " &mdash; Reporte Financiero"),
               html, fixed = TRUE)
  html <- gsub("Reporte Financiero &mdash; Networks Group",
               paste0("Reporte Financiero &mdash; ", htmltools::htmlEscape(grp_name)),
               html, fixed = TRUE)
  html
}

# =============================================================================
# .build_pulse_pages() — pages 3 & 4: current week + next week AP/AR pulse
# =============================================================================
.build_pulse_pages <- function(pulse, gen_label) {
  if (is.null(pulse)) return("")
  he   <- htmltools::htmlEscape
  fmt  <- function(x) formatC(abs(x), format = "f", digits = 2, big.mark = ",")
  fmts <- function(x) paste0(if (x < 0) "-$" else "$",
                              formatC(abs(x), format = "f", digits = 2, big.mark = ","))
  today <- pulse$today

  # ── SVG daily bar chart ────────────────────────────────────────────────────
  make_pulse_chart <- function(daily, today_date = NULL,
                                inflow_color  = "#0d9f8a",
                                outflow_color = "#92400e",
                                net_color     = "#f59e0b",
                                w = 436L, h = 88L) {
    padL <- 38L; padR <- 6L; padT <- 9L; padB <- 22L
    pw   <- w - padL - padR
    ph   <- h - padT - padB
    n    <- nrow(daily)
    if (n == 0) return(sprintf('<svg width="%d" height="%d"></svg>', w, h))

    max_bar <- max(c(daily$inflow, daily$outflow, 1), na.rm = TRUE)
    max_net <- max(abs(daily$cum_net), 1, na.rm = TRUE)
    max_val <- max(max_bar, max_net)
    y0      <- padT + ph / 2      # zero-line y coordinate
    half_ph <- ph / 2

    sc_bar <- function(v) max(0, round(v / max_val * half_ph, 1))
    sc_net <- function(v) round(y0 - v / max_val * half_ph, 1)

    fmt_k <- function(v) {
      av <- abs(v)
      if (av >= 1e6) sprintf("$%.1fM", v / 1e6)
      else if (av >= 1e3) sprintf("$%.0fk", v / 1e3)
      else sprintf("$%.0f", v)
    }

    # Y-axis grid (top / zero / bottom)
    grid_svg <- paste(vapply(c(max_val, 0, -max_val), function(v) {
      yp <- sc_net(v)
      sprintf(
        '<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#e5e7eb" stroke-width="0.5"/>
         <text x="%d" y="%.1f" text-anchor="end" font-size="5.5" fill="#9ca3af" dominant-baseline="middle">%s</text>',
        padL, yp, w - padR, yp, padL - 2L, yp, fmt_k(v))
    }, character(1)), collapse = "\n")

    grp_w <- pw / n
    bw    <- max(3L, floor(grp_w * 0.33))

    # Per-day bars + labels + TODAY marker
    bars_svg <- paste(vapply(seq_len(n), function(i) {
      d      <- daily[i, ]
      xmid   <- padL + (i - 0.5) * grp_w
      is_hoy <- isTRUE(!is.null(today_date) && !is.na(today_date) &&
                         d$date == today_date)

      in_h  <- sc_bar(d$inflow);  out_h <- sc_bar(d$outflow)
      in_r  <- if (in_h  > 0) sprintf(
        '<rect x="%.1f" y="%.1f" width="%d" height="%.1f" fill="%s" opacity="0.85" rx="1.5"/>',
        xmid - bw - 1, y0 - in_h, bw, in_h, inflow_color) else ""
      out_r <- if (out_h > 0) sprintf(
        '<rect x="%.1f" y="%.1f" width="%d" height="%.1f" fill="%s" opacity="0.72" rx="1.5"/>',
        xmid + 1, y0, bw, out_h, outflow_color) else ""
      hoy_m <- if (is_hoy) sprintf(
        '<line x1="%.1f" y1="%d" x2="%.1f" y2="%d" stroke="#f59e0b" stroke-width="0.8" stroke-dasharray="2,1.5" opacity="0.85"/>
         <text x="%.1f" y="%d" text-anchor="middle" font-size="5" font-weight="700" fill="#d97706">HOY</text>',
        xmid, padT, xmid, h - padB, xmid, padT - 2L) else ""
      x_lbl <- sprintf(
        '<text x="%.1f" y="%d" text-anchor="middle" font-size="5.5" fill="#6b7280">%s</text>',
        xmid, h - padB + 9L, format(d$date, "%a %d"))
      paste(hoy_m, in_r, out_r, x_lbl, sep = "\n")
    }, character(1)), collapse = "\n")

    # Cumulative net line
    net_pts <- paste(vapply(seq_len(n), function(i)
      sprintf("%.1f,%.1f", padL + (i - 0.5) * grp_w, sc_net(daily$cum_net[i])),
      character(1)), collapse = " ")
    net_line <- sprintf(
      '<polyline points="%s" fill="none" stroke="%s" stroke-width="1.4" stroke-linejoin="round"/>',
      net_pts, net_color)

    # Zero line
    zero <- sprintf(
      '<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#374151" stroke-width="0.6" opacity="0.3"/>',
      padL, y0, w - padR, y0)

    # Legend (upper-right)
    lx <- w - padR - 118L; ly <- padT - 2L
    legend <- sprintf(
      '<rect x="%d" y="%d" width="7" height="5" fill="%s" opacity="0.85" rx="1"/>
       <text x="%d" y="%d" font-size="5" fill="#374151" dominant-baseline="middle">Cobros</text>
       <rect x="%d" y="%d" width="7" height="5" fill="%s" opacity="0.72" rx="1"/>
       <text x="%d" y="%d" font-size="5" fill="#374151" dominant-baseline="middle">Pagos</text>
       <line x1="%d" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="1.4"/>
       <text x="%d" y="%d" font-size="5" fill="#374151" dominant-baseline="middle">Acum.</text>',
      lx,       ly,     inflow_color,
      lx + 9L,  ly + 3L,
      lx + 45L, ly,     outflow_color,
      lx + 54L, ly + 3L,
      lx + 90L, ly + 3L, lx + 99L, ly + 3L, net_color,
      lx + 101L, ly + 3L)

    sprintf(
      '<svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg" style="display:block;">%s%s%s%s%s</svg>',
      w, h, w, h, grid_svg, zero, bars_svg, net_line, legend)
  }

  # ── One currency block (KPIs + chart + focus panels) ──────────────────────
  build_currency_block <- function(pd, is_current_week) {
    currency  <- pd$currency
    net_total <- pd$inflow_total - pd$outflow_total
    lbl_class <- if (currency == "MXN") "pulse-mxn-label" else "pulse-usd-label"
    in_col    <- if (currency == "MXN") "#0d9f8a" else "#0ea5e9"
    net_class <- if (net_total >= 0) "pulse-kpi-net-pos" else "pulse-kpi-net-neg"

    chart <- make_pulse_chart(
      daily        = pd$daily,
      today_date   = if (is_current_week) today else as.Date(NA_character_),
      inflow_color = in_col
    )

    # Top inflow rows
    in_rows <- if (nrow(pd$focus_ar) > 0)
      paste(vapply(seq_len(nrow(pd$focus_ar)), function(j) {
        r <- pd$focus_ar[j, ]
        sprintf('<div class="pulse-item">
          <span class="pulse-item-name">%s</span>
          <span class="pulse-item-amt" style="color:#059669;">+$%s</span></div>',
          he(r$Parte %||% "\u2014"), fmt(r$.a))
      }, character(1)), collapse = "\n")
    else '<div class="pulse-item"><span class="pulse-item-name" style="color:#9ca3af;font-style:italic;">Sin cobros programados</span></div>'

    # Critical payable rows (with urgency badge)
    out_rows <- if (nrow(pd$focus_ap) > 0)
      paste(vapply(seq_len(nrow(pd$focus_ap)), function(j) {
        r   <- pd$focus_ap[j, ]
        urg <- isTRUE(r$is_urgent); imp <- isTRUE(r$is_important)
        badge <- if (urg && imp)
          '<span class="pulse-badge pulse-badge-both">URG+IMP</span>'
        else if (urg)
          '<span class="pulse-badge pulse-badge-urg">URG</span>'
        else if (imp)
          '<span class="pulse-badge pulse-badge-imp">IMP</span>'
        else ""
        sprintf('<div class="pulse-item">
          <span class="pulse-item-name">%s</span>%s
          <span class="pulse-item-amt" style="color:#dc2626;">-$%s</span></div>',
          he(r$Parte %||% "\u2014"), badge, fmt(r$.a))
      }, character(1)), collapse = "\n")
    else '<div class="pulse-item"><span class="pulse-item-name" style="color:#9ca3af;font-style:italic;">Sin pagos programados</span></div>'

    sprintf('
  <div class="pulse-currency-block">
    <span class="pulse-currency-label %s">%s</span>
    <div class="pulse-kpi-row">
      <div class="pulse-kpi pulse-kpi-in">
        <div class="pulse-kpi-lbl">&#9650; Cobros esperados</div>
        <div class="pulse-kpi-val">$%s</div>
      </div>
      <div class="pulse-kpi pulse-kpi-out">
        <div class="pulse-kpi-lbl">&#9660; Pagos programados</div>
        <div class="pulse-kpi-val">$%s</div>
      </div>
      <div class="pulse-kpi %s">
        <div class="pulse-kpi-lbl">&#9654; Posici&oacute;n neta</div>
        <div class="pulse-kpi-val">%s</div>
      </div>
    </div>
    <div class="pulse-chart-wrap">%s</div>
    <div class="pulse-bottom">
      <div class="pulse-panel">
        <div class="pulse-panel-hdr pulse-panel-in-hdr">TOP COBROS &mdash; %s</div>
        %s
      </div>
      <div class="pulse-panel">
        <div class="pulse-panel-hdr pulse-panel-out-hdr">PAGOS CR&Iacute;TICOS &mdash; %s</div>
        %s
      </div>
    </div>
  </div>',
      lbl_class, he(currency),
      fmt(pd$inflow_total), fmt(pd$outflow_total),
      net_class, he(fmts(net_total)),
      chart,
      he(pd$focus_label), in_rows,
      he(pd$focus_label), out_rows
    )
  }

  # ── One page per week ──────────────────────────────────────────────────────
  build_page <- function(wk, is_current_week, page_num) {
    wk_label   <- paste0(format(wk$wk_start, "%d %b"), " \u2014 ",
                         format(wk$wk_end, "%d %b %Y"))
    week_title <- if (is_current_week) "Semana Actual" else "Semana Pr\u00f3xima"

    other_note <- if (length(wk$other_currencies) > 0)
      sprintf('<div style="font-size:8px;color:#92400e;background:#fffbeb;
        border:1px solid #fde68a;border-radius:6px;padding:4px 10px;margin-bottom:10px;">
        &#9888; Monedas adicionales detectadas: %s \u2014 se muestran solo MXN y USD.</div>',
        he(paste(wk$other_currencies, collapse = ", ")))
    else ""

    sprintf('
<div class="page">
  <div class="rpt-header">
    <div>
      <div class="rpt-title">Daily Forward Cash Flow Pulse &mdash; %s</div>
      <div class="rpt-sub">S&iacute;ntesis AP/AR &bull; %s</div>
    </div>
    <div class="rpt-meta">
      <div class="rpt-brand">
        <div class="rpt-brand-text">
          <div class="rpt-brand-label">Company</div>
          <div class="rpt-brand-name">NG Finances</div>
        </div>
        <div class="rpt-logo">N</div>
      </div>
      <div class="rpt-gen">Generado: %s &nbsp;&bull;&nbsp; CONFIDENCIAL</div>
    </div>
  </div>
  <div class="page-body">
  <div class="pulse-section">
    %s%s%s
  </div>
  </div>
  <div class="rpt-footer">
    <span>Networks Group &mdash; Reporte Financiero</span>
    <span>%s</span>
    <span>P&aacute;g. %d &mdash; CONFIDENCIAL</span>
  </div>
</div>',
      he(week_title), he(wk_label), he(gen_label),
      other_note,
      build_currency_block(wk$mxn, is_current_week),
      build_currency_block(wk$usd, is_current_week),
      he(gen_label), page_num
    )
  }

  paste0(
    build_page(pulse$current,   is_current_week = TRUE,  page_num = 3L),
    build_page(pulse$next_week, is_current_week = FALSE, page_num = 4L)
  )
}

# =============================================================================
# .build_report_html() — two-page financial report
# Page 1: Dark gradient header + global totals with sampled balance bars
#         + account cards with sparklines + top GROUP movements
# Page 2: Top movements per individual account (preserved exactly)
# =============================================================================
.build_report_html <- function(balances, movimientos = NULL, desde, hasta, generated,
                                pulse = NULL) {

  he   <- htmltools::htmlEscape
  fmt  <- function(x) formatC(abs(x), format = "f", digits = 2, big.mark = ",")
  fmts <- function(x) if (x < 0) paste0("-$", fmt(x)) else paste0("$", fmt(x))
  period_label <- paste0(format(desde, "%d %b %Y"), " \u2014 ", format(hasta, "%d %b %Y"))
  gen_label    <- format(generated, "%d/%m/%Y %H:%M")

  col_pos <- "#059669"; col_neg <- "#dc2626"
  col_in  <- "#059669"; col_out <- "#dc2626"
  moneda_colors <- list(MXN = "#1e3a5f", USD = "#0d6b5e")
  moneda_light  <- list(MXN = "#dbeafe", USD = "#d1fae5")
  moneda_text   <- list(MXN = "#1e40af", USD = "#065f46")

  # Movement type display labels
  tipo_label <- function(tipo) {
    lbl <- switch(tolower(trimws(tipo %||% "")),
      "spei_in"       = "SPEI IN",
      "spei_out"      = "SPEI OUT",
      "nomina"        = "N\u00f3mina",
      "traspaso"      = "Traspaso",
      "cambio"        = "Cambio",
      "pos"           = "POS",
      "domiciliacion" = "Domiciliaci\u00f3n",
      "domiciliaci\u00f3n" = "Domiciliaci\u00f3n",
      "plazo"         = "Plazo",
      "otro"          = "Otro",
      tools::toTitleCase(trimws(tipo %||% "Otro"))
    )
    lbl
  }

  # ── Sparkline helpers ────────────────────────────────────────────────────────

  # Bar chart showing sampled running balance (always positive-oriented)
  # Every ~3 days. Bars represent the balance level, not the delta.
  make_balance_bars <- function(vals, color, w = 220, h = 48) {
    vals <- as.numeric(vals)
    vals[is.na(vals)] <- 0
    n_orig <- length(vals)
    if (n_orig == 0) return(sprintf('<svg width="%d" height="%d"></svg>', w, h))
    # Sample every 3 days → 10-15 bars for a month
    step <- max(1L, floor(n_orig / 12L))
    idx  <- seq(1, n_orig, by = step)
    sv   <- vals[idx]
    n    <- length(sv)
    mn   <- 0  # always start from zero
    mx   <- max(sv, 1)
    bw   <- max(2, floor(w / n) - 2)
    gap  <- max(1, floor((w - n * bw) / (n + 1)))
    bars <- vapply(seq_len(n), function(i) {
      v  <- sv[i]
      bh <- max(3, round(v / mx * (h - 4)))
      x  <- gap + (i - 1) * (bw + gap)
      y  <- h - bh
      # Lighter bar for earlier, full color for most recent
      op <- 0.35 + 0.65 * (i / n)
      sprintf('<rect x="%d" y="%d" width="%d" height="%d" fill="%s" opacity="%.2f" rx="2"/>',
              x, y, bw, bh, color, op)
    }, character(1))
    sprintf('<svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg">%s</svg>',
            w, h, w, h, paste(bars, collapse = ""))
  }

  # Area sparkline with gradient fill for account cards
  make_area_spark <- function(vals, color, w = 130, h = 36) {
    vals <- as.numeric(vals)
    last_k <- NA_real_
    for (k in seq_along(vals)) {
      if (!is.na(vals[k])) last_k <- vals[k]
      else if (!is.na(last_k)) vals[k] <- last_k
    }
    vals[is.na(vals)] <- 0
    mn <- min(vals); mx <- max(vals); rng <- if (mx == mn) 1 else mx - mn
    n  <- length(vals)
    xs <- round(seq(0, w, length.out = n), 1)
    ys <- round(4 + (1 - (vals - mn) / rng) * (h - 10), 1)
    pts_line  <- paste(paste(xs, ys, sep = ","), collapse = " ")
    pts_area  <- paste(c(
      paste(0, h, sep = ","),
      paste(xs, ys, sep = ","),
      paste(w, h, sep = ",")
    ), collapse = " ")
    gid <- paste0("g", sample.int(9999, 1))
    sprintf(
      '<svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg">
        <defs>
          <linearGradient id="%s" x1="0" y1="0" x2="0" y2="1">
            <stop offset="0%%%%" stop-color="%s" stop-opacity="0.18"/>
            <stop offset="100%%%%" stop-color="%s" stop-opacity="0.01"/>
          </linearGradient>
        </defs>
        <polygon fill="url(#%s)" points="%s"/>
        <polyline fill="none" stroke="%s" stroke-width="1.6" points="%s"/>
        <circle cx="%.1f" cy="%.1f" r="2.5" fill="%s"/>
      </svg>',
      w, h, w, h,
      gid, color, color, gid, pts_area,
      color, pts_line,
      xs[n], ys[n], color
    )
  }

  # Line spark for movement blocks (keep exactly as page 2 style)
  make_line_spark <- function(vals, color, w = 180, h = 32) {
    vals <- as.numeric(vals)
    last_k <- NA_real_
    for (k in seq_along(vals)) {
      if (!is.na(vals[k])) last_k <- vals[k]
      else if (!is.na(last_k)) vals[k] <- last_k
    }
    vals[is.na(vals)] <- 0
    mn <- min(vals); mx <- max(vals); rng <- if (mx == mn) 1 else mx - mn
    n  <- length(vals)
    xs <- round(seq(0, w, length.out = n), 1)
    ys <- round(4 + (1 - (vals - mn) / rng) * (h - 8), 1)
    pts <- paste(paste(xs, ys, sep = ","), collapse = " ")
    sprintf(
      '<svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg">
        <polyline fill="none" stroke="%s" stroke-width="1.5" stroke-opacity="0.6" points="%s"/>
        <circle cx="%.1f" cy="%.1f" r="2.5" fill="%s"/>
      </svg>',
      w, h, w, h, color, pts, xs[n], ys[n], color
    )
  }

  # ── Build running-balance series per account ─────────────────────────────────
  fechas <- seq.Date(desde, hasta, by = "day")

  spark_for_account <- function(cid, saldo_ini) {
    if (is.null(movimientos) || !nrow(movimientos))
      return(rep(saldo_ini, length(fechas)))
    mov_ct <- dplyr::filter(movimientos, cuenta_id == cid, !is.na(fecha))
    if (!nrow(mov_ct)) return(rep(saldo_ini, length(fechas)))
    daily <- mov_ct |>
      dplyr::group_by(fecha) |>
      dplyr::summarise(net = sum(abono, na.rm=TRUE) - sum(cargo, na.rm=TRUE),
                       .groups = "drop")
    base_net <- sum(
      dplyr::filter(mov_ct, fecha < desde)$abono -
      dplyr::filter(mov_ct, fecha < desde)$cargo, na.rm = TRUE)
    running <- saldo_ini + base_net
    vals <- numeric(length(fechas))
    for (k in seq_along(fechas)) {
      dr <- dplyr::filter(daily, fecha == fechas[k])
      if (nrow(dr)) running <- running + dr$net[1]
      vals[k] <- running
    }
    vals
  }

  global_series <- function(currency) {
    ct <- balances[balances$moneda == currency, ]
    if (!nrow(ct)) return(rep(0, length(fechas)))
    Reduce("+", lapply(seq_len(nrow(ct)), function(i)
      spark_for_account(ct$cuenta_id[i], ct$saldo_inicial[i] %||% 0)
    ))
  }

  mxn_series <- global_series("MXN")
  usd_series <- global_series("USD")

  # Format account sub-labels for total boxes
  mxn_accounts <- paste(
    balances$alias[balances$moneda == "MXN" & !is.na(balances$alias)],
    collapse = " \u2022 ")
  usd_accounts <- paste(
    balances$alias[balances$moneda == "USD" & !is.na(balances$alias)],
    collapse = " \u2022 ")

  mxn_bar_svg <- make_balance_bars(mxn_series, "#7eb3f5", 220, 44)
  usd_bar_svg <- make_balance_bars(usd_series, "#6ee7c4", 220, 44)
  mxn_total   <- sum(balances$saldo[balances$moneda == "MXN"], na.rm = TRUE)
  usd_total   <- sum(balances$saldo[balances$moneda == "USD"], na.rm = TRUE)

  # ── Period-over-period delta ──────────────────────────────────────────────────
  cur_mxn_net <- if (!is.null(movimientos) && nrow(movimientos))
    sum(dplyr::filter(movimientos, !is.na(fecha), fecha >= desde, fecha <= hasta,
        cuenta_id %in% balances$cuenta_id[balances$moneda == "MXN"])$abono, na.rm = TRUE) -
    sum(dplyr::filter(movimientos, !is.na(fecha), fecha >= desde, fecha <= hasta,
        cuenta_id %in% balances$cuenta_id[balances$moneda == "MXN"])$cargo, na.rm = TRUE)
  else 0
  cur_usd_net <- if (!is.null(movimientos) && nrow(movimientos))
    sum(dplyr::filter(movimientos, !is.na(fecha), fecha >= desde, fecha <= hasta,
        cuenta_id %in% balances$cuenta_id[balances$moneda == "USD"])$abono, na.rm = TRUE) -
    sum(dplyr::filter(movimientos, !is.na(fecha), fecha >= desde, fecha <= hasta,
        cuenta_id %in% balances$cuenta_id[balances$moneda == "USD"])$cargo, na.rm = TRUE)
  else 0

  prev_mxn <- mxn_total - cur_mxn_net
  prev_usd <- usd_total - cur_usd_net

  delta_html <- function(cur, prev) {
    if (is.na(prev) || prev == 0) return("")
    pct   <- (cur - prev) / abs(prev) * 100
    sign  <- if (pct >= 0) "+" else ""
    arrow <- if (pct >= 0) "&#x2197;" else "&#x2198;"
    col   <- if (pct >= 0) "#6ee7b7" else "#fca5a5"
    sprintf('<span style="font-size:9px;font-weight:600;color:%s;
      background:rgba(255,255,255,0.12);border-radius:20px;
      padding:2px 8px;white-space:nowrap;">%s %s%.1f%%</span>',
      col, arrow, sign, pct)
  }

  mxn_delta_html <- delta_html(mxn_total, prev_mxn)
  usd_delta_html <- delta_html(usd_total, prev_usd)

  # ── Per-account breakdown for total boxes ────────────────────────────────────
  make_acct_breakdown <- function(currency) {
    rows <- balances[balances$moneda == currency & !is.na(balances$saldo), ]
    if (!nrow(rows)) return("")
    rows <- rows[order(-abs(rows$saldo)), ]
    paste(vapply(seq_len(nrow(rows)), function(i) {
      r   <- rows[i, ]
      col <- if (r$saldo < 0) "#fca5a5" else "rgba(255,255,255,0.75)"
      amt <- if (r$saldo < 0)
        paste0("-$", formatC(abs(r$saldo), format = "f", digits = 2, big.mark = ","))
      else
        paste0("$",  formatC(r$saldo,      format = "f", digits = 2, big.mark = ","))
      sprintf(
        '<div style="display:flex;justify-content:space-between;
          align-items:center;margin-bottom:2px;">
          <span style="font-size:7.5px;opacity:0.7;overflow:hidden;
            text-overflow:ellipsis;white-space:nowrap;max-width:120px;">%s</span>
          <span style="font-size:8px;font-weight:600;color:%s;
            font-variant-numeric:tabular-nums;white-space:nowrap;margin-left:8px;">%s</span>
        </div>',
        he(r$alias %||% r$empresa), col, amt
      )
    }, character(1)), collapse = "")
  }

  mxn_breakdown <- make_acct_breakdown("MXN")
  usd_breakdown <- make_acct_breakdown("USD")

  # ── Account cards ─────────────────────────────────────────────────────────────
  balances_sorted <- balances[order(-balances$saldo, na.last = TRUE), ]
  cards_html <- paste(vapply(seq_len(nrow(balances_sorted)), function(i) {
    row    <- balances_sorted[i, ]
    mono   <- row$moneda
    col    <- moneda_colors[[mono]] %||% "#374151"
    is_neg <- row$saldo < 0
    sv     <- spark_for_account(row$cuenta_id, row$saldo_inicial %||% 0)
    svg    <- make_area_spark(sv, if (is_neg) col_neg else col)
    badge_bg  <- moneda_light[[mono]] %||% "#f3f4f6"
    badge_col <- moneda_text[[mono]]  %||% "#374151"
    card_bg   <- if (mono == "MXN") "linear-gradient(160deg,#e2eafc 0%,#eef3ff 35%,#f8faff 100%)"
                 else                "linear-gradient(160deg,#d9f0e8 0%,#e8f7f2 35%,#f4fdf9 100%)"
    brd_col   <- if (is_neg) col_neg else col
    # Period inflows/outflows for this account
    if (!is.null(movimientos) && nrow(movimientos)) {
      mv_card    <- dplyr::filter(movimientos, cuenta_id == row$cuenta_id,
                                  !is.na(fecha), fecha >= desde, fecha <= hasta)
      card_cargo <- sum(mv_card$cargo, na.rm = TRUE)
      card_abono <- sum(mv_card$abono, na.rm = TRUE)
    } else {
      card_cargo <- 0; card_abono <- 0
    }
    # Bank display — real bank name resolved via ctas_bancos (banco_id is a FK, not a name)
    banco_display <- paste0(he(row$banco %||% "\u2014"), " \u2014 ", he(row$alias %||% ""), " \u2014 ", he(mono))
    sprintf('
    <div class="acct-card" style="border-left:4px solid %s;background:%s;">
      <div class="acct-top">
        <span class="acct-emp">%s</span>
        <span class="acct-badge" style="background:%s;color:%s;">%s</span>
      </div>
      <div class="acct-bal" style="color:%s;">%s</div>
      <div class="acct-alias">%s</div>
      <div class="acct-bank">%s</div>
      <div class="acct-spark">%s</div>
      <div class="acct-stats">
        <div class="acct-stat">
          <span class="acct-stat-lbl">Ingresos</span>
          <span class="acct-stat-val" style="color:#059669;">+$%s</span>
        </div>
        <div class="acct-stat">
          <span class="acct-stat-lbl">Egresos</span>
          <span class="acct-stat-val" style="color:#dc2626;">-$%s</span>
        </div>
      </div>
    </div>',
      brd_col, card_bg,
      he(row$empresa),
      badge_bg, badge_col, he(mono),
      if (is_neg) col_neg else col,
      he(fmts(row$saldo)),
      he(row$alias %||% ""),
      banco_display,
      svg,
      formatC(card_abono, format = "f", digits = 2, big.mark = ","),
      formatC(card_cargo, format = "f", digits = 2, big.mark = ",")
    )
  }, character(1)), collapse = "\n")

  cards_grid_class <- if (nrow(balances) > 6) "cards-grid cards-4col" else "cards-grid cards-3col"

  # ── Top group-wide movements — split by currency ─────────────────────────────
  group_top_html <- ""
  if (!is.null(movimientos) && nrow(movimientos)) {
    movs_period <- dplyr::filter(movimientos,
      !is.na(fecha), fecha >= desde, fecha <= hasta,
      !tipo %in% c("comision", "iva_comision"))

    if (nrow(movs_period)) {
      bal_key <- balances |> dplyr::select(cuenta_id, empresa, alias, moneda)
      mv_base <- movs_period |>
        dplyr::left_join(bal_key, by = "cuenta_id") |>
        dplyr::mutate(
          amount    = dplyr::if_else(cargo > 0, cargo, abono),
          direction = dplyr::if_else(cargo > 0, "OUT", "IN")
        ) |>
        dplyr::filter(amount > 0)

      make_currency_rows <- function(currency) {
        mv_cur <- mv_base |>
          dplyr::filter(moneda == currency) |>
          dplyr::arrange(dplyr::desc(amount)) |>
          dplyr::slice_head(n = 5)
        if (!nrow(mv_cur)) return("")
        paste(vapply(seq_len(nrow(mv_cur)), function(j) {
          m    <- mv_cur[j, ]
          dir  <- m$direction
          dcol <- if (dir == "OUT") col_out else col_in
          dsgn <- if (dir == "OUT") "-" else "+"
          lbl  <- trimws(m$parte %||% "")
          if (!nzchar(lbl)) lbl <- trimws(m$concepto %||% "\u2014")
          if (nchar(lbl) > 28) lbl <- paste0(substr(lbl, 1, 26), "\u2026")
          sprintf('
            <tr>
              <td class="mov-date">%s</td>
              <td class="mov-desc">%s</td>
              <td><span class="ctx-badge">%s</span></td>
              <td class="mov-amt" style="color:%s;">%s$%s</td>
            </tr>',
            he(format(m$fecha, "%d/%m")),
            he(lbl),
            he(m$empresa %||% ""),
            dcol, dsgn, he(fmt(m$amount))
          )
        }, character(1)), collapse = "\n")
      }

      make_currency_col <- function(currency, col_class) {
        rows <- make_currency_rows(currency)
        if (!nzchar(trimws(rows))) return("")
        sprintf('
    <div class="group-split-col">
      <div class="group-col-hdr %s">%s</div>
      <table class="mov-table">
        <thead>
          <tr>
            <th>Fecha</th><th>Descripci&oacute;n</th><th>Cuenta</th>
            <th style="text-align:right">Importe</th>
          </tr>
        </thead>
        <tbody>%s</tbody>
      </table>
    </div>',
          col_class, he(currency), rows)
      }

      split_body <- paste0(
        make_currency_col("MXN", "group-col-mxn"),
        make_currency_col("USD", "group-col-usd")
      )

      if (nzchar(trimws(split_body))) {
        group_top_html <- sprintf('
    <div class="sec-title">Top Movimientos del Grupo &mdash; %s</div>
    <div class="group-split">%s
    </div>',
          he(period_label), split_body
        )
      }
    }
  }

  # ── Per-account top movements (page 2 — kept exactly as before) ─────────────
  top_mov_blocks <- ""
  if (!is.null(movimientos) && nrow(movimientos)) {
    movs_period <- dplyr::filter(movimientos,
      !is.na(fecha), fecha >= desde, fecha <= hasta,
      !tipo %in% c("comision", "iva_comision"))

    top_mov_blocks <- paste(vapply(seq_len(nrow(balances)), function(i) {
      row <- balances[i, ]
      cid <- row$cuenta_id
      mv  <- dplyr::filter(movs_period, cuenta_id == cid)
      if (!nrow(mv)) return("")
      mv_top <- mv |>
        dplyr::mutate(
          amount    = dplyr::if_else(cargo > 0, cargo, abono),
          direction = dplyr::if_else(cargo > 0, "OUT", "IN")
        ) |>
        dplyr::filter(amount > 0) |>
        dplyr::arrange(dplyr::desc(amount)) |>
        dplyr::slice_head(n = 5)
      if (!nrow(mv_top)) return("")

      tot_cargo <- sum(mv$cargo, na.rm = TRUE)
      tot_abono <- sum(mv$abono, na.rm = TRUE)
      mono  <- row$moneda
      col   <- moneda_colors[[mono]] %||% "#374151"
      badge_bg  <- moneda_light[[mono]]  %||% "#f3f4f6"
      badge_col <- moneda_text[[mono]]   %||% "#374151"
      si  <- row$saldo_inicial %||% 0
      sv  <- spark_for_account(cid, si)
      svg <- make_line_spark(sv, col, w = 122L, h = 26L)

      mov_rows <- paste(vapply(seq_len(nrow(mv_top)), function(j) {
        m    <- mv_top[j, ]
        dir  <- m$direction
        dcol <- if (dir == "OUT") col_out else col_in
        dsgn <- if (dir == "OUT") "-" else "+"
        lbl  <- trimws(m$parte %||% "")
        if (!nzchar(lbl)) lbl <- trimws(m$concepto %||% "\u2014")
        if (nchar(lbl) > 32) lbl <- paste0(substr(lbl, 1, 30), "\u2026")
        sprintf('
          <tr>
            <td class="mov-date">%s</td>
            <td class="mov-desc">%s</td>
            <td class="mov-tipo">%s</td>
            <td class="mov-dir">
              <span class="dir-badge" style="background:%s22;color:%s;">%s</span>
            </td>
            <td class="mov-amt" style="color:%s;">%s$%s</td>
          </tr>',
          he(format(m$fecha, "%d/%m")),
          he(lbl), he(tipo_label(m$tipo)),
          dcol, dcol, dir,
          dcol, dsgn, he(fmt(m$amount))
        )
      }, character(1)), collapse = "\n")

      card_bg_block <- if (mono == "MXN")
        "linear-gradient(160deg,#e2eafc 0%,#eef3ff 35%,#f8faff 100%)"
      else
        "linear-gradient(160deg,#d9f0e8 0%,#e8f7f2 35%,#f4fdf9 100%)"
      sprintf('
    <div class="mov-block">
      <div class="mov-block-inner">
        <div class="mov-card" style="border-top:3px solid %s;background:%s;">
          <div class="mov-card-top">
            <span class="mov-emp">%s</span>
            <span class="badge-mono" style="background:%s;color:%s;">%s</span>
          </div>
          <div class="mov-card-alias">%s</div>
          <div class="mov-card-bal" style="color:%s;">%s</div>
          <div class="mov-card-spark">%s</div>
          <div class="mov-card-footer">
            <div class="mov-card-stat">
              <span class="mov-card-lbl">Ingresos</span>
              <span class="mov-card-val" style="color:#059669;">+$%s</span>
            </div>
            <div class="mov-card-stat">
              <span class="mov-card-lbl">Egresos</span>
              <span class="mov-card-val" style="color:#dc2626;">-$%s</span>
            </div>
          </div>
        </div>
        <div class="mov-block-tbl">
          <table class="mov-table">
            <thead>
              <tr>
                <th>Fecha</th><th>Descripci&oacute;n</th><th>Tipo</th>
                <th>Dir.</th><th style="text-align:right">Importe</th>
              </tr>
            </thead>
            <tbody>%s</tbody>
          </table>
        </div>
      </div>
    </div>',
        col, card_bg_block,
        he(row$empresa), badge_bg, badge_col, he(mono),
        he(row$alias %||% ""),
        if (row$saldo < 0) col_neg else col, he(fmts(row$saldo)),
        svg,
        he(fmt(tot_abono)),
        he(fmt(tot_cargo)),
        mov_rows
      )
    }, character(1)), collapse = "\n")
  }

  # ── CSS ───────────────────────────────────────────────────────────────────────
  css <- '
    @import url("https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap");
    *{box-sizing:border-box;margin:0;padding:0;}
    html{-webkit-print-color-adjust:exact;print-color-adjust:exact;}
    body{font-family:"Inter",system-ui,sans-serif;font-size:11px;color:#1a1a2e;
         background:#fff;-webkit-print-color-adjust:exact;print-color-adjust:exact;
         margin:0;padding:0;}
    .page{width:8.5in;min-height:11in;background:#fff;padding:0;margin:0;
          break-after:page;
          display:flex;flex-direction:column;}
    .page:last-child{break-after:avoid;}
    .page-body{flex:1;}
    /* ── Header ── */
    .rpt-header{
      background:linear-gradient(120deg,#0d1b3e 0%,#1b3d7a 55%,#163368 100%);
      -webkit-print-color-adjust:exact;print-color-adjust:exact;
      color:#fff;padding:26px 40px 22px;
      display:flex;justify-content:space-between;align-items:center;
      border-bottom:3px solid #2a5298;
      break-after:avoid;break-inside:avoid;}
    .rpt-title{font-size:14px;font-weight:700;letter-spacing:0.4px;text-transform:uppercase;
      white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}
    .rpt-sub{font-size:10px;opacity:0.6;margin-top:4px;letter-spacing:0.2px;}
    .rpt-meta{display:flex;flex-direction:column;align-items:flex-end;gap:4px;}
    .rpt-brand{display:flex;align-items:center;gap:8px;}
    .rpt-logo{width:32px;height:32px;border-radius:6px;background:rgba(255,255,255,0.15);
      border:1.5px solid rgba(255,255,255,0.3);display:flex;align-items:center;
      justify-content:center;font-size:14px;font-weight:800;color:#fff;
      letter-spacing:-0.5px;flex-shrink:0;}
    .rpt-brand-text{text-align:right;}
    .rpt-brand-label{font-size:7.5px;opacity:0.5;text-transform:uppercase;
      letter-spacing:1.2px;}
    .rpt-brand-name{font-size:11px;font-weight:700;letter-spacing:0.3px;}
    .rpt-gen{font-size:8px;opacity:0.45;margin-top:2px;}
    /* ── Section ── */
    .section{padding:14px 40px 16px;}
    .sec-title{font-size:9px;font-weight:700;letter-spacing:1.4px;
      text-transform:uppercase;color:#6b7280;
      border-bottom:1px solid #e5e7eb;padding-bottom:6px;margin-bottom:12px;
      break-after:avoid;}
    /* ── Global totals ── */
    .totals-row{display:flex;gap:14px;margin-bottom:16px;}
    .tot-box{flex:1;border-radius:12px;padding:20px 24px 16px;color:#fff;
      position:relative;overflow:hidden;
      display:flex;flex-direction:column;gap:0;min-height:140px;}
    .tot-box.mxn{background:linear-gradient(135deg,#0d1b3e 0%,#1e4080 100%);
      -webkit-print-color-adjust:exact;print-color-adjust:exact;}
    .tot-box.usd{background:linear-gradient(135deg,#064e3b 0%,#0d9f8a 100%);
      -webkit-print-color-adjust:exact;print-color-adjust:exact;}
    .tot-top-row{display:flex;justify-content:space-between;align-items:flex-start;
      margin-bottom:4px;}
    .tot-left{}
    .tot-delta{align-self:flex-start;margin-top:2px;}
    .tot-label{font-size:8.5px;opacity:0.65;text-transform:uppercase;
      letter-spacing:1px;margin-bottom:5px;}
    .tot-amount{font-size:28px;font-weight:700;letter-spacing:-0.5px;
      font-variant-numeric:tabular-nums;}
    .tot-spark{opacity:0.9;margin-top:12px;line-height:0;}
    .tot-breakdown{margin-top:8px;border-top:1px solid rgba(255,255,255,0.12);
      padding-top:8px;}
    /* ── Account cards ── */
    .cards-grid{display:grid;gap:10px;margin-bottom:18px;}
    .cards-3col{grid-template-columns:repeat(3,1fr);}
    .cards-4col{grid-template-columns:repeat(4,1fr);}
    .acct-card{border:1px solid #dde4f0;border-radius:10px;padding:10px 12px 9px;
      box-shadow:0 2px 8px rgba(30,58,95,0.08);display:flex;flex-direction:column;}
    .acct-top{display:flex;justify-content:space-between;align-items:center;
      margin-bottom:5px;}
    .acct-emp{font-size:9px;font-weight:700;color:#374151;
      text-transform:uppercase;letter-spacing:0.5px;}
    .acct-badge{font-size:7.5px;font-weight:600;padding:1px 7px;border-radius:20px;}
    .acct-bal{font-size:13.5px;font-weight:700;margin-bottom:1px;
      font-variant-numeric:tabular-nums;}
    .acct-alias{font-size:7.5px;color:#9ca3af;margin-bottom:3px;}
    .acct-bank{font-size:8px;color:#94a3b8;margin-bottom:6px;
      overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}
    .acct-stats{display:flex;gap:10px;margin-top:6px;
      border-top:1px solid #e4eaf4;padding-top:6px;}
    .acct-stat{flex:1;}
    .acct-stat-lbl{font-size:7px;color:#9ca3af;text-transform:uppercase;
      letter-spacing:0.4px;display:block;}
    .acct-stat-val{font-size:8.5px;font-weight:600;font-variant-numeric:tabular-nums;}
    .acct-spark{line-height:0;margin-top:4px;flex:1;}
    /* ── Group split: two-column MXN / USD tables ── */
    .group-split{display:flex;gap:12px;align-items:flex-start;}
    .group-split-col{flex:1;border:1px solid #e8ecf4;border-radius:10px;overflow:hidden;
      box-shadow:0 1px 6px rgba(30,58,95,0.05);}
    .group-col-hdr{font-size:8.5px;font-weight:700;letter-spacing:1px;
      text-transform:uppercase;padding:6px 12px;
      -webkit-print-color-adjust:exact;print-color-adjust:exact;}
    .group-col-mxn{background:#e2eafc;color:#1e3a5f;}
    .group-col-usd{background:#d9f0e8;color:#0d6b5e;}
    .ctx-badge{font-size:8px;font-weight:700;color:#374151;
      background:#f1f5f9;padding:1px 5px;border-radius:3px;}
    /* ── Group split table: fixed layout so IMPORTE column is never clipped ── */
    .group-split-col .mov-table{table-layout:fixed;}
    .group-split-col .mov-table th,
    .group-split-col .mov-table td{padding:5px 9px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}
    .group-split-col .mov-date{width:12%;}
    .group-split-col .mov-desc{width:30%;max-width:none;}
    .group-split-col .mov-amt{width:38%;text-overflow:clip;overflow:visible;}
    /* ── Per-account movement blocks (page 2) ── */
    .mov-block{margin-bottom:14px;break-inside:avoid;page-break-inside:avoid;}
    .mov-block-inner{display:flex;gap:0;border:1px solid #e8ecf4;border-radius:10px;
      overflow:hidden;box-shadow:0 1px 4px rgba(30,58,95,0.05);}
    .mov-card{width:152px;flex-shrink:0;padding:12px 12px 10px;display:flex;
      flex-direction:column;border-right:1px solid #e8ecf4;
      -webkit-print-color-adjust:exact;print-color-adjust:exact;}
    .mov-card-top{display:flex;align-items:center;justify-content:space-between;
      margin-bottom:4px;}
    .mov-emp{font-size:9px;font-weight:700;color:#1e293b;text-transform:uppercase;
      letter-spacing:0.4px;}
    .mov-card-alias{font-size:8px;color:#94a3b8;margin-bottom:6px;}
    .mov-card-bal{font-size:13px;font-weight:700;font-variant-numeric:tabular-nums;
      margin-bottom:4px;}
    .mov-card-spark{line-height:0;margin:6px 0;overflow:hidden;}
    .mov-card-footer{border-top:1px solid #e4eaf4;padding-top:6px;
      display:flex;flex-direction:column;gap:4px;margin-top:auto;}
    .mov-card-stat{display:flex;justify-content:space-between;align-items:center;}
    .mov-card-lbl{font-size:7px;color:#94a3b8;text-transform:uppercase;letter-spacing:0.4px;}
    .mov-card-val{font-size:8.5px;font-weight:600;font-variant-numeric:tabular-nums;}
    .mov-block-tbl{flex:1;overflow:hidden;}
    .badge-mono{font-size:7.5px;font-weight:600;padding:1px 6px;border-radius:20px;
      white-space:nowrap;}
    .mov-table{width:100%;border-collapse:collapse;font-size:9.5px;}
    .mov-table th{text-align:left;font-weight:600;color:#6b7280;font-size:8px;
      text-transform:uppercase;letter-spacing:0.5px;padding:5px 12px;
      background:#f9fafb;border-bottom:1px solid #e5e7eb;}
    .mov-table td{padding:5.5px 12px;border-bottom:1px solid #f3f4f6;color:#374151;}
    .mov-table tr:last-child td{border-bottom:none;}
    .mov-table tr:nth-child(even) td{background:#fafbfd;}
    .mov-date{color:#6b7280;white-space:nowrap;width:44px;}
    .mov-desc{max-width:220px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}
    .mov-tipo{color:#94a3b8;font-size:8px;text-transform:capitalize;width:68px;}
    .mov-dir{width:40px;}
    .dir-badge{font-size:7.5px;font-weight:700;padding:1px 5px;border-radius:3px;
      letter-spacing:0.3px;}
    .mov-amt{text-align:right;font-weight:700;font-variant-numeric:tabular-nums;
      white-space:nowrap;}
    /* ── Footer ── */
    .rpt-footer{background:#f8fafc;border-top:1px solid #e5e7eb;
      padding:8px 40px;display:flex;justify-content:space-between;align-items:center;
      font-size:8px;color:#9ca3af;margin-top:auto;}
    @page{size:8.5in 11in;margin:0;}
    @media print{
      .page{break-after:page;}
      .page:last-child{break-after:avoid;}
    }
    @media screen{
      html,body{background:#cdd3e0;}
      body{display:flex;flex-direction:column;align-items:center;padding:24px 0 40px;}
      .page{box-shadow:0 3px 20px rgba(0,0,0,0.22);margin-bottom:20px;zoom:0.9;}
    }
    /* ── Cash Flow Pulse (pages 3-4) ── */
    .pulse-section{padding:10px 40px 12px;}
    .pulse-currency-block{margin-bottom:12px;}
    .pulse-currency-label{font-size:8.5px;font-weight:700;letter-spacing:1px;
      text-transform:uppercase;padding:2px 10px;border-radius:4px;
      display:inline-block;margin-bottom:8px;
      -webkit-print-color-adjust:exact;print-color-adjust:exact;}
    .pulse-mxn-label{background:#dbeafe;color:#1e3a5f;}
    .pulse-usd-label{background:#d1fae5;color:#065f46;}
    .pulse-kpi-row{display:flex;gap:8px;margin-bottom:8px;}
    .pulse-kpi{flex:1;border-radius:8px;padding:9px 13px;color:#fff;
      -webkit-print-color-adjust:exact;print-color-adjust:exact;}
    .pulse-kpi-lbl{font-size:7px;font-weight:600;text-transform:uppercase;
      letter-spacing:0.8px;opacity:0.75;margin-bottom:3px;}
    .pulse-kpi-val{font-size:14px;font-weight:700;font-variant-numeric:tabular-nums;}
    .pulse-kpi-in{background:linear-gradient(135deg,#064e3b 0%,#0d9f8a 100%);}
    .pulse-kpi-out{background:linear-gradient(135deg,#78350f 0%,#b45309 100%);}
    .pulse-kpi-net-pos{background:linear-gradient(135deg,#0d1b3e 0%,#1e4080 100%);}
    .pulse-kpi-net-neg{background:linear-gradient(135deg,#7f1d1d 0%,#dc2626 100%);}
    .pulse-chart-wrap{border:1px solid #e8ecf4;border-radius:8px;padding:8px 10px;
      margin-bottom:8px;background:#fafbff;}
    .pulse-bottom{display:flex;gap:8px;}
    .pulse-panel{flex:1;border:1px solid #e8ecf4;border-radius:8px;overflow:hidden;}
    .pulse-panel-hdr{font-size:7px;font-weight:700;letter-spacing:0.8px;
      text-transform:uppercase;padding:4px 10px;
      -webkit-print-color-adjust:exact;print-color-adjust:exact;}
    .pulse-panel-in-hdr{background:#d1fae5;color:#065f46;}
    .pulse-panel-out-hdr{background:#fee2e2;color:#7f1d1d;}
    .pulse-item{display:flex;align-items:center;padding:4px 10px;
      border-bottom:1px solid #f3f4f6;}
    .pulse-item:last-child{border-bottom:none;}
    .pulse-item-name{flex:1;font-size:8px;font-weight:600;color:#1e293b;
      overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}
    .pulse-item-amt{font-size:8.5px;font-weight:700;font-variant-numeric:tabular-nums;
      margin-left:6px;white-space:nowrap;}
    .pulse-badge{font-size:6px;font-weight:700;padding:1px 4px;border-radius:3px;
      margin-left:4px;white-space:nowrap;
      -webkit-print-color-adjust:exact;print-color-adjust:exact;}
    .pulse-badge-urg{background:#dc2626;color:#fff;}
    .pulse-badge-imp{background:#d97706;color:#fff;}
    .pulse-badge-both{background:#7c3aed;color:#fff;}
  '

  # ── Pulse pages (current week + next week) ───────────────────────────────────
  pulse_pages <- if (!is.null(pulse)) .build_pulse_pages(pulse, gen_label) else ""

  # ── Assemble page 2 ──────────────────────────────────────────────────────────
  page2 <- if (nzchar(trimws(top_mov_blocks))) sprintf('
<div class="page">
  <div class="rpt-header">
    <div>
      <div class="rpt-title">Financial Summary Report &mdash; %s</div>
      <div class="rpt-sub">%s</div>
    </div>
    <div class="rpt-meta">
      <div class="rpt-brand">
        <div class="rpt-brand-text">
          <div class="rpt-brand-label">Company</div>
          <div class="rpt-brand-name">NG Finances</div>
        </div>
        <div class="rpt-logo">N</div>
      </div>
      <div class="rpt-gen">Generado: %s &nbsp;&bull;&nbsp; CONFIDENCIAL</div>
    </div>
  </div>
  <div class="page-body">
  <div class="section">
    <div class="sec-title">Top Movimientos por Cuenta &mdash; %s</div>
    %s
  </div>
  </div>
  <div class="rpt-footer">
    <span>Networks Group &mdash; Reporte Financiero</span>
    <span>%s</span>
    <span>P&aacute;g. 2 &mdash; CONFIDENCIAL</span>
  </div>
</div>',
    he(period_label), he(period_label), he(gen_label),
    he(period_label), top_mov_blocks,
    he(gen_label)
  ) else ""

  # ── Assemble full HTML ───────────────────────────────────────────────────────
  html_out <- sprintf('<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Reporte Financiero &mdash; Networks Group</title>
<style>%s</style>
</head>
<body>
<div class="page">

  <!-- ── HEADER ─────────────────────────────────────────────────────── -->
  <div class="rpt-header">
    <div>
      <div class="rpt-title">Financial Summary Report &mdash; %s</div>
      <div class="rpt-sub">%s</div>
    </div>
    <div class="rpt-meta">
      <div class="rpt-brand">
        <div class="rpt-brand-text">
          <div class="rpt-brand-label">Company</div>
          <div class="rpt-brand-name">NG Finances</div>
        </div>
        <div class="rpt-logo">N</div>
      </div>
      <div class="rpt-gen">Generado: %s &nbsp;&bull;&nbsp; CONFIDENCIAL</div>
    </div>
  </div>

  <div class="page-body">
  <div class="section">

    <!-- ── POSICIÓN GLOBAL ─────────────────────────────────────────── -->
    <div class="sec-title" style="margin-top:4px;">Posici&oacute;n Global</div>
    <div class="totals-row">
      <div class="tot-box mxn">
        <div class="tot-top-row">
          <div class="tot-left">
            <div class="tot-label">Total Balance MXN</div>
            <div class="tot-amount">%s</div>
          </div>
          <div class="tot-delta">%s</div>
        </div>
        <div class="tot-spark">%s</div>
        <div class="tot-breakdown">%s</div>
      </div>
      <div class="tot-box usd">
        <div class="tot-top-row">
          <div class="tot-left">
            <div class="tot-label">Total Balance USD</div>
            <div class="tot-amount">%s</div>
          </div>
          <div class="tot-delta">%s</div>
        </div>
        <div class="tot-spark">%s</div>
        <div class="tot-breakdown">%s</div>
      </div>
    </div>

    <!-- ── SALDOS POR CUENTA ───────────────────────────────────────── -->
    <div class="sec-title">Saldos por Cuenta</div>
    <div class="%s">%s</div>

    <!-- ── TOP MOVIMIENTOS DEL GRUPO ──────────────────────────────── -->
    %s

  </div>
  </div>

  <div class="rpt-footer">
    <span>Networks Group &mdash; Reporte Financiero</span>
    <span>%s</span>
    <span>P&aacute;g. 1 &mdash; CONFIDENCIAL</span>
  </div>
</div>

%s
%s
</body>
</html>',
    css,
    # header: period (title), period (subtitle), gen
    he(period_label), he(period_label), he(gen_label),
    # MXN box: amount, delta, spark, breakdown
    he(fmts(mxn_total)), mxn_delta_html, mxn_bar_svg, mxn_breakdown,
    # USD box: amount, delta, spark, breakdown
    he(fmts(usd_total)), usd_delta_html, usd_bar_svg, usd_breakdown,
    # cards grid class + cards
    cards_grid_class, cards_html,
    # group top movements
    group_top_html,
    # footer
    he(gen_label),
    # page 2: per-account movement blocks
    page2,
    # pages 3-4: cash flow pulse (current week + next week)
    pulse_pages
  )
  .brand_inject(html_out)
}
