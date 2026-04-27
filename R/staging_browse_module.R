# =============================================================================
# R/staging_browse_module.R
# Combined entry modal: "Buscar en rango" tab (SAP invoice browser) +
# "Manual" tab (existing blank-form flow).
#
# Also contains the Abono Parcial modal (partial payment recording).
#
# Public API:
#   show_combined_entry_modal(ledger, sap_data, session)
#     — call from pick_ar / pick_ap observers in app.R (after removeModal())
#
#   setup_staging_browse(input, output, session,
#                        active_entry_ledger, sap_data,
#                        pagar_hoy_db, current_user)
#     — call ONCE inside the server function, at startup
#
#   show_abono_modal(sap_data, session)
#     — call from pick_abono observer in app.R (after removeModal())
#
#   setup_abono_browse(input, output, session, sap_data, abonos_db, current_user)
#     — call ONCE inside the server function, at startup
# =============================================================================

# ── Internal CSS ──────────────────────────────────────────────────────────────
.sb_css <- "
.sb-tbl  { font-size: 0.875em; }
.sb-row  { display: flex; align-items: flex-start; gap: 8px;
           padding: 5px 2px; border-bottom: 1px solid #e9ecef; }
.sb-row:last-child { border-bottom: none; }
.sb-hdr  { font-size: 0.72em; text-transform: uppercase; color: #6c757d;
           font-weight: 600; padding-bottom: 4px;
           border-bottom: 2px solid #dee2e6; }
.sb-check    { width: 18px; height: 18px; flex-shrink: 0; margin-top: 3px;
               cursor: pointer; }
.sb-empresa  { width: 52px;  flex-shrink: 0; color: #6c757d; }
.sb-parte    { flex: 1; min-width: 0; }
.sb-parte-nm { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.sb-doc      { font-size: 0.8em; color: #6c757d; }
.sb-fecha    { width: 68px; flex-shrink: 0; text-align: center; color: #495057; }
.sb-saldo    { width: 100px; flex-shrink: 0; text-align: right; color: #495057; }
.sb-amt-wrap { width: 115px; flex-shrink: 0; }
.sb-amt      { width: 100%; text-align: right; }
.sb-amt.sb-warn { border-color: #ffc107 !important; background: #fffbf0 !important; }
.sb-warn-msg { font-size: 0.72em; color: #856404; margin-top: 2px; display: none; }
.sb-amt.sb-warn ~ .sb-warn-msg { display: block; }
.sb-amt:disabled { background: #f8f9fa !important; color: #adb5bd; }
.sb-empty { color: #6c757d; font-style: italic; padding: 20px 0;
            text-align: center; }
"

# ── JS: collect checked rows → Shiny.setInputValue('sb_rows', ...) ───────────
.sb_js <- "
$(document).off('change.sbcheck').on('change.sbcheck', '.sb-check', function() {
  var idx  = $(this).data('idx');
  var $amt = $('.sb-amt[data-idx=\"' + idx + '\"]');
  if (this.checked) {
    $amt.prop('disabled', false);
  } else {
    $amt.prop('disabled', true).removeClass('sb-warn');
  }
});

$(document).off('input.sbamt').on('input.sbamt', '.sb-amt', function() {
  var max = parseFloat($(this).data('max')) || 0;
  var val = parseFloat($(this).val())       || 0;
  $(this).toggleClass('sb-warn', val > max);
});

$(document).off('click.sbstage').on('click.sbstage', '#sb_stage', function() {
  var rows = [];
  $('.sb-check:checked').each(function() {
    var idx = $(this).data('idx');
    rows.push({
      idx:        String(idx),
      empresa:    String($(this).data('empresa')),
      moneda:     String($(this).data('moneda')),
      documento:  String($(this).data('documento')),
      parte:      String($(this).data('parte')),
      codigo:     String($(this).data('codigo')),
      fecha_venc: String($(this).data('fecha-venc')),
      saldo:      parseFloat($(this).data('saldo')) || 0,
      importe:    parseFloat($('.sb-amt[data-idx=\"' + idx + '\"]').val()) || 0
    });
  });
  if (rows.length === 0) {
    alert('Selecciona al menos una factura.');
    return;
  }
  Shiny.setInputValue('sb_rows', rows, {priority: 'event'});
});

// Show Guardar button only when Manual tab is active
$(document).off('shown.bs.tab.sbmod').on('shown.bs.tab.sbmod',
    '#sbTabNav button[data-bs-toggle=\"tab\"]', function(e) {
  var isManual = ($(e.target).data('bs-target') === '#sb-pane-manual');
  $('#me_save').toggle(isManual);
});
$('#me_save').hide();
"

# ── Build one invoice row for the browse table ───────────────────────────────
.sb_row_html <- function(i, row, ledger) {
  codigo_col <- if (ledger == "AR") "C\u00f3digo de cliente" else "C\u00f3digo de proveedor"
  codigo_v   <- if (codigo_col %in% names(row)) row[[codigo_col]] %||% "" else ""
  saldo_val  <- row[["Saldo vencido"]]
  fecha_str  <- format(row[["Fecha de vencimiento"]], "%d/%m/%y")
  saldo_fmt  <- formatC(saldo_val, format = "f", digits = 0, big.mark = ",")
  mon        <- row$Moneda %||% ""

  div(class = "sb-row",
    tags$input(
      type             = "checkbox",
      class            = "sb-check",
      `data-idx`       = i,
      `data-empresa`   = row$Empresa   %||% "",
      `data-moneda`    = mon,
      `data-documento` = row$Documento %||% "",
      `data-parte`     = row$Parte     %||% "",
      `data-codigo`    = codigo_v,
      `data-fecha-venc` = as.character(row[["Fecha de vencimiento"]]),
      `data-saldo`     = saldo_val
    ),
    tags$span(class = "sb-empresa", row$Empresa %||% ""),
    div(class = "sb-parte",
      div(class = "sb-parte-nm", row$Parte %||% ""),
      div(class = "sb-doc", paste0("Doc ", row$Documento %||% ""))
    ),
    tags$span(class = "sb-fecha", fecha_str),
    tags$span(class = "sb-saldo", paste0(mon, "\u00a0", saldo_fmt)),
    div(class = "sb-amt-wrap",
      tags$input(
        type       = "number",
        class      = "form-control form-control-sm sb-amt",
        `data-idx` = i,
        `data-max` = saldo_val,
        value      = saldo_val,
        min        = 0,
        step       = "0.01",
        disabled   = NA    # renders as disabled="disabled"
      ),
      div(class = "sb-warn-msg",
          paste0("Mayor al saldo (", mon, "\u00a0", saldo_fmt, ")"))
    )
  )
}

# ── Show the combined tabbed modal ────────────────────────────────────────────
show_combined_entry_modal <- function(ledger, sap_data, session,
                                      empresa_vals = unname(COMPANY_MAP)) {
  ar    <- sap_data()$AR
  ap    <- sap_data()$AP
  ldf   <- if (ledger == "AR") ar else ap

  # Use empresa_vals (from empresa_sel_rv) as the authoritative source.
  # This prevents stale SAP snapshot Empresa names (generated with an older COMPANY_MAP)
  # from appearing as choices; picking a stale name would cause the manual entry to be
  # silently filtered out by the empresa toggle in the calendar.
  empresas_browse <- sort(unique(c(empresa_vals)))
  empresas_manual <- sort(unique(c(empresa_vals)))

  today     <- Sys.Date()
  lbl_title <- if (ledger == "AR") "Cobro \u2014 CxC" else "Pago \u2014 CxP"

  showModal(modalDialog(
    title     = lbl_title,
    size      = "xl",
    easyClose = TRUE,
    footer    = tagList(
      actionButton("me_cancel", "Cancelar", class = "btn btn-secondary"),
      # Guardar is hidden by default (browse tab active); .sb_js shows it on Manual tab
      actionButton("me_save", "Guardar", class = "btn btn-primary", style = "display:none;")
    ),

    tags$style(HTML(.sb_css)),

    # ── Tab navigation ──────────────────────────────────────────────────────
    tags$ul(
      class = "nav nav-tabs", id = "sbTabNav", role = "tablist",
      tags$li(class = "nav-item",
        tags$button(
          class = "nav-link active", id = "sb-tab-browse",
          `data-bs-toggle` = "tab", `data-bs-target` = "#sb-pane-browse",
          type = "button", role = "tab",
          icon("magnifying-glass"), " Buscar en rango"
        )
      ),
      tags$li(class = "nav-item",
        tags$button(
          class = "nav-link", id = "sb-tab-manual",
          `data-bs-toggle` = "tab", `data-bs-target` = "#sb-pane-manual",
          type = "button", role = "tab",
          icon("pen-to-square"), " Manual"
        )
      )
    ),

    # ── Tab content ─────────────────────────────────────────────────────────
    div(class = "tab-content mt-2",

      # Tab A: Buscar en rango (default)
      div(class = "tab-pane show active", id = "sb-pane-browse",
        div(class = "d-flex flex-wrap gap-2 align-items-end mb-2",
          div(
            tags$label("Desde", class = "form-label small text-muted mb-1"),
            dateInput("sb_desde", NULL, value = today - 15,
                      weekstart = 1, language = "es", width = "140px")
          ),
          div(
            tags$label("Hasta", class = "form-label small text-muted mb-1"),
            dateInput("sb_hasta", NULL, value = today + 15,
                      weekstart = 1, language = "es", width = "140px")
          ),
          div(
            tags$label("Empresa", class = "form-label small text-muted mb-1"),
            selectInput("sb_empresa", NULL,
                        choices  = c("(Todas)" = "", empresas_browse),
                        selected = "", width = "170px")
          ),
          div(
            tags$label("Moneda", class = "form-label small text-muted mb-1"),
            selectInput("sb_moneda", NULL,
                        choices  = c("(Todas)" = "", CURRENCIES),
                        selected = "", width = "110px")
          )
        ),
        div(class = "sb-tbl mt-2", uiOutput("sb_table")),
        div(class = "mt-2 d-flex justify-content-between align-items-center",
          div(
            tags$span(class = "badge bg-info text-dark", "\u2295 Buscar en rango"),
            tags$small(class = "text-muted ms-2",
                       "Selecciona facturas para agregar a Agenda de hoy.")
          ),
          actionButton("sb_stage", "+ Agregar selecci\u00f3n",
                       class = "btn btn-success")
        )
      ),

      # Tab B: Manual entry form
      div(class = "tab-pane", id = "sb-pane-manual",
        manual_entry_tab_content(ledger, empresas_manual)
      )
    ),

    tags$script(HTML(.sb_js))
  ))
}

# ── Server-side setup — call ONCE inside server() ─────────────────────────────
setup_staging_browse <- function(input, output, session,
                                  active_entry_ledger, sap_data,
                                  pagar_hoy_db, current_user) {

  # Render the filtered invoice table
  output$sb_table <- renderUI({
    ledger <- active_entry_ledger()
    if (is.null(ledger)) return(NULL)

    desde <- input$sb_desde
    hasta <- input$sb_hasta
    if (is.null(desde) || is.null(hasta)) return(NULL)

    df_src <- if (ledger == "AR") sap_data()$AR else sap_data()$AP
    if (is.null(df_src) || !nrow(df_src))
      return(div(class = "sb-empty", "Sin datos SAP. Usa la pesta\u00f1a Manual."))

    df <- df_src |>
      dplyr::filter(
        `Fecha de vencimiento` >= as.Date(desde),
        `Fecha de vencimiento` <= as.Date(hasta)
      )

    if (!is.null(input$sb_empresa) && nzchar(input$sb_empresa))
      df <- df |> dplyr::filter(Empresa == input$sb_empresa)
    if (!is.null(input$sb_moneda) && nzchar(input$sb_moneda))
      df <- df |> dplyr::filter(Moneda == input$sb_moneda)

    df <- df |> dplyr::arrange(`Fecha de vencimiento`)

    if (!nrow(df))
      return(div(class = "sb-empty", "Sin facturas en ese rango."))

    header <- div(class = "sb-row sb-hdr",
      tags$span(style = "width:18px; flex-shrink:0;", ""),
      tags$span(class = "sb-empresa", "Empresa"),
      tags$span(class = "sb-parte",   "Parte / Documento"),
      tags$span(class = "sb-fecha",   "Vence"),
      tags$span(class = "sb-saldo",   "Saldo"),
      tags$span(class = "sb-amt-wrap", "Importe")
    )

    rows_ui <- lapply(seq_len(nrow(df)), function(i)
      .sb_row_html(i, df[i, ], ledger))

    tagList(
      header,
      div(style = "max-height: 360px; overflow-y: auto;",
          do.call(tagList, rows_ui))
    )
  })

  # Stage selected rows → pagar_hoy
  observeEvent(input$sb_rows, {
    rows_data <- input$sb_rows
    if (!is.list(rows_data) || length(rows_data) == 0) return()

    ledger <- active_entry_ledger()
    if (is.null(ledger)) return()

    new_rows_df <- dplyr::bind_rows(lapply(rows_data, function(r) {
      tibble::tibble(
        id        = uuid::UUIDgenerate(),
        ledger    = ledger,
        Empresa   = r$empresa   %||% "",
        Moneda    = r$moneda    %||% "MXN",
        Documento = r$documento %||% "",
        Parte     = r$parte     %||% "",
        Codigo    = trimws(r$codigo %||% ""),
        Importe   = as.numeric(r$importe %||% 0),
        FechaVenc = tryCatch(as.Date(r$fecha_venc), error = function(e) Sys.Date()),
        staged_by = current_user(),
        staged_at = Sys.time(),
        status    = "pending"
      )
    }))

    ph_updated <- upsert_pagar_hoy(pagar_hoy_db() %||% load_pagar_hoy(), new_rows_df)
    pagar_hoy_db(ph_updated)
    tryCatch(
      save_pagar_hoy(ph_updated),
      error = function(e) showNotification(
        paste("No se pudo guardar en Agenda:", e$message), type = "warning"
      )
    )

    n <- nrow(new_rows_df)
    showNotification(
      paste0(n, if (n == 1L) " factura agregada" else " facturas agregadas",
             " a Agenda de hoy."),
      type = "message", duration = 3
    )
    removeModal()
  }, ignoreInit = TRUE)
}

# =============================================================================
# Abono Parcial — partial payment recording
# =============================================================================

# JS for the abono modal: checkbox enables amount input, warn if > max,
# collect rows on "Registrar abono" click → Shiny.setInputValue('ab_rows', ...)
.ab_js <- "
$(document).off('change.abcheck').on('change.abcheck', '.ab-check', function() {
  var idx  = $(this).data('idx');
  var $amt = $('.ab-amt[data-idx=\"' + idx + '\"]');
  if (this.checked) {
    $amt.prop('disabled', false);
  } else {
    $amt.prop('disabled', true).removeClass('sb-warn');
  }
});

$(document).off('input.abamt').on('input.abamt', '.ab-amt', function() {
  var max = parseFloat($(this).data('max')) || 0;
  var val = parseFloat($(this).val())       || 0;
  $(this).toggleClass('sb-warn', val > max);
});

$(document).off('click.abstage').on('click.abstage', '#ab_stage', function() {
  var rows = [];
  $('.ab-check:checked').each(function() {
    var idx = $(this).data('idx');
    rows.push({
      idx:        String(idx),
      empresa:    String($(this).data('empresa')),
      moneda:     String($(this).data('moneda')),
      documento:  String($(this).data('documento')),
      parte:      String($(this).data('parte')),
      codigo:     String($(this).data('codigo')),
      fecha_venc: String($(this).data('fecha-venc')),
      saldo:      parseFloat($(this).data('saldo')) || 0,
      importe:    parseFloat($('.ab-amt[data-idx=\"' + idx + '\"]').val()) || 0
    });
  });
  if (rows.length === 0) {
    alert('Selecciona al menos una factura.');
    return;
  }
  Shiny.setInputValue('ab_rows', rows, {priority: 'event'});
});
"

# Build one invoice row for the abono table (reuses sb-* CSS classes)
.ab_row_html <- function(i, row, ledger) {
  codigo_col <- if (ledger == "AR") "C\u00f3digo de cliente" else "C\u00f3digo de proveedor"
  codigo_v   <- if (codigo_col %in% names(row)) row[[codigo_col]] %||% "" else ""
  saldo_val  <- row[["Saldo vencido"]]
  fecha_str  <- format(row[["Fecha de vencimiento"]], "%d/%m/%y")
  saldo_fmt  <- formatC(saldo_val, format = "f", digits = 0, big.mark = ",")
  mon        <- row$Moneda %||% ""

  div(class = "sb-row",
    tags$input(
      type              = "checkbox",
      class             = "ab-check",
      `data-idx`        = i,
      `data-empresa`    = row$Empresa   %||% "",
      `data-moneda`     = mon,
      `data-documento`  = row$Documento %||% "",
      `data-parte`      = row$Parte     %||% "",
      `data-codigo`     = codigo_v,
      `data-fecha-venc` = as.character(row[["Fecha de vencimiento"]]),
      `data-saldo`      = saldo_val
    ),
    tags$span(class = "sb-empresa", row$Empresa %||% ""),
    div(class = "sb-parte",
      div(class = "sb-parte-nm", row$Parte     %||% ""),
      div(class = "sb-doc",      paste0("Doc ", row$Documento %||% ""))
    ),
    tags$span(class = "sb-fecha", fecha_str),
    tags$span(class = "sb-saldo", paste0(mon, "\u00a0", saldo_fmt)),
    div(class = "sb-amt-wrap",
      tags$input(
        type       = "number",
        class      = "form-control form-control-sm ab-amt sb-amt",
        `data-idx` = i,
        `data-max` = saldo_val,
        value      = saldo_val,
        min        = 0,
        step       = "0.01",
        disabled   = NA
      ),
      div(class = "sb-warn-msg",
          paste0("Mayor al saldo (", mon, "\u00a0", saldo_fmt, ")"))
    )
  )
}

# ── Show the abono modal ──────────────────────────────────────────────────────
show_abono_modal <- function(sap_data, session) {
  today <- Sys.Date()

  showModal(modalDialog(
    title     = "Abono parcial",
    size      = "xl",
    easyClose = TRUE,
    footer    = modalButton("Cerrar"),

    tags$style(HTML(.sb_css)),

    fluidRow(
      column(3,
        selectInput("ab_ledger", "Tipo",
                    choices  = c("CxC (AR)" = "AR", "CxP (AP)" = "AP"),
                    selected = "AR")
      ),
      column(2,
        dateInput("ab_desde", "Desde",
                  value = today - 15, weekstart = 1, language = "es")
      ),
      column(2,
        dateInput("ab_hasta", "Hasta",
                  value = today + 15, weekstart = 1, language = "es")
      ),
      column(3,
        selectInput("ab_empresa", "Empresa",
                    choices  = c("(Todas)" = "",
                                 sort(unique(unname(COMPANY_MAP)))),
                    selected = "")
      ),
      column(2,
        selectInput("ab_moneda", "Moneda",
                    choices  = c("(Todas)" = "", CURRENCIES),
                    selected = "")
      )
    ),

    div(class = "sb-tbl mt-2", uiOutput("ab_table")),

    div(class = "mt-3 d-flex justify-content-between align-items-center",
      div(
        tags$span(class = "badge bg-warning text-dark",
                  "\u2296 Abono parcial"),
        tags$small(class = "text-muted ms-2",
                   "Reduce el saldo mostrado en el calendario.")
      ),
      actionButton("ab_stage", "Registrar abono",
                   class = "btn btn-primary")
    ),

    tags$script(HTML(.ab_js))
  ))
}

# ── Server-side setup — call ONCE inside server() ────────────────────────────
setup_abono_browse <- function(input, output, session,
                                sap_data, abonos_db, current_user) {

  output$ab_table <- renderUI({
    ledger <- input$ab_ledger
    if (is.null(ledger)) return(NULL)

    desde <- input$ab_desde
    hasta <- input$ab_hasta
    if (is.null(desde) || is.null(hasta)) return(NULL)

    df_src <- if (ledger == "AR") sap_data()$AR else sap_data()$AP
    if (is.null(df_src) || !nrow(df_src))
      return(div(class = "sb-empty",
                 "Sin datos SAP para este tipo de ledger."))

    df <- df_src |>
      dplyr::filter(
        `Fecha de vencimiento` >= as.Date(desde),
        `Fecha de vencimiento` <= as.Date(hasta)
      )

    if (!is.null(input$ab_empresa) && nzchar(input$ab_empresa))
      df <- df |> dplyr::filter(Empresa == input$ab_empresa)
    if (!is.null(input$ab_moneda) && nzchar(input$ab_moneda))
      df <- df |> dplyr::filter(Moneda == input$ab_moneda)

    df <- df |> dplyr::arrange(`Fecha de vencimiento`)

    if (!nrow(df))
      return(div(class = "sb-empty", "Sin facturas en ese rango."))

    header <- div(class = "sb-row sb-hdr",
      tags$span(style = "width:18px; flex-shrink:0;", ""),
      tags$span(class = "sb-empresa", "Empresa"),
      tags$span(class = "sb-parte",   "Parte / Documento"),
      tags$span(class = "sb-fecha",   "Vence"),
      tags$span(class = "sb-saldo",   "Saldo"),
      tags$span(class = "sb-amt-wrap", "Importe abono")
    )

    rows_ui <- lapply(seq_len(nrow(df)), function(i)
      .ab_row_html(i, df[i, ], ledger))

    tagList(
      header,
      div(style = "max-height: 360px; overflow-y: auto;",
          do.call(tagList, rows_ui))
    )
  })

  # Save abono records
  observeEvent(input$ab_rows, {
    rows_data <- input$ab_rows
    if (!is.list(rows_data) || length(rows_data) == 0) return()

    ledger <- input$ab_ledger %||% "AR"

    new_rows_df <- dplyr::bind_rows(lapply(rows_data, function(r) {
      tibble::tibble(
        id          = uuid::UUIDgenerate(),
        ledger      = ledger,
        Empresa     = r$empresa    %||% "",
        Moneda      = r$moneda     %||% "MXN",
        Documento   = r$documento  %||% "",
        Parte       = r$parte      %||% "",
        importe     = as.numeric(r$importe %||% 0),
        fecha_abono = Sys.Date(),
        notas       = "",
        created_by  = current_user(),
        created_at  = Sys.time(),
        status      = "active"
      )
    }))

    ab_updated <- upsert_abono(abonos_db() %||% load_abonos(), new_rows_df)
    abonos_db(ab_updated)
    tryCatch(
      save_abonos(ab_updated),
      error = function(e) showNotification(
        paste("No se pudo guardar el abono:", e$message), type = "warning"
      )
    )

    n <- nrow(new_rows_df)
    showNotification(
      paste0(n, if (n == 1L) " abono registrado." else " abonos registrados."),
      type = "message", duration = 3
    )
    removeModal()
  }, ignoreInit = TRUE)
}
