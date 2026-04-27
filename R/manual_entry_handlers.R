# =============================================================================
# R/manual_entry_handlers.R
# =============================================================================

# Returns the UI content (form rows + JS) for the manual-entry tab.
# Used by the combined tabbed modal in staging_browse_module.R ("Manual" tab).
manual_entry_tab_content <- function(ledger, empresas) {
  party_lbl <- if (ledger == "AR") "Cliente" else "Proveedor"
  tagList(
    fluidRow(
      column(6,
        selectInput("me_empresa",  "Empresa",  choices = empresas,
                    selected = empresas[1]),
        selectInput("me_moneda",   "Moneda",   choices = CURRENCIES,
                    selected = "MXN"),
        textInput("me_documento",  "No. Documento *", value = ""),
        textInput("me_factura",    "No. Factura",     value = "")
      ),
      column(6,
        textInput("me_parte",      party_lbl,                     value = ""),
        if (ledger == "AP") uiOutput("me_prov_suggestions") else NULL,
        textInput("me_codigo",     paste("C\u00f3digo de", party_lbl), value = ""),
        numericInput("me_importe", "Importe", value = 0, min = 0)
      )
    ),
    fluidRow(
      column(6,
        dateInput("me_fecha", "Fecha de vencimiento",
                  value = Sys.Date(), weekstart = 1, language = "es")
      ),
      column(6,
        textAreaInput("me_notas", "Notas", rows = 3, value = "")
      )
    ),
    div(class = "mt-3",
      div(class = "mb-2",
        actionButton("me_agenda_toggle",
          label = "Agregar a Agenda de hoy",
          class = "btn btn-sm btn-outline-success"
        )
      ),
      div(
        tags$span(class = "badge bg-warning text-dark", "\u270e Entrada manual"),
        tags$small(class = "text-muted ms-2", "Solo visible en esta app.")
      )
    ),
    tags$script(HTML("
      Shiny.setInputValue('me_agenda_active', false, {priority: 'event'});
      $(document).off('click.meagenda').on('click.meagenda', '#me_agenda_toggle', function() {
        var $b = $(this);
        var nowActive = !$b.data('me-active');
        $b.data('me-active', nowActive);
        Shiny.setInputValue('me_agenda_active', nowActive, {priority: 'event'});
        if (nowActive) {
          $b.removeClass('btn-outline-success').addClass('btn-success')
            .text('\u2713 En Agenda de hoy');
        } else {
          $b.removeClass('btn-success').addClass('btn-outline-success')
            .text('Agregar a Agenda de hoy');
        }
      });
    "))
  )
}

# Opens the manual-entry form pre-filled for editing an existing row.
# Sets active_entry_ledger and stores the edit id in session$userData so
# the me_save observer in app.R can distinguish insert from update.
manual_edit_handlers <- function(input, output, session,
                                  existing_row, sap_data, manual_inv,
                                  current_user, active_entry_ledger,
                                  empresa_vals = NULL) {
  ledger <- existing_row$ledger %||% active_entry_ledger()
  if (is.null(ledger)) return()

  # Use empresa_vals (from company_map_rv) when available so choices are always
  # consistent with the empresa_sel filter; fall back to COMPANY_MAP + SAP values.
  empresas <- if (!is.null(empresa_vals) && length(empresa_vals)) {
    sort(unique(empresa_vals))
  } else {
    ar <- sap_data()$AR
    ap <- sap_data()$AP
    sort(unique(c(
      if (!is.null(ar) && "Empresa" %in% names(ar)) ar$Empresa else character(),
      if (!is.null(ap) && "Empresa" %in% names(ap)) ap$Empresa else character(),
      unname(COMPANY_MAP)
    )))
  }
  party_lbl <- if (ledger == "AR") "Cliente" else "Proveedor"

  active_entry_ledger(ledger)
  session$userData[["me_edit_id"]] <- existing_row$id %||% ""

  showModal(modalDialog(
    title     = paste("Editar \u2013", ledger),
    size      = "l",
    easyClose = TRUE,
    footer = tagList(
      actionButton("me_cancel", "Cancelar", class = "btn btn-secondary"),
      actionButton("me_save",   "Guardar",  class = "btn btn-primary")
    ),
    fluidRow(
      column(6,
        selectInput("me_empresa",  "Empresa",  choices = empresas,
                    selected = existing_row$Empresa %||% empresas[1]),
        selectInput("me_moneda",   "Moneda",   choices = CURRENCIES,
                    selected = existing_row$Moneda %||% "MXN"),
        textInput("me_documento",  "No. Documento *",
                  value = existing_row$Documento %||% ""),
        textInput("me_factura",    "No. Factura",
                  value = existing_row$Factura %||% "")
      ),
      column(6,
        textInput("me_parte",  party_lbl,
                  value = existing_row$Parte %||% ""),
        if (ledger == "AP") uiOutput("me_prov_suggestions") else NULL,
        textInput("me_codigo", paste("C\u00f3digo de", party_lbl),
                  value = existing_row$Codigo %||% ""),
        numericInput("me_importe", "Importe",
                     value = existing_row$Importe %||% 0, min = 0)
      )
    ),
    fluidRow(
      column(6,
        dateInput("me_fecha", "Fecha de vencimiento",
                  value     = tryCatch(
                    as.Date(existing_row[["Fecha de vencimiento"]]),
                    error = function(e) Sys.Date()),
                  weekstart = 1, language = "es")
      ),
      column(6,
        textAreaInput("me_notas", "Notas", rows = 3,
                      value = existing_row$Notas %||% "")
      )
    ),
    div(class = "mt-2",
      tags$span(class = "badge bg-info text-dark", "\u270e Editando entrada manual"),
      tags$small(class = "text-muted ms-2", "Solo visible en esta app.")
    )
  ))
}
