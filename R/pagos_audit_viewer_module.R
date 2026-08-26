# =============================================================================
# R/pagos_audit_viewer_module.R
#
# Structured, permanent audit viewer for every payment file ever generated
# (R/pagos_generados_persistence.R). Reached via a "switch view" button in
# the Actividad tab (see R/tiers_module.R's activity_section). Gated by the
# can_view_payment_audit permission -- a regular client-tier permission
# (like can_view_pagos/can_move_invoices), default ON for dev/admin, OFF for
# finance/analysis, with Hopdesk staff always allowed regardless of the
# client's own tier config (mirrors how staff already access everything
# once jumped into a client).
#
# CLABE/account numbers are masked to their last 4 digits in the summary
# table; a row's full detail (unmasked CLABE, full beneficiary breakdown,
# and a PDF "receipt" download) is one click away via a detail modal.
# =============================================================================

# Resolves whether the current viewer may see this audit trail. Staff
# (Hopdesk/principal) always may, regardless of the client's own tier
# config -- Mouse's explicit call, 2026-08-25 ("staff always allowed").
# Reuses .audit_viewer_resolve_perms() (R/audit_log_viewer_module.R) rather
# than re-deriving tier/overrides lookup a second time.
.payment_audit_can_view <- function(shared) {
  is_staff <- isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE))
  if (is_staff) return(TRUE)
  isTRUE(.audit_viewer_resolve_perms(shared)$can_view_payment_audit)
}

# Module-file scope (not inside pagosAuditViewerServer) -- both the server's
# own table/modal rendering AND the standalone .build_pago_receipt_html()
# function below need it, and the latter is not part of the module closure.
.TIPO_LABELS <- c(pago_ppl = "Pago", reporte_word = "Reporte Word",
                  reporte_excel = "Reporte Excel", reporte_pdf = "Reporte PDF",
                  reporte_financiero_html = "Reporte Financiero (HTML)",
                  reporte_financiero_pdf  = "Reporte Financiero (PDF)",
                  bancos_libro_excel      = "Libro de Bancos (Excel)")
.tipo_label <- function(tipo) {
  t <- as.character(tipo)
  out <- unname(.TIPO_LABELS[t])
  ifelse(is.na(out), ifelse(is.na(t) | !nzchar(t), "", t), out)
}

pagosAuditViewerUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("panel"))
}

pagosAuditViewerServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    can_view <- reactive({ .payment_audit_can_view(shared) })

    effective_cid <- reactive({
      tryCatch(shared$effective_client_id(), error = function(e) NULL) %||% Sys.getenv("CLIENT_ID")
    })

    refresh_tick <- reactiveVal(0L)
    observeEvent(input$pagos_refresh, { refresh_tick(refresh_tick() + 1L) }, ignoreInit = TRUE)

    fetched <- reactive({
      refresh_tick()
      req(can_view())
      cid <- effective_cid()
      if (is.null(cid) || !nzchar(cid)) return(.schema_pagos_generados())

      rng <- input$pagos_range
      since <- if (!is.null(rng) && length(rng) == 2 && !is.na(rng[[1]])) rng[[1]] else Sys.time() - 30 * 86400
      until <- if (!is.null(rng) && length(rng) == 2 && !is.na(rng[[2]])) rng[[2]] else Sys.time()

      tryCatch(read_pagos_generados(client_id = cid, since = since, until = until),
               error = function(e) .schema_pagos_generados())
    })

    filtered <- reactive({
      df <- fetched()
      if (is.null(df) || !nrow(df)) return(df)
      if (!is.null(input$pagos_emp_sel) && !identical(input$pagos_emp_sel, "Todas"))
        df <- df[!is.na(df$empresa) & df$empresa == input$pagos_emp_sel, , drop = FALSE]
      u_filter <- trimws(input$pagos_user_filter %||% "")
      if (nzchar(u_filter))
        df <- df[!is.na(df$user) & grepl(u_filter, df$user, ignore.case = TRUE), , drop = FALSE]
      df
    })

    observeEvent(fetched(), {
      df <- fetched()
      emps <- if (!is.null(df) && nrow(df)) sort(unique(df$empresa[!is.na(df$empresa) & nzchar(df$empresa)])) else character(0)
      updateSelectInput(session, "pagos_emp_sel",
                        choices = c("Todas", emps),
                        selected = if (isTRUE(input$pagos_emp_sel %in% emps)) input$pagos_emp_sel else "Todas")
    })

    output$panel <- renderUI({
      if (!isTRUE(can_view())) {
        return(div(class = "alert alert-warning mt-3", icon("lock"),
                   " Requiere permiso ", tags$code("can_view_payment_audit"), "."))
      }
      tagList(
        div(
          class = "d-flex align-items-end gap-2 flex-wrap mb-2 mt-3",
          div(style = "min-width:330px;",
              tags$label("Rango (fecha y hora)", class = "form-label small fw-semibold mb-1"),
              shinyWidgets::airDatepickerInput(
                ns("pagos_range"), label = NULL,
                range = TRUE, timepicker = TRUE,
                value = c(Sys.time() - 30 * 86400, Sys.time()),
                dateFormat = "dd/MM/yy HH:mm",
                language = "es",
                update_on = "close", width = "100%")),
          div(style = "min-width:170px;",
              tags$label("Empresa", class = "form-label small fw-semibold mb-1"),
              selectInput(ns("pagos_emp_sel"), NULL, choices = c("Todas"), width = "100%")),
          div(style = "min-width:150px; flex:1;",
              tags$label("Usuario", class = "form-label small fw-semibold mb-1"),
              textInput(ns("pagos_user_filter"), NULL,
                       placeholder = "Filtrar por usuario...", width = "100%")),
          actionButton(ns("pagos_refresh"), icon("rotate"),
                      class = "btn btn-sm btn-outline-secondary", title = "Actualizar")
        ),
        tags$p(class = "small text-muted mb-2",
               icon("circle-info"),
               " Los números de cuenta se muestran enmascarados. Haz clic en una fila para ver el detalle completo y descargar el recibo en PDF."),
        DT::dataTableOutput(ns("pagos_tbl")),
        uiOutput(ns("detail_btn_ui"))
      )
    })

    .pagos_display_df <- function(df) {
      if (is.null(df) || !nrow(df)) {
        return(data.frame(Hora = character(), Usuario = character(), Tipo = character(),
                          Empresa = character(), Moneda = character(), `Cuenta origen` = character(),
                          `Cant.` = integer(), Total = character(), Origen = character(),
                          check.names = FALSE))
      }
      df <- df[order(df$ts, decreasing = TRUE), , drop = FALSE]
      data.frame(
        Hora            = format(as.POSIXct(df$ts), "%d/%m/%Y %H:%M:%S"),
        Usuario         = ifelse(is.na(df$user), "", df$user),
        Tipo            = .tipo_label(df$tipo),
        Empresa         = ifelse(is.na(df$empresa), "", df$empresa),
        Moneda          = ifelse(is.na(df$moneda), "", df$moneda),
        `Cuenta origen` = mask_clabe(df$cuenta_origen),
        `Cant.`         = ifelse(is.na(df$n_pagos), 0L, df$n_pagos),
        Total           = ifelse(is.na(df$total), "", formatC(df$total, format = "f", digits = 2, big.mark = ",")),
        # Only meaningful for pago_ppl (individual vs. bulk-ZIP generation) --
        # for every other tipo, `source` just duplicates `tipo` itself (see
        # log_archivo_generado()), so leave this column blank rather than
        # show a confusingly redundant/wrong "Individual" for a report.
        Origen          = ifelse(is.na(df$tipo) | df$tipo != "pago_ppl", "",
                                 ifelse(df$source == "zip_bulk", "ZIP (todos)", "Individual")),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }

    # Rendered ONCE (isolate()); subsequent data changes go through the
    # dataTableProxy below via replaceData(), which updates rows in place
    # without resetting the widget's own sort order, search-box text,
    # current page, or row selection (clearSelection="none") -- a full
    # rebuild on every filter/date-range interaction was wiping all of
    # that (reported 2026-08-25/26, same root cause fixed in
    # R/audit_log_viewer_module.R).
    output$pagos_tbl <- DT::renderDataTable({
      isolate(.pagos_display_df(filtered()))
    }, options = list(
      pageLength = 25, dom = "ftp", order = list(),
      language = list(search = "Filtrar:", zeroRecords = "Sin registros",
                      info = "Mostrando _START_ a _END_ de _TOTAL_ entradas",
                      infoEmpty = "Sin registros",
                      paginate = list(previous = "Anterior", `next` = "Siguiente"))
    ), selection = "single", rownames = FALSE)

    pagos_tbl_proxy <- DT::dataTableProxy("pagos_tbl")
    observe({
      DT::replaceData(pagos_tbl_proxy, .pagos_display_df(filtered()),
                      resetPaging = FALSE, rownames = FALSE, clearSelection = "none")
    })

    output$detail_btn_ui <- renderUI({
      df <- filtered()
      if (is.null(df) || !nrow(df) || is.null(input$pagos_tbl_rows_selected)) return(NULL)
      div(class = "mt-2",
          actionButton(ns("btn_ver_detalle"), tagList(icon("magnifying-glass"), " Ver detalle"),
                       class = "btn btn-sm btn-primary"))
    })

    # Selected row's full record, captured at "Ver detalle" click time --
    # this is what the detail modal AND the PDF download both read from, so
    # they can never disagree about which record is being shown/exported.
    selected_record <- reactiveVal(NULL)

    observeEvent(input$btn_ver_detalle, {
      df  <- filtered()
      sel <- input$pagos_tbl_rows_selected
      if (is.null(df) || !nrow(df) || is.null(sel)) return(invisible(NULL))
      df <- df[order(df$ts, decreasing = TRUE), , drop = FALSE]
      if (sel < 1L || sel > nrow(df)) return(invisible(NULL))
      row <- df[sel, , drop = FALSE]
      selected_record(row)

      # NA/missing tipo => a row written before the tipo/detalle columns
      # existed; treat it as the only tipo that could have been logged back
      # then (pago_ppl). Explicit is.na() check rather than %||%'s NA-
      # coalescing, since that behavior differs across %||% definitions in
      # different sourcing contexts (found via a test-harness discrepancy,
      # 2026-08-26) -- this must not depend on which one happens to be in scope.
      is_pago <- is.na(row$tipo[1]) || identical(row$tipo[1], "pago_ppl")

      showModal(modalDialog(
        title = tagList(icon("receipt"),
                        if (is_pago) " Detalle del archivo de pagos" else " Detalle del archivo generado"),
        size = "l", easyClose = TRUE,
        div(class = "mb-3 small",
          tags$div(tags$strong("Usuario: "), row$user[1]),
          tags$div(tags$strong("Fecha: "), format(as.POSIXct(row$ts[1]), "%d/%m/%Y %H:%M:%S")),
          tags$div(tags$strong("Tipo: "), .tipo_label(row$tipo[1])),
          tags$div(tags$strong("Empresa: "), row$empresa[1], " (", row$moneda[1], ")"),
          if (is_pago) tags$div(tags$strong("Cuenta origen: "), row$cuenta_origen[1]),
          if (is_pago) tags$div(tags$strong("Origen: "),
                                if (identical(row$source[1], "zip_bulk")) "ZIP (todos)" else "Individual"),
          tags$div(tags$strong("Archivo: "), row$archivo[1])
        ),
        if (is_pago) {
          ben <- parse_pago_beneficiarios(row$beneficiarios[1])
          if (nrow(ben))
            tags$table(class = "table table-sm table-hover small",
              tags$thead(class = "table-light",
                tags$tr(tags$th("Beneficiario"), tags$th("CLABE"), tags$th("Banco"),
                        tags$th(class = "text-end", "Importe"), tags$th("Concepto"))),
              tags$tbody(
                lapply(seq_len(nrow(ben)), function(i)
                  tags$tr(
                    tags$td(ben$alias[i]),
                    tags$td(ben$clabe[i]),
                    tags$td(ben$banco[i]),
                    tags$td(class = "text-end", formatC(ben$importe[i], format = "f", digits = 2, big.mark = ",")),
                    tags$td(ben$concepto[i])
                  ))
              )
            )
          else tags$p(class = "text-muted small", "Sin beneficiarios registrados.")
        } else {
          det <- parse_pago_detalle(row$detalle[1])
          if (length(det))
            tags$table(class = "table table-sm small",
              tags$tbody(
                lapply(names(det), function(k)
                  tags$tr(tags$td(class = "fw-semibold", k),
                          tags$td(paste(unlist(det[[k]]), collapse = ", ")))
                )
              )
            )
          else tags$p(class = "text-muted small", "Sin detalle adicional registrado.")
        },
        footer = tagList(
          modalButton("Cerrar"),
          downloadButton(ns("dl_pago_pdf"), tagList(icon("file-pdf"), " Descargar recibo (PDF)"),
                         class = "btn btn-primary")
        )
      ))
    })

    output$dl_pago_pdf <- downloadHandler(
      filename = function() {
        row <- selected_record()
        id_part <- if (!is.null(row)) substr(row$id[1] %||% "recibo", 1, 8) else "recibo"
        paste0("recibo_pago_", id_part, ".pdf")
      },
      content = function(file) {
        row <- selected_record()
        if (is.null(row) || !nrow(row)) { writeLines("Sin datos.", file); return() }

        html_content <- .build_pago_receipt_html(row)
        tmp_html <- tempfile(fileext = ".html")
        on.exit(unlink(tmp_html), add = TRUE)
        writeLines(html_content, tmp_html, useBytes = FALSE)

        if (!requireNamespace("pagedown", quietly = TRUE)) {
          writeLines(html_content, file)
          showNotification(
            tagList(tags$strong("Instala pagedown primero:"), tags$br(),
                   tags$code('install.packages("pagedown")'), tags$br(),
                   tags$small("Se descargó el recibo como HTML mientras tanto.")),
            type = "warning", duration = 15)
          return()
        }

        tryCatch({
          html_url <- paste0("file:///", gsub("\\\\", "/", normalizePath(tmp_html, winslash = "/")))
          pagedown::chrome_print(
            input = html_url, output = file,
            options = list(printBackground = TRUE, paperWidth = 8.5, paperHeight = 11,
                          marginTop = 0.4, marginBottom = 0.4, marginLeft = 0.4, marginRight = 0.4,
                          preferCSSPageSize = FALSE),
            wait = 2, verbose = 0
          )
        }, error = function(e) {
          writeLines(html_content, file)
          showNotification(
            tagList(tags$strong("No se pudo generar el PDF (¿Chrome instalado?):"), tags$br(),
                   tags$code(conditionMessage(e)), tags$br(),
                   tags$small("Se descargó el recibo como HTML en su lugar.")),
            type = "warning", duration = 15)
        })
      }
    )
  })
}

# Builds a self-contained HTML receipt for one pagos_generados row. Full
# (unmasked) CLABE by design for pago_ppl rows -- this is the download path
# a permitted viewer already had to click through the masked summary +
# unmasked detail modal to reach, not a shortcut around the masking. Every
# other tipo (2026-08-26 extension) shows its detalle key/value pairs
# instead of a beneficiary table.
.build_pago_receipt_html <- function(row) {
  # See the identical is.na()-explicit check (and its comment) in the detail
  # modal's observeEvent above -- kept in sync deliberately, not %||%-based.
  is_pago <- is.na(row$tipo[1]) || identical(row$tipo[1], "pago_ppl")

  if (is_pago) {
    ben <- parse_pago_beneficiarios(row$beneficiarios[1])
    body_html <- if (nrow(ben)) sprintf(
      '<table><thead><tr><th>Beneficiario</th><th>CLABE</th><th>Banco</th><th style="text-align:right;">Importe</th><th>Concepto</th></tr></thead><tbody>%s</tbody></table>',
      paste(vapply(seq_len(nrow(ben)), function(i) sprintf(
        "<tr><td>%s</td><td>%s</td><td>%s</td><td style='text-align:right;'>%s</td><td>%s</td></tr>",
        htmltools::htmlEscape(ben$alias[i] %||% ""),
        htmltools::htmlEscape(ben$clabe[i] %||% ""),
        htmltools::htmlEscape(ben$banco[i] %||% ""),
        formatC(ben$importe[i], format = "f", digits = 2, big.mark = ","),
        htmltools::htmlEscape(ben$concepto[i] %||% "")
      ), character(1)), collapse = "\n")
    ) else "<p style='color:#888;'>Sin beneficiarios registrados.</p>"
    total_html <- sprintf('<div class="total">Total: %s (%d pago(s))</div>',
                          formatC(row$total[1] %||% 0, format = "f", digits = 2, big.mark = ","),
                          as.integer(row$n_pagos[1] %||% 0L))
    extra_meta <- sprintf('<div><strong>Cuenta origen:</strong> %s</div>',
                          htmltools::htmlEscape(row$cuenta_origen[1] %||% ""))
    title_txt <- "Recibo de archivo de pagos"
  } else {
    det <- parse_pago_detalle(row$detalle[1])
    body_html <- if (length(det))
      paste0("<table><tbody>", paste(vapply(names(det), function(k) sprintf(
        "<tr><td style='font-weight:bold;'>%s</td><td>%s</td></tr>",
        htmltools::htmlEscape(k),
        htmltools::htmlEscape(paste(unlist(det[[k]]), collapse = ", "))
      ), character(1)), collapse = "\n"), "</tbody></table>")
    else "<p style='color:#888;'>Sin detalle adicional registrado.</p>"
    total_html <- if (!is.na(row$total[1] %||% NA_real_))
      sprintf('<div class="total">Total: %s</div>',
             formatC(row$total[1], format = "f", digits = 2, big.mark = ",")) else ""
    extra_meta <- ""
    title_txt <- "Recibo de archivo generado"
  }

  sprintf('
<!DOCTYPE html><html><head><meta charset="utf-8">
<style>
  body { font-family: Arial, sans-serif; color: #222; padding: 24px; }
  h1 { font-size: 18px; margin-bottom: 4px; }
  .meta { font-size: 12px; color: #444; margin-bottom: 16px; }
  .meta div { margin-bottom: 2px; }
  table { width: 100%%; border-collapse: collapse; font-size: 11px; }
  th, td { border: 1px solid #ccc; padding: 6px 8px; }
  th { background: #f2f2f2; text-align: left; }
  .total { text-align: right; font-weight: bold; margin-top: 10px; font-size: 13px; }
  .footer { margin-top: 24px; font-size: 10px; color: #888; }
</style></head><body>
  <h1>%s</h1>
  <div class="meta">
    <div><strong>Generado por:</strong> %s</div>
    <div><strong>Fecha:</strong> %s</div>
    <div><strong>Tipo:</strong> %s</div>
    <div><strong>Empresa:</strong> %s (%s)</div>
    %s
    <div><strong>Archivo:</strong> %s</div>
    <div><strong>ID de registro:</strong> %s</div>
  </div>
  %s
  %s
  <div class="footer">Generado automáticamente por HopDesk a partir de un registro permanente e inmutable. Este recibo refleja exactamente lo que quedó registrado al momento de generar el archivo.</div>
</body></html>',
    title_txt,
    htmltools::htmlEscape(row$user[1] %||% ""),
    format(as.POSIXct(row$ts[1]), "%d/%m/%Y %H:%M:%S"),
    htmltools::htmlEscape(.tipo_label(row$tipo[1] %||% "pago_ppl")),
    htmltools::htmlEscape(row$empresa[1] %||% ""),
    htmltools::htmlEscape(row$moneda[1] %||% ""),
    extra_meta,
    htmltools::htmlEscape(row$archivo[1] %||% ""),
    htmltools::htmlEscape(row$id[1] %||% ""),
    body_html,
    total_html
  )
}
