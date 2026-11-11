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
  u <- tryCatch(shared$usuario(), error = function(e) NULL)
  # A `shared` without usuario() (older callers, tests): resolve from the
  # viewer's own usuarios.rds row, as the audit viewer does.
  if (is.null(u)) u <- list(permisos = unlist(.audit_viewer_resolve_perms(shared)))
  puede(u, "can_view_payment_audit")
}

# Module-file scope (not inside pagosAuditViewerServer) -- both the server's
# own table/modal rendering AND the standalone .build_pago_receipt_html()
# function below need it, and the latter is not part of the module closure.
.TIPO_LABELS <- c(pago_ppl = "Pago", reporte_word = "Reporte Word",
                  reporte_excel = "Reporte Excel", reporte_pdf = "Reporte PDF",
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
    # Actualizar reads up to now (.audit_rango(), R/audit_log_viewer_module.R).
    rango <- .audit_rango(input, session, "pagos_range", dias = 30)
    observeEvent(input$pagos_refresh, {
      rango$hasta_ahora()
      refresh_tick(refresh_tick() + 1L)
    }, ignoreInit = TRUE)

    fetched <- reactive({
      refresh_tick()
      req(can_view())
      cid <- effective_cid()
      if (is.null(cid) || !nzchar(cid)) return(.schema_pagos_generados())

      since <- rango$rango()[1]
      until <- rango$rango()[2]

      tryCatch(read_pagos_generados(client_id = cid, since = since, until = until),
               error = function(e) .schema_pagos_generados())
    })

    # The chosen user, "" for all. A list of the range's users (it was free
    # text): the PDF prints it in its header, so the filters stay on the
    # server until the table can tell it what it shows (T10, P4/P5).
    usuario_sel <- reactive({
      u <- input$pagos_user_sel %||% "Todos"
      if (identical(u, "Todos")) "" else u
    })

    filtered <- reactive({
      df <- fetched()
      if (is.null(df) || !nrow(df)) return(df)
      if (!is.null(input$pagos_emp_sel) && !identical(input$pagos_emp_sel, "Todas"))
        df <- df[!is.na(df$empresa) & df$empresa == input$pagos_emp_sel, , drop = FALSE]
      if (nzchar(usuario_sel()))
        df <- df[!is.na(df$user) & df$user == usuario_sel(), , drop = FALSE]
      df
    })

    # Also when the picker first reports (the panel is drawn): the panel
    # starts hidden, and choices sent before its selects exist are lost.
    observeEvent(list(fetched(), !is.null(input$pagos_range)), {
      df <- fetched()
      emps <- if (!is.null(df) && nrow(df)) sort(unique(df$empresa[!is.na(df$empresa) & nzchar(df$empresa)])) else character(0)
      users <- if (!is.null(df) && nrow(df)) sort(unique(df$user[!is.na(df$user) & nzchar(df$user)])) else character(0)
      updateSelectInput(session, "pagos_emp_sel",
                        choices = c("Todas", emps),
                        selected = if (isTRUE(input$pagos_emp_sel %in% emps)) input$pagos_emp_sel else "Todas")
      updateSelectInput(session, "pagos_user_sel",
                        choices = c("Todos", users),
                        selected = if (isTRUE(input$pagos_user_sel %in% users)) input$pagos_user_sel else "Todos")
    })

    output$panel <- renderUI({
      if (!isTRUE(can_view())) {
        return(div(class = "alert alert-warning mt-3", icon("lock"),
                   " Requiere permiso ", tags$code("can_view_payment_audit"), "."))
      }
      tagList(
        tags$style(HTML(.AUDIT_STICKY_CSS)),
        div(id = ns("sticky_scope"),
          div(id = ns("sticky_toolbar"), class = "audit-sticky-toolbar",
            div(
              class = "d-flex align-items-end gap-2 flex-wrap",
              div(style = "min-width:330px;",
                  tags$label("Rango (fecha y hora)", class = "form-label small fw-semibold mb-1"),
                  shinyWidgets::airDatepickerInput(
                    ns("pagos_range"), label = NULL,
                    range = TRUE, timepicker = TRUE,
                    value = isolate(rango$rango()),
                    dateFormat = "dd/MM/yy HH:mm",
                    language = "es",
                    update_on = "close", width = "100%")),
              div(style = "min-width:170px;",
                  tags$label("Empresa", class = "form-label small fw-semibold mb-1"),
                  selectInput(ns("pagos_emp_sel"), NULL, choices = c("Todas"), width = "100%")),
              div(style = "min-width:170px;",
                  tags$label("Usuario", class = "form-label small fw-semibold mb-1"),
                  selectInput(ns("pagos_user_sel"), NULL, choices = c("Todos"), width = "100%")),
              actionButton(ns("pagos_refresh"), icon("rotate"),
                          class = "btn btn-sm btn-outline-secondary", title = "Actualizar"),
              downloadButton(ns("pagos_export_pdf"), " Exportar PDF",
                            class = "btn btn-sm btn-outline-primary",
                            icon = icon("file-pdf"),
                            title = "Exportar a PDF el rango y filtros actuales")
            ),
            tags$p(class = "small text-muted mb-0 mt-2",
                   icon("circle-info"), " ", tr("tablas.archivos_pago.ayuda"))
          ),
          div(class = "audit-sticky-table", hd_table_ui(ns("pagos_t")))
        ),
        audit_sticky_js(ns("sticky_scope"))
      )
    })

    # ── The table (hd_table, register) ───────────────────────────────────────
    # Rows by the record's id: ℹ or a double click opens that record's
    # detail. The old DT kept its selection by position, so after a filter
    # changed the rows "Ver detalle" opened another record.
    # Cuenta origen carries the MASKED account as its value: copying a cell,
    # the rows or the Excel must never give the full CLABE either.
    .pagos_clave <- function(df) paste(format(as.POSIXct(df$ts), "%Y%m%d%H%M%OS3"), df$archivo)
    .pagos_tabla <- reactive({
      df <- filtered()
      if (is.null(df)) df <- .schema_pagos_generados()
      df <- df[order(df$ts, decreasing = TRUE), , drop = FALSE]
      data.frame(
        id        = .audit_ids(df$id, .pagos_clave(df)),
        ts        = as.POSIXct(df$ts),
        user      = as.character(df$user),
        tipo      = as.character(df$tipo),
        empresa   = as.character(df$empresa),
        moneda    = as.character(df$moneda),
        # mask_clabe(character(0)) gives one "" (its %||%), not zero values.
        cuenta    = if (nrow(df)) mask_clabe(df$cuenta_origen) else character(0),
        n_pagos   = ifelse(is.na(df$n_pagos), 0L, df$n_pagos),
        total     = as.numeric(df$total),
        # Only meaningful for pago_ppl (individual vs. bulk-ZIP generation) --
        # for every other tipo, `source` just duplicates `tipo` itself (see
        # log_archivo_generado()), so leave this column blank rather than
        # show a confusingly redundant/wrong "Individual" for a report.
        origen    = ifelse(is.na(df$tipo) | df$tipo != "pago_ppl", "",
                           ifelse(!is.na(df$source) & df$source == "zip_bulk", "ZIP (todos)", "Individual")),
        .acciones = rep("detalle", nrow(df)),
        stringsAsFactors = FALSE
      )
    })
    pagos_t <- hd_table_server(
      "pagos_t", tabla = "archivos_pago", datos = .pagos_tabla,
      columnas = list(
        col_fecha_hora("ts",   "tablas.archivos_pago.hora", segundos = TRUE),
        col_texto("user",      "tablas.archivos_pago.usuario", recorte = FALSE),
        col_insignia("tipo",   "tablas.archivos_pago.tipo", mapa = list(
          pago_ppl           = list(texto = "tablas.archivos_pago.tipo_pago",          tono = "info"),
          reporte_word       = list(texto = "tablas.archivos_pago.tipo_reporte_word",  tono = "neutro"),
          reporte_excel      = list(texto = "tablas.archivos_pago.tipo_reporte_excel", tono = "neutro"),
          reporte_pdf        = list(texto = "tablas.archivos_pago.tipo_reporte_pdf",   tono = "neutro"),
          bancos_libro_excel = list(texto = "tablas.archivos_pago.tipo_libro_bancos",  tono = "neutro"))),
        col_empresa("empresa", "tablas.archivos_pago.empresa"),
        col_texto("moneda",    "tablas.archivos_pago.moneda", recorte = FALSE),
        col_texto("cuenta",    "tablas.archivos_pago.cuenta_origen", recorte = FALSE),
        col_numero("n_pagos",  "tablas.archivos_pago.cantidad"),
        col_dinero("total",    "tablas.archivos_pago.total", moneda = "moneda", sumar = FALSE),
        col_texto("origen",    "tablas.archivos_pago.origen", recorte = FALSE)
      ),
      piezas        = "acc_fila",
      acciones_fila = "detalle",
      vacio         = "tablas.archivos_pago.vacio"
    )

    # Selected row's full record, captured at "Ver detalle" click time --
    # this is what the detail modal AND the PDF download both read from, so
    # they can never disagree about which record is being shown/exported.
    selected_record <- reactiveVal(NULL)

    observeEvent(pagos_t$accion(), {
      a <- pagos_t$accion()
      if (!identical(a$accion, "detalle") || !length(a$ids)) return(invisible(NULL))
      df <- filtered()
      if (is.null(df) || !nrow(df)) return(invisible(NULL))
      df <- df[order(df$ts, decreasing = TRUE), , drop = FALSE]
      k  <- match(a$ids[1], .audit_ids(df$id, .pagos_clave(df)))
      if (is.na(k)) return(invisible(NULL))
      row <- df[k, , drop = FALSE]
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

    # ── PDF export of the currently filtered "Ver archivos de pago" log ─────
    # Follow-up (2026-08-27) to the single-receipt download above and to
    # Actividad's own PDF export (R/audit_log_viewer_module.R) -- same
    # pagedown::chrome_print() pattern and HTML-fallback resilience.
    output$pagos_export_pdf <- downloadHandler(
      filename = function() {
        rng  <- rango$rango()
        from <- format(rng[1], "%d%b%Y")
        to   <- format(rng[2], "%d%b%Y")
        label <- gsub("[^A-Za-z0-9]", "", effective_cid() %||% "hopdesk")
        paste0("ArchivosDePago_HopDesk_", if (nzchar(label)) label else "reporte", "_", from, "-", to, ".pdf")
      },
      content = function(file) {
        showNotification(
          tagList(tags$span(class = "spinner-border spinner-border-sm me-1"),
                  "Generando PDF… (puede tomar unos segundos)"),
          id = "pagos_export_notif", type = "message", duration = NULL)
        on.exit(removeNotification("pagos_export_notif"), add = TRUE)

        export_df <- filtered()
        if (is.null(export_df) || !is.data.frame(export_df)) export_df <- .schema_pagos_generados()

        rng <- rango$rango()
        user_info <- tryCatch(shared$current_user_info(), error = function(e) NULL)

        html_path <- tryCatch(
          generate_pagos_audit_html_doc(export_df, list(
            generated_by    = user_info$name %||% user_info$user %||% "—",
            date_from       = rng[1],
            date_to         = rng[2],
            empresa_filter  = input$pagos_emp_sel,
            usuario_filter  = usuario_sel(),
            client_label    = effective_cid()
          )),
          error = function(e) {
            showNotification(tr("tablas.archivos_pago.aviso.error_documento", error = conditionMessage(e)),
                             type = "error", duration = 10); NULL
          })
        if (is.null(html_path)) stop("Error al generar el documento HTML.")

        if (!requireNamespace("pagedown", quietly = TRUE)) {
          file.copy(html_path, file, overwrite = TRUE)
          showNotification(
            tagList(tags$strong("Instala pagedown primero:"), tags$br(),
                   tags$code('install.packages("pagedown")'), tags$br(),
                   tags$small("Se descargó el reporte como HTML mientras tanto.")),
            type = "warning", duration = 15)
          return(invisible(NULL))
        }

        chrome_extra <- if (.Platform$OS.type == "unix")
          c("--no-sandbox", "--disable-setuid-sandbox", "--disable-dev-shm-usage") else character(0)

        tryCatch({
          pagedown::chrome_print(
            input = html_path, output = file, timeout = 120,
            extra_args = chrome_extra,
            options = list(printBackground = TRUE, preferCSSPageSize = TRUE,
                          paperWidth = 8.27, paperHeight = 11.69,
                          marginTop = 0.59, marginBottom = 0.59, marginLeft = 0.55, marginRight = 0.55)
          )
        }, error = function(e) {
          file.copy(html_path, file, overwrite = TRUE)
          showNotification(
            tagList(tags$strong("No se pudo generar el PDF (¿Chrome instalado?):"), tags$br(),
                   tags$code(conditionMessage(e)), tags$br(),
                   tags$small("Se descargó el reporte como HTML en su lugar.")),
            type = "warning", duration = 15)
        })

        tryCatch(
          log_action(
            tryCatch(shared$current_user(), error = function(e) NULL) %||% user_info$user %||% "unknown",
            "pagos_export", "exportar_pagos_pdf",
            sprintf("Exportación de Ver archivos de pago a PDF: %d entrada(s)", nrow(export_df)),
            target_id = effective_cid(),
            metadata  = list(n_entradas = nrow(export_df), empresa_filter = input$pagos_emp_sel,
                            usuario_filter = usuario_sel()),
            client_id             = effective_cid(),
            viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
          ),
          error = function(e) message("[PAGOS_AUDIT] export log_action call failed: ", conditionMessage(e))
        )
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

# =============================================================================
# Follow-up (2026-08-27): PDF export for the whole filtered "Ver archivos de
# pago" log, requested right after the same feature shipped for Actividad --
# "I like the formatting, fonts, colors", so this reuses .AUDIT_PDF_CSS
# (R/audit_log_viewer_module.R) directly rather than defining a parallel
# style. Segmented per Tipo (this table's equivalent of Actividad's Módulo),
# each entry with FULL (unmasked) detail -- consistent with this table's own
# existing single-row "Ver detalle" + receipt download, which already shows
# unmasked CLABE to a viewer already permissioned for this specific,
# separately-gated audit trail; masking again in a bulk "export everything"
# document would show LESS than one click already does.
# =============================================================================

# Per-Tipo entry counts, alphabetical order -- same stable-ordering
# rationale as R/audit_log_viewer_module.R's .audit_module_summary().
.pagos_tipo_summary <- function(df) {
  if (is.null(df) || !nrow(df)) return(data.frame(tipo = character(), n = integer()))
  labs <- .tipo_label(df$tipo)
  labs <- ifelse(is.na(labs) | !nzchar(labs), "(sin tipo)", labs)
  tab  <- sort(table(labs))
  data.frame(tipo = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)[
    order(names(tab)), , drop = FALSE]
}

# One entry's full detail block for the bulk PDF -- same content as
# .build_pago_receipt_html()'s body above, as tags instead of a standalone
# HTML document, for embedding inside the larger multi-entry export.
.pagos_pdf_entry_block <- function(row) {
  is_pago <- is.na(row$tipo[1]) || identical(row$tipo[1], "pago_ppl")
  header <- div(class = "entry-card",
    tags$div(class = "ehdr",
             sprintf("%s · %s · %s", format(as.POSIXct(row$ts[1]), "%d/%m/%Y %H:%M:%S"),
                     row$user[1] %||% "", .tipo_label(row$tipo[1] %||% "pago_ppl"))),
    div(class = "edesc",
      tags$div(tags$strong("Empresa: "), row$empresa[1] %||% "",
               if (!is.na(row$moneda[1]) && nzchar(row$moneda[1] %||% ""))
                 paste0(" (", row$moneda[1], ")") else ""),
      if (is_pago) tags$div(tags$strong("Cuenta origen: "), row$cuenta_origen[1] %||% ""),
      if (is_pago) tags$div(tags$strong("Origen: "),
                            if (identical(row$source[1], "zip_bulk")) "ZIP (todos)" else "Individual"),
      tags$div(tags$strong("Archivo: "), row$archivo[1] %||% ""),
      tags$div(tags$strong("Total: "),
               formatC(as.numeric(row$total[1] %||% 0), format = "f", digits = 2, big.mark = ","),
               if (is_pago) sprintf(" (%s pago(s))", as.character(row$n_pagos[1] %||% "")) else "")
    )
  )
  if (is_pago) {
    ben <- parse_pago_beneficiarios(row$beneficiarios[1])
    if (!nrow(ben)) return(tagList(header, tags$p(class = "text-muted small", "Sin beneficiarios registrados.")))
    tagList(header, tags$table(class = "table table-sm table-hover small",
      tags$thead(class = "table-light",
        tags$tr(tags$th("Beneficiario"), tags$th("CLABE"), tags$th("Banco"),
                tags$th(class = "text-end", "Importe"), tags$th("Concepto"))),
      tags$tbody(lapply(seq_len(nrow(ben)), function(i)
        tags$tr(tags$td(ben$alias[i]), tags$td(ben$clabe[i]), tags$td(ben$banco[i]),
                tags$td(class = "text-end", formatC(ben$importe[i], format = "f", digits = 2, big.mark = ",")),
                tags$td(ben$concepto[i]))))
    ))
  } else {
    det <- parse_pago_detalle(row$detalle[1])
    if (!length(det)) return(tagList(header, tags$p(class = "text-muted small", "Sin detalle adicional registrado.")))
    tagList(header, tags$table(class = "table table-sm small",
      tags$tbody(lapply(names(det), function(k)
        tags$tr(tags$td(class = "fw-semibold", k), tags$td(paste(unlist(det[[k]]), collapse = ", ")))))
    ))
  }
}

# Builds the full printable HTML document for the currently filtered "Ver
# archivos de pago" log and writes it to a temp file (same convention as
# generate_audit_log_html_doc()). export_df: raw pagos_generados rows
# (id/ts/user/client_id/empresa/moneda/cuenta_origen/banco_origen/n_pagos/
# total/beneficiarios/source/archivo/tipo/detalle).
generate_pagos_audit_html_doc <- function(export_df, meta_info = list()) {
  df <- export_df
  if (is.null(df) || !is.data.frame(df)) df <- .schema_pagos_generados()
  df <- df[order(df$ts, decreasing = TRUE), , drop = FALSE]

  meta_rows <- list(
    c("Rango exportado", sprintf("%s — %s",
      format(as.POSIXct(meta_info$date_from %||% Sys.time()), "%d/%m/%Y %H:%M"),
      format(as.POSIXct(meta_info$date_to   %||% Sys.time()), "%d/%m/%Y %H:%M"))),
    c("Empresa (filtro)",  if (nzchar(meta_info$empresa_filter %||% "")) meta_info$empresa_filter else "Todas"),
    c("Usuario (filtro)",  if (nzchar(meta_info$usuario_filter %||% "")) meta_info$usuario_filter else "Todos"),
    c("Cliente",           meta_info$client_label %||% "—"),
    c("Generado por",      meta_info$generated_by  %||% "—"),
    c("Fecha de generación", format(Sys.time(), "%d/%m/%Y %H:%M:%S")),
    c("Total de entradas", as.character(nrow(df)))
  )
  meta_html <- tags$table(class = "meta-tbl",
    lapply(meta_rows, function(r) tags$tr(tags$td(class = "k", r[1]), tags$td(class = "v", r[2]))))

  summ <- .pagos_tipo_summary(df)
  summary_html <- if (!nrow(summ)) NULL else
    tags$table(class = "audit-tbl", style = "max-width:520px; margin:10px auto;",
      tags$thead(tags$tr(tags$th("Tipo"), tags$th("Entradas"))),
      tags$tbody(lapply(seq_len(nrow(summ)), function(i)
        tags$tr(tags$td(summ$tipo[i]), tags$td(as.character(summ$n[i]))))))

  cover <- tags$div(class = "cover pb",
    tags$div(class = "wordmark", "HOPDESK"),
    tags$h1("Registro Oficial de Archivos de Pago"),
    tags$h2("Documento generado automáticamente por el sistema"),
    meta_html,
    summary_html,
    tags$div(class = "conf",
      "Este documento refleja exactamente lo registrado en la bitácora de archivos de pago al momento de su ",
      "generación, a partir de un registro permanente e inmutable. Cualquier alteración posterior a su ",
      "descarga no queda reflejada aquí.")
  )

  tipo_of <- function(sub_df) {
    labs <- .tipo_label(sub_df$tipo)
    ifelse(is.na(labs) | !nzchar(labs), "(sin tipo)", labs)
  }
  tipos <- sort(unique(tipo_of(df)))

  sections <- lapply(tipos, function(tp) {
    sub <- df[tipo_of(df) == tp, , drop = FALSE]
    quick_tbl <- tags$table(class = "audit-tbl",
      tags$colgroup(tags$col(style = "width:14%"), tags$col(style = "width:10%"),
                    tags$col(style = "width:20%"), tags$col(style = "width:10%"),
                    tags$col(style = "width:16%"), tags$col(style = "width:10%"),
                    tags$col(style = "width:20%")),
      tags$thead(tags$tr(tags$th("Hora"), tags$th("Usuario"), tags$th("Empresa"), tags$th("Moneda"),
                         tags$th("Cuenta origen"), tags$th(class = "text-end", "Cant."), tags$th("Archivo"))),
      tags$tbody(lapply(seq_len(nrow(sub)), function(i)
        tags$tr(
          tags$td(format(as.POSIXct(sub$ts[i]), "%d/%m/%Y %H:%M:%S")),
          tags$td(sub$user[i] %||% ""),
          tags$td(sub$empresa[i] %||% ""),
          tags$td(sub$moneda[i] %||% ""),
          tags$td(if (!is.na(sub$cuenta_origen[i])) sub$cuenta_origen[i] else ""),
          tags$td(class = "text-end", as.character(sub$n_pagos[i] %||% "")),
          tags$td(sub$archivo[i] %||% "")
        )))
    )
    cards <- tagList(lapply(seq_len(nrow(sub)), function(i) .pagos_pdf_entry_block(sub[i, , drop = FALSE])))
    tagList(
      tags$div(class = "pb"),
      tags$div(class = "sec-hdr", sprintf("%s (%d entrada%s)", tp, nrow(sub), if (nrow(sub) == 1) "" else "s")),
      quick_tbl,
      cards
    )
  })

  if (!nrow(df)) sections <- list(tagList(
    tags$div(class = "pb"),
    tags$div(class = "sec-hdr", "Sin entradas"),
    tags$div(class = "no-data", "No hay entradas en el rango/filtros exportados.")
  ))

  html_doc <- tags$html(
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$title("Registro Oficial de Archivos de Pago — HopDesk"),
      tags$style(HTML(.AUDIT_PDF_CSS))
    ),
    tags$body(cover, sections)
  )

  tmp_html <- tempfile(fileext = ".html")
  htmltools::save_html(html_doc, tmp_html, background = "white")
  tmp_html
}
