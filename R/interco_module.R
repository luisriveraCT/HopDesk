# =============================================================================
# interco_module.R — Intercompany Invoice View (read-only v1)
# =============================================================================
# Shows open IC invoices across all group companies.
# Data: SAP snapshots from S3 (one AR + one AP file, all companies combined).
# Registry: shared$interco_v2()  →  which CardCodes are IC per empresa/ledger.
# =============================================================================

intercoUI <- function(id) {
  ns <- NS(id)
  div(
    class = "container-fluid py-3",
    style = "max-width:1200px;",
    # ── Header ──────────────────────────────────────────────────────────────
    div(class = "d-flex align-items-center justify-content-between mb-3",
      div(
        tags$h5(class = "mb-1 fw-semibold",
                tagList(icon("arrows-left-right"), " Intercompany")),
        tags$p(class = "text-muted small mb-0",
               "Saldos abiertos entre empresas del grupo \u2014 fuente: snapshots SAP")
      ),
      div(class = "d-flex gap-2 align-items-center",
        div(class = "btn-group btn-group-sm",
          actionButton(ns("view_table_btn"),
                       tagList(icon("table-cells"), " Tabla"),
                       class = "btn btn-outline-secondary active"),
          actionButton(ns("view_map_btn"),
                       tagList(icon("diagram-project"), " Mapa IC"),
                       class = "btn btn-outline-secondary")
        ),
        actionButton(ns("ic_refresh"),
                     tagList(icon("rotate"), " Actualizar"),
                     class = "btn btn-outline-secondary btn-sm")
      )
    ),
    div(id = ns("ic_table_view"), uiOutput(ns("ic_body"))),
    div(id = ns("ic_map_view"), style = "display:none;",
        treasuryMapUI(ns("icmap")))
  )
}

intercoServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns         <- session$ns
    inv_map_rv <- reactive({
      cmap <- shared$company_map()
      setNames(names(cmap), unname(cmap))
    })

    # ── State ──────────────────────────────────────────────────────────────
    ic_invoices   <- reactiveVal(NULL)   # list(ar = df | NULL, ap = df | NULL)
    ic_auto_done  <- reactiveVal(FALSE)  # load once on init
    selected_pair <- reactiveVal(NULL)   # list(ini, empresa, counterpart, ledger)
    ic_view_mode  <- reactiveVal("table")

    # ── Filtered sub-data for selected pair (raw, all columns, date-sorted) ──
    ic_detail_sub <- reactive({
      pair <- selected_pair(); req(pair)
      dat  <- ic_invoices();   req(dat)
      df   <- if (pair$ledger == "AR") dat$ar else dat$ap
      req(!is.null(df), nrow(df) > 0)
      sub  <- df[df$.ini == pair$ini & df$Parte == pair$counterpart, , drop = FALSE]
      req(nrow(sub) > 0)
      due_col <- "Fecha de vencimiento"
      sub[order(as.Date(sub[[due_col]] %||% NA)), , drop = FALSE]
    })

    # ── View toggle ───────────────────────────────────────────────────────────
    observeEvent(input$view_table_btn, {
      ic_view_mode("table")
      shinyjs::show("ic_table_view"); shinyjs::hide("ic_map_view")
    }, ignoreInit = TRUE)

    observeEvent(input$view_map_btn, {
      ic_view_mode("map")
      shinyjs::hide("ic_table_view"); shinyjs::show("ic_map_view")
    }, ignoreInit = TRUE)

    # Auto-switch to map when triggered from pagar_hoy buttons
    observe({
      req(!is.null(shared$ic_map_target))
      tgt <- shared$ic_map_target()
      req(!is.null(tgt))
      ic_view_mode("map")
      shinyjs::hide("ic_table_view")
      shinyjs::show("ic_map_view")
    })

    # ── IC filter logic ────────────────────────────────────────────────────
    .load_ic_data <- function() {
      reg      <- isolate(shared$interco_v2())
      if (is.null(reg) || !length(reg$companies)) return(NULL)

      cmap    <- isolate(shared$company_map())
      inv_map <- setNames(names(cmap), unname(cmap))

      ar_pre   <- toupper(reg$ar_prefix %||% "C")
      ap_pre   <- toupper(reg$ap_prefix %||% "P")

      # Full CardCode sets per empresa, per ledger
      ic_codes <- lapply(names(cmap), function(ini) {
        list(
          ar = paste0(ar_pre, reg$companies[[ini]]$ar %||% character()),
          ap = paste0(ap_pre, reg$companies[[ini]]$ap %||% character())
        )
      })
      names(ic_codes) <- names(cmap)

      snap_ar <- tryCatch(load_sap_snapshot("AR")$data, error = function(e) NULL)
      snap_ap <- tryCatch(load_sap_snapshot("AP")$data, error = function(e) NULL)

      .filter_ic <- function(df, ledger) {
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
        code_col <- if (ledger == "AR") "C\u00f3digo de cliente" else "C\u00f3digo de proveedor"
        if (!"Empresa" %in% names(df) || !code_col %in% names(df)) return(NULL)

        slot    <- if (ledger == "AR") "ar" else "ap"
        df$.ini <- inv_map[df$Empresa]

        keep <- mapply(function(ini, code) {
          if (is.na(ini) || !nzchar(ini %||% "")) return(FALSE)
          toupper(trimws(code %||% "")) %in% (ic_codes[[ini]][[slot]] %||% character())
        }, df$.ini, df[[code_col]])

        out <- df[keep, , drop = FALSE]
        if (nrow(out) == 0) return(NULL)
        out$CardCode <- out[[code_col]]
        out$Ledger   <- ledger
        out
      }

      list(
        ar = .filter_ic(snap_ar, "AR"),
        ap = .filter_ic(snap_ap, "AP")
      )
    }

    # ── Auto-load once when registry is ready ──────────────────────────────
    observe({
      req(!ic_auto_done())
      reg <- shared$interco_v2()
      req(!is.null(reg), length(reg$companies) > 0)
      ic_invoices(.load_ic_data())
      ic_auto_done(TRUE)
    })

    observeEvent(input$ic_refresh, {
      ic_invoices(.load_ic_data())
      selected_pair(NULL)
    }, ignoreInit = TRUE)

    # ── Aggregated matrix ──────────────────────────────────────────────────
    ic_matrix_df <- reactive({
      dat <- ic_invoices()
      req(!is.null(dat))

      .agg <- function(df) {
        if (is.null(df) || nrow(df) == 0) return(NULL)
        amt_col <- if ("Saldo vencido" %in% names(df)) "Saldo vencido" else "DocTotal"
        df |>
          dplyr::group_by(.ini, Empresa, Parte, Moneda, Ledger) |>
          dplyr::summarise(
            Total    = sum(.data[[amt_col]], na.rm = TRUE),
            Facturas = dplyr::n(),
            .groups  = "drop"
          )
      }

      dplyr::bind_rows(.agg(dat$ar), .agg(dat$ap))
    })

    # ── Body UI ────────────────────────────────────────────────────────────
    output$ic_body <- renderUI({
      dat <- ic_invoices()

      if (is.null(dat) || (is.null(dat$ar) && is.null(dat$ap))) {
        return(div(
          class = "text-center text-muted py-5",
          tags$p(icon("circle-info")),
          tags$p("No hay facturas IC en los snapshots SAP."),
          tags$p(class = "small",
                 "Configura los c\u00f3digos IC en Configuraci\u00f3n \u203a Intercompany y recarga los datos SAP.")
        ))
      }

      n_ar <- if (!is.null(dat$ar)) nrow(dat$ar) else 0L
      n_ap <- if (!is.null(dat$ap)) nrow(dat$ap) else 0L

      # Currency-broken totals for value boxes
      .fmt_totals <- function(df) {
        if (is.null(df) || nrow(df) == 0) return("\u2014")
        amt_col <- if ("Saldo vencido" %in% names(df)) "Saldo vencido" else "DocTotal"
        df |>
          dplyr::group_by(Moneda) |>
          dplyr::summarise(T = sum(.data[[amt_col]], na.rm = TRUE), .groups = "drop") |>
          dplyr::arrange(Moneda) |>
          dplyr::mutate(lbl = sprintf("%s\u00a0%s", Moneda,
                                      format(round(T), big.mark = ",", scientific = FALSE))) |>
          dplyr::pull(lbl) |>
          paste(collapse = " \u00b7 ")
      }

      tagList(
        # ── Value boxes ──────────────────────────────────────────────────
        fluidRow(
          column(5,
            div(class = "border rounded p-3 mb-3 bg-primary-subtle",
              tags$p(class = "small fw-semibold mb-1 text-primary-emphasis",
                     tagList(icon("arrow-down-to-line"), " CxC Intercompany (cobros)")),
              tags$p(class = "fw-bold mb-0 fs-6", .fmt_totals(dat$ar)),
              tags$small(class = "text-muted",
                         sprintf("%d factura%s", n_ar, if (n_ar == 1) "" else "s"))
            )
          ),
          column(5,
            div(class = "border rounded p-3 mb-3 bg-danger-subtle",
              tags$p(class = "small fw-semibold mb-1 text-danger-emphasis",
                     tagList(icon("arrow-up-from-line"), " CxP Intercompany (pagos)")),
              tags$p(class = "fw-bold mb-0 fs-6", .fmt_totals(dat$ap)),
              tags$small(class = "text-muted",
                         sprintf("%d factura%s", n_ap, if (n_ap == 1) "" else "s"))
            )
          )
        ),

        # ── Matrix ───────────────────────────────────────────────────────
        tags$h6(class = "fw-semibold mb-1",
                tagList(icon("table-cells"), " Saldos por empresa y contraparte")),
        tags$p(class = "text-muted small mb-2",
               "Selecciona una fila para ver el detalle de facturas."),
        DT::dataTableOutput(ns("ic_matrix")),

        # ── Drill-down ────────────────────────────────────────────────────
        uiOutput(ns("ic_detail"))
      )
    })

    # ── Matrix DT ─────────────────────────────────────────────────────────
    output$ic_matrix <- DT::renderDataTable({
      mat <- ic_matrix_df()
      req(mat, nrow(mat) > 0)

      display <- mat |>
        dplyr::arrange(.ini, Ledger, dplyr::desc(Total)) |>
        dplyr::transmute(
          Empresa     = sprintf("%s (%s)", Empresa, .ini),
          Tipo        = dplyr::if_else(Ledger == "AR", "CxC \u2014 Cobro", "CxP \u2014 Pago"),
          Contraparte = Parte,
          Moneda      = Moneda,
          Total       = format(round(Total), big.mark = ",", scientific = FALSE),
          Facturas    = Facturas
        )

      DT::datatable(
        display,
        rownames  = FALSE,
        selection = "single",
        class     = "table table-sm table-hover",
        options   = list(
          pageLength = 25,
          dom        = "ftp",
          columnDefs = list(
            list(className = "dt-right", targets = c(4L, 5L)),
            list(className = "dt-center", targets = 1L)
          ),
          language = list(
            search      = "Buscar:",
            paginate    = list(`next` = "\u203a", previous = "\u2039"),
            info        = "Mostrando _START_\u2013_END_ de _TOTAL_",
            zeroRecords = "Sin resultados"
          )
        )
      ) |>
        DT::formatStyle(
          "Tipo",
          color      = DT::styleEqual(
            c("CxC \u2014 Cobro", "CxP \u2014 Pago"),
            c("#0d6efd",           "#dc3545")
          ),
          fontWeight = "600"
        )
    }, server = FALSE)

    # Row click → select pair
    observeEvent(input$ic_matrix_rows_selected, {
      mat <- ic_matrix_df()
      req(mat)
      idx <- input$ic_matrix_rows_selected
      req(length(idx) > 0)

      ordered <- mat |> dplyr::arrange(.ini, Ledger, dplyr::desc(Total))
      row      <- ordered[idx, ]
      selected_pair(list(
        ini         = row$.ini,
        empresa     = row$Empresa,
        counterpart = row$Parte,
        ledger      = row$Ledger
      ))
    }, ignoreNULL = TRUE)

    # ── Detail panel UI ────────────────────────────────────────────────────
    output$ic_detail <- renderUI({
      pair <- selected_pair()
      if (is.null(pair)) return(NULL)

      tipo_lbl <- if (pair$ledger == "AR") "CxC \u2014 Cobro" else "CxP \u2014 Pago"

      div(class = "mt-4",
        tags$hr(),
        div(class = "d-flex align-items-center mb-2",
          tags$h6(class = "fw-semibold mb-0",
                  sprintf("%s (%s)  \u2192  %s   \u00b7  %s",
                          pair$empresa, pair$ini, pair$counterpart, tipo_lbl)),
          actionButton(ns("ic_close_detail"), icon("xmark"),
                       class = "btn btn-sm btn-outline-secondary ms-auto",
                       title = "Cerrar detalle")
        ),
        DT::dataTableOutput(ns("ic_detail_tbl")),
        div(class = "mt-2 d-flex align-items-center gap-2",
          actionButton(ns("ic_add_selected"),
                       tagList(icon("calendar-plus"), " Agregar a Agenda de Hoy"),
                       class = "btn btn-sm btn-success"),
          tags$small(class = "text-muted",
                     "Shift+clic para seleccionar m\u00faltiples \u00b7 + para a\u00f1adir individual")
        )
      )
    })

    observeEvent(input$ic_close_detail, selected_pair(NULL), ignoreInit = TRUE)

    # ── Helper: send rows to pagar_hoy_db ─────────────────────────────────────
    .ic_send_rows <- function(rows) {
      pair    <- isolate(selected_pair()); req(pair)
      req(!is.null(rows), nrow(rows) > 0)
      amt_col <- if ("Saldo vencido" %in% names(rows)) "Saldo vencido" else "DocTotal"
      due_col <- "Fecha de vencimiento"

      ph <- tryCatch(shared$pagar_hoy_db(), error = function(e) NULL) %||%
            tibble::tibble(id = character(), ledger = character(), Empresa = character(),
                           Moneda = character(), Documento = character(), Parte = character(),
                           Importe = numeric(), FechaVenc = as.Date(character()),
                           staged_by = character(), staged_at = as.POSIXct(character()),
                           status = character())

      user_id <- tryCatch(as.character(shared$current_user()), error = function(e) "ic_table")

      new_rows <- dplyr::bind_rows(lapply(seq_len(nrow(rows)), function(i) {
        r   <- rows[i, ]
        doc <- tryCatch(as.character(r[["Documento"]][1]),
                        error = function(e) tryCatch(as.character(r[["Factura"]][1]),
                                                     error = function(e) ""))
        mon <- tryCatch(as.character(r[["Moneda"]][1]), error = function(e) "MXN")
        # IC table data comes from raw SAP snapshots — read codigo from whichever
        # column name is present (harmonized or original SAP name).
        codigo_val <- tryCatch({
          cc <- if ("Codigo" %in% names(r)) r[["Codigo"]][1]
                else if ("Código de proveedor" %in% names(r)) r[["Código de proveedor"]][1]
                else if ("Código de cliente"   %in% names(r)) r[["Código de cliente"]][1]
                else ""
          trimws(as.character(cc %||% ""))
        }, error = function(e) "")
        tibble::tibble(
          id        = paste0("IC_", pair$ini, "_", doc, "_", as.integer(Sys.time()) + i),
          ledger    = pair$ledger,
          Empresa   = pair$empresa,
          Moneda    = mon,
          Documento = doc,
          Parte     = pair$counterpart,
          Codigo    = codigo_val,
          tipo_item = "factura",
          Importe   = as.numeric(r[[amt_col]][1] %||% 0),
          FechaVenc = tryCatch(as.Date(r[[due_col]][1]), error = function(e) Sys.Date()),
          staged_by = user_id,
          staged_at = Sys.time(),
          status    = "pending"
        )
      }))

      combined <- upsert_pagar_hoy(ph, new_rows)
      shared$pagar_hoy_db(combined)
      tryCatch(save_pagar_hoy(combined), error = function(e) NULL)
      nrow(new_rows)
    }

    # Single-row "+" button click
    observeEvent(input$ic_add_single, {
      sub <- tryCatch(ic_detail_sub(), error = function(e) NULL)
      req(sub)
      idx <- input$ic_add_single$row
      req(!is.na(idx), idx >= 1, idx <= nrow(sub))
      added <- .ic_send_rows(sub[idx, , drop = FALSE])
      showNotification(paste0(added, " factura a\u00f1adida a Agenda de Hoy."),
                       type = "message", duration = 2)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Batch add via "Agregar a Agenda de Hoy" button
    observeEvent(input$ic_add_selected, {
      sub     <- tryCatch(ic_detail_sub(), error = function(e) NULL); req(sub)
      sel_idx <- input$ic_detail_tbl_rows_selected
      if (!length(sel_idx)) {
        showNotification("Selecciona al menos una fila primero.", type = "warning", duration = 2)
        return()
      }
      added <- .ic_send_rows(sub[sel_idx, , drop = FALSE])
      showNotification(paste0(added, " factura(s) a\u00f1adida(s) a Agenda de Hoy."),
                       type = "message", duration = 2)
    }, ignoreInit = TRUE)

    # ── Treasury map sub-module ────────────────────────────────────────────
    treasuryMapServer("icmap", shared, reactive(ic_invoices()))

    # ── Detail DT ─────────────────────────────────────────────────────────
    output$ic_detail_tbl <- DT::renderDataTable({
      sub <- ic_detail_sub()

      amt_col <- if ("Saldo vencido" %in% names(sub)) "Saldo vencido" else "DocTotal"
      due_col <- "Fecha de vencimiento"
      doc_col <- "Fecha de contabilizaci\u00f3n"

      display_cols <- intersect(
        c("Documento", "Factura", doc_col, due_col, amt_col, "Moneda"),
        names(sub)
      )
      display <- sub[, display_cols, drop = FALSE]

      if (due_col %in% names(display))
        display[["D\u00edas"]] <- as.integer(Sys.Date() - as.Date(display[[due_col]]))

      # Per-row "+" button column
      add_input_id <- ns("ic_add_single")
      display[[" "]] <- vapply(seq_len(nrow(display)), function(i) {
        sprintf(
          '<button class="btn btn-success btn-xs" style="padding:0 6px;font-size:.8rem;line-height:1.6;" title="A\u00f1adir a Agenda de Hoy" onclick="event.stopPropagation();Shiny.setInputValue(\'%s\',{row:%d,nonce:Math.random()},{priority:\'event\'})">+</button>',
          add_input_id, i
        )
      }, character(1))

      n_cols   <- ncol(display)
      amt_tgts <- which(names(display) %in% c(amt_col, "D\u00edas")) - 1L
      btn_tgt  <- n_cols - 1L

      dt <- DT::datatable(
        display,
        rownames  = FALSE,
        escape    = FALSE,
        selection = "multiple",
        class     = "table table-sm table-hover",
        options   = list(
          pageLength = 25,
          dom        = "ftp",
          columnDefs = list(
            list(className = "dt-right",  targets = amt_tgts),
            list(className = "dt-center", targets = btn_tgt,
                 width = "36px", orderable = FALSE)
          ),
          language = list(
            search      = "Buscar:",
            paginate    = list(`next` = "\u203a", previous = "\u2039"),
            info        = "Mostrando _START_\u2013_END_ de _TOTAL_",
            zeroRecords = "Sin resultados"
          )
        )
      )

      if (amt_col %in% names(display))
        dt <- DT::formatRound(dt, amt_col, digits = 2, mark = ",")
      if ("D\u00edas" %in% names(display))
        dt <- DT::formatStyle(dt, "D\u00edas",
                              color      = DT::styleInterval(c(0, 30), c("inherit", "#856404", "#dc3545")),
                              fontWeight = DT::styleInterval(30, c("normal", "bold")))
      dt
    }, server = FALSE)

  })
}
