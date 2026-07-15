# =============================================================================
# R/search_module.R
# Monthly invoice search modal with inline edit toolbar.
# Called from app.R via observeEvent(input$btn_search).
# Edit actions fire Shiny.setInputValue('search_action', ...) which is
# handled by observeEvent(input$search_action) in app.R.
# =============================================================================

show_search_modal <- function(input, output, session, shared, search_raw_data = NULL) {

  month_start <- tryCatch({
    ms <- lubridate::floor_date(as.Date(input$month_sel %||% Sys.Date()), "month")
    if (!length(ms)) stop("zero-length date")
    ms
  }, error = function(e) lubridate::floor_date(Sys.Date(), "month"))
  month_end  <- as.Date(lubridate::ceiling_date(month_start, "month") - lubridate::days(1))
  nav_months <- {
    yr <- as.integer(format(month_start, "%Y"))
    mo <- as.integer(format(month_start, "%m")) - 2L
    if (mo <= 0L) { mo <- mo + 12L; yr <- yr - 1L }
    seq.Date(as.Date(sprintf("%04d-%02d-01", yr, mo)), by = "month", length.out = 5)
  }
  range_start <- nav_months[1]
  range_end   <- as.Date(lubridate::ceiling_date(nav_months[5], "month") - lubridate::days(1))

  # Navigation pool: 12 months before current through 12 after = 25 total
  sm_pool <- {
    yr0 <- as.integer(format(month_start, "%Y"))
    mo0 <- as.integer(format(month_start, "%m")) - 12L
    while (mo0 <= 0L) { mo0 <- mo0 + 12L; yr0 <- yr0 - 1L }
    seq.Date(as.Date(sprintf("%04d-%02d-01", yr0, mo0)), by = "month", length.out = 25)
  }
  sm_vals        <- format(sm_pool, "%Y-%m")
  sm_labels      <- format(sm_pool, "%b %Y")
  sm_win_start   <- which(sm_vals == format(nav_months[1], "%Y-%m")) - 1L  # 0-based for JS
  sm_cur         <- format(month_start, "%Y-%m")
  sm_vals_json   <- paste0('["', paste(sm_vals,   collapse = '","'), '"]')
  sm_labels_json <- paste0('["', paste(sm_labels, collapse = '","'), '"]')

  build_ledger_table <- function(ledger_name) {
    # Pull from the same df_combined() the calendar uses — empresa filter,
    # papelera, and confirmed marks are already applied there.
    df <- tryCatch(
      if (ledger_name == "AR") shared$df_combined_AR() else shared$df_combined_AP(),
      error = function(e) NULL
    )
    if (is.null(df) || !nrow(df)) return(NULL)

    # IC filter — same logic and same code_col as df_calendar() in ledger_module
    ic_mode_val  <- tryCatch(shared$ic_mode[[ledger_name]](), error = function(e) "exclude")
    ic_codes_val <- tryCatch(build_ic_fullcodes(shared$interco_v2(), ledger_name), error = function(e) character())
    ic_rfcs_val  <- tryCatch({
      r <- unique(toupper(trimws(unname(shared$interco_v2()$rfcs %||% character()))))
      r[nzchar(r)]
    }, error = function(e) character())
    ic_code_col  <- if (ledger_name == "AR") "C\u00f3digo de cliente" else "C\u00f3digo de proveedor"
    df <- apply_ic_filter(df, mode = ic_mode_val, code_col = ic_code_col,
                          ic_codes = ic_codes_val, ic_rfcs = ic_rfcs_val)

    amt <- shared$amount_col()
    df <- df |>
      dplyr::filter(!is.na(FechaEff),
                    as.Date(FechaEff) >= range_start,
                    as.Date(FechaEff) <= range_end) |>
      dplyr::mutate(
        Importe = abs(tidyr::replace_na(
          if (amt %in% names(df)) .data[[amt]]
          else if ("Importe" %in% names(df)) .data[["Importe"]]
          else 0, 0)),
        Ledger = ledger_name,
        Tipo   = if (ledger_name == "AR") "Cobro" else "Pago",
        Fecha  = as.Date(FechaEff)
      )

    df <- add_tag_labels(df, shared$tags_db(), ledger_name)

    keep <- intersect(
      c("Ledger","Tipo","Empresa","Fecha","Parte","Documento","Factura",
        "Moneda","Importe","Etiqueta","source","id","Codigo","confirmed",
        "FechaVenc_Original","FechaVenc_Proyectada","provision_id"),
      names(df)
    )
    as.data.frame(df)[, keep, drop = FALSE]
  }

  ar_df  <- tryCatch(build_ledger_table("AR"), error = function(e) NULL)
  ap_df  <- tryCatch(build_ledger_table("AP"), error = function(e) NULL)
  all_df <- dplyr::bind_rows(ar_df, ap_df)

  # Papelera and empresa filters are already applied inside df_combined() —
  # no need to re-apply them here.

  if (is.null(all_df) || !nrow(all_df)) {
    showModal(modalDialog(
      title = "Búsqueda de facturas",
      "No hay facturas para el mes seleccionado.",
      easyClose = TRUE, footer = modalButton("Cerrar")
    ))
    return()
  }

  all_df[["tag_weight"]] <- dplyr::case_when(
    all_df[["Etiqueta"]] == "\U0001f7e0 Ambas"      ~ 1L,
    all_df[["Etiqueta"]] == "\U0001f534 Urgente"    ~ 2L,
    all_df[["Etiqueta"]] == "\U0001f7e1 Importante" ~ 3L,
    TRUE                                             ~ 4L
  )

  .fecha_orig <- if ("FechaVenc_Original" %in% names(all_df)) {
    orig <- suppressWarnings(as.Date(all_df[["FechaVenc_Original"]]))
    ifelse(!is.na(orig), format(orig, "%d/%m/%Y"), format(all_df[["Fecha"]], "%d/%m/%Y"))
  } else format(all_df[["Fecha"]], "%d/%m/%Y")

  disp_df <- data.frame(
    row_id     = seq_len(nrow(all_df)),
    Ledger     = all_df[["Ledger"]],
    Tipo       = all_df[["Tipo"]],
    Empresa    = all_df[["Empresa"]],
    Fecha      = format(all_df[["Fecha"]], "%d/%m/%Y"),
    FechaOrig  = .fecha_orig,
    Parte      = all_df[["Parte"]],
    Documento  = all_df[["Documento"]] %||% "",
    Codigo     = if ("Codigo" %in% names(all_df)) all_df[["Codigo"]] %||% "" else "",
    Referencia = if ("Factura" %in% names(all_df)) all_df[["Factura"]] %||% "" else "",
    Moneda     = all_df[["Moneda"]],
    Importe    = all_df[["Importe"]],
    Etiqueta   = all_df[["Etiqueta"]],
    Month      = format(all_df[["Fecha"]], "%Y-%m"),
    tag_weight = all_df[["tag_weight"]],
    source       = all_df[["source"]] %||% "sap",
    inv_id       = if ("id" %in% names(all_df)) all_df[["id"]] %||% "" else "",
    provision_id = if ("provision_id" %in% names(all_df)) all_df[["provision_id"]] %||% "" else "",
    stringsAsFactors = FALSE
  )
  disp_df <- disp_df[order(disp_df[["tag_weight"]], -disp_df[["Importe"]]), ]

  # Populate raw SAP data for the month (used by the "Ver datos SAP" button)
  if (is.function(search_raw_data) || is.reactive(search_raw_data)) {
    tryCatch({
      raw_list <- shared$sap_data()
      raw_month <- dplyr::bind_rows(lapply(c("AR", "AP"), function(ldg) {
        r <- raw_list[[ldg]]
        if (is.null(r) || !nrow(r)) return(NULL)
        date_col <- if ("Fecha de vencimiento" %in% names(r)) "Fecha de vencimiento"
                    else if ("FechaEff" %in% names(r)) "FechaEff"
                    else NULL
        if (!is.null(date_col)) {
          r <- r[!is.na(r[[date_col]]) &
                 as.Date(r[[date_col]]) >= month_start &
                 as.Date(r[[date_col]]) <= month_end, , drop = FALSE]
        }
        emp <- shared$empresa_sel()
        if (length(emp) && "Empresa" %in% names(r))
          r <- r[r[["Empresa"]] %in% emp, , drop = FALSE]
        if (nrow(r)) { r[["Ledger"]] <- ldg; r } else NULL
      }))
      search_raw_data(if (!is.null(raw_month) && nrow(raw_month)) raw_month else NULL)
    }, error = function(e) NULL)
  }

  showModal(modalDialog(
    title     = tagList(
      tags$span(paste0("Facturas — ", format(month_start, "%B %Y")),
                style = "margin-right: 10px;"),
      tags$span(
        style = "display:inline-flex; align-items:center; gap:4px; vertical-align:middle;",
        tags$button(
          id      = "search-month-prev",
          class   = "search-month-arrow",
          onclick = "searchMonthShift(-1)",
          HTML("&#10094;")
        ),
        tags$span(
          id    = "search-month-nav",
          class = "search-month-nav",
          style = "display:inline-flex; gap:4px;",
          lapply(seq_along(nav_months), function(i) {
            val    <- format(nav_months[[i]], "%Y-%m")
            lbl    <- format(nav_months[[i]], "%b %Y")
            is_cur <- val == format(month_start, "%Y-%m")
            tags$button(
              class        = paste0("btn btn-sm ", if (is_cur) "btn-primary" else "btn-outline-secondary"),
              `data-month` = val,
              onclick      = paste0("searchSetMonth('", val, "', this)"),
              lbl
            )
          })
        ),
        tags$button(
          id      = "search-month-next",
          class   = "search-month-arrow",
          onclick = "searchMonthShift(1)",
          HTML("&#10095;")
        ),
        tags$script(HTML(paste0(
          "window._smPool=",    sm_vals_json,   ";",
          "window._smLabels=",  sm_labels_json, ";",
          "window._smWin=",     sm_win_start,   ";",
          "window._smCur='",    sm_cur,         "';"
        )))
      )
    ),
    size      = "xl",
    easyClose = TRUE,
    footer    = tagList(
      tags$button(
        id      = "search_edit_toggle",
        class   = "btn btn-outline-secondary btn-sm me-auto",
        onclick = "toggleSearchEdit()",
        "\u270f\ufe0f Editar"
      ),
      tags$button(
        id      = "search_stage_all_btn",
        class   = "btn btn-outline-success btn-sm",
        onclick = "searchStage('all')",
        "\U0001f6d2 Agregar todo"
      ),
      tags$button(
        id      = "search_stage_sel_btn",
        class   = "btn btn-outline-success btn-sm",
        onclick = "searchStageSelected()",
        "\uff0b Agregar selecci\u00f3n"
      ),
      modalButton("Cerrar")
    ),

    tagList(
      tags$script(src = "search.js?v=3"),
      tags$style(HTML("
        .modal-xl { max-width: 1400px; }
        .search-row { cursor: pointer; user-select: none; }
        .search-row.search-row-selected > td { background-color: rgba(59,130,246,0.13) !important; }
        .search-row.search-row-selected > td:first-child { box-shadow: inset 3px 0 0 #3b82f6; }
        .srb-cobro { color:#1a6cc4; font-weight:600; }
        .srb-pago  { color:#16803c; font-weight:600; }
        .srb-sep   { color:#9ca3af; }
        .pasivos-provision { border: 1.5px dashed #a78bfa !important; background: #faf5ff !important; }
        .pasivos-tag-provision { background-color: #7c3aed; color: white; border-radius: 12px;
          padding: 2px 10px; font-size: 12px; font-weight: 500; display: inline-block; }
        .pasivos-convert-btn { background: none; border: none; cursor: pointer; padding: 0 3px;
          font-size: 1rem; line-height: 1; vertical-align: middle; opacity: 0.85; }
        .pasivos-convert-btn:hover { opacity: 1; }
        .search-row-deleting { opacity: 0.2 !important; transition: opacity 0.3s; pointer-events: none; }
        .srb-staged { color: #16a34a; font-weight: 700; font-size: 0.8em; margin-left: 4px; }
        .search-th-sort { cursor: pointer; user-select: none; white-space: nowrap; }
        .search-th-sort:hover { background: #f0f5ff; }
        .search-sort-arrow { color: #adb5bd; font-size: .75rem; margin-left: 3px; }
        .search-sort-arrow.active { color: #0a58ca; }
        .modal-title { display: flex !important; align-items: center; flex-wrap: wrap; }
        .search-month-arrow {
          background: none; border: none; cursor: pointer;
          color: #9ca3af; font-size: 1.15rem; line-height: 1;
          padding: 4px 9px; border-radius: 6px; transition: color .15s, background .15s;
        }
        .search-month-arrow:hover { color: #0a58ca; background: #eff6ff; }
        .search-month-arrow:disabled { color: #e2e8f0; cursor: default; background: none; }
        @keyframes smOutL { from { opacity:1; transform:translateX(0);      } to { opacity:0; transform:translateX(-14px); } }
        @keyframes smOutR { from { opacity:1; transform:translateX(0);      } to { opacity:0; transform:translateX( 14px); } }
        @keyframes smInR  { from { opacity:0; transform:translateX( 14px); } to { opacity:1; transform:translateX(0);      } }
        @keyframes smInL  { from { opacity:0; transform:translateX(-14px); } to { opacity:1; transform:translateX(0);      } }
        .search-month-nav.sm-out-l { animation: smOutL 0.13s ease forwards; }
        .search-month-nav.sm-out-r { animation: smOutR 0.13s ease forwards; }
        .search-month-nav.sm-in-r  { animation: smInR  0.13s ease both; }
        .search-month-nav.sm-in-l  { animation: smInL  0.13s ease both; }
      ")),

      # ── Search + filter bar ───────────────────────────────────────────────
      div(class = "search-bar-row d-flex flex-wrap gap-2 mb-3 align-items-end",
        div(class = "flex-grow-1",
          tags$input(id = "search_text", type = "text",
                     class = "form-control form-control-sm",
                     placeholder = "Buscar por cliente, proveedor, documento...")
        ),
        div(
          tags$label("Tipo", class = "form-label mb-0 small text-muted"),
          tags$select(id = "search_tipo", class = "form-select form-select-sm",
                      style = "min-width:100px;",
                      tags$option(value = "", "Todos"),
                      tags$option(value = "Cobro", "Cobros (AR)"),
                      tags$option(value = "Pago",  "Pagos (AP)"))
        ),
        div(
          tags$label("Moneda", class = "form-label mb-0 small text-muted"),
          tags$select(id = "search_moneda", class = "form-select form-select-sm",
                      style = "min-width:90px;",
                      tags$option(value = "", "Todas"),
                      lapply(sort(unique(disp_df[["Moneda"]])), function(m)
                        tags$option(value = m, m)))
        ),
        div(
          tags$label("Etiqueta", class = "form-label mb-0 small text-muted"),
          tags$select(id = "search_tag", class = "form-select form-select-sm",
                      style = "min-width:130px;",
                      tags$option(value = "",          "Todas"),
                      tags$option(value = "tagged",    "Solo etiquetadas"),
                      tags$option(value = "urgent",    "\U0001f534 Urgente"),
                      tags$option(value = "important", "\U0001f7e1 Importante"),
                      tags$option(value = "both",      "\U0001f7e0 Ambas"))
        )
      ),

      # ── Count + select-all ────────────────────────────────────────────────
      div(class = "d-flex align-items-center gap-3 mb-2",
        tags$div(id = "search_count", class = "small text-muted",
                 paste0(nrow(disp_df), " facturas")),
        tags$div(id = "search_sel_btns", class = "d-flex gap-1",
          tags$button(id = "search_sel_all_btn",
                      class = "btn btn-outline-secondary btn-sm",
                      onclick = "searchSelectAll()",
                      "Seleccionar todos"),
          tags$button(id = "search_sel_none_btn",
                      class = "btn btn-outline-secondary btn-sm",
                      style = "display:none;",
                      onclick = "searchSelectNone()",
                      "Deseleccionar")
        )
      ),

      # ── Selection sum bar ────────────────────────────────────────────────
      div(id    = "search_sum_bar",
          class = "align-items-center gap-3 px-3 py-2 mb-2 rounded",
          style = "display:none; flex-wrap:wrap; background:#eff6ff; border:1px solid #bfdbfe;",
        tags$span(id = "search_sum_count", class = "fw-bold small text-primary"),
        tags$span(id = "search_sum_detail", class = "small flex-grow-1"),
        tags$button(
          type    = "button",
          class   = "btn btn-sm btn-link p-0 ms-auto text-muted text-decoration-none",
          onclick = "searchSelectNone()",
          style   = "font-size:1.2rem; line-height:1;",
          "\u00d7"
        )
      ),

      # ── Edit toolbar (hidden until toggled) ──────────────────────────────
      div(id    = "search_edit_toolbar",
          class = "search-edit-toolbar",
          style = "display:none;",
        div(class = "d-flex flex-wrap gap-2 align-items-end",
          div(class = "d-flex gap-1",
            tags$button(class = "btn btn-sm btn-outline-danger",
                        onclick = "searchAction('tag_urgent')",
                        "\U0001f534 Urgente"),
            tags$button(class = "btn btn-sm btn-outline-warning",
                        onclick = "searchAction('tag_important')",
                        "\U0001f7e1 Importante"),
            tags$button(class = "btn btn-sm",
                        style = "border-color:#FF6B35;color:#FF6B35;",
                        onclick = "searchAction('tag_both')",
                        "\U0001f7e0 Ambas"),
            tags$button(class = "btn btn-sm btn-outline-secondary",
                        onclick = "searchAction('tag_clear')",
                        "\u2716 Quitar etiqueta")
          ),
          tags$div(class = "vr"),
          div(class = "d-flex gap-1 align-items-end",
            div(
              tags$label("Mover a:", class = "form-label mb-0 small text-muted"),
              tags$input(id = "search_move_date", type = "date",
                         class = "form-control form-control-sm",
                         style = "width:145px;",
                         value = format(Sys.Date() + 1, "%Y-%m-%d"))
            ),
            tags$button(class = "btn btn-sm btn-primary",
                        onclick = "searchAction('move')",
                        "\U0001f4c5 Mover"),
            tags$button(class = "btn btn-sm btn-outline-secondary",
                        onclick = "searchAction('restore')",
                        "\u21a9 Restaurar fecha")
          ),
          tags$div(class = "vr"),
          tags$button(class = "btn btn-sm btn-outline-danger",
                      onclick = "searchAction('delete')",
                      "\U0001f5d1 Eliminar"),
          tags$div(class = "vr"),
          tags$button(class = "btn btn-sm btn-outline-secondary",
                      onclick = "Shiny.setInputValue('btn_ver_datos', Math.random(), {priority:'event'})",
                      "\U0001f50d Ver datos SAP")
        )
      ),

      # ── Stage confirmation bar (replaces window.confirm) ─────────────────
      div(id    = "search_stage_confirm_bar",
          class = "alert alert-warning align-items-center gap-3 py-2 px-3 mb-2",
          style = "display:none;",
        span(id = "search_stage_confirm_msg", class = "flex-grow-1 small fw-bold"),
        tags$button(
          class   = "btn btn-sm btn-success",
          onclick = "confirmStage()",
          "\u2713 Confirmar"
        ),
        tags$button(
          class   = "btn btn-sm btn-outline-secondary",
          onclick = "cancelStage()",
          "Cancelar"
        )
      ),

      # ── Table ─────────────────────────────────────────────────────────────
      div(style = "max-height:52vh; overflow-y:auto; margin-top:6px;",
        tags$table(
          id    = "search_results_tbl",
          class = "table table-sm table-hover search-table",
          tags$thead(
            tags$tr(
              tags$th(class = "search-th-sort", `data-col` = "tipo",
                      onclick = "searchSortByCol('tipo')",
                      "Tipo", tags$span(class = "search-sort-arrow", "↕")),
              tags$th(class = "search-th-sort", `data-col` = "empresa",
                      onclick = "searchSortByCol('empresa')",
                      "Empresa", tags$span(class = "search-sort-arrow", "↕")),
              tags$th(class = "search-th-sort", `data-col` = "fecha",
                      onclick = "searchSortByCol('fecha')",
                      "Fecha", tags$span(class = "search-sort-arrow", "↕")),
              tags$th(class = "search-th-sort", `data-col` = "parte",
                      onclick = "searchSortByCol('parte')",
                      "Parte", tags$span(class = "search-sort-arrow", "↕")),
              tags$th(class = "search-th-sort", `data-col` = "referencia",
                      onclick = "searchSortByCol('referencia')",
                      "Referencia", tags$span(class = "search-sort-arrow", "↕")),
              tags$th(class = "search-th-sort", `data-col` = "documento",
                      onclick = "searchSortByCol('documento')",
                      "Documento", tags$span(class = "search-sort-arrow", "↕")),
              tags$th(class = "search-th-sort", `data-col` = "moneda",
                      onclick = "searchSortByCol('moneda')",
                      "Moneda", tags$span(class = "search-sort-arrow", "↕")),
              tags$th(class = "search-th-sort text-end", `data-col` = "importe",
                      onclick = "searchSortByCol('importe')",
                      "Importe", tags$span(class = "search-sort-arrow", "↕")),
              tags$th(class = "search-th-sort", `data-col` = "etiqueta",
                      onclick = "searchSortByCol('etiqueta')",
                      "Etiqueta", tags$span(class = "search-sort-arrow", "↕"))
            )
          ),
          tags$tbody(
            id = "search_tbody",
            HTML(paste(vapply(seq_len(nrow(disp_df)), function(i) {
              row    <- disp_df[i, ]
              row_bg <- switch(row[["Etiqueta"]],
                "\U0001f534 Urgente"    = "background:#fff0f0;",
                "\U0001f7e1 Importante" = "background:#fffbe6;",
                "\U0001f7e0 Ambas"      = "background:#fff3e6;",
                ""
              )
              is_provision     <- identical(row[["source"]], "provision")
              prov_id_val      <- row[["provision_id"]] %||% ""
              tipo_badge_class <- if (is_provision) "pasivos-tag-provision"
                                  else if (row[["Tipo"]] == "Cobro") "badge bg-primary"
                                  else "badge bg-success"
              tipo_label       <- if (is_provision) "Provisión" else htmltools::htmlEscape(row[["Tipo"]])
              row_extra_cls    <- if (is_provision) " pasivos-provision" else ""
              etiqueta_html    <- if (nzchar(row[["Etiqueta"]])) htmltools::htmlEscape(row[["Etiqueta"]]) else ""
              paste0(
                '<tr class="search-row', row_extra_cls, '"',
                ' data-tipo="',        htmltools::htmlEscape(row[["Tipo"]]),           '"',
                ' data-ledger="',      htmltools::htmlEscape(row[["Ledger"]]),         '"',
                ' data-month="',       htmltools::htmlEscape(row[["Month"]]),          '"',
                ' data-moneda="',      htmltools::htmlEscape(row[["Moneda"]]),         '"',
                ' data-tag="',         htmltools::htmlEscape(row[["Etiqueta"]]),       '"',
                ' data-importe="',     as.character(row[["Importe"]]),                 '"',
                ' data-fecha="',       htmltools::htmlEscape(row[["Fecha"]]),          '"',
                ' data-fecha-orig="',  htmltools::htmlEscape(row[["FechaOrig"]] %||% row[["Fecha"]]), '"',
                ' data-parte="',       htmltools::htmlEscape(tolower(row[["Parte"]])), '"',
                ' data-ref="',         htmltools::htmlEscape(tolower(row[["Referencia"]])), '"',
                ' data-doc="',         htmltools::htmlEscape(tolower(row[["Documento"]])), '"',
                ' data-empresa="',     htmltools::htmlEscape(row[["Empresa"]]),        '"',
                ' data-documento="',   htmltools::htmlEscape(row[["Documento"]]),      '"',
                ' data-source="',      htmltools::htmlEscape(row[["source"]]),         '"',
                ' data-invid="',       htmltools::htmlEscape(row[["inv_id"]]),         '"',
                ' data-provisionid="', htmltools::htmlEscape(prov_id_val),             '"',
                ' data-codigo="',      htmltools::htmlEscape(row[["Codigo"]] %||% ""), '"',
                ' data-tagweight="',   as.character(row[["tag_weight"]]),              '"',
                if (nzchar(row_bg)) paste0(' style="', row_bg, '"') else "",
                '>',
                if (is_provision && nzchar(prov_id_val))
                  sprintf('<td><span class="pasivos-tag-provision" style="cursor:pointer;" onclick="event.stopPropagation();Shiny.setInputValue(\'pasivos_convert_request\',\'%s\',{priority:\'event\'})" title="Convertir a comprobante">Provisión</span></td>',
                          htmltools::htmlEscape(prov_id_val))
                else
                  paste0('<td><span class="', tipo_badge_class, '">', tipo_label, '</span></td>'),
                '<td><span class="cart-empresa-badge">', htmltools::htmlEscape(row[["Empresa"]]), '</span></td>',
                '<td>', htmltools::htmlEscape(row[["Fecha"]]), '</td>',
                '<td style="max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">',
                  htmltools::htmlEscape(row[["Parte"]]),
                '</td>',
                '<td class="text-muted">', htmltools::htmlEscape(row[["Referencia"]]), '</td>',
                '<td class="text-muted small">', htmltools::htmlEscape(row[["Documento"]]), '</td>',
                '<td>', htmltools::htmlEscape(row[["Moneda"]]), '</td>',
                '<td class="text-end fw-bold" style="color:#1a6cc4;font-variant-numeric:tabular-nums;">',
                  fmt_money(row[["Importe"]]),
                '</td>',
                '<td>', etiqueta_html, '</td>',
                '</tr>'
              )
            }, character(1)), collapse = "\n"))
          )
        )
      ),

      # ── JavaScript ───────────────────────────────────────────────────────
      tags$script(HTML("
        (function() {

          function filterAndSort() {
            var text   = (document.getElementById('search_text')   || {}).value || '';
            var tipo   = (document.getElementById('search_tipo')   || {}).value || '';
            var moneda = (document.getElementById('search_moneda') || {}).value || '';
            var tag    = (document.getElementById('search_tag')    || {}).value || '';
            var q      = text.toLowerCase();
            var rows   = Array.from(document.querySelectorAll('#search_results_tbl .search-row'));

            var activeMonthBtn = document.querySelector('.search-month-nav .btn-primary');
            var mFilter = activeMonthBtn ? activeMonthBtn.dataset.month : '';
            var visible = rows.filter(function(r) {
              if (mFilter && r.dataset.month !== mFilter)                     return false;
              if (tipo   && r.dataset.tipo   !== tipo)                        return false;
              if (moneda && r.dataset.moneda !== moneda)                      return false;
              if (q && !(r.dataset.parte.includes(q) || r.dataset.doc.includes(q) || (r.dataset.ref || '').includes(q) || (r.dataset.importe || '').includes(q))) return false;
              if (tag === 'tagged'    && !r.dataset.tag)                         return false;
              if (tag === 'urgent'    && r.dataset.tag.indexOf('Urgente')    < 0) return false;
              if (tag === 'important' && r.dataset.tag.indexOf('Importante') < 0) return false;
              if (tag === 'both'      && r.dataset.tag.indexOf('Ambas')      < 0) return false;
              return true;
            });

            rows.forEach(function(r) { r.style.display = 'none'; });
            visible.sort(function(a, b) {
              var col = window._searchSortCol;
              var dir = window._searchSortDir === 'asc' ? 1 : -1;
              if (!col) {
                var wa = parseInt(a.dataset.tagweight || '4');
                var wb = parseInt(b.dataset.tagweight || '4');
                if (wa !== wb) return wa - wb;
                return parseFloat(b.dataset.importe) - parseFloat(a.dataset.importe);
              }
              if (col === 'importe') return dir * (parseFloat(a.dataset.importe) - parseFloat(b.dataset.importe));
              if (col === 'fecha') {
                var toYMD = function(s) { return s ? s.substring(6) + s.substring(3, 5) + s.substring(0, 2) : ''; };
                return dir * toYMD(a.dataset.fecha).localeCompare(toYMD(b.dataset.fecha));
              }
              var km = {tipo:'tipo',empresa:'empresa',parte:'parte',documento:'doc',referencia:'ref',etiqueta:'tag',moneda:'moneda'};
              var key = km[col] || col;
              return dir * (a.dataset[key]||'').toLowerCase().localeCompare((b.dataset[key]||'').toLowerCase());
            });
            var tbody = document.getElementById('search_tbody');
            visible.forEach(function(r) { r.style.display = ''; tbody.appendChild(r); });
            var cnt = document.getElementById('search_count');
            if (cnt) cnt.textContent = visible.length + ' factura' + (visible.length !== 1 ? 's' : '');
            if (typeof searchSyncSelButtons === 'function') searchSyncSelButtons();
          }

          window.toggleSearchEdit = function() {
            var toolbar = document.getElementById('search_edit_toolbar');
            var btn     = document.getElementById('search_edit_toggle');
            var active  = toolbar && toolbar.style.display !== 'none';
            if (active) {
              if (toolbar) toolbar.style.display = 'none';
              if (btn) { btn.classList.remove('btn-secondary'); btn.classList.add('btn-outline-secondary'); }
            } else {
              if (toolbar) toolbar.style.display = 'block';
              if (btn) { btn.classList.remove('btn-outline-secondary'); btn.classList.add('btn-secondary'); }
            }
          };

          window.getCheckedRows = function getCheckedRows() {
            var out = [];
            document.querySelectorAll('.search-row.search-row-selected').forEach(function(r) {
              out.push({
                ledger   : r.dataset.ledger,
                empresa  : r.dataset.empresa,
                moneda   : r.dataset.moneda,
                documento: r.dataset.documento,
                source      : r.dataset.source,
                inv_id      : r.dataset.invid,
                provision_id: r.dataset.provisionid || '',
                parte    : r.dataset.parte,
                codigo   : r.dataset.codigo || '',
                importe  : parseFloat(r.dataset.importe),
                fecha    : r.dataset.fecha,
                tipo     : r.dataset.tipo
              });
            });
            return out;
          }

          function getAllVisibleRows() {
            var out = [];
            document.querySelectorAll('.search-row').forEach(function(r) {
              if (r.style.display !== 'none') {
                out.push({
                  ledger   : r.dataset.ledger,
                  empresa  : r.dataset.empresa,
                  moneda   : r.dataset.moneda,
                  documento: r.dataset.documento,
                  source      : r.dataset.source,
                  inv_id      : r.dataset.invid,
                  provision_id: r.dataset.provisionid || '',
                  parte    : r.dataset.parte,
                  codigo   : r.dataset.codigo || '',
                  importe  : parseFloat(r.dataset.importe),
                  fecha    : r.dataset.fecha,
                  tipo     : r.dataset.tipo
                });
              }
            });
            return out;
          }

          // ── Agregar todo ─────────────────────────────────────────────────────
          window.searchStage = function(mode) {
            var rows = getAllVisibleRows();
            if (!rows.length) { Shiny.setInputValue('search_stage_toast', { msg: 'No hay facturas visibles.', type: 'warning', nonce: Math.random() }, { priority: 'event' }); return; }
            var bar = document.getElementById('search_stage_confirm_bar');
            var msg = document.getElementById('search_stage_confirm_msg');
            if (!bar || !msg) return;
            msg.textContent = '\u00bfAgregar ' + rows.length + ' factura(s) visibles a la Agenda del d\u00eda?';
            bar.style.display = 'flex';
            bar.dataset.pendingRows = JSON.stringify(rows);
          };

          window.confirmStage = function() {
            var bar = document.getElementById('search_stage_confirm_bar');
            if (!bar) return;
            var rows = JSON.parse(bar.dataset.pendingRows || '[]');
            bar.style.display = 'none';
            if (!rows.length) return;
            Shiny.setInputValue('search_action',
              { action: 'stage_all', rows: rows, nonce: Math.random() },
              { priority: 'event' });
          };

          window.cancelStage = function() {
            var bar = document.getElementById('search_stage_confirm_bar');
            if (bar) bar.style.display = 'none';
          };

          window.searchAction = function(action) {
            var rows = getCheckedRows();
            if (!rows.length) {
              Shiny.setInputValue('search_stage_toast',
                { msg: 'Selecciona al menos una factura.', type: 'warning', nonce: Math.random() },
                { priority: 'event' });
              return;
            }
            if (action === 'delete') {
              var payload = { action: 'delete', rows: rows, nonce: Math.random() };
              Shiny.setInputValue('search_action', payload, { priority: 'event' });
              if (window.searchApplyUpdate) window.searchApplyUpdate('delete', payload, searchSelectedRows());
              return;
            }
            var payload = { action: action, rows: rows, nonce: Math.random() };
            if (action === 'move') {
              var d = document.getElementById('search_move_date');
              payload.move_to = d ? d.value : '';
              if (!payload.move_to) {
                Shiny.setInputValue('search_stage_toast',
                  { msg: 'Elige una fecha para mover.', type: 'warning', nonce: Math.random() },
                  { priority: 'event' });
                return;
              }
            }
            Shiny.setInputValue('search_action', payload, { priority: 'event' });
            if (window.searchApplyUpdate) window.searchApplyUpdate(action, payload, searchSelectedRows());
          };

          window.searchFilterAndSort = filterAndSort;
          setTimeout(function() {
            window._searchSortCol = null;
            window._searchSortDir = 'asc';
            ['search_text','search_tipo','search_moneda','search_tag'].forEach(function(id) {
              var el = document.getElementById(id);
              if (el) el.addEventListener('input', filterAndSort);
            });
            filterAndSort();
          }, 100);
        })();
      ")),

      # separate script: searchStageSelected
      # (kept outside the IIFE to stay under R's 10000-char Unicode-escape limit;
      #  calls window.getCheckedRows which is exposed by the IIFE above)
      tags$script(HTML("
        // -- Row helpers (mirrors vencidos.js pattern) -------------------------
        function searchAllRows()     { return Array.from(document.querySelectorAll('#search_results_tbl .search-row')); }
        function searchVisibleRows() { return searchAllRows().filter(function(r) { return r.style.display !== 'none'; }); }
        function searchSelectedRows(){ return Array.from(document.querySelectorAll('#search_results_tbl .search-row.search-row-selected')); }

        function searchRowPayload(r) {
          return {
            ledger   : r.dataset.ledger,
            empresa  : r.dataset.empresa,
            moneda   : r.dataset.moneda,
            documento: r.dataset.documento,
            source      : r.dataset.source,
            inv_id      : r.dataset.invid || '',
            provision_id: r.dataset.provisionid || '',
            parte    : r.dataset.parte,
            importe  : parseFloat(r.dataset.importe),
            fecha    : r.dataset.fecha,
            tipo     : r.dataset.tipo
          };
        }

        // -- Sync selection UI (mirrors venSyncSelButtons) ---------------------
        window.searchSyncSelButtons = function() {
          var nTotal  = searchSelectedRows().length;
          var noneBtn = document.getElementById('search_sel_none_btn');
          var allBtn  = document.getElementById('search_sel_all_btn');
          if (noneBtn) noneBtn.style.display = nTotal > 0 ? '' : 'none';
          if (allBtn)  allBtn.style.display  = nTotal > 0 ? 'none' : '';
          updateSearchSumBar();
        };

        window.updateSearchSumBar = function() {
          var selected = searchSelectedRows();
          var bar = document.getElementById('search_sum_bar');
          if (!bar) return;
          if (!selected.length) { bar.style.display = 'none'; return; }
          var ar = {}, ap = {};
          selected.forEach(function(r) {
            var cur = r.dataset.moneda || '?';
            var lgr = (r.dataset.ledger || '').toUpperCase();
            var map = (lgr === 'AR') ? ar : ap;
            map[cur] = (map[cur] || 0) + (parseFloat(r.dataset.importe) || 0);
          });
          function makeSection(label, map, cls) {
            var keys = Object.keys(map);
            if (!keys.length) return '';
            var lines = keys.sort().map(function(cur) {
              var fmt = '$ ' + map[cur].toLocaleString('es-MX',
                          { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              return '<span class=' + cls + '>' + cur + ' ' + fmt + '</span>';
            });
            return '<b class=' + cls + '>' + label + '</b> ' + lines.join(' ');
          }
          var arHtml = makeSection('Cobros', ar, 'srb-cobro');
          var apHtml = makeSection('Pagos',  ap, 'srb-pago');
          var sep    = (arHtml && apHtml) ? '<span class=srb-sep> \u00b7 </span>' : '';
          var n = selected.length;
          var countEl  = document.getElementById('search_sum_count');
          var detailEl = document.getElementById('search_sum_detail');
          if (countEl)  countEl.textContent = n + ' factura' + (n !== 1 ? 's' : '');
          if (detailEl) detailEl.innerHTML  = arHtml + sep + apHtml;
          bar.style.display = 'flex';
        };

        window.searchSelectAll = function() {
          searchVisibleRows().forEach(function(r) { r.classList.add('search-row-selected'); });
          searchSyncSelButtons();
        };
        window.searchSelectNone = function() {
          searchAllRows().forEach(function(r) { r.classList.remove('search-row-selected'); });
          searchSyncSelButtons();
        };

        // -- Delegated row click handler (registered once) --------------------
        if (!window._searchClickBound) {
          window._searchClickBound = true;
          var _searchLastRow = null;
          document.addEventListener('click', function(e) {
            var row = e.target.closest('#search_results_tbl .search-row');
            if (!row) return;
            if (e.shiftKey && _searchLastRow && _searchLastRow !== row) {
              var vis = searchVisibleRows();
              var i1  = vis.indexOf(_searchLastRow);
              var i2  = vis.indexOf(row);
              if (i1 >= 0 && i2 >= 0) {
                var tgt = !row.classList.contains('search-row-selected');
                var lo = Math.min(i1, i2), hi = Math.max(i1, i2);
                for (var k = lo; k <= hi; k++) vis[k].classList[tgt ? 'add' : 'remove']('search-row-selected');
              } else {
                row.classList.toggle('search-row-selected');
              }
            } else {
              row.classList.toggle('search-row-selected');
            }
            _searchLastRow = row;
            searchSyncSelButtons();
          });
        }

        // -- Init arrow state (runs after pool data script in modal title) ------
        (function() {
          var prev = document.getElementById('search-month-prev');
          var next = document.getElementById('search-month-next');
          var win  = window._smWin || 0;
          var len  = (window._smPool || []).length;
          if (prev) prev.disabled = win <= 0;
          if (next) next.disabled = win + 5 >= len;
        })();

        // -- Stage selected ---------------------------------------------------
        window.searchStageSelected = function() {
          var rows = searchSelectedRows();
          if (!rows.length) { Shiny.setInputValue('search_stage_toast', { msg: 'Selecciona al menos una factura.', type: 'warning', nonce: Math.random() }, { priority: 'event' }); return; }
          Shiny.setInputValue('search_action',
            { action: 'stage_all', rows: rows.map(searchRowPayload), nonce: Math.random() },
            { priority: 'event' });
          rows.forEach(function(r) {
            var td0 = r.querySelector('td:first-child');
            if (td0 && !td0.querySelector('.srb-staged')) {
              var chip = document.createElement('span');
              chip.className = 'srb-staged';
              chip.title = 'Agregada a Agenda del día';
              chip.textContent = ' ✓';
              td0.appendChild(chip);
            }
          });
        };

        // -- Optimistic DOM patch after every toolbar action ------------------
        window.searchApplyUpdate = function(action, payload, selRows) {
          if (!selRows || !selRows.length) return;
          var TAG = {
            tag_urgent:    { label: '🔴 Urgente',    bg: '#fff0f0', tag: '🔴 Urgente',    w: '2' },
            tag_important: { label: '🟡 Importante', bg: '#fffbe6', tag: '🟡 Importante', w: '3' },
            tag_both:      { label: '🟠 Ambas',      bg: '#fff3e6', tag: '🟠 Ambas',      w: '1' },
            tag_clear:     { label: '',                        bg: '',        tag: '',           w: '4' }
          };
          if (TAG[action]) {
            var t = TAG[action];
            selRows.forEach(function(r) {
              r.dataset.tag       = t.tag;
              r.dataset.tagweight = t.w;
              r.style.background  = t.bg;
              var tds = r.querySelectorAll('td');
              if (tds.length) tds[tds.length - 1].textContent = t.label;
            });
            searchSyncSelButtons();
          } else if (action === 'move') {
            var mv  = payload.move_to;
            var pts = mv.split('-');
            var disp = pts[2] + '/' + pts[1] + '/' + pts[0];
            var mon  = mv.substring(0, 7);
            selRows.forEach(function(r) {
              r.dataset.fecha = disp;
              r.dataset.month = mon;
              var tds = r.querySelectorAll('td');
              if (tds.length >= 3) tds[2].textContent = disp;
            });
            window.searchFilterAndSort();
          } else if (action === 'restore') {
            selRows.forEach(function(r) {
              var orig = r.dataset.fechaOrig || r.dataset.fecha;
              var p    = orig.split('/');
              var mon  = p.length === 3 ? p[2] + '-' + p[1] : r.dataset.month;
              r.dataset.fecha = orig;
              r.dataset.month = mon;
              var tds = r.querySelectorAll('td');
              if (tds.length >= 3) tds[2].textContent = orig;
            });
            window.searchFilterAndSort();
          } else if (action === 'delete') {
            selRows.forEach(function(r) { r.classList.add('search-row-deleting'); });
            setTimeout(function() {
              selRows.forEach(function(r) { if (r.parentNode) r.parentNode.removeChild(r); });
              window.searchFilterAndSort();
            }, 320);
          }
        };
      ")),

    )
  ))
}


# =============================================================================
# handle_invoice_action — shared handler called by search and vencidos modules.
# payload: list with $action, $rows, optionally $move_to
# =============================================================================

handle_invoice_action <- function(payload, shared) {
  req(payload)

  action <- payload$action
  rows   <- payload$rows
  if (!length(rows)) return()

  keys_df <- data.frame(
    ledger    = vapply(rows, `[[`, character(1), "ledger"),
    Empresa   = vapply(rows, `[[`, character(1), "empresa"),
    Moneda    = vapply(rows, `[[`, character(1), "moneda"),
    Documento = vapply(rows, `[[`, character(1), "documento"),
    source    = vapply(rows, `[[`, character(1), "source"),
    inv_id       = vapply(rows, `[[`, character(1), "inv_id"),
    provision_id = vapply(rows, function(r) r[["provision_id"]] %||% "", character(1)),
    Parte     = vapply(rows, `[[`, character(1), "parte"),
    Codigo    = vapply(rows, function(r) r[["codigo"]] %||% "", character(1)),
    Importe   = vapply(rows, `[[`, numeric(1),   "importe"),
    Fecha     = vapply(rows, `[[`, character(1), "fecha"),
    tipo      = vapply(rows, function(r) r[["tipo"]] %||% "", character(1)),
    stringsAsFactors = FALSE
  )

  current_user <- tryCatch(shared$current_user(), error = function(e) "user")

  # Recover blank Empresa — happens when a row came from an old snapshot or
  # manual entry that didn't have Empresa set, so data-empresa="" in the JS.
  # Without a valid Empresa the row would be staged but invisible in Agenda de hoy
  # (pagar_hoy_module renders per-empresa and filters with Empresa == emp).
  #
  # NOTE: we do NOT try to look up raw SAP data here because the "Documento"
  # column only exists *after* build_ledger_df's ensure_documento() step.
  # The raw data still uses SAP's native column names.  We use empresa_sel()
  # as the reliable fallback instead.
  blank_emp <- !nzchar(keys_df$Empresa)
  if (any(blank_emp)) {
    emp_sel <- tryCatch(
      isolate(shared$empresa_sel()),
      error = function(e) character(0)
    )
    if (length(emp_sel) == 1L) {
      keys_df$Empresa[blank_emp] <- emp_sel[1L]
    }
  }

  if (action %in% c("tag_urgent","tag_important","tag_both","tag_clear")) {
    new_tags <- switch(action,
      tag_urgent    = "urgent",
      tag_important = "important",
      tag_both      = c("urgent","important"),
      tag_clear     = character(0)
    )
    tdb <- shared$tags_db()
    for (i in seq_len(nrow(keys_df))) {
      tdb <- set_invoice_tags(tdb,
               ledger    = keys_df$ledger[i],
               empresa   = keys_df$Empresa[i],
               moneda    = keys_df$Moneda[i],
               documento = keys_df$Documento[i],
               new_tags  = new_tags,
               tagged_by = current_user)
    }
    shared$tags_db(tdb)
    tryCatch({ save_tags(tdb, client_id = shared$active_client_id()); bump_sync_version("tags_db") }, error = function(e)
      showNotification(paste("Error guardando etiquetas:", e$message), type = "warning"))
    showNotification(paste0(nrow(keys_df), " factura(s) etiquetadas."),
                     type = "message", duration = 2)

  } else if (action == "move") {
    new_date <- tryCatch(as.Date(payload$move_to), error = function(e) NULL)
    if (is.null(new_date) || is.na(new_date)) {
      showNotification("Fecha inv\u00e1lida.", type = "warning"); return()
    }
    # Only keep schema columns — extra columns from keys_df (Fecha string, source,
    # inv_id, Parte, Importe, tipo) must NOT enter moves_db or they corrupt the
    # left_join in build_ledger_df and wipe the calendar.
    new_rows <- dplyr::transmute(keys_df,
      ledger               = .data$ledger,
      Empresa              = .data$Empresa,
      Moneda               = .data$Moneda,
      Documento            = .data$Documento,
      FechaVenc_Proyectada = new_date,
      moved_by             = current_user,
      last_updated         = Sys.time()
    )
    updated <- upsert_moves(shared$moves_db(), new_rows)
    shared$moves_db(updated)
    .save_moves_deferred(updated, client_id = shared$active_client_id())
    showNotification(
      paste0(nrow(keys_df), " factura(s) movidas a ", format(new_date, "%d/%m/%Y"), "."),
      type = "message", duration = 2)

  } else if (action == "restore") {
    # Remove move records for the selected invoices so they revert to their
    # original due date. Only the schema keys are needed for the anti_join.
    restore_keys <- dplyr::transmute(keys_df,
      ledger    = .data$ledger,
      Empresa   = .data$Empresa,
      Moneda    = .data$Moneda,
      Documento = .data$Documento
    )
    updated <- dplyr::anti_join(shared$moves_db(), restore_keys,
                                by = c("ledger","Empresa","Moneda","Documento"))
    shared$moves_db(updated)
    .save_moves_deferred(updated, client_id = shared$active_client_id())
    showNotification(
      paste0(nrow(keys_df), " factura(s) restauradas a fecha original."),
      type = "message", duration = 2)

  } else if (action == "delete") {
    # Provisions use lifecycle close, not papelera
    prov_rows <- keys_df[keys_df$source == "provision" & nzchar(keys_df$provision_id), ]
    item_rows <- keys_df[!(keys_df$source == "provision" & nzchar(keys_df$provision_id)), ]

    if (nrow(prov_rows)) {
      pids_cancel <- unique(prov_rows$provision_id)
      for (pid in pids_cancel) {
        tryCatch(pasivos_provision_cancel(provision_id = pid, user = current_user,
                                          client_id = shared$active_client_id()),
                 error = function(e) NULL)
      }
      provs_db <- tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL)
      tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
      if (!is.null(provs_db) && "id" %in% names(provs_db)) {
        mask <- !is.na(provs_db$id) & provs_db$id %in% pids_cancel
        if (any(mask)) provs_db$estado[mask] <- "closed"
        tryCatch(shared$pasivos_provisions_db(provs_db), error = function(e) NULL)
        tryCatch(bump_sync_version("pasivos_provisions_db"), error = function(e) NULL)
      } else {
        tryCatch({ shared$pasivos_provisions_db(load_pasivos_provisions()) }, error = function(e) NULL)
      }
      showNotification(paste0(nrow(prov_rows), " provisión(es) cerrada(s)."),
                       type = "message", duration = 3)
    }

    if (nrow(item_rows)) {
      cid         <- tryCatch(shared$active_client_id(), error = function(e) NULL)
      papelera_df <- tryCatch(load_papelera(client_id = cid), error = function(e) tibble::tibble())
      archive      <- item_rows
      archive$FechaEff   <- as.Date(NA)
      archive$deleted_by <- current_user
      archive$deleted_at <- Sys.time()
      papelera_df <- add_to_papelera(papelera_df, archive,
                                      ledger = "MIXED", deleted_by = current_user)
      tryCatch(save_papelera(papelera_df, client_id = shared$active_client_id()), error = function(e)
        showNotification(paste("Error guardando papelera:", e$message), type = "warning"))
      # Update shared reactive so calendars refresh without extra S3 read
      if (!is.null(shared$papelera_rv)) shared$papelera_rv(papelera_df)

      manual_rows <- item_rows[item_rows$source == "manual" & nzchar(item_rows$inv_id), ]
      if (nrow(manual_rows)) {
        m <- shared$manual_inv()
        for (mid in manual_rows$inv_id) m <- delete_manual(m, mid)
        shared$manual_inv(m)
        tryCatch(save_manual(m, client_id = shared$active_client_id()), error = function(e)
          showNotification(paste("Error actualizando manual:", e$message), type = "warning"))
      }
      showNotification(paste0(nrow(item_rows), " factura(s) enviadas a la papelera."),
                       type = "message", duration = 3)
    }

  } else if (action %in% c("stage_all", "stage_selected")) {
    # Silently exclude provision rows — they cannot be staged as regular items.
    keys_df <- pasivos_filter_out_provisions(keys_df)
    if (!nrow(keys_df)) {
      showNotification("No hay facturas válidas para agregar (se excluyeron provisiones).",
                       type = "warning", duration = 3)
      return()
    }

    # Parse fecha from dd/mm/yyyy string
    parse_fecha <- function(s) {
      d <- tryCatch(as.Date(s, "%d/%m/%Y"), error = function(e) NA_real_)
      if (is.na(d)) tryCatch(as.Date(s), error = function(e) Sys.Date())
      else d
    }

    new_rows <- data.frame(
      id        = vapply(seq_len(nrow(keys_df)), function(x) uuid::UUIDgenerate(), character(1)),
      ledger    = keys_df$ledger,
      Empresa   = keys_df$Empresa,
      Moneda    = keys_df$Moneda,
      Documento = keys_df$Documento,
      Parte     = keys_df$Parte,
      Codigo    = trimws(keys_df$Codigo %||% ""),
      tipo_item = "factura",
      Importe   = keys_df$Importe,
      FechaVenc = as.Date(vapply(keys_df$Fecha, parse_fecha, numeric(1)),
                          origin = "1970-01-01"),
      staged_by = current_user,
      staged_at = Sys.time(),
      status    = "pending",
      stringsAsFactors = FALSE
    )
    # Skip rows with blank Documento (can't key them)
    new_rows <- new_rows[nzchar(new_rows$Documento), , drop = FALSE]
    if (!nrow(new_rows)) {
      showNotification("No se encontraron facturas con documento válido.", type = "warning")
      return()
    }
    updated <- upsert_pagar_hoy(shared$pagar_hoy_db() %||% load_pagar_hoy(), new_rows)
    shared$pagar_hoy_db(updated)
    tryCatch(save_pagar_hoy(updated, shared$current_user(), client_id = shared$active_client_id()), error = function(e)
      showNotification(paste("Error guardando agenda:", e$message), type = "warning"))
    # Build a descriptive notification: count, companies, ledger type, total amount
    emp_str  <- paste(unique(new_rows$Empresa), collapse = ", ")
    total    <- sum(new_rows$Importe, na.rm = TRUE)
    cur_str  <- paste(unique(new_rows$Moneda), collapse = "/")
    ledg_str <- if (all(new_rows$ledger == "AR")) "Cobros"
                else if (all(new_rows$ledger == "AP")) "Pagos"
                else "Agenda del día"
    showNotification(
      paste0("\u2713 ", nrow(new_rows), " factura(s) enviadas a ", ledg_str,
             " \u2014 ", cur_str, " ", fmt_money(total), "."),
      type = "message", duration = 4)
  }
}

# Thin wrappers — read from the specific input slot, then delegate.
handle_search_action <- function(input, shared) {
  handle_invoice_action(input$search_action, shared)
}

handle_vencidos_action <- function(input, shared) {
  handle_invoice_action(input$vencidos_action, shared)
}
