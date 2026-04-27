# =============================================================================
# R/vencidos_module.R
# "Vencidos" tab — all unconfirmed AR + AP invoices due today or earlier.
# Respects empresa_sel filter. Always excludes intercompany.
# Edit actions (tag / move / delete / stage) share handle_invoice_action()
# with the monthly search modal (search_module.R).
#
# Row selection: click any row to select/deselect (highlighted).
# Shift+click for range. No checkboxes.
# Column headers are sortable (click to sort asc/desc).
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────

vencidosUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      /* Row selection */
      .ven-row { cursor: pointer; transition: background .1s; }
      .ven-row:hover:not(.ven-row-selected) { background: #f0f5ff !important; }
      .ven-row.ven-row-selected { background: #d0e4ff !important; outline: 2px solid #0a58ca; outline-offset:-2px; }
      /* Column sort headers */
      .ven-th-sort { cursor: pointer; user-select: none; white-space: nowrap; }
      .ven-th-sort:hover { background: #f0f5ff; }
      .ven-sort-arrow { color: #adb5bd; font-size:.75rem; margin-left:3px; }
      .ven-sort-arrow.active { color: #0a58ca; }
      /* Currency section header */
      .ven-currency-header { border-bottom: 2px solid #D6E0F0; padding-bottom:4px; margin-top:12px; }
      /* Group summary rows */
      .ven-group-row { cursor: pointer; transition: background .1s; }
      .ven-group-row:hover { background: #e8eef8 !important; }
      .ven-group-row.ven-group-all-selected { background: #d0e4ff !important; outline: 2px solid #0a58ca; outline-offset:-2px; }
      .ven-group-row.ven-group-some-selected { background: #e5efff !important; }
      /* Sub-rows: indent Parte cell */
      .ven-subrow td:nth-child(4) { padding-left: 26px !important; }
      /* Expand button inside group row */
      .ven-expand-btn { background:none; border:none; cursor:pointer; padding:0 3px;
                        font-size:0.68rem; color:#6c757d; line-height:1; vertical-align:middle; }
      .ven-expand-btn:hover { color:#0a58ca; }
      /* Selection bubble */
      #ven_sel_bubble {
        position: fixed; bottom: 22px; right: 22px; z-index: 9999;
        background: #fff; border: 2px solid #0a58ca; border-radius: 10px;
        padding: 9px 15px; box-shadow: 0 4px 18px rgba(10,88,202,.18);
        min-width: 155px; pointer-events: none;
      }
      .ven-bubble-count {
        font-size: 0.72rem; color: #6c757d; margin-bottom: 5px;
        border-bottom: 1px solid #dee2e6; padding-bottom: 4px;
      }
      .ven-bubble-line {
        display: flex; justify-content: space-between; gap: 10px;
        font-size: 0.83rem; font-weight: 600; margin-top: 3px;
      }
      .ven-bubble-cur { color: #6c757d; font-weight: 400; font-size: 0.72rem; }
      .ven-bubble-amt { font-variant-numeric: tabular-nums; }
      .ven-bubble-section-label {
        font-size: 0.65rem; font-weight: 700; text-transform: uppercase;
        letter-spacing: .05em; margin-top: 5px; margin-bottom: 1px;
      }
      .ven-bubble-sep { border-top: 1px solid #dee2e6; margin: 5px 0 2px; }
    ")),

    # Fixed selection bubble (shown whenever rows are selected)
    div(id = "ven_sel_bubble", style = "display:none;"),

    div(class = "vencidos-wrap p-3",

      # ── Top action + filter bar ──────────────────────────────────────────
      div(class = "d-flex flex-wrap gap-2 mb-2 align-items-end",
        tags$button(
          id      = "ven_edit_toggle",
          class   = "btn btn-outline-secondary btn-sm",
          onclick = "venToggleEdit()",
          "\u270f\ufe0f Editar"
        ),
        tags$button(
          id      = "ven_stage_all_btn",
          class   = "btn btn-outline-success btn-sm",
          onclick = "venStageAll()",
          "\U0001f6d2 Agregar todo"
        ),
        tags$button(
          id      = "ven_stage_sel_btn",
          class   = "btn btn-outline-success btn-sm",
          onclick = "venStageSelected()",
          "\uff0b Agregar selecci\u00f3n"
        ),
        tags$button(
          id      = "ven_sel_all_btn",
          class   = "btn btn-outline-secondary btn-sm",
          onclick = "venSelectAll()",
          "Seleccionar todos"
        ),
        tags$button(
          id      = "ven_sel_none_btn",
          class   = "btn btn-outline-secondary btn-sm",
          style   = "display:none;",
          onclick = "venSelectNone()",
          "Deseleccionar"
        ),
        # Filter controls — right-aligned
        div(class = "ms-auto d-flex flex-wrap gap-2 align-items-end",
          div(
            tags$label("Buscar", class = "form-label mb-0 small text-muted"),
            tags$input(id = "ven_search_text", type = "text",
                       class = "form-control form-control-sm",
                       style = "width:190px;",
                       placeholder = "Parte, ref., doc\u2026")
          ),
          div(
            tags$label("Tipo", class = "form-label mb-0 small text-muted"),
            tags$select(id = "ven_tipo", class = "form-select form-select-sm",
                        style = "min-width:110px;",
                        tags$option(value = "", "Todos"),
                        tags$option(value = "Cobro", "Cobros (AR)"),
                        tags$option(value = "Pago",  "Pagos (AP)"))
          ),
          div(
            tags$label("Etiqueta", class = "form-label mb-0 small text-muted"),
            tags$select(id = "ven_tag_filter", class = "form-select form-select-sm",
                        style = "min-width:130px;",
                        tags$option(value = "",          "Todas"),
                        tags$option(value = "tagged",    "Solo etiquetadas"),
                        tags$option(value = "urgent",    "\U0001f534 Urgente"),
                        tags$option(value = "important", "\U0001f7e1 Importante"),
                        tags$option(value = "both",      "\U0001f7e0 Ambas"))
          ),
          # Tiny utility buttons — group expand toggle + doc column toggle
          div(class = "d-flex gap-1 align-items-end pb-1",
            tags$button(
              id      = "ven_groups_btn",
              class   = "btn btn-outline-secondary btn-sm",
              style   = "padding:2px 6px; font-size:0.7rem; line-height:1.5;",
              onclick = "venToggleAllGroups()",
              title   = "Expandir / colapsar todos los grupos",
              HTML("&#9660;&#9660;")
            ),
            tags$button(
              id      = "ven_doc_toggle_btn",
              class   = "btn btn-outline-secondary btn-sm",
              style   = "padding:2px 6px; font-size:0.7rem; line-height:1.5;",
              onclick = "venToggleDoc()",
              title   = "Mostrar / ocultar columna Documento",
              "Documento"
            )
          )
        )
      ),

      # ── Edit toolbar (hidden until ✏ Editar) ────────────────────────────
      div(id    = "ven_edit_toolbar",
          class = "mb-2 p-2 border rounded bg-light",
          style = "display:none;",
        div(class = "d-flex flex-wrap gap-2 align-items-end",
          div(class = "d-flex gap-1",
            tags$button(class = "btn btn-sm btn-outline-danger",
                        onclick = "venAction('tag_urgent')",
                        "\U0001f534 Urgente"),
            tags$button(class = "btn btn-sm btn-outline-warning",
                        onclick = "venAction('tag_important')",
                        "\U0001f7e1 Importante"),
            tags$button(class   = "btn btn-sm",
                        style   = "border-color:#FF6B35;color:#FF6B35;",
                        onclick = "venAction('tag_both')",
                        "\U0001f7e0 Ambas"),
            tags$button(class = "btn btn-sm btn-outline-secondary",
                        onclick = "venAction('tag_clear')",
                        "\u2716 Quitar etiqueta")
          ),
          tags$div(class = "vr"),
          div(class = "d-flex gap-1 align-items-end",
            div(
              tags$label("Mover a:", class = "form-label mb-0 small text-muted"),
              tags$input(id = "ven_move_date", type = "date",
                         class = "form-control form-control-sm",
                         style = "width:145px;",
                         value = format(Sys.Date() + 1, "%Y-%m-%d"))
            ),
            tags$button(class = "btn btn-sm btn-primary",
                        onclick = "venAction('move')",
                        "\U0001f4c5 Mover"),
            tags$button(class = "btn btn-sm btn-outline-secondary",
                        onclick = "venAction('restore')",
                        "\u21a9 Restaurar fecha")
          ),
          tags$div(class = "vr"),
          tags$button(class = "btn btn-sm btn-outline-danger",
                      onclick = "venAction('delete')",
                      "\U0001f5d1 Eliminar")
        )
      ),

      # ── Stage confirmation bar ───────────────────────────────────────────
      # NOTE: no d-flex in class — Bootstrap d-flex uses !important and overrides
      # display:none. We set display:flex via JS when showing.
      div(id    = "ven_stage_confirm_bar",
          class = "alert alert-warning align-items-center gap-3 py-2 px-3 mb-2",
          style = "display:none;",
        tags$span(id = "ven_stage_confirm_msg", class = "flex-grow-1 small fw-bold"),
        tags$button(class = "btn btn-sm btn-success",
                    onclick = "venConfirmStage()",
                    "\u2713 Confirmar"),
        tags$button(class = "btn btn-sm btn-outline-secondary",
                    onclick = "venCancelStage()",
                    "Cancelar")
      ),

      # ── Dynamic table content ────────────────────────────────────────────
      uiOutput(ns("ven_table_ui"))
    ),

    # -- JavaScript (loaded from www/vencidos.js) -------------------------
    # ?v= query string forces browsers to reload after each edit.
    tags$script(src = "vencidos.js?v=6")
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

vencidosServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    # ── Build overdue data ─────────────────────────────────────────────────
    vencidos_df <- reactive({
      emp    <- shared$empresa_sel()
      raw    <- shared$sap_data()
      moves  <- shared$moves_db()
      manual <- shared$manual_inv()
      ic_reg <- shared$interco_v2()   # reactive dep — re-fires when IC codes load
      tags   <- shared$tags_db()
      today  <- Sys.Date()

      build_one <- function(ledger_name) {
        df <- tryCatch(
          build_ledger_df(
            raw_df    = raw[[ledger_name]],
            ledger    = ledger_name,
            empresa   = NULL,
            moves_df  = moves,
            manual_df = manual,
            abonos_df = shared$abonos_db()
          ),
          error = function(e) NULL
        )
        if (is.null(df) || !nrow(df)) return(NULL)

        # Empresa filter
        if (length(emp) && "Empresa" %in% names(df)) {
          df <- dplyr::filter(df, Empresa %in% emp)
          if (!nrow(df)) return(NULL)
        }

        # Mark confirmed (same two-source logic as ledger_module.R)
        tipo_val          <- if (ledger_name == "AR") "cobro" else "pago"
        df[["confirmed"]] <- FALSE

        conc_rv <- tryCatch(shared$conciliacion_rv(), error = function(e) NULL)
        if (!is.null(conc_rv) && nrow(conc_rv)) {
          ck <- unique(conc_rv[conc_rv[["tipo"]] == tipo_val,
                               c("Empresa","Moneda","Documento"), drop = FALSE])
          if (nrow(ck)) {
            mk <- paste(df[["Empresa"]], df[["Moneda"]], df[["Documento"]])
            df[["confirmed"]] <- df[["confirmed"]] | (mk %in% paste(ck$Empresa, ck$Moneda, ck$Documento))
          }
        }

        conf_db <- tryCatch(shared$bancos_confirmados(), error = function(e) NULL)
        if (!is.null(conf_db) && nrow(conf_db)) {
          ca <- conf_db[!isTRUE(conf_db[["eliminado"]]) & conf_db[["tipo"]] == tipo_val, , drop = FALSE]
          if (nrow(ca)) {
            bk <- unique(ca[, c("empresa","documento"), drop = FALSE])
            mk <- paste(df[["Empresa"]], df[["Documento"]])
            df[["confirmed"]] <- df[["confirmed"]] | (mk %in% paste(bk$empresa, bk$documento))
          }
        }

        # Drop confirmed entries
        if ("source" %in% names(df) && any(df[["confirmed"]] & df[["source"]] == "manual"))
          df <- df[!(df[["confirmed"]] & df[["source"]] == "manual"), , drop = FALSE]
        df <- df[!df[["confirmed"]], , drop = FALSE]
        if (!nrow(df)) return(NULL)

        # ── IC filter (two layers for robustness) ──────────────────────────

        # Layer 1: code-based filter (from registered interco_v2 registry)
        codes    <- build_ic_fullcodes(ic_reg, ledger_name)
        ic_rfcs  <- unique(toupper(trimws(unname(ic_reg$rfcs %||% character()))))
        ic_rfcs  <- ic_rfcs[nzchar(ic_rfcs)]
        code_col <- if (ledger_name == "AR") "C\u00f3digo de cliente" else "C\u00f3digo de proveedor"
        df <- apply_ic_filter(df, mode = "exclude",
                              code_col = code_col,
                              ic_codes = codes,
                              ic_rfcs  = ic_rfcs)
        if (!is.null(df) && !nrow(df)) return(NULL)

        # Layer 2: Parte-name fallback (catches IC when codes not yet registered)
        # If Parte matches any company's display name, the invoice is intercompany.
        if (!is.null(df) && nrow(df) && "Parte" %in% names(df)) {
          company_names_up <- toupper(unname(COMPANY_MAP))
          is_ic_by_name    <- toupper(trimws(df[["Parte"]])) %in% company_names_up
          if (any(is_ic_by_name)) {
            df <- df[!is_ic_by_name, , drop = FALSE]
            message("[VEN] IC-by-name filter removed ", sum(is_ic_by_name),
                    " rows for ledger=", ledger_name)
          }
        }
        if (is.null(df) || !nrow(df)) return(NULL)

        # Date filter: due today or overdue
        df <- df[!is.na(df[["FechaEff"]]) & as.Date(df[["FechaEff"]]) <= today, , drop = FALSE]
        if (!nrow(df)) return(NULL)

        # Tag labels
        df <- add_tag_labels(df, tags, ledger_name)

        df[["Ledger"]]  <- ledger_name
        df[["Tipo"]]    <- if (ledger_name == "AR") "Cobro" else "Pago"
        df[["Importe"]] <- abs(tidyr::replace_na(df[["Saldo vencido"]], 0))
        df[["Fecha"]]   <- as.Date(df[["FechaEff"]])
        df
      }

      ar <- tryCatch(build_one("AR"), error = function(e) { message("[VEN] AR error: ", e$message); NULL })
      ap <- tryCatch(build_one("AP"), error = function(e) { message("[VEN] AP error: ", e$message); NULL })
      dplyr::bind_rows(ar, ap)
    })

    # ── Badge ──────────────────────────────────────────────────────────────
    observe({
      df <- vencidos_df()
      n  <- if (!is.null(df) && nrow(df)) nrow(df) else 0L
      session$sendCustomMessage("vencidosBadge", list(count = n))
    })

    # ── Render table ───────────────────────────────────────────────────────
    output$ven_table_ui <- renderUI({
      df <- vencidos_df()

      if (is.null(df) || !nrow(df)) {
        return(div(
          class = "text-center text-muted py-5",
          tags$i(class = "fa fa-circle-check fa-2x mb-2 d-block", style = "color:#28a745;"),
          tags$p("Sin vencimientos pendientes \u2014 todo al d\u00eda", class = "mb-0 fw-semibold")
        ))
      }

      # Tag weight for default sort
      df[["tag_weight"]] <- dplyr::case_when(
        df[["Etiqueta"]] == "\U0001f7e0 Ambas"      ~ 1L,
        df[["Etiqueta"]] == "\U0001f534 Urgente"    ~ 2L,
        df[["Etiqueta"]] == "\U0001f7e1 Importante" ~ 3L,
        TRUE                                         ~ 4L
      )
      df <- df[order(df[["tag_weight"]], -df[["Importe"]]), ]

      disp_df <- data.frame(
        Ledger     = df[["Ledger"]],
        Tipo       = df[["Tipo"]],
        Empresa    = {
          e <- df[["Empresa"]]
          if (is.null(e)) e <- rep(NA_character_, nrow(df))
          e
        },
        Fecha      = format(df[["Fecha"]], "%d/%m/%Y"),
        Parte      = df[["Parte"]] %||% "",
        Documento  = df[["Documento"]] %||% "",
        Codigo     = if ("Codigo" %in% names(df)) df[["Codigo"]] %||% "" else "",
        Referencia = {
          fac <- if ("Factura" %in% names(df)) df[["Factura"]] else NA_character_
          replace(fac, is.na(fac), "")
        },
        Moneda     = df[["Moneda"]],
        Importe    = df[["Importe"]],
        Etiqueta   = df[["Etiqueta"]],
        tag_weight = df[["tag_weight"]],
        source     = df[["source"]] %||% "sap",
        inv_id     = if ("id" %in% names(df)) df[["id"]] %||% "" else "",
        stringsAsFactors = FALSE
      )

      # Fill missing Empresa.
      # Rows from old snapshots or manual entries may lack Empresa; without it
      # they cannot be staged (pagar_hoy_module filters per-empresa).
      # Strategy:
      #   1. Try the processed df first — SAP rows always carry Empresa there.
      #      Look up by Documento (exact match) within the same Ledger.
      #   2. Fall back to empresa_sel when it narrows to exactly one company.
      {
        blank <- !nzchar(disp_df[["Empresa"]], keepNA = FALSE)
        if (any(blank)) {
          # Pass 1: look up Empresa from other rows in the same processed df
          # that share the same Documento + Ledger. SAP rows always carry it;
          # only old-snapshot or manual rows may be missing it.
          valid_emp <- df[nzchar(df[["Empresa"]], keepNA = FALSE),
                         c("Documento","Ledger","Empresa"), drop = FALSE]
          for (i in which(blank)) {
            doc <- disp_df[["Documento"]][i]
            lgr <- disp_df[["Ledger"]][i]
            if (!nzchar(doc %||% "")) next
            hit <- valid_emp[
              !is.na(valid_emp[["Documento"]]) & valid_emp[["Documento"]] == doc &
              !is.na(valid_emp[["Ledger"]])    & valid_emp[["Ledger"]]    == lgr,
              "Empresa", drop = TRUE]
            if (length(hit) && nzchar(hit[1])) disp_df[["Empresa"]][i] <- hit[1]
          }
          # Pass 2: anything still blank — use empresa_sel if unambiguous
          still_blank <- !nzchar(disp_df[["Empresa"]], keepNA = FALSE)
          if (any(still_blank)) {
            emp_now <- shared$empresa_sel()
            if (length(emp_now) == 1L)
              disp_df[["Empresa"]][still_blank] <- emp_now[1L]
          }
        }
      }

      # Group key: collapse rows sharing Empresa + Date + Parte + Tipo + Moneda.
      # Empresa is required so invoices from different companies with the same
      # counterpart are never merged into a single group row.
      # Rows with blank Empresa get a unique per-row key so they stay standalone.
      disp_df[["group_key"]] <- dplyr::if_else(
        nzchar(disp_df[["Empresa"]], keepNA = FALSE),
        paste(disp_df[["Empresa"]], disp_df[["Fecha"]], disp_df[["Parte"]],
              disp_df[["Tipo"]],   disp_df[["Moneda"]], sep = "\x1f"),
        paste0("__solo__", seq_len(nrow(disp_df)))
      )
      gk_tab                  <- table(disp_df[["group_key"]])
      disp_df[["group_size"]] <- as.integer(gk_tab[disp_df[["group_key"]]])
      disp_df[["group_id"]]   <- match(disp_df[["group_key"]], unique(disp_df[["group_key"]]))

      currencies <- c(
        intersect("MXN", unique(disp_df[["Moneda"]])),
        sort(setdiff(unique(disp_df[["Moneda"]]), "MXN"))
      )

      # Sortable <th> helper
      make_th <- function(label, col) {
        tags$th(
          class      = "ven-th-sort",
          `data-col` = col,
          onclick    = paste0("venSortByCol('", col, "')"),
          label,
          tags$span(class = "ven-sort-arrow", "\u2195")
        )
      }

      make_table_section <- function(rows, ledger_name, tipo_label, cur) {
        if (!nrow(rows)) return(NULL)
        total     <- sum(rows[["Importe"]], na.rm = TRUE)
        badge_cls <- if (ledger_name == "AR") "badge bg-primary" else "badge bg-success"
        border_c  <- if (ledger_name == "AR") "#0a58ca" else "#198754"
        tbody_id  <- paste0("ven_tbody_", ledger_name, "_", cur)

        # ── Individual data row ──────────────────────────────────────────────
        make_data_tr <- function(row, extra_class = "", group_id_val = "", hidden = FALSE) {
          row_bg    <- switch(row[["Etiqueta"]],
            "\U0001f534 Urgente"    = "background:#fff0f0;",
            "\U0001f7e1 Importante" = "background:#fffbe6;",
            "\U0001f7e0 Ambas"      = "background:#fff3e6;",
            ""
          )
          full_style  <- paste0(row_bg, if (hidden) "display:none;" else "")
          etiq_html   <- if (nzchar(row[["Etiqueta"]])) htmltools::htmlEscape(row[["Etiqueta"]]) else ""
          ref_val     <- row[["Referencia"]] %||% ""
          empresa_val <- { ev <- row[["Empresa"]] %||% ""; if (is.na(ev)) "" else ev }

          paste0(
            '<tr class="ven-row', if (nzchar(extra_class)) paste0(" ", extra_class) else "", '"',
            if (nzchar(group_id_val)) paste0(' data-group-id="', group_id_val, '"') else "",
            ' data-tipo="',      htmltools::htmlEscape(row[["Tipo"]]),               '"',
            ' data-ledger="',    htmltools::htmlEscape(row[["Ledger"]]),             '"',
            ' data-moneda="',    htmltools::htmlEscape(row[["Moneda"]]),             '"',
            ' data-tag="',       htmltools::htmlEscape(row[["Etiqueta"]]),           '"',
            ' data-importe="',   as.character(row[["Importe"]]),                     '"',
            ' data-fecha="',     htmltools::htmlEscape(row[["Fecha"]]),              '"',
            ' data-parte="',     htmltools::htmlEscape(tolower(row[["Parte"]])),     '"',
            ' data-parteraw="',  htmltools::htmlEscape(row[["Parte"]]),              '"',
            ' data-doc="',       htmltools::htmlEscape(tolower(row[["Documento"]])), '"',
            ' data-ref="',       htmltools::htmlEscape(tolower(ref_val)),            '"',
            ' data-empresa="',   htmltools::htmlEscape(empresa_val),                '"',
            ' data-documento="', htmltools::htmlEscape(row[["Documento"]]),          '"',
            ' data-source="',    htmltools::htmlEscape(row[["source"]]),             '"',
            ' data-invid="',     htmltools::htmlEscape(row[["inv_id"]]),             '"',
            ' data-codigo="',    htmltools::htmlEscape(row[["Codigo"]] %||% ""),    '"',
            ' data-tagweight="', as.character(row[["tag_weight"]]),                  '"',
            if (nzchar(full_style)) paste0(' style="', full_style, '"') else "",
            '>',
            '<td><span class="', badge_cls, '">', htmltools::htmlEscape(row[["Tipo"]]), '</span></td>',
            '<td>', if (nzchar(empresa_val)) paste0('<span class="cart-empresa-badge">', htmltools::htmlEscape(empresa_val), '</span>') else "", '</td>',
            '<td style="white-space:nowrap;">', htmltools::htmlEscape(row[["Fecha"]]), '</td>',
            '<td style="max-width:200px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;">',
              htmltools::htmlEscape(row[["Parte"]]),
            '</td>',
            '<td class="text-muted small">', htmltools::htmlEscape(ref_val), '</td>',
            '<td class="text-muted small ven-doc-cell" style="display:none;">',
              htmltools::htmlEscape(row[["Documento"]]),
            '</td>',
            '<td class="text-end fw-bold" style="color:#1a6cc4;font-variant-numeric:tabular-nums;">',
              fmt_money(row[["Importe"]]),
            '</td>',
            '<td>', etiq_html, '</td>',
            '</tr>'
          )
        }

        # ── Group summary row ────────────────────────────────────────────────
        make_group_tr <- function(grp_rows, gid) {
          g_importe   <- sum(grp_rows[["Importe"]], na.rm = TRUE)
          g_tagweight <- min(grp_rows[["tag_weight"]], na.rm = TRUE)
          g_etiqueta  <- dplyr::case_when(
            g_tagweight == 1L ~ "\U0001f7e0 Ambas",
            g_tagweight == 2L ~ "\U0001f534 Urgente",
            g_tagweight == 3L ~ "\U0001f7e1 Importante",
            TRUE              ~ ""
          )
          row_bg   <- switch(g_etiqueta,
            "\U0001f534 Urgente"    = "background:#fff0f0;",
            "\U0001f7e1 Importante" = "background:#fffbe6;",
            "\U0001f7e0 Ambas"      = "background:#fff3e6;",
            "background:#f0f4fa;"
          )
          n_inv    <- nrow(grp_rows)
          g_tipo   <- grp_rows[["Tipo"]][1]
          g_fecha  <- grp_rows[["Fecha"]][1]
          g_parte  <- grp_rows[["Parte"]][1]
          g_moneda <- grp_rows[["Moneda"]][1]
          g_ledger <- grp_rows[["Ledger"]][1]
          # Show empresa if all sub-rows share the same one
          empresas  <- unique(grp_rows[["Empresa"]][nzchar(grp_rows[["Empresa"]] %||% "")])
          g_empresa <- if (length(empresas) == 1L) empresas else ""
          etiq_html <- if (nzchar(g_etiqueta)) htmltools::htmlEscape(g_etiqueta) else ""
          gid_str   <- as.character(gid)

          paste0(
            '<tr class="ven-group-row"',
            ' data-group-id="',  gid_str,                                    '"',
            ' data-tipo="',      htmltools::htmlEscape(g_tipo),               '"',
            ' data-ledger="',    htmltools::htmlEscape(g_ledger),             '"',
            ' data-moneda="',    htmltools::htmlEscape(g_moneda),             '"',
            ' data-tag="',       htmltools::htmlEscape(g_etiqueta),           '"',
            ' data-importe="',   as.character(g_importe),                     '"',
            ' data-fecha="',     htmltools::htmlEscape(g_fecha),              '"',
            ' data-parte="',     htmltools::htmlEscape(tolower(g_parte)),     '"',
            ' data-doc="" data-ref=""',
            ' data-tagweight="', as.character(g_tagweight),                   '"',
            ' data-expanded="false"',
            ' style="', row_bg, '"',
            '>',
            '<td><span class="', badge_cls, '">', htmltools::htmlEscape(g_tipo), '</span></td>',
            '<td>', if (nzchar(g_empresa)) paste0('<span class="cart-empresa-badge">', htmltools::htmlEscape(g_empresa), '</span>') else "", '</td>',
            '<td style="white-space:nowrap;">', htmltools::htmlEscape(g_fecha), '</td>',
            '<td style="white-space:nowrap; max-width:220px;">',
              '<button class="ven-expand-btn" onclick="event.stopPropagation();venExpandGroup(', gid_str, ')" title="Expandir/colapsar">&#9660;</button>',
              ' <span class="ven-group-count" style="font-size:0.72rem;color:#6c757d;margin-right:3px;">', n_inv, '</span>',
              '<span style="display:inline-block;max-width:155px;overflow:hidden;text-overflow:ellipsis;vertical-align:middle;white-space:nowrap;">',
                htmltools::htmlEscape(g_parte),
              '</span>',
            '</td>',
            '<td class="text-muted small"></td>',
            '<td class="text-muted small ven-doc-cell" style="display:none;"></td>',
            '<td class="text-end fw-bold" style="color:#1a6cc4;font-variant-numeric:tabular-nums;">',
              fmt_money(g_importe),
            '</td>',
            '<td>', etiq_html, '</td>',
            '</tr>'
          )
        }

        # ── Build HTML rows ──────────────────────────────────────────────────
        grouped_keys    <- names(which(table(rows[["group_key"]]) >= 2L))
        rows_html       <- character(0)
        seen_group_keys <- character(0)

        for (i in seq_len(nrow(rows))) {
          row <- rows[i, ]
          gk  <- row[["group_key"]]

          if (gk %in% grouped_keys) {
            if (gk %in% seen_group_keys) {
              next  # already emitted as part of group block
            } else {
              seen_group_keys <- c(seen_group_keys, gk)
              grp_rows <- rows[rows[["group_key"]] == gk, ]
              gid      <- grp_rows[["group_id"]][1]
              rows_html <- c(rows_html, make_group_tr(grp_rows, gid))
              for (j in seq_len(nrow(grp_rows))) {
                rows_html <- c(rows_html,
                  make_data_tr(grp_rows[j, ],
                    extra_class  = "ven-subrow",
                    group_id_val = as.character(gid),
                    hidden       = TRUE))
              }
            }
          } else {
            rows_html <- c(rows_html, make_data_tr(row))
          }
        }

        div(class = "ven-ledger-section mb-3",
          `data-ledger` = ledger_name, `data-moneda` = cur,
          div(class = "d-flex align-items-center gap-2 px-2 py-1 mb-1",
              style = paste0("background:#f1f5fb; border-left:3px solid ", border_c,
                             "; border-radius:0 4px 4px 0;"),
            tags$span(class = badge_cls, tipo_label),
            tags$span(class = "small text-muted",
                      paste(nrow(rows), "factura", if (nrow(rows) != 1) "s" else "")),
            tags$span(class = "ms-auto fw-semibold",
                      style = "font-size:.9rem; color:#0B2038;",
                      fmt_money(total), " ", cur)
          ),
          div(style = "overflow-x:auto;",
            tags$table(
              class = "table table-sm table-hover mb-1",
              tags$thead(tags$tr(
                make_th("Tipo",        "tipo"),
                make_th("Empresa",     "empresa"),
                make_th("Fecha venc.", "fecha"),
                make_th("Parte",       "parte"),
                make_th("Referencia",  "referencia"),
                tags$th(
                  class      = "ven-doc-th ven-th-sort",
                  `data-col` = "documento",
                  onclick    = "venSortByCol('documento')",
                  style      = "display:none;",
                  "Documento ", tags$span(class = "ven-sort-arrow", "\u2195")
                ),
                tags$th(class = "ven-th-sort text-end", `data-col` = "importe",
                        onclick = "venSortByCol('importe')",
                        "Importe ", tags$span(class = "ven-sort-arrow", "\u2195")),
                make_th("Etiqueta", "etiqueta")
              )),
              tags$tbody(
                id    = tbody_id, class = "ven-tbody",
                `data-ledger` = ledger_name, `data-moneda` = cur,
                HTML(paste(rows_html, collapse = "\n"))
              )
            )
          )
        )
      }

      cur_sections <- lapply(currencies, function(cur) {
        cur_df    <- disp_df[disp_df[["Moneda"]] == cur, ]
        ar_rows   <- cur_df[cur_df[["Ledger"]] == "AR", ]
        ap_rows   <- cur_df[cur_df[["Ledger"]] == "AP", ]
        cur_total <- sum(cur_df[["Importe"]], na.rm = TRUE)
        cur_n     <- nrow(cur_df)

        div(class = "ven-cur-section mb-2", `data-moneda` = cur,
          div(class = "ven-currency-header d-flex align-items-center gap-2 mb-2",
            tags$h5(class = "mb-0 fw-bold", style = "color:#0B2038; font-size:1rem;", cur),
            tags$span(class = "text-muted small",
                      paste0(cur_n, " factura", if (cur_n != 1) "s" else "")),
            tags$span(class = "ms-auto fw-bold",
                      style = "font-size:1rem; color:#0A58CA;",
                      fmt_money(cur_total))
          ),
          make_table_section(ar_rows, "AR", "Cobros", cur),
          make_table_section(ap_rows, "AP", "Pagos",  cur)
        )
      })

      total_n <- nrow(disp_df)
      tagList(
        div(id    = "ven_count",
            class = "small text-muted mb-3",
            paste0(total_n, " factura", if (total_n != 1) "s" else "",
                   " vencida", if (total_n != 1) "s" else "",
                   " o con vencimiento hoy")),
        tagList(cur_sections),
        tags$script(HTML("
          setTimeout(function(){
            if(window.venFilterAndSort) venFilterAndSort();
          }, 50);
        "))
      )
    })
  })
}
