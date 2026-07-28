# =============================================================================
# R/staging_browse_module.R
# Manual entry modal for Cobro (CxC) and Pago (CxP).
# Also contains the Abono Parcial modal (partial payment recording).
#
# Public API:
#   show_combined_entry_modal(ledger, sap_data, session)
#     — call from pick_ar / pick_ap observers in app.R (after removeModal())
#
#   show_abono_modal(sap_data, session)
#     — call from pick_abono observer in app.R (after removeModal())
#
#   setup_abono_browse(input, output, session, sap_data, abonos_db, pagar_hoy_db, current_user, client_id = NULL)
#     — call ONCE inside the server function, at startup
# =============================================================================

# ── Internal CSS (shared by manual entry and abono modals) ────────────────────
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

# ── Show the manual entry modal ───────────────────────────────────────────────
show_combined_entry_modal <- function(ledger, sap_data, session,
                                      empresa_vals = unname(COMPANY_MAP)) {
  # Use empresa_vals (from empresa_sel_rv) as the authoritative source.
  # This prevents stale SAP snapshot Empresa names (generated with an older COMPANY_MAP)
  # from appearing as choices; picking a stale name would cause the manual entry to be
  # silently filtered out by the empresa toggle in the calendar.
  empresas  <- sort(unique(c(empresa_vals)))
  lbl_title <- if (ledger == "AR") "Cobro — CxC" else "Pago — CxP"

  showModal(modalDialog(
    title     = lbl_title,
    size      = "l",
    easyClose = TRUE,
    footer    = tagList(
      actionButton("me_cancel", "Cancelar", class = "btn btn-secondary"),
      actionButton("me_save",   "Guardar",  class = "btn btn-primary"),
      if (ledger == "AP")
        actionButton("me_save_as_provision", "Guardar como provisión",
                     style = "background-color:#7c3aed;color:white;border:none;")
      else NULL
    ),

    tags$style(HTML(.sb_css)),

    manual_entry_tab_content(ledger, empresas)
  ))
}

# =============================================================================
# Abono Parcial — partial payment recording
# Rows are staged to Agenda de Hoy (pagar_hoy) as tipo_item="abono"/status="pending".
# Calendar balance deduction only takes effect after explicit "Confirmar pagos"
# in Agenda de Hoy, which writes confirmed rows to abonos_db with status="active".
# =============================================================================

.ab_css <- "
.ab-group-row       { cursor:pointer; background:#f0f4fa; }
.ab-group-row:hover { background:#e8eef8 !important; }
.ab-expand-btn  { background:none; border:none; cursor:pointer; padding:0 4px;
                  font-size:0.68rem; color:#6c757d; line-height:1; vertical-align:middle; }
.ab-expand-btn:hover { color:#0a58ca; }
.ab-ref  { font-weight:500; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; max-width:280px; }
.ab-doc  { font-size:0.78rem; color:#6c757d; margin-top:1px; }
.ab-amt  { text-align:right; }
.ab-amt.ab-warn { border-color:#ffc107 !important; background:#fffbf0 !important; }
.ab-amt:disabled { background:#f8f9fa !important; color:#adb5bd; cursor:not-allowed; }
.ab-warn-msg { font-size:0.72em; color:#856404; margin-top:2px; display:none; }
.ab-amt.ab-warn ~ .ab-warn-msg { display:block; }
.ab-empty { color:#6c757d; font-style:italic; padding:20px 0; text-align:center; }
"

.ab_js <- "
(function() {
  function abMatchesSearch(tr) {
    var q = ((document.getElementById('ab_search') || {}).value || '').toLowerCase().trim();
    if (!q) return true;
    var d = tr.dataset;
    return (d.parte || '').includes(q) ||
           (d.ref   || '').includes(q) ||
           (d.doc   || '').includes(q);
  }

  function abApplySearch() {
    var groupHasMatch = {};
    document.querySelectorAll('.ab-subrow').forEach(function(r) {
      var gid = r.dataset.gid;
      var gr  = gid ? document.querySelector('.ab-group-row[data-gid=\"' + gid + '\"]') : null;
      var m   = abMatchesSearch(r);
      if (gid) { if (!groupHasMatch[gid]) groupHasMatch[gid] = false; if (m) groupHasMatch[gid] = true; }
      r.style.display = (m && gr && gr.dataset.expanded === 'true') ? '' : 'none';
    });
    document.querySelectorAll('.ab-group-row').forEach(function(gr) {
      gr.style.display = groupHasMatch[gr.dataset.gid] ? '' : 'none';
    });
    document.querySelectorAll('.ab-standalone').forEach(function(r) {
      r.style.display = abMatchesSearch(r) ? '' : 'none';
    });
  }

  $(document).off('input.absearch').on('input.absearch', '#ab_search', abApplySearch);

  window.abExpandGroup = function(gid) {
    gid = String(gid);
    var gr = document.querySelector('.ab-group-row[data-gid=\"' + gid + '\"]');
    if (!gr) return;
    var exp = gr.dataset.expanded === 'true';
    gr.dataset.expanded = String(!exp);
    var btn = gr.querySelector('.ab-expand-btn');
    if (btn) btn.innerHTML = !exp ? '&#9650;' : '&#9660;';
    document.querySelectorAll('.ab-subrow[data-gid=\"' + gid + '\"]').forEach(function(r) {
      r.style.display = (!exp && abMatchesSearch(r)) ? '' : 'none';
    });
  };

  var _abAllExp = true;
  window.abToggleAllGroups = function() {
    _abAllExp = !_abAllExp;
    document.querySelectorAll('.ab-group-row').forEach(function(gr) {
      gr.dataset.expanded = String(_abAllExp);
      var btn = gr.querySelector('.ab-expand-btn');
      if (btn) btn.innerHTML = _abAllExp ? '&#9650;' : '&#9660;';
      var gid = gr.dataset.gid;
      document.querySelectorAll('.ab-subrow[data-gid=\"' + gid + '\"]').forEach(function(r) {
        r.style.display = (_abAllExp && abMatchesSearch(r)) ? '' : 'none';
      });
    });
    var b = document.getElementById('ab_toggle_btn');
    if (b) b.textContent = _abAllExp ? '▲▲ Colapsar' : '▼▼ Expandir';
  };

  $(document).off('change.abgroupcheck').on('change.abgroupcheck', '.ab-group-check', function() {
    var gid = String($(this).data('gid'));
    var chk = this.checked;
    $('.ab-check[data-gid=\"' + gid + '\"]').each(function() {
      this.checked = chk;
      var idx = $(this).data('idx');
      var $a  = $('.ab-amt[data-idx=\"' + idx + '\"]');
      chk ? $a.prop('disabled', false) : $a.prop('disabled', true).removeClass('ab-warn');
    });
  });

  $(document).off('change.abcheck').on('change.abcheck', '.ab-check', function() {
    var idx = $(this).data('idx');
    var $a  = $('.ab-amt[data-idx=\"' + idx + '\"]');
    this.checked ? $a.prop('disabled', false) : $a.prop('disabled', true).removeClass('ab-warn');
    var gid = $(this).data('gid');
    if (gid !== undefined && gid !== '') {
      var checks = $('.ab-check[data-gid=\"' + gid + '\"]');
      var all = checks.toArray().every(function(c) { return c.checked; });
      var any = checks.toArray().some(function(c) { return c.checked; });
      var gc  = $('.ab-group-check[data-gid=\"' + gid + '\"]')[0];
      if (gc) { gc.checked = all; gc.indeterminate = !all && any; }
    }
  });

  $(document).off('input.abamt').on('input.abamt', '.ab-amt', function() {
    var max = parseFloat($(this).data('max')) || 0;
    var val = parseFloat($(this).val())       || 0;
    $(this).toggleClass('ab-warn', val > max);
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
        referencia: String($(this).data('ref') || ''),
        fecha_venc: String($(this).data('fecha-venc')),
        saldo:      parseFloat($(this).data('saldo')) || 0,
        importe:    parseFloat($('.ab-amt[data-idx=\"' + idx + '\"]').val()) || 0
      });
    });
    if (!rows.length) { alert('Selecciona al menos una factura.'); return; }
    Shiny.setInputValue('ab_rows', rows, {priority: 'event'});
  });
})();
"

# Build a group header <tr>
.ab_group_tr <- function(gid, grp_df) {
  n       <- nrow(grp_df)
  parte   <- grp_df$Parte[1]   %||% ""
  empresa <- grp_df$Empresa[1] %||% ""
  moneda  <- grp_df$Moneda[1]  %||% ""
  total   <- sum(grp_df[["Saldo vencido"]], na.rm = TRUE)
  total_f <- formatC(total, format = "f", digits = 0, big.mark = ",")

  paste0(
    '<tr class="ab-group-row" data-gid="', gid, '" data-expanded="true">',
    '<td style="width:28px;vertical-align:middle;">',
      '<input type="checkbox" class="ab-group-check form-check-input" data-gid="', gid, '">',
    '</td>',
    '<td style="vertical-align:middle;" colspan="2">',
      '<button class="ab-expand-btn" onclick="event.stopPropagation();abExpandGroup(',
        gid, ')" title="Expandir/colapsar">&#9650;</button>',
      ' <strong style="font-size:0.9rem;">', htmltools::htmlEscape(parte), '</strong>',
      ' <span class="badge bg-secondary ms-1" style="font-size:0.65rem;">', n, '</span>',
      if (nzchar(empresa))
        paste0(' <span class="badge bg-light text-dark border ms-1" style="font-size:0.65rem;">',
               htmltools::htmlEscape(empresa), '</span>')
      else '',
    '</td>',
    '<td class="text-end text-muted small" style="vertical-align:middle;white-space:nowrap;">',
      htmltools::htmlEscape(moneda), '&nbsp;', htmltools::htmlEscape(total_f),
    '</td>',
    '<td></td>',
    '</tr>'
  )
}

# Build one invoice <tr> — grouped (hidden subrow) or standalone (visible)
.ab_row_tr <- function(i, row, ledger, gid = NULL) {
  codigo_col <- if (ledger == "AR") "Código de cliente" else "Código de proveedor"
  codigo_v   <- if (codigo_col %in% names(row)) as.character(row[[codigo_col]] %||% "") else ""
  saldo_val  <- row[["Saldo vencido"]]
  fecha_str  <- format(row[["Fecha de vencimiento"]], "%d/%m/%y")
  saldo_fmt  <- formatC(saldo_val, format = "f", digits = 0, big.mark = ",")
  mon        <- as.character(row$Moneda    %||% "")
  parte_raw  <- as.character(row$Parte     %||% "")
  empresa_v  <- as.character(row$Empresa   %||% "")
  doc_val    <- as.character(row$Documento %||% "")
  ref_val    <- if ("Factura" %in% names(row)) as.character(row$Factura %||% "") else ""
  if (!nzchar(ref_val)) ref_val <- doc_val

  is_grouped <- !is.null(gid)
  row_class  <- if (is_grouped) "ab-subrow" else "ab-standalone"
  gid_attr   <- if (is_grouped) paste0(' data-gid="', gid, '"') else ""
  hidden_sty <- ""

  paste0(
    '<tr class="', row_class, '"',
    gid_attr,
    ' data-idx="',   i,                                          '"',
    ' data-parte="', htmltools::htmlEscape(tolower(parte_raw)),  '"',
    ' data-ref="',   htmltools::htmlEscape(tolower(ref_val)),    '"',
    ' data-doc="',   htmltools::htmlEscape(tolower(doc_val)),    '"',
    hidden_sty, '>',
    # Checkbox
    '<td style="width:28px;vertical-align:middle;">',
      '<input type="checkbox" class="ab-check form-check-input"',
      ' data-idx="',        i,                                              '"',
      if (is_grouped) paste0(' data-gid="', gid, '"') else '',
      ' data-empresa="',    htmltools::htmlEscape(empresa_v),               '"',
      ' data-moneda="',     htmltools::htmlEscape(mon),                     '"',
      ' data-documento="',  htmltools::htmlEscape(doc_val),                 '"',
      ' data-parte="',      htmltools::htmlEscape(parte_raw),               '"',
      ' data-codigo="',     htmltools::htmlEscape(codigo_v),                '"',
      ' data-fecha-venc="', as.character(row[["Fecha de vencimiento"]]),    '"',
      ' data-saldo="',      saldo_val,                                      '"',
      ' data-ref="',        htmltools::htmlEscape(ref_val),                 '"',
      '>',
    '</td>',
    # Referencia / Documento
    '<td style="vertical-align:middle;max-width:300px;">',
      '<div class="ab-ref">', htmltools::htmlEscape(ref_val), '</div>',
      if (ref_val != doc_val)
        paste0('<div class="ab-doc">Doc&nbsp;', htmltools::htmlEscape(doc_val), '</div>')
      else '',
      if (!is_grouped && nzchar(empresa_v))
        paste0('<div class="ab-doc">', htmltools::htmlEscape(empresa_v), '</div>')
      else '',
    '</td>',
    # Vence
    '<td class="text-nowrap text-muted small" style="width:65px;vertical-align:middle;">',
      htmltools::htmlEscape(fecha_str),
    '</td>',
    # Saldo
    '<td class="text-end" style="width:110px;vertical-align:middle;white-space:nowrap;">',
      '<span class="text-muted small">', htmltools::htmlEscape(mon), '&nbsp;</span>',
      '<strong style="font-variant-numeric:tabular-nums;">',
        htmltools::htmlEscape(saldo_fmt),
      '</strong>',
    '</td>',
    # Amount input
    '<td style="width:130px;vertical-align:middle;">',
      '<input type="number" class="form-control form-control-sm ab-amt"',
      ' data-idx="', i,         '"',
      ' data-max="', saldo_val, '"',
      ' value="',    saldo_val, '"',
      # Disabled until its checkbox is checked -- the change.abcheck handler
      # (.ab_js) already enables/disables it on toggle, but only reacts to a
      # CHANGE event, so the initial render must start disabled to match the
      # unchecked default (found live 2026-07-27: every amount field was
      # editable from the moment the modal opened, regardless of selection).
      ' disabled',
      ' min="0" step="0.01">',
      '<div class="ab-warn-msg">Mayor al saldo (', htmltools::htmlEscape(saldo_fmt), ')</div>',
    '</td>',
    '</tr>'
  )
}

# Normalize the client's checked-rows payload to a list-of-lists, one entry
# per row, regardless of how many rows were checked. jsonlite/Shiny
# simplifies a JSON array of objects differently depending on its length --
# see the observeEvent(input$ab_rows) call site for the exact shapes and the
# live incident this fixes (2026-07-27), matching the same normalization
# R/ledger_module.R's .cart_inv_sel_to_keys() already needed for cart_inv_sel.
.normalize_ab_rows <- function(rows_data) {
  if (is.null(rows_data)) return(list())
  if (is.data.frame(rows_data)) {
    return(lapply(seq_len(nrow(rows_data)), function(k) as.list(rows_data[k, , drop = FALSE])))
  }
  if (is.list(rows_data) && !is.null(rows_data[["idx"]])) {
    n <- length(rows_data[["idx"]])
    if (n > 1L) {
      return(lapply(seq_len(n), function(k) lapply(rows_data, function(v) v[[k]])))
    }
    return(list(rows_data))
  }
  # (e) a single checked row with only scalar (never nested) fields can also
  # unbox to a plain NAMED ATOMIC VECTOR rather than a named list -- is.list()
  # is FALSE for this shape even though `[["idx"]]` still resolves. Found live
  # 2026-07-27 (session #6, ~30 min in): crashed with "$ operator is invalid
  # for atomic vectors" -- the same for-loop-over-field-values failure as
  # shape (c) above, just via a type is.list() doesn't catch. as.list()
  # converts it to the same named-list-of-one shape (c) already produces.
  if (is.atomic(rows_data) && !is.null(names(rows_data)) && !is.null(rows_data[["idx"]])) {
    return(list(as.list(rows_data)))
  }
  rows_data
}

# ── Show the abono modal ──────────────────────────────────────────────────────
show_abono_modal <- function(sap_data, session) {
  today <- Sys.Date()

  showModal(modalDialog(
    title     = "Abono parcial",
    size      = "xl",
    easyClose = TRUE,
    footer    = modalButton("Cerrar"),

    tags$style(HTML(paste0(.sb_css, .ab_css))),

    # ── Filter bar ─────────────────────────────────────────────────────────
    div(class = "d-flex flex-wrap gap-2 mb-2 align-items-end",
      div(style = "min-width:200px; flex:1;",
        tags$label("Buscar (parte, referencia, documento)",
                   class = "form-label mb-0 small text-muted"),
        tags$input(id = "ab_search", type = "text",
                   class = "form-control form-control-sm",
                   placeholder = "Escribe para filtrar...")
      ),
      div(
        tags$label("Tipo", class = "form-label mb-0 small text-muted"),
        selectInput("ab_ledger", NULL,
                    choices  = c("CxP (AP)" = "AP", "CxC (AR)" = "AR"),
                    selected = "AP", width = "115px")
      ),
      div(
        tags$label("Desde", class = "form-label mb-0 small text-muted"),
        dateInput("ab_desde", NULL,
                  value = today - 15, weekstart = 1, language = "es",
                  width = "130px")
      ),
      div(
        tags$label("Hasta", class = "form-label mb-0 small text-muted"),
        dateInput("ab_hasta", NULL,
                  value = today + 15, weekstart = 1, language = "es",
                  width = "130px")
      ),
      div(
        tags$label("Empresa", class = "form-label mb-0 small text-muted"),
        selectInput("ab_empresa", NULL,
                    choices  = c("(Todas)" = "",
                                 sort(unique(unname(COMPANY_MAP)))),
                    selected = "", width = "200px")
      ),
      div(
        tags$label("Moneda", class = "form-label mb-0 small text-muted"),
        selectInput("ab_moneda", NULL,
                    choices  = c("(Todas)" = "", CURRENCIES),
                    selected = "", width = "100px")
      ),
      tags$button(
        id      = "ab_toggle_btn",
        class   = "btn btn-outline-secondary btn-sm align-self-end",
        onclick = "abToggleAllGroups()",
        "▲▲ Colapsar"
      )
    ),

    # ── Table ───────────────────────────────────────────────────────────────
    div(style = "overflow-x:auto; max-height:430px; overflow-y:auto;",
      uiOutput("ab_table")
    ),

    # ── Footer ──────────────────────────────────────────────────────────────
    div(class = "mt-3 d-flex justify-content-between align-items-center",
      div(
        tags$span(class = "badge bg-warning text-dark", "⊟ Abono parcial"),
        tags$small(class = "text-muted ms-2",
                   "Se envía a Agenda de hoy. El saldo se reduce al confirmar.")
      ),
      actionButton("ab_stage", "Enviar a Agenda",
                   class = "btn btn-primary")
    ),

    tags$script(HTML(.ab_js))
  ))
}

# ── Server-side setup — call ONCE inside server() ────────────────────────────
setup_abono_browse <- function(input, output, session,
                                sap_data, abonos_db, pagar_hoy_db, current_user,
                                client_id = NULL) {

  output$ab_table <- renderUI({
    ledger <- input$ab_ledger
    if (is.null(ledger)) return(NULL)
    desde <- input$ab_desde
    hasta <- input$ab_hasta
    if (is.null(desde) || is.null(hasta)) return(NULL)

    df_src <- if (ledger == "AR") sap_data()$AR else sap_data()$AP
    if (is.null(df_src) || !nrow(df_src))
      return(div(class = "ab-empty", "Sin datos SAP para este tipo de ledger."))

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
      return(div(class = "ab-empty", "Sin facturas en ese rango."))

    # Net out already-CONFIRMED abonos before showing "Saldo" -- otherwise this
    # modal always shows the invoice's original SAP balance, forever, even
    # after a partial payment against it was confirmed (found 2026-07-24: the
    # `abonos_db` parameter was accepted but never actually read here). Mirrors
    # build_ledger_df()'s exact join (R/data_pipeline.R) so this modal agrees
    # with what Calendario itself would show. Only ACTIVE (confirmed) abonos
    # count -- a still-pending, unconfirmed abono staged in Agenda deliberately
    # does not reduce the balance yet, matching the documented design above.
    ab_summary <- active_abonos_summary(abonos_db() %||% tibble::tibble()) |>
      dplyr::filter(.data$ledger == !!ledger)
    if (nrow(ab_summary) > 0) {
      df <- df |>
        dplyr::left_join(
          ab_summary |> dplyr::select(Empresa, Moneda, Documento, abono_total),
          by = c("Empresa", "Moneda", "Documento")
        ) |>
        dplyr::mutate(
          abono_total       = dplyr::coalesce(abono_total, 0),
          `Saldo vencido`   = pmax(0, `Saldo vencido` - abono_total)
        )
    } else {
      df[["abono_total"]] <- 0
    }

    # An invoice already fully covered by confirmed abonos has nothing left to
    # partially pay -- drop it instead of showing a stale/zero row.
    df <- df[df[["Saldo vencido"]] > 0, , drop = FALSE]
    if (!nrow(df))
      return(div(class = "ab-empty", "Sin facturas con saldo pendiente en ese rango."))

    # ── Group by Parte + Empresa + Moneda ─────────────────────────────────
    df[["group_key"]] <- paste(df$Parte %||% "", df$Empresa %||% "",
                               df$Moneda %||% "", sep = "\x1f")
    gk_sizes <- table(df[["group_key"]])
    gk_ids   <- match(df[["group_key"]], unique(df[["group_key"]]))

    rows_html       <- character(0)
    seen_group_keys <- character(0)

    for (i in seq_len(nrow(df))) {
      row <- df[i, ]
      gk  <- row[["group_key"]]
      gid <- gk_ids[i]

      if (gk_sizes[[gk]] >= 2L) {
        if (!(gk %in% seen_group_keys)) {
          seen_group_keys <- c(seen_group_keys, gk)
          grp_idx  <- which(df[["group_key"]] == gk)
          grp_rows <- df[grp_idx, ]
          rows_html <- c(rows_html, .ab_group_tr(gid, grp_rows))
          for (j in seq_along(grp_idx)) {
            rows_html <- c(rows_html,
              .ab_row_tr(grp_idx[j], grp_rows[j, ], ledger, gid = gid))
          }
        }
      } else {
        rows_html <- c(rows_html, .ab_row_tr(i, row, ledger, gid = NULL))
      }
    }

    HTML(paste0(
      '<table class="table table-sm table-hover mb-0">',
      '<thead><tr>',
        '<th style="width:28px;"></th>',
        '<th>Referencia / Documento</th>',
        '<th style="width:65px;">Vence</th>',
        '<th class="text-end" style="width:110px;">Saldo</th>',
        '<th style="width:130px;">Importe abono</th>',
      '</tr></thead>',
      '<tbody>', paste(rows_html, collapse = "\n"), '</tbody>',
      '</table>'
    ))
  })

  # Stage abono rows to pagar_hoy ONLY — no immediate abonos_db write.
  # Calendar reduction takes effect only when "Confirmar pagos" in Agenda de Hoy
  # writes confirmed rows to abonos_db with status = "active".
  observeEvent(input$ab_rows, {
   tryCatch({
    # Normalize to list-of-lists. Shiny/jsonlite delivers the client's JSON
    # array of row objects in different shapes depending on how many rows
    # were checked -- the exact same failure mode already documented and
    # fixed for cart_inv_sel (R/ledger_module.R's .cart_inv_sel_to_keys()):
    #   (a) data.frame        — jsonlite simplifyDataFrame kicked in (2+ rows)
    #   (b) list(idx=vec,...) — named list of vectors (2+ rows)
    #   (c) list(idx=x, ...)  — single row, auto-unboxed (found live
    #       2026-07-27: checking exactly one row and clicking "Enviar a
    #       Agenda" silently did nothing -- `for (r_raw in rows_data)` was
    #       iterating over this row's individual FIELD VALUES instead of
    #       over rows, so `r_raw$saldo` errored on a plain scalar)
    #   (d) list(list(...), ...) — already correct
    #   (e) a named atomic vector — same failure as (c), see .normalize_ab_rows()
    rows_data <- .normalize_ab_rows(input$ab_rows)
    if (!length(rows_data)) return()

    # Defense in depth: even after (a)-(e) above, don't trust the shape
    # blindly -- drop (not crash on) any entry that still isn't list-like,
    # so a jsonlite/Shiny unboxing variant we haven't seen yet degrades to
    # "that one row didn't stage" instead of killing the whole session
    # (found live 2026-07-27: an unhandled shape crashed the entire app,
    # not just this feature).
    malformed <- !vapply(rows_data, is.list, logical(1))
    if (any(malformed)) {
      warning("[AB_ROWS] ", sum(malformed), " row(s) in an unrecognized shape, dropped: ",
              paste(utils::capture.output(str(input$ab_rows)), collapse = " | "))
      rows_data <- rows_data[!malformed]
    }
    if (!length(rows_data)) {
      showNotification("No se pudo leer la selección de abonos. Intenta de nuevo.",
                        type = "error", duration = 6)
      return()
    }

    ledger <- input$ab_ledger %||% "AP"
    # Read fresh from S3 first (safe_load_pagar_hoy -- mode-aware, found
    # 2026-07-25); a stale base here would silently drop another session's
    # concurrent stage/unstage.
    ph     <- tryCatch(safe_load_pagar_hoy(username = current_user(), client_id = client_id()),
                       error = function(e) NULL) %||% pagar_hoy_db()

    # The client-side warning class (.ab-warn) is cosmetic only -- nothing
    # ever stopped submitting an abono larger than the invoice's remaining
    # balance (found 2026-07-24). `r_raw$saldo` is the balance shown in the
    # table at render time, already netted against confirmed abonos by the
    # fix above, so this guard is consistent with what the user actually saw.
    accepted <- list()
    rejected <- character(0)
    for (r_raw in rows_data) {
      saldo   <- as.numeric(r_raw$saldo   %||% 0)
      importe <- as.numeric(r_raw$importe %||% 0)
      if (importe <= 0 || importe > saldo + 1e-6) {
        rejected <- c(rejected, r_raw$referencia %||% r_raw$documento %||% "")
        next
      }
      accepted[[length(accepted) + 1L]] <- r_raw
    }

    if (length(accepted)) {
      for (r_raw in accepted) {
        new_row <- tibble::tibble(
          id        = uuid::UUIDgenerate(),
          ledger    = ledger,
          Empresa   = as.character(r_raw$empresa   %||% ""),
          Moneda    = as.character(r_raw$moneda    %||% "MXN"),
          Documento = as.character(r_raw$documento %||% ""),
          Parte     = as.character(r_raw$parte     %||% ""),
          Codigo    = trimws(as.character(r_raw$codigo %||% "")),
          tipo_item = "abono",
          Importe   = as.numeric(r_raw$importe %||% 0),
          # The invoice's real due date, not the staging date (found
          # 2026-07-24: this always showed "today" in Agenda de Hoy,
          # misleading next to real factura rows in the same table).
          FechaVenc = tryCatch(as.Date(r_raw$fecha_venc), error = function(e) Sys.Date()),
          staged_by = current_user(),
          staged_at = Sys.time(),
          status    = "pending",
          source    = "manual"
        )
        ph <- upsert_pagar_hoy(ph, new_row, keys = "id")
      }

      pagar_hoy_db(ph)
      tryCatch(save_pagar_hoy(ph, current_user(), client_id = client_id()),
               error = function(e) showNotification(
                 paste("No se pudo guardar en Agenda:", e$message), type = "warning"))

      n <- length(accepted)
      showNotification(
        paste0(n, if (n == 1L) " abono enviado" else " abonos enviados",
               " a Agenda de hoy. Confirma para aplicar al saldo."),
        type = "message", duration = 4
      )
    }

    if (length(rejected)) {
      showNotification(
        paste0("No se envi", if (length(rejected) == 1L) "ó 1 abono" else paste0("aron ", length(rejected), " abonos"),
               " por exceder el saldo disponible: ", paste(rejected, collapse = ", ")),
        type = "warning", duration = 6
      )
    }

    if (length(accepted)) removeModal()
   }, error = function(e) {
     warning("[AB_ROWS] observer failed: ", conditionMessage(e))
     showNotification("Ocurrió un error al enviar los abonos a Agenda. Intenta de nuevo; si persiste, avisa a soporte.",
                       type = "error", duration = 8)
   })
  }, ignoreInit = TRUE)
}
