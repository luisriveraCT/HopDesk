# =============================================================================
# R/cashflow_preview_module.R
# In-app Cash Flow Preview panel — slides in alongside the Calendar.
#
# Stage 1 : Animation shell — arrow toggles panels, placeholder content.
# Stage 2 : Preview controls toolbar (date range, grouping, currency, FX).
# Stage 3 : Data pipeline wiring (build_export_combined_df + build_cashflow_export_data).
# Stage 4 : HTML table renderer (collapsed/expanded, tags, sticky columns).
# Stage 5 : Export shortcut — pre-fills the export modal from preview settings.
# Stage 6 : Polish (Esc key, responsive, performance, WCAG).
# =============================================================================

# ── Stage 4: HTML table renderer ─────────────────────────────────────────────

# Compact formatter: "" for 0, "$4.5M", "$802K", "$4,321"
.cf_fmt <- function(x, show_zero = FALSE) {
  if (is.na(x) || (!show_zero && x == 0)) return("")
  ax  <- abs(x); sgn <- if (x < 0) "−" else ""
  if      (ax >= 1e6) paste0(sgn, "$", formatC(ax / 1e6, digits = 1, format = "f"), "M")
  else if (ax >= 1e3) paste0(sgn, "$", formatC(ax / 1e3, digits = 0, format = "f"), "K")
  else                paste0(sgn, "$", formatC(ax,        digits = 0, format = "f",
                                               big.mark = ","))
}

# Full formatter for Total column: "$4,495,962"
.cf_fmt_full <- function(x) {
  if (is.na(x) || x == 0) return("")
  sgn <- if (x < 0) "−" else ""
  paste0(sgn, "$", formatC(abs(x), digits = 0, format = "f", big.mark = ","))
}

# Net cell color — bright enough to read on the dark navy net-row background
.cf_net_col <- function(x) {
  if (is.na(x) || x == 0) "#9ab5cc" else if (x > 0) "#5dd87a" else "#ff6b6b"
}

render_cashflow_preview_html <- function(ed, expanded = FALSE, day0_vals = list()) {
  labels  <- ed$bucket_labels
  n_bkts  <- length(labels)
  n_cols  <- n_bkts + 3L          # sticky-name + Day0 + N buckets + Total

  # ── Helpers ──────────────────────────────────────────────────────────────
  h <- function(x) htmltools::htmlEscape(as.character(x))

  tag_badge <- function(urgent, important) {
    paste0(
      if (isTRUE(urgent))    '<span class="cf-tag-u" title="Urgente">&#9679;</span>' else "",
      if (isTRUE(important)) '<span class="cf-tag-i" title="Importante">&#9733;</span>' else ""
    )
  }

  num_td <- function(v, cls = "", show_zero = FALSE) {
    paste0('<td class="cf-num ', cls, '">', .cf_fmt(v, show_zero), '</td>')
  }

  subtot_from_wide <- function(wide_df) {
    if (is.null(wide_df) || !nrow(wide_df)) return(rep(0, n_bkts))
    vapply(labels, function(lbl) {
      if (lbl %in% names(wide_df)) sum(as.numeric(wide_df[[lbl]]), na.rm = TRUE) else 0
    }, numeric(1))
  }

  # ── Header row ────────────────────────────────────────────────────────────
  hdr_html <- paste0(
    '<thead><tr class="cf-thead-row">',
    '<th class="cf-th cf-th-name">Contraparte</th>',
    '<th class="cf-th cf-th-day0" title="Saldo inicial editable">Día 0</th>',
    paste0('<th class="cf-th cf-th-bkt">', labels, '</th>', collapse = ""),
    '<th class="cf-th cf-th-total">Total</th>',
    '</tr></thead>'
  )

  # ── Section builder (AR or AP) ────────────────────────────────────────────
  build_section <- function(wide_df, sec_label, hdr_cls, subtot_label) {
    out <- paste0(
      '<tr class="cf-sec-hdr ', hdr_cls, '">',
      '<td class="cf-sec-name">', h(sec_label), '</td>',
      paste0(rep('<td></td>', n_cols - 1L), collapse = ""),
      '</tr>'
    )

    # Counterparty detail rows
    if (!is.null(wide_df) && nrow(wide_df)) {
      disp <- if (expanded) "" else ' style="display:none"'
      for (i in seq_len(nrow(wide_df))) {
        r     <- wide_df[i, ]
        pname <- h(as.character(r$Parte))
        badge <- tag_badge(isTRUE(r$tag_urgent), isTRUE(r$tag_important))
        prov  <- isTRUE(r$is_provision)
        rcls  <- paste0("cf-parte-row", if (prov) " cf-prov-row" else "",
                        if (i %% 2 == 0) " cf-row-alt" else "")

        bkt_cells <- paste0(vapply(labels, function(lbl) {
          v <- if (lbl %in% names(r)) as.numeric(r[[lbl]]) else 0
          num_td(v)
        }, character(1)), collapse = "")

        out <- paste0(out,
          '<tr class="', rcls, '"', disp, '>',
          '<td class="cf-td-name sticky-col">', pname, badge, '</td>',
          '<td class="cf-day0-empty"></td>',
          bkt_cells,
          '<td class="cf-num cf-total-cell">', .cf_fmt_full(as.numeric(r$Total)), '</td>',
          '</tr>'
        )
      }
    }

    # Subtotal row — always visible
    sub_bkts  <- subtot_from_wide(wide_df)
    sub_total <- sum(sub_bkts, na.rm = TRUE)
    sub_cells <- paste0(vapply(sub_bkts, function(v) num_td(v, "cf-subtot-num"),
                               character(1)), collapse = "")
    out <- paste0(out,
      '<tr class="cf-subtot-row">',
      '<td class="cf-td-name sticky-col cf-subtot-name">', h(subtot_label), '</td>',
      '<td class="cf-day0-empty"></td>',
      sub_cells,
      '<td class="cf-num cf-subtot-num cf-total-cell">', .cf_fmt_full(sub_total), '</td>',
      '</tr>'
    )
    out
  }

  # ── Per-currency blocks ───────────────────────────────────────────────────
  body_rows <- character(0)

  for (cur_data in ed$data) {
    cur  <- cur_data$currency
    smry <- cur_data$summary

    # Currency spacer between blocks (Separadas mode, multiple currencies)
    if (length(ed$currencies) > 1) {
      body_rows <- c(body_rows, paste0(
        '<tr class="cf-cur-spacer"><td colspan="', n_cols, '"></td></tr>'
      ))
    }

    # AR block
    body_rows <- c(body_rows,
      build_section(cur_data$ar_wide,
                    paste0("COBROS — ", cur), "cf-ar-hdr",
                    paste0("SUBTOTAL COBROS — ", cur)))

    # AP block
    body_rows <- c(body_rows,
      build_section(cur_data$ap_wide,
                    paste0("PAGOS — ", cur), "cf-ap-hdr",
                    paste0("SUBTOTAL PAGOS — ", cur)))

    # Net flow row
    if (!is.null(smry) && nrow(smry)) {
      net_cells <- paste0(vapply(seq_along(labels), function(j) {
        v <- if (j <= nrow(smry)) smry$net_day[j] else 0
        col <- .cf_net_col(v)
        paste0('<td class="cf-num cf-net-cell" style="color:', col, ';font-weight:600">',
               .cf_fmt(v, show_zero = TRUE), '</td>')
      }, character(1)), collapse = "")

      net_tot <- sum(smry$net_day, na.rm = TRUE)
      net_col <- .cf_net_col(net_tot)

      body_rows <- c(body_rows, paste0(
        '<tr class="cf-net-row">',
        '<td class="cf-td-name sticky-col cf-net-name">FLUJO NETO ', h(cur), '</td>',
        '<td class="cf-day0-empty"></td>',
        net_cells,
        '<td class="cf-num cf-net-cell cf-total-cell" style="color:', net_col,
          ';font-weight:700">', .cf_fmt_full(net_tot), '</td>',
        '</tr>'
      ))

      # Cumulative position row — includes Day 0 starting balance
      if ("net_cum" %in% names(smry)) {
        d0_val  <- day0_vals[[cur]] %||% 0

        d0_col  <- .cf_net_col(d0_val)
        d0_cell <- paste0(
          '<td class="cf-num cf-cum-cell cf-day0-cell" data-currency="', h(cur), '">',
          '<span class="cf-day0-val" style="color:', d0_col, ';font-weight:600;">',
          .cf_fmt(d0_val, show_zero = TRUE), '</span></td>')

        cum_cells <- paste0(vapply(seq_along(labels), function(j) {
          v <- (if (j <= nrow(smry)) smry$net_cum[j] else 0) + d0_val
          col <- .cf_net_col(v)
          paste0('<td class="cf-num cf-cum-cell" style="color:', col, ';font-weight:600">',
                 .cf_fmt(v, show_zero = TRUE), '</td>')
        }, character(1)), collapse = "")

        cum_last <- smry$net_cum[nrow(smry)] + d0_val
        cum_col  <- .cf_net_col(cum_last)

        body_rows <- c(body_rows, paste0(
          '<tr class="cf-cum-row">',
          '<td class="cf-td-name sticky-col cf-cum-name">POS. ACUMULADA ', h(cur), '</td>',
          d0_cell,
          cum_cells,
          '<td class="cf-num cf-cum-cell cf-total-cell" style="color:', cum_col,
            ';font-weight:700">', .cf_fmt_full(cum_last), '</td>',
          '</tr>'
        ))
      }
    }
  }

  HTML(paste0(
    '<div class="cf-table-wrap">',
    '<table class="cf-table">',
    hdr_html,
    '<tbody>', paste(body_rows, collapse = ""), '</tbody>',
    '</table></div>'
  ))
}

# ── Constants ─────────────────────────────────────────────────────────────────

.CF_STORAGE_KEY  <- "hopdesk_cf_preview_state"
.GRP_LABELS      <- c(auto = "Auto", daily = "Diario", weekly = "Semanal")
.GRP_CYCLE       <- c(auto = "daily", daily = "weekly", weekly = "auto")
.CUR_LABELS      <- c(separate = "Separadas", fused = "Base")
.M_ABBR          <- c("Ene","Feb","Mar","Abr","May","Jun",
                       "Jul","Ago","Sep","Oct","Nov","Dic")

# ── Helpers ───────────────────────────────────────────────────────────────────

# Default date range: Mon–Sun of current week
.cf_default_from <- function() lubridate::floor_date(Sys.Date(), "week", week_start = 1)
.cf_default_to   <- function() .cf_default_from() + 6L

# "02–08 Jun" label for the date range pill
.cf_range_label <- function(d1, d2) {
  paste0(sprintf("%02d", lubridate::day(d1)), "–",
         sprintf("%02d", lubridate::day(d2)), " ",
         .M_ABBR[lubridate::month(d2)])
}

# Build minimal JSON for a named numeric vector — avoids jsonlite dependency
.nvec_to_json <- function(v) {
  if (!length(v)) return("{}")
  paste0("{", paste0('"', names(v), '":', unname(v), collapse = ","), "}")
}

# Foreign currencies = all detected currencies minus the base currency
.cf_foreign_curs <- function(shared, base_cur) {
  ar <- tryCatch(shared$currency_choices[["AR"]](), error = function(e) character(0))
  ap <- tryCatch(shared$currency_choices[["AP"]](), error = function(e) character(0))
  setdiff(unique(toupper(c(ar, ap))), toupper(base_cur %||% "MXN"))
}

# All detected currencies (for base-cur selector choices)
.cf_all_curs <- function(shared) {
  ar <- tryCatch(shared$currency_choices[["AR"]](), error = function(e) character(0))
  ap <- tryCatch(shared$currency_choices[["AP"]](), error = function(e) character(0))
  unique(toupper(c(ar, ap)))
}

# ── UI ────────────────────────────────────────────────────────────────────────

cashflowPreviewUI <- function(id) {
  ns <- NS(id)

  tagList(
  div(
    id    = "cf-panel-preview",
    class = "cf-panel cf-panel-preview h-100",
    style = "display:flex; flex-direction:column;",

    # ── Main toolbar ─────────────────────────────────────────────────────────
    div(
      class = "cf-preview-toolbar",

      # Back arrow — always left-most
      actionButton(
        ns("btn_back"),
        icon("chevron-left"),
        label = NULL,
        class = "btn btn-link cf-back-arrow p-0 me-1",
        title = "Volver al calendario",
        style = "color:#1E3A5F; font-size:1.15rem; line-height:1; flex-shrink:0;"
      ),

      # Date range — button trigger + hidden input for Shiny binding.
      # A <button> is never targeted by Chrome's password manager; the
      # hidden input carries the Shiny value but is never user-focusable.
      div(
        class = "cf-ctrl cf-ctrl-date",
        style = "position:relative; display:inline-block;",

        # Visible trigger button (no autocomplete heuristics apply to buttons)
        tags$button(
          id    = ns("btn_date_range"),
          type  = "button",
          class = "form-control cf-date-btn",
          style = paste0("width:140px;text-align:left;cursor:pointer;",
                         "padding:4px 8px;background:#fff;font-size:inherit;color:inherit;"),
          .cf_range_label(.cf_default_from(), .cf_default_to())
        ),

        # Hidden airDatepicker: overlaid behind button, opacity 0, pointer-
        # events none — Chrome can never focus it through user interaction
        div(
          style = "position:absolute;top:0;left:0;opacity:0;pointer-events:none;z-index:-1;",
          shinyWidgets::airDatepickerInput(
            ns("date_range"),
            label      = NULL,
            value      = c(.cf_default_from(), .cf_default_to()),
            range      = TRUE,
            dateFormat = "dd MMM",
            width      = "140px",
            addon      = "none"
          )
        ),

        # Open the picker via the shinyWidgets binding store (ae.store[id])
        # which is the authoritative reference to the AirDatepicker instance
        tags$script(HTML(sprintf(
          "(function(){
            var inpId='%s', btnId='%s';
            function getDP(){
              try {
                var bs=Shiny.inputBindings.getBindings();
                for(var i=0;i<bs.length;i++){
                  var b=bs[i].binding;
                  if(b.store && b.store[inpId] && b.store[inpId].show) return b.store[inpId];
                }
              } catch(e){}
              return null;
            }
            // Toggle open/close on button click
            $(document).on('click','#'+btnId,function(){
              var dp=getDP(); if(!dp) return;
              if(dp.visible) dp.hide(); else dp.show();
            });
            // Close when clicking outside the calendar or button
            $(document).on('mousedown', function(e){
              var dp=getDP(); if(!dp||!dp.visible) return;
              var $cal=$(dp.$datepicker), $btn=$('#'+btnId);
              if(!$cal.is(e.target)&&!$cal.has(e.target).length&&
                 !$btn.is(e.target)&&!$btn.has(e.target).length) dp.hide();
            });
            // Close on Escape
            $(document).on('keydown', function(e){
              if(e.key==='Escape'){ var dp=getDP(); if(dp&&dp.visible) dp.hide(); }
            });
            // Update button label when date is selected
            $(document).on('change','#'+inpId,function(){
              var v=this.value; if(v) document.getElementById(btnId).textContent=v;
            });
          })();",
          ns("date_range"), ns("btn_date_range")
        )))
      ),

      # Grouping toggle (cycles Auto → Diario → Semanal)
      actionButton(ns("btn_grouping"), label = "Auto",
                   class = "btn btn-outline-secondary btn-sm cf-ctrl cf-grp-btn",
                   title = "Cambiar agrupación"),

      # Currency mode toggle (Separadas ↔ Base)
      actionButton(ns("btn_curmode"), label = "Separadas",
                   class = "btn btn-outline-secondary btn-sm cf-ctrl cf-cur-btn",
                   title = "Cambiar modo de moneda"),

      # Expand / Collapse all (Stage 4 wires the table; button state tracked here)
      actionButton(ns("btn_expand"), icon("table-cells"), label = NULL,
                   class = "btn btn-outline-secondary btn-sm cf-ctrl",
                   title = "Expandir todo"),

      # Export shortcut — pre-fills the export modal (wired in Stage 5)
      actionButton(ns("btn_export"),
                   tagList(icon("file-export"), " Exportar"),
                   class = "btn btn-outline-primary btn-sm cf-ctrl ms-auto",
                   title = "Exportar con esta configuración")
    ),

    # ── FX sub-row (visible only in Base / fused mode) ───────────────────────
    uiOutput(ns("fx_section_ui")),

    # ── Preview body (Stage 3: data status; Stage 4: cash-flow table) ───────
    div(
      class = "cf-preview-body",
      style = "flex:1; overflow:auto; min-height:0;",
      uiOutput(ns("preview_body"))
    )
  ),

  # ── Day 0 dialog (singleton, injected outside renderUI so it survives re-renders) ──
  tags$div(
    id    = "cf-day0-dlg",
    style = "display:none; position:fixed; z-index:9998;"
  ),

  tags$script(HTML(sprintf("
    (function() {
      var inputId = '%s';
      var curCurrency = null;

      // Build dialog content once
      var dlg = document.getElementById('cf-day0-dlg');
      if (dlg && !dlg.dataset.ready) {
        dlg.dataset.ready = '1';
        dlg.innerHTML =
          '<div class=\"cf-day0-dlg-inner\">' +
            '<div class=\"cf-day0-dlg-title\">Saldo Inicial \\u2014 D\\u00eda 0</div>' +
            '<input id=\"cf-day0-input\" type=\"text\" class=\"cf-day0-input\" ' +
                   'placeholder=\"ej: 5000 + 1200 + 840\" autocomplete=\"off\">' +
            '<div class=\"cf-day0-dlg-hint\">N\\u00famero o expresi\\u00f3n aritm\\u00e9tica (e.g. 5+20+7000)</div>' +
            '<div class=\"cf-day0-dlg-btns\">' +
              '<button class=\"btn btn-sm btn-outline-secondary\" id=\"cf-day0-cancel\">Cancelar</button>' +
              '<button class=\"btn btn-sm btn-primary\" id=\"cf-day0-ok\">Aplicar</button>' +
            '</div>' +
          '</div>';

        document.getElementById('cf-day0-cancel').addEventListener('click', function() {
          dlg.style.display = 'none';
        });
        document.getElementById('cf-day0-ok').addEventListener('click', submitDay0);
        document.getElementById('cf-day0-input').addEventListener('keydown', function(e) {
          if (e.key === 'Enter') submitDay0();
          if (e.key === 'Escape') dlg.style.display = 'none';
        });
        document.addEventListener('mousedown', function(e) {
          if (dlg.style.display !== 'none' &&
              !dlg.contains(e.target) &&
              !e.target.closest('.cf-day0-cell')) {
            dlg.style.display = 'none';
          }
        });
      }

      function submitDay0() {
        var expr = (document.getElementById('cf-day0-input').value || '').trim();
        if (!expr) return;
        var val;
        try {
          val = Function('\"use strict\"; return (' + expr + ')')();
          if (!isFinite(val) || isNaN(val)) throw new Error();
        } catch(err) {
          document.getElementById('cf-day0-input').style.borderColor = '#dc3545';
          return;
        }
        document.getElementById('cf-day0-input').style.borderColor = '';
        dlg.style.display = 'none';
        if (window.Shiny && curCurrency !== null) {
          Shiny.setInputValue(inputId, {currency: curCurrency, value: val}, {priority: 'event'});
        }
      }

      $(document).off('click.day0').on('click.day0', '.cf-day0-cell', function(e) {
        e.stopPropagation();
        curCurrency = $(this).data('currency');
        var rect = this.getBoundingClientRect();
        dlg.style.display = 'block';
        var top  = Math.min(rect.bottom + 4, window.innerHeight - 200);
        var left = Math.max(4, Math.min(rect.left - 60, window.innerWidth - 270));
        dlg.style.top  = top  + 'px';
        dlg.style.left = left + 'px';
        var inp = document.getElementById('cf-day0-input');
        var cur = $(this).find('.cf-day0-val').text().replace(/[$,−]/g, '').trim();
        inp.value = (cur === '0' || cur === '') ? '' : cur;
        inp.style.borderColor = '';
        setTimeout(function() { inp.focus(); inp.select(); }, 50);
      });
    })();
  ", ns("cf_day0_submit"))))
  ) # close tagList
}

# ── Server ────────────────────────────────────────────────────────────────────

cashflowPreviewServer <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Day 0 starting balances per currency (keyed by currency code)
    cf_day0_rv <- reactiveVal(list())

    # ── Reactive state ───────────────────────────────────────────────────────
    rv <- reactiveValues(
      date_from     = .cf_default_from(),
      date_to       = .cf_default_to(),
      grouping      = "auto",      # "auto" | "daily" | "weekly"
      currency_mode = "separate",  # "separate" | "fused"
      base_cur      = "MXN",
      fx_rates      = c(),         # named numeric: e.g. c(USD = 18.5)
      fx_modes      = list(),      # named list: cur -> "live" | "date"
      fx_dates      = list(),      # named list: cur -> Date
      expanded      = FALSE        # TRUE = all counterparty rows visible
    )

    # Tracks which currency inputs have had observers registered (Stage 2)
    .registered_curs <- character(0)

    # ── Panel open/close ─────────────────────────────────────────────────────

    observeEvent(input$btn_back, {
      shared$cf_preview_open(FALSE)
    })

    # ── Stage 6: Esc key closes preview ─────────────────────────────────────
    # Inject a document-level keyup listener once; fires Shiny.setInputValue
    # so the observer below can call shared$cf_preview_open(FALSE).
    observe({
      if (isTRUE(shared$cf_preview_open())) {
        shinyjs::runjs(sprintf(
          "(function(){
            if(window._cfEscBound) return;
            window._cfEscBound = true;
            document.addEventListener('keyup', function(e){
              if(e.key==='Escape'){
                Shiny.setInputValue('%s', Date.now(), {priority:'event'});
              }
            });
          })();",
          ns("esc_key")
        ))
      }
    })

    observeEvent(input$esc_key, {
      if (isTRUE(shared$cf_preview_open())) shared$cf_preview_open(FALSE)
    }, ignoreInit = TRUE)

    # ── Day 0 expression handler ─────────────────────────────────────────────
    observeEvent(input$cf_day0_submit, {
      sub <- input$cf_day0_submit
      req(!is.null(sub$currency), !is.null(sub$value), is.numeric(sub$value))
      cur_vals <- cf_day0_rv()
      cur_vals[[ sub$currency ]] <- as.numeric(sub$value)
      cf_day0_rv(cur_vals)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observe({
      if (isTRUE(shared$cf_preview_open())) {
        shinyjs::addClass(selector = ".cf-slide-wrapper",
                          class    = "cf-preview-active", asis = TRUE)
        shinyjs::addClass(selector = "body",
                          class    = "cf-preview-open",   asis = TRUE)
        # Load persisted state from localStorage on first open each day
        shinyjs::runjs(sprintf("
          (function() {
            try {
              var s = JSON.parse(localStorage.getItem('%s') || '{}');
              var today = new Date().toISOString().slice(0,10);
              if (s && s.saved_date === today) {
                Shiny.setInputValue('%s', s, {priority:'event'});
              }
            } catch(e) {}
          })();
        ", .CF_STORAGE_KEY, ns("stored_state")))
      } else {
        shinyjs::removeClass(selector = ".cf-slide-wrapper",
                             class    = "cf-preview-active", asis = TRUE)
        shinyjs::removeClass(selector = "body",
                             class    = "cf-preview-open",   asis = TRUE)
      }
    })

    # ── Restore state from localStorage ──────────────────────────────────────

    observeEvent(input$stored_state, {
      s <- input$stored_state
      tryCatch({
        if (!is.null(s$date_from) && !is.null(s$date_to)) {
          d1 <- as.Date(s$date_from); d2 <- as.Date(s$date_to)
          if (!is.na(d1) && !is.na(d2) && d2 >= d1) {
            rv$date_from <- d1; rv$date_to <- d2
            shinyWidgets::updateAirDateInput(session, "date_range",
                                             value = c(d1, d2))
            shiny::updateActionButton(session, "btn_date_range",
                                      label = .cf_range_label(d1, d2))
          }
        }
        if (!is.null(s$grouping) && s$grouping %in% names(.GRP_LABELS)) {
          rv$grouping <- s$grouping
          shiny::updateActionButton(session, "btn_grouping",
                                    label = .GRP_LABELS[[ s$grouping ]])
        }
        if (!is.null(s$currency_mode) && s$currency_mode %in% names(.CUR_LABELS)) {
          rv$currency_mode <- s$currency_mode
          shiny::updateActionButton(session, "btn_curmode",
                                    label = .CUR_LABELS[[ s$currency_mode ]])
        }
        if (!is.null(s$base_cur) && nzchar(s$base_cur)) {
          rv$base_cur <- s$base_cur
        }
        if (!is.null(s$fx_rates) && length(s$fx_rates)) {
          rates <- unlist(s$fx_rates)
          rv$fx_rates <- setNames(as.numeric(rates), names(rates))
        }
      }, error = function(e) NULL)  # stale or malformed storage — silently ignore
    }, ignoreInit = TRUE)

    # ── Save state to localStorage (debounced 800 ms) ────────────────────────

    .state_to_save <- reactive({
      list(rv$date_from, rv$date_to, rv$grouping, rv$currency_mode,
           rv$base_cur, rv$fx_rates)
    }) |> debounce(800)

    observeEvent(.state_to_save(), {
      shinyjs::runjs(sprintf(
        "try{localStorage.setItem('%s',JSON.stringify({date_from:'%s',date_to:'%s',grouping:'%s',currency_mode:'%s',base_cur:'%s',fx_rates:%s,saved_date:'%s'}));}catch(e){}",
        .CF_STORAGE_KEY,
        format(rv$date_from, "%Y-%m-%d"),
        format(rv$date_to,   "%Y-%m-%d"),
        rv$grouping,
        rv$currency_mode,
        rv$base_cur,
        .nvec_to_json(rv$fx_rates),
        format(Sys.Date(), "%Y-%m-%d")
      ))
    }, ignoreInit = TRUE)

    # ── Date range ────────────────────────────────────────────────────────────

    observeEvent(input$date_range, {
      v <- input$date_range
      if (!is.null(v) && length(v) == 2 && !anyNA(v)) {
        rv$date_from <- as.Date(v[[1]])
        rv$date_to   <- as.Date(v[[2]])
        shiny::updateActionButton(session, "btn_date_range",
                                  label = .cf_range_label(rv$date_from, rv$date_to))
      }
    }, ignoreInit = TRUE)

    # ── Grouping toggle ───────────────────────────────────────────────────────

    observeEvent(input$btn_grouping, {
      rv$grouping <- .GRP_CYCLE[[ rv$grouping ]]
      shiny::updateActionButton(session, "btn_grouping",
                                label = .GRP_LABELS[[ rv$grouping ]])
    }, ignoreInit = TRUE)

    # ── Currency mode toggle ──────────────────────────────────────────────────

    observeEvent(input$btn_curmode, {
      rv$currency_mode <- if (rv$currency_mode == "separate") "fused" else "separate"
      shiny::updateActionButton(session, "btn_curmode",
                                label = .CUR_LABELS[[ rv$currency_mode ]])
    }, ignoreInit = TRUE)

    # ── Expand / collapse all toggle ──────────────────────────────────────────

    observeEvent(input$btn_expand, {
      rv$expanded <- !rv$expanded
      shiny::updateActionButton(
        session, "btn_expand",
        icon  = if (rv$expanded) icon("table-cells-large") else icon("table-cells"),
        label = NULL
      )
    }, ignoreInit = TRUE)

    # ── Base currency selector ────────────────────────────────────────────────

    observeEvent(input$base_cur, {
      b <- input$base_cur
      if (!is.null(b) && nzchar(b)) rv$base_cur <- b
    }, ignoreInit = TRUE)

    # ── FX section UI (rendered only in fused / Base mode) ───────────────────

    output$fx_section_ui <- renderUI({
      if (rv$currency_mode != "fused") return(NULL)

      all_curs     <- .cf_all_curs(shared)
      base_choices <- if (length(all_curs)) all_curs else c("MXN","USD")
      base_sel     <- rv$base_cur %||% "MXN"
      foreign      <- setdiff(base_choices, base_sel)

      div(
        class = "cf-fx-section d-flex align-items-center flex-wrap gap-3 px-3 py-2",
        style = "border-bottom:1px solid #e5e9ef; background:#f8fafc; font-size:0.82rem;",

        # Base currency selector
        div(class = "d-flex align-items-center gap-1",
          tags$small("Base:", style = "color:#5a7596; white-space:nowrap;"),
          selectInput(ns("base_cur"), label = NULL,
                      choices  = base_choices,
                      selected = base_sel,
                      width    = "80px")
        ),

        # Per-currency FX rows
        lapply(foreign, function(cur) {
          mode <- rv$fx_modes[[cur]] %||% "live"
          # rv$fx_rates starts as c() (atomic, not list) so [[key]] on empty vector
          # throws "subscript out of bounds" — guard with a name-check first
          rate <- if (cur %in% names(rv$fx_rates)) rv$fx_rates[[cur]]
                  else .suggest_fx_rate(cur, base_sel)

          div(class = "d-flex align-items-center gap-1 cf-fx-row",
            tags$small(cur, style = "color:#5a7596; font-weight:600; min-width:28px;"),

            # Live / date toggle button
            # TODO(forecasting-fx): when Forecasting exposes FX series, replace
            # .suggest_fx_rate() with resolve_fx_rate(cur, base_sel, mode, ref_date)
            # where resolve_fx_rate() calls:
            #   mode=="live" & ref_date==today → forecasting_get_estimate(pair, today, type="open")
            #   ref_date < today               → forecasting_get_estimate(pair, ref_date, type="close")
            #   ref_date > today               → forecasting_get_estimate(pair, ref_date, type="forecast")
            actionButton(
              ns(paste0("fx_mode_", tolower(cur))),
              label = if (mode == "live") "Live"
                      else format(rv$fx_dates[[cur]] %||% Sys.Date(), "%d %b"),
              class = paste0(
                "btn btn-xs cf-fx-mode-btn ",
                if (mode == "live") "cf-fx-live" else "cf-fx-date"
              )
            ),

            # Date input — shown only when mode == "date"
            if (mode == "date") {
              dateInput(
                ns(paste0("fx_date_", tolower(cur))),
                label = NULL,
                value = rv$fx_dates[[cur]] %||% Sys.Date(),
                width = "110px"
              )
            },

            # Rate numeric input — JS focusout handler fires _blur shadow input on blur/Enter
            numericInput(
              ns(paste0("fx_rate_", tolower(cur))),
              label = NULL,
              value = round(rate, 4),
              min   = 0,
              step  = 0.01,
              width = "88px"
            )
          )
        }),

        # Bind focusout on every rate input so Shiny only sees a value when the
        # user leaves the field — not on every keystroke.
        tags$script(HTML(paste0(
          "(function(){",
            "$(document).off('.cffxblur')",
            paste0(lapply(foreign, function(cur) {
              sel  <- paste0("#", ns(paste0("fx_rate_", tolower(cur))))
              bkey <- paste0(ns(paste0("fx_rate_", tolower(cur))), "_blur")
              paste0(
                ".on('focusout.cffxblur','", sel, "',function(){",
                  "var v=parseFloat($(this).val());",
                  "if(!isNaN(v)&&v>0)Shiny.setInputValue('", bkey, "',v,{priority:'event'});",
                "})"
              )
            }), collapse = ""),
          ";})()"
        )))
      )
    })

    # ── Dynamic FX observers (registered once per currency) ──────────────────
    # Currencies can change as SAP data loads; register observers for new ones.

    available_foreign <- reactive({
      .cf_foreign_curs(shared, rv$base_cur)
    })

    observe({
      new_curs <- setdiff(available_foreign(), .registered_curs)
      if (!length(new_curs)) return()

      lapply(new_curs, function(cur) {
        local({
          cur_ <- cur

          # FX mode (Live / date) toggle
          # TODO(forecasting-fx): when mode toggles to "date", seed rv$fx_rates[[cur_]]
          # via resolve_fx_rate(cur_, rv$base_cur, "date", rv$fx_dates[[cur_]] %||% Sys.Date())
          # instead of leaving the last known rate in place.
          observeEvent(input[[ paste0("fx_mode_", tolower(cur_)) ]], {
            mode <- rv$fx_modes[[cur_]] %||% "live"
            rv$fx_modes[[cur_]] <- if (mode == "live") "date" else "live"
            if (rv$fx_modes[[cur_]] == "date" && is.null(rv$fx_dates[[cur_]])) {
              rv$fx_dates[[cur_]] <- Sys.Date()
            }
          }, ignoreInit = TRUE)

          # FX date picker (visible only when mode == "date")
          # TODO(forecasting-fx): on date change, update rv$fx_rates[[cur_]] via
          # resolve_fx_rate(cur_, rv$base_cur, "date", as.Date(d)) instead of keeping
          # the user-entered override as the sole source of truth.
          observeEvent(input[[ paste0("fx_date_", tolower(cur_)) ]], {
            d <- input[[ paste0("fx_date_", tolower(cur_)) ]]
            if (!is.null(d) && !is.na(as.Date(d))) {
              rv$fx_dates[[cur_]] <- as.Date(d)
            }
          }, ignoreInit = TRUE)

          # FX rate — observe the blur shadow input (fires only on focusout / Enter,
          # not on every keystroke). The JS in fx_section_ui sets fx_rate_{cur}_blur.
          observeEvent(input[[ paste0("fx_rate_", tolower(cur_), "_blur") ]], {
            r <- input[[ paste0("fx_rate_", tolower(cur_), "_blur") ]]
            if (!is.null(r) && !is.na(r) && is.numeric(r) && r > 0) {
              rv$fx_rates[[ cur_ ]] <- r
            }
          }, ignoreInit = TRUE)
        })
      })

      .registered_curs <<- c(.registered_curs, new_curs)
    })

    # ── Stage 3: Data pipeline ────────────────────────────────────────────────
    # Mirrors the export pipeline exactly. Guarded by req(cf_preview_open()) so
    # it never fires while the panel is hidden — no cost to calendar performance.
    # Debounced 400 ms so rapid FX-rate edits don't thrash build_export_combined_df.

    .preview_raw <- reactive({
      req(isTRUE(shared$cf_preview_open()))
      ic_val <- tryCatch(shared$ic_mode[["AR"]](), error = function(e) "exclude")
      build_export_combined_df(shared, ic_mode_val = ic_val)
    })

    preview_export_data <- reactive({
      req(isTRUE(shared$cf_preview_open()))
      df_raw <- .preview_raw()
      req(!is.null(df_raw))

      tags_df  <- tryCatch(shared$tags_db(),      error = function(e) NULL)
      emp_sel  <- tryCatch(shared$empresa_sel(),   error = function(e) character(0)) %||% character(0)
      fx_vec   <- rv$fx_rates                      # named numeric; empty c() in Separadas mode

      build_cashflow_export_data(
        df            = df_raw,
        tags_df       = tags_df,
        emp_sel       = emp_sel,
        date_from     = rv$date_from,
        date_to       = rv$date_to,
        grouping      = rv$grouping,
        currency_mode = rv$currency_mode,
        base_cur      = rv$base_cur,
        fx_rates      = fx_vec
      )
    }) |> debounce(400)

    # ── Stage 3: Preview body output (spinner → data → empty state) ───────────
    # Stage 4 replaces the placeholder div with the actual HTML table.
    # req() inside preview_export_data() throws shiny.silent.error while the
    # panel is opening / data not yet ready — treat that as a loading state,
    # not a real error.

    output$preview_body <- renderUI({
      if (!isTRUE(shared$cf_preview_open())) return(NULL)

      ed <- tryCatch(preview_export_data(), error = function(e) {
        if (inherits(e, "shiny.silent.error")) return("loading")
        message("[CF_PREVIEW] pipeline error: ", conditionMessage(e))
        NULL
      })

      if (identical(ed, "loading")) {
        div(
          class = "d-flex align-items-center justify-content-center h-100 gap-2",
          style = "color:#8ca0b8; font-size:0.9rem;",
          tags$span(class = "spinner-border spinner-border-sm", role = "status"),
          tags$span("Calculando flujo…")
        )
      } else if (is.null(ed) || !length(ed$currencies)) {
        div(
          class = "d-flex flex-column align-items-center justify-content-center h-100 gap-2",
          style = "color:#8ca0b8;",
          icon("inbox", style = "font-size:2rem;"),
          tags$span("Sin movimientos en el período seleccionado",
                    style = "font-size:0.9rem;")
        )
      } else {
        render_cashflow_preview_html(ed, expanded = isTRUE(rv$expanded), day0_vals = cf_day0_rv())
      }
    })

    # ── Export shortcut (Stage 5) ─────────────────────────────────────────────
    # Fires shared$cf_preview_prefill with the current preview settings so that
    # setup_cashflow_export_server() can pre-fill the export modal.
    observeEvent(input$btn_export, {
      shared$cf_preview_prefill(list(
        date_from     = rv$date_from,
        date_to       = rv$date_to,
        grouping      = rv$grouping,
        currency_mode = rv$currency_mode,
        base_cur      = rv$base_cur,
        fx_rates      = rv$fx_rates
      ))
    }, ignoreInit = TRUE)

  })
}
