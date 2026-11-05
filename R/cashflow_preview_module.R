# =============================================================================
# R/cashflow_preview_module.R
# In-app Cash Flow Preview panel — slides in alongside the Calendar.
#
# Stage 1 : Animation shell — arrow toggles panels, placeholder content.
# Stage 2 : Preview controls toolbar (date range, grouping, currency, FX).
# Stage 3 : Data pipeline wiring (build_export_combined_df + build_cashflow_export_data).
# Stage 4 : The table is the hd_table engine (T17c, table "flujo_caja"):
#           cashflow_tabla_datos() / cashflow_tabla_columnas() below.
# Stage 5 : Export shortcut — pre-fills the export modal from preview settings.
# Stage 6 : Polish (Esc key, responsive, performance, WCAG).
# =============================================================================

# ── Stage 4: the table, for the engine ───────────────────────────────────────
# The numbers are the module's (build_cashflow_export_data(), what the
# exports use too); here they only change shape. One row per counterparty
# (a provision its own row), grouped by currency (the band) and Cobros /
# Pagos (the group: closed, it is one row with each column's sum), then each
# currency's FLUJO NETO and POS. ACUMULADA as summary rows (.resumen) at the
# end of its band. Día 0 lives on the position row, the only one it can be
# typed in (.dia0); the position adds it to the running net.
#
# Ids: "<moneda>|<ar|ap>|<n>" for a counterparty (they last one build, which
# is all a preview needs), "neto|<moneda>" and "acum|<moneda>".
cashflow_tabla_datos <- function(ed, day0_vals = list()) {
  labels <- ed$bucket_labels %||% character(0)
  n_b <- length(labels)
  b   <- paste0("b", seq_len(n_b))
  vacia <- function() {
    d <- data.frame(id = character(0), Contraparte = character(0), Etiqueta = character(0),
                    Moneda = character(0), Tipo = character(0), Dia0 = numeric(0),
                    stringsAsFactors = FALSE)
    for (k in b) d[[k]] <- numeric(0)
    d$Total <- numeric(0); d$.resumen <- character(0); d$.dia0 <- logical(0); d$.provision <- logical(0)
    d
  }
  if (is.null(ed) || !length(ed$data)) return(vacia())
  # A data.frame from columns that are each one value or one per row.
  bloque <- function(id, nombre, etiqueta, moneda, tipo, dia0, valores, total, resumen, puede_dia0, provision) {
    d <- data.frame(id = id, Contraparte = nombre, Etiqueta = etiqueta, Moneda = moneda, Tipo = tipo,
                    Dia0 = dia0, stringsAsFactors = FALSE)
    for (j in seq_len(n_b)) d[[b[j]]] <- valores[[j]]
    d$Total <- total; d$.resumen <- resumen; d$.dia0 <- puede_dia0; d$.provision <- provision
    d
  }
  partes <- list()
  for (cd in ed$data) {
    cur <- cd$currency
    for (lado in c("ar", "ap")) {
      w <- cd[[paste0(lado, "_wide")]]
      if (is.null(w) || !nrow(w)) next
      et <- vapply(seq_len(nrow(w)), function(i)
        hd_etiqueta_clave(c(if (isTRUE(w$tag_urgent[i])) "urgent", if (isTRUE(w$tag_important[i])) "important")),
        character(1))
      prov <- if ("is_provision" %in% names(w)) as.logical(w$is_provision) else rep(FALSE, nrow(w))
      prov[is.na(prov)] <- FALSE
      partes[[length(partes) + 1L]] <- bloque(
        id = sprintf("%s|%s|%d", cur, lado, seq_len(nrow(w))), nombre = as.character(w$Parte), etiqueta = et,
        moneda = cur, tipo = tr(if (lado == "ar") "tablas.cobros" else "tablas.pagos"), dia0 = NA_real_,
        valores = lapply(labels, function(l) if (l %in% names(w)) as.numeric(w[[l]]) else rep(0, nrow(w))),
        total = as.numeric(w$Total), resumen = "", puede_dia0 = FALSE, provision = prov)
    }
    s <- cd$summary
    if (!is.null(s) && nrow(s)) {
      en <- function(v, j) if (j <= length(v)) v[j] else 0
      d0 <- as.numeric(day0_vals[[cur]] %||% 0)
      partes[[length(partes) + 1L]] <- bloque(
        id = paste0("neto|", cur), nombre = tr("tablas.flujo.neto", moneda = cur), etiqueta = "",
        moneda = cur, tipo = "", dia0 = NA_real_, valores = lapply(seq_len(n_b), function(j) en(s$net_day, j)),
        total = sum(s$net_day, na.rm = TRUE), resumen = "neto", puede_dia0 = FALSE, provision = FALSE)
      if ("net_cum" %in% names(s))
        partes[[length(partes) + 1L]] <- bloque(
          id = paste0("acum|", cur), nombre = tr("tablas.flujo.acumulada", moneda = cur), etiqueta = "",
          moneda = cur, tipo = "", dia0 = d0, valores = lapply(seq_len(n_b), function(j) en(s$net_cum, j) + d0),
          total = s$net_cum[nrow(s)] + d0, resumen = "acumulada", puede_dia0 = TRUE, provision = FALSE)
    }
  }
  if (!length(partes)) return(vacia())
  do.call(rbind, partes)
}

# The columns: the counterparty (fixed on the left), its tag, Día 0, one per
# period (compact, empty at 0; a summary row's tone by its sign) and the
# Total in whole pesos (halves up). A function: the periods change with the range.
cashflow_tabla_columnas <- function(labels) {
  c(list(col_texto("Contraparte", tr("tablas.flujo.contraparte"), fija = TRUE),
         col_etiqueta("Etiqueta", tr("tablas.flujo.etiqueta")),
         col_dinero("Dia0", tr("tablas.flujo.dia0"), sumar = FALSE, formato = "compacto", tono = "signo",
                    editable = list(expresion = TRUE, filas = ".dia0", min = -1e15))),
    lapply(seq_along(labels), function(j)
      col_dinero(paste0("b", j), labels[j], formato = "compacto", cero = "vacio", tono = "signo")),
    list(col_dinero("Total", tr("tablas.flujo.total"), formato = "entero", cero = "vacio", tono = "signo")))
}

# ── Constants ─────────────────────────────────────────────────────────────────

.CF_STORAGE_KEY  <- "hopdesk_cf_preview_state"
# Día 0 per currency, {"MXN": 1500, ...}: kept until it is changed, not just
# for the day like the rest (T17c, Luis resp. 2). Before, it lasted the session.
.CF_DIA0_KEY     <- "hopdesk_cf_dia0"
# Today in the BROWSER's own calendar, "AAAA-MM-DD". The saved state is kept
# for the day, and both the day it was saved and today come from here: the
# server's Sys.Date() wrote it and toISOString() (UTC) read it, so from 18:00
# in Mexico (already tomorrow in UTC) it was never restored.
.CF_HOY_JS <- paste0("(function(d){return d.getFullYear()+'-'+('0'+(d.getMonth()+1)).slice(-2)+'-'+",
                     "('0'+d.getDate()).slice(-2);})(new Date())")
.GRP_CYCLE       <- c(auto = "daily", daily = "weekly", weekly = "auto")
# The buttons' texts: tablas.flujo.agrupacion.<auto|daily|weekly> and
# tablas.flujo.moneda_modo.<separate|fused>.
.cf_txt_agrupacion <- function(g) tr(paste0("tablas.flujo.agrupacion.", g))
.cf_txt_moneda     <- function(m) tr(paste0("tablas.flujo.moneda_modo.", m))

# ── Helpers ───────────────────────────────────────────────────────────────────

# Default date range: Mon–Sun of current week
.cf_default_from <- function() lubridate::floor_date(Sys.Date(), "week", week_start = 1)
.cf_default_to   <- function() .cf_default_from() + 6L

# The date range button's text, from the dictionary: "02–08 nov" within a
# month, "26 oct – 01 nov" across two. The one place it is written -- the
# server sets it with shinyjs::html() whenever the range changes (the
# picker, a restored state). It used to come from here at first, then from
# the picker's own text ("26 Oct - 01 Nov") after a change, and the
# updateActionButton() calls on this plain <button> did nothing.
.cf_range_label <- function(d1, d2) {
  dd <- function(d) sprintf("%02d", lubridate::day(d))
  mes <- function(d) tr(paste0("comun.mes.", lubridate::month(d)))
  if (identical(format(d1, "%Y-%m"), format(d2, "%Y-%m")))
    tr("tablas.flujo.rango", desde = dd(d1), hasta = dd(d2), mes = mes(d2))
  else
    tr("tablas.flujo.rango_meses", desde = dd(d1), mes_desde = mes(d1), hasta = dd(d2), mes_hasta = mes(d2))
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
        title = tr("tablas.flujo.volver"),
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
          })();",
          ns("date_range"), ns("btn_date_range")
        )))
      ),

      # Grouping toggle (cycles Auto → Diario → Semanal)
      actionButton(ns("btn_grouping"), label = .cf_txt_agrupacion("auto"),
                   class = "btn btn-outline-secondary btn-sm cf-ctrl cf-grp-btn",
                   title = tr("tablas.flujo.agrupacion_ayuda")),

      # Currency mode toggle (Separadas ↔ Base)
      actionButton(ns("btn_curmode"), label = .cf_txt_moneda("separate"),
                   class = "btn btn-outline-secondary btn-sm cf-ctrl cf-cur-btn",
                   title = tr("tablas.flujo.moneda_modo_ayuda")),

      # Opening / closing every group is the engine's own button, in the table.

      # Export shortcut — pre-fills the export modal (wired in Stage 5)
      actionButton(ns("btn_export"),
                   tagList(icon("file-export"), " ", tr("tablas.flujo.exportar")),
                   class = "btn btn-outline-primary btn-sm cf-ctrl ms-auto",
                   title = tr("tablas.flujo.exportar_ayuda"))
    ),

    # ── FX sub-row (visible only in Base / fused mode) ───────────────────────
    uiOutput(ns("fx_section_ui")),

    # ── Preview body: the engine (T17c) ──────────────────────────────────────
    # Día 0 is typed in its cell (edicion_celda, arithmetic only); the
    # dialog and its own evaluator are gone.
    div(
      class = "cf-preview-body",
      style = "flex:1; overflow:auto; min-height:0;",
      hd_table_ui(ns("tabla"), alto = "ventana")
    )
  )
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
      fx_dates      = list()       # named list: cur -> Date
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
    # [CAMBIA T17c] An Esc that something else already took doesn't close it:
    # leaving a cell being typed in (Día 0), any field, or the open date
    # picker. It used to close the whole preview too. Seen on keydown, in the
    # capture phase: by keyup the editor or the picker is already gone.
    observe({
      if (isTRUE(shared$cf_preview_open())) {
        shinyjs::runjs(sprintf(
          "(function(){
            if(window._cfEscBound) return;
            window._cfEscBound = true;
            var tomada = false;
            document.addEventListener('keydown', function(e){
              if(e.key!=='Escape') return;
              var t = e.target, tag = t && t.tagName;
              tomada = tag==='INPUT' || tag==='TEXTAREA' || tag==='SELECT' || !!(t && t.isContentEditable) ||
                       !!document.querySelector('.air-datepicker.-active-');
            }, true);
            document.addEventListener('keyup', function(e){
              if(e.key==='Escape'){
                if(tomada){ tomada = false; return; }
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
              var today = %s;
              if (s && s.saved_date === today) {
                Shiny.setInputValue('%s', s, {priority:'event'});
              }
            } catch(e) {}
            try {
              var d0 = JSON.parse(localStorage.getItem('%s') || '{}');
              if (d0 && typeof d0 === 'object') Shiny.setInputValue('%s', d0, {priority:'event'});
            } catch(e) {}
          })();
        ", .CF_STORAGE_KEY, .CF_HOY_JS, ns("stored_state"), .CF_DIA0_KEY, ns("dia0_guardado")))
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
            shinyjs::html("btn_date_range", .cf_range_label(d1, d2))
          }
        }
        if (!is.null(s$grouping) && s$grouping %in% names(.GRP_CYCLE)) {
          rv$grouping <- s$grouping
          shiny::updateActionButton(session, "btn_grouping",
                                    label = .cf_txt_agrupacion(s$grouping))
        }
        if (!is.null(s$currency_mode) && s$currency_mode %in% c("separate", "fused")) {
          rv$currency_mode <- s$currency_mode
          shiny::updateActionButton(session, "btn_curmode",
                                    label = .cf_txt_moneda(s$currency_mode))
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
        "try{localStorage.setItem('%s',JSON.stringify({date_from:'%s',date_to:'%s',grouping:'%s',currency_mode:'%s',base_cur:'%s',fx_rates:%s,saved_date:%s}));}catch(e){}",
        .CF_STORAGE_KEY,
        format(rv$date_from, "%Y-%m-%d"),
        format(rv$date_to,   "%Y-%m-%d"),
        rv$grouping,
        rv$currency_mode,
        rv$base_cur,
        .nvec_to_json(rv$fx_rates),
        .CF_HOY_JS
      ))
    }, ignoreInit = TRUE)

    # ── Date range ────────────────────────────────────────────────────────────

    observeEvent(input$date_range, {
      v <- input$date_range
      if (!is.null(v) && length(v) == 2 && !anyNA(v)) {
        rv$date_from <- as.Date(v[[1]])
        rv$date_to   <- as.Date(v[[2]])
        shinyjs::html("btn_date_range", .cf_range_label(rv$date_from, rv$date_to))
      }
    }, ignoreInit = TRUE)

    # ── Grouping toggle ───────────────────────────────────────────────────────

    observeEvent(input$btn_grouping, {
      rv$grouping <- .GRP_CYCLE[[ rv$grouping ]]
      shiny::updateActionButton(session, "btn_grouping",
                                label = .cf_txt_agrupacion(rv$grouping))
    }, ignoreInit = TRUE)

    # ── Currency mode toggle ──────────────────────────────────────────────────

    observeEvent(input$btn_curmode, {
      rv$currency_mode <- if (rv$currency_mode == "separate") "fused" else "separate"
      shiny::updateActionButton(session, "btn_curmode",
                                label = .cf_txt_moneda(rv$currency_mode))
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
          tags$small(tr("tablas.flujo.base"), style = "color:#5a7596; white-space:nowrap;"),
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
              label = if (mode == "live") tr("tablas.flujo.fx_vivo")
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

    # ── Stage 4: the table (engine) ───────────────────────────────────────────
    # A pipeline error is logged and shows as no rows ("Sin movimientos ..."),
    # as before; while the data isn't ready the engine says it is loading.
    ed_tabla <- reactive({
      tryCatch(preview_export_data(), error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        message("[CF_PREVIEW] pipeline error: ", conditionMessage(e))
        NULL
      })
    })
    filas_tabla <- reactive(cashflow_tabla_datos(ed_tabla(), cf_day0_rv()))
    cols_tabla  <- reactive(cashflow_tabla_columnas(ed_tabla()$bucket_labels %||% character(0)))

    tabla <- hd_table_server(
      "tabla", tabla = "flujo_caja", datos = filas_tabla, columnas = cols_tabla,
      piezas = c("sumas_columnas", "filas_resumen", "edicion_celda", "dinero_compacto", "tono_celda",
                 "expandir", "etiquetas"),
      secciones = list(list(campo = "Moneda", orden = c("MXN", "USD", "EUR"))),
      grupo = c("Moneda", "Tipo"),
      vacio = "tablas.flujo.vacio")

    # ── Día 0: typed in the position row's cell (per currency) ───────────────
    # What was typed becomes the currency's Día 0 and the table is rebuilt
    # from it; the typed values are then let go (the data says it now).
    observeEvent(tabla$ediciones(), {
      e <- tabla$ediciones()
      e <- e[e$valido & startsWith(e$id, "acum|"), , drop = FALSE]
      if (!nrow(e)) return()
      vals <- cf_day0_rv()
      for (k in seq_len(nrow(e))) vals[[ sub("^acum\\|", "", e$id[k]) ]] <- as.numeric(e$valor[k])
      cf_day0_rv(vals)
      tabla$limpiar_ediciones()
      # [CAMBIA T17c] kept in the browser until it is changed (see .CF_DIA0_KEY).
      shinyjs::runjs(sprintf("try{localStorage.setItem('%s',%s);}catch(e){}", .CF_DIA0_KEY,
                             jsonlite::toJSON(vals, auto_unbox = TRUE, digits = NA)))
    }, ignoreInit = TRUE)

    # What the browser kept: only finite numbers under a currency's code.
    observeEvent(input$dia0_guardado, {
      g <- input$dia0_guardado
      if (!is.list(g) || !length(g) || is.null(names(g))) return()
      vals <- cf_day0_rv()
      for (cur in names(g)) {
        v <- suppressWarnings(as.numeric(g[[cur]]))
        if (grepl("^[A-Z]{3}$", cur) && length(v) == 1L && is.finite(v) && is.null(vals[[cur]])) vals[[cur]] <- v
      }
      cf_day0_rv(vals)
    }, ignoreInit = TRUE)

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

    # For tests (testServer): what the table gets.
    invisible(list(datos = filas_tabla, columnas = cols_tabla, tabla = tabla, day0 = cf_day0_rv))
  })
}
