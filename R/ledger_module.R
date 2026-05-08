# =============================================================================
# R/ledger_module.R
# A single Shiny module that handles one ledger (AR or AP).
# Instantiated twice in app.R with different config objects.
# input/output/session never leave their proper scope.
# =============================================================================

# ── Module UI ──────────────────────────────────────────────────────────────────

ledgerModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('restoreModalScroll', function(msg) {
        setTimeout(function() {
          var body = document.querySelector('.modal-body');
          if (body) body.scrollTop = msg.top;
        }, 60);
      });
    ")),
    div(
      class = "calendar-container position-relative",
      uiOutput(ns("loading_overlay")),
      uiOutput(ns("calendar"))
    )
  )
}

# ── Module Server ──────────────────────────────────────────────────────────────

ledgerModuleServer <- function(id, config, shared) {
  moduleServer(id, function(input, output, session) {
    ns     <- session$ns
    ledger <- config$ledger

    # ── Combined data: SAP + manual + moves ─────────────────────────────────
    df_combined <- reactive({
      t0_comb <- proc.time()
      raw    <- shared$sap_data()[[ledger]]
      manual <- shared$manual_inv()
      moves  <- shared$moves_db()
      emp    <- shared$empresa_sel()

      message("[DF_COMBINED] start ledger=", ledger,
              " raw_rows=", if (is.null(raw)) "NULL" else nrow(raw))

      df <- build_ledger_df(
        raw_df          = raw,
        ledger          = ledger,
        empresa         = NULL,
        moves_df        = moves,
        manual_df       = manual,
        abonos_df       = shared$abonos_db(),
        policy_moves_df = tryCatch(shared$policy_moves_db(), error = function(e) NULL),
        sap_ov          = tryCatch(shared$sap_ov_db(), error = function(e) NULL),
        provs_df        = if (ledger == "AP")
                            tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL)
                          else NULL,
        company_map     = as.list(tryCatch(shared$company_map(), error = function(e) NULL))
      )

      message("[DF_COMBINED] build_ledger_df done ledger=", ledger,
              " rows=", if (is.null(df)) "NULL" else nrow(df),
              " in ", round((proc.time() - t0_comb)[["elapsed"]], 2), "s")

      if (is.null(df) || !nrow(df)) return(NULL)

      # Hide invoices that have been soft-deleted (sent to papelera)
      # Use shared reactive (loaded once at startup) instead of S3 per invalidation
      papelera <- if (!is.null(shared$papelera_rv)) {
        tryCatch(shared$papelera_rv(), error = function(e) NULL)
      } else {
        tryCatch(load_papelera(), error = function(e) NULL)
      }
      if (!is.null(papelera) && nrow(papelera)) {
        pap_keys <- papelera[papelera[["ledger"]] == ledger,
                             c("Empresa","Moneda","Documento"), drop = FALSE]
        if (nrow(pap_keys))
          df <- dplyr::anti_join(df, pap_keys, by = c("Empresa","Moneda","Documento"))
      }

      if (length(emp) && "Empresa" %in% names(df)) {
        df_filtered <- dplyr::filter(df, Empresa %in% emp)
        if (nrow(df_filtered) == 0 && nrow(df) > 0) {
          # Filter eliminates everything — empresa name in snapshot doesn't match
          # the filter list. Show nothing rather than silently showing all data.
          warning("[DF_COMBINED] empresa filter ", paste(emp, collapse = ","),
                  " matched 0 of ", nrow(df), " rows — check empresas.rds vs snapshot")
          showNotification(
            "El filtro de Empresa no coincide con los datos cargados. Verifica la lista de empresas.",
            type = "warning", duration = 5)
        }
        df <- df_filtered
      }

      # ── Mark confirmed invoices ─────────────────────────────────────────────
      # Reads from TWO sources (both are checked for full retroactive coverage):
      #   1. conciliacion_rv  — confirmations made before the bancos wire-cut
      #   2. bancos_confirmados — confirmations made after the wire-cut
      # isolate() is intentionally NOT used here so the calendar re-renders
      # immediately when a confirmation happens in Agenda de Hoy.
      tipo_val <- if (ledger == "AR") "cobro" else "pago"
      df[["confirmed"]] <- FALSE

      # Source 1: conciliacion_rv (legacy path)
      conc_rv <- tryCatch(shared$conciliacion_rv(), error = function(e) NULL)
      if (!is.null(conc_rv) && nrow(conc_rv)) {
        conc_keys <- unique(conc_rv[conc_rv[["tipo"]] == tipo_val,
                                    c("Empresa","Moneda","Documento"), drop = FALSE])
        if (nrow(conc_keys)) {
          match_key <- paste(df[["Empresa"]], df[["Moneda"]], df[["Documento"]])
          conf_key  <- paste(conc_keys[["Empresa"]], conc_keys[["Moneda"]], conc_keys[["Documento"]])
          df[["confirmed"]] <- df[["confirmed"]] | (match_key %in% conf_key)
        }
      }

      # Source 2: bancos_confirmados (current path after wire-cut)
      conf_db <- tryCatch(shared$bancos_confirmados(), error = function(e) NULL)
      if (!is.null(conf_db) && nrow(conf_db)) {
        conf_active <- conf_db[!isTRUE(conf_db[["eliminado"]]) &
                               conf_db[["tipo"]] == tipo_val, , drop = FALSE]
        if (nrow(conf_active)) {
          bc_keys <- unique(conf_active[, c("empresa","documento","moneda"),
                                        drop = FALSE])
          # bancos_confirmados uses lowercase column names — normalize to match df.
          # Include Moneda so a USD manual entry is not falsely matched to an MXN
          # bancos_confirmados row sharing only Empresa+Documento.
          match_key <- paste(df[["Empresa"]], df[["Documento"]], df[["Moneda"]])
          conf_key  <- paste(bc_keys[["empresa"]], bc_keys[["documento"]], bc_keys[["moneda"]])
          df[["confirmed"]] <- df[["confirmed"]] | (match_key %in% conf_key)
        }
      }

      # ── Handle manual entries differently from SAP entries ──────────────────
      # SAP rows: keep in df with confirmed=TRUE → calendar excludes from totals,
      #           day modal shows with strikethrough
      # Manual rows: remove entirely from df → disappear from calendar and modal
      if ("source" %in% names(df) && any(df[["confirmed"]] & df[["source"]] == "manual")) {
        df <- df[!(df[["confirmed"]] & df[["source"]] == "manual"), , drop = FALSE]
      }

      message("[DF_COMBINED] complete ledger=", ledger,
              " final_rows=", nrow(df),
              " total ", round((proc.time() - t0_comb)[["elapsed"]], 2), "s")
      df
    })

    # ── Calendar-ready aggregation ───────────────────────────────────────────
    df_calendar <- reactive({
      t0_cal <- proc.time()
      message("[DF_CALENDAR] start ledger=", ledger)
      df   <- req(df_combined())
      message("[DF_CALENDAR] got df_combined ledger=", ledger,
              " in ", round((proc.time() - t0_cal)[["elapsed"]], 2), "s")
      amt  <- shared$amount_col()
      mode <- shared$ic_mode[[ledger]]()
      codes   <- build_ic_fullcodes(shared$interco_v2(), ledger)
      ic_rfcs <- unique(toupper(trimws(unname(shared$interco_v2()$rfcs %||% character()))))
      ic_rfcs <- ic_rfcs[nzchar(ic_rfcs)]

      df <- apply_ic_filter(df, mode = mode,
                            code_col = config$code_col,
                            ic_codes = codes,
                            ic_rfcs  = ic_rfcs)
      result <- to_calendar_data(df, amount_col = amt)
      message("[DF_CALENDAR] complete ledger=", ledger,
              " in ", round((proc.time() - t0_cal)[["elapsed"]], 2), "s")
      result
    })

    # ── Loading overlay ──────────────────────────────────────────────────────
    output$loading_overlay <- renderUI({
      if (!isTRUE(shared$sap_loading())) return(NULL)
      div(class = "sap-loading-overlay",
        tags$div(class = "spinner-border text-primary",
                 role = "status",
                 tags$span(class = "visually-hidden", "Cargando...")),
        tags$span("Conectando a SAP...")
      )
    })

    # ── Currency choices ─────────────────────────────────────────────────────
    # priority = 1: run BEFORE all default-priority (0) observers and outputs.
    # Guarantees AR CUR_CHOICES and AP CUR_CHOICES both execute back-to-back
    # before any calendar renderUI or app-level output (currency_ui, empresa_ui)
    # gets a chance to run.  Without this, output$currency_ui (registered before
    # the modules, lower observer ID) jumped the queue between AR and AP
    # CUR_CHOICES after AR set currency_choices$AR(), causing 45-91s gaps.
    observe({
      # Report two elapsed spans so we can pinpoint the bottleneck:
      #   since_handoff — time from the previous ledger's CUR_CHOICES handoff
      #                   (AR cal renders in this gap, so long = cal is slow)
      #   since_cal     — time from the last CAL_HTML render done
      #                   (long = Shiny scheduler latency or a blocking observer)
      t_now <- proc.time()
      prev_hoff <- .GlobalEnv$.t_last_curchoices_done
      prev_cal  <- .GlobalEnv$.t_last_cal_done
      since_handoff <- if (!is.null(prev_hoff) && !identical(prev_hoff$ledger, ledger))
        sprintf("  [+%.1fs since %s handoff]", (t_now - prev_hoff$t)[["elapsed"]], prev_hoff$ledger)
      else ""
      since_cal <- if (!is.null(prev_cal))
        sprintf("  [+%.1fs since last cal]", (t_now - prev_cal)[["elapsed"]])
      else ""
      message("[CUR_CHOICES] start ledger=", ledger, since_handoff, since_cal)
      t0_cur <- proc.time()
      d <- df_combined()
      manual_raw <- shared$manual_inv()
      manual <- if (!is.null(manual_raw) && nrow(manual_raw)) {
        dplyr::filter(manual_raw, .data$ledger == !!ledger)
      } else {
        data.frame(Moneda = character(), stringsAsFactors = FALSE)
      }
      raw_curs <- unique(c(
        if (!is.null(d) && nrow(d) && "Moneda" %in% names(d))
          d$Moneda else character(),
        if (nrow(manual)) manual$Moneda else character()
      ))
      # MXN first, USD second, rest alphabetically — ensures selector defaults
      # to MXN even when old snapshots contain other currencies (e.g. EUR).
      priority <- c("MXN", "USD")
      data_currencies <- c(
        intersect(priority, raw_curs),
        sort(setdiff(raw_curs, priority))
      )
      choices <- if (length(data_currencies)) data_currencies else c("MXN", "USD")
      shared$currency_choices[[ledger]](choices)
      message("[CUR_CHOICES] complete ledger=", ledger,
              " in ", round((proc.time() - t0_cur)[["elapsed"]], 2), "s")
      # Stamp so the next CUR_CHOICES observer can report how long Shiny
      # spent between this handoff and its own start (AR cal runs in that gap).
      .GlobalEnv$.t_last_curchoices_done <- list(ledger = ledger, t = proc.time())
      message("[CUR_CHOICES] → handing off to Shiny scheduler — ", ledger, " calendar renderUI pending")
    }, priority = 1)

    # ── Tag-day map (separate reactive — does NOT re-run on currency change) ──
    # Only invalidates when tags_db, df_combined, or the viewed month changes.
    # Previously this was embedded inside renderUI so it re-ran on every
    # currency switch, empresa toggle, and staged-item update.
    tags_day_map_rv <- reactive({
      t0_tags <- proc.time()
      message("[TAGS_MAP] start ledger=", ledger)
      tdb    <- shared$tags_db()
      df_full <- df_combined()
      mstart  <- shared$month_val[[ledger]]()
      req(mstart)
      mstart <- lubridate::floor_date(as.Date(mstart), "month")
      result <- tryCatch({
        if (is.null(tdb) || !nrow(tdb) || is.null(df_full) || !nrow(df_full)) return(list())
        me <- as.Date(lubridate::ceiling_date(mstart, "month") - lubridate::days(1))
        # Pre-compute Fecha once — avoids calling as.Date(FechaEff) twice
        # (once in filter, once in group_by) on the full df_full.
        df_month <- df_full |>
          dplyr::mutate(Fecha = as.Date(FechaEff)) |>
          dplyr::filter(Fecha >= mstart, Fecha <= me)
        if (!nrow(df_month)) return(list())
        day_tags <- df_month |>
          dplyr::left_join(
            tdb |> dplyr::filter(.data$ledger == !!ledger) |>
              dplyr::group_by(Empresa, Moneda, Documento) |>
              dplyr::summarise(
                has_urg = "urgent"    %in% tag,
                has_imp = "important" %in% tag,
                .groups = "drop"),
            by = c("Empresa", "Moneda", "Documento")
          ) |>
          dplyr::group_by(Fecha) |>
          dplyr::summarise(
            day_tag = dplyr::case_when(
              any(has_urg %in% TRUE) & any(has_imp %in% TRUE) ~ "both",
              any(has_urg %in% TRUE)                           ~ "urgent",
              any(has_imp %in% TRUE)                           ~ "important",
              TRUE                                             ~ ""
            ),
            .groups = "drop"
          ) |>
          dplyr::filter(nzchar(day_tag))
        setNames(as.list(day_tags$day_tag), as.character(day_tags$Fecha))
      }, error = function(e) list())
      message("[TAGS_MAP] complete ledger=", ledger,
              " in ", round((proc.time() - t0_tags)[["elapsed"]], 2), "s")
      result
    })

    # ── Staged keys reactive (separate — isolates pagar_hoy_db from calendar) ─
    staged_keys_rv <- reactive({
      tryCatch({
        ph <- shared$pagar_hoy_db()
        if (is.null(ph) || !nrow(ph)) return(NULL)
        ph |>
          dplyr::filter(.data$ledger == !!ledger, status == "pending") |>
          dplyr::select(Empresa, Moneda, Documento, FechaVenc)
      }, error = function(e) NULL)
    })

    # ── Calendar render ──────────────────────────────────────────────────────
    output$calendar <- renderUI({
      tryCatch({
      mstart <- req(shared$month_val[[ledger]]())
      mstart <- lubridate::floor_date(as.Date(mstart), "month")

      # Resolve the effective display currency.
      # input$cur_sel is a renderUI dynamic input — it lags one browser
      # round-trip behind currency_choices when the selectInput is rebuilt.
      # During that gap reactive(input$cur_sel) returns NULL (or a stale
      # value), so we check currency_choices — which updates synchronously
      # in R — and fall back to its first entry when the selector value is
      # absent or not among the currently available options.
      available <- shared$currency_choices[[ledger]]()
      sel       <- shared$currency[[ledger]]()        # reactive(input$cur_sel)
      cur <- toupper(trimws(
        if (!is.null(sel) && nzchar(sel) && sel %in% available)
          sel
        else if (length(available))
          available[[1]]
        else
          "MXN"
      ))

      message("[CAL_RENDER] renderUI fired ledger=", ledger)
      d <- tryCatch(df_calendar(), error = function(e) {
        message("[DF_CALENDAR] CAUGHT ERROR ledger=", ledger, ": ", conditionMessage(e))
        NULL
      })

      # Tags and staged keys come from dedicated reactives — cached by Shiny
      tags_day_map   <- tryCatch(tags_day_map_rv(),  error = function(e) list())
      staged_keys_all <- tryCatch(staged_keys_rv(),  error = function(e) NULL)

      # Filter staged keys down to the current currency for tile pills
      staged_keys_cur <- if (!is.null(staged_keys_all) && nrow(staged_keys_all)) {
        dplyr::filter(staged_keys_all,
                      toupper(trimws(Moneda)) == toupper(trimws(cur)))
      } else NULL

      empty_cal <- tibble::tibble(
        Fecha = as.Date(character()),
        Moneda = character(),
        Parte = character(),
        Importe = numeric()
      )

      data_in <- if (is.null(d) || !nrow(d)) {
        empty_cal
      } else {
        d_cur <- dplyr::filter(d, Moneda == cur)
        if (!nrow(d_cur)) empty_cal else d_cur
      }

      # ── Diagnostic banner when calendar would render empty ──────────────────
      # Visible to the user — helps pinpoint WHY there is no data without
      # needing to inspect the R console.
      diag_banner <- if (!nrow(data_in)) {
        sap_raw   <- tryCatch(shared$sap_data()[[ledger]], error = function(e) NULL)
        sap_rows  <- if (!is.null(sap_raw)) nrow(sap_raw) else 0L
        comb_rows <- tryCatch({
          dc <- df_combined()
          if (is.null(dc)) 0L else nrow(dc)
        }, error = function(e) -1L)

        reason <- if (sap_rows == 0L && comb_rows == 0L) {
          "Sin datos SAP cargados — reinicia la app o verifica tu conexión."
        } else if (comb_rows == 0L && sap_rows > 0L) {
          paste0("Datos SAP cargados (", sap_rows, " facturas) pero el filtro de empresa",
                 " las ocultó. Verifica los botones de empresa en el encabezado.")
        } else if (comb_rows > 0L) {
          # Data exists in df_combined but not in df_calendar (confirmed or wrong currency)
          all_curs <- tryCatch({
            dc <- df_combined()
            if (!is.null(dc) && "Moneda" %in% names(dc)) sort(unique(dc$Moneda)) else character()
          }, error = function(e) character())
          other_curs <- setdiff(toupper(all_curs), toupper(cur))
          if (length(other_curs)) {
            paste0("Sin facturas en ", cur, " para este mes.",
                   " Hay datos en: ", paste(other_curs, collapse = ", "), ".")
          } else {
            paste0("Todas las facturas en ", cur, " están confirmadas o no vencen en ",
                   format(mstart, "%B %Y"), ".")
          }
        } else {
          paste0("Sin facturas en ", cur, " para ", format(mstart, "%B %Y"), ".")
        }

        div(class = "alert alert-secondary py-2 px-3 mt-2",
            style = "font-size:0.82em; border-radius:6px;",
            tags$strong("Sin datos: "), reason,
            tags$span(class = "text-muted ms-2",
                      paste0("(SAP: ", sap_rows, " | combinado: ",
                             max(comb_rows, 0L), ")")))
      } else {
        NULL
      }

      t0 <- proc.time()
      message("[CAL_HTML] start render ledger=", ledger, " cur=", cur)
      cal_html <- calendar_html(
        data_due     = data_in,
        month_start  = mstart,
        currency     = cur,
        title_prefix = config$title_prefix,
        ledger       = ledger,
        input_id     = ns("cal_click"),
        tags_day     = tags_day_map,
        staged_keys  = staged_keys_cur
      )
      elapsed_cal <- round((proc.time() - t0)[["elapsed"]], 2)
      message("[CAL_HTML] render complete ledger=", ledger, " cur=", cur,
              " in ", elapsed_cal, "s")
      t_cal_done <- proc.time()
      # Stamp wall time so the next CUR_CHOICES can report how long Shiny
      # spent between this render and the next observer run.
      .GlobalEnv$.t_last_cal_done <- t_cal_done

      # Report full journey from last SAP_DATA write and from last CUR_CHOICES
      # handoff — reveals whether the gap was in the scheduler or the render itself.
      t_sap  <- .GlobalEnv$.t_last_sap_data_write
      t_hoff <- .GlobalEnv$.t_last_curchoices_done
      since_sap  <- if (!is.null(t_sap))
        sprintf("  [+%.1fs since SAP_DATA]", (t_cal_done - t_sap)[["elapsed"]])
      else ""
      since_hoff <- if (!is.null(t_hoff))
        sprintf("  [+%.1fs since %s handoff]",
                (t_cal_done - t_hoff$t)[["elapsed"]], t_hoff$ledger)
      else ""
      message("[CAL_HTML] → render done, Shiny scheduler taking over",
              since_sap, since_hoff)

      tagList(
        tags$div(
          class = "cal-outer",
          style = "height:100%; overflow-y:auto; padding: 12px 16px 80px;",
          cal_html,
          diag_banner
        )
      )
      }, error = function(e) {
        message("[CAL_HTML] ERROR ledger=", ledger, ": ", e$message)
        div(class = "alert alert-warning",
            paste("Error al renderizar calendario:", e$message))
      })
    })

    # ── Modal context — set once per click, read by all inner observers ──────
    # ALL modal rendering goes through one observeEvent(modal_ctx()) so there
    # is exactly one code path that ever calls showModal — eliminating doubles.
    modal_ctx     <- reactiveVal(NULL)
    cart_expanded <- reactiveVal(integer(0))   # which group rows are expanded

    # Rebuild ctx$detail from the current df_combined() snapshot so that
    # re-renders after a save show fresh data, not the snapshot from click time.
    .refresh_ctx_detail <- function(ctx) {
      if (is.null(ctx)) return(ctx)
      df <- tryCatch(df_combined(), error = function(e) NULL)
      if (is.null(df) || !nrow(df)) return(modifyList(ctx, list(nonce = runif(1))))
      sel_date <- ctx$sel_date
      cur      <- ctx$cur
      amt      <- ctx$amt %||% shared$amount_col()
      mode     <- shared$ic_mode[[ledger]]()
      codes    <- build_ic_fullcodes(shared$interco_v2(), ledger)
      ic_rfcs  <- unique(toupper(trimws(unname(shared$interco_v2()$rfcs %||% character()))))
      ic_rfcs  <- ic_rfcs[nzchar(ic_rfcs)]
      new_detail <- tryCatch({
        d <- df |>
          dplyr::filter(as.Date(.data$FechaEff) == sel_date,
                        toupper(trimws(.data$Moneda)) == cur) |>
          dplyr::mutate(
            Importe = {
              raw_col <- if (amt %in% names(df)) .data[[amt]]
                         else if ("Importe" %in% names(df)) .data[["Importe"]]
                         else 0
              abs(tidyr::replace_na(raw_col, 0))
            }
          )
        d <- apply_ic_filter(d, mode, config$code_col, codes, ic_rfcs = ic_rfcs)
        d <- add_tag_labels(d, shared$tags_db(), ledger)
        keep <- intersect(
          c("Empresa","Moneda","Documento","Factura","Parte","Importe",
            "FechaEff","FechaVenc_Original","FechaVenc_Proyectada",
            "Movida","Etiqueta","source","Tipo","Codigo","notas","confirmed",
            "has_sap_override","provision_id"),
          names(d)
        )
        as.data.frame(d)[, keep, drop = FALSE]
      }, error = function(e) ctx$detail)
      modifyList(ctx, list(detail = new_detail, nonce = runif(1)))
    }

    # Single render gate — the ONLY place showModal is called
    observeEvent(modal_ctx(), {
      ctx <- modal_ctx()
      req(ctx)
      cart_expanded(integer(0))
      tryCatch(
        .render_day_modal(ctx, input, output, session, ns, ledger, config, shared),
        error = function(e) {
          message("[modal error] ", conditionMessage(e))
          message("[modal cols] ", paste(names(ctx$detail), collapse = ", "))
          showNotification(paste("Error al abrir ventana:", conditionMessage(e)),
                           type = "error", duration = 10)
        }
      )
    }, ignoreNULL = TRUE)

    # ── Click handler ────────────────────────────────────────────────────────
    observeEvent(input$cal_click, {
      click <- input$cal_click
      req(click)

      sel_date <- tryCatch(as.Date(click$date), error = function(e) NULL)
      if (is.null(sel_date) || is.na(sel_date)) return()

      df <- tryCatch(df_combined(), error = function(e) NULL)
      if (is.null(df) || !nrow(df)) {
        showModal(modalDialog(
          title     = paste0("Sin movimientos – ", format(sel_date, "%d %b %Y")),
          "No hay facturas para este día (sin datos SAP cargados aún).",
          easyClose = TRUE, footer = modalButton("Cerrar")
        ))
        return()
      }

      cur   <- toupper(trimws(shared$currency[[ledger]]()))
      amt   <- shared$amount_col()
      mode  <- shared$ic_mode[[ledger]]()
      codes   <- build_ic_fullcodes(shared$interco_v2(), ledger)
      ic_rfcs <- unique(toupper(trimws(unname(shared$interco_v2()$rfcs %||% character()))))
      ic_rfcs <- ic_rfcs[nzchar(ic_rfcs)]

      detail <- df |>
        dplyr::filter(as.Date(FechaEff) == sel_date,
                      toupper(trimws(Moneda)) == cur) |>
        dplyr::mutate(
          Importe = {
            raw_col <- if (amt %in% names(df)) .data[[amt]]
                       else if ("Importe" %in% names(df)) .data[["Importe"]]
                       else 0
            abs(tidyr::replace_na(raw_col, 0))
          }
        )

      detail <- apply_ic_filter(detail, mode, config$code_col, codes, ic_rfcs = ic_rfcs)

      if (!nrow(detail)) {
        showModal(modalDialog(
          title     = paste0("Sin movimientos – ", format(sel_date, "%d %b %Y"),
                             " – ", cur),
          "No hay facturas para este día con los filtros actuales.",
          easyClose = TRUE, footer = modalButton("Cerrar")
        ))
        return()
      }

      detail <- add_tag_labels(detail, shared$tags_db(), ledger)

      # Narrow to only the columns the modal uses, then coerce to a plain
      # data.frame.  Tibbles with non-syntactic column names (e.g. "Abono futuro",
      # ".row_id") poison dplyr's bare-name resolution on Windows / dplyr 1.1+
      # even for completely unrelated columns like Parte.  A plain data.frame
      # uses standard [[]] evaluation and avoids the problem entirely.
      keep <- intersect(
        c("Empresa","Moneda","Documento","Factura","Parte","Importe",
          "FechaEff","FechaVenc_Original","FechaVenc_Proyectada",
          "Movida","Etiqueta","source","Tipo","Codigo","notas","confirmed",
          "has_sap_override","provision_id"),
        names(detail)
      )
      detail <- as.data.frame(detail)[, keep, drop = FALSE]

      # nonce ensures modal_ctx() always changes even when the same day is
      # clicked twice — without it Shiny skips the observeEvent(modal_ctx())
      # because the value looks identical to the previous click
      modal_ctx(list(
        detail   = detail,
        sel_date = sel_date,
        cur      = cur,
        amt      = amt,
        audit    = isTRUE(shared$audit_mode()),
        nonce    = runif(1)
      ))
    })

    # ── Audit toggle — only re-render on a real user change ──────────────────
    # ignoreInit=TRUE alone isn't enough because showModal() itself sets the
    # checkbox value, re-triggering this. We compare to the current ctx value.
    observeEvent(input$audit_toggle, {
      ctx <- modal_ctx()
      req(ctx)
      new_val <- isTRUE(input$audit_toggle)
      if (identical(new_val, ctx$audit)) return()   # no real change — ignore
      shared$audit_mode(new_val)
      removeModal()
      modal_ctx(modifyList(ctx, list(audit = new_val)))
      # modal_ctx change triggers the render gate — do NOT call .render_day_modal
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ── Stage-all button (registered once, reads current ctx) ────────────────
    observeEvent(input$stage_all, {
      ctx <- modal_ctx()
      req(ctx)
      detail <- ctx$detail
      mask   <- !is.na(detail[["Documento"]])
      d      <- pasivos_filter_out_provisions(detail[mask, , drop = FALSE])
      inv_keys <- unique(d[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE])
      n <- nrow(inv_keys)
      if (n == 0) {
        showNotification("No hay items para enviar (sólo provisiones en este día).",
                         type = "message", duration = 3)
        return()
      }
      detail_lu <- .fresh_lu(
        inv_keys, ctx$amt,
        unique(d[, c("Empresa","Moneda","Documento","Parte","Codigo","Importe","FechaEff"), drop = FALSE]))
      new_rows <- merge(inv_keys, detail_lu, by = c("Empresa","Moneda","Documento","Importe"))
      new_rows[["id"]]        <- vapply(seq_len(nrow(new_rows)), function(x) uuid::UUIDgenerate(), character(1))
      new_rows[["ledger"]]    <- ledger
      new_rows[["tipo_item"]] <- "factura"
      new_rows[["FechaVenc"]] <- as.Date(new_rows[["FechaEff"]])
      new_rows[["staged_by"]] <- shared$current_user()
      new_rows[["staged_at"]] <- Sys.time()
      new_rows[["status"]]    <- "pending"
      new_rows <- new_rows[, c("id","ledger","Empresa","Moneda","Documento",
                                "Parte","Codigo","tipo_item","Importe","FechaVenc","staged_by","staged_at","status"), drop = FALSE]
      updated <- upsert_pagar_hoy(shared$pagar_hoy_db() %||% load_pagar_hoy(), new_rows,
                                  keys = c("ledger","Empresa","Moneda","Documento","Importe"))
      shared$pagar_hoy_db(updated)
      save_pagar_hoy(updated, shared$current_user())
      lbl <- if (ledger == "AR") "Agenda del d\u00eda (Cobros)" else "Agenda del d\u00eda (Pagos)"
      emps <- paste(unique(new_rows[["Empresa"]]), collapse = ", ")
      showNotification(
        paste0("\u2713 ", nrow(new_rows), " factura(s) enviadas a ", lbl,
               " \u2014 ", emps, "."),
        type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # ── Stage selection (audit or summary mode) ───────────────────────────────
    # In audit mode  → .audit_sel_to_keys()   : row = individual invoice
    # In summary mode → .summary_sel_to_keys() : row = Parte group (all invoices)
    observeEvent(input$stage_sel, {
      ctx <- modal_ctx()
      req(ctx)
      detail <- ctx$detail
      tbl    <- .detail_to_audit_tbl(detail)
      sel    <- input[["modal_tbl_rows_selected"]] %||% integer(0)
      if (!length(sel)) {
        showNotification("Selecciona al menos una factura.", type = "warning")
        return()
      }
      keys <- if (isTRUE(ctx$audit)) {
        .audit_sel_to_keys(tbl, sel, detail)
      } else {
        .summary_sel_to_keys(sel, detail)
      }
      keys <- pasivos_filter_out_provisions(keys)
      if (!nrow(keys)) {
        showNotification("No se encontraron facturas.", type = "warning")
        return()
      }
      detail_lu <- .fresh_lu(
        keys, ctx$amt,
        unique(pasivos_filter_out_provisions(detail)[, c("Empresa","Moneda","Documento","Parte","Codigo","Importe","FechaEff"), drop = FALSE]))
      new_rows  <- merge(keys, detail_lu, by = c("Empresa","Moneda","Documento","Importe"))
      new_rows[["id"]]        <- vapply(seq_len(nrow(new_rows)), function(x) uuid::UUIDgenerate(), character(1))
      new_rows[["ledger"]]    <- ledger
      new_rows[["tipo_item"]] <- "factura"
      new_rows[["FechaVenc"]] <- as.Date(new_rows[["FechaEff"]])
      new_rows[["staged_by"]] <- shared$current_user()
      new_rows[["staged_at"]] <- Sys.time()
      new_rows[["status"]]    <- "pending"
      new_rows <- new_rows[, c("id","ledger","Empresa","Moneda","Documento",
                                "Parte","Codigo","tipo_item","Importe","FechaVenc","staged_by","staged_at","status"), drop = FALSE]
      updated <- upsert_pagar_hoy(shared$pagar_hoy_db() %||% load_pagar_hoy(), new_rows,
                                  keys = c("ledger","Empresa","Moneda","Documento","Importe"))
      shared$pagar_hoy_db(updated)
      save_pagar_hoy(updated, shared$current_user())
      lbl <- if (ledger == "AR") "Agenda del d\u00eda (Cobros)" else "Agenda del d\u00eda (Pagos)"
      emps <- paste(unique(new_rows[["Empresa"]]), collapse = ", ")
      showNotification(
        paste0("\u2713 ", nrow(new_rows), " factura(s) enviadas a ", lbl,
               " \u2014 ", emps, "."),
        type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # ── Raw data diagnostic viewer ────────────────────────────────────────────
    observeEvent(input$show_raw, {
      ctx <- modal_ctx()
      req(ctx)
      d <- ctx$detail

      # Pull in the widest possible set of raw columns from sap_data for this day
      raw <- shared$sap_data()[[ledger]]
      raw_day <- if (!is.null(raw) && nrow(raw)) {
        raw_filt <- raw[!is.na(raw[["Fecha de vencimiento"]]) &
                        as.Date(raw[["Fecha de vencimiento"]]) == ctx$sel_date, ,
                        drop = FALSE]
        # Also match empresa filter
        emp <- shared$empresa_sel()
        if (length(emp) && "Empresa" %in% names(raw_filt))
          raw_filt <- raw_filt[raw_filt[["Empresa"]] %in% emp, , drop = FALSE]
        raw_filt
      } else NULL

      showModal(modalDialog(
        title = paste0("\U0001f50d Datos crudos — ", format(ctx$sel_date, "%d %b %Y"),
                       " — ", ctx$cur),
        size  = "xl", easyClose = TRUE,
        footer = tagList(
          tags$small(class = "text-muted me-auto",
            paste0(nrow(d), " filas en detail | ",
                   if (!is.null(raw_day)) nrow(raw_day) else "?",
                   " filas en SAP raw para este día")
          ),
          modalButton("Cerrar")
        ),
        tagList(
          tags$h6("Campos procesados (moneda y amount_col aplicados)", class = "text-muted mb-1"),
          DT::dataTableOutput(ns("raw_detail_tbl")),
          tags$hr(),
          if (!is.null(raw_day) && nrow(raw_day)) tagList(
            tags$h6("SAP raw — todos los campos originales (este d\u00eda, todas las monedas)",
                    class = "text-muted mb-1"),
            DT::dataTableOutput(ns("raw_sap_tbl"))
          ) else NULL
        )
      ))

      output[[ns("raw_detail_tbl")]] <- DT::renderDataTable({
        # Priority columns first, then all remaining columns
        pri  <- intersect(c("Empresa","Moneda","Documento","Factura","Parte","Importe","source"), names(d))
        rest <- setdiff(names(d), pri)
        DT::datatable(
          d[, c(pri, rest), drop = FALSE],
          rownames   = FALSE, escape = FALSE,
          extensions = "Buttons",
          options    = list(
            pageLength = 20, scrollX = TRUE,
            dom     = "Bfrtip",
            buttons = list(list(extend = "csv", text = "\u2B07 CSV"),
                           list(extend = "excel", text = "\u2B07 Excel"))
          )
        )
      }, server = FALSE)

      output[[ns("raw_sap_tbl")]] <- DT::renderDataTable({
        req(!is.null(raw_day) && nrow(raw_day))
        # Show ALL SAP columns — priority columns first
        pri  <- intersect(c("Empresa","Moneda","Documento","Parte","Fecha de vencimiento"), names(raw_day))
        rest <- setdiff(names(raw_day), pri)
        DT::datatable(
          raw_day[, c(pri, rest), drop = FALSE],
          rownames   = FALSE, escape = FALSE,
          extensions = "Buttons",
          options    = list(
            pageLength = 20, scrollX = TRUE,
            dom     = "Bfrtip",
            buttons = list(list(extend = "csv", text = "\u2B07 CSV"),
                           list(extend = "excel", text = "\u2B07 Excel"))
          )
        )
      }, server = FALSE)
    }, ignoreInit = TRUE)

    # ── Cart buttons — up to 50 slots registered once at module init ─────────
    MAX_CART_ROWS <- 50L
    lapply(seq_len(MAX_CART_ROWS), function(i) {
      observeEvent(input[[paste0("cart_", i)]], {
        ctx <- modal_ctx()
        req(ctx)
        detail  <- ctx$detail
        grp_agg <- session$userData[[ns("cart_grp_snapshot")]]
        if (is.null(grp_agg) || i > nrow(grp_agg)) return()
        row_e   <- grp_agg[["Empresa"]][i]
        row_p   <- grp_agg[["Parte"]][i]
        mask     <- detail[["Empresa"]] == row_e & detail[["Parte"]] == row_p & !is.na(detail[["Documento"]])
        inv_keys <- pasivos_filter_out_provisions(
          unique(detail[mask, c("Empresa","Moneda","Documento","Importe",
                                if ("source" %in% names(detail)) "source" else character()), drop = FALSE])
        )
        inv_keys <- inv_keys[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE]
        if (nrow(inv_keys) == 0L) {
          showNotification(
            "No hay comprobantes que agregar; las provisiones se convierten con ⚡.",
            type = "warning", duration = 4)
          return()
        }
        ph_now  <- shared$pagar_hoy_db()
        already <- if (!is.null(ph_now) && nrow(ph_now)) {
          ph_sub <- ph_now[ph_now[["ledger"]] == ledger, , drop = FALSE]
          nrow(merge(ph_sub[, c("Empresa","Moneda","Documento","Importe"), drop=FALSE], inv_keys, by = c("Empresa","Moneda","Documento","Importe")))
        } else 0L
        if (already > 0L) {
          upd_keys <- cbind(inv_keys, ledger = ledger, stringsAsFactors = FALSE)
          updated <- unstage_pagar_hoy(ph_now, upd_keys, keys = c("ledger","Empresa","Moneda","Documento","Importe"))
          shared$pagar_hoy_db(updated); save_pagar_hoy(updated, shared$current_user())
          showNotification("Quitado de la Agenda del d\u00eda.", type = "message", duration = 2)
        } else {
          lbl_tp <- if (ledger == "AR") "cobro" else "pago"
          total_imp <- sum(detail[mask, "Importe"], na.rm = TRUE)
          cur_lbl   <- if (nrow(inv_keys)) detail[mask, "Moneda"][1] else ""
          detail_lu <- .fresh_lu(
            inv_keys,
            ctx$amt,
            unique(detail[, c("Empresa","Moneda","Documento","Parte","Codigo","Importe","FechaEff"), drop=FALSE]))
          new_rows  <- merge(inv_keys, detail_lu, by = c("Empresa","Moneda","Documento","Importe"))
          new_rows[["id"]]        <- vapply(seq_len(nrow(new_rows)), function(x) uuid::UUIDgenerate(), character(1))
          new_rows[["ledger"]]    <- ledger
          new_rows[["tipo_item"]] <- "factura"
          new_rows[["FechaVenc"]] <- as.Date(new_rows[["FechaEff"]])
          new_rows[["staged_by"]] <- shared$current_user()
          new_rows[["staged_at"]] <- Sys.time()
          new_rows[["status"]]    <- "pending"
          new_rows <- new_rows[, c("id","ledger","Empresa","Moneda","Documento",
                                    "Parte","Codigo","tipo_item","Importe","FechaVenc","staged_by","staged_at","status"), drop = FALSE]
          updated <- upsert_pagar_hoy(shared$pagar_hoy_db() %||% load_pagar_hoy(), new_rows,
                                    keys = c("ledger","Empresa","Moneda","Documento","Importe"))
          shared$pagar_hoy_db(updated); save_pagar_hoy(updated, shared$current_user())
          lbl_agenda <- if (ledger == "AR") "Agenda del d\u00eda (Cobros)" else "Agenda del d\u00eda (Pagos)"
          showNotification(
            paste0("\u2713 ", nrow(inv_keys), " factura(s) de ", row_p,
                   " enviadas a ", lbl_agenda,
                   " \u2014 ", cur_lbl, " ", fmt_money(total_imp), "."),
            type = "message", duration = 3)
        }
      }, ignoreInit = TRUE)
    })

    # confirm_cart_N observers removed — cart buttons now stage immediately
    # without a confirmation modal; a bottom notification confirms the action.

    # ── Expand toggle — event delegation ─────────────────────────────────────
    # JS fires ns("cart_expand_click") with {i, scrollTop, nonce}.
    # Single observer replaces 50 pre-registered actionButton observers.
    # No actionButton = no click-count replay on renderUI re-render.
    observeEvent(input$cart_expand_click, {
      click <- input$cart_expand_click
      req(click)
      i   <- as.integer(click$i %||% 0L)
      top <- as.integer(click$scrollTop %||% 0L)
      if (i < 1L) return()
      exp <- cart_expanded()
      cart_expanded(if (i %in% exp) setdiff(exp, i) else union(exp, i))
      # Restore modal scroll position after renderUI settles
      session$sendCustomMessage("restoreModalScroll", list(top = top))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ── Per-invoice buttons — event delegation ────────────────────────────────
    # Single observer replaces 50×200=10,000 pre-registered observers.
    # JS fires ns("cart_inv_click") with {i, j, nonce} payload.
    observeEvent(input$cart_inv_click, {
      click <- input$cart_inv_click
      req(click)
      i <- as.integer(click$i); j <- as.integer(click$j)
      ctx <- modal_ctx(); req(ctx)
      detail  <- ctx$detail
      grp_agg <- aggregate(Importe ~ Empresa + Parte, data = detail, FUN = sum, na.rm = TRUE)
      grp_agg <- grp_agg[order(-grp_agg[["Importe"]]), ]
      if (i > nrow(grp_agg)) return()
      row_e <- grp_agg[["Empresa"]][i]
      row_p <- grp_agg[["Parte"]][i]
      inv_mask <- detail[["Empresa"]] == row_e &
                  detail[["Parte"]]   == row_p &
                  !is.na(detail[["Documento"]])
      inv_rows_raw <- unique(detail[inv_mask,
        intersect(c("Empresa","Moneda","Documento","Parte","Codigo","Importe","FechaEff","source"),
                  names(detail)), drop=FALSE])
      inv_rows <- pasivos_filter_out_provisions(inv_rows_raw)
      inv_rows <- inv_rows[order(-inv_rows[["Importe"]]), ]
      if (j > nrow(inv_rows)) return()
      inv_key  <- inv_rows[j, c("Empresa","Moneda","Documento","Importe"), drop=FALSE]
      ph_now   <- shared$pagar_hoy_db()
      already  <- if (!is.null(ph_now) && nrow(ph_now)) {
        ph_sub <- ph_now[ph_now[["ledger"]] == ledger, , drop=FALSE]
        nrow(merge(ph_sub[, c("Empresa","Moneda","Documento","Importe"), drop=FALSE],
                   inv_key, by=c("Empresa","Moneda","Documento","Importe")))
      } else 0L
      if (already > 0L) {
        upd_keys <- cbind(inv_key, ledger=ledger, stringsAsFactors=FALSE)
        updated  <- unstage_pagar_hoy(ph_now, upd_keys, keys = c("ledger","Empresa","Moneda","Documento","Importe"))
        shared$pagar_hoy_db(updated); save_pagar_hoy(updated, shared$current_user())
        showNotification("Quitado de la Agenda.", type="message", duration=2)
      } else {
        one <- inv_rows[j, , drop=FALSE]
        new_row <- data.frame(
          id         = uuid::UUIDgenerate(),
          ledger     = ledger,
          Empresa    = one[["Empresa"]],
          Moneda     = one[["Moneda"]],
          Documento  = one[["Documento"]],
          Parte      = one[["Parte"]],
          Codigo     = one[["Codigo"]] %||% NA_character_,
          tipo_item  = "factura",
          Importe    = one[["Importe"]],
          FechaVenc  = as.Date(one[["FechaEff"]]),
          staged_by  = shared$current_user(),
          staged_at  = Sys.time(),
          status     = "pending",
          stringsAsFactors = FALSE
        )
        updated <- upsert_pagar_hoy(ph_now %||% load_pagar_hoy(), new_row,
                                  keys = c("ledger","Empresa","Moneda","Documento","Importe"))
        shared$pagar_hoy_db(updated); save_pagar_hoy(updated, shared$current_user())
        showNotification(
          paste0("Factura ", one[["Documento"]], " enviada a la Agenda."),
          type="message", duration=2)
      }
    }, ignoreInit=TRUE, ignoreNULL=TRUE)

    # helper: re-read Importe/Parte/FechaEff from current df_combined for a set of keys.
    # Called at staging time so edits made after the modal opened are reflected.
    # Falls back to `fallback_lu` (a pre-sliced data.frame) when df_combined fails.
    .fresh_lu <- function(inv_keys, amt, fallback_lu) {
      df_now <- tryCatch(as.data.frame(df_combined()), error = function(e) NULL)
      if (!is.null(df_now) && nrow(df_now)) {
        raw_col <- if (amt %in% names(df_now)) df_now[[amt]]
                   else if ("Importe" %in% names(df_now)) df_now[["Importe"]]
                   else rep(0, nrow(df_now))
        df_now[["Importe"]]  <- abs(replace(raw_col, is.na(raw_col), 0))
        df_now[["FechaEff"]] <- as.Date(df_now[["FechaEff"]])
        key_now <- paste(df_now[["Empresa"]], df_now[["Moneda"]], df_now[["Documento"]])
        key_inv <- paste(inv_keys[["Empresa"]], inv_keys[["Moneda"]], inv_keys[["Documento"]])
        lu <- unique(df_now[key_now %in% key_inv,
               c("Empresa","Moneda","Documento","Parte","Codigo","Importe","FechaEff"), drop = FALSE])
        if (nrow(lu)) return(lu)
      }
      fallback_lu
    }

    # helper: build audit tbl (base R only)
    .detail_to_audit_tbl <- function(detail) {
      d <- data.frame(
        Empresa   = detail[["Empresa"]],
        Documento = detail[["Documento"]],
        Referencia = if ("Factura" %in% names(detail)) detail[["Factura"]] else NA_character_,
        Parte     = detail[["Parte"]],
        Importe   = detail[["Importe"]],
        stringsAsFactors = FALSE
      )
      d[order(-d[["Importe"]]), ]
    }
    .audit_sel_to_keys <- function(tbl, sel, detail) {
      rows <- tbl[sel, , drop=FALSE]
      lu   <- unique(detail[, c("Empresa","Moneda","Documento","Importe"), drop=FALSE])
      unique(merge(rows[, c("Empresa","Documento","Importe"), drop=FALSE], lu, by=c("Empresa","Documento","Importe"))[, c("Empresa","Moneda","Documento","Importe"), drop=FALSE])
    }
    .summary_sel_to_keys <- function(sel, detail) {
      # grp rows are (Empresa, Parte) sorted by descending Importe — same order
      # as the DT table the user sees. Build the same ordering here.
      grp_agg <- aggregate(Importe ~ Empresa + Parte, data = detail,
                           FUN = sum, na.rm = TRUE)
      grp_agg <- grp_agg[order(-grp_agg[["Importe"]]), ]
      pairs   <- grp_agg[sel, , drop = FALSE]
      mask    <- paste(detail[["Empresa"]], detail[["Parte"]]) %in%
                 paste(pairs[["Empresa"]], pairs[["Parte"]]) &
                 !is.na(detail[["Documento"]])
      unique(detail[mask, c("Empresa","Moneda","Documento","Importe"), drop = FALSE])
    }

    # ── Move / restore buttons ────────────────────────────────────────────────
    observeEvent(input$do_move, {
      ctx <- modal_ctx(); req(ctx)
      new_date <- as.Date(input$move_to)
      if (is.na(new_date)) { showNotification("Elige una fecha v\u00e1lida.", type="warning"); return() }
      detail <- ctx$detail
      sel    <- input[["modal_tbl_rows_selected"]] %||% integer(0)
      if (!length(sel)) { showNotification("Selecciona al menos una fila/partida.", type="warning"); return() }
      keys <- if (ctx$audit) .audit_sel_to_keys(.detail_to_audit_tbl(detail), sel, detail) else .summary_sel_to_keys(sel, detail)
      if (!nrow(keys)) { showNotification("No se encontraron facturas.", type="warning"); return() }
      .apply_move(keys, new_date, ledger, shared)
      .sync_staged(keys, ledger, shared$pagar_hoy_db, new_date = new_date,
                   username = shared$current_user())
      showNotification(paste0(nrow(keys), " factura(s) movidas."), type="message", duration=2)
      removeModal()
    }, ignoreInit = TRUE)

    observeEvent(input$do_restore, {
      ctx <- modal_ctx(); req(ctx)
      detail <- ctx$detail
      sel    <- input[["modal_tbl_rows_selected"]] %||% integer(0)
      if (!length(sel)) { showNotification("Selecciona al menos una fila/partida.", type="warning"); return() }
      keys <- if (ctx$audit) .audit_sel_to_keys(.detail_to_audit_tbl(detail), sel, detail) else .summary_sel_to_keys(sel, detail)
      if (!nrow(keys)) { showNotification("No se encontraron facturas.", type="warning"); return() }
      .clear_moves(keys, ledger, shared)
      .sync_staged(keys, ledger, shared$pagar_hoy_db, detail = detail,
                   username = shared$current_user())
      showNotification("Fecha original restaurada.", type="message", duration=2)
      removeModal()
    }, ignoreInit = TRUE)

    # ── Delete selected rows ──────────────────────────────────────────────────
    observeEvent(input$do_delete, {
      ctx <- modal_ctx(); req(ctx)
      detail <- ctx$detail
      sel    <- input[["modal_tbl_rows_selected"]] %||% integer(0)
      if (!length(sel)) {
        showNotification("Selecciona al menos una factura para eliminar.", type = "warning")
        return()
      }
      # Always use audit key resolution — delete works at invoice level
      keys <- .audit_sel_to_keys(.detail_to_audit_tbl(detail), sel, detail)
      if (!nrow(keys)) {
        showNotification("No se encontraron facturas.", type = "warning")
        return()
      }
      n <- nrow(keys)

      # Confirm dialog
      showModal(modalDialog(
        title = tags$span(style = "color:#dc3545;", "\u26a0\ufe0f Confirmar eliminación"),
        size  = "s", easyClose = FALSE,
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirm_delete"), paste0("Eliminar ", n, " factura(s)"),
                       class = "btn btn-danger")
        ),
        tagList(
          tags$p(paste0("¿Eliminar ", n, " factura(s) seleccionada(s)?")),
          tags$ul(
            lapply(seq_len(min(n, 5)), function(i)
              tags$li(paste0(keys[["Empresa"]][i], " — ", keys[["Documento"]][i]))
            ),
            if (n > 5) tags$li(paste0("… y ", n - 5, " más")) else NULL
          ),
          tags$p(class = "text-muted small mt-2",
            "Las facturas de SAP quedarán ocultas. Las manuales se eliminarán permanentemente de la lista activa.",
            tags$br(),
            "Todas se guardan en la papelera y pueden recuperarse."
          )
        )
      ))

      # Store keys for the confirm observer
      session$userData[[paste0(ns("pending_delete_keys"))]] <- keys
      session$userData[[paste0(ns("pending_delete_ctx"))]]  <- ctx
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_delete, {
      keys <- session$userData[[paste0(ns("pending_delete_keys"))]]
      ctx  <- session$userData[[paste0(ns("pending_delete_ctx"))]]
      req(keys, ctx)

      detail <- ctx$detail
      papelera_df <- tryCatch(load_papelera(), error = function(e) .schema_papelera() |> dplyr::slice(0))

      # Get full detail rows matching the keys to archive
      rows_to_delete <- merge(
        detail,
        keys[, c("Empresa","Moneda","Documento","Importe"), drop=FALSE],
        by = c("Empresa", "Moneda", "Documento", "Importe")
      )

      # Add to papelera
      papelera_df <- add_to_papelera(papelera_df, rows_to_delete,
                                      ledger, deleted_by = shared$current_user())
      tryCatch(save_papelera(papelera_df),
               error = function(e) showNotification(
                 paste("No se pudo guardar papelera:", e$message), type = "warning"))
      # Update shared reactive so df_combined invalidates without another S3 read
      if (!is.null(shared$papelera_rv)) shared$papelera_rv(papelera_df)

      # For manual invoices: remove from manual_inv
      manual_keys <- rows_to_delete[rows_to_delete[["source"]] == "manual", , drop = FALSE]
      if (nrow(manual_keys) > 0 && !is.null(manual_keys[["id"]])) {
        m <- shared$manual_inv()
        for (mid in manual_keys[["id"]]) {
          m <- delete_manual(m, mid)
        }
        shared$manual_inv(m)
        tryCatch(save_manual(m),
                 error = function(e) showNotification(
                   paste("No se pudo guardar manual_inv:", e$message), type = "warning"))
      }

      # For SAP invoices: add a "hidden" move so they disappear from the calendar
      # We reuse the moves table with a special far-future date as a hide flag.
      # Alternatively handled in build_ledger_df via a hidden_keys list —
      # here we use the papelera presence to filter in df_combined.
      # For now: SAP invoices are hidden by storing them in papelera;
      # df_combined will anti_join against papelera keys.

      n_del <- nrow(rows_to_delete)
      removeModal()
      session$userData[[paste0(ns("pending_delete_keys"))]] <- NULL
      session$userData[[paste0(ns("pending_delete_ctx"))]]  <- NULL
      showNotification(paste0(n_del, " factura(s) enviadas a la papelera."),
                       type = "message", duration = 3)
    }, ignoreInit = TRUE)

    # ── Tag buttons (registered once) ────────────────────────────────────────
    .handle_tags_once <- function(new_tags) {
      ctx <- modal_ctx(); req(ctx)
      detail  <- ctx$detail
      tbl     <- .detail_to_audit_tbl(detail)
      sel     <- input[["modal_tbl_rows_selected"]] %||% integer(0)
      if (!length(sel)) { showNotification("Selecciona al menos una factura.", type="warning"); return() }
      rows    <- .audit_sel_to_keys(tbl, sel, detail)
      tags_db <- shared$tags_db()
      for (i in seq_len(nrow(rows))) {
        tags_db <- set_invoice_tags(tags_db, ledger, rows[["Empresa"]][i], rows[["Moneda"]][i],
                                    rows[["Documento"]][i], new_tags, tagged_by=shared$current_user())
      }
      shared$tags_db(tags_db); save_tags(tags_db)
      showNotification("Etiquetas actualizadas.", type="message", duration=2)
    }
    observeEvent(input$tag_urgent,    { .handle_tags_once("urgent") },                ignoreInit=TRUE)
    observeEvent(input$tag_important, { .handle_tags_once("important") },             ignoreInit=TRUE)
    observeEvent(input$tag_both,      { .handle_tags_once(c("important","urgent")) }, ignoreInit=TRUE)
    observeEvent(input$tag_clear,     { .handle_tags_once(character(0)) },            ignoreInit=TRUE)

    # ── Edit helpers ──────────────────────────────────────────────────────────
    # Full-form SAP edit modal. Locked fields (Empresa, Moneda, Documento,
    # Importe) are displayed as read-only divs \u2014 the original SAP snapshot
    # is never written.  Editable fields (Parte, Codigo, Factura, Fecha,
    # Notas) are saved to sap_overrides.rds and painted on top at render time.
    .open_sap_edit_modal <- function(row) {
      session$userData[[ns("pending_sap_edit_row")]] <- row
      party_lbl <- if (ledger == "AR") "Cliente" else "Proveedor"

      ov_db <- tryCatch(shared$sap_ov_db(), error = function(e) NULL)
      existing_ov <- if (!is.null(ov_db) && nrow(ov_db)) {
        ov_db[ov_db$ledger    == ledger              &
              ov_db$Empresa   == row[["Empresa"]]    &
              ov_db$Moneda    == row[["Moneda"]]     &
              ov_db$Documento == row[["Documento"]], , drop = FALSE]
      } else NULL
      .ov <- function(col, fallback) {
        v <- if (!is.null(existing_ov) && nrow(existing_ov))
               existing_ov[[col]][1] else NA_character_
        if (!is.na(v) && nzchar(trimws(v %||% ""))) v else (fallback %||% "")
      }

      prefill_fecha <- tryCatch({
        d <- as.Date(row[["FechaVenc_Proyectada"]])
        if (is.na(d)) as.Date(row[["FechaVenc_Original"]]) else d
      }, error = function(e)
        tryCatch(as.Date(row[["FechaVenc_Original"]]),
                 error = function(e2) Sys.Date()))

      prefill_moneda <- {
        v <- if (!is.null(existing_ov) && nrow(existing_ov))
               existing_ov[["Moneda_override"]][1] else NA_character_
        if (!is.na(v) && nzchar(trimws(v %||% ""))) v else (row[["Moneda"]] %||% "MXN")
      }
      prefill_importe <- {
        v <- if (!is.null(existing_ov) && nrow(existing_ov))
               existing_ov[["Importe_override"]][1] else NA_real_
        if (!is.na(v)) v else abs(as.numeric(row[["Importe"]] %||% 0))
      }

      showModal(modalDialog(
        title     = paste0("Editar \u2014 SAP / ", row[["Documento"]]),
        size      = "l", easyClose = TRUE,
        footer    = tagList(
          modalButton("Cancelar"),
          actionButton(ns("sap_edit_save"), "Guardar",
                       class = "btn btn-primary btn-sm")
        ),
        fluidRow(
          column(6,
            div(class = "mb-3",
              tags$label("Empresa", class = "form-label small text-muted"),
              div(class = "form-control bg-light text-muted",
                  style = "pointer-events:none;", row[["Empresa"]] %||% "")
            ),
            selectInput(ns("sap_edit_moneda"), "Moneda",
                        choices  = CURRENCIES,
                        selected = prefill_moneda),
            div(class = "mb-3",
              tags$label("No. Documento", class = "form-label small text-muted"),
              div(class = "form-control bg-light text-muted",
                  style = "pointer-events:none;", row[["Documento"]] %||% "")
            ),
            textInput(ns("sap_edit_factura"), "No. Factura",
                      value       = .ov("Factura_override", row[["Factura"]]),
                      placeholder = "Concepto / referencia de pago")
          ),
          column(6,
            textInput(ns("sap_edit_parte"),  party_lbl,
                      value = .ov("Parte_override",  row[["Parte"]])),
            if (ledger == "AP") uiOutput(ns("sap_prov_suggestions")) else NULL,
            textInput(ns("sap_edit_codigo"), paste("C\u00f3digo de", party_lbl),
                      value = .ov("Codigo_override", row[["Codigo"]])),
            numericInput(ns("sap_edit_importe"), "Importe",
                         value = prefill_importe, min = 0)
          )
        ),
        fluidRow(
          column(6,
            dateInput(ns("sap_edit_fecha"), "Fecha de vencimiento",
                      value     = prefill_fecha,
                      weekstart = 1, language = "es")
          ),
          column(6,
            textAreaInput(ns("sap_edit_notas"), "Notas",
                          value       = .ov("Notas_override", row[["notas"]]),
                          rows        = 3,
                          placeholder = "Notas visibles solo en esta app...")
          )
        ),
        div(class = "mt-2 d-flex align-items-center gap-2",
          tags$span(class = "badge bg-secondary", "\u26d3\ufe0f SAP"),
          tags$small(class = "text-muted",
            "Empresa y Documento son de solo lectura. Los datos originales de SAP se preservan intactos.")
        )
      ))
    }

    # Shared function: open the correct edit modal based on source.
    .open_edit_for_row <- function(row) {
      src <- row[["source"]] %||% "sap"
      if (isTRUE(src == "provision")) {
        prov_id <- row[["provision_id"]] %||% ""
        if (nzchar(prov_id)) {
          shinyjs::runjs(sprintf(
            "Shiny.setInputValue('pasivos_cell_click','%s',{priority:'event'})",
            htmltools::htmlEscape(prov_id, attribute = TRUE)
          ))
        }
        return()
      }
      if (isTRUE(src == "manual")) {
        man_row <- shared$manual_inv() |>
          dplyr::filter(
            .data$ledger    == !!ledger,
            .data$Empresa   == row[["Empresa"]],
            .data$Moneda    == row[["Moneda"]],
            .data$Documento == row[["Documento"]]
          )
        if (!nrow(man_row)) {
          showNotification("Entrada manual no encontrada.", type = "warning")
          return()
        }
        manual_edit_handlers(
          input, output, session,
          existing_row        = as.data.frame(man_row[1, , drop = FALSE]),
          sap_data            = shared$sap_data,
          manual_inv          = shared$manual_inv,
          current_user        = shared$current_user,
          active_entry_ledger = shared$active_entry_ledger,
          empresa_vals        = tryCatch(unname(shared$company_map()), error = function(e) NULL)
        )
      } else {
        .open_sap_edit_modal(row)
      }
    }

    # ── do_edit (audit mode) ──────────────────────────────────────────────────
    observeEvent(input$do_edit, {
      ctx <- modal_ctx(); req(ctx)
      detail <- ctx$detail
      sel    <- input[["modal_tbl_rows_selected"]] %||% integer(0)
      if (!length(sel)) {
        showNotification("Selecciona una factura para editar.", type = "warning")
        return()
      }
      if (length(sel) > 1)
        showNotification("Editando la primera factura seleccionada.",
                         type = "message", duration = 2)
      tbl  <- .detail_to_audit_tbl(detail)
      keys <- .audit_sel_to_keys(tbl, sel[1L], detail)
      if (!nrow(keys)) return()
      mask <- detail[["Empresa"]]   == keys[["Empresa"]][1]   &
              detail[["Moneda"]]    == keys[["Moneda"]][1]    &
              detail[["Documento"]] == keys[["Documento"]][1]
      row  <- detail[mask, , drop = FALSE][1L, , drop = FALSE]
      .open_edit_for_row(row)
    }, ignoreInit = TRUE)

    # ── edit_inv_i_j — event delegation ──────────────────────────────────────
    # Single observer replaces 50×200=10,000 pre-registered observers.
    observeEvent(input$edit_inv_click, {
      click <- input$edit_inv_click
      req(click)
      i <- as.integer(click$i); j <- as.integer(click$j)
      ctx <- modal_ctx(); req(ctx)
      detail  <- ctx$detail
      grp_agg <- aggregate(Importe ~ Empresa + Parte, data = detail,
                           FUN = sum, na.rm = TRUE)
      grp_agg <- grp_agg[order(-grp_agg[["Importe"]]), ]
      if (i > nrow(grp_agg)) return()
      row_e    <- grp_agg[["Empresa"]][i]
      row_p    <- grp_agg[["Parte"]][i]
      inv_mask <- detail[["Empresa"]] == row_e &
                  detail[["Parte"]]   == row_p &
                  !is.na(detail[["Documento"]])
      inv_cols <- intersect(
        c("Empresa","Moneda","Documento","Parte","Importe",
          "FechaEff","FechaVenc_Original","FechaVenc_Proyectada",
          "source","notas"),
        names(detail)
      )
      inv_rows <- unique(detail[inv_mask, inv_cols, drop = FALSE])
      inv_rows <- inv_rows[order(-inv_rows[["Importe"]]), ]
      if (j > nrow(inv_rows)) return()
      row <- inv_rows[j, , drop = FALSE]
      .open_edit_for_row(row)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ── sap_edit_save ─────────────────────────────────────────────────────────
    observeEvent(input$sap_edit_save, {
      row <- session$userData[[ns("pending_sap_edit_row")]]
      req(row)

      new_date <- tryCatch(as.Date(input$sap_edit_fecha),
                           error = function(e) NA_Date_)
      if (is.na(new_date)) {
        showNotification("Fecha inv\u00e1lida.", type = "warning"); return()
      }

      new_parte    <- trimws(input$sap_edit_parte   %||% "")
      new_codigo   <- trimws(input$sap_edit_codigo  %||% "")
      new_factura  <- trimws(input$sap_edit_factura %||% "")
      notas        <- trimws(input$sap_edit_notas   %||% "")
      new_moneda   <- input$sap_edit_moneda  %||% row[["Moneda"]]
      new_importe  <- tryCatch(as.numeric(input$sap_edit_importe), error = function(e) NA_real_)

      orig_moneda  <- row[["Moneda"]]  %||% ""
      orig_importe <- abs(as.numeric(row[["Importe"]] %||% 0))

      # --- date move (moves_db) ---
      new_move <- data.frame(
        ledger               = ledger,
        Empresa              = row[["Empresa"]],
        Moneda               = row[["Moneda"]],
        Documento            = row[["Documento"]],
        FechaVenc_Proyectada = new_date,
        notas                = notas,
        moved_by             = shared$current_user(),
        last_updated         = Sys.time(),
        stringsAsFactors     = FALSE
      )
      updated_moves <- upsert_moves(shared$moves_db(), new_move)
      shared$moves_db(updated_moves)
      save_moves(updated_moves)

      # --- field overrides (sap_overrides) ---
      ov_row <- tibble::tibble(
        id               = paste(ledger, row[["Empresa"]], row[["Moneda"]],
                                 row[["Documento"]], sep = "|"),
        ledger           = ledger,
        Empresa          = row[["Empresa"]],
        Moneda           = row[["Moneda"]],
        Documento        = row[["Documento"]],
        Parte_override   = if (nchar(new_parte)   > 0) new_parte   else NA_character_,
        Codigo_override  = if (nchar(new_codigo)  > 0) new_codigo  else NA_character_,
        Factura_override = if (nchar(new_factura) > 0) new_factura else NA_character_,
        Notas_override   = if (nchar(notas)       > 0) notas       else NA_character_,
        Moneda_override  = if (nzchar(new_moneda) && new_moneda != orig_moneda)
                             new_moneda else NA_character_,
        Importe_override = if (!is.na(new_importe) &&
                               abs(new_importe - orig_importe) > 0.001)
                             new_importe else NA_real_,
        updated_by       = shared$current_user(),
        updated_at       = Sys.time()
      )
      updated_ov <- upsert_sap_override(shared$sap_ov_db(), ov_row)
      shared$sap_ov_db(updated_ov)
      save_sap_overrides(updated_ov)

      # --- sync staged queue ---
      .sync_staged(
        data.frame(Empresa   = row[["Empresa"]],
                   Moneda    = row[["Moneda"]],
                   Documento = row[["Documento"]],
                   stringsAsFactors = FALSE),
        ledger, shared$pagar_hoy_db,
        new_date    = new_date,
        new_parte   = if (nchar(new_parte)  > 0) new_parte  else NULL,
        new_codigo  = if (nchar(new_codigo) > 0) new_codigo else NULL,
        new_moneda  = if (nzchar(new_moneda) && new_moneda != orig_moneda)
                        new_moneda else NULL,
        new_importe = if (!is.na(new_importe) &&
                          abs(new_importe - orig_importe) > 0.001)
                        new_importe else NULL,
        username    = shared$current_user())

      session$userData[[ns("pending_sap_edit_row")]] <- NULL
      ctx <- modal_ctx()
      if (!is.null(ctx)) {
        modal_ctx(.refresh_ctx_detail(ctx))   # showModal() in observeEvent replaces edit form
      } else {
        removeModal()
      }
      showNotification("Cambios guardados.", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    # ── Supplier autocomplete for SAP edit modal (AP only) ────────────────────
    if (ledger == "AP") {
      sap_prov_query   <- reactive({ input$sap_edit_parte %||% "" })
      sap_prov_query_d <- debounce(sap_prov_query, 300)

      output$sap_prov_suggestions <- renderUI({
        q <- trimws(sap_prov_query_d())
        if (nchar(q) < 2) return(NULL)
        provs <- tryCatch(shared$proveedores_db(), error = function(e) NULL) %||%
                 data.frame()
        if (!nrow(provs)) return(NULL)
        matches <- tryCatch(
          find_proveedor_matches(
            query          = list(parte = q, rfc = "", no_cuenta = "",
                                  nombre = q, alias = ""),
            proveedores_df = provs,
            threshold      = 15L, top_n = 8L
          ),
          error = function(e) NULL
        )
        if (is.null(matches) || !nrow(matches)) return(NULL)
        pick_id <- ns("sap_prov_pick")
        div(class = "prov-suggest-box",
            style = paste("border:1px solid #dee2e6; border-radius:6px;",
                          "margin-top:-8px; margin-bottom:8px; overflow:hidden;"),
            lapply(seq_len(nrow(matches)), function(i) {
              m       <- matches[i, ]
              payload <- jsonlite::toJSON(
                list(nombre = m$nombre %||% "", alias = m$alias %||% ""),
                auto_unbox = TRUE
              )
              tags$div(
                style = "padding:6px 10px; cursor:pointer; border-bottom:1px solid #eee;",
                onmouseover = "this.style.background='#f0f4ff'",
                onmouseout  = "this.style.background=''",
                onclick = paste0("Shiny.setInputValue('", pick_id, "',",
                                 payload, ",{priority:'event'})"),
                tags$span(m$nombre %||% ""),
                tags$span(class = "badge bg-secondary ms-2 small",
                          paste0(min(m$.score, 100L), "%"))
              )
            })
        )
      })

      observeEvent(input$sap_prov_pick, {
        p <- input$sap_prov_pick
        req(p)
        updateTextInput(session, "sap_edit_parte",  value = p$nombre %||% "")
        updateTextInput(session, "sap_edit_codigo", value = p$alias  %||% "")
      }, ignoreInit = TRUE)
    }

    # ── Day modal renderer — pure UI builder, no observer registration ────────
    # All operations on `detail` use base R [[]] to avoid dplyr NSE issues
    # with non-syntactic column names on Windows / dplyr 1.1+
    .render_day_modal <- function(ctx, input, output, session, ns,
                                   ledger, config, shared) {
      detail   <- ctx$detail          # plain data.frame, clean column names
      sel_date <- ctx$sel_date
      cur      <- ctx$cur
      amt      <- ctx$amt
      audit    <- ctx$audit

      unconfirmed_mask <- if ("confirmed" %in% names(detail))
        !(detail[["confirmed"]] %in% TRUE)
      else
        rep(TRUE, nrow(detail))
      total    <- sum(detail[["Importe"]][unconfirmed_mask], na.rm = TRUE)
      lbl_type <- if (ledger == "AR") "Cobros — CxC" else "Pagos — CxP"
      amt_pill_class <- if (grepl("vencido", tolower(amt)))
        "modal-day-pill modal-day-pill--saldo" else "modal-day-pill modal-day-pill--abono"

      context_header <- div(
        class = "modal-day-header",
        tags$span(class = "modal-day-pill modal-day-pill--date",
                  format(sel_date, "%d %b %Y")),
        tags$span(class = "modal-day-pill modal-day-pill--ledger", lbl_type),
        tags$span(class = "modal-day-pill modal-day-pill--cur", cur),
        tags$span(class = amt_pill_class, amt)
      )

      if (audit) {
        # Build display table using base R
        tbl <- data.frame(
          Empresa    = detail[["Empresa"]],
          Documento  = detail[["Documento"]],
          Parte      = detail[["Parte"]],
          Importe    = detail[["Importe"]],
          `Fecha orig.` = format(as.Date(detail[["FechaVenc_Original"]]), "%d/%m/%Y"),
          Movida     = !is.na(detail[["FechaVenc_Proyectada"]]),
          Etiqueta   = detail[["Etiqueta"]],
          Fuente     = ifelse(detail[["source"]] == "manual", "\u270e", "SAP"),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        tbl <- tbl[order(-tbl[["Importe"]]), ]

        showModal(modalDialog(
          title = lbl_type, size = "xl", easyClose = TRUE,
          footer = modalButton("Cerrar"),
          tagList(
            context_header,
            div(class = "modal-summary-bar d-flex align-items-center gap-3",
              tags$span(class = "badge bg-secondary", paste(sum(unconfirmed_mask), "facturas")),
              uiOutput(ns("sel_total_ui")),
              div(class = "ms-auto d-flex gap-2 align-items-center",
                actionButton(ns("show_raw"), "\U0001f50d Ver datos SAP",
                             class = "btn btn-sm btn-outline-secondary"),
                checkboxInput(ns("audit_toggle"), "Modo auditoría", value = TRUE))
            ),
            hr(),
            DT::dataTableOutput(ns("modal_tbl")),
            hr(),
            div(class = "d-flex gap-2 mb-2",
              actionButton(ns("tag_urgent"),    "\U0001f534 Urgente",
                           class = "btn btn-sm btn-outline-danger"),
              actionButton(ns("tag_important"), "\U0001f7e1 Importante",
                           class = "btn btn-sm btn-outline-warning"),
              actionButton(ns("tag_both"),      "\U0001f7e0 Ambas",
                           class = "btn btn-sm",
                           style = "border-color:#FF6B35;color:#FF6B35;"),
              actionButton(ns("tag_clear"),     "\u2716 Quitar",
                           class = "btn btn-sm btn-outline-secondary")
            ),
            div(class = "d-flex gap-2 mb-2",
              actionButton(ns("do_edit"), "\u270f\ufe0f Editar",
                           class = "btn btn-sm btn-outline-secondary")
            ),
              div(class = "d-flex gap-2 align-items-end",
                div(
                  dateInput(ns("move_to"), "Mover a:",
                            value = sel_date + 1, weekstart = 1, language = "es"),
                  uiOutput(ns("move_to_hint_ui"))
                ),
                actionButton(ns("do_move"),    "Mover",
                             class = "btn btn-primary btn-sm"),
                actionButton(ns("do_restore"), "Restaurar original",
                             class = "btn btn-outline-secondary btn-sm"),
                div(class = "ms-auto d-flex gap-2",
                  actionButton(ns("stage_sel"), "\U0001f6d2 Agregar selección",
                               class = "btn btn-outline-success btn-sm"),
                  actionButton(ns("do_delete"), "\U0001f5d1 Eliminar selección",
                               class = "btn btn-outline-danger btn-sm")
                )
              )
          )
        ))

        # output$modal_tbl rendered at module top level (see below)

      } else {
        # Build grouped summary using base R aggregate
        grp_raw <- aggregate(
          Importe ~ Empresa + Parte,
          data    = detail,
          FUN     = sum,
          na.rm   = TRUE
        )
        # Add Etiqueta per group — guard against zero tagged rows
        etiq_data <- detail[nzchar(detail[["Etiqueta"]]), , drop = FALSE]
        if (nrow(etiq_data)) {
          etiq_raw <- aggregate(
            Etiqueta ~ Empresa + Parte,
            data = etiq_data,
            FUN  = function(x) tag_label(x)
          )
          grp <- merge(grp_raw, etiq_raw, by = c("Empresa","Parte"), all.x = TRUE)
        } else {
          grp <- grp_raw
          grp[["Etiqueta"]] <- ""
        }
        grp[["Etiqueta"]][is.na(grp[["Etiqueta"]])] <- ""
        grp <- grp[order(-grp[["Importe"]]), ]

        # Staged pairs — which (Empresa, Parte) already in pagar_hoy
        ph <- shared$pagar_hoy_db()
        if (!is.null(ph) && nrow(ph)) {
          ph_p <- ph[ph[["ledger"]] == ledger & ph[["status"]] == "pending", ,
                     drop = FALSE]
          if (nrow(ph_p)) {
            dk <- unique(detail[, c("Empresa","Moneda","Documento","Parte"),
                                drop = FALSE])
            merged <- merge(ph_p[, c("Empresa","Moneda","Documento"), drop=FALSE],
                            dk, by = c("Empresa","Moneda","Documento"))
            staged_pairs <- unique(merged[, c("Empresa","Parte"), drop = FALSE])
          } else {
            staged_pairs <- data.frame(Empresa=character(), Parte=character())
          }
        } else {
          staged_pairs <- data.frame(Empresa=character(), Parte=character())
        }

        grp[["EnProceso"]] <- mapply(function(e, p) {
          any(staged_pairs[["Empresa"]] == e & staged_pairs[["Parte"]] == p)
        }, grp[["Empresa"]], grp[["Parte"]])

        # Fix 3: Recompute group importes excluding confirmed invoices so the
        # cart shows only unconfirmed amounts (confirmed groups get Importe = 0).
        if ("confirmed" %in% names(detail)) {
          unconf_detail <- detail[!(detail[["confirmed"]] %in% TRUE), , drop = FALSE]
          grp_unconf <- if (nrow(unconf_detail)) {
            aggregate(Importe ~ Empresa + Parte, data = unconf_detail,
                      FUN = sum, na.rm = TRUE)
          } else {
            data.frame(Empresa = character(), Parte = character(), Importe = numeric())
          }
          grp <- merge(grp[, c("Empresa","Parte","Etiqueta","EnProceso"), drop = FALSE],
                       grp_unconf, by = c("Empresa","Parte"), all.x = TRUE)
          grp[["Importe"]][is.na(grp[["Importe"]])] <- 0
        }

        # Fix 2: Count only groups that are not fully confirmed
        n_unconf_groups <- if ("confirmed" %in% names(detail)) {
          sum(vapply(seq_len(nrow(grp)), function(i) {
            mask <- detail[["Empresa"]] == grp[["Empresa"]][i] &
                    detail[["Parte"]]   == grp[["Parte"]][i]
            !all(detail[["confirmed"]][mask] %in% TRUE)
          }, logical(1)))
        } else nrow(grp)

        showModal(modalDialog(
          title = lbl_type, size = "xl", easyClose = TRUE,
          footer = modalButton("Cerrar"),
          tagList(
            context_header,
            div(class = "modal-summary-bar d-flex align-items-center gap-3",
              tags$span(class = "badge bg-secondary", paste(n_unconf_groups, "partidas")),
              tags$strong(paste("Total:", fmt_money(total))),
              div(class = "ms-auto d-flex gap-2 align-items-center",
                actionButton(ns("stage_all"),
                             if (ledger == "AR") "\U0001f6d2 Agregar todo" else "\U0001f6d2 Agregar todo",
                             class = "btn btn-sm btn-outline-success"),
                checkboxInput(ns("audit_toggle"), "Modo auditoría", value = FALSE)
              )
            ),
            hr(),
            # Cart for pagar_hoy staging (primary view in summary mode)
            uiOutput(ns("cart_table")),
            hr(),
            div(class = "d-flex gap-2 align-items-end",
              dateInput(ns("move_to"), "Mover a:",
                        value = sel_date + 1, weekstart = 1, language = "es"),
              actionButton(ns("do_move"),    "Mover",
                           class = "btn btn-primary btn-sm"),
              actionButton(ns("do_restore"), "Restaurar original",
                           class = "btn btn-outline-secondary btn-sm"),
              div(class = "ms-auto d-flex gap-2",
                actionButton(ns("stage_sel"), "\U0001f6d2 Agregar selecci\u00f3n",
                             class = "btn btn-outline-success btn-sm"),
                actionButton(ns("do_delete"), "\U0001f5d1 Eliminar selecci\u00f3n",
                             class = "btn btn-outline-danger btn-sm")
              )
            )
          )
        ))

        # output$modal_tbl rendered at module top level (see below)

        output$cart_table <- renderUI({
          ph_now <- shared$pagar_hoy_db()
          current_tags <- shared$tags_db()   # reactive dep — rerenders on tag change
          tg_cur <- current_tags[current_tags[["ledger"]] == ledger, , drop = FALSE]
          staged_now <- if (!is.null(ph_now) && nrow(ph_now)) {
            ph_now[ph_now[["ledger"]] == ledger & ph_now[["status"]] == "pending",
                   c("Empresa","Documento"), drop = FALSE]
          } else data.frame(Empresa = character(), Documento = character())

          rows_ui <- lapply(seq_len(nrow(grp)), function(i) {
            row_e <- grp[["Empresa"]][i]
            row_p <- grp[["Parte"]][i]
            inv_mask <- detail[["Empresa"]] == row_e &
                        detail[["Parte"]]   == row_p &
                        !is.na(detail[["Documento"]])
            # Skip groups where ALL invoices are confirmed — hidden in month view too
            is_conf_group <- "confirmed" %in% names(detail) && sum(inv_mask) > 0 &&
                             all(detail[["confirmed"]][inv_mask] %in% TRUE)
            if (is_conf_group) return(NULL)
            inv_keys <- unique(detail[inv_mask, c("Empresa","Documento"),
                                      drop = FALSE])
            n_inv     <- nrow(inv_keys)
            n_in_cart <- nrow(merge(inv_keys, staged_now,
                                    by = c("Empresa","Documento")))
            is_staged <- n_in_cart > 0
            btn_lbl   <- if (is_staged) "\u2713 Quitar" else "\uff0b Agregar"
            btn_cls   <- if (is_staged) "btn btn-sm btn-success cart-btn"
                         else           "btn btn-sm btn-outline-success cart-btn"
            # Live tag label from current_tags
            inv_full  <- unique(detail[inv_mask, c("Empresa","Moneda","Documento"), drop=FALSE])
            live_tags <- character(0)
            for (j in seq_len(nrow(inv_full))) {
              r <- tg_cur[tg_cur[["Empresa"]]   == inv_full[["Empresa"]][j]  &
                          tg_cur[["Moneda"]]    == inv_full[["Moneda"]][j]   &
                          tg_cur[["Documento"]] == inv_full[["Documento"]][j], , drop=FALSE]
              live_tags <- c(live_tags, r[["tag"]])
            }
            etiq_val  <- tag_label(live_tags)
            tag_span  <- if (nzchar(etiq_val))
              tags$span(class = "badge bg-warning text-dark ms-1",
                        style = "font-size:0.65rem;", etiq_val)
            else NULL

            row_bg <- switch(etiq_val,
              "\U0001f534 Urgente"    = "background:#fff0f0;",
              "\U0001f7e1 Importante" = "background:#fffbe6;",
              "\U0001f7e0 Ambas"      = "background:#fff3e6;",
              ""
            )
            # Confirmed check — all matching invoices paid/collected
            is_conf_group <- FALSE
            if ("confirmed" %in% names(detail) && sum(inv_mask) > 0) {
              is_conf_group <- all(detail[["confirmed"]][inv_mask], na.rm = TRUE)
            }
            conf_badge <- if (is_conf_group)
              tags$span(class = "badge bg-success ms-1",
                        style = "font-size:0.65rem;", "\u2713 Confirmado")
            else NULL
            conf_style <- if (is_conf_group)
              "text-decoration:line-through; color:#888;" else ""

            # Per-invoice expansion
            is_expanded <- i %in% cart_expanded()
            inv_detail  <- unique(detail[inv_mask,
              intersect(c("Empresa","Moneda","Documento","Factura","Importe","source","provision_id"),
                        names(detail)), drop=FALSE])
            inv_detail  <- inv_detail[order(-inv_detail[["Importe"]]), ]
            # Detect all-provision groups (show bolt instead of cart button)
            all_prov <- nrow(inv_detail) > 0 &&
              "source" %in% names(inv_detail) &&
              all(!is.na(inv_detail[["source"]]) & inv_detail[["source"]] == "provision")
            first_prov_id <- if (all_prov && "provision_id" %in% names(inv_detail))
              inv_detail[["provision_id"]][1] %||% "" else ""
            prov_badge <- if (all_prov)
              tags$span(
                class = "badge rounded-pill ms-1",
                style = "font-size:0.65rem; background:#6d28d9; color:white;",
                "⚡ Provisión"
              )
            else NULL
            # Pre-compute j-index for non-provision rows (matches cart_inv_click's inv_rows indexing)
            inv_detail[[".j_item"]] <- {
              is_prov_vec <- "source" %in% names(inv_detail) &
                             !is.na(inv_detail[["source"]]) &
                             inv_detail[["source"]] == "provision"
              j_seq <- cumsum(!is_prov_vec)
              ifelse(is_prov_vec, NA_integer_, j_seq)
            }

            expand_lbl <- if (n_inv >= 2L) {
              tags$small(
                class = "cart-expand-lbl",
                style = "cursor:pointer; user-select:none;",
                if (is_expanded) paste0("▲ ", n_inv, " facturas")
                else             paste0("▼ ", n_inv, " facturas")
              )
            } else NULL

            inv_rows_ui <- if (is_expanded && n_inv >= 2L) {
              lapply(seq_len(nrow(inv_detail)), function(ii) {
                doc       <- inv_detail[["Documento"]][ii]
                ref       <- if ("Factura" %in% names(inv_detail))
                  inv_detail[["Factura"]][ii] %||% ""
                else ""
                amt_ii    <- inv_detail[["Importe"]][ii]
                is_prov_ii <- "source" %in% names(inv_detail) &&
                              identical(inv_detail[["source"]][ii], "provision")
                prov_id_ii <- if (is_prov_ii && "provision_id" %in% names(inv_detail))
                  inv_detail[["provision_id"]][ii] %||% ""
                else ""
                action_btn <- if (is_prov_ii) {
                  tags$button(
                    class   = "btn btn-xs btn-outline-secondary cart-inv-btn",
                    title   = "Convertir provisi\u00f3n",
                    onclick = sprintf(
                      "Shiny.setInputValue('pasivos_convert_request', '%s', {priority:'event'})",
                      prov_id_ii
                    ),
                    "\u26a1"
                  )
                } else {
                  j_item    <- inv_detail[[".j_item"]][ii]
                  key_ii    <- inv_detail[ii, c("Empresa","Documento"), drop=FALSE]
                  in_cart_ii <- nrow(merge(key_ii, staged_now,
                                          by=c("Empresa","Documento"))) > 0
                  btn_ii_cls <- if (in_cart_ii) "btn btn-xs btn-success cart-inv-btn"
                               else             "btn btn-xs btn-outline-success cart-inv-btn"
                  tags$button(
                    class   = btn_ii_cls,
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', {i:%d, j:%d, nonce:Math.random()}, {priority:'event'})",
                      ns("cart_inv_click"), i, j_item
                    ),
                    if (in_cart_ii) "\u2713" else "\uff0b"
                  )
                }
                edit_btn <- if (!is_prov_ii) {
                  j_item <- inv_detail[[".j_item"]][ii]
                  tags$button(
                    class   = "btn btn-xs btn-outline-secondary cart-inv-btn",
                    style   = "padding:1px 5px;",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', {i:%d, j:%d, nonce:Math.random()}, {priority:'event'})",
                      ns("edit_inv_click"), i, j_item
                    ),
                    shiny::icon("pencil")
                  )
                } else NULL
                div(class = "cart-inv-row d-flex align-items-center gap-2",
                  div(class = "cart-inv-doc text-muted d-flex flex-column gap-0",
                    tags$span(
                      tags$span(class = "small fw-semibold text-dark", "Doc: "),
                      doc
                    ),
                    tags$span(
                      tags$span(class = "small fw-semibold text-dark", "Ref: "),
                      if (!is.na(ref) && nzchar(trimws(ref)) && ref != doc)
                        ref
                      else
                        tags$span(class = "text-muted fst-italic", "\u2014")
                    )
                  ),
                  tags$span(class = "cart-inv-amt ms-auto", fmt_money(amt_ii)),
                  action_btn,
                  edit_btn
                )
              })
            } else list()

            tagList(
              div(class = "cart-row d-flex align-items-center gap-2 py-2 border-bottom",
                style = row_bg,
                div(class = "flex-grow-1 min-width-0",
                  div(class = "d-flex align-items-center gap-1",
                    tags$span(class = "cart-empresa-badge", row_e),
                    tags$span(class = "cart-party", style = conf_style, row_p),
                    tag_span,
                    conf_badge,
                    prov_badge
                  ),
                  if (n_inv >= 2L)
                    tags$button(
                      class   = "btn btn-link p-0 border-0 shadow-none cart-expand-btn",
                      onclick = sprintf(
                        "var mb=document.querySelector('.modal-body');Shiny.setInputValue('%s',{i:%d,scrollTop:mb?mb.scrollTop:0,nonce:Math.random()},{priority:'event'})",
                        ns("cart_expand_click"), i
                      ),
                      expand_lbl
                    )
                  else
                    expand_lbl
                ),
                tags$span(class = "cart-amount", style = conf_style,
                          fmt_money(grp[["Importe"]][i])),
                if (all_prov && nzchar(first_prov_id)) {
                  tags$button(
                    class   = "btn btn-sm btn-outline-secondary cart-btn-provision",
                    title   = "Convertir provisión",
                    onclick = sprintf(
                      "Shiny.setInputValue('pasivos_convert_request','%s',{priority:'event'})",
                      first_prov_id
                    ),
                    "⚡"
                  )
                } else {
                  actionButton(ns(paste0("cart_", i)), btn_lbl, class = btn_cls)
                }
              ),
              if (length(inv_rows_ui))
                div(class = "cart-inv-list px-3 pb-1", !!!inv_rows_ui)
            )
          })
          rows_ui <- Filter(Negate(is.null), rows_ui)
          session$userData[[ns("cart_grp_snapshot")]] <- grp

          total_staged <- if (!is.null(ph_now) && nrow(ph_now)) {
            ph_cur <- ph_now[ph_now[["ledger"]] == ledger &
                             ph_now[["status"]] == "pending" &
                             toupper(trimws(ph_now[["Moneda"]])) == cur, ,
                             drop = FALSE]
            sum(ph_cur[["Importe"]], na.rm = TRUE)
          } else 0

          tagList(
            div(class = "cart-list", !!!rows_ui),
            if (total_staged > 0)
              div(class = "cart-total-staged mt-2 p-2 rounded",
                tags$span(if (ledger == "AR") "\U0001f6d2 En Agenda (Cobros):" else "\U0001f6d2 En Agenda (Pagos):"),
                tags$strong(fmt_money(total_staged))
              )
          )
        })
      }
    }


    # ── modal_tbl — defined at module top level so the binding persists ───────
    # MUST live here, not inside .render_day_modal / observeEvent, or Shiny
    # destroys and never re-creates the output binding after first modal close.
    output$modal_tbl <- DT::renderDataTable({
      ctx <- modal_ctx()
      if (is.null(ctx)) return(DT::datatable(data.frame()))

      detail       <- ctx$detail
      current_tags <- shared$tags_db()
      tg           <- current_tags[current_tags[["ledger"]] == ledger, , drop = FALSE]

      if (isTRUE(ctx$audit)) {
        # ── Audit mode: one row per invoice ──────────────────────────────────
        is_conf <- if ("confirmed" %in% names(detail)) detail[["confirmed"]] else FALSE

        movida_val <- if ("Movida" %in% names(detail))
          detail[["Movida"]]
        else
          ifelse(!is.na(detail[["FechaVenc_Proyectada"]]), "Manual", "Falso")

        tbl_live <- data.frame(
          Empresa       = detail[["Empresa"]],
          Documento     = detail[["Documento"]],
          Referencia    = if ("Factura" %in% names(detail)) detail[["Factura"]] else NA_character_,
          Parte         = detail[["Parte"]],
          Importe       = fmt_money(detail[["Importe"]]),
          `Fecha orig.` = format(as.Date(detail[["FechaVenc_Original"]]), "%d/%m/%Y"),
          Movida        = movida_val,
          Etiqueta      = vapply(seq_len(nrow(detail)), function(i) {
            rows <- tg[tg[["Empresa"]]   == detail[["Empresa"]][i]  &
                       tg[["Moneda"]]    == detail[["Moneda"]][i]   &
                       tg[["Documento"]] == detail[["Documento"]][i], , drop = FALSE]
            if (nrow(rows)) tag_label(rows[["tag"]]) else ""
          }, character(1)),
          Fuente        = {
            src   <- detail[["source"]]
            ov    <- if ("has_sap_override" %in% names(detail))
                       !is.na(detail[["has_sap_override"]]) & detail[["has_sap_override"]]
                     else rep(FALSE, nrow(detail))
            pids  <- if ("provision_id" %in% names(detail)) detail[["provision_id"]] else NA_character_
            vapply(seq_along(src), function(k) {
              if (!is.na(src[k]) && src[k] == "provision") {
                pid <- pids[k]
                if (!is.na(pid) && nzchar(pid)) {
                  paste0('<button class="pasivos-convert-btn" title="Convertir provisión" ',
                         'onclick="event.stopPropagation();Shiny.setInputValue(\'pasivos_convert_request\',\'',
                         pid, '\',{priority:\'event\'});">&#x26A1;</button>')
                } else "⚡"
              } else if (!is.na(src[k]) && src[k] == "manual") {
                "✎"
              } else if (ov[k]) {
                "SAP ✎"
              } else {
                "SAP"
              }
            }, character(1))
          },
          Confirmado    = is_conf,
          check.names = FALSE, stringsAsFactors = FALSE
        )
        sort_ord <- order(-detail[["Importe"]])
        tbl_live <- tbl_live[sort_ord, ]
        DT::datatable(tbl_live,
          escape = FALSE, selection = "multiple", rownames = FALSE,
          options = list(
            pageLength = 20, dom = "ftip", scrollX = TRUE,
            columnDefs = list(list(visible = FALSE, targets = which(names(tbl_live) == "Confirmado") - 1L))
          )
        ) |>
          DT::formatStyle("Movida",
            color = DT::styleEqual(
              c("Manual", "Políticas", "Falso"),
              c("#0d6efd",  "#198754",         "#6c757d")
            ),
            fontWeight = DT::styleEqual(
              c("Manual", "Políticas", "Falso"),
              c("bold",    "bold",            "normal")
            )) |>
          DT::formatStyle("Etiqueta", target = "row",
            backgroundColor = DT::styleEqual(
              c("\U0001f534 Urgente","\U0001f7e1 Importante","\U0001f7e0 Ambas",""),
              c("#fff0f0","#fffbe6","#fff3e6","transparent"))) |>
          DT::formatStyle("Confirmado",
            target         = "row",
            textDecoration = DT::styleEqual(c(TRUE, FALSE), c("line-through", "none")),
            color          = DT::styleEqual(c(TRUE, FALSE), c("#adb5bd", "inherit")),
            opacity        = DT::styleEqual(c(TRUE, FALSE), c("0.55",    "1")))

      } else {
        # ── Summary mode: one row per (Empresa, Parte) ────────────────────────
        grp_raw <- aggregate(Importe ~ Empresa + Parte, data = detail,
                             FUN = sum, na.rm = TRUE)
        live_etiq <- vapply(seq_len(nrow(grp_raw)), function(i) {
          e <- grp_raw[["Empresa"]][i]; p <- grp_raw[["Parte"]][i]
          inv_docs <- unique(detail[detail[["Empresa"]] == e & detail[["Parte"]] == p,
                                    c("Empresa","Moneda","Documento"), drop = FALSE])
          tags_for_grp <- character(0)
          for (j in seq_len(nrow(inv_docs))) {
            rows <- tg[tg[["Empresa"]]   == inv_docs[["Empresa"]][j] &
                       tg[["Moneda"]]    == inv_docs[["Moneda"]][j]  &
                       tg[["Documento"]] == inv_docs[["Documento"]][j], , drop = FALSE]
            tags_for_grp <- c(tags_for_grp, rows[["tag"]])
          }
          tag_label(tags_for_grp)
        }, character(1))
        tbl_disp <- data.frame(
          Empresa  = grp_raw[["Empresa"]],
          Parte    = grp_raw[["Parte"]],
          Importe  = fmt_money(grp_raw[["Importe"]]),
          Etiqueta = live_etiq,
          stringsAsFactors = FALSE
        )
        tbl_disp <- tbl_disp[order(-grp_raw[["Importe"]]), ]
        DT::datatable(tbl_disp,
          escape = FALSE, selection = "multiple", rownames = FALSE,
          options = list(pageLength = 20, dom = "ftip", scrollX = TRUE)
        ) |>
          DT::formatStyle("Etiqueta", target = "row",
            backgroundColor = DT::styleEqual(
              c("\U0001f534 Urgente","\U0001f7e1 Importante","\U0001f7e0 Ambas",""),
              c("#fff0f0","#fffbe6","#fff3e6","transparent")))
      }
    })

    # ── sel_total_ui — live total that reflects row selection ─────────────────
    # Lives at module level (same as modal_tbl) so the binding survives modal
    # open/close cycles. Reads input$modal_tbl_rows_selected reactively.
    output$sel_total_ui <- renderUI({
      ctx <- modal_ctx()
      if (is.null(ctx)) return(tags$strong("Total: —"))

      detail <- ctx$detail

      # Reconstruct sort order and raw numeric importes exactly as modal_tbl does
      if (isTRUE(ctx$audit)) {
        sort_ord    <- order(-detail[["Importe"]])
        sorted_amts <- detail[["Importe"]][sort_ord]
      } else {
        grp_raw     <- aggregate(Importe ~ Empresa + Parte, data = detail,
                                 FUN = sum, na.rm = TRUE)
        sort_ord    <- order(-grp_raw[["Importe"]])
        sorted_amts <- grp_raw[["Importe"]][sort_ord]
      }

      unconfirmed_mask <- if ("confirmed" %in% names(detail))
        !(detail[["confirmed"]] %in% TRUE)
      else
        rep(TRUE, nrow(detail))
      all_total <- sum(detail[["Importe"]][unconfirmed_mask], na.rm = TRUE)

      sel <- input[["modal_tbl_rows_selected"]] %||% integer(0)

      if (length(sel) == 0) {
        tags$strong(paste("Total:", fmt_money(all_total)))
      } else {
        sel_total <- sum(sorted_amts[sel], na.rm = TRUE)
        tagList(
          tags$strong(style = "color:#0a58ca;",
                      paste("Selección:", fmt_money(sel_total))),
          tags$span(class = "text-muted small",
                    paste0("(Total: ", fmt_money(all_total), ")"))
        )
      }
    })


    # ── move_to_hint_ui — policy compliance badge for the "Mover a:" date ──────
    output$move_to_hint_ui <- renderUI({
      date    <- input$move_to
      ctx     <- modal_ctx()
      if (is.null(date) || is.null(ctx) || !isTRUE(ctx$audit)) return(NULL)

      pp      <- tryCatch(shared$partner_policies_db(), error = function(e) NULL)
      catalog <- tryCatch(shared$policy_catalog_db(),   error = function(e) NULL)
      if (is.null(pp) || !nrow(pp) || is.null(catalog) || !nrow(catalog)) return(NULL)

      partes <- unique(ctx$detail[["Parte"]])
      hints  <- Filter(Negate(is.null), lapply(partes, function(p) {
        pp_p <- pp[tolower(trimws(pp$parte)) == tolower(trimws(p)) &
                     !(isTRUE(pp$is_interco)), , drop = FALSE]
        if (!nrow(pp_p)) return(NULL)
        pp_p  <- pp_p[order(pp_p$policy_order), ]
        pols  <- Filter(Negate(is.null), lapply(pp_p$policy_id, function(pid) {
          r <- catalog[catalog$id == pid, , drop = FALSE]
          if (!nrow(r)) return(NULL)
          list(type = r$type[[1]], params = r$params[[1]] %||% list(),
               roll_direction = r$roll_direction[[1]] %||% "forward")
        }))
        if (!length(pols)) return(NULL)
        pol_date <- tryCatch(compose_policies(as.Date(date), pols, list()),
                             error = function(e) NA)
        if (is.na(pol_date)) return(NULL)
        if (as.Date(pol_date) == as.Date(date)) {
          tags$span(class = "badge bg-success me-1 d-block mt-1",
                    icon("check"), " ", p)
        } else {
          tags$span(class = "badge bg-warning text-dark me-1 d-block mt-1",
                    icon("triangle-exclamation"), " ", p, " -> ",
                    format(as.Date(pol_date), "%d/%m/%Y"))
        }
      }))
      if (!length(hints)) return(NULL)
      div(class = "mt-1", hints)
    })
  }) # end moduleServer
}   # end ledgerModuleServer


# ── Core move / restore persistence ───────────────────────────────────────────

.apply_move <- function(keys, new_date, ledger, shared) {
  moves <- shared$moves_db()
  new_rows <- keys |>
    dplyr::mutate(
      ledger               = ledger,
      FechaVenc_Proyectada = as.Date(new_date),
      moved_by             = shared$current_user(),
      last_updated         = Sys.time()
    )
  updated <- upsert_moves(moves, new_rows)
  shared$moves_db(updated)
  save_moves(updated)
}

.clear_moves <- function(keys, ledger, shared) {
  moves   <- shared$moves_db()
  updated <- dplyr::anti_join(
    moves,
    keys |> dplyr::mutate(ledger = !!ledger),
    by = c("ledger","Empresa","Moneda","Documento")
  )
  shared$moves_db(updated)
  save_moves(updated)
}

# ── Sync pagar_hoy after a date-move, restore, or manual edit ─────────────────
# ph_rv    — the pagar_hoy reactiveVal (shared$pagar_hoy_db or app.R's pagar_hoy_db)
# keys     — data.frame(Empresa, Moneda, Documento)
# ledger   — "AR" | "AP"
# new_date — if non-NULL, set FechaVenc to this Date for all keys
# detail   — if new_date is NULL, read FechaVenc_Original per key from this data.frame
# new_imp  — if non-NULL, also update Importe (manual edit)
# new_parte— if non-NULL, also update Parte  (manual edit)
.sync_staged <- function(keys, ledger, ph_rv,
                         new_date    = NULL,
                         detail      = NULL,
                         new_imp     = NULL,
                         new_parte   = NULL,
                         new_codigo  = NULL,
                         new_moneda  = NULL,
                         new_importe = NULL,
                         username    = NULL) {
  ph <- ph_rv()
  if (is.null(ph) || !nrow(ph)) return(invisible(NULL))
  changed <- FALSE
  for (i in seq_len(nrow(keys))) {
    emp <- keys[["Empresa"]][i]
    mon <- keys[["Moneda"]][i]
    doc <- keys[["Documento"]][i]
    eff_date <- if (!is.null(new_date)) {
      as.Date(new_date)
    } else if (!is.null(detail)) {
      d_row <- detail[
        detail[["Empresa"]]   == emp &
        detail[["Moneda"]]    == mon &
        detail[["Documento"]] == doc, , drop = FALSE]
      if (!nrow(d_row)) next
      as.Date(d_row[["FechaVenc_Original"]][1L])
    } else next
    if (is.na(eff_date)) next
    idx <- which(ph[["ledger"]] == ledger & ph[["status"]] == "pending" &
                 ph[["Empresa"]] == emp & ph[["Moneda"]] == mon &
                 ph[["Documento"]] == doc)
    if (!length(idx)) next
    ph[["FechaVenc"]][idx] <- eff_date
    if (!is.null(new_imp))     ph[["Importe"]][idx] <- new_imp
    if (!is.null(new_parte))   ph[["Parte"]][idx]   <- new_parte
    if (!is.null(new_codigo))  ph[["Codigo"]][idx]  <- new_codigo
    if (!is.null(new_moneda))  ph[["Moneda"]][idx]  <- new_moneda
    if (!is.null(new_importe)) ph[["Importe"]][idx] <- abs(new_importe)
    changed <- TRUE
  }
  if (changed) { ph_rv(ph); save_pagar_hoy(ph, username) }
  invisible(NULL)
}
