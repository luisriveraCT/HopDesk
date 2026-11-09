# =============================================================================
# R/ledger_module.R
# A single Shiny module that handles one ledger (AR or AP).
# Instantiated twice in app.R with different config objects.
# input/output/session never leave their proper scope.
#
# The day modal's tables live in R/dia_module.R (T05), loaded here so
# global.R and app.R don't need another line.
# =============================================================================
source("R/dia_module.R")

# ── Module UI ──────────────────────────────────────────────────────────────────

ledgerModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(
      ".calendar-container .shiny-html-output.shiny-output-recalculating{opacity:1!important;transition:none!important}"
    )),
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
    # ── SAP + manual base (does NOT depend on provisions or moves) ──────────
    # Isolates the expensive build_ledger_df() call from provision saves AND
    # from invoice moves.  Only re-executes when SAP data, manual entries,
    # abonos, or SAP overrides change.
    # Moves and policy-moves are applied cheaply in df_with_moves() below.
    df_base <- reactive({
      t0 <- proc.time()
      raw    <- shared$sap_data()[[ledger]]
      manual <- shared$manual_inv()

      message("[DF_BASE] start ledger=", ledger,
              " raw_rows=", if (is.null(raw)) "NULL" else nrow(raw))

      df <- build_ledger_df(
        raw_df          = raw,
        ledger          = ledger,
        empresa         = NULL,
        moves_df        = NULL,
        manual_df       = manual,
        abonos_df       = shared$abonos_db(),
        policy_moves_df = NULL,
        sap_ov          = tryCatch(shared$sap_ov_db(), error = function(e) NULL),
        provs_df        = NULL,
        liabs_df        = NULL,
        company_map     = as.list(tryCatch(shared$company_map(), error = function(e) NULL))
      )

      message("[DF_BASE] done ledger=", ledger,
              " rows=", if (is.null(df)) "NULL" else nrow(df),
              " in ", round((proc.time() - t0)[["elapsed"]], 2), "s")
      df
    })

    # ── Moves layer: apply user moves + policy moves to df_base() ───────────
    # df_base() produces rows with FechaEff = FechaVenc_Original (no moves applied).
    # This reactive joins the move tables on top and recomputes FechaEff / Movida.
    # Cost: two lightweight left_joins (~20-30 ms) vs. the full build_ledger_df()
    # (500-1000 ms).  Only this layer re-executes when moves change; df_base()
    # stays cached.
    df_with_moves <- reactive({
      df <- df_base()
      if (is.null(df) || !nrow(df)) return(df)

      moves        <- shared$moves_db()
      policy_moves <- tryCatch(shared$policy_moves_db(), error = function(e) NULL)

      # Per-ledger move tables (same dedup logic as build_ledger_df)
      lm <- if (!is.null(moves) && nrow(moves) > 0) {
        moves |>
          dplyr::filter(.data$ledger == !!ledger) |>
          dplyr::select(Empresa, Moneda, Documento, FechaVenc_Proyectada,
                        dplyr::any_of("notas")) |>
          dplyr::arrange(dplyr::desc(FechaVenc_Proyectada)) |>
          dplyr::distinct(Empresa, Moneda, Documento, .keep_all = TRUE)
      } else {
        tibble::tibble(Empresa = character(), Moneda = character(),
                       Documento = character(), FechaVenc_Proyectada = as.Date(character()))
      }

      lpm <- if (!is.null(policy_moves) && nrow(policy_moves) > 0) {
        policy_moves |>
          dplyr::filter(.data$ledger == !!ledger) |>
          dplyr::select(Empresa, Moneda, Documento, FechaVenc_Politica) |>
          dplyr::arrange(dplyr::desc(FechaVenc_Politica)) |>
          dplyr::distinct(Empresa, Moneda, Documento, .keep_all = TRUE)
      } else {
        tibble::tibble(Empresa = character(), Moneda = character(),
                       Documento = character(), FechaVenc_Politica = as.Date(character()))
      }

      # Fast path: no moves exist — df_base already has FechaEff = FechaVenc_Original
      if (!nrow(lm) && !nrow(lpm)) return(df)

      # SAP-override notas (set by build_ledger_df) win over move notas.
      # Save them before the join wipes the column.
      notas_base <- df[["notas"]]

      # Drop columns re-derived from moves (they are all NA/Falso in df_base
      # because build_ledger_df was called with moves_df = NULL).
      df <- df |>
        dplyr::select(-dplyr::any_of(c("FechaVenc_Proyectada", "FechaVenc_Politica",
                                       "FechaEff", "Movida", "notas"))) |>
        dplyr::left_join(lm,  by = c("Empresa", "Moneda", "Documento"), na_matches = "never") |>
        dplyr::left_join(lpm, by = c("Empresa", "Moneda", "Documento"), na_matches = "never") |>
        dplyr::mutate(
          FechaVenc_Proyectada = as.Date(FechaVenc_Proyectada),
          # Policy dates are computed from SAP invoice terms and apply only to SAP
          # rows.  Manual entries carry a user-entered due date (FechaVenc_Original)
          # that must not be overridden by a policy date belonging to a different
          # historical entry that shares the same (Empresa, Moneda, Documento) key.
          FechaVenc_Politica   = dplyr::if_else(
            !is.na(.data$source) & .data$source == "manual",
            as.Date(NA_character_),
            as.Date(FechaVenc_Politica)
          ),
          FechaEff = dplyr::coalesce(FechaVenc_Proyectada, FechaVenc_Politica, FechaVenc_Original),
          Movida   = dplyr::case_when(
            !is.na(FechaVenc_Proyectada) ~ "Manual",
            !is.na(FechaVenc_Politica)   ~ "Políticas",
            TRUE                         ~ "Falso"
          )
        )

      # Restore notas: SAP-override notas first, move notas as fallback
      if (!"notas" %in% names(df)) df[["notas"]] <- NA_character_
      df[["notas"]] <- dplyr::coalesce(notas_base, df[["notas"]])
      df
    })

    # ── Provision rows only (AP ledger; fast — no SAP data) ─────────────────
    # Invalidates when pasivos_provisions_db or pasivos_liabilities_db change.
    # Does NOT depend on SAP data so provision saves skip the full SAP rebuild.
    df_prov_rows <- reactive({
      if (ledger != "AP") return(NULL)
      provs <- tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL)
      liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
      if (is.null(provs) || !nrow(provs)) return(NULL)

      prov_rows <- pasivos_provisions_as_ledger_rows(provs, liabs, ledger = "AP")
      if (is.null(prov_rows) || !nrow(prov_rows)) return(NULL)

      # Guard against the same provision UUID appearing more than once in the DB
      # (e.g., from a failed double-write). Only the first occurrence is kept.
      if ("provision_id" %in% names(prov_rows))
        prov_rows <- prov_rows[!duplicated(prov_rows[["provision_id"]]), , drop = FALSE]

      company_map <- as.list(tryCatch(shared$company_map(), error = function(e) NULL))
      if (!is.null(company_map) && length(company_map)) {
        prov_rows[["Empresa"]] <- vapply(
          prov_rows[["Empresa"]],
          function(e) company_map[[e %||% ""]] %||% e %||% NA_character_,
          character(1)
        )
      }

      # Apply date moves so provisions with a real Documento respect user-entered
      # projected dates.  na_matches = "never" mirrors build_ledger_df behaviour.
      # Notas are excluded from the join — provision notas come from the provision
      # itself (pasivos_calendar_glue) and must not be overwritten by move notas.
      moves <- shared$moves_db()
      ledger_moves <- if (!is.null(moves) && nrow(moves)) {
        moves |>
          dplyr::filter(.data$ledger == "AP") |>
          dplyr::select(Empresa, Moneda, Documento, FechaVenc_Proyectada) |>
          dplyr::arrange(dplyr::desc(FechaVenc_Proyectada)) |>
          dplyr::distinct(Empresa, Moneda, Documento, .keep_all = TRUE)
      } else {
        tibble::tibble(Empresa = character(), Moneda = character(),
                       Documento = character(),
                       FechaVenc_Proyectada = as.Date(character()))
      }

      prov_rows <- prov_rows |>
        dplyr::left_join(ledger_moves, by = c("Empresa", "Moneda", "Documento"),
                         na_matches = "never") |>
        dplyr::mutate(
          FechaVenc_Proyectada = as.Date(FechaVenc_Proyectada),
          FechaVenc_Politica   = as.Date(NA_character_),
          FechaEff             = dplyr::coalesce(FechaVenc_Proyectada, .data$FechaEff),
          Movida               = dplyr::if_else(!is.na(FechaVenc_Proyectada),
                                                "Manual", .data$Movida),
          .row_id              = dplyr::row_number()
        )

      # Provisions have no abonos or SAP field overrides — set directly.
      prov_rows[["abono_total"]]      <- 0
      prov_rows[["Saldo_original"]]   <- prov_rows[["Importe"]]
      prov_rows[["has_abono"]]        <- FALSE
      prov_rows[["has_sap_override"]] <- FALSE

      as.data.frame(prov_rows)
    })

    # ── Combined: merge moved-base + provisions, apply filters + confirmation ──
    # Reactive cache layers:
    #   df_base()       — re-runs only when SAP / manual / abonos / sap_ov change
    #   df_with_moves() — re-runs when moves or policy_moves change (~30 ms)
    #   df_prov_rows()  — re-runs when provisions change (~20 ms)
    #   df_combined()   — thin merge (~10 ms), always runs last
    df_combined <- reactive({
      t0_comb <- proc.time()
      emp <- shared$empresa_sel()

      message("[DF_COMBINED] start ledger=", ledger)

      base  <- df_with_moves()
      provs <- df_prov_rows()
      df    <- dplyr::bind_rows(base, provs)

      message("[DF_COMBINED] bind done ledger=", ledger,
              " rows=", if (is.null(df)) "NULL" else nrow(df),
              " in ", round((proc.time() - t0_comb)[["elapsed"]], 2), "s")

      if (is.null(df) || !nrow(df)) return(NULL)

      # Load papelera once.
      # SAP items soft-deleted here remain in df and are marked confirmed=TRUE
      # below (ghost: crossed out, excluded from sums).
      # Manual items: UUID-based anti-join below hides only the exact deleted
      # item; new adds with the same Empresa/Documento but a fresh UUID survive.
      # Provision items: use estado="deleted" — no anti-join needed.
      papelera <- if (!is.null(shared$papelera_rv)) {
        tryCatch(shared$papelera_rv(), error = function(e) NULL)
      } else {
        tryCatch(load_papelera(client_id = shared$effective_client_id()), error = function(e) NULL)
      }

      # Anti-join for MANUAL papelera items by UUID so that only the exact
      # deleted item is hidden. New manual items with the same
      # Empresa/Moneda/Documento but a fresh UUID remain visible.
      # SAP items are handled separately by Source 3 (confirmed=TRUE ghost).
      if (!is.null(papelera) && nrow(papelera)) {
        pap_this_m <- papelera[papelera[["ledger"]] == ledger |
                                 papelera[["ledger"]] == "MIXED", , drop = FALSE]
        if (nrow(pap_this_m) && "source" %in% names(pap_this_m)) {
          man_pap <- pap_this_m[!is.na(pap_this_m[["source"]]) &
                                  pap_this_m[["source"]] == "manual", , drop = FALSE]
          if (nrow(man_pap) && "id" %in% names(df)) {
            valid_ids <- man_pap[["id"]][!is.na(man_pap[["id"]]) &
                                          nzchar(man_pap[["id"]] %||% "")]
            if (length(valid_ids))
              df <- df[is.na(df[["id"]]) | !(df[["id"]] %in% valid_ids), , drop = FALSE]
          }
        }
      }

      if (length(emp) && "Empresa" %in% names(df)) {
        df_filtered <- dplyr::filter(df, Empresa %in% emp)
        if (nrow(df_filtered) == 0 && nrow(df) > 0) {
          warning("[DF_COMBINED] empresa filter ", paste(emp, collapse = ","),
                  " matched 0 of ", nrow(df), " rows — check empresas.rds vs snapshot")
          showNotification(
            "El filtro de Empresa no coincide con los datos cargados. Verifica la lista de empresas.",
            type = "warning", duration = 5)
        }
        df <- df_filtered
      }

      # ── Mark confirmed invoices ─────────────────────────────────────────────
      # Canonical 2-source implementation (Stage 9) — see compute_confirmed_flags()
      # in R/data_pipeline.R for the full source-by-source rationale. A third
      # source (pagar_hoy_db.status=="confirmed") existed pre-Stage-4 but is
      # now structurally dead: every confirm handler unconditionally unstages
      # the Agenda row regardless of source, so nothing can ever leave one
      # behind with status=="confirmed" again.
      # isolate() is intentionally NOT used here so the calendar re-renders
      # immediately when a confirmation happens in Agenda de Hoy.
      df <- compute_confirmed_flags(
        df, ledger,
        bancos_confirmados_df = tryCatch(shared$bancos_confirmados(), error = function(e) NULL),
        papelera_df           = papelera
      )
      message("[DF_COMBINED] complete ledger=", ledger,
              " final_rows=", nrow(df),
              " total ", round((proc.time() - t0_comb)[["elapsed"]], 2), "s")
      df
    })

    # ── Calendar-ready aggregation ───────────────────────────────────────────
    # Depends on month_val so it invalidates on navigation, but df_base is cached
    # so filtering one month and aggregating is much cheaper than all months.
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
                            ic_rfcs  = ic_rfcs, cmap = shared$company_map())

      # Filter to current month before aggregation — to_calendar_data only needs
      # the displayed month's rows; processing all history is wasteful.
      mstart_cal <- lubridate::floor_date(as.Date(req(shared$month_val[[ledger]]())), "month")
      mend_cal   <- as.Date(lubridate::ceiling_date(mstart_cal, "month") - lubridate::days(1))
      if ("FechaEff" %in% names(df)) {
        df_dates <- as.Date(df$FechaEff)
        df <- df[!is.na(df_dates) & df_dates >= mstart_cal & df_dates <= mend_cal, , drop = FALSE]
      }

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

      # Scope currency choices to the currently-viewed month so that the
      # selector auto-resets when a month has data only in one currency.
      # Without month-scoping: USD invoices from other months kept USD in the
      # selector even when the viewed month had only MXN invoices, causing an
      # empty calendar with no obvious explanation.
      mstart_cc <- tryCatch(
        lubridate::floor_date(as.Date(shared$month_val[[ledger]]()), "month"),
        error = function(e) NULL
      )
      month_curs <- if (!is.null(d) && nrow(d) && "Moneda" %in% names(d) &&
                        !is.null(mstart_cc) && "FechaEff" %in% names(d)) {
        mend_cc   <- as.Date(lubridate::ceiling_date(mstart_cc, "month") - lubridate::days(1))
        df_dates  <- as.Date(d$FechaEff)
        unique(d$Moneda[!is.na(df_dates) & df_dates >= mstart_cc & df_dates <= mend_cc])
      } else if (!is.null(d) && nrow(d) && "Moneda" %in% names(d)) {
        unique(d$Moneda)  # fallback: no FechaEff column — use all months
      } else {
        character()
      }
      manual_curs <- if (nrow(manual) && "Moneda" %in% names(manual)) manual$Moneda else character()
      raw_curs <- unique(c(month_curs, manual_curs))
      # MXN first, USD second, rest alphabetically — ensures selector defaults
      # to MXN even when old snapshots contain other currencies (e.g. EUR).
      priority <- c("MXN", "USD")
      data_currencies <- c(
        intersect(priority, raw_curs),
        sort(setdiff(raw_curs, priority))
      )
      choices <- if (length(data_currencies)) data_currencies else c("MXN", "USD")
      if (!identical(choices, isolate(shared$currency_choices[[ledger]]()))) {
        shared$currency_choices[[ledger]](choices)
      }
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

      tags_day_map   <- tryCatch(tags_day_map_rv(),  error = function(e) list())
      staged_keys_all <- tryCatch(staged_keys_rv(),  error = function(e) NULL)

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
        if (!nrow(d_cur)) {
          # Selected currency has no data this month.  If another currency does,
          # use it so the calendar renders with actual data while CUR_CHOICES
          # (priority=1) catches up and resets the selector on the next tick.
          alt_curs <- setdiff(unique(d$Moneda), cur)
          if (length(alt_curs)) {
            d_alt <- dplyr::filter(d, Moneda == alt_curs[1])
            if (nrow(d_alt)) {
              cur <- alt_curs[1]   # cur now reflects what is actually displayed
              d_alt
            } else empty_cal
          } else empty_cal
        } else d_cur
      }

      # Filter staged keys down to the effective display currency (after any
      # auto-switch above) so tile pills match the calendar being rendered.
      staged_keys_cur <- if (!is.null(staged_keys_all) && nrow(staged_keys_all)) {
        dplyr::filter(staged_keys_all,
                      toupper(trimws(Moneda)) == toupper(trimws(cur)))
      } else NULL

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
        tags_day             = tags_day_map,
        staged_keys          = staged_keys_cur,
        available_currencies = available
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
          id    = ns("cal_outer"),
          class = "cal-outer",
          style = "height:100%; overflow-y:auto; padding: 12px 16px 80px;",
          cal_html,
          diag_banner
        ),
        # OVERLAY FADE TIMING — ABANDONED
        # We tried sending Shiny.setInputValue('cal_ar_rendered', Math.random())
        # here so app.R could count post-Phase-2 AR renders and send calFadeNow.
        # n>=2: a second AR render never materialised — the 30 s JS fallback in
        #        overlay_init.js section 8 became the only thing fading it.
        # n>=1: overlay was already hidden before the browser round-trip returned
        #        to the server — something in JS was hiding it earlier; never confirmed.
        tags$script(HTML(paste0(
          "document.dispatchEvent(new CustomEvent('hop-cal-ready',{detail:{ledger:'",
          toupper(ledger),
          "'}}));"
        )))
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
    modal_open    <- reactiveVal(FALSE)        # TRUE while the day modal is showing

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
        d <- apply_ic_filter(d, mode, config$code_col, codes, ic_rfcs = ic_rfcs, cmap = shared$company_map())
        d <- add_tag_labels(d, shared$tags_db(), ledger)
        keep <- intersect(
          c("Empresa","Moneda","Documento","Factura","Parte","Importe",
            "FechaEff","FechaVenc_Original","FechaVenc_Proyectada",
            "Movida","Etiqueta","source","Tipo","Codigo","notas","confirmed",
            "has_sap_override","provision_id","id","is_ghost","is_paid_ghost"),
          names(d)
        )
        as.data.frame(d)[, keep, drop = FALSE]
      }, error = function(e) ctx$detail)
      # Not modifyList(): it walks into `detail` (a data.frame is a list) and
      # fails when the day has a different number of rows -- a delete, or a
      # move out of the day -- so the refresh used to fail right then and the
      # window closed or went stale (found 2026-11-01, T05).
      ctx$detail <- new_detail
      ctx$nonce  <- runif(1)
      ctx
    }

    # Single render gate — the ONLY place showModal is called
    observeEvent(modal_ctx(), {
      ctx <- modal_ctx()
      req(ctx)
      modal_open(TRUE)
      # A refresh (new data while the day is open) only changes the tables'
      # data: they repaint where they are, keeping view, scroll and selection.
      if (isTRUE(ctx[["prov_refresh"]])) return()
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

    # Track when the user (or code) closes the day modal so the provisions
    # observer doesn't re-open it for stale modal_ctx() values.
    # found 2026-08-03 (Stage 9, Issue B): this was input$cal_day_modal_closed
    # -- but ledgerModuleServer runs inside moduleServer(id, ...), so `input`
    # here is namespace-scoped to "ar-" or "ap-". The JS in ui_components.R
    # sends the bare root-level name (it has no way to know which module
    # instance, if any, currently owns the open modal -- there's only ever
    # one Bootstrap modal in the DOM at a time), so this observer's real
    # listened-for name (e.g. "ar-cal_day_modal_closed") never matched what
    # was actually sent, and never fired -- confirmed live via chromote
    # (a fire-counter reactiveVal stayed 0 across a real modal close). Fixed
    # by reading the ROOT session's input instead of this module's own --
    # session$rootScope() is the standard Shiny mechanism for exactly this
    # (a module reading a document-level, non-namespaced signal). Both AR's
    # and AP's instances react to the same global "a modal closed somewhere"
    # signal and reset THEIR OWN modal_open flag; harmless when it wasn't
    # theirs (already FALSE, a no-op) since only one modal can be open across
    # the whole app at a time, and the "single render gate" above immediately
    # re-sets modal_open(TRUE) on any subsequent re-render anyway (verified
    # live: replacing an open modal's content with a second showModal() call,
    # e.g. a delete-confirmation dialog, does NOT itself fire hidden.bs.modal
    # -- only an actual dismiss does, so this doesn't spuriously reset a
    # modal that's just being replaced mid-flow).
    observeEvent(session$rootScope()$input$cal_day_modal_closed, {
      modal_open(FALSE)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Auto-refresh day pane when any relevant shared dataset changes (a move,
    # delete, tag, abono confirmation, SAP field override, or provisions
    # convert/delete -- from THIS session or, now that all of these are
    # registered with sync_bus, from another session entirely) so the open
    # day pane updates live instead of only on the user's own next click.
    # Originally only watched pasivos_provisions_db (2026-07-xx); widened
    # 2026-07-30 to the full set that .refresh_ctx_detail() actually reads,
    # same template as R/interco_module.R's own observe() block for
    # ic_invoices, which already does this correctly for its own view.
    # suppress_ledger_prov_refresh is set TRUE by external PPM save handlers
    # before they write to pasivos_provisions_db; we consume it here so an
    # external add never re-opens the calendar modal.
    observeEvent({
      shared$moves_db()
      shared$manual_inv()
      shared$tags_db()
      shared$papelera_rv()
      shared$bancos_confirmados()
      shared$abonos_db()
      shared$pasivos_provisions_db()
    }, ignoreInit = TRUE, {
      if (!is.null(shared$suppress_ledger_prov_refresh) &&
          isTRUE(isolate(shared$suppress_ledger_prov_refresh()))) {
        shared$suppress_ledger_prov_refresh(FALSE)
        return()
      }
      if (!isolate(modal_open())) return()
      ctx <- isolate(modal_ctx())
      if (is.null(ctx)) return()
      new_ctx <- tryCatch(.refresh_ctx_detail(ctx), error = function(e) NULL)
      if (is.null(new_ctx)) return()
      new_ctx[["prov_refresh"]] <- TRUE
      modal_ctx(new_ctx)
    })

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

      # Mirror the same currency-fallback logic used in output$calendar so the
      # click handler uses the same effective currency even when the selectInput
      # hasn't finished binding (returns character(0) instead of "MXN").
      available_cur <- shared$currency_choices[[ledger]]()
      sel_cur       <- shared$currency[[ledger]]()
      cur <- toupper(trimws(
        if (!is.null(sel_cur) && length(sel_cur) == 1L && nzchar(sel_cur) &&
            sel_cur %in% available_cur)
          sel_cur
        else if (length(available_cur))
          available_cur[[1L]]
        else
          "MXN"
      ))
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

      detail <- apply_ic_filter(detail, mode, config$code_col, codes, ic_rfcs = ic_rfcs, cmap = shared$company_map())

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
          "has_sap_override","provision_id","id","is_ghost","is_paid_ghost"),
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

    # ── Agenda: add (Agregar todo, Agregar selección, a row's ＋) ──────────────
    # keys: the day's rows the table sent (dia_llaves(), R/dia_module.R);
    # origen says which of the three, for the log.
    .dia_agregar <- function(keys, origen) {
      # Sending to the Agenda needs can_register_payments (Luis, 2026-11-03);
      # without it the buttons are hidden too (dia_server).
      if (!puede_o_avisa(shared, "can_register_payments")) return()
      ctx <- modal_ctx()
      req(ctx)
      detail <- ctx$detail
      # found 2026-08-04: pasivos_filter_out_provisions(keys) here was a
      # no-op -- `keys` never carries a `source` column (see
      # pasivos_exclude_provision_keys()'s own header comment in
      # pasivos_calendar_glue.R for the full incident). Use the helper that
      # actually works on a post-selection keys object.
      keys <- pasivos_exclude_provision_keys(keys, detail)
      if (is.null(keys) || !nrow(keys)) {
        showNotification(tr("tablas.agenda.sin_facturas_provisiones"), type = "warning")
        return()
      }
      detail_np <- pasivos_filter_out_provisions(detail)
      # Source lookup (found 2026-07-24): without it every staged row loses
      # its source and is normalized to "sap" on the next load_pagar_hoy(), so
      # a manual entry confirmed later never gets archived out of manual_inv.
      src_lookup <- if ("source" %in% names(detail_np))
        unique(detail_np[, c("Empresa","Moneda","Documento","source"), drop = FALSE])
      else NULL
      detail_lu <- .fresh_lu(
        keys, ctx$amt,
        unique(detail_np[, c("Empresa","Moneda","Documento","Parte","Codigo","Importe","FechaEff"), drop = FALSE]))
      # found 2026-08-04 (live crash: "undefined columns selected" at the
      # final column-select below): in summary mode, `keys` already carries
      # its own Parte (from .resolve_summary_sel()/.summary_sel_to_keys()) --
      # audit mode's keys never do, which is why this only broke in summary
      # mode. Merging that Parte against detail_lu's OWN Parte (neither is a
      # join column) renamed both to Parte.x/Parte.y instead of erroring at
      # merge time, so the crash only surfaced later, at the plain "Parte"
      # column-select. detail_lu is the authoritative, freshly-read source
      # for Parte/Codigo/FechaEff -- keys only needs to contribute the join
      # columns here, exactly like stage_all's inv_keys already does.
      new_rows  <- merge(keys[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE],
                         detail_lu, by = c("Empresa","Moneda","Documento","Importe"))
      new_rows[["id"]]        <- vapply(seq_len(nrow(new_rows)), function(x) uuid::UUIDgenerate(), character(1))
      new_rows[["ledger"]]    <- ledger
      new_rows[["tipo_item"]] <- "factura"
      new_rows[["FechaVenc"]] <- as.Date(new_rows[["FechaEff"]])
      new_rows[["staged_by"]] <- shared$current_user()
      new_rows[["staged_at"]] <- Sys.time()
      new_rows[["status"]]    <- "pending"
      if (!is.null(src_lookup) && nrow(src_lookup)) {
        new_rows <- merge(new_rows, src_lookup, by = c("Empresa","Moneda","Documento"), all.x = TRUE)
        new_rows[["source"]] <- ifelse(is.na(new_rows[["source"]]) | new_rows[["source"]] != "manual",
                                       SAP_B1_ERP_TYPE, "manual")
      } else {
        new_rows[["source"]] <- SAP_B1_ERP_TYPE
      }
      new_rows <- dia_filas_agenda(new_rows, tryCatch(as.data.frame(df_combined()), error = function(e) NULL) %||% detail,
                                   shared$current_user(), ledger,
                                   tryCatch(shared$bancos_confirmados(), error = function(e) NULL))
      rechazadas <- attr(new_rows, "rechazadas")
      if (length(rechazadas))
        showNotification(tr("tablas.agenda.ya_confirmadas", documentos = paste(rechazadas, collapse = ", ")),
                         type = "warning", duration = 6)
      if (!nrow(new_rows)) return()
      new_rows <- new_rows[, c("id","ledger","Empresa","Moneda","Documento",
                                "Parte","Codigo","tipo_item","Importe","FechaVenc","staged_by","staged_at","status","source",
                                "fecha_factura"), drop = FALSE]
      # Read fresh from S3 first, not this session's possibly-stale in-memory
      # copy -- this writes the whole table back, so a stale base would
      # silently drop another session's concurrent stage/unstage. Must be
      # safe_load_pagar_hoy(), not the mode-blind load_pagar_hoy() (found
      # 2026-07-25: the latter always reads the legacy S3 key regardless of
      # sync/per-user mode, corrupting the live agenda -- see Stage 16's
      # correction in docs/LEDGER_INTEGRITY_MASTER_PLAN.md).
      ph_base <- tryCatch(safe_load_pagar_hoy(username = shared$current_user(),
                                              client_id = shared$effective_client_id()),
                          error = function(e) NULL) %||% shared$pagar_hoy_db()
      updated <- upsert_pagar_hoy(ph_base, new_rows,
                                  keys = c("ledger","Empresa","Moneda","Documento","Importe"))
      shared$pagar_hoy_db(updated)
      # found 2026-08-05: same bare-call bug fixed in the confirm/quitar
      # handlers (see save_pagar_hoy_with_retry()'s header comment in
      # R/persistence.R for the full incident) -- a transient failure here
      # left the acting user's own screen showing the item staged (the
      # reactiveVal above already updated) while nothing persisted and
      # nothing got logged.
      stage_result <- save_pagar_hoy_with_retry(updated, shared$current_user(),
                                                client_id = shared$effective_client_id(),
                                                base = ph_base)   # CM09: rebased on S3 (A2)
      if (!isTRUE(stage_result$ok)) {
        showNotification(tr("tablas.agenda.no_enviado", n = stage_result$attempts),
                         type = "error", duration = 15)
        tryCatch(log_action(
          user = tryCatch(shared$current_user(), error = function(e) "system"),
          module = paste0("ledger_", ledger), action = "enviar_a_agenda_dia_incompleto",
          description = sprintf(
            "Intento de enviar %d factura(s) no se pudo guardar tras %d intento(s): %s",
            nrow(new_rows), stage_result$attempts,
            if (!is.null(stage_result$error)) conditionMessage(stage_result$error) else "error desconocido"),
          target_id = paste(new_rows[["Documento"]][seq_len(min(5, nrow(new_rows)))], collapse = ", "),
          metadata  = list(n = nrow(new_rows), source = origen),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        ), error = function(e) NULL)
        return()
      }
      lbl <- if (ledger == "AR") "Agenda del d\u00eda (Cobros)" else "Agenda del d\u00eda (Pagos)"
      emps <- paste(unique(new_rows[["Empresa"]]), collapse = ", ")
      showNotification(
        tr(if (ledger == "AR") "tablas.agenda.enviadas_cobros" else "tablas.agenda.enviadas_pagos",
           n = nrow(new_rows), empresas = emps),
        type = "message", duration = 3)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = paste0("ledger_", ledger),
        action      = "enviar_a_agenda_dia",
        description = paste0(nrow(new_rows), " factura(s) enviada(s) a ", lbl, " -- ", emps),
        target_id   = paste(new_rows[["Documento"]][seq_len(min(5, nrow(new_rows)))], collapse = ", "),
        metadata    = list(n = nrow(new_rows), source = origen),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }

    # ── Raw data diagnostic viewer ("Ver datos SAP", both views) ───────────────
    # Two engine tables (datos_sap, T21), made once here: the day's rows as
    # processed, and every raw SAP field of those invoices.
    crudos_proc <- reactiveVal(NULL)
    crudos_sap  <- reactiveVal(NULL)
    datos_sap_tabla("dia_crudos_proc", crudos_proc)
    datos_sap_tabla("dia_crudos_sap",  crudos_sap)
    .dia_crudos <- function() {
      ctx <- modal_ctx()
      req(ctx)
      d <- ctx$detail

      # Pull in the widest possible set of raw columns from sap_data for this day.
      # Match by Empresa + Documento of the rows actually shown in the day, not by
      # the original due date: the day holds rows by FechaEff, so overdue invoices
      # rolled forward to today (and moved ones) never matched and this came back
      # with 0 rows.
      raw <- shared$sap_data()[[ledger]]
      raw_day <- if (!is.null(raw) && nrow(raw)) {
        if (all(c("Empresa", "Documento") %in% names(raw)) &&
            all(c("Empresa", "Documento") %in% names(d))) {
          key_raw <- paste(raw[["Empresa"]], as.character(raw[["Documento"]]), sep = "\r")
          key_day <- paste(d[["Empresa"]],   as.character(d[["Documento"]]),   sep = "\r")
          raw[key_raw %in% key_day, , drop = FALSE]
        } else {
          raw[!is.na(raw[["Fecha de vencimiento"]]) &
              as.Date(raw[["Fecha de vencimiento"]]) == ctx$sel_date, , drop = FALSE]
        }
      } else NULL
      con_ledger <- function(x) { if (!is.null(x) && nrow(x)) x[["Ledger"]] <- ledger; x }
      crudos_proc(con_ledger(d))
      crudos_sap(con_ledger(raw_day))

      # The table's window (T09): the counts go in the header, no footer.
      showModal(hd_modal(
        titulo = tr("tablas.dia.crudos_titulo", fecha = format(ctx$sel_date, "%d %b %Y"), moneda = ctx$cur),
        encabezado = tags$small(class = "text-muted",
            tr("tablas.sap.dia_conteo", detalle = nrow(d),
               sap = if (!is.null(raw_day)) nrow(raw_day) else "?")),
        tagList(
          tags$h6(tr("tablas.sap.dia_procesados"), class = "text-muted mb-1"),
          hd_table_ui(ns("dia_crudos_proc")),
          tags$hr(),
          if (!is.null(raw_day) && nrow(raw_day)) tagList(
            tags$h6(tr("tablas.sap.dia_crudos"), class = "text-muted mb-1"),
            hd_table_ui(ns("dia_crudos_sap"))
          ) else NULL
        )
      ))
    }

    # ── Agenda: take out (a row's ✓) ───────────────────────────────────────────
    # The row's ＋ adds (.dia_agregar) and, once the row is in the Agenda,
    # the same button takes it out -- as the day's ＋/✓ always did. Anything
    # may leave the Agenda regardless of source: it never holds the real data.
    .dia_quitar <- function(keys) {
      inv_keys <- unique(keys[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE])
      # Read fresh from S3 first (safe_load_pagar_hoy -- mode-aware, found
      # 2026-07-25): this writes the whole table back.
      ph_now <- tryCatch(safe_load_pagar_hoy(username = shared$current_user(),
                                             client_id = shared$effective_client_id()),
                         error = function(e) NULL) %||% shared$pagar_hoy_db()
      if (is.null(ph_now) || !nrow(ph_now)) return()
      upd_keys <- cbind(inv_keys, ledger = ledger, stringsAsFactors = FALSE)
      updated  <- unstage_pagar_hoy(ph_now, upd_keys, keys = c("ledger","Empresa","Moneda","Documento","Importe"))
      shared$pagar_hoy_db(updated)
      # save_pagar_hoy_with_retry (found 2026-08-05): a bare save left the
      # screen updated while nothing persisted and nothing got logged.
      site_result <- save_pagar_hoy_with_retry(updated, shared$current_user(),
                                               client_id = shared$effective_client_id(),
                                               base = ph_now)   # CM09: rebased on S3 (A2)
      if (!isTRUE(site_result$ok)) {
        showNotification(
          paste0("No se pudo quitar de la Agenda tras ", site_result$attempts,
                 " intento(s) (posible problema de conexi\u00f3n). Actualiza la p\u00e1gina e intenta de nuevo."),
          type = "error", duration = 15
        )
        tryCatch(log_action(
          user = tryCatch(shared$current_user(), error = function(e) "system"),
          module = paste0("ledger_", ledger), action = "quitar_de_agenda_dia_incompleto",
          description = sprintf("No se pudo guardar tras %d intento(s): %s", site_result$attempts,
            if (!is.null(site_result$error)) conditionMessage(site_result$error) else "error desconocido"),
          target_id = paste(inv_keys[["Documento"]][seq_len(min(5, nrow(inv_keys)))], collapse = ", "),
          metadata  = list(source = "dia_quitar"),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        ), error = function(e) NULL)
        return()
      }
      showNotification("Quitado de la Agenda del día.", type = "message", duration = 2)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = paste0("ledger_", ledger),
        action      = "quitar_de_agenda_dia",
        description = paste0(nrow(inv_keys), " factura(s) quitada(s) de la Agenda del día"),
        target_id   = paste(inv_keys[["Documento"]][seq_len(min(5, nrow(inv_keys)))], collapse = ", "),
        metadata    = list(n = nrow(inv_keys), source = "dia_quitar"),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
    }

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

    # ── Move / restore buttons ────────────────────────────────────────────────
    .dia_mover <- function(keys, new_date) {
      tryCatch({
        ctx <- modal_ctx(); req(ctx)
        if (is.null(new_date) || length(new_date) == 0L || anyNA(new_date)) {
          showNotification("Elige una fecha válida.", type = "warning")
          return()
        }
        detail <- ctx$detail
        # found 2026-08-04: same no-op as stage_sel (see
        # pasivos_exclude_provision_keys()'s header comment in
        # pasivos_calendar_glue.R) -- `keys` here never has a `source`
        # column, so pasivos_filter_out_provisions(keys) never excluded
        # anything. A provision selected alongside real invoices would get
        # its date force-moved through this generic mechanism instead of
        # going through its own Pasivos edit path.
        keys <- pasivos_exclude_provision_keys(keys, detail)
        if (is.null(keys) || !nrow(keys)) {
          showNotification("No se encontraron facturas reales (sólo provisiones).", type = "warning")
          return()
        }
        .apply_move(keys, new_date, ledger, shared, session = session)
        .sync_staged(keys, ledger, shared$pagar_hoy_db, new_date = new_date,
                     username = shared$current_user(), client_id = shared$effective_client_id())
        showNotification(paste0(nrow(keys), " factura(s) movidas."), type = "message", duration = 2)
        cur_ctx <- modal_ctx()
        if (!is.null(cur_ctx)) {
          new_ctx <- tryCatch(.refresh_ctx_detail(cur_ctx), error = function(e) NULL)
          if (!is.null(new_ctx)) {
            new_ctx[["prov_refresh"]] <- TRUE
            modal_ctx(new_ctx)
          } else removeModal()
        } else removeModal()
      }, error = function(e) {
        message("[do_move] ERROR: ", conditionMessage(e))
        showNotification(paste("Error al mover:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    }

    .dia_restaurar <- function(keys) {
      tryCatch({
        ctx <- modal_ctx(); req(ctx)
        detail <- ctx$detail
        # found 2026-08-04: same no-op as stage_sel/do_move (see
        # pasivos_exclude_provision_keys()'s header comment in
        # pasivos_calendar_glue.R).
        keys <- pasivos_exclude_provision_keys(keys, detail)
        if (is.null(keys) || !nrow(keys)) {
          showNotification("No se encontraron facturas reales (sólo provisiones).", type = "warning")
          return()
        }
        .clear_moves(keys, ledger, shared, session = session)
        .sync_staged(keys, ledger, shared$pagar_hoy_db, detail = detail,
                     username = shared$current_user(), client_id = shared$effective_client_id())
        showNotification("Fecha original restaurada.", type = "message", duration = 2)
        cur_ctx <- modal_ctx()
        if (!is.null(cur_ctx)) {
          new_ctx <- tryCatch(.refresh_ctx_detail(cur_ctx), error = function(e) NULL)
          if (!is.null(new_ctx)) {
            new_ctx[["prov_refresh"]] <- TRUE
            modal_ctx(new_ctx)
          } else removeModal()
        } else removeModal()
      }, error = function(e) {
        message("[do_restore] ERROR: ", conditionMessage(e))
        showNotification(paste("Error al restaurar:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    }

    # ── Delete ────────────────────────────────────────────────────────────────
    # The table asked first, for every row (Luis: deletes confirm). SAP rows
    # go to the papelera as ghosts; provisions and manual entries go through
    # .borrar_no_sap() below.
    .dia_eliminar <- function(keys) {
      ctx <- modal_ctx(); req(ctx)
      detail <- ctx$detail
      if (!nrow(keys)) {
        showNotification("No se encontraron facturas.", type = "warning")
        return()
      }
      n <- nrow(keys)

      # Split: SAP items ghost immediately (no dialog needed — they are reversible);
      # provisions and manual items need explicit confirmation.
      has_pid_col <- "provision_id" %in% names(keys) && "provision_id" %in% names(detail)
      key_sources <- vapply(seq_len(n), function(i) {
        pid <- if (has_pid_col) keys[["provision_id"]][i] else NA_character_
        if (!is.na(pid) && nzchar(pid %||% "")) return("provision")
        emp <- keys[["Empresa"]][i]
        doc <- keys[["Documento"]][i]
        m   <- detail[!is.na(detail[["Empresa"]]) & detail[["Empresa"]] == emp &
                      !is.na(detail[["Documento"]]) & detail[["Documento"]] == doc,
                      , drop = FALSE]
        if (nrow(m) && "source" %in% names(m) && !is.na(m[["source"]][1]))
          m[["source"]][1] else SAP_B1_ERP_TYPE
      }, character(1))
      # key_sources already folds the is_erp_sourced() NA-defaults-to-SAP_B1_ERP_TYPE
      # logic into its own "else SAP_B1_ERP_TYPE" fallback above — never NA by here.
      # Matches both the historical literal "sap" and today's SAP_B1_ERP_TYPE
      # value, since either can appear depending on when a row was created.
      sap_mask     <- key_sources %in% c("sap", SAP_B1_ERP_TYPE)
      non_sap_keys <- keys[!sap_mask, , drop = FALSE]

      # SAP items: send to papelera — they become ghosts (confirmed=TRUE),
      # not permanently deleted.
      n_sap <- sum(sap_mask)
      if (n_sap > 0) {
        sap_pap     <- tryCatch(load_papelera(client_id = shared$effective_client_id()),
                                error = function(e) .schema_papelera() |> dplyr::slice(0))
        sap_cols    <- intersect(c("Empresa","Moneda","Documento","Importe"), names(keys))
        sap_keys_sub <- keys[sap_mask, sap_cols, drop = FALSE]
        sap_detail  <- tryCatch(
          merge(detail, sap_keys_sub, by = sap_cols),
          error = function(e) detail[0, , drop = FALSE]
        )
        # BUG (found 2026-09-03, same class already fixed for
        # .save_moves_deferred()): the save used to be tryCatch'd only to a
        # transient warning toast, never gating the success notification or
        # the Activity-log write below it -- a failed save still showed "sent to
        # papelera" and logged it as a fact. Now retries, shows a persistent
        # error and skips the false-success log/notification if it still
        # fails.
        save_ok <- TRUE
        if (nrow(sap_detail) > 0) {
          sap_pap <- add_to_papelera(sap_pap, sap_detail,
                                     ledger, deleted_by = shared$current_user())
          result  <- s3_save_with_retry(function()
            save_papelera(sap_pap, client_id = shared$effective_client_id()))
          save_ok <- isTRUE(result$ok)
          if (save_ok) {
            if (!is.null(shared$papelera_rv)) shared$papelera_rv(sap_pap)
          } else {
            showNotification(
              paste0("No se pudo guardar la papelera (\"", result$error$message,
                     "\"). Las facturas SAP NO quedaron enviadas a papelera -- vuelve a intentarlo."),
              type = "error", duration = NULL
            )
          }
        }
        if (save_ok) {
          showNotification(
            paste0(n_sap, " factura(s) SAP tachada(s) y enviada(s) a la papelera."),
            type = "message", duration = 3
          )
        }
        log_action(
          user        = tryCatch(shared$current_user(), error = function(e) "system"),
          module      = paste0("ledger_", ledger),
          action      = if (save_ok) "eliminar_facturas_sap" else "eliminar_facturas_sap_fallo_guardado",
          description = if (save_ok)
            paste0(n_sap, " factura(s) SAP enviada(s) a papelera")
          else
            paste0("FALLO AL GUARDAR (no se aplico realmente): ", n_sap, " factura(s) SAP a papelera"),
          target_id   = paste(keys[["Documento"]][sap_mask][seq_len(min(5, n_sap))], collapse = ", "),
          metadata    = list(n = n_sap),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        )
      }

      # If ALL selected items were SAP, refresh the day modal right now and stop.
      if (nrow(non_sap_keys) == 0) {
        cur_ctx <- modal_ctx()
        if (!is.null(cur_ctx)) {
          new_ctx <- tryCatch(.refresh_ctx_detail(cur_ctx), error = function(e) NULL)
          if (!is.null(new_ctx)) {
            new_ctx[["prov_refresh"]] <- TRUE
            modal_ctx(new_ctx)
          } else removeModal()
        } else removeModal()
        return()
      }

      # Non-SAP items (provisions, manual).
      .borrar_no_sap(non_sap_keys, ctx)
    }

    .borrar_no_sap <- function(keys, ctx) {

      detail <- ctx$detail
      papelera_df <- tryCatch(load_papelera(client_id = shared$effective_client_id()), error = function(e) .schema_papelera() |> dplyr::slice(0))

      # Get full detail rows matching the keys.
      # Provision rows are matched by provision_id for precision — two provisions that
      # share the same Documento (same recurring obligation, different occurrences) must
      # not cascade: deleting one should not silently delete its sibling.
      # Non-provision rows use the document key as before.
      has_pid_in_keys <- "provision_id" %in% names(keys) && "provision_id" %in% names(detail)
      if (has_pid_in_keys && any(!is.na(keys[["provision_id"]]) & nzchar(keys[["provision_id"]] %||% ""))) {
        prov_pids    <- keys[["provision_id"]][!is.na(keys[["provision_id"]]) &
                                               nzchar(keys[["provision_id"]] %||% "")]
        other_keys   <- keys[is.na(keys[["provision_id"]]) | !nzchar(keys[["provision_id"]] %||% ""),
                             c("Empresa","Moneda","Documento","Importe"), drop = FALSE]
        prov_detail  <- detail[!is.na(detail[["provision_id"]]) &
                                detail[["provision_id"]] %in% prov_pids, , drop = FALSE]
        other_detail <- detail[is.na(detail[["provision_id"]]) |
                                !(detail[["provision_id"]] %in% prov_pids), , drop = FALSE]
        # Include Parte in the merge key when available so deletion stays within
        # the selected group — without it, two entries sharing (Empresa, Moneda,
        # Documento, Importe) but in different Parte groups would both be removed.
        other_merge_cols <- intersect(c("Empresa","Moneda","Documento","Importe","Parte"),
                                      names(other_keys))
        other_del <- if (nrow(other_keys) > 0 && nrow(other_detail) > 0)
          merge(other_detail, other_keys[, other_merge_cols, drop = FALSE],
                by = other_merge_cols)
        else other_detail[0, , drop = FALSE]
        rows_to_delete <- dplyr::bind_rows(prov_detail, other_del)
      } else {
        merge_cols <- intersect(c("Empresa","Moneda","Documento","Importe","Parte"),
                                names(keys))
        rows_to_delete <- merge(
          detail,
          keys[, merge_cols, drop = FALSE],
          by = merge_cols
        )
      }

      # Paid provisions stay out of a Calendario delete (see PROVISION_PAGADA).
      # Fresh read: another session may have confirmed the payment meanwhile.
      provs_db <- if ("provision_id" %in% names(rows_to_delete))
        tryCatch(load_pasivos_provisions(client_id = shared$effective_client_id()),
                 error = function(e) NULL)
      provs_base <- provs_db   # CM09: the save applies only these deletes on S3 as it is now
      pagadas <- provisiones_pagadas(provs_db, rows_to_delete[["provision_id"]])
      if (length(pagadas)) {
        rows_to_delete <- rows_to_delete[is.na(rows_to_delete[["provision_id"]]) |
                                           !(rows_to_delete[["provision_id"]] %in% pagadas), , drop = FALSE]
        showNotification(tr_n("pasivos.borrar_pagada_bloqueado", length(pagadas)),
                         type = "warning", duration = 8)
        # The table already confirmed (no dialog to close): the day stays open.
        if (!nrow(rows_to_delete)) return()
      }

      # BUG (found 2026-09-03, same class already fixed for
      # .save_moves_deferred()): each of these three saves used to be
      # tryCatch'd only to a transient warning toast, never gating the
      # success notification or the Activity-log write below -- any subset of them
      # could silently fail to persist while the user still saw "sent to
      # papelera" and Actividad still logged it as a fact. Now each retries,
      # and the final notification/log is honest about which pieces (if any)
      # never actually saved.
      save_failures <- character(0)

      # Add to papelera
      papelera_df <- add_to_papelera(papelera_df, rows_to_delete,
                                      ledger, deleted_by = shared$current_user())
      papelera_result <- s3_save_with_retry(function()
        save_papelera(papelera_df, client_id = shared$effective_client_id()))
      if (isTRUE(papelera_result$ok)) {
        # Update shared reactive so df_combined invalidates without another S3 read
        if (!is.null(shared$papelera_rv)) shared$papelera_rv(papelera_df)
      } else {
        save_failures <- c(save_failures, paste0("papelera (", papelera_result$error$message, ")"))
      }

      # For manual invoices: remove from manual_inv
      manual_keys <- rows_to_delete[rows_to_delete[["source"]] == "manual", , drop = FALSE]
      if (nrow(manual_keys) > 0 && !is.null(manual_keys[["id"]])) {
        # Read fresh from S3, same as papelera_df above -- this deletes
        # rows, so a stale in-memory read here can silently resurrect a row
        # another session already removed (found 2026-07-24, a real
        # incident).
        m <- tryCatch(load_manual(client_id = shared$effective_client_id()),
                     error = function(e) NULL) %||% shared$manual_inv()
        m_base <- m   # what the deletes start from (CM09: applied on S3 as it is now)
        for (mid in manual_keys[["id"]]) {
          m <- delete_manual(m, mid)
        }
        manual_result <- s3_save_with_retry(function()
          save_manual(m, client_id = shared$effective_client_id(), base = m_base))
        if (isTRUE(manual_result$ok)) {
          shared$manual_inv(m)
        } else {
          save_failures <- c(save_failures, paste0("manual_inv (", manual_result$error$message, ")"))
        }
      }

      # For SAP invoices: already handled in do_delete (immediate ghost via papelera confirmed=TRUE).

      # For provisions: set estado = "deleted" so pasivos_provisions_as_ledger_rows
      # stops regenerating them (papelera alone is not enough — the provision would
      # keep re-appearing each reactive cycle).
      prov_ids <- if ("provision_id" %in% names(rows_to_delete))
        rows_to_delete[["provision_id"]][!is.na(rows_to_delete[["provision_id"]]) &
                                         nzchar(rows_to_delete[["provision_id"]] %||% "")]
      else character(0)
      if (length(prov_ids) > 0 && !is.null(shared$pasivos_provisions_db)) {
        # provs_db was read fresh from S3 above, not this session's copy.
        if (is.null(provs_db))
          save_failures <- c(save_failures, "provisiones (no se pudieron leer)")
        if (!is.null(provs_db) && nrow(provs_db)) {
          del_idx <- which(provs_db[["id"]] %in% prov_ids & !is.na(provs_db[["id"]]))
          # captured BEFORE mutation, one row per id, for the audit entries
          # below (found 2026-09-05: this path deleted the provision but
          # never called pasivos_log_audit() at all -- unlike the other 3
          # provision-delete entry points (convert modal, manage-converted
          # UI, Search/Vencidos' pasivos_provision_cancel), so it never got
          # a structured provision.deleted record, only the generic ledger-
          # wide activity log call further below).
          before_rows <- provs_db[del_idx, , drop = FALSE]
          provs_db[del_idx, "estado"] <- "deleted"
          prov_result <- s3_save_with_retry(function()
            save_pasivos_provisions(provs_db, client_id = shared$effective_client_id(), base = provs_base))
          if (isTRUE(prov_result$ok)) {
            tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
            shared$pasivos_provisions_db(provs_db)
            bump_sync_version("pasivos_provisions_db", client_id = shared$effective_client_id())
            for (i in seq_len(nrow(before_rows))) {
              pid <- before_rows[["id"]][i]
              after_row <- provs_db[provs_db[["id"]] == pid, , drop = FALSE]
              tryCatch(pasivos_log_audit(
                action_type = "provision.deleted",
                user        = tryCatch(shared$current_user(), error = function(e) "system"),
                target_kind = "provision", target_id = pid,
                before      = as.list(before_rows[i, , drop = FALSE]),
                after       = as.list(after_row[1, , drop = FALSE]),
                notes       = "deleted via Calendario day-modal batch delete",
                client_id   = shared$effective_client_id()
              ), error = function(e) NULL)
            }
          } else {
            save_failures <- c(save_failures, paste0("provisiones (", prov_result$error$message, ")"))
          }
        }
      }

      if (length(save_failures)) {
        showNotification(
          paste0("No se pudo guardar completamente el borrado (", paste(save_failures, collapse = "; "),
                 "). Puede que quede en un estado parcial -- vuelve a intentarlo en unos minutos y ",
                 "no cierres la sesion mientras tanto."),
          type = "error", duration = NULL
        )
      }

      n_del <- nrow(rows_to_delete)
      # found 2026-08-03 (Stage 9, Issue D): this batch delete already got
      # logged every time (the call further down, unconditionally) even when
      # a provision was part of the batch -- Mouse's "deletes don't show in
      # Actividad" report was not this path going unlogged. But the
      # description was generic ("N factura(s)...") regardless of what was
      # actually deleted, so an entry for a provision-only delete read
      # identically to a real-invoice delete
      # -- not a missing log, but not a "sensible, readable" one either (the
      # bar this stage's doc set). Now names provisions explicitly when
      # present, same idea as pasivos_log_audit()'s notes fields elsewhere.
      n_prov <- if ("provision_id" %in% names(rows_to_delete))
        sum(!is.na(rows_to_delete[["provision_id"]]) &
            nzchar(rows_to_delete[["provision_id"]] %||% ""))
        else 0L
      desc <- if (n_prov > 0)
        sprintf("%d factura(s) enviada(s) a papelera (incluye %d provisión(es))", n_del, n_prov)
      else
        sprintf("%d factura(s) enviada(s) a papelera", n_del)
      session$userData[[paste0(ns("pending_delete_keys"))]] <- NULL
      session$userData[[paste0(ns("pending_delete_ctx"))]]  <- NULL
      if (!length(save_failures)) {
        showNotification(paste0(n_del, " factura(s) enviadas a la papelera."),
                         type = "message", duration = 3)
      }
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = paste0("ledger_", ledger),
        action      = if (!length(save_failures)) "eliminar_facturas" else "eliminar_facturas_fallo_guardado",
        description = if (!length(save_failures)) desc
          else paste0("FALLO AL GUARDAR (estado parcial): ", desc, " -- errores: ",
                      paste(save_failures, collapse = "; ")),
        target_id   = paste(rows_to_delete[["Documento"]][seq_len(min(5, nrow(rows_to_delete)))],
                            collapse = ", "),
        metadata    = list(
          n      = n_del,
          n_provisiones = n_prov,
          source = unique(rows_to_delete[["source"]] %||% SAP_B1_ERP_TYPE)
        ),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
      # The day's tables show the fresh data.
      cur_ctx <- modal_ctx()
      if (!is.null(cur_ctx)) {
        new_ctx <- tryCatch(.refresh_ctx_detail(cur_ctx), error = function(e) NULL)
        if (!is.null(new_ctx)) {
          new_ctx[["prov_refresh"]] <- TRUE
          modal_ctx(new_ctx)
        }
      }
    }

    # Spanish label for a tag action's log description -- kept in sync by
    # hand with the identical helper in R/search_module.R's
    # handle_invoice_action() (tag_urgent/tag_important/tag_both/tag_clear),
    # so a tag applied from Calendario logs identically to one applied from
    # Vencidos or the Search modal.
    .tag_label_es <- function(new_tags) {
      if (!length(new_tags)) "sin etiqueta"
      else if (length(new_tags) == 2L) "urgente e importante"
      else if (identical(new_tags, "urgent")) "urgente"
      else "importante"
    }

    # ── Tags (Etiqueta A/B/C, Quitar etiqueta) ───────────────────────────────
    .dia_etiquetar <- function(rows, new_tags) {
      # From S3 as it is now, not the session's copy (etiquetas_frescas() in
      # R/search_module.R): another session may have just tagged.
      tags_db <- etiquetas_frescas(shared)
      tags_base <- tags_db   # CM09: if that read failed this is the session's copy; save rebases on S3
      for (i in seq_len(nrow(rows))) {
        tags_db <- set_invoice_tags(tags_db, ledger, rows[["Empresa"]][i], rows[["Moneda"]][i],
                                    rows[["Documento"]][i], new_tags, tagged_by=shared$current_user())
      }
      # BUG (found 2026-09-03, same class already fixed for
      # .save_moves_deferred()): save_tags() used to have NO error handling
      # at all (an uncaught throw would crash the observer), and the success
      # notification/Activity-log write below fired unconditionally either way --
      # a failed save still showed "tags updated" and logged it as a fact.
      # Now retries, and skips the false-success notification/log if it
      # still fails, showing a persistent error instead.
      shared$tags_db(tags_db)
      tag_result <- s3_save_with_retry(function()
        save_tags(tags_db, client_id = shared$effective_client_id(), base = tags_base))
      if (isTRUE(tag_result$ok)) {
        bump_sync_version("tags_db", client_id = shared$effective_client_id())
        showNotification("Etiquetas actualizadas.", type = "message", duration = 2)
      } else {
        showNotification(
          paste0("No se pudieron guardar las etiquetas (\"", tag_result$error$message,
                 "\"). El cambio se ve en pantalla pero NO quedo guardado -- vuelve a intentarlo."),
          type = "error", duration = NULL
        )
      }
      tryCatch(
        log_action(
          user        = tryCatch(shared$current_user(), error = function(e) "system"),
          module      = paste0("ledger_", ledger),
          action      = if (isTRUE(tag_result$ok)) "etiquetar" else "etiquetar_fallo_guardado",
          description = if (isTRUE(tag_result$ok))
            sprintf("%d factura(s) etiquetada(s) como %s", nrow(rows), .tag_label_es(new_tags))
          else
            sprintf("FALLO AL GUARDAR (no se aplico realmente): %d factura(s) etiquetada(s) como %s -- error S3: %s",
                    nrow(rows), .tag_label_es(new_tags), tag_result$error$message),
          target_id   = paste(rows[["Documento"]][seq_len(min(5, nrow(rows)))], collapse = ", "),
          metadata    = list(n = nrow(rows), tags = new_tags),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        ),
        error = function(e) message("[LEDGER] log_action failed (etiquetar): ", e$message)
      )
    }

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
      src <- row[["source"]] %||% SAP_B1_ERP_TYPE
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

    # ── Edit: a row's ✎ (the day's row, all its columns) ───────────────────────
    .dia_editar <- function(row) .open_edit_for_row(row)

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
      .save_moves_deferred(updated_moves, client_id = shared$effective_client_id(), session = session,
                           rebase = function(fresh) upsert_moves(fresh, new_move))

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
      tryCatch({
        save_sap_overrides(updated_ov, client_id = shared$effective_client_id())
        bump_sync_version("sap_ov_db", client_id = shared$effective_client_id())
      },
        error = function(e) warning("[sap_edit_save] save_sap_overrides: ", e$message)
      )

      # --- sync staged queue ---
      tryCatch(
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
          username    = shared$current_user(),
          client_id   = shared$effective_client_id()),
        error = function(e) warning("[sap_edit_save] .sync_staged: ", e$message)
      )

      session$userData[[ns("pending_sap_edit_row")]] <- NULL
      ctx <- modal_ctx()
      if (!is.null(ctx)) {
        new_ctx <- tryCatch(.refresh_ctx_detail(ctx), error = function(e) NULL)
        if (!is.null(new_ctx)) modal_ctx(new_ctx) else removeModal()
      } else {
        removeModal()
      }
      showNotification("Cambios guardados.", type = "message", duration = 2)
      log_action(
        user        = tryCatch(shared$current_user(), error = function(e) "system"),
        module      = paste0("ledger_", ledger),
        action      = "editar_factura_sap",
        description = paste0("Factura ", row[["Documento"]], " editada (fecha=",
                             format(new_date, "%d/%m/%Y"), ")"),
        target_id   = row[["Documento"]],
        metadata    = list(empresa = row[["Empresa"]], new_date = as.character(new_date),
                           parte_override = ov_row$Parte_override, codigo_override = ov_row$Codigo_override,
                           factura_override = ov_row$Factura_override, notas_override = ov_row$Notas_override,
                           moneda_override = ov_row$Moneda_override, importe_override = ov_row$Importe_override),
        client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
        viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
      )
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
      sel_date <- ctx$sel_date
      cur      <- ctx$cur
      amt      <- ctx$amt
      lbl_type <- if (ledger == "AR") "Cobros — CxC" else "Pagos — CxP"
      amt_pill_class <- if (grepl("vencido", tolower(amt)))
        "modal-day-pill modal-day-pill--saldo" else "modal-day-pill modal-day-pill--abono"

      # Resumen or Detalle, the one last chosen in this session; switching
      # happens inside the window (dia_ui()).
      vista <- if (isTRUE(ctx$audit)) "detalle" else "resumen"
      # One header line (T09): the title, the day's pills (the ledger isn't
      # repeated: it is the title), the view switch and the Agenda total --
      # no separate rows, no footer that only says "Cerrar".
      context_header <- tagList(
        div(class = "modal-day-header",
          tags$span(class = "modal-day-pill modal-day-pill--date",
                    format(sel_date, "%d %b %Y")),
          tags$span(class = "modal-day-pill modal-day-pill--cur", cur),
          tags$span(class = amt_pill_class, amt)
        ),
        dia_cambio_ui(ns, vista),
        uiOutput(ns("dia_agenda"), inline = TRUE)
      )
      showModal(hd_modal(
        titulo = lbl_type, encabezado = context_header,
        div(class = "hd-dia", dia_ui(ns, vista, cambio = FALSE))
      ))
    }

    # Below the tables: what this day's companies already have in the Agenda,
    # in this currency (the day's own line, P10).
    output$dia_agenda <- renderUI({
      ctx <- modal_ctx()
      req(ctx)
      ph_now <- shared$pagar_hoy_db()
      emps   <- unique(ctx$detail[["Empresa"]])
      total  <- if (!is.null(ph_now) && nrow(ph_now)) {
        ph_cur <- ph_now[ph_now[["ledger"]] == ledger & ph_now[["status"]] == "pending" &
                         toupper(trimws(ph_now[["Moneda"]])) == ctx$cur & ph_now[["Empresa"]] %in% emps, , drop = FALSE]
        sum(ph_cur[["Importe"]], na.rm = TRUE)
      } else 0
      if (total > 0)
        tags$span(class = "cart-total-staged modal-day-pill",
            tags$span(tr(if (ledger == "AR") "tablas.dia.en_agenda_cobros" else "tablas.dia.en_agenda_pagos")),
            tags$strong(fmt_money(total)))
    })

    # The view switch remembers its choice for the next day opened.
    observeEvent(input$dia_vista, {
      shared$audit_mode(identical(input$dia_vista, "detalle"))
    }, ignoreInit = TRUE)

    # ── The day's tables (R/dia_module.R) ──────────────────────────────────────
    dia_server(input, output, session, modal_ctx, ledger, shared, manejadores = list(
      agregar = .dia_agregar, quitar = .dia_quitar, etiquetar = .dia_etiquetar,
      mover = .dia_mover, restaurar = .dia_restaurar, eliminar = .dia_eliminar,
      editar = .dia_editar, crudos = .dia_crudos))

    list(df_combined = df_combined)
  }) # end moduleServer
}   # end ledgerModuleServer


# ── Core move / restore persistence ───────────────────────────────────────────

# Defers the S3 write + sync-version bump to the next event-loop tick.
# shared$moves_db(updated) MUST be called first — that fires the reactive so
# Shiny can flush (calendar re-render) before the I/O blocks anything.
#
# BUG (found 2026-09-01): the S3 write used to be entirely fire-and-forget —
# a failure only reached warning() (the server's own R console/log), which
# nobody watches during a live session. The calendar had already shown the
# move as applied (shared$moves_db(updated) above always fires immediately),
# and the Activity-log write below used to fire unconditionally right after,
# so the Activity log recorded "moved to X" as a fact even when the write behind it
# never landed. The next fresh session (server restart, cold R process) then
# reloaded moves from S3's last ACTUALLY-successful save and every edit made
# in between silently vanished, with no error anyone ever saw — exactly the
# "my moves are gone" reports despite the underlying invoice/snapshot data
# always being intact. Root cause is almost certainly transient (S3
# permission hiccup, clock skew — hopdesk_start.bat's own VPN/clock-resync
# step already documents this class of AWS failure, or a dropped tunnel —
# the ngrok log itself shows a brief "heartbeat timeout / failed to
# reconnect" gap), so this now (1) retries once after a short delay before
# giving up, (2) shows a persistent, explicit on-screen error if it still
# fails so the user knows NOT to trust the screen and not to close the
# session, and (3) only logs "mover_fecha"/"restaurar_fecha" as a genuine
# Activity-log fact once the write is actually confirmed — a failed save
# gets its own clearly-flagged log entry instead of a misleading success one.
# rebase (2026-09-27): the three callers below all build `updated` as
# <this session's in-memory moves_db> + <their own change>, and this function
# then overwrites the WHOLE S3 table with it. Anything another user saved
# between this session's last sync poll and this write was therefore erased --
# no error, no trace, and indistinguishable from "my moves disappeared". The
# 8s sync poll narrows that window but cannot close it: nothing re-checks S3
# at the moment of writing.
#
# `rebase` closes it. It is a function(fresh_table) that re-applies THIS
# caller's change onto the table as it exists in S3 right now, so a concurrent
# writer's rows survive instead of being overwritten by our stale snapshot.
# When it is not supplied, behaviour is exactly what it was before.
# Reads invoice_moves straight from S3 (CM09): strict, past the preload and
# "missing" caches AND past .app_data_cache, which load_moves() answers from
# first and which only this process's own saves keep current -- a write from
# another process, or an offline-queue replay, never reaches it. A failed
# read stop()s, so the save retries and then reports the failure instead of
# writing this session's whole copy.
# Confirmar siempre cierra (Luis, 2026-11-04): the Agenda rows for what the
# day sends (Agregar todo, Agregar selección, a row's or a group's ＋). An ERP
# invoice goes through agenda_filas_factura(), with its posting date taken
# from the ledger (`fuente`, by Empresa + Moneda + Documento) as the close
# key; it refuses, on the server, one already confirmed (attr "rechazadas").
# A manual row keeps the row built by hand: it leaves the views on its own
# path once confirmed. a_mano: the rows .dia_agregar built (with FechaEff).
dia_filas_agenda <- function(a_mano, fuente, usuario, ledger, confirmaciones) {
  FC <- "Fecha de contabilización"
  if (!is.null(fuente) && FC %in% names(fuente)) {
    k_f <- paste(fuente$Empresa, fuente$Moneda, fuente$Documento, sep = "\u001f")
    a_mano[[FC]] <- as.Date(fuente[[FC]])[match(paste(a_mano$Empresa, a_mano$Moneda, a_mano$Documento,
                                                      sep = "\u001f"), k_f)]
  }
  erp <- is_erp_sourced(a_mano[["source"]])
  filas_erp <- agenda_filas_factura(a_mano[erp, , drop = FALSE], usuario, ledger = ledger,
                                    confirmaciones = confirmaciones)
  out <- dplyr::bind_rows(filas_erp, a_mano[!erp, , drop = FALSE])
  if (!"fecha_factura" %in% names(out)) out[["fecha_factura"]] <- as.Date(rep(NA, nrow(out)))
  attr(out, "rechazadas") <- unique(attr(filas_erp, "rechazadas"))
  out
}

.moves_read_live <- function(client_id = NULL) {
  .normalize(.s3_leer_para_escribir(S3_KEYS$moves, client_id), .schema_moves) |>
    dplyr::filter(ledger %in% c("AR", "AP"), !is.na(Empresa), !is.na(Moneda), !is.na(Documento))
}

.save_moves_deferred <- function(updated, client_id = NULL, session = NULL, log_info = NULL,
                                 rebase = NULL) {
  force(client_id); force(session); force(log_info); force(rebase)   # evaluate NOW, before later() captures them

  .log_outcome <- function(failed, err_msg = NULL) {
    if (is.null(log_info)) return(invisible(NULL))
    info <- log_info
    if (failed) {
      info$action      <- paste0(info$action, "_fallo_guardado")
      info$description <- paste0("FALLO AL GUARDAR (no se aplico realmente): ",
                                 info$description, " -- error S3: ", err_msg)
    }
    tryCatch(do.call(log_action, info), error = function(e)
      message("[LEDGER] log_action failed (", info$action, "): ", e$message))
  }

  .attempt <- function(retries_left) {
    tryCatch({
      to_write <- updated
      if (is.function(rebase)) {
        fresh <- .moves_read_live(client_id)   # a failed read goes to the retry below
        if (is.data.frame(fresh)) {
          rebased <- tryCatch(rebase(fresh), error = function(e) NULL)
          if (!is.null(rebased) && is.data.frame(rebased)) {
            if (nrow(rebased) != nrow(updated))
              message(sprintf(
                "[SAVE_MOVES] rebase: S3 cambio mientras esta sesion trabajaba (%d filas en memoria vs %d tras rebasar) -- se conservan los cambios de la otra sesion",
                nrow(updated), nrow(rebased)))
            to_write <- rebased
          }
        }
      }
      save_moves(to_write, client_id = client_id)
      bump_sync_version("moves_db", client_id = client_id)
      .log_outcome(failed = FALSE)
    }, error = function(e) {
      warning("[SAVE_MOVES] S3 write failed: ", e$message)
      if (retries_left > 0) {
        later::later(function() .attempt(retries_left - 1), delay = 2)
      } else {
        if (!is.null(session)) {
          showNotification(
            paste0("No se pudo guardar el movimiento de fecha en el servidor (\"", e$message,
                   "\"). El cambio se ve en pantalla pero NO quedo guardado -- vuelve a ",
                   "intentarlo en unos minutos y NO cierres la sesion mientras tanto."),
            type = "error", duration = NULL, session = session
          )
        }
        .log_outcome(failed = TRUE, err_msg = e$message)
      }
    })
  }
  later::later(function() .attempt(1), delay = 0)
}

.apply_move <- function(keys, new_date, ledger, shared, session = NULL) {
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
  cid <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
  .save_moves_deferred(
    updated, client_id = cid, session = session,
    rebase = function(fresh) upsert_moves(fresh, new_rows),
    log_info = list(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = paste0("ledger_", ledger),
      action      = "mover_fecha",
      description = sprintf("%d factura(s) movida(s) a %s",
                            nrow(keys), format(as.Date(new_date), "%d/%m/%Y")),
      target_id   = paste(keys[["Documento"]][seq_len(min(5, nrow(keys)))], collapse = ", "),
      metadata    = list(n = nrow(keys)),
      client_id             = cid,
      viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
    )
  )
}

.clear_moves <- function(keys, ledger, shared, session = NULL) {
  moves   <- shared$moves_db()
  updated <- dplyr::anti_join(
    moves,
    keys |> dplyr::mutate(ledger = !!ledger),
    by = c("ledger","Empresa","Moneda","Documento")
  )
  shared$moves_db(updated)
  cid <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
  .save_moves_deferred(
    updated, client_id = cid, session = session,
    rebase = function(fresh) dplyr::anti_join(
      fresh, keys |> dplyr::mutate(ledger = !!ledger),
      by = c("ledger","Empresa","Moneda","Documento")
    ),
    log_info = list(
      user        = tryCatch(shared$current_user(), error = function(e) "system"),
      module      = paste0("ledger_", ledger),
      action      = "restaurar_fecha",
      description = sprintf("%d factura(s) restaurada(s) a su fecha original", nrow(keys)),
      target_id   = paste(keys[["Documento"]][seq_len(min(5, nrow(keys)))], collapse = ", "),
      metadata    = list(n = nrow(keys)),
      client_id             = cid,
      viewer_home_client_id = tryCatch(shared$home_client_id(), error = function(e) NULL)
    )
  )
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
                         username    = NULL,
                         client_id   = NULL) {
  # Read fresh from S3 first (safe_load_pagar_hoy -- mode-aware, found
  # 2026-07-25); this edits a row in place and writes the whole table back,
  # so a stale base could silently drop another session's concurrent
  # stage/unstage. Shared by do_move/do_restore/sap_edit_save -- one fix
  # covers all three call sites.
  ph <- tryCatch(safe_load_pagar_hoy(username = username, client_id = client_id),
                 error = function(e) NULL) %||% ph_rv()
  if (is.null(ph) || !nrow(ph)) return(invisible(list(changed = FALSE, ok = TRUE, error = NULL)))
  ph_base <- ph   # CM09: if the read failed this is ph_rv(); the save rebases on S3 (A2)
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
  if (!changed) return(invisible(list(changed = FALSE, ok = TRUE, error = NULL)))
  ph_rv(ph)
  # found 2026-08-05: this used to be a bare save_pagar_hoy() call -- same
  # bug shape as the confirm/quitar/stage handlers, but a different fix is
  # right for THIS site: this whole function is a best-effort "keep an
  # already-staged Agenda row's displayed date/amount in sync with an edit
  # made elsewhere" helper, not the primary action -- every one of its 4 callers
  # (do_move/do_restore/sap_edit_save in this file, app.R's manual-entry
  # edit) already does its OWN primary save + its OWN log_action for the
  # actual user-facing action, entirely separately from this call. A failure
  # here was never "the action didn't happen" -- it only meant the staged
  # Agenda row's displayed values could go stale until the next reload. So
  # instead of adding a new user-facing error + audit-log entry (there was
  # never a SUCCESS log for this cosmetic step either, at any call site --
  # adding one only for the failure path would be new, disproportionate
  # noise), the fix here is to make the underlying write itself more
  # resilient (the same retry as everywhere else) and guarantee this
  # function can never throw uncaught, matching the existing containment
  # 3 of its 4 callers already have (do_move/do_restore wrap their whole
  # handler; sap_edit_save wraps this specific call) -- the 4th
  # (app.R's manual-entry edit-mode save) had NO containment around this
  # call at all, so this is what actually closes that one's real gap: not a
  # bespoke per-site block, but making the shared helper safe by
  # construction so every caller benefits without needing its own copy of
  # the same fix.
  sync_result <- save_pagar_hoy_with_retry(ph, username, client_id = client_id, base = ph_base)
  if (!isTRUE(sync_result$ok)) {
    warning("[.sync_staged] save_pagar_hoy failed after ", sync_result$attempts,
            " attempt(s): ", conditionMessage(sync_result$error))
    return(invisible(list(changed = TRUE, ok = FALSE, error = sync_result$error)))
  }
  invisible(list(changed = TRUE, ok = TRUE, error = NULL))
}
