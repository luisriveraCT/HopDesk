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
    selected_pair <- reactiveVal(NULL)   # list(ini, empresa, counterpart, ledger)
    ic_view_mode  <- reactiveVal("table")

    # ── Filtered sub-data for selected pair (raw, all columns, date-sorted) ──
    ic_detail_sub <- reactive({
      pair <- selected_pair(); req(pair)
      dat  <- ic_invoices();   req(dat)
      # The open invoices and, locked at $0, the already confirmed ones.
      df   <- if (pair$ledger == "AR") dplyr::bind_rows(dat$ar, dat$ar_pagadas)
              else dplyr::bind_rows(dat$ap, dat$ap_pagadas)
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
      if (is.null(reg)) return(NULL)

      cmap    <- isolate(shared$company_map())
      if (!length(cmap)) return(NULL)
      inv_map <- setNames(names(cmap), unname(cmap))

      # Company pills (toolbar) — full display names of currently toggled-on
      # companies. Empty/unset means "no filter" (same convention as
      # ledger_module.R's df_combined()), so a not-yet-initialized pill state
      # never hides everything.
      emp_sel <- isolate(shared$empresa_sel()) %||% character()

      # Confirmed-invoice sources — items that already went through
      # "Confirmar" in Agenda de Hoy, or that were soft-deleted as a SAP
      # ghost via the calendar/search trash. Treasury has settled these
      # internally even if Finance/SAP hasn't caught up yet, so they must
      # disappear from Intercompany entirely (see .filter_ic() below, which
      # now calls the canonical compute_confirmed_flags() -- Stage 13 --
      # instead of its own standalone 5th independent implementation).
      conf_db      <- isolate(shared$bancos_confirmados()) %||% tibble::tibble()
      papelera_raw <- isolate(shared$papelera_rv())         %||% tibble::tibble()
      abonos_raw   <- isolate(shared$abonos_db())           %||% tibble::tibble()

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

      # Use sap_data() from shared — it is updated by the context-switch observer
      # when a staff member jumps to a client, so it always reflects the active context.
      # load_sap_snapshot() reads from the native CLIENT_ID prefix and would return
      # hd-admin's empty snapshot even when jumped into a client's context.
      snap_ar <- isolate(shared$sap_data()$AR)
      snap_ap <- isolate(shared$sap_data()$AP)

      # pagadas = TRUE: the ERP invoices already confirmed (or trashed as a SAP
      # ghost) instead of the open ones, at $0 and flagged .pagada. Luis
      # (2026-11-04): what comes from the ERP is never removed from a view;
      # confirmed, it stays locked at $0. The invoice lists show them; the
      # totals, the matrix and the Mapa's graph count only the open ones.
      .filter_ic <- function(df, ledger, pagadas = FALSE) {
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
        code_col <- if (ledger == "AR") "C\u00f3digo de cliente" else "C\u00f3digo de proveedor"
        if (!"Empresa" %in% names(df) || !code_col %in% names(df)) return(NULL)

        slot    <- if (ledger == "AR") "ar" else "ap"
        df$.ini <- inv_map[df$Empresa]

        has_any_codes <- length(ic_codes) > 0 && any(vapply(names(cmap), function(ini) {
          length(ic_codes[[ini]][[slot]] %||% character()) > 0
        }, logical(1)))

        if (has_any_codes) {
          keep <- mapply(function(ini, code) {
            if (is.na(ini) || !nzchar(ini %||% "")) return(FALSE)
            toupper(trimws(code %||% "")) %in% (ic_codes[[ini]][[slot]] %||% character())
          }, df$.ini, df[[code_col]])
        } else if ("Parte" %in% names(df)) {
          # Fallback: name-based detection when no CardCodes are configured.
          # Matches Parte column against COMPANY_MAP display names.
          company_names_up <- toupper(unname(cmap))
          keep             <- toupper(trimws(df[["Parte"]])) %in% company_names_up
          keep[is.na(keep)] <- FALSE
          message("[IC_MODULE] name-fallback ledger=", ledger,
                  " rows_in=", nrow(df), " rows_ic=", sum(keep))
        } else {
          keep <- rep(FALSE, nrow(df))
        }

        # Restrict to companies currently toggled on via the toolbar pills —
        # filters on the owning company (Empresa), mirroring the Empresa %in%
        # emp pattern used by ledger_module.R / cashflow / search modules.
        if (length(emp_sel)) keep <- keep & (df$Empresa %in% emp_sel)

        amt_col <- if ("Saldo vencido" %in% names(df)) "Saldo vencido" else "DocTotal"

        # Captured BEFORE abono-netting below -- compute_confirmed_flags()'s
        # amount-match guard (Stage 10) matches against the pre-netting
        # amount, same as every other caller. Matching the netted balance
        # instead (as this module's own retired .ckey() implementation used
        # to) would cause exactly the false-negative Mouse flagged: an abono
        # applied after confirmation would silently reopen a still-valid one.
        if (amt_col %in% names(df)) df[["Saldo_original"]] <- df[[amt_col]]

        # ── Net out active abonos (partial payments staged via Agenda de Hoy) ──
        # Same join convention as build_ledger_df() in data_pipeline.R: raw
        # (Empresa, Moneda, Documento) equality, no case/whitespace
        # normalization — abono rows are always written with the exact same
        # casing as the invoice they were staged from. A fully-netted invoice
        # (balance rounds to 0) is intentionally NOT removed here — it stays
        # visible at $0 until it's actually run through "Confirmar".
        if (amt_col %in% names(df)) {
          ab_sum <- active_abonos_summary(abonos_raw) |>
            dplyr::filter(.data$ledger == !!ledger)
          if (nrow(ab_sum) > 0) {
            ab_key    <- paste(ab_sum$Empresa, ab_sum$Moneda, ab_sum$Documento)
            ab_lookup <- setNames(ab_sum$abono_total, ab_key)
            df_key    <- paste(df$Empresa, df$Moneda, df$Documento)
            abono_total <- dplyr::coalesce(unname(ab_lookup[df_key]), 0)
            df[[amt_col]] <- pmax(0, df[[amt_col]] - abono_total)
          }
        }

        # ── Exclude invoices already confirmed via "Confirmar" in Agenda de
        # Hoy, or soft-deleted as a SAP ghost via the calendar/search trash
        # (Stage 13) ─────────────────────────────────────────────────────
        # Treasury already settled these internally even if Finance/SAP
        # hasn't applied the payment yet. Now uses the canonical
        # compute_confirmed_flags() (R/data_pipeline.R) instead of this
        # module's own standalone 5th independent implementation -- inherits
        # the amount-match guard automatically (Stage 10) and picks up
        # papelera SAP ghosts, a source this module was previously missing
        # entirely. The dead pagar_hoy_db.status=="confirmed" source (no
        # code path can produce it since Stage 4) is retired along with it.
        df <- compute_confirmed_flags(df, ledger, conf_db, papelera_raw)
        pagada <- df[["confirmed"]] %in% TRUE
        keep <- keep & (if (pagadas) pagada else !pagada)

        out <- df[keep, , drop = FALSE]
        if (nrow(out) == 0) return(NULL)
        if (pagadas && amt_col %in% names(out)) out[[amt_col]] <- 0
        out$.pagada  <- pagadas
        out$CardCode <- out[[code_col]]
        out$Ledger   <- ledger
        out
      }

      list(
        ar = .filter_ic(snap_ar, "AR"),
        ap = .filter_ic(snap_ap, "AP"),
        ar_pagadas = .filter_ic(snap_ar, "AR", pagadas = TRUE),
        ap_pagadas = .filter_ic(snap_ap, "AP", pagadas = TRUE)
      )
    }

    # ── Auto-load: re-runs whenever registry, companies, SAP data, the
    # toolbar's company pills, or confirmed/abono state change — so context
    # switches, live SAP refreshes, pill toggles, "Confirmar", "Deshacer", and
    # staging/voiding an abono all update the IC view automatically without a
    # flag. IMPORTANT: do NOT use req() here. req() silently aborts without
    # clearing ic_invoices, so jumping to a client with no SAP data leaves
    # stale IC rows from the previous context visible. Explicit clearing is
    # required.
    observe({
      reg      <- shared$interco_v2()
      cmap     <- shared$company_map()
      sd       <- shared$sap_data()
      emp_sel  <- shared$empresa_sel()          # dependency: pill toggles
      conf_dep <- shared$bancos_confirmados()   # dependency: Confirmar / Deshacer
      ph_dep   <- shared$pagar_hoy_db()         # dependency: Confirmar / Deshacer
      ab_dep   <- shared$abonos_db()            # dependency: abonos staged/voided
      # Found 2026-07-30: .load_ic_data() below reads papelera_rv too (see its
      # own isolate() call), but this outer observe() never listed it as a
      # dependency -- a papelera-only change (e.g. restoring a deleted item)
      # never triggered a re-derive here, unlike every other input that feeds
      # .load_ic_data(). Same fix shape as the dependencies just above.
      pap_dep  <- shared$papelera_rv()
      if (is.null(reg) || length(cmap) == 0 || (is.null(sd$AR) && is.null(sd$AP))) {
        ic_invoices(NULL)
        return()
      }
      ic_invoices(.load_ic_data())
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
    # The page's frame only changes when there is or isn't IC data: the cards
    # repaint with every refresh in their own output, and the matrix's
    # container stays, so the table keeps its search, sort and scroll.
    hay_ic <- reactiveVal(FALSE)
    observe({
      dat <- ic_invoices()
      hay_ic(!(is.null(dat) || (is.null(dat$ar) && is.null(dat$ap))))
    })

    output$ic_body <- renderUI({
      if (!isTRUE(hay_ic())) {
        return(div(
          class = "text-center text-muted py-5",
          tags$p(icon("circle-info")),
          tags$p("No hay facturas IC en los snapshots SAP."),
          tags$p(class = "small",
                 "Configura los c\u00f3digos IC en Configuraci\u00f3n \u203a Intercompany y recarga los datos SAP.")
        ))
      }
      tagList(
        uiOutput(ns("ic_tarjetas")),

        # ── Matrix ───────────────────────────────────────────────────────
        tags$h6(class = "fw-semibold mb-1",
                tagList(icon("table-cells"), " Saldos por empresa y contraparte")),
        tags$p(class = "text-muted small mb-2", tr("tablas.interco.saldos_ayuda")),
        hd_table_ui(ns("saldos_t")),

        # ── Drill-down ────────────────────────────────────────────────────
        uiOutput(ns("ic_detail"))
      )
    })

    output$ic_tarjetas <- renderUI({
      dat <- ic_invoices()
      req(!is.null(dat))
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

      fluidRow(
        column(5,
          div(class = "border rounded p-3 mb-3 bg-primary-subtle",
            tags$p(class = "small fw-semibold mb-1 text-primary-emphasis",
                   tagList(icon("arrow-circle-down"), " CxC Intercompany (cobros)")),
            tags$p(class = "fw-bold mb-0 fs-6", .fmt_totals(dat$ar)),
            tags$small(class = "text-muted",
                       sprintf("%d factura%s", n_ar, if (n_ar == 1) "" else "s"))
          )
        ),
        column(5,
          div(class = "border rounded p-3 mb-3 bg-danger-subtle",
            tags$p(class = "small fw-semibold mb-1 text-danger-emphasis",
                   tagList(icon("arrow-circle-up"), " CxP Intercompany (pagos)")),
            tags$p(class = "fw-bold mb-0 fs-6", .fmt_totals(dat$ap)),
            tags$small(class = "text-muted",
                       sprintf("%d factura%s", n_ap, if (n_ap == 1) "" else "s"))
          )
        )
      )
    })

    # ── Matrix (hd_table, grouped summary) ────────────────────────────────
    # One row per company + counterpart + currency + ledger, keyed by those:
    # ℹ or a double click opens that pair's invoices below. A click used to
    # open it (the registry says to drop that), read by its position in the
    # re-sorted matrix.
    .saldos_tabla <- reactive({
      mat <- tryCatch(ic_matrix_df(), error = function(e) NULL)
      # No IC invoice at all (a client without an IC registry) gives
      # bind_rows(NULL, NULL): a tibble with no columns, not NULL. Sorting it
      # threw inside the table's observer and closed the session (T15).
      if (is.null(mat) || !nrow(mat)) mat <- tibble::tibble(.ini = character(), Empresa = character(), Parte = character(),
                                              Moneda = character(), Ledger = character(),
                                              Total = numeric(), Facturas = integer())
      mat <- mat |> dplyr::arrange(.ini, Ledger, dplyr::desc(Total))
      data.frame(
        id        = make.unique(paste(mat$.ini, mat$Ledger, mat$Parte, mat$Moneda, sep = "|"), sep = "#"),
        ini       = as.character(mat$.ini),
        empresa   = as.character(mat$Empresa),
        Empresa   = sprintf("%s (%s)", mat$Empresa, mat$.ini),
        Ledger    = as.character(mat$Ledger),
        Parte     = as.character(mat$Parte),
        Moneda    = as.character(mat$Moneda),
        Total     = as.numeric(mat$Total),
        Facturas  = as.integer(mat$Facturas),
        .acciones = rep("detalle", nrow(mat)),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    })
    saldos_t <- hd_table_server(
      "saldos_t", tabla = "ic_saldos", datos = .saldos_tabla,
      columnas = list(
        col_texto("Empresa",     "tablas.interco.empresa", recorte = FALSE),
        col_insignia("Ledger",   "tablas.interco.tipo", mapa = list(
          AR = list(texto = "tablas.interco.cobro", tono = "info"),
          AP = list(texto = "tablas.interco.pago",  tono = "peligro"))),
        col_texto("Parte",       "tablas.interco.contraparte"),
        col_texto("Moneda",      "tablas.interco.moneda", recorte = FALSE),
        col_dinero("Total",      "tablas.interco.total", moneda = "Moneda", sumar = FALSE),
        col_numero("Facturas",   "tablas.interco.facturas")
      ),
      acciones_fila = "detalle",
      vacio         = "tablas.interco.saldos_vacio",
      # Each row is a pair (empresa, libro, contraparte, moneda), not an invoice.
      textos        = list(contador = "tablas.interco.contador_pares")
    )

    observeEvent(saldos_t$accion(), {
      a <- saldos_t$accion()
      if (!identical(a$accion, "detalle") || !length(a$ids)) return()
      d   <- .saldos_tabla()
      row <- d[match(a$ids[1], d$id), , drop = FALSE]
      if (!nrow(row) || is.na(row$id)) return()
      selected_pair(list(
        ini         = row$ini,
        empresa     = row$empresa,
        counterpart = row$Parte,
        ledger      = row$Ledger
      ))
    })

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
        hd_table_ui(ns("detalle_t"))
      )
    })

    observeEvent(input$ic_close_detail, selected_pair(NULL), ignoreInit = TRUE)

    # ── Helper: send rows to pagar_hoy_db ─────────────────────────────────────
    .ic_send_rows <- function(rows) {
      # Sending to the Agenda needs can_register_payments on every path
      # (Luis, 2026-11-03; tests/test_agenda_permiso.R). It says why if not.
      if (!puede_o_avisa(shared, "can_register_payments")) return(NA_integer_)
      pair    <- isolate(selected_pair()); req(pair)
      req(!is.null(rows), nrow(rows) > 0)

      # Read fresh from S3 first (safe_load_pagar_hoy -- mode-aware, found
      # 2026-07-25); a stale base here would silently drop another session's
      # concurrent stage/unstage.
      ph <- tryCatch(safe_load_pagar_hoy(username = shared$current_user(),
                                         client_id = shared$effective_client_id()),
                    error = function(e) NULL) %||%
            tryCatch(shared$pagar_hoy_db(), error = function(e) NULL) %||%
            tibble::tibble(id = character(), ledger = character(), Empresa = character(),
                           Moneda = character(), Documento = character(), Parte = character(),
                           Importe = numeric(), FechaVenc = as.Date(character()),
                           staged_by = character(), staged_at = as.POSIXct(character()),
                           status = character())

      user_id <- tryCatch(as.character(shared$current_user()), error = function(e) "ic_table")

      # The Agenda row comes from agenda_filas_factura() (Luis, 2026-11-04:
      # confirming ALWAYS closes the invoice): it carries the invoice's own
      # identity, posting date included, which the confirmation copies, and
      # refuses on the server what is already confirmed. IC rows come from
      # the raw SAP snapshot, so the code is under its SAP column name.
      f <- as.data.frame(rows, stringsAsFactors = FALSE)
      cod <- if ("Codigo" %in% names(f)) f[["Codigo"]]
             else if (identical(pair$ledger, "AR")) f[["Código de cliente"]]
             else f[["Código de proveedor"]]
      f$Codigo <- trimws(as.character(cod %||% rep("", nrow(f))))
      new_rows <- agenda_filas_factura(f, user_id, ledger = pair$ledger,
                                       confirmaciones = tryCatch(isolate(shared$bancos_confirmados()),
                                                                 error = function(e) NULL))
      rech <- attr(new_rows, "rechazadas")
      if (length(rech))
        showNotification(tr("tablas.interco.ya_confirmadas", n = length(rech), docs = paste(rech, collapse = ", ")),
                         type = "warning", duration = 8)
      if (!nrow(new_rows)) return(NA_integer_)   # nothing left to send; it said why

      combined <- upsert_pagar_hoy(ph, new_rows)
      # A failed save is never silent (it was tryCatch(..., NULL), and the
      # user read "añadida"): same retry and wording as the Calendario's day
      # (.dia_agregar). The session shows in the Agenda only what was saved.
      res <- save_pagar_hoy_with_retry(combined, shared$current_user(),
                                       client_id = shared$effective_client_id(), base = ph)
      if (!isTRUE(res$ok)) {
        showNotification(tr("tablas.agenda.no_enviado", n = res$attempts),
                         type = "error", duration = 15)
        tryCatch(log_action(
          user        = tryCatch(shared$current_user(), error = function(e) "system"),
          module      = "interco",
          action      = "enviar_a_agenda_ic_incompleto",
          description = sprintf("Intento de enviar %d factura(s) intercompany a Agenda no se pudo guardar tras %d intento(s): %s",
                                nrow(new_rows), res$attempts,
                                if (!is.null(res$error)) conditionMessage(res$error) else "error desconocido"),
          target_id   = paste(new_rows$Documento[seq_len(min(5, nrow(new_rows)))], collapse = ", "),
          metadata    = list(n = nrow(new_rows), par = paste(pair$ini, pair$counterpart, pair$ledger)),
          client_id             = tryCatch(shared$effective_client_id(), error = function(e) NULL),
          viewer_home_client_id = tryCatch(shared$home_client_id(),      error = function(e) NULL)
        ), error = function(e) NULL)
        return(NA_integer_)
      }
      shared$pagar_hoy_db(res$data %||% combined)
      nrow(new_rows)
    }

    # ── Pair detail (hd_table, line items) ───────────────────────────────────
    # Rows keyed by facturas_ids() (ledger + company + currency + document +
    # source, as Vencidos): the row's ＋ and the bar's "Agregar a Agenda"
    # send THOSE invoices. Both used to be positions -- the ＋ a row number
    # and the batch DT's rows_selected -- read against ic_detail_sub()
    # recalculated at click time, so when another session confirmed one of
    # the pair's invoices (or an abono or a pill changed the list) the ＋ of
    # the row on screen sent its neighbour.
    # Días: how overdue, as a badge by the same cuts the colour had (≤ 0,
    # 1–30, more than 30), and as a number beside it to sort by.
    .detalle_tabla <- reactive({
      sub <- tryCatch(ic_detail_sub(), error = function(e) NULL)
      if (is.null(sub)) sub <- tibble::tibble(Ledger = character(), Empresa = character(), Moneda = character(),
                                              Documento = character())
      amt_col <- if ("Saldo vencido" %in% names(sub)) "Saldo vencido" else "DocTotal"
      due_col <- "Fecha de vencimiento"
      doc_col <- "Fecha de contabilizaci\u00f3n"
      col <- function(n, def = NA) if (n %in% names(sub)) sub[[n]] else rep(def, nrow(sub))
      dias  <- as.integer(Sys.Date() - as.Date(col(due_col)))
      tramo <- ifelse(is.na(dias), "", ifelse(dias <= 0, "al_dia", ifelse(dias <= 30, "d1_30", "mas_30")))
      # Confirmed ERP invoices: locked at $0 (not selectable, no ＋).
      pagada <- col(".pagada", FALSE) %in% TRUE
      tramo[pagada] <- "pagada"
      data.frame(
        id          = facturas_ids(sub),
        Documento   = as.character(col("Documento", "")),
        Factura     = as.character(col("Factura", "")),
        Contab      = as.Date(col(doc_col)),
        Vence       = as.Date(col(due_col)),
        Saldo       = as.numeric(col(amt_col, 0)),
        Moneda      = as.character(col("Moneda", "")),
        Tramo       = tramo,
        Dias        = dias,
        .bloqueada  = pagada,
        .acciones   = ifelse(pagada, "", "agregar"),
        stringsAsFactors = FALSE
      )
    })
    detalle_t <- hd_table_server(
      "detalle_t", tabla = "ic_detalle", datos = .detalle_tabla,
      columnas = list(
        col_texto("Documento",  "tablas.interco.documento", recorte = FALSE),
        col_texto("Factura",    "tablas.interco.referencia", recorte = FALSE),
        col_fecha("Contab",     "tablas.interco.contabilizacion"),
        col_fecha("Vence",      "tablas.interco.vencimiento"),
        col_dinero("Saldo",     "tablas.interco.saldo", moneda = "Moneda"),
        col_texto("Moneda",     "tablas.interco.moneda", recorte = FALSE),
        col_insignia("Tramo",   "tablas.interco.tramo", mapa = list(
          al_dia = list(texto = "tablas.interco.al_dia", tono = "neutro"),
          d1_30  = list(texto = "tablas.interco.d1_30",  tono = "amarillo"),
          mas_30 = list(texto = "tablas.interco.mas_30", tono = "rojo"),
          pagada = list(texto = "tablas.interco.pagada", tono = "exito"))),
        col_numero("Dias",      "tablas.interco.dias", opcional = TRUE, visible = TRUE)
      ),
      acciones_fila  = "agregar",
      acciones_barra = list(list(accion = "agregar_seleccion", texto = "tablas.accion.agregar_seleccion",
                                 confirmar = TRUE, confirmar_texto = "tablas.facturas.confirmar_agregar_sel")),
      vacio          = "tablas.interco.detalle_vacio"
    )

    observeEvent(detalle_t$accion(), {
      a <- detalle_t$accion()
      if (!a$accion %in% c("agregar", "agregar_seleccion") || !length(a$ids)) return()
      sub  <- tryCatch(ic_detail_sub(), error = function(e) NULL)
      if (is.null(sub)) return()
      rows <- sub[match(a$ids, facturas_ids(sub)), , drop = FALSE]
      rows <- rows[!is.na(rows[["Documento"]] %||% NA), , drop = FALSE]
      if (!nrow(rows)) return()
      added <- .ic_send_rows(rows)
      if (is.na(added)) return()   # it said so already
      showNotification(if (identical(a$accion, "agregar"))
                         tr("tablas.interco.anadida_una", n = added)
                       else tr("tablas.interco.anadidas", n = added),
                       type = "message", duration = 2)
    }, ignoreInit = TRUE)

    # ── Treasury map sub-module ────────────────────────────────────────────
    treasuryMapServer("icmap", shared, reactive(ic_invoices()))

  })
}
