# =============================================================================
# R/staging_browse_module.R
# Manual entry modal for Cobro (CxC) and Pago (CxP).
# Also contains the Abono Parcial modal (partial payment recording).
#
# Public API:
#   show_combined_entry_modal(ledger, sap_data, session)
#     — call from pick_ar / pick_ap observers in app.R (after removeModal())
#
#   show_abono_modal(sap_data, session, empresa_vals)
#     — call from pick_abono observer in app.R (after removeModal())
#
#   setup_abono_browse(input, output, session, sap_data, abonos_db, pagar_hoy_db, current_user,
#                      client_id = NULL, autorizado = function() FALSE)
#     — call ONCE inside the server function, at startup; its table is
#       hd_table "abono_parcial" (T25). Returns invisible(list(facturas, tabla)).
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
                                      empresa_vals = character(0)) {
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
# Abono Parcial — partial payment recording (hd_table "abono_parcial", T25)
# Rows are staged to Agenda de Hoy (pagar_hoy) as tipo_item="abono"/status="pending".
# Calendar balance deduction only takes effect after explicit "Confirmar pagos"
# in Agenda de Hoy, which writes confirmed rows to abonos_db with status="active".
# The table is the engine's: one row per invoice (id ledger|Empresa|Moneda|
# Documento), checkboxes, groups by Parte + Empresa within each currency,
# search, and the amount typed in its cell (edicion_celda, up to the
# balance; typing ticks the row). What is staged is what it always was.
# =============================================================================

# The invoices the modal offers: the ledger's SAP rows in the window and the
# company / currency filters, by due date, their balance net of CONFIRMED
# abonos (a pending one staged in the Agenda doesn't count yet -- mirrors
# build_ledger_df()'s join in R/data_pipeline.R, found 2026-07-24), and those
# with nothing left to pay dropped. Importe (where the typing starts) is the
# balance. attr(, "motivo"): the dictionary key saying why it came out empty.
abono_facturas <- function(df_src, abonos, ledger, desde, hasta, empresa = "", moneda = "") {
  vacia <- data.frame(id = character(), Parte = character(), Referencia = character(), Documento = character(),
                      Empresa = character(), Moneda = character(), Vence = as.Date(character()),
                      Saldo = numeric(), Importe = numeric(), Codigo = character(), stringsAsFactors = FALSE)
  sale <- function(motivo) { attr(vacia, "motivo") <- motivo; vacia }
  if (is.null(df_src) || !nrow(df_src)) return(sale("tablas.abono.sin_sap"))

  df <- df_src |>
    dplyr::filter(`Fecha de vencimiento` >= as.Date(desde), `Fecha de vencimiento` <= as.Date(hasta))
  if (nzchar(empresa %||% "")) df <- df |> dplyr::filter(Empresa == empresa)
  if (nzchar(moneda %||% ""))  df <- df |> dplyr::filter(Moneda == moneda)
  df <- df |> dplyr::arrange(`Fecha de vencimiento`)
  if (!nrow(df)) return(sale("tablas.abono.sin_rango"))

  ab_summary <- active_abonos_summary(abonos %||% tibble::tibble()) |>
    dplyr::filter(.data$ledger == !!ledger)
  abono_total <- if (nrow(ab_summary))
    dplyr::coalesce(ab_summary$abono_total[match(paste(df$Empresa, df$Moneda, df$Documento),
                                                 paste(ab_summary$Empresa, ab_summary$Moneda, ab_summary$Documento))], 0)
  else rep(0, nrow(df))
  df[["Saldo vencido"]] <- pmax(0, df[["Saldo vencido"]] - abono_total)
  df <- df[df[["Saldo vencido"]] > 0, , drop = FALSE]
  if (!nrow(df)) return(sale("tablas.abono.sin_saldo"))

  col  <- function(n) if (n %in% names(df)) as.character(df[[n]]) else rep("", nrow(df))
  doc  <- col("Documento"); doc[is.na(doc)] <- ""
  ref  <- col("Factura");   ref[is.na(ref) | !nzchar(ref)] <- doc[is.na(ref) | !nzchar(ref)]
  cod  <- col(if (ledger == "AR") "Código de cliente" else "Código de proveedor"); cod[is.na(cod)] <- ""
  id   <- paste(ledger, col("Empresa"), col("Moneda"), doc, sep = "|")
  n    <- stats::ave(seq_along(id), id, FUN = seq_along)
  id[n > 1L] <- paste0(id[n > 1L], "#", n[n > 1L])
  data.frame(id = id, Parte = col("Parte"), Referencia = ref, Documento = doc, Empresa = col("Empresa"),
             Moneda = col("Moneda"), Vence = as.Date(df[["Fecha de vencimiento"]]),
             Saldo = df[["Saldo vencido"]], Importe = df[["Saldo vencido"]], Codigo = cod,
             stringsAsFactors = FALSE)
}

.ab_columnas <- function() list(
  col_texto("Parte", "tablas.abono.col.parte"),
  col_texto("Referencia", "tablas.abono.col.referencia", recorte = FALSE),
  col_texto("Documento", "tablas.abono.col.documento", recorte = FALSE, opcional = TRUE),
  col_empresa("Empresa", "tablas.abono.col.empresa"),
  col_fecha("Vence", "tablas.abono.col.vence"),
  col_dinero("Saldo", "tablas.abono.col.saldo", sumar = FALSE),
  col_dinero("Importe", "tablas.abono.col.importe",
             editable = list(min = 0, max = "Saldo", aviso = "tablas.abono.mayor_saldo")))

# ── Show the abono modal ──────────────────────────────────────────────────────
# The server's filters (they change what is read, T13 P5) above the table;
# "Enviar a Agenda" in the footer. Search, groups and collapsing are the
# engine's.
show_abono_modal <- function(sap_data, session, empresa_vals) {
  today <- Sys.Date()
  campo <- function(etiqueta, control) div(tags$label(etiqueta, class = "form-label mb-0 small text-muted"), control)
  todas <- stats::setNames("", tr("tablas.abono.todas"))
  showModal(hd_modal(
    titulo = tr("tablas.abono.titulo"),
    encabezado = div(class = "d-flex flex-wrap gap-2 align-items-end",
      campo(tr("tablas.abono.tipo"),
            selectInput("ab_ledger", NULL, selected = "AP", width = "115px",
                        choices = stats::setNames(c("AP", "AR"), c(tr("tablas.abono.tipo_ap"), tr("tablas.abono.tipo_ar"))))),
      campo(tr("tablas.abono.desde"),
            dateInput("ab_desde", NULL, value = today - 15, weekstart = 1, language = "es", width = "130px")),
      campo(tr("tablas.abono.hasta"),
            dateInput("ab_hasta", NULL, value = today + 15, weekstart = 1, language = "es", width = "130px")),
      campo(tr("tablas.abono.empresa"),
            selectInput("ab_empresa", NULL, choices = c(todas, sort(unique(empresa_vals))), selected = "", width = "200px")),
      campo(tr("tablas.abono.moneda"),
            selectInput("ab_moneda", NULL, choices = c(todas, CURRENCIES), selected = "", width = "100px"))),
    pie = div(class = "d-flex justify-content-between align-items-center w-100",
      div(tags$span(class = "badge bg-warning text-dark", tr("tablas.abono.insignia")),
          tags$small(class = "text-muted ms-2", tr("tablas.abono.nota"))),
      actionButton("ab_stage", tr("tablas.abono.enviar"), class = "btn btn-primary")),
    hd_table_ui("ab_tabla", alto = "ventana")
  ))
}

# ── Server-side setup — call ONCE inside server() ────────────────────────────
setup_abono_browse <- function(input, output, session,
                                sap_data, abonos_db, pagar_hoy_db, current_user,
                                client_id = NULL, autorizado = function() FALSE) {

  # The filters live in the modal; before it opens (or while it isn't
  # there) they take the modal's defaults.
  facturas <- reactive({
    ledger <- input$ab_ledger %||% "AP"
    sap    <- sap_data()
    abono_facturas(if (ledger == "AR") sap$AR else sap$AP, abonos_db(), ledger,
                   desde   = input$ab_desde %||% (Sys.Date() - 15),
                   hasta   = input$ab_hasta %||% (Sys.Date() + 15),
                   empresa = input$ab_empresa %||% "", moneda = input$ab_moneda %||% "")
  })

  tabla <- hd_table_server("ab_tabla", tabla = "abono_parcial", datos = facturas, columnas = .ab_columnas(),
    piezas = c("casillas", "edicion_celda"),
    secciones = list(list(campo = "Moneda", orden = CURRENCIES)),
    grupo = c("Empresa", "Parte"),
    aviso = reactive({ m <- attr(facturas(), "motivo"); if (is.null(m)) NULL else tr(m) }),
    vacio = "tablas.abono.vacio")

  # Stage abono rows to pagar_hoy ONLY — no immediate abonos_db write.
  # Calendar reduction takes effect only when "Confirmar pagos" in Agenda de Hoy
  # writes confirmed rows to abonos_db with status = "active".
  observeEvent(input$ab_stage, {
   if (!isTRUE(autorizado())) return()
   tryCatch({
    df  <- facturas()
    sel <- tabla$seleccion()
    filas <- df[df$id %in% sel, , drop = FALSE]
    if (!nrow(filas)) {
      showNotification(tr("tablas.abono.sin_seleccion"), type = "warning", duration = 4)
      return()
    }
    # The amount typed in the row's cell; a row not typed in pays its balance.
    ed <- tabla$ediciones()
    importe <- filas$Saldo
    k <- match(filas$id, as.character(ed$id))
    importe[!is.na(k)] <- suppressWarnings(as.numeric(ed$valor[k[!is.na(k)]]))

    ledger <- input$ab_ledger %||% "AP"
    # Read fresh from S3 first (safe_load_pagar_hoy -- mode-aware, found
    # 2026-07-25); a stale base here would silently drop another session's
    # concurrent stage/unstage.
    ph     <- tryCatch(safe_load_pagar_hoy(username = current_user(), client_id = client_id()),
                       error = function(e) NULL) %||% pagar_hoy_db()
    ph_base <- ph   # CM09: the save applies only the accepted rows on S3 (A2)

    # The browser only warns (the cell turns red); the server checks again,
    # against ITS balance at the click (net of confirmed abonos), never the
    # one a browser says (T25 C2: it trusted data-saldo).
    acepta <- !is.na(importe) & importe > 0 & importe <= filas$Saldo + 1e-6
    rejected <- filas$Referencia[!acepta]
    accepted <- filas[acepta, , drop = FALSE]
    importe  <- importe[acepta]

    if (nrow(accepted)) {
      for (i in seq_len(nrow(accepted))) {
        new_row <- tibble::tibble(
          id        = uuid::UUIDgenerate(),
          ledger    = ledger,
          Empresa   = accepted$Empresa[i],
          Moneda    = accepted$Moneda[i],
          Documento = accepted$Documento[i],
          Parte     = accepted$Parte[i],
          Codigo    = trimws(accepted$Codigo[i]),
          tipo_item = "abono",
          Importe   = importe[i],
          # The invoice's real due date, not the staging date (found
          # 2026-07-24: this always showed "today" in Agenda de Hoy).
          FechaVenc = accepted$Vence[i],
          staged_by = current_user(),
          staged_at = Sys.time(),
          status    = "pending",
          source    = "manual"
        )
        ph <- upsert_pagar_hoy(ph, new_row, keys = "id")
      }

      # Rule 17: a save that failed never says "enviados", never logs a
      # success and never closes the modal; the session's copy changes only
      # once S3 has it.
      guardado <- tryCatch({
        save_pagar_hoy(ph, current_user(), client_id = client_id(), base = ph_base)
        TRUE
      }, error = function(e) {
        showNotification(tr("tablas.abono.error_guardar", error = conditionMessage(e)), type = "error", duration = 8)
        FALSE
      })
      if (!guardado) return()
      pagar_hoy_db(ph)

      n <- nrow(accepted)
      showNotification(tr_n("tablas.abono.enviados", n), type = "message", duration = 4)
      tryCatch(
        log_action(
          user        = tryCatch(current_user(), error = function(e) "system"),
          module      = "staging_browse",
          action      = "enviar_abonos_a_agenda",
          description = paste0(n, if (n == 1L) " abono enviado" else " abonos enviados", " a Agenda de hoy"),
          metadata    = list(n = n, ledger = ledger),
          client_id             = tryCatch(client_id(), error = function(e) NULL),
          viewer_home_client_id = NULL
        ),
        error = function(e) NULL
      )
    }

    if (length(rejected))
      showNotification(tr_n("tablas.abono.rechazados", length(rejected), facturas = paste(rejected, collapse = ", ")),
                       type = "warning", duration = 6)

    if (nrow(accepted)) {
      tabla$limpiar_ediciones()
      removeModal()
    }
   }, error = function(e) {
     warning("[AB_STAGE] observer failed: ", conditionMessage(e))
     showNotification(tr("tablas.abono.error"), type = "error", duration = 8)
   })
  }, ignoreInit = TRUE)

  invisible(list(facturas = facturas, tabla = tabla))
}
