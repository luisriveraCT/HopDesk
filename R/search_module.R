# =============================================================================
# R/search_module.R
# The Calendario's invoice lupa (lupaServer(), opened from app.R by
# input$btn_search), the raw SAP viewer, and handle_invoice_action(), which
# runs the edit and stage actions of both the lupa and Vencidos.
#
# Deliberately NOT converted to a live-refreshing view (2026-07-30, part of
# the app-wide real-time-refresh effort): its data is taken once from
# df_combined_AR()/AP() -- now a picture refilled on open, on another month
# and after each action (see lupaServer() below). Those
# same reactives are shared with nearly every other dataset in the app
# (moves, deletes, tags, confirmations, abonos, SAP refreshes all invalidate
# them), so making this modal live would mean it recomputes and rebuilds its
# entire table on every one of those changes for as long as it stays open --
# a real, constant cost, not a one-time dependency add like the other views
# in this effort. A search result is also inherently more point-in-time than
# a shared calendar/ledger view -- the value of it updating live is lower
# while the cost is higher. Revisit if/when the app's render-cost story
# changes (client-side caching, or a non-Shiny frontend), not before.
# =============================================================================

# ── The lupa on the table engine ─────────────────────────────────────────────
# Table "facturas_lupa" of the registry: the same table as Vencidos (the
# facturas_*() functions in vencidos_module.R), over one month. It keeps the
# reason above: its data is a picture taken when it opens, when another
# month is chosen and after each action -- never df_combined_*() live.

# The month's invoices ("AAAA-MM"), AR and AP, as the old lupa took them:
# df_combined_*() (papelera, empresa and confirmed marks already applied),
# the intercompany filter by code, the amount column the app shows.
lupa_facturas <- function(shared, mes) {
  desde <- as.Date(paste0(mes, "-01"))
  hasta <- as.Date(lubridate::ceiling_date(desde, "month") - lubridate::days(1))
  amt   <- tryCatch(shared$amount_col(), error = function(e) "Importe")
  una <- function(ledger_name) {
    df <- if (ledger_name == "AR") shared$df_combined_AR() else shared$df_combined_AP()
    if (is.null(df) || !nrow(df)) return(NULL)
    df <- apply_ic_filter(df, mode = tryCatch(shared$ic_mode[[ledger_name]](), error = function(e) "exclude"),
                          code_col = if (ledger_name == "AR") "Código de cliente" else "Código de proveedor",
                          ic_codes = tryCatch(build_ic_fullcodes(shared$interco_v2(), ledger_name), error = function(e) character()),
                          ic_rfcs  = tryCatch({
                            r <- unique(toupper(trimws(unname(shared$interco_v2()$rfcs %||% character()))))
                            r[nzchar(r)]
                          }, error = function(e) character()),
                          cmap = shared$company_map())
    if (is.null(df) || !nrow(df)) return(NULL)
    df <- df[!is.na(df$FechaEff) & as.Date(df$FechaEff) >= desde & as.Date(df$FechaEff) <= hasta, , drop = FALSE]
    if (!nrow(df)) return(NULL)
    imp <- if (amt %in% names(df)) df[[amt]] else if ("Importe" %in% names(df)) df[["Importe"]] else 0
    df$Importe <- abs(tidyr::replace_na(as.numeric(imp), 0))
    df$Ledger  <- ledger_name
    df$Fecha   <- as.Date(df$FechaEff)
    df$FechaOriginal <- if ("FechaVenc_Original" %in% names(df)) as.Date(df$FechaVenc_Original) else as.Date(NA)
    df
  }
  facturas_llenar_empresa(dplyr::bind_rows(una("AR"), una("AP")), shared)
}

# "Ver datos SAP": every raw SAP row due in the month, of the companies
# chosen -- not just the marked ones (Luis).
lupa_crudos <- function(shared, mes) {
  raw_list <- tryCatch(shared$sap_data(), error = function(e) NULL)
  if (is.null(raw_list)) return(NULL)
  desde <- as.Date(paste0(mes, "-01"))
  hasta <- as.Date(lubridate::ceiling_date(desde, "month") - lubridate::days(1))
  emp <- tryCatch(shared$empresa_sel(), error = function(e) character(0))
  dplyr::bind_rows(lapply(c("AR", "AP"), function(ldg) {
    r <- raw_list[[ldg]]
    if (is.null(r) || !nrow(r)) return(NULL)
    date_col <- if ("Fecha de vencimiento" %in% names(r)) "Fecha de vencimiento"
                else if ("FechaEff" %in% names(r)) "FechaEff" else NULL
    if (!is.null(date_col))
      r <- r[!is.na(r[[date_col]]) & as.Date(r[[date_col]]) >= desde & as.Date(r[[date_col]]) <= hasta, , drop = FALSE]
    if (length(emp) && "Empresa" %in% names(r)) r <- r[r[["Empresa"]] %in% emp, , drop = FALSE]
    if (nrow(r)) { r[["Ledger"]] <- ldg; r } else NULL
  }))
}

# abrir: the lupa button; mes: the Calendario's month, where the lupa opens
# each time. accion_factura: handle_invoice_action() in the app, a recorder
# in the tests (no S3).
lupaServer <- function(id, shared, abrir, mes, accion_factura = handle_invoice_action) {
  moduleServer(id, function(input, output, session) {
    datos   <- reactiveVal(NULL)   # the month's picture
    cargado <- reactiveVal(NULL)   # which month it is ("AAAA-MM")
    cargar <- function(m) {
      df <- tryCatch(isolate(lupa_facturas(shared, m)), error = function(e) {
        message("[LUPA] ", m, ": ", conditionMessage(e))
        showNotification(tr("tablas.lupa.error_carga", error = conditionMessage(e)), type = "error")
        NULL
      })
      datos(df)
      cargado(m)
    }

    t <- facturas_tabla_server(
      "tabla", tabla = "facturas_lupa", datos = datos, shared = shared,
      crudos  = function(ids, df) mostrar_datos_sap(lupa_crudos(shared, isolate(cargado()))),
      accion_factura = accion_factura,
      vacio   = list(texto = "tablas.lupa.vacio"),
      textos  = list(contador = "tablas.contador.facturas", sin_bloqueadas = TRUE),
      periodo = list(campo = "Fecha"),
      despues = function() cargar(isolate(cargado())),
      visibles = "Documento")

    # "Facturas — octubre 2026": the month shown, the first thing people read.
    output$titulo <- renderText({
      m <- cargado()
      if (is.null(m)) return(tr("tablas.lupa.titulo"))
      tr("tablas.lupa.titulo_mes", mes = tr(paste0("tablas.periodo.mes_largo.", as.integer(substr(m, 6, 7)))),
         anio = substr(m, 1, 4))
    })

    observeEvent(abrir(), {
      m <- tryCatch(format(as.Date(isolate(mes()) %||% Sys.Date()), "%Y-%m"), error = function(e) NA_character_)
      if (is.na(m)) m <- format(Sys.Date(), "%Y-%m")
      cargar(m)
      t$ir_mes(m)
      showModal(modalDialog(
        title = textOutput(session$ns("titulo"), inline = TRUE), size = "xl", easyClose = TRUE,
        footer = modalButton(tr("comun.cerrar")),
        tags$style(".modal-xl:has(.hd-lupa) { max-width: 1400px; }"),
        div(class = "hd-lupa", facturas_tabla_ui(session$ns("tabla"), alto = "calc(100vh - 230px)"))
      ))
    }, ignoreInit = TRUE)

    # Another month in the bar: load that one (B5: every month has its data).
    observeEvent(t$periodo(), {
      m <- t$periodo()
      if (is.null(isolate(cargado())) || identical(m, isolate(cargado()))) return()
      cargar(m)
    }, ignoreInit = TRUE)
  })
}

# =============================================================================
# mostrar_datos_sap — the raw SAP viewer ("Ver datos SAP" in the lupa and in
# Vencidos): every raw SAP column of the rows given,
# Ledger / Empresa / Parte / Documento / Moneda first, filterable, with CSV and
# Excel download.
# =============================================================================
mostrar_datos_sap <- function(raw) {
  if (is.null(raw) || !nrow(raw)) {
    showNotification(tr("tablas.datos_sap.vacio"), type = "warning")
    return(invisible(NULL))
  }
  disp <- as.data.frame(raw)
  priority_cols <- intersect(c("Ledger","Empresa","Parte","Documento","Moneda"), names(disp))
  rest_cols     <- setdiff(names(disp), priority_cols)
  disp          <- disp[, c(priority_cols, rest_cols), drop = FALSE]
  showModal(modalDialog(
    title     = tr("tablas.datos_sap.titulo", n = nrow(disp)),
    size      = "xl",
    easyClose = TRUE,
    footer    = tagList(
      tags$small(class = "text-muted me-auto",
                 tr("tablas.datos_sap.tamano", filas = nrow(disp), columnas = ncol(disp))),
      modalButton(tr("comun.cerrar"))
    ),
    div(style = "max-height:65vh; overflow:auto;",
      DT::datatable(
        disp,
        rownames   = FALSE,
        filter     = "top",
        extensions = "Buttons",
        options    = list(
          pageLength = 25,
          scrollX    = TRUE,
          dom        = "Bfrtip",
          buttons    = list(
            list(extend = "csv",   text = tr("tablas.datos_sap.csv")),
            list(extend = "excel", text = tr("tablas.datos_sap.excel"))
          )
        )
      )
    )
  ))
}

# =============================================================================
# handle_invoice_action — shared handler called by search and vencidos modules.
# payload: list with $action, $rows, optionally $move_to
# =============================================================================

handle_invoice_action <- function(payload, shared, session = NULL) {
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

  # Mirrors R/ledger_module.R's audit-logging for the same actions
  # (eliminar_facturas / mover_fecha / restaurar_fecha / etiquetar) so an
  # action taken from Vencidos or Search shows up in Gestión de Usuarios >
  # Actividad exactly like the same action taken from Calendario does
  # (found live 2026-07-29: deleting a manual invoice from Vencidos left no
  # log entry at all -- this shared handler never logged any action at all).
  # Split by ledger since a single call here can span AR and AP at once,
  # unlike Calendario's own handlers which only ever act on one ledger.
  .log_ledger_action <- function(keys_df, action, describe) {
    cid      <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
    home_cid <- tryCatch(shared$home_client_id(),      error = function(e) NULL)
    for (lg in unique(keys_df$ledger)) {
      sub <- keys_df[keys_df$ledger == lg, , drop = FALSE]
      tryCatch(
        log_action(
          user        = current_user,
          module      = paste0("ledger_", lg),
          action      = action,
          description = describe(sub),
          target_id   = paste(sub[["Documento"]][seq_len(min(5, nrow(sub)))], collapse = ", "),
          metadata    = list(n = nrow(sub)),
          client_id             = cid,
          viewer_home_client_id = home_cid
        ),
        error = function(e) message("[VENCIDOS/SEARCH] log_action failed (", action, "): ", e$message)
      )
    }
  }

  # Kept in sync by hand with the identical helper in R/ledger_module.R's
  # .handle_tags_once(), so a tag applied from Calendario logs identically
  # to one applied from Vencidos or the Search modal.
  .tag_label_es <- function(new_tags) {
    if (!length(new_tags)) "sin etiqueta"
    else if (length(new_tags) == 2L) "urgente e importante"
    else if (identical(new_tags, "urgent")) "urgente"
    else "importante"
  }

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
    # BUG (found 2026-09-03, same class already fixed for
    # .save_moves_deferred() in R/ledger_module.R -- this is search_module.R's
    # own separate implementation of the identical action, reached from
    # Vencidos/the Search modal rather than Calendario): the save's error
    # handler only showed a transient warning toast, never gating the success
    # notification/log below -- a failed save still showed "tags updated" and
    # logged it as a fact. Now retries, and skips the false-success
    # notification/log if it still fails, showing a persistent error instead.
    tag_result <- s3_save_with_retry(function()
      save_tags(tdb, client_id = shared$effective_client_id()))
    if (isTRUE(tag_result$ok)) {
      bump_sync_version("tags_db")
      showNotification(paste0(nrow(keys_df), " factura(s) etiquetadas."),
                       type = "message", duration = 2)
    } else {
      showNotification(
        paste0("No se pudieron guardar las etiquetas (\"", tag_result$error$message,
               "\"). El cambio se ve en pantalla pero NO quedo guardado -- vuelve a intentarlo."),
        type = "error", duration = NULL
      )
    }
    .log_ledger_action(keys_df, if (isTRUE(tag_result$ok)) "etiquetar" else "etiquetar_fallo_guardado", function(sub)
      if (isTRUE(tag_result$ok))
        sprintf("%d factura(s) etiquetada(s) como %s", nrow(sub), .tag_label_es(new_tags))
      else
        sprintf("FALLO AL GUARDAR (no se aplico realmente): %d factura(s) etiquetada(s) como %s -- error S3: %s",
                nrow(sub), .tag_label_es(new_tags), tag_result$error$message))

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
    .save_moves_deferred(updated, client_id = shared$effective_client_id(), session = session)
    showNotification(
      paste0(nrow(keys_df), " factura(s) movidas a ", format(new_date, "%d/%m/%Y"), "."),
      type = "message", duration = 2)
    .log_ledger_action(keys_df, "mover_fecha", function(sub)
      sprintf("%d factura(s) movida(s) a %s", nrow(sub), format(new_date, "%d/%m/%Y")))

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
    .save_moves_deferred(updated, client_id = shared$effective_client_id(), session = session)
    showNotification(
      paste0(nrow(keys_df), " factura(s) restauradas a fecha original."),
      type = "message", duration = 2)
    .log_ledger_action(keys_df, "restaurar_fecha", function(sub)
      sprintf("%d factura(s) restaurada(s) a su fecha original", nrow(sub)))

  } else if (action == "delete") {
    # Provisions use lifecycle close, not papelera
    prov_rows <- keys_df[keys_df$source == "provision" & nzchar(keys_df$provision_id), ]
    item_rows <- keys_df[!(keys_df$source == "provision" & nzchar(keys_df$provision_id)), ]

    if (nrow(prov_rows)) {
      pids_cancel <- unique(prov_rows$provision_id)
      for (pid in pids_cancel) {
        tryCatch(pasivos_provision_cancel(provision_id = pid, user = current_user,
                                          client_id = shared$effective_client_id()),
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

    # BUG (found 2026-09-03, same class already fixed for
    # .save_moves_deferred() in R/ledger_module.R -- this is search_module.R's
    # own separate implementation of the identical action, reached from
    # Vencidos/the Search modal rather than Calendario): both saves' error
    # handlers only showed a transient warning toast, never gating the
    # success notification or the log_action below -- either could silently
    # fail to persist while the user still saw "sent to papelera" and
    # Actividad still logged it as a fact. Now each retries, and the final
    # notification/log is honest about which piece (if any) never saved.
    item_save_failures <- character(0)
    if (nrow(item_rows)) {
      cid         <- tryCatch(shared$effective_client_id(), error = function(e) NULL)
      papelera_df <- tryCatch(load_papelera(client_id = cid), error = function(e) tibble::tibble())
      archive      <- item_rows
      archive$FechaEff   <- as.Date(NA)
      archive$deleted_by <- current_user
      archive$deleted_at <- Sys.time()
      papelera_df <- add_to_papelera(papelera_df, archive,
                                      ledger = "MIXED", deleted_by = current_user)
      papelera_result <- s3_save_with_retry(function()
        save_papelera(papelera_df, client_id = shared$effective_client_id()))
      if (isTRUE(papelera_result$ok)) {
        # Update shared reactive so calendars refresh without extra S3 read
        if (!is.null(shared$papelera_rv)) shared$papelera_rv(papelera_df)
      } else {
        item_save_failures <- c(item_save_failures, paste0("papelera (", papelera_result$error$message, ")"))
      }

      manual_rows <- item_rows[item_rows$source == "manual" & nzchar(item_rows$inv_id), ]
      if (nrow(manual_rows)) {
        # Read fresh from S3, same as papelera_df above -- this deletes
        # rows, so a stale in-memory read here can silently resurrect a row
        # another session already removed (found 2026-07-24, a real
        # incident).
        m <- tryCatch(load_manual(client_id = cid), error = function(e) NULL) %||% shared$manual_inv()
        for (mid in manual_rows$inv_id) m <- delete_manual(m, mid)
        manual_result <- s3_save_with_retry(function()
          save_manual(m, client_id = shared$effective_client_id()))
        if (isTRUE(manual_result$ok)) {
          shared$manual_inv(m)
        } else {
          item_save_failures <- c(item_save_failures, paste0("manual_inv (", manual_result$error$message, ")"))
        }
      }
      if (length(item_save_failures)) {
        showNotification(
          paste0("No se pudo guardar completamente el borrado (", paste(item_save_failures, collapse = "; "),
                 "). Puede que quede en un estado parcial -- vuelve a intentarlo en unos minutos y ",
                 "no cierres la sesion mientras tanto."),
          type = "error", duration = NULL
        )
      } else {
        showNotification(paste0(nrow(item_rows), " factura(s) enviadas a la papelera."),
                         type = "message", duration = 3)
      }
    }

    # One combined log entry per ledger for the whole deleted selection
    # (provisions + papelera items together), matching how Calendario's own
    # delete handler logs eliminar_facturas once for the full rows_to_delete
    # set regardless of source.
    .log_ledger_action(keys_df, if (!length(item_save_failures)) "eliminar_facturas" else "eliminar_facturas_fallo_guardado", function(sub)
      if (!length(item_save_failures))
        sprintf("%d factura(s) enviada(s) a papelera", nrow(sub))
      else
        sprintf("FALLO AL GUARDAR (estado parcial): %d factura(s) enviada(s) a papelera -- errores: %s",
                nrow(sub), paste(item_save_failures, collapse = "; ")))

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
      # Without this, every row staged here loses its source at write time
      # and gets normalized to "sap" -- a manual entry confirmed later would
      # then never be archived out of manual_inv (found 2026-07-24, the same
      # bug as ledger_module.R's stage_all/stage_sel, in this shared
      # handler used by both Search and Vencidos' Agregar todo/selección).
      source    = ifelse(is.na(keys_df$source) | keys_df$source != "manual", SAP_B1_ERP_TYPE, "manual"),
      stringsAsFactors = FALSE
    )
    # Skip rows with blank Documento (can't key them)
    new_rows <- new_rows[nzchar(new_rows$Documento), , drop = FALSE]
    if (!nrow(new_rows)) {
      showNotification("No se encontraron facturas con documento válido.", type = "warning")
      return()
    }
    # Read fresh from S3 first (safe_load_pagar_hoy -- mode-aware, found
    # 2026-07-25); a stale base here would silently drop another session's
    # concurrent stage/unstage.
    ph_base <- tryCatch(safe_load_pagar_hoy(username = shared$current_user(),
                                            client_id = shared$effective_client_id()),
                        error = function(e) NULL) %||% shared$pagar_hoy_db()
    updated <- upsert_pagar_hoy(ph_base, new_rows)
    shared$pagar_hoy_db(updated)
    tryCatch(save_pagar_hoy(updated, shared$current_user(), client_id = shared$effective_client_id()), error = function(e)
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
