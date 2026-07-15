# =============================================================================
# R/pasivos_module.R
# Stage 2: Calendar integration — convert-to-item modal + manual provision
# creation from the existing Agregar Item modal.
#
# Call setup_pasivos_module(input, output, session, shared) once from app.R server.
# =============================================================================

# ── Modal UI ──────────────────────────────────────────────────────────────────

# Returns a modalDialog() pre-filled from the provision row.
# empresa_choices: character vector of Empresa options from shared$empresas_db().
pasivos_convert_modal_ui <- function(provision, liabilities, empresa_choices = character()) {
  p <- provision[1, , drop = FALSE]

  liability <- if (!is.na(p$liability_id[1]) && nzchar(p$liability_id[1] %||% "") &&
                   !is.null(liabilities) && nrow(liabilities)) {
    liabilities[liabilities$id == p$liability_id[1], , drop = FALSE]
  } else {
    NULL
  }

  pago_amount <- tryCatch(pasivos_pago_amount(p),   error = function(e) NA_real_)
  pago_cur    <- tryCatch(pasivos_pago_currency(p), error = function(e) "MXN")

  choices_emp <- sort(unique(c(p$empresa[1], empresa_choices)))
  choices_emp <- choices_emp[!is.na(choices_emp) & nzchar(choices_emp)]
  if (!length(choices_emp)) choices_emp <- p$empresa[1] %||% ""

  shiny::modalDialog(
    title     = "Convertir provisión a comprobante",
    size      = "l",
    easyClose = FALSE,

    shiny::div(class = "alert alert-info mb-3",
               "Esta acción reemplaza la provisión por un item real."),

    shiny::fluidRow(
      shiny::column(6,
        shiny::selectInput("pcm_empresa",   "Empresa",
                           choices  = choices_emp,
                           selected = p$empresa[1] %||% choices_emp[1]),
        shiny::selectInput("pcm_moneda",    "Moneda",
                           choices  = CURRENCIES,
                           selected = pago_cur %||% "MXN"),
        shiny::textInput("pcm_documento",   "Documento",
                         value = p$documento[1] %||% ""),
        shiny::textInput("pcm_factura",     "No. Factura",
                         value = p$referencia[1] %||% "")
      ),
      shiny::column(6,
        shiny::textInput("pcm_parte",       "Parte / Proveedor",
                         value = p$parte[1] %||% ""),
        shiny::textInput("pcm_codigo",      "Código",
                         value = p$codigo_parte[1] %||% ""),
        shiny::numericInput("pcm_importe",  "Importe",
                            value = if (is.na(pago_amount)) 0 else pago_amount,
                            min   = 0)
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::dateInput("pcm_fecha", "Fecha de vencimiento",
                         value     = tryCatch(as.Date(p$fecha_efectiva[1]), error = function(e) Sys.Date()),
                         weekstart = 1, language = "es")
      ),
      shiny::column(6,
        shiny::textAreaInput("pcm_notas", "Notas", rows = 3,
                             value = p$notas[1] %||% "")
      )
    ),

    footer = shiny::tagList(
      shiny::modalButton("Cancelar"),
      shiny::actionButton("pcm_delete_provision", "Eliminar",
                          class = "btn btn-outline-danger me-auto"),
      shiny::actionButton("pcm_save_provision",   "Guardar provisión",
                          class = "btn btn-outline-secondary"),
      shiny::actionButton("pcm_save_only",        "Guardar como comprobante",
                          class = "btn btn-default"),
      shiny::actionButton("pcm_save_and_stage",   "Guardar y agregar a Agenda de hoy",
                          class = "btn btn-primary")
    )
  )
}

# ── Manage-converted modal UI ─────────────────────────────────────────────────

# Returns a modalDialog for a provision that is already converted.
# The body is a uiOutput that the server renders reactively (inline confirm).
pasivos_manage_converted_modal_ui <- function() {
  shiny::modalDialog(
    title     = "Gestionar comprobante de provisión",
    size      = "s",
    easyClose = TRUE,
    shiny::uiOutput("pcm_conv_body"),
    footer    = shiny::modalButton("Cerrar")
  )
}

# ── Shared conversion logic ────────────────────────────────────────────────────

# Creates manual_inv row + optional pagar_hoy row + flips provision state.
# Returns list(ok, msg, manual_id, pagar_hoy_id).
.pasivos_perform_conversion <- function(input, shared, prov_id, stage_to_agenda, user) {
  provs <- tryCatch(load_pasivos_provisions(client_id = shared$active_client_id()), error = function(e) NULL)
  if (is.null(provs)) return(list(ok = FALSE, msg = "No se pudo cargar la tabla de provisiones."))

  prov <- provs[provs$id == prov_id, , drop = FALSE]
  if (!nrow(prov) || prov$estado[1] != "provisional") {
    return(list(ok = FALSE, msg = "Provisión no disponible para conversión."))
  }

  # 1. Create manual_inv row
  new_manual_id <- uuid::UUIDgenerate()
  now           <- Sys.time()

  # Provisions store empresa as initials (e.g. "NCS"); pagar_hoy and manual_inv
  # must store full company names so pagar_hoy_module and build_ledger_df match them.
  cmap         <- tryCatch(isolate(shared$company_map()), error = function(e) NULL)
  full_empresa <- {
    key <- input$pcm_empresa %||% ""
    if (!is.null(cmap) && length(cmap) && nzchar(key)) {
      # cmap is a named character vector (initials → full name).
      # Single [ never throws for missing keys; [[ throws "subscript out of bounds".
      looked_up <- unname(cmap[key])
      if (!is.na(looked_up)) looked_up else key
    } else {
      key
    }
  }

  new_manual <- tibble::tibble(
    id                     = new_manual_id,
    ledger                 = "AP",
    Empresa                = full_empresa,
    Moneda                 = input$pcm_moneda,
    Documento              = {
      doc_raw <- trimws(input$pcm_documento %||% "")
      if (nzchar(doc_raw)) doc_raw else paste0("CONV_", prov_id)
    },
    Factura                = input$pcm_factura,
    Parte                  = input$pcm_parte,
    Codigo                 = input$pcm_codigo,
    Importe                = as.numeric(input$pcm_importe),
    `Abono futuro`         = NA_real_,
    `Fecha de vencimiento` = as.Date(input$pcm_fecha),
    Notas                  = input$pcm_notas,
    created_by             = user,
    created_at             = now,
    updated_at             = now,
    provision_id           = prov_id,
    liability_id           = prov$liability_id[1],
    referencia             = prov$referencia[1] %||% ""
  )

  manual_df <- shared$manual_inv()
  manual_df <- dplyr::bind_rows(manual_df, new_manual)
  shared$manual_inv(manual_df)
  tryCatch(save_manual(manual_df, client_id = shared$active_client_id()), error = function(e)
    warning("[pasivos] save_manual failed: ", conditionMessage(e)))

  # 2. Optional: stage to pagar_hoy
  new_pagar_hoy_id <- NA_character_
  if (stage_to_agenda) {
    new_pagar_hoy_id <- uuid::UUIDgenerate()
    new_ph_row <- tibble::tibble(
      id           = new_pagar_hoy_id,
      ledger       = "AP",
      Empresa      = full_empresa,
      Moneda       = input$pcm_moneda,
      Documento    = {
        doc_raw <- trimws(input$pcm_documento %||% "")
        if (nzchar(doc_raw)) doc_raw else paste0("CONV_", prov_id)
      },
      Parte        = input$pcm_parte,
      Codigo       = input$pcm_codigo,
      tipo_item    = "factura",
      Importe      = as.numeric(input$pcm_importe),
      FechaVenc    = as.Date(input$pcm_fecha),
      staged_by    = user,
      staged_at    = now,
      status       = "pending",
      provision_id = prov_id,
      liability_id = prov$liability_id[1],
      source       = "provision"
    )
    ph_df <- upsert_pagar_hoy(
      shared$pagar_hoy_db() %||% load_pagar_hoy(client_id = shared$active_client_id()),
      new_ph_row,
      keys = c("ledger", "Empresa", "Moneda", "Documento")
    )
    shared$pagar_hoy_db(ph_df)
    tryCatch(save_pagar_hoy(ph_df, user, client_id = shared$active_client_id()), error = function(e)
      warning("[pasivos] save_pagar_hoy failed: ", conditionMessage(e)))
  }

  # 3. Flip provision state via engine (writes audit log)
  result <- tryCatch(
    pasivos_provision_convert(
      provision_id  = prov_id,
      manual_inv_id = new_manual_id,
      pagar_hoy_id  = new_pagar_hoy_id,
      user          = user,
      client_id     = shared$active_client_id()
    ),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    return(list(ok    = FALSE,
                msg   = paste0("Provisión convertida pero el cambio de estado falló: ",
                               conditionMessage(result)),
                manual_id    = new_manual_id,
                pagar_hoy_id = new_pagar_hoy_id))
  }

  list(ok = TRUE, manual_id = new_manual_id, pagar_hoy_id = new_pagar_hoy_id)
}

# ── Server observers ──────────────────────────────────────────────────────────

# Call once from app.R server.
setup_pasivos_module <- function(input, output, session, shared) {

  rv <- shiny::reactiveValues(
    pending_provision_id = NULL,
    conv_prov_id         = NULL,   # provision id being managed in convert-mgmt modal
    conv_mgmt_view       = "main"  # "main" | "confirm_undo" | "confirm_delete"
  )

  # ── Lightning-bolt click → open convert modal ─────────────────────────────
  shiny::observeEvent(input$pasivos_convert_request, ignoreInit = TRUE, {
    prov_id <- input$pasivos_convert_request
    if (is.null(prov_id) || !nzchar(prov_id %||% "")) return()

    user <- tryCatch(shared$current_user(), error = function(e) "")

    if (!has_capability(user, "pasivos.convert_to_item")) {
      shiny::showNotification("No tienes permiso para convertir provisiones.", type = "error")
      tryCatch(pasivos_log_audit(
        action_type = "capability.denied",
        user        = user,
        target_kind = "provision", target_id = prov_id,
        notes       = "convert_to_item",
        client_id   = shared$active_client_id()
      ), error = function(e) NULL)
      return()
    }

    provs <- tryCatch(load_pasivos_provisions(client_id = shared$active_client_id()), error = function(e) NULL)
    if (is.null(provs)) {
      shiny::showNotification("Error cargando provisiones.", type = "error")
      return()
    }
    prov <- provs[provs$id == prov_id, , drop = FALSE]
    if (!nrow(prov)) {
      shiny::showNotification("Provisión no encontrada.", type = "error")
      return()
    }
    if (prov$estado[1] %in% c("converted", "item_confirmed")) {
      rv$conv_prov_id   <- prov_id
      rv$conv_mgmt_view <- "main"
      shiny::showModal(pasivos_manage_converted_modal_ui())
      return()
    }
    if (prov$estado[1] != "provisional") {
      shiny::showNotification(
        "Esta provisión ya no está disponible para conversión.", type = "warning")
      return()
    }

    rv$pending_provision_id <- prov_id

    liabs          <- tryCatch(load_pasivos_liabilities(client_id = shared$active_client_id()), error = function(e) NULL)
    empresa_choices <- tryCatch(sort(names(shared$company_map())),
                                error = function(e) sort(names(COMPANY_MAP)))

    shiny::showModal(pasivos_convert_modal_ui(prov, liabs, empresa_choices))
  })

  # ── pcm_delete_provision: Prompt confirmation before deleting ────────────
  shiny::observeEvent(input$pcm_delete_provision, ignoreInit = TRUE, {
    prov_id <- rv$pending_provision_id
    if (is.null(prov_id)) return()
    shiny::showModal(shiny::modalDialog(
      title     = "¿Eliminar provisión?",
      size      = "s",
      easyClose = TRUE,
      shiny::p("Esta acción eliminará la provisión de forma permanente. No se puede deshacer."),
      footer = shiny::tagList(
        shiny::modalButton("Cancelar"),
        shiny::actionButton("pcm_delete_confirm", "Sí, eliminar",
                            class = "btn btn-danger")
      )
    ))
  })

  # ── pcm_delete_confirm: Actually remove the provision ─────────────────────
  shiny::observeEvent(input$pcm_delete_confirm, ignoreInit = TRUE, {
    prov_id <- rv$pending_provision_id
    if (is.null(prov_id)) return()
    user <- tryCatch(shared$current_user(), error = function(e) "system")

    provs <- tryCatch(load_pasivos_provisions(client_id = shared$active_client_id()), error = function(e) NULL)
    if (is.null(provs)) {
      shiny::showNotification("Error cargando provisiones.", type = "error"); return()
    }
    idx <- which(provs$id == prov_id)
    if (!length(idx)) {
      shiny::showNotification("Provisión no encontrada.", type = "error")
      shiny::removeModal(); return()
    }
    if (provs$estado[idx[1]] != "provisional") {
      shiny::showNotification(
        "Solo se pueden eliminar provisiones en estado 'provisional'.", type = "warning")
      shiny::removeModal(); return()
    }

    now      <- Sys.time()
    prov_row <- provs[idx[1], , drop = FALSE]
    amount_del <- if (!is.na(prov_row$amount_pago_override[1]))
      prov_row$amount_pago_override[1] else prov_row$amount_pago[1]

    # Soft-delete: mark estado, keep the row in S3 forever for compliance
    provs$estado[idx]          <- "deleted"
    provs$last_edited_by[idx]  <- user
    provs$last_edited_at[idx]  <- now

    ok <- tryCatch({ save_pasivos_provisions(provs, client_id = shared$active_client_id()); TRUE },
                   error = function(e) {
                     shiny::showNotification(paste0("Error: ", conditionMessage(e)),
                                             type = "error"); FALSE
                   })
    if (!ok) return()
    tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
    shared$pasivos_provisions_db(provs)
    bump_sync_version("pasivos_provisions_db")

    # Archive a copy to the unified papelera (same S3 store used by all modules)
    tryCatch({
      papelera_df <- tryCatch(load_papelera(client_id = shared$active_client_id()), error = function(e) .schema_papelera())
      papelera_detail <- data.frame(
        id       = prov_row$id[1],
        source   = "provision",
        Empresa  = prov_row$empresa[1]    %||% "",
        Moneda   = prov_row$moneda_pago[1] %||% "MXN",
        Documento = prov_row$documento[1] %||% "",
        Parte    = prov_row$parte[1]       %||% "",
        Importe  = amount_del              %||% 0,
        FechaEff = prov_row$fecha_efectiva[1],
        stringsAsFactors = FALSE
      )
      papelera_df <- add_to_papelera(papelera_df, papelera_detail,
                                     ledger = "AP", deleted_by = user)
      save_papelera(papelera_df, client_id = shared$active_client_id())
      if (!is.null(shared$papelera_rv)) shared$papelera_rv(papelera_df)
    }, error = function(e)
      message("[pasivos] papelera write failed: ", conditionMessage(e)))

    tryCatch(pasivos_log_audit(
      action_type = "provision.deleted",
      user        = user,
      target_kind = "provision", target_id = prov_id,
      notes       = "soft-deleted via convert modal; archived to papelera",
      client_id   = shared$active_client_id()
    ), error = function(e) NULL)

    rv$pending_provision_id <- NULL
    shiny::removeModal()
    shiny::showNotification("Provisión eliminada.", type = "message", duration = 3)
  })

  # ── pcm_save_provision: Save edits back to the provision without converting ─
  shiny::observeEvent(input$pcm_save_provision, ignoreInit = TRUE, {
    prov_id <- rv$pending_provision_id
    if (is.null(prov_id)) return()
    user <- tryCatch(shared$current_user(), error = function(e) "system")

    provs <- tryCatch(load_pasivos_provisions(client_id = shared$active_client_id()), error = function(e) NULL)
    if (is.null(provs)) {
      shiny::showNotification("Error cargando provisiones.", type = "error"); return()
    }
    idx <- which(provs$id == prov_id)
    if (!length(idx)) {
      shiny::showNotification("Provisión no encontrada.", type = "error"); return()
    }

    now       <- Sys.time()
    new_fecha <- tryCatch(as.Date(input$pcm_fecha), error = function(e) as.Date(NA))
    new_imp   <- as.numeric(input$pcm_importe %||% NA_real_)

    provs$empresa[idx]       <- input$pcm_empresa   %||% provs$empresa[idx]
    provs$moneda_pago[idx]   <- input$pcm_moneda    %||% provs$moneda_pago[idx]
    provs$parte[idx]         <- input$pcm_parte     %||% provs$parte[idx]
    provs$codigo_parte[idx]  <- input$pcm_codigo    %||% provs$codigo_parte[idx]
    provs$documento[idx]     <- input$pcm_documento %||% provs$documento[idx]
    provs$referencia[idx]    <- input$pcm_factura   %||% provs$referencia[idx]
    provs$notas[idx]         <- input$pcm_notas     %||% provs$notas[idx]
    if (!is.na(new_imp))
      provs$amount_pago_override[idx] <- new_imp
    if (!is.na(new_fecha)) {
      provs$fecha_efectiva[idx]          <- new_fecha
      provs$fecha_efectiva_override[idx] <- new_fecha
    }
    provs$last_edited_by[idx] <- user
    provs$last_edited_at[idx] <- now

    ok <- tryCatch({ save_pasivos_provisions(provs, client_id = shared$active_client_id()); TRUE },
                   error = function(e) {
                     shiny::showNotification(paste0("Error: ", conditionMessage(e)),
                                             type = "error"); FALSE
                   })
    if (!ok) return()

    tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
    shared$pasivos_provisions_db(provs)
    bump_sync_version("pasivos_provisions_db")
    tryCatch(pasivos_log_audit(
      action_type = "provision.edited",
      user        = user,
      target_kind = "provision", target_id = prov_id,
      after       = list(parte = input$pcm_parte %||% "",
                         importe = new_imp,
                         fecha = as.character(new_fecha)),
      notes       = "inline provision edit via convert modal",
      client_id   = shared$active_client_id()
    ), error = function(e) NULL)

    rv$pending_provision_id <- NULL
    shiny::removeModal()
    shiny::showNotification("Provisión actualizada.", type = "message", duration = 3)
  })

  # ── pcm_save_only: Guardar como item (no agenda staging) ─────────────────
  shiny::observeEvent(input$pcm_save_only, ignoreInit = TRUE, {
    prov_id <- rv$pending_provision_id
    if (is.null(prov_id)) return()
    user <- tryCatch(shared$current_user(), error = function(e) "system")

    if (!has_capability(user, "pasivos.convert_to_item")) {
      shiny::showNotification("Sin permiso.", type = "error"); return()
    }

    res <- .pasivos_perform_conversion(input, shared, prov_id,
                                       stage_to_agenda = FALSE, user = user)
    if (!res$ok) {
      shiny::showNotification(res$msg, type = "error"); return()
    }
    rv$pending_provision_id <- NULL
    tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
    tryCatch({
      shared$pasivos_provisions_db(load_pasivos_provisions(client_id = shared$active_client_id()))
    }, error = function(e) NULL)
    shiny::removeModal()
    shiny::showNotification("Provisión convertida a comprobante.", type = "message")
  })

  # ── pcm_save_and_stage: Guardar y agregar a Agenda de hoy ────────────────
  shiny::observeEvent(input$pcm_save_and_stage, ignoreInit = TRUE, {
    prov_id <- rv$pending_provision_id
    if (is.null(prov_id)) return()
    user <- tryCatch(shared$current_user(), error = function(e) "system")

    if (!has_capability(user, "pasivos.convert_to_item")) {
      shiny::showNotification("Sin permiso.", type = "error"); return()
    }

    res <- .pasivos_perform_conversion(input, shared, prov_id,
                                       stage_to_agenda = TRUE, user = user)
    if (!res$ok) {
      shiny::showNotification(res$msg, type = "error"); return()
    }
    rv$pending_provision_id <- NULL
    tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
    tryCatch({
      shared$pasivos_provisions_db(load_pasivos_provisions(client_id = shared$active_client_id()))
    }, error = function(e) NULL)
    shiny::removeModal()
    shiny::showNotification(
      "Provisión convertida a comprobante e ingresada a Agenda de hoy.", type = "message")
  })

  # ── me_save_as_provision: Create orphan provision from Agregar Item modal ─
  shiny::observeEvent(input$me_save_as_provision, ignoreInit = TRUE, {
    user <- tryCatch(shared$current_user(), error = function(e) "")

    if (!has_capability(user, "pasivos.create_provision_manual")) {
      shiny::showNotification("No tienes permiso para crear provisiones.", type = "error")
      return()
    }

    shinyjs::disable("me_save_as_provision")
    on.exit(shinyjs::enable("me_save_as_provision"), add = TRUE)

    new_id  <- uuid::UUIDgenerate()
    now     <- Sys.time()
    empresa <- input$me_empresa %||% ""

    new_prov <- tibble::tibble(
      id                       = new_id,
      liability_id             = NA_character_,
      origin                   = "manual",
      occurrence_index         = NA_integer_,
      estado                   = "provisional",
      fecha_calculada          = as.Date(input$me_fecha),
      fecha_efectiva           = as.Date(input$me_fecha),
      policy_ids               = NA_character_,
      empresa                  = empresa,
      parte                    = input$me_parte %||% "",
      codigo_parte             = input$me_codigo %||% "",
      moneda_pago              = input$me_moneda %||% "MXN",
      cotizado_en              = NA_character_,
      amount_pago              = as.numeric(input$me_importe %||% 0),
      amount_cotizado          = NA_real_,
      fx_rate_used             = NA_real_,
      componente_capital       = NA_real_,
      componente_interes       = NA_real_,
      componente_fees          = NA_real_,
      componente_iva           = NA_real_,
      amount_pago_override     = NA_real_,
      amount_cotizado_override = NA_real_,
      fecha_efectiva_override  = as.Date(NA),
      documento                = input$me_documento %||% "",
      referencia               = input$me_referencia %||% "",
      notas                    = input$me_notas %||% "",
      manual_inv_id            = NA_character_,
      pagar_hoy_id             = NA_character_,
      bancos_conf_id           = NA_character_,
      reverted_count           = 0L,
      generated_by             = user,
      generated_at             = now,
      last_edited_by           = user,
      last_edited_at           = now
    )

    existing <- tryCatch(shared$pasivos_provisions_db(),
                         error = function(e) .schema_pasivos_provision())
    if (is.null(existing) || !is.data.frame(existing))
      existing <- .schema_pasivos_provision()

    provs <- dplyr::bind_rows(existing, new_prov)
    save_ok <- tryCatch({
      save_pasivos_provisions(provs, client_id = shared$active_client_id())
      TRUE
    }, error = function(e) {
      shiny::showNotification(
        paste0("Error al guardar la provisión: ", conditionMessage(e)),
        type = "error", duration = 15
      )
      FALSE
    })
    if (!save_ok) return()

    tryCatch(pasivos_log_audit(
      action_type = "provision.generated",
      user        = user,
      empresa     = empresa,
      target_kind = "provision",
      target_id   = new_id,
      after       = list(
        id = new_id, origin = "manual", estado = "provisional",
        empresa = empresa, moneda_pago = input$me_moneda %||% "MXN",
        amount_pago = input$me_importe %||% 0,
        fecha_efectiva = as.character(input$me_fecha)
      ),
      notes     = "manual orphan provision via Agregar modal",
      client_id = shared$active_client_id()
    ), error = function(e) NULL)

    # Refresh the shared reactive so the AP calendar picks up the new provision.
    tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
    tryCatch(shared$pasivos_provisions_db(provs), error = function(e) NULL)
    bump_sync_version("pasivos_provisions_db")

    shiny::removeModal()
    shiny::showNotification("Provisión manual creada.", type = "message")
  })

  # ── Cell click → open the same convert/edit modal ────────────────────────
  # Replaces the old pasivos_cell_editor: clicking any provision cell now
  # opens the full panel (Guardar provisión / Convertir / Eliminar).
  shiny::observeEvent(input$pasivos_cell_click, ignoreInit = TRUE, {
    prov_id <- input$pasivos_cell_click %||% ""
    if (!nzchar(prov_id)) return()

    user <- tryCatch(shared$current_user(), error = function(e) "")

    provs <- tryCatch(load_pasivos_provisions(client_id = shared$active_client_id()), error = function(e) NULL)
    if (is.null(provs)) {
      shiny::showNotification("Error cargando provisiones.", type = "error"); return()
    }
    prov <- provs[provs$id == prov_id, , drop = FALSE]
    if (!nrow(prov)) {
      shiny::showNotification("Provisión no encontrada.", type = "error"); return()
    }
    if (prov$estado[1] %in% c("converted", "item_confirmed")) {
      rv$conv_prov_id   <- prov_id
      rv$conv_mgmt_view <- "main"
      shiny::showModal(pasivos_manage_converted_modal_ui())
      return()
    }
    if (!prov$estado[1] %in% c("provisional")) {
      shiny::showNotification(
        "Esta provisión ya no está disponible para conversión.", type = "warning"); return()
    }

    if (!has_capability(user, "pasivos.convert_to_item")) {
      shiny::showNotification("No tienes permiso para convertir provisiones.", type = "error")
      return()
    }

    rv$pending_provision_id <- prov_id

    liabs           <- tryCatch(load_pasivos_liabilities(client_id = shared$active_client_id()), error = function(e) NULL)
    empresa_choices <- tryCatch(sort(names(shared$company_map())),
                                error = function(e) sort(names(COMPANY_MAP)))

    shiny::showModal(pasivos_convert_modal_ui(prov, liabs, empresa_choices))
  })

  # ── Manage-converted modal body (rendered reactively inside the open modal) ──
  output$pcm_conv_body <- shiny::renderUI({
    prov_id <- rv$conv_prov_id
    if (is.null(prov_id)) return(NULL)
    view <- rv$conv_mgmt_view %||% "main"

    provs <- tryCatch(load_pasivos_provisions(client_id = shared$active_client_id()), error = function(e) NULL)
    if (is.null(provs)) return(shiny::p(class = "text-danger", "Error cargando datos."))
    prov <- provs[provs$id == prov_id, , drop = FALSE]
    if (!nrow(prov)) return(shiny::p(class = "text-warning", "Provisión no encontrada."))

    amount   <- if (!is.na(prov$amount_pago_override[1])) prov$amount_pago_override[1] else prov$amount_pago[1]
    moneda   <- prov$moneda_pago[1] %||% "MXN"
    estado   <- prov$estado[1]
    is_conf  <- estado == "item_confirmed"

    manual_df <- tryCatch(load_manual(client_id = shared$active_client_id()), error = function(e) NULL)
    man_row <- if (!is.null(manual_df) && "provision_id" %in% names(manual_df))
      manual_df[!is.na(manual_df$provision_id) & manual_df$provision_id == prov_id, , drop = FALSE]
    else data.frame()

    if (view == "main") {
      shiny::tagList(
        shiny::div(class = "mb-3 p-2 bg-light rounded border-start border-3 border-warning",
          shiny::tags$dl(class = "row mb-0 small",
            shiny::tags$dt(class = "col-5", "Empresa"),
            shiny::tags$dd(class = "col-7 mb-0", prov$empresa[1] %||% "—"),
            shiny::tags$dt(class = "col-5", "Parte"),
            shiny::tags$dd(class = "col-7 mb-0", prov$parte[1] %||% "—"),
            shiny::tags$dt(class = "col-5", "Importe"),
            shiny::tags$dd(class = "col-7 mb-0", paste(moneda, fmt_money(amount))),
            shiny::tags$dt(class = "col-5", "Fecha"),
            shiny::tags$dd(class = "col-7 mb-0", format(as.Date(prov$fecha_efectiva[1]), "%d/%m/%Y")),
            if (nrow(man_row)) shiny::tagList(
              shiny::tags$dt(class = "col-5", "Documento"),
              shiny::tags$dd(class = "col-7 mb-0", man_row$Documento[1] %||% "—")
            )
          )
        ),
        if (is_conf)
          shiny::div(class = "alert alert-warning small py-2",
            "⚠ Este comprobante ya fue confirmado en Bancos. Solo se puede eliminar.")
        else NULL,
        shiny::div(class = "d-flex gap-2 mt-2",
          if (!is_conf)
            shiny::actionButton("pcm_conv_want_undo",
                                "↩ Deshacer conversión",
                                class = "btn btn-sm btn-outline-secondary")
          else NULL,
          shiny::actionButton("pcm_conv_want_delete",
                              "\U0001f5d1 Eliminar definitivamente",
                              class = "btn btn-sm btn-outline-danger")
        )
      )

    } else if (view == "confirm_undo") {
      shiny::tagList(
        shiny::div(class = "alert alert-warning",
          shiny::tags$strong("⚠ ¿Deshacer la conversión?"),
          shiny::tags$p(class = "mb-0 mt-1 small",
            "El comprobante se eliminará del calendario y la Agenda de hoy. ",
            "La provisión volverá a su estado original (pendiente de convertir).")
        ),
        shiny::div(class = "d-flex gap-2",
          shiny::actionButton("pcm_conv_cancel_action", "Cancelar",
                              class = "btn btn-sm btn-outline-secondary"),
          shiny::actionButton("pcm_do_undo_conversion", "✓ Confirmar",
                              class = "btn btn-sm btn-warning")
        )
      )

    } else if (view == "confirm_delete") {
      shiny::tagList(
        shiny::div(class = "alert alert-danger",
          shiny::tags$strong("⚠ ¿Eliminar definitivamente?"),
          shiny::tags$p(class = "mb-0 mt-1 small",
            "Esta acción eliminará el comprobante ",
            if (estado == "item_confirmed") "y el registro de Bancos " else "",
            "y la provisión de forma permanente. Se archivará en la papelera.")
        ),
        shiny::div(class = "d-flex gap-2",
          shiny::actionButton("pcm_conv_cancel_action", "Cancelar",
                              class = "btn btn-sm btn-outline-secondary"),
          shiny::actionButton("pcm_do_delete_converted", "\U0001f5d1 Eliminar",
                              class = "btn btn-sm btn-danger")
        )
      )
    }
  })

  # ── Inline-confirm state transitions ──────────────────────────────────────────
  shiny::observeEvent(input$pcm_conv_want_undo,   { rv$conv_mgmt_view <- "confirm_undo"   }, ignoreInit = TRUE)
  shiny::observeEvent(input$pcm_conv_want_delete, { rv$conv_mgmt_view <- "confirm_delete" }, ignoreInit = TRUE)
  shiny::observeEvent(input$pcm_conv_cancel_action, { rv$conv_mgmt_view <- "main"         }, ignoreInit = TRUE)

  # ── pcm_do_undo_conversion: revert converted provision ────────────────────────
  shiny::observeEvent(input$pcm_do_undo_conversion, ignoreInit = TRUE, {
    prov_id <- rv$conv_prov_id
    if (is.null(prov_id)) return()
    user <- tryCatch(shared$current_user(), error = function(e) "system")
    now  <- Sys.time()

    provs <- tryCatch(load_pasivos_provisions(client_id = shared$active_client_id()), error = function(e) NULL)
    if (is.null(provs)) { shiny::showNotification("Error cargando provisiones.", type = "error"); return() }
    idx <- which(provs$id == prov_id)
    if (!length(idx)) { shiny::showNotification("Provisión no encontrada.", type = "error"); return() }
    if (!provs$estado[idx[1]] %in% c("converted", "item_confirmed")) {
      shiny::showNotification("La provisión no puede ser revertida en este estado.", type = "warning")
      shiny::removeModal(); return()
    }

    manual_inv_id <- provs$manual_inv_id[idx[1]]
    pagar_hoy_id  <- provs$pagar_hoy_id[idx[1]]

    # 1. Remove manual_inv entry — capture details first for pagar_hoy cleanup below
    mi <- tryCatch(load_manual(client_id = shared$active_client_id()), error = function(e) NULL)
    mi_details <- NULL
    if (!is.null(mi) && "id" %in% names(mi) && !is.na(manual_inv_id) && nzchar(manual_inv_id %||% "")) {
      mi_details <- mi[!is.na(mi$id) & mi$id == manual_inv_id, , drop = FALSE]
      if (!nrow(mi_details)) mi_details <- NULL
      mi_new <- mi[!(!is.na(mi$id) & mi$id == manual_inv_id), , drop = FALSE]
      tryCatch(save_manual(mi_new, client_id = shared$active_client_id()), error = function(e)
        shiny::showNotification(paste("Error al guardar manual_inv:", e$message), type = "warning"))
      if (!is.null(shared$manual_inv)) shared$manual_inv(mi_new)
    }

    # 2. Remove pagar_hoy (pending only) — by FK id, by provision_id, and by
    #    business key so calendar-staged rows (no FK) are also cleaned up.
    ph <- if (!is.null(shared$pagar_hoy_db)) shared$pagar_hoy_db() else
          tryCatch(load_pagar_hoy(client_id = shared$active_client_id()), error = function(e) NULL)
    if (!is.null(ph) && nrow(ph) > 0) {
      rm_mask <- rep(FALSE, nrow(ph))
      if (!is.na(pagar_hoy_id) && nzchar(pagar_hoy_id %||% ""))
        rm_mask <- rm_mask | (!is.na(ph$id) & ph$id == pagar_hoy_id)
      if ("provision_id" %in% names(ph))
        rm_mask <- rm_mask | (!is.na(ph$provision_id) & ph$provision_id == prov_id)
      if (!is.null(mi_details)) {
        emp_k <- mi_details$Empresa[1] %||% ""
        mon_k <- mi_details$Moneda[1]  %||% ""
        doc_k <- mi_details$Documento[1] %||% ""
        if (nzchar(emp_k) && nzchar(mon_k) && nzchar(doc_k))
          rm_mask <- rm_mask | (
            !is.na(ph$ledger)    & ph$ledger    == "AP" &
            !is.na(ph$Empresa)   & ph$Empresa   == emp_k &
            !is.na(ph$Moneda)    & ph$Moneda    == mon_k &
            !is.na(ph$Documento) & ph$Documento == doc_k)
      }
      rm_mask <- rm_mask & (is.na(ph$status) | ph$status != "confirmed")
      if (any(rm_mask)) {
        ph_new <- ph[!rm_mask, , drop = FALSE]
        tryCatch(save_pagar_hoy(ph_new, user, client_id = shared$active_client_id()), error = function(e)
          shiny::showNotification(paste("Error al guardar agenda:", e$message), type = "warning"))
        if (!is.null(shared$pagar_hoy_db)) shared$pagar_hoy_db(ph_new)
      }
    }

    # 3. Revert provision estado to "provisional"
    provs$estado[idx]          <- "provisional"
    provs$manual_inv_id[idx]   <- NA_character_
    provs$pagar_hoy_id[idx]    <- NA_character_
    provs$bancos_conf_id[idx]  <- NA_character_
    provs$reverted_count[idx]  <- (as.integer(provs$reverted_count[idx[1]]) %||% 0L) + 1L
    provs$last_edited_by[idx]  <- user
    provs$last_edited_at[idx]  <- now

    ok <- tryCatch({ save_pasivos_provisions(provs, client_id = shared$active_client_id()); TRUE }, error = function(e) {
      shiny::showNotification(paste("Error:", e$message), type = "error"); FALSE })
    if (!ok) return()
    tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
    shared$pasivos_provisions_db(provs)
    bump_sync_version("pasivos_provisions_db")

    tryCatch(pasivos_log_audit(
      action_type = "provision.revived",
      user = user, target_kind = "provision", target_id = prov_id,
      notes     = "reverted from converted to provisional via UI",
      client_id = shared$active_client_id()
    ), error = function(e) NULL)

    rv$conv_prov_id   <- NULL
    rv$conv_mgmt_view <- "main"
    shiny::removeModal()
    shiny::showNotification("Conversión deshecha. La provisión fue restaurada.", type = "message", duration = 4)
  })

  # ── pcm_do_delete_converted: fully delete a converted provision ───────────────
  shiny::observeEvent(input$pcm_do_delete_converted, ignoreInit = TRUE, {
    prov_id <- rv$conv_prov_id
    if (is.null(prov_id)) return()
    user <- tryCatch(shared$current_user(), error = function(e) "system")
    now  <- Sys.time()

    provs <- tryCatch(load_pasivos_provisions(client_id = shared$active_client_id()), error = function(e) NULL)
    if (is.null(provs)) { shiny::showNotification("Error cargando provisiones.", type = "error"); return() }
    idx <- which(provs$id == prov_id)
    if (!length(idx)) { shiny::showNotification("Provisión no encontrada.", type = "error"); return() }

    manual_inv_id <- provs$manual_inv_id[idx[1]]
    pagar_hoy_id  <- provs$pagar_hoy_id[idx[1]]
    prov_row      <- provs[idx[1], , drop = FALSE]
    amount_del    <- if (!is.na(prov_row$amount_pago_override[1])) prov_row$amount_pago_override[1] else prov_row$amount_pago[1]

    # 1. Remove manual_inv entry — capture details first for pagar_hoy cleanup below
    mi <- tryCatch(load_manual(client_id = shared$active_client_id()), error = function(e) NULL)
    mi_details <- NULL
    if (!is.null(mi) && "id" %in% names(mi) && !is.na(manual_inv_id) && nzchar(manual_inv_id %||% "")) {
      mi_details <- mi[!is.na(mi$id) & mi$id == manual_inv_id, , drop = FALSE]
      if (!nrow(mi_details)) mi_details <- NULL
      mi_new <- mi[!(!is.na(mi$id) & mi$id == manual_inv_id), , drop = FALSE]
      tryCatch(save_manual(mi_new, client_id = shared$active_client_id()), error = function(e)
        shiny::showNotification(paste("Error al guardar manual_inv:", e$message), type = "warning"))
      if (!is.null(shared$manual_inv)) shared$manual_inv(mi_new)
    }

    # 2. Remove pagar_hoy (pending only) — by FK id, by provision_id, and by
    #    business key so calendar-staged rows (no FK) are also cleaned up.
    ph <- if (!is.null(shared$pagar_hoy_db)) shared$pagar_hoy_db() else
          tryCatch(load_pagar_hoy(client_id = shared$active_client_id()), error = function(e) NULL)
    if (!is.null(ph) && nrow(ph) > 0) {
      rm_mask <- rep(FALSE, nrow(ph))
      if (!is.na(pagar_hoy_id) && nzchar(pagar_hoy_id %||% ""))
        rm_mask <- rm_mask | (!is.na(ph$id) & ph$id == pagar_hoy_id)
      if ("provision_id" %in% names(ph))
        rm_mask <- rm_mask | (!is.na(ph$provision_id) & ph$provision_id == prov_id)
      if (!is.null(mi_details)) {
        emp_k <- mi_details$Empresa[1] %||% ""
        mon_k <- mi_details$Moneda[1]  %||% ""
        doc_k <- mi_details$Documento[1] %||% ""
        if (nzchar(emp_k) && nzchar(mon_k) && nzchar(doc_k))
          rm_mask <- rm_mask | (
            !is.na(ph$ledger)    & ph$ledger    == "AP" &
            !is.na(ph$Empresa)   & ph$Empresa   == emp_k &
            !is.na(ph$Moneda)    & ph$Moneda    == mon_k &
            !is.na(ph$Documento) & ph$Documento == doc_k)
      }
      rm_mask <- rm_mask & (is.na(ph$status) | ph$status != "confirmed")
      if (any(rm_mask)) {
        ph_new <- ph[!rm_mask, , drop = FALSE]
        tryCatch(save_pagar_hoy(ph_new, user, client_id = shared$active_client_id()), error = function(e)
          shiny::showNotification(paste("Error al guardar agenda:", e$message), type = "warning"))
        if (!is.null(shared$pagar_hoy_db)) shared$pagar_hoy_db(ph_new)
      }
    }

    # 3. Soft-delete provision
    provs$estado[idx]         <- "deleted"
    provs$last_edited_by[idx] <- user
    provs$last_edited_at[idx] <- now

    ok <- tryCatch({ save_pasivos_provisions(provs, client_id = shared$active_client_id()); TRUE }, error = function(e) {
      shiny::showNotification(paste("Error:", e$message), type = "error"); FALSE })
    if (!ok) return()
    tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
    shared$pasivos_provisions_db(provs)
    bump_sync_version("pasivos_provisions_db")

    # 4. Archive to papelera
    tryCatch({
      pap <- tryCatch(load_papelera(client_id = shared$active_client_id()), error = function(e) .schema_papelera())
      pap_detail <- data.frame(
        id = prov_row$id[1], source = "provision",
        Empresa = prov_row$empresa[1] %||% "", Moneda = prov_row$moneda_pago[1] %||% "MXN",
        Documento = prov_row$documento[1] %||% "", Parte = prov_row$parte[1] %||% "",
        Importe = amount_del %||% 0, FechaEff = prov_row$fecha_efectiva[1],
        stringsAsFactors = FALSE
      )
      pap <- add_to_papelera(pap, pap_detail, ledger = "AP", deleted_by = user)
      save_papelera(pap, client_id = shared$active_client_id())
      if (!is.null(shared$papelera_rv)) shared$papelera_rv(pap)
    }, error = function(e) message("[pasivos] papelera write failed: ", e$message))

    tryCatch(pasivos_log_audit(
      action_type = "provision.deleted",
      user = user, target_kind = "provision", target_id = prov_id,
      notes     = "hard-deleted converted provision via manage-converted UI",
      client_id = shared$active_client_id()
    ), error = function(e) NULL)

    rv$conv_prov_id   <- NULL
    rv$conv_mgmt_view <- "main"
    shiny::removeModal()
    shiny::showNotification("Provisión y comprobante eliminados.", type = "message", duration = 4)
  })

  invisible(NULL)
}


# =============================================================================
# Admin / diagnostic helpers (CLI only — NOT exposed in the UI)
#
# These are command-line / REPL tools for the app maintainer to inspect and
# clean the pasivos_provisions store.
#
# Usage from an R session connected to the same S3 environment:
#   pasivos_admin_inventory()                          # see what's in the store
#   pasivos_admin_purge_closed()                       # dry run — shows what WOULD be purged
#   pasivos_admin_purge_closed(dry_run = FALSE)        # actually purge
#   pasivos_admin_purge_closed(drop_estados = c("closed"), dry_run = FALSE, user = "luis")
#
# A backup is automatically written under a timestamped key before any destructive write.
# =============================================================================

# Returns a tibble summarising the current pasivos_provisions store by estado.
# Read-only — does not modify.
pasivos_admin_inventory <- function(client_id = NULL) {
  provs <- load_pasivos_provisions(client_id = client_id)
  if (!nrow(provs)) {
    message("pasivos_provisions store is empty.")
    return(invisible(NULL))
  }

  by_estado <- table(provs$estado, useNA = "ifany")
  message("Pasivos provisions inventory:")
  for (k in names(by_estado)) {
    label <- if (is.na(k) || k == "") "<NA / empty>" else k
    message(sprintf("  %-20s  %d rows", label, by_estado[[k]]))
  }
  message(sprintf("  %-20s  %d rows total", "TOTAL", nrow(provs)))

  invisible(provs)
}

# Removes rows where estado %in% drop_estados (default: "closed" only).
# Writes a backup snapshot to S3 BEFORE the destructive write, then performs
# the cleanup and logs an audit entry. Returns the count of removed rows.
# IMPORTANT: this is destructive. The backup is the only recovery path.
pasivos_admin_purge_closed <- function(drop_estados = "closed",
                                       user = "system",
                                       dry_run = TRUE,
                                       client_id = NULL) {
  provs <- load_pasivos_provisions(client_id = client_id)
  if (!nrow(provs)) {
    message("Nothing to purge.")
    return(invisible(0L))
  }

  to_drop <- provs$estado %in% drop_estados
  n_drop  <- sum(to_drop)

  if (!n_drop) {
    message(sprintf("No rows match estado in {%s}.", paste(drop_estados, collapse = ", ")))
    return(invisible(0L))
  }

  if (isTRUE(dry_run)) {
    message(sprintf("[DRY RUN] Would purge %d row(s) with estado in {%s}. Pass dry_run = FALSE to execute.",
                    n_drop, paste(drop_estados, collapse = ", ")))
    print(provs[to_drop, c("id", "liability_id", "estado", "fecha_efectiva",
                            "amount_pago", "parte"), drop = FALSE])
    return(invisible(n_drop))
  }

  backup_key <- sprintf("pasivos_provisions_backup_%s.rds",
                        format(Sys.time(), "%Y%m%d_%H%M%S"))
  tryCatch({
    .s3_write(provs, backup_key)
    message(sprintf("Backup written: %s", backup_key))
  }, error = function(e) {
    stop("[pasivos] backup failed; aborting purge. Error: ", conditionMessage(e))
  })

  cleaned <- provs[!to_drop, , drop = FALSE]
  save_pasivos_provisions(cleaned, client_id = client_id)
  bump_sync_version("pasivos_provisions_db")

  tryCatch(pasivos_log_audit(
    action_type = "bulk.purge_closed_provisions",
    user        = user,
    target_kind = "bulk",
    target_id   = NA_character_,
    notes       = sprintf("Purged %d rows with estado in {%s}; backup at %s",
                          n_drop, paste(drop_estados, collapse = ", "), backup_key),
    client_id   = client_id
  ), error = function(e) NULL)

  message(sprintf("Purged %d row(s). Backup: %s", n_drop, backup_key))
  invisible(n_drop)
}
