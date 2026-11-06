# =============================================================================
# R/pasivos_edit_confirm_module.R
# Stage 4: Edit-confirmation panel for reconciliation conflicts.
#
# Opened automatically after wizard save or "Apply to future" in the cell
# editor when pasivos_reconcile_provisions() returns conflicts.
#
# Public entry point:
#   pasivos_edit_confirm_open(session, conflicts, replacement, shared, liability_id)
# Setup call (once from app.R):
#   setup_pasivos_edit_confirm(input, output, session, shared)
# =============================================================================

# ── Open entry point ──────────────────────────────────────────────────────────
# Called from the wizard save handler and from the cell editor's apply-to-future.
# `conflicts` and `replacement` are tibbles from pasivos_reconcile_provisions().
pasivos_edit_confirm_open <- function(session, conflicts, replacement,
                                       shared, liability_id) {
  # The panel's state lives in this session, set up by
  # setup_pasivos_edit_confirm(). It used to be an environment of the package,
  # one for every session of the R process, so a user could see (and save
  # decisions on) another user's conflicts (found 2026-11-04, T18a).
  st <- session$userData$pec_state
  if (is.null(st))
    stop("pasivos_edit_confirm_open(): setup_pasivos_edit_confirm() did not run in this session",
         call. = FALSE)
  st$conflicts    <- conflicts
  st$replacement  <- replacement
  st$liability_id <- liability_id

  shiny::showModal(.pec_modal_ui())
}

# ── Modal UI ──────────────────────────────────────────────────────────────────
.pec_modal_ui <- function() {
  shiny::modalDialog(
    title     = "Confirmación de cambios",
    size      = "l",
    easyClose = FALSE,
    shiny::div(
      class = "mb-3 text-muted small",
      "El pasivo cambió. Las siguientes provisiones tienen ediciones manuales
       y NO se actualizaron automáticamente. Decide qué hacer con cada una:"
    ),
    shiny::uiOutput("pec_conflicts_table"),
    shiny::div(
      class = "mt-2 d-flex gap-2",
      shiny::actionButton("pec_all_mantener", "Aplicar todos: Mantener",
                          class = "btn btn-outline-secondary btn-sm"),
      shiny::actionButton("pec_all_nuevo",    "Aplicar todos: Nuevo valor",
                          class = "btn btn-outline-primary btn-sm")
    ),
    footer = shiny::tagList(
      shiny::actionButton("pec_cancel", "Cancelar", class = "btn btn-secondary"),
      shiny::actionButton("pec_save",   "Guardar decisiones", class = "btn btn-primary")
    )
  )
}

# ── Conflicts table ────────────────────────────────────────────────────────────
.pec_render_conflicts <- function(conflicts, replacement, decisions) {
  if (!nrow(conflicts))
    return(shiny::div(class = "text-muted small", "Sin conflictos."))

  header <- shiny::tags$thead(
    shiny::tags$tr(
      shiny::tags$th("Fecha"),
      shiny::tags$th("Antes"),
      shiny::tags$th("Nuevo (sugerido)"),
      shiny::tags$th("Acción")
    )
  )

  rows <- lapply(seq_len(nrow(conflicts)), function(i) {
    row     <- conflicts[i, , drop = FALSE]
    prov_id <- row$id[1]
    before_amt <- tryCatch(pasivos_pago_amount(row), error = function(e) NA_real_)
    moneda     <- row$moneda_pago[1] %||% "MXN"

    # Suggested new amount from replacement table
    rep_row <- if (!is.null(replacement) && nrow(replacement))
      replacement[replacement$id == prov_id, , drop = FALSE]
    else
      NULL
    new_amt <- if (!is.null(rep_row) && nrow(rep_row))
      tryCatch(pasivos_pago_amount(rep_row), error = function(e) NA_real_)
    else NA_real_

    dec <- decisions[[prov_id]] %||% "mantener"
    sel_ap <- if (dec == "aplicar") "checked" else NULL
    sel_mn <- if (dec == "mantener") "checked" else NULL
    sel_pe <- if (dec == "personalizar") "checked" else NULL

    fecha_str <- tryCatch(
      format(row$fecha_efectiva[1], "%Y-%m-%d"), error = function(e) "—")

    shiny::tags$tr(
      shiny::tags$td(fecha_str),
      shiny::tags$td(class = "text-muted",
                     if (!is.na(before_amt)) fmt_money(before_amt) else "—"),
      shiny::tags$td(class = "fw-semibold text-primary",
                     if (!is.na(new_amt)) fmt_money(new_amt) else "—"),
      shiny::tags$td(
        shiny::div(
          class = "d-flex flex-column gap-1",
          shiny::div(
            class = "form-check",
            shiny::tags$input(type = "radio",
                              class = "form-check-input",
                              id    = paste0("pec_dec_ap_", prov_id),
                              name  = paste0("pec_dec_", prov_id),
                              value = "aplicar",
                              checked = sel_ap,
                              onchange = sprintf(
                                "Shiny.setInputValue('pec_decision_%s', 'aplicar', {priority:'event'})",
                                gsub("-", "_", prov_id)
                              )),
            shiny::tags$label(class = "form-check-label small",
                              `for` = paste0("pec_dec_ap_", prov_id),
                              sprintf("Aplicar %s", if (!is.na(new_amt)) fmt_money(new_amt) else "nuevo valor"))
          ),
          shiny::div(
            class = "form-check",
            shiny::tags$input(type = "radio",
                              class = "form-check-input",
                              id    = paste0("pec_dec_mn_", prov_id),
                              name  = paste0("pec_dec_", prov_id),
                              value = "mantener",
                              checked = sel_mn,
                              onchange = sprintf(
                                "Shiny.setInputValue('pec_decision_%s', 'mantener', {priority:'event'})",
                                gsub("-", "_", prov_id)
                              )),
            shiny::tags$label(class = "form-check-label small",
                              `for` = paste0("pec_dec_mn_", prov_id), "Mantener")
          ),
          shiny::div(
            class = "form-check",
            shiny::tags$input(type = "radio",
                              class = "form-check-input",
                              id    = paste0("pec_dec_pe_", prov_id),
                              name  = paste0("pec_dec_", prov_id),
                              value = "personalizar",
                              checked = sel_pe,
                              onchange = sprintf(
                                "Shiny.setInputValue('pec_decision_%s', 'personalizar', {priority:'event'})",
                                gsub("-", "_", prov_id)
                              )),
            shiny::tags$label(class = "form-check-label small",
                              `for` = paste0("pec_dec_pe_", prov_id), "Personalizar:"),
            shiny::tags$input(
              type  = "number",
              class = "form-control form-control-sm d-inline-block",
              style = "width:120px;",
              id    = paste0("pec_custom_", gsub("-", "_", prov_id)),
              value = if (is.null(decisions[[paste0(prov_id, "_custom")]])) ""
                      else decisions[[paste0(prov_id, "_custom")]],
              oninput = sprintf(
                "Shiny.setInputValue('pec_custom_%s', this.value, {priority:'event'})",
                gsub("-", "_", prov_id)
              )
            )
          )
        )
      )
    )
  })

  shiny::tags$table(
    class = "table table-sm table-bordered small",
    header,
    shiny::tags$tbody(rows)
  )
}

# ── Apply decisions ────────────────────────────────────────────────────────────
.pec_apply_decisions <- function(conflicts, replacement, decisions, shared, user,
                                 liability_id = NA_character_) {
  # Strict read (T20): a read that fails stop()s here. It used to come back as
  # an empty table, and the session's provisions were replaced by it below.
  provs <- leer_tabla_para_escribir(S3_KEYS$pasivos_provisions, shared$effective_client_id(),
                                    .schema_pasivos_provision)
  provs_base <- provs   # CM09: the save applies only these decisions on S3 as it is now

  # Per-provision decision detail for the audit entry (2026-09-03) -- this
  # previously logged only a bare conflict count, even though the real
  # per-provision decisions (aplicar/mantener/personalizar, and the value
  # each one resolved to) were fully in scope in this loop.
  decision_log <- list()

  for (i in seq_len(nrow(conflicts))) {
    prov_id <- conflicts$id[i]
    dec     <- decisions[[prov_id]] %||% "mantener"
    idx     <- which(provs$id == prov_id)
    if (!length(idx)) next
    decision_log[[length(decision_log) + 1L]] <- list(
      provision_id = prov_id, decision = dec,
      custom_value = if (identical(dec, "personalizar"))
        as.numeric(decisions[[paste0(prov_id, "_custom")]] %||% NA) else NA_real_
    )

    if (identical(dec, "aplicar")) {
      # Clear the override, apply generated value
      rep_row <- if (!is.null(replacement) && nrow(replacement))
        replacement[replacement$id == prov_id, , drop = FALSE]
      else NULL

      if (!is.null(rep_row) && nrow(rep_row)) {
        provs$amount_pago[idx[1]]          <- rep_row$amount_pago[1]
        provs$amount_cotizado[idx[1]]      <- rep_row$amount_cotizado[1]
        provs$amount_pago_override[idx[1]] <- NA_real_
        provs$last_edited_by[idx[1]]       <- user
        provs$last_edited_at[idx[1]]       <- Sys.time()
      }

    } else if (identical(dec, "personalizar")) {
      custom_val <- as.numeric(decisions[[paste0(prov_id, "_custom")]] %||% NA)
      if (!is.na(custom_val)) {
        provs$amount_pago_override[idx[1]] <- custom_val
        provs$last_edited_by[idx[1]]       <- user
        provs$last_edited_at[idx[1]]       <- Sys.time()
      }
    }
    # "mantener" → do nothing (keep existing override)
  }

  # Regla 17: a failed save stop()s before the session's copy, the bump or the
  # audit (it used to be swallowed, and the panel said "saved" anyway).
  save_pasivos_provisions(provs, client_id = shared$effective_client_id(), base = provs_base)
  tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
  shared$pasivos_provisions_db(provs)
  bump_sync_version("pasivos_provisions_db", client_id = shared$effective_client_id())

  tryCatch(pasivos_log_audit(
    action_type = "bulk.aplicar_pagos_futuros",
    user        = user,
    target_kind = "bulk",
    target_id   = liability_id %||% NA_character_,
    after       = list(n_conflicts = nrow(conflicts), decisions = decision_log),
    notes       = sprintf("Edit-confirm: %d conflicts resolved", nrow(conflicts)),
    client_id   = shared$effective_client_id()
  ), error = function(e) NULL)
}

# ── Setup ─────────────────────────────────────────────────────────────────────
setup_pasivos_edit_confirm <- function(input, output, session, shared) {

  # This session's open panel: its conflicts, their generated rows and the
  # liability (pasivos_edit_confirm_open() fills it).
  st <- shiny::reactiveValues(conflicts = NULL, replacement = NULL, liability_id = NULL)
  session$userData$pec_state <- st

  # Per-provision decision tracking: named list prov_id -> "aplicar"|"mantener"|"personalizar"
  decisions <- shiny::reactiveValues()

  # Render conflicts table
  output$pec_conflicts_table <- shiny::renderUI({
    conf <- st$conflicts
    repl <- st$replacement
    if (is.null(conf) || !nrow(conf)) return(NULL)

    # Collect current decision values from the decisions reactiveValues
    dec_list <- as.list(decisions)
    .pec_render_conflicts(conf, repl, dec_list)
  })

  # Observe per-row decision inputs (JS fires pec_decision_<uuid_with_underscores>)
  # We use a broad observe that checks all conflicts
  shiny::observe({
    conf <- st$conflicts
    if (is.null(conf) || !nrow(conf)) return()
    for (i in seq_len(nrow(conf))) {
      prov_id <- conf$id[i]
      key     <- gsub("-", "_", prov_id)
      dec_val <- input[[paste0("pec_decision_", key)]]
      if (!is.null(dec_val)) decisions[[prov_id]] <- dec_val
      cust_val <- input[[paste0("pec_custom_", key)]]
      if (!is.null(cust_val)) decisions[[paste0(prov_id, "_custom")]] <- cust_val
    }
  })

  # Mass: mantener all
  shiny::observeEvent(input$pec_all_mantener, ignoreInit = TRUE, {
    conf <- st$conflicts
    if (is.null(conf) || !nrow(conf)) return()
    for (i in seq_len(nrow(conf))) decisions[[conf$id[i]]] <- "mantener"
  })

  # Mass: apply new value for all
  shiny::observeEvent(input$pec_all_nuevo, ignoreInit = TRUE, {
    conf <- st$conflicts
    if (is.null(conf) || !nrow(conf)) return()
    for (i in seq_len(nrow(conf))) decisions[[conf$id[i]]] <- "aplicar"
  })

  # Save decisions
  shiny::observeEvent(input$pec_save, ignoreInit = TRUE, {
    if (!pasivos_autoriza("pasivos.edit_liability")) return()
    conf <- st$conflicts
    repl <- st$replacement
    if (is.null(conf) || !nrow(conf)) { shiny::removeModal(); return() }

    user <- tryCatch(shared$current_user(), error = function(e) "system")
    dec_list <- as.list(decisions)
    # Default missing decisions to "mantener"
    for (i in seq_len(nrow(conf))) {
      pid <- conf$id[i]
      if (is.null(dec_list[[pid]])) dec_list[[pid]] <- "mantener"
    }

    # Saved, or nothing: on a failed read or write the panel stays open with
    # the choices as they are, so the person can try again (regla 17).
    guardado <- tryCatch({
      .pec_apply_decisions(conf, repl, dec_list, shared, user, liability_id = st$liability_id)
      TRUE
    }, error = function(e) {
      message("pasivos_edit_confirm: decisions not saved: ", conditionMessage(e))
      shiny::showNotification(tr("tablas.pasivos.conflictos.no_guardado"), type = "error", duration = 8)
      FALSE
    })
    if (!guardado) return()
    shiny::removeModal()
    shiny::showNotification("Decisiones guardadas.", type = "message", duration = 3)
  })

  # Cancel: confirm and default all to "mantener" (no writes)
  shiny::observeEvent(input$pec_cancel, ignoreInit = TRUE, {
    shiny::showModal(shiny::modalDialog(
      "¿Cancelar? Las provisiones marcadas se mantendrán con sus valores actuales.",
      footer = shiny::tagList(
        shiny::actionButton("pec_cancel_no",  "Continuar revisando", class = "btn btn-secondary"),
        shiny::actionButton("pec_cancel_yes", "Sí, mantener todo",   class = "btn btn-danger")
      ),
      size = "s"
    ))
  })

  shiny::observeEvent(input$pec_cancel_no, ignoreInit = TRUE, {
    shiny::removeModal()
    shiny::showModal(.pec_modal_ui())
  })

  shiny::observeEvent(input$pec_cancel_yes, ignoreInit = TRUE, {
    shiny::removeModal()
    shiny::showNotification("Cambios cancelados. Las provisiones mantienen sus valores.",
                            type = "message", duration = 3)
  })

  invisible(NULL)
}
