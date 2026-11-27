# =============================================================================
# R/pasivos_wizard_module.R
# Stage 4: Multi-step wizard for creating and editing liabilities.
#
# Entry point: setup_pasivos_wizard(input, output, session, shared)
# Called once from app.R server. All wizard inputs use "pwiz_" prefix.
# Global inputs that trigger the wizard:
#   input$pasivos_add_liability  — fired by "+ Agregar pasivo" button
#   input$pasivos_edit_liability — fired by pencil icon (value = liability_id)
# =============================================================================

# ── Draft ↔ Liability conversion ──────────────────────────────────────────────

# Convert a single liability row (tibble) to a draft list for pre-filling.
pasivos_liability_to_draft <- function(row) {
  r <- row[1, , drop = FALSE]
  rp <- r$recurrence_params[[1]] %||% list()
  list(
    categoria              = r$categoria[1] %||% "",
    flavor                 = r$flavor[1] %||% "",
    nombre                 = r$nombre[1] %||% "",
    objeto                 = .pasivos_objeto_texto(if ("objeto" %in% names(r)) r$objeto[1] else NA),   # T105
    empresa                = r$empresa[1] %||% "",
    parte                  = r$parte[1] %||% "",
    codigo_parte           = r$codigo_parte[1] %||% "",
    referencia_default     = r$referencia_default[1] %||% "",
    documento_template     = r$documento_template[1] %||% "",
    subcategoria           = r$subcategoria[1] %||% "",
    moneda_pago            = r$moneda_pago[1] %||% NA_character_,   # the selector falls on the base
    cotizado_en            = r$cotizado_en[1] %||% "",
    tarjeta_closing_day    = r$tarjeta_closing_day[1],
    tarjeta_due_day        = r$tarjeta_due_day[1],
    tarjeta_credit_limit   = r$tarjeta_credit_limit[1],
    recurrence_type        = r$recurrence_type[1] %||% "",
    recurrence_params      = rp,
    fecha_inicio           = r$fecha_inicio[1],
    amount_default         = r$amount_default[1],
    amount_default_cot     = r$amount_default_cot[1],
    tarjeta_provision_source = r$tarjeta_provision_source[1] %||% "",
    principal_original     = r$principal_original[1],
    saldo_capital          = r$saldo_capital[1],
    tasa_actual            = r$tasa_actual[1],
    tasa_tipo              = r$tasa_tipo[1] %||% "fija",
    tasa_spread            = r$tasa_spread[1] %||% 0,
    convencion_dias        = pasivos_convencion(r),   # T115: its own, or the one it had before (same figures)
    tasa_estimate_method   = r$tasa_estimate_method[1] %||% "",
    plazo_meses            = r$plazo_meses[1],
    dia_pago               = r$dia_pago[1],
    frecuencia_pago        = r$frecuencia_pago[1] %||% "mensual",
    mes_pago               = as.integer(r$mes_pago[1] %||% NA_integer_),
    periodo_gracia         = r$periodo_gracia[1] %||% 0L,
    monto_gracia           = r$monto_gracia[1],
    metodo_amortizacion    = r$metodo_amortizacion[1] %||% "francesa",
    schedule_csv_raw       = "",
    cargos_iniciales       = r$cargos_iniciales[[1]],
    impuestos              = if ("impuestos" %in% names(r)) r$impuestos[[1]] else NULL,   # T63
    valor_residual         = r$valor_residual[[1]],
    notas                  = r$notas[1] %||% "",
    # T70: a regular or tarjeta liability's end date (NA = no end)
    fecha_fin              = if ((r$categoria[1] %||% "") %in% c("regular", "tarjeta")) pasivos_fecha_fin(r) else as.Date(NA)
  )
}

# Build recurrence_params list from wizard input values.
.wiz_build_recurrence_params <- function(input, rt) {
  switch(rt,
    monthly_day        = list(day = as.integer(input$pwiz_rp_day %||% 1L)),
    monthly_nth_weekday = list(
      nth  = as.integer(input$pwiz_rp_nth  %||% 1L),
      wday = as.integer(input$pwiz_rp_wday %||% 1L)
    ),
    biweekly           = list(
      day1 = as.integer(input$pwiz_rp_day1 %||% 15L),
      day2 = as.integer(input$pwiz_rp_day2 %||% 31L)
    ),
    weekly             = list(wday = as.integer(input$pwiz_rp_wday %||% 1L)),
    interval_days      = list(
      anchor_date   = as.Date(input$pwiz_rp_anchor %||% hoy_usuario()),
      interval_days = as.integer(input$pwiz_rp_interval_days %||% 30L)
    ),
    quarterly          = list(
      anchor_month = as.integer(input$pwiz_rp_anchor_month %||% 1L),
      day          = as.integer(input$pwiz_rp_day %||% 1L)
    ),
    bimestral          = list(
      anchor_month = as.integer(input$pwiz_rp_anchor_month %||% 1L),
      day          = as.integer(input$pwiz_rp_day %||% 1L)
    ),
    yearly             = list(
      month = as.integer(input$pwiz_rp_month %||% 1L),
      day   = as.integer(input$pwiz_rp_day   %||% 1L)
    ),
    custom             = list(dates = as.Date(character(0))),  # dates managed via wiz$custom_dates
    list()
  )
}

# Convert draft to a single-row tibble matching .schema_pasivos_liability().
pasivos_wizard_draft_to_liability <- function(draft, mode = "create",
                                               edit_target_id = NULL, user = "system",
                                               previo = NULL, base = MONEDA_BASE_OMISION) {
  now <- Sys.time()
  # Editing keeps what the wizard does not ask for: the liability's estado
  # and who created it, when. It used to write "active" and the editor as
  # creator, so editing a paused or archived liability reactivated it
  # without a word (found 2026-11-04, T18a).
  edita <- identical(mode, "edit") && !is.null(previo) && nrow(previo) > 0L
  estado_previo <- if (edita) previo$estado[1] else NA_character_
  if (is.na(estado_previo %||% NA_character_)) estado_previo <- "active"
  id  <- if (mode == "edit" && !is.null(edit_target_id)) edit_target_id
         else uuid::UUIDgenerate()

  cat <- draft$categoria %||% "regular"
  fi  <- tryCatch(as.Date(draft$fecha_inicio %||% NA), error = function(e) as.Date(NA))

  # Compute fecha_vence for financiero: total periods = amortization + grace, plus 32-day
  # buffer so the last payment (snapped to dia_pago) always falls within the window.
  fv <- if (identical(cat, "financiero") && !is.na(fi)) {
    tryCatch(
      fi + months(as.integer((draft$plazo_meses    %||% 0L) +
                             (draft$periodo_gracia %||% 0L))) + 32L,
      error = function(e) as.Date(NA)
    )
  } else if (cat %in% c("regular", "tarjeta")) {
    # T70: the optional end date the user set (NA = no end)
    tryCatch(as.Date(draft$fecha_fin %||% NA), error = function(e) as.Date(NA))
  } else as.Date(NA)

  # schedule_imported from CSV raw if custom amortization
  sched_imp <- list(NULL)
  if (identical(cat, "financiero") &&
      identical(draft$metodo_amortizacion %||% "", "custom")) {
    csv_raw <- draft$schedule_csv_raw %||% ""
    if (nzchar(trimws(csv_raw))) {
      parsed <- tryCatch(
        utils::read.csv(text = csv_raw, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(parsed)) sched_imp <- list(parsed)
    }
  }

  cargos  <- if (is.null(draft$cargos_iniciales)) list(NULL) else list(draft$cargos_iniciales)
  imps    <- if (!length(draft$impuestos)) list(NULL) else list(draft$impuestos)   # T63
  residual <- if (is.null(draft$valor_residual) ||
                  (!is.null(draft$valor_residual) && length(draft$valor_residual) == 0))
               list(NULL)
              else
               list(draft$valor_residual)

  rp <- draft$recurrence_params %||% list()
  if (!is.list(rp)) rp <- list()

  tibble::tibble(
    id                       = id,
    categoria                = cat,
    subcategoria             = draft$subcategoria %||% NA_character_,
    flavor                   = if (identical(cat, "financiero")) (draft$flavor %||% "credito_simple") else NA_character_,
    nombre                   = draft$nombre %||% "",
    objeto                   = pasivos_objeto_limpio(draft$objeto),   # T105
    empresa                  = draft$empresa %||% "",
    parte                    = draft$parte %||% "",
    codigo_parte             = draft$codigo_parte %||% NA_character_,
    referencia_default       = if (nzchar(draft$referencia_default %||% "")) draft$referencia_default else NA_character_,
    documento_template       = if (nzchar(draft$documento_template %||% "")) draft$documento_template else NA_character_,
    moneda_pago              = draft$moneda_pago %||% base,
    cotizado_en              = if (nzchar(draft$cotizado_en %||% "")) draft$cotizado_en else NA_character_,
    recurrence_type          = if (cat %in% c("regular", "tarjeta"))
                                 (draft$recurrence_type %||% NA_character_) else NA_character_,
    recurrence_params        = list(rp),
    amount_default           = if (cat %in% c("regular", "tarjeta"))
                                 (draft$amount_default %||% NA_real_) else NA_real_,
    amount_default_cot       = if (cat %in% c("regular", "tarjeta"))
                                 (draft$amount_default_cot %||% NA_real_) else NA_real_,
    tarjeta_provision_source = if (identical(cat, "tarjeta"))
                                 (draft$tarjeta_provision_source %||% NA_character_) else NA_character_,
    tarjeta_closing_day      = if (identical(cat, "tarjeta"))
                                 as.integer(draft$tarjeta_closing_day %||% NA_integer_) else NA_integer_,
    tarjeta_due_day          = if (identical(cat, "tarjeta"))
                                 as.integer(draft$tarjeta_due_day %||% NA_integer_) else NA_integer_,
    tarjeta_credit_limit     = if (identical(cat, "tarjeta"))
                                 (draft$tarjeta_credit_limit %||% NA_real_) else NA_real_,
    principal_original       = if (identical(cat, "financiero"))
                                 (draft$principal_original %||% NA_real_) else NA_real_,
    saldo_capital            = if (identical(cat, "financiero"))
                                 (draft$saldo_capital %||% NA_real_) else NA_real_,
    tasa_actual              = if (identical(cat, "financiero"))
                                 (draft$tasa_actual %||% NA_real_) else NA_real_,
    tasa_tipo                = if (identical(cat, "financiero"))
                                 (draft$tasa_tipo %||% "fija") else NA_character_,
    tasa_spread              = if (identical(cat, "financiero"))
                                 (draft$tasa_spread %||% 0) else NA_real_,
    convencion_dias          = if (identical(cat, "financiero") && pasivos_convencion_valida(draft$convencion_dias))
                                 draft$convencion_dias else NA_character_,   # T115
    tasa_estimate_method     = draft$tasa_estimate_method %||% NA_character_,
    fecha_inicio             = fi,
    fecha_vence              = fv,
    plazo_meses              = if (identical(cat, "financiero"))
                                 as.integer(draft$plazo_meses %||% NA_integer_) else NA_integer_,
    dia_pago                 = if (identical(cat, "financiero"))
                                 as.integer(draft$dia_pago %||% NA_integer_) else NA_integer_,
    frecuencia_pago          = if (identical(cat, "financiero"))
                                 (draft$frecuencia_pago %||% "mensual") else NA_character_,
    mes_pago                 = if (identical(cat, "financiero"))
                                 as.integer(draft$mes_pago %||% NA_integer_) else NA_integer_,
    periodo_gracia           = if (identical(cat, "financiero"))
                                 as.integer(draft$periodo_gracia %||% 0L) else NA_integer_,
    monto_gracia             = if (identical(cat, "financiero"))
                                 (draft$monto_gracia %||% NA_real_) else NA_real_,
    metodo_amortizacion      = if (identical(cat, "financiero"))
                                 (draft$metodo_amortizacion %||% "francesa") else NA_character_,
    schedule_imported        = sched_imp,
    cargos_iniciales         = cargos,
    impuestos                = imps,
    valor_residual           = residual,
    modifier_ids             = list(character(0)),
    estado                   = estado_previo,
    notas                    = draft$notas %||% NA_character_,
    created_by               = if (edita) previo$created_by[1] else user,
    created_at               = if (edita) previo$created_at[1] else now,
    updated_by               = user,
    updated_at               = now
  )
}

# The generated row each conflict would become, under the conflict's own id,
# matched by (liability_id, occurrence_index): what "Aplicar" writes. The
# panel used to get result$update, which never holds a conflict, so "Nuevo
# (sugerido)" showed "—" and "Aplicar" changed nothing (T18a).
pasivos_conflictos_reemplazo <- function(conflicts, generated) {
  if (is.null(generated) || !nrow(generated)) return(generated)
  if (is.null(conflicts) || !nrow(conflicts)) return(generated[0L, , drop = FALSE])
  occ <- function(df) paste(df$liability_id, df$occurrence_index, sep = "\r")
  m   <- match(occ(conflicts), occ(generated))
  rep <- generated[m[!is.na(m)], , drop = FALSE]
  rep$id <- conflicts$id[!is.na(m)]
  rep
}

# The wizard's reconciliation, never losing a row. When
# pasivos_reconcile_provisions() failed, the save used to keep NONE of this
# liability's provisions and insert every generated one: it dropped the paid,
# converted and hand-edited rows from S3 (found 2026-11-22, PV-1). Now they
# stay exactly as they are, nothing is inserted, and the user hears it
# (saving again retries). `...` goes to pasivos_reconcile_provisions().
pasivos_reconciliar_o_conservar <- function(existing_for_this, generated, ...) {
  tryCatch(
    pasivos_reconcile_provisions(existing_for_this, generated, ...),
    error = function(e) {
      message("[wizard] pasivos_reconcile_provisions failed: ", conditionMessage(e))
      tryCatch(shiny::showNotification(tr("tablas.pasivos.error_generar", detalle = conditionMessage(e)),
                                       type = "error", duration = 15), error = function(e2) NULL)
      list(keep = existing_for_this,
           update = existing_for_this[0L, , drop = FALSE],
           insert = generated[0L, , drop = FALSE],
           conflicts = existing_for_this[0L, , drop = FALSE],
           error = conditionMessage(e))
    }
  )
}

# ── Save handler ───────────────────────────────────────────────────────────────
# T70: after the save, each provision the end date removed goes to the
# Papelera (as a provision deleted by hand) and is logged as provision.deleted.
# The provisions themselves are already saved as "deleted": a failure here is
# reported in the log and never undoes the save.
.pasivos_fin_a_papelera <- function(filas, liab_row, user, shared) {
  cid <- shared$effective_client_id()
  tryCatch({
    pap <- tryCatch(load_papelera(client_id = cid), error = function(e) .schema_papelera())
    det <- data.frame(
      id        = filas$id,
      source    = "provision",
      Empresa   = filas$empresa %||% "",
      Moneda    = filas$moneda_pago %||% NA_character_,
      Documento = filas$documento %||% "",
      Parte     = filas$parte %||% "",
      Importe   = ifelse(!is.na(filas$amount_pago_override), filas$amount_pago_override, filas$amount_pago),
      FechaEff  = filas$fecha_efectiva,
      stringsAsFactors = FALSE)
    pap <- add_to_papelera(pap, det, ledger = "AP", deleted_by = user)
    save_papelera(pap, client_id = cid)
    if (!is.null(shared$papelera_rv)) shared$papelera_rv(pap)
  }, error = function(e) {
    message("[wizard] T70 papelera write failed: ", conditionMessage(e))
    # T74: seen by the user, not only the server log.
    tryCatch(shiny::showNotification(tr("comun.error_guardar"), type = "error", duration = NULL),
             error = function(e2) NULL)
  })
  for (i in seq_len(nrow(filas)))
    tryCatch(pasivos_log_audit(
      action_type = "provision.deleted",
      user        = user,
      empresa     = liab_row$empresa,
      target_kind = "provision", target_id = filas$id[i],
      before      = as.list(filas[i, , drop = FALSE]),
      notes       = sprintf("T70: after the liability's end date %s; archived to papelera", format(pasivos_fecha_fin(liab_row))),
      client_id   = cid
    ), error = function(e) NULL)
  invisible(nrow(filas))
}

.pasivos_wizard_save <- function(wiz, shared, session, user) {
  empresa_choices <- tryCatch(sort(names(shared$company_map())),
                              error = function(e) character(0))
  validation <- pasivos_wizard_validate_final(wiz$draft, empresa_choices)
  if (!validation$ok) {
    wiz$validation_errors <- validation$errors
    return(list(ok = FALSE, msg = tr("tablas.pasivos.hay_errores")))
  }

  # The liability is saved on S3's table as it is now (T22): the row the edit
  # keeps estado and creator from (and the audit's `before`) is S3's, and only
  # this row is replaced or added. It used to write the session's whole copy:
  # a liability another session had just changed went back to what this
  # session held. A failed read or write saves nothing and touches nothing.
  cid        <- shared$effective_client_id()
  before_row <- NULL
  liab_row   <- NULL
  motivo     <- NULL   # why the row was not built: "borrado" or the builder's error
  # T108: the companies' map of earlier initials (CM29) is read once for both
  # writes of this save, never kept for another. If it cannot be read here,
  # each write reads it itself, as always (and stops if that fails).
  mapa <- tryCatch(iniciales_mapa_vigente(cid), error = function(e) NULL)
  liabs <- tryCatch(modificar_pasivos_liabilities(cid, mapa = mapa, cambio = function(fresco) {
    if (wiz$mode == "edit")
      before_row <<- fresco[!is.na(fresco$id) & fresco$id == (wiz$edit_target_id %||% ""), , drop = FALSE]
    # An archived liability is restored from the Papelera, never by editing it.
    if (!is.null(before_row) && nrow(before_row) && identical(before_row$estado[1], "deleted")) {
      motivo <<- "borrado"; return(fresco)
    }
    fila <- tryCatch(
      pasivos_wizard_draft_to_liability(
        wiz$draft,
        mode           = wiz$mode,
        edit_target_id = wiz$edit_target_id,
        user           = user,
        previo         = before_row,
        base           = tryCatch(shared$moneda_base_cliente()$moneda, error = function(e) NULL) %||%
                           MONEDA_BASE_OMISION
      ),
      error = function(e) e
    )
    if (inherits(fila, "error")) { motivo <<- fila; return(fresco) }
    liab_row <<- fila
    # T99: %in%, never NA. With `!=`, a row whose id is NA became an all-NA
    # row on every edit of any other liability.
    if (wiz$mode == "edit") fresco <- fresco[!(fresco$id %in% fila$id), , drop = FALSE]
    dplyr::bind_rows(fresco, fila)
  }), error = function(e) e)
  # T72: every exit of this save is in the log, also the ones that save
  # nothing (what was tried, and why it did not happen).
  .wiz_log <- function(tipo, notas, after = liab_row) tryCatch(pasivos_log_audit(
    action_type = tipo, user = user,
    empresa     = (after %||% before_row %||% list(empresa = NA_character_))$empresa[1] %||% NA_character_,
    target_kind = "liability",
    target_id   = (after$id %||% wiz$edit_target_id) %||% NA_character_,
    before      = if (!is.null(before_row) && nrow(before_row)) as.list(before_row) else NULL,
    after       = if (!is.null(after)) as.list(after) else NULL,
    notes       = notas, client_id = cid), error = function(e) NULL)
  if (inherits(liabs, "error")) {
    message("[wizard] liability not saved: ", conditionMessage(liabs))
    .wiz_log("liability.save_failed", paste0(wiz$mode, ": ", conditionMessage(liabs)))
    return(list(ok = FALSE, msg = tr("tablas.pasivos.no_guardado")))
  }
  if (identical(motivo, "borrado")) {
    shared$pasivos_liabilities_db(liabs)
    .wiz_log("liability.save_refused", "edit of a liability already in the Papelera (another session)")
    return(list(ok = FALSE, msg = tr("tablas.pasivos.no_editar_borrado")))
  }
  if (inherits(motivo, "error")) {
    .wiz_log("liability.save_refused", paste0(wiz$mode, ", the row could not be built: ", conditionMessage(motivo)))
    return(list(ok = FALSE, msg = tr("tablas.pasivos.error_construir", detalle = conditionMessage(motivo))))
  }
  shared$pasivos_liabilities_db(liabs)
  # T108: the liabilities are announced with the provisions, in ONE bump, at
  # the end. Until then their bump is pending: any other way out of this
  # save -- no provisions, a generation error, provisions that failed to
  # save, an error -- announces the liabilities alone. A table written is
  # never left without its bump.
  bump_liabs_pendiente <- TRUE
  on.exit(if (bump_liabs_pendiente) bump_sync_version("pasivos_liabilities_db", client_id = cid), add = TRUE)

  # Record current fetched base rate in the forecasting store so provisions use it
  if (startsWith(liab_row$tasa_tipo %||% "fija", "variable_") &&
      !is.na(liab_row$tasa_actual %||% NA_real_)) {
    metric_map <- c(variable_sofr = "sofr", variable_tiie28 = "tiie28", variable_prime = "prime")
    fc_metric  <- metric_map[[liab_row$tasa_tipo %||% ""]]
    if (!is.null(fc_metric))
      tryCatch(
        forecasting_set_estimate(fc_metric, Sys.Date(), liab_row$tasa_actual,
                                 source_method = "wizard_save", user = user,
                                 client_id = shared$effective_client_id()),
        # A failed read now saves nothing (T26): say so, not only in the log.
        error = function(e) {
          warning("[wizard] forecasting_set_estimate failed: ", conditionMessage(e))
          showNotification(tr("tablas.pasivos.tasa_no_guardada", error = conditionMessage(e)), type = "warning")
        }
      )
  }

  # A paused liability keeps no future provisions (pausing removed them), so
  # editing one saves its terms only; resuming and editing it again generates
  # them, as the resume notice says (T18a).
  if (!identical(liab_row$estado, "active")) {
    tryCatch(pasivos_log_audit(
      action_type = "liability.edited",
      user        = user,
      empresa     = liab_row$empresa,
      target_kind = "liability",
      target_id   = liab_row$id,
      before      = if (!is.null(before_row) && nrow(before_row)) as.list(before_row) else NULL,
      after       = as.list(liab_row),
      notes       = sprintf("estado %s: no provisions generated", liab_row$estado),
      client_id   = shared$effective_client_id()
    ), error = function(e) NULL)
    shiny::removeModal()
    shiny::showNotification(tr("tablas.pasivos.guardado_sin_provisiones"),
                            type = "message", duration = 5)
    return(list(ok = TRUE))
  }

  # Generate provisions
  # T116: the cache is country -> holidays (festivos_cache()), built from the
  # same policies passed below and the staff's overrides -- never the
  # overrides table itself.
  politicas_pasivo <- list()
  holidays <- festivos_cache(politicas_pasivo,
                             tryCatch(shared$holiday_overrides_db(), error = function(e) NULL))

  gen_err <- character(0)
  generated <- tryCatch(
    pasivos_generate_provisions(
      liability              = liab_row,
      window_start           = hoy_cliente(shared),   # T75b: the client's day
      client_id              = tryCatch(shared$effective_client_id(), error = function(e) NULL),
      window_end             = NULL,
      policies_for_liability = politicas_pasivo,
      holidays_cache         = holidays
    ),
    error = function(e) {
      gen_err <<- conditionMessage(e)
      message("[wizard] pasivos_generate_provisions failed: ", gen_err)
      .schema_pasivos_provision()
    }
  )
  # T70: an end date can leave nothing new to generate; that is not an error.
  sin_filas_por_fin <- !nrow(generated) && !length(gen_err) && !is.na(pasivos_fecha_fin(liab_row)) &&
    liab_row$categoria %in% c("regular", "tarjeta")
  if (!nrow(generated) && !sin_filas_por_fin) {
    err_detail <- if (length(gen_err)) gen_err else tr("tablas.pasivos.sin_filas_generadas")
    # T72: the liability IS saved here (above); it left no trace when its
    # provisions came out empty.
    .wiz_log(if (wiz$mode == "create") "liability.created" else "liability.edited",
             paste0("no provisions generated: ", err_detail))
    # A liability created with 0 provisions is still a liability created:
    # the team hears it like any other (the Coordinador, T72). Never throws.
    if (wiz$mode == "create") avisos_actividad(shared$effective_client_id(), "crear_pasivo", user, list(cuerpo = liab_row$nombre %||% liab_row$empresa), al_responder = session)
    shiny::showNotification(
      tr("tablas.pasivos.error_generar", detalle = err_detail),
      type = "error", duration = 15
    )
    return(list(ok = TRUE))
  }

  # Reconcile
  existing <- tryCatch(shared$pasivos_provisions_db(),
                       error = function(e) .schema_pasivos_provision())
  existing_for_this <- existing[
    !is.na(existing$liability_id) & existing$liability_id == liab_row$id,
    , drop = FALSE
  ]
  reco_mode <- if (wiz$mode == "create") "extend" else "regenerate"
  # T71: a provision with the liability's previous note takes the new one
  nota_anterior <- if (!is.null(before_row) && nrow(before_row)) pasivos_nota_de(before_row) else NA_character_
  # PV-1: a regular / tarjeta liability's rows are matched by date, from the
  # terms they were generated with (before_row) to the new ones.
  result <- pasivos_reconciliar_o_conservar(
    existing_for_this, generated, mode = reco_mode, nota_anterior = nota_anterior,
    liability = liab_row,
    liability_antes = if (!is.null(before_row) && nrow(before_row)) before_row else liab_row)

  # Apply: keep + conflicts + update + insert. A conflict (a provision edited
  # by hand that the new terms would change) stays exactly as it is until the
  # user decides in the edit-confirm panel. It used to be left out here, so
  # this save removed it from S3 before the panel opened and no decision,
  # "Mantener" included, could find it again (found 2026-11-04, T18a).
  others <- existing[
    is.na(existing$liability_id) | existing$liability_id != liab_row$id,
    , drop = FALSE
  ]
  new_provs <- dplyr::bind_rows(others, result$keep, result$conflicts,
                                result$update, result$insert)
  # T70: the pending provisions after the end date go to the Papelera, in the
  # same write (estado "deleted", as deleting one by hand does); the Revisión
  # said how many before saving.
  tras_fin <- pasivos_provisiones_tras_fin(new_provs, liab_row)
  if (nrow(tras_fin)) {
    k <- new_provs$id %in% tras_fin$id
    new_provs$estado[k]         <- "deleted"
    new_provs$last_edited_by[k] <- user
    new_provs$last_edited_at[k] <- Sys.time()
  }
  # Regla 17 (T22): this error was only a warning, and the session, the bump
  # and "Pasivo guardado. N provisiones generadas." went on as saved. The
  # liability is saved; its provisions are not, and it says so (editing it
  # again generates them).
  # T108: the session keeps what was written (the rebase, as a load returns
  # it), so its own sync bump finds nothing new and does not recompute.
  escritas <- NULL
  prov_ok <- tryCatch({
    escritas <- save_pasivos_provisions(new_provs, client_id = shared$effective_client_id(), base = existing, mapa = mapa)
    TRUE
  }, error = function(e) {
    message("[wizard] save_pasivos_provisions failed: ", conditionMessage(e))
    FALSE
  })
  if (!prov_ok) {
    tryCatch(pasivos_log_audit(
      action_type = if (wiz$mode == "create") "liability.created" else "liability.edited",
      user        = user,
      empresa     = liab_row$empresa,
      target_kind = "liability",
      target_id   = liab_row$id,
      before      = if (!is.null(before_row) && nrow(before_row)) as.list(before_row) else NULL,
      after       = as.list(liab_row),
      notes       = "provisions not saved (S3 failed)",
      client_id   = shared$effective_client_id()
    ), error = function(e) NULL)
    # C (P5): the liability itself is saved, so it is announced; never throws.
    if (wiz$mode == "create") avisos_actividad(shared$effective_client_id(), "crear_pasivo", user, list(cuerpo = liab_row$nombre %||% liab_row$empresa), al_responder = session)
    shiny::removeModal()
    shiny::showNotification(tr("tablas.pasivos.provisiones_no_guardadas"), type = "error", duration = 15)
    return(list(ok = TRUE))
  }
  tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
  shared$pasivos_provisions_db(if (is.data.frame(escritas)) escritas else new_provs)
  bump_liabs_pendiente <- FALSE
  bump_sync_version(c("pasivos_liabilities_db", "pasivos_provisions_db"), client_id = cid)
  if (nrow(tras_fin)) .pasivos_fin_a_papelera(tras_fin, liab_row, user, shared)   # T70

  # Audit -- previously captured only 5 identity columns (id/categoria/
  # nombre/empresa/estado), never the actual payment terms (recurrence,
  # amount, rate, schedule) a treasurer sets in the wizard, and never a
  # `before` on edit at all (found 2026-09-03 while extending Bancos/Pasivos
  # activity logging). Full row, same as_list(row) convention already used
  # by every pasivos_engine.R audit call.
  tryCatch(pasivos_log_audit(
    action_type = if (wiz$mode == "create") "liability.created" else "liability.edited",
    user        = user,
    empresa     = liab_row$empresa,
    target_kind = "liability",
    target_id   = liab_row$id,
    before      = if (!is.null(before_row) && nrow(before_row)) as.list(before_row) else NULL,
    after       = as.list(liab_row),
    notes       = sprintf("Generated %d, updated %d, conflicts %d",
                          nrow(result$insert), nrow(result$update),
                          nrow(result$conflicts)),
    client_id   = shared$effective_client_id()
  ), error = function(e) NULL)
  # C (Alertas del equipo): only on create, after the full save; never throws.
  if (wiz$mode == "create") avisos_actividad(shared$effective_client_id(), "crear_pasivo", user, list(cuerpo = liab_row$nombre %||% liab_row$empresa), al_responder = session)

  shiny::removeModal()

  conflicts <- result$conflicts
  n_ins <- nrow(result$insert) + nrow(result$update)
  if (nrow(conflicts)) {
    shiny::showNotification(
      tr("tablas.pasivos.guardado_conflictos", n = nrow(conflicts)),
      type = "warning", duration = 5
    )
    pasivos_edit_confirm_open(
      session     = session,
      conflicts   = conflicts,
      replacement = pasivos_conflictos_reemplazo(conflicts, generated),
      shared      = shared,
      liability_id = liab_row$id
    )
  } else {
    shiny::showNotification(
      tr("tablas.pasivos.guardado_generadas", n = n_ins),
      type = "message", duration = 3
    )
  }

  list(ok = TRUE)
}

# T70/T71: the Revisión's notice when editing, before saving: how many pending
# provisions an earlier end date sends to the Papelera, how many converted
# payments after it stay, and how many provisions take a new note (those with
# a note of their own keep it). NULL when there is nothing to say.
pasivos_aviso_edicion <- function(draft, liability_id, provs, liabs = NULL) {
  if (is.null(provs) || !nrow(provs) || is.null(liability_id)) return(list())
  mias <- provs[!is.na(provs$liability_id) & provs$liability_id == liability_id, , drop = FALSE]
  out <- list()
  liab <- list(id = liability_id, categoria = draft$categoria %||% "",
               fecha_vence = tryCatch(as.Date(draft$fecha_fin %||% NA), error = function(e) as.Date(NA)))
  liab <- as.data.frame(liab, stringsAsFactors = FALSE)
  n_fin <- nrow(pasivos_provisiones_tras_fin(mias, liab))
  if (n_fin) out$papelera <- n_fin
  fin <- pasivos_fecha_fin(liab)
  if (!is.na(fin) && liab$categoria %in% c("regular", "tarjeta")) {
    f <- as.Date(mias$fecha_efectiva)
    n_conv <- sum(mias$estado %in% c("converted", "item_confirmed") & !is.na(f) & f > fin)
    if (n_conv) out$ya_convertidas <- n_conv
  }
  antes <- if (!is.null(liabs) && nrow(liabs)) liabs[!is.na(liabs$id) & liabs$id == liability_id, , drop = FALSE] else NULL
  nota_ant <- if (!is.null(antes) && nrow(antes)) pasivos_nota_de(antes) else NA_character_
  nota_nueva <- pasivos_nota_de(data.frame(notas = draft$notas %||% NA_character_, stringsAsFactors = FALSE))
  if (!identical(nota_ant, nota_nueva)) {
    pend <- mias[mias$estado %in% "provisional", , drop = FALSE]
    propias <- vapply(pend$notas %||% rep(NA_character_, nrow(pend)), .nota_propia, logical(1), nota_anterior = nota_ant)
    if (nrow(pend)) out$nota <- c(n = sum(!propias), m = sum(propias))
  }
  out
}
pasivos_aviso_edicion_html <- function(draft, liability_id, provs, liabs = NULL) {
  a <- pasivos_aviso_edicion(draft, liability_id, provs, liabs)
  if (!length(a)) return(NULL)
  shiny::div(class = "alert alert-warning py-2 px-3 mt-2 mb-0 small",
    if (!is.null(a$papelera)) shiny::div(shiny::tags$strong(tr("tablas.pasivos.fin.papelera", n = a$papelera))),
    if (!is.null(a$ya_convertidas)) shiny::div(tr("tablas.pasivos.fin.ya_convertidas", n = a$ya_convertidas)),
    if (!is.null(a$nota)) shiny::div(tr("tablas.pasivos.nota.cambio", n = a$nota[["n"]], m = a$nota[["m"]])))
}

# T70b: what saving will generate, as the engine does it: a financiero up to
# the end of its term, a regular or tarjeta for the next 24 months or up to
# its end date when that comes first. It used to say "24 meses" for all.
pasivos_aviso_generacion <- function(draft, hoy = hoy_en()) {   # the client's day (R/zona.R), never the server's
  cat <- draft$categoria %||% ""
  fin <- tryCatch(as.Date(draft$fecha_fin %||% NA), error = function(e) as.Date(NA))
  tope <- as.Date(hoy) + lubridate::years(2)
  primera <- if (identical(cat, "financiero")) tr("tablas.pasivos.revision.genera_plazo")
    else if (cat %in% c("regular", "tarjeta") && !is.na(fin) && fin < tope)
      tr("tablas.pasivos.revision.genera_hasta_fin", fecha = format(fin, "%d/%m/%Y"))
    else tr("tablas.pasivos.revision.genera_24m")
  paste(primera, tr("tablas.pasivos.revision.aparecen"))
}

# T70: a dateInput's value as a Date, or NA when blank or not a date.
.wiz_leer_fecha <- function(v) {
  if (is.null(v) || !length(v)) return(as.Date(NA))
  tryCatch(as.Date(v[1]), error = function(e) as.Date(NA))
}

# ── Read step inputs into draft ────────────────────────────────────────────────
.wiz_collect_step <- function(input, step_id, wiz) {
  d <- wiz$draft
  switch(step_id,

    categoria = {
      d$categoria <- input$pwiz_categoria %||% ""
      d$flavor    <- input$pwiz_flavor    %||% "credito_simple"
    },

    detalles = {
      d$nombre              <- input$pwiz_nombre              %||% ""
      d$objeto              <- .pasivos_objeto_texto(input$pwiz_objeto)   # T105
      d$empresa             <- input$pwiz_empresa             %||% ""
      d$parte               <- input$pwiz_parte               %||% ""
      d$codigo_parte        <- input$pwiz_codigo_parte        %||% ""
      d$referencia_default  <- input$pwiz_referencia_default  %||% ""
      d$documento_template  <- input$pwiz_documento_template  %||% ""
      d$subcategoria        <- input$pwiz_subcategoria        %||% ""
      d$moneda_pago         <- input$pwiz_moneda_pago         %||% base_moneda()
      d$cotizado_en         <- input$pwiz_cotizado_en         %||% ""
      d$notas               <- trimws(input$pwiz_notas        %||% "")   # T71
      if (identical(d$categoria, "tarjeta")) {
        d$tarjeta_closing_day  <- as.integer(input$pwiz_tarjeta_closing_day %||% NA_integer_)
        d$tarjeta_due_day      <- as.integer(input$pwiz_tarjeta_due_day     %||% NA_integer_)
        d$tarjeta_credit_limit <- input$pwiz_tarjeta_credit_limit %||% NA_real_
      }
    },

    recurrencia = {
      d$fecha_fin <- .wiz_leer_fecha(input$pwiz_fecha_fin)   # T70: NA = no end
      if (identical(d$categoria, "tarjeta")) {
        d$tarjeta_provision_source <- input$pwiz_tarjeta_provision_source %||% "registered_expenses"
        if (identical(d$tarjeta_provision_source, "manual")) {
          d$amount_default <- input$pwiz_amount_default %||% NA_real_
        } else {
          if (is.na(d$amount_default %||% NA_real_)) d$amount_default <- 0
        }
        due_day <- as.integer(d$tarjeta_due_day %||% 15L)
        if (is.na(due_day)) due_day <- 15L
        d$recurrence_type   <- "monthly_day"
        d$recurrence_params <- list(day = due_day)
      } else {
        rt <- input$pwiz_periodicidad %||% "monthly_day"
        d$recurrence_type   <- rt
        d$recurrence_params <- .wiz_build_recurrence_params(input, rt)
        if (identical(rt, "custom"))
          d$recurrence_params$dates <- as.Date(wiz$custom_dates %||% character(0))
        d$fecha_inicio       <- tryCatch(as.Date(input$pwiz_fecha_inicio %||% NA),
                                          error = function(e) as.Date(NA))
        d$amount_default     <- input$pwiz_amount_default     %||% NA_real_
        d$amount_default_cot <- input$pwiz_amount_cotizado    %||% NA_real_
      }
    },

    terminos = {
      d$principal_original  <- input$pwiz_principal_original %||% NA_real_
      d$saldo_capital       <- input$pwiz_saldo_capital      %||% NA_real_
      d$tasa_tipo <- input$pwiz_tasa_tipo %||% "fija"
      if (identical(d$tasa_tipo, "fija")) {
        d$tasa_actual <- input$pwiz_tasa_anual   %||% NA_real_
        d$tasa_spread <- 0
      } else {
        fetched       <- as.numeric(input$pwiz_tasa_fetched)
        d$tasa_actual <- if (!is.na(fetched)) fetched else (wiz$tasa_fetched_cache %||% NA_real_)
        d$tasa_spread <- input$pwiz_tasa_spread  %||% 0
      }
      d$fecha_inicio        <- tryCatch(as.Date(input$pwiz_fecha_inicio %||% NA),
                                         error = function(e) as.Date(NA))
      cv <- input$pwiz_convencion_dias %||% d$convencion_dias              # T115
      d$convencion_dias     <- if (pasivos_convencion_valida(cv)) cv else NA_character_
      d$plazo_meses         <- as.integer(input$pwiz_plazo_meses %||% NA_integer_)
      d$frecuencia_pago     <- input$pwiz_frecuencia_pago %||% "mensual"
      # Collect dia_pago and mes_pago based on frequency
      freq_now <- d$frecuencia_pago
      if (identical(freq_now, "diaria")) {
        d$dia_pago  <- NA_integer_
        d$mes_pago  <- NA_integer_
      } else if (identical(freq_now, "semanal")) {
        d$dia_pago  <- as.integer(input$pwiz_dia_semana_pago %||% NA_integer_)
        d$mes_pago  <- NA_integer_
      } else if (identical(freq_now, "anual")) {
        d$dia_pago  <- as.integer(input$pwiz_dia_pago_anual %||% NA_integer_)
        d$mes_pago  <- as.integer(input$pwiz_mes_pago       %||% NA_integer_)
      } else {
        d$dia_pago  <- as.integer(input$pwiz_dia_pago %||% NA_integer_)
        d$mes_pago  <- NA_integer_
      }
      pg_raw                <- as.integer(input$pwiz_periodo_gracia     %||% 0L)
      d$periodo_gracia      <- if (is.na(pg_raw) || pg_raw < 0L) 0L else pg_raw
      mg_raw                <- input$pwiz_monto_gracia %||% NA_real_
      d$monto_gracia        <- if (!is.null(mg_raw) && !is.na(mg_raw) && mg_raw >= 0)
                                 mg_raw else NA_real_
      d$metodo_amortizacion <- input$pwiz_metodo_amortizacion           %||% "francesa"
      d$schedule_csv_raw    <- input$pwiz_schedule_csv                  %||% ""
      # arrendamiento_puro: force 0% fixed rate + francesa (hidden in UI but enforce here)
      if (identical(d$flavor, "arrendamiento_puro")) {
        d$tasa_actual         <- 0
        d$tasa_tipo           <- "fija"
        d$tasa_spread         <- 0
        d$metodo_amortizacion <- "francesa"
      }
    },

    cargos = {
      # Collect cargo rows
      n_cargos <- length(wiz$cargos_list)
      cargos_out <- lapply(seq_len(n_cargos), function(i) {
        list(
          desc   = input[[paste0("pwiz_cargo_desc_", i)]]   %||% "",
          monto  = input[[paste0("pwiz_cargo_monto_", i)]]  %||% 0,
          moneda = input[[paste0("pwiz_cargo_moneda_", i)]] %||% base_moneda(),
          fecha  = tryCatch(as.Date(input[[paste0("pwiz_cargo_fecha_", i)]] %||% NA),
                            error = function(e) hoy_usuario())
        )
      })
      d$cargos_iniciales <- if (length(cargos_out)) cargos_out else NULL
      # T63: the tax rows, as typed (validated by pasivos_wizard_validate_cargos()).
      d$impuestos <- .wiz_leer_impuestos(input, length(wiz$impuestos_list), d$categoria)
      if (!length(d$impuestos)) d$impuestos <- NULL

      # Residual
      if (isTRUE(input$pwiz_has_residual)) {
        d$valor_residual <- list(
          monto    = input$pwiz_residual_monto    %||% NA_real_,
          moneda   = input$pwiz_residual_moneda   %||% base_moneda(),
          fecha    = tryCatch(as.Date(input$pwiz_residual_fecha %||% NA),
                              error = function(e) as.Date(NA)),
          behavior = input$pwiz_residual_behavior %||% "replace_last"
        )
      } else {
        d$valor_residual <- NULL
      }
    }
  )
  d
}

# ── Revision summary renderer ──────────────────────────────────────────────────
# T109: the draft's cost from the SAME schedule as the amortization table
# (the rate just fetched -- usar_indice = FALSE -- and the client's day), so
# the summary and the table always add up to the same figures.
.wiz_costeo <- function(draft) {
  liab <- tryCatch(pasivos_wizard_draft_to_liability(draft, mode = "create", user = "preview"), error = function(e) NULL)
  if (is.null(liab)) return(NULL)
  tryCatch(pasivos_costeo(liab, hoy = hoy_cliente(), usar_indice = FALSE), error = function(e) NULL)
}

# T115: annual <-> per-period rate (%) by the day-count convention: efectiva
# compounds, the nominal ones (30/360, Act/360, Act/365, Act/Act) divide.
.wiz_tasa_periodo <- function(anual, ppy, conv)
  if (identical(conv, "efectiva")) ((1 + anual / 100)^(1 / ppy) - 1) * 100 else anual / ppy
.wiz_tasa_anual <- function(periodo, ppy, conv)
  if (identical(conv, "efectiva")) ((1 + periodo / 100)^ppy - 1) * 100 else periodo * ppy

# Whether any currency still has less to pay than its whole term (a payment
# already behind): then the "por pagar" lines are worth showing.
.wiz_hay_pasadas <- function(todo, falta) any(abs(todo - falta) > 0.005, na.rm = TRUE)

# T77: the Revisión's cost lines -- financiero: the cost of the financing
# (interest + charges + taxes, never the capital) over the whole term with its
# breakdown, what is still to pay (T109, when a payment is behind) and the
# next 12 months; regular and tarjeta: per month and the next 12 months, and
# with an end date the total up to it.
.wiz_costeo_filas <- function(draft) {
  c <- .wiz_costeo(draft)
  if (is.null(c) || !nrow(c$filas)) return(list())
  f <- c$filas
  mm <- function(x) paste(monto_moneda(x, f$moneda), collapse = " \u00b7 ")
  if (identical(c$categoria, "financiero")) {
    desglose <- paste(vapply(seq_len(nrow(f)), function(i) tr("tablas.pasivos.costeo.desglose",
      intereses = monto_moneda(f$intereses[i], f$moneda[i]), cargos = monto_moneda(f$cargos[i], f$moneda[i]),
      impuestos = monto_moneda(f$impuestos[i], f$moneda[i])), character(1)), collapse = " \u00b7 ")
    return(c(list(c(tr("tablas.pasivos.costeo.total_financiamiento"), mm(f$total)),
                  c(tr("tablas.pasivos.costeo.desglose_titulo"), desglose)),
             if (.wiz_hay_pasadas(f$total, f$total_faltan))
               list(c(tr("tablas.pasivos.costeo.por_pagar"), mm(f$total_faltan))),
             list(c(tr("tablas.pasivos.costeo.por_ano"), mm(f$anual)))))
  }
  out <- list(c(tr("tablas.pasivos.costeo.por_mes"), mm(f$mes)), c(tr("tablas.pasivos.costeo.por_ano"), mm(f$anual)))
  if (any(!is.na(f$total))) out <- c(out, list(c(tr("tablas.pasivos.costeo.hasta_fin"), mm(f$total))))
  out
}

.wiz_revision_summary_html <- function(draft) {
  cat  <- draft$categoria  %||% ""
  flav <- draft$flavor     %||% ""
  cat_label <- switch(cat,
    regular    = "Pago regular",
    financiero = paste0("Pasivo financiero — ", switch(flav,
      credito_simple   = "Crédito simple",
      arrendamiento    = "Arrendamiento financiero",
      arrendamiento_puro = "Arrendamiento puro",
      linea_revolvente = "Línea revolvente",
      otro             = "Otro",
      flav)),
    tarjeta    = "Tarjeta de crédito",
    cat
  )

  rows <- list(
    c("Categoría",     cat_label),
    c("Nombre",        draft$nombre     %||% "—"),
    c("Empresa",       draft$empresa    %||% "—"),
    c("Parte",         draft$parte      %||% "—"),
    c("Moneda de pago", paste0(
      draft$moneda_pago %||% "—",
      if (nzchar(draft$cotizado_en %||% ""))
        sprintf(" (cotizado en %s)", draft$cotizado_en)
      else ""
    ))
  )

  if (identical(cat, "financiero")) {
    pl   <- as.integer(draft$plazo_meses    %||% 0L)
    pg   <- as.integer(draft$periodo_gracia %||% 0L)
    if (is.na(pg)) pg <- 0L
    freq <- draft$frecuencia_pago %||% "mensual"
    freq_label <- switch(freq,
      mensual = "períodos mensuales",
      anual   = "períodos anuales",
      semanal = "períodos semanales",
      diaria  = "períodos diarios",
      "períodos"
    )
    plazo_label <- if (pg > 0L) {
      sprintf("%d gracia + %d amort. = %d total (%s)", pg, pl, pg + pl, freq_label)
    } else {
      sprintf("%d %s", pl, freq_label)
    }
    tasa_label <- if (identical(flav, "arrendamiento_puro")) {
      "0% (renta fija)"
    } else {
      tipo_t     <- draft$tasa_tipo   %||% "fija"
      tasa_base  <- draft$tasa_actual %||% 0
      spread_bps <- draft$tasa_spread %||% 0
      tasa_anual <- if (!identical(tipo_t, "fija")) tasa_base + spread_bps / 100 else tasa_base
      ppy_rev    <- switch(freq, mensual=12L, anual=1L, semanal=52L, diaria=365L, 12L)
      periodo_nombre <- switch(freq,
        mensual="mensual", anual="anual", semanal="semanal", diaria="diario", "período")
      # T115: by the draft's day-count convention (as the engine will compute).
      conv_rev <- pasivos_convencion(list(convencion_dias = draft$convencion_dias %||% NA_character_, tasa_tipo = tipo_t))
      tasa_periodo_rev <- round(.wiz_tasa_periodo(tasa_anual, ppy_rev, conv_rev), 3)
      if (!identical(tipo_t, "fija")) {
        ref_lbl <- switch(tipo_t, variable_sofr = "SOFR", variable_prime = "PRIME", "TIIE28")
        tr("tablas.pasivos.tasa_var.revision", indice = ref_lbl, base = sprintf("%.3f", tasa_base),
           pb = sprintf("%.0f", spread_bps), nominal = sprintf("%.3f", tasa_anual),
           convencion = tr(paste0("tablas.pasivos.convencion.corto.", conv_rev)),
           ear = sprintf("%.3f", pasivos_ear_convencion(tasa_anual, ppy_rev, conv_rev)))
      } else if (identical(conv_rev, "efectiva")) {
        sprintf("%.3f%% anual (%.3f%% %s)", tasa_anual, tasa_periodo_rev, periodo_nombre)
      } else {
        tr("tablas.pasivos.convencion.resumen", tasa = sprintf("%.3f", tasa_anual),
           convencion = tr(paste0("tablas.pasivos.convencion.corto.", conv_rev)))
      }
    }
    metodo_label <- switch(draft$metodo_amortizacion %||% "",
      francesa          = "Francesa",
      alemana           = "Alemana",
      americana         = "Americana",
      custom            = "Tabla propia",
      if (identical(flav, "arrendamiento_puro")) "Cuota fija"
      else (draft$metodo_amortizacion %||% "")
    )
    rows <- c(rows, list(
      c("Principal",    monto_moneda(draft$principal_original %||% NA_real_, draft$moneda_pago %||% NA_character_)),
      c("Tasa",         tasa_label),
      c("Plazo",        plazo_label),
      c("Amortización", metodo_label)
    ))
  } else if (cat %in% c("regular", "tarjeta")) {
    rows <- c(rows, list(
      c("Monto", monto_moneda(draft$amount_default %||% NA_real_, draft$moneda_pago %||% NA_character_))
    ))
  }
  # T70: the end date (regular and tarjeta)
  if (cat %in% c("regular", "tarjeta")) {
    fin <- tryCatch(as.Date(draft$fecha_fin %||% NA), error = function(e) as.Date(NA))
    rows <- c(rows, list(c(tr("tablas.pasivos.fin.resumen"),
                           if (is.na(fin)) tr("tablas.pasivos.fin.sin_fin_corto") else format(fin, "%d/%m/%Y"))))
  }
  # T105: what the liability finances or pays
  obj <- pasivos_objeto_limpio(draft$objeto)
  if (!is.na(obj)) rows <- c(rows, list(c(tr("tablas.pasivos.objeto.resumen"), obj)))
  # T71: the note every provision inherits
  if (nzchar(trimws(draft$notas %||% ""))) rows <- c(rows, list(c(tr("tablas.pasivos.nota.resumen"), trimws(draft$notas))))
  # T77: what it costs (pasivos_costeo(), from the engine; by currency,
  # never adding currencies). Opening a liability with ✎ lands on this step,
  # so this is also its detail.
  rows <- c(rows, .wiz_costeo_filas(draft))
  # T63: the liability's taxes, as the engine applies them to each payment.
  imps <- pasivos_impuestos_de(draft)
  if (length(imps)) rows <- c(rows, list(c(tr("tablas.pasivos.impuestos.resumen"),
    paste(vapply(imps, function(t) {
      v <- as.numeric(t$valor)
      if (identical(t$tipo, "fijo"))
        paste(t$nombre, monto_moneda(v, draft$moneda_pago %||% NA_character_), tr("tablas.pasivos.impuestos.fijo"))
      else paste0(t$nombre, " ", format(v, trim = TRUE), " % \u00b7 ",
                  tr(if (identical(t$base, "intereses")) "tablas.pasivos.impuestos.base_intereses"
                     else "tablas.pasivos.impuestos.base_pago"))
    }, character(1)), collapse = " \u00b7 "))))
  # T63: what the taxes add up to over the whole term (loans) or on each
  # payment (regular and tarjeta), by currency, never added across currencies.
  # T109: a loan's whole term is EVERY payment, from pasivos_costeo() -- the
  # breakdown's own figure; it used to add only the table of the next
  # payments, i.e. what was still to pay. That one now has its own line.
  if (length(imps)) {
    if (identical(cat, "financiero")) {
      co <- .wiz_costeo(draft)
      if (!is.null(co) && nrow(co$filas)) {
        mm <- function(x) paste(monto_moneda(x, co$filas$moneda), collapse = " \u00b7 ")
        rows <- c(rows, list(c(tr("tablas.pasivos.impuestos.total_plazo"), mm(co$filas$impuestos))))
        if (.wiz_hay_pasadas(co$filas$impuestos, co$filas$impuestos_faltan))
          rows <- c(rows, list(c(tr("tablas.pasivos.impuestos.por_pagar"), mm(co$filas$impuestos_faltan))))
      }
    } else {
      v <- tryCatch(pasivos_vista_provisiones(draft), error = function(e) NULL)
      if (!is.null(v) && nrow(v)) {
        t <- tapply(v$impuestos, v$moneda, function(x) x[1])
        rows <- c(rows, list(c(tr("tablas.pasivos.impuestos.por_pago"),
                               paste(monto_moneda(as.numeric(t), names(t)), collapse = " \u00b7 "))))
      }
    }
  }

  shiny::tags$dl(
    class = "row small mb-0",
    lapply(rows, function(r) {
      list(
        shiny::tags$dt(class = "col-sm-5 text-muted", r[1]),
        shiny::tags$dd(class = "col-sm-7 fw-semibold", r[2])
      )
    })
  )
}

# ── Previews of the Revisión step (amortizacion / provisiones_asistente, E) ──
# What the draft would generate, as rows for hd_table (T18). Financiero: its
# amortization schedule from today on, EVERY row (Luis: all of them, with a
# scroll; it used to show the first 12, "…" and the last 3). Regular and
# tarjeta: the first 12 provisions, as before. NULL when the draft cannot be
# turned into a liability yet.
# T109: completa = TRUE gives every payment of the contract, past ones too
# ("Ver tabla completa"), so its sums are the whole term's and the next
# payments' sums are what is still to pay -- the Revisión's two lines. Every
# row carries its saldo insoluto (the capital left after it) and its estado:
# "pagada" when its provision (liability_id + occurrence_index = periodo) is
# paid (PROVISION_PAGADA), else "pasada" before the client's day, else
# "por_pagar". Same schedule and currency as pasivos_costeo() in the wizard.
.wiz_saldo_insoluto <- function(s, principal) {
  sp <- if ("saldo_post" %in% names(s)) suppressWarnings(as.numeric(s$saldo_post)) else rep(NA_real_, nrow(s))
  # An imported (custom) schedule may bring no balance: the principal less the
  # capital paid so far, in date order; without a principal, unknown (NA).
  if (all(is.na(sp)) && length(principal) == 1L && !is.na(principal)) {
    cap <- suppressWarnings(as.numeric(s$capital)); cap[is.na(cap)] <- 0
    cap[s$origin %in% "initial_fee"] <- 0
    sp <- pmax(0, round(principal - cumsum(cap), 2))
  }
  sp
}

pasivos_vista_amortizacion <- function(draft, completa = FALSE, provs = NULL, liability_id = NULL) {
  # A variable rate takes the value just fetched (tasa_actual + spread), not
  # the estimates' store, and stays variable: nominal annual, Act/360 (it used
  # to be turned into a fixed one, which the engine takes as effective).
  liab_row <- tryCatch(
    pasivos_wizard_draft_to_liability(draft, mode = "create", user = "preview"),
    error = function(e) NULL
  )
  if (is.null(liab_row)) return(NULL)
  sched <- tryCatch(pasivos_generate_schedule(liab_row, today = hoy_cliente(), usar_indice = FALSE),
                    error = function(e) NULL)
  if (is.null(sched) || !nrow(sched)) return(NULL)
  sched <- sched[order(as.Date(sched$fecha), sched$periodo), , drop = FALSE]
  sched$.saldo <- .wiz_saldo_insoluto(sched, suppressWarnings(as.numeric(liab_row$principal_original[1])))
  futuras <- if (isTRUE(completa)) sched else sched[!isTRUE_safe(sched$historico), , drop = FALSE]
  # The currency as pasivos_costeo() has it: the row's, else the quoted one
  # (cotizado_en), else the payment's.
  base_m <- as.character(liab_row$cotizado_en[1] %||% NA_character_)
  if (is.na(base_m) || !nzchar(base_m)) base_m <- draft$moneda_pago %||% NA_character_
  mon <- if ("moneda" %in% names(futuras)) as.character(futuras$moneda) else rep(NA_character_, nrow(futuras))
  mon[is.na(mon) | !nzchar(mon)] <- base_m
  pagada <- get0("PROVISION_PAGADA", ifnotfound = character())
  occ_pagadas <- if (is.data.frame(provs) && nrow(provs) && length(liability_id) == 1L && !is.na(liability_id) &&
                     all(c("liability_id", "occurrence_index", "estado") %in% names(provs)))
    provs$occurrence_index[provs$liability_id %in% liability_id & provs$estado %in% pagada] else integer()
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }
  filas <- data.frame(
    id      = make.unique(paste0("p", futuras$periodo)),
    periodo = as.integer(futuras$periodo),
    fecha   = as.Date(futuras$fecha),
    capital = as.numeric(futuras$capital %||% 0),
    interes = as.numeric(futuras$interes %||% 0),
    cargos  = num(futuras$fees),
    total   = as.numeric(futuras$total %||% 0),
    # T63: each payment's taxes and the payment with them (0 / total with none)
    impuestos  = if ("impuestos" %in% names(futuras)) as.numeric(futuras$impuestos) else 0,
    total_pago = if ("total_pago" %in% names(futuras)) as.numeric(futuras$total_pago) else as.numeric(futuras$total %||% 0),
    saldo   = as.numeric(futuras$.saldo),
    dias    = if ("dias" %in% names(futuras)) as.numeric(futuras$dias) else NA_real_,                 # T115
    tasa_periodo = if ("tasa_periodo" %in% names(futuras)) as.numeric(futuras$tasa_periodo) else NA_real_,
    estado  = ifelse(futuras$periodo %in% occ_pagadas, "pagada",
                     ifelse(isTRUE_safe(futuras$historico), "pasada", "por_pagar")),
    moneda  = mon,
    stringsAsFactors = FALSE)
  attr(filas, "reamortizada") <- any(!is.na(sched$re_amortized) & sched$re_amortized, na.rm = TRUE)
  filas
}

pasivos_vista_provisiones <- function(draft) {
  liab_row <- tryCatch(
    pasivos_wizard_draft_to_liability(draft, mode = "create", user = "preview"),
    error = function(e) NULL
  )
  if (is.null(liab_row)) return(NULL)
  provs <- tryCatch(
    pasivos_generate_provisions(liab_row, window_start = hoy_cliente(),
                                 policies_for_liability = list(),
                                 holidays_cache = list()),
    error = function(e) NULL
  )
  if (is.null(provs) || !nrow(provs)) return(NULL)
  p <- head(provs, 12)
  iva <- as.numeric(p$componente_iva); iva[is.na(iva)] <- 0   # T63: the tax inside each amount
  data.frame(id = p$id, fecha = as.Date(p$fecha_efectiva), monto = as.numeric(p$amount_pago %||% 0),
             impuestos = iva,
             moneda = p$moneda_pago %||% (draft$moneda_pago %||% NA_character_), stringsAsFactors = FALSE)
}

.wiz_vacia_amort <- data.frame(id = character(), periodo = integer(), fecha = as.Date(character()),
                               capital = numeric(), interes = numeric(), cargos = numeric(), total = numeric(),
                               impuestos = numeric(), total_pago = numeric(), saldo = numeric(),
                               dias = numeric(), tasa_periodo = numeric(),
                               estado = character(), moneda = character(), stringsAsFactors = FALSE)
.wiz_vacia_prov  <- data.frame(id = character(), fecha = as.Date(character()), monto = numeric(),
                               impuestos = numeric(), moneda = character(), stringsAsFactors = FALSE)

# The Revisión step's preview: a notice when the schedule was re-amortized,
# and the table that fits the draft's categoría.
.wiz_revision_schedule_html <- function(draft) {
  if (identical(draft$categoria %||% "", "financiero")) {
    filas <- pasivos_vista_amortizacion(draft)
    shiny::tagList(
      shiny::uiOutput("pwiz_amort_cabecera"),   # T109: the title and "Ver tabla completa"
      if (isTRUE(attr(filas, "reamortizada"))) shiny::div(
        class = "alert alert-warning small py-1 mb-2", tr("tablas.pasivos.amort.reamortizada")),
      hd_table_ui("pwiz_amort", alto = "320px"))
  } else {
    hd_table_ui("pwiz_prov", alto = "320px")
  }
}

# The two tables' servers, once per session (from setup_pasivos_wizard).
# T109: the amortization table shows the next payments or, with "Ver tabla
# completa" (wiz$amort_vista), every payment of the contract with its estado;
# its sums are the Revisión's "por pagar" and "todo el plazo" lines.
.wiz_tablas_vista_previa <- function(wiz, shared = NULL) {
  hd_table_server("pwiz_amort", tabla = "amortizacion",
    datos = shiny::reactive({
      if (!identical(wiz$draft$categoria %||% "", "financiero")) return(.wiz_vacia_amort)
      provs <- tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL)
      pasivos_vista_amortizacion(wiz$draft, completa = identical(wiz$amort_vista, "completa"),
                                 provs = provs, liability_id = wiz$edit_target_id) %||% .wiz_vacia_amort
    }),
    columnas = list(
      col_numero("periodo", "tablas.pasivos.amort.periodo"),
      col_fecha("fecha",    "tablas.pasivos.amort.fecha"),
      col_numero("dias",    "tablas.pasivos.amort.dias"),                                   # T115
      col_numero("tasa_periodo", "tablas.pasivos.amort.tasa_periodo", decimales = 4L, opcional = TRUE, visible = FALSE),
      col_dinero("capital", "tablas.pasivos.amort.capital", moneda = "moneda", sumar = TRUE),
      col_dinero("interes", "tablas.pasivos.amort.interes", moneda = "moneda", sumar = TRUE),
      col_dinero("cargos",  "tablas.pasivos.amort.cargos",  moneda = "moneda", sumar = TRUE, opcional = TRUE, visible = FALSE),
      col_dinero("total",   "tablas.pasivos.amort.total",   moneda = "moneda", sumar = TRUE, opcional = TRUE, visible = FALSE),
      col_dinero("impuestos",  "tablas.pasivos.amort.impuestos",  moneda = "moneda", sumar = TRUE),
      col_dinero("total_pago", "tablas.pasivos.amort.total_pago", moneda = "moneda", sumar = TRUE),
      col_dinero("saldo",   "tablas.pasivos.amort.saldo",   moneda = "moneda", sumar = FALSE),
      col_insignia("estado", "tablas.pasivos.amort.estado", mapa = list(
        pagada    = list(texto = "tablas.pasivos.amort.estado_pagada",    tono = "exito"),
        pasada    = list(texto = "tablas.pasivos.amort.estado_pasada",    tono = "neutro"),
        por_pagar = list(texto = "tablas.pasivos.amort.estado_por_pagar", tono = "info")))),
    vacio = "tablas.pasivos.amort.vacio")
  hd_table_server("pwiz_prov", tabla = "provisiones_asistente",
    datos = shiny::reactive({
      if (identical(wiz$draft$categoria %||% "", "financiero")) return(.wiz_vacia_prov)
      pasivos_vista_provisiones(wiz$draft) %||% .wiz_vacia_prov
    }),
    columnas = list(
      col_fecha("fecha",  "tablas.pasivos.amort.fecha"),
      col_dinero("monto", "tablas.pasivos.amort.monto", moneda = "moneda", sumar = FALSE),
      col_dinero("impuestos", "tablas.pasivos.amort.impuestos", moneda = "moneda", sumar = TRUE)),
    vacio = "tablas.pasivos.prov.vacio")
  invisible(NULL)
}

# T63: the tax rows as typed in the cargos step. `todas`: keep the empty ones
# too (to redraw the rows while adding or removing one); otherwise a row with
# no name and no value is dropped. "intereses" only for financiero.
.wiz_leer_impuestos <- function(input, n, categoria, todas = FALSE) {
  out <- lapply(seq_len(n), function(i) {
    nombre <- trimws(input[[paste0("pwiz_imp_nombre_", i)]] %||% "")
    tipo   <- input[[paste0("pwiz_imp_tipo_", i)]] %||% "porcentual"
    valor  <- suppressWarnings(as.numeric(input[[paste0("pwiz_imp_valor_", i)]] %||% NA_real_))
    base   <- if (identical(categoria %||% "", "financiero")) input[[paste0("pwiz_imp_base_", i)]] %||% "pago" else "pago"
    list(nombre = nombre, tipo = tipo, valor = valor, base = base)
  })
  if (todas) return(out)
  Filter(function(t) nzchar(t$nombre) || !is.na(t$valor), out)
}

# ── Main setup function ────────────────────────────────────────────────────────
setup_pasivos_wizard <- function(input, output, session, shared) {

  # The client's base currency (T33), read without a dependency: what a
  # selector starts on when nothing else says. Never a fixed code.
  base_moneda <- function() shiny::isolate(tryCatch(shared$moneda_base_cliente()$moneda,
                                                    error = function(e) NULL)) %||% MONEDA_BASE_OMISION
  # What a currency selector puts on top: the base, then the currencies of
  # this client's liabilities and provisions; the rest of the catalog below.
  monedas_primero <- function() monedas_en_datos(
    shiny::isolate(tryCatch(c(shared$pasivos_liabilities_db()$moneda_pago, shared$pasivos_provisions_db()$moneda_pago),
                            error = function(e) character())), base_moneda())

  wiz <- shiny::reactiveValues(
    mode             = "create",
    step_id          = "categoria",
    edit_target_id   = NULL,
    draft            = list(),
    validation_errors = list(),
    cargos_list      = list(),     # in-progress cargo rows for the cargos step
    impuestos_list   = list(),     # T63: in-progress tax rows (cargos step)
    custom_dates     = character(), # accumulating dates for "custom" recurrence
    amort_vista      = "proximas"   # T109: "proximas" | "completa"
  )

  # The Revisión step's previews (amortizacion / provisiones_asistente, T18)
  .wiz_tablas_vista_previa(wiz, shared)

  # ── Rate dual-input: last-touched tracking ────────────────────────────────
  last_tasa_touched    <- shiny::reactiveVal("anual")
  pwiz_rate_dia_rv     <- shiny::reactiveVal("ayer")   # "hoy" | "ayer"
  pwiz_fetched_rates_rv <- shiny::reactiveVal(list())  # list(today, yesterday, ...)

  # Blur-based cross-update: fires only when user LEAVES the field (tab/click away).
  # Server-side hd_numero_update calls do NOT trigger blur, so no feedback loop.
  # T115: annual <-> per period by the chosen day-count convention
  # (efectiva compounds; the nominal ones divide).
  .conv_wiz <- function() {
    cv <- input$pwiz_convencion_dias %||% wiz$draft$convencion_dias
    if (pasivos_convencion_valida(cv)) cv else "efectiva"
  }
  .conv_var <- function() {   # a variable rate without one: Act/360, as before T115
    cv <- input$pwiz_convencion_dias %||% wiz$draft$convencion_dias
    if (pasivos_convencion_valida(cv)) cv else "act_360"
  }
  shiny::observeEvent(input$pwiz_tasa_anual_blur, {
    last_tasa_touched("anual")
    freq  <- input$pwiz_frecuencia_pago %||% "mensual"
    ppy   <- .periods_per_year(freq)
    anual <- input$pwiz_tasa_anual_blur
    if (!is.null(anual) && !is.na(anual))
      hd_numero_update(session, "pwiz_tasa_periodo",
                                value = round(.wiz_tasa_periodo(anual, ppy, .conv_wiz()), 3))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$pwiz_tasa_periodo_blur, {
    last_tasa_touched("periodo")
    freq   <- input$pwiz_frecuencia_pago %||% "mensual"
    ppy    <- .periods_per_year(freq)
    period <- input$pwiz_tasa_periodo_blur
    if (!is.null(period) && !is.na(period))
      hd_numero_update(session, "pwiz_tasa_anual",
                                value = round(.wiz_tasa_anual(period, ppy, .conv_wiz()), 3))
  }, ignoreInit = TRUE)

  # The annual rate stays; the per-period one follows a new convention.
  shiny::observeEvent(input$pwiz_convencion_dias, {
    anual <- input$pwiz_tasa_anual
    if (!is.null(anual) && !is.na(anual))
      hd_numero_update(session, "pwiz_tasa_periodo",
        value = round(.wiz_tasa_periodo(anual, .periods_per_year(input$pwiz_frecuencia_pago %||% "mensual"), .conv_wiz()), 3))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$pwiz_frecuencia_pago, {
    freq <- input$pwiz_frecuencia_pago %||% "mensual"
    ppy  <- .periods_per_year(freq)
    ltt  <- last_tasa_touched()
    if (identical(ltt, "anual") ||
        is.null(input$pwiz_tasa_periodo) || is.na(input$pwiz_tasa_periodo)) {
      anual <- input$pwiz_tasa_anual
      if (!is.null(anual) && !is.na(anual))
        hd_numero_update(session, "pwiz_tasa_periodo",
                                  value = round(.wiz_tasa_periodo(anual, ppy, .conv_wiz()), 3))
    } else {
      period <- input$pwiz_tasa_periodo
      if (!is.null(period) && !is.na(period))
        hd_numero_update(session, "pwiz_tasa_anual",
                                  value = round(.wiz_tasa_anual(period, ppy, .conv_wiz()), 3))
    }
  }, ignoreInit = TRUE)

  # ── Variable rate: fetch on tipo change or step entry ─────────────────────
  .do_fetch_rates <- function(tipo) {
    if (identical(tipo, "variable_tiie28")) .fetch_tiie28()
    else if (identical(tipo, "variable_sofr")) .fetch_sofr()
    else if (identical(tipo, "variable_prime")) .fetch_prime()
    else list()
  }

  shiny::observeEvent(input$pwiz_tasa_tipo, {
    tipo <- input$pwiz_tasa_tipo %||% "fija"
    if (!identical(tipo, "fija"))
      pwiz_fetched_rates_rv(.do_fetch_rates(tipo))
  }, ignoreInit = TRUE)

  shiny::observeEvent(wiz$step_id, {
    if (!identical(wiz$step_id, "terminos")) return()
    tipo <- wiz$draft$tasa_tipo %||% "fija"
    if (!identical(tipo, "fija"))
      pwiz_fetched_rates_rv(.do_fetch_rates(tipo))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$pwiz_rate_hoy,  { pwiz_rate_dia_rv("hoy")  }, ignoreInit = TRUE)
  shiny::observeEvent(input$pwiz_rate_ayer, { pwiz_rate_dia_rv("ayer") }, ignoreInit = TRUE)

  shiny::observeEvent(input$pwiz_retry_rate, {
    tipo <- input$pwiz_tasa_tipo %||% "fija"
    if (!identical(tipo, "fija"))
      pwiz_fetched_rates_rv(.do_fetch_rates(tipo))
  }, ignoreInit = TRUE)

  # Keep the hidden pwiz_tasa_fetched input in sync with the selected day's rate
  shiny::observe({
    rates <- pwiz_fetched_rates_rv()
    dia   <- pwiz_rate_dia_rv()
    rv <- if (is.list(rates) && length(rates) && is.null(rates$error)) {
      if (identical(dia, "hoy")) rates$today else rates$yesterday
    } else NA_real_
    if (!is.na(rv)) wiz$tasa_fetched_cache <- rv
    hd_numero_update(session, "pwiz_tasa_fetched",
                              value = if (!is.null(rv) && !is.na(rv)) rv else NA_real_)
  })

  # Hoy/Ayer button rendering with active state + dates
  output$pwiz_rate_dia_buttons <- shiny::renderUI({
    dia   <- pwiz_rate_dia_rv()
    rates <- pwiz_fetched_rates_rv()
    hoy_lbl  <- if (is.list(rates) && !is.null(rates$date_today) &&
                    !is.na(rates$date_today %||% NA))
                  sprintf("Hoy (%s)", format(rates$date_today, "%d/%m"))
                else "Hoy"
    ayer_lbl <- if (is.list(rates) && !is.null(rates$date_yesterday) &&
                    !is.na(rates$date_yesterday %||% NA))
                  sprintf("Día anterior (%s)", format(rates$date_yesterday, "%d/%m"))
                else "Día anterior"
    shiny::div(
      class = "d-flex gap-2 mb-2",
      shiny::actionButton("pwiz_rate_ayer", ayer_lbl,
        class = paste0("btn btn-sm ",
                       if (identical(dia, "ayer")) "btn-secondary" else "btn-outline-secondary")),
      shiny::actionButton("pwiz_rate_hoy", hoy_lbl,
        class = paste0("btn btn-sm ",
                       if (identical(dia, "hoy")) "btn-secondary" else "btn-outline-secondary"))
    )
  })

  # Variable rate preview panel (base + spread + effective, both days)
  output$pwiz_variable_rate_preview <- shiny::renderUI({
    tipo <- input$pwiz_tasa_tipo %||% "fija"
    if (identical(tipo, "fija")) return(NULL)
    rates      <- pwiz_fetched_rates_rv()
    dia        <- pwiz_rate_dia_rv()
    freq       <- input$pwiz_frecuencia_pago %||% "mensual"
    ppy        <- .periods_per_year(freq)
    spread_bps <- as.numeric(input$pwiz_tasa_spread %||% 0)
    if (is.na(spread_bps)) spread_bps <- 0
    tipo_lbl   <- switch(tipo, variable_tiie28 = "TIIE 28", variable_prime = "PRIME", "SOFR")

    if (!is.list(rates) || !length(rates))
      return(shiny::div(class = "text-muted small mt-1", "Obteniendo tasa de referencia…"))
    if (!is.null(rates$error) && nzchar(rates$error %||% ""))
      return(shiny::div(
        class = "alert alert-warning small py-2 mt-1",
        paste0("Error: ", rates$error),
        shiny::br(),
        shiny::actionButton("pwiz_retry_rate", "Reintentar",
                            class = "btn btn-sm btn-warning mt-1",
                            icon  = shiny::icon("rotate-right"))
      ))

    .rate_row <- function(base, date_val, row_dia) {
      if (is.null(base) || is.na(base)) return(NULL)
      total_ann <- base + spread_bps / 100
      is_sel    <- identical(row_dia, dia)
      shiny::div(
        class = paste0("mb-1", if (is_sel) " fw-semibold" else " text-muted"),
        if (is_sel) shiny::tags$span(class = "me-1", "►") else NULL,
        shiny::tags$strong(sprintf("%s %s: ", tipo_lbl, format(date_val, "%d/%m/%Y"))),
        tr("tablas.pasivos.tasa_var.fila", base = sprintf("%.3f", base), pb = sprintf("%.0f", spread_bps),
           nominal = sprintf("%.3f", total_ann), convencion = tr(paste0("tablas.pasivos.convencion.corto.", .conv_var())),
           ear = sprintf("%.3f", pasivos_ear_convencion(total_ann, ppy, .conv_var())))
      )
    }

    shiny::div(
      class = "alert alert-secondary py-2 px-3 small mt-2",
      .rate_row(rates$yesterday, rates$date_yesterday, "ayer"),
      .rate_row(rates$today,     rates$date_today,     "hoy"),
      shiny::tags$small(
        class = "text-muted d-block mt-1",
        if (identical(tipo, "variable_tiie28"))
          "Fuente: Banco de México — SIE API (serie SF43783). Uso conforme a los términos de uso de Banxico."
        else
          sprintf("Fuente: Federal Reserve Bank of St. Louis — FRED® API (serie %s). Uso conforme a FRED Terms of Use.",
                  if (identical(tipo, "variable_prime")) "DPRIME" else "SOFR")
      )
    )
  })

  # When month changes for annual: update day choices using 2024 (leap year) as reference
  shiny::observeEvent(input$pwiz_mes_pago, {
    req(identical(input$pwiz_frecuencia_pago, "anual"))
    mo     <- as.integer(input$pwiz_mes_pago %||% 1L)
    n_days <- as.integer(lubridate::days_in_month(as.Date(sprintf("2024-%02d-01", mo))))
    cur_d  <- min(as.integer(input$pwiz_dia_pago_anual %||% 1L), n_days)
    shiny::updateSelectInput(session, "pwiz_dia_pago_anual",
                             choices  = setNames(seq_len(n_days), seq_len(n_days)),
                             selected = as.character(cur_d))
  }, ignoreInit = TRUE)

  # ── Tasa dynamic labels ─────────────────────────────────────────────────
  output$pwiz_tasa_periodo_label <- shiny::renderUI({
    freq <- input$pwiz_frecuencia_pago %||% "mensual"
    periodo_nombre <- switch(freq,
      mensual = "mes", anual = "año", semanal = "semana", diaria = "día", "período")
    shiny::tags$small(class = "text-muted d-block mb-1",
                      sprintf("Por %s (%%)", periodo_nombre))
  })

  output$pwiz_tasa_nota <- shiny::renderUI({
    shiny::tags$small(
      class = "text-muted d-block mt-1",
      "Tasa efectiva anual (EAR). Tasa por período = (1 + rₙ)^(1/n) − 1"
    )
  })

  # ── Plazo helper text ───────────────────────────────────────────────────
  output$pwiz_plazo_helper_text <- shiny::renderUI({
    freq <- input$pwiz_frecuencia_pago %||% "mensual"
    freq_word <- switch(freq, mensual = "mensuales", anual = "anuales",
                        semanal = "semanales", diaria = "diarias", "")
    shiny::tags$small(
      class = "text-muted d-block mb-2",
      sprintf("Cuotas %s con amortización de capital. No incluye períodos de gracia.",
              freq_word)
    )
  })

  # ── Trigger: + Agregar pasivo ──────────────────────────────────────────────
  shiny::observeEvent(input$pasivos_add_liability, ignoreInit = TRUE, {
    user <- tryCatch(shared$current_user(), error = function(e) "")
    if (!has_capability(user, "pasivos.create_liability")) {
      shiny::showNotification(tr("tablas.pasivos.sin_permiso_crear"), type = "error"); return()
    }
    wiz$mode             <- "create"
    wiz$step_id          <- "categoria"
    wiz$edit_target_id   <- NULL
    wiz$draft            <- list()
    wiz$validation_errors <- list()
    wiz$cargos_list      <- list()
    wiz$impuestos_list   <- list()
    wiz$custom_dates     <- character()
    last_tasa_touched("anual")
    pwiz_rate_dia_rv("ayer")
    pwiz_fetched_rates_rv(list())
    shiny::showModal(.pwiz_modal())
  })

  # ── Trigger: pencil edit ───────────────────────────────────────────────────
  shiny::observeEvent(input$pasivos_edit_liability, ignoreInit = TRUE, {
    lid <- input$pasivos_edit_liability %||% ""
    if (!nzchar(lid)) return()
    user <- tryCatch(shared$current_user(), error = function(e) "")
    if (!has_capability(user, "pasivos.edit_liability")) {
      shiny::showNotification(tr("tablas.pasivos.sin_permiso_editar"), type = "error"); return()
    }
    liabs <- tryCatch(shared$pasivos_liabilities_db(),
                      error = function(e) .schema_pasivos_liability())
    row <- liabs[!is.na(liabs$id) & liabs$id == lid, , drop = FALSE]   # T99: an NA id is not a match
    if (!nrow(row)) {
      shiny::showNotification(tr("tablas.pasivos.no_encontrado"), type = "error"); return()
    }
    if (identical(row$estado[1], "deleted")) {
      shiny::showNotification(tr("tablas.pasivos.no_editar_borrado"), type = "error"); return()
    }
    draft <- pasivos_liability_to_draft(row)
    wiz$mode             <- "edit"
    wiz$edit_target_id   <- lid
    wiz$draft            <- draft
    # Editing opens on Revisión (last step) since reviewing an existing
    # pasivo means wanting the overview first; new pasivos still start at
    # "categoria" (see the sibling pasivos_add_liability observer).
    wiz$step_id          <- utils::tail(.wiz_steps_for(draft$categoria)$ids, 1)
    wiz$validation_errors <- list()
    wiz$cargos_list      <- draft$cargos_iniciales %||% list()
    wiz$impuestos_list   <- draft$impuestos %||% list()
    last_tasa_touched("anual")
    pwiz_rate_dia_rv("ayer")
    pwiz_fetched_rates_rv(list())
    wiz$custom_dates     <- tryCatch(
      as.character(draft$recurrence_params$dates %||% character(0)),
      error = function(e) character()
    )
    shiny::showModal(.pwiz_modal())
  })

  # ── Step indicator ─────────────────────────────────────────────────────────
  output$pwiz_step_indicator <- shiny::renderUI({
    steps <- .wiz_steps_for(wiz$draft$categoria)
    pasivos_wizard_step_indicator(wiz$step_id, steps$ids, steps$labels)
  })

  # ── Step content ───────────────────────────────────────────────────────────
  # T111: the step is drawn again only when the step, the draft or its
  # errors change -- never by the sync bus (company_map, the liabilities):
  # a redraw paints every field from the draft, and the nested rows (taxes,
  # cargos) from the browser's cache, so what was typed since the last
  # Siguiente was lost. The choices are read without a dependency.
  output$pwiz_step_content <- shiny::renderUI({
    empresa_choices <- shiny::isolate(tryCatch(sort(names(shared$company_map())),
                                error = function(e) character(0)))
    existing_subcats <- shiny::isolate(tryCatch({
      liabs <- shared$pasivos_liabilities_db()
      sort(unique(liabs$subcategoria[!is.na(liabs$subcategoria) &
                                       nzchar(liabs$subcategoria %||% "")]))
    }, error = function(e) character()))
    errs <- wiz$validation_errors %||% list()

    switch(wiz$step_id,
      categoria   = pasivos_step_ui_categoria(wiz$draft, errs),
      detalles    = pasivos_step_ui_detalles(wiz$draft, empresa_choices,
                                              existing_subcats, errs, base = base_moneda(),
                                              primero = monedas_primero()),
      recurrencia = pasivos_step_ui_recurrencia(wiz$draft, errs),
      terminos    = pasivos_step_ui_terminos(wiz$draft, errs,
                      convencion_omision = pasivos_convencion_cliente(shiny::isolate(tryCatch(
                        shared$group_config()$convencion_dias, error = function(e) NULL)))),   # T115
      cargos      = pasivos_step_ui_cargos(wiz$draft, wiz$cargos_list, errs, base = base_moneda(),
                                           primero = monedas_primero()),
      revision    = pasivos_step_ui_revision(wiz$draft, errs),
      NULL
    )
  })

  # ── Plazo breakdown (live, shown inside Términos step) ─────────────────────
  output$pwiz_plazo_breakdown <- shiny::renderUI({
    pg        <- as.integer(input$pwiz_periodo_gracia %||% wiz$draft$periodo_gracia %||% 0L)
    pl        <- as.integer(input$pwiz_plazo_meses    %||% wiz$draft$plazo_meses    %||% 0L)
    principal <- as.numeric(input$pwiz_principal_original %||%
                              wiz$draft$principal_original %||% NA_real_)
    tasa_tipo_bd <- input$pwiz_tasa_tipo %||% wiz$draft$tasa_tipo %||% "fija"
    tasa      <- if (identical(tasa_tipo_bd, "fija"))
                   as.numeric(input$pwiz_tasa_anual   %||% wiz$draft$tasa_actual %||% NA_real_)
                 else
                   as.numeric(input$pwiz_tasa_fetched %||% wiz$draft$tasa_actual %||% NA_real_)
    flavor    <- wiz$draft$flavor %||% ""
    freq      <- input$pwiz_frecuencia_pago %||% wiz$draft$frecuencia_pago %||% "mensual"
    ppy_bd    <- switch(freq, mensual=12L, anual=1L, semanal=52L, diaria=365L, 12L)
    if (is.na(pg)) pg <- 0L
    if (is.na(pl)) pl <- 0L
    if (pl <= 0L && pg <= 0L) return(NULL)

    freq_word <- switch(freq, mensual="mensuales", anual="anuales",
                        semanal="semanales", diaria="diarios", "períodos")
    line1 <- sprintf("Plazo total: %d gracia + %d amortización = %d %s",
                     pg, pl, pg + pl, freq_word)

    # Line 2: grace payment — custom override or auto-calculated from tasa
    monto_g  <- as.numeric(input$pwiz_monto_gracia %||% wiz$draft$monto_gracia %||% NA_real_)
    has_custom <- !is.na(monto_g) && monto_g >= 0
    line2 <- if (pg > 0L) {
      if (has_custom) {
        sprintf("Cuota de gracia: %s / período — monto personalizado",
                monto_moneda(monto_g, input$pwiz_moneda_pago %||% wiz$draft$moneda_pago %||% NA_character_))
      } else if (!is.na(principal) && principal > 0) {
        is_puro_flavor <- identical(flavor, "arrendamiento_puro")
        eff_tasa <- if (is_puro_flavor) 0 else (if (is.na(tasa)) NA_real_ else tasa)
        if (is.na(eff_tasa)) {
          "Cuota de gracia: ingresa la tasa para calcular (o usa monto personalizado)"
        } else if (eff_tasa == 0) {
          "Cuota de gracia: $0.00 — sin pago (tasa = 0%)"
        } else {
          grace_pmt <- principal * ((1 + eff_tasa/100)^(1/ppy_bd) - 1)
          sprintf("Cuota de gracia: %s / período — solo interés (auto)",
                  monto_moneda(grace_pmt, input$pwiz_moneda_pago %||% wiz$draft$moneda_pago %||% NA_character_))
        }
      } else NULL
    } else NULL

    shiny::div(
      class = "alert alert-secondary py-1 px-2 small mt-1 mb-0",
      shiny::div(line1),
      if (!is.null(line2)) shiny::div(class = "mt-1", line2) else NULL
    )
  })

  # ── Dynamic periodicidad sub-fields ────────────────────────────────────────
  # T70: what the end date means, under it, as it changes
  output$pwiz_fecha_fin_ayuda <- shiny::renderUI({
    fin <- .wiz_leer_fecha(input$pwiz_fecha_fin)
    shiny::tags$small(class = "text-muted d-block",
      if (is.na(fin)) tr("tablas.pasivos.fin.sin_fin")
      else tr("tablas.pasivos.fin.con_fin", fecha = format(fin, "%d/%m/%Y")))
  })

  output$pwiz_periodicidad_subfields <- shiny::renderUI({
    rt <- input$pwiz_periodicidad %||% (wiz$draft$recurrence_type %||% "monthly_day")
    pasivos_periodicidad_subfields_ui(rt, wiz$draft)
  })

  # ── Custom date list ────────────────────────────────────────────────────────
  output$pwiz_rp_custom_dates_list <- shiny::renderUI({
    dates <- wiz$custom_dates
    if (!length(dates)) return(shiny::div(class = "text-muted small", "Sin fechas añadidas."))
    shiny::tagList(lapply(seq_along(dates), function(i) {
      shiny::div(
        class = "d-flex align-items-center gap-2 mb-1",
        shiny::tags$span(class = "small", dates[i]),
        shiny::actionButton(paste0("pwiz_rm_custom_date_", i),
                            "×", class = "btn btn-outline-danger btn-sm py-0")
      )
    }))
  })

  shiny::observeEvent(input$pwiz_rp_custom_add_btn, ignoreInit = TRUE, {
    d <- tryCatch(as.Date(input$pwiz_rp_custom_add %||% NA), error = function(e) NA)
    if (!is.na(d)) {
      new_dates <- sort(unique(c(wiz$custom_dates, as.character(d))))
      wiz$custom_dates <- new_dates
    }
  })

  # Remove custom date buttons (observe dynamically)
  shiny::observe({
    dates <- wiz$custom_dates
    lapply(seq_along(dates), function(i) {
      local({
        idx <- i
        shiny::observeEvent(input[[paste0("pwiz_rm_custom_date_", idx)]],
                            ignoreInit = TRUE, once = TRUE, {
          wiz$custom_dates <- wiz$custom_dates[-idx]
        })
      })
    })
  })

  # ── Impuestos rows (T63) ────────────────────────────────────────────────────
  output$pwiz_impuestos_table <- shiny::renderUI({
    il <- wiz$impuestos_list
    if (!length(il)) return(shiny::div(class = "text-muted small mb-2", tr("tablas.pasivos.impuestos.ninguno")))
    fin <- identical(shiny::isolate(wiz$draft$categoria) %||% "", "financiero")
    do.call(shiny::tagList, lapply(seq_along(il), function(i) pasivos_impuesto_row_ui(i, il[[i]], financiero = fin)))
  })
  shiny::observeEvent(input$pwiz_add_impuesto, ignoreInit = TRUE, {
    il <- .wiz_leer_impuestos(input, length(wiz$impuestos_list), wiz$draft$categoria, todas = TRUE)
    wiz$impuestos_list <- c(il, list(list(nombre = "", tipo = "porcentual", valor = NA_real_, base = "pago")))
  })
  shiny::observe({
    n <- length(wiz$impuestos_list)
    lapply(seq_len(n), function(i) local({
      idx <- i
      shiny::observeEvent(input[[paste0("pwiz_imp_rm_", idx)]], ignoreInit = TRUE, once = TRUE, {
        il <- .wiz_leer_impuestos(input, length(wiz$impuestos_list), wiz$draft$categoria, todas = TRUE)
        if (idx <= length(il)) wiz$impuestos_list <- il[-idx]
      })
    }))
  })

  # ── Cargo rows table ────────────────────────────────────────────────────────
  output$pwiz_cargos_table <- shiny::renderUI({
    cl <- wiz$cargos_list
    if (!length(cl)) return(shiny::div(class = "text-muted small mb-2",
                                        "Sin cargos iniciales."))
    do.call(shiny::tagList, lapply(seq_along(cl), function(i)
      pasivos_cargo_row_ui(i, cl[[i]], base = base_moneda(), primero = monedas_primero())
    ))
  })

  # The cargo rows as typed (the list itself only changes on add / remove).
  .wiz_cargos_escritos <- function(cl = wiz$cargos_list) {
    for (i in seq_along(cl)) {
      cl[[i]]$desc   <- input[[paste0("pwiz_cargo_desc_",   i)]] %||% cl[[i]]$desc
      cl[[i]]$monto  <- input[[paste0("pwiz_cargo_monto_",  i)]] %||% cl[[i]]$monto
      cl[[i]]$moneda <- input[[paste0("pwiz_cargo_moneda_", i)]] %||% cl[[i]]$moneda
      cl[[i]]$fecha  <- tryCatch(as.Date(input[[paste0("pwiz_cargo_fecha_", i)]]),
                                 error = function(e) cl[[i]]$fecha)
    }
    cl
  }

  shiny::observeEvent(input$pwiz_add_cargo, ignoreInit = TRUE, {
    cl <- .wiz_cargos_escritos()
    wiz$cargos_list <- c(cl, list(list(desc = "", monto = 0,
                                       moneda = base_moneda(), fecha = Sys.Date())))
  })

  shiny::observe({
    cl <- wiz$cargos_list
    lapply(seq_along(cl), function(i) {
      local({
        idx <- i
        shiny::observeEvent(input[[paste0("pwiz_cargo_rm_", idx)]],
                            ignoreInit = TRUE, once = TRUE, {
          lst <- .wiz_cargos_escritos()
          if (idx <= length(lst)) wiz$cargos_list <- lst[-idx]
        })
      })
    })
  })

  # ── Parte: live match suggestions ─────────────────────────────────────────
  output$pwiz_parte_suggestions <- shiny::renderUI({
    q <- input$pwiz_parte %||% ""
    if (!nzchar(trimws(q))) return(NULL)
    prov <- tryCatch(shared$proveedores_db(), error = function(e) NULL)
    if (is.null(prov) || !nrow(prov)) return(NULL)
    matches <- tryCatch(
      find_proveedor_matches(
        query          = list(parte = q, rfc = "", no_cuenta = "", alias = ""),
        proveedores_df = prov,
        threshold      = 15L,
        top_n          = 6L
      ),
      error = function(e) NULL
    )
    if (is.null(matches) || !nrow(matches)) return(NULL)
    shiny::div(
      class = "mt-1 mb-2 border rounded p-2 bg-white shadow-sm",
      style = "max-height:220px; overflow-y:auto; font-size:13px;",
      lapply(seq_len(nrow(matches)), function(i) {
        m      <- matches[i, ]
        nom    <- m$nombre %||% ""
        ali    <- m$alias  %||% ""
        cod    <- ali  # alias is the vendor CardCode/código
        sc     <- min(as.integer(m$.score %||% 0L), 100L)
        payload <- jsonlite::toJSON(
          list(nombre = nom, codigo = cod),
          auto_unbox = TRUE
        )
        shiny::div(
          class = "d-flex align-items-start gap-2 py-1 px-1 rounded",
          style = "cursor:pointer;",
          onmouseover = "this.style.background='var(--hd-hover)'",
          onmouseout  = "this.style.background=''",
          onclick = sprintf(
            "Shiny.setInputValue('pwiz_parte_pick',%s,{priority:'event'})", payload),
          shiny::div(
            class = "flex-grow-1",
            shiny::tags$div(class = "fw-semibold", nom),
            if (nzchar(ali)) shiny::tags$div(class = "text-muted small", ali) else NULL
          ),
          shiny::tags$span(
            class = "badge rounded-pill ms-auto",
            style = "background:var(--hd-leg-texto-2-6c757d); font-size:11px; align-self:center;",
            paste0(sc, "%")
          )
        )
      })
    )
  })

  shiny::observeEvent(input$pwiz_parte_pick, ignoreInit = TRUE, {
    pick <- input$pwiz_parte_pick
    if (is.null(pick)) return()
    nom <- if (is.list(pick)) pick$nombre %||% "" else ""
    cod <- if (is.list(pick)) pick$codigo %||% "" else ""
    if (nzchar(nom))
      shiny::updateTextInput(session, "pwiz_parte", value = nom)
    shiny::updateTextInput(session, "pwiz_codigo_parte", value = cod)
  })

  # ── Cotizado auto-fill ──────────────────────────────────────────────────────
  shiny::observeEvent(input$pwiz_amount_cotizado, ignoreInit = TRUE, {
    cot_en  <- input$pwiz_cotizado_en %||% ""
    mon_pag <- input$pwiz_moneda_pago %||% base_moneda()
    if (!nzchar(cot_en)) return()
    cot_amt <- input$pwiz_amount_cotizado %||% NA_real_
    if (is.na(cot_amt)) return()
    res <- tryCatch(
      pasivos_recompute_pago_from_cotizado(cot_amt, cot_en, mon_pag, Sys.Date(),
                                           client_id = tryCatch(shared$effective_client_id(), error = function(e) NULL)),
      error = function(e) list(amount_pago = NA_real_, fx_rate_used = NA_real_)
    )
    if (!is.na(res$amount_pago)) {
      hd_numero_update(session, "pwiz_amount_default",
                                value = round(res$amount_pago, 2))
    }
  })

  # Auto badge
  output$pwiz_amount_auto_badge <- shiny::renderUI({
    cot_en <- input$pwiz_cotizado_en %||% ""
    if (nzchar(cot_en))
      shiny::tags$span(class = "badge bg-info text-dark small", "auto")
    else
      NULL
  })

  output$pwiz_cotizado_label <- shiny::renderUI({
    cot <- input$pwiz_cotizado_en %||% ""
    sprintf("Cotizado en %s", if (nzchar(cot)) cot else "—")
  })

  output$pwiz_fx_info <- shiny::renderUI({
    cot_en  <- input$pwiz_cotizado_en %||% ""
    mon_pag <- input$pwiz_moneda_pago %||% base_moneda()
    if (!nzchar(cot_en)) return(NULL)
    # The Banxico FIX (R/fx_banxico.R, fx_fix_aplica()); any other pair has none.
    fx <- if (fx_fix_aplica(cot_en, mon_pag))
      tryCatch(fx_fix(), error = function(e) NULL) else NULL
    shiny::tags$small(class = "text-muted",
      if (is.list(fx)) tr("tablas.pasivos.asistente.fx_fix", valor = formatC(fx$valor, format = "f", digits = 4),
                          fecha = format(fx$fecha, "%d/%m/%Y"))
      else tr("tablas.pasivos.asistente.fx_sin")
    )
  })

  # ── CSV parse error ────────────────────────────────────────────────────────
  output$pwiz_csv_parse_error <- shiny::renderUI({
    csv_raw <- input$pwiz_schedule_csv %||% ""
    if (!nzchar(trimws(csv_raw))) return(NULL)
    parsed <- tryCatch(
      utils::read.csv(text = csv_raw, stringsAsFactors = FALSE),
      error = function(e) e
    )
    if (inherits(parsed, "error"))
      return(shiny::div(class = "alert alert-danger small py-1",
                        paste0("Error: ", conditionMessage(parsed))))
    needed <- c("periodo", "fecha", "capital", "interes", "fees")
    missing_cols <- setdiff(needed, names(parsed))
    if (length(missing_cols))
      return(shiny::div(class = "alert alert-warning small py-1",
                        paste0("Faltan columnas: ", paste(missing_cols, collapse = ", "))))
    shiny::div(class = "alert alert-success small py-1",
               sprintf("Tabla parseada: %d filas, columnas OK.", nrow(parsed)))
  })

  # ── Revision outputs ────────────────────────────────────────────────────────
  output$pwiz_revision_summary <- shiny::renderUI({
    .wiz_revision_summary_html(wiz$draft)
  })

  output$pwiz_revision_schedule <- shiny::renderUI({
    .wiz_revision_schedule_html(wiz$draft)
  })

  # T109: the amortization table's title and its switch between the next
  # payments and the whole contract (the same table, inside this modal: a
  # second modal would close the wizard).
  output$pwiz_amort_cabecera <- shiny::renderUI({
    completa <- identical(wiz$amort_vista, "completa")
    shiny::div(class = "d-flex align-items-baseline gap-3 mb-1",
      shiny::tags$h6(class = "mb-0", tr(if (completa) "tablas.pasivos.amort.titulo_completa" else "tablas.pasivos.amort.titulo_proximas")),
      shiny::actionLink("pwiz_amort_vista", class = "small",
                        tr(if (completa) "tablas.pasivos.amort.ver_proximas" else "tablas.pasivos.amort.ver_completa")))
  })
  shiny::observeEvent(input$pwiz_amort_vista, {
    wiz$amort_vista <- if (identical(wiz$amort_vista, "completa")) "proximas" else "completa"
  }, ignoreInit = TRUE)

  output$pwiz_revision_notice <- shiny::renderUI({
    shiny::tagList(
      shiny::tags$span(pasivos_aviso_generacion(wiz$draft)),   # T70b
      # T70/T71, before saving: what an earlier end date and a new note do to
      # the provisions this liability already has.
      if (identical(wiz$mode, "edit")) {
        provs <- tryCatch(shared$pasivos_provisions_db(), error = function(e) NULL)
        liabs <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)
        pasivos_aviso_edicion_html(wiz$draft, wiz$edit_target_id, provs, liabs)
      }
    )
  })

  # ── Modal title ────────────────────────────────────────────────────────────
  output$pwiz_modal_title <- shiny::renderUI({
    if (identical(wiz$mode, "edit")) "Editar pasivo" else "Nuevo pasivo"
  })

  # ── Footer ─────────────────────────────────────────────────────────────────
  output$pwiz_footer <- shiny::renderUI({
    steps    <- .wiz_steps_for(wiz$draft$categoria)
    step_ids <- steps$ids
    cur_idx  <- match(wiz$step_id, step_ids) %||% 1L
    n        <- length(step_ids)

    cancel_btn <- shiny::actionButton(
      "pwiz_cancel", "Cancelar", class = "btn btn-secondary")

    back_btn <- if (cur_idx > 1L)
      shiny::actionButton("pwiz_back", "Atrás", class = "btn btn-outline-secondary")
    else NULL

    next_btn <- if (cur_idx < n)
      shiny::actionButton("pwiz_next", "Siguiente", class = "btn btn-primary")
    else NULL

    save_btn <- if (cur_idx == n)
      shiny::actionButton("pwiz_save", "Guardar", class = "btn btn-primary")
    else NULL

    shiny::tagList(cancel_btn, back_btn, next_btn, save_btn)
  })

  # ── Navigation ─────────────────────────────────────────────────────────────
  # T111: leaving a step -- Siguiente, Atrás, a step's name, Guardar -- keeps
  # what was typed in it, BEFORE anything is drawn again: the draft (every
  # field), and the tax and cargo rows, which are nested outputs the browser
  # repaints from its cache (for a new liability, the empty row "+ Agregar"
  # made: the IVA typed in it was dropped). Atrás and a step's name used to
  # leave without reading the step at all.
  .wiz_guardar_paso <- function(leer_paso = TRUE) {
    paso <- wiz$step_id
    if (identical(paso, "cargos")) {
      wiz$impuestos_list <- .wiz_leer_impuestos(input, length(wiz$impuestos_list), wiz$draft$categoria, todas = TRUE)
      wiz$cargos_list    <- .wiz_cargos_escritos()
    }
    if (leer_paso) wiz$draft <- tryCatch(.wiz_collect_step(input, paso, wiz), error = function(e) wiz$draft)
  }

  shiny::observeEvent(input$pwiz_next, ignoreInit = TRUE, {
    wiz$validation_errors <- list()
    .wiz_guardar_paso(leer_paso = FALSE)
    wiz$draft <- .wiz_collect_step(input, wiz$step_id, wiz)

    # Validate current step before advancing
    errs <- switch(wiz$step_id,
      detalles    = {
        ec <- tryCatch(sort(names(shared$company_map())),
                       error = function(e) character(0))
        pasivos_wizard_validate_detalles(wiz$draft, ec)$errors
      },
      recurrencia = pasivos_wizard_validate_recurrencia(wiz$draft)$errors,
      terminos    = pasivos_wizard_validate_terminos(wiz$draft)$errors,
      cargos      = pasivos_wizard_validate_cargos(wiz$draft)$errors,
      list()
    )
    if (length(errs)) {
      wiz$validation_errors <- errs
      return()
    }

    steps   <- .wiz_steps_for(wiz$draft$categoria)
    cur_idx <- match(wiz$step_id, steps$ids) %||% 1L
    if (cur_idx < length(steps$ids))
      wiz$step_id <- steps$ids[cur_idx + 1L]
  })

  shiny::observeEvent(input$pwiz_back, ignoreInit = TRUE, {
    wiz$validation_errors <- list()
    .wiz_guardar_paso()
    steps   <- .wiz_steps_for(wiz$draft$categoria)
    cur_idx <- match(wiz$step_id, steps$ids) %||% 1L
    if (cur_idx > 1L) wiz$step_id <- steps$ids[cur_idx - 1L]
  })

  shiny::observeEvent(input$pwiz_goto_step, ignoreInit = TRUE, {
    target <- input$pwiz_goto_step %||% ""
    steps  <- .wiz_steps_for(wiz$draft$categoria)
    cur_idx <- match(wiz$step_id, steps$ids) %||% 1L
    tgt_idx <- match(target, steps$ids)
    if (!is.na(tgt_idx) && tgt_idx < cur_idx) {
      wiz$validation_errors <- list()
      .wiz_guardar_paso()
      wiz$step_id <- target
    }
  })

  shiny::observeEvent(input$pwiz_save, ignoreInit = TRUE, {
    if (!pasivos_autoriza("pasivos.edit_liability")) return()
    shinyjs::disable("pwiz_save")
    on.exit(shinyjs::enable("pwiz_save"), add = TRUE)

    .wiz_guardar_paso(leer_paso = FALSE)
    wiz$draft <- .wiz_collect_step(input, wiz$step_id, wiz)
    user <- tryCatch(shared$current_user(), error = function(e) "system")
    res <- tryCatch(
      .pasivos_wizard_save(wiz, shared, session, user),
      error = function(e) {
        message("[wizard] unhandled error in save: ", conditionMessage(e))
        list(ok = FALSE, msg = tr("tablas.pasivos.error_inesperado", detalle = conditionMessage(e)))
      }
    )
    if (!res$ok) {
      shiny::showNotification(res$msg, type = "error", duration = 15)
    }
  })

  shiny::observeEvent(input$pwiz_cancel, ignoreInit = TRUE, {
    has_data <- length(wiz$draft) > 0 && any(nzchar(unlist(wiz$draft[c("nombre","parte","empresa")]) %||% ""))
    if (has_data) {
      shiny::showModal(shiny::modalDialog(
        "¿Descartar cambios?",
        footer = shiny::tagList(
          shiny::actionButton("pwiz_cancel_no",  "Continuar editando", class = "btn btn-secondary"),
          shiny::actionButton("pwiz_cancel_yes", "Descartar",          class = "btn btn-danger")
        ),
        size = "s"
      ))
    } else {
      shiny::removeModal()
    }
  })

  shiny::observeEvent(input$pwiz_cancel_no,  ignoreInit = TRUE, {
    shiny::removeModal()
    shiny::showModal(.pwiz_modal())
  })
  shiny::observeEvent(input$pwiz_cancel_yes, ignoreInit = TRUE, {
    shiny::removeModal()
  })

  # T106: the documents of the liability open in the wizard (R/adjuntos_pasivo_ui.R).
  setup_adjuntos_pasivo(input, output, session, shared, entidad_id = shiny::reactive(wiz$edit_target_id))

  invisible(NULL)
}

# ── Modal wrapper ──────────────────────────────────────────────────────────────
.pwiz_modal <- function() {
  shiny::modalDialog(
    title     = shiny::uiOutput("pwiz_modal_title"),
    size      = "xl",   # T109: the amortization table read whole at 1293 px (Diseñador, 2026-11-25)
    easyClose = FALSE,
    shiny::uiOutput("pwiz_step_indicator"),
    shiny::uiOutput("pwiz_step_content"),
    footer = shiny::uiOutput("pwiz_footer")
  )
}
