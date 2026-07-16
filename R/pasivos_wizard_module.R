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
    empresa                = r$empresa[1] %||% "",
    parte                  = r$parte[1] %||% "",
    codigo_parte           = r$codigo_parte[1] %||% "",
    referencia_default     = r$referencia_default[1] %||% "",
    documento_template     = r$documento_template[1] %||% "",
    subcategoria           = r$subcategoria[1] %||% "",
    moneda_pago            = r$moneda_pago[1] %||% "MXN",
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
    valor_residual         = r$valor_residual[[1]],
    notas                  = r$notas[1] %||% ""
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
    biweekly           = list(anchor_date = as.Date(input$pwiz_rp_anchor %||% Sys.Date())),
    weekly             = list(wday = as.integer(input$pwiz_rp_wday %||% 1L)),
    quarterly          = list(
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
                                               edit_target_id = NULL, user = "system") {
  now <- Sys.time()
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
    empresa                  = draft$empresa %||% "",
    parte                    = draft$parte %||% "",
    codigo_parte             = draft$codigo_parte %||% NA_character_,
    referencia_default       = if (nzchar(draft$referencia_default %||% "")) draft$referencia_default else NA_character_,
    documento_template       = if (nzchar(draft$documento_template %||% "")) draft$documento_template else NA_character_,
    moneda_pago              = draft$moneda_pago %||% "MXN",
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
    valor_residual           = residual,
    modifier_ids             = list(character(0)),
    estado                   = "active",
    notas                    = draft$notas %||% NA_character_,
    created_by               = user,
    created_at               = now,
    updated_by               = user,
    updated_at               = now
  )
}

# ── Save handler ───────────────────────────────────────────────────────────────
.pasivos_wizard_save <- function(wiz, shared, session, user) {
  empresa_choices <- tryCatch(sort(names(shared$company_map())),
                              error = function(e) sort(names(COMPANY_MAP)))
  validation <- pasivos_wizard_validate_final(wiz$draft, empresa_choices)
  if (!validation$ok) {
    wiz$validation_errors <- validation$errors
    return(list(ok = FALSE, msg = "Hay errores. Revisa los campos resaltados."))
  }

  liab_row <- tryCatch(
    pasivos_wizard_draft_to_liability(
      wiz$draft,
      mode           = wiz$mode,
      edit_target_id = wiz$edit_target_id,
      user           = user
    ),
    error = function(e) e
  )
  if (inherits(liab_row, "error")) {
    return(list(ok = FALSE,
                msg = paste0("Error al construir el pasivo: ", conditionMessage(liab_row))))
  }

  # Persist liability
  liabs <- tryCatch(shared$pasivos_liabilities_db(),
                    error = function(e) .schema_pasivos_liability())
  if (wiz$mode == "edit") {
    liabs <- liabs[liabs$id != liab_row$id, , drop = FALSE]
  }
  liabs <- dplyr::bind_rows(liabs, liab_row)
  save_ok <- tryCatch({
    save_pasivos_liabilities(liabs, client_id = shared$effective_client_id()); TRUE
  }, error = function(e) {
    message("[wizard] save_pasivos_liabilities failed: ", conditionMessage(e))
    FALSE
  })
  if (!save_ok) return(list(ok = FALSE, msg = "Error al guardar el pasivo en S3. Intenta de nuevo."))
  shared$pasivos_liabilities_db(liabs)
  bump_sync_version("pasivos_liabilities_db")

  # Record current fetched base rate in the forecasting store so provisions use it
  if (startsWith(liab_row$tasa_tipo %||% "fija", "variable_") &&
      !is.na(liab_row$tasa_actual %||% NA_real_)) {
    metric_map <- c(variable_sofr = "sofr", variable_tiie28 = "tiie28")
    fc_metric  <- metric_map[[liab_row$tasa_tipo %||% ""]]
    if (!is.null(fc_metric))
      tryCatch(
        forecasting_set_estimate(fc_metric, Sys.Date(), liab_row$tasa_actual,
                                 source_method = "wizard_save", user = user,
                                 client_id = shared$effective_client_id()),
        error = function(e) warning("[wizard] forecasting_set_estimate failed: ", conditionMessage(e))
      )
  }

  # Generate provisions
  holidays <- tryCatch(shared$holiday_overrides_db(), error = function(e) list())

  gen_err <- character(0)
  generated <- tryCatch(
    pasivos_generate_provisions(
      liability              = liab_row,
      window_start           = Sys.Date(),
      window_end             = NULL,
      policies_for_liability = list(),
      holidays_cache         = holidays
    ),
    error = function(e) {
      gen_err <<- conditionMessage(e)
      message("[wizard] pasivos_generate_provisions failed: ", gen_err)
      .schema_pasivos_provision()
    }
  )
  if (!nrow(generated)) {
    err_detail <- if (length(gen_err)) gen_err else "Sin filas generadas (revisa consola)"
    shiny::showNotification(
      paste0("Pasivo guardado. Error generando provisiones: ", err_detail),
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
  result <- tryCatch(
    pasivos_reconcile_provisions(existing_for_this, generated, mode = reco_mode),
    error = function(e) list(keep = .schema_pasivos_provision(),
                              update = .schema_pasivos_provision(),
                              insert = generated,
                              conflicts = .schema_pasivos_provision())
  )

  # Apply: keep + update + insert
  others <- existing[
    is.na(existing$liability_id) | existing$liability_id != liab_row$id,
    , drop = FALSE
  ]
  new_provs <- dplyr::bind_rows(others, result$keep, result$update, result$insert)
  tryCatch(save_pasivos_provisions(new_provs, client_id = shared$effective_client_id()),
           error = function(e) warning("[wizard] save_pasivos_provisions failed: ",
                                       conditionMessage(e)))
  tryCatch(shared$suppress_ledger_prov_refresh(TRUE), error = function(e) NULL)
  shared$pasivos_provisions_db(new_provs)
  bump_sync_version("pasivos_provisions_db")

  # Audit
  tryCatch(pasivos_log_audit(
    action_type = if (wiz$mode == "create") "liability.created" else "liability.edited",
    user        = user,
    empresa     = liab_row$empresa,
    target_kind = "liability",
    target_id   = liab_row$id,
    after       = as.list(liab_row[, c("id","categoria","nombre","empresa","estado")]),
    notes       = sprintf("Generated %d, updated %d, conflicts %d",
                          nrow(result$insert), nrow(result$update),
                          nrow(result$conflicts)),
    client_id   = shared$effective_client_id()
  ), error = function(e) NULL)

  shiny::removeModal()

  conflicts <- result$conflicts
  n_ins <- nrow(result$insert) + nrow(result$update)
  if (nrow(conflicts)) {
    shiny::showNotification(
      sprintf("Pasivo guardado. %d provisiones requieren tu confirmación.", nrow(conflicts)),
      type = "warning", duration = 5
    )
    pasivos_edit_confirm_open(
      session     = session,
      conflicts   = conflicts,
      replacement = result$update,
      shared      = shared,
      liability_id = liab_row$id
    )
  } else {
    shiny::showNotification(
      sprintf("Pasivo guardado. %d provisiones generadas.", n_ins),
      type = "message", duration = 3
    )
  }

  list(ok = TRUE)
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
      d$empresa             <- input$pwiz_empresa             %||% ""
      d$parte               <- input$pwiz_parte               %||% ""
      d$codigo_parte        <- input$pwiz_codigo_parte        %||% ""
      d$referencia_default  <- input$pwiz_referencia_default  %||% ""
      d$documento_template  <- input$pwiz_documento_template  %||% ""
      d$subcategoria        <- input$pwiz_subcategoria        %||% ""
      d$moneda_pago         <- input$pwiz_moneda_pago         %||% "MXN"
      d$cotizado_en         <- input$pwiz_cotizado_en         %||% ""
      if (identical(d$categoria, "tarjeta")) {
        d$tarjeta_closing_day  <- as.integer(input$pwiz_tarjeta_closing_day %||% NA_integer_)
        d$tarjeta_due_day      <- as.integer(input$pwiz_tarjeta_due_day     %||% NA_integer_)
        d$tarjeta_credit_limit <- input$pwiz_tarjeta_credit_limit %||% NA_real_
      }
    },

    recurrencia = {
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
          moneda = input[[paste0("pwiz_cargo_moneda_", i)]] %||% "MXN",
          fecha  = tryCatch(as.Date(input[[paste0("pwiz_cargo_fecha_", i)]] %||% NA),
                            error = function(e) Sys.Date())
        )
      })
      d$cargos_iniciales <- if (length(cargos_out)) cargos_out else NULL

      # Residual
      if (isTRUE(input$pwiz_has_residual)) {
        d$valor_residual <- list(
          monto    = input$pwiz_residual_monto    %||% NA_real_,
          moneda   = input$pwiz_residual_moneda   %||% "MXN",
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
      draft$moneda_pago %||% "MXN",
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
      tasa_periodo_rev <- round(((1 + tasa_anual/100)^(1/ppy_rev) - 1) * 100, 3)
      if (!identical(tipo_t, "fija")) {
        ref_lbl <- if (identical(tipo_t, "variable_sofr")) "SOFR" else "TIIE28"
        sprintf("%s %.3f%% + %.0f bps sobretasa = %.3f%% anual (%.3f%% %s)",
                ref_lbl, tasa_base, spread_bps, tasa_anual, tasa_periodo_rev, periodo_nombre)
      } else {
        sprintf("%.3f%% anual (%.3f%% %s)", tasa_anual, tasa_periodo_rev, periodo_nombre)
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
      c("Principal",    sprintf("%.2f", draft$principal_original %||% 0)),
      c("Tasa",         tasa_label),
      c("Plazo",        plazo_label),
      c("Amortización", metodo_label)
    ))
  } else if (cat %in% c("regular", "tarjeta")) {
    rows <- c(rows, list(
      c("Monto", sprintf("%.2f", draft$amount_default %||% 0))
    ))
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

# ── Amortization preview ───────────────────────────────────────────────────────
.wiz_revision_schedule_html <- function(draft) {
  cat <- draft$categoria %||% ""

  if (identical(cat, "financiero")) {
    # For preview, lock variable rates to the current fetched value (tasa_actual + spread)
    # so the table isn't poisoned by stale forecasting-store data.
    preview_draft <- draft
    if (startsWith(draft$tasa_tipo %||% "fija", "variable_") &&
        !is.na(draft$tasa_actual %||% NA_real_)) {
      preview_draft$tasa_actual <- (draft$tasa_actual %||% 0) + (draft$tasa_spread %||% 0) / 100
      preview_draft$tasa_tipo   <- "fija"
      preview_draft$tasa_spread <- 0
    }
    liab_row <- tryCatch(
      pasivos_wizard_draft_to_liability(preview_draft, mode = "create", user = "preview"),
      error = function(e) NULL
    )
    if (is.null(liab_row)) {
      return(shiny::div(class = "text-muted small",
                        "No se puede generar la vista previa con los datos actuales."))
    }
    sched <- tryCatch(
      pasivos_generate_schedule(liab_row, today = Sys.Date()),
      error = function(e) NULL
    )
    if (is.null(sched) || !nrow(sched)) {
      return(shiny::div(class = "text-muted small", "Sin datos de amortización."))
    }

    future_rows <- sched[!isTRUE_safe(sched$historico), , drop = FALSE]
    preview_top <- head(future_rows, 12)
    preview_bot <- tail(future_rows, 3)

    has_remo <- any(!is.na(sched$re_amortized) & sched$re_amortized, na.rm = TRUE)

    shiny::tagList(
      if (has_remo) shiny::div(
        class = "alert alert-warning small py-1 mb-2",
        "⚠ Re-amortización: el saldo declarado difiere del teórico. Las cuotas se recalcularon."
      ) else NULL,
      shiny::tags$table(
        class = "table table-sm table-borderless small mb-0",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("#"), shiny::tags$th("Fecha"),
            shiny::tags$th("Capital"), shiny::tags$th("Interés"),
            shiny::tags$th("Total")
          )
        ),
        shiny::tags$tbody(lapply(seq_len(nrow(preview_top)), function(i) {
          row <- preview_top[i, ]
          shiny::tags$tr(
            shiny::tags$td(row$periodo),
            shiny::tags$td(format(row$fecha, "%Y-%m-%d")),
            shiny::tags$td(fmt_money(row$capital %||% 0)),
            shiny::tags$td(fmt_money(row$interes %||% 0)),
            shiny::tags$td(class = "fw-semibold", fmt_money(row$total %||% 0))
          )
        })),
        if (nrow(preview_bot)) shiny::tags$tbody(
          shiny::tags$tr(shiny::tags$td(colspan = 5,
            class = "text-center text-muted", "…")),
          lapply(seq_len(nrow(preview_bot)), function(i) {
            row <- preview_bot[i, ]
            shiny::tags$tr(
              shiny::tags$td(row$periodo),
              shiny::tags$td(format(row$fecha, "%Y-%m-%d")),
              shiny::tags$td(fmt_money(row$capital %||% 0)),
              shiny::tags$td(fmt_money(row$interes %||% 0)),
              shiny::tags$td(class = "fw-semibold", fmt_money(row$total %||% 0))
            )
          })
        ) else NULL
      )
    )

  } else {
    # Regular / tarjeta: show next 12 provision dates
    liab_row <- tryCatch(
      pasivos_wizard_draft_to_liability(draft, mode = "create", user = "preview"),
      error = function(e) NULL
    )
    if (is.null(liab_row)) return(shiny::div(class = "text-muted small",
                                              "Sin vista previa disponible."))
    provs <- tryCatch(
      pasivos_generate_provisions(liab_row, window_start = Sys.Date(),
                                   policies_for_liability = list(),
                                   holidays_cache = list()),
      error = function(e) NULL
    )
    if (is.null(provs) || !nrow(provs)) return(
      shiny::div(class = "text-muted small", "Sin provisiones para mostrar."))
    preview <- head(provs, 12)
    shiny::tags$table(
      class = "table table-sm table-borderless small mb-0",
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th("Fecha"), shiny::tags$th("Monto")
      )),
      shiny::tags$tbody(lapply(seq_len(nrow(preview)), function(i) {
        row <- preview[i, ]
        shiny::tags$tr(
          shiny::tags$td(format(row$fecha_efectiva, "%Y-%m-%d")),
          shiny::tags$td(fmt_money(row$amount_pago %||% 0))
        )
      }))
    )
  }
}

# ── Main setup function ────────────────────────────────────────────────────────
setup_pasivos_wizard <- function(input, output, session, shared) {

  wiz <- shiny::reactiveValues(
    mode             = "create",
    step_id          = "categoria",
    edit_target_id   = NULL,
    draft            = list(),
    validation_errors = list(),
    cargos_list      = list(),     # in-progress cargo rows for the cargos step
    custom_dates     = character() # accumulating dates for "custom" recurrence
  )

  # ── Rate dual-input: last-touched tracking ────────────────────────────────
  last_tasa_touched    <- shiny::reactiveVal("anual")
  pwiz_rate_dia_rv     <- shiny::reactiveVal("ayer")   # "hoy" | "ayer"
  pwiz_fetched_rates_rv <- shiny::reactiveVal(list())  # list(today, yesterday, ...)

  # Blur-based cross-update: fires only when user LEAVES the field (tab/click away).
  # Server-side updateNumericInput calls do NOT trigger blur, so no feedback loop.
  shiny::observeEvent(input$pwiz_tasa_anual_blur, {
    last_tasa_touched("anual")
    freq  <- input$pwiz_frecuencia_pago %||% "mensual"
    ppy   <- .periods_per_year(freq)
    anual <- input$pwiz_tasa_anual_blur
    if (!is.null(anual) && !is.na(anual))
      shiny::updateNumericInput(session, "pwiz_tasa_periodo",
                                value = round(((1 + anual/100)^(1/ppy) - 1) * 100, 3))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$pwiz_tasa_periodo_blur, {
    last_tasa_touched("periodo")
    freq   <- input$pwiz_frecuencia_pago %||% "mensual"
    ppy    <- .periods_per_year(freq)
    period <- input$pwiz_tasa_periodo_blur
    if (!is.null(period) && !is.na(period))
      shiny::updateNumericInput(session, "pwiz_tasa_anual",
                                value = round(((1 + period/100)^ppy - 1) * 100, 3))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$pwiz_frecuencia_pago, {
    freq <- input$pwiz_frecuencia_pago %||% "mensual"
    ppy  <- .periods_per_year(freq)
    ltt  <- last_tasa_touched()
    if (identical(ltt, "anual") ||
        is.null(input$pwiz_tasa_periodo) || is.na(input$pwiz_tasa_periodo)) {
      anual <- input$pwiz_tasa_anual
      if (!is.null(anual) && !is.na(anual))
        shiny::updateNumericInput(session, "pwiz_tasa_periodo",
                                  value = round(((1 + anual/100)^(1/ppy) - 1) * 100, 3))
    } else {
      period <- input$pwiz_tasa_periodo
      if (!is.null(period) && !is.na(period))
        shiny::updateNumericInput(session, "pwiz_tasa_anual",
                                  value = round(((1 + period/100)^ppy - 1) * 100, 3))
    }
  }, ignoreInit = TRUE)

  # ── Variable rate: fetch on tipo change or step entry ─────────────────────
  .do_fetch_rates <- function(tipo) {
    if (identical(tipo, "variable_tiie28")) .fetch_tiie28()
    else if (identical(tipo, "variable_sofr")) .fetch_sofr()
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
    shiny::updateNumericInput(session, "pwiz_tasa_fetched",
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
    tipo_lbl   <- if (identical(tipo, "variable_tiie28")) "TIIE 28" else "SOFR"
    p_nombre   <- switch(freq, mensual="mensual", anual="anual",
                         semanal="semanal", diaria="diaria", "período")

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
      r_per     <- (1 + total_ann / 100)^(1 / ppy) - 1
      is_sel    <- identical(row_dia, dia)
      shiny::div(
        class = paste0("mb-1", if (is_sel) " fw-semibold" else " text-muted"),
        if (is_sel) shiny::tags$span(class = "me-1", "►") else NULL,
        shiny::tags$strong(sprintf("%s %s: ", tipo_lbl, format(date_val, "%d/%m/%Y"))),
        sprintf("%.3f%% + %.0f bps = %.3f%% anual → %.6f%% %s (EAR)",
                base, spread_bps, total_ann, r_per * 100, p_nombre)
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
          "Fuente: Federal Reserve Bank of St. Louis — FRED® API (serie SOFR). Uso conforme a FRED Terms of Use."
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
      shiny::showNotification("Sin permiso para crear pasivos.", type = "error"); return()
    }
    wiz$mode             <- "create"
    wiz$step_id          <- "categoria"
    wiz$edit_target_id   <- NULL
    wiz$draft            <- list()
    wiz$validation_errors <- list()
    wiz$cargos_list      <- list()
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
      shiny::showNotification("Sin permiso para editar pasivos.", type = "error"); return()
    }
    liabs <- tryCatch(shared$pasivos_liabilities_db(),
                      error = function(e) .schema_pasivos_liability())
    row <- liabs[liabs$id == lid, , drop = FALSE]
    if (!nrow(row)) {
      shiny::showNotification("Pasivo no encontrado.", type = "error"); return()
    }
    draft <- pasivos_liability_to_draft(row)
    wiz$mode             <- "edit"
    wiz$edit_target_id   <- lid
    wiz$draft            <- draft
    wiz$step_id          <- "categoria"
    wiz$validation_errors <- list()
    wiz$cargos_list      <- draft$cargos_iniciales %||% list()
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
  output$pwiz_step_content <- shiny::renderUI({
    empresa_choices <- tryCatch(sort(names(shared$company_map())),
                                error = function(e) sort(names(COMPANY_MAP)))
    existing_subcats <- tryCatch({
      liabs <- shared$pasivos_liabilities_db()
      sort(unique(liabs$subcategoria[!is.na(liabs$subcategoria) &
                                       nzchar(liabs$subcategoria %||% "")]))
    }, error = function(e) character())
    errs <- wiz$validation_errors %||% list()

    switch(wiz$step_id,
      categoria   = pasivos_step_ui_categoria(wiz$draft, errs),
      detalles    = pasivos_step_ui_detalles(wiz$draft, empresa_choices,
                                              existing_subcats, errs),
      recurrencia = pasivos_step_ui_recurrencia(wiz$draft, errs),
      terminos    = pasivos_step_ui_terminos(wiz$draft, errs),
      cargos      = pasivos_step_ui_cargos(wiz$draft, wiz$cargos_list, errs),
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
                format(round(monto_g, 2), big.mark = ",", nsmall = 2))
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
                  format(round(grace_pmt, 2), big.mark = ",", nsmall = 2))
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

  # ── Cargo rows table ────────────────────────────────────────────────────────
  output$pwiz_cargos_table <- shiny::renderUI({
    cl <- wiz$cargos_list
    if (!length(cl)) return(shiny::div(class = "text-muted small mb-2",
                                        "Sin cargos iniciales."))
    do.call(shiny::tagList, lapply(seq_along(cl), function(i)
      pasivos_cargo_row_ui(i, cl[[i]])
    ))
  })

  shiny::observeEvent(input$pwiz_add_cargo, ignoreInit = TRUE, {
    cl <- wiz$cargos_list
    for (i in seq_along(cl)) {
      cl[[i]]$desc   <- input[[paste0("pwiz_cargo_desc_",   i)]] %||% cl[[i]]$desc
      cl[[i]]$monto  <- input[[paste0("pwiz_cargo_monto_",  i)]] %||% cl[[i]]$monto
      cl[[i]]$moneda <- input[[paste0("pwiz_cargo_moneda_", i)]] %||% cl[[i]]$moneda
      cl[[i]]$fecha  <- tryCatch(as.Date(input[[paste0("pwiz_cargo_fecha_", i)]]),
                                 error = function(e) cl[[i]]$fecha)
    }
    wiz$cargos_list <- c(cl, list(list(desc = "", monto = 0,
                                       moneda = "MXN", fecha = Sys.Date())))
  })

  shiny::observe({
    cl <- wiz$cargos_list
    lapply(seq_along(cl), function(i) {
      local({
        idx <- i
        shiny::observeEvent(input[[paste0("pwiz_cargo_rm_", idx)]],
                            ignoreInit = TRUE, once = TRUE, {
          lst <- wiz$cargos_list
          for (j in seq_along(lst)) {
            lst[[j]]$desc   <- input[[paste0("pwiz_cargo_desc_",   j)]] %||% lst[[j]]$desc
            lst[[j]]$monto  <- input[[paste0("pwiz_cargo_monto_",  j)]] %||% lst[[j]]$monto
            lst[[j]]$moneda <- input[[paste0("pwiz_cargo_moneda_", j)]] %||% lst[[j]]$moneda
            lst[[j]]$fecha  <- tryCatch(as.Date(input[[paste0("pwiz_cargo_fecha_", j)]]),
                                        error = function(e) lst[[j]]$fecha)
          }
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
          onmouseover = "this.style.background='#f0f4ff'",
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
            style = "background:#6c757d; font-size:11px; align-self:center;",
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
    mon_pag <- input$pwiz_moneda_pago %||% "MXN"
    if (!nzchar(cot_en)) return()
    cot_amt <- input$pwiz_amount_cotizado %||% NA_real_
    if (is.na(cot_amt)) return()
    res <- tryCatch(
      pasivos_recompute_pago_from_cotizado(cot_amt, cot_en, mon_pag, Sys.Date()),
      error = function(e) list(amount_pago = NA_real_, fx_rate_used = NA_real_)
    )
    if (!is.na(res$amount_pago)) {
      shiny::updateNumericInput(session, "pwiz_amount_default",
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
    mon_pag <- input$pwiz_moneda_pago %||% "MXN"
    if (!nzchar(cot_en)) return(NULL)
    metric <- paste0("fx_", tolower(cot_en), "_", tolower(mon_pag))
    rate <- tryCatch(forecasting_get_estimate(metric, Sys.Date(),
                                              client_id = shared$effective_client_id()),
                     error = function(e) NA_real_)
    shiny::tags$small(class = "text-muted",
      if (!is.na(rate)) sprintf("FX usado: %.4f  (estimación: spot)", rate)
      else "FX no disponible para esta par."
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

  output$pwiz_revision_notice <- shiny::renderUI({
    cat <- wiz$draft$categoria %||% ""
    window <- switch(cat, financiero = "24 meses", "24 meses")
    shiny::tags$span(
      sprintf("Al guardar, se generarán provisiones para los próximos %s. ", window),
      "Las provisiones aparecerán en el calendario y en la tabla de Pasivos inmediatamente."
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
  shiny::observeEvent(input$pwiz_next, ignoreInit = TRUE, {
    wiz$validation_errors <- list()
    wiz$draft <- .wiz_collect_step(input, wiz$step_id, wiz)

    # Validate current step before advancing
    errs <- switch(wiz$step_id,
      detalles    = {
        ec <- tryCatch(sort(names(shared$company_map())),
                       error = function(e) sort(names(COMPANY_MAP)))
        pasivos_wizard_validate_detalles(wiz$draft, ec)$errors
      },
      recurrencia = pasivos_wizard_validate_recurrencia(wiz$draft)$errors,
      terminos    = pasivos_wizard_validate_terminos(wiz$draft)$errors,
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
      wiz$step_id <- target
    }
  })

  shiny::observeEvent(input$pwiz_save, ignoreInit = TRUE, {
    shinyjs::disable("pwiz_save")
    on.exit(shinyjs::enable("pwiz_save"), add = TRUE)

    wiz$draft <- .wiz_collect_step(input, wiz$step_id, wiz)
    user <- tryCatch(shared$current_user(), error = function(e) "system")
    res <- tryCatch(
      .pasivos_wizard_save(wiz, shared, session, user),
      error = function(e) {
        message("[wizard] unhandled error in save: ", conditionMessage(e))
        list(ok = FALSE, msg = paste0("Error inesperado al guardar: ", conditionMessage(e)))
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

  invisible(NULL)
}

# ── Modal wrapper ──────────────────────────────────────────────────────────────
.pwiz_modal <- function() {
  shiny::modalDialog(
    title     = shiny::uiOutput("pwiz_modal_title"),
    size      = "l",
    easyClose = FALSE,
    shiny::uiOutput("pwiz_step_indicator"),
    shiny::uiOutput("pwiz_step_content"),
    footer = shiny::uiOutput("pwiz_footer")
  )
}
