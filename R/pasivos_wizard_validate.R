# =============================================================================
# R/pasivos_wizard_validate.R
# Stage 4: Per-step and final validation for the pasivos wizard.
# All functions are pure — no reactive access, no side effects.
# =============================================================================

# Validate the "detalles" step fields.
# Returns list(ok, errors) where errors is a named list field -> message.
pasivos_wizard_validate_detalles <- function(draft, empresa_choices = character()) {
  errors <- list()

  nombre <- draft$nombre %||% ""
  if (!nzchar(nombre))
    errors$nombre <- tr("tablas.pasivos.asistente.nombre_obligatorio")
  else if (nchar(nombre) > 80)
    errors$nombre <- tr("tablas.pasivos.asistente.nombre_largo")

  empresa <- draft$empresa %||% ""
  if (!nzchar(empresa))
    errors$empresa <- tr("tablas.pasivos.asistente.empresa_obligatoria")
  else if (length(empresa_choices) > 0 && !(empresa %in% empresa_choices))
    errors$empresa <- tr("tablas.pasivos.asistente.empresa_desconocida")

  parte <- draft$parte %||% ""
  if (!nzchar(parte))
    errors$parte <- tr("tablas.pasivos.asistente.parte_obligatoria")

  moneda <- draft$moneda_pago %||% ""
  if (!nzchar(moneda) || !(moneda %in% CURRENCIES))
    errors$moneda_pago <- tr("tablas.pasivos.asistente.moneda_invalida")

  if (identical(draft$categoria, "tarjeta")) {
    cd <- as.integer(draft$tarjeta_closing_day %||% NA_integer_)
    dd <- as.integer(draft$tarjeta_due_day     %||% NA_integer_)
    if (is.na(cd) || cd < 1L || cd > 31L)
      errors$tarjeta_closing_day <- tr("tablas.pasivos.asistente.dia_corte")
    if (is.na(dd) || dd < 1L || dd > 31L)
      errors$tarjeta_due_day <- tr("tablas.pasivos.asistente.dia_vencimiento")
  }

  list(ok = length(errors) == 0L, errors = errors)
}

# Validate the "recurrencia" step (regular + tarjeta).
pasivos_wizard_validate_recurrencia <- function(draft) {
  errors <- list()

  if (identical(draft$categoria, "regular")) {
    rt <- draft$recurrence_type %||% ""
    if (!nzchar(rt))
      errors$recurrence_type <- tr("tablas.pasivos.asistente.periodicidad")

    monto <- draft$amount_default %||% NA_real_
    if (is.na(monto) || monto <= 0)
      errors$amount_default <- tr("tablas.pasivos.asistente.monto_positivo")

    # fecha_valida() (CM22): NA, "" and a zero-length Date (a cleared
    # dateInput) all say "missing" -- is.na() threw on the last one.
    if (!fecha_valida(draft$fecha_inicio))
      errors$fecha_inicio <- tr("tablas.pasivos.asistente.fecha_inicio")
    # "Cada N días" counts from its anchor: without one the engine stopped
    # generating, after the liability was already saved.
    if (identical(rt, "interval_days") && !fecha_valida(draft$recurrence_params$anchor_date))
      errors$anchor_date <- tr("tablas.pasivos.asistente.fecha_ancla")
  }

  if (identical(draft$categoria, "tarjeta")) {
    src <- draft$tarjeta_provision_source %||% ""
    if (!nzchar(src))
      errors$tarjeta_provision_source <- tr("tablas.pasivos.asistente.origen_provision")
    if (identical(src, "manual")) {
      monto <- draft$amount_default %||% NA_real_
      if (is.na(monto) || monto <= 0)
        errors$amount_default <- tr("tablas.pasivos.asistente.monto_base_positivo")
    }
  }

  list(ok = length(errors) == 0L, errors = errors)
}

# Validate the "terminos" step (financiero only).
pasivos_wizard_validate_terminos <- function(draft) {
  errors <- list()

  p <- draft$principal_original %||% NA_real_
  if (is.na(p) || p <= 0)
    errors$principal_original <- tr("tablas.pasivos.asistente.principal_positivo")

  # "variable_otro" is deferred to Stage 6; the engine still accepts it for existing
  # data but the wizard must not create new liabilities with it.
  valid_tasa_tipos <- c("fija", "variable_sofr", "variable_tiie28")
  if (!is.null(draft$tasa_tipo) && !draft$tasa_tipo %in% valid_tasa_tipos)
    errors$tasa_tipo <- tr("tablas.pasivos.asistente.tasa_tipo_no_disponible", tipo = draft$tasa_tipo)

  t <- draft$tasa_actual %||% NA_real_
  if (!identical(draft$flavor %||% "", "arrendamiento_puro")) {
    if (is.na(t) || t < 0)
      errors$tasa_actual <- tr("tablas.pasivos.asistente.tasa_negativa")
  }

  pl <- as.integer(draft$plazo_meses %||% NA_integer_)
  if (is.na(pl) || pl < 1L)
    errors$plazo_meses <- tr("tablas.pasivos.asistente.plazo_minimo")

  freq <- draft$frecuencia_pago %||% "mensual"
  if (identical(freq, "mensual")) {
    dp <- as.integer(draft$dia_pago %||% NA_integer_)
    if (is.na(dp) || dp < 1L || dp > 31L)
      errors$dia_pago <- tr("tablas.pasivos.asistente.dia_pago")
  } else if (identical(freq, "semanal")) {
    dp <- as.integer(draft$dia_pago %||% NA_integer_)
    if (is.na(dp) || dp < 1L || dp > 7L)
      errors$dia_pago <- tr("tablas.pasivos.asistente.dia_semana")
  } else if (identical(freq, "anual")) {
    dp <- as.integer(draft$dia_pago %||% NA_integer_)
    if (is.na(dp) || dp < 1L || dp > 31L)
      errors$dia_pago <- tr("tablas.pasivos.asistente.dia_pago")
    mp <- as.integer(draft$mes_pago %||% NA_integer_)
    if (is.na(mp) || mp < 1L || mp > 12L)
      errors$mes_pago <- tr("tablas.pasivos.asistente.mes_pago")
  }
  # diaria: no dia_pago validation needed

  if (!fecha_valida(draft$fecha_inicio))
    errors$fecha_inicio <- tr("tablas.pasivos.asistente.fecha_inicio_contrato")

  pg <- as.integer(draft$periodo_gracia %||% 0L)
  if (!is.na(pg) && pg < 0L)
    errors$periodo_gracia <- tr("tablas.pasivos.asistente.gracia_negativa")

  mg <- draft$monto_gracia %||% NA_real_
  if (!is.na(mg) && mg < 0)
    errors$monto_gracia <- tr("tablas.pasivos.asistente.cuota_gracia_negativa")

  if (identical(draft$metodo_amortizacion, "custom")) {
    csv_raw <- draft$schedule_csv_raw %||% ""
    if (!nzchar(trimws(csv_raw))) {
      errors$schedule_csv <- tr("tablas.pasivos.asistente.tabla_obligatoria")
    } else {
      parsed <- tryCatch(
        utils::read.csv(text = csv_raw, stringsAsFactors = FALSE),
        error = function(e) e
      )
      if (inherits(parsed, "error")) {
        errors$schedule_csv <- tr("tablas.pasivos.asistente.tabla_error", error = conditionMessage(parsed))
      } else {
        needed <- c("periodo", "fecha", "capital", "interes", "fees")
        missing_cols <- setdiff(needed, names(parsed))
        if (length(missing_cols))
          errors$schedule_csv <- tr("tablas.pasivos.asistente.tabla_columnas", columnas = paste(missing_cols, collapse = ", "))
      }
    }
  }

  list(ok = length(errors) == 0L, errors = errors)
}

# Validate the "cargos" step (financiero): initial fees and the residual.
# A fee with an amount needs its date -- with NA it became a provision with
# no date (never in Calendario or Vencidos, no way to delete it). A fee row
# with amount 0 is an empty row the user left: it is ignored. A residual
# paid apart ("separate") needs its own date; without one the engine fell
# back to the last payment's date without a word.
pasivos_wizard_validate_cargos <- function(draft) {
  errors <- list()
  cargos <- draft$cargos_iniciales %||% list()
  for (i in seq_along(cargos)) {
    monto <- suppressWarnings(as.numeric(cargos[[i]]$monto %||% 0))
    if (length(monto) == 1L && !is.na(monto) && monto != 0 && !fecha_valida(cargos[[i]]$fecha))
      errors[[paste0("cargo_fecha_", i)]] <- tr("tablas.pasivos.asistente.cargo_fecha", n = as.character(i))
  }
  vr <- draft$valor_residual
  if (!is.null(vr) && identical(vr$behavior, "separate") && !fecha_valida(vr$fecha))
    errors$residual_fecha <- tr("tablas.pasivos.asistente.residual_fecha")
  list(ok = length(errors) == 0L, errors = errors)
}

# Final validation pass — all required fields regardless of which step set them.
pasivos_wizard_validate_final <- function(draft, empresa_choices = character()) {
  all_errors <- list()

  cat_val <- draft$categoria %||% ""
  if (!nzchar(cat_val))
    all_errors$categoria <- tr("tablas.pasivos.asistente.categoria")

  v_det <- pasivos_wizard_validate_detalles(draft, empresa_choices)
  all_errors <- c(all_errors, v_det$errors)

  if (cat_val %in% c("regular", "tarjeta")) {
    v_rec <- pasivos_wizard_validate_recurrencia(draft)
    all_errors <- c(all_errors, v_rec$errors)
  }

  if (identical(cat_val, "financiero")) {
    v_ter <- pasivos_wizard_validate_terminos(draft)
    all_errors <- c(all_errors, v_ter$errors)
    all_errors <- c(all_errors, pasivos_wizard_validate_cargos(draft)$errors)
  }

  list(ok = length(all_errors) == 0L, errors = all_errors)
}

# Non-blocking warnings for the current step.
# Returns character vector (empty = no warnings).
pasivos_wizard_step_warnings <- function(draft, step_id) {
  w <- character(0)
  if (step_id == "detalles" && identical(draft$categoria, "tarjeta")) {
    cd <- as.integer(draft$tarjeta_closing_day %||% NA_integer_)
    dd <- as.integer(draft$tarjeta_due_day     %||% NA_integer_)
    if (!is.na(cd) && !is.na(dd) && cd == dd)
      w <- c(w, tr("tablas.pasivos.asistente.corte_igual_vencimiento"))
  }
  w
}
