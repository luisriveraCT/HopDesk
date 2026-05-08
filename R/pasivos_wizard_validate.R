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
    errors$nombre <- "El nombre es obligatorio."
  else if (nchar(nombre) > 80)
    errors$nombre <- "El nombre no puede superar 80 caracteres."

  empresa <- draft$empresa %||% ""
  if (!nzchar(empresa))
    errors$empresa <- "La empresa es obligatoria."
  else if (length(empresa_choices) > 0 && !(empresa %in% empresa_choices))
    errors$empresa <- "Empresa no reconocida."

  parte <- draft$parte %||% ""
  if (!nzchar(parte))
    errors$parte <- "El proveedor / parte es obligatorio."

  moneda <- draft$moneda_pago %||% ""
  if (!nzchar(moneda) || !(moneda %in% CURRENCIES))
    errors$moneda_pago <- "Moneda de pago no válida."

  if (identical(draft$categoria, "tarjeta")) {
    cd <- as.integer(draft$tarjeta_closing_day %||% NA_integer_)
    dd <- as.integer(draft$tarjeta_due_day     %||% NA_integer_)
    if (is.na(cd) || cd < 1L || cd > 31L)
      errors$tarjeta_closing_day <- "Día de corte debe ser entre 1 y 31."
    if (is.na(dd) || dd < 1L || dd > 31L)
      errors$tarjeta_due_day <- "Día de vencimiento debe ser entre 1 y 31."
  }

  list(ok = length(errors) == 0L, errors = errors)
}

# Validate the "recurrencia" step (regular + tarjeta).
pasivos_wizard_validate_recurrencia <- function(draft) {
  errors <- list()

  if (identical(draft$categoria, "regular")) {
    rt <- draft$recurrence_type %||% ""
    if (!nzchar(rt))
      errors$recurrence_type <- "Debes elegir una periodicidad."

    monto <- draft$amount_default %||% NA_real_
    if (is.na(monto) || monto <= 0)
      errors$amount_default <- "El monto debe ser mayor que cero."

    fi <- draft$fecha_inicio %||% NA
    if (is.na(fi))
      errors$fecha_inicio <- "La fecha de inicio es obligatoria."
  }

  if (identical(draft$categoria, "tarjeta")) {
    src <- draft$tarjeta_provision_source %||% ""
    if (!nzchar(src))
      errors$tarjeta_provision_source <- "Debes elegir el origen de la provisión."
    if (identical(src, "manual")) {
      monto <- draft$amount_default %||% NA_real_
      if (is.na(monto) || monto <= 0)
        errors$amount_default <- "El monto base debe ser mayor que cero."
    }
  }

  list(ok = length(errors) == 0L, errors = errors)
}

# Validate the "terminos" step (financiero only).
pasivos_wizard_validate_terminos <- function(draft) {
  errors <- list()

  p <- draft$principal_original %||% NA_real_
  if (is.na(p) || p <= 0)
    errors$principal_original <- "El principal debe ser mayor que cero."

  # "variable_otro" is deferred to Stage 6; the engine still accepts it for existing
  # data but the wizard must not create new liabilities with it.
  valid_tasa_tipos <- c("fija", "variable_sofr", "variable_tiie28")
  if (!is.null(draft$tasa_tipo) && !draft$tasa_tipo %in% valid_tasa_tipos)
    errors$tasa_tipo <- paste0("tasa_tipo '", draft$tasa_tipo, "' no está disponible en el asistente. ",
                               "Selecciona Fija, Variable SOFR o Variable TIIE28.")

  t <- draft$tasa_actual %||% NA_real_
  if (!identical(draft$flavor %||% "", "arrendamiento_puro")) {
    if (is.na(t) || t < 0)
      errors$tasa_actual <- "La tasa no puede ser negativa."
  }

  pl <- as.integer(draft$plazo_meses %||% NA_integer_)
  if (is.na(pl) || pl < 1L)
    errors$plazo_meses <- "El plazo debe tener al menos 1 período."

  freq <- draft$frecuencia_pago %||% "mensual"
  if (identical(freq, "mensual")) {
    dp <- as.integer(draft$dia_pago %||% NA_integer_)
    if (is.na(dp) || dp < 1L || dp > 31L)
      errors$dia_pago <- "Día de pago debe ser entre 1 y 31."
  } else if (identical(freq, "semanal")) {
    dp <- as.integer(draft$dia_pago %||% NA_integer_)
    if (is.na(dp) || dp < 1L || dp > 7L)
      errors$dia_pago <- "Día de semana debe ser entre 1 (lunes) y 7 (domingo)."
  } else if (identical(freq, "anual")) {
    dp <- as.integer(draft$dia_pago %||% NA_integer_)
    if (is.na(dp) || dp < 1L || dp > 31L)
      errors$dia_pago <- "Día de pago debe ser entre 1 y 31."
    mp <- as.integer(draft$mes_pago %||% NA_integer_)
    if (is.na(mp) || mp < 1L || mp > 12L)
      errors$mes_pago <- "Mes de pago debe ser entre 1 y 12."
  }
  # diaria: no dia_pago validation needed

  fi <- draft$fecha_inicio %||% NA
  if (is.na(fi))
    errors$fecha_inicio <- "La fecha de inicio del contrato es obligatoria."

  pg <- as.integer(draft$periodo_gracia %||% 0L)
  if (!is.na(pg) && pg < 0L)
    errors$periodo_gracia <- "El período de gracia no puede ser negativo."

  mg <- draft$monto_gracia %||% NA_real_
  if (!is.na(mg) && mg < 0)
    errors$monto_gracia <- "La cuota de gracia personalizada no puede ser negativa."

  if (identical(draft$metodo_amortizacion, "custom")) {
    csv_raw <- draft$schedule_csv_raw %||% ""
    if (!nzchar(trimws(csv_raw))) {
      errors$schedule_csv <- "Debes pegar la tabla de amortización."
    } else {
      parsed <- tryCatch(
        utils::read.csv(text = csv_raw, stringsAsFactors = FALSE),
        error = function(e) e
      )
      if (inherits(parsed, "error")) {
        errors$schedule_csv <- paste0("Error al parsear la tabla: ", conditionMessage(parsed))
      } else {
        needed <- c("periodo", "fecha", "capital", "interes", "fees")
        missing_cols <- setdiff(needed, names(parsed))
        if (length(missing_cols))
          errors$schedule_csv <- paste0(
            "Faltan columnas: ", paste(missing_cols, collapse = ", "))
      }
    }
  }

  list(ok = length(errors) == 0L, errors = errors)
}

# Final validation pass — all required fields regardless of which step set them.
pasivos_wizard_validate_final <- function(draft, empresa_choices = character()) {
  all_errors <- list()

  cat_val <- draft$categoria %||% ""
  if (!nzchar(cat_val))
    all_errors$categoria <- "Debes elegir una categoría."

  v_det <- pasivos_wizard_validate_detalles(draft, empresa_choices)
  all_errors <- c(all_errors, v_det$errors)

  if (cat_val %in% c("regular", "tarjeta")) {
    v_rec <- pasivos_wizard_validate_recurrencia(draft)
    all_errors <- c(all_errors, v_rec$errors)
  }

  if (identical(cat_val, "financiero")) {
    v_ter <- pasivos_wizard_validate_terminos(draft)
    all_errors <- c(all_errors, v_ter$errors)
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
      w <- c(w, "El día de corte y el día de vencimiento son iguales. Verifica que sea intencional.")
  }
  w
}
