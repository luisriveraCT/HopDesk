# =============================================================================
# R/pasivos_wizard_steps.R
# Stage 4: Step UI builders for the pasivos wizard.
# Each function returns a tagList rendered inside the wizard modal.
# All IDs are prefixed "pwiz_" (root-level, not module-namespaced).
# =============================================================================

# ── Step indicator ─────────────────────────────────────────────────────────────
# Returns a horizontal Bootstrap-pill breadcrumb for the wizard steps.
# Past steps are clickable (actionLink), future steps are muted, current is filled.
pasivos_wizard_step_indicator <- function(current_step_id, all_step_ids, all_step_labels) {
  n <- length(all_step_ids)
  cur_idx <- match(current_step_id, all_step_ids)
  if (is.na(cur_idx)) cur_idx <- 1L

  pills <- lapply(seq_len(n), function(i) {
    lbl <- all_step_labels[i]
    sid <- all_step_ids[i]
    if (i < cur_idx) {
      # Past — clickable
      shiny::tags$span(
        class = "badge rounded-pill border border-primary text-primary me-1",
        style = "cursor:pointer; font-size:11px; padding:5px 10px;",
        onclick = sprintf(
          "Shiny.setInputValue('pwiz_goto_step', '%s', {priority:'event'})", sid),
        lbl
      )
    } else if (i == cur_idx) {
      # Current — filled
      shiny::tags$span(
        class = "badge rounded-pill bg-primary me-1",
        style = "font-size:11px; padding:5px 10px;",
        lbl
      )
    } else {
      # Future — muted
      shiny::tags$span(
        class = "badge rounded-pill bg-secondary bg-opacity-25 text-muted me-1",
        style = "font-size:11px; padding:5px 10px;",
        lbl
      )
    }
  })

  shiny::div(
    class = "d-flex flex-wrap align-items-center mb-3 pb-2 border-bottom",
    pills
  )
}

# ── Steps per category ─────────────────────────────────────────────────────────
.wiz_steps_for <- function(categoria) {
  switch(categoria %||% "",
    regular    = list(
      ids    = c("categoria", "detalles", "recurrencia", "revision"),
      labels = c("Categoría", "Detalles", "Recurrencia", "Revisión")
    ),
    financiero = list(
      ids    = c("categoria", "detalles", "terminos", "cargos", "revision"),
      labels = c("Categoría", "Detalles", "Términos", "Cargos", "Revisión")
    ),
    tarjeta    = list(
      ids    = c("categoria", "detalles", "recurrencia", "revision"),
      labels = c("Categoría", "Detalles", "Configuración", "Revisión")
    ),
    # Default (no category chosen yet): 4-step layout
    list(
      ids    = c("categoria", "detalles", "recurrencia", "revision"),
      labels = c("Categoría", "Detalles", "Detalles 2", "Revisión")
    )
  )
}

# ── Error callout helper ──────────────────────────────────────────────────────
.wiz_error_box <- function(errors) {
  if (!length(errors)) return(NULL)
  msgs <- unlist(errors, use.names = FALSE)
  shiny::div(
    class = "alert alert-danger py-2 mb-3",
    shiny::tags$ul(
      class = "mb-0 ps-3",
      lapply(msgs, function(m) shiny::tags$li(m))
    )
  )
}

# ── Step 1: Categoría ──────────────────────────────────────────────────────────
pasivos_step_ui_categoria <- function(draft, errors = list()) {
  cur <- draft$categoria %||% ""
  cur_flavor <- draft$flavor %||% ""

  card <- function(value, title, desc, selected) {
    cls <- if (identical(selected, value))
      "card mb-2 border-primary shadow-sm"
    else
      "card mb-2 border"
    shiny::div(
      class = cls, style = "cursor:pointer;",
      onclick = sprintf(
        "document.getElementById('pwiz_categoria_%s').click()", value),
      shiny::div(
        class = "card-body py-2 px-3 d-flex align-items-start gap-2",
        shiny::tags$input(
          type  = "radio", name  = "pwiz_categoria_radio",
          id    = sprintf("pwiz_categoria_%s", value),
          value = value,
          checked = if (identical(cur, value)) NA else NULL,
          style = "margin-top:3px; flex-shrink:0;",
          onchange = sprintf(
            "Shiny.setInputValue('pwiz_categoria', '%s', {priority:'event'})", value)
        ),
        shiny::div(
          shiny::tags$strong(title),
          shiny::tags$div(class = "text-muted small", desc)
        )
      )
    )
  }

  shiny::tagList(
    .wiz_error_box(errors),
    shiny::tags$p(class = "fw-semibold mb-2", "¿Qué tipo de pasivo es?"),
    card("regular",    "Pago regular",
         "Servicios, rentas, suscripciones. Cantidad similar mes a mes.", cur),
    card("financiero", "Pasivo financiero",
         "Crédito, arrendamiento, línea con fecha de inicio y fin.", cur),
    card("tarjeta",    "Tarjeta de crédito",
         "Pago mensual con corte y vencimiento fijos.", cur),

    # Hidden Shiny input that holds the value
    shiny::tags$div(
      style = "display:none;",
      shiny::textInput("pwiz_categoria", NULL, value = cur)
    ),

    # Sub-question for financiero flavor
    shiny::conditionalPanel(
      condition = "input.pwiz_categoria == 'financiero'",
      shiny::div(
        class = "mt-3 p-3 bg-light rounded",
        shiny::tags$p(class = "fw-semibold mb-2",
                      "¿Qué tipo de instrumento financiero?"),
        shiny::radioButtons(
          "pwiz_flavor", NULL,
          choices = c(
            "Crédito simple — préstamo a plazo con amortización mensual"             = "credito_simple",
            "Arrendamiento financiero — leasing con tabla de amortización e intereses" = "arrendamiento",
            "Arrendamiento puro / operativo — renta fija con plazo definido, sin intereses" = "arrendamiento_puro",
            "Línea revolvente — pagos de interés y capital al final"                 = "linea_revolvente",
            "Otro — estructura distinta o tabla de amortización propia"              = "otro"
          ),
          selected = if (nzchar(cur_flavor)) cur_flavor else "credito_simple"
        )
      )
    )
  )
}

# ── Step 2: Detalles ───────────────────────────────────────────────────────────
pasivos_step_ui_detalles <- function(draft, empresa_choices = character(),
                                      existing_subcats = character(),
                                      errors = list()) {
  cat <- draft$categoria %||% "regular"

  subcat_choices <- sort(unique(c(
    "servicios", "renta", "credito_simple", "arrendamiento",
    "linea_revolvente", "tarjeta_credito", "impuestos", "otro",
    existing_subcats
  )))

  shiny::tagList(
    .wiz_error_box(errors),
    shiny::fluidRow(
      shiny::column(6,
        shiny::textInput("pwiz_nombre", "Nombre",
                         value = draft$nombre %||% "",
                         placeholder = "Ej. Internet oficina, BAJIO 11778274"),
        shiny::selectInput("pwiz_empresa", "Empresa",
                           choices  = empresa_choices,
                           selected = draft$empresa %||% (empresa_choices[1] %||% "")),
        shiny::textInput("pwiz_parte", "Parte (Proveedor)",
                         value = draft$parte %||% "",
                         placeholder = "Escribe para buscar proveedor..."),
        shiny::uiOutput("pwiz_parte_suggestions"),
        shiny::textInput("pwiz_codigo_parte", "Código de Parte (opcional)",
                         value = draft$codigo_parte %||% ""),
        shiny::textInput("pwiz_referencia_default", "Referencia (plantilla, opcional)",
                         value = draft$referencia_default %||% "",
                         placeholder = "Ej. REF-2026"),
        shiny::textInput("pwiz_documento_template", "No. Documento (plantilla, opcional)",
                         value = draft$documento_template %||% "",
                         placeholder = "Ej. APV00055790")
      ),
      shiny::column(6,
        shiny::selectizeInput("pwiz_subcategoria", "Sub-categoría",
                              choices  = subcat_choices,
                              selected = draft$subcategoria %||% "",
                              options  = list(create = TRUE,
                                             placeholder = "Ej. servicios, renta...")),
        shiny::selectInput("pwiz_moneda_pago", "Moneda de pago",
                           choices  = CURRENCIES,
                           selected = draft$moneda_pago %||% "MXN"),
        shiny::tags$div(
          class = "mb-3",
          shiny::tags$small(class = "text-muted",
            "La divisa en que se realiza el pago. Una vez creado, no se puede cambiar.")
        ),
        shiny::selectInput("pwiz_cotizado_en", "Cotizado en",
                           choices  = c("Misma que pago" = "", CURRENCIES),
                           selected = draft$cotizado_en %||% "")
      )
    ),

    # Tarjeta-only fields
    if (identical(cat, "tarjeta")) {
      shiny::div(
        class = "mt-2 p-3 bg-light rounded",
        shiny::tags$p(class = "fw-semibold mb-2", "Datos de la tarjeta"),
        shiny::fluidRow(
          shiny::column(4,
            shiny::numericInput("pwiz_tarjeta_closing_day", "Día de corte (1-31)",
                                value = draft$tarjeta_closing_day %||% 28L,
                                min = 1, max = 31, step = 1)
          ),
          shiny::column(4,
            shiny::numericInput("pwiz_tarjeta_due_day", "Día de vencimiento (1-31)",
                                value = draft$tarjeta_due_day %||% 15L,
                                min = 1, max = 31, step = 1)
          ),
          shiny::column(4,
            shiny::numericInput("pwiz_tarjeta_credit_limit", "Límite de crédito (opcional)",
                                value = draft$tarjeta_credit_limit %||% NA_real_,
                                min = 0)
          )
        )
      )
    } else NULL
  )
}

# ── Step 3a: Recurrencia (regular) ────────────────────────────────────────────
pasivos_step_ui_recurrencia_regular <- function(draft, errors = list()) {
  rt <- draft$recurrence_type %||% "monthly_day"
  rp <- draft$recurrence_params %||% list()

  shiny::tagList(
    .wiz_error_box(errors),
    shiny::fluidRow(
      shiny::column(6,
        shiny::selectInput("pwiz_periodicidad", "Periodicidad",
          choices = c(
            "Mensual (mismo día)" = "monthly_day",
            "Mensual (n-ésimo día de la semana)" = "monthly_nth_weekday",
            "Quincenal" = "biweekly",
            "Semanal"   = "weekly",
            "Trimestral" = "quarterly",
            "Anual"      = "yearly",
            "Personalizada (lista de fechas)" = "custom"
          ),
          selected = rt
        ),
        shiny::dateInput("pwiz_fecha_inicio", "Fecha de inicio",
                         value     = draft$fecha_inicio %||% Sys.Date(),
                         weekstart = 1, language = "es")
      ),
      shiny::column(6,
        # Dynamic sub-fields depend on periodicidad — rendered by server
        shiny::uiOutput("pwiz_periodicidad_subfields"),

        # Amount section
        shiny::div(
          class = "mt-3 p-3 bg-light rounded",
          shiny::tags$p(class = "fw-semibold mb-2 small", "Monto"),
          shiny::conditionalPanel(
            condition = "input.pwiz_cotizado_en !== ''",
            shiny::numericInput("pwiz_amount_cotizado",
                                shiny::uiOutput("pwiz_cotizado_label"),
                                value = draft$amount_default_cot %||% NA_real_,
                                min = 0)
          ),
          shiny::div(
            class = "d-flex align-items-center gap-2",
            shiny::div(class = "flex-grow-1",
              shiny::numericInput("pwiz_amount_default", "Monto a pagar",
                                  value = draft$amount_default %||% NA_real_,
                                  min = 0)
            ),
            shiny::div(
              style = "padding-top:24px;",
              shiny::uiOutput("pwiz_amount_auto_badge")
            )
          ),
          shiny::uiOutput("pwiz_fx_info")
        )
      )
    )
  )
}

# ── Step 3b: Configuración mensual (tarjeta) ──────────────────────────────────
pasivos_step_ui_recurrencia_tarjeta <- function(draft, errors = list()) {
  src <- draft$tarjeta_provision_source %||% "registered_expenses"

  shiny::tagList(
    .wiz_error_box(errors),
    shiny::tags$p(class = "fw-semibold mb-2", "Origen de la provisión"),
    shiny::radioButtons(
      "pwiz_tarjeta_provision_source",
      NULL,
      choices = c(
        "Gastos registrados en la tarjeta"   = "registered_expenses",
        "Promedio de últimos 12 meses"  = "average_12m",
        "Manual"                             = "manual"
      ),
      selected = src
    ),
    shiny::tags$small(class = "text-muted d-block mb-3",
      shiny::conditionalPanel(
        condition = "input.pwiz_tarjeta_provision_source == 'registered_expenses'",
        "Las provisiones se calculan a partir de los gastos que registres en la tarjeta."
      ),
      shiny::conditionalPanel(
        condition = "input.pwiz_tarjeta_provision_source == 'average_12m'",
        "El sistema calcula el promedio histórico y lo usa como provisión."
      ),
      shiny::conditionalPanel(
        condition = "input.pwiz_tarjeta_provision_source == 'manual'",
        "Tú escribes el monto base y lo ajustas mes a mes."
      )
    ),
    shiny::conditionalPanel(
      condition = "input.pwiz_tarjeta_provision_source == 'manual'",
      shiny::numericInput("pwiz_amount_default",
                          "Monto base mensual",
                          value = draft$amount_default %||% NA_real_,
                          min   = 0)
    )
  )
}

# ── Step 3: Dispatcher ─────────────────────────────────────────────────────────
pasivos_step_ui_recurrencia <- function(draft, errors = list()) {
  if (identical(draft$categoria, "tarjeta"))
    pasivos_step_ui_recurrencia_tarjeta(draft, errors)
  else
    pasivos_step_ui_recurrencia_regular(draft, errors)
}

# ── Step 4: Términos del crédito (financiero) ─────────────────────────────────
pasivos_step_ui_terminos <- function(draft, errors = list()) {
  flavor   <- draft$flavor %||% "credito_simple"
  is_puro  <- identical(flavor, "arrendamiento_puro")
  tt       <- draft$tasa_tipo %||% "fija"
  ma       <- draft$metodo_amortizacion %||% "francesa"
  pg       <- as.integer(draft$periodo_gracia %||% 0L)
  if (is.na(pg)) pg <- 0L

  shiny::tagList(
    .wiz_error_box(errors),
    shiny::fluidRow(

      # ── Left column ──────────────────────────────────────────────────────────
      shiny::column(6,
        shiny::numericInput("pwiz_principal_original",
                            if (is_puro) "Contraprestación total del arrendamiento"
                            else "Principal original",
                            value = draft$principal_original %||% NA_real_, min = 0),
        shiny::numericInput("pwiz_saldo_capital",
                            "Saldo de capital actual (opcional)",
                            value = draft$saldo_capital %||% NA_real_, min = 0),
        shiny::tags$small(class = "text-muted",
          "Si el pasivo ya está en curso, ingresa el saldo según tu estado de cuenta más reciente."),

        shiny::hr(class = "my-2"),

        # ── Período de gracia ───────────────────────────────────────────────
        shiny::numericInput("pwiz_periodo_gracia",
                            "Períodos de gracia",
                            value = pg, min = 0, step = 1),
        shiny::tags$small(class = "text-muted d-block",
          if (is_puro)
            "Períodos iniciales sin pago alguno (renta = $0). La amortización comienza después."
          else
            "Períodos donde sólo se paga interés sobre el saldo total (sin reducir capital). El monto de cada cuota de gracia se calcula automáticamente de la tasa capturada abajo."
        ),
        shiny::uiOutput("pwiz_plazo_breakdown"),

        # ── Cuota de gracia personalizada (sólo cuando gracia > 0) ────────
        shiny::conditionalPanel(
          condition = "input.pwiz_periodo_gracia > 0",
          shiny::div(
            class = "mt-2 p-3 bg-light rounded",
            shiny::div(
              class = "d-flex align-items-center gap-2 mb-1",
              shiny::tags$span(class = "fw-semibold small", "Cuota de gracia personalizada"),
              shiny::tags$span(class = "badge bg-secondary bg-opacity-50 small fw-normal",
                               "opcional")
            ),
            shiny::numericInput(
              "pwiz_monto_gracia",
              NULL,
              value = draft$monto_gracia %||% NA_real_,
              min   = 0
            ),
            shiny::tags$small(
              class = "text-muted",
              "Monto fijo por período de gracia. Deja en blanco para usar el cálculo ",
              "automático mostrado arriba (interés sobre el saldo total)."
            )
          )
        ),

        shiny::hr(class = "my-2"),

        # ── Tasa (oculta para arrendamiento_puro) ─────────────────────────
        if (!is_puro) {
          shiny::tagList(
            # ── Rate type selector (top) ────────────────────────────────────
            shiny::radioButtons("pwiz_tasa_tipo", "Tipo de tasa",
                                choices = c(
                                  "Fija"            = "fija",
                                  "Variable SOFR"   = "variable_sofr",
                                  "Variable TIIE28" = "variable_tiie28"
                                ),
                                selected = if (identical(tt, "variable_otro")) "fija" else tt,
                                inline = FALSE),
            if (identical(tt, "variable_otro"))
              shiny::div(class = "alert alert-info small py-2 mt-1",
                "Este pasivo usa 'Variable otra', opción que se reactivará en una versión futura. ",
                "Selecciona Fija/SOFR/TIIE28 para convertirla."
              ),

            # ── Fija: dual annual ↔ per-period inputs ───────────────────────
            shiny::conditionalPanel(
              condition = "input.pwiz_tasa_tipo === 'fija'",
              shiny::div(
                class = "mb-3",
                shiny::tags$label(class = "form-label small fw-semibold",
                                  "Tasa de interés"),
                shiny::div(
                  class = "d-flex gap-3",
                  shiny::div(
                    style = "width: 130px;",
                    shiny::tags$small(class = "text-muted d-block mb-1", "Anual (%)"),
                    shiny::numericInput("pwiz_tasa_anual", NULL,
                                        value = draft$tasa_actual %||% NA_real_,
                                        min = 0, step = 0.001)
                  ),
                  shiny::div(
                    class = "d-flex align-items-center",
                    style = "padding-top: 22px; font-size: 18px; color: #adb5bd;",
                    "⇌"
                  ),
                  shiny::div(
                    style = "width: 130px;",
                    shiny::uiOutput("pwiz_tasa_periodo_label"),
                    shiny::numericInput("pwiz_tasa_periodo", NULL,
                                        value = {
                                          ta <- draft$tasa_actual %||% NA_real_
                                          f  <- draft$frecuencia_pago %||% "mensual"
                                          p  <- switch(f, mensual=12L, anual=1L, semanal=52L, diaria=365L, 12L)
                                          if (!is.na(ta)) round(((1 + ta/100)^(1/p) - 1) * 100, 3) else NA_real_
                                        },
                                        min = 0, step = 0.001)
                  )
                ),
                shiny::uiOutput("pwiz_tasa_nota")
              )
            ),

            # ── Variable: fetch widget (Hoy/Ayer + spread + preview) ────────
            shiny::conditionalPanel(
              condition = "input.pwiz_tasa_tipo !== 'fija'",
              shiny::div(
                class = "mb-3",
                shiny::div(style = "display:none;",
                  shiny::numericInput("pwiz_tasa_fetched", NULL, value = NA_real_)
                ),
                shiny::uiOutput("pwiz_rate_dia_buttons"),
                shiny::numericInput("pwiz_tasa_spread", "Sobretasa (bps)",
                                    value = draft$tasa_spread %||% 0, min = 0, step = 1),
                shiny::uiOutput("pwiz_variable_rate_preview")
              )
            ),

            # ── JS: blur shadow inputs (fire only when user leaves the field)
            shiny::tags$script(shiny::HTML(
              "(function(){
                $(document).off('.pwizrate')
                  .on('focusout.pwizrate','#pwiz_tasa_anual',function(){
                    var v=parseFloat($(this).val());
                    if(!isNaN(v)) Shiny.setInputValue('pwiz_tasa_anual_blur',v,{priority:'event'});
                  })
                  .on('focusout.pwizrate','#pwiz_tasa_periodo',function(){
                    var v=parseFloat($(this).val());
                    if(!isNaN(v)) Shiny.setInputValue('pwiz_tasa_periodo_blur',v,{priority:'event'});
                  });
              })();"
            ))
          )
        } else {
          # Hidden inputs so collection code doesn't error on NULL
          shiny::tagList(
            shiny::div(style = "display:none;",
              shiny::numericInput("pwiz_tasa_anual",   NULL, value = 0, min = 0),
              shiny::numericInput("pwiz_tasa_periodo", NULL, value = 0, min = 0),
              shiny::numericInput("pwiz_tasa_spread",  NULL, value = 0, min = 0),
              shiny::numericInput("pwiz_tasa_fetched", NULL, value = NA_real_),
              shiny::radioButtons("pwiz_tasa_tipo",    NULL,
                                  choices = c("fija"), selected = "fija")
            ),
            shiny::div(class = "alert alert-info small py-2 mb-0",
              shiny::tags$strong("Arrendamiento puro: "),
              "sin componente de interés. La cuota se calcula como ",
              "contraprestación total ÷ períodos de amortización."
            )
          )
        }
      ),

      # ── Right column ─────────────────────────────────────────────────────────
      shiny::column(6,
        shiny::dateInput("pwiz_fecha_inicio",
                         "Fecha de inicio del contrato",
                         value     = draft$fecha_inicio %||% Sys.Date(),
                         weekstart = 1, language = "es"),
        # ── Compound plazo + frecuencia row ──────────────────────────────────
        shiny::div(
          class = "mb-1",
          shiny::tags$label(class = "form-label small fw-semibold",
                            "Períodos de amortización"),
          shiny::div(
            class = "d-flex",
            style = "max-width: 220px;",
            shiny::div(
              style = "flex: 0 0 80px;",
              shiny::numericInput("pwiz_plazo_meses", NULL,
                                  value = draft$plazo_meses %||% 60L,
                                  min = 1, step = 1,
                                  width = "80px")
            ),
            shiny::div(
              style = "flex: 0 0 120px; margin-left: -1px;",
              shiny::selectInput("pwiz_frecuencia_pago", NULL,
                                 choices = c(
                                   Meses   = "mensual",
                                   Años    = "anual",
                                   Semanas = "semanal",
                                   Días    = "diaria"
                                 ),
                                 selected = draft$frecuencia_pago %||% "mensual",
                                 width   = "120px")
            )
          )
        ),
        shiny::uiOutput("pwiz_plazo_helper_text"),

        # ── Día / semana / mes de pago (frequency-aware) ─────────────────────
        # Mensual: single day-of-month input
        shiny::conditionalPanel(
          condition = "input.pwiz_frecuencia_pago === 'mensual'",
          shiny::numericInput("pwiz_dia_pago", "Día de pago (1-31)",
                              value = draft$dia_pago %||% 28L,
                              min = 1, max = 31, step = 1)
        ),
        # Anual: month select + leap-year-aware day select side by side
        shiny::conditionalPanel(
          condition = "input.pwiz_frecuencia_pago === 'anual'",
          shiny::div(
            class = "d-flex gap-2 align-items-end",
            shiny::div(
              class = "flex-grow-1",
              shiny::selectInput("pwiz_mes_pago", "Mes de pago",
                                 choices = setNames(
                                   1:12,
                                   c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                                     "Julio","Agosto","Septiembre","Octubre",
                                     "Noviembre","Diciembre")
                                 ),
                                 selected = as.character(draft$mes_pago %||% 1L))
            ),
            shiny::div(
              style = "width: 80px; flex-shrink: 0;",
              {
                mo     <- as.integer(draft$mes_pago %||% 1L)
                n_days <- as.integer(lubridate::days_in_month(
                            as.Date(sprintf("2024-%02d-01", mo))))
                cur_d  <- min(as.integer(draft$dia_pago %||% 1L), n_days)
                shiny::selectInput("pwiz_dia_pago_anual", "Día",
                                   choices  = setNames(seq_len(n_days), seq_len(n_days)),
                                   selected = as.character(cur_d))
              }
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "input.pwiz_frecuencia_pago === 'semanal'",
          shiny::selectInput("pwiz_dia_semana_pago", "Día de la semana",
                             choices = c(Lunes=1L, Martes=2L, `Miércoles`=3L, Jueves=4L,
                                         Viernes=5L, Sábado=6L, Domingo=7L),
                             selected = as.character(draft$dia_pago %||% 5L))
        ),
        shiny::conditionalPanel(
          condition = "input.pwiz_frecuencia_pago === 'diaria'",
          shiny::tags$small(class = "text-muted d-block mb-3",
                            "Pagos diarios — no se requiere día específico.")
        ),

        shiny::br(),

        # ── Método de amortización (oculto para arrendamiento_puro) ──────
        if (!is_puro) {
          shiny::radioButtons("pwiz_metodo_amortizacion",
                              "Método de amortización",
                              choices = c(
                                "Francesa (cuota nivelada)"                    = "francesa",
                                "Alemana (capital constante)"                  = "alemana",
                                "Americana (sólo intereses + capital al final)" = "americana",
                                "Tabla propia (pegada o importada)"            = "custom"
                              ),
                              selected = ma)
        } else {
          shiny::div(style = "display:none;",
            shiny::radioButtons("pwiz_metodo_amortizacion", NULL,
                                choices = c(francesa = "francesa"),
                                selected = "francesa")
          )
        }
      )
    ),

    # ── CSV (solo para método custom, no aplica en arrendamiento_puro) ────────
    if (!is_puro) {
      shiny::conditionalPanel(
        condition = "input.pwiz_metodo_amortizacion == 'custom'",
        shiny::div(
          class = "mt-2",
          shiny::textAreaInput("pwiz_schedule_csv",
            "Pega la tabla de amortización (CSV con columnas: periodo,fecha,capital,interes,fees)",
            value = draft$schedule_csv_raw %||% "",
            rows  = 6,
            placeholder = "periodo,fecha,capital,interes,fees\n1,2026-06-28,3000,1500,0\n2,2026-07-28,3030,1470,0"
          ),
          shiny::uiOutput("pwiz_csv_parse_error")
        )
      )
    } else NULL
  )
}

# ── Step 5: Cargos iniciales y residual (financiero) ──────────────────────────
pasivos_step_ui_cargos <- function(draft, cargos_list = list(),
                                    errors = list()) {
  has_res <- !is.null(draft$valor_residual) &&
             is.list(draft$valor_residual) &&
             length(draft$valor_residual) > 0 &&
             !is.null(draft$valor_residual$monto)

  res_monto    <- if (has_res) draft$valor_residual$monto    %||% NA_real_ else NA_real_
  res_moneda   <- if (has_res) draft$valor_residual$moneda   %||% "MXN"    else "MXN"
  res_fecha    <- if (has_res) draft$valor_residual$fecha    %||% NA       else NA
  res_behavior <- if (has_res) draft$valor_residual$behavior %||% "replace_last" else "replace_last"

  shiny::tagList(
    .wiz_error_box(errors),

    # ── Cargos iniciales ──────────────────────────────────────────────────────
    shiny::tags$p(class = "fw-semibold mb-2", "Cargos iniciales (opcional)"),
    shiny::uiOutput("pwiz_cargos_table"),
    shiny::actionButton("pwiz_add_cargo", "+ Agregar cargo",
                        class = "btn btn-outline-secondary btn-sm mb-3"),

    shiny::hr(),

    # ── Valor residual ────────────────────────────────────────────────────────
    shiny::tags$p(class = "fw-semibold mb-2",
                  "Valor residual al final del contrato (opcional)"),
    shiny::checkboxInput("pwiz_has_residual",
                         "Activar valor residual",
                         value = has_res),
    shiny::conditionalPanel(
      condition = "input.pwiz_has_residual",
      shiny::fluidRow(
        shiny::column(4,
          shiny::numericInput("pwiz_residual_monto", "Monto",
                              value = res_monto, min = 0)
        ),
        shiny::column(4,
          shiny::selectInput("pwiz_residual_moneda", "Moneda",
                             choices  = CURRENCIES,
                             selected = res_moneda)
        ),
        shiny::column(4,
          shiny::dateInput("pwiz_residual_fecha", "Fecha",
                           value     = if (!is.na(res_fecha)) res_fecha else Sys.Date(),
                           weekstart = 1, language = "es")
        )
      ),
      shiny::tags$p(class = "fw-semibold small mb-1", "Comportamiento:"),
      shiny::radioButtons(
        "pwiz_residual_behavior", NULL,
        choices = c(
          "Reemplaza la última cuota regular — arrendamiento con opción de compra al final"  = "replace_last",
          "Se suma a la última cuota (misma fecha) — bullet con pago final mayor, balloon"        = "add_to_last",
          "Pago aparte en otra fecha — residual diferido o liquidación posterior"                 = "separate"
        ),
        selected = res_behavior
      )
    )
  )
}

# ── Step 6: Revisión ───────────────────────────────────────────────────────────
pasivos_step_ui_revision <- function(draft, errors = list()) {
  shiny::tagList(
    .wiz_error_box(errors),
    shiny::fluidRow(
      shiny::column(6,
        shiny::tags$h6("Resumen"),
        shiny::uiOutput("pwiz_revision_summary")
      ),
      shiny::column(6,
        shiny::tags$h6(
          if (identical(draft$categoria, "financiero"))
            "Amortización (próximas cuotas)"
          else
            "Próximas provisiones"
        ),
        shiny::uiOutput("pwiz_revision_schedule")
      )
    ),
    shiny::div(
      class = "mt-3 p-2 bg-light rounded small text-muted",
      shiny::uiOutput("pwiz_revision_notice")
    )
  )
}

# ── Periodicidad sub-fields (rendered dynamically by server) ──────────────────
# Returns tagList for the current periodicidad choice.
pasivos_periodicidad_subfields_ui <- function(periodicidad, draft) {
  rp <- draft$recurrence_params %||% list()

  switch(periodicidad %||% "monthly_day",

    monthly_day = shiny::numericInput(
      "pwiz_rp_day", "Día del mes (1-31)",
      value = rp$day %||% 1L, min = 1, max = 31, step = 1
    ),

    monthly_nth_weekday = shiny::tagList(
      shiny::selectInput("pwiz_rp_nth", "Número de ocurrencia",
        choices  = c("1ª" = 1, "2ª" = 2, "3ª" = 3,
                     "4ª" = 4, "5ª" = 5, "Última" = -1),
        selected = rp$nth %||% 1L
      ),
      shiny::selectInput("pwiz_rp_wday", "Día de la semana",
        choices  = c(Lunes = 1, Martes = 2, Miércoles = 3, Jueves = 4,
                     Viernes = 5, Sábado = 6, Domingo = 7),
        selected = rp$wday %||% 1L
      )
    ),

    biweekly = shiny::dateInput(
      "pwiz_rp_anchor", "Fecha ancla",
      value     = rp$anchor_date %||% Sys.Date(),
      weekstart = 1, language = "es"
    ),

    weekly = shiny::selectInput("pwiz_rp_wday", "Día de la semana",
      choices  = c(Lunes = 1, Martes = 2, Miércoles = 3, Jueves = 4,
                   Viernes = 5, Sábado = 6, Domingo = 7),
      selected = rp$wday %||% 5L
    ),

    quarterly = shiny::tagList(
      shiny::numericInput("pwiz_rp_anchor_month",
                          "Mes ancla (1-12)",
                          value = rp$anchor_month %||% 1L,
                          min = 1, max = 12, step = 1),
      shiny::numericInput("pwiz_rp_day",
                          "Día del mes",
                          value = rp$day %||% 1L,
                          min = 1, max = 31, step = 1)
    ),

    yearly = shiny::tagList(
      shiny::numericInput("pwiz_rp_month",
                          "Mes (1-12)",
                          value = rp$month %||% 1L,
                          min = 1, max = 12, step = 1),
      shiny::numericInput("pwiz_rp_day",
                          "Día del mes",
                          value = rp$day %||% 1L,
                          min = 1, max = 31, step = 1)
    ),

    custom = shiny::tagList(
      shiny::dateInput("pwiz_rp_custom_add", "Agregar fecha",
                       weekstart = 1, language = "es"),
      shiny::actionButton("pwiz_rp_custom_add_btn",
                          "+ Agregar fecha",
                          class = "btn btn-outline-secondary btn-sm mb-2"),
      shiny::uiOutput("pwiz_rp_custom_dates_list")
    ),

    NULL
  )
}

# ── Cargo row UI ───────────────────────────────────────────────────────────────
# Returns a single cargo row for the cargos step.
pasivos_cargo_row_ui <- function(i, cargo = list()) {
  shiny::div(
    class = "d-flex gap-2 align-items-end mb-2",
    id    = paste0("pwiz_cargo_row_", i),
    shiny::div(class = "flex-grow-1",
      shiny::textInput(paste0("pwiz_cargo_desc_", i), if (i == 1L) "Descripción" else NULL,
                       value = cargo$desc %||% "")
    ),
    shiny::div(style = "width:130px;",
      shiny::numericInput(paste0("pwiz_cargo_monto_", i), if (i == 1L) "Monto" else NULL,
                          value = cargo$monto %||% 0, min = 0)
    ),
    shiny::div(style = "width:90px;",
      shiny::selectInput(paste0("pwiz_cargo_moneda_", i), if (i == 1L) "Moneda" else NULL,
                         choices = CURRENCIES,
                         selected = cargo$moneda %||% "MXN")
    ),
    shiny::div(style = "width:150px;",
      shiny::dateInput(paste0("pwiz_cargo_fecha_", i), if (i == 1L) "Fecha" else NULL,
                       value = cargo$fecha %||% Sys.Date(),
                       weekstart = 1, language = "es")
    ),
    shiny::div(
      style = if (i == 1L) "padding-top:24px;" else NULL,
      shiny::actionButton(
        paste0("pwiz_cargo_rm_", i), "×",
        class = "btn btn-outline-danger btn-sm"
      )
    )
  )
}
