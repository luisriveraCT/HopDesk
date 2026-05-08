# =============================================================================
# R/pasivos_schemas.R
# Empty-tibble schema definitions for the Pasivos module.
# All schemas follow the same pattern as persistence.R: typed empty tibbles
# used both as fallbacks and as the contract for .normalize().
# List-column schemas require custom loaders — see pasivos_persistence.R.
# =============================================================================

.schema_pasivos_liability <- function() tibble::tibble(
  id                 = character(),
  categoria          = character(),   # "regular" | "financiero" | "tarjeta"
  subcategoria       = character(),
  flavor             = character(),   # financiero only: "credito_simple" | "arrendamiento" |
                                       # "linea_revolvente" | "otro"; NA otherwise
  nombre             = character(),
  empresa            = character(),
  parte              = character(),
  codigo_parte       = character(),
  referencia_default = character(),
  documento_template = character(),

  moneda_pago        = character(),
  cotizado_en        = character(),   # NA if same as moneda_pago

  recurrence_type    = character(),
  recurrence_params  = list(),        # list-column

  amount_default     = numeric(),
  amount_default_cot = numeric(),

  tarjeta_provision_source = character(),  # "registered_expenses" | "manual" | "average_12m"

  tarjeta_closing_day  = integer(),
  tarjeta_due_day      = integer(),
  tarjeta_credit_limit = numeric(),

  principal_original   = numeric(),
  saldo_capital        = numeric(),
  tasa_actual          = numeric(),
  tasa_tipo            = character(),   # "fija" | "variable_sofr" | "variable_tiie28" | "variable_otro"
  tasa_spread          = numeric(),
  tasa_estimate_method = character(),
  fecha_inicio         = as.Date(character()),
  fecha_vence          = as.Date(character()),
  plazo_meses          = integer(),
  dia_pago             = integer(),
  frecuencia_pago      = character(),   # "mensual" | "anual" | "semanal" | "diaria"
  mes_pago             = integer(),     # month of annual payment; NA unless frecuencia_pago == "anual"
  periodo_gracia       = integer(),     # periods at start where only interest is paid (0 = none)
  monto_gracia         = numeric(),     # fixed payment per grace period; NA = auto (interest-only)
  metodo_amortizacion  = character(),   # "francesa" | "alemana" | "americana" | "custom"
  schedule_imported    = list(),        # list-column; NULL means compute from terms

  cargos_iniciales     = list(),        # list-column; list of named lists
  valor_residual       = list(),        # list-column; named list or NULL

  modifier_ids         = list(),        # list-column; list of character vectors

  estado               = character(),   # "active" | "paused" | "closed" | "deleted"
  notas                = character(),

  created_by           = character(),
  created_at           = as.POSIXct(character()),
  updated_by           = character(),
  updated_at           = as.POSIXct(character())
)

.schema_pasivos_provision <- function() tibble::tibble(
  id                = character(),
  liability_id      = character(),    # FK; NA for orphan manual provisions
  origin            = character(),    # "rule" | "manual" | "initial_fee" | "residual"
  occurrence_index  = integer(),
  estado            = character(),    # "provisional" | "converted" | "item_confirmed" | "closed"

  fecha_calculada   = as.Date(character()),
  fecha_efectiva    = as.Date(character()),
  policy_ids        = character(),

  empresa           = character(),
  parte             = character(),
  codigo_parte      = character(),
  moneda_pago       = character(),
  cotizado_en       = character(),

  amount_pago       = numeric(),
  amount_cotizado   = numeric(),
  fx_rate_used      = numeric(),

  componente_capital = numeric(),
  componente_interes = numeric(),
  componente_fees    = numeric(),
  componente_iva     = numeric(),

  amount_pago_override     = numeric(),
  amount_cotizado_override = numeric(),
  fecha_efectiva_override  = as.Date(character()),

  documento         = character(),
  referencia        = character(),
  notas             = character(),

  manual_inv_id     = character(),
  pagar_hoy_id      = character(),
  bancos_conf_id    = character(),
  reverted_count    = integer(),

  generated_by      = character(),
  generated_at      = as.POSIXct(character()),
  last_edited_by    = character(),
  last_edited_at    = as.POSIXct(character())
)

.schema_pasivos_modifier <- function() tibble::tibble(
  id              = character(),
  scope_type      = character(),    # "global" | "liability" | "provision"
  scope_id        = character(),
  type            = character(),    # "fx_rate" | "interest_rate_sofr" | "interest_rate_tiie28" |
                                     # "inflation_index" | "custom_multiplier"
  target_field    = character(),
  estimate_method = character(),
  frozen_value    = numeric(),
  enabled         = logical(),
  display_label   = character(),
  created_by      = character(),
  created_at      = as.POSIXct(character()),
  updated_at      = as.POSIXct(character())
)

.schema_pasivos_estimates <- function() tibble::tibble(
  metric         = character(),    # "fx_usd_mxn" | "sofr" | "tiie28" | future
  fecha          = as.Date(character()),
  value          = numeric(),
  source_method  = character(),    # "spot" | "manual" | "forward_curve" | etc.
  is_frozen      = logical(),
  updated_by     = character(),
  updated_at     = as.POSIXct(character())
)

.schema_pasivos_card_expense <- function() tibble::tibble(
  id            = character(),
  liability_id  = character(),
  fecha         = as.Date(character()),
  comercio      = character(),
  monto         = numeric(),
  moneda        = character(),
  categoria     = character(),
  notas         = character(),
  registered_by = character(),
  registered_at = as.POSIXct(character())
)

.schema_pasivos_audit <- function() tibble::tibble(
  id             = character(),
  ts             = as.POSIXct(character()),
  user           = character(),
  empresa        = character(),
  action_type    = character(),
  target_kind    = character(),   # "liability" | "provision" | "modifier" | "estimate" |
                                   # "capability" | "bulk"
  target_id      = character(),
  payload_before = character(),   # JSON-serialized
  payload_after  = character(),
  session_id     = character(),
  notes          = character()
)
