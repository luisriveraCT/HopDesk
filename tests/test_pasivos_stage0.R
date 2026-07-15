# =============================================================================
# tests/test_pasivos_stage0.R
# Stage 0 foundation tests for the Pasivos module.
# Run from project root:  source("tests/test_pasivos_stage0.R")
# Requires no live S3 credentials — S3 is mocked with in-memory stubs.
# =============================================================================

library(dplyr)
library(tibble)
library(lubridate)
library(jsonlite)
library(uuid)

# Source only the files under test (no Shiny, no full app startup).
# persistence.R is required for .normalize() and schema helpers.
source("R/persistence.R")
source("R/pasivos_schemas.R")

# S3_KEYS is normally defined in global.R (sourced at app startup).
# The test stubs only the keys actually used by the pasivos files.
if (!exists("S3_KEYS")) {
  S3_KEYS <- list(
    pasivos_liabilities   = "pasivos_liabilities.rds",
    pasivos_provisions    = "pasivos_provisions.rds",
    pasivos_modifiers     = "pasivos_modifiers.rds",
    pasivos_estimates     = "pasivos_estimates.rds",
    pasivos_card_expenses = "pasivos_card_expenses.rds",
    pasivos_audit         = "pasivos_audit.rds",
    # needed by bancos_persistence.R (group 3)
    bancos_cuentas        = "bancos_cuentas.rds",
    bancos_movimientos    = "bancos_movimientos.rds",
    bancos_confirmados    = "bancos_confirmados.rds",
    # also needed by test_policy_engine.R (group 7)
    policy_catalog        = "policy_catalog.rds",
    partner_policies      = "partner_policies.rds",
    policy_moves          = "policy_moves.rds",
    holiday_overrides     = "holiday_overrides.rds"
  )
}

source("R/bancos_persistence.R")   # for .schema_bancos_confirmados in group 3
source("R/pasivos_persistence.R")
source("R/pasivos_capabilities.R")
source("R/pasivos_audit.R")
source("R/forecasting_service.R")

# ── Test runner helpers ───────────────────────────────────────────────────────
.pass <- 0L
.fail <- 0L

.chk <- function(actual, expected, label) {
  ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
  if (ok) {
    cat(sprintf("  PASS  %s\n", label))
    .pass <<- .pass + 1L
  } else {
    cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                label, deparse(expected), deparse(actual)))
    .fail <<- .fail + 1L
  }
}

.chk_true <- function(expr, label) .chk(isTRUE(expr), TRUE, label)
.chk_warn <- function(expr, label) {
  w <- tryCatch({ expr; NULL }, warning = function(w) w$message)
  if (!is.null(w)) {
    cat(sprintf("  PASS  %s  [warn: %s]\n", label, w))
    .pass <<- .pass + 1L
  } else {
    cat(sprintf("  FAIL  %s  (no warning emitted)\n", label))
    .fail <<- .fail + 1L
  }
}

# ── S3 mock ───────────────────────────────────────────────────────────────────
# Replace .s3_read / .s3_write with in-memory stubs so tests run offline.
.mock_store     <- new.env(parent = emptyenv())
.real_s3_read   <- .s3_read
.real_s3_write  <- .s3_write

.s3_read  <- function(key) .mock_store[[key]]
.s3_write <- function(obj, key) { .mock_store[[key]] <- obj; invisible(TRUE) }

# Clean the mock store between test groups
.reset_mock <- function() rm(list = ls(.mock_store), envir = .mock_store)

# ── 1. Schema round-trip ──────────────────────────────────────────────────────
cat("── 1. Schema round-trip ─────────────────────────────────────────────────\n")

.chk_schema <- function(schema_fn, expected_cols, label) {
  s <- schema_fn()
  .chk_true(tibble::is_tibble(s),       paste(label, "is tibble"))
  .chk_true(nrow(s) == 0L,              paste(label, "zero rows"))
  missing <- setdiff(expected_cols, names(s))
  .chk_true(length(missing) == 0L,
            paste(label, "has all cols", if (length(missing)) paste("(missing:", paste(missing, collapse=", "), ")") else ""))
}

.chk_schema(
  .schema_pasivos_liability,
  c("id","categoria","nombre","empresa","parte","moneda_pago","cotizado_en",
    "recurrence_type","recurrence_params","amount_default",
    "tarjeta_provision_source","tarjeta_closing_day","tarjeta_due_day",
    "principal_original","saldo_capital","tasa_actual","tasa_tipo",
    "tasa_spread","fecha_inicio","fecha_vence","plazo_meses","dia_pago",
    "metodo_amortizacion","schedule_imported","cargos_iniciales",
    "valor_residual","modifier_ids","estado","created_by","created_at",
    "updated_by","updated_at"),
  "liability schema"
)

.chk_schema(
  .schema_pasivos_provision,
  c("id","liability_id","origin","occurrence_index","estado",
    "fecha_calculada","fecha_efectiva","policy_ids",
    "empresa","parte","moneda_pago","amount_pago","fx_rate_used",
    "componente_capital","componente_interes","componente_fees","componente_iva",
    "amount_pago_override","fecha_efectiva_override",
    "documento","referencia","manual_inv_id","pagar_hoy_id","bancos_conf_id",
    "reverted_count","generated_by","generated_at","last_edited_by","last_edited_at"),
  "provision schema"
)

.chk_schema(
  .schema_pasivos_modifier,
  c("id","scope_type","scope_id","type","target_field","estimate_method",
    "frozen_value","enabled","display_label","created_by","created_at","updated_at"),
  "modifier schema"
)

.chk_schema(
  .schema_pasivos_estimates,
  c("metric","fecha","value","source_method","is_frozen","updated_by","updated_at"),
  "estimates schema"
)

.chk_schema(
  .schema_pasivos_card_expense,
  c("id","liability_id","fecha","comercio","monto","moneda",
    "categoria","notas","registered_by","registered_at"),
  "card_expense schema"
)

.chk_schema(
  .schema_pasivos_audit,
  c("id","ts","user","empresa","action_type","target_kind","target_id",
    "payload_before","payload_after","session_id","notes"),
  "audit schema"
)

# Column type checks
s_est <- .schema_pasivos_estimates()
.chk_true(inherits(s_est$fecha, "Date"),     "estimates$fecha is Date")
.chk_true(is.logical(s_est$is_frozen),       "estimates$is_frozen is logical")
.chk_true(is.numeric(s_est$value),           "estimates$value is numeric")

s_lia <- .schema_pasivos_liability()
.chk_true(is.list(s_lia$recurrence_params),  "liability$recurrence_params is list-col")
.chk_true(is.list(s_lia$schedule_imported),  "liability$schedule_imported is list-col")
.chk_true(is.list(s_lia$cargos_iniciales),   "liability$cargos_iniciales is list-col")
.chk_true(is.list(s_lia$valor_residual),     "liability$valor_residual is list-col")
.chk_true(is.list(s_lia$modifier_ids),       "liability$modifier_ids is list-col")

s_pro <- .schema_pasivos_provision()
.chk_true(inherits(s_pro$fecha_calculada, "Date"),          "provision$fecha_calculada is Date")
.chk_true(inherits(s_pro$fecha_efectiva_override, "Date"),  "provision$fecha_efectiva_override is Date")
.chk_true(is.integer(s_pro$reverted_count),                 "provision$reverted_count is integer")

# ── 2. Persistence round-trip ──────────────────────────────────────────────────
cat("── 2. Persistence round-trip ────────────────────────────────────────────\n")

.reset_mock()

# pasivos_provisions (no list cols — standard normalize)
prov_sample <- tibble::tibble(
  id               = c("p1","p2","p3"),
  liability_id     = c("l1","l1","l2"),
  origin           = c("rule","manual","rule"),
  occurrence_index = c(1L, NA_integer_, 2L),
  estado           = c("provisional","provisional","converted"),
  fecha_calculada  = as.Date(c("2026-06-01","2026-07-01","2026-08-01")),
  fecha_efectiva   = as.Date(c("2026-06-02","2026-07-02","2026-08-04")),
  policy_ids       = c("pol1","","pol1"),
  empresa          = c("NG","NTS","NG"),
  parte            = c("Proveedor A","Proveedor B","Proveedor A"),
  codigo_parte     = c("C001","C002","C001"),
  moneda_pago      = c("MXN","USD","MXN"),
  cotizado_en      = c(NA_character_, "USD", NA_character_),
  amount_pago      = c(1000, 500, 2000),
  amount_cotizado  = c(NA_real_, 500, NA_real_),
  fx_rate_used     = c(NA_real_, NA_real_, NA_real_),
  componente_capital = c(NA_real_, NA_real_, NA_real_),
  componente_interes = c(NA_real_, NA_real_, NA_real_),
  componente_fees    = c(NA_real_, NA_real_, NA_real_),
  componente_iva     = c(NA_real_, NA_real_, NA_real_),
  amount_pago_override     = c(NA_real_, NA_real_, NA_real_),
  amount_cotizado_override = c(NA_real_, NA_real_, NA_real_),
  fecha_efectiva_override  = as.Date(c(NA,NA,NA)),
  documento        = c("DOC1","DOC2","DOC3"),
  referencia       = c("REF1",NA_character_,"REF3"),
  notas            = c(NA_character_, NA_character_, NA_character_),
  manual_inv_id    = c(NA_character_, NA_character_, NA_character_),
  pagar_hoy_id     = c(NA_character_, NA_character_, NA_character_),
  bancos_conf_id   = c(NA_character_, NA_character_, NA_character_),
  reverted_count   = c(0L, 0L, 1L),
  generated_by     = c("system","dev","system"),
  generated_at     = as.POSIXct(c("2026-05-01","2026-05-01","2026-05-01")),
  last_edited_by   = c(NA_character_, NA_character_, NA_character_),
  last_edited_at   = as.POSIXct(c(NA, NA, NA))
)

save_pasivos_provisions(prov_sample)
prov_rt <- load_pasivos_provisions()
.chk(nrow(prov_rt), 3L,   "provisions round-trip: 3 rows")
.chk(prov_rt$id,    prov_sample$id, "provisions round-trip: id col")
.chk(prov_rt$amount_pago, prov_sample$amount_pago, "provisions round-trip: amount_pago")
.chk(prov_rt$fecha_calculada, prov_sample$fecha_calculada, "provisions round-trip: fecha_calculada")

# pasivos_estimates (no list cols)
est_sample <- tibble::tibble(
  metric        = c("fx_usd_mxn", "sofr", "fx_usd_mxn"),
  fecha         = as.Date(c("2026-06-01","2026-06-01","2026-07-01")),
  value         = c(18.5, 5.25, 18.7),
  source_method = c("spot","spot","spot"),
  is_frozen     = c(FALSE, FALSE, FALSE),
  updated_by    = c("dev","dev","dev"),
  updated_at    = as.POSIXct(c("2026-05-01","2026-05-01","2026-05-01"))
)
save_pasivos_estimates(est_sample)
est_rt <- load_pasivos_estimates()
.chk(nrow(est_rt), 3L,            "estimates round-trip: 3 rows")
.chk(est_rt$value, est_sample$value, "estimates round-trip: value col")
.chk_true(is.logical(est_rt$is_frozen), "estimates round-trip: is_frozen stays logical")

# pasivos_liabilities (with list cols)
lia_sample <- tibble::tibble(
  id                 = c("l1","l2","l3"),
  categoria          = c("regular","financiero","tarjeta"),
  subcategoria       = c("servicios","credito_simple","tarjeta_credito"),
  flavor             = c(NA_character_,"credito_simple",NA_character_),
  nombre             = c("Internet","Crédito Bancario","Tarjeta Corp"),
  empresa            = c("NG","NTS","NG"),
  parte              = c("Telmex","HSBC","AMEX"),
  codigo_parte       = c("T001","H001","A001"),
  referencia_default = c("INET-{MM}",NA_character_,NA_character_),
  documento_template = c(NA_character_,NA_character_,NA_character_),
  moneda_pago        = c("MXN","MXN","MXN"),
  cotizado_en        = c(NA_character_,NA_character_,NA_character_),
  recurrence_type    = c("monthly_day",NA_character_,"monthly_day"),
  recurrence_params  = list(
    list(day = 5L),
    NULL,
    list(day = 15L)
  ),
  amount_default     = c(2500, NA_real_, NA_real_),
  amount_default_cot = c(NA_real_, NA_real_, NA_real_),
  tarjeta_provision_source = c(NA_character_, NA_character_, "manual"),
  tarjeta_closing_day  = c(NA_integer_, NA_integer_, 20L),
  tarjeta_due_day      = c(NA_integer_, NA_integer_, 10L),
  tarjeta_credit_limit = c(NA_real_, NA_real_, 50000),
  principal_original   = c(NA_real_, 1000000, NA_real_),
  saldo_capital        = c(NA_real_, 800000,  NA_real_),
  tasa_actual          = c(NA_real_, 12.5,    NA_real_),
  tasa_tipo            = c(NA_character_, "fija", NA_character_),
  tasa_spread          = c(NA_real_, 0, NA_real_),
  tasa_estimate_method = c(NA_character_, NA_character_, NA_character_),
  fecha_inicio         = as.Date(c(NA, "2024-01-01", NA)),
  fecha_vence          = as.Date(c(NA, "2029-01-01", NA)),
  plazo_meses          = c(NA_integer_, 60L, NA_integer_),
  dia_pago             = c(NA_integer_, 5L, NA_integer_),
  metodo_amortizacion  = c(NA_character_, "francesa", NA_character_),
  schedule_imported    = list(NULL, NULL, NULL),
  cargos_iniciales     = list(NULL, list(list(descripcion="Comisión", monto=500, moneda="MXN", fecha="2024-01-01")), NULL),
  valor_residual       = list(NULL, NULL, NULL),
  modifier_ids         = list(character(0), character(0), character(0)),
  estado               = c("active","active","active"),
  notas                = c(NA_character_, NA_character_, NA_character_),
  created_by           = c("dev","dev","dev"),
  created_at           = as.POSIXct(c("2026-05-01","2026-05-01","2026-05-01")),
  updated_by           = c("dev","dev","dev"),
  updated_at           = as.POSIXct(c("2026-05-01","2026-05-01","2026-05-01"))
)
save_pasivos_liabilities(lia_sample)
lia_rt <- load_pasivos_liabilities()
.chk(nrow(lia_rt), 3L,              "liabilities round-trip: 3 rows")
.chk(lia_rt$id,    lia_sample$id,   "liabilities round-trip: id col")
.chk_true(is.list(lia_rt$recurrence_params),  "liabilities round-trip: recurrence_params is list")
.chk_true(is.list(lia_rt$cargos_iniciales),   "liabilities round-trip: cargos_iniciales is list")
.chk_true(is.list(lia_rt$modifier_ids),       "liabilities round-trip: modifier_ids is list")

# Spot-check list-col values survived the round-trip
.chk(as.numeric(lia_rt$recurrence_params[[1]]$day), 5,
     "liabilities round-trip: recurrence_params[[1]]$day value=5")
.chk_true(
  !is.null(lia_rt$cargos_iniciales[[2]][[1]]$descripcion),
  "liabilities round-trip: cargos_iniciales[[2]] has descripcion"
)
.chk(lia_rt$cargos_iniciales[[2]][[1]]$descripcion, "Comisión",
     "liabilities round-trip: cargos descripcion value")

# load returns empty schema when nothing stored
.reset_mock()
.chk(nrow(load_pasivos_provisions()),    0L, "load_pasivos_provisions empty → 0 rows")
.chk(nrow(load_pasivos_liabilities()),   0L, "load_pasivos_liabilities empty → 0 rows")
.chk(nrow(load_pasivos_modifiers()),     0L, "load_pasivos_modifiers empty → 0 rows")
.chk(nrow(load_pasivos_estimates()),     0L, "load_pasivos_estimates empty → 0 rows")
.chk(nrow(load_pasivos_card_expenses()), 0L, "load_pasivos_card_expenses empty → 0 rows")
.chk(nrow(load_pasivos_audit()),         0L, "load_pasivos_audit empty → 0 rows")

# ── 3. Backwards compatibility ────────────────────────────────────────────────
cat("── 3. Backwards compatibility ───────────────────────────────────────────\n")

# .schema_manual: old rows without provision_id / liability_id / referencia
old_manual <- tibble::tibble(
  id        = "m1",
  ledger    = "AP",
  Empresa   = "NG",
  Moneda    = "MXN",
  Documento = "DOC-OLD",
  Factura   = "F001",
  Parte     = "Proveedor",
  Codigo    = "C001",
  Importe   = 1000,
  `Abono futuro` = 0,
  `Fecha de vencimiento` = as.Date("2026-06-01"),
  Notas     = NA_character_,
  created_by  = "dev",
  created_at  = Sys.time(),
  updated_at  = Sys.time()
)
norm_manual <- .normalize(old_manual, .schema_manual)
.chk_true("provision_id" %in% names(norm_manual),  "manual compat: provision_id col added")
.chk_true("liability_id" %in% names(norm_manual),  "manual compat: liability_id col added")
.chk_true("referencia"   %in% names(norm_manual),  "manual compat: referencia col added")
.chk_true(is.na(norm_manual$provision_id[1]),       "manual compat: provision_id is NA")
.chk_true(is.na(norm_manual$liability_id[1]),       "manual compat: liability_id is NA")
.chk_true(is.na(norm_manual$referencia[1]),         "manual compat: referencia is NA")
# Existing columns must be intact
.chk(norm_manual$Documento[1], "DOC-OLD",           "manual compat: Documento unchanged")
.chk(norm_manual$Importe[1],   1000,                "manual compat: Importe unchanged")

# .schema_pagar_hoy: old rows without provision_id / liability_id
old_ph <- tibble::tibble(
  id        = "ph1",
  ledger    = "AP",
  Empresa   = "NG",
  Moneda    = "MXN",
  Documento = "FAC-OLD",
  Parte     = "Proveedor",
  Codigo    = "C001",
  tipo_item = "factura",
  Importe   = 500,
  FechaVenc = as.Date("2026-06-15"),
  staged_by = "dev",
  staged_at = Sys.time(),
  status    = "pending"
)
norm_ph <- .normalize(old_ph, .schema_pagar_hoy)
.chk_true("provision_id" %in% names(norm_ph), "pagar_hoy compat: provision_id col added")
.chk_true("liability_id" %in% names(norm_ph), "pagar_hoy compat: liability_id col added")
.chk_true(is.na(norm_ph$provision_id[1]),     "pagar_hoy compat: provision_id is NA")
.chk(norm_ph$status[1], "pending",            "pagar_hoy compat: status unchanged")

# .schema_bancos_confirmados: old rows without provision_id
old_conf <- tibble::tibble(
  confirmacion_id = "c1",
  agenda_item_id  = "ph1",
  empresa         = "NG",
  parte           = "Proveedor",
  documento       = "FAC-OLD",
  codigo          = "C001",
  importe         = 500,
  moneda          = "MXN",
  cuenta_id       = "acc1",
  fecha           = as.Date("2026-06-15"),
  tipo            = "pago",
  mov_id          = NA_character_,
  confirmado_at   = Sys.time(),
  eliminado       = FALSE
)
norm_conf <- .normalize(old_conf, .schema_bancos_confirmados)
.chk_true("provision_id" %in% names(norm_conf), "bancos_conf compat: provision_id col added")
.chk_true(is.na(norm_conf$provision_id[1]),     "bancos_conf compat: provision_id is NA")
.chk(norm_conf$importe[1], 500,                 "bancos_conf compat: importe unchanged")
.chk(norm_conf$eliminado[1], FALSE,             "bancos_conf compat: eliminado unchanged")

# ── 4. Capability stub ────────────────────────────────────────────────────────
cat("── 4. Capability stub ───────────────────────────────────────────────────\n")

for (cap in PASIVOS_CAPABILITIES) {
  .chk_true(has_capability("dev", cap),
            paste("has_capability TRUE:", cap))
}

# Unknown capability returns FALSE and warns
.chk(has_capability("dev", "pasivos.unknown_capability"), FALSE,
     "has_capability FALSE for unknown cap")
.chk_warn(has_capability("dev", "pasivos.does_not_exist"),
          "has_capability warns on unknown cap")

# empresa parameter is accepted without error
.chk_true(has_capability("dev", "pasivos.view", empresa = "NG"),
          "has_capability accepts empresa param")

# ── 5. Audit logger ───────────────────────────────────────────────────────────
cat("── 5. Audit logger ──────────────────────────────────────────────────────\n")

.reset_mock()

id1 <- pasivos_log_audit(
  action_type = "liability.created",
  user        = "dev",
  empresa     = "NG",
  target_kind = "liability",
  target_id   = "l1",
  before      = NULL,
  after       = list(nombre = "Internet", moneda_pago = "MXN")
)
.chk_true(nchar(id1) > 0, "audit: returns non-empty id")

log1 <- load_pasivos_audit()
.chk(nrow(log1), 1L,                   "audit: 1 row after first write")
.chk(log1$action_type[1], "liability.created", "audit: action_type correct")
.chk(log1$user[1],        "dev",               "audit: user correct")
.chk(log1$target_id[1],   "l1",                "audit: target_id correct")
.chk_true(is.na(log1$payload_before[1]),        "audit: payload_before NA for NULL")
.chk_true(!is.na(log1$payload_after[1]),        "audit: payload_after is not NA")

# payload_after must be valid JSON
parsed <- tryCatch(jsonlite::fromJSON(log1$payload_after[1]), error = function(e) NULL)
.chk_true(!is.null(parsed),                   "audit: payload_after is valid JSON")
.chk(parsed$nombre, "Internet",               "audit: payload_after contains nombre")

# Append-only: second write must not lose the first row
id2 <- pasivos_log_audit(
  action_type = "provision.edited",
  user        = "dev",
  empresa     = "NG",
  target_kind = "provision",
  target_id   = "p1",
  before      = list(amount_pago = 1000),
  after       = list(amount_pago = 1200)
)
log2 <- load_pasivos_audit()
.chk(nrow(log2), 2L,          "audit: 2 rows after second write (append-only)")
.chk(log2$id[1], id1,         "audit: first row id preserved")
.chk(log2$id[2], id2,         "audit: second row id appended")

# JSON with list-col / NA values round-trips without error
id3 <- pasivos_log_audit(
  action_type = "modifier.added",
  user        = "dev",
  empresa     = NA_character_,
  target_kind = "modifier",
  target_id   = "mod1",
  before      = NULL,
  after       = list(type = "fx_rate", frozen_value = NA, enabled = TRUE)
)
log3 <- load_pasivos_audit()
.chk(nrow(log3), 3L, "audit: 3 rows after third write")
.chk_true(!is.na(log3$payload_after[3]), "audit: NA in list serialized without error")

# Unknown action_type should warn but still write
id4 <- withCallingHandlers(
  pasivos_log_audit("unknown.action", "dev", target_kind = "bulk", target_id = "x"),
  warning = function(w) invokeRestart("muffleWarning")
)
.chk_true(!is.null(id4), "audit: unknown action_type still returns id (with warning)")

# ── 6. Forecasting stub ───────────────────────────────────────────────────────
cat("── 6. Forecasting stub ──────────────────────────────────────────────────\n")

.reset_mock()

# Empty store → NA
.chk(is.na(forecasting_get_estimate("fx_usd_mxn", Sys.Date())), TRUE,
     "forecasting: NA on empty store")

# Seed two dates
forecasting_set_estimate("fx_usd_mxn", as.Date("2026-05-01"), 18.5, user = "dev")
forecasting_set_estimate("fx_usd_mxn", as.Date("2026-06-01"), 18.7, user = "dev")

# Exact date match
.chk(forecasting_get_estimate("fx_usd_mxn", as.Date("2026-05-01")), 18.5,
     "forecasting: exact date match")
.chk(forecasting_get_estimate("fx_usd_mxn", as.Date("2026-06-01")), 18.7,
     "forecasting: exact date match (2nd row)")

# Past fallback: date between the two → most recent on-or-before = May row
.chk(forecasting_get_estimate("fx_usd_mxn", as.Date("2026-05-15")), 18.5,
     "forecasting: past fallback returns most-recent-on-or-before")

# No data before fecha: returns earliest known
.chk(forecasting_get_estimate("fx_usd_mxn", as.Date("2025-01-01")), 18.5,
     "forecasting: no past data → earliest known value")

# Unknown metric → NA
.chk(is.na(forecasting_get_estimate("sofr", as.Date("2026-05-01"))), TRUE,
     "forecasting: NA for unknown metric")

# Frozen value overrides everything
forecasting_freeze_metric("fx_usd_mxn", 99.9, user = "dev")
.chk(forecasting_get_estimate("fx_usd_mxn", as.Date("2026-05-01")), 99.9,
     "forecasting: frozen value overrides exact-date match")
.chk(forecasting_get_estimate("fx_usd_mxn", as.Date("2025-01-01")), 99.9,
     "forecasting: frozen value overrides past-fallback")

# Unfreeze restores normal lookup
forecasting_unfreeze_metric("fx_usd_mxn", user = "dev")
.chk(forecasting_get_estimate("fx_usd_mxn", as.Date("2026-05-01")), 18.5,
     "forecasting: after unfreeze, exact-date match restored")

# Upsert: set_estimate on existing (metric, fecha) updates in place
forecasting_set_estimate("fx_usd_mxn", as.Date("2026-05-01"), 19.0, user = "dev")
est_after <- load_pasivos_estimates()
may_rows <- est_after[est_after$metric == "fx_usd_mxn" &
                        !is.na(est_after$fecha) &
                        est_after$fecha == as.Date("2026-05-01") &
                        !isTRUE(est_after$is_frozen), ]
.chk(nrow(may_rows), 1L,   "forecasting: upsert does not duplicate row")
.chk(may_rows$value[1], 19.0, "forecasting: upsert updates value")
.chk(forecasting_get_estimate("fx_usd_mxn", as.Date("2026-05-01")), 19.0,
     "forecasting: get after upsert returns new value")

# list_metrics
metrics <- forecasting_list_metrics()
.chk_true("fx_usd_mxn" %in% metrics, "forecasting_list_metrics: fx_usd_mxn present")

# ── 7. Existing tests regression ──────────────────────────────────────────────
cat("── 7. Existing policy engine tests ─────────────────────────────────────\n")
# Run the existing suite in a child environment to avoid counter interference.
.pre_pass <- .pass; .pre_fail <- .fail
tryCatch({
  local({
    .pass <- 0L; .fail <- 0L
    source("tests/test_policy_engine.R", local = TRUE)
    cat(sprintf("  [policy_engine] %d pass, %d fail\n", .pass, .fail))
    if (.fail > 0L) stop("[policy_engine] regression failures")
  })
  cat("  PASS  policy engine regression (no failures)\n")
  .pass <<- .pass + 1L
}, error = function(e) {
  cat(sprintf("  FAIL  policy engine regression: %s\n", e$message))
  .fail <<- .fail + 1L
})

# ── Summary ───────────────────────────────────────────────────────────────────
cat(sprintf(
  "\n── Results: %d passed, %d failed ─────────────────────────────────────────\n",
  .pass, .fail
))
if (.fail > 0L) stop(sprintf("%d test(s) failed — see output above.", .fail))

# ── Restore real S3 functions ─────────────────────────────────────────────────
.s3_read  <- .real_s3_read
.s3_write <- .real_s3_write
