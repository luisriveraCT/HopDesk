# =============================================================================
# tests/test_pasivos_stage4_list.R
# Stage 4: Lista de pasivos — filtering, state changes, action logic.
# Run from project root:  source("tests/test_pasivos_stage4_list.R")
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(uuid)
})

source("R/persistence.R")
source("R/pasivos_schemas.R")

if (!exists("S3_KEYS")) {
  S3_KEYS <- list(
    pasivos_liabilities   = "pasivos_liabilities.rds",
    pasivos_provisions    = "pasivos_provisions.rds",
    pasivos_modifiers     = "pasivos_modifiers.rds",
    pasivos_estimates     = "pasivos_estimates.rds",
    pasivos_card_expenses = "pasivos_card_expenses.rds",
    pasivos_audit         = "pasivos_audit.rds",
    bancos_cuentas        = "bancos_cuentas.rds",
    bancos_movimientos    = "bancos_movimientos.rds",
    bancos_confirmados    = "bancos_confirmados.rds",
    policy_catalog        = "policy_catalog.rds",
    partner_policies      = "partner_policies.rds",
    policy_moves          = "policy_moves.rds",
    holiday_overrides     = "holiday_overrides.rds"
  )
}
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
if (!exists("CURRENCIES")) CURRENCIES <- c("MXN","USD","EUR","GBP","CAD","JPY","CHF","AUD","CNY","BRL")

source("R/bancos_persistence.R")
source("R/pasivos_persistence.R")
source("R/pasivos_capabilities.R")
source("R/pasivos_audit.R")
source("R/forecasting_service.R")
source("R/policy_engine.R")
source("R/pasivos_engine.R")

.pass <- 0L; .fail <- 0L
.chk <- function(actual, expected, label) {
  ok <- isTRUE(tryCatch(all.equal(actual, expected, check.attributes = FALSE),
                        error = function(e) FALSE))
  if (ok) { cat(sprintf("  PASS  %s\n", label)); .pass <<- .pass + 1L }
  else { cat(sprintf("  FAIL  %s\n        expected: %s\n        got:      %s\n",
                     label, deparse(expected), deparse(actual))); .fail <<- .fail + 1L }
}
.chk_true <- function(expr, label) .chk(isTRUE(expr), TRUE, label)

.mock_store <- new.env(parent = emptyenv())
.s3_read  <- function(key) .mock_store[[key]]
.s3_write <- function(obj, key) { .mock_store[[key]] <- obj; invisible(TRUE) }
.reset_mock <- function() rm(list = ls(.mock_store), envir = .mock_store)

# ── Liability factory ──────────────────────────────────────────────────────────
.make_liability <- function(id = uuid::UUIDgenerate(), ...) {
  base <- tibble::tibble(
    id = id, categoria = "regular", subcategoria = "servicios",
    flavor = NA_character_, nombre = "Test Liab", empresa = "NCS",
    parte = "Proveedor A", codigo_parte = "C001",
    referencia_default = NA_character_, documento_template = NA_character_,
    moneda_pago = "MXN", cotizado_en = NA_character_,
    recurrence_type = "monthly_day",
    recurrence_params = list(list(day = 15L)),
    amount_default = 2000, amount_default_cot = NA_real_,
    tarjeta_provision_source = NA_character_,
    tarjeta_closing_day = NA_integer_, tarjeta_due_day = NA_integer_,
    tarjeta_credit_limit = NA_real_,
    principal_original = NA_real_, saldo_capital = NA_real_,
    tasa_actual = NA_real_, tasa_tipo = "fija", tasa_spread = NA_real_,
    tasa_estimate_method = NA_character_,
    fecha_inicio = as.Date(NA), fecha_vence = as.Date(NA),
    plazo_meses = NA_integer_, dia_pago = NA_integer_,
    metodo_amortizacion = NA_character_,
    schedule_imported = list(NULL), cargos_iniciales = list(NULL),
    valor_residual = list(NULL), modifier_ids = list(character(0)),
    estado = "active", notas = NA_character_,
    created_by = "test", created_at = Sys.time(),
    updated_by = "test", updated_at = Sys.time()
  )
  overrides <- list(...)
  for (nm in names(overrides)) base[[nm]] <- overrides[[nm]]
  base
}

.make_prov_for <- function(liab_id, n = 6L, estado = "provisional") {
  tibble::tibble(
    id = replicate(n, uuid::UUIDgenerate()),
    liability_id = liab_id, origin = "rule",
    occurrence_index = seq_len(n), estado = estado,
    fecha_calculada = Sys.Date() + seq_len(n) * 30L,
    fecha_efectiva  = Sys.Date() + seq_len(n) * 30L,
    policy_ids = NA_character_,
    empresa = "NCS", parte = "P", codigo_parte = "",
    moneda_pago = "MXN", cotizado_en = NA_character_,
    amount_pago = 2000, amount_cotizado = NA_real_, fx_rate_used = NA_real_,
    componente_capital = NA_real_, componente_interes = NA_real_,
    componente_fees = NA_real_, componente_iva = NA_real_,
    amount_pago_override = NA_real_, amount_cotizado_override = NA_real_,
    fecha_efectiva_override = as.Date(NA),
    documento = "", referencia = "", notas = "",
    manual_inv_id = NA_character_, pagar_hoy_id = NA_character_,
    bancos_conf_id = NA_character_, reverted_count = 0L,
    generated_by = "test", generated_at = Sys.time(),
    last_edited_by = "test", last_edited_at = Sys.time()
  )
}

# =============================================================================
# Group L1 — Rendering / filtering (pure data checks)
# =============================================================================
cat("── L1. Filtering ─────────────────────────────────────────────────────────\n")
.reset_mock()

# Empty store → empty liabilities
save_pasivos_liabilities(.schema_pasivos_liability())
result_empty <- load_pasivos_liabilities()
.chk(nrow(result_empty), 0L, "L1: empty store → 0 rows")
.chk_true("nombre" %in% names(result_empty), "L1: schema columns present on empty result")

# 5 liabilities, mixed categorias
l1 <- .make_liability(id = "L1", empresa = "NCS", categoria = "regular", nombre = "A")
l2 <- .make_liability(id = "L2", empresa = "NCS", categoria = "financiero",
                       nombre = "B", flavor = "credito_simple")
l3 <- .make_liability(id = "L3", empresa = "NTS", categoria = "tarjeta",
                       nombre = "C")
l4 <- .make_liability(id = "L4", empresa = "NTS", categoria = "regular", nombre = "D")
l5 <- .make_liability(id = "L5", empresa = "NCS", categoria = "regular", nombre = "E")
all_liabs <- dplyr::bind_rows(l1, l2, l3, l4, l5)
save_pasivos_liabilities(all_liabs)

liabs <- load_pasivos_liabilities()
.chk(nrow(liabs), 5L, "L1: 5 liabilities stored and loaded")

# Filter by empresa "NCS" → l1, l2, l5
ncs <- liabs[liabs$empresa == "NCS", , drop = FALSE]
.chk(nrow(ncs), 3L, "L1: filter empresa=NCS → 3 rows")

# Filter by categoria "financiero" → l2
fin_only <- liabs[liabs$categoria == "financiero", , drop = FALSE]
.chk(nrow(fin_only), 1L, "L1: filter categoria=financiero → 1 row")
.chk(fin_only$nombre[1], "B", "L1: financiero row is 'B'")

# Default sort by (empresa, categoria, nombre)
sorted <- liabs[order(liabs$empresa, liabs$categoria, liabs$nombre), , drop = FALSE]
.chk(sorted$id[1], "L2", "L1: sorted first = L2 (NCS/financiero/B)")

# =============================================================================
# Group L2 — State actions (pause, archive)
# =============================================================================
cat("── L2. State actions ─────────────────────────────────────────────────────\n")
.reset_mock()

liab_pause <- .make_liability(id = "LP1", estado = "active")
liab_stay  <- .make_liability(id = "LP2", estado = "active")
save_pasivos_liabilities(dplyr::bind_rows(liab_pause, liab_stay))

provs_p <- dplyr::bind_rows(
  .make_prov_for("LP1", n = 6L, estado = "provisional"),
  .make_prov_for("LP2", n = 6L, estado = "provisional")
)
save_pasivos_provisions(provs_p)

# Simulate pause: flip estado, delete future provisional provisions for LP1
liabs_now <- load_pasivos_liabilities()
idx_lp1 <- which(liabs_now$id == "LP1")
liabs_now$estado[idx_lp1] <- "paused"
save_pasivos_liabilities(liabs_now)

provs_now <- load_pasivos_provisions()
keep <- !(provs_now$liability_id == "LP1" &
          provs_now$estado == "provisional" &
          !is.na(provs_now$fecha_efectiva) &
          provs_now$fecha_efectiva >= Sys.Date())
keep[is.na(keep)] <- TRUE
provs_now <- provs_now[keep, , drop = FALSE]
save_pasivos_provisions(provs_now)

liabs_after <- load_pasivos_liabilities()
provs_after <- load_pasivos_provisions()

.chk(liabs_after$estado[liabs_after$id == "LP1"], "paused",
     "L2: pause sets estado=paused")
.chk(liabs_after$estado[liabs_after$id == "LP2"], "active",
     "L2: other liability unaffected by pause")
.chk_true(!any(provs_after$liability_id == "LP1" & provs_after$estado == "provisional"),
          "L2: future provisions for LP1 removed after pause")
.chk_true(any(provs_after$liability_id == "LP2" & provs_after$estado == "provisional"),
          "L2: LP2 provisions untouched")

# Simulate resume: flip estado back
liabs_after$estado[liabs_after$id == "LP1"] <- "active"
save_pasivos_liabilities(liabs_after)
liabs_resumed <- load_pasivos_liabilities()
.chk(liabs_resumed$estado[liabs_resumed$id == "LP1"], "active",
     "L2: resume sets estado=active")

# Simulate archive
liabs_resumed$estado[liabs_resumed$id == "LP1"] <- "deleted"
save_pasivos_liabilities(liabs_resumed)
liabs_archived <- load_pasivos_liabilities()
.chk(liabs_archived$estado[liabs_archived$id == "LP1"], "deleted",
     "L2: archive sets estado=deleted")

# Archived liability hidden by default (show_archived = FALSE)
visible <- liabs_archived[liabs_archived$estado %in% c("active","paused"), , drop = FALSE]
.chk_true(!any(visible$id == "LP1"), "L2: archived liab hidden when show_archived=FALSE")

# ── Results ─────────────────────────────────────────────────────────────────
cat(sprintf("\nStage 4 Lista: %d PASS  /  %d FAIL\n\n", .pass, .fail))
if (.fail > 0) stop(sprintf("%d test(s) failed.", .fail))
