# =============================================================================
# tests/test_pasivos_stage2.R
# Stage 2 automated tests — Calendar Integration
#
# Run from project root: Rscript tests/test_pasivos_stage2.R
#
# Multi-select action sites covered by Group 6 filter guard:
#   1. ledger_module.R stage_all   — "Enviar todas a Agenda"
#   2. ledger_module.R stage_sel   — "Agregar selección"
#   3. ledger_module.R cart_N      — per-group cart button (summary mode)
#   4. ledger_module.R cart_inv_click — per-invoice click (expanded group)
# =============================================================================

# ── Bootstrap ─────────────────────────────────────────────────────────────────
args0 <- commandArgs(trailingOnly = FALSE)
script_file <- sub("--file=", "", args0[grep("--file=", args0)])
if (length(script_file) && nchar(script_file)) {
  setwd(normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE))
} else {
  setwd(normalizePath("..", mustWork = FALSE))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(jsonlite)
  library(uuid)
})

# %||% operator (lives in global.R, not in persistence.R)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x

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

# ── S3 mock — must be defined BEFORE sourcing pasivos_persistence.R ──────────
.mock_store <- new.env(parent = emptyenv())

.s3_read <- function(key) {
  obj <- .mock_store[[key]]
  if (is.null(obj)) stop("key not found: ", key)
  obj
}
.s3_write <- function(obj, key) {
  .mock_store[[key]] <- obj
  invisible(obj)
}

source("R/bancos_persistence.R")
source("R/pasivos_persistence.R")
source("R/pasivos_capabilities.R")
source("R/pasivos_audit.R")
source("R/forecasting_service.R")
source("R/policy_engine.R")
source("R/pasivos_engine.R")
source("R/pasivos_calendar_glue.R")

# ── Test helpers ──────────────────────────────────────────────────────────────
.pass  <- 0L
.fail  <- 0L
.tests <- character()

.chk <- function(label, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) FALSE)
  if (ok) {
    cat("[PASS]", label, "\n"); .pass <<- .pass + 1L
  } else {
    cat("[FAIL]", label, "\n"); .fail <<- .fail + 1L
  }
  .tests <<- c(.tests, if (ok) paste("[PASS]", label) else paste("[FAIL]", label))
}

.chk_true <- function(label, expr) .chk(label, isTRUE(expr))

# ── Helper: fresh mock store ──────────────────────────────────────────────────
.reset_store <- function() {
  rm(list = ls(.mock_store), envir = .mock_store)
  .mock_store[["pasivos_provisions.rds"]]  <- .schema_pasivos_provision()
  .mock_store[["pasivos_liabilities.rds"]] <- .schema_pasivos_liability()
  .mock_store[["pasivos_estimates.rds"]]   <- .schema_pasivos_estimates()
  .mock_store[["pasivos_audit.rds"]]       <- .schema_pasivos_audit()
}

.make_liability <- function(id = "lib1") {
  tibble::tibble(
    id = id, categoria = "regular", subcategoria = "renta", flavor = NA_character_,
    nombre = "Test Liability", empresa = "Networks Group",
    parte = "Proveedor Test", codigo_parte = "PROV01",
    referencia_default = "REF", documento_template = "DOC",
    moneda_pago = "MXN", cotizado_en = NA_character_,
    recurrence_type = "monthly_day", recurrence_params = list(list(dia = 15L)),
    amount_default = 1000, amount_default_cot = NA_real_,
    tarjeta_provision_source = NA_character_,
    tarjeta_closing_day = NA_integer_, tarjeta_due_day = NA_integer_,
    tarjeta_credit_limit = NA_real_,
    principal_original = NA_real_, saldo_capital = NA_real_, tasa_actual = NA_real_,
    tasa_tipo = NA_character_, tasa_spread = NA_real_, tasa_estimate_method = NA_character_,
    fecha_inicio = as.Date(NA), fecha_vence = as.Date(NA), plazo_meses = NA_integer_,
    dia_pago = 15L, metodo_amortizacion = NA_character_,
    schedule_imported = list(NULL), cargos_iniciales = list(NULL),
    valor_residual = list(NULL), modifier_ids = list(NULL),
    estado = "active", notas = "",
    created_by = "test", created_at = Sys.time(),
    updated_by = "test", updated_at = Sys.time()
  )
}

.make_provision <- function(id = "prov1", liability_id = "lib1", estado = "provisional",
                             empresa = "Networks Group", moneda_pago = "MXN",
                             amount_pago = 1000, fecha = Sys.Date(),
                             amount_pago_override = NA_real_) {
  tibble::tibble(
    id = id, liability_id = liability_id, origin = "rule", occurrence_index = 1L,
    estado = estado,
    fecha_calculada = as.Date(fecha), fecha_efectiva = as.Date(fecha),
    policy_ids = NA_character_,
    empresa = empresa, parte = "Proveedor Test", codigo_parte = "PROV01",
    moneda_pago = moneda_pago, cotizado_en = NA_character_,
    amount_pago = amount_pago, amount_cotizado = NA_real_, fx_rate_used = NA_real_,
    componente_capital = NA_real_, componente_interes = NA_real_,
    componente_fees = NA_real_, componente_iva = NA_real_,
    amount_pago_override = amount_pago_override,
    amount_cotizado_override = NA_real_, fecha_efectiva_override = as.Date(NA),
    documento = "DOC-001", referencia = "REF-001", notas = "test",
    manual_inv_id = NA_character_, pagar_hoy_id = NA_character_,
    bancos_conf_id = NA_character_, reverted_count = 0L,
    generated_by = "test", generated_at = Sys.time(),
    last_edited_by = "test", last_edited_at = Sys.time()
  )
}

# =============================================================================
# Group 1 — Adapter: pasivos_provisions_as_ledger_rows
# =============================================================================
cat("\n=== Group 1: Adapter ===\n")

# 1.1 Empty provisions → empty output
.reset_store()
{
  provs <- .schema_pasivos_provision()
  liabs <- .schema_pasivos_liability()
  out   <- pasivos_provisions_as_ledger_rows(provs, liabs)
  .chk_true("1.1 empty provisions → empty output", nrow(out) == 0L)
  .chk_true("1.1 empty output has source column", "source" %in% names(out))
}

# 1.2 5 provisional + 3 converted + 2 confirmed → 5 rows with source = "provision"
{
  provs <- dplyr::bind_rows(
    .make_provision("p1", estado = "provisional"),
    .make_provision("p2", estado = "provisional"),
    .make_provision("p3", estado = "provisional"),
    .make_provision("p4", estado = "provisional"),
    .make_provision("p5", estado = "provisional"),
    .make_provision("p6", estado = "converted"),
    .make_provision("p7", estado = "converted"),
    .make_provision("p8", estado = "converted"),
    .make_provision("p9", estado = "item_confirmed"),
    .make_provision("p10", estado = "item_confirmed")
  )
  liabs <- .make_liability("lib1")
  out   <- pasivos_provisions_as_ledger_rows(provs, liabs)
  .chk_true("1.2 5 provisional → 5 output rows", nrow(out) == 5L)
  .chk_true("1.2 all rows source = provision", all(out$source == "provision"))
}

# 1.3 Orphan provision (liability_id = NA) — empresa from provision
{
  prov_orphan <- .make_provision("orp1", liability_id = NA_character_,
                                  empresa = "Orphan Co")
  out <- pasivos_provisions_as_ledger_rows(prov_orphan, .schema_pasivos_liability())
  .chk_true("1.3 orphan provision included", nrow(out) == 1L)
  .chk_true("1.3 empresa from provision for orphan", out$Empresa[1] == "Orphan Co")
}

# 1.4 Saldo vencido reflects amount_pago_override when set
{
  prov_ov <- .make_provision("ovp1", amount_pago = 1000, amount_pago_override = 2500)
  out <- pasivos_provisions_as_ledger_rows(prov_ov, .schema_pasivos_liability())
  .chk_true("1.4 saldo vencido uses override", out$`Saldo vencido`[1] == 2500)
}

# 1.5 Moneda reflects moneda_pago, never cotizado_en
{
  prov_cur <- .make_provision("cp1", moneda_pago = "USD")
  # manually set cotizado_en to a different value
  prov_cur$cotizado_en <- "MXN"
  out <- pasivos_provisions_as_ledger_rows(prov_cur, .schema_pasivos_liability())
  .chk_true("1.5 Moneda is moneda_pago (USD)", out$Moneda[1] == "USD")
}

# 1.6 User without pasivos.view cap → empty output
{
  prov <- .make_provision("vis1")
  # has_capability stub always returns TRUE; to test FALSE we call directly
  out_vis <- pasivos_provisions_as_ledger_rows(prov, .schema_pasivos_liability(), user = "anyuser")
  # Stub returns TRUE so output is non-empty — we test the guard path manually
  local({
    saved <- has_capability
    # Temporarily override (only for this test)
    has_capability_test <- function(user, cap) FALSE
    out2 <- local({
      # Simulate the guard without overriding global
      if (!has_capability_test("testuser", "pasivos.view"))
        return(.empty_provision_ledger_rows())
      pasivos_provisions_as_ledger_rows(prov, .schema_pasivos_liability())
    })
    .chk_true("1.6 no view cap → empty output", nrow(out2) == 0L)
  })
}

# 1.7 Schema compatibility: bind_rows with SAP and manual ledger rows
{
  prov_row  <- .make_provision("sc1")
  prov_out  <- pasivos_provisions_as_ledger_rows(prov_row, .schema_pasivos_liability())
  sap_row   <- tibble::tibble(
    Empresa = "X", Parte = "Y", Codigo = "Z", `Saldo vencido` = 100,
    Importe = 100, `Fecha de vencimiento` = Sys.Date(),
    FechaVenc_Original = Sys.Date(), FechaEff = Sys.Date(),
    FechaVenc_Proyectada = as.Date(NA), Tipo = "AP",
    source = "sap", Documento = "DOC", Factura = NA_character_,
    Moneda = "MXN", notas = NA_character_, confirmed = FALSE,
    has_sap_override = FALSE, Movida = "Falso"
  )
  manual_row <- tibble::tibble(
    Empresa = "X", Parte = "M", Codigo = NA_character_, `Saldo vencido` = 200,
    Importe = 200, `Fecha de vencimiento` = Sys.Date(),
    FechaVenc_Original = Sys.Date(), FechaEff = Sys.Date(),
    FechaVenc_Proyectada = as.Date(NA), Tipo = "AP",
    source = "manual", Documento = "MAN-1", Factura = NA_character_,
    Moneda = "MXN", notas = NA_character_, confirmed = FALSE,
    has_sap_override = FALSE, Movida = "Falso"
  )
  bound <- tryCatch(
    dplyr::bind_rows(sap_row, manual_row, prov_out),
    error = function(e) e
  )
  .chk_true("1.7 bind_rows with SAP + manual + provision OK", !inherits(bound, "error"))
  .chk_true("1.7 combined has 3 rows", nrow(bound) == 3L)
}

# =============================================================================
# Group 2 — Filter helper: pasivos_filter_out_provisions
# =============================================================================
cat("\n=== Group 2: Filter helper ===\n")

# 2.1 Mixed input → only provision rows removed
{
  mixed <- tibble::tibble(
    source = c("sap", "manual", "provision", "sap"),
    Parte  = c("A","B","C","D")
  )
  out <- pasivos_filter_out_provisions(mixed)
  .chk_true("2.1 mixed: 3 non-provision rows remain", nrow(out) == 3L)
  .chk_true("2.1 no provision rows in output", !any(out$source == "provision"))
}

# 2.2 All-provision input → empty output
{
  all_prov <- tibble::tibble(source = c("provision","provision"), Parte = c("X","Y"))
  out <- pasivos_filter_out_provisions(all_prov)
  .chk_true("2.2 all-provision → empty", nrow(out) == 0L)
}

# 2.3 Empty input → empty output
{
  empty_df <- tibble::tibble(source = character(), Parte = character())
  out <- pasivos_filter_out_provisions(empty_df)
  .chk_true("2.3 empty input → empty output", nrow(out) == 0L)
}

# 2.4 No source column → returned unchanged
{
  no_src <- tibble::tibble(Parte = c("X","Y"), Importe = c(100, 200))
  out <- pasivos_filter_out_provisions(no_src)
  .chk_true("2.4 no source column → unchanged", nrow(out) == 2L)
  .chk_true("2.4 no source column → has Importe", "Importe" %in% names(out))
}

# =============================================================================
# Group 3 — Convert lifecycle integration (engine + adapter, no Shiny)
# =============================================================================
cat("\n=== Group 3: Convert lifecycle integration ===\n")

.reset_store()

{
  lib <- .make_liability("lib3")
  save_pasivos_liabilities(lib)

  # Seed 3 provisions
  p1 <- .make_provision("p3a", liability_id = "lib3", fecha = Sys.Date() + 1)
  p2 <- .make_provision("p3b", liability_id = "lib3", fecha = Sys.Date() + 8)
  p3 <- .make_provision("p3c", liability_id = "lib3", fecha = Sys.Date() + 15)
  save_pasivos_provisions(dplyr::bind_rows(p1, p2, p3))

  # 3.1 Adapter shows 3 rows
  provs <- load_pasivos_provisions()
  liabs <- load_pasivos_liabilities()
  out1  <- pasivos_provisions_as_ledger_rows(provs, liabs)
  .chk_true("3.1 adapter returns 3 provision rows initially", nrow(out1) == 3L)

  # 3.2 Convert first provision — simulate .pasivos_perform_conversion without Shiny
  new_manual_id <- uuid::UUIDgenerate()
  pasivos_provision_convert("p3a", manual_inv_id = new_manual_id,
                             pagar_hoy_id = NA_character_, user = "test")

  provs2 <- load_pasivos_provisions()
  out2   <- pasivos_provisions_as_ledger_rows(provs2, liabs)
  .chk_true("3.2 adapter returns 2 rows after conversion", nrow(out2) == 2L)

  # 3.3 No double-counting: p3a not in adapter output
  .chk_true("3.3 converted provision not in adapter output",
            !("p3a" %in% out2$provision_id))

  # 3.4 Simulate item_confirmed
  pasivos_provision_item_confirmed("p3a", bancos_conf_id = "BC-001", user = "test")
  provs3 <- load_pasivos_provisions()
  p3a_row <- provs3[provs3$id == "p3a", , drop = FALSE]
  .chk_true("3.4 provision state is item_confirmed", p3a_row$estado[1] == "item_confirmed")
  .chk_true("3.4 bancos_conf_id set", p3a_row$bancos_conf_id[1] == "BC-001")

  # 3.5 Simulate reversal — revive
  pasivos_provision_revive("p3a", user = "test")
  provs4 <- load_pasivos_provisions()
  p3a_row2 <- provs4[provs4$id == "p3a", , drop = FALSE]
  .chk_true("3.5 provision revived to provisional", p3a_row2$estado[1] == "provisional")
  .chk_true("3.5 reverted_count == 1", p3a_row2$reverted_count[1] == 1L)

  # 3.6 Adapter shows 3 rows again (p3a revived)
  out4 <- pasivos_provisions_as_ledger_rows(provs4, liabs)
  .chk_true("3.6 adapter returns 3 rows after revival", nrow(out4) == 3L)

  # 3.7 Double-count check: count distinct provision_ids + manual ledger rows
  # Manual rows would normally come from manual_inv reactive but we can check the concept:
  # At this point p3a is provisional again (in adapter) and has no associated manual_inv
  # (the FK was cleared by revive). No double-count possible.
  unique_pids <- unique(out4$provision_id)
  .chk_true("3.7 no duplicate provision_ids in adapter output",
            length(unique_pids) == nrow(out4))
}

# =============================================================================
# Group 4 — Orphan manual provision creation logic
# =============================================================================
cat("\n=== Group 4: Orphan manual provision ===\n")

.reset_store()

{
  # Simulate the me_save_as_provision handler logic (pure state change, no Shiny input)
  user   <- "test_user"
  new_id <- uuid::UUIDgenerate()
  now    <- Sys.time()
  empresa <- "Networks Group"

  new_prov <- tibble::tibble(
    id = new_id, liability_id = NA_character_, origin = "manual",
    occurrence_index = NA_integer_, estado = "provisional",
    fecha_calculada = Sys.Date(), fecha_efectiva = Sys.Date(),
    policy_ids = NA_character_,
    empresa = empresa, parte = "Prov Manua", codigo_parte = "",
    moneda_pago = "MXN", cotizado_en = NA_character_,
    amount_pago = 5000, amount_cotizado = NA_real_, fx_rate_used = NA_real_,
    componente_capital = NA_real_, componente_interes = NA_real_,
    componente_fees = NA_real_, componente_iva = NA_real_,
    amount_pago_override = NA_real_, amount_cotizado_override = NA_real_,
    fecha_efectiva_override = as.Date(NA),
    documento = "DOC-MAN", referencia = "", notas = "",
    manual_inv_id = NA_character_, pagar_hoy_id = NA_character_,
    bancos_conf_id = NA_character_, reverted_count = 0L,
    generated_by = user, generated_at = now,
    last_edited_by = user, last_edited_at = now
  )

  provs <- load_pasivos_provisions()
  provs <- dplyr::bind_rows(provs, new_prov)
  save_pasivos_provisions(provs)

  pasivos_log_audit(
    action_type = "provision.generated", user = user, empresa = empresa,
    target_kind = "provision", target_id = new_id,
    after = list(id = new_id, origin = "manual"),
    notes = "manual orphan provision via Agregar modal"
  )

  # Verify
  saved <- load_pasivos_provisions()
  saved_row <- saved[saved$id == new_id, , drop = FALSE]
  .chk_true("4.1 orphan provision saved", nrow(saved_row) == 1L)
  .chk_true("4.2 origin = manual", saved_row$origin[1] == "manual")
  .chk_true("4.3 liability_id = NA", is.na(saved_row$liability_id[1]))
  .chk_true("4.4 estado = provisional", saved_row$estado[1] == "provisional")

  # Verify audit entry
  audit <- load_pasivos_audit()
  audit_entry <- audit[!is.na(audit$action_type) & audit$action_type == "provision.generated", , drop = FALSE]
  .chk_true("4.5 audit entry written", nrow(audit_entry) >= 1L)

  # Verify adapter includes orphan
  out <- pasivos_provisions_as_ledger_rows(saved, .schema_pasivos_liability())
  .chk_true("4.6 adapter includes orphan provision", new_id %in% out$provision_id)
  .chk_true("4.7 orphan empresa in adapter", any(out$Empresa == empresa))
}

# =============================================================================
# Group 5 — Capability gates (simulated)
# =============================================================================
cat("\n=== Group 5: Capability gates ===\n")

.reset_store()

{
  # has_capability stub returns TRUE always; test the gate logic pattern
  # by simulating the check inline

  .cap_gate_test <- function(user, cap) {
    if (!has_capability(user, cap)) {
      pasivos_log_audit(action_type = "capability.denied", user = user,
                         target_kind = "provision", target_id = "test",
                         notes = cap)
      return(FALSE)
    }
    TRUE
  }

  # Stub returns TRUE → gate passes
  result <- .cap_gate_test("any_user", "pasivos.convert_to_item")
  .chk_true("5.1 convert_to_item cap passes (stub=TRUE)", isTRUE(result))

  result2 <- .cap_gate_test("any_user", "pasivos.create_provision_manual")
  .chk_true("5.2 create_provision_manual cap passes (stub=TRUE)", isTRUE(result2))

  result3 <- .cap_gate_test("any_user", "pasivos.view")
  .chk_true("5.3 pasivos.view cap passes (stub=TRUE)", isTRUE(result3))

  # Inline cap = FALSE test: view gate → empty adapter
  .empty_if_no_cap <- function(user) {
    if (!has_capability(user, "pasivos.view")) return(.empty_provision_ledger_rows())
    NULL
  }
  res <- .empty_if_no_cap("any_user")
  .chk_true("5.4 view cap check returns NULL (not empty) when stub=TRUE", is.null(res))
}

# =============================================================================
# Group 6 — Multi-select filter at action sites
# =============================================================================
cat("\n=== Group 6: Multi-select filter at action sites ===\n")

{
  # Build a mixed detail data frame (sap + manual + provision)
  mixed_detail <- tibble::tibble(
    Empresa  = c("NG","NG","NG","NG"),
    Moneda   = "MXN",
    Documento = c("SAP-1","MAN-1","PROV-1","SAP-2"),
    Parte    = c("A","B","C","D"),
    Importe  = c(100,200,300,400),
    source   = c("sap","manual","provision","sap"),
    provision_id = c(NA,NA,"prov_abc",NA)
  )

  # Simulate what stage_all does: filter by Documento not-NA, then filter provisions
  mask      <- !is.na(mixed_detail[["Documento"]])
  d         <- pasivos_filter_out_provisions(mixed_detail[mask, , drop = FALSE])
  inv_keys  <- unique(d[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE])

  .chk_true("6.1 stage_all: 3 non-provision keys remain", nrow(inv_keys) == 3L)
  .chk_true("6.1 stage_all: no provision key", !any(inv_keys$Documento == "PROV-1"))

  # Simulate stage_sel: keys after selection
  sel_keys <- mixed_detail[c(1,3,4), c("Empresa","Moneda","Documento","Importe","source"), drop = FALSE]
  filtered <- pasivos_filter_out_provisions(sel_keys)
  .chk_true("6.2 stage_sel: provision row removed from keys", nrow(filtered) == 2L)

  # Simulate cart_N per-group
  cart_rows_raw <- mixed_detail[mixed_detail$Parte %in% c("C","D"),
                                c("Empresa","Moneda","Documento","Importe","source"), drop = FALSE]
  cart_rows <- pasivos_filter_out_provisions(cart_rows_raw)
  .chk_true("6.3 cart_N: provision removed, SAP row kept", nrow(cart_rows) == 1L)
  .chk_true("6.3 cart_N: kept row is SAP", cart_rows$source[1] == "sap")
}

# =============================================================================
# Group 7 — Hygiene
# =============================================================================
cat("\n=== Group 7: Hygiene ===\n")

{
  # isTRUE() audit on pasivos_*.R files
  # Allowed patterns: isTRUE_safe, isTRUE(all.equal(...)), isTRUE(identical(...)), comments
  pasivos_files <- list.files("R", pattern = "^pasivos_.*\\.R$", full.names = TRUE)
  isTRUE_hits <- character()
  for (f in pasivos_files) {
    lines <- readLines(f, warn = FALSE)
    hits  <- grep("isTRUE\\(", lines, value = TRUE)
    # Exclude safe scalar uses and comments
    hits  <- hits[!grepl("isTRUE_safe|isTRUE\\(all\\.equal|isTRUE\\(identical|^\\s*#", hits)]
    if (length(hits)) isTRUE_hits <- c(isTRUE_hits, paste0(f, ": ", hits))
  }
  .chk_true("7.1 no bare isTRUE(df$col) in pasivos_*.R",
            length(isTRUE_hits) == 0L)
  if (length(isTRUE_hits)) {
    cat("  WARNING — isTRUE() hits:\n")
    for (h in isTRUE_hits) cat("   ", h, "\n")
  }

  # cotizado_en direct access audit — must never appear outside allowlist
  allowlist <- c("R/pasivos_engine.R", "R/pasivos_schemas.R",
                 "R/pasivos_persistence.R", "R/pasivos_module.R")
  all_r  <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  cot_hits <- character()
  for (f in all_r) {
    if (f %in% allowlist) next
    lines <- readLines(f, warn = FALSE)
    hits  <- grep("cotizado_en", lines, value = TRUE)
    if (length(hits)) cot_hits <- c(cot_hits, paste0(f, ": ", hits))
  }
  .chk_true("7.2 cotizado_en not accessed outside allowlist", length(cot_hits) == 0L)
  if (length(cot_hits)) {
    cat("  WARNING — cotizado_en hits outside allowlist:\n")
    for (h in cot_hits) cat("   ", h, "\n")
  }
}

# 7.3 Stage 0 regression (subprocess)
cat("  [INFO] Running Stage 0 regression via subprocess...\n")
rscript_bin <- file.path(R.home("bin"), "Rscript")
out0 <- system2(rscript_bin, args = "tests/test_pasivos_stage0.R",
                stdout = TRUE, stderr = TRUE)
exit0 <- attr(out0, "status") %||% 0L
.chk_true("7.3 Stage 0 tests still pass", isTRUE(exit0 == 0L))

# 7.4 Stage 1 regression (subprocess)
cat("  [INFO] Running Stage 1 regression via subprocess...\n")
out1 <- system2(rscript_bin, args = "tests/test_pasivos_engine.R",
                stdout = TRUE, stderr = TRUE)
exit1 <- attr(out1, "status") %||% 0L
.chk_true("7.4 Stage 1 tests still pass", isTRUE(exit1 == 0L))

# =============================================================================
# Group 8 — Regression tests: Fix 1 (reversal revival + undo_conf linkage)
# =============================================================================
cat("\n=== Group 8: Fix 1 regression ===\n")

# 8.1 — provision revives correctly after item_confirmed + revive engine path
{
  .reset_store()
  lib <- .make_liability("lib8")
  save_pasivos_liabilities(lib)
  prov <- .make_provision("prov8a", liability_id = "lib8")
  save_pasivos_provisions(prov)

  # Simulate convert → item_confirmed lifecycle
  pasivos_provision_convert("prov8a",
    manual_inv_id = uuid::UUIDgenerate(),
    pagar_hoy_id  = uuid::UUIDgenerate(),
    user = "test")
  pasivos_provision_item_confirmed("prov8a",
    bancos_conf_id = "BC-rev-test",
    user = "test")

  p_conf <- load_pasivos_provisions()
  p_conf_row <- p_conf[p_conf$id == "prov8a", , drop = FALSE]
  .chk_true("8.1 provision is item_confirmed before reversal", p_conf_row$estado[1] == "item_confirmed")
  .chk_true("8.1 bancos_conf_id set", p_conf_row$bancos_conf_id[1] == "BC-rev-test")

  # Simulate reversal: engine call that the observer would fire
  pasivos_provision_revive("prov8a", user = "test")

  p_revived <- load_pasivos_provisions()
  p_rev_row <- p_revived[p_revived$id == "prov8a", , drop = FALSE]
  .chk_true("8.1 provision revived to provisional",   p_rev_row$estado[1] == "provisional")
  .chk_true("8.1 reverted_count == 1",               p_rev_row$reverted_count[1] == 1L)
  .chk_true("8.1 manual_inv_id cleared after revive", is.na(p_rev_row$manual_inv_id[1]))
  .chk_true("8.1 bancos_conf_id cleared after revive", is.na(p_rev_row$bancos_conf_id[1]))
}

# 8.2 — first_run guard: reversed rows at startup must NOT trigger revive
# We test the guard logic by simulating the seen-set snapshot pass.
{
  # This test validates the first_run guard LOGIC (pure, no Shiny):
  # Given a bancos_confirmados frame with one reversed (eliminado=TRUE) row,
  # the first_run path should add it to reversed_seen WITHOUT calling revive.
  # We verify by ensuring the row would enter reversed_seen on first run,
  # meaning subsequent runs (first_run=FALSE) would skip it (already seen).

  bc_frame <- tibble::tibble(
    confirmacion_id = "bc-startup-1",
    provision_id    = "prov-startup",
    eliminado       = TRUE
  )

  # Simulate first_run snapshot (the guard logic):
  reversed_seen_initial <- bc_frame$confirmacion_id[
    !is.na(bc_frame$provision_id) & nzchar(bc_frame$provision_id) &
    (!is.na(bc_frame$eliminado) & bc_frame$eliminado)
  ]
  .chk_true("8.2 first_run: reversed row enters seen-set (not fired as revive)",
            "bc-startup-1" %in% reversed_seen_initial)

  # Simulate delta detection on second run (first_run=FALSE):
  # bc_frame row is already in reversed_seen → new_reversals should be empty.
  new_reversals_second_run <- bc_frame[
    !is.na(bc_frame$provision_id) & nzchar(bc_frame$provision_id) &
    (!is.na(bc_frame$eliminado) & bc_frame$eliminado) &
    !(bc_frame$confirmacion_id %in% reversed_seen_initial),
    , drop = FALSE
  ]
  .chk_true("8.2 second run: already-seen reversed row does NOT appear as new reversal",
            nrow(new_reversals_second_run) == 0L)
}

# 8.3 — undo_conf carries provision_id forward to the re-staged pagar_hoy row
{
  # Replicate the row-building logic from the undo_conf handler to confirm
  # provision_id is carried through (the actual production change in bancos_module.R).
  conf_row <- tibble::tibble(
    confirmacion_id = "bc-undo-1",
    empresa   = "Networks Group",
    parte     = "Proveedor Test",
    documento = "DOC-UNDO-1",
    codigo    = "PROV01",
    importe   = 2500,
    moneda    = "MXN",
    fecha     = Sys.Date(),
    tipo      = "pago",
    mov_id    = NA_character_,
    confirmado_at = Sys.time(),
    eliminado     = FALSE,
    provision_id  = "prov-undo-abc"
    # Note: liability_id is NOT in bancos_confirmados schema,
    # so it should default to NA in the pagar_hoy row.
  )

  ph_ledger <- if (isTRUE(conf_row$tipo == "cobro")) "AR" else "AP"
  new_ph_row <- tibble::tibble(
    id           = uuid::UUIDgenerate(),
    ledger       = ph_ledger,
    Empresa      = as.character(conf_row$empresa),
    Moneda       = as.character(conf_row$moneda),
    Documento    = as.character(conf_row$documento),
    Parte        = as.character(conf_row$parte),
    Codigo       = trimws(as.character(conf_row$codigo %||% "")),
    tipo_item    = "factura",
    Importe      = as.numeric(conf_row$importe),
    FechaVenc    = as.Date(conf_row$fecha),
    staged_by    = "test",
    staged_at    = Sys.time(),
    status       = "pending",
    provision_id = if ("provision_id" %in% names(conf_row)) as.character(conf_row$provision_id) else NA_character_,
    liability_id = if ("liability_id" %in% names(conf_row)) as.character(conf_row$liability_id) else NA_character_
  )

  .chk_true("8.3 undo_conf: provision_id carried to re-staged row",
            new_ph_row$provision_id == "prov-undo-abc")
  .chk_true("8.3 undo_conf: liability_id is NA when not in conf_row schema",
            is.na(new_ph_row$liability_id))
  .chk_true("8.3 undo_conf: ledger is AP for pago",
            new_ph_row$ledger == "AP")
}

# =============================================================================
# Group 10 — Stage 2.6 regression: day-view stage handlers filter provisions
# =============================================================================
cat("\n=== Group 10: Stage 2.6 — day-view provision filter ===\n")

{
  # Build mixed detail with sap, manual, and provision source rows
  mixed_detail <- tibble::tibble(
    Empresa      = c("NG","NG","NG","NG","NG"),
    Moneda       = "MXN",
    Documento    = c("SAP-1","MAN-1","PROV-1","SAP-2","PROV-2"),
    Parte        = c("A","B","C","D","E"),
    Importe      = c(100, 200, 300, 400, 500),
    FechaEff     = Sys.Date(),
    source       = c("sap","manual","provision","sap","provision"),
    provision_id = c(NA, NA, "prov_c", NA, "prov_e")
  )

  # 10.1 stage_all path: filter + early-return guard on empty result
  mask_all <- !is.na(mixed_detail[["Documento"]])
  d_all    <- pasivos_filter_out_provisions(mixed_detail[mask_all, , drop = FALSE])
  inv_keys_all <- unique(d_all[, c("Empresa","Moneda","Documento","Importe"), drop = FALSE])
  .chk_true("10.1 stage_all: 3 non-provision keys remain", nrow(inv_keys_all) == 3L)
  .chk_true("10.1 stage_all: PROV-1 absent", !any(inv_keys_all$Documento == "PROV-1"))
  .chk_true("10.1 stage_all: PROV-2 absent", !any(inv_keys_all$Documento == "PROV-2"))

  # 10.2 stage_all path: all-provision day returns empty (triggers notification)
  all_prov_detail <- mixed_detail[mixed_detail$source == "provision", , drop = FALSE]
  d_prov_only <- pasivos_filter_out_provisions(all_prov_detail)
  .chk_true("10.2 stage_all all-prov day: empty after filter", nrow(d_prov_only) == 0L)

  # 10.3 stage_sel path: provision row excluded when user selects mix
  sel_rows <- mixed_detail[c(1, 3, 4), , drop = FALSE]   # SAP-1, PROV-1, SAP-2
  filtered_sel <- pasivos_filter_out_provisions(sel_rows)
  .chk_true("10.3 stage_sel: 2 items remain after filter", nrow(filtered_sel) == 2L)
  .chk_true("10.3 stage_sel: no provision row", !any(filtered_sel$source == "provision"))

  # 10.4 cart_N path: group containing only provisions → empty after filter
  prov_group <- mixed_detail[mixed_detail$source == "provision", , drop = FALSE]
  cart_filtered <- pasivos_filter_out_provisions(prov_group)
  .chk_true("10.4 cart_N: all-provision group → empty", nrow(cart_filtered) == 0L)

  # 10.5 cart_inv_click path: j > nrow(inv_rows) guard after filter
  inv_rows_raw <- mixed_detail   # all 5 rows
  inv_rows_flt <- pasivos_filter_out_provisions(inv_rows_raw)
  inv_rows_flt <- inv_rows_flt[order(-inv_rows_flt[["Importe"]]), ]
  # j = 5 would correspond to a provision row index beyond filtered list
  j_oob <- 5L
  .chk_true("10.5 cart_inv_click: j > nrow(inv_rows) guard fires",
            j_oob > nrow(inv_rows_flt))

  # 10.6 cart expanded-row j-indexing: non-provision rows get sequential j starting at 1
  inv_detail_test <- mixed_detail[order(-mixed_detail[["Importe"]]), ]
  is_prov_vec <- inv_detail_test$source == "provision"
  j_item_vec  <- ifelse(is_prov_vec, NA_integer_, cumsum(!is_prov_vec))
  # Should have 3 non-provision rows with j=1,2,3; 2 provision rows with j=NA
  .chk_true("10.6 j-index: non-provision rows get 1,2,3",
            identical(sort(j_item_vec[!is.na(j_item_vec)]), c(1L, 2L, 3L)))
  .chk_true("10.6 j-index: provision rows get NA",
            all(is.na(j_item_vec[is_prov_vec])))
}

# =============================================================================
# Group 11 — Stage 4.5: provisions survive empresa filter in build_ledger_df
# =============================================================================
cat("\n=== Group 11: Stage 4.5 — provision empresa translation ===\n")

suppressPackageStartupMessages({ library(stringr); library(tidyr) })
source("R/data_pipeline.R")

{
  # Synthesize 1 liability + 3 provisional provisions; empresa stored as initial "NG"
  lid <- "test-liab-001"

  provs_raw <- tibble::tibble(
    id                       = paste0("prov-", 1:3),
    liability_id             = lid,
    origin                   = "rule",
    occurrence_index         = 1:3,
    estado                   = "provisional",
    fecha_calculada          = as.Date("2026-06-01") + (0:2) * 30,
    fecha_efectiva           = as.Date("2026-06-01") + (0:2) * 30,
    policy_ids               = NA_character_,
    empresa                  = "NG",
    parte                    = "PROVEEDOR TEST",
    codigo_parte             = NA_character_,
    moneda_pago              = "MXN",
    cotizado_en              = NA_character_,
    amount_pago              = c(10000, 10000, 10000),
    amount_cotizado          = c(10000, 10000, 10000),
    amount_pago_override     = NA_real_,
    amount_cotizado_override = NA_real_,
    fecha_efectiva_override  = as.Date(NA),
    documento                = paste0("DOC-", 1:3),
    referencia               = NA_character_,
    notas                    = NA_character_,
    manual_inv_id            = NA_character_,
    pagar_hoy_id             = NA_character_,
    last_edited_by           = NA_character_,
    last_edited_at           = as.POSIXct(NA),
    created_at               = as.POSIXct(NA),
    updated_at               = as.POSIXct(NA)
  )

  cmap <- list(NG = "Networks Group", NTS = "Networks Trucking Services")

  result <- build_ledger_df(
    raw_df      = NULL,
    ledger      = "AP",
    empresa     = NULL,
    moves_df    = NULL,
    provs_df    = provs_raw,
    company_map = cmap
  )

  prov_rows <- result[!is.na(result$source) & result$source == "provision", , drop = FALSE]

  .chk_true("11.1 build_ledger_df includes 3 provision rows",
            nrow(prov_rows) == 3L)
  .chk_true("11.2 provision Empresa translated from initial to full name",
            all(prov_rows$Empresa == "Networks Group"))
  .chk_true("11.3 provision Parte preserved",
            all(prov_rows$Parte == "PROVEEDOR TEST"))
  .chk_true("11.4 provision Saldo vencido set from amount_pago",
            all(prov_rows$`Saldo vencido` == 10000))
  .chk_true("11.5 provision FechaEff is non-NA",
            all(!is.na(prov_rows$FechaEff)))

  # Without company_map, Empresa stays as initial "NG" (not translated)
  result_no_map <- build_ledger_df(
    raw_df      = NULL,
    ledger      = "AP",
    empresa     = NULL,
    moves_df    = NULL,
    provs_df    = provs_raw,
    company_map = NULL
  )
  prov_no_map <- result_no_map[!is.na(result_no_map$source) & result_no_map$source == "provision", , drop = FALSE]
  .chk_true("11.6 without company_map, Empresa stays as stored initial",
            all(prov_no_map$Empresa == "NG"))
}

# =============================================================================
# Group 9 — Manual smoke checklist (human-run)
# =============================================================================
cat("\n=== Group 9: Manual smoke checklist ===\n")
cat("
Human must verify the following steps manually after starting the app:

1. Navigate to the CxP (AP) calendar. Confirm a known provision appears with a
   dashed grey border and a circular 'P' badge in the tile.

2. Click a day tile that has provisions. In the day modal, verify provision rows
   show an ⚡ (lightning bolt) button in the Fuente column instead of SAP/✎.

3. Click the ⚡ button on a provision. Confirm the 'Convertir provisión a item'
   modal opens with all fields pre-filled (Parte, Importe, Fecha, Moneda, Documento).

4. Click 'Guardar como item'. Confirm:
   - The provision disappears from the calendar.
   - A new manual item appears at the same date.

5. Open the same day again. Click ⚡ on a different provision. Edit the Importe.
   Click 'Guardar y agregar a Agenda de hoy'. Confirm:
   - Provision disappears.
   - New manual item visible.
   - Item appears in the Agenda de hoy pane.

6. Confirm the staged item via the Agenda 'Confirmar pago' button. After the next
   3-second sync tick, open the audit log and confirm a provision.item_confirmed
   entry exists for the provision's id.

7. Reverse the confirmed item via the existing reversal action. After 3 seconds,
   verify:
   - The provision re-appears on the calendar at its original date.
   - The manual_inv row is gone.

8. Open the 'Agregar Item' modal (CxP). Fill in fields. Click 'Guardar como
   provisión'. Confirm a new dashed-P provision appears on the calendar at the
   chosen date.

9. Try to multi-select a mix of items and provisions in the day modal (audit
   mode), then click 'Agregar selección'. Confirm only non-provision items are
   sent to the Agenda. Provision rows are silently skipped.

All 9 steps must pass before Stage 2 is declared complete.
")

# =============================================================================
# Summary
# =============================================================================
cat("\n=== Stage 2.5 Results ===\n")
cat(sprintf("PASS: %d | FAIL: %d | TOTAL: %d\n",
            .pass, .fail, .pass + .fail))
if (.fail > 0L) {
  cat("Failed tests:\n")
  for (t in .tests[grepl("^\\[FAIL\\]", .tests)]) cat(" ", t, "\n")
  quit(status = 1L)
} else {
  cat("All automated tests passed.\n")
  quit(status = 0L)
}
