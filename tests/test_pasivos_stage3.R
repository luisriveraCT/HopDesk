# =============================================================================
# tests/test_pasivos_stage3.R
# Stage 3 automated tests — Pasivos Table view
#
# Run from project root: Rscript tests/test_pasivos_stage3.R
# =============================================================================

# ── Bootstrap ─────────────────────────────────────────────────────────────────
# Works for both: Rscript tests/test_pasivos_stage3.R  AND  source("tests/...")
.proj_root <- local({
  # 1) Rscript: --file= arg gives us the script path
  args <- commandArgs(trailingOnly = FALSE)
  ff   <- sub("--file=", "", args[grep("--file=", args)])
  if (length(ff) && nchar(ff[1]))
    return(normalizePath(file.path(dirname(ff[1]), ".."), mustWork = FALSE))
  # 2) source(): walk sys.frames looking for ofile set by source()
  for (fr in rev(sys.frames())) {
    ofile <- fr$ofile
    if (!is.null(ofile) && nzchar(ofile))
      return(normalizePath(file.path(dirname(ofile), ".."), mustWork = FALSE))
  }
  # 3) Fallback: assume already at project root
  getwd()
})
setwd(.proj_root)

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(jsonlite)
  library(uuid)
})

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
source("R/pasivos_table_pivot.R")
source("R/pasivos_table_styles.R")

# isTRUE_safe helper (same as in global.R)
isTRUE_safe <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && x

# fmt_money stub for summary_line test
fmt_money <- function(x, ...) formatC(x, format = "f", digits = 0, big.mark = ",")

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

.reset_store <- function() {
  rm(list = ls(.mock_store), envir = .mock_store)
  .mock_store[["pasivos_provisions.rds"]]  <- .schema_pasivos_provision()
  .mock_store[["pasivos_liabilities.rds"]] <- .schema_pasivos_liability()
  .mock_store[["pasivos_estimates.rds"]]   <- .schema_pasivos_estimates()
  .mock_store[["pasivos_audit.rds"]]       <- .schema_pasivos_audit()
}

.make_liability <- function(id = "lib1", empresa = "Networks Group",
                             categoria = "regular", subcategoria = "renta",
                             nombre = "Test Liability", parte = "Prov Test") {
  tibble::tibble(
    id = id, categoria = categoria, subcategoria = subcategoria, flavor = NA_character_,
    nombre = nombre, empresa = empresa,
    parte = parte, codigo_parte = "PROV01",
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
    empresa = empresa, parte = "Prov Test", codigo_parte = "PROV01",
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

.empty_filters <- list(
  empresa = character(0), categorias = c("regular","financiero","tarjeta"),
  subcategorias = character(0), currency = "All", search = "", granularity = "day"
)

.empty_bancos   <- data.frame(provision_id = character(), confirmacion_id = character(),
                               eliminado = logical(), stringsAsFactors = FALSE)
.empty_manual   <- data.frame(id = character(), provision_id = character(),
                               ledger = character(), Importe = numeric(),
                               FechaVenc = as.Date(character()), stringsAsFactors = FALSE)

# =============================================================================
# Group 1 — pasivos_table_build_long (pure)
# =============================================================================
cat("\n=== Group 1: pasivos_table_build_long ===\n")

# 1.1 Empty inputs → empty output, schema preserved
{
  out <- pasivos_table_build_long(.schema_pasivos_provision(), .empty_manual,
                                  .empty_bancos, .schema_pasivos_liability(),
                                  .empty_filters)
  .chk_true("1.1 empty inputs → 0 rows", nrow(out) == 0L)
  .chk_true("1.1 schema: row_id column exists", "row_id" %in% names(out))
  .chk_true("1.1 schema: cell_kind column exists", "cell_kind" %in% names(out))
}

# 1.2 5 provisional provisions → 5 rows
{
  lib   <- .make_liability("lib2")
  provs <- dplyr::bind_rows(
    .make_provision("p2a", liability_id = "lib2", fecha = Sys.Date() + 1),
    .make_provision("p2b", liability_id = "lib2", fecha = Sys.Date() + 2),
    .make_provision("p2c", liability_id = "lib2", fecha = Sys.Date() + 3),
    .make_provision("p2d", liability_id = "lib2", fecha = Sys.Date() - 1),
    .make_provision("p2e", liability_id = "lib2", fecha = Sys.Date() - 2)
  )
  out <- pasivos_table_build_long(provs, .empty_manual, .empty_bancos, lib, .empty_filters)
  .chk_true("1.2 5 provisions → 5 rows", nrow(out) == 5L)
  future_rows <- out[out$cell_kind == "provision", ]
  past_rows   <- out[out$cell_kind == "overdue_provision", ]
  .chk_true("1.2 3 future → cell_kind = provision", nrow(future_rows) == 3L)
  .chk_true("1.2 2 past → cell_kind = overdue_provision", nrow(past_rows) == 2L)
}

# 1.3 Converted provision surfaced via manual_item (manual_item kind)
{
  lib <- .make_liability("lib3")
  prov_conv <- .make_provision("p3a", liability_id = "lib3", estado = "converted",
                                fecha = Sys.Date() + 5)
  prov_prov <- .make_provision("p3b", liability_id = "lib3", fecha = Sys.Date() + 10)
  provs     <- dplyr::bind_rows(prov_conv, prov_prov)

  mi <- tibble::tibble(
    id           = "mi3a",
    provision_id = "p3a",
    ledger       = "AP",
    Importe      = 1000,
    FechaVenc    = Sys.Date() + 5
  )

  out <- pasivos_table_build_long(provs, mi, .empty_bancos, lib, .empty_filters)
  .chk_true("1.3 converted provision → 1 manual_item row", sum(out$cell_kind == "manual_item") == 1L)
  .chk_true("1.3 provisional provision still shows", sum(out$cell_kind == "provision") == 1L)
  .chk_true("1.3 total 2 rows", nrow(out) == 2L)
}

# 1.4 confirmed_item when bancos_confirmados match exists
{
  lib <- .make_liability("lib4")
  prov <- .make_provision("p4a", liability_id = "lib4", estado = "item_confirmed",
                           fecha = Sys.Date() + 3)
  mi <- tibble::tibble(
    id = "mi4a", provision_id = "p4a", ledger = "AP",
    Importe = 2000, FechaVenc = Sys.Date() + 3
  )
  bc <- tibble::tibble(
    confirmacion_id = "bc4a", provision_id = "p4a", eliminado = FALSE
  )
  out <- pasivos_table_build_long(prov, mi, bc, lib, .empty_filters)
  .chk_true("1.4 item_confirmed + bc → confirmed_item kind", sum(out$cell_kind == "confirmed_item") == 1L)
}

# 1.5 overdue: has_overdue row-level flag
{
  lib <- .make_liability("lib5")
  provs <- dplyr::bind_rows(
    .make_provision("p5a", liability_id = "lib5", fecha = Sys.Date() - 5),  # overdue
    .make_provision("p5b", liability_id = "lib5", fecha = Sys.Date() + 5)   # future
  )
  out <- pasivos_table_build_long(provs, .empty_manual, .empty_bancos, lib, .empty_filters)
  .chk_true("1.5 overdue provision present", any(out$cell_kind == "overdue_provision"))
  .chk_true("1.5 future provision present", any(out$cell_kind == "provision"))
}

# 1.6 Filter by empresa
{
  lib_ng  <- .make_liability("lib6a", empresa = "Networks Group")
  lib_nts <- .make_liability("lib6b", empresa = "NTS")
  liabs   <- dplyr::bind_rows(lib_ng, lib_nts)
  provs   <- dplyr::bind_rows(
    .make_provision("p6a", liability_id = "lib6a", empresa = "Networks Group"),
    .make_provision("p6b", liability_id = "lib6b", empresa = "NTS")
  )
  flt <- modifyList(.empty_filters, list(empresa = "Networks Group"))
  out <- pasivos_table_build_long(provs, .empty_manual, .empty_bancos, liabs, flt)
  .chk_true("1.6 filter empresa: only NG rows", all(out$row_empresa == "Networks Group"))
  .chk_true("1.6 filter empresa: 1 row", nrow(out) == 1L)
}

# 1.7 Filter by categoria
{
  lib_reg <- .make_liability("lib7a", categoria = "regular")
  lib_fin <- .make_liability("lib7b", categoria = "financiero")
  liabs   <- dplyr::bind_rows(lib_reg, lib_fin)
  provs   <- dplyr::bind_rows(
    .make_provision("p7a", liability_id = "lib7a"),
    .make_provision("p7b", liability_id = "lib7b")
  )
  flt <- modifyList(.empty_filters, list(categorias = "financiero"))
  out <- pasivos_table_build_long(provs, .empty_manual, .empty_bancos, liabs, flt)
  .chk_true("1.7 filter categoria: only financiero", all(out$row_categoria == "financiero"))
}

# 1.8 Filter by currency
{
  lib <- .make_liability("lib8")
  provs <- dplyr::bind_rows(
    .make_provision("p8a", liability_id = "lib8", moneda_pago = "MXN"),
    .make_provision("p8b", liability_id = "lib8", moneda_pago = "USD")
  )
  lib_usd <- lib; lib_usd$moneda_pago <- "USD"
  all_liabs <- dplyr::bind_rows(lib, lib_usd)
  flt <- modifyList(.empty_filters, list(currency = "USD"))
  out <- pasivos_table_build_long(provs, .empty_manual, .empty_bancos, lib, flt)
  .chk_true("1.8 filter currency USD: only USD rows", all(out$cell_currency == "USD" | nrow(out) == 0L))
}

# 1.9 Filter by search (name match)
{
  lib_rent <- .make_liability("lib9a", nombre = "Renta Oficina")
  lib_int  <- .make_liability("lib9b", nombre = "Internet")
  liabs    <- dplyr::bind_rows(lib_rent, lib_int)
  provs    <- dplyr::bind_rows(
    .make_provision("p9a", liability_id = "lib9a"),
    .make_provision("p9b", liability_id = "lib9b")
  )
  flt <- modifyList(.empty_filters, list(search = "renta"))
  out <- pasivos_table_build_long(provs, .empty_manual, .empty_bancos, liabs, flt)
  .chk_true("1.9 search 'renta' → 1 row", nrow(out) == 1L)
  .chk_true("1.9 search row is Renta Oficina", out$row_label[1] == "Renta Oficina")
}

# 1.10 Orphan provision (no liability_id)
{
  prov_orphan <- .make_provision("porp1", liability_id = NA_character_,
                                  empresa = "Networks Group")
  flt <- modifyList(.empty_filters, list(empresa = "Networks Group"))
  out <- pasivos_table_build_long(prov_orphan, .empty_manual, .empty_bancos,
                                  .schema_pasivos_liability(), flt)
  .chk_true("1.10 orphan provision included", nrow(out) == 1L)
  .chk_true("1.10 orphan row_id starts with orphan_", grepl("^orphan_", out$row_id[1]))
}

# 1.11 Non-Pasivos manual_inv (provision_id NA) does NOT appear
{
  lib <- .make_liability("lib11")
  prov <- .make_provision("p11a", liability_id = "lib11")
  mi_regular <- tibble::tibble(
    id = "mi_reg", provision_id = NA_character_,
    ledger = "AP", Importe = 500, FechaVenc = Sys.Date() + 1
  )
  out <- pasivos_table_build_long(prov, mi_regular, .empty_bancos, lib, .empty_filters)
  # Only the provision row should appear; no row from mi_regular
  .chk_true("1.11 non-Pasivos manual_inv not in output",
            !any(out$cell_manual_id == "mi_reg", na.rm = TRUE))
}

# 1.12 Closed provisions do NOT appear in output
{
  lib <- .make_liability("lib12")
  prov_open   <- .make_provision("p12a", liability_id = "lib12", fecha = Sys.Date() + 5)
  prov_closed <- .make_provision("p12b", liability_id = "lib12", fecha = Sys.Date() + 10,
                                  estado = "closed")
  provs <- dplyr::bind_rows(prov_open, prov_closed)
  out <- pasivos_table_build_long(provs, .empty_manual, .empty_bancos, lib, .empty_filters)
  .chk_true("1.12 closed provision excluded from output",
            !any(out$cell_provision_id == "p12b", na.rm = TRUE))
  .chk_true("1.12 open provision still present", any(out$cell_provision_id == "p12a", na.rm = TRUE))
  .chk_true("1.12 only 1 row total", nrow(out) == 1L)
}

# 1.13 Unknown estado fires warning and is excluded
{
  lib <- .make_liability("lib13")
  prov_known   <- .make_provision("p13a", liability_id = "lib13", fecha = Sys.Date() + 3)
  prov_unknown <- .make_provision("p13b", liability_id = "lib13", fecha = Sys.Date() + 7,
                                   estado = "purgatory")
  provs <- dplyr::bind_rows(prov_known, prov_unknown)
  warn_fired <- FALSE
  withCallingHandlers(
    {
      out <- pasivos_table_build_long(provs, .empty_manual, .empty_bancos, lib, .empty_filters)
    },
    warning = function(w) {
      if (grepl("unknown estado", conditionMessage(w))) warn_fired <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  .chk_true("1.13 unknown estado fires warning", warn_fired)
  .chk_true("1.13 unknown estado row excluded",
            !any(out$cell_provision_id == "p13b", na.rm = TRUE))
  .chk_true("1.13 known estado row present",
            any(out$cell_provision_id == "p13a", na.rm = TRUE))
}

# =============================================================================
# Group 2 — pasivos_table_pivot_wide (pure)
# =============================================================================
cat("\n=== Group 2: pasivos_table_pivot_wide ===\n")

.make_long_row <- function(row_id = "lib1", fecha = Sys.Date(),
                            cell_kind = "provision", amount = 1000) {
  tibble::tibble(
    row_id = row_id, row_label = "Test", row_empresa = "NG",
    row_categoria = "regular", row_subcategoria = "renta",
    row_parte = "Prov", row_moneda = "MXN",
    fecha = as.Date(fecha),
    cell_kind = cell_kind, cell_amount = amount, cell_currency = "MXN",
    cell_provision_id = "prov1", cell_manual_id = NA_character_,
    cell_bancos_conf_id = NA_character_, cell_has_override = FALSE,
    cell_overdue_severity = "none"
  )
}

win_start <- Sys.Date() - 3L
win_end   <- Sys.Date() + 6L

# 2.1 Daily granularity → one column per day
{
  out <- pasivos_table_pivot_wide(.make_long_row(), "day", win_start, win_end)
  expected_cols <- as.character(seq(win_start, win_end, by = "day"))
  .chk_true("2.1 daily: correct number of columns", length(out$date_cols) == length(expected_cols))
  .chk_true("2.1 daily: column labels are ISO dates",
            all(grepl("^\\d{4}-\\d{2}-\\d{2}$", out$date_cols)))
}

# 2.2 Weekly granularity → one column per ISO week
{
  rows <- dplyr::bind_rows(
    .make_long_row(fecha = win_start, amount = 100),
    .make_long_row(fecha = win_start + 1L, amount = 200)
  )
  out <- pasivos_table_pivot_wide(rows, "week", win_start, win_end)
  .chk_true("2.2 weekly: column labels match YYYY-Www pattern",
            all(grepl("^\\d{4}-W\\d{2}$", out$date_cols)))
  # Amounts on the same week should be summed
  week_lbl <- format(win_start, "%G-W%V")
  cell_val <- out$cells[["lib1"]][[week_lbl]]
  .chk_true("2.2 weekly: amounts summed within week",
            !is.null(cell_val) && cell_val$cell_amount >= 300)
}

# 2.3 Monthly granularity → one column per YYYY-MM
{
  out <- pasivos_table_pivot_wide(.make_long_row(), "month", win_start, win_end)
  .chk_true("2.3 monthly: column labels match YYYY-MM",
            all(grepl("^\\d{4}-\\d{2}$", out$date_cols)))
}

# 2.4 Yearly granularity → one column per YYYY
{
  out <- pasivos_table_pivot_wide(.make_long_row(), "year", win_start, win_end)
  .chk_true("2.4 yearly: column labels match YYYY",
            all(grepl("^\\d{4}$", out$date_cols)))
}

# 2.5 Empty long_df → metadata empty, date_cols span window, cells empty
{
  empty_long <- .make_long_row()[integer(0), ]
  out <- pasivos_table_pivot_wide(empty_long, "day", win_start, win_end)
  .chk_true("2.5 empty long_df → metadata_cols empty", nrow(out$metadata_cols) == 0L)
  .chk_true("2.5 empty long_df → date_cols still spans window",
            length(out$date_cols) == as.integer(win_end - win_start + 1L))
  .chk_true("2.5 empty long_df → cells empty list", length(out$cells) == 0L)
}

# 2.6 Window outside long_df range → empty cells but column headers render
{
  row_outside <- .make_long_row(fecha = Sys.Date() + 1000)
  out <- pasivos_table_pivot_wide(row_outside, "day", win_start, win_end)
  .chk_true("2.6 out-of-window row: date_cols still spans window",
            length(out$date_cols) == as.integer(win_end - win_start + 1L))
  .chk_true("2.6 out-of-window row: cells empty (row not in window)",
            length(out$cells) == 0L || all(sapply(out$cells, length) == 0L))
}

# =============================================================================
# Group 3 — pasivos_cell_classes
# =============================================================================
cat("\n=== Group 3: pasivos_cell_classes ===\n")

today_ref <- Sys.Date()

.make_cell <- function(kind, fecha = today_ref, override = FALSE) {
  list(cell_kind = kind, fecha = fecha, cell_has_override = override,
       cell_overdue_severity = "none")
}

# 3.1 provision (future)
{
  cls <- pasivos_cell_classes(.make_cell("provision", today_ref + 1), today_ref)
  .chk_true("3.1 provision future: has pasivos-cell",       "pasivos-cell" %in% cls)
  .chk_true("3.1 provision future: has pasivos-cell-provision", "pasivos-cell-provision" %in% cls)
  .chk_true("3.1 provision future: no due-today",          !"pasivos-cell-due-today" %in% cls)
}

# 3.2 provision (due today)
{
  cls <- pasivos_cell_classes(.make_cell("provision", today_ref), today_ref)
  .chk_true("3.2 provision today: has due-today", "pasivos-cell-due-today" %in% cls)
}

# 3.3 overdue_provision
{
  cls <- pasivos_cell_classes(.make_cell("overdue_provision", today_ref - 1), today_ref)
  .chk_true("3.3 overdue_provision: has overdue-amber", "pasivos-cell-overdue-amber" %in% cls)
  .chk_true("3.3 overdue_provision: has provision base", "pasivos-cell-provision" %in% cls)
}

# 3.4 manual_item (future)
{
  cls <- pasivos_cell_classes(.make_cell("manual_item", today_ref + 2), today_ref)
  .chk_true("3.4 manual_item future: has item-pending", "pasivos-cell-item-pending" %in% cls)
  .chk_true("3.4 manual_item future: no due-today",     !"pasivos-cell-due-today" %in% cls)
}

# 3.5 manual_item (due today)
{
  cls <- pasivos_cell_classes(.make_cell("manual_item", today_ref), today_ref)
  .chk_true("3.5 manual_item today: has due-today", "pasivos-cell-due-today" %in% cls)
}

# 3.6 overdue_manual
{
  cls <- pasivos_cell_classes(.make_cell("overdue_manual", today_ref - 3), today_ref)
  .chk_true("3.6 overdue_manual: has overdue-red", "pasivos-cell-overdue-red" %in% cls)
}

# 3.7 confirmed_item
{
  cls <- pasivos_cell_classes(.make_cell("confirmed_item", today_ref - 1), today_ref)
  .chk_true("3.7 confirmed_item: has item-confirmed", "pasivos-cell-item-confirmed" %in% cls)
}

# 3.8 override flag adds override class regardless of kind
{
  cls <- pasivos_cell_classes(.make_cell("provision", today_ref + 1, override = TRUE), today_ref)
  .chk_true("3.8 override=TRUE: has has-override class", "pasivos-cell-has-override" %in% cls)
  cls2 <- pasivos_cell_classes(.make_cell("confirmed_item", today_ref, override = TRUE), today_ref)
  .chk_true("3.8 override on confirmed_item: has has-override class", "pasivos-cell-has-override" %in% cls2)
}

# 3.9 override=FALSE → no override class
{
  cls <- pasivos_cell_classes(.make_cell("provision", today_ref + 1, override = FALSE), today_ref)
  .chk_true("3.9 override=FALSE: no has-override class", !"pasivos-cell-has-override" %in% cls)
}

# =============================================================================
# Group 4 — Manual smoke checklist (human-run)
# =============================================================================
cat("\n=== Group 4: Manual smoke checklist ===\n")
cat("
Human must verify the following steps manually after starting the app:

1. Open the new Pasivos tab. Verify the table renders with today's column highlighted
   in a light blue background.

2. Apply a filter by Empresa. Confirm rows narrow to matching liabilities only.

3. Toggle granularity to weekly (Semana). Confirm columns collapse to ISO weeks
   and cell amounts sum within the week.

4. Find a known overdue provision (past fecha_efectiva, still provisional).
   Confirm its row has a red left-border glow and the cell has amber background.

5. Click a lightning bolt ⚡ in a provision cell. Confirm the convert modal opens
   with all fields pre-filled (same modal as in the calendar/day-view).

6. Convert the provision. Confirm the cell flips from purple italic (provision)
   to amber (manual_item pending) styling within one reactive cycle.

7. Confirm the converted item via Agenda de hoy. Confirm the cell flips to
   green (confirmed_item) styling.

8. Click '60 días →'. Confirm new future columns load to the right.

9. Click '← 60 días'. Confirm past columns load to the left.

10. Search for a liability by name. Confirm the table filters to matching rows only.

11. Verify the summary line updates after each filter change and after conversion.

All 11 steps must pass before Stage 3 is declared complete.
")

# =============================================================================
# Summary
# =============================================================================
cat("\n=== Stage 3 Results ===\n")
cat(sprintf("PASS: %d | FAIL: %d | TOTAL: %d\n", .pass, .fail, .pass + .fail))
if (.fail > 0L) {
  cat("Failed tests:\n")
  for (t in .tests[grepl("^\\[FAIL\\]", .tests)]) cat(" ", t, "\n")
  quit(status = 1L)
} else {
  cat("All automated tests passed.\n")
  quit(status = 0L)
}
