# =============================================================================
# R/bancos_persistence.R
# S3 load/save for the three Bancos tables.
# Follows the EXACT same pattern as persistence.R:
#   load_*() → returns normalized tibble (never NULL, never errors to caller)
#   save_*() → writes full dataframe to S3, returns invisible(TRUE)
#
# IMPORTANT: save_bancos_movimientos() ALWAYS saves the FULL dataframe
# (including eliminado=TRUE rows) for permanent audit trail.
# load_bancos_movimientos() filters eliminado=FALSE by default.
# =============================================================================

# ── Schemas ───────────────────────────────────────────────────────────────────

.schema_bancos_cuentas <- function() tibble::tibble(
  cuenta_id         = character(),   # UUID
  empresa           = character(),   # initials: NCS, NTS, NG, NL, NRS
  banco             = character(),   # "BanBajío", "BBVA", "HSBC", etc.
  moneda            = character(),   # "MXN" | "USD"
  numero_cuenta     = character(),   # account number (e.g. "7354301")
  clabe             = character(),   # 18-digit CLABE
  alias             = character(),   # short display name: "NCS MXP"
  saldo_inicial     = numeric(),     # opening balance for running balance calc
  fecha_inicio      = as.Date(character()),
  tarjetas_retenido = numeric(),     # retained by corporate credit cards
  activa            = logical()
)

.schema_bancos_movimientos <- function() tibble::tibble(
  id              = character(),   # UUID — assigned on save
  cuenta_id       = character(),   # FK to bancos_cuentas; NA for "sin cuenta"
  fecha           = as.Date(character()),
  hora            = character(),   # "HH:MM:SS"
  recibo          = character(),   # dedup key part 1
  descripcion_raw = character(),
  tipo            = character(),   # spei_out|spei_in|comision|traspaso|nomina|
                                   # pos|domiciliacion|cambio|plazo|otro|manual_in
  parte           = character(),
  rfc             = character(),
  referencia      = character(),
  clave_rastreo   = character(),
  concepto        = character(),
  cargo           = numeric(),     # debit; 0 (not NA) when it's a credit
  abono           = numeric(),     # credit; 0 (not NA) when it's a debit
  saldo_banco     = numeric(),     # from TXT; NA for manual/agenda sources
  conciliado      = logical(),
  doc_vinculado   = character(),   # SAP document number
  agenda_id       = character(),   # FK to pagar_hoy item
  importado_at    = as.POSIXct(character()),
  fuente          = character(),   # "txt" | "manual" | "agenda"
  eliminado       = logical(),     # TRUE = soft-deleted; never physically removed
  notas           = character()
)

.schema_bancos_confirmados <- function() tibble::tibble(
  confirmacion_id = character(),   # UUID
  agenda_item_id  = character(),   # FK to pagar_hoy$id
  empresa         = character(),
  parte           = character(),
  documento       = character(),   # SAP document number
  codigo          = character(),   # SAP CardCode (for round-trip to calendar)
  importe         = numeric(),
  moneda          = character(),
  cuenta_id       = character(),   # which bank account was used
  fecha           = as.Date(character()),
  tipo            = character(),   # "pago" | "cobro"
  mov_id          = character(),   # FK to bancos_movimientos$id
  confirmado_at   = as.POSIXct(character()),
  eliminado       = logical()
)

# ── Shared normalizer (reuses persistence.R's .normalize) ────────────────────
# .normalize() is already defined in persistence.R which is sourced first.

# ── bancos_cuentas ────────────────────────────────────────────────────────────

load_bancos_cuentas <- function() {
  raw <- .s3_read(S3_KEYS$bancos_cuentas)
  df  <- .normalize(raw, .schema_bancos_cuentas)
  # Default NAs
  df$saldo_inicial     <- ifelse(is.na(df$saldo_inicial),     0, df$saldo_inicial)
  df$tarjetas_retenido <- ifelse(is.na(df$tarjetas_retenido), 0, df$tarjetas_retenido)
  df$activa            <- ifelse(is.na(df$activa), TRUE, df$activa)
  df
}

save_bancos_cuentas <- function(df) {
  .s3_write(.normalize(df, .schema_bancos_cuentas), S3_KEYS$bancos_cuentas)
}

# ── bancos_movimientos ────────────────────────────────────────────────────────

#' @param include_deleted If FALSE (default), filter out rows where eliminado=TRUE.
load_bancos_movimientos <- function(include_deleted = FALSE) {
  raw <- .s3_read(S3_KEYS$bancos_movimientos)
  df  <- .normalize(raw, .schema_bancos_movimientos)
  # Ensure numeric cols never have NA (spec: "NA not permitted — use 0")
  df$cargo  <- ifelse(is.na(df$cargo),  0, df$cargo)
  df$abono  <- ifelse(is.na(df$abono),  0, df$abono)
  df$conciliado <- ifelse(is.na(df$conciliado), FALSE, df$conciliado)
  df$eliminado  <- ifelse(is.na(df$eliminado),  FALSE, df$eliminado)
  if (!include_deleted) df <- dplyr::filter(df, !eliminado)
  df
}

#' Always saves the COMPLETE dataframe (eliminado rows included) for audit trail.
save_bancos_movimientos <- function(df) {
  .s3_write(.normalize(df, .schema_bancos_movimientos), S3_KEYS$bancos_movimientos)
}

# ── bancos_confirmados ────────────────────────────────────────────────────────

load_bancos_confirmados <- function() {
  raw <- .s3_read(S3_KEYS$bancos_confirmados)
  df  <- .normalize(raw, .schema_bancos_confirmados)
  df$eliminado <- ifelse(is.na(df$eliminado), FALSE, df$eliminado)
  df
}

save_bancos_confirmados <- function(df) {
  .s3_write(.normalize(df, .schema_bancos_confirmados), S3_KEYS$bancos_confirmados)
}

# ── Helper: add movement + confirmation row atomically ───────────────────────
# Called by pagarHoyServer when confirming AP/AR payments.
# Returns list(movimientos = updated_df, confirmados = updated_df)
bancos_confirmar_pago <- function(
  movimientos_db,
  confirmados_db,
  agenda_item_id,
  empresa,
  parte,
  documento,
  importe,
  moneda,
  cuenta_id,
  fecha,
  tipo,           # "pago" (AP → spei_out) or "cobro" (AR → spei_in)
  ledger          # "AP" | "AR"
) {
  mov_id <- uuid::UUIDgenerate()

  # Determine cargo/abono and tipo de movimiento
  if (tipo == "pago") {
    mov_tipo <- "spei_out"
    cargo    <- importe
    abono    <- 0
  } else {
    mov_tipo <- "spei_in"
    cargo    <- 0
    abono    <- importe
  }

  new_mov <- tibble::tibble(
    id              = mov_id,
    cuenta_id       = as.character(cuenta_id),
    fecha           = as.Date(fecha),
    hora            = format(Sys.time(), "%H:%M:%S"),
    recibo          = NA_character_,
    descripcion_raw = paste0("Agenda: ", parte, " — ", documento),
    tipo            = mov_tipo,
    parte           = as.character(parte),
    rfc             = NA_character_,
    referencia      = NA_character_,
    clave_rastreo   = NA_character_,
    concepto        = as.character(documento),
    cargo           = cargo,
    abono           = abono,
    saldo_banco     = NA_real_,
    conciliado      = TRUE,
    doc_vinculado   = as.character(documento),
    agenda_id       = as.character(agenda_item_id),
    importado_at    = Sys.time(),
    fuente          = "agenda",
    eliminado       = FALSE,
    notas           = NA_character_
  )

  new_conf <- tibble::tibble(
    confirmacion_id = uuid::UUIDgenerate(),
    agenda_item_id  = as.character(agenda_item_id),
    empresa         = as.character(empresa),
    parte           = as.character(parte),
    documento       = as.character(documento),
    importe         = importe,
    moneda          = as.character(moneda),
    cuenta_id       = as.character(cuenta_id),
    fecha           = as.Date(fecha),
    tipo            = tipo,
    mov_id          = mov_id,
    confirmado_at   = Sys.time(),
    eliminado       = FALSE
  )

  list(
    movimientos = dplyr::bind_rows(movimientos_db, new_mov),
    confirmados = dplyr::bind_rows(confirmados_db, new_conf)
  )
}
