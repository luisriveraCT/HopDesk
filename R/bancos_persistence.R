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
  eliminado_por   = character(),   # username who sent to papelera (audit)
  eliminado_at    = as.POSIXct(character()),  # timestamp of soft-delete
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
  eliminado       = logical(),
  eliminado_at    = as.POSIXct(character()),  # timestamp of soft-delete
  provision_id    = character(),   # mirrored from pagar_hoy at confirmation; NA otherwise
  # ── Ledger-integrity master plan, Stage 3: permanent event log ────────────
  # Additive only, same convention as .schema_papelera(). `eliminado`/
  # `eliminado_at` keep their existing meaning and existing readers (the
  # Historial de confirmaciones filter, `confirmados()`) unchanged — undoing
  # a confirmation still flips them on the ORIGINAL row, which is still
  # never physically removed. These two new fields exist purely to record
  # the recovery as its OWN permanent, separate row (recover_confirmacion()),
  # never mutating or removing anything already written. Pre-Stage-3 rows
  # normalize to action=NA/reverses_confirmacion_id=NA — treat NA action the
  # same as "confirmed" (every row ever written before this stage was one).
  action                    = character(),   # "confirmed" | "recovered"
  reverses_confirmacion_id  = character(),   # NA unless action=="recovered"
  recovered_at              = as.POSIXct(character()),  # NA unless action=="recovered"
  # ── Stage 4: link to the papelera event archiving the real manual_inv row
  # at confirmation time. NA for ERP-sourced confirmations (nothing archived
  # -- the ERP snapshot is never touched) and for provision-derived ones
  # (deferred to Stage 5). Lets undo_conf() find and restore the exact right
  # archived row instead of reconstructing an approximation.
  archive_event_id          = character()
)

# ── Shared normalizer (reuses persistence.R's .normalize) ────────────────────
# .normalize() is already defined in persistence.R which is sourced first.

# ── bancos_cuentas ────────────────────────────────────────────────────────────

load_bancos_cuentas <- function(client_id = NULL) {
  raw <- .s3_read_with(S3_KEYS$bancos_cuentas, client_id = client_id)
  df  <- .normalize(raw, .schema_bancos_cuentas)
  # Default NAs
  df$saldo_inicial     <- ifelse(is.na(df$saldo_inicial),     0, df$saldo_inicial)
  df$tarjetas_retenido <- ifelse(is.na(df$tarjetas_retenido), 0, df$tarjetas_retenido)
  df$activa            <- ifelse(is.na(df$activa), TRUE, df$activa)
  df
}

save_bancos_cuentas <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_bancos_cuentas), S3_KEYS$bancos_cuentas, client_id = client_id)
}

# ── bancos_movimientos ────────────────────────────────────────────────────────

#' @param include_deleted If FALSE (default), filter out rows where eliminado=TRUE.
load_bancos_movimientos <- function(include_deleted = FALSE, client_id = NULL) {
  raw <- .s3_read_with(S3_KEYS$bancos_movimientos, client_id = client_id)
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
save_bancos_movimientos <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_bancos_movimientos), S3_KEYS$bancos_movimientos, client_id = client_id)
}

# ── bancos_confirmados ────────────────────────────────────────────────────────

load_bancos_confirmados <- function(client_id = NULL) {
  raw <- .s3_read_with(S3_KEYS$bancos_confirmados, client_id = client_id)
  df  <- .normalize(raw, .schema_bancos_confirmados)
  df$eliminado <- ifelse(is.na(df$eliminado), FALSE, df$eliminado)
  df
}

save_bancos_confirmados <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_bancos_confirmados), S3_KEYS$bancos_confirmados, client_id = client_id)
}

# Undo a confirmation, recording the recovery as its own permanent row
# rather than only flipping `eliminado` in place. The original row is soft-
# deleted exactly as before (existing readers keep working); a NEW row is
# appended with action="recovered", reverses_confirmacion_id pointing back
# at the original confirmacion_id, so later audits can answer "how many
# times was this recovered" and "was it eventually confirmed again" by
# walking the chain instead of relying on a single mutable flag. Display
# fields (empresa/parte/documento/importe/moneda/tipo/provision_id) are
# copied from the original row for a self-contained recovery record.
recover_confirmacion <- function(conf_db, confirmacion_id, actor = "user") {
  idx <- which(conf_db[["confirmacion_id"]] == confirmacion_id)
  if (!length(idx)) {
    stop("[bancos_confirmados] confirmacion_id not found: ", confirmacion_id)
  }
  if (length(idx) > 1L) {
    stop("[bancos_confirmados] confirmacion_id is not unique (data integrity issue): ", confirmacion_id)
  }
  if (isTRUE(conf_db[["eliminado"]][idx])) {
    stop("[bancos_confirmados] confirmacion_id already recovered: ", confirmacion_id)
  }

  now <- Sys.time()
  conf_db[["eliminado"]][idx]    <- TRUE
  conf_db[["eliminado_at"]][idx] <- now

  original <- conf_db[idx, , drop = FALSE]
  recovery_row <- original
  recovery_row[["confirmacion_id"]]           <- uuid::UUIDgenerate()
  recovery_row[["action"]]                    <- "recovered"
  recovery_row[["reverses_confirmacion_id"]]  <- confirmacion_id
  recovery_row[["recovered_at"]]              <- now
  recovery_row[["eliminado"]]                 <- FALSE
  recovery_row[["eliminado_at"]]               <- as.POSIXct(NA)

  dplyr::bind_rows(conf_db, recovery_row)
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

# ── bancos_papelera ───────────────────────────────────────────────────────────
# found 2026-08-03 (Stage 9, Issue C): Mouse asked for a dedicated, separate
# backend store for deleted bank movements, modeled on the item-level
# manual_inv + papelera.rds lifecycle (add_to_papelera()/restore_from_papelera()
# in R/persistence.R), rather than reusing that same machinery (which is
# scoped to invoice-shaped rows -- Empresa/Documento/Parte/Importe -- and
# shared across manual invoices, SAP ghosts, and provisions; a bank movement
# is a structurally different object: cargo/abono/cuenta_id, no Documento/
# Parte in the invoice sense).
#
# This table is intentionally ADDITIVE, not a replacement for the existing
# eliminado/eliminado_at flag on bancos_movimientos itself. Two things are
# both true and worth being explicit about (this file's own header already
# documents bancos_movimientos rows as "never physically removed" and always
# saved in full "for permanent audit trail" -- that part of the design
# already gave deleted movements a permanent record before this stage):
#   1. bancos_movimientos' own eliminado flag remains the mechanism that
#      hides a deleted row from the live "Libro de Banco" view
#      (load_bancos_movimientos(include_deleted=FALSE)) -- unchanged, still
#      the cheapest and already-correct way to do that.
#   2. This table is the genuinely separate, permanent, replay-able EVENT
#      log Mouse asked for -- action="archived"/"restored" pairs, mirroring
#      the enrichment .schema_papelera() itself gained in the ledger-integrity
#      master plan's Stage 3 (event_id/disposition/action/reverses_event_id),
#      adapted to bank-movement fields.
# Wired additively into do_eliminar_confirm/undo_mov_delete alongside the
# existing flag flip -- NOT yet wired as papelera_tbl's rows_mov display
# source (that still reads live off bancos_movimientos' own eliminado flag,
# unchanged). Whether rows_mov should be switched to read from this table
# instead is an open design question, flagged back rather than decided here
# -- see this stage's final report. bancos_confirmados' own deleted rows
# (rows_conf) and vinculado rows (rows_vin) already use the identical
# flag-on-the-live-table pattern this table's sibling data uses, so
# switching ONLY movimientos' display source would make Bancos' own three
# sibling "deleted record" patterns internally inconsistent with each other,
# not more consistent -- worth Mouse's explicit call, not a silent decision.
.schema_bancos_papelera <- function() tibble::tibble(
  id            = character(),   # permanent, unique id for THIS archive/restore event
  mov_id        = character(),   # original bancos_movimientos$id
  cuenta_id     = character(),
  fecha         = as.Date(character()),
  parte         = character(),
  concepto      = character(),
  cargo         = numeric(),
  abono         = numeric(),
  tipo          = character(),
  fuente        = character(),   # "txt" | "manual" | "agenda" -- copied from the movement at archive time
  disposition   = character(),   # "deleted" (the only disposition today; field kept for parity with .schema_papelera())
  action        = character(),  # "archived" | "restored"
  reverses_id   = character(),  # NA unless action == "restored"; points back at the archived row's own id
  actor         = character(),  # username who archived or restored this event
  event_at      = as.POSIXct(character()),
  original_data = list()        # full pre-delete movement row, for restore + audit
)

load_bancos_papelera <- function(client_id = NULL) {
  .normalize(.s3_read_with(S3_KEYS$bancos_papelera, client_id = client_id), .schema_bancos_papelera)
}

save_bancos_papelera <- function(df, client_id = NULL) {
  .s3_write(.normalize(df, .schema_bancos_papelera), S3_KEYS$bancos_papelera, client_id = client_id)
}

# Archive one or more just-deleted bancos_movimientos rows as their own
# permanent "archived" event. mov_rows is a plain data.frame/tibble slice of
# bancos_movimientos (the rows about to be/just soft-deleted via the
# eliminado flag) -- this does NOT flip that flag itself, callers already do
# that separately; this only writes the permanent archive record alongside it.
add_to_bancos_papelera <- function(papelera_df, mov_rows, actor = "user") {
  if (!nrow(mov_rows)) return(papelera_df)
  new_rows <- tibble::tibble(
    id            = vapply(seq_len(nrow(mov_rows)), function(i) uuid::UUIDgenerate(), character(1)),
    mov_id        = as.character(mov_rows[["id"]]),
    cuenta_id     = as.character(mov_rows[["cuenta_id"]] %||% NA_character_),
    fecha         = as.Date(mov_rows[["fecha"]]),
    parte         = as.character(mov_rows[["parte"]]    %||% ""),
    concepto      = as.character(mov_rows[["concepto"]] %||% ""),
    cargo         = as.numeric(mov_rows[["cargo"]] %||% 0),
    abono         = as.numeric(mov_rows[["abono"]] %||% 0),
    tipo          = as.character(mov_rows[["tipo"]]   %||% ""),
    fuente        = as.character(mov_rows[["fuente"]] %||% ""),
    disposition   = "deleted",
    action        = "archived",
    reverses_id   = NA_character_,
    actor         = actor,
    event_at      = Sys.time(),
    original_data = lapply(seq_len(nrow(mov_rows)), function(i) as.list(mov_rows[i, , drop = FALSE]))
  )
  dplyr::bind_rows(papelera_df, new_rows)
}

# Recover the most recent "archived" event for a given movement id, appending
# a NEW action="restored" row rather than mutating or removing the original
# (same permanent-event-log philosophy as recover_confirmacion() above and
# restore_from_papelera() in R/persistence.R). Returns list(papelera_df =
# updated archive table, original_data = the pre-delete movement row for the
# caller to use when un-flipping bancos_movimientos' own eliminado flag).
# Errors if no un-restored archived event exists for this movement.
restore_from_bancos_papelera <- function(papelera_df, mov_id, actor = "user") {
  candidates <- which(papelera_df[["mov_id"]] == mov_id & papelera_df[["action"]] == "archived")
  if (!length(candidates)) {
    stop("[bancos_papelera] no archived event found for mov_id: ", mov_id)
  }
  restored_ids <- papelera_df[["reverses_id"]][papelera_df[["action"]] == "restored"]
  still_archived <- candidates[!papelera_df[["id"]][candidates] %in% restored_ids]
  if (!length(still_archived)) {
    stop("[bancos_papelera] mov_id's archived event(s) already restored: ", mov_id)
  }
  # Most recent still-archived event for this movement.
  idx <- still_archived[which.max(papelera_df[["event_at"]][still_archived])]

  archived_row <- papelera_df[idx, , drop = FALSE]
  restore_row  <- archived_row
  restore_row[["id"]]          <- uuid::UUIDgenerate()
  restore_row[["action"]]      <- "restored"
  restore_row[["reverses_id"]] <- archived_row[["id"]][1]
  restore_row[["actor"]]       <- actor
  restore_row[["event_at"]]    <- Sys.time()

  list(
    papelera_df   = dplyr::bind_rows(papelera_df, restore_row),
    original_data = archived_row[["original_data"]][[1]]
  )
}
