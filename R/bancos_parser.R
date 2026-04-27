# =============================================================================
# R/bancos_parser.R
# Pure functions for BanBajío TXT parsing. No Shiny reactives, no side effects.
#
# Critical edge cases (verified against real NCS MXP TXT):
#  - SPEI Enviado + Comisión + IVA Comisión all share the SAME Recibo number.
#    Dedup key is recibo + tipo (not recibo alone).
#  - IVA Comisión row has $0.00 cargo in the TXT — the actual IVA amount is
#    only in the description text. Preserve as 0 cargo.
#  - BanBajío uses Spanish month abbreviations (Ene, Feb, Mar, Abr, May, Jun,
#    Jul, Ago, Sep, Oct, Nov, Dic).
# =============================================================================

# ── Month lookup (Spanish abbreviations used by BanBajío) ────────────────────
.BB_MONTHS <- c(
  Ene = 1L, Feb = 2L, Mar = 3L, Abr = 4L, May = 5L, Jun = 6L,
  Jul = 7L, Ago = 8L, Sep = 9L, Oct = 10L, Nov = 11L, Dic = 12L
)

# Parse "09-Mar-2026" → Date. Returns NA if unparseable.
.parse_bb_date <- function(x) {
  x <- trimws(x)
  m <- regexec("([0-9]{2})-([A-Za-z]{3})-([0-9]{4})", x, perl = TRUE)
  parts <- regmatches(x, m)[[1]]
  if (length(parts) < 4) return(NA_real_)
  mon <- .BB_MONTHS[parts[3]]
  if (is.na(mon)) return(NA_real_)
  as.numeric(as.Date(sprintf("%s-%02d-%s", parts[4], mon, parts[2])))
}

# ── Regex helper ─────────────────────────────────────────────────────────────
# Extract capture group `group` from first match of `pattern` in `text`.
# Returns NA_character_ if no match.
.rx <- function(text, pattern, group = 1L) {
  m <- regexec(pattern, text, perl = TRUE)
  parts <- regmatches(text, m)[[1]]
  if (length(parts) <= group) return(NA_character_)
  trimws(parts[[group + 1L]])
}

# ── Amount parser ─────────────────────────────────────────────────────────────
# Handles "1,234,567.89" or "" → numeric. Empty/NA → 0.
.parse_amt <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(0)
  v <- suppressWarnings(as.numeric(gsub(",", "", x, fixed = TRUE)))
  if (is.na(v)) 0 else v
}

# ── Line splitter ─────────────────────────────────────────────────────────────
# BanBajío format: seq,date,time,recibo,description[,,,],cargo,abono,saldo
# Description may contain commas. We know first 4 and last 3 fields are
# comma-safe (no embedded commas), so we split from both ends.
.split_bb_line <- function(line) {
  parts <- strsplit(line, ",", fixed = TRUE)[[1]]
  n <- length(parts)
  if (n < 8L) return(NULL)
  list(
    fecha_str   = parts[2L],
    hora_str    = parts[3L],
    recibo      = trimws(parts[4L]),
    descripcion = paste(parts[5L:(n - 3L)], collapse = ","),
    cargo_str   = parts[n - 2L],
    abono_str   = parts[n - 1L],
    saldo_str   = parts[n]
  )
}

# ── Type classifier ───────────────────────────────────────────────────────────
# Order matters: more-specific patterns first.
.classify_tipo <- function(desc) {
  if (startsWith(desc, "SPEI Enviado:"))                                    return("spei_out")
  if (startsWith(desc, "SPEI Recibido:"))                                   return("spei_in")
  if (grepl("^TEF Recibido",                        desc, ignore.case = TRUE))  return("spei_in")
  if (grepl("^TEF Enviado",                         desc, ignore.case = TRUE))  return("spei_out")
  if (grepl("^(Comisi\u00f3n|IVA Comisi\u00f3n|Gasto admon)",
            desc, perl = TRUE))                                             return("comision")
  if (grepl("Retiro de Recursos|Entrega de Recursos",
            desc, perl = TRUE))                                             return("traspaso")
  if (grepl("Devoluci.n de SPEI",          desc, perl = TRUE))             return("spei_in")
  if (grepl("Retiro de Nomina",            desc, ignore.case = TRUE))      return("nomina")
  if (grepl("Retiro por domiciliaci",      desc, ignore.case = TRUE))      return("domiciliacion")
  if (grepl("Pago de Servicios",           desc, ignore.case = TRUE))      return("otro")
  if (grepl("Compra - Disposicion por POS", desc, ignore.case = TRUE))     return("pos")
  if (grepl("Retiro por operacion cambios", desc, ignore.case = TRUE))     return("cambio")
  if (grepl("Plazo-",                      desc, fixed  = TRUE))           return("plazo")

  # English patterns (BanBajío USD accounts)
  if (grepl("^(Outgoing Wire|Wire Transfer Sent|WIRE OUT|Outgoing Transfer)",
            desc, ignore.case = TRUE, perl = TRUE))                        return("spei_out")
  if (grepl("^(Incoming Wire|Wire Transfer Received|WIRE IN|Credit Transfer|Incoming Transfer)",
            desc, ignore.case = TRUE, perl = TRUE))                        return("spei_in")
  if (grepl("^(commission|service fee|transfer fee|wire fee|gasto admon)",
            desc, ignore.case = TRUE, perl = TRUE))                        return("comision")
  if (grepl("internal transfer|book transfer|between accounts",
            desc, ignore.case = TRUE))                                     return("traspaso")
  if (grepl("payroll|nomina",
            desc, ignore.case = TRUE))                                     return("nomina")

  "otro"
}

# ── Field extractors by type ──────────────────────────────────────────────────

.extract_spei_out <- function(desc) {
  # Try new format first: "Beneficiario: NOMBRE (Dato no verificado…)"
  parte_raw <- .rx(desc, "Beneficiario:\\s*([^|]+)")
  # Fallback: old format without colon "Beneficiario NOMBRE Aut."
  if (is.na(parte_raw)) {
    parte_raw <- .rx(desc, "Beneficiario\\s+([A-Z][^|]+?)\\s+Aut\\.")
  }
  if (!is.na(parte_raw)) {
    parte_raw <- trimws(gsub(
      "\\s*\\(Dato no verificado por esta institucion\\)\\s*",
      "", parte_raw, ignore.case = TRUE))
  }
  # Concepto: text after "Concepto del Pago:" up to next "|" or " por (amount) mxn"
  concepto_raw <- .rx(desc, "Concepto del Pago:\\s*(.+?)\\s+por\\s+\\([0-9.,]+\\)\\s+mxn")
  if (is.na(concepto_raw)) {
    concepto_raw <- .rx(desc, "Concepto del Pago:\\s*([^|]+)")
  }
  list(
    parte         = parte_raw,
    rfc           = .rx(desc, "RFC Beneficiario:\\s*([A-Z0-9&\\.\\-]+)"),
    referencia    = .rx(desc, "Referencia:\\s*([^|]+)"),
    clave_rastreo = .rx(desc, "Clave de Rastreo:\\s*([A-Z0-9]+)"),
    concepto      = concepto_raw
  )
}

.extract_spei_in <- function(desc) {
  # Ordenante: up to "Cuenta Ordenante" or "RFC" — try lookahead first
  parte_raw <- .rx(desc, "Ordenante:\\s*([^|]+?)(?=\\s*Cuenta Ordenante|\\s*RFC)")
  if (is.na(parte_raw)) {
    parte_raw <- .rx(desc, "Ordenante:\\s*([^|]+)")
  }
  list(
    parte         = parte_raw,
    rfc           = .rx(desc, "RFC Ordenante:\\s*([A-Z0-9&\\.\\-]+)"),
    referencia    = .rx(desc, "Referencia:\\s*([^|]+)"),
    clave_rastreo = .rx(desc, "Clave de Rastreo:\\s*([A-Z0-9]+)"),
    concepto      = .rx(desc, "Concepto del Pago:\\s*(.+?)\\s*(?:\\||$)")
  )
}

.extract_traspaso <- function(desc) {
  # Concepto: text between "Suc. CODE " and " Aut." in BanBajío internal transfers
  # e.g. "Suc. 5feb NCS Fletes Aut.259711" → "NCS Fletes"
  # e.g. "Suc. 5feb Fondo de ahorro empleado2 Aut.258131" → "Fondo de ahorro empleado2"
  concepto_raw <- .rx(desc, "Suc\\.\\s*\\S+\\s+(.+?)\\s+Aut\\.")
  list(
    parte         = .rx(desc, "Beneficiario\\s*\\|\\s*([^|]+)"),
    rfc           = NA_character_,
    referencia    = .rx(desc, "Recibo\\s*#\\s*([0-9]+)"),
    clave_rastreo = NA_character_,
    concepto      = concepto_raw
  )
}

.extract_domiciliacion <- function(desc) {
  # Pattern: "Retiro por domiciliacion EMPRESA RefB[NNNNN] | por (importe) mxn"
  # Parte: everything between "domiciliacion " and " RefB["
  # [oó] covers both accented and unaccented spellings
  parte_raw <- .rx(desc, "domiciliaci[o\u00f3]n\\s+(.+?)\\s+RefB\\[")
  list(
    parte         = parte_raw,
    rfc           = NA_character_,
    referencia    = .rx(desc, "RefB\\[([^\\]]+)\\]"),
    clave_rastreo = NA_character_,
    concepto      = NA_character_
  )
}

# TEF (Transferencia Electrónica de Fondos) — BanBajío domestic wire format
# Example: "TEF Recibido por 346839.23 mxn 0026F1100000150408 BEmisor.HSBC Ordenante TBC MEXICO | ..."
.extract_tef_in <- function(desc) {
  list(
    parte         = .rx(desc, "Ordenante\\s+([^|\\n]+?)\\s*(?:\\||$)"),
    rfc           = NA_character_,
    referencia    = .rx(desc, "(?i)por\\s+[0-9.,]+\\s+mxn\\s+([A-Z0-9]+)"),
    clave_rastreo = NA_character_,
    concepto      = NA_character_
  )
}

.extract_tef_out <- function(desc) {
  list(
    parte         = .rx(desc, "Beneficiario\\s+([^|\\n]+?)\\s*(?:\\||$)"),
    rfc           = NA_character_,
    referencia    = .rx(desc, "(?i)por\\s+[0-9.,]+\\s+mxn\\s+([A-Z0-9]+)"),
    clave_rastreo = NA_character_,
    concepto      = NA_character_
  )
}

.extract_wire_out_en <- function(desc) {
  list(
    parte         = .rx(desc, "(?i)beneficiary[:\\s]+([^|\\n]+)"),
    rfc           = NA_character_,
    referencia    = .rx(desc, "(?i)reference[:\\s#]+([^|\\n]+)"),
    clave_rastreo = .rx(desc, "(?i)(trace|tracking)[\\s#:]+([A-Z0-9]+)", group = 2L),
    concepto      = .rx(desc, "(?i)concept(?:o)?[:\\s]+([^|\\n]+)")
  )
}

.extract_wire_in_en <- function(desc) {
  list(
    parte         = .rx(desc, "(?i)(?:sender|originator|from)[:\\s]+([^|\\n]+)"),
    rfc           = NA_character_,
    referencia    = .rx(desc, "(?i)reference[:\\s#]+([^|\\n]+)"),
    clave_rastreo = .rx(desc, "(?i)(trace|tracking)[\\s#:]+([A-Z0-9]+)", group = 2L),
    concepto      = .rx(desc, "(?i)concept(?:o)?[:\\s]+([^|\\n]+)")
  )
}

.empty_fields <- function() {
  list(parte = NA_character_, rfc = NA_character_,
       referencia = NA_character_, clave_rastreo = NA_character_,
       concepto = NA_character_)
}

# ── Empty schema tibble ───────────────────────────────────────────────────────
.empty_movimientos_parsed <- function() {
  tibble::tibble(
    cuenta_id       = character(),
    fecha           = as.Date(character()),
    hora            = character(),
    recibo          = character(),
    descripcion_raw = character(),
    tipo            = character(),
    parte           = character(),
    rfc             = character(),
    referencia      = character(),
    clave_rastreo   = character(),
    concepto        = character(),
    cargo           = numeric(),
    abono           = numeric(),
    saldo_banco     = numeric(),
    fuente          = character(),
    notas           = character()
  )
}

# =============================================================================
# parse_banbajio_txt()
# Main parser function.
# @param txt_content  Character vector from readLines(), OR single string.
# @param cuenta_id    UUID of the bancos_cuentas row being imported.
# @return tibble of parsed movements. Attribute "numero_cuenta" set to the
#         account number from line 1 (for validation vs. cuenta selector).
#         Columns match bancos_movimientos MINUS: id, conciliado, doc_vinculado,
#         agenda_id, importado_at, eliminado — added when saving to S3.
# =============================================================================
parse_banbajio_txt <- function(txt_content, cuenta_id) {
  # Accept single string or character vector
  if (length(txt_content) == 1L) {
    txt_content <- strsplit(txt_content, "\n", fixed = TRUE)[[1]]
  }
  # Strip Windows CR; drop truly empty lines
  txt_content <- gsub("\r", "", txt_content, fixed = TRUE)
  txt_content <- txt_content[nzchar(trimws(txt_content))]

  if (length(txt_content) < 3L) {
    warning("BanBajío TXT has fewer than 3 lines — nothing to parse.")
    out <- .empty_movimientos_parsed()
    attr(out, "numero_cuenta") <- NA_character_
    return(out)
  }

  # Line 1: account header — field 2 is the account number
  hdr         <- strsplit(txt_content[1L], ",", fixed = TRUE)[[1]]
  numero_cuenta <- if (length(hdr) >= 2L) trimws(hdr[2L]) else NA_character_

  # Line 2: column header row — skip
  # Lines 3+: movements
  data_lines <- txt_content[3L:length(txt_content)]

  rows <- lapply(data_lines, function(line) {
    p <- .split_bb_line(line)
    if (is.null(p)) return(NULL)

    tipo   <- .classify_tipo(p$descripcion)
    desc   <- p$descripcion
    fields <- switch(tipo,
      spei_out      = if (startsWith(desc, "SPEI Enviado:"))
                        .extract_spei_out(desc)
                      else if (grepl("^TEF Enviado", desc, ignore.case = TRUE))
                        .extract_tef_out(desc)
                      else
                        .extract_wire_out_en(desc),
      spei_in       = if (startsWith(desc, "SPEI Recibido:") ||
                          grepl("Devoluci", desc, fixed = TRUE))
                        .extract_spei_in(desc)
                      else if (grepl("^TEF Recibido", desc, ignore.case = TRUE))
                        .extract_tef_in(desc)
                      else
                        .extract_wire_in_en(desc),
      traspaso      = .extract_traspaso(desc),
      domiciliacion = .extract_domiciliacion(desc),
      .empty_fields()
    )

    fecha_num <- .parse_bb_date(p$fecha_str)

    tibble::tibble(
      cuenta_id       = as.character(cuenta_id),
      fecha           = fecha_num,                        # numeric days since epoch
      hora            = trimws(p$hora_str),
      recibo          = trimws(p$recibo),
      descripcion_raw = p$descripcion,
      tipo            = tipo,
      parte           = trimws(fields$parte         %||% NA_character_),
      rfc             = trimws(fields$rfc            %||% NA_character_),
      referencia      = trimws(fields$referencia     %||% NA_character_),
      clave_rastreo   = trimws(fields$clave_rastreo  %||% NA_character_),
      concepto        = trimws(fields$concepto       %||% NA_character_),
      cargo           = .parse_amt(p$cargo_str),
      abono           = .parse_amt(p$abono_str),
      saldo_banco     = .parse_amt(p$saldo_str),
      fuente          = "txt",
      notas           = NA_character_
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    out <- .empty_movimientos_parsed()
    attr(out, "numero_cuenta") <- numero_cuenta
    return(out)
  }

  result <- dplyr::bind_rows(rows) |>
    dplyr::mutate(fecha = as.Date(fecha, origin = "1970-01-01")) |>
    dplyr::arrange(dplyr::desc(fecha), dplyr::desc(hora))

  attr(result, "numero_cuenta") <- numero_cuenta
  result
}

# =============================================================================
# dedup_movimientos()
# Remove rows from `nuevos` that already exist in `existentes`.
#
# KEY: two-tier strategy based on whether the row has a monetary amount.
#
# Tier 1 — rows with cargo > 0 OR abono > 0 (real money movements):
#   4-tuple: (recibo, tipo, cargo_key, abono_key)
#   The recibo + amount pair is unique per transaction. desc_key is intentionally
#   excluded here because BanBajío can produce slightly different description
#   text for the same transaction across different TXT exports (e.g. different
#   download sessions), which caused false misses and double-imports.
#
# Tier 2 — rows with cargo = 0 AND abono = 0 (informational rows):
#   5-tuple: (recibo, tipo, cargo_key, abono_key, desc_key)
#   BanBajío emits 2 zero-amount rows per SPEI transfer sharing the same recibo:
#     1. "IVA COMISIÓN POR TRANSFE..."  cargo=0, abono=0
#     2. "COMISIÓN POR TRANSFERENC..."  cargo=0, abono=0
#   They share recibo, tipo (both "comision"), cargo=0, abono=0. The desc_key
#   (first 25 chars uppercased) is the only field that distinguishes them.
#
# Dedup is scoped to the same cuenta_id as `nuevos`. BanBajío recibo numbers
# are not globally unique across accounts — the same number can appear in
# different accounts legitimately.
#
# @return list(nuevos = tibble, n_duplicados = integer)
# =============================================================================
dedup_movimientos <- function(nuevos, existentes) {
  if (!nrow(nuevos)) return(list(nuevos = nuevos, n_duplicados = 0L))
  if (is.null(existentes) || !nrow(existentes)) {
    return(list(nuevos = nuevos, n_duplicados = 0L))
  }

  target_cuenta <- unique(nuevos$cuenta_id)
  ex_base <- existentes |>
    dplyr::filter(cuenta_id %in% target_cuenta, !is.na(recibo), nzchar(recibo),
                  !eliminado) |>
    dplyr::mutate(
      cargo_key = round(cargo, 2),
      abono_key = round(abono, 2),
      desc_key  = substr(toupper(trimws(descripcion_raw)), 1, 25),
      has_amount = cargo_key > 0 | abono_key > 0
    )

  ex_money <- ex_base |> dplyr::filter( has_amount) |>
    dplyr::distinct(recibo, tipo, cargo_key, abono_key)
  ex_zero  <- ex_base |> dplyr::filter(!has_amount) |>
    dplyr::distinct(recibo, tipo, cargo_key, abono_key, desc_key)

  nuevos_keyed <- nuevos |>
    dplyr::mutate(
      cargo_key  = round(cargo, 2),
      abono_key  = round(abono, 2),
      desc_key   = substr(toupper(trimws(descripcion_raw)), 1, 25),
      has_amount = cargo_key > 0 | abono_key > 0
    )

  money_genuinos <- nuevos_keyed |> dplyr::filter( has_amount) |>
    dplyr::anti_join(ex_money, by = c("recibo", "tipo", "cargo_key", "abono_key"))
  zero_genuinos  <- nuevos_keyed |> dplyr::filter(!has_amount) |>
    dplyr::anti_join(ex_zero,  by = c("recibo", "tipo", "cargo_key", "abono_key", "desc_key"))

  genuinos <- dplyr::bind_rows(money_genuinos, zero_genuinos) |>
    dplyr::select(-cargo_key, -abono_key, -desc_key, -has_amount)

  n_dup <- nrow(nuevos) - nrow(genuinos)

  list(nuevos = genuinos, n_duplicados = n_dup)
}

# =============================================================================
# auto_match_agenda()
# Attempt to automatically conciliate new TXT movements against confirmed
# agenda payments stored in bancos_confirmados.
#
# Match priority (first applicable wins):
#   1. Exact  : importe matches + fecha within ±0 days
#   2. Partial: importe within ±1% + fecha within ±2 days
#   (RFC-based matching is a future enhancement — confirmados lacks RFC field)
#
# Returns movimientos_nuevos with columns added:
#   conciliado, doc_vinculado, agenda_id
# =============================================================================
auto_match_agenda <- function(movimientos_nuevos, confirmados) {
  base_cols <- tibble::tibble(
    conciliado    = logical(),
    doc_vinculado = character(),
    agenda_id     = character()
  )

  if (!nrow(movimientos_nuevos)) {
    return(dplyr::bind_cols(
      movimientos_nuevos,
      base_cols[integer(0), ]
    ))
  }

  result <- dplyr::mutate(movimientos_nuevos,
    conciliado    = FALSE,
    doc_vinculado = NA_character_,
    agenda_id     = NA_character_
  )

  if (is.null(confirmados) || !nrow(confirmados)) return(result)

  conf <- confirmados |>
    dplyr::filter(!isTRUE(eliminado)) |>
    dplyr::select(agenda_item_id, documento, importe, fecha, mov_id)

  for (i in seq_len(nrow(result))) {
    if (isTRUE(result$conciliado[i])) next

    mov_importe <- if (result$cargo[i] > 0) result$cargo[i] else result$abono[i]
    if (mov_importe <= 0 || is.na(result$fecha[i])) next

    # 1. Exact match: same amount + same date
    exact <- conf |> dplyr::filter(
      abs(importe - mov_importe) < 0.01,
      !is.na(fecha),
      fecha == result$fecha[i]
    )
    if (nrow(exact) == 1L) {
      result$conciliado[i]    <- TRUE
      result$doc_vinculado[i] <- exact$documento[1L]
      result$agenda_id[i]     <- exact$agenda_item_id[1L]
      next
    }

    # 2. Partial match: importe ±1%, fecha ±2 days
    partial <- conf |> dplyr::filter(
      abs(importe - mov_importe) / pmax(mov_importe, 1) <= 0.01,
      !is.na(fecha),
      abs(as.numeric(fecha - result$fecha[i])) <= 2L
    )
    if (nrow(partial) == 1L) {
      result$conciliado[i]    <- TRUE
      result$doc_vinculado[i] <- partial$documento[1L]
      result$agenda_id[i]     <- partial$agenda_item_id[1L]
    }
  }

  result
}
