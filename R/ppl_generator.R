# =============================================================================
# R/ppl_generator.R  —  BanBajío "Pago a Proveedores" layout generator
#
# Field positions verified byte-by-byte against pagos_09032026_001.txt
# (bank-generated production file, 5 detail records, SPI + BCO).
#
# Record types:
#   01 = Header  (20 chars)
#        type(2) + seq(7) + fecha(8) + consec(3)
#
#   02 = Detail  (384 chars)
#        [0:2]    "02"
#        [2:9]    seq(7)
#        [9:11]   "01"
#        [11:19]  "00000000"        8 zeros
#        [19:31]  origen(12)        origin account, zero-padded (e.g. 073543010201)
#        [31:35]  "0140"            product code
#        [35:38]  banco(3)          dest bank CECOBAN code (first 3 digits of CLABE for SPI;
#                                   "030" for BCO internal Bajío transfers)
#        [38:53]  importe(15)       centavos, zero-padded, no decimal
#        [53:61]  fecha(8)          YYYYMMDD
#        [61:64]  medio(3)          "SPI" | "BCO"
#        [64:68]  pfx(4)            "4000" SPI | "0100" BCO
#        [68:86]  clabe(18)         destination CLABE (SPI) or internal acct (BCO)
#        [86:95]  "000000000"       IVA = 0
#        [95:110] alias(15)         left-aligned, space-padded, UPPERCASE
#        [110:125]"000000000000000" 15 zeros
#        [125:373]concepto(248)     left-aligned, space-padded
#        [373:375]"  "              2 spaces
#        [375:384]"000000000"       trailing zeros
#
#   09 = Trailer (34 chars)
#        type(2) + seq(7) + count(7) + total_centavos(18)
# =============================================================================

generate_ppl <- function(payments, cuenta_origen, fecha = Sys.Date(),
                         consecutivo = 1L) {
  stopifnot(is.data.frame(payments), nrow(payments) > 0)

  n        <- nrow(payments)
  fecha_s  <- format(as.Date(fecha), "%Y%m%d")
  consec_s <- formatC(as.integer(consecutivo), width = 3, flag = "0")

  # Helper: zero-pad a string to exactly `w` chars (left-pad, then truncate if over)
  zpad <- function(x, w) substr(stringr::str_pad(trimws(as.character(x)), w, "left",  "0"), 1, w)
  spad <- function(x, w) substr(stringr::str_pad(trimws(as.character(x)), w, "right", " "), 1, w)

  # origen: exactly 12 digits, zero-padded
  origen12 <- zpad(cuenta_origen, 12)

  # ── Header ─────────────────────────────────────────────────────────────────
  header <- paste0("01", "0000001", fecha_s, consec_s)   # 20 chars

  # ── Detail rows ────────────────────────────────────────────────────────────
  detail <- vapply(seq_len(n), function(i) {
    p     <- payments[i, ]
    seq_s <- formatC(i + 1L, width = 7, flag = "0")
    # clabe: exactly 18 chars, zero-padded from the left
    clabe <- zpad(p$clabe_dest, 18)

    # importe: 15 digits in centavos
    imp_s <- zpad(sprintf("%.0f", round(as.numeric(p$importe) * 100)), 15)

    # Auto-detect BanBajío: banco_dest field contains "030" prefix or "BAJIO",
    # OR medio_pago is explicitly set to "BCO".
    # For BCO: banco=030, pfx=0100. The clabe field already holds the account
    # number zero-padded to 18 — no change needed to that field.
    banco_field <- toupper(trimws(as.character(p$banco_dest %||% "")))
    medio_raw   <- toupper(trimws(as.character(p$medio_pago %||% "SPI")))
    is_bajio    <- grepl("^030", banco_field) || grepl("BAJIO", banco_field, fixed = TRUE)

    if (is_bajio || medio_raw == "BCO") {
      medio    <- spad("BCO", 3)
      banco_s  <- "030"
      bank_pfx <- "0100"
    } else {
      medio    <- spad(medio_raw, 3)
      banco_s  <- substr(clabe, 1, 3)
      bank_pfx <- "4000"
    }

    alias_s <- spad(toupper(substr(trimws(as.character(p$alias)), 1, 15)), 15)
    conc_s  <- spad(substr(trimws(as.character(p$concepto %||% "")), 1, 248), 248)

    row <- paste0(
      "02", seq_s, "01", "00000000",    # [0:19]
      origen12, "0140",                  # [19:35]  12+4
      banco_s,                           # [35:38]
      imp_s, fecha_s, medio, bank_pfx,  # [38:68]  15+8+3+4
      clabe, "000000000",               # [68:95]  18+9
      alias_s, "000000000000000",       # [95:125] 15+15
      conc_s,                            # [125:373]
      "  ", "000000000"                  # [373:384]
    )
    if (nchar(row) != 384)
      stop(sprintf(
        "PPL row %d has %d chars instead of 384. Check: orig=%d imp=%d clabe=%d medio=%d alias=%d conc=%d",
        i, nchar(row),
        nchar(origen12), nchar(imp_s), nchar(clabe), nchar(medio),
        nchar(alias_s), nchar(conc_s)
      ))
    row
  }, character(1))

  # ── Trailer ────────────────────────────────────────────────────────────────
  trailer <- paste0(
    "09",
    formatC(n + 2L, width = 7, flag = "0"),
    formatC(n,      width = 7, flag = "0"),
    zpad(sprintf("%.0f", round(sum(as.numeric(payments$importe)) * 100)), 18)
  )   # 34 chars

  c(header, detail, trailer)
}

# ── Write to disk: CRLF line endings, Latin-1 ─────────────────────────────────
write_ppl <- function(lines, path) {
  clean <- iconv(lines, from = "UTF-8", to = "ASCII//TRANSLIT")
  clean[is.na(clean)] <- lines[is.na(clean)]
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  for (line in clean) {
    writeBin(chartr("áéíóúñü", "aeionu u", line), con, useBytes = FALSE)
    writeBin(as.raw(c(0x0D, 0x0A)), con)
  }
  invisible(path)
}

# ── Validate before generating ────────────────────────────────────────────────
validate_ppl <- function(payments, cuenta_origen) {
  problems <- character(0)
  required <- c("alias", "clabe_dest", "medio_pago", "importe", "concepto")
  missing  <- setdiff(required, names(payments))
  if (length(missing))
    return(list(valid = FALSE,
                problems = paste("Missing columns:", paste(missing, collapse = ", "))))

  bad_amt   <- which(is.na(payments$importe) | payments$importe <= 0)
  if (length(bad_amt))
    problems <- c(problems, paste("Zero/missing importe rows:", paste(bad_amt, collapse = ",")))

  spi       <- payments[payments$medio_pago == "SPI", ]
  bad_clabe <- spi$alias[nchar(trimws(spi$clabe_dest)) != 18]
  if (length(bad_clabe))
    problems <- c(problems,
      paste("SPI needs 18-digit CLABE, invalid for:", paste(bad_clabe, collapse = ", ")))

  if (nchar(trimws(as.character(cuenta_origen))) < 11)
    problems <- c(problems, "cuenta_origen debe tener al menos 11 dígitos")

  list(valid = length(problems) == 0, problems = problems)
}