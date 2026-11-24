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
#        [9:11]   "01"              origin account type (01 = cheques)
#        [11:19]  "00000000"        8 zeros    } origin account, 20 digits
#        [19:31]  origen(12)        zero-padded} (e.g. 073543010201)
#        [31:33]  divisa(2)         "01" pesos | "02" dólar (PPL_DIVISAS, T94)
#        [33:38]  banco(5)          receiving bank, BanBajío's Anexo 4 key (PPL_BANCOS,
#                                   T94): from the CLABE's first 3 digits (SPI/SPD);
#                                   "40030" for BCO internal Bajío transfers
#        [38:53]  importe(15)       centavos, zero-padded, no decimal
#        [53:61]  fecha(8)          YYYYMMDD
#        [61:64]  medio(3)          "SPI" | "BCO"
#        [64:68]  pfx(4)            account type "40" CLABE | "01" BCO, then "00"
#        [68:86]  clabe(18)         destination CLABE (SPI) or internal acct (BCO)
#        [86:95]  "000000000"       filler
#        [95:110] alias(15)         left-aligned, space-padded, UPPERCASE
#        [110:125]"000000000000000" IVA = 0
#        [125:373]concepto(248)     left-aligned, space-padded  } Referencia, 250
#        [373:375]"  "              2 spaces                    }
#        [375:382]"0000000"         Referencia SPEI/SPID: zeros, the bank computes it
#        [382:384]clasif(2)         "00"; SPID: the operation's classification
#                                   (PPL_CLASIFICACIONES, T94)
#
# Positions as in BanBajío's "Guía Operativa Enlace Empresarial" v01.2026,
# layout "Pago a Proveedores" (pp. 26-27) -- the same bytes BajíoNet takes.
#
#   09 = Trailer (34 chars)
#        type(2) + seq(7) + count(7) + total_centavos(18)
# =============================================================================

# moneda: the file's currency (a code of PPL_DIVISAS). NULL = "01" (pesos),
# what every file carried before T94. clasificacion: the SPID operation's
# classification, used only when ppl_requiere_clasificacion(moneda);
# validated against PPL_CLASIFICACIONES (ppl_clasificacion()).
generate_ppl <- function(payments, cuenta_origen, fecha = Sys.Date(),
                         consecutivo = 1L, moneda = NULL, clasificacion = NULL) {
  stopifnot(is.data.frame(payments), nrow(payments) > 0)
  # The ORIGIN account is never blank or all zeros (CM30): zpad() below
  # would turn "" into "000000000000", a file the bank cannot debit from.
  # The screens check it first and tell the user; this is the last net.
  # (The destination CLABE in zeros is the "no account" line of _002, on
  # purpose -- that one is not checked here.)
  origen_txt <- if (length(cuenta_origen) == 1L && !is.na(cuenta_origen)) trimws(as.character(cuenta_origen)) else ""
  if (!grepl("[1-9]", origen_txt))
    stop("generate_ppl: no origin account (blank or zeros); nothing was generated", call. = FALSE)

  n        <- nrow(payments)
  fecha_s  <- format(as.Date(fecha), "%Y%m%d")
  consec_s <- formatC(as.integer(consecutivo), width = 3, flag = "0")

  # Helper: zero-pad a string to exactly `w` chars (left-pad, then truncate if over)
  zpad <- function(x, w) substr(stringr::str_pad(trimws(as.character(x)), w, "left",  "0"), 1, w)
  spad <- function(x, w) substr(stringr::str_pad(trimws(as.character(x)), w, "right", " "), 1, w)

  # origen: exactly 12 digits, zero-padded
  origen12 <- zpad(cuenta_origen, 12)

  # T94: the file's currency and, for SPID, its classification.
  divisa_s <- if (is.null(moneda)) "01" else unname(PPL_DIVISAS[toupper(trimws(as.character(moneda)))])
  if (length(divisa_s) != 1L || is.na(divisa_s))
    stop("generate_ppl: currency not in BanBajío's PPL (", paste(moneda, collapse = ","), "); nothing was generated", call. = FALSE)
  clasif_s <- if (!is.null(moneda) && ppl_requiere_clasificacion(moneda)) ppl_clasificacion(clasificacion) else "00"

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
    medio_raw   <- toupper(trimws(as.character(p$medio_pago %||% "SPI")))
    is_bajio    <- ppl_es_bajio(p$banco_dest %||% "", medio_raw)
    # T94: the receiving bank's 5-digit key from BanBajío's catalog -- it was
    # "40" + the CLABE's first 3 digits, right only for the 40xxx banks.
    banco_s <- ppl_banco_destino(p$clabe_dest, p$banco_dest %||% "", medio_raw)
    if (is.na(banco_s))
      stop(sprintf("generate_ppl: row %d, the receiving bank of CLABE prefix %s is not in BanBajío's catalog; nothing was generated",
                   i, substr(clabe, 1, 3)), call. = FALSE)

    if (is_bajio) {
      medio    <- spad("BCO", 3)
      bank_pfx <- "0100"
    } else {
      medio    <- spad(medio_raw, 3)
      bank_pfx <- "4000"
    }

    alias_s <- spad(toupper(substr(trimws(as.character(p$alias)), 1, 15)), 15)
    conc_s  <- spad(substr(trimws(as.character(p$concepto %||% "")), 1, 248), 248)

    row <- paste0(
      "02", seq_s, "01", "00000000",    # [0:19]
      origen12, divisa_s,                # [19:33]  12+2
      banco_s,                           # [33:38]  5
      imp_s, fecha_s, medio, bank_pfx,  # [38:68]  15+8+3+4
      clabe, "000000000",               # [68:95]  18+9
      alias_s, "000000000000000",       # [95:125] 15+15
      conc_s,                            # [125:373]
      "  ", "0000000", clasif_s          # [373:384] 2+7+2
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
# ── BanBajío's PPL rules on currencies (monedas fase 3, T34) ─────────────────
# These are the bank's rules, not a choice of the app, so the currency codes
# are written here (tests/test_monedas_guardian.R lists this file as an
# exception, with this reason): the PPL layout pays MXN by SPEI (SPI) or as
# an internal BanBajío transfer (BCO, destination bank 030), and USD by SPID
# (SPD). Nothing else. R/pagar_hoy_module.R asks these functions.
PPL_MONEDAS <- c("MXN", "USD")

# TRUE when the PPL file can carry payments in `moneda`.
ppl_moneda_soportada <- function(moneda) toupper(moneda) %in% PPL_MONEDAS

# ── BanBajío's PPL codes (T94) ───────────────────────────────────────────────
# Source of all three: BanBajío, "Guía Operativa Enlace Empresarial BanBajío",
# versión 01.2026 -- layout "Pago a Proveedores" (pp. 26-27) and Anexo 4,
# "Catálogo de bancos" (pp. 62-63). Bank rules, not the app's: when the bank
# publishes a new guide, this is the one place to update.

# Field 5, "Código de Divisa": the file's currency.
PPL_DIVISAS <- c(MXN = "01", USD = "02")

# Field 17, "Clasificación de la operación" -- the guide: "Si la transacción
# es un SPID, se debe clasificar la operación". Code -> its i18n key. Luis's
# treasury picks "07 Otro" in BajíoNet today, so that is the default.
PPL_CLASIFICACIONES <- c(
  "01" = "tablas.ppl.clasif_01", "02" = "tablas.ppl.clasif_02", "03" = "tablas.ppl.clasif_03",
  "04" = "tablas.ppl.clasif_04", "05" = "tablas.ppl.clasif_05", "06" = "tablas.ppl.clasif_06",
  "07" = "tablas.ppl.clasif_07")
PPL_CLASIFICACION_DEFECTO <- "07"

# The value to write: `x` only when it is one of the guide's codes; anything
# else (NULL, "", what a browser sent made up) is the default. Never the raw input.
ppl_clasificacion <- function(x) {
  x <- if (is.null(x) || !length(x)) "" else trimws(as.character(x[[1]]))
  if (!is.na(x) && x %in% names(PPL_CLASIFICACIONES)) x else PPL_CLASIFICACION_DEFECTO
}

# Field 6, "Banco Receptor": Anexo 4, keyed by the CLABE's first 3 digits
# (the last 3 of every key; no two institutions share them). 123 entries,
# read from the guide's text: 40xxx banca múltiple, 37xxx desarrollo, 90xxx
# others (STP, Nu, Klar, Mercado Pago...), 91xxx second keys, 2001 Banxico.
PPL_BANCOS <- c(
  "001" = "2001", "002" = "40002", "006" = "37006", "009" = "37009", "012" = "40012", "014" = "40014",
  "019" = "37019", "021" = "40021", "030" = "40030", "036" = "40036", "037" = "40037", "042" = "40042",
  "044" = "40044", "058" = "40058", "059" = "40059", "060" = "40060", "062" = "40062", "072" = "40072",
  "102" = "40102", "103" = "40103", "106" = "40106", "108" = "40108", "110" = "40110", "112" = "40112",
  "113" = "40113", "124" = "40124", "126" = "40126", "127" = "40127", "128" = "40128", "129" = "40129",
  "130" = "40130", "132" = "40132", "133" = "40133", "135" = "37135", "136" = "40136", "137" = "40137",
  "138" = "40138", "140" = "40140", "141" = "40141", "145" = "40145", "147" = "40147", "148" = "40148",
  "150" = "40150", "151" = "40151", "152" = "40152", "154" = "40154", "155" = "40155", "156" = "40156",
  "157" = "40157", "158" = "40158", "159" = "40159", "160" = "40160", "166" = "37166", "167" = "40167",
  "168" = "37168", "600" = "90600", "601" = "90601", "602" = "90602", "605" = "90605", "606" = "90606",
  "613" = "90613", "614" = "90614", "616" = "90616", "617" = "90617", "620" = "90620", "621" = "90621",
  "623" = "90623", "626" = "90626", "627" = "90627", "630" = "90630", "631" = "90631", "634" = "90634",
  "636" = "90636", "637" = "90637", "638" = "90638", "640" = "90640", "642" = "90642", "646" = "90646",
  "648" = "90648", "649" = "90649", "652" = "90652", "653" = "90653", "655" = "90655", "656" = "90656",
  "659" = "90659", "661" = "90661", "670" = "90670", "671" = "90671", "674" = "90674", "677" = "90677",
  "678" = "90678", "679" = "90679", "680" = "90680", "683" = "90683", "684" = "90684", "685" = "90685",
  "686" = "90686", "687" = "90687", "688" = "90688", "699" = "90699", "703" = "90703", "706" = "90706",
  "710" = "90710", "715" = "90715", "720" = "90720", "721" = "90721", "722" = "90722", "723" = "90723",
  "727" = "90727", "728" = "90728", "730" = "90730", "732" = "90732", "734" = "90734", "738" = "90738",
  "802" = "91802", "812" = "91812", "814" = "91814", "821" = "91821", "872" = "91872", "901" = "90901",
  "902" = "90902", "903" = "90903", "927" = "91927"
)

# TRUE per row when the payment is an internal BanBajío transfer (BCO): the
# supplier's bank says 030 / BAJIO, or its medio is BCO. generate_ppl()'s own
# rule, here so the Agenda asks the same thing before the file is written.
ppl_es_bajio <- function(banco_dest, medio_pago) {
  b <- toupper(trimws(as.character(banco_dest)))
  b[is.na(b)] <- ""
  m <- toupper(trimws(as.character(medio_pago)))
  m[is.na(m)] <- ""
  grepl("^030", b) | grepl("BAJIO", b, fixed = TRUE) | m == "BCO"
}

# The 5-digit "Banco Receptor" per row; NA when the CLABE's bank is not in
# BanBajío's catalog (the row cannot go in the file -- the Agenda says why).
# BCO: "40030". The app's own "no account" line (CLABE in zeros, T21b's _002)
# keeps what it always carried, "40000": BanBajío's platform completes it.
ppl_banco_destino <- function(clabe_dest, banco_dest, medio_pago) {
  # Padded exactly as generate_ppl() writes the CLABE field (zpad, 18).
  cl <- trimws(as.character(clabe_dest))
  cl[is.na(cl)] <- ""
  cl <- substr(stringr::str_pad(cl, 18, "left", "0"), 1, 18)
  pref <- substr(cl, 1, 3)
  out  <- unname(PPL_BANCOS[pref])
  out  <- ifelse(is.na(out), NA_character_, stringr::str_pad(out, 5, "left", "0"))
  # A bank of the 40 series (CLABE prefix 0xx-1xx: every 40xxx and 37xxx key
  # of the catalog is one) missing from the catalog -- CIBANCO 143, say --
  # keeps what it carried before T94, "40" + its prefix: it may well work,
  # and leaving it out would be a regression (Coordinador, 2026-11-24).
  # 6xx-9xx are the 90/91 series: not in the catalog, they stay out.
  out[ppl_banco_como_antes(clabe_dest, banco_dest, medio_pago)] <-
    paste0("40", pref[ppl_banco_como_antes(clabe_dest, banco_dest, medio_pago)])
  out[grepl("^0+$", cl)] <- "40000"
  out[ppl_es_bajio(banco_dest, medio_pago)] <- "40030"
  out
}

# TRUE per row whose bank is missing from BanBajío's catalog but is of the 40
# series (prefix 0xx-1xx): it goes in the file as before T94 ("40" + prefix)
# and the Agenda logs it ("fuera del catálogo de BanBajío, enviado como antes").
ppl_banco_como_antes <- function(clabe_dest, banco_dest, medio_pago) {
  cl <- trimws(as.character(clabe_dest))
  cl[is.na(cl)] <- ""
  cl <- substr(stringr::str_pad(cl, 18, "left", "0"), 1, 18)
  pref <- substr(cl, 1, 3)
  !pref %in% names(PPL_BANCOS) & grepl("^[01][0-9]{2}$", pref) & !grepl("^0+$", cl) &
    !ppl_es_bajio(banco_dest, medio_pago)
}

# TRUE per Agenda row whose payment can go in the file as far as its bank
# goes: the medio the file will carry (ppl_medio_pago, by the row's currency
# and bank) and then ppl_banco_destino(). The Agenda asks this, so it never
# keeps a copy of the medio rule.
ppl_banco_en_catalogo <- function(clabe_dest, banco_dest, moneda) {
  b <- dplyr::coalesce(as.character(banco_dest), "")
  !is.na(ppl_banco_destino(clabe_dest, b, ppl_medio_pago(moneda, b)))
}

# BanBajío asks for the "Clasificación de transacción" when the file is
# loaded on its platform for anything that is not MXN.
ppl_requiere_clasificacion <- function(moneda) toupper(moneda) != "MXN"

# medio_pago of each row: USD -> SPD (SPID); MXN to a BanBajío account
# (destination bank 030) -> BCO; anything else -> SPI (SPEI). `moneda` is
# one value or one per row; `banco_dest`, one per row ("" when unknown).
ppl_medio_pago <- function(moneda, banco_dest) {
  is_bajio <- grepl("^030$", trimws(banco_dest))
  dplyr::case_when(
    toupper(moneda) == "USD"            ~ "SPD",
    toupper(moneda) == "MXN" & is_bajio ~ "BCO",
    TRUE                                ~ "SPI"
  )
}

# How a Mexican bank writes a currency in words: its sheets and exports say
# "Pesos", "M.N.", "Moneda nacional", "Dólares", "DLLS"... A peso is MXN and a
# dólar is USD, whatever the client's base. The one list of those words (T36:
# bank accounts and the providers' Excel had one each, and they differed).
# "MXN" / "USD" for what it recognizes, NA for anything else.
ppl_moneda_palabra <- function(x) {
  t <- tolower(stringi::stri_trans_general(as.character(x), "Latin-ASCII"))
  t <- trimws(gsub("[^a-z0-9]+", " ", t))
  dplyr::case_when(
    grepl("peso", t) | t %in% c("mxn", "mn", "m n", "moneda nacional")               ~ "MXN",
    grepl("d.lar", t) | t %in% c("usd", "us", "dll", "dlls", "dolar americano")      ~ "USD",
    TRUE                                                                              ~ NA_character_
  )
}

# The currency of a company bank account (ctas_cuentas$Moneda). It holds a
# code ("MXN" | "USD"), but older rows and BanBajío's own sheets say "Pesos" |
# "Dólares" (ppl_moneda_palabra). A catalog code is kept as is; anything else
# (or nothing) is the client's base.
ppl_moneda_cuenta <- function(x, base) {
  mon <- toupper(trimws(x %||% base))
  dplyr::coalesce(ppl_moneda_palabra(mon), ifelse(moneda_valida(mon), mon, base))
}
