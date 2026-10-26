# =============================================================================
# R/bancos_detect.R
# Detects WHICH registered account (company + currency + subaccount) a
# BanBajío "ConsultaMovimientos" TXT belongs to.
#
# Why this exists: every BanBajío client downloads the same TXT layout, so a
# conglomerate with dozens of companies gets files that look identical except
# for the header. The old banner ranked accounts by counting 4+ letter words
# of COMPANY_MAP names inside the header razón social; it ignored currency
# entirely and, because COMPANY_MAP is global (not per client) and often
# resolves to bare initials, every account scored 0 and the first one won
# (NCS file -> "NL" suggested, 2026-10-26).
#
# Header line 1 (verified on 10 real files, 6 companies, MXN + USD):
#   <razón social>,<cuenta base>,<producto>,<subcuenta>,<fecha>,<hora>,
#   <periodo>,<filtro>,<rango>,,<n movs>,<total cargos>,<total abonos>
#   e.g. NETWORKS CROSSDOCKING SERVICES S.A. DE C.V.,7354301,Dólar,02,...
#
# Settings stores the full account as [0-pad]<base><producto 2d><subcuenta 2d>
#   7354301 + 04 + 02 -> "073543010402"  (NCS USD, header subcuenta "02")
#   13386156 + 04 + 01 -> "0133861560401" (NTS USD, header subcuenta "01")
# so base + subcuenta identify the account exactly; currency then confirms.
#
# Signals, strongest first:
#   1. account base number (header) vs numero_cuenta / CLABE in Settings
#   2. subcuenta (header field 4) vs last 2 digits of numero_cuenta
#   3. currency: header producto ("Dólar" -> USD) + vote over descriptions
#   4. razón social vs the CLIENT's empresas registry (razon_social,
#      nombre_corto, nombres_alt) — never the global COMPANY_MAP
# Integrity: header total cargos/abonos must equal the parsed sums (they
# matched to the cent on all 10 sample files; the movement COUNT does not —
# NCS USD header says 93 with 105 rows — so the count is not checked).
# =============================================================================

# User-facing strings in one place so they can be swapped for i18n later.
.BNC_DETECT_TXT <- list(
  motivo_numero    = "Coincide el número de cuenta %s",
  motivo_sub       = "subcuenta %s",
  motivo_moneda    = "moneda %s",
  motivo_nombre    = "Coincide la razón social",
  aviso_sin_numero = "Ninguna cuenta registrada tiene el número %s; sugerencia por nombre. Regístralo en Configuración → Cuentas para detección exacta.",
  aviso_moneda_no  = "No se pudo confirmar la moneda del archivo.",
  aviso_moneda_dif = "El archivo parece estar en %s y la cuenta sugerida es %s.",
  aviso_sub_dif    = "La subcuenta del archivo (%s) no coincide con la cuenta sugerida.",
  aviso_nombre_dif = "La razón social del archivo no coincide con la empresa %s.",
  aviso_empate     = "Hay %d cuentas igual de probables; confirma la correcta.",
  nota_otra_cuenta = "Seleccionaste una cuenta distinta a la sugerida.",
  aviso_nada       = "No se pudo identificar la cuenta; selecciónala manualmente.",
  aviso_totales    = "Los totales del archivo no cuadran con los movimientos leídos (cargos %s vs %s, abonos %s vs %s). El archivo podría estar incompleto."
)

# Legal-form and filler tokens that carry no identity ("S.A. DE C.V.", etc.).
.BNC_LEGAL_TOKENS <- c(
  "SA", "S", "A", "DE", "CV", "C", "V", "RL", "SC", "SAPI", "SAB", "SOFOM",
  "ENR", "ER", "SOCIEDAD", "ANONIMA", "CAPITAL", "VARIABLE",
  "RESPONSABILIDAD", "LIMITADA", "CIVIL", "Y", "EL", "LA", "LOS", "LAS", "DEL"
)

.digits <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  gsub("[^0-9]", "", x)
}
.strip_zeros <- function(x) sub("^0+", "", x)

# Uppercase, ASCII, punctuation -> space, legal-form tokens dropped.
.norm_razon <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x <- toupper(gsub("[^A-Za-z0-9]+", " ", x))
  vapply(strsplit(trimws(x), "\\s+"), function(tk) {
    tk <- tk[nzchar(tk) & !tk %in% .BNC_LEGAL_TOKENS]
    paste(tk, collapse = " ")
  }, character(1))
}

# 1 = same normalized name; otherwise token Jaccard, with a floor of 0.9
# when one multi-word name is fully contained in the other
# ("NETWORKS CROSSDOCKING" inside "NETWORKS CROSSDOCKING SERVICES").
.name_similarity <- function(a, b) {
  a <- .norm_razon(a); b <- .norm_razon(b)
  if (!nzchar(a) || !nzchar(b)) return(NA_real_)
  if (identical(a, b)) return(1)
  ta <- unique(strsplit(a, " ")[[1]]); tb <- unique(strsplit(b, " ")[[1]])
  jac <- length(intersect(ta, tb)) / length(union(ta, tb))
  small <- if (length(ta) <= length(tb)) ta else tb
  big   <- if (length(ta) <= length(tb)) tb else ta
  if (length(small) >= 2L && all(small %in% big)) max(jac, 0.9) else jac
}

# =============================================================================
# leer_encabezado_banbajio()
# Parses TXT line 1. The account base is located as the first all-digit field
# (6+ digits) instead of by position, so a razón social containing commas
# ("COCA COLA, S.A. DE C.V.") still parses.
# @return list(razon_social, numero, producto, subcuenta, n, total_cargos,
#              total_abonos) — fields NA when absent.
# =============================================================================
leer_encabezado_banbajio <- function(lines) {
  out <- list(razon_social = "", numero = NA_character_, producto = NA_character_,
              subcuenta = NA_character_, n = NA_integer_,
              total_cargos = NA_real_, total_abonos = NA_real_)
  if (!length(lines)) return(out)
  hdr <- gsub("\r", "", lines[[1L]], fixed = TRUE)
  p   <- trimws(strsplit(hdr, ",", fixed = TRUE)[[1]])
  i   <- which(grepl("^[0-9]{6,}$", p))[1]
  if (is.na(i)) {
    out$razon_social <- p[1L] %||% ""
    return(out)
  }
  out$razon_social <- paste(p[seq_len(i - 1L)], collapse = ", ")
  out$numero       <- p[i]
  out$producto     <- if (length(p) >= i + 1L) p[i + 1L] else NA_character_
  out$subcuenta    <- if (length(p) >= i + 2L && grepl("^[0-9]{1,3}$", p[i + 2L]))
                        p[i + 2L] else NA_character_
  num <- function(s) suppressWarnings(as.numeric(gsub(",", "", s, fixed = TRUE)))
  n <- length(p)
  if (n >= i + 5L) {
    out$total_cargos <- num(p[n - 1L])
    out$total_abonos <- num(p[n])
    out$n            <- suppressWarnings(as.integer(p[n - 2L]))
  }
  out
}

# =============================================================================
# .moneda_txt()
# "USD" / "MXN" / NA. The header producto decides when it is explicit
# ("Dólar" arrives mojibaked as "D��lar", hence the loose "lar"
# match); otherwise a majority vote over the movement descriptions, where
# BanBajío writes "por (X) mxn" / "usd" and SPID (USD) vs SPEI (MXN).
# =============================================================================
.moneda_txt <- function(producto, data_lines) {
  prod <- producto %||% ""
  if (!is.na(prod) && nzchar(prod)) {
    if (grepl("lar|usd|dll", prod, ignore.case = TRUE, useBytes = TRUE)) return("USD")
    if (grepl("peso|mxn|cheq", prod, ignore.case = TRUE, useBytes = TRUE)) return("MXN")
  }
  if (!length(data_lines)) return(NA_character_)
  n_usd <- sum(grepl("\\busd\\b|SPID|en dolar", data_lines, ignore.case = TRUE, useBytes = TRUE))
  n_mxn <- sum(grepl("\\bmxn\\b|SPEI", data_lines, ignore.case = TRUE, useBytes = TRUE))
  if (n_usd == 0L && n_mxn == 0L) return(NA_character_)
  if (n_usd >= 2L * max(n_mxn, 1L)) return("USD")
  if (n_mxn >= 2L * max(n_usd, 1L)) return("MXN")
  NA_character_
}

# Does a Settings account number (or CLABE) correspond to the TXT base?
# Returns list(match, sub_ok): sub_ok is NA when Settings only holds the
# short base (no subaccount digits to compare).
.cuenta_vs_base <- function(numero_cuenta, clabe, base, subcuenta) {
  b <- .strip_zeros(.digits(base))
  if (!nzchar(b)) return(list(match = FALSE, sub_ok = NA))
  n <- .strip_zeros(.digits(numero_cuenta))
  if (nzchar(n) && startsWith(n, b)) {
    rest <- substr(n, nchar(b) + 1L, nchar(n))
    # Base(7-8) + producto(2) + subcuenta(2) at most; a longer remainder means
    # the short base merely prefixes an unrelated longer number.
    if (nchar(rest) <= 4L) {
      sub_ok <- if (nchar(rest) == 4L && !is.na(subcuenta) && nzchar(subcuenta))
                  identical(substr(rest, 3L, 4L), sprintf("%02d", as.integer(subcuenta)))
                else NA
      return(list(match = TRUE, sub_ok = sub_ok))
    }
  }
  # CLABE = banco(3) + plaza(3) + cuenta(11) + dígito(1)
  cl <- .digits(clabe)
  if (nchar(cl) == 18L) {
    cta <- .strip_zeros(substr(cl, 7L, 17L))
    if (nzchar(cta) && startsWith(cta, b)) return(list(match = TRUE, sub_ok = NA))
  }
  list(match = FALSE, sub_ok = NA)
}

# Names an empresa is known by in the client's registry.
.empresa_nombres <- function(ini, empresas_df) {
  if (is.null(empresas_df) || !nrow(empresas_df) || !"initials" %in% names(empresas_df))
    return(character(0))
  r <- empresas_df[!is.na(empresas_df$initials) & empresas_df$initials == ini, , drop = FALSE]
  if (!nrow(r)) return(character(0))
  nm <- c(r$razon_social %||% character(0), r$nombre_corto %||% character(0))
  if ("nombres_alt" %in% names(r)) {
    for (j in r$nombres_alt) {
      alt <- tryCatch(as.character(jsonlite::fromJSON(j %||% "[]")),
                      error = function(e) character(0))
      nm <- c(nm, alt)
    }
  }
  nm <- trimws(as.character(nm))
  unique(nm[!is.na(nm) & nzchar(nm)])
}

# =============================================================================
# detectar_cuenta_banbajio()
# @param lines        readLines() of the TXT (header + column row + data).
# @param cuentas_df   candidate accounts (bancos_cuentas shape: cuenta_id,
#                     empresa, moneda, numero_cuenta, clabe).
# @param empresas_df  the CLIENT's empresas registry (shared$empresas_db()).
# @param parsed       optional parse_banbajio_txt() result, for the totals check.
# @return list(encabezado, moneda, ranking (cuentas_df reordered + score cols),
#              sugerida, confianza = "alta"|"media"|"baja", motivos, avisos)
# =============================================================================
detectar_cuenta_banbajio <- function(lines, cuentas_df, empresas_df = NULL,
                                     parsed = NULL) {
  T_   <- .BNC_DETECT_TXT
  lines <- gsub("\r", "", lines, fixed = TRUE)
  lines <- lines[nzchar(trimws(lines))]
  enc   <- leer_encabezado_banbajio(lines)
  data_lines <- if (length(lines) > 2L) lines[3:length(lines)] else character(0)
  moneda <- .moneda_txt(enc$producto, data_lines)

  # Integrity notes are about the FILE, not about which account it is, so they
  # are shown but never lower the identification confidence.
  avisos_archivo <- character(0)
  if (!is.null(parsed) && nrow(parsed) && !is.na(enc$total_cargos) &&
      !is.na(enc$total_abonos)) {
    sc <- round(sum(parsed$cargo, na.rm = TRUE), 2)
    sa <- round(sum(parsed$abono, na.rm = TRUE), 2)
    if (abs(sc - enc$total_cargos) > 0.005 || abs(sa - enc$total_abonos) > 0.005) {
      f <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
      avisos_archivo <- sprintf(T_$aviso_totales, f(enc$total_cargos), f(sc),
                                f(enc$total_abonos), f(sa))
    }
  }

  empty <- list(encabezado = enc, moneda = moneda, ranking = cuentas_df,
                sugerida = NA_character_, confianza = "baja",
                motivos = character(0), avisos = c(T_$aviso_nada, avisos_archivo))
  if (is.null(cuentas_df) || !nrow(cuentas_df)) return(empty)

  k <- nrow(cuentas_df)
  num_match <- logical(k); sub_ok <- rep(NA, k)
  mon_ok <- rep(NA, k); nom_sim <- rep(NA_real_, k)
  for (i in seq_len(k)) {
    m <- .cuenta_vs_base(cuentas_df$numero_cuenta[i], cuentas_df$clabe[i] %||% NA,
                         enc$numero, enc$subcuenta)
    num_match[i] <- m$match
    sub_ok[i]    <- m$sub_ok
    cm <- toupper(trimws(cuentas_df$moneda[i] %||% ""))
    if (!is.na(moneda) && nzchar(cm)) mon_ok[i] <- identical(cm, moneda)
    nms <- .empresa_nombres(cuentas_df$empresa[i], empresas_df)
    if (length(nms) && nzchar(enc$razon_social))
      nom_sim[i] <- suppressWarnings(max(vapply(nms, .name_similarity, numeric(1),
                                                b = enc$razon_social), na.rm = TRUE))
  }
  nom_sim[is.infinite(nom_sim)] <- NA_real_

  score <- 100 * num_match +
           ifelse(is.na(sub_ok), 0, ifelse(sub_ok, 20, -40)) +
           ifelse(is.na(mon_ok), 0, ifelse(mon_ok, 40, -80)) +
           30 * ifelse(is.na(nom_sim), 0, nom_sim)

  ord <- order(-score, seq_len(k))
  rk  <- cuentas_df[ord, , drop = FALSE]
  rk$det_score <- score[ord];  rk$det_num <- num_match[ord]
  rk$det_sub   <- sub_ok[ord]; rk$det_moneda <- mon_ok[ord]
  rk$det_nombre <- nom_sim[ord]

  top <- rk[1L, ]
  # Ties: other accounts with the same evidence profile as the top one.
  n_empate <- sum(abs(rk$det_score - top$det_score) < 1e-9)
  base_fmt <- enc$numero %||% ""
  avisos   <- character(0)

  motivos <- character(0)
  if (isTRUE(top$det_num)) {
    m1 <- sprintf(T_$motivo_numero, base_fmt)
    extra <- c(if (isTRUE(top$det_sub)) sprintf(T_$motivo_sub, enc$subcuenta),
               if (isTRUE(top$det_moneda)) sprintf(T_$motivo_moneda, moneda))
    motivos <- paste(c(m1, extra), collapse = " · ")
  } else if (isTRUE(top$det_nombre >= 0.8)) {
    motivos <- paste(c(T_$motivo_nombre,
                       if (isTRUE(top$det_moneda)) sprintf(T_$motivo_moneda, moneda)),
                     collapse = " · ")
  }

  if (!any(num_match) && !is.na(enc$numero))
    avisos <- c(avisos, sprintf(T_$aviso_sin_numero, base_fmt))
  if (is.na(moneda)) avisos <- c(avisos, T_$aviso_moneda_no)
  if (isFALSE(top$det_moneda))
    avisos <- c(avisos, sprintf(T_$aviso_moneda_dif, moneda, top$moneda))
  if (isFALSE(top$det_sub))
    avisos <- c(avisos, sprintf(T_$aviso_sub_dif, enc$subcuenta))
  # Number says one company, name says another: worth a second look.
  if (isTRUE(top$det_num) && !is.na(top$det_nombre) && top$det_nombre < 0.34)
    avisos <- c(avisos, sprintf(T_$aviso_nombre_dif, top$empresa))
  if (n_empate > 1L) avisos <- c(avisos, sprintf(T_$aviso_empate, n_empate))

  confianza <-
    if (isTRUE(top$det_num) && !isFALSE(top$det_moneda) && !isFALSE(top$det_sub) &&
        n_empate == 1L && !(isTRUE(top$det_nombre < 0.34)) && !length(avisos))
      "alta"
    else if (top$det_score >= 30 + 40 * 0.5 && n_empate == 1L)
      "media"
    else "baja"
  if (confianza == "baja" && top$det_score <= 0)
    avisos <- unique(c(avisos, T_$aviso_nada))

  list(encabezado = enc, moneda = moneda, ranking = rk,
       sugerida = top$cuenta_id, confianza = confianza,
       motivos = motivos, avisos = unique(c(avisos, avisos_archivo)))
}
