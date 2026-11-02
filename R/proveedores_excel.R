# =============================================================================
# R/proveedores_excel.R
# The Proveedores Excel import, as pure functions: no Shiny, no S3. The module
# (R/proveedores_module.R) reads the file with prov_leer_excel(), classifies
# with prov_clasificar(), shows the result and applies it with prov_aplicar().
#
# Why this exists (T08): the old import lost data in ways nobody saw --
# Reemplazar deleted and recreated the provider (id, empresas, activo_hasta
# and every field the file left empty were gone), invisible spaces (no-break,
# thin, zero-width, BOM) stayed in CLABEs and RFCs, headers with accents or
# dots weren't found, a CLABE Excel kept as a number came back with other
# digits, and bank 30 paid as SPI instead of BCO. Every rule below answers one
# of those (tests/test_proveedores_excel_repro.R).
# =============================================================================

# ── Cells ────────────────────────────────────────────────────────────────────
# Every Unicode space, plus zero-width ones and the BOM.
.PROV_ESPACIO <- "[\\s\\p{Z}\\u200B-\\u200D\\u2060\\uFEFF]"

# Trims every kind of space at both ends; "", "NA", "N/A" and "-" are empty.
prov_limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_replace_all_regex(x, paste0("^", .PROV_ESPACIO, "+|", .PROV_ESPACIO, "+$"), "")
  # Spaces inside become one plain space ("FLETES  EL GUERO").
  x <- stringi::stri_replace_all_regex(x, paste0(.PROV_ESPACIO, "+"), " ")
  x[is.na(x) | toupper(x) %in% c("", "NA", "N/A", "-")] <- NA_character_
  x
}

# One field's cleaning: the account loses every space and hyphen, the RFC
# goes upper case without spaces, the e-mail lower case.
prov_limpiar_campo <- function(campo, x) {
  x <- prov_limpiar_texto(x)
  switch(campo,
    no_cuenta = stringi::stri_replace_all_regex(x, paste0(.PROV_ESPACIO, "|-"), ""),
    rfc       = toupper(stringi::stri_replace_all_regex(x, .PROV_ESPACIO, "")),
    correo    = tolower(stringi::stri_replace_all_regex(x, paste0(.PROV_ESPACIO, "*,", .PROV_ESPACIO, "*"), ",")),
    x)
}

# ── Headers ──────────────────────────────────────────────────────────────────
# Clean, without accents, lower case, punctuation as space: "Núm. de cuenta"
# -> "num de cuenta", "R.F.C." -> "r f c", "no_cuenta" -> "no cuenta".
prov_normalizar_encabezado <- function(x) {
  x <- prov_limpiar_texto(x)
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- stringi::stri_replace_all_regex(x, "[^a-z0-9]+", " ")
  trimws(x)
}

# What each field's header may say, already normalized. Exact match first;
# then, for what's still missing, a header containing one of the words.
PROV_EXCEL_CAMPOS <- list(
  nombre        = c("nombre", "nombre del beneficiario", "beneficiario", "nombre del proveedor", "proveedor",
                    "razon social", "nombre beneficiario"),
  alias         = c("alias", "alias del beneficiario", "alias beneficiario", "alias corto"),
  banco         = c("banco", "banco destino", "clave banco", "clave del banco", "banco beneficiario"),
  tipo_cuenta   = c("tipo de cuenta", "tipo cuenta"),
  no_cuenta     = c("no cuenta", "no de cuenta", "num de cuenta", "numero de cuenta", "num cuenta", "cuenta",
                    "clabe", "cuenta clabe", "no cuenta clabe", "clabe interbancaria", "no de cuenta clabe", "numero"),
  rfc           = c("rfc", "r f c"),
  correo        = c("correo", "correo electronico", "email", "e mail", "mail"),
  moneda        = c("moneda", "divisa"),
  tipo_relacion = c("tipo de relacion", "tipo relacion", "relacion"),
  # BanBajío's export (T08b): informative, never written to the catalog.
  estatus        = c("estatus", "status"),
  fecha_registro = c("fecha de registro", "fecha registro", "fecha de alta")
)
# Word searched in the second pass, in an order where the specific goes before
# the generic ("Tipo de cuenta" is not the account; "Nombre del banco" is the bank).
.PROV_EXCEL_CONTIENE <- list(
  tipo_cuenta = "tipo( de)? cuenta", tipo_relacion = "relacion", banco = "banco",
  no_cuenta = c("clabe", "cuenta"), rfc = "rfc", correo = c("correo", "e ?mail"), moneda = c("moneda", "divisa"),
  alias = "alias", nombre = c("nombre", "beneficiario", "proveedor", "razon social")
)

# Field -> the file's header it reads from (NULL when not found).
prov_mapear_columnas <- function(encabezados) {
  norm  <- prov_normalizar_encabezado(encabezados)
  norm[is.na(norm)] <- ""
  usado <- logical(length(norm))
  out   <- stats::setNames(vector("list", length(PROV_EXCEL_CAMPOS)), names(PROV_EXCEL_CAMPOS))
  for (campo in names(PROV_EXCEL_CAMPOS)) {
    j <- which(!usado & norm %in% PROV_EXCEL_CAMPOS[[campo]])[1]
    if (!is.na(j)) { out[[campo]] <- encabezados[j]; usado[j] <- TRUE }
  }
  for (campo in names(.PROV_EXCEL_CONTIENE)) {
    if (!is.null(out[[campo]])) next
    pat <- paste0("\\b(", paste(.PROV_EXCEL_CONTIENE[[campo]], collapse = "|"), ")\\b")
    j <- which(!usado & nzchar(norm) & grepl(pat, norm, perl = TRUE))[1]
    if (!is.na(j)) { out[[campo]] <- encabezados[j]; usado[j] <- TRUE }
  }
  out
}

# A header row has 3 or more known headers, by exact name: a data row rarely
# says just "Banco", "RFC" or "Divisa" in its cells.
prov_tiene_encabezado <- function(fila) {
  norm <- prov_normalizar_encabezado(fila)
  sum(norm %in% unlist(PROV_EXCEL_CAMPOS), na.rm = TRUE) >= 3L
}

# The header row wherever it is among the first `hasta` rows (BanBajío's
# export has 5 rows of general data above it); NA: no header (format A).
prov_fila_encabezado <- function(texto, hasta = 30L) {
  for (i in seq_len(min(hasta, nrow(texto)))) if (prov_tiene_encabezado(texto[i, ])) return(i)
  NA_integer_
}

# An Excel date: a serial number (45785 = 2025-05-08) or a date text.
prov_fecha <- function(x) {
  x <- prov_limpiar_texto(x)
  out <- rep(as.Date(NA), length(x))
  serial <- !is.na(x) & grepl("^[0-9]{4,6}([.][0-9]+)?$", x)
  out[serial] <- as.Date(floor(as.numeric(x[serial])), origin = "1899-12-30")
  dmy <- !is.na(x) & !serial & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", x)
  out[dmy] <- as.Date(substr(x[dmy], 1, 10), format = "%d/%m/%Y")
  iso <- !is.na(x) & !serial & !dmy
  out[iso] <- suppressWarnings(as.Date(substr(x[iso], 1, 10), optional = TRUE))
  out
}

# The BanBajío file without header: columns by position.
PROV_EXCEL_POSICION <- c("nombre", "alias", "banco", "tipo_cuenta", "no_cuenta", "rfc", "correo", "moneda",
                         "tipo_relacion")

# ── Reading ──────────────────────────────────────────────────────────────────
# A cell as text, and whether Excel kept it as a number. Whole numbers are
# written without decimals or exponent; what Excel already rounded can't be
# undone here -- prov_cuenta() decides whether to trust it.
.prov_celda <- function(v) {
  if (is.null(v) || length(v) == 0L || (length(v) == 1L && is.na(v))) return(list(texto = NA_character_, numero = FALSE))
  if (is.numeric(v)) {
    txt <- if (is.finite(v) && v == round(v) && abs(v) < 1e21) sprintf("%.0f", v) else format(v, digits = 15)
    return(list(texto = txt, numero = TRUE))
  }
  if (inherits(v, c("POSIXt", "Date"))) return(list(texto = format(as.Date(v)), numero = FALSE))
  list(texto = as.character(v), numero = FALSE)
}

# Reads the file. Returns list(filas, formato = "A"/"B", columnas, faltan):
#   filas    one row per non-empty Excel row: .row_num, each field's raw text,
#            and .num_no_cuenta / .num_banco (the cell was a number in Excel)
#   columnas field -> where it came from ("No. Cuenta", or "columna E" in A)
#   faltan   fields the file doesn't have
# NULL when the file has no rows or can't be read.
prov_leer_excel <- function(path) {
  crudo <- tryCatch(suppressMessages(readxl::read_excel(path, col_names = FALSE, col_types = "list",
                                                        .name_repair = "minimal")),
                    error = function(e) NULL)
  if (is.null(crudo) || !nrow(crudo) || !ncol(crudo)) return(NULL)
  celdas <- lapply(crudo, function(col) lapply(col, .prov_celda))
  texto  <- vapply(celdas, function(col) vapply(col, `[[`, "", "texto"), character(nrow(crudo)))
  numero <- vapply(celdas, function(col) vapply(col, `[[`, TRUE, "numero"), logical(nrow(crudo)))
  if (is.null(dim(texto))) { texto <- matrix(texto, nrow = 1L); numero <- matrix(numero, nrow = 1L) }

  h <- prov_fila_encabezado(texto)
  formato_b <- !is.na(h)
  no_usadas <- character(0)
  if (formato_b) {
    mapa  <- prov_mapear_columnas(texto[h, ])
    datos <- seq_len(nrow(texto))[-seq_len(h)]
    col_de <- function(campo) if (is.null(mapa[[campo]])) NA_integer_ else match(mapa[[campo]], texto[h, ])
    columnas <- lapply(mapa, function(x) if (is.null(x)) NULL else prov_limpiar_texto(x))
    enc <- prov_limpiar_texto(texto[h, ])
    no_usadas <- enc[!is.na(enc) & !enc %in% unlist(lapply(mapa, prov_limpiar_texto)) &
                     nzchar(prov_normalizar_encabezado(enc))]
  } else {
    datos  <- seq_len(nrow(texto))
    col_de <- function(campo) { j <- match(campo, PROV_EXCEL_POSICION); if (!is.na(j) && j <= ncol(texto)) j else NA_integer_ }
    columnas <- stats::setNames(lapply(PROV_EXCEL_POSICION, function(campo) {
      j <- col_de(campo); if (is.na(j)) NULL else paste("columna", LETTERS[j])
    }), PROV_EXCEL_POSICION)
  }
  campo_txt <- function(campo) { j <- col_de(campo); if (is.na(j)) rep(NA_character_, length(datos)) else texto[datos, j] }
  campo_num <- function(campo) { j <- col_de(campo); if (is.na(j)) rep(FALSE, length(datos)) else numero[datos, j] }
  filas <- data.frame(.row_num = datos, stringsAsFactors = FALSE)
  for (campo in names(PROV_EXCEL_CAMPOS)) filas[[campo]] <- campo_txt(campo)
  filas$.num_no_cuenta <- campo_num("no_cuenta")
  filas$.num_banco     <- campo_num("banco")
  # Rows with nothing that identifies a provider are trailing blanks; a row
  # whose first cell says "Nota: ..." is the export's footnote.
  vacia <- Reduce(`&`, lapply(c("nombre", "no_cuenta", "banco", "rfc"), function(k) is.na(prov_limpiar_texto(filas[[k]]))))
  primera <- apply(texto[datos, , drop = FALSE], 1L, function(r) { r <- prov_limpiar_texto(r); r[!is.na(r)][1] %||% NA_character_ })
  nota <- !is.na(primera) & grepl("^nota\\b", prov_normalizar_encabezado(primera))
  filas <- filas[!vacia & !nota, , drop = FALSE]
  rownames(filas) <- NULL
  faltan <- setdiff(names(columnas)[vapply(columnas, is.null, logical(1))], c("estatus", "fecha_registro"))
  list(filas = filas, formato = if (formato_b) "B" else "A", columnas = columnas[!vapply(columnas, is.null, logical(1))],
       faltan = faltan, no_usadas = no_usadas,
       fila_encabezado = if (formato_b) h else NA_integer_,
       ignoradas_arriba = if (formato_b) h - 1L else 0L,
       ignoradas_nota = sum(nota))
}

# ── Accounts ─────────────────────────────────────────────────────────────────
# CLABE check digit: weights 3, 7, 1 over the first 17 digits, modulo 10.
prov_clabe_valida <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s) || !grepl("^[0-9]{18}$", s)) return(FALSE)
    d <- as.integer(strsplit(s, "", fixed = TRUE)[[1]])
    (10L - sum((d[1:17] * rep(c(3L, 7L, 1L), 6L)[1:17]) %% 10L) %% 10L) %% 10L == d[18]
  }, logical(1), USE.NAMES = FALSE)
}

# Largest whole number a double keeps exactly: above it, Excel already
# changed digits and nothing can tell which.
.PROV_ENTERO_EXACTO <- 2^53

# One account: list(valor, estado = "ok" | "reparada" | "aviso" | "revisar", motivo).
# motivo is a dictionary key (tablas.proveedores.importar.*).
prov_cuenta <- function(texto, es_numero = FALSE) {
  t <- prov_limpiar_campo("no_cuenta", texto)
  if (is.na(t)) return(list(valor = NA_character_, estado = "ok", motivo = NA_character_))
  revisar <- function(m) list(valor = t, estado = "revisar", motivo = m)
  if (grepl("^[0-9.]+[eE][+-]?[0-9]+$", t) || (isTRUE(es_numero) && grepl("[.]", t)))
    return(revisar("tablas.proveedores.importar.motivo_numero"))
  if (isTRUE(es_numero) && grepl("^[0-9]+$", t) && as.numeric(t) > .PROV_ENTERO_EXACTO)
    return(revisar("tablas.proveedores.importar.motivo_numero"))
  if (!grepl("^[0-9]+$", t)) return(list(valor = t, estado = "ok", motivo = NA_character_))  # not a CLABE (abroad, etc.)
  n <- nchar(t)
  if (n == 18L) {
    if (prov_clabe_valida(t)) return(list(valor = t, estado = "ok", motivo = NA_character_))
    return(revisar("tablas.proveedores.importar.motivo_dv"))
  }
  # Excel drops leading zeros of a number (a text cell of 17 digits too, when
  # someone typed it without the 0): put them back only if the check digit
  # proves it.
  if (n == 17L || (isTRUE(es_numero) && n == 16L)) {
    con_ceros <- paste0(strrep("0", 18L - n), t)
    if (prov_clabe_valida(con_ceros))
      return(list(valor = con_ceros, estado = "reparada", motivo = "tablas.proveedores.importar.motivo_cero"))
    if (n == 17L) return(revisar("tablas.proveedores.importar.motivo_17"))
  }
  # 10-12 or 16 digits: not a CLABE, so no check digit to prove anything. If
  # Excel had it as a number it may have dropped leading zeros: imported, but
  # with a visible warning ("aviso") instead of being held back.
  if (isTRUE(es_numero))
    return(list(valor = t, estado = "aviso", motivo = "tablas.proveedores.importar.aviso_numero"))
  list(valor = t, estado = "ok", motivo = NA_character_)
}

# A CLABE's first 3 digits ARE its bank's key: when the file's bank says
# another, one of the two is a typing error.
prov_banco_coincide <- function(cuenta, banco) {
  es_clabe <- !is.na(cuenta) & grepl("^[0-9]{18}$", cuenta) & !is.na(banco) & grepl("^[0-9]{3}$", banco)
  !es_clabe | substr(cuenta, 1L, 3L) == banco
}

# ── Bank names ───────────────────────────────────────────────────────────────
# key -> the names people write for it. Keys are BanBajío's catalog (the 89
# institutions of .BANCOS_MX_CHOICES, R/settings/settings_helpers.R); the
# extra names were checked against the CLABE prefixes of a real BanBajío
# export (T08b): KAPITAL accounts are 128 (formerly Autofin), CITI MEXICO and
# CBM BANCO are 124 (Citi's license), NUBANK is 638.
PROV_BANCOS <- list(
  "002" = c("BANAMEX", "CITIBANAMEX", "BANCO NACIONAL DE MEXICO"),
  "006" = c("BANCOMEXT"), "009" = c("BANOBRAS"),
  "012" = c("BBVA", "BBVA MEXICO", "BBVA BANCOMER", "BANCOMER"),
  "014" = c("SANTANDER", "SANTANDER MEXICO"), "019" = c("BANJERCITO"),
  "021" = c("HSBC", "HSBC MEXICO"), "030" = c("BAJIO", "BANBAJIO", "BANCO DEL BAJIO", "DEL BAJIO"),
  "036" = c("INBURSA"), "042" = c("MIFEL"), "044" = c("SCOTIABANK", "SCOTIA", "SCOTIABANK INVERLAT"),
  "058" = c("BANREGIO", "REGIO"), "059" = c("INVEX"), "060" = c("BANSI"), "062" = c("AFIRME"),
  "072" = c("BANORTE", "BANORTE IXE", "IXE"), "106" = c("BANK OF AMERICA", "BOFA"), "108" = c("MUFG"),
  "110" = c("JP MORGAN"), "112" = c("BMONEX", "MONEX"), "113" = c("VE POR MAS", "VEPORMAS"),
  "124" = c("CBM BANCO", "CITI MEXICO", "CITI", "CITIBANK"), "126" = c("CREDIT SUISSE"),
  "127" = c("AZTECA"), "128" = c("KAPITAL", "KAPITAL BANK", "AUTOFIN"), "129" = c("BARCLAYS"),
  "130" = c("COMPARTAMOS"), "132" = c("MULTIVA BANCO", "MULTIVA"), "133" = c("ACTINVER"), "135" = c("NAFIN"),
  "136" = c("INTERCAM BANCO", "INTERCAM"), "137" = c("BANCOPPEL", "COPPEL"), "138" = c("ABC CAPITAL"),
  "140" = c("CONSUBANCO"), "141" = c("VOLKSWAGEN"), "143" = c("CIBANCO"), "145" = c("BBASE"),
  "147" = c("BANKAOOL"), "148" = c("PAGATODO"), "150" = c("INMOBILIARIO"), "151" = c("DONDE"),
  "152" = c("BANCREA"), "154" = c("COVALTO"), "155" = c("ICBC"), "156" = c("SABADELL"), "157" = c("SHINHAN"),
  "158" = c("MIZUHO BANK", "MIZUHO"), "159" = c("BANK OF CHINA"), "160" = c("S3"), "166" = c("BABIEN"),
  "168" = c("HIPOTECARIA FEDERAL"), "600" = c("MONEXCB"), "601" = c("GBM"), "602" = c("MASARI"),
  "605" = c("VALUE"), "608" = c("VECTOR"), "613" = c("MULTIVA CBOLSA"), "616" = c("FINAMEX"),
  "617" = c("VALMEX"), "620" = c("PROFUTURO"), "623" = c("SKANDIA"), "630" = c("CB INTERCAM"),
  "631" = c("CI BOLSA"), "634" = c("FINCOMUN"), "636" = c("HDI SEGUROS"), "638" = c("NU MEXICO", "NUBANK", "NU"),
  "646" = c("STP"), "648" = c("TACTIV CB"), "649" = c("OSKNDIA"), "652" = c("CREDICAPITAL"), "653" = c("KUSPIT"),
  "656" = c("UNAGRA"), "659" = c("ASP INTEGRA OPC", "ASP INTEGRA"), "661" = c("KLAR"), "670" = c("LIBERTAD"),
  "677" = c("CAJA POP MEXICANA"), "680" = c("CRISTOBAL COLON"), "683" = c("CAJA TELEFONISTA"),
  "684" = c("TRANSFER"), "685" = c("FONDO FIRA"), "686" = c("INVERCAP"), "688" = c("CREDICLUB"),
  "689" = c("FOMPED"), "699" = c("FONDEADORA"), "703" = c("TESORED"), "706" = c("ARCUS"), "710" = c("NVIO"),
  "722" = c("MERCADO PAGO W", "MERCADO PAGO"), "723" = c("CUENCA"), "902" = c("INDEVAL")
)

# A bank name as it is compared: no accents, upper case, without the legal
# form or the word BANCO ("BANCO AZTECA, S.A., INSTITUCION DE BANCA MULTIPLE"
# -> "AZTECA").
prov_banco_normalizar <- function(x) {
  x <- toupper(stringi::stri_trans_general(prov_limpiar_texto(x), "Latin-ASCII"))
  x <- stringi::stri_replace_all_regex(x, "\\bS\\.?\\s*A\\.?(\\s*B\\.?)?\\b|\\bDE\\s+C\\.?\\s*V\\.?|INSTITUCION DE BANCA MULTIPLE|GRUPO FINANCIERO|\\bBANCO\\b", " ")
  x <- stringi::stri_replace_all_regex(x, "[^A-Z0-9]+", " ")
  trimws(stringi::stri_replace_all_regex(x, "\\s+", " "))
}

.PROV_BANCOS_TABLA <- local({
  claves <- rep(names(PROV_BANCOS), lengths(PROV_BANCOS))
  data.frame(clave = claves, nombre = prov_banco_normalizar(unlist(PROV_BANCOS, use.names = FALSE)),
             stringsAsFactors = FALSE)
})

# The first (display) name of a key: "012" -> "BBVA".
prov_banco_nombre <- function(clave) {
  out <- vapply(as.character(clave), function(k) PROV_BANCOS[[k]][1] %||% k, "", USE.NAMES = FALSE)
  out[is.na(clave)] <- NA_character_
  out
}

# Name -> key, vectorized; NA where not recognized. Exact first, then a known
# name contained as whole words ("BBVA MEXICO SA" -> 012). catalogo: extra
# "Nombre|CLAVE" values (the app's .BANCOS_MX_CHOICES).
prov_banco_por_nombre <- function(x, catalogo = character(0)) {
  tabla <- .PROV_BANCOS_TABLA
  if (length(catalogo)) {
    partes <- strsplit(catalogo[nzchar(catalogo) & grepl("\\|", catalogo)], "|", fixed = TRUE)
    partes <- Filter(function(p) length(p) == 2L && grepl("^[0-9]{3}$", p[2]), partes)
    if (length(partes))
      tabla <- rbind(tabla, data.frame(clave = unname(vapply(partes, `[[`, "", 2L)),
                                       nombre = prov_banco_normalizar(vapply(partes, `[[`, "", 1L)),
                                       stringsAsFactors = FALSE))
  }
  n <- prov_banco_normalizar(x)
  out <- tabla$clave[match(n, tabla$nombre)]
  # Longest name first, so "CITIBANAMEX" doesn't stop at "CITI".
  orden <- order(-nchar(tabla$nombre))
  for (i in which(is.na(out) & !is.na(n) & nzchar(n))) {
    hit <- orden[vapply(tabla$nombre[orden], function(nm) grepl(paste0("\\b", nm, "\\b"), n[i]), logical(1))][1]
    if (!is.na(hit)) out[i] <- tabla$clave[hit]
  }
  out
}

# ── Bank ─────────────────────────────────────────────────────────────────────
# The 3-digit key, as the edit modal stores it: "30" -> "030",
# "012-BBVA BANCOMER" -> "012", "Banorte" -> "072" (by the catalog's names).
# list(valor, estado = "ok" | "revisar", motivo).
prov_banco_clave <- function(texto, catalogo = character(0)) {
  t <- prov_limpiar_texto(texto)
  if (is.na(t)) return(list(valor = NA_character_, estado = "ok", motivo = NA_character_))
  ok <- function(v) list(valor = v, estado = "ok", motivo = NA_character_)
  if (grepl("^[0-9]{1,3}$", t)) return(ok(formatC(as.integer(t), width = 3L, flag = "0")))
  if (grepl("^[0-9]{3}([^0-9]|$)", t)) return(ok(substr(t, 1L, 3L)))
  clave <- prov_banco_por_nombre(t, catalogo)
  if (!is.na(clave)) return(ok(clave))
  list(valor = t, estado = "revisar", motivo = "tablas.proveedores.importar.motivo_banco")
}

# ── Currency, payment type, status ───────────────────────────────────────────
prov_moneda <- function(x) {
  t <- prov_normalizar_encabezado(x)
  out <- prov_limpiar_texto(x)
  out[!is.na(t) & grepl("^(mxn|mn|m n|peso|pesos|moneda nacional)$", t)] <- "MXN"
  out[!is.na(t) & grepl("^(usd|us|dll|dlls|dolar|dolares|dolar americano)$", t)] <- "USD"
  out
}

# The account type in words: BanBajío's codes and its export's words are the
# same thing ("40" = "CLABE", "1" = "Cuenta BB"), so a catalog that kept the
# code and a file that brings the word don't count as a change (T08b: 433 of
# 451 rows of the real export "changed" only that).
prov_tipo_cuenta <- function(x) {
  t <- prov_normalizar_encabezado(x)
  out <- prov_limpiar_texto(x)
  out[!is.na(t) & t %in% c("40", "clabe")] <- "CLABE"
  out[!is.na(t) & t %in% c("1", "cuenta bb", "cuenta bajio")] <- "Cuenta BB"
  out[!is.na(t) & t == "tarjeta de debito"] <- "Tarjeta de Débito"
  out[!is.na(t) & t == "tarjeta de credito"] <- "Tarjeta de Crédito"
  out
}

# As the edit modal: USD pays SPD, BanBajío (030) BCO, the rest SPI.
prov_medio_pago <- function(moneda, banco) {
  ifelse(!is.na(moneda) & moneda == "USD", "SPD", ifelse(!is.na(banco) & banco == "030", "BCO", "SPI"))
}

prov_status <- function(nombre, moneda, banco, no_cuenta) {
  lleno <- function(v) !is.na(v) & nzchar(trimws(v))
  ifelse(lleno(nombre) & lleno(moneda) & lleno(banco) & lleno(no_cuenta), "completo", "incompleto")
}

# ── Classifying ──────────────────────────────────────────────────────────────
# The fields the Excel can bring, in the order the review shows changes.
PROV_EXCEL_ACTUALIZA <- c("nombre", "alias", "banco", "no_cuenta", "rfc", "correo", "moneda", "tipo_relacion",
                          "tipo_cuenta")

# An existing provider's account as the import compares it: no_cuenta, else
# clabe, row by row (25 providers in dev only have it in clabe).
prov_cuenta_existente <- function(df) {
  if (is.null(df) || !nrow(df)) return(character(0))
  nc <- if ("no_cuenta" %in% names(df)) as.character(df$no_cuenta) else rep(NA_character_, nrow(df))
  cl <- if ("clabe" %in% names(df)) as.character(df$clabe) else rep(NA_character_, nrow(df))
  prov_limpiar_campo("no_cuenta", dplyr::coalesce(prov_limpiar_texto(nc), prov_limpiar_texto(cl)))
}

# Cleans the read rows and sorts each one:
#   listo        new, ready
#   incompleto   no name, account or bank
#   revisar      account or bank that can't be trusted (.motivo says why);
#                never imported until the file is fixed
#   duplicado    the same account earlier in the file
#   actualiza    an existing provider of the list it goes to, with changes
#   sin_cambios  an existing provider, nothing to change
#   reactiva     an existing INACTIVE provider, going to Activos (or
#   inactiva     the reverse): updated in place and moved
#   fuera_rango  registered before `desde` (the file's Fecha de Registro):
#                not even classified
# Adds .cambios (list column of list(campo, antes, despues)), .motivo (a
# dictionary key), .nota (dictionary keys, "|"-separated: repairs, the bank
# taken from the CLABE), .banco_archivo / .banco_clabe (for the bank
# messages), .existente_id and .existente_lista ("activos"/"inactivos"),
# .alias_vinculos (Partes linked to the old alias) and .destino (the list a
# row goes to: `tipo`, or Inactivos when the file's Estatus isn't Activo).
# tipo: the list the file goes to ("activos" / "inactivos").
prov_clasificar <- function(filas, activos, inactivos, tipo = "activos", catalogo = character(0),
                            alias_mapa = NULL, desde = NULL) {
  n <- nrow(filas)
  f <- filas
  for (campo in setdiff(names(PROV_EXCEL_CAMPOS), c("no_cuenta", "banco", "moneda")))
    f[[campo]] <- prov_limpiar_campo(campo, f[[campo]])
  f$moneda <- prov_moneda(f$moneda)
  f$tipo_cuenta <- prov_tipo_cuenta(f$tipo_cuenta)
  f$.fecha <- prov_fecha(f$fecha_registro)
  fuera <- if (!is.null(desde) && length(desde) == 1L && !is.na(desde)) !is.na(f$.fecha) & f$.fecha < as.Date(desde)
           else rep(FALSE, n)

  # ── Account ──
  cuentas <- lapply(seq_len(n), function(i) prov_cuenta(f$no_cuenta[i], isTRUE(f$.num_no_cuenta[i])))
  est_cuenta <- vapply(cuentas, `[[`, "", "estado")
  f$no_cuenta <- vapply(cuentas, `[[`, "", "valor")
  motivo <- ifelse(est_cuenta == "revisar", vapply(cuentas, `[[`, "", "motivo"), NA_character_)
  nota <- ifelse(est_cuenta %in% c("reparada", "aviso"), vapply(cuentas, `[[`, "", "motivo"), NA_character_)

  # ── Bank ── A valid CLABE says its bank (its first 3 digits): the file's
  # bank name is only checked against it. A "Cuenta BB" (11-13 digits) is
  # BanBajío. Anything else (cards, other accounts) goes by the name.
  texto_banco <- prov_limpiar_texto(f$banco)
  clave_nombre <- rep(NA_character_, n)
  num <- !is.na(texto_banco) & grepl("^[0-9]{1,3}$", texto_banco)
  clave_nombre[num] <- formatC(as.integer(texto_banco[num]), width = 3L, flag = "0")
  pref <- !is.na(texto_banco) & !num & grepl("^[0-9]{3}([^0-9]|$)", texto_banco)
  clave_nombre[pref] <- substr(texto_banco[pref], 1L, 3L)
  resto <- !is.na(texto_banco) & !num & !pref
  clave_nombre[resto] <- prov_banco_por_nombre(texto_banco[resto], catalogo)
  es_clabe <- est_cuenta %in% c("ok", "reparada") & !is.na(f$no_cuenta) & grepl("^[0-9]{18}$", f$no_cuenta) &
              prov_clabe_valida(f$no_cuenta)
  clave_clabe <- ifelse(es_clabe, substr(f$no_cuenta, 1L, 3L), NA_character_)
  es_bb <- !es_clabe & !is.na(f$tipo_cuenta) & grepl("^cuenta bb$", prov_normalizar_encabezado(f$tipo_cuenta)) &
           !is.na(f$no_cuenta) & grepl("^[0-9]{11,13}$", f$no_cuenta)
  banco <- clave_nombre
  banco[es_clabe] <- clave_clabe[es_clabe]
  banco[es_bb] <- "030"
  de_clabe <- es_clabe & is.na(clave_nombre)                      # name missing or not recognized
  choca_clabe <- es_clabe & !is.na(clave_nombre) & clave_nombre != clave_clabe
  choca_bb    <- es_bb & !is.na(clave_nombre) & clave_nombre != "030"
  sin_nombre  <- !es_clabe & !es_bb & !is.na(texto_banco) & is.na(clave_nombre)
  banco[sin_nombre] <- texto_banco[sin_nombre]
  motivo[is.na(motivo) & choca_clabe] <- "tablas.proveedores.importar.motivo_banco_clabe"
  motivo[is.na(motivo) & choca_bb]    <- "tablas.proveedores.importar.motivo_banco_bb"
  motivo[is.na(motivo) & sin_nombre]  <- "tablas.proveedores.importar.motivo_banco"
  nota[de_clabe] <- ifelse(is.na(nota[de_clabe]), "tablas.proveedores.importar.nota_banco_clabe",
                           paste(nota[de_clabe], "tablas.proveedores.importar.nota_banco_clabe", sep = "|"))
  f$banco <- banco
  f$.banco_archivo <- texto_banco
  f$.banco_clabe   <- ifelse(es_bb, "030", clave_clabe)
  f$.motivo <- motivo
  f$.nota   <- nota
  # The file's Estatus (BanBajío): a row that isn't "Activo" goes to Inactivos.
  f$.destino <- ifelse(!is.na(f$estatus) & !grepl("^activ", tolower(f$estatus)), "inactivos", tipo)

  # ── The catalog, prepared once (comparable values, vectorized) ──
  prep <- function(df) {
    if (is.null(df) || !nrow(df)) return(NULL)
    col <- function(k) if (k %in% names(df)) as.character(df[[k]]) else rep(NA_character_, nrow(df))
    b <- prov_limpiar_texto(col("banco"))
    bk <- rep(NA_character_, length(b))
    nb <- !is.na(b) & grepl("^[0-9]{1,3}$", b); bk[nb] <- formatC(as.integer(b[nb]), width = 3L, flag = "0")
    pb <- !is.na(b) & !nb & grepl("^[0-9]{3}([^0-9]|$)", b); bk[pb] <- substr(b[pb], 1L, 3L)
    rb <- !is.na(b) & !nb & !pb; bk[rb] <- prov_banco_por_nombre(b[rb], catalogo)
    bk[is.na(bk)] <- b[is.na(bk)]
    comp <- list(banco = bk, moneda = prov_moneda(col("moneda")), no_cuenta = prov_cuenta_existente(df),
                 tipo_cuenta = prov_tipo_cuenta(col("tipo_cuenta")))
    for (campo in setdiff(PROV_EXCEL_ACTUALIZA, names(comp))) comp[[campo]] <- prov_limpiar_campo(campo, col(campo))
    list(df = df, llave = comp$no_cuenta, comp = comp, crudo = lapply(stats::setNames(PROV_EXCEL_ACTUALIZA, PROV_EXCEL_ACTUALIZA),
         function(k) if (k == "no_cuenta") comp$no_cuenta else col(k)))
  }
  act <- prep(activos); ina <- prep(inactivos)
  alias_vinc <- if (!is.null(alias_mapa) && nrow(alias_mapa)) table(toupper(prov_limpiar_texto(alias_mapa$alias))) else NULL

  f$.existente_id    <- NA_character_
  f$.existente_lista <- NA_character_
  f$.cambios         <- rep(list(list()), n)
  f$.alias_vinculos  <- 0L
  estado <- character(n)
  vistas <- new.env(hash = TRUE)
  for (i in seq_len(n)) {
    if (fuera[i]) { estado[i] <- "fuera_rango"; next }
    if (!is.na(f$.motivo[i])) { estado[i] <- "revisar"; next }
    if (is.na(f$nombre[i]) || is.na(f$no_cuenta[i]) || is.na(f$banco[i])) { estado[i] <- "incompleto"; next }
    k <- f$no_cuenta[i]
    # The same account earlier in the file: a duplicate, whether it is new or
    # already in the catalog (applying it twice would be the second's word).
    if (!is.null(vistas[[k]])) { estado[i] <- "duplicado"; next }
    vistas[[k]] <- TRUE
    j_act <- if (!is.null(act)) match(k, act$llave) else NA_integer_
    j_ina <- if (!is.na(j_act) || is.null(ina)) NA_integer_ else match(k, ina$llave)
    if (!is.na(j_act) || !is.na(j_ina)) {
      lista <- if (!is.na(j_act)) "activos" else "inactivos"
      p <- if (!is.na(j_act)) act else ina
      j <- if (!is.na(j_act)) j_act else j_ina
      f$.existente_id[i] <- as.character(p$df$id[j])
      f$.existente_lista[i] <- lista
      cambios <- list()
      for (campo in PROV_EXCEL_ACTUALIZA) {
        nuevo <- f[[campo]][i]
        if (is.na(nuevo)) next                                  # empty or missing column: keep
        if (identical(p$comp[[campo]][j], nuevo)) next
        cambios[[length(cambios) + 1L]] <- list(campo = campo, antes = p$crudo[[campo]][j], despues = nuevo)
      }
      f$.cambios[[i]] <- cambios
      # A new alias breaks the Parte -> alias links (parte_alias_map): the PPL
      # would stop finding this provider. Flag it; the map isn't touched here.
      viejo <- toupper(prov_limpiar_texto(p$crudo$alias[j]))
      if (!is.na(f$alias[i]) && !is.na(viejo) && toupper(f$alias[i]) != viejo && !is.null(alias_vinc))
        f$.alias_vinculos[i] <- as.integer(alias_vinc[viejo] %||% 0L) %||% 0L
      destino <- f$.destino[i]
      estado[i] <- if (lista != destino) { if (destino == "activos") "reactiva" else "inactiva" }
                   else if (length(cambios)) "actualiza" else "sin_cambios"
      next
    }
    estado[i] <- "listo"
  }
  f$.alias_vinculos[is.na(f$.alias_vinculos)] <- 0L
  f$.estado <- estado
  f
}

# ── Applying ─────────────────────────────────────────────────────────────────
# What Importar writes. clasificadas: prov_clasificar()'s rows; decisiones:
# named list account -> "reemplazar" | "conservar" | "omitir" for the rows
# that touch an existing provider (actualiza / reactiva / inactiva; nothing
# else). opciones: list(tipo, empresa, moneda (the file's, for new rows
# without one), incluir_incompletos, incluir_duplicados, cambiar_alias,
# lote, ahora, nuevo_id = function() <uuid>).
# Returns list(activos, inactivos, cambia_activos, cambia_inactivos,
# nuevos, actualizados, movidos, omitidos_alias).
prov_aplicar <- function(clasificadas, activos, inactivos, decisiones = list(), opciones = list()) {
  op <- utils::modifyList(list(tipo = "activos", empresa = "", moneda = "MXN", incluir_incompletos = FALSE,
                               incluir_duplicados = FALSE, cambiar_alias = FALSE, lote = NA_character_,
                               ahora = as.character(Sys.time()), nuevo_id = function() uuid::UUIDgenerate()),
                          opciones)
  f <- clasificadas
  out <- list(activos = activos, inactivos = inactivos, cambia_activos = FALSE, cambia_inactivos = FALSE,
              nuevos = 0L, actualizados = 0L, movidos = 0L, omitidos_alias = 0L)
  # Each row's list: the file's (op$tipo), or the row's own .destino (the
  # Estatus of BanBajío's export).
  destino_de <- function(i) if (".destino" %in% names(f) && !is.na(f$.destino[i])) f$.destino[i]
                            else if (op$tipo == "activos") "activos" else "inactivos"

  # New providers: ready rows, and the opted-in incomplete / first duplicate.
  vistos <- character(0)
  nuevas <- integer(0)
  for (i in seq_len(nrow(f))) {
    e <- f$.estado[i]
    if (e == "listo" || (e == "incompleto" && isTRUE(op$incluir_incompletos))) { nuevas <- c(nuevas, i); next }
    if (e == "duplicado" && isTRUE(op$incluir_duplicados) && !f$no_cuenta[i] %in% vistos) {
      nuevas <- c(nuevas, i); vistos <- c(vistos, f$no_cuenta[i])
    }
  }
  for (lista in c("activos", "inactivos")) {
    de_lista <- nuevas[vapply(nuevas, destino_de, "") == lista]
    if (!length(de_lista)) next
    x <- f[de_lista, , drop = FALSE]
    moneda <- dplyr::coalesce(x$moneda, op$moneda)
    filas_nuevas <- data.frame(
      id = vapply(seq_along(de_lista), function(k) op$nuevo_id(), ""), Empresa = op$empresa, codigo = NA_character_,
      nombre = x$nombre, alias = x$alias, clabe = x$no_cuenta, medio_pago = prov_medio_pago(moneda, x$banco),
      rfc = x$rfc, tipo = "012", banco_destino = x$banco, activo = lista == "activos", no_cuenta = x$no_cuenta,
      banco = x$banco, tipo_cuenta = x$tipo_cuenta, correo = x$correo, moneda = moneda,
      tipo_relacion = x$tipo_relacion, empresas = op$empresa,
      status = prov_status(x$nombre, moneda, x$banco, x$no_cuenta), fuente = "excel", upload_batch = op$lote,
      activo_hasta = "indefinido", created_at = op$ahora, updated_at = op$ahora, stringsAsFactors = FALSE)
    out[[lista]] <- dplyr::bind_rows(out[[lista]], filas_nuevas)
    out[[paste0("cambia_", lista)]] <- TRUE
    out$nuevos <- out$nuevos + length(de_lista)
  }

  # Existing providers: update in place -- only what the file brings and
  # differs -- and move them when the file goes to the other list.
  for (i in which(f$.estado %in% c("actualiza", "reactiva", "inactiva"))) {
    if (!identical(decisiones[[f$no_cuenta[i]]], "reemplazar")) next
    if (f$.alias_vinculos[i] > 0L && !isTRUE(op$cambiar_alias)) { out$omitidos_alias <- out$omitidos_alias + 1L; next }
    origen <- f$.existente_lista[i]
    j <- match(f$.existente_id[i], as.character(out[[origen]]$id))
    if (is.na(j)) next
    for (c in f$.cambios[[i]])                                  # a field the list never had (tipo_cuenta)
      if (!c$campo %in% names(out[[origen]])) out[[origen]][[c$campo]] <- NA_character_
    fila <- out[[origen]][j, , drop = FALSE]
    for (c in f$.cambios[[i]]) {
      fila[[c$campo]] <- c$despues
      if (c$campo == "no_cuenta") fila$clabe <- c$despues
      if (c$campo == "banco") fila$banco_destino <- c$despues
    }
    moneda <- prov_moneda(fila$moneda)
    fila$medio_pago <- prov_medio_pago(moneda, prov_banco_clave(fila$banco)$valor)
    fila$status     <- prov_status(fila$nombre, moneda, fila$banco, dplyr::coalesce(fila$no_cuenta, fila$clabe))
    fila$updated_at <- op$ahora
    tipo_lista <- destino_de(i)
    if (origen == tipo_lista) {
      out[[origen]][j, ] <- fila
      out[[paste0("cambia_", origen)]] <- TRUE
      out$actualizados <- out$actualizados + 1L
    } else {
      fila$activo <- tipo_lista == "activos"
      out[[origen]] <- out[[origen]][-j, , drop = FALSE]
      out[[tipo_lista]] <- dplyr::bind_rows(out[[tipo_lista]], fila)
      out$cambia_activos <- TRUE; out$cambia_inactivos <- TRUE
      out$movidos <- out$movidos + 1L
    }
  }
  out
}

# ── Template ─────────────────────────────────────────────────────────────────
# The round-trip file: every column the import reads, with the headers it
# recognizes, and the account, RFC and bank as TEXT cells so Excel doesn't
# turn them into numbers while someone edits them.
PROV_PLANTILLA_COLUMNAS <- c(nombre = "Nombre", alias = "Alias", banco = "Banco", tipo_cuenta = "Tipo de cuenta",
                             no_cuenta = "No. Cuenta", rfc = "RFC", correo = "Correo", moneda = "Moneda",
                             tipo_relacion = "Tipo de relación")

prov_plantilla_datos <- function(df, catalogo = character(0)) {
  if (is.null(df) || !nrow(df)) df <- data.frame(nombre = character(0))
  col <- function(k) if (k %in% names(df)) as.character(df[[k]]) else rep(NA_character_, nrow(df))
  out <- data.frame(
    nombre = col("nombre"), alias = col("alias"),
    banco = vapply(col("banco"), function(v) { b <- prov_banco_clave(v, catalogo); if (b$estado == "ok") b$valor else v },
                   "", USE.NAMES = FALSE),
    tipo_cuenta = col("tipo_cuenta"), no_cuenta = prov_cuenta_existente(df), rfc = col("rfc"),
    correo = col("correo"), moneda = col("moneda"), tipo_relacion = col("tipo_relacion"), stringsAsFactors = FALSE)
  out[is.na(out)] <- ""
  stats::setNames(out, PROV_PLANTILLA_COLUMNAS[names(out)])
}

prov_plantilla_escribir <- function(df, archivo, catalogo = character(0), filas_extra = 500L) {
  datos <- prov_plantilla_datos(df, catalogo)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Proveedores")
  texto <- openxlsx::createStyle(numFmt = "@")
  cols_texto <- match(c("Banco", "No. Cuenta", "RFC"), names(datos))
  # Text format before the values (and on spare rows below), so typing there
  # stays text too.
  openxlsx::addStyle(wb, "Proveedores", texto, rows = 2:(nrow(datos) + 1L + filas_extra),
                     cols = cols_texto, gridExpand = TRUE)
  openxlsx::writeData(wb, "Proveedores", datos)
  openxlsx::addStyle(wb, "Proveedores", openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = seq_along(datos))
  openxlsx::setColWidths(wb, "Proveedores", cols = seq_along(datos), widths = "auto")
  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)
  invisible(archivo)
}
