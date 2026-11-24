# =============================================================================
# R/bancos_respuesta.R  --  T93: BajíoNet's "Transferencias por archivo" report
#
# What the bank says it did with each payment of a file loaded in BajíoNet:
# a CSV (UTF-8, comma, no quotes, CRLF) the user downloads after the batch
# is processed (reporte TXT; BajíoNet also offers it as Excel and PDF).
#   line 1  the batch's summary -- two shapes:
#           "por archivo"  ordenante, cliente, fecha, hora, ARCHIVO, fecha y
#                          hora de proceso, two counters, total, estatus
#           "filtrado"     ordenante, cliente, fecha, hora, "Por Archivo",
#                          filtros..., ARCHIVO, ...
#   line 2  the header: 26 named columns (# ... Mensaje)
#   line 3+ one payment per line, 30 fields: the 26 named and 4 unnamed
#           (Canceló, Devolución, Canal, Registro -- by position and by
#           BajíoNet's own PDF receipt)
# Read from real files of 2026-11-24 (structure only, never their data).
#
# leer_transferencias_bajionet() reads it into one row per payment, by
# column NAME (never by position, except the 4 unnamed ones), and reports
# every line it cannot read -- it never truncates or guesses.
# The matching (T93 paths A and B) is built on top of this.
# =============================================================================

# The report's header, normalised (lower case, no accents) -> our column.
.BTR_COLUMNAS <- c(
  "#"                                       = "n",
  "fecha"                                   = "fecha",
  "hora de operacion"                       = "hora",
  "cuenta origen"                           = "cuenta_origen",
  "nombre del ordenante"                    = "ordenante",
  "cuenta destino"                          = "cuenta_destino",
  "tipo cuenta destino"                     = "tipo_cuenta",
  "banco destino"                           = "banco_destino",
  "nombre del beneficiario"                 = "beneficiario",
  "alias del beneficiario"                  = "alias",
  "rfc"                                     = "rfc",
  "correo del beneficiario"                 = "correo",
  "transaccion"                             = "transaccion",
  "importe de la operacion"                 = "importe",
  "iva"                                     = "iva",
  "divisa"                                  = "divisa",
  "clasificacion de la operacion"           = "clasificacion",
  "concepto de pago"                        = "concepto",
  "referencia"                              = "referencia",
  "comision"                                = "comision",
  "iva comision"                            = "iva_comision",
  "no. de autorizacion / clave de rastreo"  = "autorizacion_rastreo",
  "capturo"                                 = "capturo",
  "autorizo"                                = "autorizo",
  "estatus de transferencia"                = "estatus",
  "mensaje"                                 = "mensaje"
)
# The 4 fields after the 26 named ones, which carry no header.
.BTR_SIN_ENCABEZADO <- c("cancelo", "devolucion", "canal", "registro")
.BTR_CAMPOS <- length(.BTR_COLUMNAS) + length(.BTR_SIN_ENCABEZADO)   # 30

.btr_normalizar <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  gsub("\\s+", " ", x)
}

# Bytes -> lines: UTF-8 (BOM dropped) when it is valid UTF-8, else Latin-1.
.btr_lineas <- function(archivo = NULL, lineas = NULL) {
  if (!is.null(lineas)) return(sub("\r$", "", enc2utf8(as.character(lineas))))
  raw <- readBin(archivo, "raw", file.info(archivo)$size)
  if (length(raw) >= 3L && identical(raw[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) raw <- raw[-(1:3)]
  txt <- rawToChar(raw)
  if (!validUTF8(txt)) txt <- iconv(txt, from = "latin1", to = "UTF-8")
  else Encoding(txt) <- "UTF-8"
  l <- strsplit(txt, "\r?\n")[[1]]
  l[nzchar(trimws(l))]
}

.btr_numero <- function(x) suppressWarnings(as.numeric(gsub("[$, ]", "", as.character(x))))

# leer_transferencias_bajionet(archivo) or (lineas = readLines(...)) ->
#   list(
#     lote      = list(variante = "por_archivo" | "filtrado" | NA, ordenante,
#                      cliente, fecha (Date), hora, archivo, estatus),
#     pagos     = data.frame, one row per readable payment (columns below),
#     problemas = data.frame(linea, codigo) -- every line not read and why:
#                 "encabezado" (the header is not the report's), "campos"
#                 (not 30 fields), "importe", "fecha"
#   )
# pagos: n, fecha, hora, cuenta_origen, cuenta_destino, tipo_cuenta,
# banco_destino, beneficiario, alias, rfc, transaccion, importe, iva, divisa,
# clasificacion, concepto, referencia, comision, iva_comision, autorizacion,
# clave_rastreo, estatus, mensaje, cancelo, devolucion, canal, registro,
# linea (its line in the file). Text as the bank wrote it, trimmed; the
# accounts are digits only. autorizacion / clave_rastreo: the two parts of
# "No. de Autorización / Clave de Rastreo" (SPEI "<13>/BB<13>"; an internal
# transfer has only the authorisation, so its clave_rastreo is NA).
leer_transferencias_bajionet <- function(archivo = NULL, lineas = NULL) {
  l <- .btr_lineas(archivo, lineas)
  vacia_p <- data.frame(linea = integer(), codigo = character(), stringsAsFactors = FALSE)
  out <- list(lote = list(variante = NA_character_), pagos = NULL, problemas = vacia_p)
  problema <- function(linea, codigo)
    out$problemas <<- rbind(out$problemas, data.frame(linea = linea, codigo = codigo, stringsAsFactors = FALSE))

  if (length(l) < 2L) { problema(1L, "encabezado"); out$pagos <- .btr_pagos_vacios(); return(out) }

  # ── Line 2: the header must be the report's, column by column ──
  enc <- .btr_normalizar(strsplit(l[2], ",", fixed = TRUE)[[1]])
  enc <- enc[seq_len(min(length(enc), length(.BTR_COLUMNAS)))]
  if (!identical(enc, names(.BTR_COLUMNAS))) {
    problema(2L, "encabezado"); out$pagos <- .btr_pagos_vacios(); return(out)
  }

  # ── Line 1: the batch ──
  r1 <- trimws(strsplit(l[1], ",", fixed = TRUE)[[1]])
  # Dates come as DDMMAAAA (checked on the real report: 24112026).
  fecha_txt <- function(x) as.Date(x, format = "%d%m%Y")
  out$lote <- if (length(r1) >= 11L && .btr_normalizar(r1[5]) == "por archivo") list(
      variante = "filtrado", ordenante = r1[1], cliente = r1[2], fecha = fecha_txt(r1[3]), hora = r1[4],
      archivo = r1[9], estatus = NA_character_)
    else if (length(r1) >= 11L) list(
      variante = "por_archivo", ordenante = r1[1], cliente = r1[2], fecha = fecha_txt(r1[3]), hora = r1[4],
      archivo = r1[5], estatus = r1[11])
    else list(variante = NA_character_, ordenante = NA_character_, cliente = NA_character_,
              fecha = as.Date(NA), hora = NA_character_, archivo = NA_character_, estatus = NA_character_)

  # ── Lines 3+: the payments, 30 fields each ──
  filas <- list()
  for (i in seq_along(l)[-(1:2)]) {
    f <- trimws(strsplit(l[i], ",", fixed = TRUE)[[1]])
    # strsplit drops trailing empty fields: "a,b,," has 4 fields, not 2.
    n_comas <- lengths(regmatches(l[i], gregexpr(",", l[i], fixed = TRUE)))
    f <- c(f, rep("", max(0L, n_comas + 1L - length(f))))
    if (length(f) != .BTR_CAMPOS) { problema(i, "campos"); next }
    names(f) <- c(unname(.BTR_COLUMNAS), .BTR_SIN_ENCABEZADO)
    imp <- .btr_numero(f[["importe"]])
    if (is.na(imp)) { problema(i, "importe"); next }
    fec <- fecha_txt(f[["fecha"]])
    if (is.na(fec)) { problema(i, "fecha"); next }
    ar <- strsplit(f[["autorizacion_rastreo"]], "/", fixed = TRUE)[[1]]
    filas[[length(filas) + 1L]] <- data.frame(
      n = suppressWarnings(as.integer(f[["n"]])), fecha = fec, hora = f[["hora"]],
      cuenta_origen = gsub("[^0-9]", "", f[["cuenta_origen"]]),
      cuenta_destino = gsub("[^0-9]", "", f[["cuenta_destino"]]),
      tipo_cuenta = f[["tipo_cuenta"]], banco_destino = f[["banco_destino"]],
      beneficiario = f[["beneficiario"]], alias = f[["alias"]], rfc = f[["rfc"]],
      transaccion = f[["transaccion"]], importe = imp, iva = .btr_numero(f[["iva"]]),
      divisa = f[["divisa"]], clasificacion = f[["clasificacion"]], concepto = f[["concepto"]],
      referencia = f[["referencia"]], comision = .btr_numero(f[["comision"]]),
      iva_comision = .btr_numero(f[["iva_comision"]]),
      autorizacion = trimws(ar[1] %||% NA_character_),
      clave_rastreo = if (length(ar) >= 2L && nzchar(trimws(ar[2]))) trimws(ar[2]) else NA_character_,
      estatus = f[["estatus"]], mensaje = f[["mensaje"]], cancelo = f[["cancelo"]],
      devolucion = f[["devolucion"]], canal = f[["canal"]], registro = f[["registro"]],
      linea = i, stringsAsFactors = FALSE)
  }
  out$pagos <- if (length(filas)) do.call(rbind, filas) else .btr_pagos_vacios()
  out
}

.btr_pagos_vacios <- function() data.frame(
  n = integer(), fecha = as.Date(character()), hora = character(), cuenta_origen = character(),
  cuenta_destino = character(), tipo_cuenta = character(), banco_destino = character(),
  beneficiario = character(), alias = character(), rfc = character(), transaccion = character(),
  importe = numeric(), iva = numeric(), divisa = character(), clasificacion = character(),
  concepto = character(), referencia = character(), comision = numeric(), iva_comision = numeric(),
  autorizacion = character(), clave_rastreo = character(), estatus = character(), mensaje = character(),
  cancelo = character(), devolucion = character(), canal = character(), registro = character(),
  linea = integer(), stringsAsFactors = FALSE)

# What the bank's word for each payment means for HopDesk (T93):
#   "aplicado"    Estatus "Aplicado", nothing cancelled or returned
#   "no_aplicado" anything else, or a Canceló / Devolución mark
# and, apart, where the row came from (Registro):
#   "correcto" from the file as loaded | "editado" changed in BajíoNet |
#   "agregado" typed in BajíoNet, not in the file | NA unknown.
# Only "aplicado" + "correcto" + a unique match may ever be confirmed alone.
btr_estado <- function(pagos) {
  e <- .btr_normalizar(pagos$estatus)
  limpio <- !nzchar(trimws(pagos$cancelo %||% "")) & !nzchar(trimws(pagos$devolucion %||% ""))
  ifelse(e == "aplicado" & limpio, "aplicado", "no_aplicado")
}
btr_registro <- function(pagos) {
  r <- .btr_normalizar(pagos$registro)
  ifelse(r %in% c("correcto", "editado", "agregado"), r, NA_character_)
}

# ── T93 · The beneficiaries of a generated file, with their Agenda row ──────
# parse_pago_beneficiarios() (R/pagos_generados_persistence.R) keeps its 5
# columns -- the payment-file viewer and its tests read exactly those. This
# adds the 3 that .log_pago_generado() writes since T93 (2b): agenda_item_id,
# documento, fecha_factura; NA for an entry written before (never guessed).
btr_beneficiarios <- function(json_str) {
  b <- parse_pago_beneficiarios(json_str)
  extra <- c("agenda_item_id", "documento", "fecha_factura")
  for (k in extra) b[[k]] <- rep(NA_character_, nrow(b))
  if (!nrow(b)) return(b)
  crudo <- tryCatch(jsonlite::fromJSON(json_str), error = function(e) NULL)
  if (is.data.frame(crudo) && nrow(crudo) == nrow(b))
    for (k in intersect(extra, names(crudo))) b[[k]] <- as.character(crudo[[k]])
  b
}

# ── T93 · Matching ───────────────────────────────────────────────────────────
# Accounts compared as numbers written with digits: no leading zeros (the
# report writes the origin with 11 digits, pagos_generados may keep 12; a
# BanBajío account is 12 in the report, zero-padded to 18 in the PPL).
.btr_cuenta <- function(x) { x <- gsub("[^0-9]", "", as.character(x)); x[is.na(x)] <- ""; sub("^0+", "", x) }
.btr_mismo_importe <- function(a, b) !is.na(a) & !is.na(b) & abs(as.numeric(a) - as.numeric(b)) < 0.005

# The report's Divisa ("MN", "USD", "Dólares"...) as an ISO code, through the
# one list of a Mexican bank's currency words (ppl_moneda_palabra); a code of
# the catalog stays as is; anything else NA (never guessed).
btr_moneda <- function(divisa) {
  d <- toupper(trimws(as.character(divisa)))
  m <- ppl_moneda_palabra(d)
  ifelse(is.na(m) & vapply(d, function(x) isTRUE(moneda_valida(x)), logical(1)), d, m)
}

# Which generated file (pagos_generados, tipo "pago_ppl") is this batch?
#   1  by name: the report's file name is the one HopDesk gave it;
#   2  else by content: same origin account, generated up to 7 days before
#      the batch's date, and every payment the report says came from the
#      file (Registro Correcto/Editado) has a beneficiary with the same
#      destination account and amount (an "Editado" may have changed its
#      amount: only its account is asked for).
# Exactly one candidate -> its row number in `generados`; else NA (path B).
btr_lote_generado <- function(lote, pagos, generados) {
  if (!is.data.frame(generados) || !nrow(generados)) return(NA_integer_)
  g <- which(generados$tipo %in% "pago_ppl")
  if (!length(g)) return(NA_integer_)
  arch <- tolower(trimws(as.character(lote$archivo %||% "")))
  quita_ext <- function(x) sub("[.]txt$", "", tolower(trimws(as.character(x))))
  if (nzchar(arch)) {
    por_nombre <- g[quita_ext(generados$archivo[g]) == quita_ext(arch)]
    if (length(por_nombre) == 1L) return(por_nombre)
    if (length(por_nombre) > 1L) g <- por_nombre       # same name twice: let the content decide
  }
  fecha <- lote$fecha %||% as.Date(NA)
  if (!is.na(fecha)) {
    dias <- as.numeric(fecha - as.Date(as.POSIXct(generados$ts[g])))
    g <- g[!is.na(dias) & dias >= 0 & dias <= 7]
  }
  if (nrow(pagos)) {
    origen <- unique(.btr_cuenta(pagos$cuenta_origen))
    g <- g[.btr_cuenta(generados$cuenta_origen[g]) %in% origen]
  }
  reg <- btr_registro(pagos)
  del_archivo <- pagos[reg %in% c("correcto", "editado"), , drop = FALSE]
  if (!nrow(del_archivo)) return(NA_integer_)
  cabe <- vapply(g, function(k) {
    b <- parse_pago_beneficiarios(generados$beneficiarios[k])
    if (!nrow(b)) return(FALSE)
    cb <- .btr_cuenta(b$clabe)
    all(vapply(seq_len(nrow(del_archivo)), function(i) {
      misma <- cb == .btr_cuenta(del_archivo$cuenta_destino[i])
      if (identical(btr_registro(del_archivo[i, , drop = FALSE]), "editado")) any(misma)
      else any(misma & .btr_mismo_importe(b$importe, del_archivo$importe[i]))
    }, logical(1)))
  }, logical(1))
  g <- g[cabe]
  if (length(g) == 1L) g else NA_integer_
}

# Path A: each payment of the report against the beneficiaries of ITS file
# (the JSON of pagos_generados, parse_pago_beneficiarios()). A beneficiary is
# used once. Key: destination account + amount + alias; the concepto breaks
# a tie. Returns, per payment of `pagos`:
#   beneficiario  its row in `beneficiarios` (NA when none or not unique)
#   estado        "confirmar"    Aplicado + Correcto + a unique match: the
#                                only one HopDesk may confirm alone
#                 "editado"      changed in BajíoNet: shown, never alone
#                 "agregado"     not in HopDesk's file: shown, never alone
#                 "no_aplicado"  rejected / cancelled / returned: stays in
#                                the Agenda with the bank's message
#                 "sin_casar"    Correcto + Aplicado but no unique match
btr_casar_a <- function(pagos, beneficiarios) {
  n <- nrow(pagos)
  out <- data.frame(beneficiario = rep(NA_integer_, n), estado = rep("sin_casar", n), stringsAsFactors = FALSE)
  if (!n) return(out)
  est <- btr_estado(pagos); reg <- btr_registro(pagos)
  b <- if (is.data.frame(beneficiarios)) beneficiarios else data.frame()
  usado <- rep(FALSE, nrow(b))
  cb <- .btr_cuenta(b$clabe %||% character()); ab <- toupper(trimws(as.character(b$alias %||% character())))
  for (i in seq_len(n)) {
    if (identical(reg[i], "agregado")) { out$estado[i] <- "agregado"; next }
    cand <- which(!usado & cb == .btr_cuenta(pagos$cuenta_destino[i]) &
                    ab == toupper(trimws(pagos$alias[i])) &
                    (identical(reg[i], "editado") | .btr_mismo_importe(b$importe, pagos$importe[i])))
    if (length(cand) > 1L) {
      desempate <- cand[trimws(as.character(b$concepto[cand])) == trimws(pagos$concepto[i])]
      if (length(desempate) == 1L) cand <- desempate
    }
    if (length(cand) == 1L) { out$beneficiario[i] <- cand; usado[cand] <- TRUE }
    out$estado[i] <- if (est[i] != "aplicado") "no_aplicado"
                     else if (identical(reg[i], "editado")) "editado"
                     else if (length(cand) == 1L) "confirmar"
                     else "sin_casar"
  }
  out
}

# Path B (a batch HopDesk did not generate): for each applied payment, the
# open Agenda rows of the same supplier BY ITS REGISTERED ACCOUNT, the exact
# amount and the same currency. `agenda`: pending AP rows as the Agenda
# annotates them (id, Moneda, Importe, clabe_dest). Never confirms anything:
#   estado  "sugerido"   one candidate (the user ticks it)
#           "varias"     more than one (the user picks one or none)
#           "sin_casar"  none -- the payment is shown for a manual decision
#           "no_aplicado" not applied: nothing pending of it in HopDesk
#   candidatos  list column: the Agenda ids of each payment's candidates.
btr_sugerir_b <- function(pagos, agenda) {
  n <- nrow(pagos)
  out <- data.frame(estado = rep("sin_casar", n), stringsAsFactors = FALSE)
  out$candidatos <- replicate(n, character(0), simplify = FALSE)
  if (!n) return(out)
  est <- btr_estado(pagos); mon <- btr_moneda(pagos$divisa)
  a <- if (is.data.frame(agenda)) agenda else data.frame()
  ca <- .btr_cuenta(a$clabe_dest %||% character())
  for (i in seq_len(n)) {
    if (est[i] != "aplicado") { out$estado[i] <- "no_aplicado"; next }
    k <- which(nzchar(ca) & ca == .btr_cuenta(pagos$cuenta_destino[i]) &
                 .btr_mismo_importe(a$Importe, pagos$importe[i]) &
                 !is.na(mon[i]) & toupper(as.character(a$Moneda)) == mon[i])
    out$candidatos[[i]] <- as.character(a$id[k])
    out$estado[i] <- if (length(k) == 1L) "sugerido" else if (length(k) > 1L) "varias" else "sin_casar"
  }
  out
}
