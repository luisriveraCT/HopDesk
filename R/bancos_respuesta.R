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

# ── T93 · respuestas_banco.rds: what the bank said, kept forever ────────────
# One row per payment of every report uploaded. Only ever appended (like
# pagos_generados): a report uploaded twice adds nothing -- the key is the
# bank's authorisation number (`llave`; for a payment without one, the
# batch's file + the line). Never a full account: the destination is kept
# masked (****1234), as the screen and the log show it.
.schema_respuestas_banco <- function() tibble::tibble(
  id               = character(),   # UUID
  llave            = character(),   # "aut:<autorización>" | "lin:<archivo del lote>#<línea>"
  ts               = as.POSIXct(character()),
  user             = character(),
  archivo_reporte  = character(),   # the uploaded file's name
  lote_archivo     = character(),   # the batch's file name, as the report says
  camino           = character(),   # "A" (HopDesk's file) | "B" (another one)
  pago_generado_id = character(),   # pagos_generados$id of the batch (A), else NA
  linea            = integer(),
  fecha_operacion  = as.Date(character()),
  autorizacion     = character(),
  clave_rastreo    = character(),
  referencia       = character(),
  cuenta_destino   = character(),   # masked: ****1234
  alias            = character(),
  importe          = numeric(),
  moneda           = character(),   # ISO (btr_moneda)
  comision         = numeric(),
  iva_comision     = numeric(),
  estatus          = character(),   # as the bank wrote it
  mensaje          = character(),
  registro         = character(),   # correcto / editado / agregado
  estado           = character(),   # confirmar / editado / agregado / no_aplicado / sin_casar / sugerido / varias
  agenda_item_id   = character(),   # the Agenda row it confirmed, if any
  confirmacion_id  = character()    # bancos_confirmados$confirmacion_id, if confirmed
)

btr_enmascarar <- function(cuenta) {
  d <- gsub("[^0-9]", "", as.character(cuenta)); d[is.na(d)] <- ""
  ifelse(nchar(d) >= 4L, paste0("****", substr(d, nchar(d) - 3L, nchar(d))), ifelse(nzchar(d), "****", ""))
}

btr_llave <- function(autorizacion, lote_archivo, linea) {
  a <- trimws(as.character(autorizacion)); a[is.na(a)] <- ""
  ifelse(nzchar(a), paste0("aut:", a), paste0("lin:", as.character(lote_archivo %||% ""), "#", linea))
}

# The client's table; a failed read stops (a failed read is never an empty
# table -- A2). client_id always explicit.
leer_respuestas_banco <- function(client_id) {
  if (is.null(client_id) || !nzchar(client_id)) stop("leer_respuestas_banco: sin client_id", call. = FALSE)
  .normalize(.s3_read_with(S3_KEYS$respuestas_banco, client_id = client_id), .schema_respuestas_banco)
}

# Appends `filas` (columns of .schema_respuestas_banco; id, llave and ts are
# filled when missing) on S3's table as it is now (modificar_tabla: strict
# read, the write net), skipping every llave already there or repeated in
# `filas`. evento: the T72 event, logged by the write layer once the save
# worked. Returns list(tabla, nuevas = the rows added, repetidas = how many
# were already there).
modificar_respuestas_banco <- function(client_id, filas, evento = NULL) {
  if (is.null(client_id) || !nzchar(client_id)) stop("modificar_respuestas_banco: sin client_id", call. = FALSE)
  f <- as.data.frame(filas, stringsAsFactors = FALSE)
  n <- nrow(f)
  if (!"llave" %in% names(f) || anyNA(f$llave)) f$llave <- btr_llave(f$autorizacion, f$lote_archivo, f$linea)
  if (!"id" %in% names(f)) f$id <- vapply(seq_len(n), function(i) uuid::UUIDgenerate(), character(1))
  if (!"ts" %in% names(f)) f$ts <- rep(Sys.time(), n)
  f <- f[!duplicated(f$llave), , drop = FALSE]
  nuevas <- NULL
  cambio <- function(actual) {
    nuevas <<- f[!f$llave %in% actual$llave, , drop = FALSE]
    if (!nrow(nuevas)) return(actual)
    dplyr::bind_rows(actual, .normalize(nuevas, .schema_respuestas_banco))
  }
  tabla <- modificar_tabla(S3_KEYS$respuestas_banco, client_id, .schema_respuestas_banco, cambio, evento = evento)
  list(tabla = tabla, nuevas = nuevas %||% f[0, , drop = FALSE], repetidas = nrow(f) - nrow(nuevas %||% f[0, , drop = FALSE]))
}

# ── T93 · The screen's data: from the report to what may be confirmed ───────
# The registered account of each Agenda row's supplier, for path B. EXACT
# matches only -- the Agenda's own fuzzy matching lives in its server, and a
# suggestion the user confirms must never come from a guess: the Parte's
# saved link (parte_alias_map), else the supplier's code, else its exact
# name or alias. Ambiguous or none: NA (that row is never suggested).
btr_agenda_cuentas <- function(agenda, proveedores, alias_map = NULL) {
  n <- nrow(agenda)
  out <- rep(NA_character_, n)
  if (!n || !is.data.frame(proveedores) || !nrow(proveedores)) return(out)
  up <- function(x) toupper(trimws(as.character(x)))
  cuenta <- dplyr::coalesce(as.character(proveedores$clabe), as.character(proveedores[["no_cuenta"]] %||% NA_character_))
  unica <- function(i) { i <- unique(i[!is.na(cuenta[i]) & nzchar(cuenta[i])]); if (length(i) == 1L) cuenta[i] else NA_character_ }
  for (r in seq_len(n)) {
    parte <- up(agenda$Parte[r]); emp <- as.character(agenda$Empresa[r])
    al <- if (is.data.frame(alias_map) && nrow(alias_map))
      alias_map$alias[up(alias_map$Parte) == parte & (is.na(alias_map$Empresa) | alias_map$Empresa %in% c("", emp))] else character()
    k <- if (length(al)) which(up(proveedores$alias) %in% up(al))
         else if (nzchar(up(agenda[["Codigo"]][r] %||% ""))) which(up(proveedores$codigo) == up(agenda$Codigo[r]))
         else integer()
    if (!length(k)) k <- which(up(proveedores$nombre) == parte | up(proveedores$alias) == parte)
    out[r] <- unica(k)
  }
  out
}

# Everything the preview shows and the server needs, from one report:
#   reporte    leer_transferencias_bajionet()'s result
#   generados  the client's pagos_generados (read_pagos_generados())
#   agenda     the pending AP rows of the Agenda (id, Empresa, Moneda,
#              Documento, Parte, Codigo, Importe)
#   proveedores, alias_map  for path B (btr_agenda_cuentas)
#   registradas  llaves already in respuestas_banco (an upload twice)
# Returns list(camino = "A" | "B", generado = pagos_generados row or NULL,
# filas = one row per payment): linea, fecha, alias, cuenta (masked),
# importe, moneda, estatus, mensaje, registro, autorizacion, clave_rastreo,
# referencia, comision, iva_comision, llave, estado, agenda_item_id,
# empresa, elegible (may be confirmed), marcado (ticked by default: only
# path A's "confirmar"). Estados besides btr_casar_a()'s / btr_sugerir_b()'s:
#   "ya_registrado"  this payment's report row was uploaded before
#   "ya_confirmado"  its Agenda row is no longer pending
#   "sin_agenda"     A's match, but the file predates 2b (no Agenda row in
#                    it): never confirmed alone, the user does it in the Agenda
btr_preparar <- function(reporte, generados, agenda, proveedores = NULL, alias_map = NULL, registradas = character()) {
  p <- reporte$pagos
  n <- nrow(p)
  gi <- btr_lote_generado(reporte$lote, p, generados)
  camino <- if (is.na(gi)) "B" else "A"
  filas <- data.frame(
    linea = as.integer(p$linea), fecha = p$fecha, alias = p$alias, cuenta = btr_enmascarar(p$cuenta_destino),
    importe = p$importe, moneda = btr_moneda(p$divisa), estatus = p$estatus, mensaje = p$mensaje,
    registro = btr_registro(p), autorizacion = p$autorizacion, clave_rastreo = p$clave_rastreo,
    referencia = p$referencia, comision = p$comision, iva_comision = p$iva_comision,
    llave = btr_llave(p$autorizacion, reporte$lote$archivo, p$linea),
    estado = rep("sin_casar", n), agenda_item_id = rep(NA_character_, n), empresa = rep(NA_character_, n),
    stringsAsFactors = FALSE)
  filas$candidatos <- replicate(n, character(0), simplify = FALSE)
  pend <- if (is.data.frame(agenda)) agenda else data.frame(id = character())
  if (camino == "A") {
    ben <- btr_beneficiarios(generados$beneficiarios[gi])
    ca  <- btr_casar_a(p, ben)
    filas$estado <- ca$estado
    filas$agenda_item_id <- ben$agenda_item_id[ca$beneficiario]
    sin_ag <- filas$estado == "confirmar" & is.na(filas$agenda_item_id)
    filas$estado[sin_ag] <- "sin_agenda"
  } else {
    if (nrow(pend)) pend$clabe_dest <- btr_agenda_cuentas(pend, proveedores, alias_map)
    sb <- btr_sugerir_b(p, pend)
    filas$estado <- sb$estado
    filas$candidatos <- sb$candidatos
    uno <- filas$estado == "sugerido"
    filas$agenda_item_id[uno] <- vapply(sb$candidatos[uno], `[`, "", 1L)
  }
  # Its Agenda row must still be pending; else it was confirmed meanwhile.
  con_fila <- !is.na(filas$agenda_item_id)
  ya <- con_fila & !filas$agenda_item_id %in% as.character(pend$id)
  filas$estado[ya & filas$estado %in% c("confirmar", "sugerido")] <- "ya_confirmado"
  filas$empresa <- as.character(pend$Empresa)[match(filas$agenda_item_id, as.character(pend$id))]
  # The currency: the report's (btr_moneda) must be the Agenda row's. A
  # Divisa it does not recognise (NA) or another currency than the row's is
  # never confirmed -- "moneda_distinta", locked like an Editado. When they
  # match, the row's own Moneda is what the confirmation keeps.
  filas$moneda_agenda <- as.character(pend$Moneda)[match(filas$agenda_item_id, as.character(pend$id))]
  dist <- filas$estado %in% c("confirmar", "sugerido") &
    (is.na(filas$moneda) | is.na(filas$moneda_agenda) | toupper(filas$moneda) != toupper(filas$moneda_agenda))
  filas$estado[dist] <- "moneda_distinta"
  filas$estado[filas$llave %in% registradas] <- "ya_registrado"
  filas$elegible <- filas$estado %in% c("confirmar", "sugerido")
  filas$marcado  <- filas$estado == "confirmar"
  list(camino = camino, generado = if (camino == "A") generados[gi, , drop = FALSE] else NULL, filas = filas)
}

# What is confirmed, decided on the server from the preview it built itself
# (never from the browser's copy): every elegible row that is ticked -- A's
# "confirmar" come ticked, B's "sugerido" only if the user ticked them.
# `marcados`: the lines the user left ticked (anything else is ignored).
btr_a_confirmar <- function(prep, marcados) {
  f <- prep$filas
  f[f$elegible & f$linea %in% suppressWarnings(as.integer(marcados)) & !is.na(f$agenda_item_id), , drop = FALSE]
}

# ── T93 · The screen (in "Archivos de pago") ────────────────────────────────
BTR_ESTADOS <- c("confirmar", "editado", "agregado", "no_aplicado", "sin_casar", "sugerido", "varias",
                 "ya_registrado", "ya_confirmado", "sin_agenda", "moneda_distinta")
.btr_tono <- c(confirmar = "exito", sugerido = "info", varias = "aviso", editado = "aviso", agregado = "aviso",
               no_aplicado = "peligro", sin_casar = "peligro", ya_registrado = "neutro", ya_confirmado = "neutro",
               sin_agenda = "aviso", moneda_distinta = "aviso")
.btr_columnas <- function() list(
  col_opcion("decision", "tablas.archivos_pago.btr.col_decision", opciones = c(
    confirmar = "tablas.archivos_pago.btr.op_confirmar", omitir = "tablas.archivos_pago.btr.op_omitir")),
  col_insignia("estado", "tablas.archivos_pago.btr.col_estado", mapa = stats::setNames(
    lapply(BTR_ESTADOS, function(e) list(texto = paste0("tablas.archivos_pago.btr.estado.", e), tono = .btr_tono[[e]])),
    BTR_ESTADOS)),
  col_fecha("fecha", "tablas.archivos_pago.btr.col_fecha"),
  col_texto("alias", "tablas.archivos_pago.btr.col_alias"),
  col_texto("cuenta", "tablas.archivos_pago.btr.col_cuenta"),
  col_dinero("importe", "tablas.archivos_pago.btr.col_importe", moneda = "moneda"),
  col_texto("mensaje", "tablas.archivos_pago.btr.col_mensaje", opcional = TRUE))

# The preview's rows: one per payment, by its line ("L<linea>"); what may
# not be confirmed is locked (no choice); A's "confirmar" comes ticked.
.btr_vista <- function(prep) {
  vacia <- data.frame(id = character(), decision = character(), estado = character(), fecha = as.Date(character()),
                      alias = character(), cuenta = character(), importe = numeric(), moneda = character(),
                      mensaje = character(), .bloqueada = logical(), stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(prep) || !nrow(prep$filas)) return(vacia)
  f <- prep$filas
  data.frame(id = paste0("L", f$linea), decision = ifelse(f$marcado, "confirmar", "omitir"), estado = f$estado,
             fecha = f$fecha, alias = f$alias, cuenta = f$cuenta, importe = f$importe, moneda = f$moneda,
             mensaje = ifelse(is.na(f$mensaje), "", f$mensaje), .bloqueada = !f$elegible,
             stringsAsFactors = FALSE, check.names = FALSE)
}

# Confirms `conf` (btr_a_confirmar()'s rows) through the Agenda's own
# Confirmar (.ph_confirmar_ap), grouped by company, currency and date, then
# records every payment of the report in respuestas_banco with what was
# done. Server-side only. Returns list(confirmados, registrados, error).
btr_confirmar <- function(shared, prep, conf, ahora = Sys.time()) {
  cid  <- shared$effective_client_id()
  user <- tryCatch(shared$current_user(), error = function(e) NULL) %||% "unknown"
  home <- tryCatch(shared$home_client_id(), error = function(e) NULL)
  fallido <- function(accion, de, emp, e, aviso = tr("comun.error_guardar")) {
    message("[archivos_pago] ", accion, " failed: ", conditionMessage(e))
    showNotification(aviso, type = "warning")
    tryCatch(log_action(user, "archivos_pago", paste0(accion, "_fallido"), conditionMessage(e), target_id = emp,
                        metadata = list(accion = de, empresa = emp), client_id = cid, viewer_home_client_id = home),
             error = function(e2) NULL)
    NULL
  }
  f <- prep$filas
  f$respuesta_id <- vapply(seq_len(nrow(f)), function(i) uuid::UUIDgenerate(), character(1))
  conf$respuesta_id <- f$respuesta_id[match(conf$linea, f$linea)]
  confirmados <- character()
  if (nrow(conf)) {
    ph <- tryCatch(safe_load_pagar_hoy(username = user, client_id = cid), error = function(e) NULL)
    if (is.null(ph)) return(list(confirmados = character(), registrados = 0L, error = "agenda"))
    # By the Agenda row's currency (btr_preparar() already left out any row
    # whose report currency differs); the row read now must still have it.
    grupos <- split(seq_len(nrow(conf)), paste(conf$empresa, conf$moneda_agenda, conf$fecha, sep = "\x1f"))
    for (g in grupos) {
      cg   <- conf[g, , drop = FALSE]
      rows <- ph[as.character(ph$id) %in% cg$agenda_item_id & ph$ledger %in% "AP" &
                   toupper(as.character(ph$Moneda)) %in% toupper(cg$moneda_agenda[1]), , drop = FALSE]
      if (!nrow(rows)) next
      # The confirmation's writes are parts of THIS action (the upload, whose
      # event "subir_reporte_banco" is written below); .ph_confirmar_ap()
      # still logs its own "confirmar_pago" for the client, as from the Agenda.
      .parte <- evento_parte_de("subir_reporte_banco", "archivos_pago", user, home)
      .ph_confirmar_ap(shared, cg$empresa[1], cg$moneda_agenda[1], rows, NA_character_, as.Date(cg$fecha[1]), .parte,
                       .ph_fallido = fallido, .link_counter = function(x) if (missing(x)) 0L else invisible(NULL),
                       extra = cg[, c("agenda_item_id", "clave_rastreo", "autorizacion", "respuesta_id")])
      ya <- tryCatch(shared$bancos_confirmados(), error = function(e) NULL)
      if (is.data.frame(ya) && "respuesta_id" %in% names(ya))
        confirmados <- c(confirmados, intersect(cg$respuesta_id, as.character(ya$respuesta_id)))
    }
  }
  ya <- tryCatch(shared$bancos_confirmados(), error = function(e) NULL)
  conf_id <- if (is.data.frame(ya) && "respuesta_id" %in% names(ya))
    as.character(ya$confirmacion_id)[match(f$respuesta_id, as.character(ya$respuesta_id))] else rep(NA_character_, nrow(f))
  filas <- data.frame(
    id = f$respuesta_id, llave = f$llave, ts = rep(ahora, nrow(f)), user = rep(user, nrow(f)),
    archivo_reporte = rep(prep$archivo %||% NA_character_, nrow(f)),
    lote_archivo = rep(prep$lote_archivo %||% NA_character_, nrow(f)), camino = rep(prep$camino, nrow(f)),
    pago_generado_id = rep(if (is.null(prep$generado)) NA_character_ else as.character(prep$generado$id), nrow(f)),
    linea = f$linea, fecha_operacion = f$fecha, autorizacion = f$autorizacion, clave_rastreo = f$clave_rastreo,
    referencia = f$referencia, cuenta_destino = f$cuenta, alias = f$alias, importe = f$importe, moneda = f$moneda,
    comision = f$comision, iva_comision = f$iva_comision, estatus = f$estatus, mensaje = f$mensaje,
    registro = f$registro, estado = f$estado,
    agenda_item_id = ifelse(f$respuesta_id %in% confirmados, f$agenda_item_id, NA_character_),
    confirmacion_id = conf_id, stringsAsFactors = FALSE)
  filas <- filas[f$estado != "ya_registrado", , drop = FALSE]
  cuenta <- table(factor(filas$estado, levels = BTR_ESTADOS))
  ev <- list(usuario = user, modulo = "archivos_pago", accion = "subir_reporte_banco",
             descripcion = tr("tablas.archivos_pago.btr.log", archivo = prep$archivo %||% "",
                              n = as.character(nrow(filas)), confirmados = as.character(length(confirmados))),
             metadata = list(archivo = prep$archivo, lote = prep$lote_archivo, camino = prep$camino,
                             confirmados = length(confirmados), estados = as.list(cuenta[cuenta > 0])),
             target_id = prep$archivo, home = home)
  reg <- tryCatch(if (nrow(filas)) modificar_respuestas_banco(cid, filas, evento = ev) else list(nuevas = filas),
                  error = function(e) { message("[archivos_pago] respuestas_banco: ", conditionMessage(e)); NULL })
  list(confirmados = confirmados, registrados = if (is.null(reg)) NA_integer_ else nrow(reg$nuevas),
       error = if (is.null(reg)) "registro" else NULL)
}

# The upload's server, called once from pagosAuditViewerServer (its ns and
# shared). Permission can_confirm_payments on the server at every step.
.btr_servidor <- function(input, output, session, shared, al_terminar = function() NULL) {
  ns <- session$ns
  prep_rv <- reactiveVal(NULL)
  t_btr <- hd_table_server("btr_t", tabla = "respuesta_banco",
                           datos = reactive(.btr_vista(prep_rv())), columnas = reactive(.btr_columnas()))
  observeEvent(input$btr_subir, {
    if (!puede_o_avisa(shared, "can_confirm_payments")) return()
    showModal(modalDialog(
      title = tagList(icon("file-import"), " ", tr("tablas.archivos_pago.btr.titulo")), easyClose = TRUE,
      tags$p(class = "small text-muted", tr("tablas.archivos_pago.btr.ayuda")),
      fileInput(ns("btr_archivo"), tr("tablas.archivos_pago.btr.elegir"), accept = c(".txt", ".csv")),
      footer = modalButton(tr("comun.cancelar"))))
  }, ignoreInit = TRUE)
  observeEvent(input$btr_archivo, {
    if (!puede_o_avisa(shared, "can_confirm_payments")) return()
    a <- input$btr_archivo
    if (is.null(a) || !nrow(a)) return()
    cid <- shared$effective_client_id()
    rep <- tryCatch(leer_transferencias_bajionet(a$datapath[1]), error = function(e) NULL)
    if (is.null(rep) || !nrow(rep$pagos)) {
      showNotification(tr("tablas.archivos_pago.btr.no_legible"), type = "error", duration = 8); return()
    }
    gen <- tryCatch(read_pagos_generados(client_id = cid), error = function(e) NULL)
    reg <- tryCatch(leer_respuestas_banco(cid)$llave, error = function(e) NULL)
    ag  <- tryCatch(safe_load_pagar_hoy(username = shared$current_user(), client_id = cid), error = function(e) NULL)
    if (is.null(gen) || is.null(reg) || is.null(ag)) {
      showNotification(tr("comun.reintentar"), type = "error", duration = 8); return()
    }
    ag <- ag[ag$ledger %in% "AP", , drop = FALSE]
    prov <- tryCatch(shared$proveedores_db(), error = function(e) NULL)
    amap <- tryCatch(shared$parte_alias_map_db(), error = function(e) NULL)
    prep <- btr_preparar(rep, gen, ag, prov, amap, reg)
    prep$archivo <- as.character(a$name[1]); prep$lote_archivo <- rep$lote$archivo %||% NA_character_
    prep$problemas <- nrow(rep$problemas)
    prep_rv(prep)
    n_est <- table(factor(prep$filas$estado, levels = BTR_ESTADOS))
    showModal(modalDialog(
      title = tagList(icon("building-columns"), " ", tr("tablas.archivos_pago.btr.titulo")), size = "l", easyClose = FALSE,
      div(class = "small mb-2",
          tags$b(tr(paste0("tablas.archivos_pago.btr.camino_", tolower(prep$camino)), archivo = prep$lote_archivo %||% "")),
          if (prep$problemas > 0) div(class = "text-danger", tr("tablas.archivos_pago.btr.problemas", n = as.character(prep$problemas)))),
      div(class = "d-flex flex-wrap gap-1 mb-2",
          lapply(names(n_est)[n_est > 0], function(e)
            tags$span(class = "badge bg-secondary", paste0(tr(paste0("tablas.archivos_pago.btr.estado.", e)), ": ", n_est[[e]])))),
      hd_table_ui(ns("btr_t")),
      tags$p(class = "small text-muted mt-2 mb-0", icon("circle-info"), " ", tr("tablas.archivos_pago.btr.nota")),
      footer = tagList(modalButton(tr("comun.cancelar")),
                       actionButton(ns("btr_confirmar"), tr("tablas.archivos_pago.btr.boton"), class = "btn btn-success"))))
  }, ignoreInit = TRUE)
  observeEvent(input$btr_confirmar, {
    if (!puede_o_avisa(shared, "can_confirm_payments")) return()
    prep <- prep_rv()
    if (is.null(prep)) return()
    op <- t_btr$opciones()
    marcados <- sub("^L", "", op$id[op$opcion %in% "confirmar"])
    conf <- btr_a_confirmar(prep, marcados)
    r <- btr_confirmar(shared, prep, conf)
    prep_rv(NULL)
    removeModal()
    if (identical(r$error, "agenda")) { showNotification(tr("comun.reintentar"), type = "error", duration = 8); return() }
    showNotification(tr("tablas.archivos_pago.btr.hecho", confirmados = as.character(length(r$confirmados)),
                        n = as.character(nrow(prep$filas))),
                     type = if (is.null(r$error)) "message" else "warning", duration = 8)
    if (identical(r$error, "registro")) showNotification(tr("tablas.archivos_pago.btr.error_registro"), type = "error", duration = 12)
    al_terminar()
  }, ignoreInit = TRUE)
  invisible(list(prep = prep_rv, tabla = t_btr))
}
