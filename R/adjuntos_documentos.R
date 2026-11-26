# =============================================================================
# R/adjuntos_documentos.R -- T106: documents linked to any entity
#
# One table per client, <cliente>/adjuntos_documentos.rds (grow-only: S3's net
# refuses a shrink -- .S3_SOLO_CRECEN), one row per document: what it is
# (clase archivo | tabla, ext, mime, size, sha256 of the stored bytes), what
# it belongs to (entidad + entidad_id, R/adjuntos_registro.R), its kind
# (tipo_doc, ADJ_TIPOS_DOC), alternate keys in `refs` (JSON; e.g. a CFDI's
# fiscal UUID, phase 2), where its bytes are (almacen + llave,
# R/adjuntos_almacen.R) and who/when, trash included. Trashing is a mark;
# the stored object stays.
#
# What is accepted is decided by the BYTES, never the name or the browser:
#   PNG/JPG/WEBP/PDF/XLSX  Requisiciones' own checks (adjunto_preparar(),
#                          R/adjuntos_s3.R): signature, macro-free workbook,
#                          images decoded and re-encoded (only the copy is
#                          stored), pixel cap before decoding;
#   CSV                    valid UTF-8 text, no NUL or control bytes, read as
#                          a table and stored in the canonical CSV (below);
#   XML                    valid UTF-8 text that starts with "<", with NO
#                          <!DOCTYPE and NO <!ENTITY at all (no external
#                          entities can ever be resolved: XXE), no SVG/HTML
#                          element (script), 5 MB at most;
#   anything else          refused (exe, js, html, svg, zip, xlsm, docm...).
# Pasted Excel cells (Requisiciones' tabla_pegada_preparar(), its caps and
# trim notice) and uploaded CSV are the same thing: a table, stored as
# canonical CSV -- RFC 4180, UTF-8 without BOM, comma, double quotes only when
# a cell needs them, CRLF. Cells are stored as given; neutralizar_celdas()
# applies only when copying or exporting (OWASP CSV injection), as today.
#
# Every write passes its event (T72): adjuntar_documento,
# pegar_tabla_documento, papelera_documento, restaurar_documento -- client
# actions, C in T87's catalog. Permissions are the callers' (the screen asks
# adj_puede_editar() / adj_puede_ver(), R/adjuntos_registro.R).
# =============================================================================

ADJDOC_KEY      <- "adjuntos_documentos.rds"

# The stored bytes' SHA-256, as plain lowercase hex.
adjdoc_sha256 <- function(bytes) paste(sprintf("%02x", as.integer(openssl::sha256(bytes))), collapse = "")
ADJDOC_MAX_XML  <- 5 * 1024^2

.adjdoc_t0 <- function() as.POSIXct(character(), tz = "UTC")
.schema_adjuntos_documentos <- function() tibble::tibble(
  id           = character(),
  entidad      = character(),
  entidad_id   = character(),
  tipo_doc     = character(),
  clase        = character(),   # "archivo" | "tabla"
  ext          = character(),
  mime         = character(),
  tamano       = numeric(),
  sha256       = character(),
  nombre       = character(),   # original file name, shown only (escaped)
  titulo       = character(),
  nota         = character(),
  ancho        = integer(),
  alto         = integer(),
  filas        = integer(),
  columnas     = integer(),
  encabezado   = logical(),
  refs         = character(),   # JSON of alternate keys, "{}" when none
  ref_uuid     = character(),   # T106 2a: a CFDI's fiscal UUID (upper case), NA otherwise -- searched without opening refs
  almacen      = character(),
  llave        = character(),
  comprimido   = character(),   # "no" today (R/adjuntos_almacen.R)
  creado_por   = character(),
  creado       = .adjdoc_t0(),
  papelera_por = character(),
  papelera_en  = .adjdoc_t0()
)

# An attachments error the screen shows as is (already translated).
.adjdoc_error <- function(clave, ...) {
  stop(structure(class = c("adjdoc_error", "error", "condition"),
                 list(message = tr(clave, ...), call = NULL, clave = clave)))
}

.adjdoc_cid <- function(client_id) {
  cid <- tolower(trimws(client_id %||% ""))
  if (!nzchar(cid)) stop("[adjuntos] client_id es obligatorio", call. = FALSE)
  cid
}

# ── What a file is ───────────────────────────────────────────────────────────

# Text that may be read as CSV or XML: valid UTF-8 (a BOM is dropped), no NUL
# and no control bytes but tab, CR and LF. NULL when it is not such text.
.adjdoc_texto <- function(b) {
  if (length(b) >= 3L && identical(as.integer(b[1:3]), c(0xEFL, 0xBBL, 0xBFL))) b <- b[-(1:3)]
  if (!length(b) || any(b == as.raw(0))) return(NULL)
  ctrl <- as.integer(b) < 32L & !as.integer(b) %in% c(9L, 10L, 13L)
  if (any(ctrl) || any(as.integer(b) == 127L)) return(NULL)
  txt <- rawToChar(b)
  if (!validUTF8(txt)) return(NULL)
  Encoding(txt) <- "UTF-8"
  txt
}

# "xml" | "csv" | NA for a text file. XML: starts with "<" and carries no
# DTD and no entity declaration at all (so nothing external can be resolved).
.adjdoc_tipo_texto <- function(txt) {
  inicio <- sub("^[[:space:]]+", "", substr(txt, 1L, 2000L))
  if (startsWith(inicio, "<")) {
    if (grepl("<!(DOCTYPE|ENTITY)", txt, ignore.case = TRUE)) return(NA_character_)
    # SVG can carry script; HTML is a page, not a document: never stored.
    if (grepl("<(svg|html|head|body|script|iframe|object|embed)([[:space:]>/]|$)", txt, ignore.case = TRUE)) return(NA_character_)
    return("xml")
  }
  "csv"
}

# A CSV text into a character matrix (every cell as text; nothing converted).
.adjdoc_csv_leer <- function(txt) {
  txt <- sub("(\r?\n)+$", "", gsub("\r\n?", "\n", txt))
  if (!nzchar(gsub("[[:space:],]", "", txt))) .adjdoc_error("comun.adjuntos.tabla.error_vacia")
  df <- tryCatch(utils::read.csv(text = txt, header = FALSE, colClasses = "character", quote = "\"",
                                 na.strings = character(0), check.names = FALSE, strip.white = FALSE,
                                 blank.lines.skip = FALSE, fill = TRUE, encoding = "UTF-8"),
                 error = function(e) .adjdoc_error("comun.adjuntos.tabla.error_csv"))
  m <- as.matrix(df); dimnames(m) <- NULL
  m[is.na(m)] <- ""
  Encoding(m) <- "UTF-8"
  m
}

# The canonical CSV of a character matrix: RFC 4180, UTF-8 without BOM,
# comma, a cell quoted only when it holds a comma, a quote, CR or LF (inner
# quotes doubled), CRLF after every row. Returns raw bytes.
adjdoc_csv_escribir <- function(celdas) {
  m <- celdas; m[is.na(m)] <- ""
  esc <- function(x) ifelse(grepl("[\",\r\n]", x), paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\""), x)
  lineas <- apply(matrix(esc(enc2utf8(m)), nrow = nrow(m)), 1L, paste, collapse = ",")
  charToRaw(enc2utf8(paste0(paste(lineas, collapse = "\r\n"), "\r\n")))
}

# The same trim (caps and notice) as a pasted table (tabla_pegada_preparar),
# for a CSV that came as a file.
.adjdoc_tabla_desde_matriz <- function(m) {
  nf <- nrow(m); nc <- ncol(m)
  fr <- min(nf, TABLA_MAX_FILAS); cr <- min(nc, TABLA_MAX_COLS)
  celdas <- m[seq_len(fr), seq_len(cr), drop = FALSE]
  recortada <- nf > fr || nc > cr
  aviso <- if (!recortada) NA_character_
    else if (nf > fr && nc > cr) tr("requisiciones.tabla.recorte_ambos", filas = nf, guardadas = fr, columnas = nc, cguardadas = cr)
    else if (nf > fr) tr("requisiciones.tabla.recorte_filas", filas = nf, guardadas = fr)
    else tr("requisiciones.tabla.recorte_columnas", columnas = nc, cguardadas = cr)
  primera <- celdas[1, ]
  encabezado <- fr > 1L && all(nzchar(trimws(primera))) && !any(.parece_numero(primera)) && !anyDuplicated(primera)
  cuerpo <- if (encabezado) celdas[-1, , drop = FALSE] else celdas
  numericas <- vapply(seq_len(cr), function(j) { v <- cuerpo[, j]; v <- v[nzchar(trimws(v))]
    length(v) > 0L && all(.parece_numero(v)) }, logical(1))
  list(celdas = celdas, filas = fr, columnas = cr, filas_pegadas = nf, columnas_pegadas = nc,
       recortada = recortada, aviso = aviso, encabezado_sugerido = encabezado, numericas = numericas)
}

# Requisiciones' validation errors, in the attachments' own words (theirs
# name a requisition and its file types).
.ADJDOC_DE_REQ <- c("requisiciones.evidencia.error_leer" = "comun.adjuntos.error_leer",
                    "requisiciones.evidencia.error_tamano" = "comun.adjuntos.error_tamano",
                    "requisiciones.evidencia.error_tipo" = "comun.adjuntos.error_tipo",
                    "requisiciones.evidencia.error_imagen" = "comun.adjuntos.error_imagen",
                    "requisiciones.tabla.error_vacia" = "comun.adjuntos.tabla.error_vacia",
                    "requisiciones.tabla.error_grande" = "comun.adjuntos.tabla.error_grande")
.adjdoc_traducir <- function(expr, mb = ADJ_MAX_ARCHIVO / 1024^2) {
  tryCatch(expr, req_error = function(e) {
    k <- unname(.ADJDOC_DE_REQ[e$clave])
    if (is.na(k)) .adjdoc_error("comun.adjuntos.error_leer")
    if (identical(k, "comun.adjuntos.error_tamano")) .adjdoc_error(k, mb = mb)
    .adjdoc_error(k)
  })
}

# Validate one uploaded file. origen: "archivo" | "pegado" (a pasted image).
# Returns list(clase = "archivo", ext, mime, bytes, tamano, ancho, alto, nombre)
# or list(clase = "tabla", ..., nombre) for a CSV; stop()s with adjdoc_error.
adjdoc_preparar <- function(ruta, nombre, origen = c("archivo", "pegado")) {
  origen <- match.arg(origen)
  tam <- suppressWarnings(file.info(ruta)$size)
  if (is.na(tam)) .adjdoc_error("comun.adjuntos.error_leer")
  if (tam == 0) .adjdoc_error("comun.adjuntos.error_tipo")
  if (tam > ADJ_MAX_ARCHIVO) .adjdoc_error("comun.adjuntos.error_tamano", mb = ADJ_MAX_ARCHIVO / 1024^2)
  if (!is.na(adjunto_tipo_por_bytes(readBin(ruta, "raw", n = tam)))) {
    p <- .adjdoc_traducir(adjunto_preparar(ruta, nombre, origen),
                          mb = (if (origen == "pegado") ADJ_MAX_PEGADO else ADJ_MAX_ARCHIVO) / 1024^2)
    return(list(clase = "archivo", ext = p$ext, mime = unname(ADJDOC_EXT[p$ext]), bytes = p$bytes,
                tamano = p$tamano, ancho = p$ancho, alto = p$alto, nombre = p$nombre))
  }
  if (origen == "pegado") .adjdoc_error("comun.adjuntos.error_tipo")
  txt <- .adjdoc_texto(readBin(ruta, "raw", n = tam))
  if (is.null(txt)) .adjdoc_error("comun.adjuntos.error_tipo")
  tipo <- .adjdoc_tipo_texto(txt)
  if (is.na(tipo)) .adjdoc_error("comun.adjuntos.error_tipo")
  nom <- .adj_nombre_limpio(nombre)
  if (tipo == "xml") {
    if (tam > ADJDOC_MAX_XML) .adjdoc_error("comun.adjuntos.error_tamano", mb = ADJDOC_MAX_XML / 1024^2)
    bytes <- charToRaw(enc2utf8(txt))
    return(list(clase = "archivo", ext = "xml", mime = unname(ADJDOC_EXT["xml"]), bytes = bytes,
                tamano = length(bytes), ancho = NA_integer_, alto = NA_integer_, nombre = nom))
  }
  if (nchar(txt) > TABLA_MAX_TEXTO) .adjdoc_error("comun.adjuntos.tabla.error_grande")
  c(list(clase = "tabla", nombre = nom), .adjdoc_tabla_desde_matriz(.adjdoc_csv_leer(txt)))
}

# Excel cells pasted as text: Requisiciones' parser, its caps and notice.
adjdoc_preparar_pegado <- function(texto) .adjdoc_traducir(tabla_pegada_preparar(texto))

# ── The table ────────────────────────────────────────────────────────────────

.adjdoc_leer <- function(cid) leer_tabla_para_escribir(ADJDOC_KEY, cid, .schema_adjuntos_documentos)

.adjdoc_vivos <- function(df, entidad, entidad_id, clase)
  sum(df$entidad %in% entidad & df$entidad_id %in% entidad_id & df$clase %in% clase & is.na(df$papelera_en))

.adjdoc_tope <- function(e, clase) if (identical(clase, "tabla")) e$max_tablas else e$max_vivos
.adjdoc_tope_error <- function(e, clase)
  .adjdoc_error(if (identical(clase, "tabla")) "comun.adjuntos.error_limite_tablas" else "comun.adjuntos.error_limite", n = .adjdoc_tope(e, clase))

.adjdoc_revisar <- function(cid, entidad, entidad_id, tipo_doc) {
  e <- adj_entidad(entidad)
  if (!is.character(entidad_id) || length(entidad_id) != 1L || !grepl(.ADJDOC_ID_OK, entidad_id))
    stop("[adjuntos] id inválido", call. = FALSE)
  if (!isTRUE(do.call(get(e$existe, mode = "function"), list(cid, entidad_id))))
    .adjdoc_error("comun.adjuntos.error_no_existe")
  if (!tipo_doc %in% e$tipos_doc) .adjdoc_error("comun.adjuntos.error_tipo_doc")
  e
}

# Store the bytes first (server-made key), then the row: a row never points
# to bytes S3 lacks. Returns the document's id.
.adjdoc_guardar <- function(cid, entidad, entidad_id, tipo_doc, fila, bytes, mime, accion, por, home) {
  e <- .adjdoc_revisar(cid, entidad, entidad_id, tipo_doc)
  actual <- .adjdoc_leer(cid)
  if (.adjdoc_vivos(actual, entidad, entidad_id, fila$clase) >= .adjdoc_tope(e, fila$clase))
    .adjdoc_tope_error(e, fila$clase)
  .adjdoc_uuid_repetido(actual, entidad, entidad_id, fila$ref_uuid)
  id <- uuid::UUIDgenerate()
  llave <- adjdoc_llave(cid, entidad, entidad_id, id, fila$ext)
  almacen_guardar(ADJ_ALMACEN_ACTUAL, llave, bytes, mime)
  nueva <- tibble::tibble(id = id, entidad = entidad, entidad_id = entidad_id, tipo_doc = tipo_doc,
    clase = fila$clase, ext = fila$ext, mime = mime, tamano = as.numeric(length(bytes)),
    sha256 = adjdoc_sha256(bytes), nombre = fila$nombre %||% NA_character_,
    titulo = fila$titulo %||% NA_character_, nota = NA_character_,
    ancho = as.integer(fila$ancho %||% NA_integer_), alto = as.integer(fila$alto %||% NA_integer_),
    filas = as.integer(fila$filas %||% NA_integer_), columnas = as.integer(fila$columnas %||% NA_integer_),
    encabezado = if (is.null(fila$encabezado)) NA else isTRUE(fila$encabezado),
    refs = fila$refs %||% "{}", ref_uuid = fila$ref_uuid %||% NA_character_,
    almacen = ADJ_ALMACEN_ACTUAL, llave = llave, comprimido = "no",
    creado_por = por, creado = as.POSIXct(Sys.time(), tz = "UTC"))
  desc <- fila$titulo %||% fila$nombre %||% ""
  if (identical(accion, "pegar_tabla_documento"))
    modificar_tabla(ADJDOC_KEY, cid, .schema_adjuntos_documentos, function(df) {
      if (.adjdoc_vivos(df, entidad, entidad_id, fila$clase) >= .adjdoc_tope(e, fila$clase)) .adjdoc_tope_error(e, fila$clase)
      .adjdoc_uuid_repetido(df, entidad, entidad_id, fila$ref_uuid)
      dplyr::bind_rows(df, nueva)
    }, evento = list(usuario = por, modulo = "adjuntos", accion = "pegar_tabla_documento", descripcion = desc,
                     target_id = id, metadata = list(entidad = entidad, entidad_id = entidad_id, tipo_doc = tipo_doc),
                     home = home))
  else
    modificar_tabla(ADJDOC_KEY, cid, .schema_adjuntos_documentos, function(df) {
      if (.adjdoc_vivos(df, entidad, entidad_id, fila$clase) >= .adjdoc_tope(e, fila$clase)) .adjdoc_tope_error(e, fila$clase)
      .adjdoc_uuid_repetido(df, entidad, entidad_id, fila$ref_uuid)
      dplyr::bind_rows(df, nueva)
    }, evento = list(usuario = por, modulo = "adjuntos", accion = "adjuntar_documento", descripcion = desc,
                     target_id = id, metadata = list(entidad = entidad, entidad_id = entidad_id, tipo_doc = tipo_doc),
                     home = home))
  id
}

# T106 2a: the same CFDI (fiscal UUID) twice on one entity is refused; on
# another entity it is allowed (a CFDI may back a liability and its invoice).
.adjdoc_uuid_repetido <- function(df, entidad, entidad_id, uuid) {
  if (is.null(uuid) || is.na(uuid)) return(invisible(FALSE))
  hay <- df$entidad %in% entidad & df$entidad_id %in% entidad_id & df$ref_uuid %in% uuid & is.na(df$papelera_en)
  if (any(hay)) .adjdoc_error("comun.adjuntos.error_cfdi_repetido")
  invisible(FALSE)
}

# A prepared file (adjdoc_preparar()). A CSV becomes a table (canonical CSV).
# T106 2a: an XML that is a stamped CFDI keeps its fiscal fields in refs and
# its UUID in ref_uuid, and is filed as "cfdi" when the entity takes that kind.
documento_adjuntar <- function(client_id, entidad, entidad_id, preparado, tipo_doc, por, home = NULL) {
  cid <- .adjdoc_cid(client_id)
  refs <- "{}"; ref_uuid <- NA_character_
  if (identical(preparado$ext, "xml")) {
    cfdi <- tryCatch(adjdoc_leer_cfdi(preparado$bytes), error = function(e) NULL)
    if (!is.null(cfdi)) {
      refs <- adjdoc_refs_cfdi(cfdi); ref_uuid <- cfdi$uuid
      if ("cfdi" %in% adj_entidad(entidad)$tipos_doc) tipo_doc <- "cfdi"
    }
  }
  if (identical(preparado$clase, "tabla"))
    return(documento_tabla_agregar(cid, entidad, entidad_id, preparado, titulo = preparado$nombre,
                                   encabezado = isTRUE(preparado$encabezado_sugerido), tipo_doc = tipo_doc,
                                   por = por, home = home, accion = "adjuntar_documento"))
  .adjdoc_guardar(cid, entidad, entidad_id, tipo_doc,
                  list(clase = "archivo", ext = preparado$ext, nombre = preparado$nombre,
                       ancho = preparado$ancho, alto = preparado$alto, refs = refs, ref_uuid = ref_uuid),
                  preparado$bytes, preparado$mime, "adjuntar_documento", por, home)
}

# A prepared table (pasted, or a CSV file), after its preview.
documento_tabla_agregar <- function(client_id, entidad, entidad_id, preparada, titulo, encabezado, tipo_doc,
                                    por, home = NULL, accion = "pegar_tabla_documento") {
  cid <- .adjdoc_cid(client_id)
  celdas <- preparada$celdas
  if (!is.matrix(celdas) || !nrow(celdas) || nrow(celdas) > TABLA_MAX_FILAS || ncol(celdas) > TABLA_MAX_COLS)
    .adjdoc_error("comun.adjuntos.tabla.error_vacia")
  t <- trimws(as.character(titulo %||% ""))[1]
  if (is.na(t) || !nzchar(t)) t <- NA_character_ else if (nchar(t) > 200L) t <- substr(t, 1L, 200L)
  .adjdoc_guardar(cid, entidad, entidad_id, tipo_doc,
                  list(clase = "tabla", ext = "csv", titulo = t, nombre = preparada$nombre,
                       filas = nrow(celdas), columnas = ncol(celdas), encabezado = isTRUE(encabezado)),
                  adjdoc_csv_escribir(celdas), unname(ADJDOC_EXT["csv"]), accion, por, home)
}

# An entity's documents (live only unless incluir_papelera), oldest first.
documentos_de <- function(client_id, entidad, entidad_id, incluir_papelera = FALSE) {
  adj_entidad(entidad)
  df <- .adjdoc_leer(.adjdoc_cid(client_id))
  df <- df[df$entidad %in% entidad & df$entidad_id %in% entidad_id & (incluir_papelera | is.na(df$papelera_en)), , drop = FALSE]
  df[order(df$creado), , drop = FALSE]
}

.adjdoc_fila <- function(df, id) {
  i <- which(df$id %in% id)
  if (length(i) != 1L) .adjdoc_error("comun.adjuntos.error_no_existe")
  i
}

documento_papelera <- function(client_id, doc_id, por, home = NULL) {
  cid <- .adjdoc_cid(client_id)
  fila <- .adjdoc_leer(cid); fila <- fila[.adjdoc_fila(fila, doc_id), , drop = FALSE]
  modificar_tabla(ADJDOC_KEY, cid, .schema_adjuntos_documentos, function(df) {
    i <- .adjdoc_fila(df, doc_id)
    if (is.na(df$papelera_en[i])) { df$papelera_por[i] <- por; df$papelera_en[i] <- as.POSIXct(Sys.time(), tz = "UTC") }
    df
  }, evento = list(usuario = por, modulo = "adjuntos", accion = "papelera_documento",
                   descripcion = fila$titulo %|na|% fila$nombre %|na|% "", target_id = doc_id,
                   metadata = list(entidad = fila$entidad, entidad_id = fila$entidad_id), home = home))
  invisible(TRUE)
}

documento_restaurar <- function(client_id, doc_id, por, home = NULL) {
  cid <- .adjdoc_cid(client_id)
  fila <- .adjdoc_leer(cid); fila <- fila[.adjdoc_fila(fila, doc_id), , drop = FALSE]
  e <- adj_entidad(fila$entidad)
  modificar_tabla(ADJDOC_KEY, cid, .schema_adjuntos_documentos, function(df) {
    i <- .adjdoc_fila(df, doc_id)
    if (is.na(df$papelera_en[i])) return(df)
    if (.adjdoc_vivos(df, df$entidad[i], df$entidad_id[i], df$clase[i]) >= .adjdoc_tope(e, df$clase[i]))
      .adjdoc_tope_error(e, df$clase[i])
    df$papelera_por[i] <- NA_character_; df$papelera_en[i] <- as.POSIXct(NA, tz = "UTC")
    df
  }, evento = list(usuario = por, modulo = "adjuntos", accion = "restaurar_documento",
                   descripcion = fila$titulo %|na|% fila$nombre %|na|% "", target_id = doc_id,
                   metadata = list(entidad = fila$entidad, entidad_id = fila$entidad_id), home = home))
  invisible(TRUE)
}

# A document's bytes for a download, from its stored key (which must sit
# under this client's prefix -- almacen_leer() checks).
documento_bytes <- function(client_id, doc_id) {
  cid <- .adjdoc_cid(client_id)
  df <- .adjdoc_leer(cid); fila <- df[.adjdoc_fila(df, doc_id), , drop = FALSE]
  list(bytes = almacen_leer(fila$almacen, cid, fila$llave), mime = fila$mime, ext = fila$ext,
       nombre = fila$titulo %|na|% fila$nombre %|na|% "documento", clase = fila$clase)
}

# A stored table: list(celdas = character matrix, encabezado).
documento_tabla <- function(client_id, doc_id) {
  b <- documento_bytes(client_id, doc_id)
  if (!identical(b$clase, "tabla")) stop("[adjuntos] no es una tabla", call. = FALSE)
  txt <- rawToChar(b$bytes); Encoding(txt) <- "UTF-8"
  df <- .adjdoc_leer(.adjdoc_cid(client_id))
  list(celdas = .adjdoc_csv_leer(txt), encabezado = isTRUE(df$encabezado[df$id %in% doc_id][1]))
}

`%|na|%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1]) || !nzchar(a[1])) b else a[1]
