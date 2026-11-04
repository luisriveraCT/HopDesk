# =============================================================================
# R/adjuntos_s3.R
# Evidence files for Requisiciones (front E): what a file really is, judged by
# its first bytes; server-side re-encoding of images; the S3 objects that hold
# attachments and pasted tables; and the pasted-Excel parser.
#
# Security rules (FRENTE_E_BRIEF §3):
#   - the type comes from the bytes, never from the name or the browser;
#   - images are decoded and re-encoded here (magick) and only the re-encoded
#     copy is stored -- the uploaded bytes never reach S3, so a polyglot file
#     cannot be served back;
#   - the S3 key is built here: <cliente>/adjuntos/<requisicion>/<id>.<ext>;
#     the original name is kept only as text to show (escaped by the screen);
#   - pasted tables are plain text only (never the clipboard's HTML) and
#     nothing in them is reinterpreted; on export/copy, cells starting with
#     = + - @ get a leading apostrophe (neutralizar_celdas()).
# =============================================================================

ADJ_TIPOS <- list(
  png  = list(mime = "image/png",  imagen = TRUE),
  jpg  = list(mime = "image/jpeg", imagen = TRUE),
  webp = list(mime = "image/webp", imagen = TRUE),
  pdf  = list(mime = "application/pdf", imagen = FALSE),
  xlsx = list(mime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", imagen = FALSE)
)

ADJ_MAX_PEGADO   <- 4 * 1024^2     # pasted image (Ctrl+V)
ADJ_MAX_ARCHIVO  <- 10 * 1024^2    # dropped or chosen file, and any stored copy
ADJ_MAX_PIXELES  <- 50e6           # refuse before decoding: decompression bombs
ADJ_MAX_POR_REQ  <- 20L            # live attachments per requisition
TABLA_MAX_FILAS  <- 200L
TABLA_MAX_COLS   <- 20L
TABLA_MAX_POR_REQ <- 10L
TABLA_MAX_TEXTO  <- 2e6            # characters of clipboard text accepted at all

# A validation failure the screen can show as is: message already translated.
.req_error <- function(clave, ...) {
  stop(structure(class = c("req_error", "error", "condition"),
                 list(message = tr(clave, ...), call = NULL, clave = clave)))
}

# ── What a file is, by its first bytes ───────────────────────────────────────

.adj_empieza <- function(b, firma, desde = 1L) {
  n <- length(firma)
  length(b) >= desde + n - 1L && identical(as.integer(b[desde:(desde + n - 1L)]), as.integer(firma))
}

# "png" | "jpg" | "webp" | "pdf" | "xlsx", or NA for anything else (an .exe
# renamed to .png is "MZ...", so NA). An xlsx is a zip: it must also look
# like a workbook inside, and a workbook with macros (vbaProject.bin, i.e. an
# .xlsm renamed) is refused.
adjunto_tipo_por_bytes <- function(b) {
  if (!is.raw(b) || length(b) < 8L) return(NA_character_)
  if (.adj_empieza(b, as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)))) return("png")
  if (.adj_empieza(b, as.raw(c(0xFF, 0xD8, 0xFF)))) return("jpg")
  if (.adj_empieza(b, charToRaw("RIFF")) && .adj_empieza(b, charToRaw("WEBP"), 9L)) return("webp")
  if (.adj_empieza(b, charToRaw("%PDF-"))) return("pdf")
  if (.adj_empieza(b, as.raw(c(0x50, 0x4B, 0x03, 0x04)))) {
    tf <- tempfile(fileext = ".zip"); on.exit(unlink(tf), add = TRUE)
    writeBin(b, tf)
    nombres <- tryCatch(utils::unzip(tf, list = TRUE)$Name, error = function(e) character())
    if ("[Content_Types].xml" %in% nombres && "xl/workbook.xml" %in% nombres &&
        !any(grepl("vbaProject\\.bin$", nombres, ignore.case = TRUE)))
      return("xlsx")
  }
  NA_character_
}

# Width and height read from the header, before anything decodes the image.
# NULL when the header cannot be read (the file is then refused).
.adj_u16be <- function(b, i) as.integer(b[i]) * 256L + as.integer(b[i + 1L])
.adj_u32be <- function(b, i) as.numeric(.adj_u16be(b, i)) * 65536 + .adj_u16be(b, i + 2L)
.adj_u16le <- function(b, i) as.integer(b[i]) + as.integer(b[i + 1L]) * 256L
.adj_u24le <- function(b, i) as.integer(b[i]) + as.integer(b[i + 1L]) * 256L + as.integer(b[i + 2L]) * 65536L

.adj_dimensiones <- function(b, tipo) {
  n <- length(b)
  if (tipo == "png") {
    if (n < 24L || !identical(rawToChar(b[13:16]), "IHDR")) return(NULL)
    return(c(.adj_u32be(b, 17L), .adj_u32be(b, 21L)))
  }
  if (tipo == "jpg") {
    sof <- c(0xC0:0xC3, 0xC5:0xC7, 0xC9:0xCB, 0xCD:0xCF)
    i <- 3L
    while (i + 8L <= n) {
      if (b[i] != as.raw(0xFF)) return(NULL)
      m <- as.integer(b[i + 1L])
      if (m == 0xFF) { i <- i + 1L; next }
      if (m %in% c(0x01, 0xD0:0xD7)) { i <- i + 2L; next }
      if (m %in% sof) return(c(.adj_u16be(b, i + 7L), .adj_u16be(b, i + 5L)))
      if (m %in% c(0xD9, 0xDA)) return(NULL)
      i <- i + 2L + .adj_u16be(b, i + 2L)
    }
    return(NULL)
  }
  if (tipo == "webp") {
    if (n < 30L) return(NULL)
    chunk <- rawToChar(b[13:16])
    if (chunk == "VP8X") return(c(.adj_u24le(b, 25L) + 1, .adj_u24le(b, 28L) + 1))
    if (chunk == "VP8 ") return(c(bitwAnd(.adj_u16le(b, 27L), 0x3FFF), bitwAnd(.adj_u16le(b, 29L), 0x3FFF)))
    if (chunk == "VP8L") {
      v <- as.numeric(b[22]) + as.numeric(b[23]) * 256 + as.numeric(b[24]) * 65536 + as.numeric(b[25]) * 16777216
      return(c(v %% 16384 + 1, (v %/% 16384) %% 16384 + 1))
    }
    return(NULL)
  }
  NULL
}

# Decode and re-encode on the server. EXIF rotation is applied and every
# metadata block dropped. PNG and JPEG keep their format; WEBP becomes PNG
# (fewer codecs needed where the app runs), or JPEG if that PNG is too big.
# Returns list(bytes, ext, ancho, alto) or stop()s (the caller refuses).
.adj_recodificar <- function(b, tipo) {
  if (!requireNamespace("magick", quietly = TRUE)) stop("magick no disponible", call. = FALSE)
  img <- magick::image_read(b)
  if (length(img) != 1L) img <- img[1]           # animated: first frame only
  img <- magick::image_strip(magick::image_orient(img))
  salida <- if (tipo == "jpg") "jpeg" else "png"
  out <- magick::image_write(img, format = salida, quality = if (salida == "jpeg") 90 else NULL)
  if (tipo == "webp" && length(out) > ADJ_MAX_ARCHIVO) {
    salida <- "jpeg"
    out <- magick::image_write(magick::image_flatten(magick::image_background(img, "white")),
                               format = "jpeg", quality = 90)
  }
  info <- magick::image_info(magick::image_read(out))
  list(bytes = out, ext = if (salida == "jpeg") "jpg" else "png",
       ancho = as.integer(info$width), alto = as.integer(info$height))
}

# The name the user gave the file, kept only to show it: no path, no control
# characters, at most 200 characters. Never used to build a key.
.adj_nombre_limpio <- function(nombre) {
  x <- as.character(nombre %||% "")[1]
  if (is.na(x)) x <- ""
  x <- basename(gsub("\\\\", "/", x))
  x <- gsub("[[:cntrl:]]", "", x)
  x <- trimws(x)
  if (nchar(x) > 200L) x <- substr(x, 1L, 200L)
  if (!nzchar(x)) x <- "archivo"
  x
}

# Validate and prepare one file for evidencia_adjuntar() (R/requisiciones_persistence.R).
#   ruta    path of the uploaded file (Shiny's fileInput datapath)
#   nombre  original name, shown only
#   origen  "pegado" (Ctrl+V of an image, 4 MB) or "archivo" (10 MB)
# Returns list(tipo, ext, mime, es_imagen, bytes, tamano, ancho, alto, nombre)
# or stop()s with a req_error.
adjunto_preparar <- function(ruta, nombre, origen = c("archivo", "pegado")) {
  origen <- match.arg(origen)
  limite <- if (origen == "pegado") ADJ_MAX_PEGADO else ADJ_MAX_ARCHIVO
  tam <- suppressWarnings(file.info(ruta)$size)
  if (is.na(tam)) .req_error("requisiciones.evidencia.error_leer")
  if (tam > limite) .req_error("requisiciones.evidencia.error_tamano", mb = limite / 1024^2)
  if (tam == 0) .req_error("requisiciones.evidencia.error_tipo")
  b <- readBin(ruta, "raw", n = tam)
  tipo <- adjunto_tipo_por_bytes(b)
  if (is.na(tipo)) .req_error("requisiciones.evidencia.error_tipo")
  if (origen == "pegado" && !ADJ_TIPOS[[tipo]]$imagen) .req_error("requisiciones.evidencia.error_tipo")
  ancho <- NA_integer_; alto <- NA_integer_
  if (ADJ_TIPOS[[tipo]]$imagen) {
    dim <- .adj_dimensiones(b, tipo)
    if (is.null(dim) || any(!is.finite(dim)) || any(dim < 1) || prod(dim) > ADJ_MAX_PIXELES)
      .req_error("requisiciones.evidencia.error_imagen")
    rec <- tryCatch(.adj_recodificar(b, tipo), error = function(e) {
      message("[adjuntos] recodificación falló: ", conditionMessage(e)); NULL })
    if (is.null(rec)) .req_error("requisiciones.evidencia.error_imagen")
    if (length(rec$bytes) > ADJ_MAX_ARCHIVO)
      .req_error("requisiciones.evidencia.error_tamano", mb = ADJ_MAX_ARCHIVO / 1024^2)
    b <- rec$bytes; ancho <- rec$ancho; alto <- rec$alto
    tipo_guardado <- rec$ext
  } else tipo_guardado <- tipo
  list(tipo = tipo_guardado, ext = tipo_guardado, mime = ADJ_TIPOS[[tipo_guardado]]$mime,
       es_imagen = ADJ_TIPOS[[tipo_guardado]]$imagen, bytes = b, tamano = length(b),
       ancho = ancho, alto = alto, nombre = .adj_nombre_limpio(nombre))
}

# ── S3 objects ───────────────────────────────────────────────────────────────

.ADJ_ID_OK <- "^[A-Za-z0-9-]{8,64}$"

# The only way a key is built. Ids are server-made UUIDs; anything else is
# refused so no user text can ever become part of a path.
.adj_llave <- function(client_id, requisicion_id, evid_id, ext) {
  cid <- tolower(trimws(client_id %||% ""))
  if (!nzchar(cid)) stop("[adjuntos] client_id es obligatorio", call. = FALSE)
  if (!grepl(.ADJ_ID_OK, requisicion_id) || !grepl(.ADJ_ID_OK, evid_id))
    stop("[adjuntos] id inválido", call. = FALSE)
  if (!ext %in% c(names(ADJ_TIPOS), "json")) stop("[adjuntos] extensión inválida", call. = FALSE)
  paste0(cid, "/adjuntos/", requisicion_id, "/", evid_id, ".", ext)
}

# Upload raw bytes under a full key (already prefixed by .adj_llave()). One
# retry; stop()s if both fail, so the caller never records a file S3 lacks.
.adj_s3_poner <- function(bytes, llave, mime) {
  ultimo <- NULL
  for (intento in 1:2) {
    tf <- tempfile(); writeBin(bytes, tf)
    ok <- tryCatch({
      res <- suppressMessages(aws.s3::put_object(file = tf, object = llave, bucket = .s3_bucket(),
                                                 headers = list(`Content-Type` = mime)))
      isTRUE(res)
    }, error = function(e) { ultimo <<- conditionMessage(e); FALSE })
    unlink(tf)
    if (ok) return(invisible(TRUE))
  }
  stop("[adjuntos] no se pudo subir ", llave, if (!is.null(ultimo)) paste0(": ", ultimo), call. = FALSE)
}

# Bytes of a stored object; stop()s if it cannot be read (never "empty").
.adj_s3_traer <- function(llave) {
  ultimo <- NULL
  for (intento in 1:3) {
    x <- tryCatch(aws.s3::get_object(object = llave, bucket = .s3_bucket()),
                  error = function(e) { ultimo <<- conditionMessage(e); NULL })
    if (is.raw(x)) return(x)
    if (intento < 3) Sys.sleep(0.3)
  }
  stop("[adjuntos] no se pudo leer ", llave, if (!is.null(ultimo)) paste0(": ", ultimo), call. = FALSE)
}

# ── Pasted Excel cells ───────────────────────────────────────────────────────

# Excel's tab-separated clipboard text into a list of rows (character vectors).
# A field is quoted only when it starts with a double quote (Excel does that
# for cells holding tabs, line breaks or a leading quote); inside, "" is ".
# Quotes in the middle of a field (5" tubo) are text. Works on code points and
# jumps between separators, so a large paste is not read character by character.
.tabla_partir <- function(texto) {
  cp <- utf8ToInt(texto)
  n <- length(cp)
  if (!n) return(list())
  comillas <- which(cp == 34L)
  delims   <- which(cp == 9L | cp == 10L)
  siguiente <- function(v, desde) { k <- findInterval(desde - 1, v) + 1L; if (k > length(v)) NA_integer_ else v[k] }
  txt <- function(a, b) if (b < a) "" else intToUtf8(cp[a:b])
  filas <- list(); fila <- character(); i <- 1L
  repeat {
    campo <- NULL
    if (i <= n && cp[i] == 34L) {
      j <- siguiente(comillas, i + 1L)
      while (!is.na(j) && j < n && cp[j + 1L] == 34L) j <- siguiente(comillas, j + 2L)
      if (!is.na(j)) {
        campo <- gsub("\"\"", "\"", txt(i + 1L, j - 1L), fixed = TRUE)
        d <- siguiente(delims, j + 1L)
        fin <- if (is.na(d)) n else d - 1L
        campo <- paste0(campo, txt(j + 1L, fin))
      }
    }
    if (is.null(campo)) {
      d <- siguiente(delims, i)
      campo <- txt(i, if (is.na(d)) n else d - 1L)
    }
    fila <- c(fila, campo)
    if (is.na(d)) { filas[[length(filas) + 1L]] <- fila; break }
    if (cp[d] == 10L) { filas[[length(filas) + 1L]] <- fila; fila <- character() }
    i <- d + 1L
    if (i > n) { if (cp[d] == 9L) filas[[length(filas) + 1L]] <- c(fila, "") else NULL; break }
  }
  filas
}

.parece_numero <- function(x) grepl("^\\s*[-+]?\\(?[$]?\\s*[0-9][0-9,]*(\\.[0-9]+)?\\)?\\s*%?\\s*$", x)

# Clipboard text -> table ready to preview and confirm. Nothing is
# reinterpreted: every cell stays the exact text that was pasted (no dates,
# leading zeros kept). Trimmed to TABLA_MAX_FILAS x TABLA_MAX_COLS, and the
# trimming is always reported in `aviso` (never silent).
# Returns list(celdas = character matrix, filas, columnas, filas_pegadas,
#              columnas_pegadas, recortada, aviso, encabezado_sugerido,
#              numericas = logical per column)
tabla_pegada_preparar <- function(texto) {
  if (!is.character(texto) || length(texto) != 1L || is.na(texto))
    .req_error("requisiciones.tabla.error_vacia")
  if (nchar(texto, type = "chars", allowNA = TRUE) %in% NA)
    .req_error("requisiciones.tabla.error_vacia")
  if (nchar(texto) > TABLA_MAX_TEXTO) .req_error("requisiciones.tabla.error_grande")
  texto <- gsub("\r\n?", "\n", enc2utf8(texto))
  texto <- sub("\n+$", "", texto)
  if (!nzchar(gsub("[\t\n ]", "", texto))) .req_error("requisiciones.tabla.error_vacia")
  filas <- .tabla_partir(texto)
  nf <- length(filas)
  nc <- max(lengths(filas))
  fr <- min(nf, TABLA_MAX_FILAS); cr <- min(nc, TABLA_MAX_COLS)
  celdas <- matrix("", nrow = fr, ncol = cr)
  for (r in seq_len(fr)) {
    v <- filas[[r]][seq_len(min(length(filas[[r]]), cr))]
    celdas[r, seq_along(v)] <- v
  }
  recortada <- nf > fr || nc > cr
  aviso <- if (!recortada) NA_character_
    else if (nf > fr && nc > cr) tr("requisiciones.tabla.recorte_ambos", filas = nf, guardadas = fr, columnas = nc, cguardadas = cr)
    else if (nf > fr) tr("requisiciones.tabla.recorte_filas", filas = nf, guardadas = fr)
    else tr("requisiciones.tabla.recorte_columnas", columnas = nc, cguardadas = cr)
  primera <- celdas[1, ]
  encabezado <- fr > 1L && all(nzchar(trimws(primera))) && !any(.parece_numero(primera)) &&
    !anyDuplicated(primera)
  cuerpo <- if (encabezado) celdas[-1, , drop = FALSE] else celdas
  numericas <- vapply(seq_len(cr), function(j) {
    v <- cuerpo[, j]; v <- v[nzchar(trimws(v))]
    length(v) > 0L && all(.parece_numero(v))
  }, logical(1))
  list(celdas = celdas, filas = fr, columnas = cr, filas_pegadas = nf, columnas_pegadas = nc,
       recortada = recortada, aviso = aviso, encabezado_sugerido = encabezado, numericas = numericas)
}

# For export and copy only: a cell that starts with = + - @ would run as a
# formula when opened in Excel, so it gets a leading apostrophe. Stored and
# shown text is never altered.
neutralizar_celdas <- function(x) {
  if (is.null(x)) return(x)
  if (is.data.frame(x)) { x[] <- lapply(x, function(col) if (is.character(col)) neutralizar_celdas(col) else col); return(x) }
  if (!is.character(x)) return(x)
  peligro <- !is.na(x) & grepl("^[=+@-]", x)
  x[peligro] <- paste0("'", x[peligro])
  x
}

# Tab-separated text of a stored table, ready to copy back into Excel.
tabla_a_texto_copiable <- function(celdas) {
  m <- neutralizar_celdas(celdas)
  m[] <- gsub("[\t\r\n]", " ", m)
  paste(apply(m, 1L, paste, collapse = "\t"), collapse = "\r\n")
}
