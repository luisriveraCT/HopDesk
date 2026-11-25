# =============================================================================
# R/adjuntos_almacen.R -- T106: where a document's bytes live (pluggable)
#
# One interface for the attachments layer; S3 is the only store today, through
# Requisiciones' own upload and read (R/adjuntos_s3.R, front E -- wrapped,
# never edited): .adj_s3_poner() uploads with one retry and stop()s if both
# fail; .adj_s3_traer() reads with retries and stop()s, never "empty".
#   guardar(bytes, llave, mime)  -> invisible(TRUE) or stop()
#   leer(llave)                  -> raw bytes or stop()
# Trashing is the table's (a mark), never the store's: no object is deleted.
# Room left on purpose, not programmed (T106 paso 0 §3): another store (the
# cloud data project) is one more entry; a "transformar" pair (compress /
# encrypt before saving, undo after reading) would go in the entry, with the
# document's `comprimido` column saying which applied. Today: "no".
# =============================================================================

ADJ_ALMACENES <- list(
  s3 = list(guardar = ".adj_s3_poner", leer = ".adj_s3_traer")
)
ADJ_ALMACEN_ACTUAL <- "s3"

# Stored formats (the extension in the key and what a download carries).
ADJDOC_EXT <- c(png = "image/png", jpg = "image/jpeg", pdf = "application/pdf",
                xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                csv = "text/csv", xml = "application/xml")

.ADJDOC_ID_OK <- "^[A-Za-z0-9-]{8,64}$"

# The only way a document's key is built:
#   <cliente>/adjuntos/<entidad>/<entidad_id>/<doc_id>.<ext>
# Ids are server-made or validated; no user text ever becomes part of a path.
adjdoc_llave <- function(client_id, entidad, entidad_id, doc_id, ext) {
  cid <- tolower(trimws(client_id %||% ""))
  if (!nzchar(cid)) stop("[adjuntos] client_id es obligatorio", call. = FALSE)
  adj_entidad(entidad)
  if (!grepl(.ADJDOC_ID_OK, entidad_id) || !grepl(.ADJDOC_ID_OK, doc_id))
    stop("[adjuntos] id inválido", call. = FALSE)
  if (!ext %in% names(ADJDOC_EXT)) stop("[adjuntos] extensión inválida", call. = FALSE)
  paste0(cid, "/adjuntos/", entidad, "/", entidad_id, "/", doc_id, ".", ext)
}

almacen_guardar <- function(almacen, llave, bytes, mime) {
  a <- ADJ_ALMACENES[[almacen]]
  if (is.null(a)) stop("[adjuntos] almacén desconocido: ", almacen, call. = FALSE)
  do.call(get(a$guardar, mode = "function"), list(bytes, llave, mime))
}

# Reads only keys under this client's attachments prefix.
almacen_leer <- function(almacen, client_id, llave) {
  a <- ADJ_ALMACENES[[almacen]]
  if (is.null(a)) stop("[adjuntos] almacén desconocido: ", almacen, call. = FALSE)
  cid <- tolower(trimws(client_id %||% ""))
  if (!nzchar(cid) || !startsWith(llave, paste0(cid, "/adjuntos/")))
    stop("[adjuntos] llave ajena", call. = FALSE)
  do.call(get(a$leer, mode = "function"), list(llave))
}
