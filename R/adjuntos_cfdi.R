# =============================================================================
# R/adjuntos_cfdi.R -- T106 phase 2a: what a CFDI says, read when it is attached
#
# A Mexican electronic invoice (CFDI 3.3 / 4.0) is an XML whose stamp
# (tfd:TimbreFiscalDigital) carries its fiscal UUID. The attachments layer
# (R/adjuntos_documentos.R) keeps, per document, refs (JSON) and ref_uuid --
# so a CFDI attached today links by its UUID to an invoice once the ERP
# brings that UUID, with nothing to redo.
#
# Read with xml2 (installed with aws.s3, which imports it): options NONET (no
# network) and no entity substitution; a DTD or an entity declaration never
# gets here (refused when the file is prepared, .adjdoc_tipo_texto()).
# Elements found by local name, whatever prefix the file uses. Only fiscal
# fields are read -- never concepts, addresses or amounts per line.
# adjdoc_leer_cfdi(bytes): list(uuid, version, tipo, serie, folio, serie_folio,
# fecha, total, moneda, rfc_emisor, rfc_receptor) or NULL when the XML is not
# a stamped CFDI (it is kept all the same, as a plain XML).
# =============================================================================

.CFDI_UUID_OK <- "^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$"

adjdoc_leer_cfdi <- function(bytes) {
  doc <- tryCatch(xml2::read_xml(bytes, options = c("NONET", "NOBLANKS")), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  raiz <- xml2::xml_root(doc)
  if (!identical(xml2::xml_name(raiz), "Comprobante")) return(NULL)
  timbre <- xml2::xml_find_first(raiz, ".//*[local-name()='TimbreFiscalDigital']")
  uuid <- toupper(trimws(xml2::xml_attr(timbre, "UUID")))
  if (length(uuid) != 1L || is.na(uuid) || !grepl(.CFDI_UUID_OK, uuid)) return(NULL)
  attr_raiz <- function(n) { v <- xml2::xml_attr(raiz, n); if (length(v) != 1L || is.na(v)) NA_character_ else trimws(v) }
  rfc <- function(nodo) {
    v <- xml2::xml_attr(xml2::xml_find_first(raiz, sprintf("./*[local-name()='%s']", nodo)), "Rfc")
    if (length(v) != 1L || is.na(v)) NA_character_ else toupper(trimws(v))
  }
  serie <- attr_raiz("Serie"); folio <- attr_raiz("Folio")
  list(uuid = uuid, version = attr_raiz("Version"), tipo = attr_raiz("TipoDeComprobante"),
       serie = serie, folio = folio,
       serie_folio = paste0(if (is.na(serie)) "" else serie, if (is.na(folio)) "" else folio),
       fecha = attr_raiz("Fecha"), total = suppressWarnings(as.numeric(attr_raiz("Total"))),
       moneda = attr_raiz("Moneda"), rfc_emisor = rfc("Emisor"), rfc_receptor = rfc("Receptor"))
}

# The refs JSON of a read CFDI (what the document row keeps).
adjdoc_refs_cfdi <- function(cfdi) {
  as.character(jsonlite::toJSON(list(cfdi_uuid = cfdi$uuid, rfc_emisor = cfdi$rfc_emisor,
    rfc_receptor = cfdi$rfc_receptor, serie_folio = cfdi$serie_folio, total = cfdi$total,
    moneda = cfdi$moneda, fecha = cfdi$fecha, tipo = cfdi$tipo, version = cfdi$version),
    auto_unbox = TRUE, na = "null"))
}

# Documents of this client carrying `valor` in refs$<clave>, any entity, live
# only unless incluir_papelera. cfdi_uuid goes by the ref_uuid column.
adjuntos_buscar_ref <- function(client_id, clave, valor, incluir_papelera = FALSE) {
  df <- .adjdoc_leer(.adjdoc_cid(client_id))
  if (!incluir_papelera) df <- df[is.na(df$papelera_en), , drop = FALSE]
  if (identical(clave, "cfdi_uuid")) return(df[df$ref_uuid %in% toupper(trimws(valor)), , drop = FALSE])
  hay <- vapply(df$refs, function(j) {
    r <- tryCatch(jsonlite::fromJSON(j %||% "{}"), error = function(e) list())
    identical(as.character(r[[clave]] %||% NA_character_), as.character(valor))
  }, logical(1))
  df[hay, , drop = FALSE]
}
