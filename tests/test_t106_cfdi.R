# =============================================================================
# tests/test_t106_cfdi.R
# T106 phase 2a: a CFDI attached to an entity keeps what it is -- its fiscal
# UUID (ref_uuid) and its fiscal fields (refs) -- so it links to its invoice
# by UUID once the ERP brings it. INVENTED CFDIs only (RFCs of the SAT's
# generic kind, made-up UUIDs and amounts). In-memory S3.
#   1  adjdoc_leer_cfdi(): CFDI 4.0 and 3.3, any prefix; a plain XML or a
#      CFDI without stamp is not a CFDI (NULL); UUID in upper case
#   2  attaching one: refs + ref_uuid, filed as "cfdi", the XML stored as is
#   3  the same UUID twice on one entity: refused, nothing stored; on another
#      entity: allowed; after trashing the first: allowed again
#   4  adjuntos_buscar_ref() finds it by UUID, any entity
#   5  a plain XML is kept as XML, refs "{}" and no ref_uuid
#   6  the schema: an old row (no ref_uuid) reads as NA
#   7  the card shows the CFDI's summary (UUID block, issuer, total in its currency)
#
# Run from project root:  Rscript tests/test_t106_cfdi.R
# =============================================================================
cat("=== test_t106_cfdi ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/monedas.R", "R/tier_registry.R", "R/permisos.R", "R/app_audit.R", "R/adjuntos_s3.R",
            "R/adjuntos_registro.R", "R/adjuntos_almacen.R", "R/adjuntos_documentos.R", "R/adjuntos_cfdi.R",
            "R/adjuntos_pasivo_ui.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
s3_memoria_reinstalar()
q <- function(expr) invisible(suppressWarnings(suppressMessages(expr)))
err <- function(expr) tryCatch({ q(expr); NULL }, error = function(e) e)

CID <- "demo"; P1 <- "11111111-aaaa-bbbb-cccc-000000000001"; P2 <- "11111111-aaaa-bbbb-cccc-000000000002"
existe_prueba <- function(client_id, id) id %in% c(P1, P2)
ADJ_ENTIDADES$pasivo$existe <- "existe_prueba"
UUID <- "a1b2c3d4-0000-4000-8000-00000f1c7101"
cfdi40 <- function(uuid = UUID) charToRaw(enc2utf8(sprintf(paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<cfdi:Comprobante xmlns:cfdi="http://www.sat.gob.mx/cfd/4" xmlns:tfd="http://www.sat.gob.mx/TimbreFiscalDigital" ',
  'Version="4.0" Serie="F" Folio="1001" Fecha="2026-11-20T10:00:00" Moneda="MXN" Total="11600.00" TipoDeComprobante="I">',
  '<cfdi:Emisor Rfc="EKU9003173C9" Nombre="EMPRESA FICTICIA EMISORA"/><cfdi:Receptor Rfc="xaxx010101000" Nombre="CLIENTE FICTICIO"/>',
  '<cfdi:Complemento><tfd:TimbreFiscalDigital Version="1.1" UUID="%s"/></cfdi:Complemento></cfdi:Comprobante>\n'), uuid)))
cfdi33 <- charToRaw(paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n<c:Comprobante xmlns:c="http://www.sat.gob.mx/cfd/3" Version="3.3" Folio="77" ',
  'Moneda="USD" Total="250.50" TipoDeComprobante="I"><c:Emisor Rfc="AAA010101AAA"/><c:Receptor Rfc="BBB010101BBB"/>',
  '<c:Complemento><t:TimbreFiscalDigital xmlns:t="http://www.sat.gob.mx/TimbreFiscalDigital" UUID="B2C3D4E5-1111-4111-9111-00000F1C7102"/></c:Complemento></c:Comprobante>'))
sin_timbre <- charToRaw('<?xml version="1.0"?><cfdi:Comprobante xmlns:cfdi="http://www.sat.gob.mx/cfd/4" Version="4.0" Total="1"><cfdi:Emisor Rfc="EKU9003173C9"/></cfdi:Comprobante>')
xml_comun <- charToRaw('<?xml version="1.0"?><estado><linea monto="10"/></estado>')
prep <- function(bytes, nombre = "cfdi_ficticio.xml") { f <- tempfile(fileext = ".xml"); writeBin(bytes, f); adjdoc_preparar(f, nombre, "archivo") }

cat("== 1. Leer el CFDI ==\n")
c4 <- adjdoc_leer_cfdi(cfdi40())
ok("1.1 CFDI 4.0: UUID en mayúsculas, RFCs, serie/folio, total, moneda, tipo",
   identical(c4$uuid, toupper(UUID)) && c4$rfc_emisor == "EKU9003173C9" && c4$rfc_receptor == "XAXX010101000" &&
     c4$serie_folio == "F1001" && c4$total == 11600 && c4$moneda == "MXN" && c4$tipo == "I" && c4$version == "4.0")
c3 <- adjdoc_leer_cfdi(cfdi33)
ok("1.2 CFDI 3.3 con otros prefijos (c:, t:): se lee igual", identical(c3$uuid, "B2C3D4E5-1111-4111-9111-00000F1C7102") &&
     c3$serie_folio == "77" && c3$total == 250.5 && c3$moneda == "USD")
ok("1.3 un CFDI sin timbre no es CFDI (NULL)", is.null(adjdoc_leer_cfdi(sin_timbre)))
ok("1.4 un XML cualquiera no es CFDI (NULL)", is.null(adjdoc_leer_cfdi(xml_comun)))
ok("1.5 un UUID con forma inválida no cuenta", is.null(adjdoc_leer_cfdi(cfdi40("no-es-un-uuid"))))

cat("== 2. Adjuntarlo ==\n")
s3_memoria_reset()
id1 <- q(documento_adjuntar(CID, "pasivo", P1, prep(cfdi40()), tipo_doc = "otro", por = "operador.ficticio"))
r <- q(documentos_de(CID, "pasivo", P1)); r <- r[r$id == id1, ]
refs <- jsonlite::fromJSON(r$refs)
ok("2.1 ref_uuid y refs guardan lo fiscal", identical(r$ref_uuid, toupper(UUID)) && identical(refs$cfdi_uuid, toupper(UUID)) &&
     refs$rfc_emisor == "EKU9003173C9" && refs$total == 11600 && refs$serie_folio == "F1001")
ok("2.2 se archiva como cfdi aunque se haya elegido otro tipo", identical(r$tipo_doc, "cfdi"))
ok("2.3 el XML se guarda tal cual", identical(q(documento_bytes(CID, id1))$bytes, cfdi40()))

cat("== 3. El mismo UUID ==\n")
n_obj <- function() length(grep("/adjuntos/", ls(.mem)))
antes <- n_obj()
e <- err(documento_adjuntar(CID, "pasivo", P1, prep(cfdi40(), "otra_copia.xml"), "cfdi", "operador.ficticio"))
ok("3.1 dos veces en el mismo pasivo: rechazado con su motivo", inherits(e, "adjdoc_error") &&
     identical(e$clave, "comun.adjuntos.error_cfdi_repetido"))
ok("3.2 ...y no se subió nada", n_obj() == antes)
ok("3.3 en otro pasivo sí se permite", is.null(err(documento_adjuntar(CID, "pasivo", P2, prep(cfdi40()), "cfdi", "operador.ficticio"))))
q(documento_papelera(CID, id1, "operador.ficticio"))
ok("3.4 tras mandar el primero a la papelera, se puede volver a adjuntar",
   is.null(err(documento_adjuntar(CID, "pasivo", P1, prep(cfdi40()), "cfdi", "operador.ficticio"))))

cat("== 4. Buscar por UUID ==\n")
b <- q(adjuntos_buscar_ref(CID, "cfdi_uuid", tolower(UUID)))
ok("4.1 encuentra los vivos con ese UUID en cualquier entidad (P1 y P2), sin el de la papelera",
   nrow(b) == 2L && setequal(b$entidad_id, c(P1, P2)) && !id1 %in% b$id)
ok("4.2 con incluir_papelera aparece también el de la papelera",
   id1 %in% q(adjuntos_buscar_ref(CID, "cfdi_uuid", UUID, incluir_papelera = TRUE))$id)
ok("4.3 por otra clave de refs (rfc_emisor)", nrow(q(adjuntos_buscar_ref(CID, "rfc_emisor", "EKU9003173C9"))) == 2L)

cat("== 5. Un XML común ==\n")
idx <- q(documento_adjuntar(CID, "pasivo", P2, prep(xml_comun, "estado.xml"), "estado_cuenta", "operador.ficticio"))
rx <- q(documentos_de(CID, "pasivo", P2)); rx <- rx[rx$id == idx, ]
ok("5.1 se guarda como XML con su tipo elegido, refs {} y sin ref_uuid",
   rx$ext == "xml" && rx$tipo_doc == "estado_cuenta" && rx$refs == "{}" && is.na(rx$ref_uuid))

cat("== 6. Filas viejas ==\n")
vieja <- .normalize(tibble::tibble(id = "x", entidad = "pasivo", entidad_id = P1, refs = "{}"), .schema_adjuntos_documentos)
ok("6.1 una fila sin ref_uuid se lee con NA", "ref_uuid" %in% names(vieja) && is.na(vieja$ref_uuid))

cat("== 7. La tarjeta ==\n")
r2 <- q(documentos_de(CID, "pasivo", P2)); r2 <- r2[!is.na(r2$ref_uuid), ][1, ]
t <- paste(as.character(.adjdoc_cfdi_resumen(r2)), collapse = "")
ok("7.1 dice el primer bloque del UUID, el RFC emisor y el total con su moneda (sin «$»)",
   grepl(substr(toupper(UUID), 1, 8), t, fixed = TRUE) && grepl("EKU9003173C9", t, fixed = TRUE) &&
     grepl("11,600.00", t, fixed = TRUE) && grepl("MXN", t, fixed = TRUE) && !grepl("$", t, fixed = TRUE))
ok("7.2 un documento que no es CFDI no lleva resumen", is.null(.adjdoc_cfdi_resumen(rx)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
