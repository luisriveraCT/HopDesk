# =============================================================================
# tests/test_t106_adjuntos.R
# T106 phase 1: the generic attachments layer (R/adjuntos_registro.R,
# R/adjuntos_almacen.R, R/adjuntos_documentos.R), with Pasivos as its first
# entity. In-memory S3 (tests/helpers/s3_memoria.R); invented data only.
#   1  what is accepted is decided by the bytes (exe, svg, html, DTD/ENTITY
#      XML, NUL in a CSV are refused; PNG, PDF, CSV, a CFDI-like XML pass)
#   2  canonical CSV: RFC 4180, UTF-8 without BOM, CRLF, round trip exact,
#      formulas stored as given
#   3  attach: bytes first under a server-made key, then the row (sha256,
#      refs, comprimido, almacen); a pasted table stored as CSV
#   4  limits, unknown entity, invalid document kind: nothing stored
#   5  trash is a mark (the object stays); restore respects the limit
#   6  reads are strict (never empty) and keys stay under the client's prefix
#   7  every write logs its event (T72), the four actions are C (T87)
#   8  permissions: tier gate to see, can_manage_pasivos to change
#   9  the table is grow-only (.S3_SOLO_CRECEN); the registry is consistent
#
# Run from project root:  Rscript tests/test_t106_adjuntos.R
# =============================================================================
cat("=== test_t106_adjuntos ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/app_audit.R", "R/adjuntos_s3.R",
            "R/adjuntos_registro.R", "R/adjuntos_almacen.R", "R/adjuntos_documentos.R", "R/adjuntos_cfdi.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
s3_memoria_reinstalar()
q <- function(expr) invisible(suppressWarnings(suppressMessages(expr)))
err <- function(expr) tryCatch({ q(expr); NULL }, error = function(e) e)

CID <- "demo"; P1 <- "11111111-aaaa-bbbb-cccc-000000000001"; P2 <- "11111111-aaaa-bbbb-cccc-000000000002"
pasivos_existentes <- c(P1, P2)
existe_prueba <- function(client_id, id) id %in% pasivos_existentes
ADJ_ENTIDADES$pasivo$existe <- "existe_prueba"   # the registry stores names: a test swaps one
archivo <- function(bytes, ext) { f <- tempfile(fileext = paste0(".", ext)); writeBin(bytes, f); f }
txt <- function(s) charToRaw(enc2utf8(s))
png <- local({ f <- tempfile(fileext = ".png"); magick::image_write(magick::image_blank(20, 10, "red"), f, format = "png")
               readBin(f, "raw", file.info(f)$size) })
pdf <- txt("%PDF-1.4\n1 0 obj << /Type /Catalog >> endobj\ntrailer << >>\n%%EOF\n")
cfdi <- txt('<?xml version="1.0" encoding="UTF-8"?>\n<cfdi:Comprobante Version="4.0" Total="100.00"><cfdi:Complemento><tfd:TimbreFiscalDigital UUID="AAAAAAAA-0000-0000-0000-FICTICIO0001"/></cfdi:Complemento></cfdi:Comprobante>\n')
prep <- function(bytes, ext, nombre = paste0("doc_ficticio.", ext), origen = "archivo") adjdoc_preparar(archivo(bytes, ext), nombre, origen)

cat("== 1. Lo que se acepta lo dicen los bytes ==\n")
ok("1.1 PNG: aceptado y recodificado", identical(q(prep(png, "png"))$ext, "png"))
ok("1.2 PDF: aceptado", identical(prep(pdf, "pdf")$ext, "pdf"))
ok("1.3 un XML tipo CFDI: aceptado como archivo XML", identical(prep(cfdi, "xml")$ext, "xml") && identical(prep(cfdi, "xml")$clase, "archivo"))
ok("1.4 un CSV: aceptado como tabla", identical(prep(txt("a,b\n1,2\n"), "csv")$clase, "tabla"))
for (caso in list(list("1.5 un .exe renombrado a .pdf", c(txt("MZ"), as.raw(rep(0, 60))), "pdf"),
                  list("1.6 un SVG (puede llevar script)", txt('<svg xmlns="http://www.w3.org/2000/svg"><script>alert(1)</script></svg>'), "xml"),
                  list("1.7 un HTML renombrado a .csv", txt("<html><body>hola</body></html>"), "csv"),
                  list("1.8 XML con DOCTYPE (XXE)", txt('<?xml version="1.0"?><!DOCTYPE x [<!ENTITY e SYSTEM "file:///etc/passwd">]><x>&e;</x>'), "xml"),
                  list("1.9 XML con ENTITY suelta", txt('<x><!ENTITY e "y"></x>'), "xml"),
                  list("1.10 un CSV con un byte NUL", c(txt("a,b\n"), as.raw(0), txt("1,2\n")), "csv"),
                  list("1.11 texto que no es UTF-8", as.raw(c(0x61, 0xff, 0xfe, 0x2c, 0x62)), "csv")))
  ok(paste(caso[[1]], "se rechaza"), inherits(err(prep(caso[[2]], caso[[3]])), "adjdoc_error"))
ok("1.12 una imagen pegada que no es imagen se rechaza", inherits(err(prep(pdf, "pdf", origen = "pegado")), "adjdoc_error"))
ok("1.13 los errores salen en palabras de adjuntos, no de requisiciones",
   identical(err(prep(c(txt("MZ"), as.raw(rep(0, 60))), "pdf"))$clave, "comun.adjuntos.error_tipo"))

cat("== 2. CSV canónico ==\n")
celdas <- matrix(c("Concepto", "Monto", "Renta, oficina \"A\"", "=1+1", "línea\ndos", "-150.00", "ñandú", ""), ncol = 2, byrow = TRUE)
b <- adjdoc_csv_escribir(celdas)
ok("2.1 sin BOM, en UTF-8, cada fila termina en CRLF",
   !identical(as.integer(b[1:3]), c(0xEFL, 0xBBL, 0xBFL)) && validUTF8(rawToChar(b)) &&
     endsWith(rawToChar(b), "\r\n") && length(gregexpr("\r\n", rawToChar(b))[[1]]) == 4L)
ok("2.2 comillas solo donde hacen falta, y las internas dobladas",
   startsWith(rawToChar(b), "Concepto,Monto\r\n\"Renta, oficina \"\"A\"\"\",=1+1\r\n"))
vuelta <- .adjdoc_csv_leer(rawToChar(b))
ok("2.3 ida y vuelta exacta (acentos, comas, comillas, salto de línea, vacío)", identical(vuelta, celdas))
ok("2.4 la fórmula se guarda tal cual; neutralizar solo al copiar/exportar",
   vuelta[2, 2] == "=1+1" && neutralizar_celdas(vuelta[2, 2]) == "'=1+1")

cat("== 3. Adjuntar ==\n")
s3_memoria_reset()
p <- prep(pdf, "pdf", "contrato ficticio.pdf")
id1 <- q(documento_adjuntar(CID, "pasivo", P1, p, "contrato", por = "operador.ficticio"))
df <- q(documentos_de(CID, "pasivo", P1)); r <- df[df$id == id1, ]
llave <- paste0(CID, "/adjuntos/pasivo/", P1, "/", id1, ".pdf")
ok("3.1 los bytes viven en la llave que arma el servidor", identical(r$llave, llave) && exists(llave, envir = .mem))
ok("3.2 la fila: entidad, tipo, clase, sha256 de lo guardado, refs {}, almacen s3, comprimido no",
   r$entidad == "pasivo" && r$entidad_id == P1 && r$tipo_doc == "contrato" && r$clase == "archivo" &&
     r$sha256 == paste(sprintf("%02x", as.integer(openssl::sha256(pdf))), collapse = "") && nchar(r$sha256) == 64L && r$refs == "{}" && r$almacen == "s3" && r$comprimido == "no" &&
     r$nombre == "contrato ficticio.pdf" && r$mime == "application/pdf")
ok("3.3 documento_bytes devuelve exactamente lo subido", identical(q(documento_bytes(CID, id1))$bytes, pdf))
pp <- adjdoc_preparar_pegado("Fecha\tMonto\n2026-11-01\t1,000.00\n2026-12-01\t1,000.00")
idt <- q(documento_tabla_agregar(CID, "pasivo", P1, pp, "Calendario ficticio", TRUE, "tabla_amortizacion", por = "operador.ficticio"))
rt <- q(documentos_de(CID, "pasivo", P1)); rt <- rt[rt$id == idt, ]
ok("3.4 una tabla pegada se guarda como CSV canónico, con su tamaño y encabezado",
   rt$ext == "csv" && rt$mime == "text/csv" && rt$clase == "tabla" && rt$filas == 3L && rt$columnas == 2L && isTRUE(rt$encabezado) &&
     identical(q(documento_tabla(CID, idt))$celdas, pp$celdas))
idc <- q(documento_adjuntar(CID, "pasivo", P1, prep(txt("a;b\n"), "csv", "subido.csv"), "otro", por = "operador.ficticio"))
ok("3.5 un CSV subido también queda como tabla CSV", identical(q(documentos_de(CID, "pasivo", P1))$clase[q(documentos_de(CID, "pasivo", P1))$id == idc], "tabla"))

cat("== 4. Límites y entradas malas: nada se guarda ==\n")
n_obj <- function() length(grep("/adjuntos/", ls(.mem), value = TRUE))
antes_obj <- n_obj(); antes_filas <- nrow(q(.adjdoc_leer(CID)))
ok("4.1 un pasivo que no existe", inherits(err(documento_adjuntar(CID, "pasivo", "99999999-0000-0000-0000-000000000000", p, "contrato", "x")), "adjdoc_error"))
ok("4.2 un tipo de documento que la entidad no admite", inherits(err(documento_adjuntar(CID, "pasivo", P1, p, "orden_compra", "x")), "adjdoc_error"))
ok("4.3 una entidad que no está en el registro", !is.null(err(documento_adjuntar(CID, "factura", P1, p, "cfdi", "x"))))
ok("4.4 un id con texto de usuario", !is.null(err(documento_adjuntar(CID, "pasivo", "../otro", p, "contrato", "x"))))
ok("4.5 sin client_id", !is.null(err(documento_adjuntar("", "pasivo", P1, p, "contrato", "x"))))
ADJ_ENTIDADES$pasivo$max_vivos <- 1L
ok("4.6 el tope de archivos por entidad", inherits(err(documento_adjuntar(CID, "pasivo", P1, p, "contrato", "x")), "adjdoc_error"))
ok("4.7 ...y en ninguno de esos casos se subió un objeto ni se escribió una fila",
   n_obj() == antes_obj && nrow(q(.adjdoc_leer(CID))) == antes_filas)

cat("== 5. Papelera ==\n")
q(documento_papelera(CID, id1, por = "operador.ficticio"))
ok("5.1 es una marca: la fila sigue, el objeto sigue, ya no está en la lista viva",
   exists(llave, envir = .mem) && id1 %in% q(.adjdoc_leer(CID))$id && !id1 %in% q(documentos_de(CID, "pasivo", P1))$id &&
     id1 %in% q(documentos_de(CID, "pasivo", P1, incluir_papelera = TRUE))$id)
id_otro <- q(documento_adjuntar(CID, "pasivo", P1, prep(png, "png"), "otro", por = "operador.ficticio"))
ok("5.2 restaurar respeta el tope (1 vivo): se niega", inherits(err(documento_restaurar(CID, id1, "operador.ficticio")), "adjdoc_error"))
ADJ_ENTIDADES$pasivo$max_vivos <- 20L
q(documento_restaurar(CID, id1, por = "operador.ficticio"))
ok("5.3 con lugar, restaura", id1 %in% q(documentos_de(CID, "pasivo", P1))$id)

cat("== 6. Lecturas estrictas y llaves propias ==\n")
.mem_falla <- paste0(CID, "/", ADJDOC_KEY)
ok("6.1 una lectura fallida es error, nunca una lista vacía", !is.null(err(documentos_de(CID, "pasivo", P1))))
.mem_falla <- character()
ok("6.2 una llave de otro cliente no se lee", !is.null(err(almacen_leer("s3", CID, paste0("otro/adjuntos/pasivo/", P1, "/x.pdf")))))
ok("6.3 adjdoc_llave rechaza entidad, id o extensión inválidos",
   !is.null(err(adjdoc_llave(CID, "nada", P1, P2, "pdf"))) && !is.null(err(adjdoc_llave(CID, "pasivo", "a/b", P2, "pdf"))) &&
     !is.null(err(adjdoc_llave(CID, "pasivo", P1, P2, "exe"))))
ok("6.4 cada llave guardada vive bajo <cliente>/adjuntos/pasivo/<id>/",
   all(startsWith(q(.adjdoc_leer(CID))$llave, paste0(CID, "/adjuntos/pasivo/"))))

cat("== 7. Registro (T72 / T87) ==\n")
# The rows may still be queued, or already flushed to the client's log chunk
# (later::later, ~2 s): look in both.
acciones <- c(tryCatch(.audit_pending[[CID]]$action, error = function(e) character()),
              tryCatch(.mem[[.audit_current_key(CID)]]$action, error = function(e) character()))
ok("7.1 las cuatro acciones quedaron en el registro del cliente",
   all(c("adjuntar_documento", "pegar_tabla_documento", "papelera_documento", "restaurar_documento") %in% acciones),
   paste(unique(acciones), collapse = ", "))
cat87 <- readLines("tests/test_t87_catalogo.R", warn = FALSE)
ok("7.2 las cuatro son C en el catálogo T87", all(vapply(c("adjuntar_documento", "pegar_tabla_documento", "papelera_documento", "restaurar_documento"),
   function(a) any(grepl(paste0('"', a, '"'), cat87, fixed = TRUE)), logical(1))))

cat("== 8. Permisos ==\n")
# Tiers chosen by the registry's own gate (nivel_pestanas() is NULL for a
# tier that sees every tab), never written by hand.
ve_psv  <- function(t) isTRUE(nivel_reactivo_permitido(t, "pasivos_liabilities_db"))
con_psv <- Filter(function(t) ve_psv(t) && isTRUE(permisos_resueltos(t, NULL)[["can_manage_pasivos"]]), niveles_cliente())
sin_psv <- Filter(function(t) !ve_psv(t), niveles_cliente())
u <- function(t) list(tier = t, permisos = permisos_resueltos(t, NULL))
ok("8.1 un nivel con Pasivos y el permiso ve y edita", length(con_psv) && adj_puede_ver(u(con_psv[1]), "pasivo") && adj_puede_editar(u(con_psv[1]), "pasivo"))
ok("8.2 un nivel sin la pestaña de Pasivos no ve nada", length(sin_psv) && !adj_puede_ver(u(sin_psv[1]), "pasivo") && !adj_puede_editar(u(sin_psv[1]), "pasivo"))
v <- u(con_psv[1]); v$permisos["can_manage_pasivos"] <- FALSE
ok("8.3 sin can_manage_pasivos ve pero no edita", adj_puede_ver(v, "pasivo") && !adj_puede_editar(v, "pasivo"))
ok("8.4 un nivel desconocido: nada (falla cerrada)", !adj_puede_ver(list(tier = "no-existe", permisos = list()), "pasivo"))

cat("== 9. Red de S3 y registro ==\n")
ok("9.1 adjuntos_documentos.rds solo crece (.S3_SOLO_CRECEN)", ADJDOC_KEY %in% .S3_SOLO_CRECEN())
ok("9.2 cada tipo de documento de una entidad está en el catálogo y tiene texto en es y en",
   all(vapply(ADJ_ENTIDADES, function(e) all(e$tipos_doc %in% ADJ_TIPOS_DOC), logical(1))) &&
     all(paste0("comun.adjuntos.tipo_doc.", ADJ_TIPOS_DOC) %in% names(jsonlite::fromJSON("www/i18n/es.json"))) &&
     all(paste0("comun.adjuntos.tipo_doc.", ADJ_TIPOS_DOC) %in% names(jsonlite::fromJSON("www/i18n/en.json"))))
ok("9.3 la función existe() real de pasivo está definida", exists(".adj_pasivo_existe", mode = "function"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
