# =============================================================================
# tests/test_t93_pantalla.R
# T93 (4): the upload in "Archivos de pago" (.btr_servidor), on the real
# server (testServer) with made-up data and no S3:
#   1  without can_confirm_payments nothing is read nor confirmed (server)
#   2  path A: the preview comes from the server; confirming calls the
#      Agenda's own Confirmar (.ph_confirmar_ap) for ITS Agenda row only,
#      with clave de rastreo, authorisation and respuesta_id; the report is
#      recorded with a T72 event (accion subir_reporte_banco) and no whole
#      account; an Editado / Agregado / rejected payment is never confirmed,
#      even if the browser says "confirmar" for it
#   3  path B: nothing is confirmed unless the user ticks the suggestion
#   4  the button shows only with the permission (UI), and the dictionary
#      has every text in es and en
#
# Run from project root:  Rscript tests/test_t93_pantalla.R
# =============================================================================
cat("=== test_t93_pantalla ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/monedas.R",
            "R/ppl_generator.R", "R/bancos_respuesta.R"))
  source(f)
for (e in parse("R/pagos_generados_persistence.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "parse_pago_beneficiarios"))
    eval(e, globalenv())
evento_parte_de <- function(accion, modulo = NULL, usuario = NULL, home = NULL)
  list(.parte_de = accion, modulo = modulo, usuario = usuario, home = home)

# ── Stubs: what the server would write ──────────────────────────────────────
PUEDE <- TRUE
puede_o_avisa <- function(shared, permiso, ...) PUEDE && identical(permiso, "can_confirm_payments")
puede <- function(usuario, permiso) PUEDE
CONF <- list(); REG <- NULL; AVISOS <- character()
.ph_confirmar_ap <- function(shared, emp, cur, rows, cuenta_sel, fecha_pago, .parte_ap, .ph_fallido, .link_counter, extra = NULL) {
  CONF[[length(CONF) + 1L]] <<- list(emp = emp, cur = cur, ids = rows$id, fecha = fecha_pago, extra = extra, parte = .parte_ap)
  bc <- shared$bancos_confirmados()
  shared$bancos_confirmados(rbind(bc, data.frame(confirmacion_id = paste0("C-", extra$agenda_item_id),
                                                 respuesta_id = extra$respuesta_id, stringsAsFactors = FALSE)))
  invisible(NULL)
}
modificar_respuestas_banco <- function(client_id, filas, evento = NULL) { REG <<- list(cid = client_id, filas = filas, evento = evento); list(nuevas = filas) }
leer_respuestas_banco <- function(client_id) data.frame(llave = character())
log_action <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)

GEN <- data.frame(id = "G1", ts = as.POSIXct("2026-11-24 09:00", tz = "America/Mexico_City"), cuenta_origen = "073543010201",
                  tipo = "pago_ppl", archivo = "pagos_NCS_24112026_001.txt",
                  beneficiarios = as.character(jsonlite::toJSON(list(
                    list(alias = "UNO", clabe = "012180000000000001", banco = "012", importe = 100, concepto = "F",
                         agenda_item_id = "A1", documento = "D1", fecha_factura = "2026-11-01"),
                    list(alias = "DOS", clabe = "012180000000000002", banco = "012", importe = 200, concepto = "F",
                         agenda_item_id = "A2", documento = "D2", fecha_factura = "2026-11-01")), auto_unbox = TRUE)),
                  stringsAsFactors = FALSE)
read_pagos_generados <- function(client_id, ...) GEN
AGENDA <- data.frame(id = c("A1", "A2", "B1"), ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = c("D1", "D2", "D3"),
                     Parte = c("UNO SA", "DOS SA", "CINCO SA"), Codigo = "", Importe = c(100, 200, 500), stringsAsFactors = FALSE)
safe_load_pagar_hoy <- function(...) AGENDA

ENC <- paste0("#,Fecha,Hora de Operación,Cuenta Origen,Nombre del Ordenante,Cuenta Destino,Tipo Cuenta Destino,",
              "Banco Destino,Nombre del Beneficiario,Alias del Beneficiario,RFC,Correo del beneficiario,Transacción,",
              "Importe de la Operación,IVA,Divisa,Clasificación de la Operación,Concepto de Pago,Referencia,",
              "Comisión,IVA Comisión,No. de Autorización / Clave de Rastreo,Capturó,Autorizó,",
              "Estatus de Transferencia,Mensaje")
linea <- function(n, cuenta, alias, imp, aut, registro = "Correcto", estatus = "Aplicado", divisa = "MN")
  paste(n, "24112026", "101500", "73543010201", "EMPRESA SA", cuenta, "CLABE", "BBVA", "PROV", alias, "XAXX010101000", "",
        "Transferencia Interbancaria SPEI", imp, "0.00", divisa, "", "F", "123456", "0.00", "0.00", paste0(aut, "/BB", aut),
        "U 24-Nov-2026", "U 24-Nov-2026 Horas", estatus, "", "", "", "Bajionet", registro, sep = ",")
archivo_reporte <- function(nombre_lote, ...) {
  f <- tempfile(fileext = ".txt")
  writeLines(c(paste0("EMPRESA SA,1234567,24112026,152859,", nombre_lote, ",24112026,152500,2,2,300.00,Aplicado"), ENC, ...), f, useBytes = TRUE)
  data.frame(name = "TransferenciasporArchivo_x.txt", size = 1, type = "text/plain", datapath = f, stringsAsFactors = FALSE)
}

correr <- function(subida, decisiones = NULL, prov = NULL) {
  CONF <<- list(); REG <<- NULL; AVISOS <<- character()
  shared <- list(effective_client_id = function() "acme", current_user = function() "luis", home_client_id = function() "acme",
                 usuario = reactiveVal(list(user = "luis")), bancos_confirmados = reactiveVal(data.frame(confirmacion_id = character(), respuesta_id = character())),
                 proveedores_db = reactiveVal(prov), parte_alias_map_db = reactiveVal(NULL))
  res <- list()
  srv <- function(input, output, session) { h <- .btr_servidor(input, output, session, shared); session$userData$h <- h }
  invisible(capture.output(testServer(srv, {
    session$setInputs(btr_subir = 1)
    session$setInputs(btr_archivo = subida)
    res$prep <<- session$userData$h$prep()
    if (!is.null(decisiones))
      session$setInputs(`btr_t-opciones` = list(ids = names(decisiones), valores = unname(decisiones)))
    session$setInputs(btr_confirmar = 1)
  })))
  res
}

# ── 1 · Without the permission ──────────────────────────────────────────────
PUEDE <- FALSE
r <- correr(archivo_reporte("pagos_NCS_24112026_001.txt", linea(1, "012180000000000001", "UNO", "100.00", "1111111111111")))
ok("1 · sin can_confirm_payments: no lee, no confirma, no registra", is.null(r$prep) && !length(CONF) && is.null(REG))
PUEDE <- TRUE

# ── 2 · Path A ──────────────────────────────────────────────────────────────
up <- archivo_reporte("pagos_NCS_24112026_001.txt",
                      linea(1, "012180000000000001", "UNO", "100.00", "1111111111111"),
                      linea(2, "012180000000000002", "DOS", "250.00", "2222222222222", registro = "Editado"))
r <- correr(up, decisiones = c(L4 = "confirmar"))   # the browser asks to confirm the Editado one too
ok("2 · la vista previa la arma el servidor: camino A, UNO \"confirmar\", DOS \"editado\"",
   r$prep$camino == "A" && identical(r$prep$filas$estado, c("confirmar", "editado")))
ok("2 · confirma por el Confirmar de la Agenda SOLO la fila de UNO (A1); el Editado no, aunque el navegador lo pida",
   length(CONF) == 1L && identical(CONF[[1]]$ids, "A1") && CONF[[1]]$emp == "NCS" && CONF[[1]]$cur == "MXN")
ok("2 · con su clave de rastreo, autorización y respuesta_id; fecha del banco; un evento /parte de subir_reporte_banco",
   CONF[[1]]$extra$clave_rastreo == "BB1111111111111" && CONF[[1]]$extra$autorizacion == "1111111111111" &&
     !is.na(CONF[[1]]$extra$respuesta_id) && identical(CONF[[1]]$fecha, as.Date("2026-11-24")) &&
     identical(CONF[[1]]$parte$.parte_de, "subir_reporte_banco"))
ok("2 · el reporte se registra con evento T72 (subir_reporte_banco) y el cliente explícito",
   !is.null(REG) && REG$cid == "acme" && REG$evento$accion == "subir_reporte_banco" && nrow(REG$filas) == 2L)
ok("2 · el registro liga UNO con su confirmación; el Editado queda sin confirmación",
   REG$filas$confirmacion_id[1] == "C-A1" && is.na(REG$filas$confirmacion_id[2]) && is.na(REG$filas$agenda_item_id[2]))
ok("2 · ninguna cuenta completa en el registro ni en el evento",
   !any(grepl("[0-9]{6,}", REG$filas$cuenta_destino)) && !grepl("012180000000000001", paste(deparse(REG$evento), collapse = "")))

# ── 3 · Path B ──────────────────────────────────────────────────────────────
prov <- data.frame(nombre = "CINCO SA", alias = "CINCO", codigo = "", clabe = "014180000000000005", stringsAsFactors = FALSE)
upb <- archivo_reporte("pagos_24112026_001NCS", linea(1, "014180000000000005", "X", "500.00", "5555555555555"))
r <- correr(upb, prov = prov)
ok("3 · camino B: \"sugerido\", y sin marcar no confirma nada (pero sí registra el reporte)",
   r$prep$camino == "B" && r$prep$filas$estado == "sugerido" && !length(CONF) && nrow(REG$filas) == 1L)
r <- correr(upb, decisiones = c(L3 = "confirmar"), prov = prov)
ok("3 · marcado por el usuario: confirma su fila sugerida (B1)", length(CONF) == 1L && identical(CONF[[1]]$ids, "B1"))

# ── 3b · The currency: the report's must be the Agenda row's ────────────────
r <- correr(archivo_reporte("pagos_NCS_24112026_001.txt", linea(1, "012180000000000001", "UNO", "100.00", "1111111111111", divisa = "USD")),
            decisiones = c(L3 = "confirmar"))
ok("3b · USD en el reporte, fila MXN: \"moneda_distinta\", y no se confirma aunque el navegador lo pida",
   r$prep$filas$estado == "moneda_distinta" && !length(CONF) && REG$filas$estado == "moneda_distinta")
r <- correr(archivo_reporte("pagos_NCS_24112026_001.txt", linea(1, "012180000000000001", "UNO", "100.00", "1111111111111", divisa = "XYZ")))
ok("3b · divisa no reconocida: \"moneda_distinta\", no se confirma", r$prep$filas$estado == "moneda_distinta" && !length(CONF))
AGENDA_BASE <- AGENDA
AGENDA$Moneda <- "mxn"   # the same currency, written otherwise: shows WHICH value is kept
r <- correr(archivo_reporte("pagos_NCS_24112026_001.txt", linea(1, "012180000000000001", "UNO", "100.00", "1111111111111")))
ok("3b · contraprueba: misma moneda -> se confirma y guarda la MONEDA DE LA FILA (\"mxn\"), no la del reporte (\"MXN\")",
   length(CONF) == 1L && identical(CONF[[1]]$cur, "mxn"))
AGENDA <- AGENDA_BASE

# ── 4 · UI and dictionary ───────────────────────────────────────────────────
src <- readLines("R/pagos_audit_viewer_module.R", warn = FALSE, encoding = "UTF-8")
ok("4 · el botón se dibuja solo con el permiso (y el servidor lo revisa otra vez)",
   any(grepl("if (.btr_puede())", src, fixed = TRUE)) && any(grepl('puede(shared$usuario(), "can_confirm_payments")', src, fixed = TRUE)))
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
usadas <- unique(c(paste0("tablas.archivos_pago.btr.estado.", BTR_ESTADOS),
                   paste0("tablas.archivos_pago.btr.camino_", c("a", "b")),
                   regmatches(paste(readLines("R/bancos_respuesta.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                              gregexpr("tablas\\.archivos_pago\\.btr\\.[a-z_]+(\\.[a-z_]+)?(?=\")",
                                       paste(readLines("R/bancos_respuesta.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n"), perl = TRUE))[[1]]))
usadas <- usadas[!grepl("[._]$", usadas)]
ok("4 · cada texto de la pantalla está en es y en", all(usadas %in% names(es)) && all(usadas %in% names(en)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
