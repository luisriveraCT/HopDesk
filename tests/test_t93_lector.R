# =============================================================================
# tests/test_t93_lector.R
# T93: leer_transferencias_bajionet() over BajíoNet's "Transferencias por
# archivo" report. The data below is MADE UP, with the shape of the real
# reports of 2026-11-24 (two summary variants, 26 named + 4 unnamed fields,
# DDMMAAAA dates, SPEI "<13>/BB<13>", Registro Correcto/Editado/Agregado).
#   1  both summary variants: the batch's file name, date and status;
#   2  one row per payment, by column name; accounts digits only; the
#      authorisation and the clave de rastreo split; trailing empty fields kept;
#   3  btr_estado / btr_registro: only "Aplicado" with no Canceló/Devolución
#      is "aplicado"; Registro as the bank wrote it;
#   4  a line it cannot read is reported with its number, never truncated
#      (a comma inside a name, a bad amount, a bad date); a header that is not
#      the report's reads nothing;
#   5  Latin-1 and a UTF-8 BOM read the same as plain UTF-8.
#
# Run from project root:  Rscript tests/test_t93_lector.R
# =============================================================================
cat("=== test_t93_lector ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
source("R/bancos_respuesta.R")

ENC <- paste0("#,Fecha,Hora de Operación,Cuenta Origen,Nombre del Ordenante,Cuenta Destino,Tipo Cuenta Destino,",
              "Banco Destino,Nombre del Beneficiario,Alias del Beneficiario,RFC,Correo del beneficiario,Transacción,",
              "Importe de la Operación,IVA,Divisa,Clasificación de la Operación,Concepto de Pago,Referencia,",
              "Comisión,IVA Comisión,No. de Autorización / Clave de Rastreo,Capturó,Autorizó,",
              "Estatus de Transferencia,Mensaje")
RES_ARCHIVO <- "EMPRESA PRUEBA SA DE CV,1234567,24112026,152859,pagos_EMP_24112026_001.txt,24112026,152500,3,3,4567.89,Aplicado"
RES_FILTRO  <- "EMPRESA PRUEBA SA DE CV,1234567,24112026,152832,Por Archivo,Todos,Todas las cuentas origen,Todas las cuentas destino,pagos_EMP_24112026_001.txt,Todas,,"
pago <- function(n, cuenta, tipo, banco, alias, trans, imp, ref, rastreo, estatus = "Aplicado", registro = "Correcto",
                 cancelo = "", nombre = "PROVEEDOR UNO SA")
  paste(n, "24112026", "101500", "11111111111", "EMPRESA PRUEBA SA DE CV", cuenta, tipo, banco, nombre, alias,
        "XAXX010101000", "", trans, imp, "0.00", "MN", "", "F-100", ref, "0.00", "0.00", rastreo,
        "USUARIO UNO 24-Nov-2026 10:10:00", "USUARIO UNO 24-Nov-2026 10:12:00 Horas", estatus, "", cancelo, "",
        "Bajionet", registro, sep = ",")
P1 <- pago(1, "012180000000000001", "CLABE", "BBVA", "PROVUNO", "Transferencia Interbancaria SPEI", "1234.56",
           "123456", "1234567890123/BB1234567890123", registro = "Correcto")
P2 <- pago(2, "000000000002", "Cuenta BB", "BAJIO", "PROVDOS", "Transferencia a Tercero BanBajío", "3000.00",
           "1234567", "2234567890123", registro = "Editado")
P3 <- pago(3, "014180000000000003", "CLABE", "SANTANDER", "PROVTRES", "Transferencia Interbancaria SPEI", "333.33",
           "", "3234567890123/BB3234567890123", estatus = "Rechazado", registro = "Agregado")
P4 <- pago(4, "014180000000000004", "CLABE", "SANTANDER", "PROVCUAT", "Transferencia Interbancaria SPEI", "10.00",
           "", "4234567890123/BB4234567890123", cancelo = "USUARIO DOS")

# ── 1 · Both summary variants ───────────────────────────────────────────────
a <- leer_transferencias_bajionet(lineas = c(RES_ARCHIVO, ENC, P1, P2, P3, P4))
b <- leer_transferencias_bajionet(lineas = c(RES_FILTRO, ENC, P1, P2, P3, P4))
ok("1 · \"por archivo\": nombre del archivo, fecha (DDMMAAAA) y estatus del lote",
   a$lote$variante == "por_archivo" && a$lote$archivo == "pagos_EMP_24112026_001.txt" &&
     identical(a$lote$fecha, as.Date("2026-11-24")) && a$lote$estatus == "Aplicado")
ok("1 · \"filtrado\": el mismo archivo y fecha, sin estatus", b$lote$variante == "filtrado" &&
     b$lote$archivo == "pagos_EMP_24112026_001.txt" && identical(b$lote$fecha, as.Date("2026-11-24")) && is.na(b$lote$estatus))
ok("1 · las dos variantes dan los mismos pagos", identical(a$pagos, b$pagos))

# ── 2 · The payments ────────────────────────────────────────────────────────
p <- a$pagos
ok("2 · 4 pagos, 0 problemas", nrow(p) == 4L && nrow(a$problemas) == 0L)
ok("2 · fecha, importe y comisión numéricos; cuenta destino solo dígitos (18 y 12)",
   identical(p$fecha[1], as.Date("2026-11-24")) && identical(p$importe, c(1234.56, 3000, 333.33, 10)) &&
     identical(nchar(p$cuenta_destino), c(18L, 12L, 18L, 18L)) && all(p$comision == 0))
ok("2 · SPEI: autorización y clave de rastreo separadas; interna: solo autorización",
   p$autorizacion[1] == "1234567890123" && p$clave_rastreo[1] == "BB1234567890123" &&
     p$autorizacion[2] == "2234567890123" && is.na(p$clave_rastreo[2]))
ok("2 · los 4 campos sin encabezado: Canceló, Devolución, Canal, Registro",
   p$canal[1] == "Bajionet" && p$registro[2] == "Editado" && p$cancelo[4] == "USUARIO DOS" && p$devolucion[1] == "")
ok("2 · referencia como texto (6, 7 dígitos o vacía) y la línea de cada pago",
   identical(p$referencia, c("123456", "1234567", "", "")) && identical(p$linea, 3:6))

# ── 3 · What the bank's words mean ──────────────────────────────────────────
ok("3 · estado: Aplicado = aplicado; Rechazado o con Canceló = no_aplicado",
   identical(btr_estado(p), c("aplicado", "aplicado", "no_aplicado", "no_aplicado")))
ok("3 · registro: correcto / editado / agregado", identical(btr_registro(p)[1:3], c("correcto", "editado", "agregado")))

# ── 4 · Lines it cannot read ────────────────────────────────────────────────
coma <- pago(5, "012180000000000005", "CLABE", "BBVA", "PROVCOMA", "Transferencia Interbancaria SPEI", "50.00",
             "", "5234567890123/BB5234567890123", nombre = "PROVEEDOR, CON COMA SA")
malimp <- sub(",1234.56,", ",12x4.56,", P1, fixed = TRUE)
malfec <- sub(",24112026,", ",2026-11-24,", P1, fixed = TRUE)
r <- leer_transferencias_bajionet(lineas = c(RES_ARCHIVO, ENC, P1, coma, malimp, malfec, P2))
ok("4 · una coma dentro de un nombre: la línea se reporta (\"campos\"), no se corre ni se trunca",
   any(r$problemas$linea == 4L & r$problemas$codigo == "campos") && !"PROVCOMA" %in% r$pagos$alias)
ok("4 · importe y fecha ilegibles se reportan con su línea",
   any(r$problemas$linea == 5L & r$problemas$codigo == "importe") &&
     any(r$problemas$linea == 6L & r$problemas$codigo == "fecha"))
ok("4 · las líneas buenas se leen igual (P1 y P2)", identical(r$pagos$alias, c("PROVUNO", "PROVDOS")))
ok("4 · un encabezado que no es el del reporte no lee nada",
   { x <- leer_transferencias_bajionet(lineas = c(RES_ARCHIVO, sub("Mensaje", "Otro", ENC), P1))
     nrow(x$pagos) == 0L && x$problemas$codigo == "encabezado" })
ok("4 · un archivo de una sola línea no lee nada y lo dice",
   { x <- leer_transferencias_bajionet(lineas = RES_ARCHIVO); nrow(x$pagos) == 0L && nrow(x$problemas) == 1L })

# ── 5 · Encodings ───────────────────────────────────────────────────────────
escribir <- function(bytes) { f <- tempfile(fileext = ".txt"); writeBin(bytes, f); f }
txt <- paste(c(RES_ARCHIVO, ENC, P1, P2), collapse = "\r\n")
utf8   <- escribir(charToRaw(enc2utf8(txt)))
bom    <- escribir(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(enc2utf8(txt))))
latin1 <- escribir(charToRaw(iconv(txt, "UTF-8", "latin1")))
ref <- leer_transferencias_bajionet(utf8)
ok("5 · UTF-8 desde archivo: 2 pagos y la transacción con su acento",
   nrow(ref$pagos) == 2L && ref$pagos$transaccion[2] == "Transferencia a Tercero BanBajío")
ok("5 · con BOM: igual", identical(leer_transferencias_bajionet(bom)$pagos, ref$pagos))
ok("5 · en Latin-1: igual", identical(leer_transferencias_bajionet(latin1)$pagos, ref$pagos))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
