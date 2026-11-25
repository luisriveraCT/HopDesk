# =============================================================================
# tests/test_t93_registro.R
# T93 (2)+(5), over the in-memory S3 (tests/helpers/s3_memoria.R):
#   1  registrar_respuestas_banco(): appends on S3's table as it is now,
#      client_id explicit; the same report uploaded twice adds nothing (key:
#      the bank's authorisation; without one, the batch's file + the line)
#   2  a failed read writes nothing and stops (never an empty table)
#   3  the destination account is never kept whole (****1234)
#   4  bancos_confirmados: the 3 new columns exist, NA on a row written
#      before T93 (load_bancos_confirmados of an old table)
#   5  S3_KEYS has respuestas_banco, and global.R sources R/bancos_respuesta.R
#      after the two files it needs
#
# Run from project root:  Rscript tests/test_t93_registro.R
# =============================================================================
cat("=== test_t93_registro ===\n\n")
source("tests/helpers/s3_memoria.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
source("R/bancos_persistence.R")
source("R/bancos_respuesta.R")

CID <- "acme"
k <- paste0(CID, "/", S3_KEYS$respuestas_banco)
fila <- function(aut, linea, imp = 100, cuenta = "012180000000001234")
  data.frame(user = "luis", archivo_reporte = "TransferenciasporArchivo_x.txt", lote_archivo = "pagos_NCS_24112026_001.txt",
             camino = "A", pago_generado_id = "G1", linea = as.integer(linea), fecha_operacion = as.Date("2026-11-24"),
             autorizacion = aut, clave_rastreo = if (is.na(aut)) NA_character_ else paste0("BB", aut),
             referencia = "123456", cuenta_destino = btr_enmascarar(cuenta), alias = "PROV", importe = imp,
             moneda = "MXN", comision = 0, iva_comision = 0, estatus = "Aplicado", mensaje = "", registro = "Correcto",
             estado = "confirmar", agenda_item_id = paste0("A", linea), confirmacion_id = NA_character_,
             stringsAsFactors = FALSE)

# ── 1 · Append-only, no duplicates ──────────────────────────────────────────
r1 <- registrar_respuestas_banco(CID, rbind(fila("1111111111111", 3), fila("2222222222222", 4)))
ok("1 · primera subida: 2 renglones nuevos en S3", nrow(r1$nuevas) == 2L && nrow(.mem[[k]]) == 2L && r1$repetidas == 0L)
r2 <- registrar_respuestas_banco(CID, rbind(fila("1111111111111", 3), fila("2222222222222", 4), fila("3333333333333", 5)))
ok("1 · el mismo reporte otra vez (más uno nuevo): solo entra el nuevo, 2 repetidos",
   nrow(r2$nuevas) == 1L && r2$repetidas == 2L && nrow(.mem[[k]]) == 3L)
ok("1 · la llave es la autorización", identical(sort(.mem[[k]]$llave), paste0("aut:", c("1111111111111", "2222222222222", "3333333333333"))))
r3 <- registrar_respuestas_banco(CID, rbind(fila(NA, 7), fila(NA, 7), fila(NA, 8)))
ok("1 · sin autorización: llave archivo del lote + línea; repetida dentro de la subida, una vez",
   nrow(r3$nuevas) == 2L && all(c("lin:pagos_NCS_24112026_001.txt#7", "lin:pagos_NCS_24112026_001.txt#8") %in% .mem[[k]]$llave))
ok("1 · lo de antes no se toca: los 3 primeros siguen igual",
   identical(.mem[[k]]$id[1:3], c(r1$nuevas$id, r2$nuevas$id)))
ok("1 · sin client_id no escribe", falla(registrar_respuestas_banco("", fila("9", 9))) && nrow(.mem[[k]]) == 5L)
ok("1 · leer_respuestas_banco() devuelve la tabla del cliente con su esquema",
   nrow(leer_respuestas_banco(CID)) == 5L && identical(names(leer_respuestas_banco(CID)), names(.schema_respuestas_banco())))

# ── 2 · A failed read writes nothing ────────────────────────────────────────
antes <- .mem_escrituras
.mem_falla <- k
ok("2 · lectura fallida: se detiene y no escribe nada",
   falla(registrar_respuestas_banco(CID, fila("4444444444444", 9))) && .mem_escrituras == antes)
.mem_falla <- character()
ok("2 · y la tabla sigue con sus 5 renglones", nrow(.mem[[k]]) == 5L)

# ── 3 · Never the whole account ─────────────────────────────────────────────
ok("3 · ****1234 (18 dígitos, 12 dígitos, vacío)",
   identical(btr_enmascarar(c("012180000000001234", "000000005678", "")), c("****1234", "****5678", "")))
ok("3 · en S3 no queda ninguna cuenta completa", !any(grepl("[0-9]{5,}", .mem[[k]]$cuenta_destino)))

# ── 4 · bancos_confirmados: additive columns ────────────────────────────────
viejo <- tibble::tibble(confirmacion_id = "C1", agenda_item_id = "A1", empresa = "NTS", parte = "P", documento = "D1",
                        codigo = "", importe = 100, moneda = "MXN", cuenta_id = NA_character_, fecha = as.Date("2026-11-01"),
                        tipo = "pago", mov_id = NA_character_, confirmado_at = as.POSIXct("2026-11-01 10:00:00", tz = "UTC"),
                        eliminado = FALSE)
.mem[[paste0(CID, "/", S3_KEYS$bancos_confirmados)]] <- viejo
cf <- load_bancos_confirmados(client_id = CID)
ok("4 · una confirmación de antes se lee con clave_rastreo, autorizacion y respuesta_id en NA",
   nrow(cf) == 1L && all(c("clave_rastreo", "autorizacion", "respuesta_id") %in% names(cf)) &&
     is.na(cf$clave_rastreo) && is.na(cf$autorizacion) && is.na(cf$respuesta_id))
ok("4 · las 3 son texto en el esquema", all(vapply(.schema_bancos_confirmados()[c("clave_rastreo", "autorizacion", "respuesta_id")],
                                                   is.character, logical(1))))

# ── 5 · Wiring ──────────────────────────────────────────────────────────────
g <- readLines("R/global.R", warn = FALSE, encoding = "UTF-8")
i_resp <- grep('^source\\("R/bancos_respuesta.R"\\)', g)
ok("5 · S3_KEYS$respuestas_banco", identical(S3_KEYS$respuestas_banco, "respuestas_banco.rds"))
ok("5 · global.R carga R/bancos_respuesta.R una vez, después de ppl_generator y pagos_generados_persistence",
   length(i_resp) == 1L && i_resp > grep('^source\\("R/ppl_generator.R"\\)', g) &&
     i_resp > grep('^source\\("R/pagos_generados_persistence.R"\\)', g))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
