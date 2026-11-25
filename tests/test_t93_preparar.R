# =============================================================================
# tests/test_t93_preparar.R
# T93: from the report to what may be confirmed (made-up data).
#   1  path A: "confirmar" comes ticked and carries ITS Agenda row (2b);
#      a file from before 2b is "sin_agenda" (never alone); a row no longer
#      pending is "ya_confirmado"; a row uploaded before is "ya_registrado"
#   2  path B: btr_agenda_cuentas() only by exact link / code / name / alias,
#      never a guess; a "sugerido" is elegible but NOT ticked
#   3  btr_a_confirmar(): only elegible + ticked + with an Agenda row; a line
#      the browser made up, or a non-elegible line, is ignored
#   4  never a whole account in what the screen gets
#
# Run from project root:  Rscript tests/test_t93_preparar.R
# =============================================================================
cat("=== test_t93_preparar ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/monedas.R", "R/ppl_generator.R", "R/pagos_layouts.R", "R/bancos_respuesta.R")) source(f)
for (e in parse("R/pagos_generados_persistence.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "parse_pago_beneficiarios"))
    eval(e, globalenv())

pago <- function(linea, cuenta, alias, imp, registro = "Correcto", estatus = "Aplicado", aut = NULL)
  data.frame(linea = linea, fecha = as.Date("2026-11-24"), cuenta_origen = "73543010201", cuenta_destino = cuenta,
             alias = alias, importe = imp, concepto = "F", divisa = "MN", estatus = estatus, mensaje = "",
             cancelo = "", devolucion = "", registro = registro,
             autorizacion = aut %||% sprintf("%013d", linea), clave_rastreo = paste0("BB", sprintf("%013d", linea)),
             referencia = "1", comision = 0, iva_comision = 0, stringsAsFactors = FALSE)
reporte <- function(archivo, ...) list(lote = list(archivo = archivo, fecha = as.Date("2026-11-24")), pagos = rbind(...))
ben <- function(clabe, alias, imp, ag = NULL) c(list(alias = alias, clabe = clabe, banco = "012", importe = imp, concepto = "F"),
                                               if (!is.null(ag)) list(agenda_item_id = ag, documento = "D", fecha_factura = "2026-11-01"))
G <- data.frame(id = "G1", ts = as.POSIXct("2026-11-24 09:00", tz = "America/Mexico_City"), cuenta_origen = "073543010201",
                tipo = "pago_ppl", archivo = "pagos_NCS_24112026_001.txt",
                beneficiarios = as.character(jsonlite::toJSON(list(ben("012180000000000001", "UNO", 100, "A1"),
                                                                   ben("012180000000000002", "DOS", 200, "A2"),
                                                                   ben("012180000000000003", "TRES", 300, "A9")), auto_unbox = TRUE)),
                stringsAsFactors = FALSE)
AG <- data.frame(id = c("A1", "A2", "B1", "B2"), Empresa = "NCS", Moneda = "MXN", Documento = c("D1", "D2", "D3", "D4"),
                 Parte = c("UNO SA", "DOS SA", "CINCO SA", "SEIS SA"), Codigo = c("", "", "", "C6"),
                 Importe = c(100, 200, 500, 600), stringsAsFactors = FALSE)

# ── 1 · Path A ──────────────────────────────────────────────────────────────
pa <- btr_preparar(reporte("pagos_NCS_24112026_001.txt",
                           pago(3, "012180000000000001", "UNO", 100),
                           pago(4, "012180000000000002", "DOS", 200),
                           pago(5, "012180000000000003", "TRES", 300)), G, AG, registradas = "aut:0000000000004")
f <- pa$filas
ok("1 · camino A con su archivo", pa$camino == "A" && identical(pa$generado$id, "G1"))
ok("1 · \"confirmar\" llega marcado, elegible y con SU fila de la Agenda (A1)",
   f$estado[1] == "confirmar" && f$marcado[1] && f$elegible[1] && f$agenda_item_id[1] == "A1" && f$empresa[1] == "NCS")
ok("1 · subido antes -> \"ya_registrado\", no elegible", f$estado[2] == "ya_registrado" && !f$elegible[2])
ok("1 · su fila ya no está pendiente (A9) -> \"ya_confirmado\", no elegible", f$estado[3] == "ya_confirmado" && !f$elegible[3])
G0 <- G; G0$beneficiarios <- as.character(jsonlite::toJSON(list(ben("012180000000000001", "UNO", 100)), auto_unbox = TRUE))
f0 <- btr_preparar(reporte("pagos_NCS_24112026_001.txt", pago(3, "012180000000000001", "UNO", 100)), G0, AG)$filas
ok("1 · un archivo de antes de 2b (sin fila de Agenda) -> \"sin_agenda\", nunca solo", f0$estado == "sin_agenda" && !f0$elegible)
# The currency: the report's must be the Agenda row's (A1 is MXN).
con_divisa <- function(d) { p <- pago(3, "012180000000000001", "UNO", 100); p$divisa <- d; p }
fz <- btr_preparar(reporte("pagos_NCS_24112026_001.txt", con_divisa("ZZZ")), G, AG)$filas
ok("1 · divisa que no se reconoce (NA) -> \"moneda_distinta\", bloqueada y sin marcar",
   is.na(fz$moneda) && fz$estado == "moneda_distinta" && !fz$elegible && !fz$marcado)
fu <- btr_preparar(reporte("pagos_NCS_24112026_001.txt", con_divisa("USD")), G, AG)$filas
ok("1 · USD en el reporte contra una fila MXN -> \"moneda_distinta\", bloqueada",
   fu$moneda == "USD" && fu$estado == "moneda_distinta" && !fu$elegible)
fm <- btr_preparar(reporte("pagos_NCS_24112026_001.txt", con_divisa("MN")), G, AG)$filas
ok("1 · contraprueba: MN (= MXN) contra la fila MXN se confirma, con la moneda de la fila",
   fm$estado == "confirmar" && fm$elegible && fm$moneda_agenda == "MXN")

# ── 2 · Path B ──────────────────────────────────────────────────────────────
prov <- data.frame(nombre = c("CINCO SA", "SEIS SA", "SIETE SA"), alias = c("CINCO", "SEIS", "SIETE"),
                   codigo = c("", "C6", ""), clabe = c("014180000000000005", "014180000000000006", "014180000000000007"),
                   stringsAsFactors = FALSE)
ok("2 · cuentas de la Agenda: por nombre exacto (B1) y por código (B2); sin proveedor -> NA",
   identical(btr_agenda_cuentas(AG, prov), c(NA, NA, "014180000000000005", "014180000000000006")))
amap <- data.frame(Parte = "UNO SA", alias = "SIETE", Empresa = "", stringsAsFactors = FALSE)
ok("2 · el vínculo guardado Parte -> alias manda", btr_agenda_cuentas(AG[1, ], prov, amap) == "014180000000000007")
pb <- btr_preparar(reporte("pagos_24112026_001NCS", pago(7, "014180000000000005", "X", 500), pago(8, "014180000000000006", "Y", 999)),
                   G, AG, prov)
ok("2 · lote externo -> camino B", pb$camino == "B" && is.null(pb$generado))
ok("2 · \"sugerido\": elegible pero NO marcado (lo marca el usuario)",
   pb$filas$estado[1] == "sugerido" && pb$filas$elegible[1] && !pb$filas$marcado[1] && pb$filas$agenda_item_id[1] == "B1")
ok("2 · importe distinto -> \"sin_casar\"", pb$filas$estado[2] == "sin_casar" && !pb$filas$elegible[2])

# ── 3 · What is confirmed ───────────────────────────────────────────────────
ok("3 · A: lo marcado y elegible", identical(btr_a_confirmar(pa, f$linea[f$marcado])$linea, 3L))
ok("3 · una línea inventada o no elegible se ignora", nrow(btr_a_confirmar(pa, c("99", "4", "5", "<x>"))) == 0L)
ok("3 · B: solo si el usuario la marcó", nrow(btr_a_confirmar(pb, integer())) == 0L &&
     identical(btr_a_confirmar(pb, 7L)$agenda_item_id, "B1"))

# ── 4 · Never a whole account ───────────────────────────────────────────────
ok("4 · la pantalla recibe la cuenta enmascarada", all(grepl("^\\*\\*\\*\\*[0-9]{4}$", c(f$cuenta, pb$filas$cuenta))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
