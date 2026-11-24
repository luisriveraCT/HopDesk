# =============================================================================
# tests/test_t93_casar.R
# T93: matching BajíoNet's report to what HopDesk generated (made-up data).
#   1  btr_lote_generado(): the batch's file by name; else by content (origin
#      account, up to 7 days, every Correcto/Editado payment in the file);
#      none or two candidates -> NA (path B)
#   2  btr_casar_a(): only Aplicado + Correcto + a unique match is "confirmar";
#      Editado and Agregado are shown, never confirmed alone; a rejected one
#      is "no_aplicado"; a beneficiary is used once; the concepto breaks a tie
#   3  btr_sugerir_b(): a batch HopDesk did not generate only SUGGESTS the
#      Agenda row with the supplier's account, exact amount and currency;
#      two candidates are "varias"; none is "sin_casar"; never "confirmar"
#   4  btr_moneda(): "MN" is MXN, through the one list of bank words
#
# Run from project root:  Rscript tests/test_t93_casar.R
# =============================================================================
cat("=== test_t93_casar ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/monedas.R", "R/ppl_generator.R", "R/bancos_respuesta.R")) source(f)
for (e in parse("R/pagos_generados_persistence.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "parse_pago_beneficiarios"))
    eval(e, globalenv())

pago <- function(cuenta, alias, imp, registro = "Correcto", estatus = "Aplicado", concepto = "F-1", divisa = "MN")
  data.frame(cuenta_origen = "73543010201", cuenta_destino = cuenta, alias = alias, importe = imp,
             concepto = concepto, divisa = divisa, estatus = estatus, cancelo = "", devolucion = "",
             registro = registro, stringsAsFactors = FALSE)
ben_json <- function(...) as.character(jsonlite::toJSON(list(...), auto_unbox = TRUE))
b <- function(clabe, alias, importe, concepto = "F-1") list(alias = alias, clabe = clabe, banco = substr(clabe, 1, 3),
                                                            importe = importe, concepto = concepto)
generado <- function(archivo, ts, origen, ...) data.frame(
  id = archivo, ts = as.POSIXct(ts, tz = "America/Mexico_City"), cuenta_origen = origen, tipo = "pago_ppl",
  archivo = archivo, beneficiarios = ben_json(...), stringsAsFactors = FALSE)

# ── 1 · Which file is this batch ────────────────────────────────────────────
G <- rbind(
  generado("pagos_NCS_24112026_001.txt", "2026-11-24 09:00", "073543010201",
           b("012180000000000001", "PROVUNO", 100), b("000000000000000002", "PROVDOS", 200)),
  generado("pagos_NCS_20112026_001.txt", "2026-11-20 09:00", "073543010201",
           b("014180000000000009", "PROVNUEVE", 900)),
  generado("pagos_OTRA_24112026_001.txt", "2026-11-24 09:00", "099999999999",
           b("012180000000000001", "PROVUNO", 100)))
P <- rbind(pago("012180000000000001", "PROVUNO", 100), pago("000000000002", "PROVDOS", 200))
lote <- function(archivo) list(archivo = archivo, fecha = as.Date("2026-11-24"))
ok("1 · por nombre: el archivo que generó HopDesk", identical(btr_lote_generado(lote("pagos_NCS_24112026_001.txt"), P, G), 1L))
ok("1 · por nombre, sin la extensión .txt", identical(btr_lote_generado(lote("pagos_NCS_24112026_001"), P, G), 1L))
ok("1 · por contenido (otro nombre): misma cuenta de origen, fecha y pagos -> el de NCS, no el de OTRA",
   identical(btr_lote_generado(lote("pagos_24112026_001NCS"), P, G), 1L))
ok("1 · un pago que no está en ningún archivo -> NA (camino B)",
   is.na(btr_lote_generado(lote("pagos_x"), rbind(P, pago("012180000000000077", "OTRO", 5)), G)))
ok("1 · más de 7 días: no cuenta",
   is.na(btr_lote_generado(list(archivo = "x", fecha = as.Date("2026-12-05")), P, G)))
ok("1 · dos archivos que cumplen -> NA, nunca adivina",
   is.na(btr_lote_generado(lote("x"), P, rbind(G, transform(G[1, ], archivo = "copia.txt", id = "copia")))))
ok("1 · un Agregado no cuenta para decidir el archivo",
   identical(btr_lote_generado(lote("x"), rbind(P, pago("012180000000000077", "OTRO", 5, registro = "Agregado")), G), 1L))

# ── 2 · Path A ──────────────────────────────────────────────────────────────
ben <- parse_pago_beneficiarios(G$beneficiarios[1])
pa <- rbind(pago("012180000000000001", "PROVUNO", 100),                          # Correcto, aplicado
            pago("000000000002", "PROVDOS", 250, registro = "Editado"),          # changed in BajíoNet
            pago("014180000000000005", "PROVCINCO", 50, registro = "Agregado"))   # not in the file
r <- btr_casar_a(pa, ben)
ok("2 · Aplicado + Correcto + único -> \"confirmar\", con su beneficiario", r$estado[1] == "confirmar" && r$beneficiario[1] == 1L)
ok("2 · Editado -> \"editado\" (se muestra, nunca solo), aunque el importe cambió", r$estado[2] == "editado" && r$beneficiario[2] == 2L)
ok("2 · Agregado -> \"agregado\", sin beneficiario", r$estado[3] == "agregado" && is.na(r$beneficiario[3]))
ok("2 · rechazado -> \"no_aplicado\"",
   btr_casar_a(pago("012180000000000001", "PROVUNO", 100, estatus = "Rechazado"), ben)$estado == "no_aplicado")
ok("2 · importe distinto en un Correcto -> \"sin_casar\"",
   btr_casar_a(pago("012180000000000001", "PROVUNO", 101), ben)$estado == "sin_casar")
ok("2 · alias distinto -> \"sin_casar\"", btr_casar_a(pago("012180000000000001", "OTRO", 100), ben)$estado == "sin_casar")
dos <- parse_pago_beneficiarios(ben_json(b("012180000000000001", "PROVUNO", 100, "F-1"), b("012180000000000001", "PROVUNO", 100, "F-2")))
r2 <- btr_casar_a(rbind(pago("012180000000000001", "PROVUNO", 100, concepto = "F-2"),
                        pago("012180000000000001", "PROVUNO", 100, concepto = "F-1")), dos)
ok("2 · dos pagos iguales: el concepto desempata y cada beneficiario se usa una vez",
   identical(r2$beneficiario, c(2L, 1L)) && all(r2$estado == "confirmar"))
r3 <- btr_casar_a(rbind(pago("012180000000000001", "PROVUNO", 100, concepto = "X"),
                        pago("012180000000000001", "PROVUNO", 100, concepto = "Y")), dos)
ok("2 · sin desempate posible -> \"sin_casar\", nunca adivina", all(r3$estado == "sin_casar"))

# ── 3 · Path B: suggestions only ────────────────────────────────────────────
agenda <- data.frame(id = c("A1", "A2", "A3", "A4"), Moneda = c("MXN", "MXN", "USD", "MXN"),
                     Importe = c(100, 100, 100, 300), clabe_dest = c("012180000000000001", "014180000000000002",
                                                                     "012180000000000001", "014180000000000002"),
                     stringsAsFactors = FALSE)
s <- btr_sugerir_b(rbind(pago("012180000000000001", "X", 100),                    # A1 only (A3 is USD)
                         pago("014180000000000002", "Y", 100),                    # A2
                         pago("014180000000000002", "Y", 999),                    # none
                         pago("012180000000000001", "X", 100, estatus = "Devuelto")), agenda)
ok("3 · cuenta + importe exacto + misma moneda -> \"sugerido\" (A1; la de USD no)",
   s$estado[1] == "sugerido" && identical(s$candidatos[[1]], "A1"))
ok("3 · otra cuenta registrada -> su propia fila (A2)", s$estado[2] == "sugerido" && identical(s$candidatos[[2]], "A2"))
ok("3 · sin coincidencia -> \"sin_casar\"", s$estado[3] == "sin_casar" && !length(s$candidatos[[3]]))
ok("3 · no aplicado -> \"no_aplicado\", sin candidatos", s$estado[4] == "no_aplicado" && !length(s$candidatos[[4]]))
s2 <- btr_sugerir_b(pago("014180000000000002", "Y", 300), rbind(agenda, transform(agenda[4, ], id = "A5")))
ok("3 · dos filas posibles -> \"varias\" con las dos", s2$estado == "varias" && setequal(s2$candidatos[[1]], c("A4", "A5")))
ok("3 · el camino B nunca da \"confirmar\"", !any(c(s$estado, s2$estado) == "confirmar"))

# ── 4 · The report's currency ───────────────────────────────────────────────
ok("4 · \"MN\" es MXN, \"USD\" es USD, \"Dólares\" es USD, algo raro es NA",
   identical(unname(btr_moneda(c("MN", "USD", "Dólares", "ZZZ"))), c("MXN", "USD", "USD", NA_character_)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
