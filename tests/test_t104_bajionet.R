# =============================================================================
# tests/test_t104_bajionet.R
# T104: BajíoNet's report (T93) confirms with its batch's origin account.
#   1  btr_preparar(): path A takes the batch's account (pagos_generados),
#      path B the report line's, as an account of the catalog in the row's
#      currency -- its id for the confirmation and its masked text for the
#      preview's "Cuenta origen" column
#   2  a row that would be confirmed without a resolvable account is locked,
#      "sin_cuenta_origen" (no catalog, or a number that is not in it); a row
#      uploaded before stays "ya_registrado"; other locked states keep theirs
#   3  btr_confirmar() passes each group's account to .ph_confirmar_ap(),
#      never NA (it used to)
#   4  the screen: the column, the state with its tone and texts in es/en
# Run from project root:  Rscript tests/test_t104_bajionet.R
# =============================================================================
cat("=== test_t104_bajionet ===\n\n")
suppressPackageStartupMessages({ library(shiny) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/monedas.R", "R/ppl_generator.R",
            "R/bancos_respuesta.R"))
  suppressMessages(source(f))
for (e in parse("R/pagos_generados_persistence.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "parse_pago_beneficiarios"))
    eval(e, globalenv())
source("tests/helpers/cuenta_origen_t104.R")

pago <- function(linea, cuenta, alias, imp, origen = "73543010201")
  data.frame(linea = linea, fecha = as.Date("2026-11-24"), cuenta_origen = origen, cuenta_destino = cuenta,
             alias = alias, importe = imp, concepto = "F", divisa = "MN", estatus = "Aplicado", mensaje = "",
             cancelo = "", devolucion = "", registro = "Correcto", autorizacion = sprintf("%013d", linea),
             clave_rastreo = paste0("BB", sprintf("%013d", linea)), referencia = "1", comision = 0, iva_comision = 0,
             stringsAsFactors = FALSE)
reporte <- function(archivo, ...) list(lote = list(archivo = archivo, fecha = as.Date("2026-11-24")), pagos = rbind(...))
ben <- function(clabe, alias, imp, ag) list(alias = alias, clabe = clabe, banco = "012", importe = imp, concepto = "F",
                                          agenda_item_id = ag, documento = "D", fecha_factura = "2026-11-01")
G <- function(origen = "073543010201")
  data.frame(id = "G1", ts = as.POSIXct("2026-11-24 09:00", tz = "America/Mexico_City"), cuenta_origen = origen,
             tipo = "pago_ppl", archivo = "pagos_NCS_24112026_001.txt",
             beneficiarios = as.character(jsonlite::toJSON(list(ben("012180000000000001", "UNO", 100, "A1"),
                                                                ben("012180000000000002", "DOS", 200, "A2")), auto_unbox = TRUE)),
             stringsAsFactors = FALSE)
AG <- data.frame(id = c("A1", "A2", "B1"), Empresa = "NCS", Moneda = "MXN", Documento = c("D1", "D2", "D3"),
                 Parte = c("UNO SA", "DOS SA", "CINCO SA"), Codigo = "", Importe = c(100, 200, 500), stringsAsFactors = FALSE)
RA <- reporte("pagos_NCS_24112026_001.txt", pago(3, "012180000000000001", "UNO", 100), pago(4, "012180000000000002", "DOS", 200))
TXT <- "••••0201 · NCSBB — BanBajío"

# ── 1 · The account is resolved ──────────────────────────────────────────────
pa <- btr_preparar(RA, G(), AG, ctas = CTAS_T104, bancos = BANCOS_T104)$filas
ok("1 · camino A: la cuenta del lote (073543010201) es la NCS MXN del catálogo, y se confirma",
   all(pa$cuenta_origen_id == "CT-NCS-MXN") && all(pa$estado == "confirmar") && all(pa$elegible))
ok("1 · …y la vista previa la muestra enmascarada, con alias y banco", all(pa$cuenta_origen == TXT))
prov <- data.frame(nombre = "CINCO SA", alias = "CINCO", codigo = "", clabe = "014180000000000005", stringsAsFactors = FALSE)
pb <- btr_preparar(reporte("pagos_24112026_001NCS", pago(7, "014180000000000005", "X", 500)), G(), AG, prov,
                   ctas = CTAS_T104, bancos = BANCOS_T104)
ok("1 · camino B (lote externo): la cuenta de la línea del reporte",
   pb$camino == "B" && pb$filas$estado == "sugerido" && pb$filas$cuenta_origen_id == "CT-NCS-MXN")

# ── 2 · Locked without it ────────────────────────────────────────────────────
p0 <- btr_preparar(RA, G(), AG)$filas
ok("2 · sin catálogo: \"sin_cuenta_origen\", bloqueada y sin marcar",
   all(p0$estado == "sin_cuenta_origen") && !any(p0$elegible) && !any(p0$marcado) && all(is.na(p0$cuenta_origen_id)))
ok("2 · …y la vista previa dice la cuenta del lote igual, enmascarada (••••0201)",
   all(p0$cuenta_origen == "••••0201"))
pz <- btr_preparar(RA, G("099999999999"), AG, ctas = CTAS_T104)$filas
ok("2 · un lote de una cuenta que no está en el catálogo: bloqueada, nunca otra cuenta",
   all(pz$estado == "sin_cuenta_origen") && !any(pz$elegible) && all(pz$cuenta_origen == "••••9999"))
pr <- btr_preparar(RA, G(), AG, registradas = "aut:0000000000004")$filas
ok("2 · subida antes: sigue \"ya_registrado\" (no \"sin_cuenta_origen\")", pr$estado[2] == "ya_registrado")
usd <- RA; usd$pagos$divisa <- "USD"
pu <- btr_preparar(usd, G(), AG, ctas = CTAS_T104)$filas
ok("2 · otro bloqueo (moneda distinta) conserva el suyo", all(pu$estado == "moneda_distinta"))
ok("2 · btr_a_confirmar() no toma una fila sin cuenta aunque el navegador la marque",
   nrow(btr_a_confirmar(list(filas = p0), c("3", "4"))) == 0L)

# ── 3 · Confirmed with its account ───────────────────────────────────────────
LLAMADAS <- list()
.ph_confirmar_ap <- function(shared, emp, cur, rows, cuenta_sel, fecha_pago, .parte_ap, .ph_fallido, .link_counter, extra = NULL) {
  LLAMADAS[[length(LLAMADAS) + 1L]] <<- list(ids = rows$id, cuenta = cuenta_sel); invisible(NULL)
}
evento_parte_de <- function(...) list()
safe_load_pagar_hoy <- function(...) transform(AG, ledger = "AP")
modificar_respuestas_banco <- function(client_id, filas, evento = NULL) list(nuevas = filas)
shared <- list(effective_client_id = function() "acme", current_user = function() "luis", home_client_id = function() "acme",
               bancos_confirmados = function() data.frame(confirmacion_id = character(), respuesta_id = character()))
dos <- rbind(CTAS_T104[1, ], transform(CTAS_T104[1, ], id = "CT-NCS-MXN-2", cuenta = "73543010202", alias = "NCS2"))
rb <- reporte("pagos_24112026_001NCS", pago(7, "012180000000000001", "UNO", 100),
              pago(8, "012180000000000002", "DOS", 200, origen = "73543010202"))
prov2 <- data.frame(nombre = c("UNO SA", "DOS SA"), alias = c("UNO", "DOS"), codigo = "",
                    clabe = c("012180000000000001", "012180000000000002"), stringsAsFactors = FALSE)
prep <- btr_preparar(rb, G()[0, ], AG, prov2, ctas = dos)
prep$archivo <- "x.txt"; prep$lote_archivo <- "x"
invisible(btr_confirmar(shared, prep, btr_a_confirmar(prep, c("7", "8"))))
cuentas <- vapply(LLAMADAS, function(l) l$cuenta, "")
ok("3 · cada grupo se confirma con SU cuenta (dos cuentas, dos llamadas)",
   length(LLAMADAS) == 2L && setequal(cuentas, c("CT-NCS-MXN", "CT-NCS-MXN-2")))
ok("3 · …nunca con NA", !anyNA(cuentas))

# ── 4 · The screen ───────────────────────────────────────────────────────────
v <- .btr_vista(list(filas = pa))
ok("4 · la vista previa trae la columna \"cuenta_origen\" (también vacía)",
   identical(v$cuenta_origen, pa$cuenta_origen) && "cuenta_origen" %in% names(.btr_vista(NULL)))
ok("4 · la tabla la declara", "cuenta_origen" %in% vapply(.btr_columnas(), function(c) c$campo %||% c$id %||% "", ""))
ok("4 · el estado existe con su tono, y sus textos en es y en",
   "sin_cuenta_origen" %in% BTR_ESTADOS && identical(.btr_tono[["sin_cuenta_origen"]], "aviso") &&
     all(c("tablas.archivos_pago.btr.estado.sin_cuenta_origen", "tablas.archivos_pago.btr.col_cuenta_origen") %in%
           intersect(names(i18n_dict("es")), names(i18n_dict("en")))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
