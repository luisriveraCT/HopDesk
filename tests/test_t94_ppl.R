# =============================================================================
# tests/test_t94_ppl.R
# T94. BanBajío's PPL ("Pago a Proveedores", guía Enlace Empresarial v01.2026)
# had three fields off the guide: the divisa (always "01"), the receiving bank
# ("40" + the CLABE's first 3 digits: wrong for the 37xxx/90xxx/91xxx banks
# of its Anexo 4) and SPID's classification (always "00"). Now:
#   1  a MXN file to 40xxx banks (SPI, BCO and the _002's zeros line) is
#      IDENTICAL, byte for byte, to master 01554be4's -- the lines below were
#      written by master's own generate_ppl() with these same payments;
#   2  a USD row differs from master's ONLY in the divisa (pos. 32-33,
#      "01" -> "02") and the classification (pos. 383-384, "00" -> "07");
#   3  the receiving bank comes from PPL_BANCOS (STP 90646, Nu 90638,
#      BANAMEX2 91802, Banobras 37009, Banxico 02001); a prefix outside the
#      catalog has no key and generate_ppl() stops (the last net);
#   4  the classification is one of the guide's codes or the default "07":
#      what a browser sends is never written as is;
#   5  the real pagarHoyServer (testServer, the harness of test_cm30): a
#      supplier whose CLABE's bank is outside the catalog (608), mixed with
#      valid ones (STP, now 90646; CIBANCO 143, 40 series, as before and
#      logged), says why in the preview, never reaches a file or the payment
#      log (one company, the ZIP, nor the ZIP's _002 even if "included"), and
#      stays pending in the Agenda; the USD file carries the classification
#      chosen in the preview, checked on the server, and says "por probar".
#
# Run from project root:  Rscript tests/test_t94_ppl.R
# =============================================================================
cat("=== test_t94_ppl ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
fmt_money <- function(x) paste0("$ ", formatC(x, format = "f", digits = 2, big.mark = ","))

for (e in parse("R/erp_connector_registry.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "SAP_B1_ERP_TYPE"))
    eval(e, globalenv())
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "is_erp_sourced"))
    eval(e, globalenv())
for (e in parse("R/data_pipeline.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".politica_doc_unica"))
    eval(e, globalenv())
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/monedas.R",
            "R/zona.R", "R/ppl_generator.R", "R/pagos_layouts.R"))
  source(f)
for (e in parse("R/search_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".datos_sap_filas", ".datos_sap_columnas", "datos_sap_tabla", "datos_sap_visor"))
    eval(e, globalenv())
source("R/agenda_filas.R")
source("R/pagar_hoy_module.R")

# ── 1-2 · Byte for byte against master ──────────────────────────────────────
pagos <- data.frame(
  alias      = c("ACME", "SERVBAJIO", "ACME", ""),
  clabe_dest = c("012345678901234567", "030180003333333333", "021180009999999999", "000000000000000000"),
  banco_dest = c("012", "030", "021", ""),
  medio_pago = c("SPI", "BCO", "SPD", "SPI"),
  importe    = c(1234.56, 2500.75, 50, 30),
  concepto   = c("F-1", "F-2", "F-3", "F-4"), stringsAsFactors = FALSE)
FECHA <- as.Date("2026-11-24")
# master 01554be4's generate_ppl(pagos, "073543010201", FECHA, 1L), line by
# line: the first 125 characters, then the concepto padded to 250, then the
# 9 zeros (Referencia SPEI "0000000" + classification "00").
fila_master <- function(cabeza, conc) paste0(cabeza, conc, strrep(" ", 250 - nchar(conc)), "000000000")
MASTER <- c(
  "01000000120261124001",
  fila_master("0200000020100000000073543010201014001200000000012345620261124SPI4000012345678901234567000000000ACME           000000000000000", "F-1"),
  fila_master("0200000030100000000073543010201014003000000000025007520261124BCO0100030180003333333333000000000SERVBAJIO      000000000000000", "F-2"),
  fila_master("0200000040100000000073543010201014002100000000000500020261124SPD4000021180009999999999000000000ACME           000000000000000", "F-3"),
  fila_master("0200000050100000000073543010201014000000000000000300020261124SPI4000000000000000000000000000000               000000000000000", "F-4"),
  "0900000060000004000000000000381531")
ok("las líneas de referencia de master miden 20 / 384 / 34", identical(nchar(MASTER), c(20L, rep(384L, 4L), 34L)))

mxn <- generate_ppl(pagos, "073543010201", FECHA, 1L, moneda = "MXN", clasificacion = "03")
ok("1 · MXN (SPI 40xxx, BCO 40030, la línea en ceros del _002): idéntico a master byte por byte",
   identical(mxn, MASTER))
ok("1 · sin moneda (un llamado viejo): también idéntico a master", identical(generate_ppl(pagos, "073543010201", FECHA, 1L), MASTER))

usd <- generate_ppl(pagos, "073543010201", FECHA, 1L, moneda = "USD")
difiere <- function(a, b) which(strsplit(a, "")[[1]] != strsplit(b, "")[[1]])
cat("   diff USD vs master, renglón SPD:\n")
cat(sprintf("     pos. 32-33 (divisa):        master \"%s\" -> \"%s\"\n", substr(MASTER[4], 32, 33), substr(usd[4], 32, 33)))
cat(sprintf("     pos. 383-384 (clasificación): master \"%s\" -> \"%s\"\n", substr(MASTER[4], 383, 384), substr(usd[4], 383, 384)))
ok("2 · USD: cada renglón de pago difiere de master SOLO en la divisa (33) y la clasificación (384)",
   all(vapply(2:5, function(i) identical(difiere(usd[i], MASTER[i]), c(33L, 384L)), logical(1))))
ok("2 · USD: divisa \"02\" y clasificación \"07 Otro\" de serie",
   substr(usd[4], 32, 33) == "02" && substr(usd[4], 383, 384) == "07")
ok("2 · USD: encabezado y sumario idénticos a master", identical(usd[c(1, 6)], MASTER[c(1, 6)]))
ok("2 · una moneda fuera del PPL se rechaza sin escribir nada",
   inherits(tryCatch(generate_ppl(pagos, "073543010201", FECHA, 1L, moneda = "EUR"), error = function(e) e), "error"))

# ── 3 · The receiving bank from BanBajío's catalog ──────────────────────────
banco_de <- function(clabe, medio = "SPI") {
  p <- pagos[1, ]; p$clabe_dest <- clabe; p$banco_dest <- substr(clabe, 1, 3); p$medio_pago <- medio
  substr(generate_ppl(p, "073543010201", FECHA, 1L, moneda = "MXN")[2], 34, 38)
}
ok("3 · STP (646) -> 90646", banco_de("646180001234567890") == "90646")
ok("3 · Nu (638) -> 90638", banco_de("638180001234567890") == "90638")
ok("3 · BANAMEX2 (802) -> 91802", banco_de("802180001234567890") == "91802")
ok("3 · Banobras (009) -> 37009", banco_de("009180001234567890") == "37009")
ok("3 · Banxico (001) -> 02001 (su clave de 4, a 5 con cero)", banco_de("001180001234567890") == "02001")
ok("3 · BBVA (012) sigue 40012", banco_de("012180001234567890") == "40012")
ok("3 · 123 instituciones en PPL_BANCOS, todas de 4-5 dígitos y sin prefijo repetido",
   length(PPL_BANCOS) == 123L && all(grepl("^[0-9]{4,5}$", PPL_BANCOS)) && !anyDuplicated(names(PPL_BANCOS)))
# A 40-series bank missing from the catalog (CIBANCO 143) goes as before
# (Coordinador): the line master's generate_ppl() wrote for this payment.
cib <- data.frame(alias = "CIBANCO", clabe_dest = "143180001234567890", banco_dest = "143", medio_pago = "SPI",
                  importe = 999.99, concepto = "F-143", stringsAsFactors = FALSE)
CIB_MASTER <- fila_master("0200000020100000000073543010201014014300000000009999920261124SPI4000143180001234567890000000000CIBANCO        000000000000000", "F-143")
ok("3 · CIBANCO (143, serie 40, fuera del catálogo): sale idéntico a master (40143)",
   identical(generate_ppl(cib, "073543010201", FECHA, 1L, moneda = "MXN")[2], CIB_MASTER) &&
     isTRUE(ppl_banco_como_antes("143180001234567890", "143", "SPI")))
ok("3 · un prefijo fuera del catálogo y fuera de la serie 40 (608, serie 90) no tiene clave",
   is.na(ppl_banco_destino("608180001234567890", "608", "SPI")) &&
     !ppl_banco_como_antes("608180001234567890", "608", "SPI"))
ok("3 · generate_ppl() se detiene con ese banco (última red)",
   inherits(tryCatch(banco_de("608180001234567890"), error = function(e) e), "error"))
ok("3 · \"como antes\" nunca para un banco del catálogo, BCO ni la línea en ceros",
   !any(ppl_banco_como_antes(c("012180001234567890", "030180003333333333", "000000000000000000"),
                             c("012", "030", ""), c("SPI", "BCO", "SPI"))))
ok("3 · ppl_banco_destino() es por renglón: BCO 40030, ceros 40000, STP 90646",
   identical(ppl_banco_destino(c("030180003333333333", "000000000000000000", "646180001234567890"),
                               c("030", "", "646"), c("BCO", "SPI", "SPI")), c("40030", "40000", "90646")))

# ── 4 · The classification: the guide's codes, never the raw input ─────────
ok("4 · un código de la guía pasa tal cual", ppl_clasificacion("03") == "03")
ok("4 · lo que no es de la guía es \"07\" (vacío, NULL, NA, 99, HTML)",
   all(vapply(list("", NULL, NA, "99", "<b>07</b>", "7"), ppl_clasificacion, character(1)) == "07"))
ok("4 · en MXN la clasificación no se escribe (\"00\"), aunque llegue una",
   all(substr(mxn[2:5], 383, 384) == "00"))
ok("4 · las 7 opciones tienen texto en es y en",
   all(vapply(unname(PPL_CLASIFICACIONES), function(k) {
     es <- tr(k, idioma = "es"); en <- tr(k, idioma = "en"); nzchar(es) && nzchar(en) && es != k && en != k }, logical(1))))

# ── 5 · The real server: a mix of valid and invalid rows ───────────────────
PPL <- list(); ultimo_modal <- NULL; AVISOS <- character()
.log_pago_generado <- function(shared, emp, moneda, cuenta_origen, ppl_rows, ..., source_type = NA_character_) {
  PPL[[length(PPL) + 1L]] <<- list(tipo = source_type, moneda = moneda, clabes = ppl_rows$clabe_dest,
                                   importes = ppl_rows$importe)
  invisible(NULL)
}
LOGS <- list()
log_action <- function(user, module, action, description = "", ..., metadata = NULL) {
  LOGS[[length(LOGS) + 1L]] <<- list(action = action, description = description, metadata = metadata)
  invisible(NULL)
}
avisos_actividad <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
hd_numero_update <- function(...) invisible(NULL)   # R/ui_components.R; switching the currency calls it
puede_o_avisa <- function(...) TRUE
puede <- function(...) TRUE
showModal <- function(ui, ...) ultimo_modal <<- as.character(ui)
removeModal <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
Sys.time <- function() as.POSIXct("2026-11-24 12:00:00", tz = "America/Mexico_City")
ORIGEN <- c(MXN = "073543010201", USD = "013386156002")
get_cuenta_ppl <- function(emp, cur, shared) unname(ORIGEN[toupper(cur)]) %||% ""
get_cuentas_ppl_data <- function(emp, cur, shared)
  data.frame(cuenta = get_cuenta_ppl(emp, cur, shared), banco = "030", alias = "Operativa",
             is_ppl_default = TRUE, stringsAsFactors = FALSE)

EMP <- "Networks"
CLABE_FUERA <- "608180001234567890"   # 608: not in BanBajío's Anexo 4, 90 series -> stays out
CLABE_STP   <- "646180001234567890"   # STP: in it, as 90646
CLABE_CIB   <- "143180001234567890"   # CIBANCO: not in it, 40 series -> as before, 40143, logged
catalogo <- tibble::tibble(nombre = c("ACME SA", "PAGOS FUERA SA", "PAGOS STP SA", "PAGOS CIBANCO SA"),
                           alias = c("ACME", "PAGOFUERA", "PAGOSTP", "PAGOCIB"),
                           clabe = c("012345678901234567", CLABE_FUERA, CLABE_STP, CLABE_CIB), medio_pago = "SPEI",
                           banco = c("012", "608", "646", "143"), Empresa = NA_character_)
fila <- function(id, doc, moneda = "MXN", parte = "ACME SA", imp = 100)
  tibble::tibble(id = id, ledger = "AP", Empresa = EMP, Moneda = moneda, Documento = doc, Parte = parte,
                 Codigo = "", Importe = imp, FechaVenc = as.Date("2026-11-25"), status = "pending",
                 tipo_item = NA_character_, source = SAP_B1_ERP_TYPE, provision_id = NA_character_,
                 fecha_factura = as.Date("2026-11-01"))
AGENDA <- dplyr::bind_rows(fila("A1", "D1", imp = 100),
                           fila("A4", "D4", parte = "PAGOS FUERA SA", imp = 777),   # bank outside the catalog
                           fila("A5", "D5", parte = "PAGOS STP SA", imp = 333),     # STP: goes in, 90646
                           fila("A6", "D6", parte = "PAGOS CIBANCO SA", imp = 444), # CIBANCO: goes in, 40143
                           fila("A2", "D2", moneda = "USD", imp = 50))
SAP <- data.frame(Empresa = "NTS", Documento = c("D1", "D4", "D5", "D6", "D2"), Factura = c("F-1", "F-4", "F-5", "F-6", "F-2"),
                  Parte = c("ACME SA", "PAGOS FUERA SA", "PAGOS STP SA", "PAGOS CIBANCO SA", "ACME SA"),
                  Moneda = c("MXN", "MXN", "MXN", "MXN", "USD"),
                  `Fecha de contabilización` = as.Date("2026-11-01"), check.names = FALSE)

correr <- function(modo, clasif = NULL, incluir = character()) {
  PPL <<- list(); AVISOS <<- character(); ultimo_modal <<- NULL; LOGS <<- list()
  res <- list()
  agenda_rv <- reactiveVal(AGENDA)
  shared <- list(
    company_map = reactiveVal(c(NTS = EMP)), proveedores_db = reactiveVal(catalogo),
    parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = agenda_rv,
    sap_data = reactiveVal(list(AP = SAP)), sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
    bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
    effective_client_id = function() "dev", current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list(can_register_payments = TRUE))),
    home_client_id = function() "dev")
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    if (modo %in% c("MXN", "USD")) {
      if (modo == "USD") session$setInputs(bal_cur_Networks = "USD")
      session$setInputs(dl_preview_Networks = 1)
      res$modal <<- ultimo_modal %||% ""
      if (!is.null(clasif)) session$setInputs(ppl_clasif_Networks = clasif)
      res$archivo <<- tryCatch(readLines(output$dl_ppl_Networks, warn = FALSE),
                               error = function(e) paste("ERROR", conditionMessage(e)))
    } else {
      session$setInputs(preview_all = 1)
      res$vista <<- .descargar_vista()
      if (length(incluir)) {
        ids <- paste0(incluir, "#1")
        session$setInputs(`descargar_t-opciones` = list(ids = ids, valores = rep("incluir", length(ids))))
      }
      dir <- tempfile(); dir.create(dir)
      res$zip <<- tryCatch({ utils::unzip(output$download_all, exdir = dir); sort(list.files(dir)) },
                           error = function(e) paste("ERROR", conditionMessage(e)))
      res$contenido <<- unlist(lapply(file.path(dir, list.files(dir)), readLines, warn = FALSE))
    }
    res$agenda <<- agenda_rv()
  })))
  res
}
detalle <- function(lineas) lineas[startsWith(lineas, "02")]
sin_fuera <- function(r) !any(unlist(lapply(PPL, `[[`, "clabes")) %in% CLABE_FUERA) &&
  !any(unlist(lapply(PPL, `[[`, "importes")) == 777)
como_antes <- function() Filter(function(l) identical(l$action, "ppl_banco_fuera_catalogo"), LOGS)

r <- correr("MXN")
ok("5 · una empresa, MXN: la vista previa dice el motivo del banco fuera del catálogo (prefijo 608)",
   grepl(tr("tablas.ppl.motivo_banco", prefijo = "608"), r$modal, fixed = TRUE))
d <- detalle(r$archivo)
ok("5 · el archivo lleva los 3 pagos válidos: ACME 40012, STP 90646 (antes 40646) y CIBANCO 40143 (como antes)",
   length(d) == 3L && identical(sort(substr(d, 34, 38)), c("40012", "40143", "90646")) && !any(grepl(CLABE_FUERA, d, fixed = TRUE)))
ok("5 · CIBANCO deja en el log \"fuera del catálogo de BanBajío, enviado como antes\", con su prefijo y sin cuenta",
   length(como_antes()) == 1L && identical(como_antes()[[1]]$metadata$prefijos, "143") &&
     identical(como_antes()[[1]]$metadata$n, 1L) && !grepl(CLABE_CIB, como_antes()[[1]]$description, fixed = TRUE) &&
     grepl("enviado(s) como antes", como_antes()[[1]]$description, fixed = TRUE))
ok("5 · el registro de pagos generados no tiene el pago fuera del catálogo", length(PPL) == 1L && sin_fuera())
ok("5 · el pago fuera del catálogo sigue pendiente en la Agenda", "A4" %in% r$agenda$id &&
     r$agenda$status[r$agenda$id == "A4"] == "pending")
ok("5 · MXN no muestra selector de clasificación ni \"por probar\"",
   !grepl("ppl_clasif_Networks", r$modal, fixed = TRUE) && !grepl(tr("tablas.ppl.por_probar", moneda = "MXN"), r$modal, fixed = TRUE))

r <- correr("ZIP", incluir = "A4")
v <- r$vista
ok("5 · ZIP: la vista previa marca el pago fuera del catálogo \"banco fuera del catálogo\", bloqueado",
   v$motivo[startsWith(v$id, "A4#")] == "banco_fuera_catalogo" && isTRUE(v$.bloqueada[startsWith(v$id, "A4#")]))
ok("5 · ZIP: aunque se marque \"incluir\", el pago fuera del catálogo no entra a ningún archivo (ni al _002)",
   !any(grepl(CLABE_FUERA, r$contenido, fixed = TRUE)) && !any(grepl("_002", r$zip, fixed = TRUE)))
ok("5 · ZIP: el registro no tiene el pago fuera del catálogo y la Agenda lo conserva", sin_fuera() && "A4" %in% r$agenda$id)
ok("5 · ZIP: el archivo MXN lleva los pagos válidos, CIBANCO como antes, y su rastro en el log",
   any(grepl("012345678901234567", r$contenido, fixed = TRUE)) &&
     any(substr(detalle(r$contenido), 34, 38) == "40143") && length(como_antes()) == 1L)

r <- correr("USD", clasif = "<script>alert(1)</script>")
ok("5 · USD: el modal trae el selector (\"07 Otro\" elegido) y la nota \"por probar\"",
   grepl("ppl_clasif_Networks", r$modal, fixed = TRUE) && grepl('<option value="07" selected>', r$modal, fixed = TRUE) &&
     grepl(tr("tablas.ppl.por_probar", moneda = "USD"), r$modal, fixed = TRUE))
ok("5 · USD: un valor que no es de la guía llega al archivo como \"07\" (lo valida el servidor)",
   length(detalle(r$archivo)) == 1L && substr(detalle(r$archivo), 383, 384) == "07" && substr(detalle(r$archivo), 32, 33) == "02")
r <- correr("USD", clasif = "04")
ok("5 · USD: la clasificación elegida (04) va en el archivo", substr(detalle(r$archivo), 383, 384) == "04")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
