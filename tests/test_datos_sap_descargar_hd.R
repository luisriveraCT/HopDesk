# =============================================================================
# tests/test_datos_sap_descargar_hd.R
# T21: the raw SAP viewer (datos_sap) and the "Descargar todos los pagos"
# preview (descargar_pagos) on the table engine.
#   1. datos_sap rows: Ledger / Empresa / Parte / Documento / Moneda first, a
#      unique id even when installments repeat the Documento, the filter
#      fields always there, list columns as text, NULL keeps the columns;
#   2. datos_sap columns as SAP sends them: dates, numbers with their own
#      decimals (no money format), text; SAP's own names as titles;
#   3. the preview is what the ZIP holds: the rows the ZIP leaves out say why
#      ("Se omite: sin alias" / "sin CLABE"); the button and the pills count
#      and add up only what goes in -- the same count as the ZIP's lines.
#
# Run from project root:  Rscript tests/test_datos_sap_descargar_hd.R
# =============================================================================
cat("=== test_datos_sap_descargar_hd ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

invisible(capture.output(source("tests/helpers/agenda_fixture_app.R")))
Sys.Date <- function() as.Date("2026-11-05")

# ── 1. datos_sap rows ────────────────────────────────────────────────────────
RAW <- data.frame(`Saldo vencido` = c(100.5, 20.25, 7), Documento = c("4001", "4001", "4002"),
                  Empresa = "NTS", `Fecha de vencimiento` = as.Date(c("2026-11-01", "2026-12-01", "2026-11-03")),
                  Parte = "ACEROS", Cuotas = c(1L, 2L, 1L), check.names = FALSE, stringsAsFactors = FALSE)
RAW$Notas <- I(list(c("a", "b"), "c", character()))
f <- tryCatch(.datos_sap_filas(RAW), error = function(e) { message("  ERROR: ", conditionMessage(e)); NULL })
ok("prioritarias primero (Ledger, Empresa, Parte, Documento, Moneda), luego las de SAP en su orden",
   identical(head(names(f), 5), c("Ledger", "Empresa", "Parte", "Documento", "Moneda")) &&
     identical(setdiff(names(f), c("Ledger", "Empresa", "Parte", "Documento", "Moneda", "hd_id")),
               c("Saldo vencido", "Fecha de vencimiento", "Cuotas", "Notas")))
ok("id único aunque la parcialidad repita el Documento", !anyDuplicated(f$hd_id) && length(f$hd_id) == 3L)
ok("Ledger y Moneda existen aunque SAP no los mande (los filtros los necesitan)",
   is.data.frame(f) && all(is.na(f$Ledger)) && all(is.na(f$Moneda)))
ok("una columna lista sale como texto", identical(f$Notas, c("a, b", "c", "")))
ok("sin filas: 0 filas, con las columnas de los filtros",
   { v <- .datos_sap_filas(NULL); !nrow(v) && all(c("hd_id", "Ledger", "Empresa", "Moneda") %in% names(v)) })

# ── 2. datos_sap columns ─────────────────────────────────────────────────────
cols <- tryCatch(.datos_sap_columnas(f), error = function(e) { message("  ERROR: ", conditionMessage(e)); list() })
por <- function(campo) Find(function(c) identical(c$campo, campo), cols)
ok("una columna por campo, sin el id, con el nombre de SAP como título",
   identical(vapply(cols, function(c) c$campo, ""), setdiff(names(f), "hd_id")) &&
     identical(por("Saldo vencido")$titulo, "Saldo vencido"))
ok("fecha como fecha", identical(por("Fecha de vencimiento")$tipo, "fecha"))
ok("número con sus decimales (2), sin formato de dinero",
   identical(por("Saldo vencido")$tipo, "numero") && identical(por("Saldo vencido")$decimales, 2L))
ok("entero con 0 decimales; texto como texto",
   identical(por("Cuotas")$decimales, 0L) && identical(por("Documento")$tipo, "texto"))

# ── 3. The preview is what the ZIP holds ─────────────────────────────────────
catalogo_zip <- dplyr::bind_rows(catalogo, tibble::tibble(
  nombre = "SERVICIOS BAJIO SA", alias = "SERVBAJIO", clabe = "030180003333333333",
  medio_pago = "SPEI", banco = "030", Empresa = NA_character_))
agenda_zip <- dplyr::bind_rows(agenda0, pago("P7", "SERVICIOS BAJIO SA", "4007", 2500.75, "2026-11-04"))
MODAL <- NULL
showModal <- function(ui, ...) { MODAL <<- ui; invisible(NULL) }
vista <- NULL; lineas <- NULL; html_salidas <- ""
shared <- list(
  company_map = reactiveVal(c(NTS = EMP, OTR = OTR)), proveedores_db = reactiveVal(catalogo_zip),
  parte_alias_map_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(agenda_zip),
  sap_data = reactiveVal(NULL), sap_ov_db = reactiveVal(NULL), manual_inv = reactiveVal(NULL),
  bancos_confirmados = reactiveVal(tibble::tibble()), abonos_db = reactiveVal(NULL),
  effective_client_id = function() "dev", current_user = function() "luis",
    usuario = reactiveVal(list(user = "luis", permisos = list(can_register_payments = TRUE))), home_client_id = function() "dev")
error <- tryCatch({
  invisible(capture.output(testServer(pagarHoyServer, args = list(shared = shared), {
    session$userData$anota <- function(...) invisible(NULL)
    session$flushReact()
    session$setInputs(preview_all = 1)
    vista <<- .descargar_vista()
    # The pills and the button follow the choices (T21b): their own outputs.
    html_salidas <<- paste(as.character(output$descargar_resumen$html), as.character(output$descargar_boton$html))
    dir <- tempfile(); dir.create(dir)
    utils::unzip(output$download_all, exdir = dir)
    # Payment lines (type 02) per file.
    lineas <<- sum(vapply(list.files(dir, full.names = TRUE),
                          function(a) sum(startsWith(readLines(a, warn = FALSE), "02")), integer(1)))
  }))); NULL
}, error = function(e) conditionMessage(e))
if (!is.null(error)) message("  ", error)
html <- paste(paste(as.character(MODAL), collapse = ""), html_salidas)

ok("la vista previa lista todos los pagos AP (8), cada uno por su id",
   is.null(error) && is.data.frame(vista) && nrow(vista) == 8L && !anyDuplicated(vista$id))
ok("los que el ZIP omite dicen qué les falta: BODEGAS sin CLABE, TRANSPORTES sin alias",
   identical(sort(vista$motivo[!is.na(vista$motivo)]), c("no_clabe", "no_clabe", "no_match")) &&
     all(vista$motivo[vista$Parte == "BODEGAS DEL NORTE SA"] == "no_clabe") &&
     identical(vista$motivo[vista$Parte == "TRANSPORTES DESCONOCIDOS"], "no_match"))
ok("el botón cuenta solo lo que entra: 5 pagos, igual que las líneas del ZIP",
   grepl(tr("tablas.pagos.descargar_zip", n = 5), html, fixed = TRUE) && identical(lineas, 5L) &&
     sum(vista$zip == "entra") == 5L)
ok("las píldoras suman solo lo que entra (MXN 4 por $18,600.75; USD 1 por $1,200.00)",
   grepl(tr("tablas.pagos.pildora", n = 4, moneda = "MXN", total = fmt_money(18600.75)), html, fixed = TRUE) &&
     grepl(tr("tablas.pagos.pildora", n = 1, moneda = "USD", total = fmt_money(1200)), html, fixed = TRUE))
ok("la tabla es la del motor (descargar_t), no HTML a mano", grepl("descargar_t-hdt", html, fixed = TRUE) &&
     !grepl("<tbody>", html, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
