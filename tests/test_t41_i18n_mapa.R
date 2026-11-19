# =============================================================================
# tests/test_t41_i18n_mapa.R
# T41 · the Mapa de tesorería's visible text from the dictionary
# (tablas.mapa.*):
#   1. Spanish identical to before: every key, with its arguments, gives the
#      literal the module wrote by hand until T41.
#   2. The UI (treasuryMapUI) in es has those literals; in en, none of them.
#   3. The server's outputs (nodes, edges, routes, plan) in es and en.
# Run from project root:  Rscript tests/test_t41_i18n_mapa.R
# Contraprueba: TMAP_R=<the module before T41> fails sections 2 and 3 in en.
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
seccion <- function(t) cat("\n--", t, "--\n")
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% nombres) eval(e, envir = globalenv())

source("R/monedas.R")
source("R/ppl_generator.R")
cargar("R/global.R", c("strip_accents"))
cargar("R/erp_connector_registry.R", "SAP_B1_ERP_TYPE")
cargar("R/vencidos_module.R", "facturas_ids")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R")) source(f)
.i18n_dir <- file.path(getwd(), "www", "i18n")
puede <- function(usuario, permiso) TRUE
source(Sys.getenv("TMAP_R", "R/treasury_map_module.R"))
updateSelectInput <- function(...) invisible(NULL)
idioma <- function(x) options(hopdesk.idioma = x)

# ── 1. Spanish identical ────────────────────────────────────────────────────
seccion("1. every tablas.mapa.* key in es == the literal written by hand before T41")
idioma("es")
ANTES <- list(
  list("tablas.mapa.empresa_objetivo", list(), "Empresa objetivo:"),
  list("tablas.mapa.moneda", list(), "Moneda:"),
  list("tablas.mapa.rutas_titulo", list(), "Rutas al objetivo"),
  list("tablas.mapa.plan_titulo", list(), "Plan de pago"),
  list("tablas.mapa.enviar_agenda", list(), "Enviar a Agenda de Hoy"),
  list("tablas.mapa.sin_datos", list(), "sin datos"),
  list("tablas.mapa.nodo_saldo", list(moneda = "USD"), "Saldo USD:"),
  list("tablas.mapa.arista_facturas", list(n = 3L), "3 factura(s)"),
  list("tablas.mapa.sin_saldos", list(), "No hay saldos IC para esta moneda."),
  list("tablas.mapa.sin_rutas", list(empresa = "NTS"), "Ninguna empresa debe directa o indirectamente a NTS."),
  list("tablas.mapa.directo", list(), "directo"),
  list("tablas.mapa.pasos", list(n = 2), "2 pasos"),
  list("tablas.mapa.max", list(), "máx"),
  list("tablas.mapa.detalle_titulo", list(), "Detalle de facturas"),
  list("tablas.mapa.detalle_ayuda", list(), "Haz clic en una flecha del grafo para ver las facturas de ese par de empresas."),
  list("tablas.mapa.debe_a", list(de = "NCS", a = "NTS"), "NCS debe a NTS"),
  list("tablas.mapa.agregar_ayuda", list(), "Selecciona filas y usa ‘Agregar al plan’ para incluirlas en el plan de pago."),
  list("tablas.mapa.agregar_plan", list(), "Agregar al plan"),
  list("tablas.mapa.plan_vacio", list(), "Haz clic en una flecha, selecciona facturas y agrégalas aquí."),
  list("tablas.mapa.total", list(), "Total"),
  list("tablas.mapa.limpiar", list(), "Limpiar"))
for (a in ANTES)
  ok(sprintf("%s == \"%s\"", a[[1]], a[[3]]), identical(do.call(tr, c(list(a[[1]]), a[[2]])), a[[3]]))
ok("comun.cerrar (the edge panel's close button) is still \"Cerrar\"", identical(tr("comun.cerrar"), "Cerrar"))
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
mapa <- grep("^tablas\\.mapa\\.", names(es), value = TRUE)
ok("es has exactly the 21 tablas.mapa.* keys, and en the same ones",
   length(mapa) == 21L && setequal(mapa, grep("^tablas\\.mapa\\.", names(en), value = TRUE)) &&
     setequal(mapa, vapply(ANTES, `[[`, "", 1)))
ok("en has its own text for every key but Total", all(vapply(setdiff(mapa, "tablas.mapa.total"), function(k) !identical(es[[k]], en[[k]]), TRUE)))
# The Spanish texts that must not appear in English (long enough to be unambiguous).
ES_TEXTOS <- vapply(ANTES, `[[`, "", 3)
ES_TEXTOS <- setdiff(ES_TEXTOS[nchar(ES_TEXTOS) >= 6], "Total")
hay_es <- function(html) ES_TEXTOS[vapply(ES_TEXTOS, function(t) grepl(t, html, fixed = TRUE), TRUE)]

# ── 2. UI ───────────────────────────────────────────────────────────────────
seccion("2. treasuryMapUI in es and en")
ui_html <- function() as.character(htmltools::renderTags(treasuryMapUI("m"))$html)
idioma("es"); h <- ui_html()
ok("es: the labels, panel titles and send button are as before",
   all(vapply(c("Empresa objetivo:", "Moneda:", "Rutas al objetivo", "Plan de pago", " Enviar a Agenda de Hoy"),
              function(t) grepl(t, h, fixed = TRUE), TRUE)))
idioma("en"); h <- ui_html()
ok("en: Target company:, Currency:, Routes to the target, Payment plan, Send to Today's Agenda",
   all(vapply(c("Target company:", "Currency:", "Routes to the target", "Payment plan", "Send to Today's Agenda"),
              function(t) grepl(t, h, fixed = TRUE), TRUE)))
ok("en: no Spanish text left in the UI", { x <- hay_es(h); if (length(x)) cat("     left:", x, sep = " | "); !length(x) })

# ── 3. Server outputs ───────────────────────────────────────────────────────
seccion("3. treasuryMapServer outputs in es and en")
# BBB's AR on Empresa A: AAA owes BBB (one edge, 2 invoices in MXN).
ar_map <- data.frame(.ini = "BBB", Parte = "Empresa A", Documento = c("D1", "D2"),
                     `Fecha de vencimiento` = as.Date("2026-10-05"), `Saldo vencido` = c(100, 200),
                     Moneda = "MXN", check.names = FALSE, stringsAsFactors = FALSE)
CTAS <- data.frame(id = "c1", Empresa = "AAA", Moneda = "MXN", activa = TRUE, saldo_inicial = 1000,
                   stringsAsFactors = FALSE)
sh <- list(company_map = function() c(AAA = "Empresa A", BBB = "Empresa B"), current_user = function() "tester",
           ctas_cuentas = function() CTAS, bancos_movimientos = function() NULL,
           moneda_base_cliente = reactive("MXN"))
salidas <- function(lang) {
  idioma(lang); r <- list()
  shiny::testServer(treasuryMapServer, args = list(shared = sh, ic_invoices_rv = reactive(list(ar = ar_map))), {
    session$setInputs(map_cur = "MXN", target_co = "BBB"); session$flushReact()
    md <- map_data()
    r$nodos   <<- paste(md$nodes$title, collapse = "\n")
    r$aristas <<- paste(md$edges$title, collapse = "\n")
    r$rutas   <<- output$bfs_routes_ui$html
    r$plan    <<- output$payment_plan_ui$html
    r$detalle <<- output$edge_detail_ui$html
    session$setInputs(target_co = "AAA"); session$flushReact()
    r$sin_rutas <<- output$bfs_routes_ui$html
    session$setInputs(map_cur = "EUR"); session$flushReact()
    r$sin_saldos <<- output$bfs_routes_ui$html
    r$nodos_eur  <<- paste(map_data()$nodes$title, collapse = "\n")
    # One invoice in the plan: the item, its Total and Limpiar.
    payment_plan(list(list(from_ini = "AAA", to_ini = "BBB", Documento = "D1", Importe = 100, Moneda = "MXN")))
    session$flushReact()
    r$plan_lleno <<- output$payment_plan_ui$html
  })
  r
}
s <- salidas("es")
# [CAMBIA T55c] the IC Map's amounts: the code after the number ("1,000 MXN").
ok("es: node bubble 'Saldo MXN: <b>1,000 MXN</b>' as before", grepl("Saldo MXN: <b>1,000 MXN</b>", s$nodos, fixed = TRUE))
ok("es: a node with no balance in EUR says 'sin datos'", grepl("Saldo EUR: <b>sin datos</b>", s$nodos_eur, fixed = TRUE))
ok("es: edge bubble ends with '2 factura(s)'", grepl("<br/>2 factura(s)", s$aristas, fixed = TRUE))
ok("es: the route says 'directo' and 'máx 300 MXN'",
   grepl(">directo<", s$rutas, fixed = TRUE) && grepl("máx 300 MXN", s$rutas, fixed = TRUE))
ok("es: nobody owes AAA: 'Ninguna empresa debe directa o indirectamente a AAA.'",
   grepl("Ninguna empresa debe directa o indirectamente a AAA.", s$sin_rutas, fixed = TRUE))
ok("es: EUR has no edges: 'No hay saldos IC para esta moneda.'", grepl("No hay saldos IC para esta moneda.", s$sin_saldos, fixed = TRUE))
ok("es: empty plan and edge detail as before",
   grepl("Haz clic en una flecha, selecciona facturas y agrégalas aquí.", s$plan, fixed = TRUE) &&
     grepl("Detalle de facturas", s$detalle, fixed = TRUE) &&
     grepl("Haz clic en una flecha del grafo para ver las facturas de ese par de empresas.", s$detalle, fixed = TRUE))
ok("es: the plan's Total and Limpiar", grepl(">Total<", s$plan_lleno, fixed = TRUE) && grepl(" Limpiar", s$plan_lleno, fixed = TRUE))

s <- salidas("en")
ok("en: node bubble 'Balance MXN:' and 'no data'",
   grepl("Balance MXN: <b>1,000 MXN</b>", s$nodos, fixed = TRUE) && grepl("Balance EUR: <b>no data</b>", s$nodos_eur, fixed = TRUE))
ok("en: edge bubble '2 invoice(s)'", grepl("<br/>2 invoice(s)", s$aristas, fixed = TRUE))
ok("en: route 'direct' and 'max 300 MXN'", grepl(">direct<", s$rutas, fixed = TRUE) && grepl("max 300 MXN", s$rutas, fixed = TRUE))
ok("en: 'No company owes AAA, directly or indirectly.' and 'No IC balances in this currency.'",
   grepl("No company owes AAA, directly or indirectly.", s$sin_rutas, fixed = TRUE) &&
     grepl("No IC balances in this currency.", s$sin_saldos, fixed = TRUE))
ok("en: empty plan, edge detail, Total and Clear",
   grepl("Click an arrow, select invoices and add them here.", s$plan, fixed = TRUE) &&
     grepl("Invoice detail", s$detalle, fixed = TRUE) && grepl(" Clear", s$plan_lleno, fixed = TRUE))
ok("en: no Spanish text left in any output", {
  x <- hay_es(paste(unlist(s), collapse = "\n"))
  x <- c(x, if (grepl("factura(s)", s$aristas, fixed = TRUE)) "factura(s)",
            if (grepl(">directo<", s$rutas, fixed = TRUE)) "directo")
  if (length(x)) cat("     left:", x, sep = " | ")
  !length(x)
})
idioma("es")

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
quit(status = if (.fail) 1L else 0L)
