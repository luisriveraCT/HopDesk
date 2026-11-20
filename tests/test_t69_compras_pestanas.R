# =============================================================================
# tests/test_t69_compras_pestanas.R
# T69 (Luis, 2026-11-20): Compras sees only Panorama and Requisiciones, and a
# buyer's browser never receives treasury data.
#   1. TIER_REGISTRY$compras: pestanas PAN + REQ, no calendario, keeps
#      can_edit_provisiones ("Crear provisión" opens its modal from REQ).
#   2. nivel_tabla_permitida(): a tier with a tab list only gets its tabs'
#      tables; a tier without one gets every table, as before.
#   3. Every registered table is classified (HD_PESTANA_ARCHIVOS or
#      HD_ARCHIVOS_SIN_PESTANA): a new table cannot slip through unclassified.
#   4. hd_table_server, run: with the gate set to Compras, a treasury table
#      never pushes "hdt-datos"; Requisiciones' does; with no gate (old
#      fixtures) or a full tier, everything as before; a gate that fails = no.
#   5. app.R: the gate is set from the effective tier; a client tier with a
#      list loses the other tabs with nav_remove (never a staff session); the
#      Agenda's badges are gated by tab.
# Run from project root:  Rscript tests/test_t69_compras_pestanas.R
# Counter-proof: on master, 1, 2, 4 and 5 fail (Compras gets the Agenda).
# =============================================================================
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R",
            "R/hd_table.R", "R/permisos.R", "R/tier_registry.R")) source(f)

COMPRAS <- "compras"   # the registry's key; the test reads it from there
stopifnot(COMPRAS %in% names(TIER_REGISTRY))
TOTAL <- names(TIER_REGISTRY)[vapply(TIER_REGISTRY, function(e) is.null(e$pestanas) && !isTRUE(e$interno),
                                     logical(1))][1]

# ── 1. The registry ──────────────────────────────────────────────────────────
test_that("Compras: only Panorama and Requisiciones, no fixed Calendario", {
  expect_identical(nivel_pestanas(COMPRAS), c("PAN", "REQ"))
  expect_null(nivel_calendario(COMPRAS))
  expect_true("can_edit_provisiones" %in% TIER_REGISTRY[[COMPRAS]]$permisos)
  expect_true("can_view_requisiciones" %in% TIER_REGISTRY[[COMPRAS]]$permisos)
})

# ── 2. Which tables travel ───────────────────────────────────────────────────
test_that("Compras gets Requisiciones' and Panorama's tables, never treasury's", {
  for (t in c("requisiciones", "provisiones", "evidencia_excel", "avisos_publicados"))
    expect_true(nivel_tabla_permitida(COMPRAS, t), info = t)
  for (t in c("agenda_pagos", "agenda_cobros", "vencidos", "libro_banco", "lista_pasivos",
              "pasivos_tabla", "mapa_ic", "ic_saldos", "facturas_lupa", "datos_sap", "flujo_caja",
              "usuarios", "empresas", "cuentas_empresa"))
    expect_false(nivel_tabla_permitida(COMPRAS, t), info = t)
  expect_false(suppressMessages(nivel_tabla_permitida(COMPRAS, "no_registrada")))
})

test_that("a tier without a tab list gets every table, as before", {
  expect_false(is.na(TOTAL))
  for (t in c("agenda_pagos", "vencidos", "requisiciones", "no_registrada"))
    expect_true(nivel_tabla_permitida(TOTAL, t), info = t)
})

test_that("an unknown tier gets nothing (fail closed): NULL, empty, NA, not in the registry", {
  for (t in list(NULL, "", NA_character_, "no_es_nivel", c("finance", "compras")))
    expect_false(nivel_tabla_permitida(t, "requisiciones"), info = paste(t, collapse = ","))
  expect_false(nivel_pestana_permitida(NULL, "PH"))
  expect_false(nivel_pestana_permitida("", "PAN"))
  expect_true(nivel_pestana_permitida(TOTAL, "PH"))
  expect_false(nivel_pestana_permitida(COMPRAS, "PH"))
  expect_true(nivel_pestana_permitida(COMPRAS, "REQ"))
})

# ── 3. Every registered table is classified ─────────────────────────────────
test_that("every hd_registro table's module file is in HD_PESTANA_ARCHIVOS or HD_ARCHIVOS_SIN_PESTANA", {
  conocidos <- c(unlist(HD_PESTANA_ARCHIVOS, use.names = FALSE), HD_ARCHIVOS_SIN_PESTANA)
  arch <- vapply(hd_registro, function(r) r$archivo %||% NA_character_, character(1))
  sin <- names(arch)[!arch %in% conocidos]
  expect_identical(sin, character(), info = paste(sin, arch[sin], collapse = "; "))
  expect_true(all(names(HD_PESTANA_ARCHIVOS) %in% c("PAN", "CAL", "VEN", "PH", "BNC", "IC", "PSV", "REQ", "FC", "RPT")))
})

# ── 4. hd_table_server, run ──────────────────────────────────────────────────
filas <- data.frame(id = c("a", "b"), Referencia = c("F1", "F2"), Moneda = "MXN",
                    Importe = c(1000, 500), stringsAsFactors = FALSE)
cols <- list(col_texto("Referencia", "Referencia"), col_dinero("Importe", "Importe"))

envios <- function(tabla, puerta) {
  msgs <- list()
  testServer(function(input, output, session) {
    if (!is.null(puerta)) session$userData$hd_tabla_permitida <- puerta
    hd_table_server("tab", tabla = tabla, datos = reactive(filas), columnas = cols)
  }, {
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- list(type = type, message = message)
    session$setInputs(`tab-hdt_pedir` = 1)
    session$flushReact()
  })
  vapply(msgs, function(m) m$type, character(1))
}
puerta_de <- function(tier) function(tabla) nivel_tabla_permitida(tier, tabla)

test_that("Compras: a treasury table never sends its data; Requisiciones' does", {
  expect_false("hdt-datos" %in% envios("agenda_pagos", puerta_de(COMPRAS)))
  expect_false("hdt-datos" %in% envios("vencidos", puerta_de(COMPRAS)))
  expect_false("hdt-datos" %in% envios("libro_banco", puerta_de(COMPRAS)))
  expect_true("hdt-datos" %in% envios("requisiciones", puerta_de(COMPRAS)))
})

test_that("a full tier, or no gate at all (old fixtures): every table sends, as before", {
  expect_true("hdt-datos" %in% envios("agenda_pagos", puerta_de(TOTAL)))
  expect_true("hdt-datos" %in% envios("agenda_pagos", NULL))
})

test_that("tier not known yet: nothing travels; when a full tier arrives, the table goes out", {
  nivel <- reactiveVal("")
  msgs <- list()
  testServer(function(input, output, session) {
    session$userData$hd_tabla_permitida <- function(tabla) nivel_tabla_permitida(nivel(), tabla)
    hd_table_server("tab", tabla = "agenda_pagos", datos = reactive(filas), columnas = cols)
  }, {
    session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1L]] <<- list(type = type, message = message)
    session$flushReact()
    expect_false("hdt-datos" %in% vapply(msgs, function(m) m$type, character(1)))
    nivel(TOTAL)
    session$flushReact()
    expect_true("hdt-datos" %in% vapply(msgs, function(m) m$type, character(1)))
  })
})

test_that("a gate that fails counts as no (never sends, never stops the session)", {
  expect_false("hdt-datos" %in% suppressMessages(envios("agenda_pagos", function(tabla) stop("boom"))))
})

# ── 5. app.R and the Agenda's badges ─────────────────────────────────────────
codigo <- function(f) sub("#.*$", "", readLines(f, warn = FALSE, encoding = "UTF-8"))
app <- codigo(Sys.getenv("HD_APP_R", "app.R"))
test_that("app.R sets the gates from the effective tier", {
  expect_true(any(grepl("session$userData$hd_tabla_permitida <- function(tabla) nivel_tabla_permitida(tier_efectivo(), tabla)",
                        app, fixed = TRUE)))
  expect_true(any(grepl("session$userData$hd_pestana_permitida <- function(tab) nivel_pestana_permitida(tier_efectivo(), tab)",
                        app, fixed = TRUE)))
})
test_that("app.R removes the other tabs for a client tier with a list, never for staff", {
  i <- grep(".tabs_quitadas <- FALSE", app, fixed = TRUE)[1]
  expect_false(is.na(i))
  bloque <- app[i:(i + 20L)]
  expect_true(any(grepl("isTRUE(info$is_staff)) return()", bloque, fixed = TRUE)))
  expect_true(any(grepl("bslib::nav_remove(\"nav\", target = tab, session = session)", bloque, fixed = TRUE)))
  expect_true(any(grepl("setdiff(.TABS_CLIENTE, lista)", bloque, fixed = TRUE)))
})
test_that("the Agenda's badges are not sent to a tier without the Agenda tab", {
  ph <- codigo("R/pagar_hoy_module.R")
  i <- grep('session$sendCustomMessage("ph_update_badge"', ph, fixed = TRUE)[1]
  expect_false(is.na(i))
  antes <- ph[max(1L, i - 15L):i]
  expect_true(any(grepl('permitida("PH")', antes, fixed = TRUE)))
})
