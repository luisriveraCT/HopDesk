# =============================================================================
# tests/test_pasivos_tabla_caracterizacion_r.R
# Pasivos · Tabla as it is today (T17b, step 1): the safety net before it
# moves to the engine. The real pasivos_table_module_server() under
# testServer(), with a fake `shared` in memory (never S3), read through
# tests/helpers/pasivos_tabla_adaptador.R -- so the same cases can run
# against the engine's table later. Case numbers (C1..C28) are the ones in
# hopdesk_coord/tablas/reportes/T17b_paso0.md.
#
# Records TODAY's behaviour, including what Luis may change (archived
# liabilities still show, C29) and known failures (C26, the ' in an id).
# The whole grid is also compared with tests/fixtures/pasivos_tabla_antes.json
# (periods relative to today). PT_GENERAR=1 rewrites that file -- only on
# master, never to make a test pass.
#
# Run from the repo root: Rscript tests/test_pasivos_tabla_caracterizacion_r.R
# =============================================================================
suppressPackageStartupMessages({ library(testthat); library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", c("strip_accents", "fmt_money", "CURRENCIES"))
cargar("R/persistence.R", "confirmaciones_vigentes")
cargar("R/pasivos_observers.R", "isTRUE_safe")
for (f in c("R/pasivos_schemas.R", "R/pasivos_table_pivot.R", "R/pasivos_table_styles.R",
            "R/pasivos_table_module.R", "R/pasivos_list_module.R")) source(f)
source("tests/helpers/pasivos_datos_prueba.R")
source("tests/helpers/pasivos_tabla_adaptador.R")

# "+ Provisión manual" saves through these: in memory, in this process only
# (no aws.s3 anywhere). PERMITIDO says what has_capability() answers.
GUARDADAS <- new.env()
PERMITIDO <- TRUE
has_capability <- function(user, cap) PERMITIDO
save_pasivos_provisions <- function(df, client_id, base = NULL) { GUARDADAS$df <- df; GUARDADAS$client_id <- client_id; invisible(TRUE) }
bump_sync_version <- function(...) invisible(NULL)
pasivos_log_audit <- function(...) invisible(NULL)
find_proveedor_matches <- function(...) NULL

D <- pasivos_datos_prueba()
HOY <- D$hoy
nuevo_shared <- function(d = D) list(
  pasivos_provisions_db = reactiveVal(d$provisions), pasivos_liabilities_db = reactiveVal(d$liabilities),
  manual_inv = reactiveVal(d$manual_inv), bancos_confirmados = reactiveVal(d$bancos_confirmados),
  visible_initials = reactiveVal(d$visibles), empresa_sel = reactiveVal(d$visibles),
  proveedores_db = reactiveVal(NULL), current_user = reactiveVal("tester"), effective_client_id = function() "prueba",
  suppress_ledger_prov_refresh = reactiveVal(FALSE))

# Runs the module, applies `entradas` (list of setInputs calls, in order) and
# gives back the grid and the summary line as text.
ver <- function(entradas = list(), granularidad = "day", shared = nuevo_shared()) {
  out <- NULL
  suppressWarnings(testServer(pasivos_table_module_server, args = list(shared = shared), {
    session$flushReact()
    for (e in entradas) do.call(session$setInputs, e)
    session$flushReact()
    out <<- list(rejilla = pt_rejilla(output$pasivos_table$html, HOY, granularidad),
                 resumen = gsub("[[:space:]]+", " ", trimws(xml2::xml_text(xml2::read_html(paste0("<div>", output$summary_line$html, "</div>"))))))
  }))
  out
}
celda <- function(r, etiqueta, clave) pt_buscar_fila(r, etiqueta)$celdas[[clave]]

BASE <- ver()
R0 <- BASE$rejilla

# ── Rows and columns ────────────────────────────────────────────────────────
test_that("C1: rows by currency (MXN, USD, EUR, others), then company, category, name", {
  expect_identical(pt_etiquetas(R0), c("Papelería Centro", "Arrendamiento archivado", "Renta oficina", "Crédito Banorte",
                                       "Comisión por apertura", "Comisión por disposición", "Tarjeta Amex",
                                       "Licencias <b>Ñandú</b> & 'Co'"))
})
test_that("C2: one currency band before its first row", {
  tipos <- vapply(R0$filas, function(f) if (f$tipo == "moneda") paste0("=", f$moneda) else "f", character(1))
  expect_identical(tipos, c("=MXN", "f", "f", "f", "=USD", "f", "f", "f", "=EUR", "f", "=CAD", "f"))
})
test_that("C3: the initial fees go under their liability, in index order, without ✎, part or pill, never tinted", {
  for (e in c("Comisión por apertura", "Comisión por disposición")) {
    f <- pt_buscar_fila(R0, e)
    expect_true(f$comision); expect_true(is.na(f$editar)); expect_identical(f$parte, ""); expect_identical(f$urgencia, "")
  }
  expect_identical(celda(R0, "Comisión por apertura", "d+2")$texto, "$ 250.00")
})
test_that("C4: Día shows today - 7 .. today + 53; Semana / Mes / Año group", {
  expect_identical(vapply(R0$columnas, `[[`, character(1), "clave"), sprintf("d%+d", -7:53))
  s <- ver(list(list(granularity = "week")), "week")$rejilla
  expect_true(all(grepl("^s[+-]\\d+$", vapply(s$columnas, `[[`, character(1), "clave"))))
  m <- ver(list(list(granularity = "month")), "month")$rejilla
  expect_true("m+0" %in% vapply(m$columnas, `[[`, character(1), "clave"))
  a <- ver(list(list(granularity = "year")), "year")$rejilla
  expect_true("a+0" %in% vapply(a$columnas, `[[`, character(1), "clave"))
})
test_that("C5: today's column is marked, only it", {
  expect_identical(vapply(Filter(function(c) c$hoy, R0$columnas), `[[`, character(1), "clave"), "d+0")
})
test_that("C6: ← 60 / 60 → widen the window; -8 and +54 show only then", {
  expect_null(celda(R0, "Renta oficina", "d-8")); expect_null(celda(R0, "Renta oficina", "d+54"))
  r <- ver(list(list(extend_left = 1), list(extend_right = 1)))$rejilla
  expect_identical(celda(r, "Renta oficina", "d-8")$texto, "$ 900.00")
  expect_identical(celda(r, "Renta oficina", "d+54")$texto, "$ 950.00")
  expect_identical(vapply(r$columnas, `[[`, character(1), "clave")[c(1, length(r$columnas))], c("d-67", "d+113"))
})

# ── Cells ───────────────────────────────────────────────────────────────────
test_that("C7: a future provision: amount + ⚡ (convertir) with its id", {
  c1 <- celda(R0, "Renta oficina", "d+5")
  expect_identical(c1[c("texto", "estado", "marca", "boton", "boton_id")],
                   list(texto = "$ 1,300.00", estado = "provision", marca = "", boton = "convertir", boton_id = "p1d"))
})
test_that("C8: an overdue provision: amber, still ⚡", {
  expect_identical(celda(R0, "Renta oficina", "d-3")[c("estado", "boton", "boton_id")],
                   list(estado = "provision_vencida", boton = "convertir", boton_id = "p1a"))
})
test_that("C9: an item pending ◷ ↩, overdue ! ↩, with the provision's id", {
  expect_identical(celda(R0, "Crédito Banorte", "d+3")[c("texto", "estado", "marca", "boton", "boton_id")],
                   list(texto = "$ 700.00", estado = "pendiente", marca = "pendiente", boton = "comprobante", boton_id = "p2p"))
  expect_identical(celda(R0, "Crédito Banorte", "d-1")[c("texto", "estado", "marca", "boton", "boton_id")],
                   list(texto = "$ 710.00", estado = "pendiente_vencido", marca = "vencido", boton = "comprobante", boton_id = "p2v"))
})
test_that("C10: a confirmed item: ✓, no button; a deleted confirmation doesn't confirm", {
  expect_identical(celda(R0, "Crédito Banorte", "d-2")[c("estado", "marca", "boton")],
                   list(estado = "confirmado", marca = "confirmado", boton = ""))
  expect_identical(celda(R0, "Crédito Banorte", "d-4")$estado, "pendiente_vencido")
})
test_that("C11: a cell with a provision sends its id on click; an empty one has nothing", {
  expect_identical(celda(R0, "Renta oficina", "d+1")$clic_id, "p1c")
  expect_identical(celda(R0, "Crédito Banorte", "d-2")$clic_id, "p2c")
  expect_null(celda(R0, "Renta oficina", "d+2"))
})
test_that("C12: an amount or a date changed by hand is marked", {
  expect_true(celda(R0, "Renta oficina", "d+20")$a_mano)
  expect_true(celda(R0, "Renta oficina", "d+30")$a_mano)
  expect_false(celda(R0, "Renta oficina", "d+5")$a_mano)
})
test_that("C13: the row is tinted by its most urgent: overdue > within 2 days > within 7 days", {
  expect_identical(pt_buscar_fila(R0, "Renta oficina")$urgencia, "vencida")
  expect_identical(pt_buscar_fila(R0, "Papelería Centro")$urgencia, "pronto")
  expect_identical(pt_buscar_fila(R0, "Tarjeta Amex")$urgencia, if (D$lunes - HOY <= 2L) "muy_pronto" else "pronto")
  expect_identical(celda(R0, "Renta oficina", "d+0")$vence_hoy, TRUE)
})
test_that("C14 [CAMBIA, P4]: one liability, one day, two kinds: amounts summed, kind and id of the first", {
  expect_identical(celda(R0, "Renta oficina", "d+20")[c("texto", "estado", "boton_id")],
                   list(texto = "$ 1,950.00", estado = "provision", boton_id = "p1e"))
})
test_that("C15 [CAMBIA, P4]: Semana with two in one week: summed, kind and id of the first", {
  s <- ver(list(list(granularity = "week")), "week")$rejilla
  k <- .pt_clave_periodo(format(D$lunes, "%G-W%V"), HOY, "week")
  expect_identical(celda(s, "Tarjeta Amex", k)[c("texto", "estado", "boton_id")],
                   list(texto = "$ 350.00", estado = "provision", boton_id = "p3a"))
})

# ── Row ─────────────────────────────────────────────────────────────────────
test_that("C16: ✎ only on real liabilities, sending the liability's id (the same event as the Lista, T18)", {
  expect_identical(pt_buscar_fila(R0, "Renta oficina")$editar, "lib1")
  expect_true(is.na(pt_buscar_fila(R0, "Papelería Centro")$editar))
})
test_that("C17: the part under the name, the currency pill beside it", {
  f <- pt_buscar_fila(R0, "Crédito Banorte")
  expect_identical(c(f$parte, f$moneda), c("Banorte", "USD"))
})

# ── Filters and summary ─────────────────────────────────────────────────────
test_that("C18: Empresa, Categoría, Subcategoría, Moneda, Provisiones and Buscar (no accents)", {
  expect_identical(pt_etiquetas(ver(list(list(empresa_sel = "BOD")))$rejilla),
                   c("Crédito Banorte", "Comisión por apertura", "Comisión por disposición"))
  expect_identical(pt_etiquetas(ver(list(list(categoria_sel = "tarjeta")))$rejilla), c("Papelería Centro", "Tarjeta Amex"))
  expect_identical(pt_etiquetas(ver(list(list(subcategoria_sel = "software")))$rejilla),
                   c("Papelería Centro", "Licencias <b>Ñandú</b> & 'Co'"))
  expect_identical(pt_etiquetas(ver(list(list(currency_sel = "USD")))$rejilla),
                   c("Crédito Banorte", "Comisión por apertura", "Comisión por disposición"))
  expect_identical(pt_etiquetas(ver(list(list(provision_type = "manuales")))$rejilla), "Papelería Centro")
  expect_false("Papelería Centro" %in% pt_etiquetas(ver(list(list(provision_type = "programadas")))$rejilla))
  expect_identical(pt_etiquetas(ver(list(list(search_text = "credito")))$rejilla),
                   c("Papelería Centro", "Crédito Banorte", "Comisión por apertura", "Comisión por disposición"))
  expect_identical(pt_etiquetas(ver(list(list(search_text = "american")))$rejilla), c("Papelería Centro", "Tarjeta Amex"))
})
test_that("C19: an orphan passes the company filter by its own company", {
  expect_true("Papelería Centro" %in% pt_etiquetas(ver(list(list(empresa_sel = "NRS")))$rejilla))
  expect_false("Papelería Centro" %in% pt_etiquetas(ver(list(list(empresa_sel = "NTS")))$rejilla))
})
# Nothing at all: BOD has no orphan, and Manuales keeps only orphans.
NADA <- list(list(empresa_sel = "BOD"), list(provision_type = "manuales"))
test_that("C20: the summary line: counts, and per currency what is due in the next 7 days", {
  # L3's Monday is always within today + 1 .. today + 7.
  expect_identical(BASE$resumen, paste(
    "Mostrando 8 pasivos · 15 provisiones · 5 items",
    "MXN $ 3,680.00 · 4 próx.", "USD $ 370.00 · 2 próx.", "EUR $ 300.00 · 1 próx.", "CAD $ 60.00 · 1 próx."))
  expect_match(ver(list(list(provision_type = "manuales")))$resumen, "^Mostrando 1 provisiones manuales · 0 items")
  expect_identical(ver(NADA)$resumen, "Sin datos para los filtros seleccionados.")
})
test_that("C21: no rows: 'No hay pasivos para los filtros seleccionados.'", {
  expect_identical(ver(NADA)$rejilla$vacio, "No hay pasivos para los filtros seleccionados.")
})
test_that("C18b [visto de paso]: Buscar filters liabilities only: an orphan provision shows whatever is searched", {
  expect_identical(pt_etiquetas(ver(list(list(search_text = "zzz")))$rejilla), "Papelería Centro")
})

# ── + Provisión manual ──────────────────────────────────────────────────────
alta <- function(permitido) {
  PERMITIDO <<- permitido
  GUARDADAS$df <- NULL
  sh <- nuevo_shared()
  suppressWarnings(testServer(pasivos_table_module_server, args = list(shared = sh), {
    session$setInputs(btn_add_provision_manual = 1)
    session$setInputs(ppm_empresa = "NTS", ppm_moneda = "MXN", ppm_parte = "Papelería Nueva", ppm_codigo = "",
                      ppm_importe = 321.5, ppm_fecha = HOY + 9L, ppm_documento = "", ppm_referencia = "", ppm_notas = "")
    session$setInputs(ppm_save = 1)
    session$flushReact()
    GUARDADAS$rejilla <- pt_rejilla(output$pasivos_table$html, HOY)
  }))
  GUARDADAS
}
test_that("C23: + Provisión manual saves an orphan provision and its cell shows on its date without reloading", {
  g <- alta(TRUE)
  expect_identical(g$client_id, "prueba")
  nueva <- g$df[!g$df$id %in% D$provisions$id, ]
  expect_identical(c(nrow(nueva), nueva$origin, nueva$estado, nueva$parte), c("1", "manual", "provisional", "Papelería Nueva"))
  expect_true(is.na(nueva$liability_id))
  expect_identical(celda(g$rejilla, "Papelería Nueva", "d+9")$texto, "$ 321.50")
})
test_that("C24: without pasivos.create_provision_manual it doesn't save", {
  expect_null(alta(FALSE)$df)
  PERMITIDO <<- TRUE
})

# ── Safety and data ─────────────────────────────────────────────────────────
test_that("C25: names, parts and references are text, never HTML", {
  f <- pt_buscar_fila(R0, "Licencias <b>Ñandú</b> & 'Co'")
  expect_identical(f$parte, "Proveedor \"X\" <i>")
})
test_that("C26 [falla conocida]: a ' in a liability id breaks the ✎ (htmlEscape inside onclick, PENDIENTES)", {
  # The onclick reads ...('pasivos_edit_liability','lib'4',...): the ' closes
  # the string early and the click does nothing. Other ids work. Step 3 turns
  # this expectation around (the engine sends ids, not code).
  expect_false(pt_buscar_fila(R0, "Licencias <b>Ñandú</b> & 'Co'")$editar_funciona)
  expect_true(pt_buscar_fila(R0, "Renta oficina")$editar_funciona)
})
test_that("C27: a company outside visible_initials shows nowhere", {
  expect_false("Luz bodega" %in% pt_etiquetas(R0))
  expect_false(grepl("777", BASE$resumen))
})
test_that("C28: closed, deleted, unknown and dateless provisions don't show; the unknown one warns", {
  f <- pt_buscar_fila(R0, "Renta oficina")
  expect_false(any(c("d+6", "d+7", "d+8", "d+9") %in% names(f$celdas)))
  sh <- nuevo_shared()
  expect_warning(testServer(pasivos_table_module_server, args = list(shared = sh), {
    session$flushReact(); output$pasivos_table
  }), "unknown estado values seen: en_revision")
})
test_that("C29 [Luis decide]: an ARCHIVED liability's overdue provision still shows, with ⚡ (the Calendar hides it since CM06)", {
  expect_identical(celda(R0, "Arrendamiento archivado", "d-5")[c("estado", "boton_id")],
                   list(estado = "provision_vencida", boton_id = "p6"))
})

# ── The whole grid, against the reference ───────────────────────────────────
test_that("the whole Día grid is the reference's (all but L3, whose Monday moves with the weekday: C13, C15)", {
  # Semana's columns move with the weekday too; C4 and C15 cover it.
  ahora <- R0
  ahora$filas <- Filter(function(f) !identical(f$etiqueta, "Tarjeta Amex"), ahora$filas)
  json <- jsonlite::toJSON(ahora, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
  archivo <- "tests/fixtures/pasivos_tabla_antes.json"
  if (identical(Sys.getenv("PT_GENERAR"), "1")) writeLines(json, archivo, useBytes = TRUE)
  expect_identical(as.character(json), paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
})
