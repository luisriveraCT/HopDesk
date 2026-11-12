# =============================================================================
# tests/test_abono_caracterizacion_r.R
# Abono parcial (R/staging_browse_module.R). Step 0b of T25 recorded here how
# master's hand-made table worked; since the migration the same cases run on
# hd_table "abono_parcial". The real setup_abono_browse() runs under
# shiny::testServer over tests/helpers/abono_datos_prueba.R; the Agenda read
# and save, log_action and the notifications are stubs in this process (never
# S3). The table is read through tests/helpers/abono_adaptador.R; ticking and
# typing go in as the engine's inputs (ab_tabla-sel, ab_tabla-ediciones).
# [CAMBIA] marks what reportes/T25_diseno.md changes on purpose (approved C1-
# C7 and rule 17); each one says how it was before.
#
# Run from project root:  Rscript tests/test_abono_caracterizacion_r.R
# =============================================================================
suppressPackageStartupMessages({ library(testthat); library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/global.R", "CURRENCIES")
cargar("R/persistence.R", c("active_abonos_summary", "upsert_pagar_hoy"))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/staging_browse_module.R")) source(f)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
source("tests/helpers/abono_datos_prueba.R")
source("tests/helpers/abono_adaptador.R")

HOY <- Sys.Date()
D <- abono_datos_prueba(HOY)

# ── Stubs: what the module calls outside itself, recorded ───────────────────
R <- new.env()
reiniciar <- function(falla_lectura = FALSE, falla_guardado = FALSE) {
  R$avisos <- list(); R$guardados <- list(); R$bitacora <- list(); R$cierres <- 0L
  R$falla_lectura <- falla_lectura; R$falla_guardado <- falla_guardado
  R$s3 <- D$pagar_hoy
}
safe_load_pagar_hoy <- function(username = NULL, client_id = NULL) {
  if (isTRUE(R$falla_lectura)) stop("S3 read failed")
  R$s3
}
save_pagar_hoy <- function(df, username = NULL, client_id = NULL, base = NULL) {
  if (isTRUE(R$falla_guardado)) stop("S3 write failed")
  R$guardados[[length(R$guardados) + 1L]] <- list(df = df, usuario = username, cliente = client_id, base = base)
  invisible(TRUE)
}
log_action <- function(...) R$bitacora[[length(R$bitacora) + 1L]] <- list(...)
showNotification <- function(ui, ..., type = "default") R$avisos[[length(R$avisos) + 1L]] <- list(texto = paste(ui, collapse = ""), tipo = type)
removeModal <- function(...) R$cierres <- R$cierres + 1L

# ── The module, as app.R wires it (top-level session, inputs ab_*) ──────────
correr <- function(codigo, autorizado = TRUE, sap = D$sap, agenda_sesion = tibble::tibble()) {
  srv <- function(input, output, session) {
    ph <- reactiveVal(agenda_sesion)
    ab <- reactiveVal(D$abonos)
    session$userData$ph <- ph; session$userData$abonos <- ab
    session$userData$ab <- setup_abono_browse(input, output, session,
      sap_data = reactive(sap), abonos_db = ab, pagar_hoy_db = ph,
      current_user = function() "tester", client_id = function() "cliente_a",
      autorizado = function() autorizado)
  }
  eval(bquote(testServer(srv, .(substitute(codigo)))))
}
filtros <- function(session, ledger = "AP", desde = HOY - 15L, hasta = HOY + 15L, empresa = "", moneda = "")
  session$setInputs(ab_ledger = ledger, ab_desde = desde, ab_hasta = hasta, ab_empresa = empresa, ab_moneda = moneda)
datos   <- function(session) session$userData$ab$facturas()
rejilla <- function(session) ab_rejilla(datos(session))
# Tick the invoices named in `importes` (Documento = amount) and type each
# amount that isn't NA in its cell, then "Enviar a Agenda" -- as the engine's
# browser side reports them.
enviar <- function(session, importes) {
  d <- datos(session)
  ids <- d$id[match(names(importes), d$Documento)]
  escritas <- !is.na(importes)
  session$setInputs(`ab_tabla-sel` = ids,
                    `ab_tabla-ediciones` = list(ids = as.list(ids[escritas]), valores = as.list(unname(importes[escritas]))))
  session$setInputs(ab_stage = (session$input$ab_stage %||% 0) + 1)
}
textos <- function(tipo) vapply(Filter(function(a) identical(a$tipo, tipo), R$avisos), `[[`, "", "texto")
nuevas <- function(k = 1L) { g <- R$guardados[[k]]$df; g[g$id != "ph1", ] }

# ── The table ────────────────────────────────────────────────────────────────
test_that("A1 AP, default window: invoices with a balance, net of confirmed abonos only", {
  reiniciar()
  correr({
    filtros(session)
    g <- rejilla(session)
    e <- D$esperado
    expect_setequal(g$Documento, e$Documento)
    expect_identical(g$Saldo[match(e$Documento, g$Documento)], e$Saldo)
    # F-103: 400 - 150 confirmed; F-101's voided abono and AR's abono don't
    # count; F-102's abono pending in the Agenda doesn't either; F-301 is
    # fully paid and gone; F-401 / F-402 fall outside the window.
    expect_false(any(c("F-301", "F-401", "F-402") %in% g$Documento))
  })
})

test_that("[CAMBIA C7] A2 painted order: currency bands, then due date, a group's invoices together", {
  # Before: no bands -- F-101, F-102, F-103, F-201, F-501, F-403 (USD in
  # the middle, by due date).
  reiniciar()
  correr({
    filtros(session)
    expect_identical(rejilla(session)$Documento, c("F-101", "F-102", "F-103", "F-501", "F-403", "F-201"))
    expect_identical(datos(session)$Documento, c("F-101", "F-201", "F-102", "F-103", "F-501", "F-403"))   # by due date
  })
})

test_that("A3 groups: Parte + Empresa + Moneda with 2 or more; the same Parte in another company stands alone", {
  reiniciar()
  correr({
    filtros(session)
    g <- rejilla(session)
    e <- D$esperado
    expect_identical(g$Grupo[match(e$Documento, g$Documento)], e$Grupo)
    gr <- ab_grupos(datos(session))
    expect_identical(gr$Grupo, "Proveedor Uno|NCS|MXN")
    expect_identical(gr$n, 3L)
  })
})

test_that("[CAMBIA C3] A4 formats: the engine's money with cents and its date", {
  # Before: "MXN 2,500" for 2,500.50 (no cents), group total "MXN 3,750",
  # due date dd/mm/yy.
  reiniciar()
  correr({
    filtros(session)
    g <- rejilla(session)
    expect_identical(g$SaldoTxt[g$Documento == "F-102"], "$2,500.50")
    expect_identical(g$Vence[g$Documento == "F-101"], .ab_columnas()[[5]]$valores(HOY - 3L)$mostrar)
    expect_identical(g$FechaVenc[g$Documento == "F-101"], as.character(HOY - 3L))
  })
})

test_that("[CAMBIA C4] A5 reference: Factura when there is one, else Documento; Documento in its own column", {
  # Before: one cell, "Doc F-102" under the reference only when they differed.
  reiniciar()
  correr({
    filtros(session)
    d <- datos(session)
    expect_identical(d$Referencia[match(c("F-101", "F-102"), d$Documento)], c("F-101", "A-77"))
    expect_true("Documento" %in% vapply(.ab_columnas(), `[[`, "", "campo"))
  })
})

test_that("A6 server filters: company, currency, AR and the date window (both ends included)", {
  reiniciar()
  correr({
    filtros(session, empresa = "NTS");   expect_setequal(rejilla(session)$Documento, c("F-501", "F-403"))
    filtros(session, moneda = "USD");    expect_identical(rejilla(session)$Documento, "F-201")
    filtros(session, desde = HOY, hasta = HOY + 2L)
    expect_identical(datos(session)$Documento, c("F-201", "F-102"))
    filtros(session, hasta = HOY + 15L, desde = HOY + 15L, empresa = "NTS")
    expect_identical(rejilla(session)$Documento, "F-403")
    filtros(session, ledger = "AR")
    g <- rejilla(session)
    expect_identical(g$Documento, "F-901"); expect_identical(g$Codigo, "C001")
  })
})

test_that("A7 empty states: no SAP data, nothing in range, nothing left to pay", {
  reiniciar()
  correr({
    filtros(session, ledger = "AR")
    expect_identical(ab_vacio(datos(session)), "Sin datos SAP para este tipo (CxP o CxC).")
  }, sap = list(AP = D$sap$AP, AR = NULL))
  correr({
    filtros(session, desde = HOY + 30L, hasta = HOY + 40L)
    expect_identical(ab_vacio(datos(session)), "Sin facturas en ese rango.")
    filtros(session, desde = HOY, hasta = HOY, empresa = "NTS")     # only F-301, fully paid
    expect_identical(ab_vacio(datos(session)), "Sin facturas con saldo pendiente en ese rango.")
  })
})

test_that("A8 ids: ledger|Empresa|Moneda|Documento, one per invoice", {
  reiniciar()
  correr({
    filtros(session)
    d <- datos(session)
    expect_identical(d$id[d$Documento == "F-201"], "AP|NCS|USD|F-201")
    expect_false(anyDuplicated(d$id) > 0L)
  })
})

# ── Enviar a Agenda ──────────────────────────────────────────────────────────
test_that("E1 two abonos: two Agenda rows, field by field, over the fresh read; message, log, modal closes", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, c("F-101" = 400, "F-201" = NA))      # F-201 not typed in: its balance, 800
    expect_length(R$guardados, 1L)
    gd <- R$guardados[[1]]
    expect_identical(gd$base, D$pagar_hoy)                       # the fresh read is the base (A2)
    expect_identical(gd$cliente, "cliente_a"); expect_identical(gd$usuario, "tester")
    n <- nuevas()
    expect_identical(nrow(gd$df), 3L)                            # ph1 kept
    expect_identical(n$Documento, c("F-101", "F-201"))
    expect_identical(n$ledger, c("AP", "AP"))
    expect_identical(n$Empresa, c("NCS", "NCS"))
    expect_identical(n$Moneda, c("MXN", "USD"))
    expect_identical(n$Parte, c("Proveedor Uno", "Proveedor Dos"))
    expect_identical(n$Codigo, c("P001", "P002"))                # trimmed
    expect_identical(n$tipo_item, c("abono", "abono"))
    expect_identical(n$Importe, c(400, 800))
    expect_identical(n$FechaVenc, HOY + c(-3L, 1L))              # the invoice's due date
    expect_identical(n$staged_by, c("tester", "tester"))
    expect_identical(n$status, c("pending", "pending"))
    expect_identical(n$source, c("manual", "manual"))
    expect_identical(names(n), c("id", "ledger", "Empresa", "Moneda", "Documento", "Parte", "Codigo", "tipo_item",
                                 "Importe", "FechaVenc", "staged_by", "staged_at", "status", "source"))
    expect_true(all(nchar(n$id) == 36L) && !anyDuplicated(n$id))
    expect_identical(session$userData$ph()$Documento, gd$df$Documento)   # the session copy too
    expect_identical(textos("message"), "2 abonos enviados a Agenda de hoy. Confirma para aplicar al saldo.")
    expect_length(R$bitacora, 1L)
    expect_identical(R$bitacora[[1]]$action, "enviar_abonos_a_agenda")
    expect_identical(R$bitacora[[1]]$description, "2 abonos enviados a Agenda de hoy")
    expect_identical(R$bitacora[[1]]$metadata, list(n = 2L, ledger = "AP"))
    expect_identical(R$cierres, 1L)
  })
})

test_that("E2 one abono: singular message; an amount equal to the balance is accepted", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, c("F-102" = 2500.5))
    expect_identical(nuevas()$Importe, 2500.5)
    expect_identical(textos("message"), "1 abono enviado a Agenda de hoy. Confirma para aplicar al saldo.")
  })
})

test_that("E3 over the balance or <= 0: nothing staged, the warning names them, the modal stays", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, c("F-103" = 300, "F-101" = 0))
    expect_length(R$guardados, 0L)
    # Before, in the order ticked (F-103, F-101); now in the table's.
    expect_identical(textos("warning"), "No se enviaron 2 abonos por exceder el saldo disponible: F-101, F-103")
    expect_length(R$bitacora, 0L)
    expect_identical(R$cierres, 0L)
  })
})

test_that("E4 mixed: the good one is staged, the other named; the modal closes", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, c("F-101" = 500, "F-103" = 250.01))
    expect_identical(nuevas()$Documento, "F-101")
    expect_identical(textos("warning"), "No se envió 1 abono por exceder el saldo disponible: F-103")
    expect_identical(R$cierres, 1L)
  })
})

test_that("E5 AR: ledger AR, the customer's code", {
  reiniciar()
  correr({
    filtros(session, ledger = "AR")
    enviar(session, c("F-901" = 1200))
    n <- nuevas()
    expect_identical(n$ledger, "AR"); expect_identical(n$Codigo, "C001")
  })
})

test_that("E6 without can_register_payments nothing happens", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, c("F-101" = 400))
    expect_length(R$guardados, 0L); expect_length(R$avisos, 0L); expect_identical(R$cierres, 0L)
  }, autorizado = FALSE)
})

test_that("[CAMBIA] E7 the Agenda read fails: nothing is saved, an error says so, the session and the modal stay", {
  # Before: the session's copy became the base of the save (a stale base
  # could overwrite what another session staged) -- the pattern T23 removed
  # from Vencidos / the lupa.
  reiniciar(falla_lectura = TRUE)
  sesion <- D$pagar_hoy; sesion$id <- "ph_sesion"
  correr({
    filtros(session)
    enviar(session, c("F-101" = 400))
    expect_length(R$guardados, 0L)
    expect_identical(textos("error"), "No se pudo leer la Agenda; no se envió nada. Intenta de nuevo.")
    expect_length(textos("message"), 0L)
    expect_length(R$bitacora, 0L)
    expect_identical(R$cierres, 0L)
    expect_identical(session$userData$ph()$id, "ph_sesion")
  }, agenda_sesion = sesion)
})

test_that("[CAMBIA C2] E8 the balance checked is the server's at the click, not what the browser showed", {
  # Before: the browser's data-saldo -- a payload saying 9,999 got 300
  # through against a balance of 250.
  reiniciar()
  correr({
    filtros(session)
    d <- datos(session)
    # Another session confirms 200 on F-101 after the table was painted:
    # its balance is 800 now. The 1,000 the browser showed, typed, is
    # refused; untouched, the row pays the server's 800 (before: the
    # browser's 1,000 went through).
    ab <- session$userData$abonos()
    ab <- rbind(ab, transform(ab[1, ], id = "ab9", Documento = "F-101", importe = 200, status = "active"))
    session$userData$abonos(ab)
    session$setInputs(`ab_tabla-sel` = d$id[d$Documento == "F-101"],
                      `ab_tabla-ediciones` = list(ids = list(d$id[d$Documento == "F-101"]), valores = list(1000)))
    session$setInputs(ab_stage = 1)
    expect_length(R$guardados, 0L)
    expect_identical(textos("warning"), "No se envió 1 abono por exceder el saldo disponible: F-101")
    session$setInputs(`ab_tabla-ediciones` = list(ids = list(), valores = list()))
    session$setInputs(ab_stage = 2)
    expect_identical(nuevas()$Importe, 800)
    reiniciar()
    enviar(session, c("F-103" = 300))                         # over 250, whatever the browser thought
    expect_length(R$guardados, 0L)
  })
})

test_that("[CAMBIA regla 17] E9 the save fails: only the error -- no 'enviados', no log, the modal stays, the session's Agenda unchanged", {
  # Before (master): a warning, and then "1 abono enviado", the log and the
  # modal closing, with F-101 only in this session's copy.
  reiniciar(falla_guardado = TRUE)
  correr({
    filtros(session)
    enviar(session, c("F-101" = 400))
    expect_identical(textos("error"), "No se pudo guardar en Agenda: S3 write failed")
    expect_length(textos("message"), 0L)
    expect_length(R$bitacora, 0L)
    expect_identical(R$cierres, 0L)
    expect_false("F-101" %in% session$userData$ph()$Documento)
  })
})

test_that("[CAMBIA C5] E10 Enviar with nothing ticked: a warning from the dictionary, nothing sent", {
  # Before: the browser's alert('Selecciona al menos una factura.').
  reiniciar()
  correr({
    filtros(session)
    session$setInputs(ab_stage = 1)
    expect_identical(textos("warning"), "Selecciona al menos una factura.")
    expect_length(R$guardados, 0L); expect_identical(R$cierres, 0L)
  })
})

test_that("[CAMBIA C1] E11 a typed amount ticks its row (the engine's); an amount on an unticked row is not sent", {
  # Before: the amount was disabled until its box was ticked.
  reiniciar()
  correr({
    filtros(session)
    d <- datos(session)
    session$setInputs(`ab_tabla-sel` = d$id[d$Documento == "F-201"],
                      `ab_tabla-ediciones` = list(ids = list(d$id[d$Documento == "F-101"]), valores = list(10)))
    session$setInputs(ab_stage = 1)
    expect_identical(nuevas()$Documento, "F-201")
  })
})

test_that("A0 startup: SAP and abonos still NULL, the modal never opened -- no table failure", {
  reiniciar()
  avisos <- character(0)
  withCallingHandlers(
    testServer(function(input, output, session) {
      session$userData$ab <- setup_abono_browse(input, output, session,
        sap_data = reactive(NULL), abonos_db = reactiveVal(NULL), pagar_hoy_db = reactiveVal(NULL),
        current_user = function() "tester", client_id = function() "cliente_a", autorizado = function() TRUE)
    }, {
      session$flushReact()
      expect_identical(ab_vacio(session$userData$ab$facturas()), "Sin datos SAP para este tipo (CxP o CxC).")
    }),
    message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_false(any(grepl("the (datos|aviso) failed", avisos)))
})
