# =============================================================================
# tests/test_abono_caracterizacion_r.R
# Abono parcial (R/staging_browse_module.R) as it works on master, recorded
# before it moves to the table engine (T25 step 0b). The real
# setup_abono_browse() runs under shiny::testServer over
# tests/helpers/abono_datos_prueba.R; the Agenda read and save, log_action and
# the notifications are stubs in this process (never S3). The table is read
# through tests/helpers/abono_adaptador.R, so after the migration the same
# cases run on the engine's rows. [CAMBIA] marks what reportes/T25_diseno.md
# changes on purpose (approved: C2, C5 and rule 17); each one flips its
# expectation in the migration commit.
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
source("R/staging_browse_module.R")
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
    session$userData$ph <- ph
    setup_abono_browse(input, output, session,
      sap_data = reactive(sap), abonos_db = reactiveVal(D$abonos), pagar_hoy_db = ph,
      current_user = function() "tester", client_id = function() "cliente_a",
      autorizado = function() autorizado)
  }
  eval(bquote(testServer(srv, .(substitute(codigo)))))
}
filtros <- function(session, ledger = "AP", desde = HOY - 15L, hasta = HOY + 15L, empresa = "", moneda = "")
  session$setInputs(ab_ledger = ledger, ab_desde = desde, ab_hasta = hasta, ab_empresa = empresa, ab_moneda = moneda)
rejilla <- function(output) ab_rejilla_viejo(output$ab_table)
enviar <- function(session, output, importes, saldo = NULL) {
  p <- ab_payload_viejo(rejilla(output), importes)
  if (!is.null(saldo)) for (k in seq_along(p)) if (p[[k]]$documento %in% names(saldo)) p[[k]]$saldo <- saldo[[p[[k]]$documento]]
  session$setInputs(ab_rows = p)
}
textos <- function(tipo) vapply(Filter(function(a) identical(a$tipo, tipo), R$avisos), `[[`, "", "texto")

# ── The table ────────────────────────────────────────────────────────────────
test_that("A1 AP, default window: invoices with a balance, net of confirmed abonos only", {
  reiniciar()
  correr({
    filtros(session)
    g <- rejilla(output)
    e <- D$esperado
    expect_setequal(g$Documento, e$Documento)
    expect_identical(g$Saldo[match(e$Documento, g$Documento)], e$Saldo)
    # F-103: 400 - 150 confirmed; F-101's voided abono and AR's abono don't
    # count; F-102's abono pending in the Agenda doesn't either; F-301 is
    # fully paid and gone; F-401 / F-402 fall outside the window.
    expect_false(any(c("F-301", "F-401", "F-402") %in% g$Documento))
  })
})

test_that("A2 painted order: by due date, a group's invoices together where the group first appears", {
  reiniciar()
  correr({
    filtros(session)
    g <- rejilla(output)
    expect_identical(g$Documento, c("F-101", "F-102", "F-103", "F-201", "F-501", "F-403"))
  })
})

test_that("A3 groups: Parte + Empresa + Moneda with 2 or more; the same Parte in another company stands alone", {
  reiniciar()
  correr({
    filtros(session)
    g <- rejilla(output)
    e <- D$esperado
    expect_identical(g$Grupo[match(e$Documento, g$Documento)], e$Grupo)
    gr <- ab_grupos_viejo(output$ab_table)
    expect_identical(nrow(gr), 1L)
    expect_identical(gr$Grupo, "Proveedor Uno|NCS|MXN")
    expect_identical(gr$n, 3L)
  })
})

test_that("[CAMBIA C3] A4 formats: balance and group total without cents, due date dd/mm/yy", {
  reiniciar()
  correr({
    filtros(session)
    g <- rejilla(output)
    expect_identical(g$SaldoTxt[g$Documento == "F-102"], "MXN 2,500")      # 2,500.50 shows no cents
    expect_identical(g$SaldoTxt[g$Documento == "F-201"], "USD 800")
    expect_identical(ab_grupos_viejo(output$ab_table)$TotalTxt, "MXN 3,750")   # 1,000 + 2,500.50 + 250
    expect_identical(g$Vence[g$Documento == "F-101"], format(HOY - 3L, "%d/%m/%y"))
  })
})

test_that("[CAMBIA C4] A5 reference: Factura when there is one, else Documento; 'Doc X' under it only when they differ", {
  reiniciar()
  correr({
    filtros(session)
    g <- rejilla(output)
    expect_identical(g$Referencia[match(c("F-101", "F-102"), g$Documento)], c("F-101", "A-77"))
    expect_identical(g$DocAparte[match(c("F-101", "F-102"), g$Documento)], c(FALSE, TRUE))
  })
})

test_that("A6 server filters: company, currency, AR and the date window (both ends included)", {
  reiniciar()
  correr({
    filtros(session, empresa = "NTS");   expect_setequal(rejilla(output)$Documento, c("F-501", "F-403"))
    filtros(session, moneda = "USD");    expect_identical(rejilla(output)$Documento, "F-201")
    filtros(session, desde = HOY, hasta = HOY + 2L)
    expect_identical(rejilla(output)$Documento, c("F-201", "F-102"))
    filtros(session, hasta = HOY + 15L, desde = HOY + 15L, empresa = "NTS")
    expect_identical(rejilla(output)$Documento, "F-403")
    filtros(session, ledger = "AR")
    g <- rejilla(output)
    expect_identical(g$Documento, "F-901"); expect_identical(g$Codigo, "C001")
  })
})

test_that("A7 empty states: no SAP data, nothing in range, nothing left to pay", {
  reiniciar()
  correr({
    filtros(session, ledger = "AR")
    expect_identical(ab_vacio_viejo(output$ab_table), "Sin datos SAP para este tipo de ledger.")
  }, sap = list(AP = D$sap$AP, AR = NULL))
  correr({
    filtros(session, desde = HOY + 30L, hasta = HOY + 40L)
    expect_identical(ab_vacio_viejo(output$ab_table), "Sin facturas en ese rango.")
    filtros(session, desde = HOY, hasta = HOY, empresa = "NTS")     # only F-301, fully paid
    expect_identical(ab_vacio_viejo(output$ab_table), "Sin facturas con saldo pendiente en ese rango.")
  })
})

# ── Enviar a Agenda ──────────────────────────────────────────────────────────
test_that("E1 two abonos: two Agenda rows, field by field, over the fresh read; message, log, modal closes", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, output, c("F-101" = 400, "F-201" = 800))
    expect_length(R$guardados, 1L)
    gd <- R$guardados[[1]]
    expect_identical(gd$base, D$pagar_hoy)                       # the fresh read is the base (A2)
    expect_identical(gd$cliente, "cliente_a"); expect_identical(gd$usuario, "tester")
    nuevas <- gd$df[gd$df$id != "ph1", ]
    expect_identical(nrow(gd$df), 3L)                            # ph1 kept
    expect_identical(nuevas$Documento, c("F-101", "F-201"))
    expect_identical(nuevas$ledger, c("AP", "AP"))
    expect_identical(nuevas$Empresa, c("NCS", "NCS"))
    expect_identical(nuevas$Moneda, c("MXN", "USD"))
    expect_identical(nuevas$Parte, c("Proveedor Uno", "Proveedor Dos"))
    expect_identical(nuevas$Codigo, c("P001", "P002"))           # trimmed
    expect_identical(nuevas$tipo_item, c("abono", "abono"))
    expect_identical(nuevas$Importe, c(400, 800))
    expect_identical(nuevas$FechaVenc, HOY + c(-3L, 1L))         # the invoice's due date
    expect_identical(nuevas$staged_by, c("tester", "tester"))
    expect_identical(nuevas$status, c("pending", "pending"))
    expect_identical(nuevas$source, c("manual", "manual"))
    expect_true(all(nchar(nuevas$id) == 36L) && !anyDuplicated(nuevas$id))
    expect_identical(session$userData$ph()$Documento, gd$df$Documento)   # the session copy too
    expect_identical(textos("message"), "2 abonos enviados a Agenda de hoy. Confirma para aplicar al saldo.")
    expect_length(R$bitacora, 1L)
    expect_identical(R$bitacora[[1]]$action, "enviar_abonos_a_agenda")
    expect_identical(R$bitacora[[1]]$metadata, list(n = 2L, ledger = "AP"))
    expect_identical(R$cierres, 1L)
  })
})

test_that("E2 one abono: singular message; an amount equal to the balance is accepted", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, output, c("F-102" = 2500.5))
    expect_identical(R$guardados[[1]]$df$Importe[R$guardados[[1]]$df$id != "ph1"], 2500.5)
    expect_identical(textos("message"), "1 abono enviado a Agenda de hoy. Confirma para aplicar al saldo.")
  })
})

test_that("E3 over the balance or <= 0: nothing staged, the warning names them, the modal stays", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, output, c("F-103" = 300, "F-101" = 0))
    expect_length(R$guardados, 0L)
    expect_identical(textos("warning"), "No se enviaron 2 abonos por exceder el saldo disponible: F-103, F-101")
    expect_length(R$bitacora, 0L)
    expect_identical(R$cierres, 0L)
  })
})

test_that("E4 mixed: the good one is staged, the other named; the modal closes", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, output, c("F-101" = 500, "F-103" = 250.01))
    nuevas <- R$guardados[[1]]$df[R$guardados[[1]]$df$id != "ph1", ]
    expect_identical(nuevas$Documento, "F-101")
    expect_identical(textos("warning"), "No se envió 1 abono por exceder el saldo disponible: F-103")
    expect_identical(R$cierres, 1L)
  })
})

test_that("E5 AR: ledger AR, the customer's code", {
  reiniciar()
  correr({
    filtros(session, ledger = "AR")
    enviar(session, output, c("F-901" = 1200))
    nuevas <- R$guardados[[1]]$df[R$guardados[[1]]$df$id != "ph1", ]
    expect_identical(nuevas$ledger, "AR"); expect_identical(nuevas$Codigo, "C001")
  })
})

test_that("E6 without can_register_payments nothing happens", {
  reiniciar()
  correr({
    filtros(session)
    enviar(session, output, c("F-101" = 400))
    expect_length(R$guardados, 0L); expect_length(R$avisos, 0L); expect_identical(R$cierres, 0L)
  }, autorizado = FALSE)
})

test_that("E7 the Agenda read fails: the session's copy is the base", {
  reiniciar(falla_lectura = TRUE)
  sesion <- D$pagar_hoy; sesion$id <- "ph_sesion"
  correr({
    filtros(session)
    enviar(session, output, c("F-101" = 400))
    expect_identical(R$guardados[[1]]$base$id, "ph_sesion")
    expect_true("ph_sesion" %in% R$guardados[[1]]$df$id)
  }, agenda_sesion = sesion)
})

test_that("[CAMBIA C2] E8 the balance checked is the one the browser sent, not the server's", {
  reiniciar()
  correr({
    filtros(session)
    # F-103's balance is 250; a payload that says 9,999 gets 300 through today.
    enviar(session, output, c("F-103" = 300), saldo = c("F-103" = 9999))
    expect_identical(R$guardados[[1]]$df$Importe[R$guardados[[1]]$df$id != "ph1"], 300)
  })
})

test_that("[CAMBIA regla 17] E9 the save fails: a warning, and still 'enviados', the log and the modal closing", {
  reiniciar(falla_guardado = TRUE)
  correr({
    filtros(session)
    enviar(session, output, c("F-101" = 400))
    expect_identical(textos("warning"), "No se pudo guardar en Agenda: S3 write failed")
    expect_identical(textos("message"), "1 abono enviado a Agenda de hoy. Confirma para aplicar al saldo.")
    expect_length(R$bitacora, 1L)
    expect_identical(R$cierres, 1L)
    expect_true("F-101" %in% session$userData$ph()$Documento)    # only in this session
  })
})
