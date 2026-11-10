# Requisiciones, stage E3a: "Crear provisión" on a requisition (handed to the
# provision form the app wires in) and the read-only detail opened from a
# provision (Calendario, Pasivos, confirmation history). Over
# tests/helpers/requisiciones_fixture_app.R (in-memory S3 local to it).
# Run from the repo root: Rscript tests/test_requisiciones_e3.R
library(testthat)
suppressPackageStartupMessages(library(shiny))

cargar_app <- function(nivel = "compras", usuario = "dani", abrir = TRUE) {
  Sys.setenv(REQ_NIVEL = nivel, REQ_USUARIO = usuario, HD_ARRANQUE_NULL = "", REQ_FALLA_LECTURA = "",
             REQ_OVERRIDES = "", REQ_ABRIR_PROVISION = if (abrir) "1" else "")
  suppressMessages(source("tests/helpers/requisiciones_fixture_app.R")$value)
}
fila   <- function(titulo) { r <- .req_mem[["fixture/requisiciones.rds"]]; r[r$titulo == titulo, ] }
accion <- function(session, tabla, accion, ids, n = stats::runif(1))
  do.call(session$setInputs, stats::setNames(list(list(accion = accion, ids = as.list(ids), nonce = n)),
                                             paste0("req-", tabla, "-accion")))

invisible(cargar_app(abrir = FALSE))   # loads the functions for the pure test below

test_that("el prellenado sale solo de la requisición", {
  p <- requisicion_prefill_provision(list(id = "r1", folio = "REQ-2026-0004", titulo = "Extintores",
                                          empresa = "NTS", proveedor = "Seguridad Total", monto = 12000,
                                          moneda = "MXN", notas = NA))
  expect_identical(p$requisicion_id, "r1")
  expect_identical(p$empresa, "NTS"); expect_identical(p$parte, "Seguridad Total")
  expect_identical(p$monto, 12000); expect_identical(p$moneda, "MXN")
  expect_identical(p$referencia, "REQ-2026-0004 · Extintores")
  expect_true(is.na(p$notas))
  vacio <- requisicion_prefill_provision(list(id = "r2", folio = "REQ-2026-0005", titulo = "x", empresa = "NCS",
                                              proveedor = "  ", monto = NA, moneda = NA))
  expect_true(is.na(vacio$parte)); expect_true(is.na(vacio$monto))
})

test_that("sin formulario conectado, 'Crear provisión' no existe ni por una acción forjada", {
  app <- cargar_app(abrir = FALSE)
  testServer(app, {
    session$flushReact()
    expect_true(all(mod$datos()$.acciones == "detalle"))
    antes <- .req_mem[["fixture/requisiciones.rds"]]
    accion(session, "t", "crear_provision", fila("Extintores")$id)
    expect_length(.req_llamadas, 0L)
    expect_identical(.req_mem[["fixture/requisiciones.rds"]], antes)
  })
})

test_that("con formulario: solo en filas Sin provisión; abre prellenado y al guardar liga", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    d <- mod$datos()
    expect_identical(d$.acciones[d$titulo == "Extintores"], "detalle,crear_provision")
    expect_identical(d$.acciones[d$titulo == "Luz bodega 3"], "detalle")       # provisionada
    expect_identical(d$.acciones[d$titulo == "Renta noviembre"], "detalle")    # en pago
    id <- fila("Extintores")$id
    accion(session, "t", "crear_provision", id)
    expect_length(.req_llamadas, 1L)
    p <- .req_llamadas[[1]]$prefill
    expect_identical(p$requisicion_id, id)
    expect_identical(p$parte, "Seguridad Total"); expect_identical(p$monto, 12000)
    expect_match(p$referencia, "^REQ-\\d{4}-\\d{4} · Extintores$")
    expect_true(.req_llamadas[[1]]$al_guardar("p-libre1"))
    expect_identical(fila("Extintores")$provision_id, "p-libre1")
    h <- .req_mem[["fixture/requisiciones_historial.rds"]]
    expect_true(any(h$requisicion_id == id & h$accion == "ligada"))
    session$flushReact()
    expect_identical(mod$datos()$etapa[mod$datos()$id == id], "provisionada")
    # the same form saved twice: the second provision is refused, the first link stays
    expect_false(.req_llamadas[[1]]$al_guardar("p-libre2"))
    expect_identical(fila("Extintores")$provision_id, "p-libre1")
  })
})

test_that("una acción forjada sobre una fila con provisión no abre el formulario", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    accion(session, "t", "crear_provision", fila("Luz bodega 3")$id)
    accion(session, "t", "crear_provision", "no-existe")
    expect_length(.req_llamadas, 0L)
  })
})

test_that("sin los permisos no se abre el formulario (Análisis, y Finanzas sin Requisiciones)", {
  for (nv in c("analysis", "finance")) {
    app <- cargar_app(nivel = nv, usuario = "ana")
    testServer(app, {
      session$flushReact()
      antes <- .req_mem[["fixture/requisiciones.rds"]]
      accion(session, "t", "crear_provision", fila("Extintores")$id)
      expect_length(.req_llamadas, 0L)
      expect_identical(.req_mem[["fixture/requisiciones.rds"]], antes)
    })
  }
})

test_that("si la sesión cambió de cliente antes de guardar, no se liga en ningún lado", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    accion(session, "t", "crear_provision", fila("Extintores")$id)
    session$setInputs(cambiar_cliente = "otro")
    expect_false(.req_llamadas[[1]]$al_guardar("p-libre1"))
    expect_true(is.na(fila("Extintores")$provision_id))
    expect_null(.req_mem[["otro/requisiciones.rds"]])
  })
})

test_that("Ver evidencia desde una provisión: pagada incluida, solo lectura", {
  app <- cargar_app(nivel = "compras")
  testServer(app, {
    session$flushReact()
    expect_true(mod$abrir_por_provision("p-pagada"))
    expect_identical(mod$detalle_id(), fila("Papelería octubre")$id)
    expect_true(mod$lectura())
    expect_false(mod$abrir_por_provision("p-libre2"))         # no requisition behind it
    expect_true(mod$abrir_por_provision("p-luz"))              # provisionada, from outside: still read-only
    expect_true(mod$lectura())
  })
})

test_that("Ver evidencia: solo quien ve Requisiciones (Luis, 2026-11-10); Finanzas y Análisis no", {
  for (nv in c("finance", "analysis")) {
    app <- cargar_app(nivel = nv, usuario = "ana")
    testServer(app, {
      session$flushReact()
      expect_false(mod$abrir_por_provision("p-pagada"))
      expect_null(mod$detalle_id())
    })
  }
  expect_false(requisiciones_puede_ver_evidencia(list(tier = "finance", permisos = permisos_resueltos("finance"))))
  expect_true(requisiciones_puede_ver_evidencia(list(tier = "compras", permisos = permisos_resueltos("compras"))))
})
