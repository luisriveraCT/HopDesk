# Requisiciones, stage E2 (the screen): R/requisiciones_module.R under
# shiny::testServer over tests/helpers/requisiciones_fixture_app.R (an
# in-memory S3 local to the fixture; aws.s3 is never patched).
# The browser checks (no horizontal scroll at 1293 x 632, nothing that
# converts to payment) are in tests/test_requisiciones_chromote.R.
# Run from the repo root: Rscript tests/test_requisiciones_pantalla.R
library(testthat)
suppressPackageStartupMessages(library(shiny))

cargar_app <- function(nivel = "compras", usuario = "dani", null = FALSE, falla = FALSE, overrides = "") {
  Sys.setenv(REQ_NIVEL = nivel, REQ_USUARIO = usuario, HD_ARRANQUE_NULL = if (null) "1" else "",
             REQ_FALLA_LECTURA = if (falla) "1" else "", REQ_OVERRIDES = overrides)
  suppressMessages(source("tests/helpers/requisiciones_fixture_app.R")$value)
}
ids_de <- function(titulos) { r <- .req_mem[["fixture/requisiciones.rds"]]; r$id[match(titulos, r$titulo)] }
fila   <- function(titulo) { r <- .req_mem[["fixture/requisiciones.rds"]]; r[r$titulo == titulo, ] }
accion <- function(session, tabla, accion, ids, n = stats::runif(1))
  do.call(session$setInputs, stats::setNames(list(list(accion = accion, ids = as.list(ids), nonce = n)),
                                             paste0("req-", tabla, "-accion")))

test_that("fuente NULL al arranque: la sesión sigue y ninguna tabla falla", {
  app <- cargar_app(null = TRUE)
  avisos <- character()
  withCallingHandlers(testServer(app, {
    session$flushReact(); session$flushReact()
    expect_null(mod$datos())
    expect_null(mod$vista())
  }), message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_false(any(grepl("the (datos|aviso) failed", avisos)))
})

test_that("la lista: sin pagadas nunca, papelera solo en su vista, Sin provisión fijas arriba", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    d <- mod$datos()
    expect_setequal(d$titulo, c("Cemento y varilla para la cimentación de la nave 2", "Luz bodega 3",
                                "Renta noviembre", "Extintores"))
    expect_false("Papelería octubre" %in% d$titulo)                     # paid: in no view
    expect_identical(d$.fijar, d$etapa == "sin_provision")
    expect_identical(d$etapa[d$titulo == "Renta noviembre"], "en_pago")
    expect_identical(d$prioridad_txt[d$titulo == "Extintores"], "4")
    expect_identical(d$.adjuntos[grepl("^Cemento", d$titulo)], 2L)       # 1 image + 1 table
    expect_true(all(d$.acciones == "detalle"))                          # no convert-to-payment, ever
    expect_false(".provision" %in% names(d))
    session$setInputs(`req-papelera_toggle` = 1)
    expect_identical(mod$datos()$titulo, "Sillas de oficina (cotización vieja)")
    session$setInputs(`req-papelera_toggle` = 2)
    session$setInputs(`req-solo_mias` = TRUE)
    expect_setequal(mod$datos()$creado_por, "dani")
    session$setInputs(`req-solo_mias` = FALSE)
    carp <- .req_mem[["fixture/requisiciones_carpetas.rds"]]
    session$setInputs(`req-carpeta_click` = carp$id[carp$nombre == "Mantenimiento"])
    expect_setequal(mod$datos()$titulo, c("Luz bodega 3", "Extintores"))
    session$setInputs(`req-carpeta_click` = "sin")
    expect_identical(mod$datos()$titulo, "Renta noviembre")
  })
})

test_that("selección por id con dos sesiones: la otra agrega y paga, la acción va a los ids marcados", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    marcados <- ids_de(c("Extintores", "Cemento y varilla para la cimentación de la nave 2"))
    session$setInputs(`req-t-sel` = as.list(marcados))
    # another session creates a row (positions shift) and pays Luz bodega 3
    session$setInputs(otra_sesion = 1, otra_sesion_paga = 1)
    session$elapse(5000); session$flushReact()
    d <- mod$datos()
    expect_true("Creada en otra sesión" %in% d$titulo)
    expect_false("Luz bodega 3" %in% d$titulo)                 # paid elsewhere: out of the list
    carp <- .req_mem[["fixture/requisiciones_carpetas.rds"]]
    destino <- carp$id[carp$nombre == "Obra nave 2"]
    accion(session, "t", "mover", marcados)
    session$setInputs(`req-mover_a` = destino, `req-mover_ok` = 1)
    r <- .req_mem[["fixture/requisiciones.rds"]]
    expect_true(all(r$carpeta_id[r$id %in% marcados] == destino))
    expect_true(is.na(r$carpeta_id[r$titulo == "Creada en otra sesión"]))
    expect_false(identical(r$carpeta_id[r$titulo == "Luz bodega 3"], destino))
  })
})

test_that("sin permiso no se escribe nada aunque la acción llegue (Análisis)", {
  app <- cargar_app(nivel = "analysis", usuario = "ana")
  testServer(app, {
    session$flushReact()
    expect_null(mod$datos())                                   # cannot even see the list
    antes <- .req_mem[["fixture/requisiciones.rds"]]
    accion(session, "t", "papelera", ids_de("Extintores"))
    accion(session, "t", "mover", ids_de("Extintores"))
    session$setInputs(`req-mover_a` = "", `req-mover_ok` = 1)
    session$setInputs(`req-nueva` = 1, `req-f_titulo` = "x", `req-f_empresa` = "NCS", `req-crear` = 1)
    session$setInputs(`req-carp_nombre` = "Nueva", `req-carp_guardar` = 1)
    expect_identical(.req_mem[["fixture/requisiciones.rds"]], antes)
    expect_equal(nrow(.req_mem[["fixture/requisiciones_carpetas.rds"]]), 2)
  })
})

test_that("papelera: propias sí, de otros solo con su permiso; restaurar", {
  app <- cargar_app(nivel = "compras", usuario = "dani",
                    overrides = '{"can_trash_others_requisiciones": false}')
  testServer(app, {
    session$flushReact()
    ids <- ids_de(c("Extintores", "Luz bodega 3"))          # dani's and eli's
    accion(session, "t", "papelera", ids)
    expect_false(is.na(fila("Extintores")$papelera_en))
    expect_true(is.na(fila("Luz bodega 3")$papelera_en))     # someone else's: refused
    session$setInputs(`req-papelera_toggle` = 1)
    accion(session, "t", "restaurar", ids_de("Extintores"))
    expect_true(is.na(fila("Extintores")$papelera_en))
  })
})

test_that("nueva requisición con captura y celdas pegadas; el recorte se avisa", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    session$setInputs(`req-nueva` = 1)
    png <- magick::image_write(magick::image_blank(60, 40, "orange"), format = "png")
    f <- tempfile(fileext = ".png"); writeBin(png, f)
    session$setInputs(`req-archivo_origen` = "pegado")
    session$setInputs(`req-archivo` = data.frame(name = "captura.png", size = length(png), type = "image/png",
                                                 datapath = f, stringsAsFactors = FALSE))
    filas <- paste(c("Concepto\tImporte", sprintf("Partida %d\t%d", 1:340, 1:340)), collapse = "\n")
    session$setInputs(`req-pegado_texto` = list(texto = filas, t = 1))
    expect_match(mod$pegado()$aviso, "Se pegaron 341 filas; se guardaron 200")
    session$setInputs(`req-pegado_titulo` = "Partidas", `req-pegado_agregar` = 1)
    session$setInputs(`req-f_titulo` = "Bombas de agua", `req-f_empresa` = "NCS", `req-f_proveedor` = "",
                      `req-f_monto` = 32000, `req-f_moneda` = "MXN", `req-f_prioridad` = "10",
                      `req-f_carpeta` = "", `req-f_notas` = "Se quemó la bomba principal.")
    session$setInputs(`req-crear` = 1)
    r <- fila("Bombas de agua")
    expect_equal(nrow(r), 1); expect_identical(r$prioridad, 10L)
    ev <- .req_mem[["fixture/requisiciones_evidencia.rds"]]
    ev <- ev[ev$requisicion_id == r$id, ]
    expect_setequal(ev$tipo, c("adjunto", "tabla"))
    expect_identical(ev$filas_pegadas[ev$tipo == "tabla"], 341L)
    expect_identical(ev$filas[ev$tipo == "tabla"], 200L)
  })
})

test_that("una lectura fallida se dice y nunca se muestra como lista vacía", {
  app <- cargar_app(falla = TRUE)
  testServer(app, {
    session$flushReact()
    expect_error(mod$vista(), "could not read")
    expect_match(output$`req-aviso_lectura`$html, "No se pudieron leer")
  })
})

test_that("comentar desde el detalle avisa y cuenta como leído para el autor", {
  app <- cargar_app(usuario = "dani")
  testServer(app, {
    session$flushReact()
    id <- ids_de("Extintores")
    accion(session, "t", "detalle", id)
    expect_identical(mod$detalle_id(), id)
    session$setInputs(`req-comentario` = "¿Lo pagamos esta semana?", `req-comentar` = 1)
    cm <- .req_mem[["fixture/requisiciones_comentarios.rds"]]
    expect_true(any(cm$requisicion_id == id & cm$autor == "dani"))
    d <- mod$datos()
    expect_identical(d$comentarios_sin_leer[d$id == id], 0L)
  })
})
