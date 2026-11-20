# T61, the screen: the folder tree in the Requisiciones panel (General, the
# virtual folder, first), the folder modal ("Dentro de" and its companies),
# folders in the trash, and the requisition's company chosen among its
# folder's. R/requisiciones_module.R under shiny::testServer over
# tests/helpers/requisiciones_fixture_app.R (in-memory S3; aws.s3 is never
# patched). The look at 1293 x 632 is checked in Chrome.
# Run from the repo root: Rscript tests/test_requisiciones_carpetas_pantalla.R
library(testthat)
suppressPackageStartupMessages(library(shiny))

cargar_app <- function(nivel = "compras", usuario = "dani") {
  Sys.setenv(REQ_NIVEL = nivel, REQ_USUARIO = usuario, HD_ARRANQUE_NULL = "", REQ_FALLA_LECTURA = "",
             REQ_OVERRIDES = "", REQ_ABRIR_PROVISION = "")
  suppressMessages(source("tests/helpers/requisiciones_fixture_app.R")$value)
}
invisible(cargar_app())
CARP <- function() .req_mem[["fixture/requisiciones_carpetas.rds"]]
id_carpeta <- function(nombre) { c <- CARP(); c$id[c$nombre == nombre] }
fila <- function(titulo) { r <- .req_mem[["fixture/requisiciones.rds"]]; r[r$titulo == titulo, ] }

test_that("opciones de carpeta: General primero, luego cada carpeta con su ruta; excluir quita una rama", {
  cs <- requisiciones_carpetas("fixture")
  sub <- requisiciones_carpeta_crear("fixture", list(nombre = "Bombas", padre_id = id_carpeta("Mantenimiento")), "dani")
  cs <- requisiciones_carpetas("fixture")
  op <- .reqm_opciones_carpeta(cs)
  expect_identical(unname(op[1]), ""); expect_identical(names(op)[1], "General")
  expect_true("Mantenimiento › Bombas" %in% names(op))
  op2 <- .reqm_opciones_carpeta(cs, "requisiciones.carpeta.raiz", excluir = .req_carpeta_subarbol(cs, id_carpeta("Mantenimiento")))
  expect_false(any(c(id_carpeta("Mantenimiento"), sub) %in% op2))
  expect_identical(names(op2)[1], "Ninguna (primer nivel)")
})

test_that("empresas de la carpeta para el select: General = todas; carpeta de NCS = solo NCS; la guardada de fuera se queda, señalada", {
  invisible(cargar_app())
  cmap <- c(NCS = "Networks Crossdocking Services", NTS = "Networks Trucking Services")
  e <- .req_fixture_empresas
  solo <- requisiciones_carpeta_crear("fixture", list(nombre = "Solo NCS", empresas = "emp-ncs"), "dani")
  cs <- requisiciones_carpetas("fixture")
  expect_identical(unname(.reqm_empresas_carpeta(cs, "", cmap, e)), c("NCS", "NTS"))
  expect_identical(unname(.reqm_empresas_carpeta(cs, id_carpeta("Mantenimiento"), cmap, e)), c("NCS", "NTS"))
  op <- .reqm_empresas_carpeta(cs, solo, cmap, e)
  expect_identical(unname(op), "NCS")
  op <- .reqm_empresas_carpeta(cs, solo, cmap, e, actual = "NTS")
  expect_identical(unname(op), c("NCS", "NTS")); expect_match(names(op)[2], "no es de esta carpeta")
})

test_that("panel: Todas, General y el árbol anidado; elegir una carpeta muestra su rama", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    mant <- id_carpeta("Mantenimiento")
    sub <- requisiciones_carpeta_crear("fixture", list(nombre = "Bombas", padre_id = mant), "dani")
    requisicion_crear("fixture", list(titulo = "Bomba nueva", empresa = "NCS", carpeta_id = sub), "dani")
    .reqm_tocar("fixture"); session$elapse(5000); session$flushReact()
    h <- output$`req-carpetas`$html
    expect_match(h, "General"); expect_match(h, 'data-cliente="fixture"')
    expect_match(h, sprintf('data-carpeta="%s"[^>]*>.*hdreq-sub.*data-carpeta="%s"', mant, sub))
    expect_match(h, "hdreq-plegar")
    session$setInputs(`req-carpeta_click` = mant)
    expect_setequal(mod$datos()$titulo, c("Luz bodega 3", "Extintores", "Bomba nueva"))   # the whole branch
    session$setInputs(`req-carpeta_click` = sub)
    expect_identical(mod$datos()$titulo, "Bomba nueva")
    session$setInputs(`req-carpeta_click` = "general")
    expect_setequal(mod$datos()$titulo, c("Renta noviembre", "Papelería octubre"))   # CM35: the paid one stays too
  })
})

test_that("modal de carpeta: crea dentro de otra; todas marcadas se guarda como 'todas' (NA); una lista se guarda por id", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    mant <- id_carpeta("Mantenimiento")
    session$setInputs(`req-carpeta_click` = mant, `req-carpeta_nueva` = 1)
    session$setInputs(`req-carp_nombre` = "Eléctrico", `req-carp_padre` = mant,
                      `req-carp_empresas` = c("emp-ncs", "emp-nts"), `req-carp_guardar` = 1)
    c <- CARP(); n <- c[c$nombre == "Eléctrico", ]
    expect_identical(n$padre_id, mant); expect_true(is.na(n$empresas))
    session$setInputs(`req-carpeta_nueva` = 2)
    session$setInputs(`req-carp_nombre` = "Solo NTS", `req-carp_padre` = "",
                      `req-carp_empresas` = "emp-nts", `req-carp_guardar` = 2)
    c <- CARP(); n <- c[c$nombre == "Solo NTS", ]
    expect_true(is.na(n$padre_id)); expect_identical(n$empresas, "[\"emp-nts\"]")
    # Moving it under another folder works, its subfolder goes along...
    session$setInputs(`req-carpeta_editar` = mant)
    session$setInputs(`req-carp_nombre` = "Mantenimiento", `req-carp_padre` = n$id,
                      `req-carp_empresas` = c("emp-ncs", "emp-nts"), `req-carp_guardar` = 3)
    expect_identical(CARP()$padre_id[CARP()$id == mant], n$id)
    expect_identical(requisiciones_carpetas("fixture")$ruta[requisiciones_carpetas("fixture")$nombre == "Eléctrico"],
                     "Solo NTS › Mantenimiento › Eléctrico")
    # ...and moving it inside its own branch is refused by the server.
    e <- id_carpeta("Eléctrico")
    session$setInputs(`req-carpeta_editar` = mant)
    session$setInputs(`req-carp_padre` = e, `req-carp_guardar` = 4)
    expect_false(identical(CARP()$padre_id[CARP()$id == mant], e))
  })
})

test_that("papelera de carpetas: se listan con 'Ver papelera' y se restauran con lo suyo", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    v <- requisiciones_carpeta_crear("fixture", list(nombre = "Vacía"), "dani")
    h1 <- requisiciones_carpeta_crear("fixture", list(nombre = "Hija", padre_id = v), "dani")
    requisiciones_carpeta_papelera("fixture", v, "dani")
    .reqm_tocar("fixture"); session$elapse(5000); session$flushReact()
    expect_false(grepl(sprintf('data-carpeta="%s"', h1), output$`req-carpetas`$html))
    session$setInputs(`req-papelera_toggle` = 1)
    expect_match(output$`req-carpetas`$html, "Carpetas en la papelera")
    session$setInputs(`req-carpeta_restaurar` = v)
    expect_true(is.na(CARP()$papelera_en[CARP()$id == v]))
    session$setInputs(`req-papelera_toggle` = 2)
    expect_match(output$`req-carpetas`$html, sprintf('data-carpeta="%s"', h1))
  })
})

test_that("nueva requisición: la empresa debe ser de la carpeta; si no, no se guarda", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    solo <- requisiciones_carpeta_crear("fixture", list(nombre = "Solo NCS", empresas = "emp-ncs"), "dani")
    session$setInputs(`req-nueva` = 1)
    session$setInputs(`req-f_titulo` = "Fuera", `req-f_carpeta` = solo, `req-f_empresa` = "NTS", `req-f_proveedor` = "",
                      `req-f_monto` = NA, `req-f_moneda` = "MXN", `req-f_prioridad` = "", `req-f_notas` = "")
    session$setInputs(`req-crear` = 1)
    expect_identical(nrow(fila("Fuera")), 0L)
    session$setInputs(`req-f_titulo` = "Dentro", `req-f_empresa` = "NCS", `req-crear` = 2)
    expect_identical(fila("Dentro")$carpeta_id, solo)
  })
})

test_that("burbujas: id de las empresas encendidas; sin catálogo o sin burbujas, sin filtro", {
  e <- .req_fixture_empresas
  expect_identical(.reqm_burbujas(e, "Networks Trucking Services")$activas, "emp-nts")
  expect_identical(.reqm_burbujas(e, character())$activas, character())
  expect_identical(.reqm_burbujas(e, character())$existentes, c("emp-ncs", "emp-nts"))
  expect_null(.reqm_burbujas(NULL, "x")$activas); expect_null(.reqm_burbujas(e, NULL)$activas)
})

test_that("burbujas: la carpeta sin empresa encendida se oculta con su rama y sus requisiciones, también en Todas; General nunca", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    nts <- requisiciones_carpeta_crear("fixture", list(nombre = "Patio NTS", empresas = "emp-nts"), "dani")
    hija <- requisiciones_carpeta_crear("fixture", list(nombre = "Grúas", padre_id = nts), "dani")   # every company
    requisicion_crear("fixture", list(titulo = "Grúa", empresa = "NTS", carpeta_id = hija), "dani")
    .reqm_tocar("fixture"); session$elapse(5000); session$flushReact()
    n_todas <- nrow(mod$datos())
    expect_true("Grúa" %in% mod$datos()$titulo)
    session$setInputs(`req-carpeta_click` = hija)
    shared$empresa_sel("Networks Crossdocking Services")          # NTS off
    session$flushReact()
    expect_identical(mod$carpeta_sel(), "todas")                  # the chosen folder hid: back to Todas
    d <- mod$datos()
    expect_false("Grúa" %in% d$titulo)
    expect_identical(nrow(d), n_todas - 1L)
    expect_true("Renta noviembre" %in% d$titulo)                  # General never hides
    h <- output$`req-carpetas`$html
    expect_false(grepl(sprintf('data-carpeta="%s"', nts), h)); expect_false(grepl(sprintf('data-carpeta="%s"', hija), h))
    expect_match(h, "1 requisición oculta por empresas apagadas")
    expect_identical(nrow(.req_mem[["fixture/requisiciones.rds"]][.req_mem[["fixture/requisiciones.rds"]]$titulo == "Grúa", ]), 1L)
    shared$empresa_sel(c("Networks Crossdocking Services", "Networks Trucking Services"))
    session$flushReact()
    expect_true("Grúa" %in% mod$datos()$titulo)
    expect_false(grepl("hdreq-ocultas", output$`req-carpetas`$html))   # not "oculta": the fold button says "ocultar"
  })
})

test_that("la columna 'Aviso de empresa' sale solo mientras alguna requisición tiene aviso (la lista no se ensancha)", {
  campos <- function(cols) vapply(cols, function(c) c$campo, character(1))
  expect_false("empresa_aviso" %in% campos(.reqm_columnas(c())))
  expect_true("empresa_aviso" %in% campos(.reqm_columnas(c(), aviso = TRUE)))
  expect_true("carpeta_ruta" %in% campos(.reqm_columnas(c())))
})

test_that("T64: la fixture deja las 4 requisiciones vivas en el mes en curso, sea el día que sea", {
  expect_identical(.req_fin_de_mes(as.Date("2026-11-19")), as.Date("2026-11-30"))
  expect_identical(.req_fin_de_mes(as.Date("2026-12-31")), as.Date("2026-12-31"))
  expect_identical(.req_fin_de_mes(as.Date("2028-02-10")), as.Date("2028-02-29"))
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    d <- mod$datos()
    vivas <- d[d$etapa != "papelera", , drop = FALSE]
    expect_identical(nrow(vivas), 4L)
    expect_true(all(format(vivas$fecha_mes, "%Y-%m") == format(Sys.Date(), "%Y-%m")))
  })
})
