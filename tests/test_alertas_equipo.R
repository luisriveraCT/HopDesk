# Alertas del equipo (front C): the users × actions matrix and
# alertas_preparar(), in R/avisos_alertas_module.R.
# S3 is the in-memory store of tests/helpers/s3_memoria.R (restored at the end).
# Run from the repo root: Rscript tests/test_alertas_equipo.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
source("tests/helpers/s3_memoria.R")
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos_alertas_module.R")

CID <- "acme"
KEY <- paste0(CID, "/avisos_alertas.rds")
auth_load_usuarios <- function(client_id = NULL) data.frame(
  username = c("Ana", "beto", "caro", "dani", "eli", "staff1"),
  display_name = c("Ana López", "", "Caro", "Dani", "Eli", "Soporte"),
  tier = c("admin", "finance", "analysis", "compras", "finance", setdiff(niveles_todos(), niveles_cliente())[1]),
  deleted = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
  activo  = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), stringsAsFactors = FALSE)

marca <- function(usuario, crear = FALSE, archivo_pago = FALSE, confirmar_pago = FALSE, bancos = FALSE)
  data.frame(usuario = usuario, crear = crear, archivo_pago = archivo_pago,
             confirmar_pago = confirmar_pago, bancos = bancos, stringsAsFactors = FALSE)

test_that("A. la matriz lista solo usuarios vivos del cliente, nunca staff", {
  u <- alertas_usuarios(CID)
  expect_setequal(u$usuario, c("ana", "beto", "caro"))   # dani inactivo, eli borrado, staff1 interno
  expect_false("staff1" %in% u$usuario)
  expect_equal(u$nombre[u$usuario == "beto"], "beto")   # sin nombre: el usuario
  expect_error(alertas_usuarios(""), "client_id")
})

test_that("B. guardar relee S3, conserva lo de otra sesión y rechaza ajenos", {
  s3_memoria_reset()
  alertas_guardar(CID, marca("ana", confirmar_pago = TRUE), por = "ana")
  # another session saves beto meanwhile, straight to S3
  m <- get(KEY, envir = .mem)
  assign(KEY, dplyr::bind_rows(m, tibble::tibble(usuario = "beto", crear = TRUE, archivo_pago = FALSE,
    confirmar_pago = FALSE, bancos = FALSE, actualizado_por = "ana",
    actualizado = as.POSIXct(Sys.time(), tz = "UTC"))), envir = .mem)
  alertas_guardar(CID, marca("caro", bancos = TRUE), por = "ana")
  m <- get(KEY, envir = .mem)
  expect_setequal(m$usuario, c("ana", "beto", "caro"))
  expect_true(m$confirmar_pago[m$usuario == "ana"])

  # all unmarked: the row goes
  alertas_guardar(CID, marca("ana"), por = "ana")
  expect_false("ana" %in% get(KEY, envir = .mem)$usuario)

  # staff, a deleted user or another client's user is never stored
  n <- .mem_escrituras
  expect_error(alertas_guardar(CID, marca("staff1", crear = TRUE), por = "ana"), "no es de este cliente")
  expect_error(alertas_guardar(CID, marca("eli", crear = TRUE), por = "ana"), "no es de este cliente")
  expect_equal(.mem_escrituras, n)
})

test_that("B2. una lectura fallida no guarda nada (nunca es matriz vacía)", {
  s3_memoria_reset()
  alertas_guardar(CID, marca("ana", crear = TRUE), por = "ana")
  antes <- get(KEY, envir = .mem); n <- .mem_escrituras
  .mem_falla <<- KEY
  expect_error(alertas_guardar(CID, marca("beto", bancos = TRUE), por = "ana"))
  expect_equal(.mem_escrituras, n)
  .mem_falla <<- character()
  expect_identical(get(KEY, envir = .mem), antes)
})

test_that("C. alertas_preparar: solo el usuario marcado, para esa acción", {
  s3_memoria_reset()
  expect_null(alertas_preparar(CID, "confirmar_pago", "ana"))          # sin matriz
  alertas_guardar(CID, marca("ana", confirmar_pago = TRUE), por = "ana")
  expect_null(alertas_preparar(CID, "confirmar_pago", "beto"))         # no marcado
  expect_null(alertas_preparar(CID, "importar_bancos", "ana"))         # otra acción
  expect_null(alertas_preparar(CID, "confirmar_pago", ""))
  expect_message(expect_null(alertas_preparar(CID, "inventada", "ana")), "desconocida")
  p <- alertas_preparar(CID, "confirmar_pago", "ANA", list(cuerpo = "2 pago(s)"))
  expect_equal(p$cuerpo, "2 pago(s)")
  expect_equal(p$permiso, "can_confirm_payments")
  expect_equal(p$clave, "act|confirmar_pago|ana")
  expect_equal(p$ventana_min, 10L)
})

test_that("C2. cada acción va a su columna y su permiso", {
  s3_memoria_reset()
  alertas_guardar(CID, marca("beto", crear = TRUE, archivo_pago = TRUE, bancos = TRUE), por = "ana")
  esperado <- c(crear_item = "can_register_payments", crear_provision = "can_edit_provisiones",
                crear_pasivo = "can_manage_pasivos", crear_requisicion = "can_view_requisiciones",
                generar_archivo_pago = "can_register_payments",
                importar_bancos = "can_manage_bancos", movimiento_manual = "can_manage_bancos")
  for (a in names(esperado)) expect_equal(alertas_preparar(CID, a, "beto")$permiso, esperado[[a]], label = a)
  expect_null(alertas_preparar(CID, "confirmar_pago", "beto"))
  # every permiso exists
  expect_true(all(vapply(ALERTAS_ACCIONES, `[[`, character(1), "permiso") %in% names(PERMISOS)))
})

test_that("C3. matriz ilegible: no publica y deja un message", {
  s3_memoria_reset()
  alertas_guardar(CID, marca("ana", confirmar_pago = TRUE), por = "ana")
  .mem_falla <<- KEY
  expect_message(expect_null(alertas_preparar(CID, "confirmar_pago", "ana")), "no se pudo leer")
  .mem_falla <<- character()
})

s3_memoria_restaurar()
