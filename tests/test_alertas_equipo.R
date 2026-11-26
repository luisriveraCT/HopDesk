# Alertas del equipo (front C): the users × actions matrix and
# alertas_preparar(), in R/avisos_alertas_module.R.
# T68: the matrix no longer decides what is published (its tab left Publicar;
# A/B/B2 keep its screen's functions honest while they exist). The gate is
# permissions only (Coordinador, 2026-11-20): an "act" is published when
# another live user of the client holds the action's permission; each
# reader's "De quién recibo avisos" filters when shown.
# S3 is the in-memory store of tests/helpers/s3_memoria.R (restored at the end).
# Run from the repo root: Rscript tests/test_alertas_equipo.R
suppressPackageStartupMessages({ library(testthat); library(shiny) })
source("tests/helpers/s3_memoria.R")
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos_alertas_module.R")
source(Sys.getenv("ALERTAS_AVISOS_R", "R/avisos.R"))   # the contraprueba points it at the old file
source("R/panorama_datos.R")   # T68: avisos_de_alguien_puede(), the gate

CID <- "acme"
KEY <- paste0(CID, "/avisos_alertas.rds")
auth_load_usuarios <- function(client_id = NULL) data.frame(
  username = c("Ana", "beto", "caro", "dani", "eli", "staff1"),
  display_name = c("Ana López", "", "Caro", "Dani", "Eli", "Soporte"),
  tier = c("admin", "finance", "analysis", "compras", "finance", setdiff(niveles_todos(), niveles_cliente())[1]),
  deleted = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
  activo  = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE), stringsAsFactors = FALSE)

# T68: the gate reads the client's users from S3 (usuarios.rds), strictly.
USUARIOS_S3 <- data.frame(username = c("ana", "beto", "caro", "dani"), tier = c("admin", "finance", "analysis", "compras"),
                          account_code = c("A1", "B1", "C1", "D1"), permisos = "{}", deleted = FALSE,
                          activo = c(TRUE, TRUE, TRUE, FALSE), stringsAsFactors = FALSE)
con_usuarios <- function(u = USUARIOS_S3) .s3_write(u, S3_KEYS$usuarios, client_id = CID, permitir_reduccion = TRUE)

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

# B and B2 tested alertas_guardar(), retired with the screen (T113).

test_that("C. alertas_preparar (T68): another user with the action's permission, never the matrix", {
  s3_memoria_reset(); con_usuarios()
  p <- alertas_preparar(CID, "confirmar_pago", "ANA", list(cuerpo = "2 pago(s)"))    # beto (finance) may see it
  expect_equal(p$cuerpo, "2 pago(s)")
  expect_equal(p$permiso, "can_confirm_payments")
  expect_equal(p$clave, "act|confirmar_pago|ana")
  expect_equal(p$ventana_min, 10L)
  expect_false(is.null(alertas_preparar(CID, "confirmar_pago", "beto")))          # ana (admin) may see it; no matrix needed
  con_usuarios(USUARIOS_S3[USUARIOS_S3$username %in% c("ana", "caro"), ])
  expect_null(alertas_preparar(CID, "confirmar_pago", "ana"))                       # only an analyst besides the author
  expect_null(alertas_preparar(CID, "confirmar_pago", ""))
  expect_message(expect_null(alertas_preparar(CID, "inventada", "ana")), "desconocida")
})

test_that("C2. cada acción va con su permiso; crear_requisicion ya no es act (T68)", {
  s3_memoria_reset(); con_usuarios()
  esperado <- c(crear_item = "can_register_payments", crear_provision = "can_edit_provisiones",
                crear_pasivo = "can_manage_pasivos",
                generar_archivo_pago = "can_register_payments",
                importar_bancos = "can_manage_bancos", movimiento_manual = "can_manage_bancos")
  for (a in names(esperado)) expect_equal(alertas_preparar(CID, a, "beto")$permiso, esperado[[a]], label = a)
  expect_null(alertas_preparar(CID, "crear_requisicion", "beto"))
  # every permiso exists
  expect_true(all(vapply(ALERTAS_ACCIONES, `[[`, character(1), "permiso") %in% names(PERMISOS)))
})

test_that("C3. usuarios ilegibles: publica igual y deja un message (T68)", {
  s3_memoria_reset(); con_usuarios()
  .mem_falla <<- paste0(CID, "/", S3_KEYS$usuarios)
  p <- NULL
  expect_message(p <- alertas_preparar(CID, "confirmar_pago", "ana"), "publishing anyway")
  expect_equal(p$permiso, "can_confirm_payments")
  .mem_falla <<- character()
})

# ── D. avisos_actividad() with the matrix (R/avisos.R) ──────────────────────
AVK <- paste0(CID, "/", .AVISOS_KEY)
avisos_mem <- function() if (exists(AVK, envir = .mem)) get(AVK, envir = .mem) else NULL
usr <- function(user, tier) list(user = user, tier = tier, permisos = permisos_resueltos(tier))

test_that("D1. sin nadie más con el permiso no publica; con alguien, una vez (T68)", {
  s3_memoria_reset(); con_usuarios(USUARIOS_S3[USUARIOS_S3$username %in% c("ana", "caro"), ])
  avisos_actividad(CID, "confirmar_pago", "ana", list(cuerpo = "1 pago(s)"))
  expect_null(avisos_mem())
  con_usuarios()
  avisos_actividad(CID, "confirmar_pago", "ana", list(cuerpo = "1 pago(s)"))
  a <- avisos_mem()
  expect_equal(nrow(a), 1L)
  expect_equal(a$permiso, "can_confirm_payments")
  expect_equal(a$tipo, "automatico")
  expect_equal(a$creado_por, "ana")
})

test_that("D2. tres confirmaciones en 10 minutos son un aviso con contador 3", {
  s3_memoria_reset(); con_usuarios()
  for (i in 1:3) avisos_actividad(CID, "confirmar_pago", "ana", list(cuerpo = paste(i, "pago(s)")))
  avisos_actividad(CID, "importar_bancos", "ana")   # another action: its own aviso
  a <- avisos_mem()
  expect_equal(nrow(a), 2L)
  expect_equal(a$contador[a$permiso == "can_confirm_payments"], 3L)
  expect_equal(a$contador[a$permiso == "can_manage_bancos"], 1L)
  # an hour later: a new one
  a$actualizado <- a$actualizado - 3600
  assign(AVK, a, envir = .mem)
  avisos_actividad(CID, "confirmar_pago", "ana")
  expect_equal(sum(avisos_mem()$permiso == "can_confirm_payments"), 2L)
})

test_that("D3. quien no tiene el permiso de la acción no ve el aviso", {
  s3_memoria_reset(); con_usuarios()
  avisos_actividad(CID, "importar_bancos", "ana")
  ve <- function(u) nrow(avisos_visibles(CID, u)) > 0
  expect_true(ve(usr("beto", "finance")))
  sin <- usr("caro", "analysis"); sin$permisos["can_manage_bancos"] <- FALSE
  expect_false(ve(sin))
})

test_that("D4. nunca tumba la acción: alertas_preparar que truena, S3 caído", {
  s3_memoria_reset(); con_usuarios()

  orig <- alertas_preparar
  alertas_preparar <<- function(...) stop("boom")
  expect_message(r <- avisos_actividad(CID, "confirmar_pago", "ana"), "boom")
  expect_null(r); expect_null(avisos_mem())
  alertas_preparar <<- orig

  .mem_falla <<- AVK   # avisos.rds unreadable: avisos_agregar stop()s, caught
  assign(AVK, tibble::tibble(), envir = .mem)
  expect_message(r <- avisos_actividad(CID, "confirmar_pago", "ana"), "falló")
  expect_null(r)
  .mem_falla <<- character()
})

s3_memoria_restaurar()
