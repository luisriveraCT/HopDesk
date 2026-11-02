# =============================================================================
# tests/test_proveedores_escrituras.R
# Proveedores' one-row writes when S3 fails (T07 VP2/VP3), through the real
# proveedoresServer in testServer (tests/helpers/proveedores_testserver.R).
#   VP2: moving a provider between lists writes the receiving list first, so
#        a failed second write leaves it in both lists, never in neither.
#   VP3: every one-row write that fails tells the user and is logged (rule 8),
#        instead of an error that ends the observer in silence.
#
# Run from project root:  Rscript tests/test_proveedores_escrituras.R
# =============================================================================
suppressPackageStartupMessages(library(testthat))
source("tests/helpers/proveedores_testserver.R")

a1 <- prov_fila("p-1", "ACEROS SA", "012180001111111119")
a2 <- prov_fila("p-2", "FLETES SA", "072180002222222224", banco = "072")
i1 <- prov_fila("p-9", "VIEJO SA", "014180004444444448", banco = "014", activo = FALSE)

# Runs `pasos` (a function of session) against the real module over a1, a2
# (active) and i1 (inactive), with `falla` failing; returns where each
# provider ended up: what was saved, or the list as it was if not saved.
mover <- function(pasos, falla = NULL) {
  rm(list = ls(.prov_guardado), envir = .prov_guardado)
  prov_logs(limpiar = TRUE)
  prov_falla(falla); on.exit(prov_falla(NULL))
  shared <- list(
    proveedores_db = reactiveVal(rbind(a1, a2)), proveedores_inactivos_db = reactiveVal(i1),
    company_map = reactive(c(NTS = "Networks Trucking")), effective_client_id = function() "prueba",
    home_client_id = function() "prueba", current_user = function() "prueba", usuario = function() "prueba")
  avisos <- character(0)
  previo <- if (exists("showNotification", envir = globalenv(), inherits = FALSE)) get("showNotification", envir = globalenv())
  on.exit(if (is.null(previo)) rm("showNotification", envir = globalenv()) else assign("showNotification", previo, envir = globalenv()),
          add = TRUE)
  assign("showNotification", function(ui, ...) { avisos <<- c(avisos, paste(as.character(ui), collapse = " ")); invisible(NULL) },
         envir = globalenv())
  error <- tryCatch({ suppressMessages(shiny::testServer(proveedoresServer, args = list(shared = shared), {
    # The page's first flush, as in a browser: observers with ignoreInit skip
    # it, so the first "click" below is a real event, not their initial value.
    session$flushReact()
    pasos(session)
  })); NULL },
                    error = function(e) conditionMessage(e))
  activos   <- .prov_guardado$activos   %||% rbind(a1, a2)
  inactivos <- .prov_guardado$inactivos %||% i1
  list(activos = activos$id, inactivos = inactivos$id, error = error, avisos = avisos, logs = prov_logs())
}
# A row action as the table sends it (input "<tabla>-accion": accion, ids, nonce).
.nonce <- 0L
accion <- function(session, tabla, accion, id) {
  .nonce <<- .nonce + 1L
  do.call(session$setInputs, setNames(list(list(accion = accion, ids = list(id), nonce = .nonce)), paste0(tabla, "-accion")))
}
inactivar <- function(id) function(session) { accion(session, "catalogo", "inactivar", id); session$setInputs(prov_confirm_inactivar = 1) }
reactivar <- function(id) function(session) { session$setInputs(prov_reactivar_hasta = "indefinido")
                                              accion(session, "inactivos", "reactivar", id)
                                              session$setInputs(prov_confirm_reactivar = 1) }

test_that("VP2: Inactivar moves the provider (both writes fine)", {
  r <- mover(inactivar("p-1"))
  expect_identical(r$activos, "p-2")
  expect_setequal(r$inactivos, c("p-9", "p-1"))
})

test_that("VP2: Inactivar with the second write failing leaves it in both lists, never in neither", {
  r <- mover(inactivar("p-1"), falla = "activos")
  expect_true("p-1" %in% r$inactivos)
  expect_true("p-1" %in% r$activos)
})

test_that("VP2: Reactivar moves the provider (both writes fine)", {
  r <- mover(reactivar("p-9"))
  expect_setequal(r$activos, c("p-1", "p-2", "p-9"))
  expect_length(r$inactivos, 0L)
})

test_that("VP2: Reactivar with the second write failing leaves it in both lists, never in neither", {
  r <- mover(reactivar("p-9"), falla = "inactivos")
  expect_true("p-9" %in% r$activos)
  expect_true("p-9" %in% r$inactivos)
})

# ── VP3 ─────────────────────────────────────────────────────────────────────
eliminar     <- function(id) function(session) accion(session, "catalogo", "eliminar", id)
eliminar_ina <- function(id) function(session) accion(session, "inactivos", "eliminar", id)
editar <- function(id, nombre) function(session) {
  accion(session, "catalogo", "editar", id)
  session$setInputs(prov_nombre = nombre, prov_alias = "", prov_rfc = "", prov_no_cuenta = "012180001111111119",
                    prov_banco = "BBVA Bancomer|012", prov_correo = "", prov_moneda = "MXN", prov_tipo_relacion = "",
                    prov_empresas = "NTS", prov_activo_hasta_sel = "indefinido")
  session$setInputs(prov_modal_save = 1)
}
falla_visible <- function(r, accion, id) {
  expect_null(r$error)                                             # the observer didn't die
  expect_true(any(grepl("No se pudo guardar el cambio en el catálogo de proveedores", r$avisos)), info = accion)
  expect_true(paste0(accion, "_fallido ", id) %in% r$logs, info = paste(r$logs, collapse = " | "))
}

test_that("VP3: a failed Inactivar is shown and logged", {
  falla_visible(mover(inactivar("p-1"), falla = "inactivos"), "inactivar_proveedor", "p-1")
})
test_that("VP3: a failed Reactivar is shown and logged", {
  falla_visible(mover(reactivar("p-9"), falla = "activos"), "reactivar_proveedor", "p-9")
})
test_that("VP3: a failed Eliminar is shown and logged, and the provider is still there", {
  r <- mover(eliminar("p-2"), falla = "activos")
  falla_visible(r, "eliminar_proveedor", "p-2")
  expect_true("p-2" %in% r$activos)
})
test_that("VP3: a failed Eliminar of an inactive one is shown and logged", {
  falla_visible(mover(eliminar_ina("p-9"), falla = "inactivos"), "eliminar_proveedor_inactivo", "p-9")
})
test_that("VP3: a failed edit is shown and logged; nothing changes", {
  r <- mover(editar("p-1", "ACEROS EDITADO SA"), falla = "activos")
  falla_visible(r, "editar_proveedor", "p-1")
})
test_that("VP3: when nothing fails there is no error notice and no failure log", {
  r <- mover(editar("p-1", "ACEROS EDITADO SA"))
  expect_null(r$error)
  expect_false(any(grepl("No se pudo guardar", r$avisos)))
  expect_false(any(grepl("_fallido", r$logs)))
  expect_true(any(grepl("Proveedor actualizado", r$avisos)))
})
