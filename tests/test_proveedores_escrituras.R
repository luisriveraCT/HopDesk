# =============================================================================
# tests/test_proveedores_escrituras.R
# Proveedores' one-row writes when S3 fails (T07 VP2/VP3), through the real
# proveedoresServer in testServer (tests/helpers/proveedores_testserver.R).
#   VP2: moving a provider between lists writes the receiving list first, so
#        a failed second write leaves it in both lists, never in neither.
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
  list(activos = activos$id, inactivos = inactivos$id, error = error, avisos = avisos)
}
inactivar <- function(id) function(session) { session$setInputs(prov_inactivar_id = id); session$setInputs(prov_confirm_inactivar = 1) }
reactivar <- function(id) function(session) { session$setInputs(prov_reactivar_id = id, prov_reactivar_hasta = "indefinido")
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
