# Requisiciones, stage E3b-0: the contract other tabs use for the evidence
# clip (shared$req_clip / req_abrir_evidencia / req_version, wired in app.R).
# Data over tests/helpers/requisiciones_fixture_app.R (in-memory S3).
# Run from the repo root: Rscript tests/test_requisiciones_e3b.R
library(testthat)
suppressPackageStartupMessages(library(shiny))

cargar_app <- function(nivel = "compras", usuario = "dani") {
  Sys.setenv(REQ_NIVEL = nivel, REQ_USUARIO = usuario, HD_ARRANQUE_NULL = "", REQ_FALLA_LECTURA = "",
             REQ_OVERRIDES = "", REQ_ABRIR_PROVISION = "")
  suppressMessages(source("tests/helpers/requisiciones_fixture_app.R")$value)
}
invisible(cargar_app())
fila <- function(titulo) { r <- .req_mem[["fixture/requisiciones.rds"]]; r[r$titulo == titulo, ] }
u    <- function(nivel) list(user = "x", tier = nivel, permisos = permisos_resueltos(nivel))
png1 <- function() {
  f <- tempfile(fileext = ".png")
  writeBin(magick::image_write(magick::image_blank(30, 20, "red"), format = "png"), f)
  adjunto_preparar(f, "factura.png", "archivo")
}

test_that("el clip cuenta la evidencia viva por provisión, sin requisición no hay fila", {
  evidencia_adjuntar(CID, fila("Luz bodega 3")$id, png1(), "eli")
  c <- requisiciones_clip_por_provision(CID, c("p-luz", "p-pagada", "p-libre1", NA, "p-luz"))
  expect_setequal(c$provision_id, c("p-luz", "p-pagada"))            # p-libre1: no requisition
  expect_identical(c$total[c$provision_id == "p-luz"], 1L)
  expect_identical(c$total[c$provision_id == "p-pagada"], 0L)        # paid is still found
  expect_identical(c$requisicion_id[c$provision_id == "p-luz"], fila("Luz bodega 3")$id)
  ev <- .req_mem[["fixture/requisiciones_evidencia.rds"]]
  evidencia_papelera(CID, ev$id[ev$requisicion_id == fila("Luz bodega 3")$id][1], "eli")
  expect_identical(requisiciones_clip_por_provision(CID, "p-luz")$total, 0L)   # trashed evidence: not counted
  expect_equal(nrow(requisiciones_clip_por_provision(CID, character())), 0)
})

test_that("una requisición en la papelera no da clip a su provisión", {
  r <- requisicion_crear(CID, list(titulo = "Para tirar", empresa = "NCS", provision_id = "p-libre2"), "dani")
  expect_equal(nrow(requisiciones_clip_por_provision(CID, "p-libre2")), 1)
  requisiciones_papelera(CID, r$id, "dani")
  expect_equal(nrow(requisiciones_clip_por_provision(CID, "p-libre2")), 0)
})

test_that("shared$req_clip: solo quien ve Requisiciones, y nunca un error", {
  expect_gt(nrow(requisiciones_clip(CID, u("compras"), "p-luz")), 0)
  expect_equal(nrow(requisiciones_clip(CID, u("finance"), "p-luz")), 0)    # Luis: only Requisiciones
  expect_equal(nrow(requisiciones_clip(CID, u("analysis"), "p-luz")), 0)
  expect_equal(nrow(requisiciones_clip(CID, NULL, "p-luz")), 0)
  expect_equal(nrow(requisiciones_clip(NULL, u("compras"), "p-luz")), 0)
  expect_equal(nrow(requisiciones_clip("", u("compras"), "p-luz")), 0)
  assign(".req_falla_lectura", TRUE, envir = globalenv())
  expect_no_error(x <- suppressMessages(requisiciones_clip(CID, u("compras"), "p-luz")))
  expect_equal(nrow(x), 0)
  assign(".req_falla_lectura", FALSE, envir = globalenv())
})

test_that("req_abrir_evidencia por el puente abre la requisición de la provisión", {
  app <- cargar_app()
  testServer(app, {
    session$flushReact()
    puente <- new.env(); puente$abrir <- mod$abrir_por_provision
    abrir <- function(pid) { f <- puente$abrir; if (is.function(f)) f(pid) else invisible(FALSE) }
    expect_true(abrir("p-pagada"))
    expect_identical(mod$detalle_id(), fila("Papelería octubre")$id)
    expect_true(mod$lectura())
  })
})

test_that("app.R pone el contrato en shared desde el inicio y conecta el puente", {
  src <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
  lista <- regmatches(src, regexpr("shared <- list\\((.|\n)*?\n  \\)\n", src))
  expect_length(lista, 1L)
  for (k in c("req_version", "req_clip", "req_abrir_evidencia"))
    expect_match(lista, paste0("\n    ", k, " +="), info = k)
  expect_match(src, "req_puente$abrir <- req_srv$abrir_por_provision", fixed = TRUE)
  expect_match(lista, "requisiciones_clip(", fixed = TRUE)
  # E3b-1: "Crear provisión" opens Pasivos' manual provision form
  expect_match(src, "pt <- pasivos_table_module_server(\"pt\", shared)", fixed = TRUE)
  expect_match(src, "abrir_provision = pt$abrir_provision_manual", fixed = TRUE)
  expect_lt(regexpr("pt <- pasivos_table_module_server", src, fixed = TRUE),
            regexpr("req_srv <- requisicionesServer", src, fixed = TRUE))
})
