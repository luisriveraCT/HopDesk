# =============================================================================
# tests/test_t72_prefs_log.R
# T72: a saved preference (avisos, "De quién recibo avisos", Panorama) is
# in the Actividad log, category "preferencias", with only what changed and
# its antes/despues. Panorama's ultima_visita (written on every visit) is
# not logged, like the "leído" marks. A log that fails never fails the save.
# Harness: the one of tests/test_avisos.R (in-memory store, aws.s3 never
# patched); log_action is a recorder.
# Run from the repo root: Rscript tests/test_t72_prefs_log.R
# Contraprueba: on 867d251 the first two blocks fail.
# =============================================================================
library(testthat)
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")

.mem <- new.env()
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.s3_key <- function(key, client_id = NULL) paste0(client_id, "/", key)
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.fallar_escrituras <- 0L
.s3_write <- function(obj, key, client_id = NULL, ...) {
  if (.fallar_escrituras > 0L) { .fallar_escrituras <<- .fallar_escrituras - 1L; stop("simulada") }
  assign(paste0(client_id, "/", key), obj, envir = .mem)
}
LOG <- list()
log_action <- function(user, module, action, description, target_id = NA_character_, s3_key = NA_character_,
                       metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
  LOG[[length(LOG) + 1L]] <<- list(user = user, module = module, action = action, description = description,
                                   metadata = metadata, client_id = client_id, home = viewer_home_client_id)
  invisible(NULL)
}
CID <- "cliente_prueba"
reiniciar <- function() { rm(list = ls(.mem), envir = .mem); LOG <<- list() }

test_that("guardar una preferencia deja una entrada 'preferencias' con su antes y su después", {
  reiniciar()
  prefs_guardar(CID, "Ana", list(req_avisos = "mias"))
  expect_length(LOG, 1L)
  e <- LOG[[1]]
  expect_identical(e$module, "preferencias")
  expect_identical(e$action, "guardar_preferencias")
  expect_identical(e$user, "ana")
  expect_identical(e$client_id, CID)
  expect_identical(e$home, CID)
  expect_null(e$metadata$cambios$req_avisos$antes)
  expect_identical(e$metadata$cambios$req_avisos$despues, "mias")
  expect_match(e$description, "req_avisos", fixed = TRUE)
  LOG <<- list()
  prefs_guardar(CID, "ana", list(req_avisos = "todas"), home = "hd-admin")
  expect_identical(LOG[[1]]$metadata$cambios$req_avisos, list(antes = "mias", despues = "todas"))
  expect_identical(LOG[[1]]$home, "hd-admin")
})

test_that("solo lo que cambió; sin cambios, nada; ultima_visita nunca", {
  reiniciar()
  prefs_guardar(CID, "ana", list(idioma = "en", inicio = "PSV")); LOG <<- list()
  prefs_guardar(CID, "ana", list(idioma = "en", inicio = "Bancos"))
  expect_identical(names(LOG[[1]]$metadata$cambios), "inicio")
  LOG <<- list()
  prefs_guardar(CID, "ana", list(idioma = "en"))                                 # the same value
  expect_length(LOG, 0L)
  prefs_guardar(CID, "ana", list(ultima_visita = format(Sys.time(), tz = "UTC")))  # every visit
  expect_length(LOG, 0L)
  expect_identical(prefs_leer(CID, "ana")$inicio, "Bancos")
})

test_that("un log que falla nunca rompe el guardado; una escritura que falla no deja log", {
  reiniciar()
  log_action <<- function(...) stop("S3 caído")
  expect_silent(suppressMessages(prefs_guardar(CID, "ana", list(inicio = "PSV"))))
  expect_identical(prefs_leer(CID, "ana")$inicio, "PSV")
  log_action <<- function(...) { LOG[[length(LOG) + 1L]] <<- list(...); invisible(NULL) }
  LOG <<- list(); .fallar_escrituras <<- 2L                   # .s3_modificar tries twice
  expect_error(suppressMessages(prefs_guardar(CID, "ana", list(inicio = "Bancos"))))
  expect_length(LOG, 0L)
  .fallar_escrituras <<- 1L                                  # the retry works: one entry
  suppressMessages(prefs_guardar(CID, "ana", list(inicio = "Bancos")))
  expect_length(LOG, 1L)
})

test_that("la clave avisos.log.preferencias existe en es y en", {
  es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
  expect_true(nzchar(es[["avisos.log.preferencias"]] %||% "") && nzchar(en[["avisos.log.preferencias"]] %||% ""))
})
