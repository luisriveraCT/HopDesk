# The Usuarios permissions editor opens for every tier. Run from the repo root:
#   Rscript tests/test_editor_permisos_abre.R
# permisos_resueltos() returns a named logical vector; `current_perms$key` on
# it is "$ operator is invalid for atomic vectors", so the editor broke for
# any account (found by Ejecutor 2 in the T28 screenshots, 2026-11-10).
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
# Only the file's function definitions (the editor and its helpers): its
# other top-level code needs the whole app.
local({
  ex <- parse("R/tiers_module.R", keep.source = FALSE)
  es <- vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                 is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")), logical(1))
  for (e in ex[es]) eval(e, globalenv())
  stopifnot(exists(".tiers_perm_editor_ui"))
})
if (!exists(".tier_color")) .tier_color <- function(t) nivel_color(t)

ns  <- shiny::NS("t")
reg <- data.frame(client_id = c("acme", "beta"), display_name = c("Acme", "Beta"),
                  status = "active", stringsAsFactors = FALSE)
fila <- function(tier, permisos = "{}")
  data.frame(username = "x", tier = tier, permisos = permisos, allowed_clients = NA_character_,
             stringsAsFactors = FALSE)

test_that("permisos_resueltos sigue siendo un vector lógico con nombres", {
  expect_type(permisos_resueltos(nivel_ti()), "logical")
})

test_that("el editor abre para cada nivel, visto por el nivel más alto", {
  for (t in niveles_todos())
    expect_no_error(.tiers_perm_editor_ui(ns, fila(t), viewer_tier = nivel_tope(), registry = reg,
                                          viewer_is_staff = TRUE), message = t)
})

test_that("con can_jump_clients encendido sale la sección de clientes permitidos", {
  t  <- niveles_staff()[length(niveles_staff())]
  ui <- .tiers_perm_editor_ui(ns, fila(t, '{"can_jump_clients": true}'), viewer_tier = nivel_tope(),
                              registry = reg, viewer_is_staff = TRUE)
  expect_match(as.character(ui), "Acme", fixed = TRUE)
})

test_that("un cliente lo abre para los niveles que puede ver", {
  for (t in niveles_cliente())
    expect_no_error(.tiers_perm_editor_ui(ns, fila(t), viewer_tier = nivel_ti(), registry = NULL,
                                          viewer_is_staff = FALSE), message = t)
})
