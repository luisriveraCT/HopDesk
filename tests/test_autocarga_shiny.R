# Guardian: the app loads the way shiny (and shinyapps.io) loads it.
# Before app.R runs, shiny sources every file in R/ in alphabetical order
# (shiny::loadSupport). A top-level call to a function defined in a later
# file (bancos_module.R calling col_fecha() from hd_columnas.R) breaks the
# real app, while T15 still passes: it sources app.R directly.
# Loading all of global.R takes the single-instance lock and copies users
# from prod to dev: tests/helpers/aislar_global.R gives this process its own
# lock and no network first (candado, 2026-11-11).
# Run from the repo root: Rscript tests/test_autocarga_shiny.R
library(testthat)
source("tests/helpers/aislar_global.R")

test_that("R/ carga en el orden alfabético de shiny::loadSupport", {
  # A separate R process: global.R reads the environment and defines globals.
  # It inherits this one's environment (its own lock, no network).
  r <- callr::r(function(raiz, sin_red) {
    Sys.unsetenv("HOPDESK_PRODUCTION")
    tryCatch({
      shiny::loadSupport(raiz, renv = new.env(parent = globalenv()), globalrenv = NULL)
      # Checked AFTER global.R ran its readRenviron(): still no way out.
      if (sin_red()) "OK" else "global.R cargado CON red hacia AWS"
    }, error = function(e) conditionMessage(e))
  }, args = list(raiz = normalizePath(".", winslash = "/"), sin_red = aislar_global_sin_red))
  expect_identical(r, "OK")
})
