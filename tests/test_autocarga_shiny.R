# Guardian: the app loads the way shiny (and shinyapps.io) loads it.
# Before app.R runs, shiny sources every file in R/ in alphabetical order
# (shiny::loadSupport). A top-level call to a function defined in a later
# file (bancos_module.R calling col_fecha() from hd_columnas.R) breaks the
# real app, while T15 still passes: it sources app.R directly.
# Run from the repo root: Rscript tests/test_autocarga_shiny.R
library(testthat)

test_that("R/ carga en el orden alfabético de shiny::loadSupport", {
  # A separate R process: global.R reads the environment and defines globals.
  r <- callr::r(function(raiz) {
    Sys.unsetenv("HOPDESK_PRODUCTION")
    tryCatch({
      shiny::loadSupport(raiz, renv = new.env(parent = globalenv()), globalrenv = NULL)
      "OK"
    }, error = function(e) conditionMessage(e))
  }, args = list(raiz = normalizePath(".", winslash = "/")))
  expect_identical(r, "OK")
})
