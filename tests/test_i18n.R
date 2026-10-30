# Shared language dictionary (R/i18n.R, www/i18n/*.json).
# Run from the repo root: Rscript tests/test_i18n.R
library(testthat)
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")

es <- i18n_dict("es")
en <- i18n_dict("en")
placeholders <- function(s) sort(unique(regmatches(s, gregexpr("\\{[a-z_]+\\}", s))[[1]]))

test_that("keys follow <area>.<screen>.<thing> and areas are known", {
  ok <- grepl("^(comun|tablas|panorama|requisiciones|niveles|permisos|avisos)(\\.[a-z0-9_]+)+$", names(es))
  expect_true(all(ok), info = paste(names(es)[!ok], collapse = ", "))
})

test_that("every translated key exists in Spanish", {
  extra <- setdiff(names(en), names(es))
  expect_length(extra, 0)
})

test_that("translations keep the same placeholders", {
  for (k in intersect(names(en), names(es)))
    expect_identical(placeholders(en[[k]]), placeholders(es[[k]]), info = k)
})

test_that("plurals come in pairs", {
  base_uno   <- sub("\\.uno$", "", grep("\\.uno$", names(es), value = TRUE))
  base_otros <- sub("\\.otros$", "", grep("\\.otros$", names(es), value = TRUE))
  expect_setequal(base_uno, base_otros)
})

test_that("tr() fills named placeholders and falls back", {
  expect_identical(tr_n("tablas.contador.facturas", 1), "1 factura")
  expect_identical(tr_n("tablas.contador.facturas", 1594), "1,594 facturas")
  expect_identical(tr("tablas.confirmar_accion", accion = "Eliminar", n = 3), "¿Eliminar 3 filas?")
  expect_identical(tr("comun.cerrar", idioma = "en"), "Close")
  expect_identical(tr("comun.cerrar", idioma = "fr"), "Cerrar")
  expect_warning(expect_identical(tr("tablas.no_existe"), "tablas.no_existe"), "missing key")
})

test_that("every literal key used in code exists in Spanish", {
  # The dictionary's own docs (R/i18n.R, www/i18n.js) show example keys; skip them.
  files <- c(list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE),
             list.files("www", "\\.js$", full.names = TRUE))
  files <- files[!basename(files) %in% c("i18n.R", "i18n.js")]
  # (?<![$.\w]) keeps htmltools' tags$tr("...") and obj.tr(...) out of the scan.
  keys_in <- function(fn) unlist(lapply(files, function(f) {
    src <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    pat <- paste0("(?<![$.\\w])(", fn, ")\\(\\s*[\"']([a-z0-9_.]+)[\"']")
    m <- regmatches(src, gregexpr(pat, src, perl = TRUE))[[1]]
    sub("^.*[\"']([a-z0-9_.]+)[\"']$", "\\1", m)
  }))
  used   <- keys_in("tr|hdT")
  plural <- keys_in("tr_n|hdTn")
  # paste0(character(0), ".uno") is ".uno", not character(0): guard the empty case.
  faltan <- c(setdiff(used, names(es)),
              if (length(plural)) setdiff(paste0(plural, ".uno"), names(es)))
  expect_length(faltan, 0)
  if (length(faltan)) message("Missing keys: ", paste(faltan, collapse = ", "))
})
