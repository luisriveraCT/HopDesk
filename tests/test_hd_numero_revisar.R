# =============================================================================
# tests/test_hd_numero_revisar.R
# T66: hd_numero_revisar() (R/ui_components.R), the server-side check every
# save that reads a hd_numero_input() runs before writing: the browser no
# longer enforces min/max. A number inside the range passes as a number;
# NA, a decimal where an entero goes, or a number outside the range gives
# ok = FALSE and an aviso from the dictionary (keys in es and en).
# Run from the repo root:  Rscript tests/test_hd_numero_revisar.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/i18n.R", "R/ui_components.R")) suppressMessages(source(f))

test_that("un número dentro del rango pasa, como número", {
  r <- hd_numero_revisar(1500.5, "Cargo", min = 0)
  expect_true(r$ok); expect_identical(r$valor, 1500.5); expect_null(r$aviso)
  expect_identical(hd_numero_revisar(0, "Cargo", min = 0)$valor, 0)
  expect_identical(hd_numero_revisar(-2500, "Saldo")$valor, -2500)          # no min: a negative balance
  expect_identical(hd_numero_revisar(500L, "Límite", min = 1, max = 500, entero = TRUE)$valor, 500)
  expect_identical(hd_numero_revisar(-365, "Días", min = -365, max = 365, entero = TRUE)$valor, -365)
})

test_that("NA, vacío o texto: no es número", {
  for (x in list(NA, NA_real_, NULL, numeric(0), "abc", c(1, 2), Inf)) {
    r <- hd_numero_revisar(x, "Cargo", min = 0)
    expect_false(r$ok); expect_true(is.na(r$valor))
    expect_identical(r$aviso, "Cargo: escribe un número.")
  }
})

test_that("un decimal donde va un entero se rechaza, no se trunca", {
  r <- hd_numero_revisar(2.7, "Límite de usuarios", min = 1, max = 500, entero = TRUE)
  expect_false(r$ok)
  expect_identical(r$aviso, "Límite de usuarios: escribe un número entero, sin decimales.")
  expect_true(hd_numero_revisar(2.7, "Monto")$ok)                             # not entero: fine
})

test_that("fuera de rango: el aviso dice el límite", {
  expect_identical(hd_numero_revisar(-0.01, "Cargo", min = 0)$aviso, "Cargo no puede ser negativo.")
  expect_identical(hd_numero_revisar(0, "Límite", min = 1)$aviso, "Límite no puede ser menor que 1.")
  expect_identical(hd_numero_revisar(1e6, "Tasa", max = 100)$aviso, "Tasa no puede ser mayor que 100.")
  expect_identical(hd_numero_revisar(501, "Límite", min = 1, max = 500, entero = TRUE)$aviso,
                   "Límite debe estar entre 1 y 500.")
  expect_identical(hd_numero_revisar(0, "Límite", min = 1, max = 500, entero = TRUE)$aviso,
                   "Límite debe estar entre 1 y 500.")
  expect_identical(hd_numero_revisar(-366, "Días", min = -365, max = 365, entero = TRUE)$aviso,
                   "Días debe estar entre -365 y 365.")
})

test_that("los avisos existen en los dos idiomas, con los mismos huecos", {
  claves <- paste0("comun.numero.aviso.", c("no_es_numero", "entero", "negativo", "minimo", "maximo", "rango"))
  es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
  for (k in claves) {
    expect_true(nzchar(es[[k]] %||% ""), info = k); expect_true(nzchar(en[[k]] %||% ""), info = k)
    huecos <- function(s) sort(regmatches(s, gregexpr("\\{[a-z_]+\\}", s))[[1]])
    expect_identical(huecos(en[[k]]), huecos(es[[k]]), info = k)
  }
  expect_identical(tr("comun.numero.aviso.rango", campo = "Limit", min = "1", max = "500", idioma = "en"),
                   "Limit must be between 1 and 500.")
})
