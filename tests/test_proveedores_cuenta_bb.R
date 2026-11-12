# =============================================================================
# tests/test_proveedores_cuenta_bb.R
# 2026-11-11 (PENDIENTES §2 "Menores"). A BanBajío account has 12 digits (56
# of 59 in prod and dev); Excel, when the cell was a number, dropped the
# leading 0 of two of them. The import used to keep the 11 digits with only a
# warning, and no_cuenta is the catalog's dedup key: the same account with
# and without its 0 became two suppliers on the next import.
#   1  prov_cuenta_bb12(): 10-11 digits of a BanBajío account to 12; any
#      other length, and any other bank, as they are.
#   2  import: BanBajío (bank 030 or "Cuenta BB") as a number with 10-11
#      digits -> 12, with its note; typed as text, 13 digits, or another
#      bank -> as before.
#   3  the catalog key: an account stored without its 0 and the same one
#      with it are one supplier (an update that puts the 0 back), not two.
# Run from project root:  Rscript tests/test_proveedores_cuenta_bb.R
# =============================================================================
suppressPackageStartupMessages(library(testthat))
source("tests/helpers/proveedores_testserver.R")

fila <- function(nombre, banco, cuenta, tipo = NA_character_, numero = TRUE) {
  f <- data.frame(.row_num = 2L, nombre = nombre, banco = banco, no_cuenta = cuenta, tipo_cuenta = tipo,
                  moneda = "MXN", .num_no_cuenta = numero, .num_banco = FALSE, stringsAsFactors = FALSE)
  for (k in setdiff(names(PROV_EXCEL_CAMPOS), names(f))) f[[k]] <- NA_character_
  f
}
clasificar <- function(filas, activos = NULL) suppressMessages(prov_clasificar(filas, activos, NULL))
NOTA_BB <- "tablas.proveedores.importar.nota_cero_bb"

test_that("1 prov_cuenta_bb12: 10-11 digits of a BanBajío account to 12, nothing else", {
  if (!exists("prov_cuenta_bb12")) fail("prov_cuenta_bb12() does not exist")
  expect_identical(prov_cuenta_bb12(c("12345678901", "1234567890", "012345678901", "1234567890123", "12345678901"),
                                    c(TRUE, TRUE, TRUE, TRUE, FALSE)),
                   c("012345678901", "001234567890", "012345678901", "1234567890123", "12345678901"))
  expect_identical(prov_cuenta_bb12(c(NA, "ABC12345678"), TRUE), c(NA, "ABC12345678"))
})

test_that("1b a row's notes: the new one is joined to another note, and replaces aviso_numero", {
  if (!exists("prov_nota_agregar")) fail("prov_nota_agregar() does not exist")
  clabe <- "tablas.proveedores.importar.nota_banco_clabe"; aviso <- "tablas.proveedores.importar.aviso_numero"
  expect_identical(prov_nota_agregar(c(NA, clabe, aviso, paste(clabe, aviso, sep = "|")), NOTA_BB, quitar = aviso),
                   c(NOTA_BB, paste(clabe, NOTA_BB, sep = "|"), NOTA_BB, paste(clabe, NOTA_BB, sep = "|")))
})

test_that("2a BanBajío by its bank, 11 digits as a number: 12, said in the row", {
  f <- clasificar(fila("BAJIO SA", "030", "12345678901"))
  expect_identical(f$no_cuenta, "012345678901")
  expect_identical(f$.nota, NOTA_BB)
  expect_identical(f$.estado, "listo")
})

test_that("2b BanBajío by 'Cuenta BB' and no bank name, 10 digits as a number: 12, bank 030", {
  f <- clasificar(fila("BB SA", NA_character_, "1234567890", tipo = "Cuenta BB"))
  expect_identical(c(f$no_cuenta, f$banco), c("001234567890", "030"))
  expect_identical(f$.nota, NOTA_BB)
})

test_that("2c the same account typed as text, a 13-digit one and another bank's: as before", {
  expect_identical(clasificar(fila("TEXTO SA", "030", "12345678901", numero = FALSE))$no_cuenta, "12345678901")
  expect_identical(clasificar(fila("TRECE SA", "030", "1234567890123"))$no_cuenta, "1234567890123")
  otro <- clasificar(fila("OTRO SA", "012", "12345678901"))
  expect_identical(otro$no_cuenta, "12345678901")
  expect_identical(otro$.nota, "tablas.proveedores.importar.aviso_numero")
})

test_that("3a stored without its 0, the file brings it with the 0: one supplier, an update that puts it back", {
  ex <- prov_fila("p-1", "BAJIO SA", "12345678901", banco = "030")
  f <- clasificar(fila("BAJIO SA", "030", "012345678901", numero = FALSE), ex)
  expect_identical(f$.existente_id, "p-1")
  expect_identical(f$.estado, "actualiza")
  cambio <- Filter(function(c) identical(c$campo, "no_cuenta"), f$.cambios[[1]])
  expect_length(cambio, 1L)
  expect_identical(c(cambio[[1]]$antes, cambio[[1]]$despues), c("12345678901", "012345678901"))
})

test_that("3b stored with its 0, the file lost it (a number): the same supplier, no change", {
  ex <- prov_fila("p-1", "BAJIO SA", "012345678901", banco = "030")
  f <- clasificar(fila("BAJIO SA", "030", "12345678901"), ex)
  expect_identical(f$.existente_id, "p-1")
  expect_identical(f$.estado, "sin_cambios")
})

test_that("3c another bank's account is not padded in the key: no false match", {
  ex <- prov_fila("p-1", "OTRO SA", "012345678901", banco = "012")
  f <- clasificar(fila("OTRO SA", "012", "12345678901"), ex)
  expect_true(is.na(f$.existente_id))
})
