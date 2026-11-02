# =============================================================================
# tests/test_proveedores_excel_repro.R
# T08 paso 0: what the Proveedores Excel import does TODAY with each problem
# Luis described ("se le van cosas, omite entradas, muchos se quedan sin CLABE
# o ciertos datos"). One block per fixture in tests/fixtures/proveedores_excel/
# (built by generar.R). Each test states what SHOULD happen, so on the code of
# ab9a5b9 they fail -- that failure is the reproduction. T08 paso 1 makes them
# pass. (Paso 1 renamed the states: a row that touches an existing provider
# is "actualiza" / "reactiva" instead of conflicto_s3_activo / _inactivo.)
#
# It drives the real proveedoresServer in testServer (the file input, the
# conflict buttons, Importar) through tests/helpers/proveedores_testserver.R;
# nothing touches S3.
#
# Run from project root:  Rscript tests/test_proveedores_excel_repro.R
# =============================================================================
suppressPackageStartupMessages(library(testthat))
source("tests/helpers/proveedores_testserver.R")
Sys.setenv(PROV_EXCEL_SOLO_DATOS = "1")
source("tests/fixtures/proveedores_excel/generar.R")
Sys.unsetenv("PROV_EXCEL_SOLO_DATOS")
C  <- clabes_prueba()
fx <- function(f) file.path("tests/fixtures/proveedores_excel", f)
de <- function(df, cuenta) df[!is.na(df$no_cuenta) & df$no_cuenta == cuenta, , drop = FALSE]
quiet <- function(expr) suppressMessages(expr)

aceros <- prov_fila("p-aceros", "ACEROS MONTERREY SA", C$aceros, banco = "012", alias = "ACEROS",
                    rfc = "AMO010101AA1", correo = "pagos@aceros.mx", empresas = "NTS,NCS", codigo = "C-01",
                    tipo_relacion = "Proveedor", activo_hasta = "2027-06-30", created_at = "2025-01-15 10:00:00")
fletes <- prov_fila("p-fletes", "FLETES EL GUERO SA", C$fletes, banco = "072", alias = "FLETES")

# Every block runs and reports, failing or not (a bare test_that() stops the
# script at the first failure, and here failing is the point today).
lista <- testthat::ListReporter$new()
informe <- testthat::MultiReporter$new(list(testthat::SummaryReporter$new(max_reports = Inf), lista))
testthat::with_reporter(informe, {
informe$start_file("test_proveedores_excel_repro.R")

test_that("01 Reemplazar keeps what the Excel doesn't bring (empty cell or missing column)", {
  r <- quiet(prov_importar(fx("01_reemplazo.xlsx"), activos = aceros, decision = "reemplazar"))
  expect_identical(r$filas$.clasificacion, "actualiza")               # an existing provider, never new
  fila <- de(r$activos, C$aceros)
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$nombre, "ACEROS MONTERREY SA DE CV")         # what the Excel changes
  expect_identical(fila$id, "p-aceros")                              # identity kept
  expect_identical(fila$codigo, "C-01")
  expect_identical(fila$created_at, "2025-01-15 10:00:00")
  expect_identical(fila$activo_hasta, "2027-06-30")
  expect_identical(fila$empresas, "NTS,NCS")
  expect_identical(fila$alias, "ACEROS")                             # empty cell: kept
  expect_identical(fila$correo, "pagos@aceros.mx")                   # missing column: kept
  expect_identical(fila$rfc, "AMO010101AA1")
  expect_identical(fila$moneda, "MXN")
  expect_identical(fila$tipo_relacion, "Proveedor")
})

test_that("02 invisible spaces (no-break, thin, BOM) never reach the catalog", {
  r <- quiet(prov_importar(fx("02_espacios.xlsx")))
  expect_true(all(r$filas$.clasificacion == "listo"))
  expect_setequal(r$activos$no_cuenta, c(C$papel, C$guero))
  expect_setequal(r$activos$rfc, c("NAN010101AB1", "GUE020202CD2"))
  expect_setequal(r$activos$nombre, c("PAPELERIA ÑANDU", "FLETES EL GÜERO"))
  expect_setequal(r$activos$alias, c("PAPELERIA", "GUERO"))
})

test_that("03 headers with accents, punctuation and no-break spaces are found", {
  r <- quiet(prov_importar(fx("03_encabezados.xlsx")))
  expect_equal(nrow(r$filas), 2L)
  expect_identical(r$filas$nombre, c("ENCABEZADO UNO SA", "ENCABEZADO DOS SA"))
  expect_identical(r$filas$no_cuenta, c(C$enc1, C$enc2))             # "Núm. de cuenta"
  expect_identical(r$filas$rfc, c("EUN010101AA1", "EDO020202BB2"))   # "R.F.C."
  expect_setequal(r$activos$no_cuenta, c(C$enc1, C$enc2))
})

test_that("04 a format A file (no header) is read as format A even if column 5 is dirty", {
  r <- quiet(prov_importar(fx("04_formato_a.xlsx")))
  expect_equal(nrow(r$filas), 3L)
  expect_identical(r$filas$nombre, c("FORMATO A UNO", "FORMATO A DOS", "FORMATO A TRES"))
  expect_identical(r$filas$no_cuenta, c(C$fa1, C$fa2, C$fa3))
})

test_that("05 an account Excel kept as a number is never saved altered", {
  r <- quiet(prov_importar(fx("05_clabe_numero.xlsx")))
  guardadas <- r$activos$no_cuenta %||% character(0)
  # Whatever is saved must be a real account of the file, digit for digit
  # (the 0 put back only if the check digit proves it).
  expect_true(all(guardadas %in% c(C$stp, C$bbva0)),
              info = paste("saved:", paste(guardadas, collapse = ", ")))
  # And what can't be proven is held back for review, not imported.
  expect_false(any(grepl("CIENTIFICO", r$activos$nombre %||% character(0))))
})

test_that("06 a bank key saved as a number (30) is BanBajío 030 and pays as BCO", {
  r <- quiet(prov_importar(fx("06_banco_numero.xlsx")))
  expect_identical(r$activos$banco, "030")
  expect_identical(r$activos$medio_pago, "BCO")
})

test_that("07 a row repeating an existing account (with a trailing space) is a conflict, never a duplicate", {
  r <- quiet(prov_importar(fx("07_clabe_existente.xlsx"), activos = rbind(aceros, fletes), decision = "conservar"))
  expect_identical(r$filas$.clasificacion, c("actualiza", "actualiza"))
  todas <- r$activos$no_cuenta %||% c(C$aceros, C$fletes)
  expect_equal(sum(trimws(todas, whitespace = "[\\s\\p{Z}﻿]") == C$aceros), 1L)
  expect_equal(sum(trimws(todas, whitespace = "[\\s\\p{Z}﻿]") == C$fletes), 1L)
})

test_that("08 an existing provider with its account only in `clabe` is a conflict, never a duplicate", {
  legado <- prov_fila("p-legado", "LEGADO SA", NA_character_, banco = "036", clabe = C$legado)
  # With more than one provider, as in any real catalog: `no_cuenta %||% clabe`
  # only falls back to clabe for a one-row table.
  r <- quiet(prov_importar(fx("08_clabe_legado.xlsx"), activos = rbind(aceros, legado), decision = "conservar"))
  expect_identical(r$filas$.clasificacion, "actualiza")
  guardado <- r$activos %||% rbind(aceros, legado)
  expect_equal(sum(dplyr::coalesce(guardado$no_cuenta, guardado$clabe) == C$legado, na.rm = TRUE), 1L)
})

test_that("09 Reemplazar on an inactive provider never leaves it in both lists", {
  inactivo <- prov_fila("p-inac", "INACTIVO SA", C$inactivo, banco = "044", activo = FALSE)
  r <- quiet(prov_importar(fx("09_reemplazo_inactivo.xlsx"), inactivos = inactivo, decision = "reemplazar"))
  expect_identical(r$filas$.clasificacion, "reactiva")
  # What each list holds after Importar (a list that wasn't saved is as it was;
  # a saved empty list is empty -- not "unsaved").
  activos_d   <- if (is.null(r$activos))   inactivo[0, ] else r$activos
  inactivos_d <- if (is.null(r$inactivos)) inactivo     else r$inactivos
  en_activos   <- sum(activos_d$no_cuenta   == C$inactivo, na.rm = TRUE)
  en_inactivos <- sum(inactivos_d$no_cuenta == C$inactivo, na.rm = TRUE)
  expect_equal(en_activos + en_inactivos, 1L)
  # Reactivated in place: the same provider (id), active again, with the new name.
  fila <- de(activos_d, C$inactivo)
  expect_identical(fila$id, "p-inac")
  expect_true(isTRUE(fila$activo))
  expect_identical(fila$nombre, "INACTIVO REGRESA SA")
})
informe$end_file()
})

res <- as.data.frame(lista$get_results())
cat("
=== por caso ===
")
for (i in seq_len(nrow(res)))
  cat(sprintf(" %s  %s  (%d de %d revisiones fallan)
", if (res$failed[i] > 0 || res$error[i]) "FALLA" else "PASA ",
              res$test[i], res$failed[i], res$nb[i]))
cat(sprintf("
=== results: %d casos pasan, %d fallan ===
", sum(res$failed == 0 & !res$error), sum(res$failed > 0 | res$error)))
quit(status = if (any(res$failed > 0 | res$error)) 1L else 0L)
