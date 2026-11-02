# =============================================================================
# tests/test_proveedores_excel.R
# T08 paso 1: the pure functions of R/proveedores_excel.R, and the import end
# to end through the real proveedoresServer (testServer, no S3) for what only
# shows there: the currency selector, the alias with linked Partes and the
# template's round trip.
#
# Run from project root:  Rscript tests/test_proveedores_excel.R
# =============================================================================
suppressPackageStartupMessages(library(testthat))
source("tests/helpers/proveedores_testserver.R")
quiet <- function(expr) suppressMessages(expr)
CAT <- .BANCOS_MX_CHOICES

# A valid CLABE from its first 17 digits -- written here independently of
# prov_clabe_valida(), so the test doesn't check the code against itself.
con_dv <- function(base17) {
  pesos <- c(3, 7, 1)
  s <- 0
  for (k in 1:17) s <- s + (as.integer(substr(base17, k, k)) * pesos[(k - 1) %% 3 + 1]) %% 10
  paste0(base17, (10 - s %% 10) %% 10)
}

test_that("check digit: published CLABEs validate", {
  # Banxico's documentation example, a Banamex example and an STP example.
  expect_true(all(prov_clabe_valida(c("032180000118359719", "002010077777777771", "646180157000000004"))))
})

test_that("check digit: one CLABE per common bank validates, and every single-digit change is rejected", {
  bancos <- c("002", "012", "014", "021", "030", "036", "044", "072", "137", "646")
  clabes <- vapply(bancos, function(b) con_dv(paste0(b, "180", sprintf("%011d", as.integer(b) * 7919L))), "")
  expect_true(all(prov_clabe_valida(clabes)))
  for (cl in clabes) for (pos in 1:18) {
    d <- as.integer(substr(cl, pos, pos))
    alterada <- paste0(substr(cl, 1, pos - 1), (d + 1L) %% 10L, substr(cl, pos + 1, 18))
    expect_false(prov_clabe_valida(alterada), info = paste(cl, "pos", pos))
  }
  expect_false(any(prov_clabe_valida(c("12345", NA, "01218000123456789X", ""))))
})

test_that("cells: every Unicode space goes, and the empty words are empty", {
  expect_identical(prov_limpiar_texto(c(" ACME ", "﻿X​", " A    B ", "N/A", "-", "na", "")),
                   c("ACME", "X", "A B", NA, NA, NA, NA))
  expect_identical(prov_limpiar_campo("no_cuenta", "0121-8000 1111 111119 "), "012180001111111119")
  expect_identical(prov_limpiar_campo("rfc", " amo 010101 aa1 "), "AMO010101AA1")
  expect_identical(prov_limpiar_campo("correo", " Pagos@Aceros.MX , otro@x.mx "), "pagos@aceros.mx,otro@x.mx")
})

test_that("headers: accents, dots and spaces are normalized; specific before generic", {
  m <- prov_mapear_columnas(c(" Nombre del Beneficiario", "Nombre del banco", "Tipo de cuenta", "Núm. de Cuenta",
                              "R.F.C.", "Correo electrónico", "Divisa", "Tipo de relación"))
  expect_identical(m$nombre, " Nombre del Beneficiario")
  expect_identical(m$banco, "Nombre del banco")
  expect_identical(m$tipo_cuenta, "Tipo de cuenta")
  expect_identical(m$no_cuenta, "Núm. de Cuenta")
  expect_identical(m$rfc, "R.F.C.")
  expect_identical(m$correo, "Correo electrónico")
  expect_identical(m$moneda, "Divisa")
  expect_identical(m$tipo_relacion, "Tipo de relación")
  expect_null(prov_mapear_columnas(c("Nombre", "Banco"))$no_cuenta)
})

test_that("format A/B is decided by the headers, not by one cell", {
  expect_true(prov_tiene_encabezado(c("Nombre", "Alias", "Banco", "No. Cuenta")))
  expect_true(prov_tiene_encabezado(c("Beneficiario", "CLABE")))
  # A BanBajío data row: a bank name, a dirty account -- not a header.
  expect_false(prov_tiene_encabezado(c("PROVEEDOR XYZ", "XYZ", "127-BANCO AZTECA", "40", "012180007777777771 ")))
})

test_that("accounts: what is kept, what is repaired, what waits for review", {
  ok <- con_dv("01218000111111111")
  expect_identical(prov_cuenta(ok)$estado, "ok")
  expect_identical(prov_cuenta(paste0(substr(ok, 1, 17), (as.integer(substr(ok, 18, 18)) + 1) %% 10))$motivo,
                   "tablas.proveedores.importar.motivo_dv")
  # 17 digits typed without the 0: put back only because the check digit proves it.
  r <- prov_cuenta(substr(ok, 2, 18))
  expect_identical(c(r$valor, r$estado), c(ok, "reparada"))
  # A Banamex 002 CLABE kept as a number: 16 digits, exact as a double -> repaired.
  banamex <- con_dv("00218000555555555")
  r <- prov_cuenta(sprintf("%.0f", as.numeric(banamex)), es_numero = TRUE)
  expect_identical(c(r$valor, r$estado), c(banamex, "reparada"))
  # Above 2^53 Excel already rounded: never trusted, even if it looks right.
  expect_identical(prov_cuenta("646180110400000000", es_numero = TRUE)$estado, "revisar")
  expect_identical(prov_cuenta(con_dv("64618011040000000"), es_numero = TRUE)$estado, "revisar")
  expect_identical(prov_cuenta("1.2E+17")$motivo, "tablas.proveedores.importar.motivo_numero")
  expect_identical(prov_cuenta("12180001234567900")$estado, "revisar")     # 17 digits that don't prove out
  # Not CLABEs: only cleaned.
  expect_identical(prov_cuenta("0123 4567 8901")$valor, "012345678901")      # BanBajío, 12
  expect_identical(prov_cuenta("4152-3133-0000-1234")$estado, "ok")          # card, 16
  expect_identical(prov_cuenta("GB29NWBK60161331926819")$estado, "ok")       # abroad (USD)
  expect_true(is.na(prov_cuenta(" ")$valor))
})

test_that("bank: always the 3-digit key, as the edit modal stores it", {
  expect_identical(prov_banco_clave("30")$valor, "030")
  expect_identical(prov_banco_clave(" 2 ")$valor, "002")
  expect_identical(prov_banco_clave("012-BBVA BANCOMER")$valor, "012")
  expect_identical(prov_banco_clave("072 - Banorte")$valor, "072")
  expect_identical(prov_banco_clave("Banorte", CAT)$valor, "072")
  expect_identical(prov_banco_clave("BanBajío", CAT)$valor, "030")
  expect_identical(prov_banco_clave("Banco Patito", CAT)$estado, "revisar")
  expect_true(is.na(prov_banco_clave("")$valor))
})

test_that("currency and payment type", {
  expect_identical(prov_moneda(c("Pesos", "M.N.", "mxn", "Dólar", "DLLS", "USD", NA, "EUR")),
                   c("MXN", "MXN", "MXN", "USD", "USD", "USD", NA, "EUR"))
  expect_identical(prov_medio_pago(c("MXN", "MXN", "USD", NA), c("030", "012", "030", "012")),
                   c("BCO", "SPI", "SPD", "SPI"))
})

# ── End to end, through the module ──────────────────────────────────────────
escribir_xlsx <- function(df) { f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(df, f); f }

test_that("no Moneda column: new providers take the selector's currency, existing ones keep theirs", {
  existente <- prov_fila("p-1", "EXISTENTE SA", con_dv("01218000101010101"), moneda = "USD")
  f <- escribir_xlsx(data.frame(Nombre = c("NUEVO SA", "EXISTENTE SA DE CV"), Banco = c("072", "012"),
                                `No. Cuenta` = c(con_dv("07218000202020202"), existente$no_cuenta), check.names = FALSE))
  r <- quiet(prov_importar(f, activos = existente, decision = "reemplazar", moneda = "USD"))
  nuevo <- r$activos[r$activos$nombre == "NUEVO SA", ]
  expect_identical(c(nuevo$moneda, nuevo$medio_pago, nuevo$status), c("USD", "SPD", "completo"))
  ex <- r$activos[r$activos$id == "p-1", ]
  expect_identical(c(ex$nombre, ex$moneda), c("EXISTENTE SA DE CV", "USD"))
  # Without choosing: MXN.
  r <- quiet(prov_importar(f, activos = existente[0, ]))
  expect_true(all(r$activos$moneda == "MXN"))
})

test_that("a Moneda cell left empty keeps the existing provider's currency", {
  existente <- prov_fila("p-1", "EXISTENTE SA", con_dv("01218000101010101"), moneda = "USD")
  f <- escribir_xlsx(data.frame(Nombre = "EXISTENTE DOS SA", Banco = "012", `No. Cuenta` = existente$no_cuenta,
                                Moneda = "", check.names = FALSE))
  r <- quiet(prov_importar(f, activos = existente, decision = "reemplazar", moneda = "MXN"))
  expect_identical(r$activos$moneda, "USD")
  expect_identical(r$activos$nombre, "EXISTENTE DOS SA")
})

test_that("an alias change that breaks Parte -> alias links is flagged and only imported with the checkbox", {
  existente <- prov_fila("p-1", "ACEROS SA", con_dv("01218000101010101"), alias = "ACEROS")
  mapa <- data.frame(Parte = c("ACEROS MTY", "ACEROS DEL NORTE"), Empresa = "NTS", alias = c("ACEROS", "aceros"),
                     linked_by = "x", linked_at = "2026-01-01", stringsAsFactors = FALSE)
  f <- escribir_xlsx(data.frame(Nombre = "ACEROS SA", Alias = "ACEROSMTY", Banco = "012",
                                `No. Cuenta` = existente$no_cuenta, check.names = FALSE))
  r <- quiet(prov_importar(f, activos = existente, decision = "reemplazar", alias_mapa = mapa))
  expect_identical(r$filas$.alias_vinculos, 2L)
  expect_null(r$activos)                                        # not imported without the checkbox
  expect_true(any(grepl("Nada que importar", r$avisos)))
  r <- quiet(prov_importar(f, activos = existente, decision = "reemplazar", alias_mapa = mapa, cambiar_alias = TRUE))
  expect_identical(r$activos$alias, "ACEROSMTY")
  expect_identical(r$activos$id, "p-1")
})

test_that("a catalog where only one row has no_cuenta: the others match by clabe, row by row", {
  a <- prov_fila("p-a", "A SA", con_dv("01218000111111111"))
  b <- prov_fila("p-b", "B SA", NA_character_, banco = "072", clabe = con_dv("07218000222222222"))
  c <- prov_fila("p-c", "C SA", NA_character_, banco = "014", clabe = con_dv("01418000333333333"))
  f <- escribir_xlsx(data.frame(Nombre = c("B SA", "C SA DE CV"), Banco = c("072", "014"),
                                `No. Cuenta` = c(b$clabe, c$clabe), check.names = FALSE))
  r <- quiet(prov_importar(f, activos = rbind(a, b, c), decision = "reemplazar"))
  expect_identical(r$filas$.estado, c("sin_cambios", "actualiza"))
  expect_equal(nrow(r$activos), 3L)
  expect_identical(r$activos$nombre[r$activos$id == "p-c"], "C SA DE CV")
})

test_that("an account already saved in scientific notation is held for review if it comes again", {
  f <- escribir_xlsx(data.frame(Nombre = "CIENTIFICO SA", Banco = "012", `No. Cuenta` = "1.26800012345E+17",
                                check.names = FALSE))
  r <- quiet(prov_importar(f))
  expect_identical(r$filas$.estado, "revisar")
  expect_null(r$activos)
})

test_that("template round trip: export -> import unchanged = 0 changes, 0 empty fields, 0 duplicates", {
  # A catalog like dev's: legacy bank text, "Pesos" and empty currency,
  # an account only in clabe, a BanBajío account, USD.
  catalogo <- rbind(
    prov_fila("p-1", "ACEROS SA", con_dv("01218000111111111"), banco = "012-BBVA BANCOMER", alias = "ACEROS",
              rfc = "AMO010101AA1", correo = "pagos@aceros.mx", tipo_relacion = "Proveedor"),
    prov_fila("p-2", "FLETES SA", NA_character_, banco = "072", alias = "FLETES", moneda = NA, clabe = con_dv("07218000222222222")),
    prov_fila("p-3", "BAJIO SA", "012345678901", banco = "030", alias = "BAJIO", moneda = "Pesos"),
    prov_fila("p-4", "SOFTWARE CORP", con_dv("00218000555555555"), banco = "002", alias = "SOFT", moneda = "USD"))
  f <- tempfile(fileext = ".xlsx")
  prov_plantilla_escribir(catalogo, f, CAT)
  # Account, RFC and bank are TEXT cells (Excel won't turn them into numbers).
  celdas <- readxl::read_excel(f, col_types = "list")
  for (h in c("No. Cuenta", "RFC", "Banco"))
    expect_true(all(vapply(celdas[[h]], function(v) is.character(v) || (length(v) == 1 && is.na(v)), logical(1))), info = h)
  expect_identical(celdas[["No. Cuenta"]][[2]], con_dv("07218000222222222"))   # from clabe
  r <- quiet(prov_importar(f, activos = catalogo, decision = "reemplazar"))
  expect_identical(r$filas$.estado, rep("sin_cambios", 4))
  expect_equal(sum(lengths(r$filas$.cambios)), 0L)
  expect_null(r$activos)                                        # nothing to write
  # No field the catalog had came back empty.
  for (campo in c("nombre", "alias", "banco", "no_cuenta"))
    expect_false(any(is.na(r$filas[[campo]])), info = campo)
  expect_identical(r$filas$rfc[1], "AMO010101AA1")
  expect_identical(r$filas$correo[1], "pagos@aceros.mx")
})

test_that("template round trip with one real edit: only that field changes", {
  catalogo <- prov_fila("p-1", "ACEROS SA", con_dv("01218000111111111"), alias = "ACEROS", correo = "a@x.mx")
  f <- tempfile(fileext = ".xlsx")
  prov_plantilla_escribir(catalogo, f, CAT)
  d <- readxl::read_excel(f, col_types = "text")
  d$Correo <- "nuevo@x.mx"
  f2 <- escribir_xlsx(as.data.frame(d))
  r <- quiet(prov_importar(f2, activos = catalogo, decision = "reemplazar"))
  expect_identical(r$filas$.estado, "actualiza")
  expect_identical(vapply(r$filas$.cambios[[1]], `[[`, "", "campo"), "correo")
  expect_identical(c(r$activos$correo, r$activos$alias, r$activos$id), c("nuevo@x.mx", "ACEROS", "p-1"))
})

# ── After T08: bank vs CLABE, and non-CLABE accounts that came as numbers ───
test_that("a CLABE's first 3 digits are its bank: when the file's bank says another, it waits for review", {
  expect_identical(prov_banco_coincide(c(con_dv("01218000111111111"), con_dv("01218000111111111"), "012345678901", NA),
                                       c("012", "002", "030", "012")),
                   c(TRUE, FALSE, TRUE, TRUE))
  f <- escribir_xlsx(data.frame(Nombre = c("BIEN SA", "MAL BANCO SA", "MAL REPARADA SA"), Banco = c("012", "002", "072"),
                                `No. Cuenta` = c(con_dv("01218000111111111"), con_dv("01218000222222222"),
                                                 substr(con_dv("01418000333333333"), 2, 18)),
                                Moneda = "MXN", check.names = FALSE))
  r <- quiet(prov_importar(f))
  expect_identical(r$filas$.estado, c("listo", "revisar", "revisar"))
  expect_identical(r$filas$.motivo[2:3], rep("tablas.proveedores.importar.motivo_banco_clabe", 2))
  expect_identical(r$activos$nombre, "BIEN SA")                     # the held-back ones never go in
  expect_identical(tr("tablas.proveedores.importar.motivo_banco_clabe", clabe = "012", banco = "002"),
                   "La CLABE es del banco 012 y el banco dice 002.")
})

test_that("a non-CLABE account that came as a number imports, with a visible warning", {
  r <- prov_cuenta("12345678901", es_numero = TRUE)                  # BanBajío, maybe without its leading 0
  expect_identical(c(r$valor, r$estado, r$motivo), c("12345678901", "aviso", "tablas.proveedores.importar.aviso_numero"))
  tarjeta <- "4152313300001234"                                      # 16 digits that don't prove out as a CLABE
  expect_false(prov_clabe_valida(paste0("00", tarjeta)))
  expect_identical(prov_cuenta(tarjeta, es_numero = TRUE)$estado, "aviso")
  expect_identical(prov_cuenta("17 dígitos que no cuadran", es_numero = FALSE)$estado, "ok")
  expect_identical(prov_cuenta("12180001234567890")$estado, "revisar")   # 17 digits: still held back
  # End to end: imported, with the note.
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "P")
  openxlsx::writeData(wb, "P", data.frame(Nombre = "BAJIO NUMERO SA", Banco = "030", `No. Cuenta` = "", Moneda = "MXN",
                                          check.names = FALSE))
  openxlsx::writeData(wb, "P", 12345678901, startCol = 3, startRow = 2)
  f <- tempfile(fileext = ".xlsx"); openxlsx::saveWorkbook(wb, f)
  r <- quiet(prov_importar(f))
  expect_identical(r$filas$.estado, "listo")
  expect_identical(r$filas$.nota, "tablas.proveedores.importar.aviso_numero")
  expect_identical(c(r$activos$no_cuenta, r$activos$medio_pago), c("12345678901", "BCO"))
})
