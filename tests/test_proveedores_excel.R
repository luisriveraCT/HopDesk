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
  expect_true(prov_tiene_encabezado(c("Beneficiario", "CLABE", "Banco")))
  expect_false(prov_tiene_encabezado(c("Beneficiario", "CLABE")))   # T08b: 3 or more, as the header search
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
  expect_identical(tr("tablas.proveedores.importar.motivo_banco_clabe", nombre_clabe = "BBVA", clabe = "012", banco = "002"),
                   "La CLABE es de BBVA (012) y el archivo dice «002».")
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
  # Cuenta sin 0 inicial (2026-11-11): a BanBajío account of 11 digits that
  # Excel had as a number gets its 0 back, to 12 -- it used to import as
  # "12345678901" with only a warning. tests/test_proveedores_cuenta_bb.R.
  expect_identical(r$filas$.nota, "tablas.proveedores.importar.motivo_cero_bb")
  expect_identical(c(r$activos$no_cuenta, r$activos$medio_pago), c("012345678901", "BCO"))
})

# ── T08b: BanBajío's real export ("Cuentas Destino Registradas") ────────────
BAJIO <- "tests/fixtures/proveedores_excel/10_bajio_export.xlsx"

test_that("T08b: the header is found on row 6; general data above and the final note are ignored", {
  r <- prov_leer_excel(BAJIO)
  expect_identical(c(r$formato, as.character(r$fila_encabezado), as.character(r$ignoradas_arriba), as.character(r$ignoradas_nota)),
                   c("B", "6", "5", "1"))
  expect_equal(nrow(r$filas), 451L)
  expect_identical(r$columnas$nombre, "Nombre Beneficiario")      # wins over Alias
  expect_identical(r$columnas$alias, "Alias")
  expect_identical(r$columnas$no_cuenta, "Número")
  expect_identical(r$columnas$moneda, "Divisa")
  expect_identical(r$columnas$tipo_relacion, "Tipo de Relación")
  expect_true(all(c("Otro Tipo de Relación", "Importe Máximo a Transferir", "Canal") %in% r$no_usadas))
  expect_length(r$faltan, 0L)
})

test_that("T08b: every bank of the export is recognized; the bank comes out of the CLABE", {
  f <- prov_clasificar(prov_leer_excel(BAJIO)$filas, NULL, NULL)
  expect_equal(sum(f$.motivo %in% "tablas.proveedores.importar.motivo_banco"), 0L)   # 0 "no se reconoce"
  expect_identical(as.vector(table(f$.estado)[c("listo", "revisar")]), c(450L, 1L))
  expect_identical(f$.motivo[f$.estado == "revisar"], "tablas.proveedores.importar.motivo_banco_clabe")
  expect_identical(f$.banco_archivo[f$.estado == "revisar"], "SANTANDER")
  expect_identical(f$.banco_clabe[f$.estado == "revisar"], "012")
  bb <- f$tipo_cuenta %in% "Cuenta BB"
  expect_equal(sum(bb), 30L)
  expect_true(all(f$banco[bb] == "030"))                           # "Cuenta BB" = BanBajío
  expect_identical(as.vector(table(f$moneda)[c("MXN", "USD")]), c(426L, 25L))   # from Divisa
  expect_false(any(grepl("^Nota", f$nombre)))
})

test_that("T08b: bank names, as people write them, to their key (checked against real CLABE prefixes)", {
  nombres <- c("BBVA MEXICO", "BANAMEX", "BANORTE", "BAJIO", "SANTANDER", "HSBC", "SCOTIABANK", "AZTECA", "BANREGIO",
               "KAPITAL", "INTERCAM BANCO", "INBURSA", "STP", "AFIRME", "BANK OF AMERICA", "BMONEX", "CBM BANCO",
               "ASP INTEGRA OPC", "BANCOPPEL", "CITI MEXICO", "MIFEL", "NUBANK", "SABADELL")
  claves  <- c("012", "002", "072", "030", "014", "021", "044", "127", "058", "128", "136", "036", "646", "062", "106",
               "112", "124", "659", "137", "124", "042", "638", "156")
  expect_identical(prov_banco_por_nombre(nombres), claves)
  expect_identical(prov_banco_por_nombre(c("BANCO AZTECA, S.A., INSTITUCION DE BANCA MULTIPLE", "Banco del Bajío",
                                           "BBVA Bancomer, S.A.", "CITIBANAMEX", "Nu México", "Banco Patito")),
                   c("127", "030", "012", "002", "638", NA))
  expect_identical(prov_banco_nombre(c("012", "128", NA)), c("BBVA", "KAPITAL", NA))
})

test_that("T08b: an unrecognized name takes the CLABE's bank (with a note); a recognized different one waits", {
  filas <- data.frame(.row_num = 2:5, nombre = c("A SA", "B SA", "C SA", "D SA"),
                      banco = c("BANCO RARO", "SANTANDER", NA, "SANTANDER"),
                      no_cuenta = c(con_dv("01218000111111111"), con_dv("01218000222222222"), con_dv("07218000333333333"), "012345678901"),
                      tipo_cuenta = c("CLABE", "CLABE", "CLABE", "Cuenta BB"),
                      .num_no_cuenta = FALSE, .num_banco = FALSE, stringsAsFactors = FALSE)
  for (k in setdiff(names(PROV_EXCEL_CAMPOS), names(filas))) filas[[k]] <- NA_character_
  f <- prov_clasificar(filas, NULL, NULL)
  expect_identical(f$.estado, c("listo", "revisar", "listo", "revisar"))
  expect_identical(f$banco[c(1, 3)], c("012", "072"))
  expect_identical(f$.nota[c(1, 3)], rep("tablas.proveedores.importar.nota_banco_clabe", 2))
  expect_identical(f$.motivo[c(2, 4)], c("tablas.proveedores.importar.motivo_banco_clabe",
                                          "tablas.proveedores.importar.motivo_banco_bb"))
  expect_identical(tr("tablas.proveedores.importar.motivo_banco_clabe", nombre_clabe = "BBVA", clabe = "012", banco = "SANTANDER"),
                   "La CLABE es de BBVA (012) y el archivo dice «SANTANDER».")
})

test_that("T08b: account type in words or BanBajío's code is the same thing (no false change)", {
  expect_identical(prov_tipo_cuenta(c("40", "CLABE", "1", "Cuenta BB", "Tarjeta de debito", NA)),
                   c("CLABE", "CLABE", "Cuenta BB", "Cuenta BB", "Tarjeta de Débito", NA))
  ex <- prov_fila("p-1", "A SA", con_dv("01218000111111111")); ex$tipo_cuenta <- "40"
  filas <- data.frame(.row_num = 2L, nombre = "A SA", banco = "BBVA MEXICO", no_cuenta = ex$no_cuenta, tipo_cuenta = "CLABE",
                      moneda = "MXN", .num_no_cuenta = FALSE, .num_banco = FALSE, stringsAsFactors = FALSE)
  for (k in setdiff(names(PROV_EXCEL_CAMPOS), names(filas))) filas[[k]] <- NA_character_
  expect_identical(prov_clasificar(filas, ex, NULL)$.estado, "sin_cambios")
})

test_that("T08b: only accounts registered since a date are classified; the rest are out of range", {
  expect_identical(prov_fecha(c("45785", "08/05/2025", "2025-05-08", NA, "x")), as.Date(c("2025-05-08", "2025-05-08", "2025-05-08", NA, NA)))
  r <- prov_leer_excel(BAJIO)
  desde <- as.Date(44924 + 300 * 3, origin = "1899-12-30")          # the fixture registers one every 3 days
  f <- prov_clasificar(r$filas, NULL, NULL, desde = desde)
  expect_equal(sum(f$.estado == "fuera_rango"), 299L)
  expect_equal(sum(f$.estado != "fuera_rango"), 152L)
  expect_true(all(prov_fecha(f$fecha_registro[f$.estado != "fuera_rango"]) >= desde))
})

test_that("T08b: an Estatus that isn't Activo sends the row to Inactivos", {
  ex <- prov_fila("p-1", "A SA", con_dv("01218000111111111"))
  filas <- data.frame(.row_num = 2:3, nombre = c("A SA", "NUEVO SA"), banco = c("012", "072"),
                      no_cuenta = c(ex$no_cuenta, con_dv("07218000999999999")), estatus = c("Inactivo", "Bloqueado"),
                      moneda = "MXN", .num_no_cuenta = FALSE, .num_banco = FALSE, stringsAsFactors = FALSE)
  for (k in setdiff(names(PROV_EXCEL_CAMPOS), names(filas))) filas[[k]] <- NA_character_
  f <- prov_clasificar(filas, ex, NULL)
  expect_identical(f$.estado, c("inactiva", "listo"))
  res <- prov_aplicar(f, ex, ex[0, ], decisiones = setNames(list("reemplazar"), ex$no_cuenta),
                      opciones = list(nuevo_id = function() "p-nuevo"))
  expect_equal(nrow(res$activos), 0L)
  expect_setequal(res$inactivos$id, c("p-1", "p-nuevo"))
  expect_false(any(res$inactivos$activo))
})

test_that("T08b speed: 500 rows against a 900-provider catalog classify in under 2 s", {
  r <- prov_leer_excel(BAJIO)
  filas <- r$filas[rep(seq_len(nrow(r$filas)), length.out = 500), ]
  filas$.row_num <- seq_len(500)
  # A catalog of 900 where the file's accounts exist (the update path, the slow one).
  base <- prov_clasificar(r$filas, NULL, NULL)
  cat900 <- do.call(rbind, lapply(seq_len(900), function(i) {
    j <- (i - 1) %% nrow(base) + 1
    prov_fila(paste0("p-", i), paste("VIEJO", i), if (i <= nrow(base)) base$no_cuenta[j] else con_dv(sprintf("01218%012d", i)),
              banco = base$banco[j], moneda = NA)
  }))
  t <- system.time(f <- prov_clasificar(filas, cat900, NULL))[["elapsed"]]
  cat(sprintf("  500 filas contra 900 proveedores: %.2f s\n", t))
  expect_lt(t, 2)
  expect_equal(nrow(f), 500L)
})
