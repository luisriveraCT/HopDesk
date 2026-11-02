# =============================================================================
# tests/fixtures/proveedores_excel/generar.R
# Builds the T08 Excel fixtures, one problem per file (never by hand). Every
# CLABE here is valid: 17 digits plus its check digit (weights 3, 7, 1,
# modulo 10), so a test can tell a real account from one Excel altered.
#
# Run from project root:  Rscript tests/fixtures/proveedores_excel/generar.R
# The account numbers the tests expect are in clabes_prueba() below; the test
# sources this file for them with PROV_EXCEL_SOLO_DATOS=1 (writes nothing).
# =============================================================================
suppressPackageStartupMessages(library(openxlsx))

clabe_dv <- function(base17) {
  d <- as.integer(strsplit(base17, "")[[1]])
  w <- rep(c(3L, 7L, 1L), 6)[1:17]
  paste0(base17, (10L - sum((d * w) %% 10L) %% 10L) %% 10L)
}

clabes_prueba <- function() list(
  aceros   = clabe_dv("01218000111111111"),   # BBVA; the existing provider of 01 and 07
  fletes   = clabe_dv("07218000222222222"),   # Banorte; existing provider of 07
  papel    = clabe_dv("07218000333333333"),   # new, 02
  guero    = clabe_dv("01418000444444444"),   # new, 02
  enc1     = clabe_dv("00218000555555555"),   # new, 03
  enc2     = clabe_dv("02118000666666666"),   # new, 03
  fa1      = clabe_dv("01218000777777777"),   # new, 04
  fa2      = clabe_dv("07218000888888888"),   # new, 04
  fa3      = clabe_dv("01418000999999999"),   # new, 04
  stp      = clabe_dv("64618011040000000"),   # STP, 18 digits starting with 6 -- 05
  bbva0    = clabe_dv("01218000123456789"),   # starts with 0 -- 05
  legado   = clabe_dv("03618000121212121"),   # existing provider with the account only in `clabe` -- 08
  inactivo = clabe_dv("04418000343434343")    # existing INACTIVE provider -- 09
)

generar <- function(dir = "tests/fixtures/proveedores_excel") {
  C <- clabes_prueba()
  NB <- " "; THIN <- " "; BOM <- "﻿"
  guardar <- function(df, archivo, encabezado = TRUE) {
    wb <- createWorkbook(); addWorksheet(wb, "Proveedores")
    writeData(wb, "Proveedores", df, colNames = encabezado)
    saveWorkbook(wb, file.path(dir, archivo), overwrite = TRUE)
  }

  # 01 · Reemplazar an existing provider: Alias cell empty, and the file has
  # no Correo, RFC, Moneda or Tipo de relación columns at all.
  guardar(data.frame(Nombre = "ACEROS MONTERREY SA DE CV", Alias = "", Banco = "012",
                     `No. Cuenta` = C$aceros, check.names = FALSE), "01_reemplazo.xlsx")

  # 02 · Invisible spaces: no-break space, thin space and BOM, at the end and
  # at the start of CLABE, RFC and name.
  guardar(data.frame(Nombre = c(paste0("PAPELERIA ÑANDU", THIN), paste0("FLETES EL GÜERO", NB)),
                     Alias  = c("PAPELERIA", paste0(NB, "GUERO")),
                     Banco  = c("072", "014"),
                     `No. Cuenta` = c(paste0(C$papel, NB), paste0(BOM, C$guero, " ")),
                     RFC    = c(paste0(" NAN010101AB1", NB), paste0("GUE020202CD2", THIN)),
                     Moneda = "MXN", check.names = FALSE), "02_espacios.xlsx")

  # 03 · Headers people really type: accents, punctuation, no-break spaces.
  guardar(data.frame(a = c("ENCABEZADO UNO SA", "ENCABEZADO DOS SA"), b = c("002", "021"),
                     c = c(C$enc1, C$enc2), d = c("EUN010101AA1", "EDO020202BB2"), e = "MXN")
          |> setNames(c(paste0(NB, "Nombre del beneficiario", NB), "Banco", "Núm. de cuenta", "R.F.C.", "Moneda")),
          "03_encabezados.xlsx")

  # 04 · Format A (BanBajío, no header row) whose first account has a
  # no-break space at the end.
  guardar(data.frame(n = c("FORMATO A UNO", "FORMATO A DOS", "FORMATO A TRES"), a = c("FAUNO", "FADOS", "FATRES"),
                     b = c("012-BBVA BANCOMER", "072-BANORTE / IXE", "014-SANTANDER"), t = "40",
                     c = c(paste0(C$fa1, NB), C$fa2, C$fa3), r = "", e = "", m = "", tr = ""),
          "04_formato_a.xlsx", encabezado = FALSE)

  # 05 · Accounts saved as NUMBERS in Excel: an 18-digit STP CLABE (more
  # digits than a double keeps), a CLABE starting with 0 (Excel drops it:
  # 17 digits) and one typed in scientific notation.
  guardar(data.frame(Nombre = c("NUMERO STP SA", "NUMERO BBVA SA", "NUMERO CIENTIFICO SA"),
                     Banco = c("646", "012", "012"),
                     `No. Cuenta` = c(as.numeric(C$stp), as.numeric(C$bbva0), 1.2E17),
                     Moneda = "MXN", check.names = FALSE), "05_clabe_numero.xlsx")

  # 06 · Bank key as a number: BanBajío 030 saved as 30 (a 12-digit BanBajío
  # account, which pays as BCO, not SPI).
  guardar(data.frame(Nombre = "BAJIO NUMERICO SA", Banco = 30, `No. Cuenta` = "012345678901",
                     Moneda = "MXN", check.names = FALSE), "06_banco_numero.xlsx")

  # 07 · Rows repeating EXISTING accounts, one with a plain trailing space
  # and one with a no-break space.
  guardar(data.frame(Nombre = c("ACEROS OTRA VEZ", "FLETES OTRA VEZ"), Banco = c("012", "072"),
                     `No. Cuenta` = c(paste0(C$aceros, " "), paste0(C$fletes, NB)),
                     Moneda = "MXN", check.names = FALSE), "07_clabe_existente.xlsx")

  # 08 · (found in dev: 25 providers) an existing provider whose account is
  # only in `clabe`, no_cuenta empty.
  guardar(data.frame(Nombre = "LEGADO OTRA VEZ", Banco = "036", `No. Cuenta` = C$legado,
                     Moneda = "MXN", check.names = FALSE), "08_clabe_legado.xlsx")

  # 09 · (found reading the code) Reemplazar on a conflict with an INACTIVE
  # provider while importing into Activos.
  guardar(data.frame(Nombre = "INACTIVO REGRESA SA", Banco = "044", `No. Cuenta` = C$inactivo,
                     Moneda = "MXN", check.names = FALSE), "09_reemplazo_inactivo.xlsx")

  # 10 · (T08b) BanBajío's "Cuentas Destino Registradas" export, with the
  # exact structure of the real one and invented data.
  wb <- createWorkbook(); addWorksheet(wb, "CuentasDestino_XLS")
  writeData(wb, 1, bajio_export(), colNames = FALSE)
  saveWorkbook(wb, file.path(dir, "10_bajio_export.xlsx"), overwrite = TRUE)
  invisible(TRUE)
}

# BanBajío's export as a character matrix: 5 rows of general data, the
# 25-column header on row 6, 451 accounts (all text) and a closing note.
# Banks, account types, lengths and currencies in the real file's
# proportions; one row says SANTANDER with a BBVA (012) CLABE.
BAJIO_ENCABEZADO <- c("#", "Estatus", "Alias", "Número", "Tipo Cuenta", "Banco", "Divisa", "Nombre Beneficiario",
                      "Tiempo de uso", "Usuarios que podrán utilizarla", "Importe Máximo a Transferir",
                      "Periodo del Importe Máximo a Transferir", "Suma de lo Transferido", "Tipo de Relación",
                      "Otro Tipo de Relación", "RFC", "Correo", "Fecha de Registro", "Hora de Registro",
                      "Registrada por", "Usuario", "Nombre Archivo", "Canal", "Registro", "Autorización")
bajio_export <- function() {
  # name, CLABE prefix, how many CLABEs, Cuenta BB lengths, cards
  bancos <- list(
    list("BBVA MEXICO", "012", 154, NULL, 2), list("BANAMEX", "002", 64, NULL, 0), list("BANORTE", "072", 49, NULL, 0),
    list("BAJIO", "030", 18, c(rep(12, 27), 11, 11, 13), 0), list("SANTANDER", "014", 45, NULL, 2),
    list("HSBC", "021", 22, NULL, 0), list("SCOTIABANK", "044", 9, NULL, 0), list("AZTECA", "127", 8, NULL, 0),
    list("BANREGIO", "058", 8, NULL, 0), list("KAPITAL", "128", 8, NULL, 0), list("INTERCAM BANCO", "136", 7, NULL, 0),
    list("INBURSA", "036", 6, NULL, 0), list("STP", "646", 5, NULL, 0), list("AFIRME", "062", 2, NULL, 0),
    list("BANK OF AMERICA", "106", 2, NULL, 0), list("BMONEX", "112", 2, NULL, 0), list("CBM BANCO", "124", 2, NULL, 0),
    list("ASP INTEGRA OPC", "659", 1, NULL, 0), list("BANCOPPEL", "137", 0, NULL, 1), list("CITI MEXICO", "124", 1, NULL, 0),
    list("MIFEL", "042", 1, NULL, 0), list("NUBANK", "638", 1, NULL, 0), list("SABADELL", "156", 1, NULL, 0))
  filas <- list(); k <- 0L
  fila <- function(banco, numero, tipo) {
    k <<- k + 1L
    c(as.character(k), "Activo", sprintf("PROV%03d", k), numero, tipo, banco, if (k %% 18 == 0) "USD" else "MXN",
      sprintf("PROVEEDOR DE PRUEBA %03d SA DE CV", k), "Indefinido", "Todos", "0", "", "0", "Proveedor", "",
      sprintf("PDP%06d%s", 100000 + k, c("AA1", "BB2", "CC3")[k %% 3 + 1]),
      if (k %% 2) sprintf("pagos%03d@prueba.mx,cxp%03d@prueba.mx", k, k) else sprintf("pagos%03d@prueba.mx", k),
      as.character(44924 + k * 3), "10:00:00", "USUARIO PRUEBA", "uprueba", "", "Web", "Alta", "Autorizada")
  }
  for (b in bancos) {
    for (i in seq_len(b[[3]])) filas[[length(filas) + 1L]] <- fila(b[[1]], clabe_dv(sprintf("%s180%011d", b[[2]], k * 7919L + i)), "CLABE")
    for (n in b[[4]]) filas[[length(filas) + 1L]] <- fila(b[[1]], substr(sprintf("%013.0f", 4400000000000 + k * 31), 14 - n, 13), "Cuenta BB")
    for (i in seq_len(b[[5]])) filas[[length(filas) + 1L]] <- fila(b[[1]], sprintf("4152%012d", k * 97L + i),
                                                                 if (i == 1 && b[[1]] == "BANCOPPEL") "Tarjeta de Crédito" else "Tarjeta de Débito")
  }
  datos <- do.call(rbind, filas)
  # One SANTANDER row with a BBVA CLABE: must wait for review.
  j <- which(datos[, 6] == "SANTANDER" & datos[, 5] == "CLABE")[1]
  datos[j, 4] <- clabe_dv(sprintf("012180%011d", 99999999L))
  arriba <- matrix("", 5, 25)
  arriba[1, 1] <- "Datos Generales"
  arriba[2, c(1, 3)] <- c("Nombre Cliente", "EMPRESA DE PRUEBA SA DE CV")
  arriba[3, c(1, 3, 5, 7)] <- c("Cliente Principal", "1234567", "Cuentas Destino Registradas", as.character(nrow(datos)))
  arriba[4, c(1, 3)] <- c("Generado el", "02/11/2026 08:42:53")
  arriba[5, 1] <- "Cuentas Destino"
  nota <- matrix("", 1, 25); nota[1, 1] <- "Nota: La información contenida en este archivo es exclusivamente de carácter informativo."
  out <- rbind(arriba, BAJIO_ENCABEZADO, datos, nota)
  out[out == ""] <- NA
  out
}

if (!identical(Sys.getenv("PROV_EXCEL_SOLO_DATOS"), "1")) {
  generar()
  cat("fixtures written to tests/fixtures/proveedores_excel/\n")
}
