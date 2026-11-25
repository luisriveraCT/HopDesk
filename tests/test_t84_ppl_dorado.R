# =============================================================================
# tests/test_t84_ppl_dorado.R
# T84 B1. The BanBajío PPL file ("Pago a Proveedores"), frozen byte by byte
# for four INVENTED scenarios (tests/fixtures/t84/ppl_escenarios.R; the
# files ppl_<scenario>.txt were written by master 0bad2024's generate_ppl()
# + write_ppl(), T94 in, and reviewed field by field against the guide):
#   mxn_mixto       SPEI to two catalog banks + an internal BanBajío (BCO)
#   usd_spid        USD by SPID, with the operation's classification
#   fuera_catalogo  a 40-series bank outside the catalog ("as before", 40143)
#                   and the "no account" line (CLABE in zeros, 40000)
#   individual_002  one payment, the day's second file (consecutive 002)
# The layouts' registry (T84 B2) must write exactly these bytes.
#   1  each scenario gives exactly its frozen bytes
#   2  the records' shape: 20 / 384 / 34 chars, sequence, trailer count and total
#   3  the fields that matter per scenario (bank key, medio, divisa, classif.)
#   4  what must NOT be generated: a 9x bank outside the catalog, no origin
#   5  accents never reach the bank: pure ASCII, same record length
#   6  FINDING kept as is: every record ends in NUL + CRLF (write_ppl()'s
#      writeBin of a string adds the NUL; so since the first commit, and
#      BajíoNet takes it). Changing it changes what the bank receives: Luis's
#      call and a BajíoNet try, never a side effect of a refactor.
#   7  the fixtures are invented
#
# Run from project root:  Rscript tests/test_t84_ppl_dorado.R
# =============================================================================
cat("=== test_t84_ppl_dorado ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
source("R/ppl_generator.R")

DIR <- "tests/fixtures/t84"
esc <- eval(parse(file.path(DIR, "ppl_escenarios.R")))
bytes_de <- function(f) readBin(f, "raw", file.info(f)$size)
generar <- function(e) generate_ppl(e$pagos, e$cuenta_origen, fecha = e$fecha, consecutivo = e$consecutivo,
                                    moneda = e$moneda, clasificacion = e$clasificacion)
escribir <- function(lineas) { f <- tempfile(fileext = ".txt"); write_ppl(lineas, f); bytes_de(f) }
# The records of a file's bytes (each ends in NUL CR LF, see 6).
registros <- function(b) {
  txt <- rawToChar(b[b != as.raw(0)])
  strsplit(sub("\r\n$", "", txt), "\r\n", fixed = TRUE)[[1]]
}

cat("== 1. Los bytes congelados ==\n")
for (n in names(esc)) {
  esperado <- bytes_de(file.path(DIR, paste0("ppl_", n, ".txt")))
  hoy <- escribir(generar(esc[[n]]))
  ok(sprintf("1 %s: exactamente los %d bytes congelados", n, length(esperado)), identical(hoy, esperado),
     if (!identical(hoy, esperado)) sprintf("primer byte distinto: %s", which(hoy[seq_len(min(length(hoy), length(esperado)))] !=
                                                                        esperado[seq_len(min(length(hoy), length(esperado)))])[1]))
}

cat("== 2. La forma de los registros ==\n")
for (n in names(esc)) {
  r <- registros(bytes_de(file.path(DIR, paste0("ppl_", n, ".txt"))))
  k <- length(r); pagos <- esc[[n]]$pagos
  ok(sprintf("2 %s: encabezado 20, %d detalles de 384, pie 34", n, nrow(pagos)),
     nchar(r[1]) == 20L && all(nchar(r[2:(k - 1)]) == 384L) && nchar(r[k]) == 34L && k == nrow(pagos) + 2L)
  ok(sprintf("2 %s: tipos 01/02/09 y secuencia 1..%d", n, k),
     startsWith(r[1], "01") && all(startsWith(r[2:(k - 1)], "02")) && startsWith(r[k], "09") &&
       identical(as.integer(substr(r, 3, 9)), seq_len(k)))
  ok(sprintf("2 %s: el pie cuenta los pagos y suma sus centavos", n),
     as.integer(substr(r[k], 10, 16)) == nrow(pagos) &&
       as.numeric(substr(r[k], 17, 34)) == round(sum(pagos$importe) * 100) &&
       sum(as.numeric(substr(r[2:(k - 1)], 39, 53))) == round(sum(pagos$importe) * 100))
}

cat("== 3. Los campos de cada escenario ==\n")
det <- function(n) { r <- registros(bytes_de(file.path(DIR, paste0("ppl_", n, ".txt")))); r[2:(length(r) - 1)] }
d <- det("mxn_mixto")
ok("3.1 MXN: divisa 01, banco del catálogo por prefijo (40012, 40002) y BCO 40030",
   all(substr(d, 32, 33) == "01") && identical(substr(d, 34, 38), c("40012", "40002", "40030")))
ok("3.2 MXN: medio SPI/SPI/BCO con su prefijo de cuenta (4000 / 0100)",
   identical(substr(d, 62, 64), c("SPI", "SPI", "BCO")) && identical(substr(d, 65, 68), c("4000", "4000", "0100")))
ok("3.3 MXN: un centavo es 000000000000001 y el concepto largo se corta en 248",
   substr(d[2], 39, 53) == "000000000000001" && nchar(trimws(substr(d[3], 126, 373))) <= 248L)
ok("3.4 MXN: clasificación 00 y referencia en ceros (la calcula el banco)",
   all(substr(d, 383, 384) == "00") && all(substr(d, 376, 382) == "0000000"))
d <- det("usd_spid")
ok("3.5 USD: divisa 02, medio SPD y la clasificación pedida (03)",
   all(substr(d, 32, 33) == "02") && all(substr(d, 62, 64) == "SPD") && all(substr(d, 383, 384) == "03"))
e <- esc$usd_spid; e$clasificacion <- NULL
ok("3.6 USD sin clasificación: la de omisión (07), nunca vacía",
   all(substr(generar(e)[2:3], 383, 384) == "07"))
e$clasificacion <- "99"
ok("3.7 USD con una clasificación inventada: la de omisión, nunca la entrada cruda",
   all(substr(generar(e)[2:3], 383, 384) == "07"))
d <- det("fuera_catalogo")
ok("3.8 un banco de la serie 40 fuera del catálogo va como antes (40143)", substr(d[1], 34, 38) == "40143")
ok("3.9 la línea sin cuenta (CLABE en ceros) lleva 40000", substr(d[2], 34, 38) == "40000" &&
     substr(d[2], 69, 86) == strrep("0", 18))
r <- registros(bytes_de(file.path(DIR, "ppl_individual_002.txt")))
ok("3.10 el segundo archivo del día lleva consecutivo 002 y la fecha 20261127",
   substr(r[1], 18, 20) == "002" && substr(r[1], 10, 17) == "20261127")

cat("== 4. Lo que no se genera ==\n")
e <- esc$fuera_catalogo; e$pagos$clabe_dest[1] <- "697180001234567890"
ok("4.1 un banco 6xx fuera del catálogo: error y nada generado",
   inherits(tryCatch(generar(e), error = function(x) x), "error") && !"697" %in% names(PPL_BANCOS))
e <- esc$individual_002; e$cuenta_origen <- "000000000000"
ok("4.2 sin cuenta de origen (ceros): error y nada generado",
   inherits(tryCatch(generar(e), error = function(x) x), "error"))

cat("== 5. Acentos ==\n")
e <- esc$individual_002
e$pagos$alias <- "ÑANDÚ FICTICIO"; e$pagos$concepto <- "Pago de facturación electrónica"
b <- escribir(generar(e))
ok("5.1 con acentos el archivo sigue siendo ASCII puro", all(as.integer(b) < 128L))
ok("5.2 y cada detalle sigue midiendo 384", all(nchar(registros(b)[2]) == 384L))

cat("== 6. Hallazgo congelado: NUL antes de cada CRLF ==\n")
b <- bytes_de(file.path(DIR, "ppl_mxn_mixto.txt"))
fin <- which(b == as.raw(0x0A))
ok("6.1 cada uno de los 5 registros termina en 00 0D 0A (lo que BajíoNet recibe hoy)",
   length(fin) == 5L && all(b[fin - 1L] == as.raw(0x0D)) && all(b[fin - 2L] == as.raw(0x00)) &&
     sum(b == as.raw(0)) == 5L)

cat("== 7. Solo datos inventados ==\n")
todos <- do.call(rbind, lapply(esc, `[[`, "pagos"))
ok("7.1 cuenta de origen ficticia y todos los nombres dicen FICT", all(vapply(esc, function(x) x$cuenta_origen, "") == "012345678901") &&
     all(grepl("FICT", todos$alias)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
