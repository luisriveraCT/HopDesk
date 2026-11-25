# =============================================================================
# tests/test_t84_importadores.R
# T84 A2: Bancos' "Importar TXT" asks the importers' registry
# (R/bancos_importadores.R) instead of naming BanBajío.
#   1  the registry: every function it names exists; BanBajío's TXT is there
#   2  through the registry, A1's invented TXT gives exactly the frozen rows
#   3  leer(): the same lines as before (UTF-8, then latin1); NULL if unreadable
#   4  acepta_cuenta(): the accounts Bancos used to filter (active, "baj")
#   5  importador_de_cuenta(): BanBajío with or without accounts
#   6  GUARDIAN: no file in R/ or app.R but the registry (and the files that
#      define them) calls parse_banbajio_txt() or detectar_cuenta_banbajio()
#   7  bancos_module.R reads and parses only through importador_llamar()
#
# Run from project root:  Rscript tests/test_t84_importadores.R
# =============================================================================
cat("=== test_t84_importadores ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/bancos_parser.R", "R/empresas_identidad.R", "R/bancos_detect.R", "R/bancos_importadores.R"))
  sys.source(f, envir = globalenv())

DIR <- "tests/fixtures/t84"
TXT <- file.path(DIR, "banbajio_movimientos.txt")
esperado <- eval(parse(file.path(DIR, "banbajio_movimientos_esperado.R"), encoding = "UTF-8"))
sin_attr <- function(x) { attr(x, "numero_cuenta") <- NULL; x }

cat("== 1. El registro ==\n")
campos <- c("acepta_cuenta", "leer", "detectar", "parsear")
ok("1.1 BanBajío TXT está registrado", "banbajio_txt" %in% names(BANCOS_IMPORTADORES))
ok("1.2 cada función que nombra el registro existe", all(vapply(BANCOS_IMPORTADORES, function(e)
  all(vapply(e[campos], function(fn) exists(fn, mode = "function"), logical(1))), logical(1))))
ok("1.3 un importador desconocido es un error, no un silencio",
   inherits(tryCatch(importador_llamar("otro_banco", "parsear", "x", "c"), error = function(e) e), "error"))

cat("== 2. Las filas del dorado A1, por el registro ==\n")
lineas <- importador_llamar("banbajio_txt", "leer", TXT)
r <- importador_llamar("banbajio_txt", "parsear", lineas, "cta-ficticia-01")
ok("2.1 exactamente las 26 filas congeladas", identical(sin_attr(r), esperado))
ok("2.2 y el número de cuenta del encabezado", identical(attr(r, "numero_cuenta"), "0009876543"))

cat("== 3. leer() ==\n")
ok("3.1 las mismas líneas que readLines() en UTF-8", identical(lineas, readLines(TXT, encoding = "UTF-8", warn = FALSE)))
ok("3.2 un archivo que no existe da NULL", is.null(suppressWarnings(importador_llamar("banbajio_txt", "leer", tempfile()))))

cat("== 4. acepta_cuenta() ==\n")
cts <- tibble(cuenta_id = c("a", "b", "c", "d", "e"), activa = c(TRUE, TRUE, FALSE, NA, TRUE),
              banco = c("BanBajío", "BBVA", "BANBAJIO", "Bajio", NA))
ok("4.1 activas y de BanBajío, como antes (dplyr::filter(activa == TRUE, grepl('baj')))",
   identical(cts$cuenta_id[importador_llamar("banbajio_txt", "acepta_cuenta", cts)],
             dplyr::filter(cts, activa == TRUE, grepl("baj", banco, ignore.case = TRUE))$cuenta_id))

cat("== 5. importador_de_cuenta() ==\n")
ok("5.1 con cuentas de BanBajío: banbajio_txt", identical(importador_de_cuenta(cts), "banbajio_txt"))
ok("5.2 sin cuentas: el primero (la pantalla dice que no hay cuenta, como antes)",
   identical(importador_de_cuenta(cts[0, ]), "banbajio_txt") && identical(importador_de_cuenta(NULL), "banbajio_txt"))

cat("== 6. Guardiana: nadie llama al parser ni al detector directo ==\n")
llamadas <- function(f, nombres) {
  d <- utils::getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"))
  unique(d$text[d$token == "SYMBOL_FUNCTION_CALL" & d$text %in% nombres])
}
DEFINEN <- c("R/bancos_parser.R", "R/bancos_detect.R", "R/bancos_importadores.R")
archivos <- setdiff(c("app.R", list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE)), DEFINEN)
directos <- Filter(length, setNames(lapply(archivos, llamadas, c("parse_banbajio_txt", "detectar_cuenta_banbajio")), archivos))
ok("6.1 ningún archivo fuera del registro llama parse_banbajio_txt / detectar_cuenta_banbajio",
   !length(directos), paste(names(directos), collapse = ", "))

cat("== 7. bancos_module.R ==\n")
src <- readLines("R/bancos_module.R", warn = FALSE, encoding = "UTF-8")
ok("7.1 lee el archivo solo por el registro (ningún readLines latin1 propio)",
   !any(grepl('readLines(path, encoding = "latin1"', src, fixed = TRUE)) &&
     sum(grepl('importador_llamar(imp, "leer", path)', src, fixed = TRUE)) == 2L)
ok("7.2 parsea (2) y detecta (1) solo por el registro",
   sum(grepl('importador_llamar(imp, "parsear"', src, fixed = TRUE)) == 2L &&
     sum(grepl('importador_llamar(imp, "detectar"', src, fixed = TRUE)) == 1L)
ok("7.3 global.R carga el registro después del parser y del detector", {
  g <- readLines("R/global.R", warn = FALSE)
  i <- grep('source("R/bancos_importadores.R")', g, fixed = TRUE)
  length(i) == 1L && i > grep('source("R/bancos_detect.R")', g, fixed = TRUE) &&
    i < grep('source("R/bancos_module.R")', g, fixed = TRUE)
})

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
