# =============================================================================
# tests/test_t84_layouts.R
# T84 B2: the Agenda writes its payment files through the layouts' registry
# (R/pagos_layouts.R) instead of naming BanBajío's PPL.
#   1  the registry: every function it names exists; BanBajío's PPL is there
#   2  through the registry, B1's four invented scenarios come out byte for byte
#   3  layout_de_cuenta(): BanBajío's PPL for any origin account today
#   4  GUARDIAN: no file in R/ or app.R but the registry (and the generator)
#      calls generate_ppl() or write_ppl()
#   5  pagar_hoy_module.R: its three downloads go through layout_llamar()
#
# Run from project root:  Rscript tests/test_t84_layouts.R
# =============================================================================
cat("=== test_t84_layouts ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
for (f in c("R/monedas.R", "R/ppl_generator.R", "R/bancos_respuesta.R", "R/pagos_layouts.R")) suppressMessages(source(f))

DIR <- "tests/fixtures/t84"
esc <- eval(parse(file.path(DIR, "ppl_escenarios.R")))
bytes_de <- function(f) readBin(f, "raw", file.info(f)$size)

cat("== 1. El registro ==\n")
campos <- c("validar", "generar", "escribir", "moneda_ok", "medio_pago", "leer_respuesta")
ok("1.1 el PPL de BanBajío está registrado", "banbajio_ppl" %in% names(PAGOS_LAYOUTS))
ok("1.2 cada función que nombra el registro existe", all(vapply(PAGOS_LAYOUTS, function(e)
  all(vapply(e[campos], function(fn) exists(fn, mode = "function"), logical(1))), logical(1))))
ok("1.3 un layout desconocido es un error", inherits(tryCatch(layout_llamar("otro", "generar"), error = function(e) e), "error"))

cat("== 2. Los bytes de la B1, por el registro ==\n")
for (n in names(esc)) {
  e <- esc[[n]]; lay <- layout_de_cuenta(e$cuenta_origen)
  f <- tempfile(fileext = ".txt")
  layout_llamar(lay, "escribir", layout_llamar(lay, "generar", e$pagos, e$cuenta_origen, e$fecha, e$consecutivo,
                                               moneda = e$moneda, clasificacion = e$clasificacion), f)
  ok(sprintf("2 %s: exactamente los bytes congelados", n), identical(bytes_de(f), bytes_de(file.path(DIR, paste0("ppl_", n, ".txt")))))
}

cat("== 3. layout_de_cuenta() ==\n")
ok("3.1 cualquier cuenta de origen, hoy: banbajio_ppl",
   identical(layout_de_cuenta("012345678901"), "banbajio_ppl") && identical(layout_de_cuenta(NULL), "banbajio_ppl"))

cat("== 4. Guardiana: nadie llama al generador directo ==\n")
llamadas <- function(f, nombres) {
  d <- utils::getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"))
  unique(d$text[d$token == "SYMBOL_FUNCTION_CALL" & d$text %in% nombres])
}
archivos <- setdiff(c("app.R", list.files("R", "[.]R$", recursive = TRUE, full.names = TRUE)),
                    c("R/ppl_generator.R", "R/pagos_layouts.R"))
directos <- Filter(length, setNames(lapply(archivos, llamadas, c("generate_ppl", "write_ppl")), archivos))
ok("4.1 ningún archivo fuera del registro llama generate_ppl / write_ppl", !length(directos),
   paste(names(directos), collapse = ", "))

cat("== 5. La Agenda ==\n")
src <- readLines("R/pagar_hoy_module.R", warn = FALSE, encoding = "UTF-8")
ok("5.1 sus tres descargas generan y escriben por el registro",
   sum(grepl('layout_llamar[(]lay(_i)?, "generar"', src)) == 3L && sum(grepl('layout_llamar[(]lay(_i)?, "escribir"', src)) == 3L)
ok("5.2 el individual sigue siendo el consecutivo 2 y los otros el 1",
   sum(grepl('hoy(), 2L, moneda = cur', src, fixed = TRUE)) == 1L && sum(grepl('hoy(), 1L, moneda = cur', src, fixed = TRUE)) == 2L)
ok("5.3 global.R carga el registro después del generador", {
  g <- readLines("R/global.R", warn = FALSE)
  i <- grep('source("R/pagos_layouts.R")', g, fixed = TRUE)
  length(i) == 1L && i > grep('source("R/ppl_generator.R")', g, fixed = TRUE)
})

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
