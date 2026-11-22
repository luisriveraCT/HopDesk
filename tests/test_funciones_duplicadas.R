# A top-level object defined in two files of R/ (or in R/ and app.R) is
# silently replaced by whichever file global.R / app.R loads last. That is
# how the Proveedores catalog took the app down on 2026-11-03:
# pagar_hoy_module.R redefined .schema_proveedores() with 9 of its columns,
# loaded after persistence.R, and every caller got the short schema. It is
# not only functions: on 2026-11-22 .TIPO_LABELS, a plain vector, was defined
# in bancos_module.R and pagos_audit_viewer_module.R; the payment files'
# viewer, loaded later, gave Bancos its labels (prod bug, fixed in
# tema/tipo-labels). Each name -- function or value -- lives in one file.
# KNOWN: duplicates that exist today, as a ratchet that only goes down.
# Run from the repo root:  Rscript tests/test_funciones_duplicadas.R
#   LISTAR=1 prints every duplicate with its files.
library(testthat)

# None left. The last five functions (forecasting_get_estimate & co.) went
# with pron-duplicadas, 2026-11-10; .TIPO_LABELS with tema/tipo-labels.
KNOWN <- character(0)

# Every top-level `name <- ...` / `name = ...` of a file: functions and
# values alike (a constant, a list, a vector).
definiciones <- function(f) {
  ex <- tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(ex)) return(data.frame(nombre = character(), tipo = character()))
  out <- list()
  for (e in as.list(ex)) {
    if (!is.call(e) || length(e) != 3L) next
    op <- e[[1]]
    if (!(identical(op, as.name("<-")) || identical(op, as.name("=")))) next
    if (!is.name(e[[2]])) next
    rhs <- e[[3]]
    tipo <- if (is.call(rhs) && identical(rhs[[1]], as.name("function"))) "funcion" else "valor"
    out[[length(out) + 1L]] <- data.frame(nombre = deparse(e[[2]]), tipo = tipo, stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out) else data.frame(nombre = character(), tipo = character())
}

tabla_de <- function(archivos) do.call(rbind, lapply(archivos, function(f) {
  d <- definiciones(f)
  if (nrow(d)) cbind(archivo = f, d, stringsAsFactors = FALSE) else NULL
}))
archivos <- c(list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), "app.R")
defs <- tabla_de(archivos)
# A name assigned in two DIFFERENT files (twice in one file is that file's
# own business: one load order).
repetidos_en <- function(d) sort(names(which(tapply(d$archivo, d$nombre, function(a) length(unique(a))) > 1L)))
repetidas <- repetidos_en(defs)

if (!interactive() && identical(Sys.getenv("LISTAR"), "1"))
  for (n in repetidas) cat(n, ":", paste(sprintf("%s (%s)", defs$archivo[defs$nombre == n], defs$tipo[defs$nombre == n]),
                                         collapse = ", "), "\n")

test_that("ningún objeto global (función o valor) se define en dos archivos de R/ o app.R (salvo los conocidos, que solo bajan)", {
  nuevas <- setdiff(repetidas, KNOWN)
  for (n in nuevas) message("  duplicada: ", n, " en ",
                            paste(sprintf("%s (%s)", defs$archivo[defs$nombre == n], defs$tipo[defs$nombre == n]), collapse = ", "))
  expect_length(nuevas, 0)
  # A name that is no longer duplicated must leave KNOWN (the ratchet goes down).
  expect_length(setdiff(KNOWN, repetidas), 0)
})

test_that("la guardiana no está ciega: ve un valor duplicado, una función contra un valor, y no un nombre repetido en un solo archivo", {
  d <- tempfile("dup"); dir.create(d)
  writeLines(c('.ETIQUETAS <- c(a = "A")', 'f_sola <- function() 1', 'f_sola <- function() 2'), file.path(d, "uno.R"))
  writeLines(c('.ETIQUETAS <- c(b = "B")', 'cruzada <- 3'), file.path(d, "dos.R"))
  writeLines('cruzada <- function() 4', file.path(d, "tres.R"))
  p <- repetidos_en(tabla_de(file.path(d, c("uno.R", "dos.R", "tres.R"))))
  unlink(d, recursive = TRUE)
  expect_identical(p, c(".ETIQUETAS", "cruzada"))
})

test_that(".schema_proveedores() vive solo en persistence.R", {
  expect_identical(defs$archivo[defs$nombre == ".schema_proveedores"], "R/persistence.R")
})

test_that(".TIPO_LABELS (las etiquetas de tipo de Bancos) vive solo en bancos_module.R", {
  expect_identical(defs$archivo[defs$nombre == ".TIPO_LABELS"], "R/bancos_module.R")
})

# The second case found with this test: the Flujo de caja preview redefined
# the export's .cf_fmt(x, prefix) as a compact K/M formatter, and being loaded
# later (app.R) it made the exported PDF/HTML show "$1.2M" and blank zeros.
test_that("el exportador del Flujo formatea con su propia .cf_fmt (cifra completa)", {
  expect_identical(defs$archivo[defs$nombre == ".cf_fmt"], "R/cashflow_export_module.R")
  expect_false(".cf_fmt_corto" %in% defs$nombre)   # T17c: the preview paints with the engine now
  ex <- parse("R/cashflow_export_module.R", keep.source = FALSE)
  env <- new.env()
  for (e in as.list(ex))
    if (is.call(e) && length(e) == 3L && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".cf_fmt")))
      eval(e, env)
  # [CAMBIA T55b] monto_moneda()'s format in whole units, no "$": still the
  # full figure (never "1.2M") and a visible zero (never blank).
  for (e in as.list(parse("R/monedas.R", keep.source = FALSE))) eval(e, env)
  expect_identical(env$.cf_fmt(1234567), "1,234,567")
  expect_identical(env$.cf_fmt(0), "0")
  expect_identical(env$.cf_fmt(-1234567.6, "MXN", codigo = TRUE), paste0(intToUtf8(0x2212L), "1,234,568 MXN"))
})
