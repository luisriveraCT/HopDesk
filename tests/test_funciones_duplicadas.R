# A top-level function defined in two files of R/ is silently replaced by
# whichever file global.R / app.R loads last. That is how the Proveedores
# catalog took the app down on 2026-11-03: pagar_hoy_module.R redefined
# .schema_proveedores() with 9 of its columns, loaded after persistence.R,
# and every caller got the short schema. Each name lives in one file.
# KNOWN: duplicates that exist today, as a ratchet that only goes down.
# Run from the repo root:  Rscript tests/test_funciones_duplicadas.R
library(testthat)

# forecasting_service.R was replaced by forecasting_resolve.R and the app no
# longer sources it, but eight Pasivos tests still do; its five functions are
# known duplicates until those tests move to forecasting_resolve.R.
KNOWN <- c("forecasting_freeze_metric", "forecasting_get_estimate", "forecasting_list_metrics",
           "forecasting_set_estimate", "forecasting_unfreeze_metric")

definiciones <- function(f) {
  ex <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
  if (is.null(ex)) return(character())
  out <- character()
  for (e in as.list(ex)) {
    if (!is.call(e) || length(e) != 3L) next
    op <- e[[1]]
    if (!(identical(op, as.name("<-")) || identical(op, as.name("=")))) next
    if (!is.name(e[[2]])) next
    rhs <- e[[3]]
    if (is.call(rhs) && identical(rhs[[1]], as.name("function"))) out <- c(out, deparse(e[[2]]))
  }
  out
}

archivos <- list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
defs <- do.call(rbind, lapply(archivos, function(f) {
  n <- definiciones(f)
  if (length(n)) data.frame(archivo = f, nombre = n, stringsAsFactors = FALSE) else NULL
}))
repetidas <- sort(unique(defs$nombre[duplicated(defs$nombre)]))

if (!interactive() && identical(Sys.getenv("LISTAR"), "1"))
  for (n in repetidas) cat(n, ":", paste(defs$archivo[defs$nombre == n], collapse = ", "), "\n")

test_that("ninguna función global se define en dos archivos de R/ (salvo las conocidas, que solo bajan)", {
  nuevas <- setdiff(repetidas, KNOWN)
  for (n in nuevas) message("  duplicada: ", n, " en ", paste(defs$archivo[defs$nombre == n], collapse = ", "))
  expect_length(nuevas, 0)
  # A name that is no longer duplicated must leave KNOWN (the ratchet goes down).
  expect_length(setdiff(KNOWN, repetidas), 0)
})

test_that(".schema_proveedores() vive solo en persistence.R", {
  expect_identical(defs$archivo[defs$nombre == ".schema_proveedores"], "R/persistence.R")
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
  expect_identical(env$.cf_fmt(1234567), "$1,234,567")
  expect_identical(env$.cf_fmt(0), "$0")
})
