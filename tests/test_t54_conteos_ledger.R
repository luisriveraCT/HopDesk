# =============================================================================
# tests/test_t54_conteos_ledger.R
# T54: Ajustes -> Aplicar políticas shows, per scope, how many documents it
# will write. That count keyed invoices by Empresa·Moneda·Documento, without
# the ledger, while the save keys them with it: an AR and an AP with the same
# number covered each other and the notice said fewer than the save wrote.
# Now the count and the save make the same selection (.politica_a_escribir()
# over .politica_nuevos()), so they cannot disagree.
#
# Run from project root:  Rscript --vanilla tests/test_t54_conteos_ledger.R
# =============================================================================
cat("=== test_t54_conteos_ledger ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
}
source("R/data_pipeline.R")       # .politica_doc_unica()
cargar("R/settings/settings_companies.R",
       c(".policy_move_doc", ".policy_move_llave", ".policy_moves_sin_fecha", ".policy_moves_doc_unica",
         ".policy_moves_cubiertos", ".policy_moves_sumar", ".policy_moves_faltan",
         ".politica_a_escribir"))

fila <- function(ledger, doc, fecha, Empresa = "NL", Moneda = "MXN")
  tibble::tibble(ledger = ledger, Empresa = Empresa, Moneda = Moneda, Documento = doc,
                 fecha_factura = as.Date(fecha), FechaVenc_Proyectada = as.Date(fecha) + 30)
# A CxC and a CxP with the same number (NL, MXN, "100"), plus a reused CxC
# number in two years ("200", the T52 case) and a CxP of its own ("300").
nuevos <- dplyr::bind_rows(
  fila("AR", "100", "2026-01-10"), fila("AP", "100", "2026-02-01"),
  fila("AR", "200", "2025-03-01"), fila("AR", "200", "2026-03-01"),
  fila("AP", "300", "2026-04-01"))
existentes <- dplyr::bind_rows(fila("AP", "100", "2026-02-01"), fila("AR", "200", "2025-03-01"))
manual     <- tibble::tibble(ledger = "AP", Empresa = "NL", Moneda = "MXN", Documento = "100",
                             FechaVenc_Proyectada = as.Date("2026-05-01"))
facturas <- nuevos[, c("ledger", "Empresa", "Moneda", "Documento", "fecha_factura")]
cuenta <- function(scope) NROW(.politica_a_escribir(scope, nuevos, existentes, manual, facturas))

cat("-- the count is what is written --\n")
ok("new: the CxC 100 counts although the CxP 100 has its row; the 2026 CxC 200 too (3)",
   cuenta("new") == 3L &&
     setequal(paste(.politica_a_escribir("new", nuevos, existentes, manual, facturas)$ledger,
                    .politica_a_escribir("new", nuevos, existentes, manual, facturas)$Documento),
              c("AR 100", "AR 200", "AP 300")))
ok("new: exactly the rows .policy_moves_sumar() adds",
   cuenta("new") == nrow(.policy_moves_sumar(existentes, nuevos, facturas)) - nrow(existentes))
ok("skip_manual: only the CxP 100 has a date moved by hand; the CxC 100 still counts (4)",
   cuenta("skip_manual") == 4L &&
     !any(.politica_a_escribir("skip_manual", nuevos, existentes, manual)$ledger == "AP" &
            .politica_a_escribir("skip_manual", nuevos, existentes, manual)$Documento == "100"))
ok("all: every computed row (5)", cuenta("all") == 5L)
ok("nothing computed: 0 in every scope",
   all(vapply(c("new", "skip_manual", "all"),
              function(s) NROW(.politica_a_escribir(s, nuevos[0, ], existentes, manual, facturas)) == 0L, logical(1))))
ok("no previous rows, no manual dates: new = skip_manual = all",
   NROW(.politica_a_escribir("new", nuevos, NULL, NULL, facturas)) == 5L &&
     NROW(.politica_a_escribir("skip_manual", nuevos, NULL, NULL)) == 5L)

cat("\n-- the old count (Empresa·Moneda·Documento, no ledger) was short --\n")
viejo <- function(scope) {
  k  <- paste(nuevos$Empresa, nuevos$Moneda, nuevos$Documento, sep = "·")
  ex <- paste(existentes$Empresa, existentes$Moneda, existentes$Documento, sep = "·")
  mn <- paste(manual$Empresa, manual$Moneda, manual$Documento, sep = "·")
  sum(switch(scope, new = !k %in% ex, skip_manual = !k %in% mn, TRUE))
}
ok("contraprueba: the old count says 1 for new and 3 for skip_manual (the save writes 3 and 4)",
   viejo("new") == 1L && viejo("skip_manual") == 3L)

cat("\n-- the code: one selection for both --\n")
src <- paste(sub("#.*$", "", readLines("R/settings/settings_companies.R", warn = FALSE, encoding = "UTF-8")),
             collapse = "\n")
ok("the modal counts .politica_a_escribir() over .politica_nuevos()",
   grepl("NROW(.politica_a_escribir(scope, nuevos, ex_moves, manual_db, facturas))", src, fixed = TRUE) &&
     grepl("nuevos    <- .politica_nuevos(sap,", src, fixed = TRUE))
ok("the save computes with .politica_nuevos() and selects skip_manual with .politica_a_escribir()",
   grepl("new_moves <- .politica_nuevos(sap, pp, catalog, ov, user,", src, fixed = TRUE) &&
     grepl('.politica_a_escribir("skip_manual", new_moves, manual = man_db, facturas = facturas)', src, fixed = TRUE))
ok(".policy_moves_sumar() adds .policy_moves_faltan()",
   grepl("faltan <- .policy_moves_faltan(fresco, nuevos, facturas)", src, fixed = TRUE))
ok("no document key without the ledger left in the file",
   !grepl("paste\\([a-z_]+\\$Empresa, [a-z_]+\\$Moneda, [a-z_]+\\$Documento", src))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
