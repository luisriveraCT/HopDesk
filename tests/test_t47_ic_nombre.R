# =============================================================================
# tests/test_t47_ic_nombre.R
# T47: apply_ic_filter()'s name fallback (R/data_pipeline.R), the one that runs
# when a client has no intercompany registry (no CardCodes, no RFCs). Today it
# compares the counterpart, upper-cased, with the companies' SHORT names
# exactly, so the ERP's legal name ("NETWORKS LOGISTICS SA DE CV") is never
# taken as intercompany: measured in dev 2026-11-17, 372 of 645 AP rows between
# the group's own companies passed as third-party flow (and into the weekly
# emails as real payments).
#
# What it must do, and must not:
#   1. the legal name with accents, punctuation and "S.A. de C.V." is IC;
#   2. a third party whose name only CONTAINS the company's is not (no fuzzy
#      match: "NETWORKS LOGISTICS INTERNATIONAL" is someone else);
#   3. the exact short name keeps working (it does today);
#   4. a registry with CardCodes or RFCs is untouched (the fallback never runs).
# PASO 0 (no product code yet): 1 FAILS today; 2-4 pass.
#
# Run from project root:  Rscript tests/test_t47_ic_nombre.R
# =============================================================================
cat("=== test_t47_ic_nombre ===\n\n")
suppressPackageStartupMessages({ library(dplyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% c("strip_accents"))
    eval(e, globalenv())
source("R/monedas.R")
normalize_code <- eval(parse(text = grep("^normalize_code <- ", readLines("R/persistence.R"), value = TRUE)))   # R/persistence.R
suppressMessages(source("R/data_pipeline.R"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CMAP <- c(NL = "Networks Logistics", NTS = "Networks Transportes", NCS = "Networks Crossdocking Services")
df <- data.frame(
  Parte = c("NETWORKS LOGISTICS SA DE CV", "Networks Transportes, S.A. de C.V.", "NETWORKS LOGISTICS",
            "NETWORKS LOGISTICS INTERNATIONAL SA DE CV", "Aceros del Bajío SA de CV"),
  `Código de proveedor` = c("P1", "P2", "P3", "P4", "P5"), RFC = NA_character_,
  check.names = FALSE, stringsAsFactors = FALSE)
fuera <- function(d, ...) suppressMessages(apply_ic_filter(d, mode = "exclude", code_col = "Código de proveedor",
                                                           ic_codes = character(), cmap = CMAP, ...))$Parte
queda <- fuera(df)

ok("1: la razón social del ERP ('NETWORKS LOGISTICS SA DE CV') es intercompañía",
   !"NETWORKS LOGISTICS SA DE CV" %in% queda)
ok("1b: con acentos, comas y 'S.A. de C.V.' también ('Networks Transportes, S.A. de C.V.')",
   !"Networks Transportes, S.A. de C.V." %in% queda)
ok("2: un tercero cuyo nombre solo CONTIENE el de la empresa no es IC (sin coincidencias aproximadas)",
   "NETWORKS LOGISTICS INTERNATIONAL SA DE CV" %in% queda && "Aceros del Bajío SA de CV" %in% queda)
ok("3: el nombre corto exacto sigue siendo IC (como hoy)", !"NETWORKS LOGISTICS" %in% queda)
ok("4: con CardCodes registrados el respaldo por nombre no corre (P5 es IC por código; la razón social sola no decide)", {
  q <- suppressMessages(apply_ic_filter(df, mode = "exclude", code_col = "Código de proveedor", ic_codes = "P5", cmap = CMAP))$Parte
  !"Aceros del Bajío SA de CV" %in% q && "NETWORKS LOGISTICS SA DE CV" %in% q
})

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
