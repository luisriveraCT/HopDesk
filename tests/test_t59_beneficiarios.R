# =============================================================================
# tests/test_t59_beneficiarios.R
# T59 (4): .audit_normalize_beneficiarios() (R/audit_log_viewer_module.R)
# reads a payment file's beneficiaries from the log to SHOW them (Bitácora,
# receipt, PDF). It used as.data.frame(), which takes a list of lists (one
# per beneficiary, the shape .log_pago_generado() hands in before any JSON
# round trip) as COLUMNS: one row, the others lost. Now dplyr::bind_rows(), as
# .audit_normalize_bancos_rows() in the same file already does: every shape
# gives one row per beneficiary. Reading only; the log is never written here.
#
# Run from project root:  Rscript tests/test_t59_beneficiarios.R
# =============================================================================
cat("=== test_t59_beneficiarios ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres) eval(e, globalenv())
cargar(Sys.getenv("T59_VISOR", "R/audit_log_viewer_module.R"), ".audit_normalize_beneficiarios")

benef <- function(alias, clabe, importe) list(alias = alias, clabe = clabe, banco = "BBVA", importe = importe, concepto = "Pago")
lista <- list(benef("ACME", "012345678901234567", 1000.5), benef("BETA", "012345678901234568", 250),
              benef("GAMA", "012345678901234569", 75.25))

d <- .audit_normalize_beneficiarios(lista)
ok("a list of lists (one per beneficiary): one row each, none lost (3 of 3)", is.data.frame(d) && nrow(d) == 3L)
ok("…each in its own row, in order, with its own amount",
   identical(d$alias, c("ACME", "BETA", "GAMA")) && isTRUE(all.equal(d$importe, c(1000.5, 250, 75.25))))
ok("…and the five columns of the view, in their order",
   identical(names(d), c("alias", "clabe", "banco", "importe", "concepto")))

json <- jsonlite::fromJSON(jsonlite::toJSON(lista, auto_unbox = TRUE), simplifyVector = TRUE)
dj <- .audit_normalize_beneficiarios(json)
ok("the shape the log really returns (through JSON, a data.frame) gives the same rows as before",
   is.data.frame(json) && nrow(dj) == 3L && identical(dj$alias, c("ACME", "BETA", "GAMA")) &&
     isTRUE(all.equal(dj$importe, c(1000.5, 250, 75.25))))

sin_concepto <- lapply(lista, function(b) b[setdiff(names(b), "concepto")])
ds <- .audit_normalize_beneficiarios(sin_concepto)
ok("a column the record lacks is filled with NA, rows kept",
   nrow(ds) == 3L && "concepto" %in% names(ds) && all(is.na(ds$concepto)))
mixta <- list(benef("ACME", "012345678901234567", 10), list(alias = "BETA", importe = 20))
dm <- .audit_normalize_beneficiarios(mixta)
ok("beneficiaries with different fields: both rows, the missing ones NA",
   nrow(dm) == 2L && identical(dm$alias, c("ACME", "BETA")) && is.na(dm$clabe[2]))
for (vacio in list(NULL, list(), data.frame()))
  ok(sprintf("nothing recorded (%s): the empty table with the five columns", if (is.null(vacio)) "NULL" else class(vacio)[1]),
     { e <- .audit_normalize_beneficiarios(vacio); is.data.frame(e) && nrow(e) == 0L && ncol(e) == 5L })
ok("a value it can't read gives the empty table, never an error",
   { e <- .audit_normalize_beneficiarios(function() 1); is.data.frame(e) && nrow(e) == 0L })

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
