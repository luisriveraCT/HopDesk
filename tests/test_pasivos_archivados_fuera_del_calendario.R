# =============================================================================
# tests/test_pasivos_archivados_fuera_del_calendario.R
# pasivos_provisions_as_ledger_rows() -- what the Calendar (ledger_module.R)
# and the cash-flow export (data_pipeline.R) show for Pasivos -- kept every
# provisional provision, even of an ARCHIVED liability (estado "deleted").
# Archiving only removes future provisional provisions, so past ones stayed
# on screen as overdue payments (found 2026-10-31, CM06: Dell Leasing
# México's duplicated provisions). They are now left out; manual provisions
# (no liability) and those of active liabilities still show.
#
# Run from project root:  Rscript tests/test_pasivos_archivados_fuera_del_calendario.R
# =============================================================================
cat("=== test_pasivos_archivados_fuera_del_calendario ===\n\n")
suppressMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

env <- new.env()
for (e in parse("R/pasivos_schemas.R")) eval(e, env)
for (e in parse("R/pasivos_calendar_glue.R")) eval(e, env)
env$`%||%` <- `%||%`
env$has_capability     <- function(user, cap) TRUE
env$pasivos_pago_amount   <- function(p) p$amount_pago[1]
env$pasivos_pago_currency <- function(p) p$moneda_pago[1]

liabs <- tibble::tibble(id = c("L-activo", "L-archivado"), empresa = "NCS",
                        categoria = "financiero", subcategoria = "arrendamiento",
                        estado = c("active", "deleted"))
prov <- function(id, lid) {
  r <- env$.schema_pasivos_provision()[NA_integer_, ]
  r$id <- id; r$liability_id <- lid; r$estado <- "provisional"; r$origin <- "regular"
  r$fecha_calculada <- r$fecha_efectiva <- as.Date("2026-08-30")
  r$amount_pago <- 40001.87; r$moneda_pago <- "MXN"; r$empresa <- "NCS"; r$parte <- "Arrendadora"
  r
}
provs <- dplyr::bind_rows(prov("p-activo", "L-activo"), prov("p-archivado", "L-archivado"),
                          prov("p-manual", NA_character_))

rows <- env$pasivos_provisions_as_ledger_rows(provs, liabs, ledger = "AP")
ids  <- rows$provision_id %||% character()
ok("the provision of an active liability shows", "p-activo" %in% ids)
ok("a manual provision (no liability) shows", "p-manual" %in% ids)
ok("the provision of an ARCHIVED liability no longer shows", !"p-archivado" %in% ids)
ok("exactly 2 rows", nrow(rows) == 2L)

only_archived <- env$pasivos_provisions_as_ledger_rows(provs[2, ], liabs, ledger = "AP")
ok("only archived provisions -> no rows, same empty shape", nrow(only_archived) == 0L)

no_liabs <- env$pasivos_provisions_as_ledger_rows(provs, NULL, ledger = "AP")
ok("without the liabilities table nothing is dropped (unchanged behavior)", nrow(no_liabs) == 3L)

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
