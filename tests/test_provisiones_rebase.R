# =============================================================================
# tests/test_provisiones_rebase.R
# CM09 (2026-11-01): Pasivos saved the whole provisions table built from the
# session's copy (or a lenient read), so a provision another session
# created, converted or paid meanwhile was dropped or reverted. The saves now
# pass `base` and only this action's provisions are applied on S3's table as
# it is now (rebase_por_id inside modificar_tabla).
#
# Run from project root:  Rscript tests/test_provisiones_rebase.R
# =============================================================================
cat("=== test_provisiones_rebase ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressMessages({ source("R/pasivos_schemas.R"); source("R/pasivos_persistence.R") })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(expr)

PK <- paste0("acme/", S3_KEYS$pasivos_provisions)
prov <- function(ids, estado = "provisional") .normalize(tibble::tibble(id = ids, liability_id = "L1",
          estado = estado, fecha_efectiva = as.Date("2026-12-01"), amount_pago = 100), .schema_pasivos_provision)
en_s3 <- function() { x <- get(PK, envir = .mem); x[order(x$id), ] }

# 1. A soft delete in a stale session copy keeps another session's conversion
s3_memoria_reset()
assign(PK, dplyr::bind_rows(prov("p1"), prov("p2", "converted"), prov("p3")), envir = .mem)  # p2 converted elsewhere, p3 new
base <- prov(c("p1", "p2"))                                                                  # this session's copy
nuevo <- base; nuevo$estado[nuevo$id == "p1"] <- "deleted"
q(save_pasivos_provisions(nuevo, client_id = "acme", base = base))
s <- en_s3()
ok("my delete of p1 is applied", s$estado[s$id == "p1"] == "deleted")
ok("the other session's conversion of p2 is kept (not reverted to provisional)", s$estado[s$id == "p2"] == "converted")
ok("the other session's new p3 survives", "p3" %in% s$id && nrow(s) == 3L)

# 2. Pausing a liability drops its future provisions only
s3_memoria_reset(); assign(PK, prov(c("p1", "p2", "p3", "p9")), envir = .mem)                 # p9: another session
base <- prov(c("p1", "p2", "p3"))
q(save_pasivos_provisions(base[base$id == "p1", ], client_id = "acme", base = base))
ok("dropped rows leave; the other session's p9 stays", identical(en_s3()$id, c("p1", "p9")))

# 3. A failed read writes nothing
s3_memoria_reset(); assign(PK, prov(c("p1", "p2")), envir = .mem); .mem_falla <- PK
r <- tryCatch(q(save_pasivos_provisions(prov(c("p1", "p2", "p5")), client_id = "acme", base = prov(c("p1", "p2")))),
              error = function(e) e)
ok("a failed read raises and writes nothing", inherits(r, "error") && .mem_escrituras == 0L)

# 4. With base, an intended purge still passes its flag
s3_memoria_reset(); assign(PK, prov(as.character(1:10)), envir = .mem)
base <- prov(as.character(1:10))
r <- tryCatch(q(save_pasivos_provisions(base[1, ], client_id = "acme", base = base)), error = function(e) e)
ok("with base, a drastic shrink without the flag is refused by the net", inherits(r, "error") && nrow(en_s3()) == 10L)
q(save_pasivos_provisions(base[1, ], client_id = "acme", base = base, permitir_reduccion = TRUE))
ok("... and passes with permitir_reduccion", nrow(en_s3()) == 1L)

# 5. Every provision save built from a copy passes its base
archivos <- c("R/ledger_module.R", "R/pasivos_edit_confirm_module.R", "R/pasivos_engine.R", "R/pasivos_list_module.R",
              "R/pasivos_module.R", "R/pasivos_table_module.R", "R/pasivos_wizard_module.R")
llamadas <- unlist(lapply(archivos, function(f) {
  x <- readLines(f, warn = FALSE); grep("save_pasivos_provisions(", x[!grepl("^\\s*#", x)], fixed = TRUE, value = TRUE)
}))
sin_base <- llamadas[!grepl("base = ", llamadas, fixed = TRUE) & !grepl("permitir_reduccion = TRUE", llamadas, fixed = TRUE)]
for (s in sin_base) message("  without base: ", trimws(s))
# 12 since T18: deleting a liability ("Borrar") no longer saves provisions.
ok("every provision save passes base (only the purge passes the flag instead)",
   length(llamadas) == 12L && !length(sin_base))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
