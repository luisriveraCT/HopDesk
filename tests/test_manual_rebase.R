# =============================================================================
# tests/test_manual_rebase.R
# CM09 (2026-11-01): manual invoices were saved as a whole copy built from a
# fresh read OR, when that read failed, from the session's copy -- dropping
# or resurrecting items another session saved or deleted meanwhile. The
# saves now pass `base` (the copy the change was built from) and only this
# action's rows are applied on S3's table as it is now (rebase_por_id).
#
# Run from project root:  Rscript tests/test_manual_rebase.R
# =============================================================================
cat("=== test_manual_rebase ===\n\n")
source("tests/helpers/s3_memoria.R")
bump_sync_version <- function(...) invisible(TRUE)

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(expr)

MK <- "acme/manual_invoices.rds"
item <- function(ids) .normalize(tibble::tibble(id = ids, ledger = "AP", Empresa = "NCS", Moneda = "MXN",
                                                Documento = paste0("D", ids), Importe = 100), .schema_manual)
ids_s3 <- function() sort(get(MK, envir = .mem)$id)

# 1. A new item: another session's item survives
s3_memoria_reset(); assign(MK, item(c("m1", "m2", "m3")), envir = .mem)   # m3: another session
base <- item(c("m1", "m2"))                                                 # this session's (stale) copy
q(save_manual(dplyr::bind_rows(base, item("m4")), client_id = "acme", base = base))
ok("new item: added, and the other session's m3 survives", identical(ids_s3(), c("m1", "m2", "m3", "m4")))

# 2. A delete: only that item goes; nothing comes back
s3_memoria_reset(); assign(MK, item(c("m2", "m3")), envir = .mem)          # m1 deleted elsewhere, m3 added
base <- item(c("m1", "m2"))
q(save_manual(delete_manual(base, "m2"), client_id = "acme", base = base))
ok("delete: m2 removed, m3 kept, the deleted m1 not resurrected", identical(ids_s3(), "m3"))

# 3. An edit wins on its row only
s3_memoria_reset(); assign(MK, item(c("m1", "m2")), envir = .mem)
base <- item(c("m1", "m2")); nuevo <- base; nuevo$Importe[nuevo$id == "m1"] <- 999
q(save_manual(nuevo, client_id = "acme", base = base))
s3 <- get(MK, envir = .mem)
ok("edit: my change is applied", s3$Importe[s3$id == "m1"] == 999 && nrow(s3) == 2L)

# 4. A failed read writes nothing
s3_memoria_reset(); assign(MK, item(c("m1", "m2")), envir = .mem); .mem_falla <- MK
r <- tryCatch(q(save_manual(item(c("m1", "m2", "m9")), client_id = "acme", base = item(c("m1", "m2")))),
              error = function(e) e)
ok("a failed read raises and writes nothing", inherits(r, "error") && .mem_escrituras == 0L)

# 5. Every manual save outside the Agenda passes its base
archivos <- c("app.R", "R/bancos_module.R", "R/ledger_module.R", "R/pasivos_module.R", "R/search_module.R")
llamadas <- unlist(lapply(archivos, function(f)
  grep("save_manual(", readLines(f, warn = FALSE), fixed = TRUE, value = TRUE)))
sin_base <- llamadas[!grepl("base = ", llamadas, fixed = TRUE)]
for (s in sin_base) message("  without base: ", trimws(s))
ok("every save_manual() outside pagar_hoy_module.R passes base", length(llamadas) == 8L && !length(sin_base))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
