# =============================================================================
# tests/test_proveedores_rebase.R
# CM09 (2026-11-01): the supplier catalog was saved as the session's whole
# active-only copy, dropping a supplier another session added or edited
# meanwhile, and any legacy activo=FALSE row. set_provs()/set_inac() now pass
# the session copy as `base` and only the action's rows are applied on S3.
#
# Run from project root:  Rscript tests/test_proveedores_rebase.R
# =============================================================================
cat("=== test_proveedores_rebase ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(expr)

PK <- paste0("acme/", S3_KEYS$proveedores)
prov <- function(ids, alias = "A", activo = TRUE) .normalize(tibble::tibble(id = ids, nombre = paste("Prov", ids),
          alias = alias, clabe = "012180000000000001", activo = activo), .schema_proveedores)
en_s3 <- function() get(PK, envir = .mem)

# 1. Inactivating p1 from a stale active-only copy
s3_memoria_reset()
assign(PK, dplyr::bind_rows(prov("p1"), prov("p2", alias = "OTRA"), prov("p3"), prov("pX", activo = FALSE)), envir = .mem)
base <- prov(c("p1", "p2"))                     # session copy: before p2's edit and p3's creation
q(save_proveedores(base[base$id != "p1", ], client_id = "acme", base = base))
s <- en_s3()
ok("p1 leaves the catalog", !"p1" %in% s$id)
ok("the other session's edit of p2 is kept", s$alias[s$id == "p2"] == "OTRA")
ok("the other session's new p3 is kept", "p3" %in% s$id)
ok("the legacy activo=FALSE row pX is kept", "pX" %in% s$id)

# 2. A failed read writes nothing
s3_memoria_reset(); assign(PK, prov(c("p1", "p2")), envir = .mem); .mem_falla <- PK
r <- tryCatch(q(save_proveedores(prov(c("p1", "p2", "p9")), client_id = "acme", base = prov(c("p1", "p2")))),
              error = function(e) e)
ok("a failed read raises and writes nothing", inherits(r, "error") && .mem_escrituras == 0L)

# 3. Rows without a unique id: the whole table is written, as before
s3_memoria_reset(); assign(PK, prov(c("p1", "p2")), envir = .mem)
sin_id <- prov(c("p1", NA))
q(save_proveedores(sin_id, client_id = "acme", base = prov(c("p1", "p2"))))
ok("without unique ids it falls back to the whole-table save", nrow(en_s3()) == 2L && anyNA(en_s3()$id))

# 4. The module passes the session copy as base
src <- readLines("R/proveedores_module.R", warn = FALSE)
ok("set_provs() passes base = get_provs()", any(grepl("save_proveedores(df, client_id = shared$effective_client_id(), base = get_provs())", src, fixed = TRUE)))
ok("set_inac() passes base = get_inac()", any(grepl("save_proveedores_inactivos(df, client_id = shared$effective_client_id(), base = get_inac())", src, fixed = TRUE)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
