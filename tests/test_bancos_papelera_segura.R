# =============================================================================
# tests/test_bancos_papelera_segura.R
# CM19 (1), 2026-11-06: Bancos' papelera (bancos_papelera.rds) was read with
# load_bancos_papelera(), which turned a failed read into an empty table,
# and saved with save_bancos_papelera() over that copy, no base, outside the
# only-grows net: archiving a deletion with S3 unreachable left a papelera
# holding only that event. Now the read is strict and both writes (archive
# on delete, restore event on undo) change S3's current table through
# modificar_bancos_papelera(); the table only grows.
#
# Run from project root:  Rscript tests/test_bancos_papelera_segura.R
# =============================================================================
cat("=== test_bancos_papelera_segura ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/bancos_persistence.R"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
fuente <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }

CID <- "acme"
k <- paste0(CID, "/", S3_KEYS$bancos_papelera)
mov <- function(id) tibble::tibble(id = id, cuenta_id = "c1", fecha = as.Date("2026-11-01"), parte = "P",
                                   concepto = "x", cargo = 10, abono = 0, tipo = "cargo", fuente = "manual",
                                   eliminado = TRUE)
archivo <- function(...) {
  p <- .schema_bancos_papelera()
  for (id in c(...)) p <- add_to_bancos_papelera(p, mov(id), actor = "t")
  p
}
# Old code (no modificar_bancos_papelera): read, change, save_bancos_papelera.
if (!exists("modificar_bancos_papelera")) modificar_bancos_papelera <- function(cambio, client_id)
  save_bancos_papelera(cambio(load_bancos_papelera(client_id = client_id)), client_id = client_id)
archivar <- function(id) modificar_bancos_papelera(function(p) add_to_bancos_papelera(p, mov(id), actor = "t"), client_id = CID)

# 1. A failed read: the strict read stops, and nothing is written
s3_memoria_reset(); assign(k, archivo("m1", "m2"), envir = .mem); .mem_falla <- k
ok("failed read: load_bancos_papelera stops (it returned an empty table)", falla(load_bancos_papelera(client_id = CID)))
ok("failed read: archiving stops", falla(archivar("m3")))
ok("failed read: nothing written", .mem_escrituras == 0L)
.mem_falla <- character()
ok("failed read: m1 and m2 still archived", identical(get(k, envir = .mem)$mov_id, c("m1", "m2")))

# 2. Archiving adds to S3's current papelera
s3_memoria_reset(); assign(k, archivo("m1"), envir = .mem)
archivar("m2")
ok("archive: m2 lands next to m1", identical(get(k, envir = .mem)$mov_id, c("m1", "m2")))

# 3. Restoring adds a 'restored' event, keeps the archive; none to restore: no write
s3_memoria_reset(); assign(k, archivo("m1", "m2"), envir = .mem)
modificar_bancos_papelera(function(p) restore_from_bancos_papelera(p, "m1", actor = "t")$papelera_df, client_id = CID)
p <- get(k, envir = .mem)
ok("restore: 3 events, the new one 'restored' for m1",
   nrow(p) == 3L && identical(p$action[3], "restored") && identical(p$mov_id[3], "m1"))
n <- .mem_escrituras
modificar_bancos_papelera(function(p) p, client_id = CID)
ok("an unchanged papelera is not written", .mem_escrituras == n)

# 4. Only grows
ok("bancos_papelera is in .S3_SOLO_CRECEN", S3_KEYS$bancos_papelera %in% .S3_SOLO_CRECEN())
s3_memoria_reset(); assign(k, archivo("m1", "m2"), envir = .mem)
ok("a change that drops an event is refused", falla(modificar_bancos_papelera(function(p) p[1, ], client_id = CID)))
ok("... and nothing written", .mem_escrituras == 0L)

# 5. Both writes in Bancos go through it
codigo <- unlist(lapply(c("app.R", list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE)), fuente))
ok("no save_bancos_papelera() left", !any(grepl("save_bancos_papelera(", codigo, fixed = TRUE)))
ok("Bancos archives and restores through modificar_bancos_papelera (2 calls)",
   sum(grepl("modificar_bancos_papelera(", fuente("R/bancos_module.R"), fixed = TRUE)) == 2L)

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
