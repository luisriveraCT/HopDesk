# =============================================================================
# tests/test_empresas_lectura.R
# VP1 (2026-11-04): load_empresas() back-filled account_code / group_id by
# writing back the copy it had just read. A read could then undo another
# session's write made since, and with no grupo yet every read wrote.
# Now a read only writes when something is really missing, and the fill goes
# through modificar_tabla (a strict reread of S3 right before writing).
#
# Run from project root:  Rscript tests/test_empresas_lectura.R
# =============================================================================
cat("=== test_empresas_lectura ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

k_emp <- .s3_key(S3_KEYS$empresas, client_id = "acme")
k_grp <- .s3_key(S3_KEYS$grupos,   client_id = "acme")
emp <- function(ini, code = NA_character_, gid = NA_character_)
  .normalize(tibble::tibble(id = paste0("id_", ini), account_code = code, initials = ini,
                            nombre_corto = ini, activa = TRUE, deleted = FALSE, group_id = gid),
             .schema_empresas)
grupo <- .normalize(tibble::tibble(id = "g1", name = "Networks Group", companies = "[]",
                                   client_id = "acme"), .schema_grupos)
leer <- function() { s3_memoria_reset_caches(); load_empresas(client_id = "acme") }
s3_memoria_reset_caches <- function()
  for (env in list(.s3_preload_cache, .s3_missing_cache)) rm(list = ls(env), envir = env)

# 1. A complete table: reading never writes
s3_memoria_reset()
assign(k_emp, dplyr::bind_rows(emp("NCS", "E0001", "g1"), emp("NTS", "E0002", "g1")), envir = .mem)
assign(k_grp, grupo, envir = .mem)
r <- leer(); r <- leer()
ok("complete table: two reads, zero writes", .mem_escrituras == 0L && nrow(r) == 2L)

# 2. No grupo yet: an empty group_id cannot be filled, so no write (it wrote on every read)
s3_memoria_reset()
assign(k_emp, emp("NCS", "E0001"), envir = .mem)
r <- leer(); r <- leer()
ok("no grupo to assign: two reads, zero writes", .mem_escrituras == 0L)

# 3. Really missing: one write, then reads are quiet again
s3_memoria_reset()
assign(k_emp, dplyr::bind_rows(emp("NCS", "E0001", "g1"), emp("NTS"), emp("NL")), envir = .mem)
assign(k_grp, grupo, envir = .mem)
r <- leer()
ok("missing codes and group: filled in what the read returns",
   identical(r$account_code, c("E0001", "E0002", "E0003")) && all(r$group_id == "g1"))
ok("... and written once", .mem_escrituras == 1L &&
   identical(get(k_emp, envir = .mem)$account_code, c("E0001", "E0002", "E0003")))
r <- leer()
ok("the next read does not write", .mem_escrituras == 1L)

# 4. Concurrent: the read is stale (preload cache), and meanwhile another
#    session added NRS and renamed NCS on S3. The fill must keep both.
s3_memoria_reset()
assign(k_grp, grupo, envir = .mem)
viejo <- dplyr::bind_rows(emp("NCS", "E0001", "g1"), emp("NTS", NA, "g1"))
otra  <- viejo; otra$nombre_corto[1] <- "Networks CS"
otra  <- dplyr::bind_rows(otra, emp("NRS", "E0007", "g1"))
assign(k_emp, otra, envir = .mem)
assign(k_emp, viejo, envir = .s3_preload_cache)
r <- load_empresas(client_id = "acme")
s3 <- get(k_emp, envir = .mem)
ok("concurrent: the other session's new company is still on S3", "NRS" %in% s3$initials)
ok("concurrent: the other session's rename is still on S3",
   identical(s3$nombre_corto[s3$initials == "NCS"], "Networks CS"))
ok("concurrent: the fill numbers after S3's codes (NTS gets E0008, not E0002)",
   identical(s3$account_code[s3$initials == "NTS"], "E0008"))
ok("concurrent: the read returns what was written", identical(sort(r$initials), c("NCS", "NRS", "NTS")))

# 5. The fill's reread fails: nothing written, the filled copy still comes back
s3_memoria_reset()
assign(k_emp, emp("NTS", NA, NA), envir = .mem)
assign(k_emp, emp("NTS", NA, NA), envir = .s3_preload_cache)
.mem_falla <- k_emp
r <- load_empresas(client_id = "acme")
ok("failed reread: nothing written", .mem_escrituras == 0L)
ok("failed reread: the read still returns the filled copy", identical(r$account_code, "E0001"))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
