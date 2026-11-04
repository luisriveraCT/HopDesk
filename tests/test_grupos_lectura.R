# =============================================================================
# tests/test_grupos_lectura.R
# CM11 (2026-11-04): load_grupos() back-filled client_id by writing back the
# copy it had just read, so a stale read could undo another session's write,
# and the fill used client_id %||% CLIENT_ID. Now a read only writes when
# something is really missing, through modificar_tabla (a strict reread of S3
# right before writing), and the tenant is the prefix of the file read.
#
# Run from project root:  Rscript tests/test_grupos_lectura.R
# =============================================================================
cat("=== test_grupos_lectura ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

.cid_antes <- Sys.getenv("CLIENT_ID")
Sys.setenv(CLIENT_ID = "networks")

k_acme <- .s3_key(S3_KEYS$grupos, client_id = "acme")
grp <- function(id, name, cid = NA_character_)
  .normalize(tibble::tibble(id = id, name = name, companies = "[]", client_id = cid),
             .schema_grupos)
s3_memoria_reset_caches <- function()
  for (env in list(.s3_preload_cache, .s3_missing_cache)) rm(list = ls(env), envir = env)
leer <- function(cid = "acme") { s3_memoria_reset_caches(); load_grupos(client_id = cid) }

# 1. A complete table: reading never writes
s3_memoria_reset()
assign(k_acme, grp("g1", "Acme Group", "acme"), envir = .mem)
r <- leer(); r <- leer()
ok("complete table: two reads, zero writes", .mem_escrituras == 0L && nrow(r) == 1L)

# 2. Really missing: filled once (HopDesk's own grupo keeps "hopdesk"), then quiet
s3_memoria_reset()
assign(k_acme, dplyr::bind_rows(grp("g1", "Acme Group"), grp("g2", "Hopdesk")), envir = .mem)
r <- leer()
ok("missing client_id: filled with the file's tenant, hopdesk kept as hopdesk",
   identical(r$client_id, c("acme", "hopdesk")))
ok("... and written once", .mem_escrituras == 1L &&
   identical(get(k_acme, envir = .mem)$client_id, c("acme", "hopdesk")))
r <- leer()
ok("the next read does not write", .mem_escrituras == 1L)

# 3. Jump: a staff session (CLIENT_ID=networks) reads acme's grupos.
#    The fill is acme, the prefix read, never the deployment's CLIENT_ID.
s3_memoria_reset()
assign(k_acme, grp("g1", "Acme Group"), envir = .mem)
r <- leer("ACME ")
ok("jump: filled with the tenant read (acme), not CLIENT_ID (networks)",
   identical(r$client_id, "acme") && identical(get(k_acme, envir = .mem)$client_id, "acme"))

# 4. Concurrent: the read is stale (preload cache), and meanwhile another
#    session added g3 and renamed g1 on S3. The fill must keep both.
s3_memoria_reset()
viejo <- grp("g1", "Acme Group")
otra  <- viejo; otra$name[1] <- "Acme Holding"
otra  <- dplyr::bind_rows(otra, grp("g3", "Acme Retail", "acme"))
assign(k_acme, otra, envir = .mem)
assign(k_acme, viejo, envir = .s3_preload_cache)
r <- load_grupos(client_id = "acme")
s3 <- get(k_acme, envir = .mem)
ok("concurrent: the other session's new grupo is still on S3", "g3" %in% s3$id)
ok("concurrent: the other session's rename is still on S3",
   identical(s3$name[s3$id == "g1"], "Acme Holding"))
ok("concurrent: the fill landed on S3", identical(s3$client_id[s3$id == "g1"], "acme"))
ok("concurrent: the read returns what was written", identical(sort(r$id), c("g1", "g3")))

# 5. The read itself fails: nothing written
s3_memoria_reset()
assign(k_acme, grp("g1", "Acme Group"), envir = .mem)
.mem_falla <- k_acme
r <- leer()
ok("failed read: nothing written", .mem_escrituras == 0L)

# 6. The fill's reread fails: nothing written, the filled copy still comes back
s3_memoria_reset()
assign(k_acme, grp("g1", "Acme Group"), envir = .mem)
assign(k_acme, grp("g1", "Acme Group"), envir = .s3_preload_cache)
.mem_falla <- k_acme
r <- load_grupos(client_id = "acme")
ok("failed reread: nothing written", .mem_escrituras == 0L)
ok("failed reread: the read still returns the filled copy", identical(r$client_id, "acme"))

# 7. No client_id: the copy is filled with the tenant it was read from, but
#    nothing is written (modificar_tabla needs an explicit client_id)
s3_memoria_reset()
k_net <- .s3_key(S3_KEYS$grupos, client_id = "networks")
assign(k_net, grp("g1", "Networks Group"), envir = .mem)
s3_memoria_reset_caches()
r <- load_grupos()
ok("no client_id: filled in the read, nothing written",
   identical(r$client_id, "networks") && .mem_escrituras == 0L)

# 8. Unknown tenant: the pure fill leaves the row empty instead of guessing
ok("unknown tenant: client_id stays empty",
   is.na(.grupos_rellenar(grp("g1", "Acme Group"), NA_character_)$client_id))

Sys.setenv(CLIENT_ID = .cid_antes)
s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
