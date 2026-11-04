# =============================================================================
# tests/test_hdadmin_lectura.R
# CM13 (2026-11-04): the hd-admin control-plane tables (client registry,
# client requests, hop requests and grants, invites, staff notifications,
# emergency locks) were read with an empty-table fallback and saved with
# aws.s3::s3saveRDS() over that copy, outside the .s3_write net. Approving a
# client request with S3 unreachable left the registry with that one client.
# Now every change runs through modificar_hd_admin(): strict read, the change
# on S3's current table, .s3_write (net included).
# For each table: a failed read writes nothing; two sessions lose no rows.
#
# Run from project root:  Rscript tests/test_hdadmin_lectura.R
# =============================================================================
cat("=== test_hdadmin_lectura ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
falla <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
fuente <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }

# ── 1. Client registry ────────────────────────────────────────────────────────
k_reg <- "hd-admin/client_registry.rds"
cliente <- function(cid, users = 0L)
  data.frame(client_id = cid, display_name = toupper(cid), max_users = 5L, current_users = users,
             contact_email = NA_character_, status = "active", created_at = "2026-01-01",
             created_by = "t", stringsAsFactors = FALSE)
registro <- function(...) do.call(rbind, lapply(c(...), cliente))

# 1a. A failed read writes nothing (it wrote a one-client registry)
s3_memoria_reset()
assign(k_reg, registro("networks", "hopdesk", "tesoreria"), envir = .mem)
.mem_falla <- k_reg
ok("registry, failed read: modificar stops",
   falla(modificar_client_registry(function(r) rbind(r, cliente("nuevo")))))
ok("registry, failed read: nothing written", .mem_escrituras == 0L)
ok("registry, failed read: strict read stops too",
   falla(leer_hd_admin("client_registry.rds", .schema_client_registry)))

# 1b. Two sessions: the change runs on S3's table, not on a stale copy
s3_memoria_reset()
assign(k_reg, registro("networks", "hopdesk"), envir = .mem)
viejo <- read_client_registry()                       # session A reads
modificar_client_registry(function(r) rbind(r, cliente("tesoreria")))   # session B adds
modificar_client_registry(function(r) rbind(r, cliente("acme")))        # A adds on fresh
r <- get(k_reg, envir = .mem)
ok("registry, two sessions: both new clients kept",
   identical(sort(r$client_id), c("acme", "hopdesk", "networks", "tesoreria")))

# 1c. The registry only grows: a write that drops clients is refused
ok("registry is in .S3_SOLO_CRECEN", "client_registry.rds" %in% .S3_SOLO_CRECEN())
n0 <- .mem_escrituras
ok("registry, dropping a client is refused",
   falla(modificar_client_registry(function(r) r[r$client_id != "acme", , drop = FALSE])))
ok("... and nothing written", .mem_escrituras == n0 && nrow(get(k_reg, envir = .mem)) == 4L)

# 1d. Columns the schema does not list survive a change
s3_memoria_reset()
extra <- registro("networks"); extra$erp <- "sap"
assign(k_reg, extra, envir = .mem)
modificar_client_registry(function(r) { r$max_users <- 9L; r })
ok("registry, a column outside the schema is kept", identical(get(k_reg, envir = .mem)$erp, "sap"))

# 1e. update_client_user_count(): failed registry read -> nothing written
s3_memoria_reset()
assign(k_reg, registro("networks", "hopdesk"), envir = .mem)
assign("networks/usuarios.rds", data.frame(username = c("a", "b"), deleted = FALSE), envir = .mem)
.mem_falla <- k_reg
r <- update_client_user_count("networks")
ok("user count, failed read: FALSE and nothing written", identical(r, FALSE) && .mem_escrituras == 0L)
.mem_falla <- character()
r <- update_client_user_count("networks")
reg <- get(k_reg, envir = .mem)
ok("user count: updates the client on S3, keeps the other",
   isTRUE(r) && reg$current_users[reg$client_id == "networks"] == 2L && nrow(reg) == 2L)

# 1f. Nothing writes the registry outside modificar_client_registry()
codigo <- unlist(lapply(c("app.R", list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE)), fuente))
ok("no write_client_registry() left", !any(grepl("write_client_registry(", codigo, fixed = TRUE)))
ok("no direct s3saveRDS on hd-admin/client_registry.rds outside the dev-bucket copy",
   sum(grepl("s3saveRDS\\(.*hd-admin/client_registry\\.rds", codigo)) <= 1L)

# ── 2. Hop grants (accesos de salto) ──────────────────────────────────────────
k_hg <- "hd-admin/hop_grants.rds"
acceso <- function(id, cid = "networks", revoked = FALSE)
  data.frame(id = id, request_id = NA_character_, grantee = "bunny", client_id = cid,
             granted_by = "luis", granted_at = as.POSIXct("2026-11-01", tz = "UTC"),
             expires_at = as.POSIXct("2026-11-09", tz = "UTC"), revoked = revoked,
             stringsAsFactors = FALSE)

# 2a. A failed read writes nothing, and the caller sees the error (it was a
#     warning, so "Acceso concedido" showed for a grant never saved)
s3_memoria_reset()
assign(k_hg, rbind(acceso("g1"), acceso("g2")), envir = .mem)
.mem_falla <- k_hg
ok("grants, failed read: modificar stops (no silent warning)",
   falla(modificar_hop_grants(function(g) rbind(g, acceso("g3")))))
ok("grants, failed read: nothing written", .mem_escrituras == 0L)

# 2b. Two sessions: a revoke and a new grant from a stale copy both land
s3_memoria_reset()
assign(k_hg, rbind(acceso("g1"), acceso("g2")), envir = .mem)
modificar_hop_grants(function(g) rbind(g, acceso("g3", "hopdesk")))       # session B grants
modificar_hop_grants(function(g) { g$revoked[g$id == "g1"] <- TRUE; g })  # A revokes
g <- get(k_hg, envir = .mem)
ok("grants, two sessions: B's new grant kept", "g3" %in% g$id && nrow(g) == 3L)
ok("grants, two sessions: A's revoke applied", isTRUE(g$revoked[g$id == "g1"]))
ok("grants only grow", "hop_grants.rds" %in% .S3_SOLO_CRECEN() &&
   falla(modificar_hop_grants(function(g) g[g$id != "g2", , drop = FALSE])))
ok("no save_hop_grants() left", !any(grepl("save_hop_grants(", codigo, fixed = TRUE)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
