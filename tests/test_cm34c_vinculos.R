# =============================================================================
# tests/test_cm34c_vinculos.R
# CM34c, third commit. Vincular / Desvincular proveedor (Agenda) saved the
# session's copy of parte_alias_map.rds whole, outside any tryCatch: a
# session with an older copy undid another session's link, and a failed
# write ended the session -- always so when removing the client's last link
# (the net refuses 1 -> 0). Now each is a cambio() on S3's links
# (modificar_parte_alias_map) inside a tryCatch:
#   1  linking keeps the link another session added meanwhile;
#   2  linking a supplier again replaces its link (one per Parte);
#   3  removing the last link empties the table, no error, session alive;
#   4  a failed read: nothing written, an error notice, no success notice,
#      no log and no sync bump; the session goes on;
#   5  the CM29 initials are brought up to date on the written table.
# The real pagarHoyServer (testServer, through the Agenda fixture's server)
# over the in-memory S3.
# Run from project root:  Rscript tests/test_cm34c_vinculos.R
# =============================================================================
cat("=== test_cm34c_vinculos ===\n\n")
source("tests/helpers/s3_memoria.R")            # the real persistence.R over an in-memory S3
modificar_real <- modificar_parte_alias_map
suppressMessages(source("tests/helpers/agenda_fixture_app.R"))   # pagarHoyServer + its stubs (they win)
modificar_parte_alias_map <- modificar_real

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

LOGS <- character(); AVISOS <- character(); BUMPS <- character()
log_action <- function(user, module, action, ...) { LOGS <<- c(LOGS, action); invisible(NULL) }
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, type); invisible("id") }
bump_sync_version <- function(name, ...) { BUMPS <<- c(BUMPS, name); invisible(NULL) }

CID <- "dev"                                    # the fixture's effective_client_id()
KEY <- paste0(CID, "/", S3_KEYS$parte_alias_map)
vinculo <- function(parte, alias, emp = "") tibble::tibble(Parte = parte, Empresa = emp, alias = alias,
                                                          linked_by = "x", linked_at = "2026-11-01")
poner <- function(df) assign(KEY, df, envir = .mem)
leer  <- function() get(KEY, envir = .mem)
limpiar <- function() { LOGS <<- character(); AVISOS <<- character(); BUMPS <<- character() }
# One click in a session of the real module (its own copy of the links: NULL,
# as the fixture's shared starts).
clic <- function(id, valor) {
  vivo <- FALSE
  testServer(server, {
    session$flushReact()
    do.call(session$setInputs, stats::setNames(list(valor), paste0("ph-", id)))
    vivo <<- TRUE
  })
  vivo
}
vincular    <- function(parte, alias) clic("ph_link_supplier", list(parte = parte, alias = alias, emp = ""))
desvincular <- function(parte) clic("ph_unlink_supplier", list(parte = parte, emp = ""))

# ── 1. Another session's link stays ─────────────────────────────────────────
s3_memoria_reset(); limpiar()
poner(vinculo("PROV OTRA", "OTRA"))             # linked by another session; this one never loaded it
vivo <- vincular("ACEROS MONTERREY SA", "ACEROS")
am <- leer()
ok("1a the new link is saved and the other session's link stays",
   vivo && setequal(am$Parte, c("PROV OTRA", "ACEROS MONTERREY SA")))
ok("1b success notice, log and sync bump", "message" %in% AVISOS && "vincular_proveedor" %in% LOGS &&
     "parte_alias_map_db" %in% BUMPS)

# ── 2. Linking a supplier again replaces its link ───────────────────────────
vincular("aceros monterrey sa", "ACEROS2")
am <- leer()
ok("2 one link per Parte (case-insensitive): the new alias replaces the old one",
   sum(toupper(am$Parte) == "ACEROS MONTERREY SA") == 1L && "ACEROS2" %in% am$alias && "PROV OTRA" %in% am$Parte)

# ── 3. Removing the last link ───────────────────────────────────────────────
s3_memoria_reset(); limpiar()
poner(vinculo("ACEROS MONTERREY SA", "ACEROS"))
vivo <- desvincular("ACEROS MONTERREY SA")
ok("3 removing the last link empties the table without error, and the session goes on",
   vivo && nrow(leer()) == 0L && !"error" %in% AVISOS && "desvincular_proveedor" %in% LOGS)

# ── 4. A failed read ─────────────────────────────────────────────────────────
s3_memoria_reset(); limpiar()
poner(vinculo("PROV OTRA", "OTRA")); antes <- leer()
.mem_falla <- KEY; n0 <- .mem_escrituras
vivo1 <- vincular("ACEROS MONTERREY SA", "ACEROS")
vivo2 <- desvincular("PROV OTRA")
.mem_falla <- character()
ok("4a a failed read writes nothing, link or unlink", .mem_escrituras == n0 && identical(leer(), antes))
ok("4b only error notices: no success notice, no log, no sync bump -- and the session goes on",
   vivo1 && vivo2 && length(AVISOS) == 2L && all(AVISOS == "error") && !length(LOGS) && !length(BUMPS))

# ── 5. The CM29 initials on the written table ───────────────────────────────
s3_memoria_reset(); limpiar()
assign(paste0(CID, "/", S3_KEYS$empresas),
       .normalize(tibble::tibble(id = "e1", initials = "NEW", iniciales_anteriores = '["OLD"]',
                                 activa = TRUE, deleted = FALSE), .schema_empresas), envir = .mem)
poner(vinculo("PROV OTRA", "OTRA", emp = "OLD"))
vincular("ACEROS MONTERREY SA", "ACEROS")
am <- leer()
ok("5 the link written with the initials up to date (OLD -> NEW on the other row)",
   identical(am$Empresa[am$Parte == "PROV OTRA"], "NEW"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
