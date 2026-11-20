# =============================================================================
# tests/test_cm34c_politicas.R
# CM34c, second commit. Ajustes › Políticas saved the session's copy of
# policy_catalog.rds and partner_policies.rds whole (save_policy_catalog,
# save_partner_policies): a session with an older copy undid another
# session's new policy, edit or assignment. Now each action is a cambio() on
# S3's current tables (modificar_policy_catalog, modificar_partner_policies):
#   1  two sessions from the same copy: one creates a policy, the other edits
#      another one -- both survive;
#   2  deleting a policy leaves the one another session just created, and its
#      cascade removes only that policy's assignments; deleting the last
#      policy empties the catalog without error;
#   3  editing a policy another session deleted: "not found", nothing written;
#   4  assigning from an older copy keeps another session's assignment of
#      the same policy to another supplier;
#   5  a failed read: nothing written, an error notice, no success notice
#      or log.
# The real observers (testServer) over the in-memory S3.
# Run from project root:  Rscript tests/test_cm34c_politicas.R
# =============================================================================
cat("=== test_cm34c_politicas ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/policy_engine.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/settings/settings_policies.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

LOGS <- character(); AVISOS <- character()
log_action <- function(..., action = NULL) { LOGS <<- c(LOGS, action %||% "?"); invisible(NULL) }
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, type); invisible("id") }
bump_sync_version <- function(...) invisible(NULL)
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)

CID <- "acme"
k <- function(key) paste0(CID, "/", key)
leer_cat <- function() get(k(S3_KEYS$policy_catalog), envir = .mem)
leer_pp  <- function() get(k(S3_KEYS$partner_policies), envir = .mem)
pol <- function(id, nombre) tibble::tibble(id = id, name = nombre, type = "weekdays", params = list(list()),
                                           roll_direction = "forward", created_by = "x",
                                           created_at = Sys.time(), updated_at = Sys.time())
asig <- function(parte, pid, orden = 1L) tibble::tibble(parte = parte, policy_id = pid, policy_order = orden,
                                                        ledger = "AP", is_interco = FALSE, is_manual = FALSE,
                                                        linked_by = "x", linked_at = Sys.time())
sembrar <- function(cat = dplyr::bind_rows(pol("p1", "Viernes"), pol("p2", "Quincena")),
                    pp = dplyr::bind_rows(asig("PROV A", "p1"), asig("PROV A", "p2", 2L))) {
  s3_memoria_reset(); LOGS <<- character(); AVISOS <<- character()
  assign(k(S3_KEYS$policy_catalog), cat, envir = .mem)
  assign(k(S3_KEYS$partner_policies), pp, envir = .mem)
}
admin <- list(user = "ana", tier = "admin", permisos = permisos_resueltos("admin"))
# One session: its own copies of both tables, as loaded now.
sesion <- function() list(
  usuario = reactiveVal(admin), current_user = function() "ana",
  current_user_info = reactive(list(username = "ana", tier = "admin")),
  effective_client_id = function() CID, home_client_id = function() CID,
  ctx_no_cargadas = reactiveVal(character()), company_map = reactive(c(NT = "Nt SA")),
  sap_data = reactiveVal(NULL), policy_catalog_db = reactiveVal(leer_cat()),
  partner_policies_db = reactiveVal(leer_pp()), policy_moves_db = reactiveVal(NULL),
  holiday_overrides_db = reactiveVal(NULL), moves_db = reactiveVal(NULL))
correr <- function(shared, pasos) testServer(function(input, output, session) {
  settings_policies_observer(input, output, session, shared)
}, { session$flushReact(); pasos(session) })
crear  <- function(nombre) function(s) {
  s$setInputs(pol_btn_new = runif(1))
  s$setInputs(pol_name = nombre, pol_type = "weekdays", pol_btn_save = runif(1))
}
editar <- function(id, nombre) function(s) {
  s$setInputs(pol_action = list(action = "edit", id = id, name = "", nonce = runif(1)))
  s$setInputs(pol_name = nombre, pol_type = "weekdays", pol_btn_save = 1)
}
borrar <- function(id) function(s) {
  s$setInputs(pol_action = list(action = "delete", id = id, name = "", nonce = runif(1)))
  s$setInputs(pol_delete_confirm = 1)
}
asignar <- function(id, partes) function(s) {
  s$setInputs(pol_action = list(action = "assign", id = id, name = "", nonce = runif(1)))
  s$setInputs(pol_assign_ledger = "AP", pol_assign_partners = partes, pol_assign_save = 1)
}

# ── 1. Create and edit from the same copy ────────────────────────────────────
sembrar(); s1 <- sesion(); s2 <- sesion()
correr(s1, crear("Lunes"))
correr(s2, editar("p2", "Quincena nueva"))   # s2's copy predates "Lunes"
c1 <- leer_cat()
ok("1a S1's new policy and S2's edit both survive", "Lunes" %in% c1$name &&
     identical(c1$name[c1$id == "p2"], "Quincena nueva") && nrow(c1) == 3L)
ok("1b the edit kept the policy's other fields and params list", is.list(c1$params) &&
     identical(c1$type[c1$id == "p2"], "weekdays"))

# ── 2. Delete ───────────────────────────────────────────────────────────────
sembrar(); s1 <- sesion(); s2 <- sesion()
correr(s1, crear("Lunes"))
pp_otra <- dplyr::bind_rows(leer_pp(), asig("PROV B", "p2"))   # and another session assigns p2 to PROV B
assign(k(S3_KEYS$partner_policies), pp_otra, envir = .mem)
correr(s2, borrar("p1"))                     # s2's copy predates both
c2 <- leer_cat(); p2 <- leer_pp()
ok("2a deleting p1 leaves p2 and the policy another session created", setequal(c2$name, c("Quincena", "Lunes")))
ok("2b the cascade removes p1's assignments only; another session's p2 assignment stays",
   !"p1" %in% p2$policy_id && all(c("PROV A", "PROV B") %in% p2$parte[p2$policy_id == "p2"]))
sembrar(cat = pol("p1", "Única"), pp = asig("PROV A", "p1")); AVISOS <- character()
correr(sesion(), borrar("p1"))
ok("2c deleting the last policy empties the catalog and its assignments, without error",
   nrow(leer_cat()) == 0L && nrow(leer_pp()) == 0L && !"error" %in% AVISOS && "eliminar_politica" %in% LOGS)

# ── 3. Edit a policy another session deleted ─────────────────────────────────
sembrar(); s1 <- sesion()
assign(k(S3_KEYS$policy_catalog), pol("p1", "Viernes"), envir = .mem)   # p2 deleted elsewhere
n0 <- .mem_escrituras; LOGS <- character(); AVISOS <- character()
correr(s1, editar("p2", "Quincena nueva"))
ok("3 editing a policy deleted meanwhile: an error notice, nothing written, no log",
   .mem_escrituras == n0 && identical(AVISOS, "error") && !length(LOGS) && !"p2" %in% leer_cat()$id)

# ── 4. Assign from an older copy ─────────────────────────────────────────────
sembrar(); s1 <- sesion(); s2 <- sesion()
correr(s1, asignar("p1", c("PROV A", "PROV B")))   # S1 adds PROV B to p1
correr(s2, asignar("p1", c("PROV A", "PROV C")))   # S2 (older copy) adds PROV C
p4 <- leer_pp()
ok("4a both sessions' new assignments of p1 survive (PROV B and PROV C)",
   setequal(p4$parte[p4$policy_id == "p1"], c("PROV A", "PROV B", "PROV C")))
ok("4b no assignment is duplicated, and PROV A keeps p2", !anyDuplicated(paste(p4$parte, p4$policy_id)) &&
     "p2" %in% p4$policy_id[p4$parte == "PROV A"])
s3 <- sesion()
correr(s3, asignar("p1", c("PROV A", "PROV B")))   # S3 takes PROV C off p1
p4b <- leer_pp()
ok("4c taking a supplier off removes only that assignment",
   setequal(p4b$parte[p4b$policy_id == "p1"], c("PROV A", "PROV B")) && nrow(p4b) == nrow(p4) - 1L)

# ── 5. A failed read ─────────────────────────────────────────────────────────
sembrar(); s1 <- sesion()
.mem_falla <- k(S3_KEYS$policy_catalog); n0 <- .mem_escrituras; LOGS <- character(); AVISOS <- character()
correr(s1, crear("Lunes"))
correr(s1, borrar("p1"))
.mem_falla <- character()
ok("5 with the catalog unreadable, create and delete write nothing; only error notices, no log",
   .mem_escrituras == n0 && length(AVISOS) == 2L && all(AVISOS == "error") && !length(LOGS) &&
     setequal(leer_cat()$id, c("p1", "p2")))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
