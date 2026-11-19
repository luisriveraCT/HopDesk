# =============================================================================
# tests/test_cm31_papelera_erp.R
# CM31. Every ERP connection save (new, edit, test result, delete) goes
# through .erp_mutate_and_save() in R/empresas_module.R, which rereads S3 with
# load_erp_connections() and writes back the WHOLE table it got:
#   1  load_erp_connections() leaves out the rows in the trash (deleted =
#      TRUE), so the next save of ANY connection erases them from S3 -- and
#      "Restaurar" (Stage 3b) then has nothing to restore;
#   2  a second session's save does the same to what the first one just
#      sent to the trash;
#   3  load_erp_connections() reads with .s3_read_with(): a read that fails
#      once is an empty table, and the save writes only its own row. The
#      write net (CM09, [S3_RED]) refuses it only when the table would be
#      emptied or lose more than half of 4+ rows; 3 -> 1 is written.
# The real empresasServer ERP handlers (testServer) over the in-memory S3.
# Rows in the trash must come back identical(), secrets included; the test
# never prints a secret or a config.
#
# Run from project root:  Rscript tests/test_cm31_papelera_erp.R
# =============================================================================
cat("=== test_cm31_papelera_erp ===\n\n")
suppressPackageStartupMessages(library(shiny))
source("tests/helpers/s3_memoria.R")
for (f in c("R/bancos_persistence.R", "R/pasivos_schemas.R", "R/pasivos_persistence.R",
            "R/settings/settings_helpers.R", "R/requisiciones_persistence.R",
            "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/monedas.R", "R/zona.R", "R/erp_connector_registry.R", "R/empresas_module.R"))
  suppressMessages(source(f, encoding = "UTF-8"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# Around the handlers: no audit, no bus, no screen; access granted; no key to
# encrypt with here, so the ciphertext is a fixed string.
AVISOS <- character()
showNotification   <- function(ui, ...) { AVISOS <<- c(AVISOS, as.character(ui)); invisible(NULL) }
log_action         <- function(...) invisible(NULL)
bump_sync_version  <- function(...) invisible(TRUE)
puede              <- function(...) TRUE
erp_access_allowed <- function(...) TRUE
encrypt_secret     <- function(named_list) "Y2lmcmFkbw=="

CID <- "acme"
k <- function(key) paste0(CID, "/", key)
poner <- function(key, df) assign(k(key), df, envir = .mem)
leer  <- function(key) get(k(key), envir = .mem)
fila  <- function(schema, ...) .normalize(tibble::as_tibble(list(...)), schema)
s3    <- function() leer(S3_KEYS$erp_connections)

# A read of erp_connections.rds that fails ONCE (the next one works): what a
# transient S3 error looks like to load_erp_connections().
.leer_crudo_real <- .s3_read_raw_with_retry
FALLA_UNA <- FALSE
.s3_read_raw_with_retry <- function(full_key, bucket, ...) {
  if (isTRUE(FALLA_UNA) && identical(full_key, k(S3_KEYS$erp_connections))) {
    FALLA_UNA <<- FALSE
    return(list(ok = FALSE, value = NULL))
  }
  .leer_crudo_real(full_key, bucket, ...)
}

# x1, x2 active; x3, x4 in the trash -- all of company UNO.
sembrar <- function(papelera = c("x3", "x4")) {
  s3_memoria_reset(); AVISOS <<- character(); FALLA_UNA <<- FALSE
  ids <- c("x1", "x2", "x3", "x4"); borrada <- ids %in% papelera
  poner(S3_KEYS$empresas, fila(.schema_empresas, id = "e1", initials = "UNO", razon_social = "Uno",
                               nombre_corto = "Uno", activa = TRUE, deleted = FALSE))
  poner(S3_KEYS$erp_connections, fila(.schema_erp_connections, id = ids, client_id = CID,
                                      label = c("Uno", "Dos", "Tres", "Cuatro"), erp_type = "sap_b1_service_layer",
                                      company_initials = '["UNO"]', config = '{"url":"u"}',
                                      secrets_encrypted = c("czE=", "czI=", "czM=", "czQ="),
                                      active = !borrada, deleted = borrada,
                                      deleted_at = ifelse(borrada, "2026-11-01 10:00:00", NA_character_)))
}
# The rows in the trash, as S3 has them now (for identical() after a save).
papelera <- function() { x <- s3(); x[isTRUE_v(x$deleted), , drop = FALSE] }
isTRUE_v <- function(v) !is.na(v) & v
misma_papelera <- function(antes) identical(papelera()[order(papelera()$id), ], antes[order(antes$id), ])

# One session with the ERP modal of UNO open; each step runs inside it.
nueva_sesion <- function() list(empresas_db = reactiveVal(leer(S3_KEYS$empresas)),
  effective_client_id = function() CID, home_client_id = function() CID, current_user = function() "luis",
  current_user_info = function() list(tier = "dev", user = "luis"), usuario = reactiveVal(list(user = "luis")),
  visible_initials = function() NULL, jump_client_id = function() NULL)
sesion <- function(accion, shared = nueva_sesion()) {
  suppressMessages(testServer(empresasServer, args = list(shared = shared), {
    erp_modal$company <- "UNO"
    accion(session, erp_modal)
  }))
}
alta <- function(session, erp_modal) {
  session$setInputs(btn_new_erp = 1)
  session$setInputs(erp_label = "Nueva", cfg_url = "https://sl.example:50000", cfg_company = "SBO",
                    cfg_ssl_verify = TRUE, secret_user = "u", secret_password = "p", btn_save_erp = 1)
}
editar_x1 <- function(session, erp_modal) {   # new label, same credentials (secrets left blank)
  session$setInputs(erp_edit_click = "x1")
  session$setInputs(erp_label = "Uno editada", cfg_url = "https://sl.example:50000", cfg_company = "SBO",
                    cfg_ssl_verify = TRUE, secret_user = "", secret_password = "", btn_save_erp = 1)
}
borrar <- function(id) function(session, erp_modal) {
  erp_modal$confirm_delete_id <- id
  session$setInputs(btn_confirm_erp_delete = 1)
}
restaurar <- function(id) function(session, erp_modal) session$setInputs(erp_restore_click = id)
en_papelera <- function(id) { x <- s3(); id %in% x$id && isTRUE(x$deleted[x$id == id]) }

# ── Control: what works today ───────────────────────────────────────────────
sembrar(); sesion(alta)
ok("control: a new connection is saved next to the active ones",
   all(c("x1", "x2") %in% s3()$id) && "Nueva" %in% s3()$label)
sembrar(); sesion(restaurar("x3"))
ok("control: restoring x3 straight away works (x3 active again, x4 still in the trash)",
   isTRUE(s3()$active[s3()$id == "x3"]) && !isTRUE(s3()$deleted[s3()$id == "x3"]) && en_papelera("x4"))

# ── 1. Saving any connection keeps the trash, identical() ───────────────────
sembrar(); antes <- papelera(); sesion(alta)
ok("1 new connection: x3 and x4 stay in the trash, identical() (secrets included)", misma_papelera(antes))
sembrar(); antes <- papelera(); sesion(editar_x1)
ok("1 edit of x1: the edit is saved and x3, x4 stay in the trash, identical()",
   identical(s3()$label[s3()$id == "x1"], "Uno editada") && misma_papelera(antes))
sembrar(); antes <- papelera(); sesion(borrar("x1"))
ok("1 delete of x1: x1 goes to the trash and x3, x4 stay there, identical()",
   en_papelera("x1") && misma_papelera(rbind(antes, s3()[s3()$id == "x1", ])))
sembrar(); sesion(alta); sesion(restaurar("x3"))
ok("1 after another save, x3 can still be restored (and x4 is still in the trash)",
   "x3" %in% s3()$id && isTRUE(s3()$active[s3()$id == "x3"]) && en_papelera("x4"))

# ── 2. Two sessions ─────────────────────────────────────────────────────────
# B has the modal open; A deletes x2; then B saves a new connection. A is what
# its delete writes (deleted, deleted_at, active = FALSE): testServer() cannot
# run inside another testServer().
borra_a <- function(id) {
  x <- s3(); i <- x$id == id
  x$deleted[i] <- TRUE; x$deleted_at[i] <- "2026-11-18 09:00:00"; x$active[i] <- FALSE
  poner(S3_KEYS$erp_connections, x)
}
sembrar(papelera = character())
sh_b <- nueva_sesion()
sesion(function(session, erp_modal) {
  session$setInputs(btn_new_erp = 1)                     # B opens the form
  borra_a("x2")                                           # meanwhile A deletes x2
  session$setInputs(erp_label = "Nueva", cfg_url = "https://sl.example:50000", cfg_company = "SBO",
                    cfg_ssl_verify = TRUE, secret_user = "u", secret_password = "p", btn_save_erp = 1)
}, shared = sh_b)
ok("2 two sessions: A's delete of x2 survives B's save (x2 in the trash, not erased, not back)",
   en_papelera("x2") && "Nueva" %in% s3()$label && "x1" %in% s3()$id)

# ── 3. A read that fails once never writes a smaller table ──────────────────
# No trash here (that is case 1): three active connections, x1, x2, x4.
tres_activas <- function() {
  sembrar(papelera = character())
  x <- s3(); poner(S3_KEYS$erp_connections, x[x$id != "x3", ])
}
tres_activas(); sesion(alta)
ok("3 control: with every read working, the new connection joins x1, x2, x4",
   all(c("x1", "x2", "x4") %in% s3()$id) && "Nueva" %in% s3()$label)
tres_activas(); antes <- s3()
sesion(function(session, erp_modal) {
  session$setInputs(btn_new_erp = 1)        # the screen has read the table already
  FALLA_UNA <<- TRUE                        # the save's own reread is the one that fails
  session$setInputs(erp_label = "Nueva", cfg_url = "https://sl.example:50000", cfg_company = "SBO",
                    cfg_ssl_verify = TRUE, secret_user = "u", secret_password = "p", btn_save_erp = 1)
})
ok("3 setup: the failure was used by a read (FALLA_UNA consumed)", !isTRUE(FALLA_UNA))
ok("3 the read before the save fails once: x1, x2 and x4 are all still in S3",
   all(c("x1", "x2", "x4") %in% s3()$id))
ok("3 …and the rows that were there come back identical()",
   identical(s3()[s3()$id %in% antes$id, ][order(antes$id), ], antes[order(antes$id), ]))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
