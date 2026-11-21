# =============================================================================
# tests/test_cm29b_erp_iniciales.R
# CM29b. A session opens the ERP connections modal of company OLD; another
# session renames OLD -> NEW (complete); the first one then saves a new
# connection. Its company comes from the modal (erp_modal$company, set when it
# opened), so before CM29b the connection was written under OLD -- a
# connection no company has, and NEW stays without its ERP.
# save_erp_connections() now runs the CM29 net (iniciales_vigentes()) on
# company_initials only: config and secrets_encrypted are written exactly as
# the session sent them, and the connection the rename already moved is left
# identical().
# Two sessions: the real empresasServer ERP save handler (testServer) and the
# real rename (rename_empresa_initials + the Empresas write of empresas.rds),
# over the in-memory S3.
#
# Run from project root:  Rscript tests/test_cm29b_erp_iniciales.R
# =============================================================================
cat("=== test_cm29b_erp_iniciales ===\n\n")
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

# Around the save: no audit, no bus, no screen; access granted; no key to
# encrypt with here, so the ciphertext is a fixed, recognisable string.
AVISOS <- character()
showNotification   <- function(ui, ...) { AVISOS <<- c(AVISOS, as.character(ui)); invisible(NULL) }
log_action         <- function(...) invisible(NULL)
bump_sync_version  <- function(...) invisible(TRUE)
puede              <- function(...) TRUE
erp_access_allowed <- function(...) TRUE
CIFRADO <- "Y2lmcmFkby1kZS1wcnVlYmE="
encrypt_secret     <- function(named_list) CIFRADO

CID <- "acme"
k <- function(key) paste0(CID, "/", key)
poner <- function(key, df) assign(k(key), df, envir = .mem)
leer  <- function(key) get(k(key), envir = .mem)
fila  <- function(schema, ...) .normalize(tibble::as_tibble(list(...)), schema)
MSG <- character()
guardar <- function(expr) withCallingHandlers(expr, message = function(m) {
  MSG <<- c(MSG, conditionMessage(m)); invokeRestart("muffleMessage") })

sembrar <- function() {
  s3_memoria_reset(); AVISOS <<- character(); MSG <<- character()
  poner(S3_KEYS$empresas, fila(.schema_empresas, id = c("e1", "e2"), initials = c("OLD", "OTRA"),
                               razon_social = c("Uno", "Dos"), nombre_corto = c("Uno", "Dos"),
                               activa = c(TRUE, TRUE), deleted = c(FALSE, FALSE)))
  poner(S3_KEYS$erp_connections, fila(.schema_erp_connections, id = c("x1", "x2"), client_id = CID,
                                      label = c("Primera", "Otra"), erp_type = "sap_b1_service_layer",
                                      company_initials = c('["OLD"]', '["OTRA"]'),
                                      config = c('{"url":"u1"}', '{"url":"u2"}'),
                                      secrets_encrypted = c("c2VjcmV0by11bm8=", "c2VjcmV0by1kb3M="),
                                      active = TRUE, deleted = FALSE))
}
# Session A: the Empresas edit (cascade, then empresas.rds with NEW and the
# earlier initials, as the handler writes them).
# CM40: the edit's three steps -- reserve NEW in empresas.rds, the cascade,
# confirm (initials NEW, OLD among the earlier ones, reservation cleared).
renombrar <- function() {
  suppressMessages(reservar_renombre_empresa("e1", "OLD", "NEW", CID))
  suppressMessages(rename_empresa_initials("OLD", "NEW", client_id = CID))
  suppressMessages(confirmar_renombre_empresa("e1", "OLD", "NEW", CID))
}
# Session B: opens the ERP modal of `empresa`, then -- after `entre()` (session
# A's work) -- fills the form of a new connection and saves it.
sesion_b <- function(empresa, entre = function() NULL) {
  shared <- list(empresas_db = reactiveVal(leer(S3_KEYS$empresas)), effective_client_id = function() CID,
                 home_client_id = function() CID, current_user = function() "luis",
                 current_user_info = function() list(tier = "dev", user = "luis"),
                 usuario = reactiveVal(list(user = "luis")), visible_initials = function() NULL,
                 jump_client_id = function() NULL)
  guardar(testServer(empresasServer, args = list(shared = shared), {
    erp_modal$company <- empresa                         # the modal, opened now
    session$setInputs(btn_new_erp = 1)
    entre()                                              # meanwhile, the other session
    session$setInputs(erp_label = "Nueva SAP", cfg_url = "https://sl.example:50000",
                      cfg_company = "SBO_PRUEBA", cfg_ssl_verify = TRUE,
                      secret_user = "u", secret_password = "p", btn_save_erp = 1)
  }))
}
nueva <- function() { x <- leer(S3_KEYS$erp_connections); x[x$label == "Nueva SAP", ] }

# ── 1. The modal was opened before the rename ───────────────────────────────
sembrar()
sesion_b("OLD", entre = renombrar)

n <- nueva()
ok("1 the new connection is saved once", nrow(n) == 1L)
ok("1 …under the company's initials now (NEW), not OLD: NEW keeps its ERP",
   identical(n$company_initials, '["NEW"]'))
ok("1 …its config and secrets_encrypted are exactly what the session sent",
   identical(n$secrets_encrypted, CIFRADO) &&
     identical(jsonlite::fromJSON(n$config)[c("url", "company")],
               list(url = "https://sl.example:50000", company = "SBO_PRUEBA")))
x <- leer(S3_KEYS$erp_connections)
ok("1 …the connections the rename moved stay as the rename left them (x1 NEW, x2 OTRA, secrets identical)",
   identical(x$company_initials[x$id %in% c("x1", "x2")], c('["NEW"]', '["OTRA"]')) &&
     identical(x$secrets_encrypted[x$id %in% c("x1", "x2")], c("c2VjcmV0by11bm8=", "c2VjcmV0by1kb3M=")) &&
     identical(x$config[x$id %in% c("x1", "x2")], c('{"url":"u1"}', '{"url":"u2"}')))
ok("1 …and the translation is counted in a message (table key and initials only)",
   any(grepl("[CM29] erp_connections: 1 value(s)", MSG, fixed = TRUE)) && any(grepl("OLD->NEW", MSG, fixed = TRUE)))

# ── 2. No rename: the save is what it always was ────────────────────────────
sembrar()
sesion_b("OTRA")
n <- nueva()
ok("2 no rename: the new connection is under OTRA, and no [CM29] message",
   identical(n$company_initials, '["OTRA"]') && !any(grepl("[CM29]", MSG, fixed = TRUE)))

# ── 3. empresas.rds cannot be read: nothing is written ──────────────────────
sembrar(); renombrar()
antes <- leer(S3_KEYS$erp_connections)
con_x9 <- dplyr::bind_rows(antes, fila(.schema_erp_connections, id = "x9", client_id = CID,
                                       company_initials = '["OLD"]'))
intento <- function() tryCatch({ suppressMessages(save_erp_connections(con_x9, client_id = CID)); "guardado" },
                                error = function(e) "detenido")
r <- intento()
ok("3 setup: the same save works while empresas.rds can be read (x9 under NEW)",
   identical(r, "guardado") && identical(leer(S3_KEYS$erp_connections)$company_initials[3], '["NEW"]'))
poner(S3_KEYS$erp_connections, antes)
.mem_falla <- k(S3_KEYS$empresas)
r <- intento()
.mem_falla <- character()
ok("3 empresas.rds cannot be read: the save stops and the connections are untouched",
   identical(r, "detenido") && identical(leer(S3_KEYS$erp_connections), antes))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
