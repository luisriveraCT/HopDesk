# =============================================================================
# tests/test_cm47_nombre_corto_fijo.R
# CM47: the nombre_corto IS the Empresa of every SAP invoice, and nine tables
# store it to match against it (bancos_confirmados, papelera, pagar_hoy,
# moves, policy_moves, tags, sap_overrides, abonos, manual_inv). Editing it
# reopened confirmations and lost moves, overrides and abonos (CM45, D3).
# It is fixed once the company exists, in the page and on the server:
#   1  .nombre_corto_cambia(): same / trailing spaces / NA vs "" are no change;
#   2  Guardar with another nombre_corto: refused with the notice, nothing
#      written, no cascade, no log -- even together with a rename (the check
#      runs BEFORE the cascade);
#   3  Guardar with the same nombre_corto still saves the other fields;
#   4  the panel draws the field disabled, with the explanation.
# The real empresasServer (testServer) over the in-memory S3, as CM40's.
# Against fe9486a, 2 fails (the new name is written).
# Run from project root:  Rscript tests/test_cm47_nombre_corto_fijo.R
# =============================================================================
cat("=== test_cm47_nombre_corto_fijo ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages({ library(shiny) })
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/empresas_module.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

puede <- function(usuario, permiso, ...) TRUE
erp_access_allowed <- function(...) TRUE
sap_creds_source <- function(...) NULL
load_erp_connections <- function(...) .schema_erp_connections()
bump_sync_version <- function(...) invisible(NULL)
CASCADAS <- character()
rename_empresa_initials <- function(old_ini, new_ini, client_id = NULL) {
  CASCADAS <<- c(CASCADAS, paste0(old_ini, "->", new_ini)); invisible(character())
}
LOGS <- character(); AVISOS <- character()
log_action <- function(..., action = NULL) { if (identical(action, "sin_evento")) return(invisible(NULL)); LOGS <<- c(LOGS, action %||% "?"); invisible(NULL) }
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, as.character(ui)); invisible(NULL) }

emp <- function(id, ini) .normalize(tibble::tibble(
  id = id, account_code = sprintf("E%04d", seq_along(id)), initials = ini, razon_social = paste(ini, "SA"),
  nombre_corto = paste("Empresa", ini), nombres_alt = "[]", rfc = paste0(ini, "X010101AA1"), activa = TRUE,
  deleted = FALSE, created_at = "2026-01-01", updated_at = "2026-01-01"), .schema_empresas)
CID <- "a"
KEY <- paste0(CID, "/", S3_KEYS$empresas)
poner <- function(df) assign(KEY, df, envir = .mem)
leer  <- function() get(KEY, envir = .mem)
x     <- function() { e <- leer(); e[e$id == "x1", , drop = FALSE] }
sembrar <- function() { s3_memoria_reset(); limpiar(); poner(emp(c("x1", "y1"), c("AA", "BB"))) }
limpiar <- function() { LOGS <<- character(); AVISOS <<- character(); CASCADAS <<- character() }
nuevo_shared <- function() {
  db <- reactiveVal(leer())
  list(empresas_db = db, company_map = reactive(.company_map_from_empresas(db())),
       effective_client_id = function() CID, home_client_id = function() CID,
       current_user = function() "ana", usuario = function() "ana",
       current_user_info = reactive(list(tier = "admin", is_staff = FALSE)),
       jump_client_id = function() NULL, visible_initials = function() NULL,
       group_config = reactiveVal(NULL), sap_data = reactiveVal(NULL))
}
editar <- function(sh, nombre = NULL, iniciales = "AA", rfc = NULL) {
  testServer(empresasServer, args = list(shared = sh), {
    selected_id("x1"); session$flushReact()
    f <- isolate(sh$empresas_db()); f <- f[f$id == "x1", , drop = FALSE]
    session$setInputs(edit_ini_apertura = "AA", edit_initials = iniciales,
                      edit_razon_social = f$razon_social, edit_nombre_corto = nombre %||% f$nombre_corto,
                      edit_rfc = rfc %||% f$rfc, edit_activa = TRUE, edit_nombres_alt = "")
    session$setInputs(btn_save_edit = as.numeric(Sys.time()))
  })
}
aviso <- tr("tablas.empresas.aviso.nombre_corto_fijo", idioma = "es")

# ── 1. The pure check ───────────────────────────────────────────────────────
f1 <- emp("x1", "AA")
ok("1a the same name is no change", !.nombre_corto_cambia(f1, "Empresa AA"))
ok("1b trailing spaces are no change", !.nombre_corto_cambia(f1, "  Empresa AA "))
ok("1c another name is a change", .nombre_corto_cambia(f1, "Empresa Nueva"))
f2 <- f1; f2$nombre_corto <- NA_character_
ok("1d NA against \"\" is no change", !.nombre_corto_cambia(f2, ""))

# ── 2. Guardar with another nombre_corto: refused ───────────────────────────
sembrar(); antes <- leer(); n0 <- .mem_escrituras
editar(nuevo_shared(), nombre = "Otro nombre")
ok("2a refused with the notice", identical(AVISOS, aviso))
ok("2b nothing written, no cascade, no log",
   .mem_escrituras == n0 && identical(leer(), antes) && !length(CASCADAS) && !length(LOGS))
sembrar(); antes <- leer(); n0 <- .mem_escrituras
editar(nuevo_shared(), nombre = "Otro nombre", iniciales = "NT")
ok("2c together with a rename: refused BEFORE the cascade (no half-done edit)",
   identical(AVISOS, aviso) && !length(CASCADAS) && .mem_escrituras == n0 && identical(leer(), antes))

# ── 3. Same nombre_corto: the other fields still save ───────────────────────
sembrar()
editar(nuevo_shared(), rfc = "RFCNUEVO47")
r <- x()
ok("3 the RFC is saved, the nombre_corto stays", identical(r$rfc, "RFCNUEVO47") && identical(r$nombre_corto, "Empresa AA") &&
     !(aviso %in% AVISOS))

# ── 4. The panel ────────────────────────────────────────────────────────────
src <- paste(readLines("R/empresas_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("4 the edit panel draws nombre_corto disabled, with the explanation",
   grepl('shinyjs::disabled\\(textInput\\(ns\\("edit_nombre_corto"\\)', src) &&
     grepl('tr\\("tablas.empresas.nombre_corto_fijo"\\)', src))

cat(sprintf("\n=== %d passed, %d failed ===\n", .pass, .fail))
if (.fail) quit(status = 1)
