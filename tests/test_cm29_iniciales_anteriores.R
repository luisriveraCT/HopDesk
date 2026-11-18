# =============================================================================
# tests/test_cm29_iniciales_anteriores.R
# CM29 parts 2 and 3. empresas.rds keeps, per company, the initials it had
# before a complete rename (iniciales_anteriores), and the tables the rename
# handles translate them to the current ones when they are written.
#   A  the Empresas edit (real empresasServer, testServer, in-memory S3):
#      a complete rename writes OLD into iniciales_anteriores; a chain
#      A -> B -> C keeps A and B; going back takes the current one out; an
#      incomplete rename (hd_renombrar_incompleto) adds nothing; another
#      company's earlier initials are refused, on edit and on create (tr()).
# Run from project root:  Rscript tests/test_cm29_iniciales_anteriores.R
# =============================================================================
cat("=== test_cm29_iniciales_anteriores ===\n\n")
suppressPackageStartupMessages(library(shiny))
source("tests/helpers/s3_memoria.R")
for (f in c("R/bancos_persistence.R", "R/pasivos_schemas.R", "R/pasivos_persistence.R",
            "R/settings/settings_helpers.R", "R/requisiciones_persistence.R",
            "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/monedas.R", "R/zona.R", "R/empresas_module.R"))
  suppressMessages(source(f, encoding = "UTF-8"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# What the module calls around the save: no audit, no bus, no screen here.
AVISOS <- character()
showNotification  <- function(ui, ...) { AVISOS <<- c(AVISOS, as.character(ui)); invisible(NULL) }
log_action        <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(TRUE)
puede             <- function(...) TRUE

CID <- "acme"
k <- function(key) paste0(CID, "/", key)
poner <- function(key, df) assign(k(key), df, envir = .mem)
leer  <- function(key) get(k(key), envir = .mem)
fila  <- function(schema, ...) .normalize(tibble::as_tibble(list(...)), schema)
sembrar <- function() {
  s3_memoria_reset(); AVISOS <<- character()
  poner(S3_KEYS$empresas, fila(.schema_empresas, id = c("e1", "e2"), initials = c("A", "OTRA"),
                               razon_social = c("Uno", "Dos"), nombre_corto = c("Uno", "Dos"),
                               activa = c(TRUE, TRUE), deleted = c(FALSE, FALSE)))
  poner(S3_KEYS$proveedores, fila(.schema_proveedores, id = "p1", nombre = "UNO", Empresa = "A",
                                  empresas = "A", activo = TRUE))
  poner(S3_KEYS$pasivos_provisions, fila(.schema_pasivos_provision, id = "v1", empresa = "A"))
}
anteriores <- function(id) {
  e <- leer(S3_KEYS$empresas)
  .iniciales_anteriores_de(e$iniciales_anteriores[e$id == id])[[1]]
}
ini_de <- function(id) { e <- leer(S3_KEYS$empresas); e$initials[e$id == id] }

# Runs the Empresas edit of company `id` to `nueva` (or creates `crear`) over
# what S3 holds now, as a session that just loaded.
editar <- function(eid, nueva) {   # not `id`: inside testServer that is the module's
  e <- leer(S3_KEYS$empresas)
  shared <- list(empresas_db = reactiveVal(e), effective_client_id = function() CID,
                 home_client_id = function() CID, current_user = function() "luis",
                 current_user_info = function() list(tier = "dev"), usuario = reactiveVal(list(user = "luis")),
                 visible_initials = function() NULL, proveedores_db = reactiveVal(NULL),
                 parte_alias_map_db = reactiveVal(NULL), ctas_cuentas = reactiveVal(NULL),
                 group_config = reactiveVal(NULL), jump_client_id = function() NULL,
                 moneda_base_cliente = function() "MXN", sap_data = reactiveVal(NULL),
                 interco_v2 = reactiveVal(NULL))
  fila_e <- e[e$id == eid, ]
  suppressMessages(testServer(empresasServer, args = list(shared = shared), {
    selected_id(eid)
    session$setInputs(edit_initials = nueva, edit_razon_social = fila_e$razon_social,
                      edit_nombre_corto = fila_e$nombre_corto, edit_rfc = "", edit_activa = TRUE,
                      edit_nombres_alt = "", btn_save_edit = 1)
  }))
}
crear <- function(ini) {
  e <- leer(S3_KEYS$empresas)
  shared <- list(empresas_db = reactiveVal(e), effective_client_id = function() CID,
                 home_client_id = function() CID, current_user = function() "luis",
                 current_user_info = function() list(tier = "dev"), usuario = reactiveVal(list(user = "luis")),
                 visible_initials = function() NULL)
  suppressMessages(testServer(empresasServer, args = list(shared = shared), {
    session$setInputs(new_initials = ini, new_razon_social = "Nueva", new_nombre_corto = "",
                      new_rfc = "", new_activa = TRUE, new_nombres_alt = "", btn_save_new_empresa = 1)
  }))
}
aviso_de_antes <- function() any(grepl(tr("tablas.empresas.aviso.iniciales_de_antes"), AVISOS, fixed = TRUE))

# ── A. iniciales_anteriores, written by the Empresas edit ────────────────────
sembrar()
editar("e1", "B")
ok("A1 a complete rename A -> B: e1 is B and remembers A",
   identical(ini_de("e1"), "B") && identical(anteriores("e1"), "A"))
ok("A1 …and the cascade ran (p1 and v1 are B)",
   identical(leer(S3_KEYS$proveedores)$Empresa, "B") && identical(leer(S3_KEYS$pasivos_provisions)$empresa, "B"))
editar("e1", "C")
ok("A2 a chain A -> B -> C: e1 is C and remembers A and B",
   identical(ini_de("e1"), "C") && identical(sort(anteriores("e1")), c("A", "B")))
editar("e1", "B")
ok("A3 going back C -> B: B leaves the list (it is current again) and C enters",
   identical(ini_de("e1"), "B") && identical(sort(anteriores("e1")), c("A", "C")))

sembrar()
.mem_falla <- k(S3_KEYS$pasivos_provisions)   # one step of the cascade fails
editar("e1", "B")
.mem_falla <- character()
ok("A4 an incomplete rename (hd_renombrar_incompleto): e1 keeps A and nothing is remembered",
   identical(ini_de("e1"), "A") && !length(anteriores("e1")))

sembrar()
editar("e1", "B")                      # e1 had A
AVISOS <- character()
editar("e2", "A")                      # e2 asks for e1's earlier initials
ok("A5 renaming another company to initials e1 had before is refused, with its notice",
   identical(ini_de("e2"), "OTRA") && aviso_de_antes())
AVISOS <- character()
crear("A")
ok("A6 creating a company with initials e1 had before is refused, with its notice",
   !"A" %in% leer(S3_KEYS$empresas)$initials && aviso_de_antes())
editar("e1", "A")
ok("A7 …but e1 itself may go back to its own earlier initials (B -> A)",
   identical(ini_de("e1"), "A") && identical(anteriores("e1"), "B"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
