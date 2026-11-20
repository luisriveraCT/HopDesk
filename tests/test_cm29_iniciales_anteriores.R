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

# ── B. The net: what is written never carries earlier initials ──────────────
# A session that loaded before the renames writes rows under earlier
# initials; the real save functions translate them, reading empresas.rds.
MSG <- character()
guardar <- function(expr) withCallingHandlers(expr, message = function(m) {
  MSG <<- c(MSG, conditionMessage(m)); invokeRestart("muffleMessage") })
prov <- function(id, ini) fila(.schema_proveedores, id = id, nombre = id, Empresa = ini, empresas = ini, activo = TRUE)

sembrar(); editar("e1", "B"); editar("e1", "C")          # A -> B -> C
base <- leer(S3_KEYS$proveedores)
MSG <- character()
guardar(save_proveedores(dplyr::bind_rows(base, prov("pa", "A"), prov("pb", "B")), client_id = CID, base = base))
p <- leer(S3_KEYS$proveedores)
ok("B1 a chain A -> B -> C: rows written under A and under B are both saved under C",
   identical(p$Empresa[p$id %in% c("pa", "pb")], c("C", "C")) && identical(p$empresas[p$id %in% c("pa", "pb")], c("C", "C")))
ok("B1 …and the translation is counted in a message (table key and initials only)",
   any(grepl("[CM29] proveedores: 4 value(s)", MSG, fixed = TRUE)) && any(grepl("A->C", MSG, fixed = TRUE)) &&
     any(grepl("B->C", MSG, fixed = TRUE)))

editar("e1", "B")                                         # going back C -> B
base <- leer(S3_KEYS$proveedores)
guardar(save_proveedores(dplyr::bind_rows(base, prov("pc", "C"), prov("pb2", "B")), client_id = CID, base = base))
p <- leer(S3_KEYS$proveedores)
ok("B2 going back C -> B: a row under C is saved under B, and one under B stays B",
   identical(p$Empresa[p$id == "pc"], "B") && identical(p$Empresa[p$id == "pb2"], "B"))

# Legacy: before CM29 refused it, another company may hold initials e1 had.
sembrar()
e <- leer(S3_KEYS$empresas); e$initials[e$id == "e1"] <- "N"; e$iniciales_anteriores[e$id == "e1"] <- '["OTRA"]'
poner(S3_KEYS$empresas, e)
base <- leer(S3_KEYS$proveedores)
guardar(save_proveedores(dplyr::bind_rows(base, prov("po", "OTRA")), client_id = CID, base = base))
p <- leer(S3_KEYS$proveedores)
ok("B3 earlier initials that are another company's current ones (OTRA) are never translated",
   identical(p$Empresa[p$id == "po"], "OTRA"))

sembrar(); editar("e1", "B")
base <- leer(S3_KEYS$proveedores); antes <- base
.mem_falla <- k(S3_KEYS$empresas)
r <- tryCatch({ guardar(save_proveedores(dplyr::bind_rows(base, prov("px", "A")), client_id = CID, base = base)); "guardado" },
              error = function(e) "detenido")
.mem_falla <- character()
ok("B4 empresas.rds cannot be read: the save stops (an error for its notice) and nothing is written",
   identical(r, "detenido") && identical(leer(S3_KEYS$proveedores), antes))

# The other tables of the net, through their real save functions.
sembrar(); editar("e1", "B")
guardar(modificar_parte_alias_map(function(f) fila(.schema_parte_alias_map, Parte = "X", Empresa = c("A", "OTRA"), alias = "x"),
                                  client_id = CID))   # CM34c: the links are written with modificar_parte_alias_map()
ok("B5 parte_alias_map: A -> B, OTRA untouched", identical(leer(S3_KEYS$parte_alias_map)$Empresa, c("B", "OTRA")))
guardar(modificar_ctas_cuentas(function(t) dplyr::bind_rows(t, fila(.schema_ctas_cuentas, id = "c9", Empresa = "A")),
                               client_id = CID))
ok("B5 ctas_cuentas: a new account under A is saved under B",
   identical(leer(S3_KEYS$ctas_cuentas)$Empresa[leer(S3_KEYS$ctas_cuentas)$id == "c9"], "B"))
guardar(save_pasivos_liabilities(fila(.schema_pasivos_liability, id = "l9", empresa = "A"), client_id = CID))
ok("B5 pasivos_liabilities: A -> B", identical(leer(S3_KEYS$pasivos_liabilities)$empresa, "B"))
base <- leer(S3_KEYS$pasivos_provisions)
guardar(save_pasivos_provisions(dplyr::bind_rows(base, fila(.schema_pasivos_provision, id = "v9", empresa = "A")),
                                client_id = CID, base = base))
v <- leer(S3_KEYS$pasivos_provisions)
ok("B5 pasivos_provisions: A -> B", identical(v$empresa[v$id == "v9"], "B"))
inac <- fila(.schema_proveedores, id = "pi9", nombre = "I", Empresa = "A", empresas = "OTRA, A", activo = FALSE)
guardar(save_proveedores_inactivos(inac, client_id = CID))
ok("B5 proveedores_inactivos: A -> B, in the comma list too (spaces kept)",
   identical(leer(S3_KEYS$proveedores_inactivos)$Empresa, "B") &&
     identical(leer(S3_KEYS$proveedores_inactivos)$empresas, "OTRA, B"))

# A new company takes initials that were "earlier" (A -> B, then a new A).
# The Empresas screen refuses it (A6); if it is there anyway (written outside
# the screen), the net leaves the new company's rows under A.
sembrar(); editar("e1", "B")                               # complete rename A -> B
AVISOS <- character(); crear("A")
ok("B7 a new company with initials that were earlier (A) is refused on the screen",
   !"A" %in% leer(S3_KEYS$empresas)$initials && aviso_de_antes())
e <- leer(S3_KEYS$empresas)
poner(S3_KEYS$empresas, dplyr::bind_rows(e, fila(.schema_empresas, id = "e3", initials = "A", razon_social = "Nueva",
                                                 nombre_corto = "Nueva", activa = TRUE, deleted = FALSE)))
base <- leer(S3_KEYS$proveedores); MSG <- character()
guardar(save_proveedores(dplyr::bind_rows(base, prov("pn3", "A"), prov("pb3", "B")), client_id = CID, base = base))
p <- leer(S3_KEYS$proveedores)
ok("B7 …written anyway: the new company's row stays A, e1's stays B, and nothing says A->",
   identical(p$Empresa[p$id == "pn3"], "A") && identical(p$empresas[p$id == "pn3"], "A") &&
     identical(p$Empresa[p$id == "pb3"], "B") && !any(grepl("A->", MSG, fixed = TRUE)))
guardar(modificar_parte_alias_map(function(f) fila(.schema_parte_alias_map, Parte = "X", Empresa = c("A", "B"), alias = "x"),
                                  client_id = CID))
base <- leer(S3_KEYS$pasivos_provisions)
guardar(save_pasivos_provisions(dplyr::bind_rows(base, fila(.schema_pasivos_provision, id = "v3", empresa = "A")),
                                client_id = CID, base = base))
v <- leer(S3_KEYS$pasivos_provisions)
ok("B7 …and in parte_alias_map and pasivos_provisions too: the new company's A is never redirected to B",
   identical(leer(S3_KEYS$parte_alias_map)$Empresa, c("A", "B")) && identical(v$empresa[v$id == "v3"], "A"))

# Without earlier initials anywhere, a save is exactly what it always was.
sembrar()
base <- leer(S3_KEYS$proveedores); MSG <- character()
nuevo <- dplyr::bind_rows(base, prov("pn", "OTRA"))
guardar(save_proveedores(nuevo, client_id = CID, base = base))
ok("B6 no renames: the table written is identical() to what the session saved, and no [CM29] message",
   identical(leer(S3_KEYS$proveedores), .normalize(nuevo, .schema_proveedores)) && !any(grepl("[CM29]", MSG, fixed = TRUE)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
