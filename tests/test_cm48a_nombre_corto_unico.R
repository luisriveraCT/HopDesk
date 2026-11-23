# =============================================================================
# tests/test_cm48a_nombre_corto_unico.R
# CM48a: the nombre_corto IS the Empresa of every SAP invoice, and nine tables
# match on it (CM47). Two companies sharing one would cross confirmations,
# overrides, moves and abonos. A new company's short name must be free --
# without case or spaces, against every row (deleted ones too) -- on the
# session's copy and, again, on S3's table as it is written (as CM39 does for
# the initials):
#   1  two sessions create "Acme" and " ACME " at once: only one is written,
#      B gets the notice, no log, no sync bump;
#   2  the name of a DELETED company is not free either;
#   3  an empty nombre_corto takes the razón social: that is what is checked;
#   4  a different name still creates;
#   5  .nombre_corto_llave() / .nombre_corto_ocupado().
# The real empresasServer (testServer) over the in-memory S3, CM39's harness.
# Against d879ed0 (CM47), 1, 2 and 3 fail.
# Run from project root:  Rscript tests/test_cm48a_nombre_corto_unico.R
# =============================================================================
cat("=== test_cm48a_nombre_corto_unico ===\n\n")
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
rename_empresa_initials <- function(...) invisible(character())
LOGS <- 0L; AVISOS <- character(); BUMPS <- 0L
log_action <- function(..., action = NULL) { if (identical(action, "sin_evento")) return(invisible(NULL)); LOGS <<- LOGS + 1L; invisible(NULL) }
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, as.character(ui)); invisible(NULL) }
bump_sync_version <- function(...) { BUMPS <<- BUMPS + 1L; invisible(NULL) }

emp <- function(id, ini, nombre, borrada = FALSE) .normalize(tibble::tibble(
  id = id, account_code = sprintf("E%04d", seq_along(id)), initials = ini, razon_social = paste(ini, "SA"),
  nombre_corto = nombre, nombres_alt = "[]", rfc = paste0(ini, "010101AA1"), activa = !borrada, deleted = borrada,
  created_at = "2026-01-01", updated_at = "2026-01-01"), .schema_empresas)
CID <- "a"
KEY <- paste0(CID, "/", S3_KEYS$empresas)
poner <- function(df) assign(KEY, df, envir = .mem)
leer  <- function() get(KEY, envir = .mem)
limpiar <- function() { LOGS <<- 0L; AVISOS <<- character(); BUMPS <<- 0L }
sembrar <- function(base = emp(c("a1", "a2"), c("AA", "AB"), c("Alfa", "Beta"))) { s3_memoria_reset(); limpiar(); poner(base) }
nuevo_shared <- function() {
  db <- reactiveVal(leer())
  list(empresas_db = db, company_map = reactive(.company_map_from_empresas(db())),
       effective_client_id = function() CID, home_client_id = function() CID,
       current_user = function() "ana", usuario = function() "ana",
       current_user_info = reactive(list(tier = "admin", is_staff = FALSE)),
       jump_client_id = function() NULL, visible_initials = function() NULL,
       group_config = reactiveVal(NULL), sap_data = reactiveVal(NULL))
}
crear <- function(sh, ini, nombre, razon = paste(ini, "SA")) testServer(empresasServer, args = list(shared = sh), {
  session$setInputs(new_initials = ini, new_razon_social = razon, new_nombre_corto = nombre,
                    new_rfc = "", new_nombres_alt = "", new_activa = TRUE)
  session$setInputs(btn_save_new_empresa = as.numeric(Sys.time()))
})
aviso <- function(n) tr("tablas.empresas.aviso.nombre_corto_existe", nombre = n, idioma = "es")
con_nombre <- function(n) sum(.nombre_corto_llave(leer()$nombre_corto) == .nombre_corto_llave(n))

# ── 1. Two sessions create "Acme" / " ACME " at once ────────────────────────
sembrar()
sa <- nuevo_shared(); sb <- nuevo_shared()        # both copies taken before either saves
crear(sa, "NA1", "Acme")
limpiar()
crear(sb, "NB1", " ACME ")                        # B's copy has no "Acme"
ok("1a only one company named acme (B's ' ACME ' was not written)", con_nombre("acme") == 1L && nrow(leer()) == 3L)
ok("1b B gets the notice -- and nothing else: no log, no sync bump",
   identical(AVISOS, aviso("ACME")) && LOGS == 0L && BUMPS == 0L)

# ── 2. A deleted company's name is not free ─────────────────────────────────
sembrar(emp(c("a1", "a2"), c("AA", "ZZ"), c("Alfa", "Vieja SA"), borrada = c(FALSE, TRUE)))
crear(nuevo_shared(), "NV1", "viejasa")
ok("2 'viejasa' is refused: a deleted company's stored rows carry 'Vieja SA'",
   con_nombre("viejasa") == 1L && nrow(leer()) == 2L && identical(AVISOS, aviso("viejasa")))

# ── 3. Empty nombre_corto takes the razón social, which is checked ──────────
sembrar()
crear(nuevo_shared(), "NC1", "", razon = "Beta")
ok("3 empty short name with razón social 'Beta' (taken): refused", con_nombre("beta") == 1L && nrow(leer()) == 2L &&
     identical(AVISOS, aviso("Beta")))

# ── 4. A free name still creates ────────────────────────────────────────────
sembrar()
crear(nuevo_shared(), "ND1", "Delta")
ok("4 'Delta' is created", con_nombre("Delta") == 1L && nrow(leer()) == 3L && !any(AVISOS == aviso("Delta")))

# ── 5. The helpers ──────────────────────────────────────────────────────────
ok("5a the key ignores case and every space", identical(.nombre_corto_llave(c(" Ac Me ", NA)), c("acme", "")))
e <- emp("a1", "AA", "Alfa")
ok("5b occupied / free / empty", .nombre_corto_ocupado(e, "ALFA") && !.nombre_corto_ocupado(e, "Beta") &&
     !.nombre_corto_ocupado(e, "  "))

cat(sprintf("\n=== %d passed, %d failed ===\n", .pass, .fail))
if (.fail) quit(status = 1)
