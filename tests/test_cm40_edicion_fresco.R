# =============================================================================
# tests/test_cm40_edicion_fresco.R
# CM40, first commit. Guardar in the Empresas edit panel wrote the WHOLE row
# of the session's copy (rebase_por_id: a changed row wins whole). A session
# whose copy predates another session's rename OLD -> NEW wrote OLD back into
# empresas.rds -- every table already under NEW, orphaned -- and lost the
# other session's other fields. And a rename was "the form's initials differ
# from the copy's": a copy reloaded under a page still showing OLD launched
# the cascade back NEW -> OLD. Now Guardar applies only the fields the form
# changed, on S3's current row, and a rename is judged against the initials
# the page was drawn with (edit_ini_apertura):
#   1  S1 renames X AA -> NT; S2 (copy from before) changes X's RFC: X keeps
#      NT and its earlier [AA], and gets S2's RFC;
#   2  S1 changes X's razón social, S2 (copy from before) its RFC: both stay;
#   3  S2's copy reloaded (X is NT) but its page still shows AA, unchanged:
#      no cascade, X stays NT, the RFC is saved;
#   4  S2's page shows AA and the user types NS while X is NT in S3: refused
#      (renombrada_otra_sesion), no cascade, nothing written;
#   5  a plain edit still works: "actualizada" notice and log.
# The real empresasServer (testServer), two sessions over the in-memory S3;
# the cascade is recorded, not run (this is about empresas.rds).
# Against 29d829e, 1, 2, 3 and 4 fail.
# Run from project root:  Rscript tests/test_cm40_edicion_fresco.R
# =============================================================================
cat("=== test_cm40_edicion_fresco ===\n\n")
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
LOGS <- character(); AVISOS <- character(); TIPOS <- character()
log_action <- function(..., action = NULL) { if (identical(action, "sin_evento")) return(invisible(NULL)); LOGS <<- c(LOGS, action %||% "?"); invisible(NULL) }   # T72 B: sin_evento is the write layer's (test_t72b_evento)
showNotification <- function(ui, ..., type = "default") {
  AVISOS <<- c(AVISOS, as.character(ui)); TIPOS <<- c(TIPOS, type); invisible(NULL)
}

emp <- function(id, ini) .normalize(tibble::tibble(
  id = id, account_code = sprintf("E%04d", seq_along(id)), initials = ini, razon_social = paste(ini, "SA"),
  nombre_corto = ini, nombres_alt = "[]", rfc = paste0(ini, "X010101AA1"), activa = TRUE, deleted = FALSE,
  created_at = "2026-01-01", updated_at = "2026-01-01"), .schema_empresas)
CID <- "a"
KEY <- paste0(CID, "/", S3_KEYS$empresas)
poner <- function(df) assign(KEY, df, envir = .mem)
leer  <- function() get(KEY, envir = .mem)
x     <- function() { e <- leer(); e[e$id == "x1", , drop = FALSE] }
sembrar <- function() {
  s3_memoria_reset(); limpiar()
  poner(emp(c("x1", "y1"), c("AA", "BB")))
}
limpiar <- function() { LOGS <<- character(); AVISOS <<- character(); TIPOS <<- character(); CASCADAS <<- character() }
nuevo_shared <- function() {
  db <- reactiveVal(leer())
  list(empresas_db = db, company_map = reactive(.company_map_from_empresas(db())),
       effective_client_id = function() CID, home_client_id = function() CID,
       current_user = function() "ana", usuario = function() "ana",
       current_user_info = reactive(list(tier = "admin", is_staff = FALSE)),
       jump_client_id = function() NULL, visible_initials = function() NULL,
       group_config = reactiveVal(NULL), sap_data = reactiveVal(NULL))
}
# Guardar on X's edit panel. `pagina`: the initials the page was drawn with
# (edit_ini_apertura); the other fields are X's as the session's copy has
# them unless given.
editar <- function(sh, pagina, iniciales = pagina, rfc = NULL, razon = NULL) {
  testServer(empresasServer, args = list(shared = sh), {
    selected_id("x1"); session$flushReact()
    f <- isolate(sh$empresas_db()); f <- f[f$id == "x1", , drop = FALSE]
    session$setInputs(edit_ini_apertura = pagina, edit_initials = iniciales,
                      edit_razon_social = razon %||% f$razon_social, edit_nombre_corto = f$nombre_corto,
                      edit_rfc = rfc %||% f$rfc, edit_activa = TRUE, edit_nombres_alt = "")
    session$setInputs(btn_save_edit = as.numeric(Sys.time()))
  })
}
es <- function(k, ...) tr(paste0("tablas.empresas.aviso.", k), ..., idioma = "es")
anteriores <- function(r) unlist(.iniciales_anteriores_de(r$iniciales_anteriores))

# ── 1. S2's old copy does not undo S1's rename ──────────────────────────────
sembrar()
s1 <- nuevo_shared(); s2 <- nuevo_shared()
editar(s1, "AA", iniciales = "NT")                       # S1 renames X
editar(s2, "AA", rfc = "RFCNUEVO01")                     # S2's copy: X is still AA
r <- x()
ok("1a X keeps NT and its earlier initials [AA] (S2 did not write AA back)",
   identical(r$initials, "NT") && identical(anteriores(r), "AA"))
ok("1b ...and gets S2's RFC", identical(r$rfc, "RFCNUEVO01"))
ok("1c S2 ran no cascade (it renamed nothing)", identical(CASCADAS, "AA->NT"))

# ── 2. Two sessions change different fields of X ────────────────────────────
sembrar()
s1 <- nuevo_shared(); s2 <- nuevo_shared()
editar(s1, "AA", razon = "AA SA de CV nueva")
editar(s2, "AA", rfc = "RFCNUEVO02")
r <- x()
ok("2 S1's razón social and S2's RFC both stay",
   identical(r$razon_social, "AA SA de CV nueva") && identical(r$rfc, "RFCNUEVO02") && identical(r$initials, "AA"))

# ── 3. Copy reloaded, page still drawn with AA, initials untouched ──────────
sembrar()
s1 <- nuevo_shared(); editar(s1, "AA", iniciales = "NT")
s2 <- nuevo_shared(); limpiar()                          # S2's copy already says NT ...
editar(s2, "AA", iniciales = "AA", rfc = "RFCNUEVO03")   # ... its page still shows AA
r <- x()
ok("3a no cascade back NT -> AA", !length(CASCADAS))
ok("3b X stays NT, with the RFC saved", identical(r$initials, "NT") && identical(r$rfc, "RFCNUEVO03"))

# ── 4. Renaming from initials the company no longer has ─────────────────────
sembrar()
s1 <- nuevo_shared(); editar(s1, "AA", iniciales = "NT")
s2 <- nuevo_shared(); limpiar(); antes <- leer(); n0 <- .mem_escrituras
editar(s2, "AA", iniciales = "NS")
ok("4 page AA -> NS while X is NT: refused with the clear notice, no cascade, nothing written",
   !length(CASCADAS) && .mem_escrituras == n0 && identical(leer(), antes) &&
     identical(AVISOS, es("renombrada_otra_sesion", iniciales = "NT")) && !length(LOGS))

# ── 5. A plain edit ──────────────────────────────────────────────────────────
sembrar()
editar(nuevo_shared(), "AA", razon = "AA nueva")
ok("5 a plain edit: saved, \"actualizada\" notice and log",
   identical(x()$razon_social, "AA nueva") && identical(AVISOS, es("actualizada")) && identical(LOGS, "editar_empresa"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
