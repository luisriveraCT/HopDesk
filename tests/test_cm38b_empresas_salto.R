# =============================================================================
# tests/test_cm38b_empresas_salto.R
# CM38b. Empresas kept a module-local copy (.empresas_local) filled on every
# save and never cleared: staff saved a company in client A, jumped to B,
# saw A's companies, and the next save wrote A's list into B/empresas.rds
# (B's own companies gone, nothing failing). And every save wrote the
# session's whole copy. Now a save is rebased on S3's current table and the
# module reads only shared$empresas_db(), which the jump reloads:
#   1  save in A, jump to B, save in B: B keeps its companies and gets none
#      of A's; A keeps its own (in ONE session, as staff does it);
#   2  if the session still holds A's companies while in B (a jump whose
#      load failed), Guardar stops: nothing written in B, no "saved" notice
#      or log;
#   3  two sessions of the same client add / edit at once: both survive.
# The real empresasServer (testServer) over the in-memory S3.
# Against master, 1 fails (B ends with A's list).
# Run from project root:  Rscript tests/test_cm38b_empresas_salto.R
# =============================================================================
cat("=== test_cm38b_empresas_salto ===\n\n")
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
bump_sync_version <- function(...) invisible(NULL)
erp_access_allowed <- function(...) TRUE
sap_creds_source <- function(...) NULL
load_erp_connections <- function(...) .schema_erp_connections()
rename_empresa_initials <- function(...) invisible(character())
LOGS <- character(); AVISOS <- character()
log_action <- function(..., action = NULL) { LOGS <<- c(LOGS, action %||% "?"); invisible(NULL) }
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, type); invisible(NULL) }

emp <- function(id, ini, rfc) .normalize(tibble::tibble(
  id = id, account_code = sprintf("E%04d", seq_along(id)), initials = ini, razon_social = paste(ini, "SA"),
  nombre_corto = ini, nombres_alt = "[]", rfc = rfc, activa = TRUE, deleted = FALSE,
  created_at = "2026-01-01", updated_at = "2026-01-01"), .schema_empresas)
k <- function(cid) paste0(cid, "/", S3_KEYS$empresas)
poner <- function(cid, df) assign(k(cid), df, envir = .mem)
leer  <- function(cid) get(k(cid), envir = .mem)
sembrar <- function() {
  s3_memoria_reset(); LOGS <<- character(); AVISOS <<- character()
  poner("a", emp(c("a1", "a2"), c("AA", "AB"), c("AAA010101AA1", "AAB010101AA2")))
  poner("b", emp(c("b1", "b2"), c("BA", "BB"), c("BBA010101BB1", "BBB010101BB2")))
}
# A session's shared, as app.R builds it: the client being viewed and its
# companies (the jump reloads them: .ctx_load(load_empresas, empresas_db)).
nuevo_shared <- function(cid0) {
  cid <- reactiveVal(cid0); db <- reactiveVal(leer(cid0))
  list(cid = cid, shared = list(
    empresas_db = db, company_map = reactive(.company_map_from_empresas(db())),
    effective_client_id = function() cid(), home_client_id = function() "hd-admin",
    current_user = function() "staff", usuario = function() "staff",
    current_user_info = reactive(list(tier = "dev", is_staff = TRUE)),
    jump_client_id = function() cid(), visible_initials = function() NULL,
    group_config = reactiveVal(NULL), sap_data = reactiveVal(NULL)))
}
crear <- function(session, ini, rfc) {
  session$setInputs(new_initials = ini, new_razon_social = paste(ini, "SA"), new_nombre_corto = "",
                    new_rfc = rfc, new_nombres_alt = "", new_activa = TRUE)
  session$setInputs(btn_save_new_empresa = as.numeric(Sys.time()))
}

# ── 1. Save in A, jump to B, save in B ───────────────────────────────────────
sembrar()
s <- nuevo_shared("a"); vista_b <- NULL
r1 <- tryCatch(testServer(empresasServer, args = list(shared = s$shared), {
  crear(session, "AX", "AAX010101AA3")                    # staff saves a company in A
  s$cid("b"); s$shared$empresas_db(leer("b"))            # jumps to B (the jump reloads B's companies)
  crear(session, "BX", "BBX010101BB3")                    # and saves one in B
  vista_b <<- all_empresas_rv()$initials
  TRUE
}), error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
b <- leer("b"); a <- leer("a")
ok("1a B keeps its own companies and gets its new one", setequal(b$id[b$initials != "BX"], c("b1", "b2")) &&
     "BX" %in% b$initials)
ok("1b none of A's companies land in B", !any(c("a1", "a2") %in% b$id) && !any(c("AA", "AB", "AX") %in% b$initials))
ok("1c A keeps its companies and the one saved there", setequal(a$initials, c("AA", "AB", "AX")))
ok("1d after the jump the module shows B's companies, not A's",
   !is.null(vista_b) && setequal(vista_b, c("BA", "BB", "BX")))

# ── 2. The session still holds A's companies while in B ──────────────────────
sembrar()
s <- nuevo_shared("a"); n0 <- .mem_escrituras
testServer(empresasServer, args = list(shared = s$shared), {
  s$cid("b")                                              # the jump's load failed: the copy stays A's
  selected_id("a1")
  session$setInputs(edit_initials = "AA", edit_razon_social = "AA SA editada", edit_nombre_corto = "AA",
                    edit_rfc = "AAA010101AA1", edit_activa = TRUE, edit_nombres_alt = "")
  session$setInputs(btn_save_edit = 1)
  crear(session, "AY", "AAY010101AA4")
})
ok("2a Guardar stops: nothing written in B (nor in A)", .mem_escrituras == n0 &&
     setequal(leer("b")$id, c("b1", "b2")) && setequal(leer("a")$id, c("a1", "a2")))
ok("2b no 'saved' notice and no log: only the error notices",
   !length(LOGS) && length(AVISOS) >= 2L && all(AVISOS == "error"))

# ── 3. Two sessions of the same client at once ───────────────────────────────
sembrar()
s1 <- nuevo_shared("a"); s2 <- nuevo_shared("a")          # both load A
testServer(empresasServer, args = list(shared = s1$shared), crear(session, "S1", "SSA010101AA5"))
testServer(empresasServer, args = list(shared = s2$shared), {
  crear(session, "S2", "SSB010101AA6")                    # S2's copy predates S1's company
  selected_id("a2")
  session$setInputs(edit_initials = "AB", edit_razon_social = "AB SA nueva", edit_nombre_corto = "AB",
                    edit_rfc = "AAB010101AA2", edit_activa = TRUE, edit_nombres_alt = "")
  session$setInputs(btn_save_edit = 1)
})
a <- leer("a")
ok("3a both new companies survive (S1's and S2's)", all(c("S1", "S2", "AA", "AB") %in% a$initials) && nrow(a) == 4L)
ok("3b S2's edit is saved and S1's company is still there",
   identical(a$razon_social[a$id == "a2"], "AB SA nueva") && "S1" %in% a$initials)
ok("3c S2's session copy is S3's table (it sees S1's company)", "S1" %in% isolate(s2$shared$empresas_db())$initials)

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
