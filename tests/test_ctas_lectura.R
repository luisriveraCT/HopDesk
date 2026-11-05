# =============================================================================
# tests/test_ctas_lectura.R
# CM12 (a, b), 2026-11-04: the Cuentas saves of Ajustes (bank catalog and
# company accounts) wrote the copy just read leniently, or the session's
# ctas_cuentas: a failed read saved an empty catalog (every account gone),
# and a stale copy undid another session's accounts. They now change S3's
# current table through modificar_tabla_completa() (strict read, no column
# dropped), a failure is told and nothing else changes, and every
# load_ctas_cuentas() names its client.
#
# Runs the REAL settings_cuentas_observer in shiny::testServer over the
# in-memory S3 (the harness of test_ajustes_permiso.R).
#
# Run from project root:  Rscript tests/test_ctas_lectura.R
# =============================================================================
cat("=== test_ctas_lectura ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", ".schema_ctas_cuentas", "load_ctas_bancos",
                                          "load_ctas_cuentas", "modificar_ctas_bancos", "modificar_ctas_cuentas",
                                          ".BANCOS_MX_CHOICES", "borrar_banco_si_libre",
                                          ".banco_en_uso_filas", ".banco_en_uso_texto"))
cargar("R/adjuntos_s3.R", c(".NUMERO_PURO", ".CELDA_PELIGRO", "neutralizar_celdas", "tabla_a_texto_copiable"))
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/settings/settings_cuentas.R"))
  suppressMessages(source(f))

AVISOS <- character()
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, as.character(ui)); invisible("id") }
removeModal <- function(...) invisible(NULL)

CID  <- "acme"
CMAP <- c(NTS = "Networks Trucking")
ADMIN <- list(user = "admin", tier = "admin", permisos = permisos_resueltos("admin"))
k_b <- paste0(CID, "/", S3_KEYS$ctas_bancos)
k_c <- paste0(CID, "/", S3_KEYS$ctas_cuentas)
BANCOS0 <- tibble::tibble(id = c("b1", "b2"), nombre = c("BanBajío", "Santander"), clave = c("030", "014"))
cta <- function(id, cuenta, moneda = "MXN", default = FALSE)
  tibble::tibble(id = id, banco_id = "b1", Empresa = "NTS", razon_social = "NT SA", rfc = "NTS010101AA1",
                 Moneda = moneda, alias = id, cuenta = cuenta, clabe_interbancaria = "",
                 is_ppl_default = default, saldo_inicial = 0, activa = TRUE)

# Runs `clics` with `copia` as the session's ctas_cuentas, over S3 as `sembrar` leaves it.
correr <- function(copia, sembrar, clics) {
  s3_memoria_reset(); AVISOS <<- character()
  sembrar()
  shared <- list(usuario = reactiveVal(ADMIN), current_user = function() "admin",
                 effective_client_id = function() CID, home_client_id = function() CID,
                 company_map = reactive(CMAP), ctas_cuentas = reactiveVal(copia))
  testServer(function(input, output, session) settings_cuentas_observer(input, output, session, shared),
             { session$flushReact(); clics(session) })
  invisible(shared)
}
nueva_cuenta <- function(session, default = FALSE) {
  session$setInputs(cta_empresa = "NTS", cta_cuenta = "0300", cta_moneda = "MXN", cta_banco_id = "b1",
                    cta_is_ppl_default = default)
  session$setInputs(cuenta_save = 1)
}

# 1. A failed read writes nothing, for each of the four saves, and says so.
#    S3 fails right before the save click (after the row is selected).
falla_lectura <- function() { assign(k_b, BANCOS0, envir = .mem); assign(k_c, cta("c1", "0100"), envir = .mem) }
cae <- function() .mem_falla <<- c(k_b, k_c)
CASOS <- list(
  banco_save    = function(s) { s$setInputs(banco_nombre = "Banorte", banco_clave = "072"); cae(); s$setInputs(banco_save = 1) },
  banco_delete  = function(s) { s$setInputs(tbl_bancos_cat_rows_selected = 1); cae(); s$setInputs(banco_delete = 1) },
  cuenta_save   = function(s) { cae(); nueva_cuenta(s) },
  cuenta_delete = function(s) { s$setInputs(tbl_cuentas_rows_selected = 1); cae(); s$setInputs(cuenta_delete = 1) })
for (nm in names(CASOS)) {
  correr(cta("c1", "0100"), falla_lectura, CASOS[[nm]])
  ok(sprintf("%s, failed read: nothing written", nm), .mem_escrituras == 0L)
  ok(sprintf("%s, failed read: the user is told", nm), tr("comun.error_guardar") %in% AVISOS)
}
.mem_falla <- character()

# 2. Stale session copy (c1); another session added c2 since. A new account
#    lands next to both; it used to save "copy + new" and drop c2.
correr(cta("c1", "0100"), function() {
  assign(k_b, BANCOS0, envir = .mem); assign(k_c, rbind(cta("c1", "0100"), cta("c2", "0200")), envir = .mem)
}, function(s) nueva_cuenta(s))
c <- get(k_c, envir = .mem)
ok("two sessions: the other session's account c2 survives", "c2" %in% c$id)
ok("two sessions: the new account is added", "0300" %in% c$cuenta && nrow(c) == 3L)

# 3. The PPL default of the siblings is cleared on S3's table, not the copy's
correr(cta("c1", "0100"), function() {
  assign(k_b, BANCOS0, envir = .mem)
  assign(k_c, rbind(cta("c1", "0100"), cta("c2", "0200", default = TRUE)), envir = .mem)
}, function(s) nueva_cuenta(s, default = TRUE))
c <- get(k_c, envir = .mem)
ok("PPL default: c2 (only on S3) loses it, the new account has it",
   identical(c$is_ppl_default[c$id == "c2"], FALSE) && isTRUE(c$is_ppl_default[c$cuenta == "0300"]))
ok("PPL default: exactly one default for NTS MXN (a new default left two)",
   sum(c$is_ppl_default & c$Empresa == "NTS" & c$Moneda == "MXN", na.rm = TRUE) == 1L)

# 4. Deactivating an account changes only that one, on S3's table
correr(cta("c1", "0100"), function() {
  assign(k_b, BANCOS0, envir = .mem); assign(k_c, rbind(cta("c1", "0100"), cta("c2", "0200")), envir = .mem)
}, function(s) { s$setInputs(tbl_cuentas_rows_selected = 1); s$setInputs(cuenta_delete = 1) })
c <- get(k_c, envir = .mem)
ok("deactivate: c1 inactive, c2 (the other session's) untouched",
   identical(c$activa[c$id == "c1"], FALSE) && isTRUE(c$activa[c$id == "c2"]))

# 5. Deleting the last bank is allowed (it is the intent), only that bank goes.
#    No account uses it: since CM17 a bank in use is never deleted.
correr(cta("c1", "0100"), function() {
  assign(k_b, BANCOS0[1, ], envir = .mem)
}, function(s) { s$setInputs(tbl_bancos_cat_rows_selected = 1); s$setInputs(banco_delete = 1) })
ok("deleting the last bank empties the catalog (was refused by the net)", nrow(get(k_b, envir = .mem)) == 0L)

# 6. A column S3's table has and the schema does not list is kept
correr(cta("c1", "0100"), function() {
  x <- cta("c1", "0100"); x$legacy_id <- "old-1"
  assign(k_b, BANCOS0, envir = .mem); assign(k_c, x, envir = .mem)
}, function(s) nueva_cuenta(s))
ok("an extra column of ctas_cuentas survives a save", "legacy_id" %in% names(get(k_c, envir = .mem)))

# 7. (b) Every load_ctas_cuentas() names its client
fuente <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
codigo <- unlist(lapply(c("app.R", list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE)), fuente))
ok("no load_ctas_cuentas() without client_id", !any(grepl("load_ctas_cuentas()", codigo, fixed = TRUE)))
ok("no save_ctas_bancos()/save_ctas_cuentas() left", !any(grepl("save_ctas_(bancos|cuentas)\\(", codigo)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
