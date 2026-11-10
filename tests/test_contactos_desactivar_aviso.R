# =============================================================================
# tests/test_contactos_desactivar_aviso.R
# CM20b, 2026-11-10. tiersServer's ⊘ on a client's contact, one click from
# start-up (the harness of tests/test_admin_clientes_hd.R, copied):
#   - deactivate_contact() says FALSE (nothing to deactivate, e.g. another
#     session already did): it used to announce success and log
#     desactivar_contacto anyway; now a neutral notice, no log;
#   - TRUE: success notice and the log, as before; an error: the error
#     notice, no log;
#   - the name came from a second, lenient read_contacts(): when that read
#     failed the click did nothing at all; now it comes from the row the
#     table showed (by id);
#   - the three notices come from the dictionary.
#
# Run from project root:  Rscript tests/test_contactos_desactivar_aviso.R
# =============================================================================
cat("=== test_contactos_desactivar_aviso ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
NOMBRES <- c(".contactos_filas", "tiers_contactos_ui", "tiers_contactos_server", "tiersServer")
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% NOMBRES)
    eval(e, envir = globalenv())

CON_KEY <- "acme/contacts.rds"
contacto <- function(id, nombre, activo = TRUE)
  data.frame(id = id, name = nombre, email = paste0(id, "@acme.mx"), role = "", receives_limit_alerts = TRUE,
             receives_audit_summaries = FALSE, active = activo, created_at = "", stringsAsFactors = FALSE)
CONS <- rbind(contacto("c-1", "María"), contacto("c-2", "Pedro"))

# tiersServer's contacts table and its ⊘ observer, as wired in the module.
cableado <- list()
buscar <- function(x) {
  if (!is.call(x)) return(invisible())
  txt <- paste(deparse(x[1:min(2L, length(x))], width.cutoff = 500L), collapse = " ")
  es_tabla <- identical(x[[1]], as.name("<-")) && is.call(x[[3]]) &&
    identical(as.character(x[[3]][[1]])[1], "tiers_contactos_server")
  es_obs <- identical(x[[1]], as.name("observeEvent")) && grepl("^observeEvent\\(contactos_t\\$desactivar", txt)
  if (es_tabla || es_obs) { cableado[[length(cableado) + 1L]] <<- x; return(invisible()) }
  xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) buscar(xs[[k]])
}
buscar(body(tiersServer))
ok("tiersServer: the contacts table and its ⊘ observer are found", length(cableado) == 2L)

# One ⊘ on `id`. resultado: what deactivate_contact() does (TRUE, FALSE or
# "error"). falla_lectura: S3 stops answering after the table was drawn.
clic <- function(id, resultado, falla_lectura = FALSE) {
  s3_memoria_reset(); assign(CON_KEY, CONS, envir = .mem)
  rec <- new.env(); rec$avisos <- character(0); rec$logs <- 0L; rec$llamadas <- character(0)
  error <- tryCatch(suppressMessages(testServer(function(input, output, session) {
    is_principal <- reactive(TRUE); is_hopdesk <- reactive(FALSE)
    contacts_refresh <- reactiveVal(0L); activity_refresh <- reactiveVal(0L)
    contacts_client_rv <- reactiveVal("acme")
    shared <- list(current_user = function() "mouse", home_client_id = function() "hd-admin")
    deactivate_contact <- function(cid, cid_contacto) {
      rec$llamadas <- c(rec$llamadas, cid_contacto)
      if (identical(resultado, "error")) stop("simulated S3 failure") else resultado
    }
    showNotification <- function(ui, ..., type = "default") { rec$avisos <- c(rec$avisos, paste0(type, ": ", ui)); invisible("id") }
    log_action <- function(...) { rec$logs <- rec$logs + 1L; invisible(NULL) }
    for (x in cableado) eval(x)
    session$flushReact()
    if (falla_lectura) .mem_falla <<- CON_KEY
    session$setInputs(`contactos-tabla-accion` = list(accion = "desactivar", ids = id, nonce = 1))
    session$flushReact()
  }, {})), error = function(e) conditionMessage(e))
  .mem_falla <<- character()
  list(error = error, avisos = rec$avisos, logs = rec$logs, llamadas = rec$llamadas)
}
txt <- function(k, nombre = NULL) { x <- tr(k); if (!is.null(nombre)) x <- gsub("{nombre}", nombre, x, fixed = TRUE); x }

# 1. Nothing to deactivate (FALSE): a neutral notice, no success, no log
r <- clic("c-2", FALSE)
ok("FALSE: no error", is.null(r$error))
ok("FALSE: no success notice", !any(grepl("^warning:", r$avisos)))
ok("FALSE: a neutral notice, 'already deactivated', with the name",
   identical(r$avisos, paste0("message: ", txt("niveles.contactos.ya_inactivo", "Pedro"))))
ok("FALSE: no desactivar_contacto log", r$logs == 0L)

# 2. Deactivated (TRUE): as before
r <- clic("c-2", TRUE)
ok("TRUE: success notice with the name, from the dictionary",
   is.null(r$error) && identical(r$avisos, paste0("warning: ", txt("niveles.contactos.desactivado", "Pedro"))))
ok("TRUE: one desactivar_contacto log", r$logs == 1L)

# 3. The save failed: error notice, no log
r <- clic("c-2", "error")
ok("error: the error notice from the dictionary, no log",
   is.null(r$error) && identical(r$avisos, paste0("error: ", txt("niveles.contactos.desactivar_error"))) && r$logs == 0L)

# 4. S3 stops answering after the table was drawn: the click still acts
r <- clic("c-1", TRUE, falla_lectura = TRUE)
ok("read fails after drawing: ⊘ still calls deactivate_contact for that id", identical(r$llamadas, "c-1"))
ok("read fails after drawing: the notice names the contact from the table's row",
   any(grepl("María", r$avisos, fixed = TRUE)))

# 5. The three notices exist in both languages
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
claves <- paste0("niveles.contactos.", c("desactivado", "ya_inactivo", "desactivar_error"))
ok("the 3 keys are in es.json and en.json", all(claves %in% names(es)) && all(claves %in% names(en)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
