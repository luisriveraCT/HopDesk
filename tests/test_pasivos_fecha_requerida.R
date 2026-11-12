# =============================================================================
# tests/test_pasivos_fecha_requerida.R
# CM22, 2026-11-11. A cleared dateInput reaches the server as NA (or a
# zero-length Date). Four ways to save a manual item or a provision wrote it
# unchecked -- the item then never shows in Calendario, Vencidos or the
# search and cannot be deleted (CM21/CM23) -- and editing a provision kept
# the old date while saying "actualizada". Now each one, without a date,
# says comun.aviso.fecha_requerida and writes nothing; with a date, it saves
# as before. On the in-memory S3 (tests/helpers/s3_memoria.R), the real R/
# loaded as the app loads it.
#
# Run from project root:  Rscript tests/test_pasivos_fecha_requerida.R
# =============================================================================
cat("=== test_pasivos_fecha_requerida ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
source("tests/helpers/aislar_global.R")   # own instance lock, no network: before global.R loads
Sys.unsetenv("HOPDESK_PRODUCTION")
suppressWarnings(suppressMessages(shiny::loadSupport(".", renv = globalenv(), globalrenv = NULL)))
stopifnot("global.R se cargó con red hacia AWS" = aislar_global_sin_red())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
AVISO <- tr("comun.aviso.fecha_requerida")
CID <- "acme"
k_prov <- paste0(CID, "/", S3_KEYS$pasivos_provisions)
k_man  <- paste0(CID, "/", S3_KEYS$manual)

# Notices, without a browser.
G <- new.env(); G$avisos <- character(0)
suppressMessages(trace("showNotification", where = asNamespace("shiny"), print = FALSE,
                       tracer = quote(G$avisos <- c(G$avisos, paste(as.character(ui), collapse = "")))))
has_capability <- function(user, capability, empresa = NULL) TRUE
pasivos_autoriza <- function(capability) TRUE

prov <- function(id, fecha = Sys.Date() + 10) {
  p <- .schema_pasivos_provision()[NA_integer_, ]
  p$id <- id; p$estado <- "provisional"; p$origin <- "manual"; p$empresa <- "NTS"; p$parte <- "Proveedor"
  p$moneda_pago <- "MXN"; p$amount_pago <- 100; p$fecha_calculada <- fecha; p$fecha_efectiva <- fecha
  p$reverted_count <- 0L
  p
}
sembrar <- function() {
  s3_memoria_reset(); G$avisos <- character(0)
  assign(k_prov, prov("p1"), envir = .mem)
  assign(k_man, .schema_manual(), envir = .mem)
}
nuevo_shared <- function() list(
  effective_client_id = function() CID, current_user = reactiveVal("tester"),
  company_map = reactive(c(NTS = "Networks Transport")), empresa_sel = reactiveVal("Networks Transport"),
  pasivos_provisions_db = reactiveVal(prov("p1")), pasivos_liabilities_db = reactiveVal(.schema_pasivos_liability()),
  manual_inv = reactiveVal(.schema_manual()), pagar_hoy_db = reactiveVal(NULL),
  bancos_confirmados = reactiveVal(NULL), visible_initials = reactiveVal(NULL),
  proveedores_db = reactiveVal(NULL), suppress_ledger_prov_refresh = reactiveVal(FALSE))

# ── 0. fecha_valida() ──────────────────────────────────────────────────────
ok("fecha_valida: a date is valid; NA, a zero-length Date, '' and NULL are not",
   fecha_valida(Sys.Date()) && !fecha_valida(as.Date(NA)) && !fecha_valida(as.Date(character())) &&
     !fecha_valida("") && !fecha_valida(NULL))

# ── 1. Convert a provision (both buttons go through this) ─────────────────
convertir <- function(fecha) {
  sembrar()
  .pasivos_perform_conversion(
    input = list(pcm_fecha = fecha, pcm_empresa = "NTS", pcm_moneda = "MXN", pcm_documento = "D-1",
                 pcm_factura = "", pcm_parte = "Proveedor", pcm_codigo = "", pcm_importe = 100, pcm_notas = ""),
    shared = nuevo_shared(), prov_id = "p1", stage_to_agenda = FALSE, user = "tester")
}
r <- convertir(as.Date(NA))
ok("convert, no date: not ok, the notice, nothing written",
   isFALSE(r$ok) && identical(r$msg, AVISO) && .mem_escrituras == 0L)
ok("convert, no date: the provision stays provisional, no item", identical(get(k_prov, envir = .mem)$estado, "provisional") &&
     !nrow(get(k_man, envir = .mem)))
r <- convertir(as.Date(character()))
ok("convert, zero-length date: not ok, nothing written", isFALSE(r$ok) && .mem_escrituras == 0L)
r <- convertir(Sys.Date() + 7)
m <- get(k_man, envir = .mem)
ok("convert, with a date: the item is saved with that due date",
   isTRUE(r$ok) && nrow(m) == 1L && identical(m$`Fecha de vencimiento`, Sys.Date() + 7))

# ── 2-5. The handlers, on testServer ──────────────────────────────────────
servidor <- function(input, output, session) setup_pasivos_module(input, output, session, G$shared)
correr <- function(expr) {
  sembrar(); G$shared <- nuevo_shared()
  err <- tryCatch(suppressWarnings(suppressMessages(testServer(servidor, { session$flushReact(); eval(expr) }))),
                  error = function(e) conditionMessage(e))
  if (!is.null(err)) message("  testServer: ", err)
}
me_inputs <- function(session, fecha)
  session$setInputs(me_fecha = fecha, me_empresa = "NTS", me_parte = "Proveedor", me_codigo = "",
                    me_moneda = "MXN", me_importe = 50, me_documento = "", me_referencia = "", me_notas = "")

correr(quote({ me_inputs(session, as.Date(NA)); session$setInputs(me_save_as_provision = 1) }))
ok("Guardar como provisión, no date: the notice, nothing written", .mem_escrituras == 0L && AVISO %in% G$avisos)
correr(quote({ me_inputs(session, Sys.Date() + 3); session$setInputs(me_save_as_provision = 1) }))
ok("Guardar como provisión, with a date: a second provision is saved with it",
   nrow(get(k_prov, envir = .mem)) == 2L && (Sys.Date() + 3) %in% get(k_prov, envir = .mem)$fecha_efectiva)

editar <- function(fecha) quote({
  session$setInputs(pasivos_convert_request = "p1"); session$flushReact()
  session$setInputs(pcm_fecha = FECHA, pcm_empresa = "NTS", pcm_moneda = "MXN", pcm_parte = "Proveedor",
                    pcm_codigo = "", pcm_documento = "", pcm_factura = "", pcm_notas = "", pcm_importe = 100)
  session$setInputs(pcm_save_provision = 1)
})
FECHA <- as.Date(NA); correr(editar())
ok("edit a provision, no date: the notice, nothing written (it used to keep the old date and say 'actualizada')",
   .mem_escrituras == 0L && AVISO %in% G$avisos)
FECHA <- Sys.Date() + 20; correr(editar())
ok("edit a provision, with a date: saved with it", identical(get(k_prov, envir = .mem)$fecha_efectiva, Sys.Date() + 20))

# ── 3. "+ Provisión manual" (Pasivos table) ───────────────────────────────
ppm <- function(fecha) {
  sembrar(); G$shared <- nuevo_shared()
  err <- tryCatch(suppressWarnings(suppressMessages(testServer(pasivos_table_module_server, args = list(shared = G$shared), {
    session$flushReact()
    session$setInputs(btn_add_provision_manual = 1)
    session$setInputs(ppm_empresa = "Networks Transport", ppm_moneda = "MXN", ppm_parte = "P", ppm_codigo = "",
                      ppm_importe = 10, ppm_fecha = fecha, ppm_documento = "", ppm_referencia = "", ppm_notas = "")
    n0 <- .mem_escrituras
    session$setInputs(ppm_save = 1)
    # Read inside the session, right after the click: closing this module's
    # test session clears G$avisos and writes on its own.
    G$avisos_ppm <- G$avisos; G$esc_ppm <- .mem_escrituras - n0
  }))), error = function(e) conditionMessage(e))
  if (!is.null(err)) message("  testServer: ", err)
}
ppm(as.Date(NA))
ok("+ Provisión manual, no date: the notice, nothing written by the save", identical(G$esc_ppm, 0L) && AVISO %in% G$avisos_ppm)
ppm(Sys.Date() + 4)
ok("+ Provisión manual, with a date: saved with it", (Sys.Date() + 4) %in% get(k_prov, envir = .mem)$fecha_efectiva)

# ── 4. Agregar in Calendario / Vencidos (app.R me_save) ───────────────────
obs <- NULL
buscar <- function(x) {
  if (!is.call(x) || !is.null(obs)) return(invisible())
  if (identical(x[[1]], as.name("observeEvent")) && identical(deparse(x[[2]]), "input$me_save")) { obs <<- x; return(invisible()) }
  # Never into a variable: an empty argument (x[i, ]) would then be "missing".
  for (k in seq_along(x)) if (typeof(x[[k]]) == "language") buscar(x[[k]])
}
for (e in parse("app.R", keep.source = FALSE)) buscar(e)
ok("app.R: the me_save observer is found", !is.null(obs))
me_save <- function(fecha) {
  sembrar(); G$guardado <- NULL
  err <- tryCatch(suppressWarnings(suppressMessages(testServer(function(input, output, session) {
    shared <- list(); puede_o_avisa <- function(...) TRUE
    active_entry_ledger <- reactiveVal("AP"); current_user <- reactiveVal("tester")
    effective_client_id <- function() CID; manual_inv <- reactiveVal(.schema_manual())
    save_manual <- function(df, client_id = NULL, base = NULL) { G$guardado <- df; .mem_escrituras <<- .mem_escrituras + 1L }
    active_entry_ledger <- reactiveVal("AP")
    log_action <- function(...) invisible(NULL)
    showNotification <- function(ui, ...) { G$avisos <- c(G$avisos, paste(as.character(ui), collapse = "")); invisible("id") }
    eval(obs)
  }, {
    session$userData[["me_edit_id"]] <- ""
    session$setInputs(me_documento = "D-9", me_empresa = "Networks Transport", me_moneda = "MXN", me_factura = "",
                      me_parte = "P", me_codigo = "", me_importe = 5, me_fecha = fecha, me_notas = "",
                      me_agenda_active = FALSE)
    session$setInputs(me_save = 1)
    G$avisos_me <- G$avisos   # inside the session, as in ppm()
  }))), error = function(e) conditionMessage(e))
  if (!is.null(err)) message("  testServer: ", err)
}
me_save(as.Date(NA))
ok("Agregar (app.R), no date: the notice, nothing saved", is.null(G$guardado) && AVISO %in% G$avisos_me)
me_save(Sys.Date() + 6)
ok("Agregar (app.R), with a date: saved with it", identical(G$guardado$`Fecha de vencimiento`, Sys.Date() + 6))

# ── The notice in both languages ──────────────────────────────────────────
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
ok("comun.aviso.fecha_requerida is in es.json and en.json",
   "comun.aviso.fecha_requerida" %in% names(es) && "comun.aviso.fecha_requerida" %in% names(en))

suppressMessages(untrace("showNotification", where = asNamespace("shiny")))
s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
