# =============================================================================
# tests/test_tema_cliente.R
# TEMA stage 5a: the client's theme. One value per client, `tema` in
# group_config.rds; NA (or anything unknown, or a failed read) is the
# system's default and nothing is written for it. Checks:
#   1  reading: tema_de_cliente() (the emails) and the schema;
#   2  writing: modificar_group_config() keeps the logo and the currency when
#      the theme changes and the other way round; two sessions both survive;
#      a failed read writes nothing; the T72 event carries what changed;
#   3  the real Guardar of Empresas (testServer): the permission, the offered
#      themes (a client cannot pick Marca while ofrecer = FALSE; staff can;
#      the one a client already has is kept), "" = the default;
#   4  the session: app.R's own hd_tema line (parsed from app.R) follows the
#      client being viewed -- two sessions of different clients, a jump and
#      the return -- and tema_de_sesion() reads it, also outside a reactive
#      context (a download's content).
# In-memory S3 (tests/helpers/s3_memoria.R), aws.s3 never patched.
# Run from project root:  Rscript tests/test_tema_cliente.R
# =============================================================================
cat("=== test_tema_cliente ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))
suppressMessages(source("R/tema_registro.R", encoding = "UTF-8"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"; OTRO <- "beta"
llave <- function(cid) paste0(cid, "/", S3_KEYS$group_config)
poner <- function(x, cid = CID) assign(llave(cid), x, envir = .mem)
leer  <- function(cid = CID) get(llave(cid), envir = .mem)
LOGO  <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 1))
semilla <- list(group_name = "Grupo Acme", logo_raw = LOGO, logo_ext = "png", moneda_base = "MXN")
LOGS <- list()
log_action <- function(...) { LOGS[[length(LOGS) + 1L]] <<- list(...); invisible(NULL) }

# ── 1. Reading ───────────────────────────────────────────────────────────────
ok("1a the schema has tema = NA (the default)", is.na(.schema_group_config()$tema) &&
     "tema" %in% unlist(.GROUP_CONFIG_UNIDADES))
s3_memoria_reset(); poner(semilla)
ok("1b an older file without tema: the default", identical(tema_de_cliente(CID), HD_TEMA_OMISION))
poner(c(semilla, list(tema = "marca")))
ok("1c a client with Marca: marca", identical(tema_de_cliente(CID), "marca"))
poner(c(semilla, list(tema = "neon")))
ok("1d a theme the registry does not know: the default", identical(tema_de_cliente(CID), HD_TEMA_OMISION))
poner(c(semilla, list(tema = "marca"))); .mem_falla <- llave(CID)
ok("1e a failed read: the default, never an error", identical(tema_de_cliente(CID), HD_TEMA_OMISION))
.mem_falla <- character()
ok("1f no client: the default", identical(tema_de_cliente(NULL), HD_TEMA_OMISION) &&
     identical(tema_de_cliente(""), HD_TEMA_OMISION))
ok("1g each client its own (no file for beta: the default)",
   identical(tema_de_cliente(CID), "marca") && identical(tema_de_cliente(OTRO), HD_TEMA_OMISION))
# tema_elegido_cliente(): "chose Clásico" is not "chose nothing" (the weekly mails).
poner(c(semilla, list(tema = "clasico")))
elegido_clasico <- tema_elegido_cliente(CID)
poner(semilla); sin_elegir <- tema_elegido_cliente(CID)
poner(c(semilla, list(tema = "neon"))); desconocido <- tema_elegido_cliente(CID)
poner(c(semilla, list(tema = " Marca "))); con_espacios <- tema_elegido_cliente(CID)
.mem_falla <- llave(CID); fallida <- tema_elegido_cliente(CID); .mem_falla <- character()
ok("1h tema_elegido_cliente(): the chosen one ('clasico' too), NULL when none, unknown or the read failed",
   identical(elegido_clasico, "clasico") && is.null(sin_elegir) && is.null(desconocido) &&
     identical(con_espacios, "marca") && is.null(fallida) && is.null(tema_elegido_cliente("")))
poner(c(semilla, list(tema = "marca")))

# ── 2. Writing ───────────────────────────────────────────────────────────────
guardar <- function(nuevo, base, evento = NULL)
  suppressMessages(modificar_group_config(function(fresco) .group_config_rebase(nuevo, base, fresco), CID,
                                          evento = evento))
s3_memoria_reset(); poner(semilla); base <- leer()
a <- base; a$tema <- "marca"; guardar(a, base)
ok("2a changing the theme keeps the name, the logo and the currency",
   identical(leer()$tema, "marca") && identical(leer()$logo_raw, LOGO) &&
     identical(leer()$moneda_base, "MXN") && identical(leer()$group_name, "Grupo Acme"))
b <- base; b$moneda_base <- "USD"; guardar(b, base)   # B started before A's save
ok("2b another session's currency change keeps A's theme (both survive)",
   identical(leer()$tema, "marca") && identical(leer()$moneda_base, "USD"))
c0 <- leer(); c1 <- c0; c1$tema <- NA_character_; guardar(c1, c0)
ok("2c back to automatic: NA (the default), not 'clasico' written", is.na(leer()$tema))
n0 <- .mem_escrituras; .mem_falla <- llave(CID)
d <- leer(); d1 <- d; d1$tema <- "marca"
e <- tryCatch(guardar(d1, d), error = function(e) e)
.mem_falla <- character()
ok("2d a failed read writes nothing", inherits(e, "error") && .mem_escrituras == n0 && is.na(leer()$tema))
LOGS <- list(); d <- leer(); d1 <- d; d1$tema <- "marca"
guardar(d1, d, evento = list(usuario = "luis", modulo = "empresas", accion = "guardar_config_grupo",
                             descripcion = "x", home = "hd-admin"))
ev <- Filter(function(l) identical(l$action, "guardar_config_grupo"), LOGS)
ok("2e one log entry, with what changed (the theme) and the viewer's home",
   length(ev) == 1L && identical(ev[[1]]$metadata$diferencia$tema$despues, "marca") &&
     identical(ev[[1]]$viewer_home_client_id, "hd-admin") && identical(ev[[1]]$client_id, CID) &&
     is.null(ev[[1]]$metadata$diferencia$logo))
ok("2f the logo's bytes never go into the log",
   identical(.group_config_diferencia(list(logo_raw = NULL, logo_ext = "png"), list(logo_raw = LOGO, logo_ext = "png"))$logo,
             list(antes = FALSE, despues = "cambiado")))

# ── 3. The real Guardar of Empresas ──────────────────────────────────────────
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_engine.R", "R/empresas_module.R"))   # T115: the convention selector
  suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")
PERMISO <- TRUE
puede <- function(usuario, permiso, ...) PERMISO
bump_sync_version <- function(...) invisible(NULL)
erp_access_allowed <- function(...) TRUE
sap_creds_source <- function(...) NULL
load_erp_connections <- function(...) .schema_erp_connections()

guardar_empresas <- function(copia, inputs, staff = FALSE) {
  gc_rv <- reactiveVal(copia); res <- NULL; html <- NULL
  shared <- list(
    empresas_db = reactiveVal(.schema_empresas()), company_map = reactive(character()),
    effective_client_id = function() CID, home_client_id = function() if (staff) "hd-admin" else CID,
    current_user = function() "luis", usuario = function() list(user = "luis", tier = "x"),
    current_user_info = reactive(list(tier = "x", is_staff = staff)),
    jump_client_id = function() if (staff) CID else NULL, visible_initials = function() NULL,
    group_config = gc_rv, sap_data = reactiveVal(NULL),
    moneda_base_cliente = reactive(moneda_base_resolver(gc_rv()$moneda_base)))
  testServer(empresasServer, args = list(shared = shared), {
    html <<- tryCatch(as.character(output$group_config_section$html), error = function(e) "")
    do.call(session$setInputs, utils::modifyList(list(group_name_input = copia$group_name), inputs))
    session$setInputs(btn_save_group_config = 1)
    res <<- gc_rv()
  })
  list(cfg = res, html = html)
}
quiet <- function(expr) suppressMessages(suppressWarnings(expr))

s3_memoria_reset(); poner(semilla); n0 <- .mem_escrituras
r <- quiet(guardar_empresas(leer(), list(tema_input = "marca"), staff = FALSE))
ok("3a a client cannot pick Marca while it is not offered: nothing written",
   .mem_escrituras == n0 && is.null(leer()$tema) && !grepl('value="marca"', r$html, fixed = TRUE))
r <- quiet(guardar_empresas(leer(), list(tema_input = "marca"), staff = TRUE))
ok("3b HopDesk staff can (to try it on a test client), and the selector shows it to them",
   identical(leer()$tema, "marca") && grepl('value="marca"', r$html, fixed = TRUE))
r <- quiet(guardar_empresas(leer(), list(tema_input = "marca", group_name_input = "Otro nombre"), staff = FALSE))
ok("3c a client whose theme is already Marca sees it and keeps it when saving the name",
   identical(leer()$tema, "marca") && identical(leer()$group_name, "Otro nombre") &&
     grepl('value="marca"', r$html, fixed = TRUE))
quiet(guardar_empresas(leer(), list(tema_input = ""), staff = FALSE))
ok("3d '' = automatic: NA", is.na(leer()$tema))
n0 <- .mem_escrituras
quiet(guardar_empresas(leer(), list(tema_input = "neon"), staff = TRUE))
ok("3e a theme outside the registry is refused even for staff", .mem_escrituras == n0 && is.na(leer()$tema))
PERMISO <- FALSE; n0 <- .mem_escrituras
r <- quiet(guardar_empresas(leer(), list(tema_input = "clasico", group_name_input = "Sin permiso"), staff = TRUE))
PERMISO <- TRUE
ok("3f without can_manage_group_branding: no card, and the server writes nothing",
   .mem_escrituras == n0 && !identical(leer()$group_name, "Sin permiso") && !nzchar(r$html %||% ""))
quiet(guardar_empresas(leer(), list(tema_input = "clasico"), staff = FALSE))
ok("3g a client may pick Clásico (offered)", identical(leer()$tema, "clasico"))

# ── 4. The session's theme (app.R's own lines) ───────────────────────────────
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
linea <- grep("^\\s*hd_tema <- reactive\\(", app, value = TRUE)
ok("4a app.R derives hd_tema from group_config_rv() and puts it in session$userData",
   length(linea) == 1L && grepl("group_config_rv()$tema", linea, fixed = TRUE) &&
     any(grepl("^\\s*session\\$userData\\$hd_tema <- hd_tema\\s*$", app)) &&
     any(grepl("sendCustomMessage(\"hdTema\"", app, fixed = TRUE)))
# The same line, run in a mock server: group_config_rv() is what app.R's
# context-switch observer reloads on a jump and on the return.
servidor <- function(cfg_inicial) function(input, output, session) {
  group_config_rv <- reactiveVal(cfg_inicial)
  eval(parse(text = linea))
  session$userData$hd_tema <- hd_tema
  output$t <- renderText(tema_de_sesion(session))
  session$userData$gc <- group_config_rv
}
vistos <- list()
testServer(servidor(c(semilla, list(tema = "marca"))), {
  vistos$a <<- output$t
})
testServer(servidor(semilla), { vistos$b <<- output$t })
ok("4b two sessions of different clients: each its own theme (Marca / default)",
   identical(vistos$a, "marca") && identical(vistos$b, HD_TEMA_OMISION))
# testServer() runs its body isolated; a real download's content does not:
# the same reactive, read from the top level, with no reactive context.
falsa <- new.env(); falsa$userData <- new.env()
falsa$userData$hd_tema <- reactive(tema_resolver(cliente = "marca"))
ok("4c tema_de_sesion() outside a reactive context (a download) reads it too",
   identical(tema_de_sesion(falsa), "marca"))
testServer(servidor(semilla), {
  vistos$antes <- output$t
  session$userData$gc(c(semilla, list(tema = "marca"))); session$flushReact(); vistos$salto <- output$t
  session$userData$gc(.schema_group_config());          session$flushReact(); vistos$fallo <- output$t
  session$userData$gc(semilla);                         session$flushReact(); vistos$vuelta <- output$t
  vistos <<- vistos
})
ok("4d a jump to a Marca client turns it to Marca; a failed read (empty schema) is the default; the return goes back",
   identical(vistos$antes, HD_TEMA_OMISION) && identical(vistos$salto, "marca") &&
     identical(vistos$fallo, HD_TEMA_OMISION) && identical(vistos$vuelta, HD_TEMA_OMISION))
ok("4e no session: the default", identical(tema_de_sesion(NULL), HD_TEMA_OMISION))
# Chrome (2026-11-22) saw marca -> clasico -> marca at a second sign-in: the
# message went out with the empty schema's default before the client's
# group_config was read. It waits for both loads now.
cuerpo <- paste(app, collapse = "\n")
# The read moved from Phase 2 to its own "Phase 1b" observer (priority -0.5,
# before the SAP dispatch at -1): the bump still comes right after this
# client's group_config read, and unconditionally (a failed read bumps too).
ok("4f the hdTema message waits until this client's group_config was read (Phase 1b and the jump bump it)",
   grepl("req(tema_cargado() > 0L)", cuerpo, fixed = TRUE) &&
     grepl("cliente = isolate(effective_client_id())", cuerpo, fixed = TRUE) &&
     grepl("if \\(!is\\.null\\(cfg\\)\\) group_config_rv\\(cfg\\)\\s*\\n\\s*tema_cargado\\(tema_cargado\\(\\) \\+ 1L\\)", cuerpo) &&
     grepl("vacio = \\.schema_group_config\\(\\)\\)\\s*\\n\\s*tema_cargado\\(isolate\\(tema_cargado\\(\\)\\) \\+ 1L\\)", cuerpo))
ok("4g Phase 1b reads group_config before the SAP dispatch (priority -0.5), and Phase 2 no longer does",
   grepl("load_group_config\\(client_id = cid_g\\)[\\s\\S]*?\\}, priority = -0\\.5\\)", cuerpo, perl = TRUE) &&
     !grepl("load_group_config(client_id = cid_lo)", cuerpo, fixed = TRUE))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
