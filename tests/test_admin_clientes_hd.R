# =============================================================================
# tests/test_admin_clientes_hd.R
# Clientes › Clientes registrados and a client's Contactos on the table
# engine (T28, class D):
#   1. the rows: one per client (id = client_id), Cupo "lleno" / "pocos"
#      instead of a tinted row (P3); the active contacts (id = contact id);
#   2. ✎ / 📇 / ⊘ name the row by its id, also when another session added
#      one ahead of it after drawing;
#   3. without HopDesk/Principal: no rows, an action sent by hand names
#      nothing (integrator's condition c);
#   4. start-up: no file is 0 rows; an unreadable registry or contact list is
#      the table's notice, never "no clients" / "no contacts" (P7);
#   5. tiersServer's own observers, one click from start-up: ✎ opens the
#      edit modal, 📇 the contacts modal for that client, ⊘ deactivates.
#
# Run from project root:  Rscript tests/test_admin_clientes_hd.R
# =============================================================================
cat("=== test_admin_clientes_hd ===\n\n")
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
NOMBRES <- c(".clientes_ultima_actividad", ".clientes_filas", "tiers_clientes_ui", "tiers_clientes_server",
             ".contactos_filas", "tiers_contactos_ui", "tiers_contactos_server", "tiersServer")
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% NOMBRES)
    eval(e, envir = globalenv())

REG_KEY <- "hd-admin/client_registry.rds"; CON_KEY <- "acme/contacts.rds"
cliente <- function(id, maxu, cur, status = "active")
  data.frame(client_id = id, display_name = toupper(id), max_users = maxu, current_users = cur,
             contact_email = paste0("ti@", id, ".mx"), status = status, created_at = "", created_by = "",
             stringsAsFactors = FALSE)
REG <- rbind(cliente("acme", 5L, 2L), cliente("beta", 3L, 3L), cliente("gamma", 10L, 9L), cliente("delta", 4L, 0L, "archived"))
contacto <- function(id, nombre, activo = TRUE, alertas = TRUE)
  data.frame(id = id, name = nombre, email = paste0(id, "@acme.mx"), role = "", receives_limit_alerts = alertas,
             receives_audit_summaries = FALSE, active = activo, created_at = "", stringsAsFactors = FALSE)
CONS <- rbind(contacto("c-1", "María"), contacto("c-2", "Pedro", alertas = FALSE), contacto("c-0", "Ex", activo = FALSE))
sembrar <- function(reg = REG, cons = CONS) {
  s3_memoria_reset()
  if (!is.null(reg))  assign(REG_KEY, reg, envir = .mem)
  if (!is.null(cons)) assign(CON_KEY, cons, envir = .mem)
  assign("acme/app_audit.rds", data.frame(ts = "2026-11-08 09:30:00"), envir = .mem)
}

# ── 1. The rows ──────────────────────────────────────────────────────────────
f <- .clientes_filas(REG, c(1, NA, NA, NA))
ok("una fila por cliente; el id es el client_id", identical(f$id, c("acme", "beta", "gamma", "delta")))
ok("Cupo: lleno sin lugares, pocos con 2 o menos, nada con lugar (en vez de teñir la fila)",
   identical(f$cupo, c("", "lleno", "pocos", "")))
ok("Usuarios dice “usados / límite” y Estado activo o archivado",
   identical(f$usuarios[1], "2 / 5") && identical(f$estado, c("active", "active", "active", "archived")))
ok("contactos: solo los activos, con su id; las alertas como insignia",
   identical(.contactos_filas(CONS)$id, c("c-1", "c-2")) && identical(.contactos_filas(CONS)$alertas, c("si", "")))
for (caso in list(list("NULL", NULL), list("vacío", REG[0, ])))
  ok(sprintf("registro %s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .clientes_filas(caso[[2]]); is.data.frame(v) && !nrow(v) && length(names(v)) == 7L })
ok("contactos NULL o sin activos: 0 filas", !nrow(.contactos_filas(NULL)) && !nrow(.contactos_filas(CONS[3, ])))

# ── 2-4. On the table servers ────────────────────────────────────────────────
correr <- function(srv, args, pasos, salidas) {
  avisos <- character(0); caja <- new.env(); caja$filas <- "sin leer"; caja$hechos <- character(0)
  error <- tryCatch(withCallingHandlers(testServer(srv, args = args, {
    for (s in salidas) local({ s <- s; observe({ r <- session$returned[[s]]()
      if (!is.null(r)) caja$hechos <- c(caja$hechos, paste(s, r$id)) }) })
    session$flushReact()
    caja$filas <- tryCatch(session$returned$datos(), error = function(e) paste("error:", conditionMessage(e)))
    pasos(session)
  }), message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") }),
  error = function(e) conditionMessage(e))
  list(error = error, filas = caja$filas, hechos = caja$hechos, fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}
accion <- function(nombre, id, nonce) function(session) {
  session$setInputs(`tabla-accion` = list(accion = nombre, ids = id, nonce = nonce)); session$flushReact()
}
cli <- function(permitido) list(permitido = reactive(permitido), refresh = reactiveVal(0L))
con <- function(permitido, cid = "acme") list(permitido = reactive(permitido), cliente = reactiveVal(cid), refresh = reactiveVal(0L))

sembrar()
r <- correr(tiers_clientes_server, cli(TRUE), function(session) {
  assign(REG_KEY, rbind(cliente("zeta", 2L, 0L), get(REG_KEY, envir = .mem)), envir = .mem)
  accion("editar", "gamma", 1)(session); accion("contactos", "acme", 2)(session)
}, c("editar", "contactos"))
ok("clientes: HopDesk/Principal ven los 4, con la última actividad de acme",
   is.null(r$error) && identical(r$filas$id, c("acme", "beta", "gamma", "delta")) && !is.na(r$filas$ultima[1]))
ok("✎ en gamma y 📇 en acme los nombran por id, aunque otra sesión agregó uno antes",
   identical(r$hechos, c("editar gamma", "contactos acme")))

sembrar()
r <- correr(tiers_clientes_server, cli(FALSE), function(session) {
  accion("editar", "acme", 1)(session); accion("contactos", "acme", 2)(session)
}, c("editar", "contactos"))
ok("clientes sin HopDesk/Principal: 0 filas y ✎/📇 a mano no nombran nada",
   is.null(r$error) && is.null(r$filas) && !length(r$hechos))

sembrar()
r <- correr(tiers_contactos_server, con(TRUE), function(session) {
  assign(CON_KEY, rbind(contacto("c-9", "Nueva"), get(CON_KEY, envir = .mem)), envir = .mem)
  accion("desactivar", "c-2", 1)(session)
}, "desactivar")
ok("contactos: los activos del cliente abierto", is.null(r$error) && identical(r$filas$id, c("c-1", "c-2")))
ok("⊘ en c-2 lo nombra por id, aunque otra sesión agregó uno antes", identical(r$hechos, "desactivar c-2"))

sembrar()
r <- correr(tiers_contactos_server, con(FALSE), accion("desactivar", "c-1", 1), "desactivar")
ok("contactos sin HopDesk/Principal: 0 filas y ⊘ a mano no nombra nada",
   is.null(r$error) && is.null(r$filas) && !length(r$hechos))

sembrar(NULL, NULL)
r1 <- correr(tiers_clientes_server, cli(TRUE), function(s) s$flushReact(), character(0))
r2 <- correr(tiers_contactos_server, con(TRUE), function(s) s$flushReact(), character(0))
ok("arranque sin registro ni contactos: 0 filas y ninguna falla del motor",
   is.null(r1$error) && is.null(r2$error) && !nrow(r1$filas) && !nrow(r2$filas) && !length(c(r1$fallas, r2$fallas)))

sembrar(); .mem_falla <- c(REG_KEY, CON_KEY)
r1 <- correr(tiers_clientes_server, cli(TRUE), function(s) s$flushReact(), character(0))
r2 <- correr(tiers_contactos_server, con(TRUE), function(s) s$flushReact(), character(0))
ok("registro o contactos ilegibles: la sesión sigue y cada tabla avisa la falla (no 'no hay clientes/contactos')",
   is.null(r1$error) && is.null(r2$error) && length(r1$fallas) > 0L && length(r2$fallas) > 0L)

# ── 5. tiersServer's own observers, one click from start-up ─────────────────
cableado <- list()
buscar <- function(x) {
  if (!is.call(x)) return(invisible())
  txt <- paste(deparse(x[1:min(2L, length(x))], width.cutoff = 500L), collapse = " ")
  es_tabla <- identical(x[[1]], as.name("<-")) && is.call(x[[3]]) &&
    as.character(x[[3]][[1]])[1] %in% c("tiers_clientes_server", "tiers_contactos_server")
  es_obs <- identical(x[[1]], as.name("observeEvent")) && grepl("^observeEvent\\((clientes_t|contactos_t)\\$", txt)
  es_form <- identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name(".tiers_contactos_form"))
  if (es_tabla || es_obs || es_form) { cableado[[length(cableado) + 1L]] <<- x; return(invisible()) }
  xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) buscar(xs[[k]])
}
buscar(body(tiersServer))
ok("tiersServer crea las dos tablas y conecta ✎, 📇 y ⊘ (más el formulario de contactos)", length(cableado) == 6L)

clic <- function(id, accion, ids, permitido = TRUE) {
  sembrar()
  rec <- new.env(); rec$x <- character(0); anota <- function(v) rec$x <- c(rec$x, v)
  error <- tryCatch(suppressMessages(testServer(function(input, output, session) {
    is_principal <- reactive(permitido); is_hopdesk <- reactive(FALSE)
    clients_refresh <- reactiveVal(0L); contacts_refresh <- reactiveVal(0L); activity_refresh <- reactiveVal(0L)
    contacts_client_rv <- reactiveVal(NULL)
    shared <- list(current_user = function() "mouse", home_client_id = function() "hd-admin")
    read_client_registry <- function() REG; read_contacts <- function(cid) CONS
    showModal <- function(ui) { force(ui); anota("modal") }   # force first: the modal notes what it is
    modalDialog <- function(..., title = NULL) { anota(paste("editar:", paste(as.character(title), collapse = ""))); NULL }
    hd_modal <- function(..., titulo) { anota(paste("contactos:", isolate(contacts_client_rv()))); NULL }
    deactivate_contact <- function(cid, id) { anota(paste("desactivado", cid, id)); TRUE }
    showNotification <- function(...) invisible("id"); log_action <- function(...) invisible(NULL)
    for (x in cableado) eval(x)
    session$flushReact()
    if (identical(id, "contactos")) contacts_client_rv("acme")
    do.call(session$setInputs, stats::setNames(list(list(accion = accion, ids = ids, nonce = 1)), paste0(id, "-tabla-accion")))
    session$flushReact()
  }, {})), error = function(e) conditionMessage(e))
  list(error = error, x = rec$x)
}
r <- clic("clientes", "editar", "acme")
ok("un solo clic en ✎ desde el arranque abre el modal de editar ese cliente",
   is.null(r$error) && any(grepl("^editar:.*ACME", r$x)))
r <- clic("clientes", "contactos", "beta")
ok("un solo clic en 📇 desde el arranque abre los contactos de ese cliente", is.null(r$error) && "contactos: beta" %in% r$x)
r <- clic("contactos", "desactivar", "c-2")
ok("un solo clic en ⊘ (confirmado) desde el arranque desactiva ese contacto", is.null(r$error) && "desactivado acme c-2" %in% r$x)
r <- clic("clientes", "editar", "acme", permitido = FALSE)
ok("sin HopDesk/Principal, ✎ no abre nada", is.null(r$error) && !any(grepl("^editar:", r$x)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
