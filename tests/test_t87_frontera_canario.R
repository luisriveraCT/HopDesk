# =============================================================================
# tests/test_t87_frontera_canario.R
# T87: the wall between HopDesk staff and a client, measured on WHAT LEAVES a
# client session -- never on how the code is written. A staff "canary" (an
# account and actions with unmistakable markers) works inside client acme;
# then sessions of acme's own users (Dev, Admin, Finanzas) run the real
# Usuarios tab (tiersServer: Usuarios, Actividad and its detail, Invitaciones,
# Configuración de niveles), and everything the session sends to the browser
# is captured: every custom message (the hd_table rows), every modal, every
# notification, every output, and the page head. Nothing may carry:
#   - the canary's user name, surname or account code;  "hd-admin";
#     "home_client"; another client's data; the name of an internal
#     permission (anywhere in the page, the dictionary included);
#   - the email service's internals (RESEND_API_KEY, .Renviron, the runbook);
#   - for a user who cannot see Usuarios: the Usuarios tab itself, the
#     activity log's rows (M6: tiersServer runs in every session and its
#     tables push their data even when the tab is hidden), or the name of a
#     permission he does not hold.
# And it MUST carry (Luis, 2026-11-24, T87_regla.md and the paso 1 inventory):
#   - the canary's own actions that a client user could also do, signed
#     "<first name> · <tr('staff.etiqueta')>";
#   - one neutral line for the canary's access (entry/exit);
#   - the subscription lines (C-neutral): the limit change and the account
#     deactivated by it, signed by nobody;
#   - never an action of the staff's own (S): "ver como", the access process,
#     contacts...
# Contraprueba: written on 7227425b, where it fails in A1 (Actividad shows the
# staff), M1 (whole dictionary), M2 (email internals) and M5 (permission
# names, the Usuarios tab built for everyone) and M6 (the activity log sent to
# Admin and Finanzas).
#
# Uses tests/helpers/tiers_fixture_app.R (all of R/ over S3 in memory; nothing
# reaches AWS). The canary's actions are written through the real
# log_action(), as their call sites write them today; when the catalog step
# changes how a staff action is written, these calls follow the new writer.
# Run from project root:  Rscript --vanilla tests/test_t87_frontera_canario.R
# =============================================================================
cat("=== test_t87_frontera_canario ===\n\n")
# The fixture blocks aws.s3 in this process (no network); it must never run
# inside a live R session, where that would leak (2026-08-04 incident).
if (interactive()) stop("Run this test with Rscript, never inside an R session.")
suppressPackageStartupMessages({ library(shiny); library(jsonlite) })

# DEUDA: the cases that fail on master today, each with its finding. A case
# here that fails is PENDIENTE (the battery stays green); one that passes
# while still listed FAILS: take it out of the list in the same commit that
# fixes it. The list only goes down (as T72's DEUDA_CAMINO); a new case is
# never added to it.
DEUDA <- c(
  "1.1"         = "A1: Actividad muestra al staff (usuario, home_client: hd-admin)",
  "1.2"         = "A1: lo del staff no sale firmado '<primer nombre> · HD Staff'",
  "1.3"         = "A1: la entrada/salida del staff está escondida o no existe",
  "1.4"         = "catálogo S/C: acciones propias del staff en el log del cliente",
  "1.6"         = "C-neutral: la suscripción sale con el usuario del staff",
  "1.7"         = "A1: el detalle vuelca la metadata (home_client: hd-admin)",
  "1.8"         = "M2: Invitaciones nombra RESEND_API_KEY / .Renviron / runbook",
  "2.1"         = "M1: el diccionario completo va en la página",
  "3.1 admin"   = "M5: la pestaña Usuarios se construye para todos",
  "3.2 admin"   = "M5: el rechazo nombra el permiso que falta",
  "3.3 admin"   = "M1 (diccionario) y un home_client en lo enviado: rastro del staff en la sesión",
  "3.1 finance" = "M5",
  "3.2 finance" = "M5",
  "3.3 finance" = "M1")

.pass <- 0L; .fail <- 0L; .pend <- 0L
ok <- function(label, cond, detalle = NULL) {
  id <- if (grepl("^[0-9.]+ [a-z]+:", label)) sub(":.*$", "", label) else sub(" .*$", "", label)
  en_deuda <- id %in% names(DEUDA)
  if (isTRUE(cond) && !en_deuda) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L; return(invisible()) }
  if (isTRUE(cond) && en_deuda) {
    cat(" FAIL:", label, "\n        ya pasa: quita", sQuote(id), "de DEUDA (solo baja)\n")
    .fail <<- .fail + 1L
    return(invisible())
  }
  if (en_deuda) {
    cat(" PENDIENTE (DEUDA):", label, "--", DEUDA[[id]], "\n")
    if (nzchar(Sys.getenv("T87_DEPURAR")) && length(detalle))
      cat("       ", paste(utils::head(detalle, 6), collapse = "\n        "), "\n")
    .pend <<- .pend + 1L
    .vistos <<- c(.vistos, id)
    return(invisible())
  }
  cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
  if (length(detalle)) cat("       ", paste(utils::head(detalle, 6), collapse = "\n        "), "\n")
}
.vistos <- character()

Sys.setenv(TIERS_ROL = "dev")
invisible(capture.output(source("tests/helpers/tiers_fixture_app.R", local = globalenv())))

# ── The canary ──────────────────────────────────────────────────────────────
CANARIO <- list(user = "zz_canario_staff", nombre = "Zelda Kanaryx Qwertzil", code = "S9999",
                primer = "Zelda", apellidos = c("Kanaryx", "Qwertzil"))
.mem_put <- function(key, df) assign(key, df, envir = .mem)
.mem_get <- function(key) get(key, envir = .mem)
.mem_put("hd-admin/usuarios.rds", rbind(.mem_get("hd-admin/usuarios.rds"),
  usuario(CANARIO$user, "hopdesk", "hd-admin", CANARIO$nombre, code = CANARIO$code)))
.mem_put("acme/usuarios.rds", rbind(.mem_get("acme/usuarios.rds"),
  usuario("fede", "finance", "acme", "Fede Finanzas")))
suppressWarnings(rm(list = intersect(ls(.mem), "acme/app_audit.rds"), envir = .mem))
# The activity log is read by listing its chunks (read_audit_log()); the
# fixture lists nothing. Here the listing is the in-memory store's own keys.
utils::assignInNamespace("get_bucket", function(bucket, prefix = "", ...) {
  k <- ls(.mem); k <- k[startsWith(k, prefix)]
  lapply(k, function(x) list(Key = x))
}, "aws.s3")
# The real delivery status (the fixture stubs it), with no key configured:
# what a client Dev sees when the service is not live.
for (e in parse("R/email_service.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("email_service_status")))
    eval(e, envir = globalenv())
Sys.unsetenv("RESEND_API_KEY")

# Another client's markers (beta, in the fixture): never in an acme session.
AJENOS <- c("x@beta.mx", "Beta Uno", "Beta Transportes", "admin@beta.mx", "tok-beta-1")

# The label for staff, from the dictionary once it exists.
ETIQUETA <- tryCatch({ e <- i18n_dict("es")[["staff.etiqueta"]]; if (is.null(e)) "HD Staff" else e },
                     error = function(e) "HD Staff")

# ── What the canary does inside acme (log_action as each call site does) ────
canario_hace <- function() {
  log_action(CANARIO$user, "clientes", "client_access",
             sprintf("Staff switched active context to '%s'", "acme"), target_id = "acme",
             client_id = "hd-admin", viewer_home_client_id = "hd-admin")
  log_action(CANARIO$user, "clientes", "external_access",
             "HopDesk staff accessed this client's data (session context switch)", target_id = "acme",
             client_id = "acme", viewer_home_client_id = "hd-admin")
  # C: something an acme user can do too.
  log_action(CANARIO$user, "notes", "eliminar_nota", "Nota eliminada: Cierre de noviembre",
             target_id = "nota-1", metadata = list(ledger = "AR"),
             client_id = "acme", viewer_home_client_id = "hd-admin")
  # S: the staff's own.
  log_action(CANARIO$user, "niveles", "ver_como", sprintf("%s (como %s)", CANARIO$user, nivel_nombre("finance")),
             target_id = "finance", metadata = list(nivel = "finance"),
             client_id = "acme", viewer_home_client_id = "hd-admin")
  log_action("mouse", "tiers", "hop_access_approved",
             sprintf("%s aprobó acceso de %s a %s (%s)", "Mouse", CANARIO$user, "acme", "24h"),
             target_id = "hop-9", metadata = list(client_id = "acme", grantee = CANARIO$user),
             client_id = "acme", viewer_home_client_id = "hd-admin")
  log_action(CANARIO$user, "tiers", "agregar_contacto",
             "Contacto 'Lupe' (lupe@acme.mx) agregado a 'acme'", target_id = "c-9",
             client_id = "acme", viewer_home_client_id = "hd-admin")
  # C-neutral: the subscription (Luis): seen by the client, signed by nobody.
  log_action(CANARIO$user, "tiers", "cambio_limite_usuarios",
             sprintf("Límite cambiado de %d a %d por %s", 5L, 4L, CANARIO$user), target_id = "acme",
             metadata = list(old_limit = 5L, new_limit = 4L),
             client_id = "acme", viewer_home_client_id = "hd-admin")
  log_action(CANARIO$user, "tiers", "user_deactivated_by_limit_reduction",
             paste0("Cuenta 'caro' desactivada por reducción de límite (", CANARIO$user, ")"),
             target_id = "caro", client_id = "acme", viewer_home_client_id = "hd-admin")
  log_action(CANARIO$user, "clientes", "client_exit", "Staff left 'acme'", target_id = "acme",
             client_id = "hd-admin", viewer_home_client_id = "hd-admin")
  invisible(.audit_flush_ahora())
}
canario_hace()

# The staff's own actions (S): from the catalog once it exists.
ACCIONES_S <- if (exists("ACCIONES_STAFF")) names(Filter(function(a) identical(a$clase, "S"), ACCIONES_STAFF)) else
  c("client_access", "client_exit", "ver_como", "ctx_load_fallido", "hop_access_requested", "hop_access_approved",
    "hop_access_denied", "hop_access_revoked", "hop_access_direct_grant", "agregar_contacto",
    "desactivar_contacto", "aprobacion_cliente", "emergency_lock", "emergency_unlock")

# ── Everything a session sends to the browser ───────────────────────────────
.salida <- new.env()
.anotar <- function(tipo, x) {
  txt <- tryCatch(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, force = TRUE, null = "null")),
                  error = function(e) paste(deparse(x), collapse = " "))
  .salida$todo <- c(.salida$todo, txt)
  .salida[[tipo]] <- c(.salida[[tipo]], list(x))
}
# The session's user as app.R's usuario_sesion() builds it for a client user
# (own tier, own overrides from usuarios.rds).
sesion_usuario <- function() {
  us <- .mem_get("acme/usuarios.rds")
  ov <- us$permisos[us$username == quien$user][1] %||% "{}"
  list(user = quien$user, tier = quien$tier, is_staff = FALSE, viendo_como = FALSE,
       permisos = permisos_resueltos(quien$tier, ov))
}
# The canary measures app.R's gate only if it reproduces it: once the M6 gate
# exists, app.R must be using it on usuario_sesion().
if (exists("nivel_tabla_permitida_usuario"))
  stopifnot("app.R no usa la puerta de T87-M6; el canario ya no la reproduce" = any(grepl(
    "nivel_tabla_permitida_usuario(usuario_sesion(), tabla)", readLines("app.R", warn = FALSE), fixed = TRUE)))
# The test session's own senders, replaced before the app's server runs, so
# nothing it sends (from its very first flush) goes unseen.
server_capturado <- function(input, output, session) {
  session$sendCustomMessage <- function(type, message) .anotar(paste0("msg:", type), message)
  session$sendModal         <- function(type, message) .anotar("modal", message)
  session$sendNotification  <- function(type, message) .anotar("notif", message)
  session$sendInsertUI      <- function(selector, multiple, where, content, ...) .anotar("insert", content)
  # The table gate app.R puts in every session (the fixture has none): as
  # app.R builds it -- the T87-M6 one once it exists, the tier's (T69) before.
  u <- sesion_usuario()
  session$userData$hd_tabla_permitida <- if (exists("nivel_tabla_permitida_usuario"))
    function(tabla) nivel_tabla_permitida_usuario(u, tabla) else function(tabla) nivel_tabla_permitida(u$tier, tabla)
  server(input, output, session)
}

# Outputs the Usuarios tab and its Actividad define (read straight from the
# source, so a new output is measured too).
.outputs_de <- function(archivo, prefijo) {
  x <- readLines(archivo, warn = FALSE, encoding = "UTF-8")
  n <- unique(sub("^.*output\\$([A-Za-z0-9_]+).*$", "\\1", grep("output\\$[A-Za-z0-9_]+\\s*<-", x, value = TRUE)))
  paste0(prefijo, n)
}
OUTPUTS <- c(.outputs_de("R/tiers_module.R", "tiers-"),
             .outputs_de("R/audit_log_viewer_module.R", "tiers-activity_viewer-"))

# An output's value without shiny's getOutput(), which waits forever on a
# render that never resolves: the render's promise, given a bounded wait.
leer_output <- function(session, nombre, espera = 5) {
  outs <- session$.__enclos_env__$private$outs
  o <- outs[[nombre]]
  if (is.null(o)) return(NULL)
  if (is.null(o$promise)) { session$flushReact(); o <- outs[[nombre]] }
  if (is.null(o$promise)) return(NULL)
  val <- NULL; listo <- FALSE
  promises::then(o$promise, function(v) { val <<- v; listo <<- TRUE }, function(e) { listo <<- TRUE })
  fin <- Sys.time() + espera
  while (!listo && Sys.time() < fin) later::run_now(0.05)
  if (!listo) .paso(paste("  output", nombre, "no se resolvió"))
  v <- val
  if (is.list(v) && !is.null(v$val)) v$val else v
}
.paso <- function(x) if (nzchar(Sys.getenv("T87_DEPURAR"))) message("[t87] ", format(Sys.time(), "%H:%M:%S"), " ", x)
# One session of acme's `user` (tier from the fixture's users), all captured.
correr <- function(user, tier, como_ve = function(...) NULL) {
  rm(list = ls(.salida), envir = .salida); .salida$todo <- character()
  quien <<- list(user = user, tier = tier, is_staff = FALSE, home = "acme")
  out <- list()
  testServer(server_capturado, {
    .paso("flush")
    session$flushReact()
    for (o in OUTPUTS) {
      .paso(o)
      v <- leer_output(session, o)
      if (!is.null(v)) { out[[o]] <<- v; .anotar(paste0("out:", o), v) }
    }
    # Open the detail of every Actividad row the browser received.
    for (m in .salida[["msg:hdt-datos"]]) {
      if (!grepl("activity_viewer-audit_t-hdt$", m$id %||% "")) next
      ids <- as.character(unlist(m$filas$ids))
      ns_t <- sub("-hdt$", "", m$id)
      for (i in seq_along(ids)) {
        do.call(session$setInputs, stats::setNames(list(list(accion = "detalle", ids = ids[i], nonce = i)),
                                                  paste0(ns_t, "-accion")))
      }
    }
    como_ve(session)
  })
  list(todo = paste(.salida$todo, collapse = "\n"), salida = .salida, outputs = out)
}

# The Actividad rows the browser got: one text per row (every cell of it, as
# the engine sends them: filas$ids and filas$celdas[[col]][[part]][i]).
filas_actividad <- function(r) {
  ms <- Filter(function(m) grepl("activity_viewer-audit_t-hdt$", m$id %||% ""), r$salida[["msg:hdt-datos"]])
  if (!length(ms)) return(NULL)
  f <- ms[[length(ms)]]$filas
  ids <- as.character(unlist(f$ids))
  vapply(seq_along(ids), function(i) paste(c(ids[i], unlist(lapply(f$celdas, function(col)
    lapply(col, function(parte) { x <- unlist(parte); if (length(x) >= i) as.character(x[i]) })))),
    collapse = " | "), character(1))
}
ids_actividad <- function(r) {
  ms <- Filter(function(m) grepl("activity_viewer-audit_t-hdt$", m$id %||% ""), r$salida[["msg:hdt-datos"]])
  if (!length(ms)) character() else as.character(unlist(ms[[length(ms)]]$filas$ids))
}
hay <- function(texto, patrones) {
  h <- patrones[vapply(patrones, function(p) grepl(p, texto, fixed = TRUE), logical(1))]
  # With its context, so a failure says where it came from.
  vapply(h, function(p) {
    i <- regexpr(p, texto, fixed = TRUE)
    paste0(p, "  <<", gsub("[[:space:]]+", " ", substr(texto, max(1, i - 90), i + 60)), ">>")
  }, character(1), USE.NAMES = FALSE)
}

INTERNOS <- vapply(permisos_internos(), function(k) tr(paste0("permisos.", k)), character(1))
PROHIBIDO <- c(CANARIO$user, CANARIO$apellidos, CANARIO$code, "hd-admin", "home_client", AJENOS, INTERNOS)

# ── 1. Dev of acme (sees Usuarios and Actividad) ────────────────────────────
cat("== 1. Dev del cliente ==\n")
r <- correr("devacme", "dev")
ok("1.0 la sesión mandó la tabla de Actividad (el canario mide algo)", !is.null(filas_actividad(r)))
ok("1.1 nada del staff ni de otro cliente sale al navegador (A1)", !length(hay(r$todo, PROHIBIDO)),
   hay(r$todo, PROHIBIDO))
txt_fila <- filas_actividad(r) %||% character()
.paso(paste("Actividad:", paste(txt_fila, collapse = "
  ")))
ok(sprintf("1.2 lo que el staff hizo como cliente sale firmado '%s · %s'", CANARIO$primer, ETIQUETA),
   any(grepl(paste(CANARIO$primer, "·", ETIQUETA), txt_fila, fixed = TRUE) &
         grepl("eliminar_nota", txt_fila, fixed = TRUE)))
ok("1.3 su acceso sale en una línea neutral (entrada y salida)",
   any(grepl(CANARIO$primer, txt_fila, fixed = TRUE) & !grepl("eliminar_nota", txt_fila, fixed = TRUE) &
         !grepl("cambio_limite_usuarios|user_deactivated", txt_fila)))
ok("1.4 ninguna acción propia del staff en el log del cliente",
   !any(vapply(ACCIONES_S, function(a) any(grepl(paste0("\\b", a, "\\b"), txt_fila)), logical(1))),
   ACCIONES_S[vapply(ACCIONES_S, function(a) any(grepl(paste0("\\b", a, "\\b"), txt_fila)), logical(1))])
ok("1.5 la suscripción sí sale: el cambio de límite y la cuenta desactivada (C-neutral)",
   any(grepl("cambio_limite_usuarios", txt_fila, fixed = TRUE)) &&
     any(grepl("user_deactivated_by_limit_reduction", txt_fila, fixed = TRUE)))
neutras <- txt_fila[grepl("cambio_limite_usuarios|user_deactivated", txt_fila)]
ok("1.6 ...y sin persona: ni el usuario, ni el nombre, ni el código del staff",
   length(neutras) > 0 && !length(hay(paste(neutras, collapse = " "), c(CANARIO$user, CANARIO$primer, CANARIO$apellidos, CANARIO$code))),
   hay(paste(neutras, collapse = " "), c(CANARIO$user, CANARIO$primer, CANARIO$apellidos, CANARIO$code)))
ok("1.7 abrir el detalle de cada línea no saca nada del staff", length(r$salida[["modal"]]) > 0 &&
     !length(hay(paste(vapply(r$salida[["modal"]], function(m) paste(unlist(m), collapse = " "), ""), collapse = " "),
                 PROHIBIDO)))
mail <- paste(unlist(r$outputs[grepl("inv_estado_correo", names(r$outputs))]), collapse = " ")
ok("1.8 Invitaciones no enseña la operación del correo de HopDesk (M2)",
   !length(hay(mail, c("RESEND", ".Renviron", "RUNBOOK", "docs/"))), hay(mail, c("RESEND", ".Renviron", "RUNBOOK", "docs/")))

# ── 2. The page head (the dictionary) for a client ──────────────────────────
cat("== 2. La cabecera de la página ==\n")
head_html <- paste(as.character(htmltools::renderTags(tiersUI("tiers"))$head), collapse = "\n")
claves_internas <- c(paste0("permisos.", permisos_internos()), paste0("niveles.nombre.", niveles_staff()))
ok("2.1 el diccionario del cliente no trae niveles ni permisos internos (M1)",
   !length(hay(head_html, paste0('"', claves_internas, '"'))), hay(head_html, paste0('"', claves_internas, '"')))

# ── 3. Users who cannot see Usuarios (Admin without it, Finanzas) ───────────
cat("== 3. Quien no ve Usuarios ==\n")
for (q in list(c("ana", "admin"), c("fede", "finance"))) {
  r <- correr(q[1], q[2], como_ve = function(session) {
    u <- list(user = q[1], tier = q[2], permisos = permisos_resueltos(q[2], "{}"))
    sh <- list(usuario = function() u)
    falta <- setdiff(perm_visible_keys(FALSE), names(Filter(isTRUE, u$permisos)))
    for (k in falta) puede_o_avisa(sh, k, session = session)
  })
  u_perm <- permisos_resueltos(q[2], "{}")
  if (isTRUE(u_perm[["can_view_tiers"]])) next
  ok(sprintf("3.0 %s: el log de Actividad no viaja a su navegador (M6)", q[2]), !length(ids_actividad(r)),
     sprintf("%d filas de Actividad enviadas", length(ids_actividad(r))))
  tabs <- r$outputs[["tiers-main_tabs_ui"]]
  ok(sprintf("3.1 %s: la pestaña Usuarios no se construye", q[2]),
     is.null(tabs) || !nzchar(paste(unlist(tabs), collapse = "")))
  notif <- paste(vapply(r$salida[["notif"]] %||% list(), function(m) paste(unlist(m), collapse = " "), ""), collapse = " ")
  sin <- setdiff(perm_visible_keys(FALSE), names(Filter(isTRUE, u_perm)))
  nombres <- vapply(sin, function(k) tr(paste0("permisos.", k)), character(1))
  ok(sprintf("3.2 %s: ningún aviso nombra un permiso que no tiene (M5)", q[2]),
     length(r$salida[["notif"]]) > 0 && !length(hay(notif, nombres)), hay(notif, nombres))
  ok(sprintf("3.3 %s: nada del staff ni de otro cliente", q[2]), !length(hay(r$todo, PROHIBIDO)), hay(r$todo, PROHIBIDO))
}

# Every DEUDA entry must name a case that ran (a stale entry would hide a
# future regression of that same case).
fantasmas <- setdiff(names(DEUDA), .vistos)
ok("9.0 cada entrada de DEUDA nombra un caso que corrió y sigue en rojo", !length(fantasmas), fantasmas)
cat(sprintf("\n=== results: %d passed, %d failed (pendientes en DEUDA: %d) ===\n", .pass, .fail, .pend))
if (.fail > 0L) quit(status = 1L)
