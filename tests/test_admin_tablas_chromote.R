# =============================================================================
# tests/test_admin_tablas_chromote.R
# The eight Administración tables (T28) in a real headless Chrome, on
# tests/helpers/tiers_fixture_app.R (the real tiersUI/tiersServer over S3 in
# memory, no network), at Luis's window (1293 x 632), with real mouse clicks.
#   A1  Principal, Seguridad first and then every other tab: no table stays
#       "Cargando…" (the engine used to arrive with the first table drawn
#       and the others lost their first message), each with its rows;
#   A2  🔓 on beto in Seguridad: that lock goes, the other stays;
#   A3  ⊘ on an invitation asks in the bar; Confirmar revokes only that one;
#   A4  📇 on acme opens its contacts (2) in the big modal, form below;
#   A5  ✎ on carla in Usuarios opens the editor on carla;
#   B1  HopDesk: Seguridad is Principal's; Accesos without Usuario nor ⊘;
#   C1  a client's Dev: only its own client's invitations, no staff tabs.
# Screenshots to T28_CAPTURAS if set.
#
# Run from project root:  Rscript tests/test_admin_tablas_chromote.R
# =============================================================================
cat("=== test_admin_tablas_chromote ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("tests/helpers/live_app_harness.R")
repo <- normalizePath(".", winslash = "/")
PORT <- 39091L
CAPTURAS <- Sys.getenv("T28_CAPTURAS", "")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)

abrir_app <- function(rol) {
  try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app)
  app <<- start_bg_app(sprintf('Sys.setenv(TIERS_ROL = "%s"); setwd("%s"); source("tests/helpers/tiers_fixture_app.R")$value',
                               rol, repo), PORT, timeout_s = 120)
  b <<- new_chromote_session()
  b$Emulation$setDeviceMetricsOverride(width = 1293L, height = 632L, deviceScaleFactor = 1, mobile = FALSE)
  b$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT)); b$Page$loadEventFired(wait_ = TRUE)
  espera(function() isTRUE(ev("!!document.querySelector('a[data-value=\"usuarios\"]')")), 60)
}
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
espera <- function(f, s = 15) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = s)
raton <- function(tipo, p, botones = 0L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved" && !botones) "none" else "left",
                             buttons = botones, clickCount = 1L)
clic <- function(js_el) {
  p <- ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
    var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p); Sys.sleep(0.4)
}
pestana <- function(valor) { clic(sprintf("document.querySelector('a[data-value=\"%s\"]')", valor)); Sys.sleep(0.6) }
tabla   <- function(nombre) sprintf("tiers-%s-tabla-hdt", nombre)
vis     <- function(nombre) unlist(ev(sprintf("(HDT.estado('%s') || {}).visibles", tabla(nombre))))
cargando <- function() unlist(ev("Array.from(document.querySelectorAll('.tab-pane.active .hdt')).filter(function(e){ return !!e.querySelector('.hdt-cargando'); }).map(function(e){ return e.id; })"))
boton   <- function(nombre, fila, accion)
  sprintf("HDT.fila('%s', '%s').querySelector('button[data-accion=\"%s\"]')", tabla(nombre), fila, accion)
titulos <- function(nombre) unlist(ev(sprintf("Array.from(document.querySelectorAll('#%s thead th')).map(function(t){ return t.textContent.trim(); })", tabla(nombre))))
captura <- function(n) if (nzchar(CAPTURAS)) { dir.create(CAPTURAS, showWarnings = FALSE, recursive = TRUE); b$screenshot(file.path(CAPTURAS, n), show = FALSE) }
carga <- function(nombre, ids) espera(function() setequal(vis(nombre), ids))

# ── Principal ────────────────────────────────────────────────────────────────
ok("la app de prueba arranca (Principal)", { abrir_app("principal"); TRUE })

# A1: Seguridad first (the order that left the others on "Cargando…").
pestana("security")
ok("A1 Seguridad: los 2 bloqueos", carga("bloq", c("beto", "exempleado")))
pestana("clients")
ok("A1 Clientes después de Seguridad: solicitudes (2) y clientes (3) cargan",
   carga("solic", c("req-1", "req-2")) && carga("clientes", c("acme", "beta", "gamma")))
ok("A1 Clientes: ninguna tabla en 'Cargando…'", !length(cargando()))
captura("A1_principal_clientes.png")
pestana("invites")
ok("A1 Invitaciones: las 3 pendientes de todos y la cola (2)",
   carga("invit", c("tok-acme-1", "tok-acme-2", "tok-beta-1")) && carga("cola", c("q-1", "q-2")) && !length(cargando()))
pestana("hop_perms")
ok("A1 Salto: los 2 accesos activos, con Usuario", carga("accesos", c("g-1", "g-2")) && "Usuario" %in% titulos("accesos"))
pestana("usuarios")
ok("A1 Usuarios: las 4 cuentas de acme, con '#' para staff",
   carga("usuarios", c("devacme", "ana", "beto", "caro")) && "#" %in% titulos("usuarios"))

# A2: 🔓 on beto.
pestana("security"); carga("bloq", c("beto", "exempleado"))
clic(boton("bloq", "beto", "desbloquear"))
ok("A2 🔓 en beto: su bloqueo se va y el otro se queda", carga("bloq", "exempleado"))

# A3: ⊘ on an invitation confirms in the bar.
pestana("invites"); carga("invit", c("tok-acme-1", "tok-acme-2", "tok-beta-1"))
clic(boton("invit", "tok-acme-2", "revocar"))
ok("A3 ⊘ pide confirmar en la barra (aún no se revoca)",
   espera(function() isTRUE(ev(sprintf("!!document.querySelector('#%s .hdt-confirmar-si')", tabla("invit"))))) &&
     setequal(vis("invit"), c("tok-acme-1", "tok-acme-2", "tok-beta-1")))
captura("A3_principal_revocar.png")
clic(sprintf("document.querySelector('#%s .hdt-confirmar-si')", tabla("invit")))
ok("A3 Confirmar revoca solo esa invitación", carga("invit", c("tok-acme-1", "tok-beta-1")))

# A4: 📇 on acme.
pestana("clients"); carga("clientes", c("acme", "beta", "gamma"))
clic(boton("clientes", "acme", "contactos"))
ok("A4 📇 en acme abre sus contactos (2) en el modal, con el formulario abajo",
   espera(function() isTRUE(ev("!!document.querySelector('.modal.show .hd-modal')"))) && carga("contactos", c("c-1", "c-2")) &&
     isTRUE(ev("!!document.querySelector('.modal.show input[id$=\"contact_name\"]')")))
captura("A4_principal_contactos.png")
clic("document.querySelector('.modal.show .hd-modal-cerrar')"); espera(function() !isTRUE(ev("!!document.querySelector('.modal.show')")))

# A5: ✎ on caro in Usuarios.
pestana("usuarios"); carga("usuarios", c("devacme", "ana", "beto", "caro"))
clic(boton("usuarios", "caro", "editar"))
editor <- function() ev("(document.getElementById('tiers-perm_editor_ui') || {}).textContent || ''")
ok("A5 ✎ en caro abre el editor de caro",
   espera(function() grepl("caro", editor(), fixed = TRUE)) || { message("  editor: ", substr(gsub("\\s+", " ", editor()), 1, 300)); FALSE })
captura("A5_principal_usuarios.png")

# ── HopDesk ──────────────────────────────────────────────────────────────────
abrir_app("hopdesk")
pestana("security")
ok("B1 HopDesk: Seguridad es solo de Principal (aviso, sin tabla)",
   espera(function() grepl("Principal", ev("document.querySelector('.tab-pane.active').textContent"), fixed = TRUE)) &&
     !isTRUE(ev(sprintf("!!document.getElementById('%s')", tabla("bloq")))))
pestana("hop_perms")
ok("B1 HopDesk: solo sus accesos (bunny), sin columna Usuario ni ⊘",
   carga("accesos", c("g-1", "g-2")) && !"Usuario" %in% titulos("accesos") &&
     !isTRUE(ev(sprintf("!!document.querySelector('#%s button[data-accion=\"revocar\"]')", tabla("accesos")))))

# ── A client's Dev ───────────────────────────────────────────────────────────
abrir_app("dev")
ok("C1 Dev: sin pestañas de staff (Seguridad, Clientes, Salto)",
   !isTRUE(ev("!!document.querySelector('a[data-value=\"security\"], a[data-value=\"clients\"], a[data-value=\"hop_perms\"]')")))
pestana("invites")
ok("C1 Dev: solo las invitaciones de su empresa, y sin la cola de correos",
   carga("invit", c("tok-acme-1", "tok-acme-2")) && !isTRUE(ev(sprintf("!!document.getElementById('%s')", tabla("cola")))))
captura("C1_dev_invitaciones.png")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
