# =============================================================================
# tests/test_t87_m6_tablas_permiso.R
# T87-M6: the Usuarios screen (tiersServer, with its Actividad) is built in
# every session, and its tables pushed their rows to anyone's browser: Admin
# and Finanzas received the whole activity log with the tab hidden. Now a
# table whose module file is in HD_ARCHIVOS_PERMISO (R/tier_registry.R)
# travels only to a session that holds that permission
# (nivel_tabla_permitida_usuario(), fed by usuario_sesion() in app.R:
# effective tier, permissions resolved for it).
#   1. Admin without can_view_tiers gets no Usuarios/Actividad table; Dev does.
#   2. An Admin given can_view_tiers by hand gets them.
#   3. Staff at home gets them; staff in "ver como" gets what that tier gets
#      (as Admin: no; as Dev: yes) -- "ver como" holds.
#   4. Tables of other screens are untouched (the tier gate decides).
#   5. Fails closed: no user, no tier, an unknown tier, puede() missing.
#   6. The list's guardian: every file is a module of hd_registro, every
#      permission exists; app.R's gate is this function on usuario_sesion().
# Counter-proof: on 7227425b nivel_tabla_permitida_usuario does not exist
# and the canary's 3.0 (test_t87_frontera_canario.R) is PENDIENTE.
# Run from project root:  Rscript --vanilla tests/test_t87_m6_tablas_permiso.R
# =============================================================================
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R",
            "R/hd_table.R", "R/permisos.R", "R/tier_registry.R")) source(f)

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# A session's user as usuario_sesion() builds it: the effective tier, its
# permissions resolved (overrides only when not jumped; the client's config).
sesion <- function(tier, overrides = "{}", cfg = NULL, is_staff = FALSE)
  list(user = "x", tier = tier, is_staff = is_staff, permisos = permisos_resueltos(tier, overrides, cfg))

# Usuarios' tables: gated by a client permission (can_view_tiers). A table
# gated by an INTERNAL permission (T116 F2: mercado_inhabiles) is staff-only:
# no client ever receives it -- section 7.
puerta_de <- function(t) HD_ARCHIVOS_PERMISO[[t$archivo[1]]]
CON_PUERTA <- Filter(function(t) isTRUE(t$archivo[1] %in% names(HD_ARCHIVOS_PERMISO)), hd_registro)
INTERNAS  <- names(Filter(function(t) puerta_de(t) %in% permisos_internos(), CON_PUERTA))
PANTALLA  <- setdiff(names(CON_PUERTA), INTERNAS)
OTRAS    <- c("agenda_pagos", "libro_banco", "requisiciones", "empresas")
ADMIN <- "admin"; DEV <- nivel_ti(); STAFF <- setdiff(niveles_staff(), nivel_tope())[1]

cat("== 1. Admin sin el permiso no; Dev sí ==\n")
ok("1.0 la pantalla tiene sus tablas (usuarios y actividad entre ellas)", all(c("usuarios", "actividad") %in% PANTALLA))
ok("1.1 Admin sin can_view_tiers no recibe ninguna",
   !any(vapply(PANTALLA, function(t) nivel_tabla_permitida_usuario(sesion(ADMIN), t), logical(1))))
ok("1.2 Finanzas tampoco",
   !any(vapply(PANTALLA, function(t) nivel_tabla_permitida_usuario(sesion("finance"), t), logical(1))))
ok("1.3 Dev las recibe", all(vapply(PANTALLA, function(t) nivel_tabla_permitida_usuario(sesion(DEV), t), logical(1))))

cat("== 2. Admin con el permiso dado a mano ==\n")
ok("2.1 las recibe", all(vapply(PANTALLA, function(t)
  nivel_tabla_permitida_usuario(sesion(ADMIN, '{"can_view_tiers": true}'), t), logical(1))))

cat("== 3. Staff, en casa y en \"ver como\" ==\n")
ok("3.1 staff en casa las recibe", all(vapply(PANTALLA, function(t)
  nivel_tabla_permitida_usuario(sesion(STAFF, is_staff = TRUE), t), logical(1))))
ok("3.2 Principal en casa las recibe", nivel_tabla_permitida_usuario(sesion(nivel_tope(), is_staff = TRUE), "actividad"))
# In a jump usuario_sesion() resolves the viewed tier, without overrides.
ok("3.3 staff viendo como Admin: no", !nivel_tabla_permitida_usuario(sesion(ADMIN, is_staff = TRUE), "actividad"))
ok("3.4 staff viendo como Dev: sí", nivel_tabla_permitida_usuario(sesion(DEV, is_staff = TRUE), "actividad"))

cat("== 4. Las demás pantallas, como antes ==\n")
ok("4.1 Admin sigue recibiendo lo de sus pestañas", all(vapply(OTRAS, function(t)
  identical(nivel_tabla_permitida_usuario(sesion(ADMIN), t), nivel_tabla_permitida(ADMIN, t)), logical(1))))
ok("4.2 Compras, como dice su lista de pestañas", all(vapply(c(OTRAS, "actividad"), function(t)
  identical(nivel_tabla_permitida_usuario(sesion("compras"), t), suppressMessages(nivel_tabla_permitida("compras", t))),
  logical(1))))

cat("== 5. Falla cerrado ==\n")
ok("5.1 sin usuario", !nivel_tabla_permitida_usuario(NULL, "agenda_pagos"))
ok("5.2 sin nivel", !nivel_tabla_permitida_usuario(list(tier = "", permisos = list()), "agenda_pagos"))
ok("5.3 nivel desconocido", !nivel_tabla_permitida_usuario(list(tier = "no_es_nivel", permisos = list(can_view_tiers = TRUE)), "actividad"))
.puede <- puede; rm(puede)
ok("5.4 sin puede() (fixture sin permisos.R): no, y no revienta",
   identical(tryCatch(nivel_tabla_permitida_usuario(sesion(DEV), "actividad"), error = function(e) "error"), FALSE))
puede <- .puede

cat("== 6. La guardiana de la lista ==\n")
archivos <- unique(unlist(lapply(hd_registro, function(t) t$archivo[1])))
ok("6.1 cada archivo de la lista es un módulo de hd_registro", all(names(HD_ARCHIVOS_PERMISO) %in% archivos))
ok("6.2 cada permiso de la lista existe", all(unlist(HD_ARCHIVOS_PERMISO) %in% permisos_claves()))
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
ok("6.3 la puerta de app.R es esta función, sobre usuario_sesion()",
   any(grepl("hd_tabla_permitida <- function(tabla) nivel_tabla_permitida_usuario(usuario_sesion(), tabla)", app, fixed = TRUE)))

cat("== 7. Tablas con puerta interna: solo staff ==\n")
ok("7.0 mercado_inhabiles es una de ellas", "mercado_inhabiles" %in% INTERNAS)
for (t in INTERNAS) {
  k <- sprintf('{"%s": true}', puerta_de(hd_registro[[t]]))
  ok(sprintf("7.1 %s: ningún nivel cliente la recibe, ni con el permiso forzado a mano", t),
     !any(vapply(niveles_cliente(), function(n) nivel_tabla_permitida_usuario(sesion(n), t) ||
                   nivel_tabla_permitida_usuario(sesion(n, k), t), logical(1))))
  ok(sprintf("7.2 %s: staff en casa sí", t), nivel_tabla_permitida_usuario(sesion(STAFF, is_staff = TRUE), t))
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
