# =============================================================================
# tests/test_alertas_pantalla.R
# Alertas del equipo, the screen (R/avisos_alertas_module.R), in testServer on
# the in-memory S3 of tests/helpers/s3_memoria.R, with each level's real
# permissions (permisos_resueltos()):
#   P  admin (can_publish_panorama): sees the client's users, never staff;
#      ✎ + Guardar saves that user's marks and logs it;
#   N  finance (without it): hidden, no rows, and neither ✎ nor a forced
#      Guardar writes anything; told no;
#   F  a failed save writes nothing, says so, keeps the modal and logs
#      configurar_alertas_fallido; a failed read is the table's notice,
#      never an empty matrix.
# Run from project root:  Rscript tests/test_alertas_pantalla.R
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/hd_registro.R", "R/hd_piezas.R",
            "R/hd_columnas.R", "R/hd_table.R", "R/avisos_alertas_module.R"))
  suppressMessages(source(f))

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

registro <- list()
log_action <- function(user, module, action, ...) { registro[[length(registro) + 1L]] <<- list(user = user, action = action); invisible(NULL) }
avisos <- character(); toggles <- list(); modales <- 0L; cerrados <- 0L
local({
  ns_shiny <- asNamespace("shiny")
  for (nm in c("showNotification", "showModal", "removeModal")) unlockBinding(nm, ns_shiny)
  assign("showNotification", function(ui, ...) { avisos <<- c(avisos, paste(as.character(ui), collapse = " ")); invisible(NULL) }, envir = ns_shiny)
  assign("showModal", function(ui, ...) { modales <<- modales + 1L; invisible(NULL) }, envir = ns_shiny)
  assign("removeModal", function(...) { cerrados <<- cerrados + 1L; invisible(NULL) }, envir = ns_shiny)
  ns_js <- asNamespace("shinyjs"); unlockBinding("toggle", ns_js)
  assign("toggle", function(id = NULL, ..., condition = NULL) { toggles[[id]] <<- condition; invisible(NULL) }, envir = ns_js)
})

CID <- "acme"
KEY <- paste0(CID, "/avisos_alertas.rds")
STAFF <- setdiff(niveles_todos(), niveles_cliente())[1]
auth_load_usuarios <- function(client_id = NULL) data.frame(
  username = c("ana", "beto", "caro", "soporte"), display_name = c("Ana", "Beto", "Caro", "Soporte"),
  tier = c("admin", "finance", "analysis", STAFF), deleted = FALSE, activo = TRUE, stringsAsFactors = FALSE)
denegado <- sub("%s.*$", "", tr("permisos.denegado", permiso = "%s"))

sesion <- function(nivel, guion) {
  avisos <<- character(); toggles <<- list(); registro <<- list(); modales <<- 0L; cerrados <<- 0L
  usr <- list(user = paste0("u_", nivel), tier = nivel, permisos = permisos_resueltos(nivel))
  shared <- list(effective_client_id = function() CID, home_client_id = function() CID,
                 current_user = function() usr$user, usuario = reactiveVal(usr))
  out <- list()
  r <- tryCatch({
    invisible(utils::capture.output(suppressMessages(suppressWarnings(
      testServer(alertas_equipo_server, args = list(shared = shared), {
        session$flushReact()   # observers first, or ignoreInit eats the first click
        out <<- guion(session, function() tryCatch(.filas(), error = function(e) e), function(u) editando(u))
      })))))
    "ok"
  }, error = function(e) conditionMessage(e))
  c(list(r = r), out)
}
editar_beto <- function(session, filas, ...) {
  antes <- filas()
  session$setInputs(`tabla-accion` = list(accion = "editar", ids = list("beto"), nonce = 1))
  session$setInputs(m_crear = TRUE, m_archivo_pago = FALSE, m_confirmar_pago = TRUE, m_bancos = FALSE)
  session$setInputs(guardar = 1)
  list(antes = antes, despues = filas())
}

# ── P: admin ─────────────────────────────────────────────────────────────────
s3_memoria_reset()
x <- sesion("admin", editar_beto)
ok(identical(x$r, "ok") && isTRUE(toggles$caja), "P admin: la pantalla se muestra")
ok(setequal(x$antes$id, c("ana", "beto", "caro")) && !"soporte" %in% x$antes$id,
   "P admin: lista a los usuarios del cliente, nunca al staff")
ok(all(x$antes$crear == "no") && all(x$antes$.acciones == "editar"), "P admin: sin marcas, todos con ✎")
m <- get0(KEY, envir = .mem)
ok(is.data.frame(m) && identical(m$usuario, "beto") && isTRUE(m$crear) && isTRUE(m$confirmar_pago) &&
     !isTRUE(m$bancos) && identical(m$actualizado_por, "u_admin"), "P admin: Guardar guarda las marcas de beto")
ok(identical(x$despues$crear[x$despues$id == "beto"], "si"), "P admin: la tabla muestra lo guardado")
ok(modales == 1L && cerrados == 1L, "P admin: ✎ abre el editor y Guardar lo cierra")
ok(any(vapply(registro, function(e) identical(e$action, "configurar_alertas"), logical(1))),
   "P admin: queda en Actividad (configurar_alertas)")

# ── N: finance, without can_publish_panorama ─────────────────────────────────
s3_memoria_reset()
n0 <- .mem_escrituras
x <- sesion("finance", function(session, filas, forzar) {
  antes <- filas()
  session$setInputs(`tabla-accion` = list(accion = "editar", ids = list("beto"), nonce = 1))
  session$setInputs(`tabla-accion` = list(accion = "detalle", ids = list("beto"), nonce = 2))
  forzar(NULL)   # even with an id forced into the editor, Guardar is refused
  forzar("beto")
  session$setInputs(m_crear = TRUE, guardar = 1)
  list(antes = antes)
})
cat("  r =", x$r, "
"); ok(identical(x$r, "ok") && identical(toggles$caja, FALSE), "N finance: la pantalla queda oculta")
ok(is.data.frame(x$antes) && nrow(x$antes) == 0L, "N finance: el servidor no manda filas")
ok(.mem_escrituras == n0 && is.null(get0(KEY, envir = .mem)), "N finance: ni ✎ ni Guardar escriben nada")
ok(modales == 0L, "N finance: no se abre el editor")
ok(sum(grepl(denegado, avisos, fixed = TRUE)) >= 2L, "N finance: se le dice que no tiene permiso")

# ── F: failures ──────────────────────────────────────────────────────────────
s3_memoria_reset()
alertas_guardar(CID, data.frame(usuario = "ana", crear = TRUE, archivo_pago = FALSE,
                                confirmar_pago = FALSE, bancos = FALSE), por = "x")
antes <- get(KEY, envir = .mem); n0 <- .mem_escrituras
x <- sesion("admin", function(session, filas, ...) {
  session$setInputs(`tabla-accion` = list(accion = "editar", ids = list("beto"), nonce = 1))
  .mem_falla <<- KEY
  session$setInputs(m_crear = TRUE, guardar = 1)
  r <- list(leida = filas())
  .mem_falla <<- character()
  r
})
ok(.mem_escrituras == n0 && identical(get(KEY, envir = .mem), antes), "F guardar con S3 caído no escribe nada")
ok(any(grepl(tr("comun.error_guardar"), avisos, fixed = TRUE)) &&
     !any(grepl(tr("avisos.alertas.guardado"), avisos, fixed = TRUE)), "F lo dice, y nunca 'Alertas guardadas'")
ok(cerrados == 0L, "F el editor sigue abierto para reintentar")
ok(any(vapply(registro, function(e) identical(e$action, "configurar_alertas_fallido"), logical(1))),
   "F queda en Actividad (configurar_alertas_fallido)")

s3_memoria_reset()
alertas_guardar(CID, data.frame(usuario = "ana", crear = TRUE, archivo_pago = FALSE,
                                confirmar_pago = FALSE, bancos = FALSE), por = "x")
.mem_falla <<- KEY
x <- sesion("admin", function(session, filas, ...) list(leida = filas()))
.mem_falla <<- character()
ok(inherits(x$leida, "error"), "F una lectura fallida es un error de la tabla, nunca la matriz vacía")

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
