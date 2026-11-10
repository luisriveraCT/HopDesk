# =============================================================================
# tests/test_admin_usuarios_hd.R
# Usuarios on the table engine (T28, class D) -- the riskiest of the eight:
# an action on the wrong account gives or takes access.
#   1. the rows: the viewer's list by username; the tier by its name; an NA
#      "activo" reads Inactivo (it read "NA");
#   2. the table server: "#" only for staff; ✎ / double click / ✕ name the
#      account by username, also when another session's account lands on
#      top after drawing; without permitido() nothing is named;
#   3. the REAL tiersServer (testServer, S3 in memory), one click each from
#      start-up: ✎ opens the editor on that account; the T19 scenario --
#      editor open on carla, beto deleted -- the editor stays on carla;
#      ✕ on the only live Dev is refused by the last-Dev lock, nothing saved;
#      ✕ + confirm on another account soft-deletes that one only.
#
# Run from project root:  Rscript tests/test_admin_usuarios_hd.R
# =============================================================================
cat("=== test_admin_usuarios_hd ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# As test_usuarios_cambio_cliente.R: every function definition in R/, then
# the files this module needs in full.
suppressMessages(for (f in list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)) {
  ex <- tryCatch(parse(f, keep.source = FALSE), error = function(e) expression())
  for (e in ex)
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")))
      tryCatch(eval(e, envir = globalenv()), error = function(err) NULL)
})
suppressMessages({
  for (f in c("R/tier_registry.R", "R/permisos.R", "R/i18n.R", "R/auth.R", "R/tiers_tab_config.R",
              "R/audit_log_viewer_module.R", "R/pagos_audit_viewer_module.R", "R/hd_registro.R",
              "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/tiers_module.R"))
    if (file.exists(f)) source(f, local = FALSE)
})
IS_ADMIN_DEPLOYMENT <- TRUE
log_action <- function(...) invisible(NULL)
# get_bucket and put_object stay in s3_memoria's store, off the network (T30).

DEV <- niveles_unico_vivo()[1]
u <- function(name, tier = "admin", activo = TRUE)
  data.frame(id = paste0("id_", name), account_code = paste0("U_", name), username = name, password_hash = "x",
             display_name = toupper(name), tier = tier, client_id = "acme", permisos = "{}", group_ids = "[]",
             allowed_clients = "[]", email = NA_character_, requires_password_change = FALSE, activo = activo,
             created_at = "", last_login = NA_character_, deleted = FALSE, deleted_at = NA_character_,
             stringsAsFactors = FALSE)
TABLA <- rbind(u("dev1", DEV), u("ana"), u("beto", "finance"), u("carla", "analysis", activo = NA))

# ── 1. The rows ──────────────────────────────────────────────────────────────
f <- .usuarios_filas(TABLA)
ok("una fila por cuenta; el id es el username", identical(f$id, c("dev1", "ana", "beto", "carla")))
ok("el nivel con su nombre", identical(f$nivel[2], nivel_nombre("admin")))
ok("activo NA se lee Inactivo (antes decía 'NA')", identical(f$estado, c("activo", "activo", "activo", "inactivo")))
ok("lista NULL o vacía: 0 filas", !nrow(.usuarios_filas(NULL)) && !nrow(.usuarios_filas(TABLA[0, ])))

# ── 2. The table server ──────────────────────────────────────────────────────
servidor <- function(con_codigo, permitido, pasos) {
  caja <- new.env(); caja$hechos <- character(0); lista <- reactiveVal(TABLA)
  error <- tryCatch(suppressMessages(testServer(tiers_usuarios_server,
    args = list(usuarios = lista, con_codigo = reactive(con_codigo), permitido = reactive(permitido)), {
      observe({ r <- session$returned$editar();   if (!is.null(r)) caja$hechos <- c(caja$hechos, paste("editar", r$id)) })
      observe({ r <- session$returned$eliminar(); if (!is.null(r)) caja$hechos <- c(caja$hechos, paste("eliminar", r$id)) })
      session$flushReact()
      pasos(session, lista)
    })), error = function(e) conditionMessage(e))
  list(error = error, hechos = caja$hechos)
}
accion <- function(session, a, id, n) { session$setInputs(`tabla-accion` = list(accion = a, ids = id, nonce = n)); session$flushReact() }
r <- servidor(TRUE, TRUE, function(session, lista) {
  lista(rbind(u("zeta"), TABLA))                    # another session's account lands on top
  accion(session, "editar", "carla", 1); accion(session, "detalle", "beto", 2); accion(session, "eliminar", "ana", 3)
})
ok("✎, doble clic y ✕ nombran la cuenta por username, aunque otra llegó arriba",
   is.null(r$error) && identical(r$hechos, c("editar carla", "editar beto", "eliminar ana")))
r <- servidor(FALSE, FALSE, function(session, lista) { accion(session, "editar", "ana", 1); accion(session, "eliminar", "ana", 2) })
ok("sin permitido(): ✎ y ✕ mandados a mano no nombran nada", is.null(r$error) && !length(r$hechos))
# The list full (as the session's local copy answers, before req_authorized())
# but no permission: no rows reach the browser.
filas_sin_permiso <- "sin leer"
r <- servidor(FALSE, FALSE, function(session, lista) {
  filas_sin_permiso <<- tryCatch(session$returned$datos(), error = function(e) paste("error:", conditionMessage(e)))
})
ok("sin permitido() y con la lista local llena: la tabla no recibe filas", is.null(filas_sin_permiso))

# ── 3. The real tiersServer ──────────────────────────────────────────────────
KEY <- "acme/usuarios.rds"
shared <- list(
  current_user_info   = reactiveVal(list(user = "luis", tier = nivel_tope(), is_staff = TRUE)),
  usuario             = reactiveVal(list(user = "luis", tier = nivel_tope(), permisos = permisos_resueltos(nivel_tope()))),
  effective_client_id = function() "acme", home_client_id = function() "hd-admin", current_user = function() "luis",
  jump_client_id = function() "acme", permisos_version = reactiveVal(0L), hop_grants_db = reactiveVal(NULL),
  grupos_db = reactiveVal(NULL))
avisos <- character(0)
showNotification <- function(ui, ...) { avisos <<- c(avisos, paste(ui, collapse = "")); invisible("id") }
modales <- 0L
showModal <- function(ui) { modales <<- modales + 1L; invisible(NULL) }
removeModal <- function(...) invisible(NULL)

s3_memoria_reset(); assign(KEY, TABLA, envir = .mem)
.al_final <- FALSE
res <- tryCatch(testServer(tiersServer, args = list(shared = shared), {
  session$flushReact()
  clic <- function(a, id, n) { session$setInputs(`usuarios-tabla-accion` = list(accion = a, ids = id, nonce = n)); session$flushReact() }

  clic("editar", "carla", 1)
  ok("un solo clic en ✎ desde el arranque abre el editor de carla", identical(selected_user(), "carla"))

  # T19: with the editor open on carla, beto is deleted (✕ + confirm).
  clic("eliminar", "beto", 2)
  ok("✕ en beto deja a beto esperando confirmación (y abre el modal de siempre)",
     identical(pending_del_user(), "beto") && modales >= 1L)
  session$setInputs(btn_confirm_delete = 1); session$flushReact()
  s3 <- get(KEY, envir = .mem)
  ok("confirmar borra (suave) a beto y solo a beto", identical(s3$username[s3$deleted %in% TRUE], "beto"))
  # Confirming a delete closes the editor (selected_user(NULL), as before T28);
  # what T19 forbids is the editor moving to ANOTHER account.
  ok("T19: después de borrar a beto, el editor no quedó en otra cuenta (cerrado o en carla)",
     is.null(selected_user()) || identical(selected_user(), "carla"))
  clic("editar", "carla", 4)
  ok("… y ✎ en carla lo vuelve a abrir en carla", identical(selected_user(), "carla") &&
     identical(usuarios_rv()$username[.fila_usuario(usuarios_rv(), selected_user())], "carla"))

  # The last live Dev: ✕ on dev1 is refused, nothing awaits, nothing saved.
  escrituras <- .mem_escrituras; avisos <<- character(0)
  clic("eliminar", "dev1", 3)
  ok("✕ en el único Dev vivo: el candado lo rechaza y no queda nada por confirmar",
     is.null(pending_del_user()) && any(grepl(nivel_nombre(DEV), avisos, fixed = TRUE)) &&
       identical(.mem_escrituras, escrituras))
  .al_final <<- TRUE
}), error = function(e) { message("  ERROR testServer: ", conditionMessage(e)); FALSE })
ok("el módulo corrió hasta el final", isTRUE(.al_final))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail) quit(status = 1)
