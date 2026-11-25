# =============================================================================
# tests/test_usuarios_cambio_cliente.R
# Usuarios por id, isolation (asked by Luis, 2026-11-04): with the editor open
# on account "x" of client A, switching to client B (which also has an "x")
# must drop the selection. Otherwise B's "x" would open, and Guardar would
# write on it. Same for an account awaiting deletion.
# Runs the REAL tiersServer in shiny::testServer over the in-memory S3;
# auth_load_usuarios is stubbed per client.
# TM_ARCHIVO: the tiers_module.R to load (default: this checkout's).
# Run from project root:  Rscript tests/test_usuarios_cambio_cliente.R
# =============================================================================
cat("=== test_usuarios_cambio_cliente ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# T100: the real server's files come from ONE shared list
# (tests/helpers/server_usuarios_archivos.R); stand-ins go after it.
source("tests/helpers/server_usuarios_archivos.R")
cargar_server_usuarios(c("R/tiers_module.R" = Sys.getenv("TM_ARCHIVO", "R/tiers_module.R")))
tr <- function(k, ...) k
IS_ADMIN_DEPLOYMENT <- TRUE   # staff deployment: the one that switches clients
log_action <- function(...) invisible(NULL)
# get_bucket and put_object (.s3_write's backup, the audit lists) stay in
# s3_memoria's store, off the network (T30).

usuario <- function(u, tier, cid)
  data.frame(username = u, display_name = paste(u, cid), tier = tier, activo = TRUE,
             deleted = FALSE, permisos = "{}", client_id = cid, stringsAsFactors = FALSE)
.por_cliente <- list(
  a = rbind(usuario("x", "admin", "a"), usuario("da", "dev", "a")),
  b = rbind(usuario("x", "finance", "b"), usuario("db", "dev", "b")))
auth_load_usuarios <- function(client_id = NULL, ...) .por_cliente[[client_id]]

cid <- reactiveVal("a")
shared <- list(
  current_user_info   = reactiveVal(list(user = "luis", tier = nivel_tope(), is_staff = TRUE)),
  usuario             = reactiveVal(list(user = "luis", tier = nivel_tope(),
                                         permisos = permisos_resueltos(nivel_tope()))),
  effective_client_id = function() cid(),
  home_client_id      = function() "hd-admin",
  current_user        = function() "luis",
  jump_client_id      = function() cid(),
  permisos_version    = reactiveVal(0L),
  hop_grants_db       = reactiveVal(NULL),
  grupos_db           = reactiveVal(NULL))

.al_final <- FALSE
res <- tryCatch(testServer(tiersServer, args = list(shared = shared), {
  session$flushReact()

  # ── Editor open on "x" of A, then jump to B ────────────────────────────────
  # T28: ✎ on the engine's table (it names the account by username).
  session$setInputs(`usuarios-tabla-accion` = list(accion = "editar", ids = "x", nonce = 1))
  ok("A: Editar on 'x' selects 'x'", identical(selected_user(), "x"))
  ok("A: the editor finds the 'x' of client A",
     identical(usuarios_rv()$display_name[.fila_usuario(usuarios_rv(), selected_user())], "x a"))

  cid("b"); session$flushReact()
  ok("after switching to B, nothing is selected", is.null(selected_user()))
  ok("after switching to B, the editor is not drawn",
     inherits(tryCatch(output$perm_editor_ui, error = function(e) e), "error") ||
     !nzchar(paste(output$perm_editor_ui$html %||% "", collapse = "")))

  # ── An account awaiting deletion, then jump back to A ─────────────────────
  session$setInputs(`usuarios-tabla-accion` = list(accion = "eliminar", ids = "x", nonce = 2))   # T28: ✕
  ok("B: the trash on 'x' leaves 'x' awaiting confirmation", identical(pending_del_user(), "x"))
  cid("a"); session$flushReact()
  ok("after switching back to A, nothing awaits deletion", is.null(pending_del_user()))
  session$setInputs(btn_confirm_delete = 1)
  ok("confirming after the switch deletes nothing", .mem_escrituras == 0L)
  .al_final <<- TRUE
}), error = function(e) { message("  ERROR testServer: ", conditionMessage(e)); FALSE })
ok("the module ran to the end", isTRUE(.al_final))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail) quit(status = 1)
