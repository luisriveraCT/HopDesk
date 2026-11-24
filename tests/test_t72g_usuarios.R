# =============================================================================
# tests/test_t72g_usuarios.R
# T72-G (usuarios). auth_save_usuarios() writes usuarios.rds with its own
# s3saveRDS, outside .s3_write(): no write-layer entry at all (no sin_evento,
# no alarm). Now it takes evento = NULL and, once the write worked, logs the
# same entry .s3_write() does (.t72_entrada): the event's row, or sin_evento.
# The activation (?invite=) and Usuarios' "Nueva cuenta" pass their row as
# the event (guardar_cuenta_nueva(evento =)), the words of always.
#   1 activation: one activacion_usuario row, with the table and its rows
#     before/after; no hash, no row detail in it;
#   2 a refused or failed activation leaves no row;
#   3 a write without an event leaves sin_evento (it used to leave nothing);
#   4 guardar_cuenta_nueva(evento =) (Nueva cuenta's path): its row;
#   5 (the paso 0's open question) the activation's row reaches app_audit in
#     S3 with no session at all: the invite page returns before
#     onSessionEnded is registered, so only log_action's own later:: flush
#     writes it -- and it does, in its ~2 s.
# Real code: activar_invitacion, guardar_cuenta_nueva, auth_save_usuarios,
# .t72_entrada (and, in 5, R/app_audit.R's log_action) over the in-memory S3.
# Run from project root:  Rscript --vanilla tests/test_t72g_usuarios.R
# =============================================================================
cat("=== test_t72g_usuarios ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/i18n.R"))
source("R/tier_registry.R")
for (e in parse("R/auth.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("auth_load_usuarios", "auth_save_usuarios", ".usuarios_leer_estricto",
                                  ".usuarios_existe", "usuarios_vivos_nivel", "usuarios_niveles_perdidos",
                                  "hash_password", "verify_password"))
    eval(e, globalenv())
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".next_account_code"))
    eval(e, globalenv())
source("R/usuarios_cuentas.R")
auth_invalidate_credentials <- function() invisible(NULL)
.migrate_legacy_passwords <- function(raw, cid) raw
auth_get_credentials <- function() data.frame(user = "staff1", stringsAsFactors = FALSE)
avisar_cuenta_nueva <- function(...) invisible(NULL)   # T82's aviso: not this test's

LOGS <- list()
log_action <- function(user, module, action, description = "", target_id = NULL, metadata = NULL,
                       client_id = NULL, viewer_home_client_id = NULL, ...)
  { LOGS[[length(LOGS) + 1L]] <<- list(user = user, module = module, action = action, description = description,
                                       target_id = target_id, metadata = metadata, client_id = client_id,
                                       viewer_home_client_id = viewer_home_client_id); invisible(NULL) }
filas <- function(accion) Filter(function(x) identical(x$action, accion), LOGS)

base_usuarios <- data.frame(
  id = c("a1", "a2", "a3"), account_code = c("U0001", "U0003", "U0002"), username = c("ana", "beto", "carla"),
  password_hash = "x", display_name = c("Ana", "Beto", "Carla"), tier = "admin", client_id = "acme", permisos = "{}", group_ids = "[]",
  allowed_clients = "[]", email = NA_character_, requires_password_change = FALSE, activo = TRUE,
  created_at = "", last_login = NA_character_, deleted = FALSE, deleted_at = NA_character_,
  stringsAsFactors = FALSE)
K_U <- "acme/usuarios.rds"; K_I <- "hd-admin/username_index.rds"; K_INV <- "hd-admin/pending_invites.rds"
reiniciar <- function() {
  rm(list = ls(.mem), envir = .mem)
  assign(K_U, base_usuarios, envir = .mem)
  assign(K_I, data.frame(username = c("ana", "beto", "ocupado"), home_folder = c("acme", "acme", "otro"),
                         account_code = c("U0001", "U0003", "U0001"), created_at = "", active = TRUE,
                         stringsAsFactors = FALSE), envir = .mem)
  assign(K_INV, .schema_pending_invites(), envir = .mem)
  .mem_falla <<- character(); .mem_falla_escritura <<- character()
  tok <- create_invite("maria@empresa.mx", "María", "acme", "admin", "luis")
  inv <- resolve_invite(tok)
  LOGS <<- list()
  inv
}
buenos <- list(nombre = "María Núñez", puesto = "Analista", usuario = "MNunez",
               contrasena = "Segura#2026", confirmar = "Segura#2026")

# ── 1. Activation ────────────────────────────────────────────────────────
inv <- reiniciar()
r <- activar_invitacion(inv, buenos)
a <- filas("activacion_usuario")
ok("control: the account is saved", isTRUE(r$ok) && any(get(K_U, envir = .mem)$username == "mnunez"))
ok("1 one activacion_usuario row, by the new user, module auth, its words of always (cuentas.activacion.log)",
   length(a) == 1L && identical(a[[1]]$user, "mnunez") && identical(a[[1]]$module, "auth") &&
     identical(a[[1]]$description, tr("cuentas.activacion.log", usuario = "mnunez")) &&
     identical(a[[1]]$target_id, r$account_code))
ok("1 …its metadata: the table and its rows before/after (3 -> 4)",
   identical(a[[1]]$metadata$tabla, "usuarios.rds") && identical(a[[1]]$metadata$filas_antes, 3L) &&
     identical(a[[1]]$metadata$filas_despues, 4L))
ok("1 …no row detail, no password hash anywhere in it",
   is.null(a[[1]]$metadata$diferencia) &&
     !any(grepl("scrypt|\\$", unlist(lapply(LOGS, function(x) c(x$description, unlist(x$metadata)))))))
ok("1 …in the invitee's client: client_id and viewer_home_client_id are the invitation's (no user session needed)",
   identical(a[[1]]$client_id, "acme") && identical(a[[1]]$viewer_home_client_id, "acme"))
ok("1 the users write leaves no sin_evento (it carried the event)",
   !any(vapply(filas("sin_evento"), function(x) identical(x$metadata$tabla, "usuarios.rds"), NA)))

# ── 2. Refused / failed ──────────────────────────────────────────────────
inv <- reiniciar()
r <- activar_invitacion(inv, utils::modifyList(buenos, list(usuario = "Ocupado")))
ok("2 a taken user name: nothing saved, no activacion_usuario row", !isTRUE(r$ok) && !length(filas("activacion_usuario")))
inv <- reiniciar(); .mem_falla_escritura <- K_U
r <- tryCatch(activar_invitacion(inv, buenos), error = function(e) e)
.mem_falla_escritura <- character()
ok("2 the users write fails: the activation stops, no activacion_usuario row",
   inherits(r, "error") && !length(filas("activacion_usuario")))

# ── 3. A write without an event ──────────────────────────────────────────
reiniciar()
u <- get(K_U, envir = .mem); u$display_name[1] <- "Ana María"
auth_save_usuarios(u, client_id = "acme", base = base_usuarios)
s <- Filter(function(x) identical(x$metadata$tabla, "usuarios.rds"), filas("sin_evento"))
ok("3 auth_save_usuarios without an event: one sin_evento entry with the table and its origin",
   length(s) == 1L && grepl("auth_save_usuarios", s[[1]]$metadata$origen %||% "", fixed = TRUE))

# ── 4. Nueva cuenta's path ───────────────────────────────────────────────
reiniciar()
fila <- base_usuarios[1, ]; fila$id <- "a9"; fila$account_code <- "U0009"; fila$username <- "nuevo"
ev <- list(accion = "crear_usuario", modulo = "usuarios", usuario = "ana", home = "acme",
           descripcion = "Cuenta 'nuevo' creada (tier: admin)", target_id = "U0009",
           metadata = list(tier = "admin", display_name = "Ana"))
guardar_cuenta_nueva(base_usuarios, fila, "acme", evento = ev)
cu <- filas("crear_usuario")
ok("4 guardar_cuenta_nueva(evento =): one crear_usuario row, its words and metadata of always",
   length(cu) == 1L && identical(cu[[1]]$description, "Cuenta 'nuevo' creada (tier: admin)") &&
     identical(cu[[1]]$metadata$tier, "admin") && identical(cu[[1]]$metadata$display_name, "Ana") &&
     identical(cu[[1]]$target_id, "U0009"))

reiniciar(); .mem_falla_escritura <- K_U
r <- tryCatch(guardar_cuenta_nueva(base_usuarios, fila, "acme", evento = ev), error = function(e) e)
.mem_falla_escritura <- character()
ok("4 Nueva cuenta whose users write fails: an error (the screen says it), no crear_usuario row",
   inherits(r, "error") && !length(filas("crear_usuario")))

# ── 5. The activation's row reaches app_audit with no session ───────────
for (e in parse("R/app_audit.R", keep.source = FALSE, encoding = "UTF-8")) eval(e, globalenv())   # the real log_action
inv <- reiniciar()
r <- activar_invitacion(inv, buenos)
audit_key <- function() grep("^acme/app_audit", ls(.mem), value = TRUE)
ok("5 control: right after the activation, the row is only queued (nothing in S3 yet)", !length(audit_key()))
t0 <- Sys.time()
while (!length(audit_key()) && difftime(Sys.time(), t0, units = "secs") < 15) { later::run_now(1) }
fila_audit <- function() { k <- audit_key(); if (!length(k)) return(NULL); d <- get(k[1], envir = .mem)
  if (inherits(d, "s3_memoria_bytes")) d <- .mem_falsos$s3readRDS(k[1], ""); d }
d <- fila_audit()
ok("5 with no session, log_action's own flush writes it to app_audit (the activation's row is there)",
   is.data.frame(d) && any(d$action == "activacion_usuario" & d$user == "mnunez"))

utils::flush.console()
s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
