# =============================================================================
# tests/test_t72h_usuarios.R
# T72-H. The other callers of auth_save_usuarios() log with the save
# (evento =): written only once the users write worked.
#   A  Usuarios (the real tiersServer):
#      - editar: one editar_usuario row only if it saved;
#      - eliminar: one eliminar_usuario row only if it saved;
#      - lower the user limit: the per-user rows only after step B (the
#        registry) held; if step B fails and step A is rolled back, no
#        "desactivada" row, one cambio_limite_revertido row, and the users
#        table is back as it was; the step A write leaves a staff /parte.
#   B  the forced password change (app.R): its event, as written there,
#      gives one cambio_contrasena_obligatorio row in the account's client.
#   C  reset_aplicar (?reset=): one reset_contrasena_cambiada row, in the
#      account's client (no user session), only if it saved.
# In every case: neither the password, its hash nor the reset token is in
# any row (description, metadata, anything), and a failed write leaves no
# row. S3 is the in-memory store of tests/helpers/s3_memoria.R.
# Run from project root:  Rscript --vanilla tests/test_t72h_usuarios.R
# =============================================================================
cat("=== test_t72h_usuarios ===\n\n")
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
cargar_server_usuarios()
IS_ADMIN_DEPLOYMENT <- TRUE
auth_get_credentials <- function() data.frame(user = character(), stringsAsFactors = FALSE)
auth_invalidate_credentials <- function() invisible(NULL)
.migrate_legacy_passwords <- function(raw, cid) raw
update_client_user_count <- function(...) invisible(NULL)
notify_user_limit <- function(...) invisible(NULL)
avisar_cuenta_nueva <- function(...) invisible(NULL)
avisos_agregar <- function(...) "id"
correos <- list()
send_email <- function(to, subject, html, queue_on_fail = TRUE) { correos[[length(correos) + 1L]] <<- list(html = html); TRUE }

LOGS <- list()
log_action <- function(user, module, action, description = "", target_id = NA, s3_key = NA, metadata = NULL,
                       client_id = NULL, viewer_home_client_id = NULL)
  { LOGS[[length(LOGS) + 1L]] <<- list(user = user, module = module, action = action, description = description,
                                       target_id = target_id, metadata = metadata, client_id = client_id,
                                       viewer_home_client_id = viewer_home_client_id); invisible(NULL) }
filas  <- function(accion) Filter(function(x) identical(x$action, accion), LOGS)
# Every word of every row, metadata included, as one text: where a hash or a
# token must never be.
todo_el_log <- function() paste(vapply(LOGS, function(x) paste(deparse(x), collapse = " "), ""), collapse = "\n")
nada_de <- function(...) { t <- todo_el_log(); !any(vapply(c(...), function(s) grepl(s, t, fixed = TRUE), NA)) }

DEV <- niveles_unico_vivo()[1]
u <- function(name, tier = "admin", activo = TRUE, hash = "x")
  data.frame(id = paste0("id_", name), account_code = paste0("U000", match(name, c("dev1", "ana", "carla", "beto"))),
             username = name, password_hash = hash, display_name = toupper(name), tier = tier, client_id = "acme",
             permisos = "{}", group_ids = "[]", allowed_clients = "[]", email = paste0(name, "@x.mx"),
             requires_password_change = FALSE, activo = activo, created_at = "", last_login = NA_character_,
             deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE)
HASH_CARLA <- hash_password("Vieja#2026x")
TABLA <- rbind(u("dev1", DEV), u("ana"), u("carla", hash = HASH_CARLA), u("beto"))
KEY <- "acme/usuarios.rds"; K_I <- "hd-admin/username_index.rds"; K_REG <- "hd-admin/client_registry.rds"
s3 <- function() get(KEY, envir = .mem)
registro_cliente <- function() {
  reg <- .schema_client_registry()[0, , drop = FALSE]
  .hd_admin_agregar(reg, data.frame(client_id = "acme", display_name = "Acme", status = "active",
                                    max_users = 5L, current_users = 4L, stringsAsFactors = FALSE))
}
reiniciar <- function() {
  s3_memoria_reset(); assign(KEY, TABLA, envir = .mem)
  assign(K_I, data.frame(username = c("dev1", "ana", "carla", "beto"), home_folder = "acme",
                         account_code = c("U0001", "U0002", "U0003", "U0004"), created_at = "", active = TRUE,
                         stringsAsFactors = FALSE), envir = .mem)
  assign(K_REG, registro_cliente(), envir = .mem)
  .mem_falla_escritura <<- character()
  LOGS <<- list()
}

shared <- list(
  current_user_info   = reactiveVal(list(user = "luis", tier = nivel_tope(), is_staff = TRUE)),
  usuario             = reactiveVal(list(user = "luis", tier = nivel_tope(), permisos = permisos_resueltos(nivel_tope()))),
  effective_client_id = function() "acme", home_client_id = function() "hd-admin", current_user = function() "luis",
  jump_client_id = function() "acme", permisos_version = reactiveVal(0L), hop_grants_db = reactiveVal(NULL),
  grupos_db = reactiveVal(NULL))
avisos <- character(0)
showNotification <- function(ui, ...) { avisos <<- c(avisos, paste(ui, collapse = "")); invisible("id") }
showModal <- function(ui) invisible(NULL)
removeModal <- function(...) invisible(NULL)

# ── A. Usuarios ──────────────────────────────────────────────────────────
correr_tiers <- function(cuerpo) {
  .al_final <- FALSE
  tryCatch(testServer(tiersServer, args = list(shared = shared), {
    session$flushReact()
    clic <- function(a, id, n) { session$setInputs(`usuarios-tabla-accion` = list(accion = a, ids = id, nonce = n)); session$flushReact() }
    guardar <- function(n, ...) {
      session$setInputs(edit_tier = "admin", edit_activo = TRUE, edit_password = "", edit_password2 = "", ...)
      session$setInputs(btn_save_perms = n); session$flushReact() }
    eliminar <- function(quien, n) { clic("eliminar", quien, 100 + n); session$setInputs(btn_confirm_delete = n); session$flushReact() }
    limite <- function(nuevo, conservar, n) {
      session$userData$editing_client_id <- "acme"
      session$setInputs(edit_display_name = "Acme", edit_contact_email = "", edit_max_users = nuevo)
      session$setInputs(btn_save_client_edit = n); session$flushReact()
      session$setInputs(limit_confirm_keep = conservar)
      session$setInputs(btn_confirm_limit_change = n); session$flushReact() }
    cuerpo(environment())
    .al_final <<- TRUE
  }), error = function(e) message("  ERROR testServer: ", conditionMessage(e)))
  .al_final
}

# editar
reiniciar()
corrio <- correr_tiers(function(env) with(env, { clic("editar", "carla", 1); guardar(1, edit_display_name = "Carla Ruiz") }))
e <- filas("editar_usuario")
ok("A control: the module ran, carla's name is saved", corrio && identical(s3()$display_name[s3()$username == "carla"], "Carla Ruiz"))
ok("A editar: one editar_usuario row, its words and metadata of always, in acme",
   length(e) == 1L && identical(e[[1]]$description, "Permisos/tier de 'carla' actualizados") &&
     identical(e[[1]]$target_id, "carla") && identical(e[[1]]$metadata$tier, "admin") &&
     identical(e[[1]]$client_id, "acme") && identical(e[[1]]$user, "luis"))
ok("A editar: no hash in any row", nada_de(HASH_CARLA))
reiniciar(); .mem_falla_escritura <- KEY; avisos <- character(0)
correr_tiers(function(env) with(env, { clic("editar", "carla", 1); guardar(1, edit_display_name = "Carla Ruiz") }))
.mem_falla_escritura <- character()
ok("A editar, the write fails: no editar_usuario row, the notice of always",
   !length(filas("editar_usuario")) && any(grepl(tr("niveles.usuarios.aviso.permisos_no_guardados"), avisos, fixed = TRUE)))

# eliminar
reiniciar()
correr_tiers(function(env) with(env, eliminar("beto", 1)))
d <- filas("eliminar_usuario")
ok("A eliminar: one eliminar_usuario row, its words and metadata of always, in acme",
   length(d) == 1L && identical(d[[1]]$description, "Cuenta 'beto' eliminada (tier: admin)") &&
     identical(d[[1]]$metadata$tier, "admin") && identical(d[[1]]$client_id, "acme") &&
     isTRUE(s3()$deleted[s3()$username == "beto"]))
reiniciar(); .mem_falla_escritura <- KEY; avisos <- character(0)
correr_tiers(function(env) with(env, eliminar("beto", 1)))
.mem_falla_escritura <- character()
ok("A eliminar, the write fails: no eliminar_usuario row, the notice of always",
   !length(filas("eliminar_usuario")) && any(grepl(tr("niveles.usuarios.aviso.cambio_no_guardado"), avisos, fixed = TRUE)))

# lower the limit 5 -> 2, keeping dev1 and ana: carla and beto are deactivated
reiniciar()
correr_tiers(function(env) with(env, limite(2, c("U0001", "U0002"), 1)))
dz <- filas("user_deactivated_by_limit_reduction")
ok("A limit: carla and beto deactivated in S3, the registry says 2",
   all(s3()$activo[s3()$username %in% c("carla", "beto")] %in% FALSE) &&
     identical(get(K_REG, envir = .mem)$max_users[get(K_REG, envir = .mem)$client_id == "acme"], 2L))
ok("A limit: one desactivada row per account (2), its words of always",
   length(dz) == 2L && setequal(vapply(dz, `[[`, "", "target_id"), c("U0003", "U0004")) &&
     all(grepl("desactivada por reducción de límite en 'acme'", vapply(dz, `[[`, "", "description"), fixed = TRUE)))
acc <- vapply(LOGS, `[[`, "", "action")
ok("A limit: the per-user rows come after step B (after the step A /parte), the change's two rows after them",
   identical(acc[acc != "sin_evento"], c("cambio_limite_usuarios/parte", "user_deactivated_by_limit_reduction",
                                          "user_deactivated_by_limit_reduction", "cambio_limite_usuarios",
                                          "cambio_limite_usuarios")))
ok("A limit: no cambio_limite_revertido", !length(filas("cambio_limite_revertido")))

# step B fails: step A rolled back
reiniciar(); .mem_falla_escritura <- K_REG; avisos <- character(0)
correr_tiers(function(env) with(env, limite(2, c("U0001", "U0002"), 1)))
.mem_falla_escritura <- character()
rv <- filas("cambio_limite_revertido")
ok("A limit, step B fails: the users are back as they were (step A rolled back)",
   identical(s3()$activo, TABLA$activo) && identical(s3()$deleted, TABLA$deleted))
ok("A limit, step B fails: NO desactivada row (none was left deactivated)",
   !length(filas("user_deactivated_by_limit_reduction")))
ok("A limit, step B fails: one cambio_limite_revertido row, in acme, the user sees error_revertido",
   length(rv) == 1L && identical(rv[[1]]$description, tr("niveles.clientes.log.limite_revertido", cliente = "acme")) &&
     identical(rv[[1]]$client_id, "acme") && any(grepl(tr("niveles.clientes.aviso.error_revertido"), avisos, fixed = TRUE)))

# ── B. The forced password change (app.R): its event as written there ────
app <- parse("app.R", keep.source = FALSE, encoding = "UTF-8")
ev_app <- NULL
buscar <- function(e) {
  if (!is.call(e)) return(invisible())
  if (identical(e[[1]], as.name("auth_save_usuarios")) && "evento" %in% names(as.list(e))) ev_app <<- c(ev_app, list(e$evento))
  for (k in seq_along(e)[-1]) { a <- e[[k]]; if (!missing(a) && is.call(a)) buscar(a) }
}
for (e in app) buscar(e)
ok("B app.R has one auth_save_usuarios(evento =) (the forced password change)", length(ev_app) == 1L)
ok("B its event names no password, hash or pw variable",
   !grepl("pw1|pw2|password|hash|contrasena\\b", paste(deparse(ev_app[[1]]), collapse = " ")))
reiniciar()
info <- list(user = "carla", client_id = "acme")
usuarios <- s3(); base_usuarios <- usuarios
nuevo_hash <- hash_password("Nueva#2026x")
usuarios$password_hash[usuarios$username == "carla"] <- nuevo_hash
usuarios$requires_password_change[usuarios$username == "carla"] <- FALSE
auth_save_usuarios(usuarios, client_id = info$client_id, base = base_usuarios, evento = eval(ev_app[[1]]))
co <- filas("cambio_contrasena_obligatorio")
ok("B one cambio_contrasena_obligatorio row, by carla, in acme, the dictionary's words",
   length(co) == 1L && identical(co[[1]]$user, "carla") && identical(co[[1]]$client_id, "acme") &&
     identical(co[[1]]$viewer_home_client_id, "acme") &&
     identical(co[[1]]$description, tr("cuentas.log.contrasena_obligatoria")))
ok("B neither the new hash, the old one nor the password in any row", nada_de(nuevo_hash, HASH_CARLA, "Nueva#2026x"))
reiniciar(); .mem_falla_escritura <- KEY
r <- tryCatch(auth_save_usuarios(usuarios, client_id = "acme", base = base_usuarios, evento = eval(ev_app[[1]])),
              error = function(e) e)
.mem_falla_escritura <- character()
ok("B the write fails: an error (the screen says try again), no row", inherits(r, "error") &&
     !length(filas("cambio_contrasena_obligatorio")))

# ── C. reset_aplicar (?reset=), no user session ──────────────────────────
T0 <- as.POSIXct("2026-11-12 10:00:00", tz = "UTC")
reiniciar()
assign("hd-admin/password_resets.rds", .schema_password_resets(), envir = .mem)
assign("hd-admin/emergency_lock.rds", data.frame(username = character(), stringsAsFactors = FALSE), envir = .mem)
.emergency_lock_cache$data <- NULL; .emergency_lock_cache$ts <- NULL
.reset_invalidos$t <- NULL
correos <- list()
pedir <- reset_solicitar("carla@x.mx", ahora = T0, base_url = "https://hopdesk.app")
tk <- regmatches(correos[[1]]$html, regexpr("(?<=\\?reset=)[0-9a-f]{64}", correos[[1]]$html, perl = TRUE))
LOGS <- list()
r <- reset_aplicar(tk, "Otra#2026xy", "Otra#2026xy", T0 + 60)
rs <- filas("reset_contrasena_cambiada")
ok("C control: the reset worked", isTRUE(r$ok) && verify_password("Otra#2026xy", s3()$password_hash[s3()$username == "carla"]))
ok("C one reset_contrasena_cambiada row, by carla, in acme (client_id and viewer_home), the words of always",
   length(rs) == 1L && identical(rs[[1]]$user, "carla") && identical(rs[[1]]$client_id, "acme") &&
     identical(rs[[1]]$viewer_home_client_id, "acme") &&
     identical(rs[[1]]$description, tr("cuentas.reset.log.contrasena_cambiada")))
ok("C neither the token, its sha256, the new hash nor the password in any row",
   nada_de(tk, reset_hash(tk), s3()$password_hash[s3()$username == "carla"], "Otra#2026xy"))
# a failed users write
reiniciar()
assign("hd-admin/password_resets.rds", .schema_password_resets(), envir = .mem)
assign("hd-admin/emergency_lock.rds", data.frame(username = character(), stringsAsFactors = FALSE), envir = .mem)
.reset_invalidos$t <- NULL; correos <- list()
reset_solicitar("carla@x.mx", ahora = T0, base_url = "https://hopdesk.app")
tk <- regmatches(correos[[1]]$html, regexpr("(?<=\\?reset=)[0-9a-f]{64}", correos[[1]]$html, perl = TRUE))
LOGS <- list(); .mem_falla_escritura <- KEY
r <- reset_aplicar(tk, "Otra#2026xy", "Otra#2026xy", T0 + 60)
.mem_falla_escritura <- character()
ok("C the users write fails: try again, no reset_contrasena_cambiada row",
   identical(r$error, "comun.reintentar") && !length(filas("reset_contrasena_cambiada")))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail) quit(status = 1)
