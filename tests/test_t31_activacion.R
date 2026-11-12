# =============================================================================
# tests/test_t31_activacion.R
# T31: the activation page (?invite=) -- activar_invitacion() and the page.
#   1. a good activation saves the account with the name, job title and user
#      name the invitee typed, a UUID, the next free code and a hashed
#      password; reserves the name in the global index; uses the invitation;
#   2. each wrong field comes back under its field, nothing saved;
#   3. a user name taken in ANOTHER client is refused; nothing saved, the
#      invitation still usable;
#   4. a failed read of the client's users or of the index stop()s with
#      nothing written; a failed save gives the reserved name back;
#   5. injection (HTML, JS, quotes, regex, a huge value, a forged list) in
#      every field: refused or stored as plain text, never markup;
#   6. tr_negritas() escapes the name and the client it puts in bold;
#   7. the page (app.R): every text from the dictionary, no Referer, the
#      token leaves the address bar, no HTML(sprintf(...)) with names.
# S3 is the in-memory store of tests/helpers/s3_memoria.R (no network).
# Run from project root:  Rscript tests/test_t31_activacion.R
# CUENTAS_R / APP_R = other copies to check (contraprueba).
# =============================================================================
cat("=== test_t31_activacion ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/i18n.R"))
source("R/tier_registry.R")
F_APP <- Sys.getenv("APP_R", "app.R")
for (e in parse("R/auth.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("auth_load_usuarios", "auth_save_usuarios", ".usuarios_leer_estricto",
                                  ".usuarios_existe", "usuarios_vivos_nivel", "usuarios_niveles_perdidos",
                                  "hash_password", "verify_password"))
    eval(e, globalenv())
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), ".next_account_code"))
    eval(e, globalenv())
source(Sys.getenv("CUENTAS_R", "R/usuarios_cuentas.R"))
auth_invalidate_credentials <- function() invisible(NULL)
.migrate_legacy_passwords <- function(raw, cid) raw
creds <- c("staff1")
auth_get_credentials <- function() data.frame(user = creds, stringsAsFactors = FALSE)

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
  resolve_invite(tok)
}
buenos <- list(nombre = " María José Núñez ", puesto = "Analista de Tesorería", usuario = "MJNunez",
               contrasena = "Segura#2026", confirmar = "Segura#2026")
con <- function(...) utils::modifyList(buenos, list(...))
nada_guardado <- function() identical(get(K_U, envir = .mem), base_usuarios)
inv_estado <- function(tok) { i <- get(K_INV, envir = .mem); i$status[i$token == tok] }
indice <- function() get(K_I, envir = .mem)

# ── 1. Good activation ───────────────────────────────────────────────────────
inv <- reiniciar()
r <- activar_invitacion(inv, buenos)
u <- get(K_U, envir = .mem); n <- u[u$username == "mjnunez", ]
ok("la activación guarda la cuenta con el usuario en minúsculas", isTRUE(r$ok) && nrow(n) == 1L)
ok("... con el nombre y el puesto que escribió (sin espacios de más)",
   identical(n$display_name, "María José Núñez") && identical(n$puesto, "Analista de Tesorería"))
ok("... con UUID, el siguiente código libre (U0004) y su correo",
   grepl("^[0-9a-f-]{36}$", n$id) && identical(n$account_code, "U0004") && identical(n$email, "maria@empresa.mx"))
ok("... con la contraseña en scrypt, que verifica",
   nchar(n$password_hash) == 128L && verify_password("Segura#2026", n$password_hash))
ok("... el usuario queda reservado en el índice global",
   any(indice()$username == "mjnunez" & indice()$active & indice()$home_folder == "acme"))
ok("... y la invitación queda usada", identical(inv_estado(inv$token), "accepted"))

# The integrator's review: one invitation, two tabs. The second activation
# finds the invitation already taken and saves nothing.
r2 <- activar_invitacion(inv, con(usuario = "otra_pestana"))
u2 <- get(K_U, envir = .mem)
ok("una invitación ya usada (otra pestaña) no crea una segunda cuenta",
   !isTRUE(r2$ok) && identical(r2$errores, list(invitacion = "cuentas.activacion.enlace_ya_no")) &&
     !any(u2$username == "otra_pestana") && nrow(u2) == nrow(u))

# ── 2. Field errors ──────────────────────────────────────────────────────────
inv <- reiniciar()
r <- activar_invitacion(inv, list(nombre = "", puesto = "  ", usuario = "a b", contrasena = "corta",
                                  confirmar = "otra"))
ok("cada campo mal vuelve con su error",
   !isTRUE(r$ok) && identical(sort(names(r$errores)), c("contrasena", "nombre", "puesto", "usuario")) &&
     identical(r$errores$puesto, "cuentas.error.puesto_obligatorio"))
r <- activar_invitacion(inv, con(confirmar = "Segura#2027"))
ok("contraseñas distintas: error en confirmar", identical(r$errores, list(confirmar = "cuentas.error.contrasenas_distintas")))
ok("... y no se guardó nada, la invitación sigue pendiente",
   nada_guardado() && identical(inv_estado(inv$token), "pending"))

# ── 3. Taken user name ───────────────────────────────────────────────────────
inv <- reiniciar(); antes <- .mem_escrituras
r <- activar_invitacion(inv, con(usuario = "Ocupado"))
ok("un usuario de OTRO cliente está ocupado", identical(r$errores, list(usuario = "cuentas.error.usuario_ocupado")))
ok("... nada guardado y la invitación sigue pendiente",
   nada_guardado() && identical(inv_estado(inv$token), "pending"))
r <- activar_invitacion(inv, con(usuario = "Beto"))
ok("un usuario del mismo cliente está ocupado", identical(r$errores$usuario, "cuentas.error.usuario_ocupado"))
# carla predates the index: only the client's own table knows her.
r <- activar_invitacion(inv, con(usuario = "Carla"))
ok("un usuario del mismo cliente que no está en el índice está ocupado",
   identical(r$errores$usuario, "cuentas.error.usuario_ocupado"))
r <- activar_invitacion(inv, con(usuario = "staff1"))
ok("un usuario que solo está en las credenciales cargadas está ocupado",
   identical(r$errores$usuario, "cuentas.error.usuario_ocupado"))

# ── 4. Failed reads and writes ───────────────────────────────────────────────
inv <- reiniciar(); .mem_falla <- K_U; antes <- .mem_escrituras
r <- tryCatch(activar_invitacion(inv, buenos), error = function(e) e)
ok("si no se leen los usuarios del cliente: error y nada escrito",
   inherits(r, "error") && .mem_escrituras == antes)
inv <- reiniciar(); .mem_falla <- K_I; idx_antes <- get(K_I, envir = .mem)
r <- tryCatch(activar_invitacion(inv, buenos), error = function(e) e)
# The invitation is taken and given back (two writes to pending_invites);
# what must hold: no account, the index untouched, the invitation usable.
ok("si no se lee el índice: error (nunca 'libre'), ninguna cuenta, índice intacto, invitación pendiente",
   inherits(r, "error") && !inherits(r, "usuario_ocupado") && nada_guardado() &&
     identical(get(K_I, envir = .mem), idx_antes) && identical(inv_estado(inv$token), "pending"))
inv <- reiniciar(); .mem_falla_escritura <- K_U
r <- tryCatch(activar_invitacion(inv, buenos), error = function(e) e)
ok("si falla el guardado: error, el usuario se libera y la invitación sigue pendiente",
   inherits(r, "error") && !any(indice()$username == "mjnunez" & indice()$active) &&
     identical(inv_estado(inv$token), "pending"))
.mem_falla_escritura <- character()

# ── 5. Injection ─────────────────────────────────────────────────────────────
inv <- reiniciar()
ataques <- list("<script>alert(1)</script>", "\"><img src=x onerror=alert(1)>", "' OR '1'='1", ".*",
                "(a+)+$", "{cliente}", strrep("x", 5e6), list("luis"), c("a", "b"))
sin_efecto <- vapply(ataques, function(a) {
  todos <- c("nombre", "puesto", "usuario", "contrasena")
  all(vapply(todos, function(campo) {
    campos <- buenos; campos[[campo]] <- a; if (campo == "contrasena") campos$confirmar <- a
    r <- tryCatch(activar_invitacion(inv, campos), error = function(e) list(ok = "error"))
    if (isTRUE(r$ok)) {
      # Only a harmless text may pass (quotes in a name): stored as typed,
      # then the account is removed again for the next attempt.
      g <- get(K_U, envir = .mem); fila <- g[g$username == "mjnunez", ]
      bien <- !grepl("[<>{}]", paste(fila$display_name, fila$puesto))
      reiniciar(); inv <<- resolve_invite(get(K_INV, envir = .mem)$token[1]); bien
    } else identical(r$ok, FALSE) && nada_guardado()
  }, logical(1)))
}, logical(1))
ok(sprintf("%d ataques en los 4 campos: rechazados o guardados como texto, nunca como marcado",
           length(ataques)), all(sin_efecto))

# ── 6. tr_negritas ───────────────────────────────────────────────────────────
h <- as.character(tr_negritas("cuentas.activacion.saludo", nombre = "<script>alert(1)</script>",
                              cliente = "A & B \\1 {nombre}"))
ok("tr_negritas escapa el nombre y el cliente", !grepl("<script>", h, fixed = TRUE) &&
     grepl("&lt;script&gt;", h, fixed = TRUE) && grepl("A &amp; B", h, fixed = TRUE))
ok("... una barra invertida o un {placeholder} escrito quedan como texto",
   grepl("\\1 {nombre}", h, fixed = TRUE) && grepl("^Hola <strong>", h))

# ── 7. The page ──────────────────────────────────────────────────────────────
app <- readLines(F_APP, warn = FALSE, encoding = "UTF-8")
ini <- grep("^\\.invite_page_shell <- function", app); fin <- grep("^  # The activation page mounts nothing else", app)
bloque <- if (length(ini) == 1 && length(fin) == 1) app[ini:fin] else character()
codigo <- sub("#.*$", "", bloque)
viejos <- c("Activa tu cuenta\"", "\"Cuenta activada\"", "\"Enlace no válido\"", "Mínimo 6", "\"Nueva contraseña\"",
            "\"Activar cuenta\"", "Las contraseñas no coinciden", "Tu cuenta de HopDesk está activa")
ok("la página no tiene ninguno de sus textos fijos de antes",
   length(bloque) > 0 && !any(vapply(viejos, function(v) any(grepl(v, codigo, fixed = TRUE)), logical(1))))
ok("no hay HTML(sprintf(...)) con nombres; el saludo va por tr_negritas()",
   !any(grepl("HTML(sprintf(", codigo, fixed = TRUE)) && any(grepl("tr_negritas(", codigo, fixed = TRUE)))
ok("la página lleva <meta name=referrer content=no-referrer>",
   any(grepl('tags$meta(name = "referrer", content = "no-referrer")', codigo, fixed = TRUE)))
ok("al activar, el token sale de la barra (history.replaceState)",
   any(grepl("history.replaceState", codigo, fixed = TRUE)))
ok("la activación pasa por activar_invitacion() con los 5 campos",
   any(grepl("activar_invitacion(invite", codigo, fixed = TRUE)) &&
     all(vapply(c("invite_nombre", "invite_puesto", "invite_usuario", "invite_pw_new", "invite_pw_conf"),
                function(i) any(grepl(paste0("input$", i), codigo, fixed = TRUE)), logical(1))))
claves <- unique(unlist(regmatches(paste(bloque, collapse = "\n"),
                                   gregexpr("cuentas\\.[a-z_]+\\.[a-z_]+", paste(bloque, collapse = "\n")))))
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
ok(sprintf("las %d claves cuentas.* de la página existen en es y en", length(claves)),
   length(claves) >= 20L && all(claves %in% names(es)) && all(claves %in% names(en)))
ok("ningún message() del bloque imprime QUERY_STRING ni url_search",
   !any(grepl("message\\(.*(QUERY_STRING|url_search|token)", codigo)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
