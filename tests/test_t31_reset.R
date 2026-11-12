# =============================================================================
# tests/test_t31_reset.R
# T31: "¿Olvidaste tu contraseña?" (R/auth_reset.R), without Shiny.
#   1. a request for an account: one mail, one button per account, the user
#      name almost hidden, the link built from the pinned base URL; only the
#      sha256 of the token is stored;
#   2. the link: one use, 60 minutes, a newer request cancels it; the new
#      password is saved with scrypt and password_changed_at;
#   3. limits: 3 per e-mail per hour, 5 links per account per day, 30
#      requests per 15 minutes in all of HopDesk;
#   4. fail closed: an unreadable password_resets, registry, users file or
#      lock list sends nothing; no HOPDESK_APP_URL sends nothing;
#   5. who hears it: staff in hd-admin; the client's Dev only for its own
#      accounts, never for a staff account, never naming HopDesk's tiers;
#   6. the log never holds the token, the password or the e-mail;
#   7. injection and forged tokens: refused, nothing written.
# S3 is the in-memory store of tests/helpers/s3_memoria.R (no network).
# Run from project root:  Rscript tests/test_t31_reset.R
# RESET_R = another copy of R/auth_reset.R (contraprueba).
# =============================================================================
cat("=== test_t31_reset ===\n\n")
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
suppressMessages(source("R/email_service.R"))
source("R/usuarios_cuentas.R")
source(Sys.getenv("RESET_R", "R/auth_reset.R"))
auth_invalidate_credentials <- function() invisible(NULL)
.migrate_legacy_passwords <- function(raw, cid) raw

registro <- list(); alertas <- list(); correos <- list()
log_action <- function(user, module, action, description, target_id = NA, s3_key = NA, metadata = NULL,
                       client_id = NULL, viewer_home_client_id = NULL) {
  registro[[length(registro) + 1L]] <<- list(user = user, action = action, description = description,
                                             target_id = target_id, client_id = client_id,
                                             metadata = jsonlite::toJSON(metadata, auto_unbox = TRUE))
  invisible(NULL)
}
avisos_agregar <- function(client_id, aviso, por = "sistema") {
  alertas[[length(alertas) + 1L]] <<- c(list(cid = client_id), aviso); "id" }
envio_ok <- TRUE
send_email <- function(to, subject, html, queue_on_fail = TRUE) {
  correos[[length(correos) + 1L]] <<- list(to = to, subject = subject, html = html, cola = queue_on_fail)
  envio_ok }

STAFF <- niveles_staff()[1]; DEV <- nivel_ti()
cuenta <- function(name, email, tier = "admin", cid = "acme", activo = TRUE)
  data.frame(id = paste0("id_", name), account_code = "U0001", username = name, password_hash = hash_password("Vieja#2026"),
             display_name = name, tier = tier, client_id = cid, permisos = "{}", group_ids = "[]", allowed_clients = "[]",
             email = email, requires_password_change = FALSE, activo = activo, created_at = "", last_login = NA_character_,
             deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE)
BASE <- "https://hopdesk.app"
T0 <- as.POSIXct("2026-11-12 10:00:00", tz = "UTC")
K_R <- "hd-admin/password_resets.rds"
reiniciar <- function() {
  rm(list = ls(.mem), envir = .mem)
  .mem_falla <<- character(); .mem_falla_escritura <<- character()
  reg <- .schema_client_registry()[0, , drop = FALSE]
  reg <- .hd_admin_agregar(reg, data.frame(client_id = c("acme", "otro", "viejo"),
                                            display_name = c("<b>Acme</b> & Co", "Otro SA", "Viejo"),
                                            status = c("active", "active", "inactive"), stringsAsFactors = FALSE))
  assign("hd-admin/client_registry.rds", reg, envir = .mem)
  assign("acme/usuarios.rds", rbind(cuenta("ana", "ana@x.mx"), cuenta("dev1", "dev@x.mx", DEV),
                                    cuenta("multi", "multi@x.mx"), cuenta("carla", "carla@x.mx"),
                                    cuenta("dani", "dani@x.mx", activo = FALSE)), envir = .mem)
  assign("otro/usuarios.rds", cuenta("multi2", "Multi@X.mx", cid = "otro"), envir = .mem)
  assign("viejo/usuarios.rds", cuenta("viejo1", "viejo@x.mx", cid = "viejo"), envir = .mem)
  assign("hd-admin/usuarios.rds", cuenta("staff1", "staff@x.mx", STAFF, "hd-admin"), envir = .mem)
  assign("hd-admin/emergency_lock.rds", data.frame(username = "carla", stringsAsFactors = FALSE), envir = .mem)
  .emergency_lock_cache$data <- NULL; .emergency_lock_cache$ts <- NULL
  .reset_invalidos$t <- NULL
  assign(K_R, .schema_password_resets(), envir = .mem)   # exists: a failed read is not "new"
  registro <<- list(); alertas <<- list(); correos <<- list(); envio_ok <<- TRUE
}
tokens_de <- function(html) regmatches(html, gregexpr("(?<=\\?reset=)[0-9a-f]{64}", html, perl = TRUE))[[1]]
tabla <- function() get(K_R, envir = .mem)
pedir <- function(correo, ahora = T0, base = BASE) reset_solicitar(correo, ahora = ahora, base_url = base)

# ── 1. A request ─────────────────────────────────────────────────────────────
reiniciar()
r <- pedir(" Ana@X.MX ")
ok("una cuenta: un correo, a su dirección, sin pasar por la cola", identical(r$motivo, "enviado") &&
     length(correos) == 1L && identical(correos[[1]]$to, "ana@x.mx") && isFALSE(correos[[1]]$cola))
tk <- tokens_de(correos[[1]]$html)
ok("el correo trae un botón con el enlace base/?reset=<64 hex>",
   length(tk) >= 1L && length(unique(tk)) == 1L && grepl(paste0(BASE, "/?reset=", tk[1]), correos[[1]]$html, fixed = TRUE))
ok("el usuario va casi oculto (a…), nunca completo",
   grepl("a…", correos[[1]]$html, fixed = TRUE) && !grepl(">ana<|cuenta ana|Cuenta ana", correos[[1]]$html))
ok("el nombre del cliente va escapado en el correo",
   grepl("&lt;b&gt;Acme&lt;/b&gt; &amp;amp; Co|&lt;b&gt;Acme&lt;/b&gt; &amp; Co", correos[[1]]$html) &&
     !grepl("<b>Acme</b>", correos[[1]]$html, fixed = TRUE))
ok("en S3 solo está el sha256 del token, nunca el token",
   any(tabla()$token_hash %in% reset_hash(tk[1])) && !any(grepl(tk[1], unlist(tabla()), fixed = TRUE)))
ok("en S3 no queda el correo en claro", !any(grepl("ana@x.mx", unlist(tabla()), fixed = TRUE)))

reiniciar(); r <- pedir("multi@x.mx")
ok("un correo con dos cuentas (dos clientes): un solo correo con un botón por cuenta",
   length(correos) == 1L && length(unique(tokens_de(correos[[1]]$html))) == 2L && r$enviados == 2L)

reiniciar(); r <- pedir("nadie@x.mx")
ok("un correo sin cuenta: nada enviado", identical(r$motivo, "sin_cuenta") && !length(correos))
reiniciar(); r <- pedir("dani@x.mx"); r2 <- pedir("viejo@x.mx")
ok("una cuenta inactiva o de un cliente inactivo: nada enviado", !length(correos) &&
     identical(r$motivo, "sin_cuenta") && identical(r2$motivo, "sin_cuenta"))

# ── 2. The link ──────────────────────────────────────────────────────────────
reiniciar(); pedir("ana@x.mx"); tk <- tokens_de(correos[[1]]$html)[1]
ok("el enlace vigente se resuelve a su cuenta", identical(reset_resolver(tk, T0 + 60)$username, "ana"))
ok("a los 61 minutos ya no", is.null(reset_resolver(tk, T0 + 61 * 60)))
r <- reset_aplicar(tk, "corta", "corta", T0 + 60)
ok("una contraseña fuera de la regla se rechaza y el enlace sigue vigente",
   identical(r$error, "cuentas.error.contrasena_regla") && !is.null(reset_resolver(tk, T0 + 60, registrar = FALSE)))
r <- reset_aplicar(tk, "Nueva#2026x", "Nueva#2026x", T0 + 120)
u <- get("acme/usuarios.rds", envir = .mem); ana <- u[u$username == "ana", ]
ok("con el enlace se guarda la contraseña nueva (scrypt) y password_changed_at",
   isTRUE(r$ok) && verify_password("Nueva#2026x", ana$password_hash) && !verify_password("Vieja#2026", ana$password_hash) &&
     identical(ana$password_changed_at, "2026-11-12T10:02:00Z"))
ok("la respuesta solo trae el usuario oculto", identical(r$usuario_oculto, "a…") && !"username" %in% names(r))
ok("un solo uso: el mismo enlace ya no sirve",
   identical(reset_aplicar(tk, "Otra#20261", "Otra#20261", T0 + 180)$error, "cuentas.reset.enlace_invalido"))
reiniciar(); pedir("ana@x.mx", T0); pedir("ana@x.mx", T0 + 60)
t1 <- tokens_de(correos[[1]]$html)[1]; t2 <- tokens_de(correos[[2]]$html)[1]
ok("una solicitud nueva anula el enlace anterior", is.null(reset_resolver(t1, T0 + 120)) &&
     !is.null(reset_resolver(t2, T0 + 120)))
reiniciar(); pedir("ana@x.mx"); tk <- tokens_de(correos[[1]]$html)[1]; .mem_falla_escritura <- "acme/usuarios.rds"
r <- reset_aplicar(tk, "Nueva#2026x", "Nueva#2026x", T0 + 60); .mem_falla_escritura <- character()
ok("si el guardado falla: 'reintenta' y el enlace se devuelve (sigue vigente)",
   identical(r$error, "comun.reintentar") && !is.null(reset_resolver(tk, T0 + 120, registrar = FALSE)))

# ── 3. Limits ────────────────────────────────────────────────────────────────
reiniciar(); for (k in 0:3) r <- pedir("ana@x.mx", T0 + k * 60)
ok("la 4ª solicitud de un correo en una hora no envía nada", identical(r$motivo, "limite_correo") && length(correos) == 3L)
ok("... y a la hora siguiente vuelve a enviar", identical(pedir("ana@x.mx", T0 + 3700)$motivo, "enviado"))
reiniciar(); for (k in 0:5) r <- pedir("ana@x.mx", T0 + k * 2 * 3600)
ok("el 6º enlace de una cuenta en un día no se envía", identical(r$motivo, "limite_cuenta") && length(correos) == 5L)
reiniciar(); for (k in 1:31) r <- pedir(sprintf("p%02d@x.mx", k), T0 + k)
ok("la solicitud 31 en 15 minutos (todo HopDesk) se frena", identical(r$motivo, "limite_global"))
ok("... y avisa al staff en hd-admin", any(vapply(alertas, function(a) identical(a$cid, "hd-admin") &&
                                                    grepl("limite_global", a$clave), logical(1))))

# ── 4. Fail closed ───────────────────────────────────────────────────────────
falla <- function(key) { reiniciar(); .mem_falla <<- key; antes <- .mem_escrituras
  r <- pedir("ana@x.mx"); .mem_falla <<- character()
  identical(r$motivo, "lectura") && !length(correos) && .mem_escrituras == antes }
ok("password_resets.rds ilegible: nada enviado ni escrito", falla(K_R))
ok("el registro de clientes ilegible: nada enviado", falla("hd-admin/client_registry.rds"))
ok("los usuarios de un cliente ilegibles: nada enviado", falla("otro/usuarios.rds"))
ok("la lista de bloqueos ilegible (y nunca leída): nada enviado", falla("hd-admin/emergency_lock.rds"))
ok("... y una falla de lectura avisa al staff", any(vapply(alertas, function(a) grepl("lectura", a$clave), logical(1))))
reiniciar(); r <- pedir("ana@x.mx", base = "")
ok("sin HOPDESK_APP_URL: nada enviado y aviso al staff", identical(r$motivo, "sin_url") && !length(correos) &&
     any(vapply(alertas, function(a) identical(a$cid, "hd-admin") && grepl("sin_url", a$clave), logical(1))))
reiniciar(); envio_ok <- FALSE; r <- pedir("ana@x.mx")
ok("si el correo no sale, queda en el log como envio_fallido",
   identical(r$motivo, "envio_fallido") && any(vapply(registro, function(x) x$action == "reset_envio_fallido", logical(1))))

# ── 5. Who hears it ──────────────────────────────────────────────────────────
nombres_internos <- c(vapply(niveles_staff(), nivel_nombre, character(1)), "permiso", "nivel")
de <- function(cid) Filter(function(a) identical(a$cid, cid), alertas)
reiniciar(); r <- pedir("carla@x.mx")
ok("cuenta bloqueada: nada enviado; aviso al staff y al Dev de su cliente",
   !length(correos) && length(de("hd-admin")) == 1L && length(de("acme")) == 1L &&
     identical(de("acme")[[1]]$niveles, nivel_ti()))
reiniciar(); r <- pedir("staff@x.mx")
ok("cuenta de staff: se envía; el aviso va SOLO a hd-admin", identical(r$motivo, "enviado") &&
     length(alertas) >= 1L && all(vapply(alertas, function(a) identical(a$cid, "hd-admin"), logical(1))))
reiniciar(); r <- pedir("dev@x.mx")
ok("cuenta Dev de un cliente: aviso al staff y al Dev del cliente", length(de("hd-admin")) == 1L && length(de("acme")) == 1L)
ok("ningún aviso al cliente nombra niveles o permisos de HopDesk",
   { reiniciar(); pedir("dev@x.mx"); pedir("carla@x.mx"); for (k in 1:3) pedir("ana@x.mx", T0 + k)
     txt <- unlist(lapply(de("acme"), function(a) c(a$titulo, a$cuerpo)))
     length(txt) > 0 && !any(vapply(nombres_internos, function(n) any(grepl(n, txt, ignore.case = TRUE)), logical(1))) })
ok("3 solicitudes de una cuenta en una hora avisan (una vez por ventana)",
   { reiniciar(); for (k in 0:2) pedir("ana@x.mx", T0 + k * 60)
     sum(vapply(alertas, function(a) grepl("repetidas", a$clave) && identical(a$cid, "hd-admin"), logical(1))) >= 1L &&
       all(vapply(alertas, function(a) identical(a$ventana_min, 60), logical(1))) })
ok("el cambio con el enlace avisa al Dev del cliente (informativo)",
   { reiniciar(); pedir("ana@x.mx"); alertas <<- list()
     reset_aplicar(tokens_de(correos[[1]]$html)[1], "Nueva#2026x", "Nueva#2026x", T0 + 60)
     length(de("acme")) == 1L && identical(de("acme")[[1]]$severidad, "info") })

# ── 6. The log ───────────────────────────────────────────────────────────────
reiniciar(); pedir("ana@x.mx"); tk <- tokens_de(correos[[1]]$html)[1]
reset_resolver(paste0(substr(tk, 1, 63), "0"), T0 + 1); reset_aplicar(tk, "Nueva#2026x", "Nueva#2026x", T0 + 60)
pedir("nadie@x.mx", T0 + 2)
todo_log <- paste(unlist(registro), collapse = " | ")
acciones <- vapply(registro, `[[`, "", "action")
ok("el log registra solicitud, envío, enlace inválido y cambio",
   all(c("reset_solicitado", "reset_enviado", "reset_enlace_invalido", "reset_contrasena_cambiada") %in% acciones))
ok("el log no trae el token, la contraseña ni el correo",
   !grepl(tk, todo_log, fixed = TRUE) && !grepl("Nueva#2026x", todo_log, fixed = TRUE) &&
     !grepl("ana@x.mx", todo_log, fixed = TRUE) && !grepl("nadie@x.mx", todo_log, fixed = TRUE))
ok("la solicitud sin cuenta queda como anonimo con un hash corto del correo",
   any(vapply(registro, function(x) identical(x$user, "anonimo") &&
                grepl(substr(reset_hash("nadie@x.mx"), 1, 8), x$metadata, fixed = TRUE), logical(1))))

# ── 7. Injection and forged tokens ───────────────────────────────────────────
reiniciar(); antes <- .mem_escrituras
malos <- list("<script>alert(1)</script>@x.mx", "ana@x.mx\nBcc: z@z.mx", "ana@x.mx' OR '1'='1", ".*@.*",
              strrep("a", 5e6), list("ana@x.mx"), c("ana@x.mx", "multi@x.mx"), NA_character_, NULL)
ok("correos con inyección: formato inválido, nada leído de S3 ni enviado",
   all(vapply(malos, function(m) identical(pedir(m)$motivo, "formato"), logical(1))) && !length(correos) &&
     .mem_escrituras == antes)
tokens_malos <- list("<script>alert(1)</script>", "' OR 1=1 --", ".*", strrep("a", 64), strrep("A", 64),
                     strrep("0", 5e6), list("x"), c(strrep("0", 64), strrep("1", 64)), NA_character_, NULL)
ok("tokens forjados: ninguno se resuelve ni cambia nada",
   { reiniciar(); pedir("ana@x.mx"); antes <- get("acme/usuarios.rds", envir = .mem)
     all(vapply(tokens_malos, function(t) is.null(reset_resolver(t, T0 + 1)) &&
                  identical(reset_aplicar(t, "Nueva#2026x", "Nueva#2026x", T0 + 1)$ok, FALSE), logical(1))) &&
       identical(get("acme/usuarios.rds", envir = .mem), antes) })
ok("5 enlaces inválidos en 15 minutos avisan al staff una vez",
   { reiniciar(); for (k in 1:6) reset_resolver(strrep("f", 64), T0 + k)
     sum(vapply(alertas, function(a) grepl("invalidos", a$clave), logical(1))) == 1L })
ok("comparación en tiempo constante: igual/distinto/largo distinto",
   .igual_tiempo_constante("abc", "abc") && !.igual_tiempo_constante("abc", "abd") &&
     !.igual_tiempo_constante("abc", "abcd") && !.igual_tiempo_constante(NA_character_, "a"))
claves <- unique(unlist(regmatches(paste(readLines(Sys.getenv("RESET_R", "R/auth_reset.R"), warn = FALSE), collapse = "\n"),
                                   gregexpr("cuentas\\.[a-z_]+\\.[a-z_.]+[a-z]", paste(readLines(Sys.getenv("RESET_R", "R/auth_reset.R"), warn = FALSE), collapse = "\n")))))
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
reglas <- c("sin_url", "lectura", "bloqueada", "staff", "ti", "repetidas", "limite_global", "invalidos", "cambiada")
acc <- c("solicitado", "sin_url", "lectura_fallida", "limite", "enviado", "envio_fallido", "enlace_invalido", "contrasena_cambiada")
todas <- unique(c(setdiff(claves, c("cuentas.reset.log", "cuentas.reset.alerta")),   # paste0() prefixes
                  paste0("cuentas.reset.alerta.", rep(reglas, each = 2), c(".titulo", ".cuerpo")),
                  paste0("cuentas.reset.log.", acc)))
ok(sprintf("las %d claves del olvidé existen en es y en", length(todas)),
   { falta <- setdiff(todas, intersect(names(es), names(en))); if (length(falta)) message("  faltan: ", paste(falta, collapse = ", "))
     !length(falta) })

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
