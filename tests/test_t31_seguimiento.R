# =============================================================================
# tests/test_t31_seguimiento.R
# The two findings left by T31 (feat/t31-seguimiento):
#   A. The forced password change (app.R, "First-login password change
#      gate" and force_pw_submit) read -- and saved -- the user table of
#      CLIENT_ID (hd-admin in production) instead of the account's own
#      folder: a client's user never saw the modal, and a save would have
#      gone to hd-admin's table. Run with testServer on that very block of
#      app.R, with the real auth_load_usuarios()/auth_save_usuarios() over S3
#      in memory.
#   B. .hd_admin_completar() (R/persistence.R) threw on a 0-row table that
#      lacked a column of its schema ("replacement has 1 row, data has 0").
# Run from project root:  Rscript tests/test_t31_seguimiento.R
# APP_R / PERSIST_R = other copies (contraprueba).
# =============================================================================
cat("=== test_t31_seguimiento ===\n\n")
suppressPackageStartupMessages(library(shiny))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
F_PER <- Sys.getenv("PERSIST_R", "R/persistence.R")
if (!identical(F_PER, "R/persistence.R")) source(F_PER)
suppressMessages(source("R/i18n.R"))
source("R/tier_registry.R")
for (e in parse("R/auth.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("auth_load_usuarios", "auth_save_usuarios", ".usuarios_leer_estricto",
                                  ".usuarios_existe", "usuarios_vivos_nivel", "usuarios_niveles_perdidos",
                                  "hash_password", "verify_password"))
    eval(e, globalenv())
source("R/usuarios_cuentas.R")
auth_invalidate_credentials <- function() invisible(NULL)
.migrate_legacy_passwords <- function(raw, cid) raw

# ── B. .hd_admin_completar() ─────────────────────────────────────────────────
esquema <- function() data.frame(username = character(), locked_at = as.POSIXct(character(), tz = "UTC"),
                                 reason = character(), stringsAsFactors = FALSE)
r0 <- tryCatch(.hd_admin_completar(data.frame(username = character(), stringsAsFactors = FALSE), esquema),
               error = function(e) e)
ok("B: una tabla de 0 filas sin una columna se completa sin error",
   is.data.frame(r0) && nrow(r0) == 0L && identical(names(r0), names(esquema())))
ok("B: ... y cada columna nueva lleva el tipo del esquema (POSIXct sigue siendo POSIXct)",
   is.data.frame(r0) && inherits(r0$locked_at, "POSIXct") && is.character(r0$reason))
r2 <- .hd_admin_completar(data.frame(username = c("a", "b"), stringsAsFactors = FALSE), esquema)
ok("B: con filas, cada columna nueva va en NA en todas ellas",
   nrow(r2) == 2L && all(is.na(r2$reason)) && all(is.na(r2$locked_at)) && inherits(r2$locked_at, "POSIXct"))
ok("B: una tabla NULL sigue dando el esquema vacío", identical(.hd_admin_completar(NULL, esquema), esquema()))
ok("B: la lista de bloqueos vacía con una sola columna ya no tumba read_emergency_lock()",
   { assign("hd-admin/emergency_lock.rds", data.frame(username = character(), stringsAsFactors = FALSE), envir = .mem)
     .emergency_lock_cache$data <- NULL; .emergency_lock_cache$ts <- NULL
     l <- read_emergency_lock(); is.data.frame(l) && nrow(l) == 0L && "reason" %in% names(l) })

# ── A. The forced password change ────────────────────────────────────────────
F_APP <- Sys.getenv("APP_R", "app.R")
app <- readLines(F_APP, warn = FALSE, encoding = "UTF-8")
ini <- grep("^  # ── First-login password change gate", app)
sub <- grep("observeEvent\\(input\\$force_pw_submit", app)
fin <- sub - 1L + grep("^  \\}, ignoreInit = TRUE\\)", app[sub:length(app)])[1]
servidor <- eval(parse(text = c("function(input, output, session) {", app[ini:fin], "}")))

cuenta <- function(name, cid, tier = "admin", cambio = FALSE)
  data.frame(id = paste0("id_", name), account_code = "U0001", username = name, password_hash = hash_password("Vieja#2026"),
             display_name = name, tier = tier, client_id = cid, permisos = "{}", group_ids = "[]", allowed_clients = "[]",
             email = NA_character_, requires_password_change = cambio, activo = TRUE, created_at = "",
             last_login = NA_character_, deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE)
STAFF <- niveles_staff()[1]
modales <- 0L; avisos <- character()
showModal <- function(ui) { modales <<- modales + 1L; invisible(NULL) }
removeModal <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { avisos <<- c(avisos, paste(ui, collapse = "")); invisible("id") }
Sys.setenv(CLIENT_ID = "hd-admin")   # as in production
reiniciar <- function() {
  rm(list = ls(.mem), envir = .mem); modales <<- 0L; avisos <<- character()
  assign("acme/usuarios.rds", rbind(cuenta("ana", "acme", cambio = TRUE), cuenta("beto", "acme")), envir = .mem)
  assign("hd-admin/usuarios.rds", rbind(cuenta("staff1", "hd-admin", STAFF), cuenta("staff2", "hd-admin", STAFF)),
         envir = .mem)
}
correr <- function(user, cid, cuerpo) {
  current_user_info <<- reactive(list(user = user, client_id = cid, tier = "admin", is_staff = FALSE))
  home_client_id <<- reactiveVal(NULL)   # the block also holds the home-client observer
  testServer(servidor, { session$flushReact(); cuerpo(session) })
}

reiniciar()
correr("ana", "acme", function(session) {
  ok("A: un usuario de cliente con cambio pendiente ve el modal (en prod, CLIENT_ID = hd-admin)", modales == 1L)
  hd_antes <- get("hd-admin/usuarios.rds", envir = .mem)
  session$setInputs(force_pw_new = "Nueva#2026x", force_pw_conf = "Nueva#2026x", force_pw_submit = 1)
  session$flushReact()
  a <- get("acme/usuarios.rds", envir = .mem); fila <- a[a$username == "ana", ]
  ok("A: la contraseña nueva se guarda en la carpeta de su cliente, con password_changed_at",
     verify_password("Nueva#2026x", fila$password_hash) && isFALSE(fila$requires_password_change) &&
       !is.na(fila$password_changed_at))
  ok("A: la tabla de hd-admin no se toca", identical(get("hd-admin/usuarios.rds", envir = .mem), hd_antes))
  ok("A: y avisa que se actualizó", any(grepl(tr("niveles.usuarios.aviso.contrasena_actualizada"), avisos, fixed = TRUE)))
})
reiniciar()
correr("beto", "acme", function(session)
  ok("A: un usuario sin cambio pendiente no ve el modal", modales == 0L))
reiniciar()
x <- get("hd-admin/usuarios.rds", envir = .mem); x$requires_password_change[1] <- TRUE
assign("hd-admin/usuarios.rds", x, envir = .mem)
correr("staff1", "hd-admin", function(session)
  ok("A: una cuenta de hd-admin con cambio pendiente lo sigue viendo", modales == 1L))
reiniciar(); .mem_falla <- "acme/usuarios.rds"
correr("ana", "acme", function(session)
  ok("A: si la tabla del cliente no se lee, no hay modal ni error", modales == 0L))
.mem_falla <- character()

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
