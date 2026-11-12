# =============================================================================
# tests/test_t31_usuarios.R
# T31 in Grupo ▸ Usuarios (R/tiers_module.R), on the real tiersServer:
#   1. the table shows name, user name and job title (an old account's job
#      title is empty);
#   2. ✎: the user name is NOT editable -- a forged edit_username changes
#      nothing --; job title and e-mail are saved (the e-mail in lowercase),
#      a bad e-mail or an injected job title is refused with nothing saved,
#      and an emptied job title is cleared;
#   3. "Nueva cuenta": job title required; the user name is reserved in the
#      global index; a name taken in another client is refused, nothing
#      saved; the one password rule.
# S3 is the in-memory store of tests/helpers/s3_memoria.R (no network).
# Run from project root:  Rscript tests/test_t31_usuarios.R
# =============================================================================
cat("=== test_t31_usuarios ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
# As test_admin_usuarios_hd.R: every function definition in R/, then the
# files this module needs in full.
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
              "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/usuarios_cuentas.R", "R/tiers_module.R"))
    if (file.exists(f)) source(f, local = FALSE)
})
IS_ADMIN_DEPLOYMENT <- TRUE
log_action <- function(...) invisible(NULL)
auth_get_credentials <- function() data.frame(user = character(), stringsAsFactors = FALSE)
update_client_user_count <- function(...) invisible(NULL)
notify_user_limit <- function(...) invisible(NULL)

DEV <- niveles_unico_vivo()[1]
u <- function(name, tier = "admin", activo = TRUE)
  data.frame(id = paste0("id_", name), account_code = paste0("U000", match(name, c("dev1", "ana", "carla"))),
             username = name, password_hash = "x", display_name = toupper(name), tier = tier, client_id = "acme",
             permisos = "{}", group_ids = "[]", allowed_clients = "[]", email = NA_character_,
             requires_password_change = FALSE, activo = activo, created_at = "", last_login = NA_character_,
             deleted = FALSE, deleted_at = NA_character_, stringsAsFactors = FALSE)
TABLA <- rbind(u("dev1", DEV), u("ana"), u("carla"))
TABLA$puesto <- c("Dirección", NA, NA)
KEY <- "acme/usuarios.rds"; K_I <- "hd-admin/username_index.rds"

# ── 1. The rows ──────────────────────────────────────────────────────────────
f <- .usuarios_filas(TABLA)
ok("la tabla trae nombre, usuario y puesto", all(c("nombre", "usuario", "puesto") %in% names(f)) &&
     identical(f$usuario, c("dev1", "ana", "carla")) && identical(f$puesto[1], "Dirección") && is.na(f$puesto[2]))
ok("una tabla vieja sin columna puesto da puestos vacíos", all(is.na(.usuarios_filas(TABLA[, names(TABLA) != "puesto"])$puesto)))

# ── 2 and 3. The real tiersServer ────────────────────────────────────────────
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

s3_memoria_reset(); assign(KEY, TABLA, envir = .mem)
assign(K_I, data.frame(username = c("dev1", "ana", "carla", "ocupado"), home_folder = c("acme", "acme", "acme", "otro"),
                       account_code = c("U0001", "U0002", "U0003", "U0001"), created_at = "", active = TRUE,
                       stringsAsFactors = FALSE), envir = .mem)
s3 <- function() get(KEY, envir = .mem)
.al_final <- FALSE
res <- tryCatch(testServer(tiersServer, args = list(shared = shared), {
  session$flushReact()
  clic <- function(a, id, n) { session$setInputs(`usuarios-tabla-accion` = list(accion = a, ids = id, nonce = n)); session$flushReact() }
  guardar <- local({ n <- 0L; function(...) {
    n <<- n + 1L
    session$setInputs(edit_tier = "admin", edit_activo = TRUE, edit_password = "", edit_password2 = "", ...)
    session$setInputs(btn_save_perms = n); session$flushReact() } })

  # ── ✎ ──
  clic("editar", "carla", 1)
  guardar(edit_username = "hacker", edit_display_name = "Carla Ruiz", edit_puesto = " Tesorera ",
          edit_email = " Carla@Empresa.MX ")
  fila <- s3()[s3()$id == "id_carla", ]
  ok("✎: un edit_username forjado no cambia el usuario", identical(fila$username, "carla") &&
       !any(s3()$username == "hacker"))
  ok("✎: nombre, puesto y correo (en minúsculas) se guardan",
     identical(fila$display_name, "Carla Ruiz") && identical(fila$puesto, "Tesorera") &&
       identical(fila$email, "carla@empresa.mx"))

  antes <- s3(); avisos <<- character(0)
  clic("editar", "carla", 2)
  guardar(edit_display_name = "Carla Ruiz", edit_puesto = "Tesorera", edit_email = "carla@empresa.mx<script>")
  ok("✎: un correo inválido se rechaza con aviso y no se guarda nada",
     identical(s3(), antes) && any(grepl(tr("cuentas.error.correo_formato"), avisos, fixed = TRUE)))
  clic("editar", "carla", 3)
  guardar(edit_display_name = "Carla Ruiz", edit_puesto = "<img src=x onerror=alert(1)>", edit_email = "carla@empresa.mx")
  ok("✎: un puesto con marcado se rechaza y no se guarda nada", identical(s3(), antes))
  clic("editar", "carla", 4)
  guardar(edit_display_name = "Carla Ruiz", edit_puesto = "", edit_email = "carla@empresa.mx")
  ok("✎: vaciar el puesto lo borra (Luis lo llena después)", is.na(s3()$puesto[s3()$id == "id_carla"]))

  # ── Nueva cuenta ──
  nueva <- local({ n <- 0L; function(...) {
    n <<- n + 1L
    do.call(session$setInputs, utils::modifyList(
      list(new_tier = "admin", new_activo = TRUE, new_password = "Segura#2026", new_password2 = "Segura#2026"),
      list(...)))
    session$setInputs(btn_save_new_user = n); session$flushReact() } })
  antes <- s3(); avisos <<- character(0)
  nueva(new_display_name = "Diana", new_username = "diana", new_puesto = "")
  ok("alta sin puesto: se rechaza con aviso y no se guarda nada",
     identical(s3(), antes) && any(grepl(tr("cuentas.error.puesto_obligatorio"), avisos, fixed = TRUE)))
  nueva(new_display_name = "Diana", new_username = "ocupado", new_puesto = "Contadora")
  ok("alta con un usuario de OTRO cliente: se rechaza y no se guarda nada", identical(s3(), antes))
  nueva(new_display_name = "Diana", new_username = "diana", new_puesto = "Contadora",
        new_password = "sinnumero!", new_password2 = "sinnumero!")
  ok("alta con contraseña fuera de la regla: se rechaza", identical(s3(), antes))
  nueva(new_display_name = "Diana", new_username = "diana", new_puesto = "Contadora")
  d <- s3()[s3()$username %in% "diana", ]
  ok("alta buena: la cuenta trae su puesto y se reserva en el índice global",
     nrow(d) == 1L && identical(d$puesto, "Contadora") &&
       any(get(K_I, envir = .mem)$username == "diana" & get(K_I, envir = .mem)$active))
  .al_final <<- TRUE
}), error = function(e) { message("  ERROR testServer: ", conditionMessage(e)); FALSE })
ok("el módulo corrió hasta el final", isTRUE(.al_final))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail) quit(status = 1)
