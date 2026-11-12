# =============================================================================
# tests/helpers/cuentas_fixture_app.R
# T31: the account page above secure_app -- ?invite=, ?olvide=, ?reset= --
# as its own app, for tests/test_t31_cuentas_chromote.R. It runs app.R's
# real shell, ui() and the very block of server() that fills the page, over:
#   - S3 in memory (tests/helpers/s3_memoria.R), seeded with one client
#     ("acme", account ana / ana@x.mx) and one pending invitation;
#   - mail simulated: send_email() writes each message's HTML to
#     CUENTAS_CORREOS/<n>.html and sends nothing;
#   - no network: dead proxy, no AWS keys; log and alerts to files.
# Login (no parameter) is a stub page: this fixture never boots the app.
# Env: CUENTAS_DIR (files the test reads: invite token, mails, log),
#      HOPDESK_APP_URL (the base of the mailed link: this fixture's address).
# =============================================================================
local({
  muerto <- "http://127.0.0.1:9"
  Sys.setenv(http_proxy = muerto, https_proxy = muerto, HTTP_PROXY = muerto, HTTPS_PROXY = muerto,
             no_proxy = "127.0.0.1,localhost", NO_PROXY = "127.0.0.1,localhost")
  Sys.unsetenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN", "RESEND_API_KEY"))
})
suppressPackageStartupMessages(library(shiny))
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/i18n.R"))
# runApp() moves the working directory to the app file's: read both
# dictionaries now, from the repo (tr() caches them).
invisible(i18n_dict("es")); invisible(i18n_dict("en"))
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
suppressMessages(source("R/email_service.R"))
source("R/usuarios_cuentas.R")
source("R/auth_reset.R")

DIR <- Sys.getenv("CUENTAS_DIR"); stopifnot(nzchar(DIR))
dir.create(file.path(DIR, "correos"), showWarnings = FALSE, recursive = TRUE)
auth_invalidate_credentials <- function() invisible(NULL)
.migrate_legacy_passwords <- function(raw, cid) raw
auth_get_credentials <- function() data.frame(user = character(), stringsAsFactors = FALSE)
update_client_user_count <- function(...) invisible(NULL)
notify_user_limit <- function(...) invisible(NULL)
.n_correo <- 0L
send_email <- function(to, subject, html, queue_on_fail = TRUE) {
  .n_correo <<- .n_correo + 1L
  writeLines(c(to, subject, html), file.path(DIR, "correos", sprintf("%03d.html", .n_correo)), useBytes = TRUE)
  TRUE
}
log_action <- function(user, module, action, description, ...)
  cat(paste(user, action, description, sep = " | "), "\n", file = file.path(DIR, "log.txt"), append = TRUE)
avisos_agregar <- function(client_id, aviso, por = "sistema") {
  cat(client_id, aviso$clave, "\n", file = file.path(DIR, "alertas.txt"), append = TRUE); "id" }

# ── Data ─────────────────────────────────────────────────────────────────────
reg <- .hd_admin_agregar(.schema_client_registry()[0, , drop = FALSE],
                         data.frame(client_id = "acme", display_name = "Acme SA", status = "active",
                                    stringsAsFactors = FALSE))
assign("hd-admin/client_registry.rds", reg, envir = .mem)
ana <- data.frame(id = "id_ana", account_code = "U0001", username = "ana", password_hash = hash_password("Vieja#2026"),
                  display_name = "Ana", tier = "admin", client_id = "acme", permisos = "{}", group_ids = "[]",
                  allowed_clients = "[]", email = "ana@x.mx", requires_password_change = FALSE, activo = TRUE,
                  created_at = "", last_login = NA_character_, deleted = FALSE, deleted_at = NA_character_,
                  stringsAsFactors = FALSE)
assign("acme/usuarios.rds", ana, envir = .mem)
# No hd-admin/usuarios.rds: a missing file is "no accounts" (an existing
# empty one is a failed read for the strict reader).
# Empty lock list with every column, as the app writes it (a 0-row table
# missing columns trips .hd_admin_completar()).
assign("hd-admin/emergency_lock.rds", .schema_emergency_lock(), envir = .mem)
assign("hd-admin/password_resets.rds", .schema_password_resets(), envir = .mem)
assign("hd-admin/username_index.rds", data.frame(username = "ana", home_folder = "acme", account_code = "U0001",
                                                 created_at = "", active = TRUE, stringsAsFactors = FALSE), envir = .mem)
assign("hd-admin/pending_invites.rds", .schema_pending_invites(), envir = .mem)
tok <- create_invite("maria@x.mx", "María <b>Núñez</b>", "acme", "admin", "luis")
writeLines(tok, file.path(DIR, "invite_token.txt"))

# ── app.R's shell, ui() and the account block of server() ────────────────────
app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
for (e in parse(text = app, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".invite_page_shell", ".hopdesk_brandmark", "ui") &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) eval(e, globalenv())
secured_ui <- function(request) fluidPage(tags$p(id = "login-stub", "LOGIN"))
ini <- grep("^  # ── Invitation activation", app)
fin <- grep("^    message\\(\"\\[INVITE\\] account page", app) + 2L
server <- eval(parse(text = c("function(input, output, session) {", ".sn <- 1L", app[ini:fin], "}")))
shinyApp(ui, server)
