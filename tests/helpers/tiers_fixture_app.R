# =============================================================================
# tests/helpers/tiers_fixture_app.R
# The Usuarios / Administración tab (R/tiers_module.R) as one viewer sees it,
# over S3 in memory -- never dev nor prod (T28, paso 0b). All of R/ is loaded
# the way shiny does it (shiny::loadSupport), after aws.s3 is replaced by the
# in-memory store of tests/helpers/s3_memoria.R, so every read and write of
# the module goes through the real persistence code into memory.
#
# TIERS_ROL: who is looking --
#   principal  mouse  (Principal, HopDesk staff, home hd-admin, viewing acme)
#   hopdesk    bunny  (HopDesk staff, home hd-admin, viewing acme)
#   dev        devacme (the client's Dev, home acme)
#   cliente    ana    (the client's Admin, home acme)
# TIERS_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# HD_ARRANQUE_NULL = 1: nothing in S3 at all (the sources start empty).
#
# Data (all of it fake): clients acme (5 seats, 4 used), beta (full) and
# gamma (archived); 2 pending client requests; invitations of acme and beta;
# a queued and a failed email; 2 locked accounts; a pending hop request and
# 2 active hop accesses; 2 active contacts of acme.
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("TIERS_RAIZ", "."), winslash = "/")
rol  <- Sys.getenv("TIERS_ROL", "principal")
stopifnot(rol %in% c("principal", "hopdesk", "dev", "cliente"))

# ── S3 in memory, then R/ as shiny loads it ─────────────────────────────────
# Nothing here may reach AWS (2026-11-09: a first version let get_bucket()
# list a bucket for real -- read-only, but out of the machine):
#   - no credentials: R/global.R reads .Renviron and .Renviron.local from the
#     working directory (and with both it copies prod's accounts to dev), so
#     R/ is loaded from an empty folder, and AWS_* is cleared before and after;
#   - every other aws.s3 function that talks to the network stops, except the
#     bucket listings (pagos generados, app audit), which come back empty.
.sin_llaves <- function() Sys.unsetenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN",
                                         "AWS_DEFAULT_REGION", "AWS_PROFILE"))
.sin_llaves()
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  source("tests/helpers/s3_memoria.R", local = globalenv())
})
local({
  ns <- asNamespace("aws.s3")
  propias <- c("s3readRDS", "s3saveRDS", "object_exists")   # in memory (s3_memoria.R)
  for (nm in setdiff(getNamespaceExports("aws.s3"), propias)) {
    if (!is.function(get(nm, envir = ns))) next
    f <- if (nm %in% c("get_bucket", "get_bucket_df")) local({ n <- nm; function(...) if (n == "get_bucket") list() else data.frame() })
         else local({ n <- nm; function(...) stop(sprintf("aws.s3::%s is blocked in the T28 fixture (no network)", n), call. = FALSE) })
    utils::assignInNamespace(nm, f, "aws.s3")
  }
})
local({
  vacia <- tempfile("t28_sin_renviron_"); dir.create(vacia)
  owd <- setwd(vacia); on.exit(setwd(owd))
  Sys.unsetenv("HOPDESK_PRODUCTION")
  shiny::loadSupport(raiz, renv = globalenv(), globalrenv = NULL)
})
.sin_llaves()
Sys.setenv(S3_BUCKET = "fake-bucket", CLIENT_ID = "acme")
Sys.unsetenv("HOPDESK_PRODUCTION")
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
# No email leaves this app.
assign("email_service_status", function(...) list(mode = "sandbox", label = "Correo de prueba (fixture)",
                                                   detail = "Nada sale de esta app."), envir = globalenv())
assign("retry_queued_email", function(queue_id) FALSE, envir = globalenv())
assign("retry_all_queued_emails", function(...) list(sent = 0L, failed = 0L), envir = globalenv())

# ── Fake data ───────────────────────────────────────────────────────────────
# One row of a schema: the values given, NA for the rest, in schema order.
fila <- function(schema, ...) {
  v <- list(...); s <- schema()
  out <- lapply(names(s), function(n) {
    x <- if (!is.null(v[[n]])) v[[n]] else NA
    if (inherits(s[[n]], "POSIXct")) as.POSIXct(x, tz = "America/Mexico_City")
    else methods::as(x, class(s[[n]])[1])
  })
  as.data.frame(stats::setNames(out, names(s)), stringsAsFactors = FALSE)
}
tabla <- function(...) do.call(rbind, list(...))
usuario <- function(name, tier, cid, display = name, activo = TRUE, code = paste0("U-", name))
  data.frame(id = paste0("id_", name), account_code = code, username = name, password_hash = "x",
             display_name = display, tier = tier, client_id = cid, permisos = "{}", group_ids = "[]",
             allowed_clients = "[]", email = paste0(name, "@ejemplo.com"), requires_password_change = FALSE,
             activo = activo, created_at = "2026-10-01", last_login = NA_character_, deleted = FALSE,
             deleted_at = NA_character_, stringsAsFactors = FALSE)
ahora <- Sys.time()
poner <- function(key, df) assign(key, df, envir = .mem)

if (!identical(Sys.getenv("HD_ARRANQUE_NULL"), "1")) {
  poner("hd-admin/usuarios.rds", tabla(
    usuario("mouse", "principal", "hd-admin", "Mouse"),
    usuario("bunny", "hopdesk", "hd-admin", "Bunny")))
  poner("acme/usuarios.rds", tabla(
    usuario("devacme", "dev", "acme", "Dev de Acme"),
    usuario("ana", "admin", "acme", "Ana Martínez"),
    usuario("beto", "finance", "acme", "Beto Ruiz"),
    usuario("caro", "analysis", "acme", "Caro Díaz", activo = FALSE)))
  poner("hd-admin/client_registry.rds", tabla(
    fila(.schema_client_registry, client_id = "acme", display_name = "Acme Logística", max_users = 5L,
         current_users = 4L, contact_email = "ti@acme.mx", status = "active"),
    fila(.schema_client_registry, client_id = "beta", display_name = "Beta Transportes", max_users = 3L,
         current_users = 3L, contact_email = "admin@beta.mx", status = "active"),
    fila(.schema_client_registry, client_id = "gamma", display_name = "Gamma (archivado)", max_users = 10L,
         current_users = 0L, status = "archived")))
  poner("hd-admin/client_requests.rds", tabla(
    fila(.schema_client_requests, id = "req-1", requested_by = "bunny", client_id_proposed = "delta",
         display_name = "Delta Carga", contact_email = "it@delta.mx", max_users = 8L, status = "pending",
         requested_at = "2026-11-07 10:15:00"),
    fila(.schema_client_requests, id = "req-2", requested_by = "bunny", client_id_proposed = "epsilon",
         display_name = "Épsilon Aduanas", max_users = 4L, status = "pending", requested_at = "2026-11-08 09:02:00"),
    fila(.schema_client_requests, id = "req-0", requested_by = "bunny", client_id_proposed = "beta",
         display_name = "Beta Transportes", max_users = 3L, status = "approved", requested_at = "2026-10-01 12:00:00")))
  poner(.INVITES_KEY, tabla(
    fila(.schema_pending_invites, token = "tok-acme-1", email = "nuevo@acme.mx", display_name = "Nuevo Acme",
         client_id = "acme", tier = "finance", invited_by = "devacme", created_at = "2026-11-06",
         expires_at = "2026-11-13 10:00:00", status = "pending"),
    fila(.schema_pending_invites, token = "tok-acme-2", email = "otra@acme.mx", display_name = "Otra Acme",
         client_id = "acme", tier = "analysis", invited_by = "ana", created_at = "2026-11-07",
         expires_at = "2026-11-14 10:00:00", status = "pending"),
    fila(.schema_pending_invites, token = "tok-beta-1", email = "x@beta.mx", display_name = "Beta Uno",
         client_id = "beta", tier = "dev", invited_by = "bunny", created_at = "2026-11-05",
         expires_at = "2026-11-12 10:00:00", status = "pending"),
    fila(.schema_pending_invites, token = "tok-acme-0", email = "vieja@acme.mx", display_name = "Revocada",
         client_id = "acme", tier = "admin", invited_by = "devacme", created_at = "2026-10-01",
         expires_at = "2026-10-08 10:00:00", status = "revoked")))
  poner("hd-admin/email_queue.rds", tabla(
    fila(.schema_email_queue, id = "q-1", to = "nuevo@acme.mx", subject = "Te invitaron a HopDesk",
         queued_at = "2026-11-06 10:00:00", attempts = 1L, status = "pending"),
    fila(.schema_email_queue, id = "q-2", to = "x@beta.mx", subject = "Te invitaron a HopDesk",
         queued_at = "2026-11-05 10:00:00", attempts = 5L, last_error = "401", status = "failed"),
    fila(.schema_email_queue, id = "q-3", to = "ok@acme.mx", subject = "Enviado", attempts = 1L, status = "sent")))
  poner("hd-admin/emergency_lock.rds", tabla(
    fila(.schema_emergency_lock, username = "beto", locked_at = "2026-11-07 18:30:00", locked_by = "mouse",
         reason = "Credenciales comprometidas"),
    fila(.schema_emergency_lock, username = "exempleado", locked_at = "2026-11-01 09:00:00", locked_by = "mouse",
         reason = "Baja")))
  poner(.HOP_REQUESTS_KEY, tabla(
    fila(.schema_hop_requests, id = "hop-1", requester = "bunny", requester_name = "Bunny",
         clients_json = '["acme","beta"]', decisions_json = "{}", message = "Revisar conciliación",
         requested_at = format(ahora - 600), status = "pending")))
  poner(.HOP_GRANTS_KEY, tabla(
    fila(.schema_hop_grants, id = "g-1", request_id = "hop-0", grantee = "bunny", client_id = "acme",
         granted_by = "mouse", granted_at = format(ahora - 3600), expires_at = format(ahora + 86400), revoked = FALSE),
    fila(.schema_hop_grants, id = "g-2", request_id = "hop-0", grantee = "bunny", client_id = "beta",
         granted_by = "mouse", granted_at = format(ahora - 3600), revoked = FALSE),
    fila(.schema_hop_grants, id = "g-0", request_id = "hop-0", grantee = "bunny", client_id = "gamma",
         granted_by = "mouse", granted_at = format(ahora - 7200), expires_at = format(ahora + 3600), revoked = TRUE)))
  poner("acme/contacts.rds", tabla(
    fila(.schema_contacts, id = "c-1", name = "María García", email = "maria@acme.mx", role = "CFO",
         receives_limit_alerts = TRUE, receives_audit_summaries = FALSE, active = TRUE, created_at = "2026-10-01"),
    fila(.schema_contacts, id = "c-2", name = "Pedro López", email = "pedro@acme.mx", role = "",
         receives_limit_alerts = FALSE, receives_audit_summaries = FALSE, active = TRUE, created_at = "2026-10-02"),
    fila(.schema_contacts, id = "c-0", name = "Ex Contacto", email = "ex@acme.mx", role = "TI",
         receives_limit_alerts = TRUE, receives_audit_summaries = FALSE, active = FALSE, created_at = "2026-09-01")))
  poner("acme/app_audit.rds", data.frame(ts = format(ahora - 1800), stringsAsFactors = FALSE))
}

# ── The viewer ──────────────────────────────────────────────────────────────
quien <- switch(rol,
  principal = list(user = "mouse",   tier = "principal", is_staff = TRUE,  home = "hd-admin"),
  hopdesk   = list(user = "bunny",   tier = "hopdesk",   is_staff = TRUE,  home = "hd-admin"),
  dev       = list(user = "devacme", tier = "dev",       is_staff = FALSE, home = "acme"),
  cliente   = list(user = "ana",     tier = "admin",     is_staff = FALSE, home = "acme"))

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(),
  tags$div(class = "small text-muted p-1", sprintf("Fixture T28 · rol: %s (%s) · S3 en memoria", rol, quien$user)),
  tiersUI("tiers")
)

server <- function(input, output, session) {
  info <- reactive(list(user = quien$user, tier = quien$tier, is_staff = quien$is_staff,
                        client_id = quien$home))
  shared <- list(
    current_user        = function() quien$user,
    current_user_info   = info,
    usuario             = function() {
      u <- tryCatch(auth_load_usuarios(client_id = quien$home), error = function(e) NULL)
      if (is.null(u)) return(NULL)
      fila_u <- u[u$username == quien$user, , drop = FALSE]
      # As usuario_sesion() builds it (app.R): permisos already resolved.
      list(username = quien$user, tier = quien$tier, client_id = quien$home,
           permisos = permisos_resueltos(quien$tier, fila_u$permisos[1] %||% "{}"))
    },
    effective_client_id = function() "acme",
    home_client_id      = function() quien$home,
    jump_client_id      = function() if (quien$is_staff) "acme" else NULL,
    grupos_db           = reactiveVal(data.frame(client_id = "acme", name = "Acme Logística")),
    hop_grants_db       = reactiveVal(NULL),
    permisos_version    = reactiveVal(0L)
  )
  tiersServer("tiers", shared)
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
