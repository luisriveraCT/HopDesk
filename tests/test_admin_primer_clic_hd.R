# =============================================================================
# tests/test_admin_primer_clic_hd.R
# T28 (integrator's review of 2b): the FIRST click on ℹ (Revisar), ↻
# (Reintentar) and "Reintentar todo" must reach its handler. The observers
# use ignoreInit = TRUE, and in Shiny 1.11 a reactive that req()-fails at
# start-up leaves them uninitialized, so the first real click was dropped.
#
# Not a copy: the observers and the tables' wiring are taken from
# tiersServer's own source (R/tiers_module.R) and run on the real table
# servers, with what they reach outside stubbed and recorded. One click each,
# from start-up.
#
# Run from project root:  Rscript tests/test_admin_primer_clic_hd.R
# =============================================================================
cat("=== test_admin_primer_clic_hd ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
for (archivo in c("R/email_service.R", "R/tiers_module.R"))
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% c(".schema_email_queue", "read_email_queue", "tiersServer",
                                    ".solicitudes_filas", "tiers_solicitudes_ui", "tiers_solicitudes_server",
                                    ".cola_filas", "tiers_cola_ui", "tiers_cola_server",
                                    ".invitaciones_filas", "tiers_invitaciones_ui", "tiers_invitaciones_server"))
      eval(e, envir = globalenv())

# ── The wiring, from tiersServer's own body ─────────────────────────────────
# Every `x <- tiers_*_server(...)` and every observeEvent() on solicitudes_t /
# cola_t / invitaciones_t, as written there.
TABLAS <- c("tiers_solicitudes_server", "tiers_cola_server", "tiers_invitaciones_server")
cableado <- list()
buscar <- function(x) {
  if (!is.call(x)) return(invisible())
  txt <- paste(deparse(x[1:min(2L, length(x))], width.cutoff = 500L), collapse = " ")
  es_tabla <- identical(x[[1]], as.name("<-")) && is.call(x[[3]]) &&
    as.character(x[[3]][[1]])[1] %in% TABLAS
  es_obs <- identical(x[[1]], as.name("observeEvent")) &&
    grepl("^observeEvent\\((solicitudes_t|cola_t|invitaciones_t)\\$", txt)
  if (es_tabla || es_obs) { cableado[[length(cableado) + 1L]] <<- x; return(invisible()) }
  xs <- as.list(x)   # an empty argument (x[i, ]) can't sit in a variable: index it
  for (k in seq_along(xs)) if (is.call(xs[[k]])) buscar(xs[[k]])
}
buscar(body(tiersServer))
ok("tiersServer crea las tres tablas y conecta sus 4 observers (Revisar, Reintentar, Reintentar todo, Revocar)",
   length(cableado) == 7L)

REQS <- data.frame(id = "req-1", requested_by = "bunny", client_id_proposed = "delta", display_name = "Delta",
                   contact_email = NA_character_, max_users = 8L, notes = NA_character_, status = "pending",
                   requested_at = "2026-11-07 10:15:00", reviewed_by = NA_character_, reviewed_at = NA_character_,
                   rejection_reason = NA_character_, stringsAsFactors = FALSE)
COLA <- data.frame(id = "q-1", to = "nuevo@acme.mx", subject = "Invitación", html = "", queued_at = "2026-11-06",
                   last_tried = NA_character_, attempts = 1L, last_error = NA_character_, status = "pending",
                   stringsAsFactors = FALSE)

inv <- function(token, cid)
  data.frame(token = token, email = paste0(token, "@x.mx"), display_name = token, client_id = cid, tier = "finance",
             invited_by = "x", created_at = "2026-11-06", expires_at = "2026-11-13", accepted_at = NA_character_,
             status = "pending", stringsAsFactors = FALSE)
INVS <- rbind(inv("t-a1", "acme"), inv("t-b1", "beta"))
estado_inv <- function(token) { i <- get("hd-admin/pending_invites.rds", envir = .mem); i$status[i$token == token] }

# rol: "principal" (staff) or "dev_acme" (a client's Dev: may invite and
# revoke, only its own client's).
correr <- function(clic, rol = "principal") {
  s3_memoria_reset()
  assign("hd-admin/client_requests.rds", REQS, envir = .mem)
  assign("hd-admin/email_queue.rds", COLA, envir = .mem)
  assign("hd-admin/pending_invites.rds", INVS, envir = .mem)
  hechos <- new.env(); hechos$lista <- character(0)
  anota  <- function(x) hechos$lista <- c(hechos$lista, x)
  error <- tryCatch(suppressMessages(testServer(function(input, output, session) {
    # What the observers reach in tiersServer, stubbed and recorded.
    principal <- identical(rol, "principal")
    is_principal <- reactive(principal); is_hopdesk <- reactive(FALSE); is_dev <- reactive(!principal)
    viewer_is_staff <- reactive(principal); current_client_id <- reactive(if (principal) "hd-admin" else "acme")
    requests_refresh <- reactiveVal(0L); invites_refresh <- reactiveVal(0L); activity_refresh <- reactiveVal(0L)
    shared <- list(current_user = function() "mouse", home_client_id = function() "hd-admin")
    read_client_requests <- function() REQS
    showModal <- function(...) anota("modal")
    showNotification <- function(ui, ...) { anota(paste("aviso:", ui)); invisible("id") }
    log_action <- function(...) invisible(NULL)
    retry_queued_email <- function(queue_id) { anota(paste("reintentar", queue_id)); TRUE }
    retry_all_queued_emails <- function(...) { anota("reintentar_todo"); list(sent = 1L, failed = 0L) }
    for (x in cableado) eval(x)
    session$flushReact()
    clic(session)
  }, {})), error = function(e) conditionMessage(e))
  list(error = error, hechos = hechos$lista)
}
un_clic <- function(id, accion, ids) function(session) {
  do.call(session$setInputs, stats::setNames(list(list(accion = accion, ids = ids, nonce = 1)), paste0(id, "-tabla-accion")))
  session$flushReact()
}

r <- correr(un_clic("solic", "detalle", "req-1"))
ok("un solo clic en ℹ (Revisar) desde el arranque abre el modal de revisión",
   is.null(r$error) && identical(r$hechos, "modal"))

r <- correr(un_clic("cola", "reintentar", "q-1"))
ok("un solo clic en ↻ (Reintentar) desde el arranque reintenta ese correo",
   is.null(r$error) && identical(r$hechos[!startsWith(r$hechos, "aviso:")], "reintentar q-1"))

r <- correr(un_clic("cola", "reintentar_todo", list("q-1")))
ok("un solo clic en “Reintentar todo” desde el arranque reintenta la cola",
   is.null(r$error) && identical(r$hechos[!startsWith(r$hechos, "aviso:")], "reintentar_todo"))

r <- correr(un_clic("invit", "revocar", "t-a1"), rol = "dev_acme")
ok("un solo clic en ⊘ (Revocar, ya confirmado) desde el arranque revoca la invitación de su empresa",
   is.null(r$error) && identical(estado_inv("t-a1"), "revoked") && identical(estado_inv("t-b1"), "pending"))

r <- correr(un_clic("invit", "revocar", "t-b1"), rol = "dev_acme")
ok("el Dev de acme no puede revocar una invitación de beta aunque mande su token",
   is.null(r$error) && identical(estado_inv("t-b1"), "pending") &&
     any(grepl("no pertenece a tu empresa", r$hechos, fixed = TRUE)))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
