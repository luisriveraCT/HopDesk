# =============================================================================
# tests/test_admin_invitar_form_hd.R
# T28 (integrator's review of 55c7c36): since the invitations are a table of
# the engine, the Invitaciones form is no longer rebuilt by invites_refresh.
# What that rebuild used to do, it now does on its own:
#   1. after "Enviar" creates the invite, the email and the name are emptied
#      (a second click sent the same person another invitation); a failed
#      create keeps them, to fix and send again;
#   2. ↻ checks the delivery mode again (email_service_status());
#   3. (T58 replaced it) there is no "Cliente destino" any more: the form
#      shows "Empresa: <group name> <logo>" of the client being viewed, and
#      Enviar saves the invitation in that same client -- what is seen is
#      where it is saved, whatever the browser sends; staff at home
#      (hd-admin) invites nobody (button off, Enviar refused).
# The handler, the output and the observer are taken from tiersServer's own
# source and run with what they reach outside stubbed and recorded.
#
# Run from project root:  Rscript tests/test_admin_invitar_form_hd.R
# =============================================================================
cat("=== test_admin_invitar_form_hd ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("R/i18n.R")
source("R/tier_registry.R")
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("tiersServer", ".inv_destino", ".inv_destino_ui"))
    eval(e, envir = globalenv())
for (e in parse("R/empresas_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("logo_grupo_img")))
    eval(e, envir = globalenv())
.HD_ADMIN_CID <- "hd-admin"
`%||%` <- function(a, b) if (is.null(a)) b else a

# From tiersServer's body: observeEvent(input$btn_send_invite, ...),
# output$inv_estado_correo <- renderUI(...) and observeEvent(invites_refresh(), ...).
piezas <- list()
buscar <- function(x) {
  if (!is.call(x)) return(invisible())
  txt <- paste(deparse(x[1:min(2L, length(x))], width.cutoff = 500L), collapse = " ")
  if (grepl("^observeEvent\\((input\\$btn_send_invite|invites_refresh\\(\\))", txt) ||
      (identical(x[[1]], as.name("<-")) &&
         deparse(x[[2]]) %in% c("output$inv_estado_correo", "output$invites_section", "inv_destino"))) {
    piezas[[length(piezas) + 1L]] <<- x; return(invisible())
  }
  xs <- as.list(x)
  for (k in seq_along(xs)) if (is.call(xs[[k]])) buscar(xs[[k]])
}
buscar(body(tiersServer))
ok("tiersServer tiene el handler de Enviar, el aviso del modo de envío, el formulario y su destino (inv_destino)",
   length(piezas) == 4L)
ok("T58: ya no hay un observer de ↻ para la lista de clientes (no hay “Cliente destino”)",
   !any(vapply(piezas, function(x) grepl("^observeEvent\\(invites_refresh", deparse(x)[1]), logical(1))))

REG <- function(...) data.frame(client_id = c("acme", ...), display_name = c("Acme", toupper(c(...))),
                                status = "active", stringsAsFactors = FALSE)
# cid: the client being viewed; cfg: each client's group_config (T58).
correr <- function(pasos, crea = TRUE, cid = "acme", cfg = list(), registro = REG()) {
  rec <- new.env(); rec$texto <- list(); rec$select <- list(); rec$estado <- 0L; rec$invitaciones <- 0L
  rec$registro <- registro; rec$destinos <- character(); rec$cfg_leidos <- character()
  error <- tryCatch(suppressMessages(testServer(function(input, output, session) {
    is_principal <- reactive(TRUE); is_hopdesk <- reactive(FALSE); is_dev <- reactive(FALSE)
    viewer_is_staff <- reactive(TRUE); current_client_id <- reactive(cid); current_tier <- reactive("principal")
    load_group_config <- function(client_id = NULL) { rec$cfg_leidos <- c(rec$cfg_leidos, client_id); cfg[[client_id]] }
    tiers_invitaciones_ui <- function(...) div(); tiers_cola_ui <- function(...) div(); ns <- session$ns
    invites_refresh <- reactiveVal(0L); activity_refresh <- reactiveVal(0L)
    shared <- list(current_user = function() "mouse", home_client_id = function() "hd-admin",
                   current_user_info = function() list(client_id = "hd-admin"))
    updateTextInput <- function(session, inputId, value = NULL, ...) rec$texto[[length(rec$texto) + 1L]] <- c(inputId, value)
    updateSelectInput <- function(session, inputId, choices = NULL, selected = NULL, ...)
      rec$select[[length(rec$select) + 1L]] <- list(choices = choices, selected = selected)
    create_invite <- function(..., client_id) {
      rec$invitaciones <- rec$invitaciones + 1L; rec$destinos <- c(rec$destinos, client_id)
      if (crea) "tok-1" else stop("S3 caído")
    }
    invite_activation_url <- function(token, session) paste0("https://x/?invite=", token)
    read_client_registry <- function() rec$registro
    .schema_client_registry <- function() rec$registro[0, ]
    auth_load_usuarios <- function(...) data.frame(username = "mouse", display_name = "Mouse", deleted = FALSE)
    email_invite_html <- function(...) "<p>x</p>"
    send_email <- function(...) TRUE
    email_error_of <- function(...) ""
    email_service_status <- function() { rec$estado <- rec$estado + 1L; list(mode = "live", label = "ok", detail = "") }
    showModal <- function(...) invisible(NULL); showNotification <- function(...) invisible("id")
    log_action <- function(...) invisible(NULL)
    for (x in piezas) eval(x)
    session$flushReact()
    pasos(session, output, invites_refresh, rec)
  }, {})), error = function(e) conditionMessage(e))
  list(error = error, rec = rec)
}
enviar <- function(session) {
  session$setInputs(inv_email = "ana@acme.mx", inv_display_name = "Ana", inv_client_id = "acme", inv_tier = "finance")
  session$setInputs(btn_send_invite = 1)
}
vaciados <- function(rec) {
  t <- rec$texto
  any(vapply(t, identical, logical(1), c("inv_email", ""))) &&
    any(vapply(t, identical, logical(1), c("inv_display_name", "")))
}

r <- correr(function(session, output, refresh, rec) enviar(session))
ok("Enviar con la invitación creada: se vacían el correo y el nombre",
   is.null(r$error) && identical(r$rec$invitaciones, 1L) && vaciados(r$rec))

r <- correr(function(session, output, refresh, rec) enviar(session), crea = FALSE)
ok("Enviar con la creación fallida: lo escrito se queda (para corregir y volver a enviar)",
   is.null(r$error) && identical(r$rec$invitaciones, 1L) && !vaciados(r$rec))

r <- correr(function(session, output, refresh, rec) {
  invisible(output$inv_estado_correo); antes <- rec$estado
  refresh(refresh() + 1L); session$flushReact(); invisible(output$inv_estado_correo)
  rec$antes <- antes
})
ok("↻ vuelve a revisar el modo de envío del correo", is.null(r$error) && r$rec$estado > r$rec$antes)

# ── T58: "Empresa: <name> <logo>", and it is where the invitation is saved ──
PNG <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
CFG <- list(acme = list(group_name = "Grupo Acme & Cía", logo_raw = PNG, logo_ext = "png"),
            beta = list(group_name = "", logo_raw = NULL, logo_ext = "png"))
forma <- function(session, output, refresh, rec) rec$html <- as.character(output$invites_section$html)

r <- correr(forma, cid = "acme", cfg = CFG)
ok("T58: el formulario ya no tiene “Cliente destino” (ni select ni campo)",
   is.null(r$error) && !grepl("inv_client_id|Cliente destino", r$rec$html))
ok("T58: muestra “Empresa” con el nombre del grupo (escapado) y su logo",
   grepl(tr("niveles.invitaciones.empresa"), r$rec$html, fixed = TRUE) &&
     grepl("Grupo Acme &amp; Cía", r$rec$html, fixed = TRUE) &&
     grepl('<img src="data:image/png;base64,', r$rec$html, fixed = TRUE))
r <- correr(forma, cid = "beta", cfg = CFG, registro = REG("beta"))
ok("T58: sin nombre ni logo de grupo, el nombre del registro y ningún <img>",
   is.null(r$error) && grepl('inv-destino">\\s*<span class="fw-semibold">BETA</span>\\s*</div>', r$rec$html) &&
     !grepl("<img", r$rec$html, fixed = TRUE))
r <- correr(forma, cid = "gamma", cfg = list())
ok("T58: sin group_config, sin error: el id del cliente", is.null(r$error) && grepl(">gamma<", r$rec$html, fixed = TRUE))

# What is seen is where it is saved (Coordinador): the form reads the group
# of the client it names, and Enviar saves in that client -- even when the
# browser forges the old inv_client_id.
r <- correr(function(session, output, refresh, rec) {
  forma(session, output, refresh, rec)
  session$setInputs(inv_email = "ana@acme.mx", inv_display_name = "Ana", inv_client_id = "beta", inv_tier = "finance")
  session$setInputs(btn_send_invite = 1)
}, cid = "acme", cfg = CFG)
ok("T58: lo que se ve es donde se guarda: el formulario dice Grupo Acme y la invitación cae en acme (no en el beta forjado)",
   is.null(r$error) && grepl("Grupo Acme", r$rec$html, fixed = TRUE) && all(r$rec$cfg_leidos == "acme") &&
     identical(r$rec$destinos, "acme"))

# Staff at home (hd-admin): nobody is invited there (Coordinador).
r <- correr(function(session, output, refresh, rec) {
  forma(session, output, refresh, rec)
  session$setInputs(inv_email = "x@hd.mx", inv_display_name = "X", inv_client_id = "acme", inv_tier = "finance")
  session$setInputs(btn_send_invite = 1)
}, cid = "hd-admin")
ok("T58: staff en hd-admin: el botón Enviar sale deshabilitado, con el aviso",
   is.null(r$error) && grepl('btn_send_invite"[^>]*disabled|disabled[^>]*btn_send_invite"', r$rec$html) &&
     grepl(tr("niveles.invitaciones.entra_a_cliente"), r$rec$html, fixed = TRUE))
ok("T58: staff en hd-admin: un clic forjado no crea la invitación", identical(r$rec$invitaciones, 0L))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
