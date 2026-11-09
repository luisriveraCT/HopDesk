# =============================================================================
# tests/test_admin_invitar_form_hd.R
# T28 (integrator's review of 55c7c36): since the invitations are a table of
# the engine, the Invitaciones form is no longer rebuilt by invites_refresh.
# What that rebuild used to do, it now does on its own:
#   1. after "Enviar" creates the invite, the email and the name are emptied
#      (a second click sent the same person another invitation); a failed
#      create keeps them, to fix and send again;
#   2. ↻ checks the delivery mode again (email_service_status());
#   3. ↻ brings staff's "Cliente destino" the clients approved since, keeping
#      the one chosen.
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
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("tiersServer")))
    eval(e, envir = globalenv())

# From tiersServer's body: observeEvent(input$btn_send_invite, ...),
# output$inv_estado_correo <- renderUI(...) and observeEvent(invites_refresh(), ...).
piezas <- list()
buscar <- function(x) {
  if (!is.call(x)) return(invisible())
  txt <- paste(deparse(x[1:min(2L, length(x))], width.cutoff = 500L), collapse = " ")
  if (grepl("^observeEvent\\((input\\$btn_send_invite|invites_refresh\\(\\))", txt) ||
      grepl("^`?<-`?\\(output\\$inv_estado_correo", txt) ||
      (identical(x[[1]], as.name("<-")) && identical(deparse(x[[2]]), "output$inv_estado_correo"))) {
    piezas[[length(piezas) + 1L]] <<- x; return(invisible())
  }
  xs <- as.list(x)
  for (k in seq_along(xs)) if (is.call(xs[[k]])) buscar(xs[[k]])
}
buscar(body(tiersServer))
ok("tiersServer tiene el handler de Enviar, el aviso del modo de envío y la lista de clientes", length(piezas) == 3L)

REG <- function(...) data.frame(client_id = c("acme", ...), display_name = c("Acme", toupper(c(...))),
                                status = "active", stringsAsFactors = FALSE)
correr <- function(pasos, crea = TRUE) {
  rec <- new.env(); rec$texto <- list(); rec$select <- list(); rec$estado <- 0L; rec$invitaciones <- 0L
  rec$registro <- REG()
  error <- tryCatch(suppressMessages(testServer(function(input, output, session) {
    is_principal <- reactive(TRUE); is_hopdesk <- reactive(FALSE); is_dev <- reactive(FALSE)
    viewer_is_staff <- reactive(TRUE); current_client_id <- reactive("acme"); current_tier <- reactive("principal")
    invites_refresh <- reactiveVal(0L); activity_refresh <- reactiveVal(0L)
    shared <- list(current_user = function() "mouse", home_client_id = function() "hd-admin",
                   current_user_info = function() list(client_id = "hd-admin"))
    updateTextInput <- function(session, inputId, value = NULL, ...) rec$texto[[length(rec$texto) + 1L]] <- c(inputId, value)
    updateSelectInput <- function(session, inputId, choices = NULL, selected = NULL, ...)
      rec$select[[length(rec$select) + 1L]] <- list(choices = choices, selected = selected)
    create_invite <- function(...) { rec$invitaciones <- rec$invitaciones + 1L; if (crea) "tok-1" else stop("S3 caído") }
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

r <- correr(function(session, output, refresh, rec) {
  session$setInputs(inv_client_id = "acme")
  rec$registro <- REG("delta")                 # Principal approved delta meanwhile
  refresh(refresh() + 1L); session$flushReact()
})
ok("↻ trae a “Cliente destino” los clientes aprobados después, sin cambiar el elegido",
   is.null(r$error) && length(r$rec$select) == 1L &&
     identical(unname(r$rec$select[[1]]$choices), c("acme", "delta")) &&
     identical(r$rec$select[[1]]$selected, "acme"))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
