# =============================================================================
# tests/test_admin_cola_hd.R
# Invitaciones › Cola de correos on the table engine (T28, class C):
#   1. the rows: pending and failed emails only, id = the queue's id;
#   2. ↻ names the email by its id (also when another session queued one
#      ahead of it after drawing); "Reintentar todo" in the bar;
#   3. only HopDesk staff: without it no rows, and a ↻ or "Reintentar todo"
#      sent by hand names nothing (the queue is every client's mail);
#   4. start-up: no queue file is 0 rows; an unreadable one is the table's
#      notice (never "empty queue", P7) and the session goes on;
#   5. the two handlers are reached from the table and keep their staff gate.
#
# Run from project root:  Rscript tests/test_admin_cola_hd.R
# =============================================================================
cat("=== test_admin_cola_hd ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
for (archivo in c("R/email_service.R", "R/tiers_module.R"))
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% c(".schema_email_queue", "read_email_queue",
                                    ".cola_filas", "tiers_cola_ui", "tiers_cola_server"))
      eval(e, envir = globalenv())

KEY <- "hd-admin/email_queue.rds"
correo <- function(id, para, status, intentos = 1L)
  data.frame(id = id, to = para, subject = "Te invitaron a HopDesk", html = "<p>x</p>", queued_at = "2026-11-06 10:00:00",
             last_tried = NA_character_, attempts = intentos, last_error = NA_character_, status = status,
             stringsAsFactors = FALSE)
COLA <- rbind(correo("q-1", "nuevo@acme.mx", "pending"), correo("q-2", "x@beta.mx", "failed", 5L),
              correo("q-3", "ok@acme.mx", "sent"))
sembrar <- function(q = COLA) { s3_memoria_reset(); if (!is.null(q)) assign(KEY, q, envir = .mem) }
COLS <- c("id", "para", "asunto", "estado", "intentos")

# ── 1. The rows ──────────────────────────────────────────────────────────────
f <- .cola_filas(COLA)
ok("solo pendientes y fallidos; el id es el de la cola", identical(f$id, c("q-1", "q-2")))
ok("para, asunto, estado (crudo, la insignia lo traduce) e intentos",
   identical(f$para, c("nuevo@acme.mx", "x@beta.mx")) && identical(f$estado, c("pending", "failed")) &&
     identical(f$intentos, c(1L, 5L)))
for (caso in list(list("NULL", NULL), list("vacía", COLA[0, ]), list("todo enviado", COLA[3, ])))
  ok(sprintf("cola %s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .cola_filas(caso[[2]]); is.data.frame(v) && !nrow(v) && identical(names(v), COLS) })

# ── 2-4. On the module ───────────────────────────────────────────────────────
correr <- function(staff, pasos) {
  avisos <- character(0); caja <- new.env(); caja$filas <- "sin leer"; caja$uno <- list(); caja$todo <- 0L
  error <- tryCatch(withCallingHandlers(
    testServer(tiers_cola_server, args = list(permitido = reactive(staff), refresh = reactiveVal(0L)), {
      observe({ r <- tryCatch(session$returned$reintentar(), error = function(e) NULL)
                if (!is.null(r)) caja$uno <- c(caja$uno, list(r)) })
      observe({ r <- tryCatch(session$returned$reintentar_todo(), error = function(e) NULL)
                if (!is.null(r)) caja$todo <- caja$todo + 1L })
      session$flushReact()
      caja$filas <- tryCatch(session$returned$datos(), error = function(e) paste("error:", conditionMessage(e)))
      pasos(session)
    }),
    message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") }),
    error = function(e) conditionMessage(e))
  list(error = error, filas = caja$filas, uno = vapply(caja$uno, `[[`, "", "id"), todo = caja$todo,
       fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}
accion <- function(nombre, ids, nonce) function(session) {
  session$setInputs(`tabla-accion` = list(accion = nombre, ids = ids, nonce = nonce))
  session$flushReact()
}

sembrar()
r <- correr(TRUE, function(session) {
  assign(KEY, rbind(correo("q-9", "otro@acme.mx", "pending"), get(KEY, envir = .mem)), envir = .mem)
  accion("reintentar", "q-2", 1)(session)
  accion("reintentar_todo", list("q-1", "q-2"), 2)(session)
})
ok("staff ve los 2 correos por enviar", is.null(r$error) && identical(r$filas$id, c("q-1", "q-2")))
ok("↻ en q-2 reintenta q-2, aunque otra sesión encoló uno antes", identical(r$uno, "q-2"))
ok("“Reintentar todo” de la barra llega una vez", identical(r$todo, 1L))

sembrar()
r <- correr(FALSE, function(session) {
  accion("reintentar", "q-1", 1)(session)
  accion("reintentar_todo", list(), 2)(session)
})
ok("sin ser staff: la tabla no recibe filas", is.null(r$error) && is.null(r$filas))
ok("sin ser staff: ↻ y “Reintentar todo” mandados a mano no llegan", !length(r$uno) && identical(r$todo, 0L))

sembrar(NULL)
r <- correr(TRUE, function(session) session$flushReact())
ok("arranque sin cola: 0 filas y ninguna falla del motor",
   is.null(r$error) && is.data.frame(r$filas) && !nrow(r$filas) && !length(r$fallas))

sembrar()
.mem_falla <- KEY
r <- correr(TRUE, function(session) session$flushReact())
ok("cola ilegible: la sesión sigue y la tabla avisa la falla (no 'la cola está vacía')",
   is.null(r$error) && length(r$fallas) > 0L)

# ── 5. The handlers in tiersServer ───────────────────────────────────────────
tm <- paste(readLines("R/tiers_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
bloque <- function(ancla) regmatches(tm, regexpr(paste0(ancla, "[\\s\\S]{0,500}"), tm, perl = TRUE))
uno  <- bloque("observeEvent\\(cola_t\\$reintentar\\(\\), \\{")
todo <- bloque("observeEvent\\(cola_t\\$reintentar_todo\\(\\), \\{")
ok("↻ llega al handler de siempre, con su gate de staff y el id de la tabla",
   length(uno) == 1L && grepl("req(isTRUE(is_hopdesk()) || isTRUE(is_principal()))", uno, fixed = TRUE) &&
     grepl("queue_id <- cola_t$reintentar()$id", uno, fixed = TRUE) && grepl("retry_queued_email(queue_id)", uno, fixed = TRUE))
ok("“Reintentar todo” llega al handler de siempre, con su gate de staff",
   length(todo) == 1L && grepl("req(isTRUE(is_hopdesk()) || isTRUE(is_principal()))", todo, fixed = TRUE) &&
     grepl("retry_all_queued_emails()", todo, fixed = TRUE))
ok("ya no hay botones que manden email_retry_id ni btn_email_retry_all",
   !grepl("email_retry_id", tm, fixed = TRUE) && !grepl("btn_email_retry_all", tm, fixed = TRUE))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
