# =============================================================================
# tests/test_admin_solicitudes_hd.R
# Clientes › Solicitudes pendientes on the table engine (T28, class D):
#   1. the rows: the pending requests only, id = the request's id;
#   2. ℹ / double click names the request by its id, also when another
#      session added a request ahead of it after the table was drawn;
#   3. only Principal: without it no rows, and a "detalle" sent by hand names
#      nothing (integrator's condition c, T28);
#   4. start-up: no file is 0 rows; an unreadable one is the table's notice
#      (never "no requests", P7) and the session goes on;
#   5. the review modal is opened by the table, and still checks Principal.
#
# Run from project root:  Rscript tests/test_admin_solicitudes_hd.R
# =============================================================================
cat("=== test_admin_solicitudes_hd ===\n\n")
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
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".solicitudes_filas", "tiers_solicitudes_ui", "tiers_solicitudes_server"))
    eval(e, envir = globalenv())

KEY <- "hd-admin/client_requests.rds"
solicitud <- function(id, propuesto, status = "pending", cuando = "2026-11-07 10:15:00")
  data.frame(id = id, requested_by = "bunny", client_id_proposed = propuesto, display_name = toupper(propuesto),
             contact_email = NA_character_, max_users = 8L, notes = NA_character_, status = status,
             requested_at = cuando, reviewed_by = NA_character_, reviewed_at = NA_character_,
             rejection_reason = NA_character_, stringsAsFactors = FALSE)
REQS <- rbind(solicitud("req-1", "delta"), solicitud("req-2", "epsilon"), solicitud("req-0", "beta", "approved"))
sembrar <- function(r = REQS) { s3_memoria_reset(); if (!is.null(r)) assign(KEY, r, envir = .mem) }
COLS <- c("id", "propuesto", "nombre", "solicitado_por", "fecha", "limite")

# ── 1. The rows ──────────────────────────────────────────────────────────────
f <- .solicitudes_filas(REQS)
ok("solo las pendientes; el id es el de la solicitud", identical(f$id, c("req-1", "req-2")))
ok("ID propuesto, nombre, quién, fecha (hora de México) y límite",
   identical(f$propuesto, c("delta", "epsilon")) && identical(f$limite, c(8L, 8L)) &&
     identical(format(f$fecha[1], "%Y-%m-%d %H:%M", tz = "America/Mexico_City"), "2026-11-07 10:15"))
for (caso in list(list("NULL", NULL), list("vacía", REQS[0, ]), list("sin pendientes", REQS[3, ])))
  ok(sprintf("solicitudes %s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .solicitudes_filas(caso[[2]]); is.data.frame(v) && !nrow(v) && identical(names(v), COLS) })

# ── 2-4. On the module ───────────────────────────────────────────────────────
correr <- function(principal, pasos) {
  avisos <- character(0); caja <- new.env(); caja$filas <- "sin leer"; caja$revisar <- list()
  error <- tryCatch(withCallingHandlers(
    testServer(tiers_solicitudes_server, args = list(permitido = reactive(principal), refresh = reactiveVal(0L)), {
      observe({ r <- tryCatch(session$returned$revisar(), error = function(e) NULL)
                if (!is.null(r)) caja$revisar <- c(caja$revisar, list(r)) })
      session$flushReact()
      caja$filas <- tryCatch(session$returned$datos(), error = function(e) paste("error:", conditionMessage(e)))
      pasos(session)
    }),
    message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") }),
    error = function(e) conditionMessage(e))
  list(error = error, filas = caja$filas, revisar = vapply(caja$revisar, `[[`, "", "id"),
       fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}
detalle <- function(id, nonce = 1) function(session) {
  session$setInputs(`tabla-accion` = list(accion = "detalle", ids = id, nonce = nonce))
  session$flushReact()
}

sembrar()
r <- correr(TRUE, function(session) {
  # Another session's request arrives ahead of the others after drawing.
  assign(KEY, rbind(solicitud("req-9", "zeta"), get(KEY, envir = .mem)), envir = .mem)
  detalle("req-2")(session)
})
ok("Principal ve las 2 pendientes", is.null(r$error) && identical(r$filas$id, c("req-1", "req-2")))
ok("ℹ en req-2 abre req-2, aunque otra sesión agregó una antes", identical(r$revisar, "req-2"))

sembrar()
r <- correr(FALSE, detalle("req-1"))
ok("sin ser Principal: la tabla no recibe filas", is.null(r$error) && is.null(r$filas))
ok("sin ser Principal: un 'detalle' mandado a mano no nombra ninguna solicitud", !length(r$revisar))

sembrar(NULL)
r <- correr(TRUE, function(session) session$flushReact())
ok("arranque sin archivo de solicitudes: 0 filas y ninguna falla del motor",
   is.null(r$error) && is.data.frame(r$filas) && !nrow(r$filas) && !length(r$fallas))

sembrar()
.mem_falla <- KEY
r <- correr(TRUE, function(session) session$flushReact())
ok("lectura ilegible: la sesión sigue y la tabla avisa la falla (no 'sin solicitudes')",
   is.null(r$error) && length(r$fallas) > 0L)

# ── 5. The review modal in tiersServer ───────────────────────────────────────
tm <- paste(readLines("R/tiers_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
bloque <- regmatches(tm, regexpr("observeEvent\\(solicitudes_t\\$revisar\\(\\), \\{[\\s\\S]{0,400}", tm, perl = TRUE))
ok("el modal de revisión se abre desde la tabla y sigue exigiendo Principal",
   length(bloque) == 1L && grepl("req(is_principal())", bloque, fixed = TRUE) &&
     grepl("rid <- solicitudes_t$revisar()$id", bloque, fixed = TRUE))
ok("ya no hay botón que mande review_request_id", !grepl("review_request_id", tm, fixed = TRUE))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
