# =============================================================================
# tests/test_t108_avisos_al_responder.R
# T108 (1): avisos_actividad(..., al_responder = session) publishes after the
# user has the answer -- on the session's next flush -- not inside the save.
#   A  nothing is published at the call; exactly one aviso on the flush
#   B  the arguments are the call's (changing them afterwards changes nothing)
#   C  a failure while publishing never throws and is logged ("[avisos]")
#   D  an argument that fails at the call: logged, nothing deferred, no throw
#   E  no session: published right away, as before
#   F  a real Shiny session (testServer): the aviso goes out on its flush
# Run from project root:  Rscript tests/test_t108_avisos_al_responder.R
# =============================================================================
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
source("R/i18n.R")
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
source("R/avisos.R")

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
# What avisos_actividad() calls to publish, stubbed: the matrix lets every
# action through; avisos_agregar() records (or fails on demand).
G <- new.env(); G$publicados <- list(); G$falla <- FALSE
alertas_preparar <- function(client_id, accion, usuario, params) params
avisos_agregar <- function(client_id, aviso, por = "sistema") {
  if (G$falla) stop("S3 caído")
  G$publicados[[length(G$publicados) + 1L]] <- list(cid = client_id, titulo = aviso$titulo, cuerpo = aviso$cuerpo, por = por)
  invisible("id")
}
# A session as Shiny's: onFlushed(fn, once = TRUE) keeps fn for the flush.
sesion <- function() {
  e <- new.env(); e$pendientes <- list()
  e$onFlushed <- function(fn, once = TRUE) { e$pendientes[[length(e$pendientes) + 1L]] <- fn; invisible(NULL) }
  e$flush <- function() { p <- e$pendientes; e$pendientes <- list(); for (f in p) f() }
  e
}
mensajes <- function(expr) { m <- character(0)
  withCallingHandlers(expr, message = function(c) { m <<- c(m, conditionMessage(c)); invokeRestart("muffleMessage") }); m }

# A, B
s <- sesion(); cid <- "acme"; cuerpo <- "Renta oficina"
r <- avisos_actividad(cid, "crear_pasivo", "beto", list(cuerpo = cuerpo), al_responder = s)
cid <- "otro"; cuerpo <- "cambiado"
ok(length(G$publicados) == 0L, "A nada se publica en la llamada")
ok(length(s$pendientes) == 1L, "A queda uno pendiente para el flush")
s$flush()
ok(length(G$publicados) == 1L, "A en el flush se publica exactamente uno")
ok(identical(G$publicados[[1]]$cid, "acme") && identical(G$publicados[[1]]$cuerpo, "Renta oficina") &&
     identical(G$publicados[[1]]$por, "beto"),
   "B con los argumentos de la llamada, no con los de después")
s$flush()
ok(length(G$publicados) == 1L, "A un segundo flush no lo repite")

# C
G$falla <- TRUE; G$publicados <- list(); s <- sesion()
avisos_actividad("acme", "crear_pasivo", "beto", list(cuerpo = "x"), al_responder = s)
m <- mensajes(r <- tryCatch({ s$flush(); "sin error" }, error = function(e) conditionMessage(e)))
ok(identical(r, "sin error"), "C un fallo al publicar no lanza error")
ok(any(grepl("[avisos]", m, fixed = TRUE) & grepl("crear_pasivo", m, fixed = TRUE) & grepl("S3 caído", m, fixed = TRUE)),
   "C y queda en el log con [avisos], la acción y la causa")
G$falla <- FALSE

# D
s <- sesion(); G$publicados <- list()
roto <- function() stop("sin cliente")
m <- mensajes(r <- tryCatch({ avisos_actividad(roto(), "crear_pasivo", "beto", list(), al_responder = s); "sin error" },
                            error = function(e) conditionMessage(e)))
ok(identical(r, "sin error"), "D un argumento que truena en la llamada no lanza error")
ok(any(grepl("[avisos]", m, fixed = TRUE) & grepl("sin cliente", m, fixed = TRUE)), "D y queda en el log")
ok(length(s$pendientes) == 0L && length(G$publicados) == 0L, "D y no se difiere nada")

# E
G$publicados <- list()
avisos_actividad("acme", "crear_pasivo", "beto", list(cuerpo = "y"))
ok(length(G$publicados) == 1L, "E sin sesión se publica de inmediato, como antes")

# F
G$publicados <- list(); antes <- NA
testServer(function(input, output, session) {
  avisos_actividad("acme", "crear_pasivo", "beto", list(cuerpo = "z"), al_responder = session)
  antes <<- length(G$publicados)
}, { session$flushReact() })
ok(identical(antes, 0L), "F con una sesión de Shiny, nada dentro del servidor")
ok(length(G$publicados) == 1L, "F y uno al hacer flush")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
