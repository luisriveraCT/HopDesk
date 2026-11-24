# =============================================================================
# tests/test_t72b_alarma.R
# T72 fase B, commit 2: the alarm of a write OUTSIDE the known debt
# (.t72_registrar in R/persistence.R). A write without an event whose call
# stack has no function of T72_DEUDA_EVENTO tells staff, in hd-admin's
# Panorama, once per client per hour. The integrador's conditions
# (requisiciones/bitacora.md, 2026-11-21, commit 2) are the checks:
#   1  outside the debt: one alarm to hd-admin, niveles_staff(), clave
#      t72:sin_evento:<cid>, ventana_min 60; a known site (a function of the
#      list in the stack) and a write with an event: none;
#   2  the alarm never breaks the save: avisos_agregar() that throws, or
#      that does not exist -- the write is done and logged;
#   3  no recursion: the aviso's own write logs nothing and never alarms; 3
#      writes in a row outside the debt give ONE alarm;
#   4  the brake: once per client per hour, in memory; two clients, two
#      alarms; after an hour, again;
#   5  the text names the client, the table and the origin (function names
#      only), never a value;
#   6  tiersServer's inline write (.tiers_guardar_inline) does not alarm.
# On the in-memory S3 (tests/helpers/s3_memoria.R).
# Run from project root:  Rscript tests/test_t72b_alarma.R
# Contraprueba: on 9a1711e (commit 1: no list, no alarm) 0 and 1 fail and it
# stops (no function of the list to call).
# =============================================================================
cat("=== test_t72b_alarma ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
quieto <- function(expr) suppressMessages(suppressWarnings(expr))
L <- new.env(); L$log <- list(); L$al <- list()
log_action <- function(user, module, action, description, target_id = NA_character_, s3_key = NA_character_,
                       metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
  L$log[[length(L$log) + 1L]] <- list(module = module, action = action, client_id = client_id)
  invisible(TRUE)
}
niveles_staff <- function() c("dev", "hopdesk")
tr <- function(clave, ...) { a <- list(...); paste(clave, paste(names(a), unlist(a), sep = "=", collapse = " ")) }
avisos_agregar <- function(client_id, aviso, por = "sistema") {
  L$al[[length(L$al) + 1L]] <- list(client_id = client_id, aviso = aviso); invisible("id")
}
limpiar <- function() { L$log <- list(); L$al <- list(); options(hopdesk.t72_alarmas = NULL); s3_memoria_reset() }
fila <- function(v = "VALOR-SECRETO-123") tibble(id = "r1", nombre = v)
escribe <- function(cid = "acme", key = "t72_tabla.rds") quieto(.s3_write(fila(), key, client_id = cid))
guardado <- function(cid = "acme", key = "t72_tabla.rds") exists(paste0(cid, "/", key), envir = .mem)
ok("0 T72_DEUDA_EVENTO is loaded with R/persistence.R", exists("T72_DEUDA_EVENTO") && length(T72_DEUDA_EVENTO) > 0L)

# ── 1. Outside the debt / known site / with an event ───────────────────────
limpiar(); escribe()
a <- if (length(L$al)) L$al[[1]] else NULL
ok("1 a write outside the debt: one alarm, to hd-admin, niveles_staff(), clave t72:sin_evento:acme, ventana 60",
   length(L$al) == 1L && identical(a$client_id, "hd-admin") && identical(a$aviso$niveles, niveles_staff()) &&
     identical(a$aviso$clave, "t72:sin_evento:acme") && identical(a$aviso$ventana_min, 60) && identical(a$aviso$tipo, "automatico"))
ok("1 ... and the write is done and logged as sin_evento", guardado() && length(L$log) == 1L && L$log[[1]]$action == "sin_evento")
conocida <- sub("^.*::", "", names(T72_DEUDA_EVENTO)[grepl("::save_moves$", names(T72_DEUDA_EVENTO))])   # save_moves: T72-G took save_notes out of the list
limpiar()
assign(conocida, function() escribe(), envir = globalenv())   # a function of the list, in the stack
eval(as.call(list(as.name(conocida))))   # called by its name, as the app does
ok(sprintf("1 a known site (%s() in the stack): logged as sin_evento, no alarm", conocida),
   length(conocida) == 1L && guardado() && length(L$log) == 1L && !length(L$al))
limpiar(); quieto(.s3_write(fila(), "t72_tabla.rds", client_id = "acme", evento = list(usuario = "ana", modulo = "m", accion = "a")))
ok("1 a write with an event: no alarm", guardado() && !length(L$al))

# ── 2. Never breaks the save ───────────────────────────────────────────────
bueno <- avisos_agregar
avisos_agregar <- function(...) stop("avisos caído")
limpiar(); r <- tryCatch({ escribe(); "ok" }, error = function(e) conditionMessage(e))
ok("2 avisos_agregar() that throws: the write is done and logged", identical(r, "ok") && guardado() && length(L$log) == 1L)
rm(avisos_agregar)
limpiar(); r <- tryCatch({ escribe(); "ok" }, error = function(e) conditionMessage(e))
ok("2 no avisos_agregar() (the emails job): the write is done and logged", identical(r, "ok") && guardado() && length(L$log) == 1L)
avisos_agregar <- bueno

# ── 3. No recursion; 3 writes, one alarm ───────────────────────────────────
avisos_agregar <- function(client_id, aviso, por = "sistema") {
  L$al[[length(L$al) + 1L]] <- list(client_id = client_id, aviso = aviso)
  .s3_write(tibble(id = "a1", titulo = "x"), "avisos.rds", client_id = client_id)   # the aviso's own write
  invisible("id")
}
limpiar(); escribe()
ok("3 the aviso's own write (under the reentry option): saved, logs nothing, no second alarm",
   guardado("hd-admin", "avisos.rds") && length(L$log) == 1L && length(L$al) == 1L)
avisos_agregar <- bueno
limpiar(); escribe(); escribe(); escribe()
ok("3 three writes in a row outside the debt: three sin_evento entries, ONE alarm", length(L$log) == 3L && length(L$al) == 1L)

# ── 4. The brake ───────────────────────────────────────────────────────────
limpiar(); escribe("acme"); escribe("beta"); escribe("acme")
ok("4 two clients: two alarms (one each)",
   length(L$al) == 2L && setequal(vapply(L$al, function(x) x$aviso$clave, ""), c("t72:sin_evento:acme", "t72:sin_evento:beta")))
f <- getOption("hopdesk.t72_alarmas"); f$acme <- Sys.time() - 61 * 60; options(hopdesk.t72_alarmas = f)
escribe("acme")
ok("4 after an hour, the same client alarms again", length(L$al) == 3L)
f <- getOption("hopdesk.t72_alarmas"); f$acme <- Sys.time() - 59 * 60; options(hopdesk.t72_alarmas = f)
escribe("acme")
ok("4 ... and not before", length(L$al) == 3L)

# ── 5. The text ────────────────────────────────────────────────────────────
desde_funcion <- function() escribe("acme", "t72_otra.rds")
limpiar(); desde_funcion()
txt <- if (length(L$al)) paste(L$al[[1]]$aviso$titulo, L$al[[1]]$aviso$cuerpo) else ""
ok("5 the text names the client, the table and the origin (desde_funcion)",
   grepl("cliente=acme", txt, fixed = TRUE) && grepl("tabla=t72_otra.rds", txt, fixed = TRUE) && grepl("desde_funcion", txt, fixed = TRUE))
ok("5 ... and never a value", nzchar(txt) && !grepl("VALOR-SECRETO", txt, fixed = TRUE))

# ── 6. tiersServer's inline write (integrador, 2026-11-21) ─────────────────
# The observer saving a tier's config wrote inline: tiersServer is not in the
# stack when an observer runs, so it alarmed every time (a known false
# alarm). It is now .tiers_guardar_inline(), local to the observer and in the
# list. The REAL definition, taken from R/tiers_module.R, called from an
# anonymous function like the observer's handler.
def <- NULL
andar <- function(e) {
  if (!is.call(e)) return(invisible())
  if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".tiers_guardar_inline"))) def <<- e[[3]]
  for (k in seq_along(e)[-1]) { a <- e[[k]]; if (!missing(a) && is.call(a)) andar(a) }
}
for (e in parse("R/tiers_module.R", keep.source = FALSE, encoding = "UTF-8")) andar(e)
ok("6 R/tiers_module.R defines .tiers_guardar_inline, and it is in T72_DEUDA_EVENTO",
   !is.null(def) && "R/tiers_module.R::.tiers_guardar_inline" %in% names(T72_DEUDA_EVENTO))
if (!exists("S3_KEYS")) S3_KEYS <- list()
if (is.null(S3_KEYS$tiers_config)) S3_KEYS$tiers_config <- "tiers_config.rds"
manejador <- function() {   # stands for the observer: tiersServer is not in the stack
  .tiers_guardar_inline <- eval(def)
  (function() .tiers_guardar_inline(list(finance = list(a = TRUE)), "acme"))()
}
limpiar(); quieto(manejador())
ok("6 saving a tier's config: saved, logged as sin_evento, NO alarm",
   !is.null(def) && guardado("acme", S3_KEYS$tiers_config) && length(L$log) == 1L && L$log[[1]]$action == "sin_evento" && !length(L$al))
inline <- function() (function() .s3_write(list(finance = list(a = TRUE)), S3_KEYS$tiers_config, client_id = "acme"))()
limpiar(); quieto(inline())
ok("6 contraprueba: the same write inline (as before) alarms", length(L$al) == 1L)

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
