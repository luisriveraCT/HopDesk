# =============================================================================
# tests/test_t95a_descartar.R
# T95a (R/avisos.R): each user's own view of the avisos.
#   - descartar takes an aviso off ONE user's view (descartados, twin of
#     hechos); everyone else still sees it; nothing is deleted;
#   - what may not be discarded (a solicitud for me, not done) is refused on
#     the server;
#   - restaurar puts it back; a grouped aviso that grows is news again;
#   - every such write carries its T72 event (who, which action);
#   - avisos_fuera_de_vista(): a SEEN aviso leaves the default view after its
#     days by kind; what was never seen never leaves by itself;
#   - avisos_visibles() keeps its contract: a discarded aviso is still visible
#     (the view decides, T95b), and old rows without the column read fine.
# S3 is an in-memory store local to this test (as tests/test_avisos.R): only
# the persistence helpers avisos.R calls are redefined; aws.s3 is never patched.
# Run from the repo root: Rscript --vanilla tests/test_t95a_descartar.R
# =============================================================================
cat("=== test_t95a_descartar ===\n\n")
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}

.mem <- new.env()
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.s3_read_with <- function(key, client_id = NULL) .mem[[paste0(client_id, "/", key)]]
.s3_leer_para_escribir <- function(key, client_id) .mem[[paste0(client_id, "/", key)]]
.s3_existe <- function(key, client_id) !is.null(.mem[[paste0(client_id, "/", key)]])
EVENTOS <- list()
.fallar_escrituras <- 0L
.s3_write <- function(obj, key, client_id = NULL, evento = NULL) {
  if (.fallar_escrituras > 0L) { .fallar_escrituras <<- .fallar_escrituras - 1L; stop("simulada") }
  EVENTOS[[length(EVENTOS) + 1L]] <<- list(key = key, evento = evento)
  assign(paste0(client_id, "/", key), obj, envir = .mem)
}
ultimo_evento <- function() EVENTOS[[length(EVENTOS)]]$evento
usr <- function(user, tier) list(user = user, tier = tier, permisos = permisos_resueltos(tier))
CID <- "cliente_prueba"
fila <- function(id) { df <- avisos_leer(CID); df[df$id == id, , drop = FALSE] }

# ── 1. Descartar: one user's view only ───────────────────────────────────────
com <- avisos_agregar(CID, list(tipo = "publicado", titulo = "Cierre de mes", niveles = c("admin", "finance")), por = "ana")
r <- avisos_descartar(CID, com, "Beto", home = "cliente_prueba")
ok("1a descartar marks the user (lower-cased) in descartados, and returns the id",
   identical(r$descartados, com) && !length(r$rechazados) && identical(.de_json(fila(com)$descartados), "beto"))
ok("1b only for that user: avisos_descartado() is TRUE for beto, FALSE for caro",
   isTRUE(avisos_descartado(fila(com), "beto")) && identical(avisos_descartado(fila(com), "caro"), FALSE))
ok("1c avisos_visibles() keeps its contract: the discarded aviso is still visible to beto (the view decides, T95b)",
   com %in% avisos_visibles(CID, usr("beto", "admin"))$id)
ok("1d nothing is deleted: the row is there, not retired",
   nrow(fila(com)) == 1L && !isTRUE(fila(com)$retirado))
ev <- ultimo_evento()
ok("1e the write carries its T72 event: user, module panorama, accion descartar_aviso, a description, the home",
   identical(ev$usuario, "beto") && identical(ev$modulo, "panorama") && identical(ev$accion, "descartar_aviso") &&
     is.character(ev$descripcion) && nzchar(ev$descripcion) && !grepl("avisos.log", ev$descripcion, fixed = TRUE) &&
     identical(ev$home, "cliente_prueba"))
r2 <- avisos_descartar(CID, com, "beto")
ok("1f discarding twice keeps one mark", identical(.de_json(fila(com)$descartados), "beto"))

# ── 2. What may not be discarded ─────────────────────────────────────────────
sol <- avisos_agregar(CID, list(tipo = "personal", clase = "solicitud", titulo = "Sube los comprobantes",
                                usuarios = "caro", clave = "sol:1|caro"), por = "ana")
r <- avisos_descartar(CID, c(sol, com), "caro")
ok("2a a solicitud for me, not done, is refused on the server (the other id still goes)",
   identical(r$rechazados, sol) && identical(r$descartados, com) && !length(.de_json(fila(sol)$descartados)))
avisos_marcar_hecho(CID, sol, "caro")
r <- avisos_descartar(CID, sol, "caro")
ok("2b once done, it may be discarded", identical(r$descartados, sol))
ok("2c a solicitud for someone else is discardable for me (not mine to do)",
   isTRUE(avisos_descartable(fila(sol), "dani")))
ok("2d a comunicado, an activity and a req aviso are discardable",
   all(avisos_descartable(rbind(fila(com)), "beto")))
ok("2e an old row with no clase is never a solicitud",
   isTRUE(avisos_descartable(transform(fila(com), clase = NA_character_, tipo = "personal", usuarios = '["beto"]'), "beto")))

# ── 3. Limpiar lo visto: many in one write, its own accion ──────────────────
a1 <- avisos_agregar(CID, list(tipo = "automatico", titulo = "Carlos confirmó un pago", clave = "act:1"))
a2 <- avisos_agregar(CID, list(tipo = "automatico", titulo = "Marta registró bancos", clave = "act:2"))
n0 <- length(EVENTOS)
r <- avisos_descartar(CID, c(a1, a2), "beto", accion = "limpiar_avisos")
ok("3a Limpiar lo visto: both ids in ONE write, accion limpiar_avisos",
   setequal(r$descartados, c(a1, a2)) && length(EVENTOS) == n0 + 1L && identical(ultimo_evento()$accion, "limpiar_avisos"))
ok("3b an unknown accion is refused", inherits(tryCatch(avisos_descartar(CID, a1, "beto", accion = "borrar"), error = function(e) e), "error"))

# ── 4. Restaurar ─────────────────────────────────────────────────────────────
h <- avisos_restaurar(CID, c(a1, com), "beto")
ok("4a restaurar takes the user off descartados and returns what it changed",
   setequal(h, c(a1, com)) && !"beto" %in% .de_json(fila(a1)$descartados))
ok("4b ... with its T72 event (restaurar_aviso)", identical(ultimo_evento()$accion, "restaurar_aviso"))
ok("4c the other users' marks stay (caro still has com discarded)", "caro" %in% .de_json(fila(com)$descartados))
ok("4d restoring what was not discarded changes nothing", !length(avisos_restaurar(CID, a1, "beto")))

# ── 5. A grouped aviso that grows is news again ─────────────────────────────
avisos_descartar(CID, a2, "caro")
avisos_agregar(CID, list(tipo = "automatico", titulo = "Marta registró bancos", clave = "act:2"))
ok("5a the same clave again: contador + 1 and nobody has it discarded any more",
   fila(a2)$contador == 2L && !length(.de_json(fila(a2)$descartados)))
p1 <- avisos_agregar(CID, list(tipo = "automatico", titulo = "Nota publicada", clave = "nota|9", repetir = FALSE))
avisos_descartar(CID, p1, "beto")
avisos_agregar(CID, list(tipo = "automatico", titulo = "Nota publicada", clave = "nota|9", repetir = FALSE))
ok("5b repetir = FALSE (T62) leaves it alone: still discarded", "beto" %in% .de_json(fila(p1)$descartados))

# ── 6. Edge cases ────────────────────────────────────────────────────────────
ok("6a no user or no ids: nothing written", {
  n0 <- length(EVENTOS)
  avisos_descartar(CID, com, ""); avisos_descartar(CID, character(), "beto"); avisos_restaurar(CID, com, NULL)
  length(EVENTOS) == n0 })
ok("6b without client_id it fails closed", inherits(tryCatch(avisos_descartar(NULL, com, "beto"), error = function(e) e), "error"))
.fallar_escrituras <- 1L
ok("6c one failed write is retried (re-reading), the mark lands", {
  avisos_descartar(CID, a1, "eli"); "eli" %in% .de_json(fila(a1)$descartados) })
.mem[[paste0(CID, "/avisos/avisos.rds")]]$descartados <- NULL   # a file written before T95
ok("6d an avisos.rds written before the column reads, discards and restores fine", {
  df <- avisos_leer(CID); r <- avisos_descartar(CID, com, "beto")
  "descartados" %in% names(df) && all(is.na(df$descartados)) && identical(r$descartados, com) })
# Nothing to discard = nothing written, nothing logged (as T119, asked in T95b).
pend <- avisos_personales(CID, "dani", list(tipo = "personal", clase = "solicitud", titulo = "Firma el contrato",
                                           cuerpo = "", clave = "sol:t95a6", autor = "ana"))
ok("6e every id refused (a solicitud pending for me, an unknown id): NO write, no log; refused ones returned", {
  n0 <- length(EVENTOS)
  r <- avisos_descartar(CID, c(pend, "no-existe"), "dani")
  length(EVENTOS) == n0 && identical(r$rechazados, pend) && !length(r$descartados) })
ok("6f every id already off my view: NO write, no log", {
  avisos_descartar(CID, a1, "dani"); n0 <- length(EVENTOS)
  r <- avisos_descartar(CID, a1, "dani")
  length(EVENTOS) == n0 && !length(r$descartados) })
ok("6g counter-test: one discardable among refused ones -> ONE write; the log counts only what was discarded", {
  n0 <- length(EVENTOS)
  r <- avisos_descartar(CID, c(pend, a2, "no-existe"), "dani", accion = "limpiar_avisos")
  length(EVENTOS) == n0 + 1L && identical(r$descartados, a2) && identical(r$rechazados, pend) &&
    identical(ultimo_evento()$descripcion, tr("avisos.log.descartar", n = 1L)) })

# ── 7. What leaves the default view by itself ────────────────────────────────
ahora <- as.POSIXct("2026-11-24 12:00:00", tz = "UTC")
dias <- function(d) ahora - d * 86400
uv   <- dias(0.5)   # last visit: this morning
f <- avisos_fuera_de_vista(
  tipo_feed = c("act",    "act",    "req",    "req",    "com",    "com",     "com",    "sol",    "ale",    "act"),
  cuando    = c(dias(2),  dias(3),  dias(6),  dias(7),  dias(13), dias(20),  dias(20), dias(40), dias(40), dias(0.2)),
  ultima_visita = uv,
  vence     = c(NA,       NA,       NA,       NA,       NA,       NA,        ahora + 86400, NA,  NA,       NA),
  ahora = ahora)
ok("7a actividad: seen, 2 days stays, 3 days leaves", identical(f[1:2], c(FALSE, TRUE)))
ok("7b requisición: 6 days stays, 7 leaves", identical(f[3:4], c(FALSE, TRUE)))
ok("7c comunicado without a vence: 13 days stays, 20 leaves; with a vence, the vence decides (stays)",
   identical(f[5:7], c(FALSE, TRUE, FALSE)))
ok("7d solicitudes and alerts never leave by days", identical(f[8:9], c(FALSE, FALSE)))
ok("7e what came after the last visit (not seen) stays", identical(f[10], FALSE))
ok("7f what was never seen never leaves by itself: no last visit -> nothing leaves",
   !any(avisos_fuera_de_vista(c("act", "req", "com"), c(dias(30), dias(30), dias(30)), NULL, ahora = ahora)))
ok("7g the windows are the approved constants: actividad 3, requisición 7, comunicado 14",
   identical(AVISOS_VENTANA_VISTO, c(act = 3, req = 7, com = 14)))
ok("7h empty in, empty out", identical(avisos_fuera_de_vista(character(), NULL, uv, ahora = ahora), logical()))

# ── 8. The dictionary ────────────────────────────────────────────────────────
es <- jsonlite::fromJSON("www/i18n/es.json"); en <- jsonlite::fromJSON("www/i18n/en.json")
ok("8a the two log descriptions exist in es and en, with {n}",
   all(c("avisos.log.descartar", "avisos.log.restaurar") %in% names(es)) &&
     all(c("avisos.log.descartar", "avisos.log.restaurar") %in% names(en)) &&
     all(grepl("{n}", c(es[["avisos.log.descartar"]], en[["avisos.log.restaurar"]]), fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
