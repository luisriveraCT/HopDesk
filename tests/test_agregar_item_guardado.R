# =============================================================================
# tests/test_agregar_item_guardado.R
# Agregar Item (app.R, me_save) never announces a save that did not happen
# (PENDIENTES, integrador; R17):
#   1. edit mode: save_manual() first; on failure nothing changes in the
#      session, the Agenda is not touched, "Entrada actualizada" is not shown,
#      the modal stays, and editar_item_fallido is logged (as insert mode does
#      since 36717c7);
#   2. "Enviar a Agenda": the Agenda is read as S3 holds it now -- a failed
#      read is never replaced by the session's copy -- and a failed write is
#      neither applied to the session nor announced as "enviada a Agenda"
#      (agregar_a_agenda() in R/data_pipeline.R).
#
# Run from project root:  Rscript tests/test_agregar_item_guardado.R
# Counter-proof: HD_APP_R=<path to 4554de3's app.R> fails the app.R checks.
# =============================================================================
cat("=== test_agregar_item_guardado ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── 1. agregar_a_agenda(): behaviour ─────────────────────────────────────────
for (e in parse("R/data_pipeline.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("agregar_a_agenda", "stage_manual_row_to_agenda"))
    eval(e, globalenv())
# The real upsert (R/persistence.R), on its own.
for (e in parse("R/persistence.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "upsert_pagar_hoy"))
    eval(e, globalenv())

ok(exists("agregar_a_agenda"), "agregar_a_agenda() exists in R/data_pipeline.R")
if (exists("agregar_a_agenda")) {
  vigente <- tibble::tibble(id = c("otra-sesion"), ledger = "AP", Empresa = "E", Moneda = "MXN",
                            Documento = "D1", Importe = 10)
  fila    <- tibble::tibble(id = "nueva", ledger = "AP", Empresa = "E", Moneda = "MXN",
                            Documento = "D2", Importe = 20)
  up <- function(ph) upsert_pagar_hoy(ph, fila, keys = "id")
  llamadas <- list()
  guardar_ok <- function(ph, base) { llamadas[[length(llamadas) + 1L]] <<- list(ph = ph, base = base); invisible(TRUE) }

  r <- agregar_a_agenda(leer = function() vigente, cambio = up, guardar = guardar_ok)
  ok(isTRUE(r$ok) && setequal(r$ph$id, c("otra-sesion", "nueva")),
     "ok: the new row is added to the Agenda as S3 holds it (the other session's row stays)")
  ok(length(llamadas) == 1L && identical(llamadas[[1]]$base, vigente),
     "ok: written once, rebased on what was read")

  llamadas <- list()
  r <- agregar_a_agenda(leer = function() stop("S3 caído"), cambio = up, guardar = guardar_ok)
  ok(!isTRUE(r$ok) && is.null(r$ph) && identical(r$error, "S3 caído"),
     "failed read: not ok, no Agenda, the error is given back")
  ok(length(llamadas) == 0L, "failed read: nothing is written (no fallback to the session's copy)")

  r <- agregar_a_agenda(leer = function() NULL, cambio = up, guardar = guardar_ok)
  ok(!isTRUE(r$ok) && length(llamadas) == 0L, "a read that gives no table: not ok, nothing written")

  r <- agregar_a_agenda(leer = function() vigente, cambio = up, guardar = function(ph, base) stop("403"))
  ok(!isTRUE(r$ok) && is.null(r$ph) && identical(r$error, "403"),
     "failed write: not ok and no Agenda to put in the session")
  llamadas <- list()
  r <- agregar_a_agenda(leer = function() vigente, cambio = function(ph) stop("mala fila"), guardar = guardar_ok)
  ok(!isTRUE(r$ok) && identical(r$error, "mala fila") && length(llamadas) == 0L,
     "a change that fails: not ok, nothing written")
}

# ── 2. app.R, me_save ────────────────────────────────────────────────────────
app <- Sys.getenv("HD_APP_R", "app.R")
src <- readLines(app, warn = FALSE, encoding = "UTF-8")
codigo <- sub("#.*$", "", src)
ini <- grep("observeEvent(input$me_save", src, fixed = TRUE)[1]
fin <- grep("observeEvent(input$me_cancel", src, fixed = TRUE)[1]
ok(!is.na(ini) && !is.na(fin) && fin > ini, "me_save found in app.R")
blk <- codigo[ini:fin]
edit_ini <- grep("Edit mode", src[ini:fin], fixed = TRUE)[1]
ins_ini  <- grep("Insert mode", src[ini:fin], fixed = TRUE)[1]
edit <- blk[edit_ini:ins_ini]
ins  <- blk[ins_ini:length(blk)]

primera <- function(lineas, patron) grep(patron, lineas, fixed = TRUE)[1]
g_save  <- primera(edit, "save_manual(")
g_check <- primera(edit, "if (!isTRUE(guardado))")
g_set   <- primera(edit, "manual_inv(df)")
g_sync  <- primera(edit, ".sync_staged(")
g_msg   <- primera(edit, "tablas.manual.aviso.actualizada")
ok(!is.na(g_check) && !is.na(g_set) && g_save < g_check && g_check < g_set,
   "edit: saved first; the session takes the row only after the save is checked")
ok(!is.na(g_check) && !is.na(g_sync) && !is.na(g_msg) && g_check < g_sync && g_check < g_msg,
   "edit: the Agenda sync and 'Entrada actualizada' come only after a good save")
fallo <- if (is.na(g_check)) character() else edit[g_check:(g_check + 10L)]
ok(any(grepl("comun.error_guardar", fallo, fixed = TRUE)) && any(grepl('"editar_item_fallido"', fallo, fixed = TRUE)) &&
     any(grepl("return()", fallo, fixed = TRUE)),
   "edit: a failed save says so, is logged (editar_item_fallido) and keeps the modal")
ok(!any(grepl("tablas.manual.aviso.no_guardada", edit, fixed = TRUE)),
   "edit: no more warning-and-go-on for a failed save")

ok(!any(grepl("%||% pagar_hoy_db()", ins, fixed = TRUE)),
   "agenda: a failed read never falls back to the session's copy")
ok(any(grepl("agregar_a_agenda(", ins, fixed = TRUE)), "agenda: through agregar_a_agenda()")
ok(any(grepl('cambio  = function(ph_base) upsert_pagar_hoy(ph_base, ph_row, keys = "id")', ins, fixed = TRUE)),
   "agenda: the upsert stays in app.R (the Agenda guardians see its permission check)")
dp <- sub("#.*$", "", readLines("R/data_pipeline.R", warn = FALSE, encoding = "UTF-8"))
ok(!any(grepl("upsert_pagar_hoy(", dp, fixed = TRUE)),
   "R/data_pipeline.R writes no Agenda row itself (no new path for test_agenda_permiso / test_confirmar_cierra)")
ok(any(grepl("if (isTRUE(agenda$ok)) pagar_hoy_db(agenda$ph)", ins, fixed = TRUE)),
   "agenda: the session takes the Agenda only when it was saved")
ok(any(grepl("send_to_agenda <- isTRUE(agenda$ok)", ins, fixed = TRUE)),
   "agenda: 'enviada a Agenda' only when it was saved; otherwise just 'Entrada agregada'")

# ── 3. app.R's own call, run ─────────────────────────────────────────────────
# The statement `agenda <- agregar_a_agenda(...)` is taken from app.R as it is
# and evaluated with the Agenda's I/O stubbed: a call that does not match the
# helper's signature fails here (a static check alone passed with it broken).
llamada <- NULL
buscar <- function(e) {
  if (!is.null(llamada) || !is.call(e)) return(invisible())
  if (identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("agenda")) &&
      is.call(e[[3]]) && identical(e[[3]][[1]], as.name("agregar_a_agenda"))) { llamada <<- e; return(invisible()) }
  for (i in seq_along(e)[-1])   # an empty argument (x[, 1]) is skipped, never touched
    if (!identical(e[[i]], quote(expr = )) && is.call(e[[i]])) buscar(e[[i]])
}
for (e in parse(text = src, keep.source = FALSE)) buscar(e)
ok(!is.null(llamada), "app.R's agenda <- agregar_a_agenda(...) found")
if (!is.null(llamada) && exists("agregar_a_agenda")) {
  ent <- new.env(parent = globalenv())
  ent$usr_ag <- "u"; ent$cid_ag <- "cli"
  ent$ph_row <- tibble::tibble(id = "nueva", ledger = "AP", Empresa = "E", Moneda = "MXN", Documento = "D2", Importe = 20)
  vig <- tibble::tibble(id = "otra-sesion", ledger = "AP", Empresa = "E", Moneda = "MXN", Documento = "D1", Importe = 10)
  escrito <- NULL
  ent$safe_load_pagar_hoy <- function(username, client_id) { stopifnot(identical(client_id, "cli")); vig }
  ent$save_pagar_hoy <- function(df, username, client_id, base) {
    stopifnot(identical(client_id, "cli"), identical(base, vig)); escrito <<- df; invisible(df) }
  r <- tryCatch({ eval(llamada, ent); ent$agenda }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  ok(isTRUE(r$ok) && setequal(r$ph$id, c("otra-sesion", "nueva")) && identical(escrito, r$ph),
     "app.R's call works: reads S3's Agenda, adds the row, saves it rebased (\"Enviar a Agenda\")")
  ent$safe_load_pagar_hoy <- function(username, client_id) stop("S3 caído"); escrito <- NULL
  r <- tryCatch({ eval(llamada, ent); ent$agenda }, error = function(e) list(ok = TRUE))
  ok(!isTRUE(r$ok) && is.null(escrito), "app.R's call with a failed read: not ok, nothing saved")
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
