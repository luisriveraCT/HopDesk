# =============================================================================
# tests/test_t72g_notas.R
# T72-G (notas). Creating and editing a calendar note used to leave nothing
# in the log; deleting left log_action("eliminar_nota") after the save. Now
# each save carries its action's event (save_notes(evento =)) and the write
# layer writes the one row once the save worked. On the real notes_handlers
# (testServer) and the real save_notes / modificar_tabla over the in-memory
# S3:
#   1 crear (public): one crear_nota row with the title; editar: editar_nota;
#   2 a personal note's row never carries its title;
#   3 eliminar: the same words and metadata as the log_action it replaces;
#   4 no write leaves a sin_evento entry; a failed save leaves no row (and
#     the user's notice of always);
#   5 the S3 write sequence (key, order, object) is identical to save_notes
#     called without the event (= the calls of before).
# T81's avisos are not this test's: notas_aviso_* are no-ops here.
# Run from project root:  Rscript --vanilla tests/test_t72g_notas.R
# =============================================================================
cat("=== test_t72g_notas ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
source("tests/helpers/s3_memoria.R")          # R/persistence.R + S3_KEYS
source("R/i18n.R")
source("R/notes_handlers.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

LOGS <- list(); AVISOS <- character()
log_action <- function(user, module, action, description = "", target_id = NULL, metadata = NULL, ...)
  { LOGS[[length(LOGS) + 1L]] <<- list(user = user, module = module, action = action, description = description,
                                       target_id = target_id, metadata = metadata); invisible(NULL) }
bump_sync_version <- function(...) invisible(NULL)
notas_aviso_publicar <- function(...) invisible(NULL)
notas_aviso_retirar  <- function(...) invisible(NULL)
showNotification <- function(ui, ...) { AVISOS <<- c(AVISOS, paste(as.character(ui), collapse = "")); invisible("id") }
showModal <- function(...) invisible(NULL); removeModal <- function(...) invisible(NULL)
hoy_usuario <- function() as.Date("2026-11-23")
mes_es <- function(x) format(as.Date(x), "%Y-%m")
notes_panel_ui <- function(...) div(class = "panel-de-prueba")

Sys.time <- function() as.POSIXct("2026-11-23 10:00:00", tz = "UTC")
.uuid_n <- 0L
.uuid_orig <- uuid::UUIDgenerate
utils::assignInNamespace("UUIDgenerate", function(...) { .uuid_n <<- .uuid_n + 1L; sprintf("n-%04d", .uuid_n) }, "uuid")

SEQ <- list()
.mem_escribir_orig <- .mem_escribir
.mem_escribir <- function(object, valor) {
  r <- .mem_escribir_orig(object, valor)
  SEQ[[length(SEQ) + 1L]] <<- list(key = object, obj = valor)
  r
}

CID <- "dev"; KEY <- paste0(CID, "/", S3_KEYS$notes)
srv <- function(input, output, session) {
  shared <- list(effective_client_id = function() CID, home_client_id = function() CID,
                 current_user_info = function() list(name = "Ana", account_code = "U0001", user = "ana"))
  notes_handlers(input, output, session, shared, notes_df = reactiveVal(.schema_notes()),
                 current_user = function() "ana")
}
notas_s3 <- function() if (exists(KEY, envir = .mem)) get(KEY, envir = .mem) else .schema_notes()
pon <- function(session, nombre, valor) do.call(session$setInputs, stats::setNames(list(valor), nombre))

# The whole story: a public note, a personal one, edit the public one, delete it.
correr <- function() {
  s3_memoria_reset(); SEQ <<- list(); LOGS <<- list(); AVISOS <<- character(); .uuid_n <<- 0L
  out <- list()
  invisible(capture.output(testServer(srv, {
    session$setInputs(month_sel = as.Date("2026-11-01"))
    session$setInputs(add_note = 1, note_title_input = "Cierre de noviembre", note_body_input = "cuerpo")
    session$setInputs(note_save_public = 1)
    session$setInputs(add_note = 2, note_title_input = "Mi recordatorio secreto", note_body_input = "privado")
    session$setInputs(note_save_personal = 1)
    pub <- notas_s3()[notas_s3()$visibility == "public", ]
    pon(session, paste0("edit_note_", pub$id[1]), 1)
    session$setInputs(note_title_input = "Cierre de noviembre (v2)")
    session$setInputs(note_save_public = 2)
    pon(session, paste0("delete_note_", pub$id[1]), 1)
    session$setInputs(note_delete_confirm = 1)
    out$pub_id <<- pub$id[1]
  })))
  c(out, list(seq = SEQ, logs = LOGS, avisos = AVISOS))
}
filas <- function(r, accion) Filter(function(x) identical(x$action, accion), r$logs)

r <- correr()
cr <- filas(r, "crear_nota"); ed <- filas(r, "editar_nota"); el <- filas(r, "eliminar_nota")
ok("control: four saves reached S3 (2 created, 1 edited, 1 deleted)", length(r$seq) == 4L)
ok("1 crear: one crear_nota row per created note (2), by its author, module notes",
   length(cr) == 2L && all(vapply(cr, function(x) identical(x$user, "ana") && identical(x$module, "notes"), NA)))
ok("1 …the public one says its title (tablas.notas.log.creada)",
   identical(cr[[1]]$description, tr("tablas.notas.log.creada", titulo = "Cierre de noviembre")))
ok("1 …its target is the note and its metadata the ledger, visibility and the table",
   identical(cr[[1]]$target_id, r$pub_id) && identical(cr[[1]]$metadata$visibilidad, "public") &&
     identical(cr[[1]]$metadata$ledger, "AR") && identical(cr[[1]]$metadata$tabla, S3_KEYS$notes))
ok("1 editar: one editar_nota row with the new title",
   length(ed) == 1L && identical(ed[[1]]$description, tr("tablas.notas.log.editada", titulo = "Cierre de noviembre (v2)")))
ok("2 calendar_notes gives no row detail (diferencia): a note's words never reach the metadata",
   all(vapply(r$logs, function(x) is.null(x$metadata$diferencia), NA)))
ok("2 a personal note's row says only that one was created, never its title",
   identical(cr[[2]]$description, tr("tablas.notas.log.creada_personal")) &&
     !any(grepl("secreto", vapply(r$logs, function(x) paste(c(x$description, unlist(x$metadata)), collapse = " "), ""), fixed = TRUE)))
ok("3 eliminar: one row, the words and metadata of the log_action it replaces",
   length(el) == 1L && identical(el[[1]]$description, "Nota eliminada: Cierre de noviembre (v2)") &&
     identical(el[[1]]$target_id, r$pub_id) && identical(el[[1]]$metadata$ledger, "AR"))
ok("4 no save leaves a sin_evento entry; nothing else is logged",
   length(r$logs) == 4L && !length(filas(r, "sin_evento")))

# ── 4: a failed save ─────────────────────────────────────────────────────
s3_memoria_reset(); LOGS <- list(); AVISOS <- character()
.mem_falla_escritura <- KEY
invisible(capture.output(testServer(srv, {
  session$setInputs(month_sel = as.Date("2026-11-01"))
  session$setInputs(add_note = 1, note_title_input = "No se guarda", note_body_input = "x")
  session$setInputs(note_save_public = 1)
})))
.mem_falla_escritura <- character()
ok("4 a failed save leaves no crear_nota row, and the user sees comun.error_guardar",
   !length(filas(list(logs = LOGS), "crear_nota")) && tr("comun.error_guardar") %in% AVISOS)

# ── 5: the same story with save_notes called without the event ──────────
save_notes_real <- save_notes
save_notes <- function(df, client_id = NULL, base = NULL, evento = NULL) save_notes_real(df, client_id = client_id, base = base)
s <- correr()
save_notes <- save_notes_real
ok("5 the S3 write sequence (key, order, object) is identical with and without the events",
   identical(lapply(r$seq, `[[`, "key"), lapply(s$seq, `[[`, "key")) &&
     identical(lapply(r$seq, `[[`, "obj"), lapply(s$seq, `[[`, "obj")))
ok("5 …and the notices too", identical(r$avisos, s$avisos))

utils::assignInNamespace("UUIDgenerate", .uuid_orig, "uuid")
s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
