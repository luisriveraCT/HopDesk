# =============================================================================
# tests/test_cola_sesion.R
# CM09, option B (Luis, 2026-11-01) for the offline queue replay:
#   - within one session, only the newest pending entry per table is
#     replayed; the older ones become "superseded";
#   - between sessions, last write wins with the victim backed up
#     (_conflict_replay), exactly as before;
#   - an entry keeps permitir_reduccion, so a queued purge does upload;
#   - a drastic shrink without the flag is "blocked" (_queue_red_refused);
#   - a manifest written before session_id/permitir_reduccion existed still
#     takes new entries, and its rows replay one by one as before.
#
# Run from project root:  Rscript tests/test_cola_sesion.R
# =============================================================================
cat("=== test_cola_sesion ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/offline_queue.R"))
for (e in parse("R/sync_bus.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c(".SYNC_REGISTRY", "register_synced"))
    eval(e, globalenv())
if (!exists(".SYNC_REGISTRY")) .SYNC_REGISTRY <- list()
register_synced("moves_db", "invoice_moves.rds", function(client_id = NULL) NULL)

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(expr)

# put_object (the replay's direct upload) goes to s3_memoria's store, which
# counts it in .mem_subidas and keeps its bytes as S3 does: read MK back
# through s3readRDS (T30: no stub of its own).
leer <- function(k) aws.s3::s3readRDS(object = k, bucket = "fake-bucket")

.LOG <- list()
log_action <- function(user, module, action, description, ...) {
  .LOG[[length(.LOG) + 1L]] <<- list(action = action, description = description); invisible(NULL)
}
acciones <- function() vapply(.LOG, `[[`, character(1), "action")
# The version stamp lives in the store; a successful replay bumps it.
VK <- "acme/sync_versions.rds"
bump_sync_version <- function(table_name, client_id) {
  v <- get(VK, envir = .mem); v[[table_name]] <- (v[[table_name]] %||% 0L) + 1L
  assign(VK, v, envir = .mem); TRUE
}

options(hopdesk.queue_dir = file.path(tempdir(), paste0("cola_sesion_", Sys.getpid())))
MK <- "acme/invoice_moves.rds"
tabla <- function(ids) data.frame(id = as.character(ids), v = paste0("v", ids), stringsAsFactors = FALSE)
encolar <- function(df, sesion, version = 5L, permitir = FALSE) {
  .GlobalEnv$.sync_versions <- list(moves_db = version)
  tf <- tempfile(fileext = ".rds"); saveRDS(df, tf)
  r <- .queue_stage_entry(tf, MK, "invoice_moves.rds", "acme",
                          acting_user = "t", permitir_reduccion = permitir, session_id = sesion)
  unlink(tf); r
}
empezar <- function(filas_s3 = 1:6) {
  s3_memoria_reset(); .LOG <<- list()
  unlink(.queue_root_dir(), recursive = TRUE)
  assign(MK, tabla(filas_s3), envir = .mem)
  assign(VK, list(moves_db = 5L), envir = .mem)
}
estado <- function() .queue_read_manifest("acme")$status

# 1. Same session: only the newest entry is replayed
empezar()
ok("setup: two entries from the same session staged",
   encolar(tabla(1:7), "s1") && encolar(tabla(1:8), "s1"))
q(.queue_flush_client("acme"))
ok("same session: seq 1 is superseded and seq 2 synced", identical(estado(), c("superseded", "synced")))
ok("same session: one upload, and S3 holds seq 2", .mem_subidas == 1L && nrow(leer(MK)) == 8L)
ok("same session: no false conflict is logged", !"_conflict_replay" %in% acciones() &&
     "_queue_superseded" %in% acciones())
ok("same session: the superseded payload stays on disk",
   file.exists(file.path(.queue_entries_dir("acme"), sprintf("%012d.rds", 1L))))

# 2. Different sessions: last write wins, with the victim backed up
empezar()
encolar(tabla(1:7), "s1"); encolar(tabla(c(1:6, 9)), "s2")
q(.queue_flush_client("acme"))
ok("different sessions: both are replayed", identical(estado(), c("synced", "synced")) && .mem_subidas == 2L)
ok("different sessions: the second one wins", "9" %in% leer(MK)$id)
ok("different sessions: a conflict is logged for the second",
   sum(acciones() == "_conflict_replay") == 1L)
bk <- .queue_conflict_backup_path("acme", 2L)
ok("different sessions: the victim (seq 1's table) is backed up",
   file.exists(bk) && "7" %in% readRDS(bk)$id)

# 3. A queued purge (permitir_reduccion) uploads
empezar(1:10)
encolar(tabla(1), "s1", permitir = TRUE)
ok("the entry keeps the flag", isTRUE(.queue_read_manifest("acme")$permitir_reduccion))
q(.queue_flush_client("acme"))
ok("a queued purge with the flag is synced and S3 holds 1 row",
   identical(estado(), "synced") && nrow(leer(MK)) == 1L)

# 4. A drastic shrink without the flag is blocked
empezar(1:10)
encolar(tabla(1), "s1")
q(.queue_flush_client("acme"))
ok("a drastic shrink without the flag is blocked", identical(estado(), "blocked"))
ok("... S3 keeps its 10 rows, and the refusal is logged",
   nrow(leer(MK)) == 10L && "_queue_red_refused" %in% acciones() && .mem_subidas == 0L)

# 5. A queue left on disk before session_id/permitir_reduccion existed
empezar()
viejo <- .queue_manifest_schema()[, setdiff(names(.queue_manifest_schema()), c("session_id", "permitir_reduccion"))]
dir.create(.queue_entries_dir("acme"), recursive = TRUE, showWarnings = FALSE)
saveRDS(tabla(1:7), file.path(.queue_entries_dir("acme"), sprintf("%012d.rds", 1L)))
viejo <- rbind(viejo, data.frame(seq = 1L, s3_key = MK, bucket = .s3_bucket(), table_name = "moves_db",
                                 client_id = "acme", expected_sync_version = 5L, acting_user = "t",
                                 acting_home_client_id = "acme", status = "pending",
                                 staged_at = Sys.time(), synced_at = as.POSIXct(NA),
                                 stringsAsFactors = FALSE))
.queue_write_manifest("acme", viejo)
.queue_mark_client_pending("acme")
ok("an old manifest still takes a new entry", encolar(tabla(1:8), NA_character_))
q(.queue_flush_client("acme"))
ok("old/no-session entries replay one by one, as before", identical(estado(), c("synced", "synced")) && .mem_subidas == 2L)

# 6. .s3_write stages the flag and the session when the upload fails
empezar(1:10)
# S3 refuses this table's upload, both ways (s3saveRDS and the put_object
# fallback): s3_memoria's .mem_falla_escritura.
.mem_falla_escritura <- .s3_key("invoice_moves.rds", client_id = "acme")
r <- tryCatch(q(shiny::withReactiveDomain(list(token = "tok1"),
        .s3_write(tabla(1), "invoice_moves.rds", "acme", permitir_reduccion = TRUE))), error = function(e) e)
.mem_falla_escritura <- character(0)
m <- .queue_read_manifest("acme")
ok(".s3_write: the failed upload raised its error and was queued", inherits(r, "error") && nrow(m) == 1L)
ok(".s3_write: the entry keeps permitir_reduccion", isTRUE(m$permitir_reduccion))
ok(".s3_write: the entry records the session (pid:token)", identical(m$session_id, paste0(Sys.getpid(), ":tok1")))

s3_memoria_restaurar()
unlink(.queue_root_dir(), recursive = TRUE)
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
