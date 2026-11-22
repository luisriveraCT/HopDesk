# =============================================================================
# tests/test_t74_sueno_comun.R
#
# T74 sleep proposal, common part (tablas/reportes/T74_F1a_propuesta_sueno.md
# v2, approved 2026-11-21). Measured on Connect Cloud: the process dies
# 5-8 min after the last visit and its disk (R_user_dir) is wiped.
#   Part A: with HOPDESK_HOST=connect_cloud the offline queue stages
#           nothing (an entry would never be replayed); without it, it
#           stages as before (counter-proof that the switch is the reason).
#   Part B: .audit_flush_ahora() writes every client's pending audit rows
#           right away, leaves a client already in retry backoff to its own
#           chain, never throws, and a failure falls into the usual retry.
#   Part C: app.R's onSessionEnded calls it after the logout entry.
# Standalone: Rscript --vanilla tests/test_t74_sueno_comun.R (repo root).
# =============================================================================
cat("=== test_t74_sueno_comun ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
.extract_fn <- function(file, fn_name) {
  for (e in parse(file, keep.source = FALSE))
    if (is.call(e) && length(e) >= 2 && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(fn_name))) {
      assign(fn_name, eval(e[[3]], envir = globalenv()), envir = globalenv())
      return(invisible(TRUE))
    }
  stop(sprintf("%s not found in %s", fn_name, file))
}
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x, envir = globalenv())
host_original <- Sys.getenv("HOPDESK_HOST", unset = NA)
on.exit(if (is.na(host_original)) Sys.unsetenv("HOPDESK_HOST") else Sys.setenv(HOPDESK_HOST = host_original), add = TRUE)

# ── Part A: offline queue switch ──────────────────────────────────────────────
cat("Part A: offline queue off on Connect Cloud\n")
for (fn in c(".queue_root_dir", ".queue_slug", ".queue_valid_client_id",
             ".queue_client_dir", ".queue_entries_dir", ".queue_manifest_path",
             ".queue_pending_index_path", ".queue_manifest_schema",
             ".queue_read_manifest", ".queue_write_manifest", ".queue_next_seq",
             ".QUEUE_LOCK_STALE_SECS", ".queue_lock_dir", ".queue_with_client_lock",
             ".queue_mark_client_pending", ".queue_table_name_for_key",
             ".queue_stage_entry", ".queue_session_actual",
             ".queue_read_pending_index", ".queue_write_pending_index"))
  .extract_fn("R/offline_queue.R", fn)
assign(".s3_bucket", function(...) "bucket-de-prueba", envir = globalenv())
assign(".SYNC_REGISTRY", list(moves_db = list(s3_key = "invoice_moves.rds")), envir = globalenv())
assign(".sync_versions", list(moves_db = 3L), envir = globalenv())
qdir <- file.path(tempdir(), paste0("t74_cola_", Sys.getpid()))
options(hopdesk.queue_dir = qdir)
payload <- tempfile(fileext = ".rds"); saveRDS(data.frame(id = "a"), payload)
etapa <- function() suppressMessages(.queue_stage_entry(payload_path = payload, s3_key = "acme/invoice_moves.rds",
                                                        key = "invoice_moves.rds", client_id = "acme"))
archivos <- function() length(list.files(qdir, recursive = TRUE))

Sys.setenv(HOPDESK_HOST = "connect_cloud")
ok("A1 connect_cloud: nothing staged (FALSE)", identical(etapa(), FALSE))
ok("A1 connect_cloud: no file written under the queue dir", archivos() == 0L)
Sys.setenv(HOPDESK_HOST = "  Connect_Cloud ")
ok("A2 case and spaces do not matter", identical(etapa(), FALSE) && archivos() == 0L)
msg <- character(0)
invisible(withCallingHandlers(.queue_stage_entry(payload_path = payload, s3_key = "acme/invoice_moves.rds",
                                       key = "invoice_moves.rds", client_id = "acme"),
                    message = function(m) { msg <<- c(msg, conditionMessage(m)); invokeRestart("muffleMessage") }))
ok("A3 says so in the log", any(grepl("disabled on this host", msg, fixed = TRUE)))
Sys.unsetenv("HOPDESK_HOST")
ok("A4 counter-proof: without HOPDESK_HOST it stages (TRUE)", isTRUE(etapa()) && archivos() > 0L)
unlink(qdir, recursive = TRUE)
Sys.setenv(HOPDESK_HOST = "shinyapps")
ok("A5 another host: it stages", isTRUE(etapa()))
unlink(qdir, recursive = TRUE); options(hopdesk.queue_dir = NULL)

# ── Part B: .audit_flush_ahora ────────────────────────────────────────────────
cat("\nPart B: flush the activity log when a session ends\n")
for (fn in c(".audit_pending", ".audit_flush_scheduled", ".audit_retry_count",
             ".AUDIT_FLUSH_DELAY", ".AUDIT_FLUSH_RETRY_DELAY", ".AUDIT_MAX_RETRIES",
             ".audit_queue", ".audit_flush_pending", ".audit_flush_ahora"))
  .extract_fn("R/app_audit.R", fn)
almacen <- new.env()
falla <- FALSE
assign(".audit_current_key", function(cid) paste0(cid, "/app_audit.rds"), envir = globalenv())
assign(".audit_normalizar", function(x) if (is.null(x)) data.frame(action = character(0)) else x, envir = globalenv())
assign("rotate_log_if_needed", function(...) FALSE, envir = globalenv())
assign(".audit_read_chunk_para_escribir", function(key) almacen[[key]], envir = globalenv())
assign(".audit_write_chunk", function(df, key) { if (falla) stop("S3 caido"); almacen[[key]] <- df; invisible(TRUE) }, envir = globalenv())
assign(".audit_rescatar", function(...) invisible(FALSE), envir = globalenv())
filas <- function(cid) NROW(almacen[[paste0(cid, "/app_audit.rds")]])

.audit_queue("acme", data.frame(action = "logout"))
.audit_queue("acme", data.frame(action = "guardar"))
.audit_queue("beta", data.frame(action = "logout"))
ok("B0 rows wait in memory before the flush", filas("acme") == 0L && filas("beta") == 0L)
hechos <- suppressMessages(.audit_flush_ahora())
ok("B1 both clients written right away", filas("acme") == 2L && filas("beta") == 1L)
ok("B1 returns the clients it flushed", setequal(hechos, c("acme", "beta")))
ok("B1 nothing left pending", is.null(.audit_pending[["acme"]]) && is.null(.audit_pending[["beta"]]))
later::run_now(.AUDIT_FLUSH_DELAY + 1)   # the 2 s timers scheduled by .audit_queue fire on an empty buffer
ok("B2 the old 2 s timer does not write the rows twice", filas("acme") == 2L && filas("beta") == 1L)

.audit_queue("acme", data.frame(action = "otra"))
assign("acme", 2L, envir = .audit_retry_count)
hechos <- suppressMessages(.audit_flush_ahora())
ok("B3 a client in retry backoff is left to its own chain", !"acme" %in% hechos && NROW(.audit_pending[["acme"]]) == 1L)
rm("acme", envir = .audit_retry_count)

falla <- TRUE
res <- tryCatch({ suppressMessages(.audit_flush_ahora()); "sin error" }, error = function(e) "truena")
ok("B4 an S3 failure never throws", identical(res, "sin error"))
ok("B4 the rows go back to the buffer for the usual retry", NROW(.audit_pending[["acme"]]) == 1L &&
     identical(.audit_retry_count[["acme"]], 1L))
falla <- FALSE
ok("B5 nothing pending: no-op", { for (k in ls(.audit_pending)) .audit_pending[[k]] <- NULL
                                   length(suppressMessages(.audit_flush_ahora())) == 0L })

# ── Part C: app.R wiring ──────────────────────────────────────────────────────
cat("\nPart C: app.R calls it on session end\n")
app <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
bloque <- regmatches(app, regexpr("session\\$onSessionEnded\\(function\\(\\) \\{[^}]*\\}\\)", app))
ok("C1 onSessionEnded calls .log_logout_once() then .audit_flush_ahora()",
   length(bloque) == 1L && grepl(".log_logout_once\\(\\)[\\s\\S]*\\.audit_flush_ahora\\(\\)", bloque, perl = TRUE))

cat(sprintf("\n=== %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
