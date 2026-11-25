# =============================================================================
# tests/test_t88_1d_excel_reloj.R
#
# T88-1d (2026-11-25): the Flujo Excel's poll observer read a 1-second
# reactiveTimer BEFORE checking whether a job runs, so it woke every second in
# every session. Each run of an observer sends Shiny's busy/idle to the
# browser (Observer$.createContext -> incrementBusyCount), so the loading
# overlay never saw its 1.5 s of silence and always went by its 30 s cap.
# The observer now asks for its tick only while a job runs.
#   A. idle: the observer does not run again over a minute;
#   B. a job starts: it polls about once a second;
#   C. the job ends: "done", the file, the notification -- then it is quiet;
#   D. the 60 s warning still comes (warn60 keeps polling);
#   E. the module has no reactiveTimer left.
# The real observer, taken from R/cashflow_export_module.R by parse(), run
# under shiny::testServer() with fake state and a fake job (no callr, no S3).
# Run from project root.
# =============================================================================
cat("=== test_t88_1d_excel_reloj ===\n\n")
suppressPackageStartupMessages(library(shiny))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

src <- "R/cashflow_export_module.R"
buscar <- function(e) {
  if (is.call(e) && identical(e[[1]], as.name("observe")) && any(grepl("is_alive", deparse(e)))) return(list(e))
  if (is.call(e) || is.expression(e))
    return(do.call(c, lapply(as.list(e), function(x) if (missing(x)) NULL else buscar(x))))
  NULL
}
obs <- do.call(c, lapply(parse(src, keep.source = FALSE), buscar))
if (length(obs) != 1L) stop("expected exactly one poll observer (is_alive) in ", src, "; found ", length(obs))
POLL <- obs[[1]]

# One run of the scenario. Returns what happened; `pasos` gets the session.
correr <- function(pasos) {
  r <- new.env()
  r$corridas <- 0L; r$avisos <- character(); r$modales <- 0L; r$vivo <- TRUE
  srv <- function(input, output, session) {
    estado <- reactiveVal("idle")
    .xl_state <- function(v) {
      if (missing(v)) { r$corridas <- r$corridas + 1L; return(estado()) }   # every run reads it first
      estado(v)
    }
    .xl_job <- reactiveVal(NULL); .xl_start <- reactiveVal(NULL)
    .xl_warn_at <- reactiveVal(60); .xl_file <- reactiveVal(NULL)
    .cf_excel_resultado <- function(job, out) list(estado = "done", aviso = NULL)
    showNotification <- function(ui, ...) { r$avisos <- c(r$avisos, paste(unlist(ui), collapse = "")); invisible() }
    showModal <- function(...) { r$modales <- r$modales + 1L; invisible() }
    removeModal <- function(...) invisible()
    tr <- function(k, ...) k
    .xl_timer <- reactiveTimer(1000)   # the old code's tick, so the old observer runs here too (unused by the fix)
    r$empezar <- function(hace_s = 0) {
      .xl_job(list(job = list(is_alive = function() r$vivo, kill = function() NULL),
                   snap_path = tempfile(), out_path = tempfile()))
      .xl_start(Sys.time() - hace_s); .xl_warn_at(60); estado("building")
    }
    r$estado <- estado; r$archivo <- .xl_file
    eval(POLL)
  }
  testServer(srv, { pasos(session, r) })
  r
}

cat("A. idle\n")
rA <- correr(function(session, r) {
  session$flushReact(); r$al_inicio <- r$corridas
  session$elapse(60000); session$flushReact()
})
ok(sprintf("A1 idle for 60 s, the observer does not run again (runs: %d at start, %d after)", rA$al_inicio, rA$corridas),
   rA$al_inicio >= 1L && rA$corridas == rA$al_inicio)

cat("\nB-C. a job that runs, then ends\n")
rB <- correr(function(session, r) {
  session$flushReact(); r$empezar(); session$flushReact(); r$c0 <- r$corridas
  for (i in 1:5) { session$elapse(1000); session$flushReact() }
  r$c5 <- r$corridas; r$estado_vivo <- isolate(r$estado())
  r$vivo <- FALSE; session$elapse(1000); session$flushReact()
  r$estado_fin <- isolate(r$estado()); r$archivo_fin <- isolate(r$archivo()); r$c_fin <- r$corridas
  session$elapse(60000); session$flushReact()
})
ok(sprintf("B1 with a job running it polls about once a second (%d runs in 5 s)", rB$c5 - rB$c0),
   (rB$c5 - rB$c0) >= 4L && (rB$c5 - rB$c0) <= 6L && identical(rB$estado_vivo, "building"))
ok("C1 when the job ends: state 'done', the file kept, the 'Excel listo' notification",
   identical(rB$estado_fin, "done") && !is.null(rB$archivo_fin) && length(rB$avisos) == 1L && grepl("Excel listo", rB$avisos[1]))
ok(sprintf("C2 afterwards it is quiet again (%d runs at the end, %d a minute later)", rB$c_fin, rB$corridas),
   rB$corridas - rB$c_fin <= 1L)

cat("\nD. the 60 s warning\n")
rD <- correr(function(session, r) {
  # elapsed is read from the real clock (Sys.time()), not the mock one: the
  # job is started 61 s in the past
  session$flushReact(); r$empezar(hace_s = 61); session$flushReact()
  for (i in 1:2) { session$elapse(1000); session$flushReact() }
  r$estado_d <- isolate(r$estado()); r$c0 <- r$corridas
  for (i in 1:3) { session$elapse(1000); session$flushReact() }
})
ok("D1 a job past 60 s still gets its warning dialog (state warn60)", rD$modales == 1L && identical(rD$estado_d, "warn60"))
ok(sprintf("D2 and in warn60 it keeps polling (%d runs in 3 s)", rD$corridas - rD$c0), (rD$corridas - rD$c0) >= 2L)

cat("\nE. no timer left\n")
txt <- paste(readLines(src, warn = FALSE), collapse = "\n")
ok("E1 R/cashflow_export_module.R has no reactiveTimer (a timer read first wakes the observer every second)",
   !grepl("reactiveTimer(", txt, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
