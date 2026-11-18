# =============================================================================
# tests/test_cm28_lupa_moves_rebase.R
# CM28 (2026-11-18). Vencidos and the lupa move and restore dates through
# handle_invoice_action() (R/search_module.R). Its saves went to
# .save_moves_deferred() WITHOUT `rebase`, so they wrote this session's whole
# copy of invoice_moves.rds: a date another session moved after this one
# loaded was erased (rule 15; the same cause as "se pierden las maniobras de
# tesorería", fixed for the Calendario on 2026-09-27).
#
# Two sessions, A and B, load the same moves; B moves T1 and saves; then A,
# still on its old copy, moves or restores from Vencidos. B's T1 must survive.
# Real code: handle_invoice_action(), .save_moves_deferred(), .moves_read_live()
# and R/persistence.R over the in-memory S3 (tests/helpers/s3_memoria.R).
#
# Run from project root:  Rscript tests/test_cm28_lupa_moves_rebase.R
# =============================================================================
cat("=== test_cm28_lupa_moves_rebase ===\n\n")
suppressPackageStartupMessages(library(shiny))
source("tests/helpers/s3_memoria.R")
.app_data_cache <- list()
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".cache_get", ".cache_set", ".cache_scoped_key"))
    eval(e, globalenv())
for (f in c("R/ledger_module.R", "R/search_module.R"))
  for (e in parse(f, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% c(".moves_read_live", ".save_moves_deferred", "handle_invoice_action"))
      eval(e, globalenv())

# What the handler calls around the save: no screen, no audit, no sync here.
showNotification  <- function(...) invisible(NULL)
tr                <- function(clave, ...) clave
log_action        <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"; MK <- paste0(CID, "/", S3_KEYS$moves)
mov <- function(docs, fecha, quien) .normalize(tibble::tibble(
  ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = docs,
  FechaVenc_Proyectada = as.Date(fecha), moved_by = quien, last_updated = Sys.time()), .schema_moves)
# A session: its own copy of moves_db, as the app's reactiveVal holds it.
sesion <- function(usuario, copia) {
  moves <- copia
  list(moves_db = function(v) if (missing(v)) moves else moves <<- v,
       current_user = function() usuario, effective_client_id = function() CID,
       home_client_id = function() CID)
}
fila <- function(doc) list(ledger = "AP", empresa = "NCS", moneda = "MXN", documento = doc,
                           source = "sap", inv_id = doc, parte = "PROVEEDOR", importe = 100,
                           fecha = "2026-11-20")
s3 <- function() get(MK, envir = .mem)
fecha_de <- function(doc) as.character(s3()$FechaVenc_Proyectada[s3()$Documento == doc])
correr <- function() for (i in 1:5) later::run_now()

# Both sessions start from S3 holding A1..A4 (enough rows that the CM09 net
# never refuses the restore below as a shrink).
inicio <- function() {
  s3_memoria_reset()
  assign(MK, mov(c("A1", "A2", "A3", "A4"), "2026-11-25", "inicio"), envir = .mem)
  list(A = sesion("a", s3()), B = sesion("b", s3()))
}

# 1. Move (Vencidos / lupa "Mover")
s <- inicio()
handle_invoice_action(list(action = "move", move_to = "2026-12-01", rows = list(fila("A2"))), s$B)
correr()
ok("setup: B's move of A2 reached S3", identical(fecha_de("A2"), "2026-12-01"))
handle_invoice_action(list(action = "move", move_to = "2026-12-15", rows = list(fila("A1"))), s$A)
correr()
ok("move: A's move of A1 is saved", identical(fecha_de("A1"), "2026-12-15"))
ok("move: B's move of A2 survives A's save (A never saw it)", identical(fecha_de("A2"), "2026-12-01"))

# 2. Restore (Vencidos / lupa "Restaurar")
s <- inicio()
handle_invoice_action(list(action = "move", move_to = "2026-12-01", rows = list(fila("A2"))), s$B)
correr()
handle_invoice_action(list(action = "restore", rows = list(fila("A1"))), s$A)
correr()
ok("restore: only A1's move is gone (A2, A3, A4 stay, one row each)",
   identical(sort(s3()$Documento), c("A2", "A3", "A4")))
ok("restore: B's move of A2 survives A's restore", identical(fecha_de("A2"), "2026-12-01"))

# 3. A row another session added meanwhile is not removed either
s <- inicio()
assign(MK, dplyr::bind_rows(s3(), mov("T9", "2026-12-03", "tesoreria")), envir = .mem)
handle_invoice_action(list(action = "restore", rows = list(fila("A1"))), s$A)
correr()
ok("restore: a move tesorería added after A loaded (T9) survives", identical(fecha_de("T9"), "2026-12-03"))

# 4. A failed live read writes nothing (the retry, then the error on screen)
s <- inicio(); .mem_escrituras <- 0L; .mem_falla <- MK
handle_invoice_action(list(action = "move", move_to = "2026-12-15", rows = list(fila("A1"))), s$A)
for (i in 1:3) { later::run_now(); Sys.sleep(1.1); later::run_now(); Sys.sleep(1.1) }
later::run_now()
.mem_falla <- character()
ok("a failed live read: nothing is written (no whole-copy fallback)", .mem_escrituras == 0L)

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
