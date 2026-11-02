# =============================================================================
# tests/test_bancos_rebase.R
# CM09 / CM10-C5 (2026-11-01): Bancos saved its session copy of
# bancos_confirmados (six places) and of bancos_movimientos in the same
# observers, dropping what another session wrote meanwhile (an Agenda
# confirmation, an undo, a new movement). The saves now pass `base` (the
# session copy before the change) and only the rows the action added or
# changed are applied on S3's table as it is now (rebase_por_id).
#
# Run from project root:  Rscript tests/test_bancos_rebase.R
# =============================================================================
cat("=== test_bancos_rebase ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressMessages(source("R/bancos_persistence.R"))
bump_sync_version <- function(...) invisible(TRUE)

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
q <- function(expr) suppressMessages(expr)

# 1. rebase_por_id
t <- function(ids, v) data.frame(id = ids, v = v, stringsAsFactors = FALSE)
base   <- t(c("1", "2", "3"), c("a", "b", "c"))
nuevo  <- t(c("1", "2", "3", "4"), c("a", "B-mio", "c", "d-mio"))       # changed 2, added 4
fresco <- t(c("1", "2", "3", "5"), c("a", "b", "C-otro", "e-otro"))     # someone changed 3, added 5
r <- rebase_por_id(nuevo, base, fresco, "id")
ok("rebase: S3's row order is kept, my changed row in place and new rows last",
   identical(r$id, c("1", "2", "3", "5", "4")))
r <- r[order(r$id), ]
ok("rebase: my changed row wins", r$v[r$id == "2"] == "B-mio")
ok("rebase: my new row is added", r$v[r$id == "4"] == "d-mio")
ok("rebase: a row I did not touch keeps the other session's change", r$v[r$id == "3"] == "C-otro")
ok("rebase: the other session's new row survives", r$v[r$id == "5"] == "e-otro")
r2 <- rebase_por_id(t(c("1", "3"), c("a", "c")), base, fresco, "id")
ok("rebase: a row I removed stays removed", !"2" %in% r2$id && "5" %in% r2$id)

# 2. save_bancos_confirmados with base
conf_fila <- function(id) {
  c0 <- .schema_bancos_confirmados(); c0[1, "confirmacion_id"] <- id; c0[1, "eliminado"] <- FALSE; c0
}
CK <- "acme/bancos_confirmados.rds"
s3_memoria_reset()
assign(CK, dplyr::bind_rows(conf_fila("c1"), conf_fila("c2"), conf_fila("c3")), envir = .mem)  # c3: another session
base_c  <- dplyr::bind_rows(conf_fila("c1"), conf_fila("c2"))                                    # this session's copy
nuevo_c <- base_c; nuevo_c$eliminado[nuevo_c$confirmacion_id == "c1"] <- TRUE                   # "Eliminar confirmación"
q(save_bancos_confirmados(nuevo_c, client_id = "acme", base = base_c))
s3 <- get(CK, envir = .mem)
ok("confirmations: my change (c1 deleted) is applied", isTRUE(s3$eliminado[s3$confirmacion_id == "c1"]))
ok("confirmations: the other session's confirmation c3 survives", "c3" %in% s3$confirmacion_id && nrow(s3) == 3L)

s3_memoria_reset()
assign(CK, dplyr::bind_rows(conf_fila("c1"), conf_fila("c2"), conf_fila("c3")), envir = .mem)
r <- tryCatch(q(save_bancos_confirmados(nuevo_c, client_id = "acme")), error = function(e) e)
ok("without base, the stale whole-copy save is refused by the net (c3 would be lost)",
   inherits(r, "error") && nrow(get(CK, envir = .mem)) == 3L)

s3_memoria_reset()
assign(CK, dplyr::bind_rows(conf_fila("c1"), conf_fila("c2")), envir = .mem); .mem_falla <- CK
r <- tryCatch(q(save_bancos_confirmados(nuevo_c, client_id = "acme", base = base_c)), error = function(e) e)
ok("a failed read writes nothing", inherits(r, "error") && .mem_escrituras == 0L)

# 3. save_bancos_movimientos with base
mov <- function(id) { m <- .schema_bancos_movimientos(); m[1, "id"] <- id; m[1, "eliminado"] <- FALSE; m }
MK <- "acme/bancos_movimientos.rds"
s3_memoria_reset()
assign(MK, dplyr::bind_rows(mov("m1"), mov("m2"), mov("m3")), envir = .mem)                    # m3: imported meanwhile
base_m <- dplyr::bind_rows(mov("m1"), mov("m2")); nuevo_m <- base_m; nuevo_m$eliminado[1] <- TRUE
q(save_bancos_movimientos(nuevo_m, client_id = "acme", base = base_m))
s3m <- get(MK, envir = .mem)
ok("movements: my change applied and the imported m3 survives",
   isTRUE(s3m$eliminado[s3m$id == "m1"]) && "m3" %in% s3m$id)

# 3b. A column whose type changed in the session copy (ifelse() drops the
# POSIXct class) does not mark every row changed, and keeps its values
t0 <- as.POSIXct("2026-10-01 10:00:00", tz = "UTC")
mov_t <- function(id, notas = "") { m <- mov(id); m$eliminado_at <- t0; m$notas <- notas; m }
s3_memoria_reset()
assign(MK, dplyr::bind_rows(mov_t("m1"), mov_t("m2", "editada por otra sesion")), envir = .mem)
base_t  <- dplyr::bind_rows(mov_t("m1"), mov_t("m2"))
nuevo_t <- base_t
nuevo_t$eliminado    <- c(TRUE, FALSE)
nuevo_t$eliminado_at <- ifelse(nuevo_t$eliminado, as.numeric(Sys.time()), nuevo_t$eliminado_at)  # now numeric
q(save_bancos_movimientos(nuevo_t, client_id = "acme", base = base_t))
s3t <- get(MK, envir = .mem)
ok("type change: the other session's edit of m2 survives (m2 not counted as changed)",
   s3t$notas[s3t$id == "m2"] == "editada por otra sesion")
ok("type change: the times are kept as times, not blanked to NA",
   inherits(s3t$eliminado_at, "POSIXct") && !anyNA(s3t$eliminado_at) &&
   isTRUE(s3t$eliminado[s3t$id == "m1"]))

# 4. The six Bancos saves pass their base
src <- readLines("R/bancos_module.R", warn = FALSE)
saves <- grep("save_bancos_confirmados(", src, fixed = TRUE, value = TRUE)
ok("the five Bancos confirmation saves pass base = conf_base (the recovery is done inside modificar)",
   length(saves) == 5L && all(grepl("base = conf_base", saves, fixed = TRUE)))
ok("Bancos recovers a confirmation inside modificar_bancos_confirmados()",
   any(grepl("function(fresco) recover_confirmacion(fresco, conf_id", src, fixed = TRUE)))

# 4b. Two sessions recover the same confirmation: only one "recovered" row
s3_memoria_reset()
assign(CK, dplyr::bind_rows(conf_fila("c1"), conf_fila("c2")), envir = .mem)
rec <- function() q(modificar_bancos_confirmados("acme", function(f) recover_confirmacion(f, "c1", actor = "t")))
r1 <- tryCatch(rec(), error = function(e) e)
r2 <- tryCatch(rec(), error = function(e) e)                     # the other session, same click
s3 <- get(CK, envir = .mem)
ok("recover twice: the first one saves, the second sees it on S3 and refuses",
   !inherits(r1, "error") && inherits(r2, "error") && grepl("already recovered", conditionMessage(r2)))
ok("recover twice: exactly one 'recovered' row", sum(s3$action %in% "recovered") == 1L)
movs_saves <- unlist(lapply(c("R/bancos_module.R", "R/pasivos_module.R"), function(f)
  grep("save_bancos_movimientos(", readLines(f, warn = FALSE), fixed = TRUE, value = TRUE)))
ok("every bancos_movimientos save in Bancos and Pasivos passes base = movs_base",
   length(movs_saves) == 14L && all(grepl("base = movs_base", movs_saves, fixed = TRUE)))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
