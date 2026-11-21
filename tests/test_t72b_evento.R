# =============================================================================
# tests/test_t72b_evento.R
# T72 fase B, commit 1: the log is born in the write layer (R/persistence.R).
#   modificar_tabla(..., evento =) / modificar_tabla_completa(..., evento =)
#   .s3_write(..., evento =), evento_sistema(origen, accion)
# On the in-memory S3 (tests/helpers/s3_memoria.R); log_action() is a stub
# that keeps what it was given. The integrador's conditions (requisiciones/
# bitacora.md, 2026-11-21) are the checks:
#   1  with an event: ONE entry, after the write, with who/what, the table's
#      rows before/after and what changed by id (nuevas/cambiadas/quitadas);
#   2  nothing secret: a password/hash/token column is only "(cambió)";
#      usuarios / pending_invites / erp_connections give counts only;
#   3  what is saved is identical with and without an event;
#   4  a refused write (S3 fails, the CM09 net, the T69b gate) logs no event;
#   5  without an event: one "sin_evento" entry whose origin is the nearest
#      app functions BY NAME -- never an argument's value;
#   6  not logged: app_audit*, sync_versions, *_backup_*, preferencias.rds;
#   7  no log_action() (the emails job): the write goes on;
#   8  no recursion: a log_action() that writes logs nothing more;
#   9  evento_sistema(); 10  the difference of 50 000 rows takes < 200 ms;
#  11  the integrador's fixes: (A) a list column with 0 ids in common does
#      not lose the difference; (B) contrase/credencial/llave/private/passw
#      are secret; (C) the two copies of .t72_diferencia stay identical.
# Run from project root:  Rscript tests/test_t72b_evento.R
# Contraprueba: on e554ec3 there is no `evento` argument: 1-3, 5, 6 fail.
# =============================================================================
cat("=== test_t72b_evento ===\n\n")
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
L <- new.env(); L$x <- list()
log_action <- function(user, module, action, description, target_id = NA_character_, s3_key = NA_character_,
                       metadata = NULL, client_id = NULL, viewer_home_client_id = NULL) {
  L$x[[length(L$x) + 1L]] <- list(user = user, module = module, action = action, description = description,
                                  target_id = target_id, metadata = metadata, client_id = client_id,
                                  home = viewer_home_client_id)
  invisible(TRUE)
}
limpiar <- function() L$x <- list()
quieto <- function(expr) suppressMessages(suppressWarnings(expr))
CID <- "acme"
esquema <- function() tibble(id = character(), nombre = character(), monto = numeric(), password_hash = character(),
                             token = character())
tabla <- function(n = 3) tibble(id = paste0("r", seq_len(n)), nombre = paste("fila", seq_len(n)), monto = seq_len(n) * 10,
                                password_hash = paste0("HASH-DE-MENTIRA-", seq_len(n)), token = paste0("TOKEN-DE-MENTIRA-", seq_len(n)))
EV <- list(usuario = "ana", modulo = "pruebas", accion = "editar", descripcion = "Editó una fila", home = "hogar")
sembrar <- function(key, df) { s3_memoria_reset(); assign(paste0(CID, "/", key), df, envir = .mem); limpiar() }
cambio <- function(d) { d$monto[d$id == "r2"] <- 99; d$password_hash[d$id == "r2"] <- "HASH-NUEVO-SECRETO"
                        d$token[d$id == "r2"] <- "TOKEN-NUEVO-SECRETO"
                        bind_rows(d, tibble(id = "r9", nombre = "nueva", monto = 1, password_hash = "HASH-NUEVA-FILA", token = "TOKEN-NUEVA-FILA")) }
texto <- function(x) paste(capture.output(str(x, vec.len = 100, nchar.max = 1000)), collapse = "\n")

# ── 1. With an event ───────────────────────────────────────────────────────
sembrar("t72_prueba.rds", tabla())
quieto(modificar_tabla("t72_prueba.rds", CID, esquema, cambio, evento = EV))
e <- L$x
ok("1 one entry, with the event's user, module, action and the client / home",
   length(e) == 1L && e[[1]]$user == "ana" && e[[1]]$module == "pruebas" && e[[1]]$action == "editar" &&
     e[[1]]$client_id == CID && e[[1]]$home == "hogar")
ok("1 the description keeps the event's and adds the table and its rows (3 -> 4)",
   length(e) == 1L && grepl("Editó una fila", e[[1]]$description, fixed = TRUE) &&
     grepl("tabla=t72_prueba.rds filas 3 -> 4", e[[1]]$description, fixed = TRUE))
d <- if (length(e)) e[[1]]$metadata$diferencia else NULL
ok("1 what changed: 1 new (r9), 1 changed (r2: monto 20 -> 99), 0 removed",
   !is.null(d) && identical(unname(d$n), c(1L, 1L, 0L)) && identical(d$cambiadas$r2$monto, list(antes = 20, despues = 99)) &&
     identical(d$nuevas[[1]]$id, "r9"))

# ── 2. Nothing secret ──────────────────────────────────────────────────────
t2 <- texto(e)
ok("2 a hash or a token never appears in the entry -- only \"(cambió)\"",
   !grepl("HASH-", t2) && !grepl("TOKEN-", t2) && identical(d$cambiadas$r2$password_hash$despues, "(cambió)") &&
     identical(d$nuevas[[1]]$token, "(cambió)"))
for (k in c("usuarios.rds", "pending_invites.rds", "erp_connections.rds")) {
  sembrar(k, tabla())
  quieto(modificar_tabla(k, CID, esquema, cambio, evento = EV))
  ok(sprintf("2 %s: counts only (rows 3 -> 4), no difference", k),
     length(L$x) == 1L && is.null(L$x[[1]]$metadata$diferencia) && grepl("filas 3 -> 4", L$x[[1]]$description, fixed = TRUE) &&
       !grepl("HASH-|TOKEN-", texto(L$x)))
}

# ── 3. What is saved does not change ───────────────────────────────────────
sembrar("t72_prueba.rds", tabla()); quieto(modificar_tabla("t72_prueba.rds", CID, esquema, cambio))
sin <- get(paste0(CID, "/t72_prueba.rds"), envir = .mem)
sembrar("t72_prueba.rds", tabla()); quieto(modificar_tabla("t72_prueba.rds", CID, esquema, cambio, evento = EV))
con <- get(paste0(CID, "/t72_prueba.rds"), envir = .mem)
ok("3 the object saved is identical with and without an event", identical(sin, con))
sembrar("t72_prueba.rds", tabla()); quieto(modificar_tabla_completa("t72_prueba.rds", CID, esquema, cambio, evento = EV))
ok("3 modificar_tabla_completa() logs its event too, with the difference",
   length(L$x) == 1L && L$x[[1]]$action == "editar" && identical(unname(L$x[[1]]$metadata$diferencia$n), c(1L, 1L, 0L)))

# ── 4. A refused write logs no event ───────────────────────────────────────
sembrar("t72_prueba.rds", tabla())
assign(".mem_falla_escritura", paste0(CID, "/t72_prueba.rds"), envir = globalenv())
r <- tryCatch({ quieto(modificar_tabla("t72_prueba.rds", CID, esquema, cambio, evento = EV)); "escribió" }, error = function(e) "falló")
assign(".mem_falla_escritura", character(0), envir = globalenv())
ok("4 S3 refuses the upload: the write fails and no event is logged",
   identical(r, "falló") && !any(vapply(L$x, function(x) identical(x$action, "editar"), TRUE)))
sembrar("t72_prueba.rds", tabla(10))
r <- tryCatch({ quieto(modificar_tabla("t72_prueba.rds", CID, esquema, function(d) d[1:2, ], evento = EV)); "escribió" },
              error = function(e) "falló")
ok("4 the CM09 net refuses a shrink (10 -> 2): no event", identical(r, "falló") && !length(L$x))
s <- shiny::MockShinySession$new()
s$userData$hd_escritura_permitida <- function(key) FALSE
sembrar("t72_prueba.rds", tabla())
r <- tryCatch({ quieto(shiny::withReactiveDomain(s, modificar_tabla("t72_prueba.rds", CID, esquema, cambio, evento = EV))); "escribió" },
              error = function(e) "falló")
ok("4 the T69b gate refuses: no success event (only the gate's own refusal trail)",
   identical(r, "falló") && !any(vapply(L$x, function(x) identical(x$action, "editar"), TRUE)))

# ── 5. Without an event ────────────────────────────────────────────────────
guardar_de_prueba <- function(secreto) modificar_tabla("t72_prueba.rds", CID, esquema, function(d) { d$nombre[1] <- secreto; d })
sembrar("t72_prueba.rds", tabla())
quieto(guardar_de_prueba("VALOR-SECRETO-ARGUMENTO"))
ok("5 one \"sin_evento\" entry, user sistema, module registro, with the table and its rows",
   length(L$x) == 1L && L$x[[1]]$action == "sin_evento" && L$x[[1]]$user == "sistema" && L$x[[1]]$module == "registro" &&
     grepl("tabla=t72_prueba.rds filas 3 -> 3", L$x[[1]]$description, fixed = TRUE))
ok("5 its origin names the caller (guardar_de_prueba), never an argument's value",
   length(L$x) == 1L && grepl("guardar_de_prueba", L$x[[1]]$metadata$origen, fixed = TRUE) &&
     !grepl("VALOR-SECRETO", texto(L$x)) && nchar(L$x[[1]]$metadata$origen) <= 160L)
ok("5 no difference is computed without an event (counts only)", length(L$x) == 1L && is.null(L$x[[1]]$metadata$diferencia))

# ── 6. Not logged ──────────────────────────────────────────────────────────
for (k in c("app_audit.rds", "app_audit_pendiente_20261121.rds", "sync_versions.rds", "pasivos_backup_20261121.rds", "avisos/preferencias.rds")) {
  s3_memoria_reset(); limpiar()
  quieto(.s3_write(tabla(), k, client_id = CID))
  ok(sprintf("6 %s: written, no entry", k), exists(paste0(CID, "/", k), envir = .mem) && !length(L$x))
}

# ── 7. No log_action() ─────────────────────────────────────────────────────
guardado <- log_action; rm(log_action)
sembrar("t72_prueba.rds", tabla())
r <- tryCatch({ quieto(modificar_tabla("t72_prueba.rds", CID, esquema, cambio, evento = EV)); "ok" }, error = function(e) conditionMessage(e))
log_action <- guardado
ok("7 with no log_action() (the emails job) the write goes on", identical(r, "ok") && nrow(get(paste0(CID, "/t72_prueba.rds"), envir = .mem)) == 4L)

# ── 8. No recursion ────────────────────────────────────────────────────────
log_action <- function(...) { L$x[[length(L$x) + 1L]] <- list(...); .s3_write(tabla(), "t72_otra.rds", client_id = CID); invisible(TRUE) }
sembrar("t72_prueba.rds", tabla())
quieto(modificar_tabla("t72_prueba.rds", CID, esquema, cambio, evento = EV))
log_action <- guardado
ok("8 a log_action() that itself writes: that write logs nothing (one entry in all)",
   length(L$x) == 1L && exists(paste0(CID, "/t72_otra.rds"), envir = .mem))

# ── 9. evento_sistema() ────────────────────────────────────────────────────
es <- evento_sistema("banxico", "fx_fix", "FIX del día")
ok("9 evento_sistema(): user sistema:<origen>, module the origen",
   identical(es$usuario, "sistema:banxico") && identical(es$modulo, "banxico") && identical(es$accion, "fx_fix"))

# ── 10. Performance ────────────────────────────────────────────────────────
dif <- NULL
for (e1 in as.list(body(modificar_tabla)))
  if (is.call(e1) && identical(e1[[1]], as.name("<-")) && identical(e1[[2]], as.name(".t72_diferencia"))) dif <- eval(e1[[3]])
N <- 50000L
grande <- tibble(id = paste0("m", seq_len(N)), fecha = as.Date("2026-01-01") + seq_len(N) %% 300, importe = seq_len(N) / 7,
                 concepto = paste("Movimiento", seq_len(N)), banco = rep(c("BBVA", "Banorte", "BanBajio"), length.out = N),
                 conciliado = seq_len(N) %% 2 == 0, creado = as.POSIXct("2026-01-01", tz = "UTC") + seq_len(N))
otra <- grande; idx <- sample.int(N, 500L); otra$importe[idx] <- otra$importe[idx] + 1
otra <- bind_rows(otra[-(1:10), ], tibble(id = "nuevo", fecha = as.Date("2026-11-21"), importe = 1, concepto = "x", banco = "BBVA",
                                          conciliado = FALSE, creado = as.POSIXct("2026-11-21", tz = "UTC")))
invisible(dif("bancos_movimientos.rds", grande, otra))   # warm-up
ms <- min(vapply(1:3, function(i) system.time(dd <<- dif("bancos_movimientos.rds", grande, otra))[["elapsed"]], 1)) * 1000
cat(sprintf("   (50 000 rows: %.0f ms)\n", ms))
ok(sprintf("10 the difference of 50 000 rows (500 changed, 10 removed, 1 new) takes < 200 ms (%.0f ms)", ms),
   !is.null(dif) && ms < 200 && identical(unname(dd$n), c(1L, 500L, 10L)))

# ── 11. The integrador's fixes (A, B, C) ───────────────────────────────────
dif_de <- function(f) { d <- NULL
  for (e1 in as.list(body(f)))
    if (is.call(e1) && identical(e1[[1]], as.name("<-")) && identical(e1[[2]], as.name(".t72_diferencia"))) d <- e1[[3]]
  d }
d_mt <- dif_de(modificar_tabla); d_mtc <- dif_de(modificar_tabla_completa)
ok("11C the two copies of .t72_diferencia (modificar_tabla / _completa) are identical",
   !is.null(d_mt) && !is.null(d_mtc) && identical(deparse(d_mt), deparse(d_mtc)))
# A: a list column with NO ids in common -- mapply() gave list() and !list() threw: the whole difference was lost.
la <- tibble(id = c("a1", "a2"), datos = list(list(x = 1), list(x = 2)))
lb <- tibble(id = "b1", datos = list(list(x = 3)))
d0 <- tryCatch(dif("t72_prueba.rds", la, lb), error = function(e) NULL)
ok("11A list column, 0 rows in common: the difference is there (1 new, 0 changed, 2 removed)",
   !is.null(d0) && identical(unname(d0$n), c(1L, 0L, 2L)))
lc <- tibble(id = c("a1", "a2"), datos = list(list(x = 1), list(x = 20)))
d1 <- tryCatch(dif("t72_prueba.rds", la, lc), error = function(e) NULL)
ok("11A list column with rows in common: only the row that changed counts",
   !is.null(d1) && identical(unname(d1$n), c(0L, 1L, 0L)) && identical(names(d1$cambiadas), "a2"))
# B: more names that look like a secret.
sa <- tibble(id = "s1", contrasena = "C-VIEJA", credencial_sap = "CR-VIEJA", llave_api = "LL-VIEJA",
             private_key = "PK-VIEJA", passwd = "PW-VIEJA", nombre = "x")
sb <- sa; for (cl in setdiff(names(sb), c("id", "nombre"))) sb[[cl]] <- paste0(sb[[cl]], "-NUEVA-SECRETA")
sb <- bind_rows(sb, tibble(id = "s2", contrasena = "C-FILA-NUEVA", credencial_sap = "CR-FILA-NUEVA", llave_api = "LL-FILA-NUEVA",
                           private_key = "PK-FILA-NUEVA", passwd = "PW-FILA-NUEVA", nombre = "y"))
ds <- tryCatch(dif("t72_prueba.rds", sa, sb), error = function(e) NULL)
ok("11B contrase / credencial / llave / private / passw columns are only \"(cambió)\", never copied",
   !is.null(ds) && !grepl("VIEJA|NUEVA", texto(ds)) && length(ds$cambiadas$s1) == 5L)

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
