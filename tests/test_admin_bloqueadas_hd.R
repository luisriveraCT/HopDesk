# =============================================================================
# tests/test_admin_bloqueadas_hd.R
# Seguridad › Cuentas bloqueadas on the table engine (T28, class D):
#   1. the rows: one per locked account, id = the lower-case username;
#      no list or an empty one keeps the columns;
#   2. 🔓 unlocks the account named by the row's id, on S3's current list:
#      another session's lock, added since the table was drawn, stays;
#   3. only Principal: without it the table gets no rows, and an unlock sent
#      by hand does nothing (integrator's condition c, T28);
#   4. start-up: no lock file is 0 rows; an unreadable one is the table's
#      notice (never "no locks", P7), the session goes on and nothing is written.
#
# Run from project root:  Rscript tests/test_admin_bloqueadas_hd.R
# =============================================================================
cat("=== test_admin_bloqueadas_hd ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".bloqueadas_filas", "tiers_bloqueadas_ui", "tiers_bloqueadas_server"))
    eval(e, envir = globalenv())
registro <- character(0)
log_action <- function(user, module, action, ...) registro <<- c(registro, action)
showNotification <- function(ui, ...) { registro <<- c(registro, paste("aviso:", ui)); invisible("id") }

KEY <- "hd-admin/emergency_lock.rds"
lock <- function(u, por = "mouse", motivo = "x", cuando = "2026-11-07 18:30:00")
  data.frame(username = u, locked_at = as.POSIXct(cuando), locked_by = por, reason = motivo, stringsAsFactors = FALSE)
LOCKS <- rbind(lock("Beto", motivo = "Credenciales"), lock("exempleado", motivo = "Baja"))
sembrar <- function(l = LOCKS) {
  s3_memoria_reset()
  rm(list = ls(.emergency_lock_cache), envir = .emergency_lock_cache)   # its 60 s cache, per test
  if (!is.null(l)) assign(KEY, l, envir = .mem)
}
COLS <- c("id", "usuario", "bloqueado_el", "por", "motivo")

# ── 1. The rows ──────────────────────────────────────────────────────────────
f <- .bloqueadas_filas(LOCKS)
ok("una fila por cuenta; el id es el usuario en minúsculas", identical(f$id, c("beto", "exempleado")))
ok("se muestra el usuario como está, con su fecha, quién y el motivo",
   identical(f$usuario, c("Beto", "exempleado")) && inherits(f$bloqueado_el, "POSIXct") &&
     identical(f$motivo, c("Credenciales", "Baja")))
ok("el mismo usuario escrito dos veces es una sola fila (el id no se repite)",
   identical(.bloqueadas_filas(rbind(LOCKS, lock("BETO")))$id, c("beto", "exempleado")))
for (caso in list(list("NULL", NULL), list("vacía", LOCKS[0, ])))
  ok(sprintf("lista %s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .bloqueadas_filas(caso[[2]]); is.data.frame(v) && !nrow(v) && identical(names(v), COLS) })

# ── 2-4. On the module ───────────────────────────────────────────────────────
correr <- function(principal, pasos) {
  avisos <- character(0); registro <<- character(0); caja <- new.env(); caja$filas <- "sin leer"
  shared <- list(current_user = function() "mouse", home_client_id = function() "hd-admin")
  error <- tryCatch(withCallingHandlers(
    testServer(tiers_bloqueadas_server,
               args = list(shared = shared, permitido = reactive(principal), refresh = reactiveVal(0L),
                           actividad = reactiveVal(0L)), {
      session$flushReact()
      caja$filas <- tryCatch(session$returned$datos(), error = function(e) paste("error:", conditionMessage(e)))
      pasos(session)
    }),
    message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") }),
    error = function(e) conditionMessage(e))
  list(error = error, filas = caja$filas, fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}
desbloquear <- function(id, nonce = 1) function(session)
  session$setInputs(`tabla-accion` = list(accion = "desbloquear", ids = id, nonce = nonce))
en_s3 <- function() tolower(get(KEY, envir = .mem)$username)

sembrar()
r <- correr(TRUE, function(session) {
  # Another session locks "caro" after this table was drawn.
  assign(KEY, rbind(get(KEY, envir = .mem), lock("caro")), envir = .mem)
  desbloquear("beto")(session)
})
ok("Principal ve las cuentas bloqueadas", is.null(r$error) && identical(r$filas$id, c("beto", "exempleado")))
ok("🔓 en beto: se quita solo beto", !"beto" %in% en_s3() && "exempleado" %in% en_s3())
ok("dos sesiones: el bloqueo que otra sesión agregó después (caro) sigue", "caro" %in% en_s3())
ok("se registra emergency_unlock y avisa", "emergency_unlock" %in% registro &&
     any(grepl("desbloqueada", registro)))

sembrar()
r <- correr(TRUE, desbloquear("nadie"))
ok("🔓 con un id que no está: no quita ningún bloqueo", setequal(en_s3(), c("beto", "exempleado")))

sembrar()
r <- correr(FALSE, desbloquear("beto"))
ok("sin ser Principal: la tabla no recibe filas", is.null(r$error) && is.null(r$filas))
ok("sin ser Principal: un 🔓 mandado a mano no desbloquea ni registra",
   "beto" %in% en_s3() && !"emergency_unlock" %in% registro)

sembrar(NULL)
r <- correr(TRUE, function(session) session$flushReact())
ok("arranque sin archivo de bloqueos: 0 filas y ninguna falla del motor",
   is.null(r$error) && is.data.frame(r$filas) && !nrow(r$filas) && !length(r$fallas))

sembrar()
.mem_falla <- KEY
escrituras0 <- .mem_escrituras
r <- correr(TRUE, function(session) session$flushReact())
ok("lectura ilegible: la sesión sigue y la tabla avisa la falla (no dice 'no hay bloqueos')",
   is.null(r$error) && length(r$fallas) > 0L)
ok("lectura ilegible: no se escribe nada", identical(.mem_escrituras, escrituras0))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
