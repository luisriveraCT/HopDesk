# =============================================================================
# tests/test_admin_accesos_hd.R
# Accesos de Salto › Accesos activos on the table engine (T28, class D):
#   1. the rows: active accesses only (not revoked, not expired), id = the
#      grant's id; Principal sees everyone's, other staff only its own; what
#      is left in words, from the dictionary;
#   2. on the module: Principal gets the Usuario column and ⊘; other staff
#      neither (the button is hidden and a ⊘ sent by hand names nothing);
#      ⊘ names the grant by id when another session granted one ahead;
#   3. not staff: no rows; start-up with no file: 0 rows; an unreadable
#      file: the table's notice (P7);
#   4. tiersServer's own observer, one click from start-up: ⊘ revokes that
#      grant on S3 (and nothing else), with its permission gate.
#
# Run from project root:  Rscript tests/test_admin_accesos_hd.R
# =============================================================================
cat("=== test_admin_accesos_hd ===\n\n")
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
      as.character(e[[2]]) %in% c(".accesos_quedan", ".accesos_filas", "tiers_accesos_ui", "tiers_accesos_server",
                                  "tiersServer"))
    eval(e, envir = globalenv())

KEY <- "hd-admin/hop_grants.rds"
AHORA <- Sys.time()   # the module's datos() compares with the real clock
acceso <- function(id, quien, cid, expira = NA, revocado = FALSE)
  data.frame(id = id, request_id = "r", grantee = quien, client_id = cid, granted_by = "mouse",
             granted_at = AHORA - 3600, expires_at = as.POSIXct(expira), revoked = revocado, stringsAsFactors = FALSE)
G <- rbind(acceso("g-1", "bunny", "acme", AHORA + 86400 * 2), acceso("g-2", "otro", "beta"),
           acceso("g-3", "bunny", "beta", AHORA + 1800), acceso("g-0", "bunny", "gamma", AHORA + 600, TRUE),
           acceso("g-x", "bunny", "acme", AHORA - 60))
REG <- data.frame(client_id = c("acme", "beta"), display_name = c("Acme", "Beta"), status = "active",
                  stringsAsFactors = FALSE)
sembrar <- function(g = G) {
  s3_memoria_reset()
  if (!is.null(g)) assign(KEY, g, envir = .mem)
  assign("hd-admin/client_registry.rds", REG, envir = .mem)
}

# ── 1. The rows ──────────────────────────────────────────────────────────────
p <- .accesos_filas(G, REG, principal = TRUE, me = "mouse", ahora = AHORA)
s <- .accesos_filas(G, REG, principal = FALSE, me = "bunny", ahora = AHORA)
ok("Principal ve todos los activos (sin revocados ni vencidos)", identical(p$id, c("g-1", "g-2", "g-3")))
ok("otro staff ve solo los suyos", identical(s$id, c("g-1", "g-3")))
ok("el cliente con su nombre", identical(p$cliente, c("Acme", "Beta", "Beta")))
ok("lo que queda, en palabras del diccionario: días, sin expiración, minutos",
   identical(p$quedan, c("2 días", "Sin expiración", "30 min")))
ok("horas y minutos", identical(.accesos_quedan(AHORA + 3600 * 5 + 60 * 7, AHORA), "5 h 07 min"))
ok("sin accesos (NULL o vacío): 0 filas",
   !nrow(.accesos_filas(NULL, REG, TRUE, "x")) && !nrow(.accesos_filas(G[0, ], REG, TRUE, "x")))

# ── 2-3. On the module ───────────────────────────────────────────────────────
correr <- function(staff, principal, puede, me, pasos) {
  avisos <- character(0); caja <- new.env(); caja$filas <- "sin leer"; caja$rev <- character(0); caja$ocultas <- NULL
  caja$columnas <- NULL
  error <- tryCatch(withCallingHandlers(testServer(tiers_accesos_server,
    args = list(permitido = reactive(staff), principal = reactive(principal), me = reactive(me),
                puede_revocar = reactive(puede), refresh = reactiveVal(0L)), {
      observe({ r <- session$returned$revocar(); if (!is.null(r)) caja$rev <- c(caja$rev, r$id) })
      session$flushReact()
      caja$filas <- tryCatch(session$returned$datos(), error = function(e) paste("error:", conditionMessage(e)))
      pasos(session)
    }), message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") }),
    error = function(e) conditionMessage(e))
  list(error = error, filas = caja$filas, rev = caja$rev, fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}
revocar <- function(id, nonce = 1) function(session) {
  session$setInputs(`tabla-accion` = list(accion = "revocar", ids = id, nonce = nonce)); session$flushReact()
}

sembrar()
r <- correr(TRUE, TRUE, TRUE, "mouse", function(session) {
  assign(KEY, rbind(acceso("g-9", "nuevo", "acme"), get(KEY, envir = .mem)), envir = .mem)
  revocar("g-2")(session)
})
ok("Principal: los activos de todos", is.null(r$error) && setequal(r$filas$id, c("g-1", "g-2", "g-3")))
ok("⊘ en g-2 lo nombra por id, aunque otra sesión concedió uno antes", identical(r$rev, "g-2"))

sembrar()
r <- correr(TRUE, FALSE, FALSE, "bunny", revocar("g-1"))
ok("otro staff: solo los suyos, y un ⊘ mandado a mano no nombra nada",
   is.null(r$error) && identical(r$filas$id, c("g-1", "g-3")) && !length(r$rev))

sembrar()
r <- correr(FALSE, FALSE, FALSE, "ana", function(s) s$flushReact())
ok("quien no es staff: la tabla no recibe filas", is.null(r$error) && is.null(r$filas))

sembrar(NULL)
r <- correr(TRUE, TRUE, TRUE, "mouse", function(s) s$flushReact())
ok("arranque sin archivo de accesos: 0 filas y ninguna falla del motor",
   is.null(r$error) && is.data.frame(r$filas) && !nrow(r$filas) && !length(r$fallas))

sembrar(); .mem_falla <- KEY
r <- correr(TRUE, TRUE, TRUE, "mouse", function(s) s$flushReact())
ok("accesos ilegibles: la sesión sigue y la tabla avisa la falla (no 'no hay accesos')",
   is.null(r$error) && length(r$fallas) > 0L)

# ── 4. tiersServer's own observer, one click from start-up ──────────────────
cableado <- list()
buscar <- function(x) {
  if (!is.call(x)) return(invisible())
  txt <- paste(deparse(x[1:min(2L, length(x))], width.cutoff = 500L), collapse = " ")
  if ((identical(x[[1]], as.name("<-")) && is.call(x[[3]]) && identical(x[[3]][[1]], as.name("tiers_accesos_server"))) ||
      (identical(x[[1]], as.name("observeEvent")) && grepl("^observeEvent\\(accesos_t\\$", txt))) {
    cableado[[length(cableado) + 1L]] <<- x; return(invisible())
  }
  xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) buscar(xs[[k]])
}
buscar(body(tiersServer))
ok("tiersServer crea la tabla de accesos y conecta su ⊘", length(cableado) == 2L)

clic <- function(id, permiso = TRUE) {
  sembrar()
  error <- tryCatch(suppressMessages(testServer(function(input, output, session) {
    is_principal <- reactive(TRUE)
    staff_perms <- reactive(list(can_manage_hopdesk_perms = permiso))
    hop_refresh <- reactiveVal(0L); activity_refresh <- reactiveVal(0L)
    shared <- list(current_user = function() "mouse", home_client_id = function() "hd-admin",
                   current_user_info = reactive(list(is_staff = TRUE)), hop_grants_db = reactiveVal(NULL))
    bump_sync_version <- function(...) invisible(NULL)
    showNotification <- function(...) invisible("id"); log_action <- function(...) invisible(NULL)
    for (x in cableado) eval(x)
    session$flushReact()
    session$setInputs(`accesos-tabla-accion` = list(accion = "revocar", ids = id, nonce = 1))
    session$flushReact()
  }, {})), error = function(e) conditionMessage(e))
  g <- get(KEY, envir = .mem)
  list(error = error, revocados = g$id[g$revoked %in% TRUE])
}
r <- clic("g-2")
ok("un solo clic en ⊘ (confirmado) desde el arranque revoca g-2 en S3, y nada más",
   is.null(r$error) && setequal(r$revocados, c("g-0", "g-2")))
r <- clic("g-2", permiso = FALSE)
ok("sin can_manage_hopdesk_perms, ⊘ no revoca nada", is.null(r$error) && identical(r$revocados, "g-0"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
