# =============================================================================
# tests/test_admin_invitaciones_hd.R
# Invitaciones › Invitaciones pendientes on the table engine (T28, class D):
#   1. the rows: pending only; a client sees only its own client's (a row
#      without client_id goes to nobody), staff sees all; client and tier by
#      their names; id = the token;
#   2. on the module: a client's Dev never receives another client's emails;
#      ⊘ names the invite by its token, also when another session invited
#      someone ahead of it after drawing;
#   3. without the role (Admin, Finanzas...): no rows, a ⊘ sent by hand names
#      nothing (integrator's condition c);
#   4. start-up: no file is 0 rows; an unreadable one is the table's notice.
# The handler that revokes (with its tenant check) is exercised from
# tiersServer's own source in test_admin_primer_clic_hd.R.
#
# Run from project root:  Rscript tests/test_admin_invitaciones_hd.R
# =============================================================================
cat("=== test_admin_invitaciones_hd ===\n\n")
suppressPackageStartupMessages(library(shiny))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".invitaciones_filas", "tiers_invitaciones_ui", "tiers_invitaciones_server"))
    eval(e, envir = globalenv())

KEY <- "hd-admin/pending_invites.rds"
inv <- function(token, email, cid, tier = "finance", status = "pending")
  data.frame(token = token, email = email, display_name = toupper(substr(email, 1, 3)), client_id = cid, tier = tier,
             invited_by = "x", created_at = "2026-11-06", expires_at = "2026-11-13 10:00:00", accepted_at = NA_character_,
             status = status, stringsAsFactors = FALSE)
INVS <- rbind(inv("t-a1", "uno@acme.mx", "acme"), inv("t-b1", "x@beta.mx", "beta", "dev"),
              inv("t-a2", "dos@acme.mx", "acme", "analysis"), inv("t-na", "nadie@x.mx", NA_character_),
              inv("t-a0", "vieja@acme.mx", "acme", status = "revoked"))
REG <- data.frame(client_id = c("acme", "beta"), display_name = c("Acme Logística", "Beta Transportes"),
                  stringsAsFactors = FALSE)
sembrar <- function(i = INVS) {
  s3_memoria_reset()
  if (!is.null(i)) assign(KEY, i, envir = .mem)
  assign("hd-admin/client_registry.rds", REG, envir = .mem)
}
COLS <- c("id", "email", "nombre", "cliente", "nivel", "expira")

# ── 1. The rows ──────────────────────────────────────────────────────────────
cli <- .invitaciones_filas(INVS, REG, staff = FALSE, own_client = "acme")
stf <- .invitaciones_filas(INVS, REG, staff = TRUE, own_client = "hd-admin")
ok("un cliente ve solo las pendientes de su empresa", identical(cli$id, c("t-a1", "t-a2")))
ok("staff ve todas las pendientes (sin revocadas)", identical(stf$id, c("t-a1", "t-b1", "t-a2", "t-na")))
ok("una invitación sin cliente no se le muestra a ningún cliente", !"t-na" %in% cli$id)
ok("el cliente con su nombre, el nivel con su nombre y la fecha de expiración",
   identical(stf$cliente[1:2], c("Acme Logística", "Beta Transportes")) &&
     identical(cli$nivel, c(nivel_nombre("finance"), nivel_nombre("analysis"))) &&
     identical(cli$expira[1], as.Date("2026-11-13")))
ok("sin registro de clientes, el cliente sale con su id (no falla)",
   identical(.invitaciones_filas(INVS, NULL, TRUE, "x")$cliente[1], "acme"))
for (caso in list(list("NULL", NULL), list("vacías", INVS[0, ]), list("solo revocadas", INVS[5, ])))
  ok(sprintf("invitaciones %s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .invitaciones_filas(caso[[2]], REG, TRUE, "x"); is.data.frame(v) && !nrow(v) && identical(names(v), COLS) })

# ── 2-4. On the module ───────────────────────────────────────────────────────
correr <- function(permitido, staff, own, pasos) {
  avisos <- character(0); caja <- new.env(); caja$filas <- "sin leer"; caja$revocar <- list()
  error <- tryCatch(withCallingHandlers(
    testServer(tiers_invitaciones_server,
               args = list(permitido = reactive(permitido), staff = reactive(staff), own_client = reactive(own),
                           refresh = reactiveVal(0L)), {
      observe({ r <- session$returned$revocar(); if (!is.null(r)) caja$revocar <- c(caja$revocar, list(r)) })
      session$flushReact()
      caja$filas <- tryCatch(session$returned$datos(), error = function(e) paste("error:", conditionMessage(e)))
      pasos(session)
    }),
    message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") }),
    error = function(e) conditionMessage(e))
  list(error = error, filas = caja$filas, revocar = vapply(caja$revocar, `[[`, "", "id"),
       fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}
revocar <- function(token, nonce = 1) function(session) {
  session$setInputs(`tabla-accion` = list(accion = "revocar", ids = token, nonce = nonce))
  session$flushReact()
}

sembrar()
r <- correr(TRUE, FALSE, "acme", function(session) {
  assign(KEY, rbind(inv("t-a9", "nueva@acme.mx", "acme"), get(KEY, envir = .mem)), envir = .mem)
  revocar("t-a2")(session)
})
ok("el Dev de acme recibe solo las de acme: ningún correo de beta llega al navegador",
   is.null(r$error) && identical(r$filas$id, c("t-a1", "t-a2")) && !any(grepl("beta", r$filas$email)))
ok("⊘ en t-a2 nombra t-a2, aunque otra sesión invitó a alguien antes", identical(r$revocar, "t-a2"))

sembrar()
r <- correr(FALSE, FALSE, "acme", revocar("t-a1"))
ok("sin el rol (Admin, Finanzas…): la tabla no recibe filas", is.null(r$error) && is.null(r$filas))
ok("sin el rol: un ⊘ mandado a mano no nombra ninguna invitación", !length(r$revocar))

sembrar(NULL)
r <- correr(TRUE, TRUE, "hd-admin", function(session) session$flushReact())
ok("arranque sin archivo de invitaciones: 0 filas y ninguna falla del motor",
   is.null(r$error) && is.data.frame(r$filas) && !nrow(r$filas) && !length(r$fallas))

sembrar()
.mem_falla <- KEY
r <- correr(TRUE, TRUE, "hd-admin", function(session) session$flushReact())
ok("lectura ilegible: la sesión sigue y la tabla avisa la falla (no 'sin invitaciones')",
   is.null(r$error) && length(r$fallas) > 0L)

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
