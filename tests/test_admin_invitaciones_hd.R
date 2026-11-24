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
for (f in c("R/i18n.R", "R/tier_registry.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
for (e in parse("R/tiers_module.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".invitaciones_filas", "tiers_invitaciones_ui", "tiers_invitaciones_server"))
    eval(e, envir = globalenv())

KEY <- "hd-admin/pending_invites.rds"
# Valid for a week from now, as create_invite() writes it (T57: a fixed date
# expires, and the table hides the expired ones).
EXP <- format(Sys.time() + 7 * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
inv <- function(token, email, cid, tier = "finance", status = "pending", expires_at = EXP)
  data.frame(token = token, email = email, display_name = toupper(substr(email, 1, 3)), client_id = cid, tier = tier,
             invited_by = "x", created_at = "2026-11-06", expires_at = expires_at, accepted_at = NA_character_,
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
     identical(cli$expira[1], as.Date(substr(EXP, 1, 10))))
ok("un cliente sin cliente propio (NA o NULL) no ve ninguna, ni filas vacías",
   !nrow(.invitaciones_filas(INVS, REG, FALSE, NA_character_)) && !nrow(.invitaciones_filas(INVS, REG, FALSE, NULL)))
ok("sin registro de clientes, el cliente sale con su id (no falla)",
   identical(.invitaciones_filas(INVS, NULL, TRUE, "x")$cliente[1], "acme"))
for (caso in list(list("NULL", NULL), list("vacías", INVS[0, ]), list("solo revocadas", INVS[5, ])))
  ok(sprintf("invitaciones %s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .invitaciones_filas(caso[[2]], REG, TRUE, "x"); is.data.frame(v) && !nrow(v) && identical(names(v), COLS) })

# ── 1b. Expired invitations (T57) ───────────────────────────────────────────
# Luis's case: one that expired on Oct 30 and nobody opened -- still
# "pending" in the file -- showed in the table. The table now uses the
# link's own rule, invitacion_vencida() (R/persistence.R).
AHORA <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")
VEN <- rbind(inv("t-vieja", "vieja@acme.mx", "acme", expires_at = "2026-10-30T18:00:00Z"),
             inv("t-viva",  "viva@acme.mx",  "acme", expires_at = "2026-11-21T18:00:00Z"),
             inv("t-justo", "justo@acme.mx", "acme", expires_at = "2026-11-19T12:00:00Z"),
             inv("t-rara",  "rara@acme.mx",  "acme", expires_at = "pronto"),
             inv("t-na",    "na@acme.mx",    "acme", expires_at = NA_character_))
v <- .invitaciones_filas(VEN, REG, staff = TRUE, own_client = "hd-admin", ahora = AHORA)
ok("T57: an expired invitation (Oct 30, never opened) is not pending", !"t-vieja" %in% v$id)
ok("T57: a valid one is", "t-viva" %in% v$id)
ok("T57: the edge is the link's rule (expired only when now > expires_at)", "t-justo" %in% v$id &&
     !"t-justo" %in% .invitaciones_filas(VEN, REG, TRUE, "hd-admin", ahora = AHORA + 1)$id)
ok("T57: an unreadable or missing date still shows (to be revoked)", all(c("t-rara", "t-na") %in% v$id))
ok("T57: the same for a client (its own, unexpired)",
   identical(.invitaciones_filas(VEN, REG, FALSE, "acme", ahora = AHORA)$id, c("t-viva", "t-justo", "t-rara", "t-na")))
ok("T57: invitacion_vencida() is TRUE / FALSE / NA, vectorized",
   identical(invitacion_vencida(c("2026-10-30T18:00:00Z", "2026-11-21T18:00:00Z", "pronto", NA), AHORA),
             c(TRUE, FALSE, NA, NA)))
# The link (resolve_invite, R/persistence.R), the same rule. Before T57 an
# unreadable date made `if (NA)` throw inside it (app.R's tryCatch refused
# the link): now it refuses without an error and marks nothing.
sembrar(VEN)
ok("T57: resolve_invite, unreadable date: NULL, no error, not marked",
   { r <- tryCatch(suppressMessages(resolve_invite("t-rara", ahora = AHORA)), error = function(e) "error")
     is.null(r) && identical(get(KEY, envir = .mem)$status[get(KEY, envir = .mem)$token == "t-rara"], "pending") })
ok("T57: resolve_invite, expired: NULL and marked expired",
   is.null(suppressMessages(resolve_invite("t-vieja", ahora = AHORA))) &&
     identical(get(KEY, envir = .mem)$status[get(KEY, envir = .mem)$token == "t-vieja"], "expired"))
ok("T57: resolve_invite, valid: the invitation", identical(suppressMessages(resolve_invite("t-viva", ahora = AHORA))$email, "viva@acme.mx"))
# ── 1c. Deleting the expired ones (T57, Luis: "que se borre") ───────────────
PUR <- rbind(VEN[VEN$token %in% c("t-vieja", "t-viva", "t-rara", "t-na"), ],
             inv("t-marcada", "m@acme.mx", "acme", status = "expired", expires_at = "2026-10-01T00:00:00Z"),
             inv("t-acept",   "a@acme.mx", "acme", status = "accepted", expires_at = "2026-10-01T00:00:00Z"),
             inv("t-revoc",   "r@acme.mx", "acme", status = "revoked",  expires_at = "2026-10-01T00:00:00Z"),
             inv("t-beta-v",  "v@beta.mx", "beta", expires_at = "2026-10-30T18:00:00Z"))
quedan <- function() get(KEY, envir = .mem)$token
sembrar(PUR)
n <- purgar_invitaciones_vencidas("acme", ahora = AHORA)
ok("T57 purge, a client: its expired pending one and its marked 'expired' one leave",
   !any(c("t-vieja", "t-marcada") %in% quedan()) && identical(c(n), c(acme = 2L)))
ok("T57 purge, a client: never a valid one, an unreadable date, accepted or revoked",
   all(c("t-viva", "t-rara", "t-na", "t-acept", "t-revoc") %in% quedan()))
ok("T57 purge, a client: another client's expired one stays", "t-beta-v" %in% quedan())
ok("T57 purge: the rows that stay are unchanged",
   { a <- get(KEY, envir = .mem); b <- PUR[match(a$token, PUR$token), ]; cc <- c("token", "email", "status", "expires_at")
     identical(lapply(a[, cc], as.character), lapply(b[, cc], as.character)) })
n <- purgar_invitaciones_vencidas(NULL, ahora = AHORA)
ok("T57 purge, staff (NULL): every client's", !"t-beta-v" %in% quedan() && identical(c(n), c(beta = 1L)))
e0 <- .mem_escrituras
n <- purgar_invitaciones_vencidas(NULL, ahora = AHORA)
ok("T57 purge with nothing to delete: no write, no count", .mem_escrituras == e0 && !length(n))
sembrar(NULL)
ok("T57 purge with no file: no error, no write",
   { e0 <- .mem_escrituras; r <- tryCatch(purgar_invitaciones_vencidas(NULL, ahora = AHORA), error = function(e) "error")
     !identical(r, "error") && .mem_escrituras == e0 })
# The trigger, in tiersServer: Invitaciones opened (once a session) and ↻,
# the client's own scope for a non-staff viewer, and the log by counts only.
tm <- paste(readLines("R/tiers_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("T57: tiersServer purges on opening Invitaciones and on ↻, own client unless staff, counts in the log",
   grepl('identical(input$main_tab, "invites")', tm, fixed = TRUE) &&
     grepl("observeEvent(input$btn_inv_refresh, inv_purga()", tm, fixed = TRUE) &&
     grepl("if (isTRUE(viewer_is_staff())) NULL else current_client_id()", tm, fixed = TRUE) &&
     grepl('action      = "invitacion_vencida_eliminada"', tm, fixed = TRUE))
sembrar(VEN)
ok("T57: the log line of an unreadable date names neither token nor email",
   { m <- character(); withCallingHandlers(resolve_invite("t-rara"),
       message = function(x) { m <<- c(m, conditionMessage(x)); invokeRestart("muffleMessage") })
     any(grepl("unreadable expiry", m)) && !any(grepl("t-rara|rara@acme", m)) })

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
