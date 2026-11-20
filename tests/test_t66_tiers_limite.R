# =============================================================================
# tests/test_t66_tiers_limite.R
# T66: "Límite de usuarios" (R/tiers_module.R) is a hd_numero_input entero
# in the client request (req_max_users) and the client edit
# (edit_max_users). The browser no longer stops a text, a decimal or a
# number out of 1..500, and the handlers read it with as.integer(x %||% n):
# global.R's %||% takes NA too, so an empty field became 5 / 1 silently
# (1 opened the panel to deactivate accounts) and a decimal was truncated.
#
# Taken from the parsed module (no testServer harness for the tiers
# server), and run with stand-ins:
#   1. btn_save_client_edit, the WHOLE handler: NA, 2.5, 0 and 501 are told
#      and nothing is staged nor shown; 3 (below 5 active users) stages the
#      change WITH the deactivation panel; 10 stages it without;
#   2. btn_submit_client_req, its check: NA, 2.5, 0 and 501 stop it;
#      1, 500 and 37 go on as integers;
#   3. both fields are hd_numero_input entero.
# Run from the repo root:  Rscript tests/test_t66_tiers_limite.R
# Contraprueba: TIERS_R=<5e102e3's tiers_module.R> fails 1 and 2.
# =============================================================================
cat("=== test_t66_tiers_limite ===\n\n")
suppressPackageStartupMessages(library(shiny))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
suppressMessages(source("R/i18n.R"))
cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
    eval(e, envir = globalenv())
cargar("R/ui_components.R", c("HD_NUMERO_FORMATOS", ".hd_numero_decimales", "hd_numero_texto", "hd_numero_deps",
                              "hd_numero_input", "hd_numero_update", "hd_numero_revisar"))
F_T <- Sys.getenv("TIERS_R", "R/tiers_module.R")
ex <- parse(F_T, keep.source = FALSE, encoding = "UTF-8")
manejador <- function(evento) {
  buscar <- function(x) {
    if (!is.call(x)) return(NULL)
    if (identical(x[[1]], as.name("observeEvent")) && length(x) >= 3L &&
        identical(deparse(x[[2]]), paste0("input$", evento))) return(x[[3]])
    args <- as.list(x)[-1]   # an empty argument (x[, 1]) can't be bound to a variable
    for (i in seq_along(args)) if (is.call(args[[i]])) { r <- buscar(args[[i]]); if (!is.null(r)) return(r) }
    NULL
  }
  for (e in ex) { r <- buscar(e); if (!is.null(r)) return(r) }
  NULL
}

# ── 1. btn_save_client_edit, the whole handler ───────────────────────────────
editar <- manejador("btn_save_client_edit")
ok("btn_save_client_edit found", is.call(editar))
REG <- data.frame(client_id = "acme", display_name = "Acme", contact_email = "", max_users = 10L,
                  current_users = 5L, stringsAsFactors = FALSE)
USUARIOS <- data.frame(account_code = paste0("a", 1:5), username = paste0("u", 1:5),
                       display_name = paste0("U", 1:5), deleted = FALSE, stringsAsFactors = FALSE)
correr_edicion <- function(valor) {
  AVISOS <- character(0); MODAL <- 0L; PEND <- NULL
  showNotification <- function(ui, ...) AVISOS <<- c(AVISOS, as.character(ui))
  showModal <- function(...) MODAL <<- MODAL + 1L
  removeModal <- function(...) invisible(NULL)
  is_hopdesk <- function() TRUE; is_principal <- function() FALSE
  read_client_registry <- function(...) REG
  auth_load_usuarios <- function(...) USUARIOS
  pending_client_edit <- function(x) PEND <<- x
  session <- list(userData = list(editing_client_id = "acme"), ns = function(x) x)
  f <- function(input) NULL
  body(f) <- editar
  environment(f) <- environment()
  tryCatch(f(list(edit_display_name = "Acme", edit_contact_email = "", edit_max_users = valor)),
           shiny.silent.error = function(e) NULL)
  list(avisos = AVISOS, modal = MODAL, pend = PEND)
}
for (caso in list(list(NA_real_, "Límite de usuarios: escribe un número."),
                  list(2.5, "Límite de usuarios: escribe un número entero, sin decimales."),
                  list(501, "Límite de usuarios no puede ser mayor que 500."),
                  list(0, "El límite debe ser al menos 1."))) {
  r <- correr_edicion(caso[[1]])
  ok(sprintf("edit, limit %s: '%s'; nothing staged, no confirmation", caso[[1]], caso[[2]]),
     identical(r$avisos, caso[[2]]) && is.null(r$pend) && r$modal == 0L)
}
r <- correr_edicion(3)
ok("edit, limit 3 with 5 active users: staged as 3 WITH the deactivation panel (unchanged)",
   !length(r$avisos) && identical(r$pend$new_max_users, 3L) && isTRUE(r$pend$needs_deact) &&
     nrow(r$pend$active_users) == 5L && r$modal == 1L)
r <- correr_edicion(10)
ok("edit, limit 10: staged as 10, no panel", !length(r$avisos) && identical(r$pend$new_max_users, 10L) &&
     isFALSE(r$pend$needs_deact) && r$modal == 1L)

# ── 2. btn_submit_client_req, its check ──────────────────────────────────────
solicitar <- manejador("btn_submit_client_req")
pasos <- if (is.call(solicitar)) as.list(solicitar)[-1] else list()
txt <- vapply(pasos, function(p) paste(deparse(p), collapse = " "), "")
i_rev <- which(grepl("hd_numero_revisar(input$req_max_users", txt, fixed = TRUE))
i_max <- which(grepl("max_u <- as.integer(rev_max$valor)", txt, fixed = TRUE))
i_reg <- which(grepl("read_client_registry()", txt, fixed = TRUE))
ok("request: one check, then max_u from it, before the registry is read",
   length(i_rev) == 1L && length(i_max) == 1L && i_rev < i_max && length(i_reg) && i_max < i_reg[1] &&
     !any(grepl("input$req_max_users %||%", txt, fixed = TRUE)))
correr_solicitud <- function(valor) {
  AVISOS <- character(0)
  showNotification <- function(ui, ...) AVISOS <<- c(AVISOS, as.character(ui))
  f <- function(input) NULL
  body(f) <- as.call(c(as.name("{"), pasos[i_rev:i_max], quote(list("sigue", max_u))))
  environment(f) <- environment()
  r <- f(list(req_max_users = valor))
  list(sigue = is.list(r) && identical(r[[1]], "sigue"), max_u = if (is.list(r)) r[[2]], avisos = AVISOS)
}
for (v in list(NA_real_, 2.5, 0, 501)) {
  r <- correr_solicitud(v)
  ok(sprintf("request, limit %s: told, stops", v), !r$sigue && length(r$avisos) == 1L)
}
ok("request, limit 0: 'Límite de usuarios debe estar entre 1 y 500.'",
   identical(correr_solicitud(0)$avisos, "Límite de usuarios debe estar entre 1 y 500."))
for (v in c(1, 500, 37)) {
  r <- correr_solicitud(v)
  ok(sprintf("request, limit %s: goes on as the integer %s", v, v), r$sigue && identical(r$max_u, as.integer(v)))
}

# ── 3. The fields ────────────────────────────────────────────────────────────
tm <- paste(readLines(F_T, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("both fields are hd_numero_input(..., formato = \"entero\")",
   grepl('hd_numero_input\\(ns\\("req_max_users"\\), "Límite de usuarios", formato = "entero"', tm) &&
     grepl('hd_numero_input\\(ns\\("edit_max_users"\\), "Límite de usuarios", formato = "entero"', tm))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
