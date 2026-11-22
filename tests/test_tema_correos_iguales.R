# =============================================================================
# tests/test_tema_correos_iguales.R
# TEMA stage 4b: the account mails (R/email_service.R: invitation, welcome,
# password reset, user-limit warning / reached / request, limit change) take
# their colours from the theme registry: every colour is a {{rol}} marker that
# .email_pintar() resolves (mail clients do not understand var()). Chrome never
# sees a mail, so this is their check. The base and the branch build the seven
# mails from the same arguments, each in a clean process, and:
#   - with the colours masked, every mail is the same text, byte for byte;
#   - every colour is paired with the base's, in order: same count and each
#     pair within ΔE 2 (CIE76), so Clásico looks the same;
#   - nothing is left unpainted ({{rol}}), under Clásico or Marca;
#   - tema = NULL is Clásico, and under Marca the header band is Marca's.
#
#   TEMA_BASE=<checkout before the mails' 4b> Rscript --vanilla tests/test_tema_correos_iguales.R
# Without TEMA_BASE only the branch's own checks run.
# =============================================================================
cat("=== test_tema_correos_iguales ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
RAMA <- normalizePath(".", winslash = "/")
BASE <- Sys.getenv("TEMA_BASE")
if (nzchar(BASE)) BASE <- normalizePath(BASE, winslash = "/")

# The seven mails of one checkout, in a clean process. `tema` is only passed
# when given (the base's functions have no such argument).
correos <- function(raiz, tema = NULL) callr::r(function(raiz, tema) {
  setwd(raiz)
  `%||%` <- function(a, b) if (is.null(a)) b else a
  assign("%||%", `%||%`, envir = globalenv())
  for (f in c("R/i18n.R", "R/tema_registro.R", "R/email_service.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
  t <- if (is.null(tema)) list() else list(tema = tema)
  llama <- function(fn, ...) do.call(get(fn, envir = globalenv()), c(list(...), t))
  list(
    invitacion  = llama("email_invite_html", "Ana Pérez", "Acme & Co", "https://app/?invite=abc",
                        expires_hours = 48, inviter_name = "Luis"),
    invitacion_sin = llama("email_invite_html", "Ana", "Acme", "https://app/?invite=x"),
    bienvenida  = llama("email_welcome_html", "Ana Pérez", "ana.perez", "Acme", "https://app/"),
    contrasena  = llama("email_password_reset_html", "Ana", "https://app/?reset=r", 2),
    aviso       = llama("email_user_limit_warning_html", "Acme", 8L, 10L),
    tope        = llama("email_user_limit_reached_html", "Acme", 10L, 10L),
    solicitud   = llama("email_user_request_html", "Acme", 10L, 10L, "ti@acme.mx"),
    sube        = llama("email_limit_change_html", "Acme", 10L, 15L, "Luis"),
    baja        = llama("email_limit_change_html", "Acme", 15L, 10L, "Luis", deactivated = c("ana", "beto")))
}, args = list(raiz = raiz, tema = tema))

RX <- "(?<![&\\w])#(?:[0-9A-Fa-f]{8}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})(?![0-9A-Za-z_-])|rgba?\\([^)]*\\)"
enmascarar <- function(html) gsub(RX, "§color", html, perl = TRUE)
colores <- function(html) {
  x <- regmatches(html, gregexpr(RX, html, perl = TRUE))[[1]]
  lapply(x, function(h) {
    h <- substring(h, 2L); if (nchar(h) == 3L) h <- paste(rep(strsplit(h, "")[[1]], each = 2L), collapse = "")
    strtoi(substring(h, c(1, 3, 5), c(2, 4, 6)), 16L)
  })
}
lab <- function(rgb) grDevices::convertColor(matrix(rgb / 255, nrow = 1), from = "sRGB", to = "Lab")
de  <- function(a, b) sqrt(sum((lab(a) - lab(b))^2))

reg <- new.env(); sys.source("R/tema_registro.R", envir = reg)
rama  <- correos(RAMA)
marca <- correos(RAMA, tema = "marca")
clas  <- correos(RAMA, tema = "clasico")

ok("the branch builds the 9 mails (7 functions)", length(rama) == 9L && all(vapply(rama, nzchar, logical(1))))
ok("nothing left unpainted ({{...}}) under Clásico or Marca",
   !any(grepl("{{", c(unlist(rama), unlist(marca)), fixed = TRUE)))
ok("tema = NULL is Clásico (the default theme): the same mails", identical(rama, clas))
ok("under Marca the header band is Marca's (leg-encabezado-07090d -> encabezado)",
   grepl(sprintf('bgcolor="%s"', reg$tema_color("encabezado", "marca")), marca$invitacion, fixed = TRUE) &&
   !identical(marca$invitacion, rama$invitacion))
ok("under Marca the warning, the limit and the request follow Marca's aviso, peligro and primario",
   grepl(reg$tema_color("aviso", "marca"), marca$aviso, fixed = TRUE) &&
   grepl(reg$tema_color("peligro", "marca"), marca$tope, fixed = TRUE) &&
   grepl(reg$tema_color("primario", "marca"), marca$solicitud, fixed = TRUE))

if (nzchar(BASE)) {
  base <- correos(BASE)
  ok("the base builds the same 9 mails", identical(names(base), names(rama)))
  for (k in names(rama)) {
    ok(sprintf("%s: with the colours masked, the same text as the base, byte for byte", k),
       identical(enmascarar(base[[k]]), enmascarar(rama[[k]])))
    cb <- colores(base[[k]]); cr <- colores(rama[[k]])
    d <- if (length(cb) == length(cr)) mapply(de, cb, cr) else Inf
    ok(sprintf("%s: %d colours paired in order, each within ΔE 2 (max %.2f)", k, length(cr), max(c(0, d))),
       length(cb) == length(cr) && all(d <= 2))
  }
} else cat(" (sin TEMA_BASE: no se compara contra la base)\n")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
