# =============================================================================
# tests/test_tema_politicas_igual.R
# TEMA stage 4: the payment policies' calendar simulator (Ajustes → Políticas,
# .policy_cal_preview_render() in R/settings/settings_policies.R) takes its
# colours from theme roles (var(--hd-*)). The base and the branch build the
# simulator from the same inputs (a policy that skips holidays, selected and
# projected days, the holiday list; and a weekdays policy), each in a clean
# process. The branch's var(--hd-*) are resolved with Clásico's tokens and:
#   - with the colours masked, the HTML is the base's, byte for byte;
#   - every colour is paired with the base's, in order, within ΔE 2 (CIE76);
#     a base rgba() tint is flattened over white (the panel's surface), and a
#     glow (box-shadow) must be the same text;
#   - every var(--hd-*) the branch writes exists in both themes.
#
#   TEMA_BASE=<checkout before this change> Rscript --vanilla tests/test_tema_politicas_igual.R
# Without TEMA_BASE only the branch's own checks run.
# =============================================================================
cat("=== test_tema_politicas_igual ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
RAMA <- normalizePath(".", winslash = "/")
BASE <- Sys.getenv("TEMA_BASE")
if (nzchar(BASE)) BASE <- normalizePath(BASE, winslash = "/")

# The simulator of one checkout, in a clean process. Fixed dates: the first
# month shown is this month (the function starts from Sys.Date()).
simulador <- function(raiz, idioma = "es") callr::r(function(raiz, idioma) {
  setwd(raiz)
  options(hopdesk.idioma = idioma)
  suppressPackageStartupMessages({ library(shiny); library(lubridate) })
  `%||%` <- function(a, b) if (is.null(a)) b else a
  assign("%||%", `%||%`, envir = globalenv())
  for (f in c("R/i18n.R", "R/tema_registro.R", "R/policy_engine.R", "R/settings/settings_policies.R"))
    if (file.exists(f)) suppressMessages(sys.source(f, envir = globalenv()))
  hoy <- lubridate::floor_date(Sys.Date(), "month")
  sel <- format(hoy + c(0L, 4L, 9L, 15L))
  anio <- as.integer(format(hoy, "%Y"))
  hcache <- list(MX = as.Date(c(sprintf("%d-01-01", anio - 1L), format(hoy + 4L), format(hoy + 9L),
                               sprintf("%d-12-25", anio))))
  c(festivos = as.character(.policy_cal_preview_render("skip_holidays", list(country = "MX"), "forward",
                                                       hcache, 0L, sel)),
    semana = as.character(.policy_cal_preview_render("weekdays", list(days = 1:5), "backward",
                                                     list(), 1L, format(hoy + 31L + c(1L, 5L)))))
}, args = list(raiz = raiz, idioma = idioma))

reg <- new.env(); sys.source("R/tema_registro.R", envir = reg)
TK <- list(clasico = reg$tema_tokens("clasico"), marca = reg$tema_tokens("marca"))
resolver <- function(html, tk) {
  vs <- unique(regmatches(html, gregexpr("var\\(--hd-[a-z0-9-]+\\)", html))[[1]])
  for (v in vs) html <- gsub(v, tk[[sub("^var\\(--hd-(.+)\\)$", "\\1", v)]], html, fixed = TRUE)
  # color-mix(in srgb, #rrggbb P%, transparent) paints rgba(r, g, b, P/100).
  cm <- unique(regmatches(html, gregexpr("color-mix\\(in srgb, #[0-9A-Fa-f]{6} [0-9.]+%, transparent\\)", html))[[1]])
  for (x in cm) {
    h <- sub("^.*#([0-9A-Fa-f]{6}).*$", "\\1", x); p <- as.numeric(sub("^.* ([0-9.]+)%.*$", "\\1", x))
    v <- strtoi(substring(h, c(1, 3, 5), c(2, 4, 6)), 16L)
    html <- gsub(x, sprintf("rgba(%d, %d, %d, %s)", v[1], v[2], v[3], format(p / 100)), html, fixed = TRUE)
  }
  html
}
RX <- "(?<![&\\w])#(?:[0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})(?![0-9A-Za-z_-])|rgba?\\([^)]*\\)"
enmascarar <- function(html) gsub(RX, "§color", html, perl = TRUE)
# Colour list in order; inside a box-shadow the shadow text is kept whole.
colores <- function(html) {
  x <- regmatches(html, gregexpr(RX, html, perl = TRUE))[[1]]
  lapply(x, function(s) {
    if (startsWith(s, "rgb")) {
      v <- as.numeric(regmatches(s, gregexpr("[0-9.]+", s))[[1]]); a <- if (length(v) == 4L) v[4] else 1
      list(txt = s, rgb = round(255 - (255 - v[1:3]) * a), a = a)   # flattened over white
    } else {
      h <- substring(s, 2L); if (nchar(h) == 3L) h <- paste(rep(strsplit(h, "")[[1]], each = 2L), collapse = "")
      list(txt = toupper(s), rgb = strtoi(substring(h, c(1, 3, 5), c(2, 4, 6)), 16L), a = 1)
    }
  })
}
lab <- function(rgb) grDevices::convertColor(matrix(rgb / 255, nrow = 1), from = "sRGB", to = "Lab")
de  <- function(a, b) sqrt(sum((lab(a) - lab(b))^2))

rama <- simulador(RAMA)
ok("the branch builds the simulator (skip holidays + weekdays)", length(rama) == 2L && all(nchar(rama) > 2000L))
ok("the simulator writes no hex or rgb() of its own: only var(--hd-*)", !any(grepl(RX, rama, perl = TRUE)))
usados <- unique(unlist(regmatches(rama, gregexpr("(?<=var\\(--hd-)[a-z0-9-]+(?=\\))", rama, perl = TRUE))))
ok(sprintf("every var(--hd-*) it uses exists in Clásico and Marca (%d)", length(usados)),
   all(usados %in% names(TK$clasico)) && all(usados %in% names(TK$marca)))
marca <- vapply(rama, resolver, "", tk = TK$marca)
ok("under Marca the selected days take Marca's acento and the projected ones its primario",
   grepl(TK$marca[["acento"]], marca[["festivos"]], fixed = TRUE) && grepl(TK$marca[["primario"]], marca[["festivos"]], fixed = TRUE))

if (nzchar(BASE)) {
  base <- simulador(BASE)
  clas <- vapply(rama, resolver, "", tk = TK$clasico)
  for (k in names(base)) {
    ok(sprintf("%s: with the colours masked, the base's HTML byte for byte", k),
       identical(enmascarar(base[[k]]), enmascarar(clas[[k]])))
    cb <- colores(base[[k]]); cr <- colores(clas[[k]])
    malos <- 0L; maxde <- 0
    if (length(cb) != length(cr)) malos <- 1L else for (j in seq_along(cb)) {
      d <- de(cb[[j]]$rgb, cr[[j]]$rgb); maxde <- max(maxde, d)
      if (d > 2) malos <- malos + 1L
    }
    ok(sprintf("%s: %d colours paired in order, each within ΔE 2 (max %.2f)", k, length(cr), maxde),
       malos == 0L && length(cr) > 0L)
  }
  sombras <- function(h) regmatches(h, gregexpr("box-shadow:[^;\"]+", h))[[1]]
  ok("the glows (box-shadow) are the base's text exactly",
     identical(lapply(base, sombras), lapply(unname(clas), sombras) |> stats::setNames(names(base))))
} else cat(" (sin TEMA_BASE: no se compara contra la base)\n")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
