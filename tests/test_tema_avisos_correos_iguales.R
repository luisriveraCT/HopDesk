# =============================================================================
# tests/test_tema_avisos_correos_iguales.R
# TEMA stage 4b: the weekly emails (R/avisos_correos.R) take their colours
# from the theme registry: CORREO_COLORES is built with tema_color(<rol>,
# "marca") and the brand's palette (.HD_PALETA), with no hex of its own. The
# mails keep Marca for every client until stage 5, so nothing may change.
#
# The scenarios are tests/test_t45_correos_semana.R's (Monday, Friday, the
# close without SAP, the base in EUR, the review mail...): that script runs in
# a clean process from the base and from the branch, with correo_lunes_html(),
# correo_viernes_html() and correo_revision() wrapped to keep every mail they
# build, in order. Then, mail by mail:
#   - with the colours masked, the same text, byte for byte;
#   - every colour paired with the base's, in order, each within ΔE 2 (CIE76);
#   - and only the two unified ones move (linea_tabla, cambio).
# Also: CORREO_COLORES has no hex of its own in R/avisos_correos.R, and its
# values are Marca's from the registry.
#
#   TEMA_BASE=<checkout before this change> Rscript --vanilla tests/test_tema_avisos_correos_iguales.R
# Without TEMA_BASE only the branch's own checks run.
# =============================================================================
cat("=== test_tema_avisos_correos_iguales ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
RAMA <- normalizePath(".", winslash = "/")
BASE <- Sys.getenv("TEMA_BASE")
if (nzchar(BASE)) BASE <- normalizePath(BASE, winslash = "/")
T45 <- file.path(RAMA, "tests/test_t45_correos_semana.R")

# Every weekly mail t45 builds, from one checkout, in order.
correos <- function(raiz) callr::r(function(raiz, t45) {
  setwd(raiz)
  .hd_capturados <- list()
  envolver <- function(fn) {
    orig <- get(fn, envir = globalenv())
    nuevo <- function(...) {
      h <- orig(...)
      .hd_capturados[[length(.hd_capturados) + 1L]] <<- list(fn = fn, html = paste(as.character(h), collapse = ""))
      h
    }
    attr(nuevo, "hd_envuelta") <- TRUE
    assign(fn, nuevo, envir = globalenv())
  }
  FNS <- c("correo_lunes_html", "correo_viernes_html", "correo_revision")
  sink(tempfile())
  on.exit(sink(), add = TRUE)
  for (e in parse(t45, keep.source = FALSE)) {
    if (is.call(e) && identical(e[[1]], as.name("quit"))) next
    try(eval(e, globalenv()), silent = TRUE)
    for (fn in FNS)
      if (exists(fn, envir = globalenv(), inherits = FALSE) &&
          is.null(attr(get(fn, envir = globalenv()), "hd_envuelta"))) envolver(fn)
  }
  .hd_capturados
}, args = list(raiz = raiz, t45 = T45))

RX <- "(?<![&\\w])#(?:[0-9A-Fa-f]{8}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})(?![0-9A-Za-z_-])|rgba?\\([^)]*\\)"
enmascarar <- function(html) gsub(RX, "§color", html, perl = TRUE)
colores <- function(html) {
  x <- regmatches(html, gregexpr(RX, html, perl = TRUE))[[1]]
  lapply(toupper(x), function(h) {
    h <- substring(h, 2L); if (nchar(h) == 3L) h <- paste(rep(strsplit(h, "")[[1]], each = 2L), collapse = "")
    list(hex = h, rgb = strtoi(substring(h, c(1, 3, 5), c(2, 4, 6)), 16L))
  })
}
lab <- function(rgb) grDevices::convertColor(matrix(rgb / 255, nrow = 1), from = "sRGB", to = "Lab")
de  <- function(a, b) sqrt(sum((lab(a) - lab(b))^2))

# The registry: CORREO_COLORES is Marca's, with no hex in R/avisos_correos.R.
reg <- new.env(); sys.source("R/tema_registro.R", envir = reg)
fuente <- readLines("R/avisos_correos.R", warn = FALSE, encoding = "UTF-8")
fuente <- fuente[!grepl("^\\s*#", fuente)]
ok("R/avisos_correos.R has no hex or rgb() of its own (CORREO_COLORES comes from the registry)",
   !any(grepl(RX, fuente, perl = TRUE)))

# shiny sources R/ in alphabetical order: avisos_correos.R before tema_registro.R.
# CORREO_COLORES is lazy, so loading in that order and then using it works.
ok("loaded in shiny's alphabetical order (avisos_correos.R, then tema_registro.R), CORREO_COLORES is Marca's",
   isTRUE(callr::r(function(raiz) {
     setwd(raiz); e <- new.env(parent = globalenv())
     assign("%||%", function(a, b) if (is.null(a)) b else a, envir = e)
     for (f in sort(c("R/avisos_correos.R", "R/tema_registro.R", "R/i18n.R"))) sys.source(f, envir = e)
     identical(e$CORREO_COLORES$marino, e$tema_color("primario", "marca")) &&
       identical(e$.cr_C$arena_tinta, e$tema_color("arena-fg", "marca")) &&
       !any(c("logro", "logro_borde") %in% names(e$CORREO_COLORES))
   }, args = list(raiz = RAMA))))
ok("every field keeps its value but the two unified ones (Marca's roles and palette = the old hex)", {
  esperado <- c(fondo = "#EDEBE4", papel = "#FFFFFF", tinta = "#1A2233", gris = "#5A6474", marino = "#13213A",
    dorado = "#C9AE6E", dorado_osc = "#8A6D2C", linea = "#E4E1D8", pie = "#F7F6F2", arena = "#EFE6D2",
    arena_tinta = "#6B5320", gris_claro = "#9AA6BA", cobre = "#9A4524", revision = "#F7E9E1",
    revision_tinta = "#7C3519", verde = "#2F6B4F", logro_tinta = "#8A6D2C", riel = "#ECE9E1", reprog = "#5F6D87",
    por_vencer = "#D7DCE5", liquidado_ar = "#E6D6AC", liquidado_ap = "#5B6A86", linea_oscura = "#2A3A58",
    linea_tabla = "#F3F1EA", cambio = "#D9D4C6")   # the last two: superficie-2 (was #EFECE4), borde (was #D9D3C4)
  # reprog: Marca's texto-tenue, #5F6D87 since TEMA 6-7/6-9 (was #8B97AD, 2.95:1; the Coordinador's readability decision)
  cc <- callr::r(function(raiz) {
    setwd(raiz); e <- new.env(parent = globalenv())
    assign("%||%", function(a, b) if (is.null(a)) b else a, envir = e)
    for (f in c("R/i18n.R", "R/tema_registro.R", "R/avisos_correos.R")) sys.source(f, envir = e)
    unlist(e$CORREO_COLORES)
  }, args = list(raiz = RAMA))
  identical(toupper(cc[names(esperado)]), esperado) && setequal(names(cc), names(esperado))
})

rama <- correos(RAMA)
ok(sprintf("t45's scenarios build the weekly mails in the branch (%d)", length(rama)), length(rama) >= 6L)
# (correo_revision() is "" when there is nothing to review: that is a mail too)
ok("nothing left unpainted ({{...}}) in the branch's mails, and most of them are real mails",
   !any(vapply(rama, function(x) grepl("{{", x$html, fixed = TRUE), logical(1))) &&
   sum(vapply(rama, function(x) nchar(x$html) > 1000L, logical(1))) >= 6L)

if (nzchar(BASE)) {
  base <- correos(BASE)
  ok(sprintf("the base builds the same %d mails, in the same order", length(base)),
     identical(vapply(base, `[[`, "", "fn"), vapply(rama, `[[`, "", "fn")))
  if (length(base) == length(rama)) {
    textos <- mapply(function(b, r) identical(enmascarar(b$html), enmascarar(r$html)), base, rama)
    ok(sprintf("every mail, with the colours masked, is the base's byte for byte (%d of %d)", sum(textos), length(textos)),
       all(textos))
    UNIFICADOS <- c("EFECE4", "D9D3C4")   # linea_tabla -> superficie-2, cambio -> borde (ΔE <= 2)
    pares <- 0L; maxde <- 0; movidos <- character(); malos <- 0L
    for (i in seq_along(base)) {
      cb <- colores(base[[i]]$html); cr <- colores(rama[[i]]$html)
      if (length(cb) != length(cr)) { malos <- malos + 1L; next }
      for (j in seq_along(cb)) {
        pares <- pares + 1L
        if (!identical(cb[[j]]$hex, cr[[j]]$hex)) {
          d <- de(cb[[j]]$rgb, cr[[j]]$rgb); maxde <- max(maxde, d)
          movidos <- union(movidos, cb[[j]]$hex)
          if (d > 2) malos <- malos + 1L
        }
      }
    }
    ok(sprintf("%d colours paired in order, each within ΔE 2 (max %.2f)", pares, maxde), malos == 0L && pares > 0L)
    ok(sprintf("only the unified colours move (%s)", paste(movidos, collapse = ", ") %||% ""),
       all(movidos %in% UNIFICADOS))
  }
} else cat(" (sin TEMA_BASE: no se compara contra la base)\n")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
