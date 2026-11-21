# =============================================================================
# tests/test_tema_reporte_igual.R
# TEMA stage 4b: the financial report (R/reporte_module.R) takes its colours
# from the theme registry. The report is a standalone HTML (the tab's iframe,
# the HTML and PDF downloads) that Chrome's "Clásico the same" never sees
# inside, so this is its check: the base and the branch build the report from
# one fixture (tests/helpers/reporte_fixture.R, Networks Group, with pulse
# tags A, B and C) in clean processes, and every colour the HTML writes is
# paired with the base's, in order:
#   - same count; each pair within ΔE 2 (CIE76) and the same alpha, except
#     the pulse's tag badges, which are etq-a/b/c-texto (Luis' rule);
#   - under Marca the report follows the theme (the header is Marca's);
#   - nothing left unpainted ({{rol}} placeholders);
#   - with the language in Spanish, no fixed English text left.
#
#   TEMA_BASE=<checkout before the report's 4b> Rscript --vanilla tests/test_tema_reporte_igual.R
# Without TEMA_BASE only the branch's own checks run.
# TEMA_REP_ETIQUETAS_VIEJAS=1: a checkout before the badges' commit (tags still
# the old red/amber/purple) must equal the base, badges included.
# =============================================================================
cat("=== test_tema_reporte_igual ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
RAMA <- normalizePath(".", winslash = "/")
BASE <- Sys.getenv("TEMA_BASE")
if (nzchar(BASE)) BASE <- normalizePath(BASE, winslash = "/")

# The report of one checkout, in a clean process (the branch's fixture).
reporte <- function(raiz, tema = NULL, idioma = "es", ui = FALSE) callr::r(function(raiz, ayudante, tema, idioma, ui) {
  setwd(raiz)
  options(hopdesk.idioma = idioma)
  source(ayudante)
  if (ui) return(as.character(reporteUI("rpt")))
  ng <- reporte_ng()
  # The pulse's payables panel shows today's (this week) and the whole next
  # week: P1 (today) urgent = A; P3 (next week) both = B; P4 (next week) important = C.
  tags <- data.frame(ledger = "AP", Empresa = "NG", Moneda = c("MXN", "MXN", "MXN", "USD"),
                     Documento = c("P1", "P3", "P3", "P4"), tag = c("urgent", "urgent", "important", "important"),
                     stringsAsFactors = FALSE)
  args <- list(ng$cuentas, ng$movs, ng$sap, etiquetas = tags)
  if (!is.null(tema)) args$tema <- tema
  if (!"etiquetas" %in% names(formals(reporte_correr))) args$etiquetas <- NULL
  do.call(reporte_correr, args)$html
}, args = list(raiz = raiz, ayudante = file.path(RAMA, "tests/helpers/reporte_fixture.R"), tema = tema,
               idioma = idioma, ui = ui))

# Every colour of an HTML, in order: hex (3, 6 or 8 digits) and rgb[a]().
RX <- "(?<![&\\w])#(?:[0-9A-Fa-f]{8}|[0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})(?![0-9A-Za-z_-])|rgba?\\([^)]*\\)"
colores <- function(html) {
  m <- gregexpr(RX, html, perl = TRUE)[[1]]
  txt <- regmatches(html, list(m))[[1]]
  ctx <- substring(html, pmax(1L, m - 60L), m - 1L)
  out <- lapply(txt, function(x) {
    if (startsWith(x, "rgb")) {
      v <- as.numeric(regmatches(x, gregexpr("[0-9.]+", x))[[1]])
      list(rgb = v[1:3], a = if (length(v) == 4L) v[4] else 1)
    } else {
      h <- substring(x, 2L); if (nchar(h) == 3L) h <- paste(rep(strsplit(h, "")[[1]], each = 2L), collapse = "")
      list(rgb = strtoi(substring(h, c(1, 3, 5), c(2, 4, 6)), 16L),
           a = if (nchar(h) == 8L) round(strtoi(substring(h, 7, 8), 16L) / 255, 2) else 1)
    }
  })
  list(col = out, txt = txt, ctx = ctx)
}
lab <- function(rgb) grDevices::convertColor(matrix(rgb / 255, nrow = 1), from = "sRGB", to = "Lab")
de  <- function(a, b) sqrt(sum((lab(a) - lab(b))^2))
hexde <- function(rgb) sprintf("%02X%02X%02X", rgb[1], rgb[2], rgb[3])

reg <- new.env(); sys.source("R/tema_registro.R", envir = reg)
ETQ <- c(urg = reg$tema_color("etq-a-texto", "clasico"), both = reg$tema_color("etq-b-texto", "clasico"),
         imp = reg$tema_color("etq-c-texto", "clasico"))
VIEJAS <- c(urg = "#DC2626", both = "#7C3AED", imp = "#D97706")   # the pulse's badges before TEMA 4b
if (identical(Sys.getenv("TEMA_REP_ETIQUETAS_VIEJAS"), "1")) ETQ <- VIEJAS
es_insignia <- function(ctx) grepl("pulse-badge-(urg|both|imp)\\{background:$", ctx)

rama_html <- reporte(RAMA)
rama <- colores(rama_html)
ok("the branch builds the report (4 pages) and it writes colours", length(rama$col) > 100L &&
     grepl("Semana Pr", rama_html, fixed = TRUE))
ok("nothing left unpainted: no {{rol}} placeholder in the report", !grepl("{{", rama_html, fixed = TRUE))
ok("the pulse shows the three tags (its fixture tags P1, P3 and P4)",
   all(vapply(c("pulse-badge pulse-badge-urg", "pulse-badge pulse-badge-imp", "pulse-badge pulse-badge-both"),
              function(k) grepl(k, rama_html, fixed = TRUE), TRUE)))
for (k in names(ETQ)) {
  i <- which(grepl(paste0("pulse-badge-", k, "\\{background:$"), rama$ctx))
  ok(sprintf("the %s badge's colour is %s", k, ETQ[[k]]),
     length(i) == 1L && identical(toupper(rama$txt[i]), toupper(ETQ[[k]])))
}

if (!identical(ETQ, VIEJAS)) {
  ok("the badges are named A, B and C from the dictionary (tablas.etiquetas.*), not URG / IMP",
     grepl('pulse-badge-urg">A<', rama_html, fixed = TRUE) && grepl('pulse-badge-both">B<', rama_html, fixed = TRUE) &&
       grepl('pulse-badge-imp">C<', rama_html, fixed = TRUE) && !grepl(">URG|>IMP", rama_html))
  lum <- function(h) { v <- strtoi(substring(h, c(2, 4, 6), c(3, 5, 7)), 16L) / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4); sum(c(0.2126, 0.7152, 0.0722) * v) }
  contraste <- function(a, b) { l <- sort(c(lum(a), lum(b)), decreasing = TRUE); (l[1] + 0.05) / (l[2] + 0.05) }
  blanco <- reg$tema_color("texto-inverso", "clasico")
  cs <- vapply(ETQ, function(e) contraste(e, blanco), 0)
  ok(sprintf("the badge's text (texto-inverso) reads on A, B and C: contrast %s (>= 4.5)", paste(sprintf("%.1f", cs), collapse = ", ")),
     all(cs >= 4.5))
}

if (nzchar(BASE)) {
  base <- colores(reporte(BASE))
  ok(sprintf("under Clásico the report writes as many colours as the base (%d)", length(base$col)),
     length(base$col) == length(rama$col))
  if (length(base$col) == length(rama$col)) {
    malos <- character()
    for (i in seq_along(base$col)) {
      b <- base$col[[i]]; r <- rama$col[[i]]
      if (es_insignia(base$ctx[i])) next
      if (de(b$rgb, r$rgb) > 2 || abs(b$a - r$a) > 0.005)
        malos <- c(malos, sprintf("%s -> %s (ΔE %.1f) after \"%s\"", base$txt[i], rama$txt[i], de(b$rgb, r$rgb),
                                  substring(base$ctx[i], 40L)))
    }
    if (length(malos)) message("    ", paste(utils::head(malos, 10), collapse = "\n    "))
    ok("every colour, pair by pair, within ΔE 2 of the base's and the same alpha (badges apart)", !length(malos))
  }
} else cat(" (sin TEMA_BASE: no se compara contra la base)\n")

# The report's words come from the dictionary (TEMA 4b): in Spanish no fixed
# English is left, in English no fixed Spanish; nothing left unresolved.
INGLES  <- c("Financial Summary Report", "Daily Forward", ">Company<", "Finances", "Total Balance", ">IN<", ">OUT<",
             "SPEI IN", "SPEI OUT", "CONFIDENTIAL", "Generated", ">Mon ", ">Tue ", ">Wed ", ">Thu ", ">Fri ",
             " Nov ", " Nov<", ">URG", ">IMP")
ESPANOL <- c("Resumen financiero", "Pulso diario", ">Empresa<", "Finanzas", "Saldo total", ">Abono<", ">Cargo<",
             "CONFIDENCIAL", "Generado", "Posición Global", "Saldos por Cuenta", "Cobros esperados",
             "Semana Actual", "Reporte financiero")
fijos <- function(html, lista) lista[vapply(lista, function(x) grepl(x, html, fixed = TRUE), TRUE)]
ok("nothing left unresolved: no [[t:clave]] nor [[idioma]] in the report", !grepl("[[", rama_html, fixed = TRUE))
quedan <- fijos(rama_html, INGLES)
ok(sprintf("in Spanish, no fixed English text in the report (%s)", if (length(quedan)) paste(quedan, collapse = ", ") else "none"),
   !length(quedan) && all(lengths(lapply(ESPANOL, function(x) fijos(rama_html, x))) == 1L) &&
     grepl('<html lang="es">', rama_html, fixed = TRUE))
en_html <- reporte(RAMA, idioma = "en")
quedan <- fijos(en_html, ESPANOL)
ok(sprintf("in English, the same words in English and no fixed Spanish (%s)", if (length(quedan)) paste(quedan, collapse = ", ") else "none"),
   !length(quedan) && all(vapply(c("Financial Summary Report", ">Company<", "Total Balance", "Daily Forward"),
                                 function(x) grepl(x, en_html, fixed = TRUE), TRUE)) &&
     grepl('<html lang="en">', en_html, fixed = TRUE) && !grepl("[[", en_html, fixed = TRUE))
ui_es <- reporte(RAMA, ui = TRUE); ui_en <- reporte(RAMA, idioma = "en", ui = TRUE)
ok("the tab (reporteUI) in both languages: its titles and labels from the dictionary",
   all(vapply(c("Configuración del reporte", "Volver a Bancos", "Aplicar y generar"), function(x) grepl(x, ui_es, fixed = TRUE), TRUE)) &&
     all(vapply(c("Report settings", "Back to Banks", "Apply and generate"), function(x) grepl(x, ui_en, fixed = TRUE), TRUE)) &&
     !grepl("Configuración del reporte", ui_en, fixed = TRUE))

# Under Marca the report follows the theme.
marca <- reporte(RAMA, tema = "marca")
enc_m <- toupper(reg$tema_color("encabezado", "marca"))
ok(sprintf("under Marca: the header's gradient is Marca's encabezado (%s), not Clásico's #0D1B3E", enc_m),
   grepl(paste0("linear-gradient(120deg,", enc_m), marca, fixed = TRUE) && !grepl("#0D1B3E", toupper(marca), fixed = TRUE))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
