# =============================================================================
# tests/test_tema_t63.R
# TEMA 6 (T6-3): Actividad, its sticky bar and the two audit PDFs (Actividad,
# Archivos de pago) to theme roles.
#   1. .AUDIT_PDF_CSS has no colour of its own: only {{rol}} marks;
#   2. painted in Clásico it is today's stylesheet, colour for colour (the
#      only change is the spelling: #222 -> #222222, lower -> upper case);
#   3. painted in Marca every mark resolves and no Clásico-only colour is left;
#   4. both PDFs paint it with the exporting session's theme
#      (tema_pintar(.AUDIT_PDF_CSS, tema), tema = tema_de_sesion() at the call);
#   5. the sticky bar and Actividad's toggle message use var(--hd-*).
# Run from project root:  Rscript tests/test_tema_t63.R
# =============================================================================
cat("=== test_tema_t63 ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
tema <- new.env(); sys.source("R/tema_registro.R", envir = tema)

# .AUDIT_PDF_CSS, evaluated alone from its file.
css_de <- function(texto) {
  e <- new.env()
  for (ex in parse(text = texto, keep.source = FALSE))
    if (is.call(ex) && identical(ex[[1]], as.name("<-")) && identical(deparse(ex[[2]]), ".AUDIT_PDF_CSS")) eval(ex, e)
  e$.AUDIT_PDF_CSS
}
css <- css_de(leer("R/audit_log_viewer_module.R"))
# What the stylesheet was before T6-3 (master 7227425b), kept here to compare.
ANTES <- c("#222", "#1E3A5F", "#1E3A5F", "#444", "#E0E0E0", "#333", "#888", "#1E3A5F", "#fff",
           "#ccc", "#f2f2f2", "#ddd", "#1E3A5F", "#ddd", "#888", "#888")
largo <- function(h) toupper(ifelse(nchar(h) == 4L, gsub("^#(.)(.)(.)$", "#\\1\\1\\2\\2\\3\\3", h), h))

# ── 1 ──
ok("1: .AUDIT_PDF_CSS has no hex or rgb() of its own",
   is.character(css) && !grepl("#[0-9A-Fa-f]{3,8}\\b", css) && !grepl("rgba?\\(", css))
marcas <- regmatches(css, gregexpr("\\{\\{[a-z0-9-]+\\}\\}", css))[[1]]
ok(sprintf("1: it carries %d {{rol}} marks, one per colour of before (%d)", length(marcas), length(ANTES)),
   length(marcas) == length(ANTES))

# ── 2 ──
clasico <- tema$tema_pintar(css, "clasico")
despues <- regmatches(clasico, gregexpr("#[0-9A-Fa-f]{6}\\b", clasico))[[1]]
ok("2: in Clásico, colour for colour and in the same order, today's stylesheet",
   identical(toupper(despues), largo(ANTES)))
ok("2: in Clásico nothing else changes (the text around the colours is the same)",
   identical(gsub("#[0-9A-Fa-f]{3,6}\\b", "C", clasico), gsub("\\{\\{[a-z0-9-]+\\}\\}", "C", css)))

# ── 3 ──
marca <- tema$tema_pintar(css, "marca")
ok("3: in Marca every mark resolves", !grepl("{{", marca, fixed = TRUE))
en_marca <- toupper(regmatches(marca, gregexpr("#[0-9A-Fa-f]{6}\\b", marca))[[1]])
roles_marca <- toupper(unlist(tema$HD_TEMAS$marca$tokens))
ok(sprintf("3: in Marca every colour is one of Marca's roles (%s)", paste(unique(en_marca), collapse = " ")),
   all(en_marca %in% roles_marca))
ok("3: in Marca the cover and the section bands take Marca's encabezado (navy)",
   grepl(toupper(sprintf("background:%s;", tema$tema_color("encabezado", "marca"))), toupper(marca), fixed = TRUE))

# ── 4 ──
for (f in c("R/audit_log_viewer_module.R", "R/pagos_audit_viewer_module.R")) {
  s <- leer(f)
  ok(sprintf("4: %s paints the stylesheet with the theme it is given", f),
     grepl("tags$style(HTML(tema_pintar(.AUDIT_PDF_CSS, tema)))", s, fixed = TRUE) &&
       !grepl("tags$style(HTML(.AUDIT_PDF_CSS))", s, fixed = TRUE) &&
       grepl("_html_doc <- function(export_df, meta_info = list(), tema = NULL)", s, fixed = TRUE))
  ok(sprintf("4: %s exports with the session's theme", f), grepl("), tema = tema_de_sesion()),", s, fixed = TRUE))
}

# ── 5 ──
pega <- leer("R/audit_sticky_ui.R")
ok("5: the sticky bar: surface, border and search icon by role (no hex, no rgba)",
   grepl("background: var(--hd-superficie);", pega, fixed = TRUE) &&
     grepl("border-bottom: 1px solid var(--hd-leg-borde-dee2e6);", pega, fixed = TRUE) &&
     grepl("color: color-mix(in srgb, var(--hd-leg-primario-0d6efd) 55%, transparent);", pega, fixed = TRUE))
ok("5: Actividad's live-refresh message by role",
   grepl("color:var(--hd-leg-texto-2-6c757d);", leer("R/audit_log_viewer_module.R"), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
