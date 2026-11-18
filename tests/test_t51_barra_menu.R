# =============================================================================
# tests/test_t51_barra_menu.R
# T51 (Luis, 2026-11-17): the navbar as one row -- brand, ☰ (with Vencidos'
# count) and the active tab -- and everything else in a panel under the ☰.
# Static checks of R/ui_components.R (app_styles(), barra_menu_ui()); the
# browser checks are in reportes/T51_LISTO.md.
#
# Run from project root:  Rscript tests/test_t51_barra_menu.R
# =============================================================================
cat("=== test_t51_barra_menu ===\n\n")
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
.i18n_dir <- file.path(getwd(), "www", "i18n")
source("R/i18n.R")
for (e in parse("R/ui_components.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("app_styles", "app_scripts", "barra_menu_ui"))
    eval(e, globalenv())
css <- as.character(app_styles())
js  <- paste(as.character(app_scripts()), collapse = "\n")

cat("-- one row --\n")
ok("the bar's height is one variable, 48px, and the navbar takes it",
   grepl(":root { --hd-barra-alto: 48px; }", css, fixed = TRUE) &&
     grepl("nav.navbar { height: var(--hd-barra-alto);", css, fixed = TRUE))
ok("the links' panel is hidden until the ☰ opens it, and Bootstrap's own toggler is not used",
   grepl("nav.navbar .navbar-collapse { display: none !important; }", css, fixed = TRUE) &&
     grepl("nav.navbar.hd-menu-abierto .navbar-collapse {", css, fixed = TRUE) &&
     grepl("nav.navbar .navbar-toggle, nav.navbar .navbar-toggler { display: none !important; }", css, fixed = TRUE))
# The tier, permission and admin-deployment rules hide links with
# display:none !important: the menu must never set display on a link.
reglas <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css))[[1]]
de_enlace <- reglas[grepl("navbar-collapse[^{]*(\\.nav-link|\\.dropdown-item)", reglas)]
ok("no rule of the menu sets display on a .nav-link or .dropdown-item (the visibility rules stay in charge)",
   length(de_enlace) > 0L && !any(grepl("display\\s*:", sub("^[^{]*\\{", "", de_enlace))))

cat("\n-- the three fixed heights read the variable --\n")
ok("ui_components.R (the calendar)", grepl("calc(100vh - var(--hd-barra-alto) - 36px)", css, fixed = TRUE))
for (f in c("R/tiers_module.R", "R/empresas_module.R"))
  ok(f, any(grepl("calc(100vh - var(--hd-barra-alto) - 17px)", readLines(f, warn = FALSE), fixed = TRUE)))
todo <- paste(vapply(c("app.R", list.files("R", "\\.R$", full.names = TRUE)),
                     function(f) paste(readLines(f, warn = FALSE), collapse = "\n"), ""), collapse = "\n")
ok("no 100vh - 84px / - 65px left", !grepl("100vh - 84px|100vh - 65px", todo))

cat("\n-- keyboard and screen readers --\n")
ok("the ☰ is a <button> with aria-expanded and aria-controls (the panel's id)",
   grepl("btn.type = 'button'", js, fixed = TRUE) && grepl("setAttribute('aria-expanded', 'false')", js, fixed = TRUE) &&
     grepl("setAttribute('aria-controls', p.id)", js, fixed = TRUE))
ok("Esc closes and gives the focus back; opening focuses the active tab",
   grepl("e.key === 'Escape' && abierto()) cerrar(true)", js, fixed = TRUE) &&
     grepl("if (destino) destino.focus()", js, fixed = TRUE) && grepl("if (devolverFoco) btn.focus()", js, fixed = TRUE))
ok("choosing a tab, clicking outside or tabbing out closes it",
   grepl("a[data-bs-toggle=tab]", js, fixed = TRUE) && grepl("!e.target.closest('nav.navbar')", js, fixed = TRUE) &&
     grepl("focusout", js, fixed = TRUE))
ok("the active tab carries aria-current=page", grepl("setAttribute('aria-current', 'page')", js, fixed = TRUE))
ok("a hidden tab is never named in the bar (the app loading, a tier without it)",
   grepl("if (a && visible(a))", js, fixed = TRUE))

cat("\n-- words --\n")
claves <- unique(regmatches(js, gregexpr("t\\('comun\\.menu\\.[a-z_]+'", js))[[1]])
claves <- sub("^t\\('", "", sub("'$", "", claves))
dic <- lapply(c(es = "es", en = "en"), function(l) jsonlite::fromJSON(sprintf("www/i18n/%s.json", l)))
ok("every text of the ☰ comes from the dictionary, in es and en",
   length(claves) == 3L && all(vapply(dic, function(d) all(claves %in% names(d)), logical(1))))
ok("the dictionary reaches the page's <head> with the menu (hdT)",
   grepl("window.HD_I18N=", as.character(htmltools::renderTags(app_scripts())$head), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
