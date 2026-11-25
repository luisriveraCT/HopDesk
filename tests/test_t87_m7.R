# =============================================================================
# tests/test_t87_m7.R
# T87-M7 (found by ISO, T98): after a client signed in, the ☰ panel showed
# the Grupo dropdown's entries loose ("TIERS EMP") for ~10 s, before the
# server's rules arrived: the static CSS hid the GRUPO toggle but not its
# entries, and the observer "showed" them by removing a style. Since T87-M8
# the menu has one rule (R/tier_registry.R) and one observer; this test keeps
# M7's demands on them:
#   1. The first paint hides the toggle AND every dropdown entry -- now every
#      navbar entry (HD_MENU_CSS_OCULTO), so a new entry is born hidden; the
#      first paint never shows a Grupo entry (they are behind permissions).
#   2. The selectors match the navbar bslib really builds (no .nav-link).
#   3. Which Grupo entries a session may see comes from usuario_sesion(): the
#      effective tier and its permissions -- "ver como" holds (as T87-M6).
#   4. The observer shows or hides EACH entry with an inline !important and
#      never removes a style; with a known tier it always paints, and while
#      the tier is unknown it sends nothing, so everything stays hidden.
# Run from project root:  Rscript --vanilla tests/test_t87_m7.R
# =============================================================================
cat("=== test_t87_m7 ===\n\n")
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R")) suppressMessages(source(f))
txt <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
P <- NULL
for (e in parse(text = txt, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("HD_PESTANAS_FINANCIERAS"))) P <- eval(e[[3]])

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

cat("== 1. El primer dibujo ==\n")
ok("1.1 esconde el botón GRUPO (y su li) sin depender de la clase nav-link",
   grepl(".navbar a[data-value]", HD_MENU_CSS_OCULTO, fixed = TRUE) &&
     grepl(".navbar li:has(> a[data-value])", HD_MENU_CSS_OCULTO, fixed = TRUE) && !grepl("nav-link", HD_MENU_CSS_OCULTO, fixed = TRUE))
ok("1.2 esconde TODA entrada (enlace y li), no una lista: también las del desplegable",
   !grepl("data-value=", HD_MENU_CSS_OCULTO, fixed = TRUE) && grepl("{display:none!important}", HD_MENU_CSS_OCULTO, fixed = TRUE))
ok("1.3 la cabecera de la página lo lleva", any(grepl("tags$style(HTML(HD_MENU_CSS_OCULTO))", txt, fixed = TRUE)))
ok("1.4 el primer dibujo nunca muestra una entrada de Grupo, para ningún nivel",
   !any(vapply(niveles_todos(), function(t) { css <- menu_css_inicial(t, P)
     !is.null(css) && any(vapply(HD_MENU_GRUPO, function(g) grepl(sprintf("'%s'", g), css, fixed = TRUE), logical(1))) }, logical(1))))

cat("== 2. Los selectores corresponden a la barra que arma bslib ==\n")
barra <- as.character(bslib::page_navbar(id = "nav",
  bslib::nav_panel("Calendario", value = "CAL"),
  bslib::nav_menu("Grupo", value = "GRUPO",
    bslib::nav_panel("Usuarios", value = "TIERS"), bslib::nav_panel("Empresas", value = "EMP"))))
ok("2.1 el botón es un <a data-value='GRUPO'> (sin .nav-link en el primer dibujo: por eso el selector no la usa)",
   grepl('<a[^>]*data-value="GRUPO"', barra) && !grepl('<a[^>]*class="[^"]*nav-link[^"]*"[^>]*data-value="GRUPO"', barra))
menu <- regmatches(barra, regexpr('<ul class="dropdown-menu[^"]*"[^>]*>.*?</ul>', barra))
ok("2.2 Usuarios y Empresas son <a data-value> dentro de un <li> de .dropdown-menu",
   length(menu) == 1L && grepl('<li>\\s*<a[^>]*data-value="TIERS"', menu) && grepl('<li>\\s*<a[^>]*data-value="EMP"', menu))
ok("2.3 cada entrada de la barra (pestaña, botón, desplegable) es un <a data-value> hijo directo de un <li>",
   all(vapply(c("CAL", HD_MENU_GRUPO), function(v) grepl(sprintf('<li[^>]*>\\s*<a[^>]*data-value="%s"', v), barra), logical(1))))

cat("== 3. Quién ve qué (usuario_sesion: nivel efectivo) ==\n")
u <- function(tier, ov = "{}", staff = FALSE) list(user = "x", tier = tier, is_staff = staff, permisos = permisos_resueltos(tier, ov))
v <- function(..., en_cliente = TRUE) menu_visible(u(...), P, en_cliente)[HD_MENU_GRUPO]
ok("3.1 Dev: Grupo, Usuarios y Empresas", all(v(nivel_ti())))
ok("3.2 Admin: Grupo y Empresas, sin Usuarios", identical(v("admin"), c(GRUPO = TRUE, TIERS = FALSE, EMP = TRUE)))
ok("3.3 Admin con can_view_tiers a mano: también Usuarios", all(v("admin", '{"can_view_tiers": true}')))
ok("3.4 Finanzas: nada", !any(v("finance")))
ok("3.5 Compras: nada", !any(v("compras")))
ok("3.6 staff en casa: todo", all(v("hopdesk", staff = TRUE, en_cliente = FALSE)) && all(v(nivel_tope(), staff = TRUE, en_cliente = FALSE)))
ok("3.7 staff viendo como Admin: lo de Admin (sin Usuarios), no lo suyo", identical(v("admin", staff = TRUE), v("admin")))
ok("3.8 sin usuario o sin puede(): nada (falla cerrado)", !any(menu_visible(NULL, P)[HD_MENU_GRUPO]) && {
  .p <- puede; rm(puede); r <- !any(v(nivel_ti())); puede <- .p; r })

cat("== 4. El observador ==\n")
i <- grep("menu_vis <- reactive({", txt, fixed = TRUE)
cuerpo <- if (length(i) == 1L) paste(txt[i:(i + 9L)], collapse = "\n") else ""
ok("4.1 decide con menu_visible(usuario_sesion())",
   grepl("u <- usuario_sesion()", cuerpo, fixed = TRUE) && grepl("menu_visible(u, HD_PESTANAS_FINANCIERAS, en_cliente)", cuerpo, fixed = TRUE))
js <- menu_js_pintar(menu_visible(u("admin"), P))
ok("4.2 muestra o esconde cada entrada con !important (nunca quita el estilo)",
   all(vapply(HD_MENU_GRUPO, function(g) grepl(sprintf("a[data-value=\"%s\"]').forEach(function(el) { el.style.setProperty('display', '", g), js, fixed = TRUE), logical(1))) &&
     !grepl("removeProperty", js, fixed = TRUE))
ok("4.3 con nivel conocido siempre pinta (sin salida temprana); sin nivel no manda nada y todo sigue oculto",
   length(gregexpr("return(", cuerpo, fixed = TRUE)[[1]]) == 1L &&
     grepl('if (!.nivel_conocido(u$tier %||% "")) return(NULL)', cuerpo, fixed = TRUE) &&
     grepl("if (!is.null(vis)) shinyjs::runjs(menu_js_pintar(vis))", cuerpo, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
