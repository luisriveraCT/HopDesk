# =============================================================================
# tests/test_t87_m7.R
# T87-M7 (found by ISO, T98): after a client signed in, the ☰ panel showed
# the Grupo dropdown's entries loose ("TIERS EMP") for ~10 s, before the
# server's rules arrived: the static CSS hid the GRUPO toggle but not its
# entries, and the observer "showed" them by removing a style. Now:
#   1. The first paint hides the toggle AND every dropdown entry (any
#      a[data-value] inside .dropdown-menu, so a new entry is born hidden).
#   2. The selectors match the navbar bslib really builds.
#   3. Which entries a session may see comes from usuario_sesion(): the
#      effective tier and its permissions -- "ver como" holds (as T87-M6).
#   4. The observer shows or hides EACH entry with an inline !important and
#      never returns early; if it never runs, everything stays hidden.
# Chrome (a client just signed in, and staff) goes in E3's turn after the
# TANDA64 (the Coordinador's condition 4).
# Run from project root:  Rscript --vanilla tests/test_t87_m7.R
# =============================================================================
cat("=== test_t87_m7 ===\n\n")
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R")) suppressMessages(source(f))
app <- parse("app.R", keep.source = FALSE)
for (e in app)
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(".MENU_GRUPO_CSS", "menu_grupo_visibles"))
    eval(e, envir = globalenv())
txt <- readLines("app.R", warn = FALSE, encoding = "UTF-8")

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

cat("== 1. El primer dibujo ==\n")
ok("1.1 esconde el botón GRUPO (y su li) sin depender de la clase nav-link",
   grepl(".navbar a[data-value='GRUPO']", .MENU_GRUPO_CSS, fixed = TRUE) &&
     grepl(".navbar li:has(> a[data-value='GRUPO'])", .MENU_GRUPO_CSS, fixed = TRUE))
ok("1.2 esconde TODA entrada del desplegable (enlace y li), no una lista",
   grepl(".navbar .dropdown-menu a[data-value]", .MENU_GRUPO_CSS, fixed = TRUE) &&
     grepl(".navbar .dropdown-menu li:has(> a[data-value])", .MENU_GRUPO_CSS, fixed = TRUE) &&
     grepl("{display:none!important}", .MENU_GRUPO_CSS, fixed = TRUE))
ok("1.3 la cabecera de la página lo lleva", any(grepl("tags$style(HTML(.MENU_GRUPO_CSS))", txt, fixed = TRUE)))

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

cat("== 3. Quién ve qué (usuario_sesion: nivel efectivo) ==\n")
u <- function(tier, ov = "{}", staff = FALSE) list(user = "x", tier = tier, is_staff = staff, permisos = permisos_resueltos(tier, ov))
v <- function(...) menu_grupo_visibles(u(...))
ok("3.1 Dev: Grupo, Usuarios y Empresas", all(v(nivel_ti())))
ok("3.2 Admin: Grupo y Empresas, sin Usuarios", identical(v("admin"), c(GRUPO = TRUE, TIERS = FALSE, EMP = TRUE)))
ok("3.3 Admin con can_view_tiers a mano: también Usuarios", all(v("admin", '{"can_view_tiers": true}')))
ok("3.4 Finanzas: nada", !any(v("finance")))
ok("3.5 Compras: nada", !any(v("compras")))
ok("3.6 staff en casa: todo", all(v("hopdesk", staff = TRUE)) && all(v(nivel_tope(), staff = TRUE)))
ok("3.7 staff viendo como Admin: lo de Admin (sin Usuarios), no lo suyo", identical(v("admin", staff = TRUE), v("admin")))
ok("3.8 sin usuario o sin puede(): nada (falla cerrado)", !any(menu_grupo_visibles(NULL)) && {
  .p <- puede; rm(puede); r <- !any(menu_grupo_visibles(u(nivel_ti()))); puede <- .p; r })

cat("== 4. El observador ==\n")
i <- grep("observeEvent(usuario_sesion(), {", txt, fixed = TRUE)
cuerpo <- if (length(i) == 1L) paste(txt[i:(i + 15L)], collapse = "\n") else ""
ok("4.1 decide con menu_grupo_visibles(usuario_sesion())", grepl("menu_grupo_visibles(usuario_sesion())", cuerpo, fixed = TRUE))
ok("4.2 muestra o esconde cada entrada con !important (nunca quita el estilo)",
   grepl("setProperty('display', '%s', 'important')", cuerpo, fixed = TRUE) && !grepl("removeProperty", cuerpo, fixed = TRUE))
ok("4.3 sin salida temprana: nada de return() antes de mandar el JS", !grepl("return()", cuerpo, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
