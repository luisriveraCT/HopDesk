# =============================================================================
# tests/test_t87_m8.R
# T87-M8: one observer paints the ☰ menu, from the registry.
#   1. menu_visible() (R/tier_registry.R): the one decision, for every entry
#      (the client tabs and the Grupo dropdown), by the effective tier and its
#      resolved permissions; fail-closed.
#   2. menu_css_inicial(): the first paint shows only what the token's tier
#      alone decides, and outranks the "born hidden" rule.
#   3. menu_js_pintar(): every entry, every time, with an inline !important;
#      never removes a style.
#   4. app.R's menu block run in testServer: a staff session at home, a jump,
#      "ver como" Compras -> Admin -> Compras (the M8 finding: the tabs come
#      back with no jump), the tab moved off a hidden one, a deployment that
#      is not the admin one.
#   5. The one-writer guardian: no other code in app.R, R/ or www/ sets the
#      display of a navbar entry (inline style, jQuery, shinyjs, hideTab) or
#      writes a CSS display rule for one. It is checked against the shapes the
#      three replaced observers had.
# Chrome (a client just signed in, staff with a jump and "ver como" both
# ways, local non-admin) goes in E3's turn: the Coordinador's condition 4.
# Run from project root:  Rscript --vanilla tests/test_t87_m8.R
# =============================================================================
cat("=== test_t87_m8 ===\n\n")
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R")) suppressMessages(source(f))

.pass <- 0L; .fail <- 0L
ok <- function(label, cond, detalle = NULL) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else {
    cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L
    if (length(detalle)) cat("       ", paste(utils::head(detalle, 8), collapse = "\n        "), "\n")
  }
}
app_txt <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
P <- NULL
for (e in parse(text = app_txt, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("HD_PESTANAS_FINANCIERAS"))) P <- eval(e[[3]])
ok("0.1 app.R defines the client tabs", is.character(P) && length(P) >= 10L)
TODAS <- c(P, HD_MENU_GRUPO)

u <- function(tier, ov = "{}", staff = is_staff_tier(tier))
  list(user = "x", tier = tier, is_staff = staff, permisos = permisos_resueltos(tier, ov))
ves <- function(v) names(v)[v]
COMPRAS <- "compras"; ADMIN <- "admin"; FINANZAS <- "finance"
LISTA_COMPRAS <- nivel_pestanas(COMPRAS)

cat("== 1. menu_visible ==\n")
v <- menu_visible(u(nivel_tope()), P, en_cliente = FALSE)
ok("1.1 staff at home (admin deployment): no client tab; Grupo, Usuarios, Empresas",
   identical(ves(v), HD_MENU_GRUPO) && identical(names(v), TODAS))
ok("1.2 staff inside a client: every client tab and Grupo", all(menu_visible(u(nivel_tope()), P, TRUE)))
ok("1.3 staff 'ver como' Compras: only Compras' tabs, no Grupo (the effective tier)",
   setequal(ves(menu_visible(u(COMPRAS, staff = TRUE), P, TRUE)), intersect(LISTA_COMPRAS, P)))
ok("1.4 Admin: every client tab, Grupo and Empresas, no Usuarios",
   setequal(ves(menu_visible(u(ADMIN), P)), c(P, "GRUPO", "EMP")))
ok("1.5 Admin with can_view_tiers given: Usuarios too", all(menu_visible(u(ADMIN, '{"can_view_tiers": true}'), P)))
ok("1.6 Admin without can_view_requisiciones: no REQ, the rest stays",
   setequal(ves(menu_visible(u(ADMIN, '{"can_view_requisiciones": false}'), P)), c(setdiff(P, "REQ"), "GRUPO", "EMP")))
ok("1.7 Finanzas: its tabs (REQ only with its permission), no Grupo",
   setequal(ves(menu_visible(u(FINANZAS), P)),
            P[vapply(P, function(t) nivel_pestana_permitida(FINANZAS, t) &&
                       (t != "REQ" || puede(u(FINANZAS), "can_view_requisiciones")), logical(1))]))
ok("1.8 a client is never 'at home without a client' (en_cliente only matters for staff)",
   identical(menu_visible(u(ADMIN), P, FALSE), menu_visible(u(ADMIN), P, TRUE)))
cerrado <- function(x) is.logical(x) && identical(names(x), TODAS) && !any(x)
ok("1.9 fail-closed: no user, an empty or unknown tier, a broken user: nothing",
   cerrado(menu_visible(NULL, P)) && cerrado(menu_visible(list(tier = ""), P)) &&
     cerrado(menu_visible(list(tier = "no_es_nivel"), P)) && cerrado(menu_visible("x", P)))
ok("1.10 ...and without puede(): no entry behind a permission",
   { .p <- puede; rm(puede); r <- menu_visible(u(nivel_tope()), P); puede <- .p
     !any(r[c("REQ", HD_MENU_GRUPO)]) })
ok("1.11 the permissions come from the registry (HD_PESTANA_PERMISO, every one a real key)",
   all(unlist(HD_PESTANA_PERMISO) %in% permisos_claves()) && all(names(HD_PESTANA_PERMISO) %in% TODAS))

cat("== 2. menu_css_inicial ==\n")
css <- menu_css_inicial(COMPRAS, P)
muestra <- function(css) if (is.null(css)) character() else unique(regmatches(css, gregexpr("(?<=a\\[data-value=')[A-Z]+", css, perl = TRUE))[[1]])
ok("2.1 Compras: shows its tabs without a permission, link and li, only before the observer",
   setequal(muestra(css), setdiff(intersect(LISTA_COMPRAS, P), names(HD_PESTANA_PERMISO))) &&
     all(vapply(muestra(css), function(t) grepl(sprintf("html:not(.hd-nivel-aplicado) nav.navbar li:has(> a[data-value='%s'])", t), css, fixed = TRUE), logical(1))))
ok("2.2 an entry behind a permission (REQ, Grupo) never shows in the first paint (the token has no overrides)",
   !any(c(names(HD_PESTANA_PERMISO), HD_MENU_GRUPO) %in% unlist(lapply(niveles_todos(), function(t) muestra(menu_css_inicial(t, P))))))
ok("2.3 a full client tier: every client tab but REQ", setequal(muestra(menu_css_inicial(ADMIN, P)), setdiff(P, "REQ")))
ok("2.4 staff and an unknown tier: nothing (stays hidden)",
   all(vapply(c(niveles_staff(), list(NULL, "", NA_character_, "no_es_nivel")), function(t) is.null(menu_css_inicial(t, P)), logical(1))))
ok("2.5 it only shows (no display:none) and never names .nav-link",
   !grepl("display:none", css, fixed = TRUE) && !grepl("nav-link", css, fixed = TRUE))
# Specificity (a, b, c) of a simple selector string, enough for these rules.
espec <- function(sel) {
  s <- gsub(":(not|has)\\(", " ", sel)
  c(length(gregexpr("#", s)[[1]][gregexpr("#", s)[[1]] > 0]),
    lengths(regmatches(s, gregexpr("\\.[A-Za-z][-\\w]*|\\[[^]]+\\]|:(?!not|has)[a-z-]+", s, perl = TRUE))),
    lengths(regmatches(s, gregexpr("(^|[\\s>(])[a-z]+(?=[.\\[:\\s]|$)", s, perl = TRUE))))
}
mayor <- function(x, y) { d <- x - y; i <- which(d != 0)[1]; !is.na(i) && d[i] > 0 }
oculta <- strsplit(sub("\\{.*$", "", HD_MENU_CSS_OCULTO), ",")[[1]]
sel_css <- unlist(strsplit(gsub("\\{[^}]*\\}", ",", css), ",\\s*"))
sel_css <- trimws(sel_css[nzchar(trimws(sel_css))])
ok("2.6 each shown selector outranks the hidden rule (both !important, so the order of the sheets does not matter)",
   length(sel_css) == 2L * length(muestra(css)) &&
     all(vapply(sel_css, function(s) mayor(espec(s), espec(if (grepl("li:has", s)) oculta[2] else oculta[1])), logical(1))))
ok("2.7 the hidden rule: every navbar a[data-value] and its li, without .nav-link",
   identical(oculta, c(".navbar a[data-value]", ".navbar li:has(> a[data-value])")) &&
     grepl("{display:none!important}", HD_MENU_CSS_OCULTO, fixed = TRUE))

cat("== 3. menu_js_pintar ==\n")
# What a paint message sets: entry -> c(link, li).
leer_pintura <- function(js) {
  m <- regmatches(js, gregexpr("a\\[data-value=\"([A-Z]+)\"\\]'\\)\\.forEach\\(function\\(el\\) \\{ el\\.style\\.setProperty\\('display', '([a-z-]+)', 'important'\\); var li = el\\.closest\\('li'\\); if \\(li\\) li\\.style\\.setProperty\\('display', '([a-z-]+)', 'important'\\);", js))[[1]]
  r <- lapply(m, function(x) regmatches(x, regexec("data-value=\"([A-Z]+)\".*'display', '([a-z-]+)'.*'display', '([a-z-]+)'", x))[[1]][2:4])
  stats::setNames(lapply(r, `[`, 2:3), vapply(r, `[`, "", 1))
}
vc <- menu_visible(u(COMPRAS), P)
js <- menu_js_pintar(vc)
p <- leer_pintura(js)
ok("3.1 every entry, once", setequal(names(p), TODAS) && !anyDuplicated(names(p)))
ok("3.2 shown: link flex (block inside the dropdown), li list-item; hidden: none and none",
   all(vapply(names(p), function(e) identical(p[[e]],
     if (vc[[e]]) c(if (e %in% c("TIERS", "EMP")) "block" else "flex", "list-item") else c("none", "none")), logical(1))))
ok("3.3 never removes a style, and marks <html> at the end",
   !grepl("removeProperty", js, fixed = TRUE) &&
     endsWith(js, "document.documentElement.classList.add('hd-nivel-aplicado');"))

cat("== 4. app.R's menu block, in a session ==\n")
i0 <- grep("^  menu_vis <- reactive\\(\\{", app_txt)
i1 <- grep("^  # ── Admin deployment: the staff landing tab and the hop grants", app_txt)
ok("4.0 app.R has the menu block (menu_vis, the painter, the tab mover)", length(i0) == 1L && length(i1) == 1L && i1 > i0)
bloque <- parse(text = app_txt[i0:(i1 - 1L)], keep.source = FALSE)
ok("4.0b the block is the reactive and two observers, nothing else",
   length(bloque) == 3L && identical(bloque[[2]][[1]], as.name("observe")) && identical(bloque[[3]][[1]], as.name("observe")))
correr <- function(admin, pasos) {
  pinturas <- list(); movidas <- character(); salida <- NULL
  testServer(function(input, output, session) {
    session$sendCustomMessage <- function(type, message)
      if (identical(type, "shinyjs-runjs")) pinturas[[length(pinturas) + 1L]] <<- message$code %||% message$script %||% unlist(message)[1]
    session$sendInputMessage <- function(inputId, message)
      if (identical(inputId, "nav")) movidas <<- c(movidas, message$value %||% NA_character_)
    IS_ADMIN_DEPLOYMENT <- admin
    HD_PESTANAS_FINANCIERAS <- P
    estado <- reactiveVal(list(user = "x", tier = ""))
    salto <- reactiveVal(NULL)
    usuario_sesion <- function() estado()
    jump_client_id <- function() salto()
    for (e in bloque) eval(e)
    session$userData$.pasos <- list(estado = estado, salto = salto)
  }, {
    res <- list()
    for (paso in pasos) {
      n0 <- length(pinturas)
      if (!is.null(paso$usuario)) session$userData$.pasos$estado(paso$usuario)
      if ("salto" %in% names(paso)) session$userData$.pasos$salto(paso$salto)
      if (!is.null(paso$nav)) session$setInputs(nav = paso$nav)
      session$flushReact()
      res[[paso$nombre]] <- list(nuevas = if (length(pinturas) > n0) pinturas[(n0 + 1L):length(pinturas)] else list(),
                                 movidas = movidas)
    }
    salida <<- res
  })
  salida
}
estado_final <- function(r) { if (!length(r$nuevas)) return(NULL); leer_pintura(r$nuevas[[length(r$nuevas)]]) }
completo_igual <- function(r, esperado) {
  p <- estado_final(r)
  !is.null(p) && setequal(names(p), TODAS) &&
    setequal(names(p)[vapply(p, function(x) x[1] != "none" && x[2] != "none", logical(1))], esperado)
}
staff <- u(nivel_tope())
# A staff "ver como", as usuario_sesion() builds it: the effective tier and the real one.
vc <- function(t) utils::modifyList(u(t, staff = TRUE), list(tier_real = nivel_tope(), viendo_como = TRUE))
r <- correr(TRUE, list(
  list(nombre = "sin_nivel"),
  list(nombre = "casa", usuario = staff),
  list(nombre = "salto", salto = "acme"),
  list(nombre = "compras", usuario = vc(COMPRAS)),
  list(nombre = "admin", usuario = vc(ADMIN)),
  list(nombre = "compras2", usuario = vc(COMPRAS), nav = "FC"),
  list(nombre = "admin2", usuario = vc(ADMIN))))
ok("4.1 tier still unknown: nothing is sent (the hidden rule and the first paint stay)", !length(r$sin_nivel$nuevas))
ok("4.2 staff at home: one paint, Grupo only", length(r$casa$nuevas) == 1L && completo_igual(r$casa, HD_MENU_GRUPO))
ok("4.3 the jump: one paint, every entry", length(r$salto$nuevas) == 1L && completo_igual(r$salto, TODAS))
ok("4.4 'ver como' Compras: Compras' tabs, every other entry hidden explicitly", completo_igual(r$compras, intersect(LISTA_COMPRAS, P)))
ok("4.5 THE FINDING: 'ver como' Admin after Compras, with no jump -- every client tab comes back, plus Grupo and Empresas",
   completo_igual(r$admin, c(P, "GRUPO", "EMP")))
ok("4.6 ...and the other way (Admin -> Compras): the other tabs leave, no jump", completo_igual(r$compras2, intersect(LISTA_COMPRAS, P)))
ok("4.7 the active tab hidden by 'ver como' (FC as Compras): moved to Compras' first tab, once",
   identical(utils::tail(r$compras2$movidas, 1), intersect(P, LISTA_COMPRAS)[1]) &&
     sum(r$compras2$movidas == intersect(P, LISTA_COMPRAS)[1]) == 1L)
ok("4.8 Admin again from Compras: complete again (no state carried from earlier paints)", completo_igual(r$admin2, c(P, "GRUPO", "EMP")))
ok("4.9 every paint is complete: each one sets all the entries (so the last one alone is the state)",
   { todas_p <- unlist(lapply(r, `[[`, "nuevas"), recursive = FALSE)
     length(todas_p) >= 5L && all(vapply(todas_p, function(js) setequal(names(leer_pintura(js)), TODAS), logical(1))) })
r2 <- correr(FALSE, list(list(nombre = "casa", usuario = staff), list(nombre = "cliente", usuario = u(FINANZAS))))
ok("4.10 not the admin deployment: staff at home see the client tabs (as before M8)", completo_igual(r2$casa, TODAS))
ok("4.11 not the admin deployment: a client sees its tabs", completo_igual(r2$cliente, ves(menu_visible(u(FINANZAS), P))))

# T117: every selectable entry counts, not only the client tabs.
movs <- function(r, paso) r[[paso]]$movidas
r3 <- correr(TRUE, list(
  list(nombre = "casa", usuario = staff, nav = "TIERS"),
  list(nombre = "salto", salto = "acme"),
  list(nombre = "compras", usuario = vc(COMPRAS)),
  list(nombre = "en_req", nav = "REQ"),
  list(nombre = "admin_cal", usuario = vc(ADMIN), nav = "CAL"),
  list(nombre = "a_casa", usuario = staff, salto = NULL)))
ok("4.12 T117 (contraprueba): staff at home on Usuarios, a visible entry -- not moved",
   !length(movs(r3, "casa")))
ok("4.13 T117 THE FINDING: 'ver como' Compras while on Usuarios -- moved to Compras' first tab, so Usuarios is not left drawn",
   identical(utils::tail(movs(r3, "compras"), 1), intersect(P, LISTA_COMPRAS)[1]) && length(movs(r3, "compras")) == 1L)
ok("4.14 T117 (contraprueba): on a tab Compras sees -- not moved", length(movs(r3, "en_req")) == 1L)
ok("4.15 T117: back home from CAL -- moved to Usuarios (the first entry staff see at home), the client's tab is not left drawn",
   identical(utils::tail(movs(r3, "a_casa"), 1), "TIERS") && length(movs(r3, "a_casa")) == 2L)
ok("4.16 T117: never to GRUPO (the dropdown, not a panel)", !"GRUPO" %in% unlist(lapply(r3, `[[`, "movidas")))

cat("== 5. The one-writer guardian ==\n")
ENTRADA <- "data-value|nav-link|dropdown-item|navbar-nav|nav-item"
ESCRIBE <- paste0("setProperty\\(\\s*\\\\?['\"]display|removeProperty\\(\\s*\\\\?['\"]display|\\.style\\.display\\s*=|",
                  "\\.css\\(\\s*\\\\?['\"]display|\\.(hide|show|toggle|fadeIn|fadeOut)\\(\\s*\\)|",
                  "classList\\.(add|toggle|remove)\\(\\s*\\\\?['\"](d-none|hidden|hd-oculta)")
# Lines of `txt` that write the display of a navbar entry: a write on a line
# that names an entry, or on a variable taken from an entry (its query, a
# forEach over it, its closest/parent).
escritores <- function(txt) {
  todo <- paste(txt, collapse = "\n")
  sucias <- character()
  for (m in regmatches(todo, gregexpr("querySelectorAll\\(([^)]*)\\)\\s*\\.forEach\\(\\s*function\\s*\\(\\s*(\\w+)", todo, perl = TRUE))[[1]])
    if (grepl(ENTRADA, m)) sucias <- c(sucias, sub(".*function\\s*\\(\\s*(\\w+)$", "\\1", m, perl = TRUE))
  for (m in regmatches(todo, gregexpr("(var|let|const)\\s+\\w+\\s*=\\s*[^;\\n]*", todo, perl = TRUE))[[1]])
    if (grepl(ENTRADA, m) && grepl("querySelector|getElementsBy|closest|\\$\\(", m))
      sucias <- c(sucias, sub("^(var|let|const)\\s+(\\w+).*$", "\\2", m, perl = TRUE))
  repeat {
    n <- length(sucias)
    for (m in regmatches(todo, gregexpr("(var|let|const)\\s+\\w+\\s*=\\s*\\w+\\.(closest|parentElement|parentNode)", todo, perl = TRUE))[[1]]) {
      x <- regmatches(m, regexec("(var|let|const)\\s+(\\w+)\\s*=\\s*(\\w+)\\.", m, perl = TRUE))[[1]]
      if (x[4] %in% sucias) sucias <- unique(c(sucias, x[3]))
    }
    if (length(unique(sucias)) == n) break
  }
  sucias <- unique(sucias)
  objetivo <- if (length(sucias)) paste0("\\b(", paste(sucias, collapse = "|"), ")\\.(style|classList)|\\$\\(\\s*(", paste(sucias, collapse = "|"), ")\\s*\\)") else "$^"
  which(grepl(ESCRIBE, txt, perl = TRUE) & (grepl(ENTRADA, txt) | grepl(objetivo, txt, perl = TRUE)))
}
# Server-side ways to hide a navbar entry without JavaScript.
OTROS <- paste0("(hideTab|showTab|nav_hide|nav_show|nav_remove)\\(\\s*(inputId\\s*=\\s*)?['\"]nav['\"]|",
                "shinyjs::(hide|show|toggle)\\([^)]*(", ENTRADA, ")")
# CSS display rules for an entry.
CSS_ENTRADA <- "\\[data-value[^]]*\\][^{]*\\{[^}]*display\\s*:|nav-link[^{]*\\{[^}]*display\\s*:\\s*none"
lineas_de <- function(f, nombre) {   # lines of a top-level definition in an R file
  ex <- parse(f, keep.source = TRUE); sr <- attr(ex, "srcref")
  for (i in seq_along(ex)) if (is.call(ex[[i]]) && identical(ex[[i]][[1]], as.name("<-")) && identical(ex[[i]][[2]], as.name(nombre)))
    return(sr[[i]][1]:sr[[i]][3])
  integer()
}
permitido <- list(
  "R/tier_registry.R" = c(lineas_de("R/tier_registry.R", "menu_js_pintar"),
                          lineas_de("R/tier_registry.R", "HD_MENU_CSS_OCULTO"),
                          lineas_de("R/tier_registry.R", "menu_css_inicial")))
archivos <- c("app.R", list.files("R", "[.]R$", recursive = TRUE, full.names = TRUE),
              list.files("www", "[.](js|css)$", recursive = TRUE, full.names = TRUE))
hallados <- character()
for (f in archivos) {
  txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
  cod <- if (grepl("[.]R$", f)) sub("^\\s*#.*$", "", txt) else txt
  malas <- sort(unique(c(escritores(cod), grep(OTROS, cod, perl = TRUE), grep(CSS_ENTRADA, cod, perl = TRUE))))
  malas <- setdiff(malas, permitido[[f]] %||% integer())
  if (length(malas)) hallados <- c(hallados, sprintf("%s:%d  %s", f, malas, substr(trimws(txt[malas]), 1, 110)))
}
ok("5.1 nothing but menu_js_pintar (and the two registry CSS rules) sets the display of a navbar entry", !length(hallados), hallados)
uso <- unlist(lapply(archivos[grepl("[.]R$", archivos)], function(f) {
  l <- sub("#.*$", "", readLines(f, warn = FALSE, encoding = "UTF-8")); paste0(f, ":", grep("menu_js_pintar(", l, fixed = TRUE)) }))
uso <- setdiff(uso[!grepl(":$", uso)], paste0("R/tier_registry.R:", lineas_de("R/tier_registry.R", "menu_js_pintar")))
ok("5.2 one caller: the menu observer in app.R", length(uso) == 1L && startsWith(uso, "app.R:"), uso)
ok("5.3 app.R no longer has the replaced pieces (FINANCIAL_TABS, .TABS_CLIENTE, ocultaNivel, menu_grupo_visibles)",
   !any(grepl("FINANCIAL_TABS|\\.TABS_CLIENTE|ocultaNivel|menu_grupo_visibles|\\.MENU_GRUPO_CSS", sub("#.*$", "", app_txt))))
# The guardian against the shapes it must catch (the three replaced observers
# and two other ways), so it cannot go blind.
formas <- list(
  nivel = c("document.querySelectorAll('.navbar .nav-link[data-value=\\\"%s\\\"]').forEach(function(el) {",
            "  var li = el.closest('li');", "  if (li) li.style.removeProperty('display');", "});"),
  admin = c("document.querySelectorAll('.navbar .nav-link[data-value=\\\"%s\\\"]').forEach(function(x) {",
            "  x.style.setProperty('display', 'flex', 'important');", "});"),
  grupo = c("var t = document.querySelector('.dropdown-menu a[data-value=\"TIERS\"]');", "t.style.display = 'none';"),
  jquery = c("$('.navbar a[data-value=\"FC\"]').hide();"),
  tab = c("    hideTab(\"nav\", \"FC\")"))
atrapa <- vapply(formas, function(x) length(c(escritores(x), grep(OTROS, x, perl = TRUE))) > 0L, logical(1))
ok("5.4 it catches the shapes of 'Pestañas por nivel', the admin observer, M7, jQuery and hideTab", all(atrapa), names(atrapa)[!atrapa])
ok("5.5 ...and not a write on something else (the ☰ bubble, a module's own element)",
   !length(escritores(c("var g = document.getElementById('hd-menu-globo');", "g.style.display = 'none';",
                        "var ok = visible(p.querySelector('.nav-link[data-value=VEN]'));",
                        "raiz.querySelectorAll('.hd-dia-vista').forEach(function (c) { c.style.display = ''; });"))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
