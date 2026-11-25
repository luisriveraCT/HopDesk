# =============================================================================
# tests/test_t69c_barra_inicial.R
# T69c: a tier with a tab list (Compras: PAN + REQ) never sees the other tabs,
# not even while the session's first load runs. Since T87-M8 every menu entry
# is born hidden (HD_MENU_CSS_OCULTO), and the page itself carries CSS that
# SHOWS the tabs the token's tier may see -- built from the tier of the token
# shinymanager has already resolved (server side; nothing from the browser is
# the tier). A tab behind a permission (REQ) is not in it: the token has no
# permission overrides, so it waits for the menu's observer, which takes over
# by adding `hd-nivel-aplicado` to <html> once the tier is known.
#   1. menu_css_inicial(): Compras shows PAN and nothing else (every other
#      client tab stays hidden); a full tier, every tab but REQ; staff and an
#      unknown tier give nothing.
#   2. app.R's own .t69c_estilo_inicial(), run against shinymanager's real
#      token registry: a valid token gives its tier's CSS; no token, an
#      unknown token give nothing; a tier sent by the browser is ignored.
#   3. Through secure_app(): the page it serves for a valid token carries the
#      CSS (shinymanager still hands the request to a function ui).
#   4. app.R's wiring: secure_app gets the wrapper (with the page captured, not
#      the router `ui`); the class is added once, by the menu's one painter,
#      after every entry, only with a known tier; the class name is the CSS's.
#   5. Guard: shinymanager:::.tok still has is_valid()/get()/add()/remove().
# Run from project root:  Rscript tests/test_t69c_barra_inicial.R
# Counter-proof: on 29d829e (without T69c) 1, 2 and 4 fail; on 0bad2024
# (hiding, before T87-M8) 1 and 4 fail.
# =============================================================================
cat("=== test_t69c_barra_inicial ===\n\n")
suppressPackageStartupMessages({ library(shiny) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/i18n.R", "R/tier_registry.R")) source(f)
COMPRAS <- "compras"
TOTAL <- names(TIER_REGISTRY)[vapply(TIER_REGISTRY, function(e) is.null(e$pestanas) && !isTRUE(e$interno), logical(1))][1]
STAFF <- names(TIER_REGISTRY)[vapply(TIER_REGISTRY, function(e) isTRUE(e$interno), logical(1))][1]

app_src <- Sys.getenv("HD_APP_R", "app.R")
exprs <- parse(app_src, keep.source = FALSE, encoding = "UTF-8")
txt <- readLines(app_src, warn = FALSE, encoding = "UTF-8")
codigo <- sub("#.*$", "", txt)
def <- list()
buscar_def <- function(e) {
  if (!is.call(e)) return(invisible())
  if (identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c("HD_PESTANAS_FINANCIERAS", ".t69c_estilo_inicial"))
    def[[as.character(e[[2]])]] <<- e
  for (i in seq_along(e)[-1]) if (!identical(e[[i]], quote(expr = )) && is.call(e[[i]])) buscar_def(e[[i]])
}
for (e in exprs) buscar_def(e)
ok("app.R defines HD_PESTANAS_FINANCIERAS and .t69c_estilo_inicial()",
   all(c("HD_PESTANAS_FINANCIERAS", ".t69c_estilo_inicial") %in% names(def)))
for (d in def) eval(d)
TABS <- if (exists("HD_PESTANAS_FINANCIERAS")) HD_PESTANAS_FINANCIERAS else character()

# ── 1. The CSS by tier ───────────────────────────────────────────────────────
ok("menu_css_inicial() exists", exists("menu_css_inicial", mode = "function"))
css <- tryCatch(menu_css_inicial(COMPRAS, TABS), error = function(e) NULL)
muestra <- function(x) if (is.null(x)) character() else unique(regmatches(x, gregexpr("(?<=a\\[data-value=')[A-Z]+", x, perl = TRUE))[[1]])
otras <- setdiff(TABS, nivel_pestanas(COMPRAS))
ok(sprintf("Compras: the %d other client tabs are never shown (they stay hidden)", length(otras)),
   is.character(css) && length(otras) >= 8L && !length(intersect(muestra(css), otras)))
ok("Compras: PAN is shown (link and li); REQ, behind a permission, waits for the observer",
   identical(muestra(css), "PAN") &&
     grepl("nav.navbar a[data-value='PAN']", css, fixed = TRUE) && grepl("nav.navbar li:has(> a[data-value='PAN'])", css, fixed = TRUE))
ok("the CSS only applies before the server's observer takes over",
   is.character(css) && !grepl("display:none", css, fixed = TRUE) &&
     length(gregexpr("html:not(.hd-nivel-aplicado) ", css, fixed = TRUE)[[1]]) == 2L * length(muestra(css)))
ok("the hidden rule it overrides hides every entry (so anything it does not show stays hidden)",
   identical(HD_MENU_CSS_OCULTO, ".navbar a[data-value],.navbar li:has(> a[data-value]){display:none!important}"))
ok("a full client tier: every client tab but REQ",
   exists("menu_css_inicial") && setequal(muestra(menu_css_inicial(TOTAL, TABS)), setdiff(TABS, "REQ")))
ok("staff get no CSS (at home in the admin deployment they see no client tab)",
   exists("menu_css_inicial") && is.null(menu_css_inicial(STAFF, TABS)))
ok("an unknown tier gets no CSS",
   exists("menu_css_inicial") &&
     all(vapply(list(NULL, "", NA_character_, "no_es_nivel"), function(t) is.null(menu_css_inicial(t, TABS)), logical(1))))

# ── 2. app.R's wrapper against shinymanager's own token registry ─────────────
tok <- shinymanager:::.tok
pedir <- function(qs) list(QUERY_STRING = qs)
html_de <- function(x) if (is.null(x)) "" else as.character(htmltools::renderTags(x)$head)
if (exists(".t69c_estilo_inicial", mode = "function")) {
  tk_c <- paste0("t69c_compras_", as.integer(Sys.time())); tk_f <- paste0("t69c_total_", as.integer(Sys.time()))
  tok$add(tk_c, list(user = "prueba_compras", tier = COMPRAS))
  tok$add(tk_f, list(user = "prueba_total", tier = TOTAL))
  h <- html_de(.t69c_estilo_inicial(pedir(paste0("?token=", tk_c))))
  ok("a valid Compras token: the page head carries the CSS, with PAN and without CAL", grepl("hd-t69c-pestanas", h, fixed = TRUE) &&
       grepl("a[data-value='PAN']", h, fixed = TRUE) && !grepl("data-value='CAL'", h, fixed = TRUE))
  ok("the token quoted as shinymanager writes it also works",
     grepl("hd-t69c-pestanas", html_de(.t69c_estilo_inicial(pedir(sprintf("?token=%%22%s%%22", tk_c)))), fixed = TRUE))
  h_f <- html_de(.t69c_estilo_inicial(pedir(paste0("?token=", tk_f))))
  ok("a valid full-tier token: its CSS, with CAL", grepl("hd-t69c-pestanas", h_f, fixed = TRUE) && grepl("a[data-value='CAL']", h_f, fixed = TRUE))
  ok("no token: nothing", is.null(.t69c_estilo_inicial(pedir(""))) && is.null(.t69c_estilo_inicial(list())))
  ok("an unknown token: nothing", is.null(.t69c_estilo_inicial(pedir("?token=no_existe"))))
  ok("a tier sent by the browser is ignored (only the token's counts)",
     identical(html_de(.t69c_estilo_inicial(pedir(paste0("?token=", tk_f, "&tier=", COMPRAS)))), h_f) &&
       identical(html_de(.t69c_estilo_inicial(pedir(paste0("?token=", tk_c, "&tier=", TOTAL)))), h))
  ok("the wrapper never spends the token: after all the calls above, shinymanager's single-use is_valid() is still TRUE",
     isTRUE(tok$is_valid(tk_c)) && isTRUE(tok$is_valid(tk_f)))
  tok$reset_count(tk_c); tok$reset_count(tk_f)

  # ── 3. Through secure_app() ───────────────────────────────────────────────
  sa <- shinymanager::secure_app(function(request) tagList(.t69c_estilo_inicial(request), tags$div("pagina")))
  pag <- htmltools::renderTags(sa(pedir(paste0("?token=", tk_c))))
  ok("secure_app() serves a valid Compras token the page with the CSS (PAN, not CAL)",
     grepl("hd-t69c-pestanas", pag$head, fixed = TRUE) && grepl("pagina", pag$html, fixed = TRUE) &&
       !grepl("data-value='CAL'", pag$head, fixed = TRUE))
  pag_f <- htmltools::renderTags(sa(pedir(paste0("?token=", tk_f))))
  ok("... and a full-tier token the page with its own (CAL shown)", grepl("a[data-value='CAL']", pag_f$head, fixed = TRUE))
  tok$remove(tk_c); tok$remove(tk_f)
}

# ── 4. app.R's wiring ────────────────────────────────────────────────────────
i_sa <- grep("^secured_ui <- shinymanager::secure_app\\(", codigo)[1]
ok("secure_app() gets the wrapper, with the page captured before `ui` becomes the router",
   !is.na(i_sa) && grepl("local({", codigo[i_sa + 1L], fixed = TRUE) &&
     any(grepl("pagina <- ui", codigo[i_sa + 1:3], fixed = TRUE)) &&
     any(grepl("function(request) tagList(.t69c_estilo_inicial(request), pagina)", codigo[i_sa + 1:4], fixed = TRUE)))
reg <- sub("#.*$", "", readLines("R/tier_registry.R", warn = FALSE, encoding = "UTF-8"))
i_cls <- grep("classList.add('hd-nivel-aplicado')", c(codigo, reg), fixed = TRUE)
ok("the observer decides over the one tab list", any(grepl("menu_visible(u, HD_PESTANAS_FINANCIERAS, en_cliente)", codigo, fixed = TRUE)))
ok("the class is added in one place, the menu's painter, after every entry",
   length(i_cls) == 1L && i_cls > length(codigo) && exists("menu_js_pintar") &&
     endsWith(menu_js_pintar(c(PAN = TRUE, CAL = FALSE)), "document.documentElement.classList.add('hd-nivel-aplicado');"))
i_vis <- grep("menu_vis <- reactive({", codigo, fixed = TRUE)
ok("...and only with a known tier (the painter is not called while it is unknown)",
   length(i_vis) == 1L && grepl('if (!.nivel_conocido(u$tier %||% "")) return(NULL)', codigo[i_vis + 2L], fixed = TRUE) &&
     any(grepl("if (!is.null(vis)) shinyjs::runjs(menu_js_pintar(vis))", codigo, fixed = TRUE)))

# ── 5. shinymanager's registry is still there ────────────────────────────────
ok("shinymanager:::.tok still has is_valid_server(), get(), add(), remove() and reset_count()",
   all(c("is_valid_server", "get", "add", "remove", "reset_count") %in% ls(tok)))
ok("app.R never calls the single-use tok$is_valid() (it would spend the login's token)",
   !any(grepl("tok$is_valid(", codigo, fixed = TRUE)) && any(grepl("tok$is_valid_server(token)", codigo, fixed = TRUE)))
ok("secure_app() still hands the request to a function ui",
   grepl("ui(request)", paste(deparse(body(shinymanager::secure_app)), collapse = " "), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
