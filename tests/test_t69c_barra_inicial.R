# =============================================================================
# tests/test_t69c_barra_inicial.R
# T69c: a tier with a tab list (Compras: PAN + REQ) never sees the other tabs,
# not even while the session's first load runs. The page itself carries CSS
# that hides them, built from the tier of the token shinymanager has already
# resolved (server side; nothing from the browser is the tier). The server's
# "Pestañas por nivel" observer takes over by adding `hd-nivel-aplicado` to
# <html> once the tier is known.
#   1. nivel_css_pestanas_iniciales(): Compras hides every other client tab and
#      neither PAN nor REQ; a full tier, staff, an unknown tier give nothing.
#   2. app.R's own .t69c_estilo_inicial(), run against shinymanager's real
#      token registry: a valid Compras token gives the CSS; finance, no token,
#      an unknown token give nothing.
#   3. Through secure_app(): the page it serves for a valid Compras token
#      carries the CSS (shinymanager still hands the request to a function ui).
#   4. app.R's wiring: secure_app gets the wrapper (with the page captured, not
#      the router `ui`); the observer adds the class only with a known tier,
#      after its loop; the class name is the CSS's.
#   5. Guard: shinymanager:::.tok still has is_valid()/get()/add()/remove().
# Run from project root:  Rscript tests/test_t69c_barra_inicial.R
# Counter-proof: on 29d829e (without T69c) 1, 2 and 4 fail.
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
ok("nivel_css_pestanas_iniciales() exists", exists("nivel_css_pestanas_iniciales", mode = "function"))
css <- tryCatch(nivel_css_pestanas_iniciales(COMPRAS, TABS), error = function(e) NULL)
otras <- setdiff(TABS, nivel_pestanas(COMPRAS))
ok(sprintf("Compras: the CSS hides the %d other client tabs (link and li)", length(otras)),
   is.character(css) && length(otras) >= 8L &&
     all(vapply(otras, function(t) grepl(sprintf(".nav-link[data-value='%s']", t), css, fixed = TRUE) &&
                  grepl(sprintf("li:has(> .nav-link[data-value='%s'])", t), css, fixed = TRUE), logical(1))))
ok("Compras: PAN and REQ are not hidden",
   is.character(css) && !grepl("'PAN'", css, fixed = TRUE) && !grepl("'REQ'", css, fixed = TRUE))
ok("the CSS only applies before the server's observer takes over",
   is.character(css) && grepl("{display:none!important}", css, fixed = TRUE) &&
     length(gregexpr("html:not(.hd-nivel-aplicado) ", css, fixed = TRUE)[[1]]) == 2L * length(otras))
ok("a full client tier and staff get no CSS (counter-proof: the navbar as before)",
   exists("nivel_css_pestanas_iniciales") &&
     is.null(nivel_css_pestanas_iniciales(TOTAL, TABS)) && is.null(nivel_css_pestanas_iniciales(STAFF, TABS)))
ok("an unknown tier gets no CSS",
   exists("nivel_css_pestanas_iniciales") &&
     all(vapply(list(NULL, "", NA_character_, "no_es_nivel"), function(t) is.null(nivel_css_pestanas_iniciales(t, TABS)), logical(1))))

# ── 2. app.R's wrapper against shinymanager's own token registry ─────────────
tok <- shinymanager:::.tok
pedir <- function(qs) list(QUERY_STRING = qs)
html_de <- function(x) if (is.null(x)) "" else as.character(htmltools::renderTags(x)$head)
if (exists(".t69c_estilo_inicial", mode = "function")) {
  tk_c <- paste0("t69c_compras_", as.integer(Sys.time())); tk_f <- paste0("t69c_total_", as.integer(Sys.time()))
  tok$add(tk_c, list(user = "prueba_compras", tier = COMPRAS))
  tok$add(tk_f, list(user = "prueba_total", tier = TOTAL))
  h <- html_de(.t69c_estilo_inicial(pedir(paste0("?token=", tk_c))))
  ok("a valid Compras token: the page head carries the CSS", grepl("hd-t69c-pestanas", h, fixed = TRUE) &&
       grepl(".nav-link[data-value='CAL']", h, fixed = TRUE))
  ok("the token quoted as shinymanager writes it also works",
     grepl("hd-t69c-pestanas", html_de(.t69c_estilo_inicial(pedir(sprintf("?token=%%22%s%%22", tk_c)))), fixed = TRUE))
  ok("a valid full-tier token: nothing", is.null(.t69c_estilo_inicial(pedir(paste0("?token=", tk_f)))))
  ok("no token: nothing", is.null(.t69c_estilo_inicial(pedir(""))) && is.null(.t69c_estilo_inicial(list())))
  ok("an unknown token: nothing", is.null(.t69c_estilo_inicial(pedir("?token=no_existe"))))
  ok("a tier sent by the browser is ignored (only the token's counts)",
     is.null(.t69c_estilo_inicial(pedir(paste0("?token=", tk_f, "&tier=", COMPRAS)))))
  ok("the wrapper never spends the token: after all the calls above, shinymanager's single-use is_valid() is still TRUE",
     isTRUE(tok$is_valid(tk_c)) && isTRUE(tok$is_valid(tk_f)))
  tok$reset_count(tk_c); tok$reset_count(tk_f)

  # ── 3. Through secure_app() ───────────────────────────────────────────────
  sa <- shinymanager::secure_app(function(request) tagList(.t69c_estilo_inicial(request), tags$div("pagina")))
  pag <- htmltools::renderTags(sa(pedir(paste0("?token=", tk_c))))
  ok("secure_app() serves a valid Compras token the page with the CSS",
     grepl("hd-t69c-pestanas", pag$head, fixed = TRUE) && grepl("pagina", pag$html, fixed = TRUE))
  pag_f <- htmltools::renderTags(sa(pedir(paste0("?token=", tk_f))))
  ok("... and a full-tier token the page without it", !grepl("hd-t69c-pestanas", paste(pag_f$head, pag_f$html), fixed = TRUE))
  tok$remove(tk_c); tok$remove(tk_f)
}

# ── 4. app.R's wiring ────────────────────────────────────────────────────────
i_sa <- grep("^secured_ui <- shinymanager::secure_app\\(", codigo)[1]
ok("secure_app() gets the wrapper, with the page captured before `ui` becomes the router",
   !is.na(i_sa) && grepl("local({", codigo[i_sa + 1L], fixed = TRUE) &&
     any(grepl("pagina <- ui", codigo[i_sa + 1:3], fixed = TRUE)) &&
     any(grepl("function(request) tagList(.t69c_estilo_inicial(request), pagina)", codigo[i_sa + 1:4], fixed = TRUE)))
i_obs <- grep(".TABS_CLIENTE <- HD_PESTANAS_FINANCIERAS", codigo, fixed = TRUE)[1]
i_cls <- grep("classList.add('hd-nivel-aplicado')", codigo, fixed = TRUE)
i_lazo <- grep("for (tab in .TABS_CLIENTE)", codigo, fixed = TRUE)[1]
ok("the observer uses the one tab list", !is.na(i_obs))
ok("the observer adds the class once, after its loop, only with a known tier",
   length(i_cls) == 1L && !is.na(i_lazo) && i_cls > i_lazo &&
     grepl("if (.nivel_conocido(.tier_pest))", codigo[i_cls - 1L], fixed = TRUE))

# ── 5. shinymanager's registry is still there ────────────────────────────────
ok("shinymanager:::.tok still has is_valid_server(), get(), add(), remove() and reset_count()",
   all(c("is_valid_server", "get", "add", "remove", "reset_count") %in% ls(tok)))
ok("app.R never calls the single-use tok$is_valid() (it would spend the login's token)",
   !any(grepl("tok$is_valid(", codigo, fixed = TRUE)) && any(grepl("tok$is_valid_server(token)", codigo, fixed = TRUE)))
ok("secure_app() still hands the request to a function ui",
   grepl("ui(request)", paste(deparse(body(shinymanager::secure_app)), collapse = " "), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
