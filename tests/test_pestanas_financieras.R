# =============================================================================
# tests/test_pestanas_financieras.R
# The client tabs come from one list, HD_PESTANAS_FINANCIERAS (app.R). Two
# hand-written lists drifted once -- REQ was missing from the static CSS, so
# "Requisiciones" showed alone while the app loaded (T51).
# Since T87-M8 there is no per-tab hiding list at all: every navbar entry is
# born hidden by one generic rule (HD_MENU_CSS_OCULTO, R/tier_registry.R), in
# every deployment, so no tab of the list -- or a new one -- can show before
# the server says so; and both places that SHOW a tab (the first paint,
# menu_css_inicial, and the menu's one observer, menu_visible) take the list.
#
# Run from project root:  Rscript tests/test_pestanas_financieras.R
# Counter-proof: HD_APP_R=<4554de3's app.R> fails (REQ missing).
# =============================================================================
cat("=== test_pestanas_financieras ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else              { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

app <- Sys.getenv("HD_APP_R", "app.R")
src <- readLines(app, warn = FALSE, encoding = "UTF-8")
codigo <- sub("#.*$", "", src)
reg <- sub("#.*$", "", readLines("R/tier_registry.R", warn = FALSE, encoding = "UTF-8"))

lista <- NULL
for (e in parse(text = src, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "HD_PESTANAS_FINANCIERAS"))
    lista <- eval(e[[3]], baseenv())
ok(is.character(lista) && length(lista) > 0L, "HD_PESTANAS_FINANCIERAS is defined at the top of app.R")
ok("REQ" %in% lista, "Requisiciones (REQ) is one of them")
ok(!anyDuplicated(lista), "no tab twice")

# No data-value of a client tab written by hand, in app.R or in the registry.
patron <- "data-value='(PAN|CAL|VEN|PH|BNC|IC|PSV|REQ|FC|RPT)'"
ok(!length(grep(patron, c(codigo, reg))), "no client tab's selector written by hand (app.R, R/tier_registry.R)")

# The hidden rule: generic (any entry, so every tab of the list and any new
# one), in the page head of EVERY deployment -- not inside an if.
oculto <- NULL
for (e in parse("R/tier_registry.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("HD_MENU_CSS_OCULTO"))) oculto <- eval(e[[3]], baseenv())
ok(is.character(oculto) && grepl("^\\.navbar a\\[data-value\\],\\.navbar li:has\\(> a\\[data-value\\]\\)\\{display:none!important\\}$", oculto),
   "the hidden rule hides every navbar a[data-value] and its li, with no value filter")
i_css <- grep("tags$style(HTML(HD_MENU_CSS_OCULTO))", codigo, fixed = TRUE)
ok(length(i_css) == 1L && !grepl("^\\s*if\\b", codigo[i_css]) && !grepl("IS_ADMIN_DEPLOYMENT", codigo[i_css], fixed = TRUE),
   "the page head carries it once, in every deployment")
if (is.character(lista)) {
  css <- paste0(paste0(".navbar a[data-value='", lista, "']", collapse = ","), "{display:none!important}")
  ok(all(vapply(lista, function(t) grepl(sprintf("data-value='%s'", t), css, fixed = TRUE), logical(1))),
     "the list names every tab, REQ included")
}

# Both places that show a tab take the list.
ok(any(grepl("menu_css_inicial(tok$get(token)$tier, HD_PESTANAS_FINANCIERAS)", codigo, fixed = TRUE)),
   "the first paint shows from the list")
ok(any(grepl("menu_visible(u, HD_PESTANAS_FINANCIERAS, en_cliente)", codigo, fixed = TRUE)),
   "the menu's observer decides over the list")
ok(!any(grepl("FINANCIAL_TABS", codigo, fixed = TRUE)) && !any(grepl(".TABS_CLIENTE", codigo, fixed = TRUE)),
   "no second copy of the list")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
