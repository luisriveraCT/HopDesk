# =============================================================================
# tests/test_pestanas_financieras.R
# The admin deployment hides the financial tabs twice: a static CSS rule in
# the UI (before the server knows who logged in) and the server's observer.
# Two hand-written lists drifted -- REQ was missing from the CSS, so
# "Requisiciones" showed alone while the app loaded (T51). Both now come from
# HD_PESTANAS_FINANCIERAS (app.R).
#
# Run from project root:  Rscript tests/test_pestanas_financieras.R
# Counter-proof: HD_APP_R=<4554de3's app.R> fails.
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

lista <- NULL
for (e in parse(text = src, keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "HD_PESTANAS_FINANCIERAS"))
    lista <- eval(e[[3]], baseenv())
ok(is.character(lista) && length(lista) > 0L, "HD_PESTANAS_FINANCIERAS is defined at the top of app.R")
ok("REQ" %in% lista, "Requisiciones (REQ) is one of them")
ok(!anyDuplicated(lista), "no tab twice")

# No data-value of a financial tab written by hand anywhere else in app.R.
a_mano <- grep("data-value='(PAN|CAL|VEN|PH|BNC|IC|PSV|REQ|FC|RPT)'", codigo, value = TRUE)
ok(!length(a_mano), sprintf("the static CSS builds its selectors from the list (%d written by hand)", length(a_mano)))
ok(any(grepl("HD_PESTANAS_FINANCIERAS, \"']\", collapse", codigo, fixed = TRUE)),
   "the static CSS uses HD_PESTANAS_FINANCIERAS")
ok(any(grepl("FINANCIAL_TABS <- HD_PESTANAS_FINANCIERAS", codigo, fixed = TRUE)) &&
     !any(grepl("FINANCIAL_TABS <- c(", codigo, fixed = TRUE)),
   "the server's observer uses the same list")

# The CSS it makes: every tab of the list, one rule.
if (is.character(lista)) {
  css <- paste0(paste0(".navbar .nav-link[data-value='", lista, "']", collapse = ","), "{display:none!important}")
  ok(all(vapply(lista, function(t) grepl(sprintf("data-value='%s'", t), css, fixed = TRUE), logical(1))),
     "the generated rule names every tab, REQ included")
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
