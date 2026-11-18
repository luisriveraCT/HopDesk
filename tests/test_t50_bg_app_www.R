# =============================================================================
# tests/test_t50_bg_app_www.R
# T50: start_bg_app() (tests/helpers/live_app_harness.R) serves www/ the way
# the app does. Before, it wrote the app alone in tempdir() and runApp()
# served no www/ next to it: every asset (hdtable.css, i18n/*.json) was a 404
# unless the fixture served www/ itself, and test apps in Chrome came out
# without the app's CSS. No Chrome here: plain HTTP with curl.
#
#   1. /hdtable.css and /i18n/es.json answer 200, with www/'s own bytes;
#   2. www = NULL keeps the old behavior (404);
#   3. the app's own handler answers first: a fixture that serves another
#      repo's www/ (VEN_RAIZ, comparing against master) is never shadowed --
#      why www/ is not copied next to the app file (Shiny's static folders
#      answer before any handler: found writing this test);
#   4. nothing is copied or linked: the real www/ is only read.
#
# Run from project root:  Rscript tests/test_t50_bg_app_www.R
# =============================================================================
cat("=== test_t50_bg_app_www ===\n\n")
source("tests/helpers/live_app_harness.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
pedir <- function(port, ruta) tryCatch(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/%s", port, ruta)),
                                       error = function(e) NULL)
estado <- function(port, ruta) { r <- pedir(port, ruta); if (is.null(r)) NA_integer_ else r$status_code }
bytes <- function(f) readBin(f, "raw", file.size(f))
codigo <- 'library(shiny); shinyApp(fluidPage(tags$link(rel = "stylesheet", href = "hdtable.css"), "t50"), function(input, output) {})'
antes_tmp <- list.files(tempdir())
www_antes <- tools::md5sum(list.files("www", recursive = TRUE, full.names = TRUE))

app <- NULL
on.exit(stop_bg_app(app), add = TRUE)

app <- start_bg_app(codigo, 39601L)
ok("1: /hdtable.css responde 200 con los mismos bytes que www/hdtable.css",
   identical(estado(39601L, "hdtable.css"), 200L) && identical(pedir(39601L, "hdtable.css")$content, bytes("www/hdtable.css")))
ok("1b: también lo de las subcarpetas (/i18n/es.json)", identical(estado(39601L, "i18n/es.json"), 200L))
ok("1c: la app sigue respondiendo en / y lo que no existe sigue siendo 404",
   identical(estado(39601L, ""), 200L) && identical(estado(39601L, "no_existe.css"), 404L))
stop_bg_app(app); app <- NULL

app <- start_bg_app(codigo, 39602L, www = NULL)
ok("2: con www = NULL, como antes: /hdtable.css da 404", identical(estado(39602L, "hdtable.css"), 404L))
stop_bg_app(app); app <- NULL

# A fixture serving ANOTHER repo's www/ (as vencidos_fixture_app.R does with
# VEN_RAIZ): its file must win over this repo's.
otro <- file.path(tempdir(), paste0("t50_otro_repo_", Sys.getpid())); dir.create(otro)
writeLines("/* otro repo */", file.path(otro, "hdtable.css"))
fixture <- sprintf('library(shiny); app <- shinyApp(fluidPage("t50"), function(input, output) {})
del_otro <- shiny:::staticHandler("%s"); propio <- app$httpHandler
app$httpHandler <- function(req) { r <- del_otro(req); if (is.null(r)) propio(req) else r }
app', normalizePath(otro, winslash = "/"))
app <- start_bg_app(fixture, 39603L)
ok("3: la fixture que sirve el www/ de otro repo contesta primero; lo demás sale de este www/",
   grepl("otro repo", rawToChar(pedir(39603L, "hdtable.css")$content), fixed = TRUE) &&
     identical(estado(39603L, "i18n/es.json"), 200L))
stop_bg_app(app); app <- NULL
unlink(otro, recursive = TRUE)

ok("4: no se copió ni enlazó nada: tempdir() sin carpetas nuevas (salvo la de callr) y www/ idéntico",
   !length(setdiff(list.dirs(tempdir(), recursive = FALSE, full.names = FALSE), c(antes_tmp, basename(otro), "callr"))) &&
     identical(tools::md5sum(list.files("www", recursive = TRUE, full.names = TRUE)), www_antes))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
