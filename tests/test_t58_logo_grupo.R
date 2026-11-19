# =============================================================================
# tests/test_t58_logo_grupo.R
# T58 (Luis): Ajustes › Configuración del Grupo shows the SAVED logo (a
# thumbnail), not just "✓ Logo guardado", so one can see which it is -- the
# one the reports carry. logo_grupo_img() (R/empresas_module.R) draws it,
# and the "Empresa" line of a new invitation uses the same function.
#
# Run from project root:  Rscript tests/test_t58_logo_grupo.R
# =============================================================================
cat("=== test_t58_logo_grupo ===\n\n")
suppressPackageStartupMessages(library(shiny))
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a
for (e in parse("R/empresas_module.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("logo_grupo_img")))
    eval(e, envir = globalenv())

PNG <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
h <- function(x) as.character(x)
ok("logo_grupo_img exists", exists("logo_grupo_img", mode = "function"))
ok("a PNG logo: an inline <img>, its bytes in base64, at most `alto` px high",
   { s <- h(logo_grupo_img(PNG, "png", alto = 64))
     grepl(paste0('src="data:image/png;base64,', base64enc::base64encode(PNG), '"'), s, fixed = TRUE) &&
       grepl("max-height:64px", s, fixed = TRUE) })
ok("a JPG logo: image/jpeg", grepl("data:image/jpeg;base64,", h(logo_grupo_img(PNG, "jpg")), fixed = TRUE))
ok("no logo (NULL, empty, not raw): nothing", is.null(logo_grupo_img(NULL)) && is.null(logo_grupo_img(raw(0))) &&
     is.null(logo_grupo_img("x")))

# The Ajustes card draws it where "✓ Logo guardado" was, only with a logo.
src <- paste(readLines("R/empresas_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
i <- regexpr("output$group_config_section <- renderUI(", src, fixed = TRUE)
card <- substr(src, i, i + 4000)
ok("Ajustes › Configuración del Grupo: the saved logo as a thumbnail, when there is one",
   i > 0 && grepl("if (has_logo) {", card, fixed = TRUE) &&
     grepl("logo_grupo_img(cfg$logo_raw, cfg$logo_ext, alto = 64)", card, fixed = TRUE))
ok("…with its label from the dictionary, not a fixed \"Logo guardado\"",
   grepl('tr("tablas.empresas.logo_guardado")', card, fixed = TRUE) && !grepl('"Logo guardado"', card, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
