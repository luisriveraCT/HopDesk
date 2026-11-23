# =============================================================================
# tests/test_overlay_textos.R
# The loading overlay (www/overlay_init.js) takes every text from the
# dictionary (cuentas.carga.*). The sign-in page does not carry the whole
# dictionary: app.R puts only those keys there (i18n_head_parcial()), before
# the overlay's script.
# Run from project root:  Rscript tests/test_overlay_textos.R
# =============================================================================
cat("=== test_overlay_textos ===\n\n")
suppressMessages(source("R/i18n.R", encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

es <- i18n_dict("es"); en <- i18n_dict("en")
carga <- function(d) sort(grep("^cuentas\\.carga\\.", names(d), value = TRUE))
ok("es and en have the same cuentas.carga.* keys", identical(carga(es), carga(en)))
n <- length(grep("^cuentas\\.carga\\.mensaje\\.[0-9]{2}\\.titulo$", names(es)))
ok(sprintf("the messages are numbered 01..%02d, each with titulo and detalle (the overlay stops at the first gap)", n),
   n >= 10L && all(sprintf("cuentas.carga.mensaje.%02d.%s", rep(seq_len(n), each = 2L), c("titulo", "detalle")) %in% names(es)))

html <- as.character(i18n_head_parcial("cuentas.carga."))
js <- sub("^.*window\\.HD_I18N=Object\\.assign\\(window\\.HD_I18N\\|\\|\\{\\},(.*)\\);.*$", "\\1", html)
dic <- jsonlite::fromJSON(js, simplifyVector = FALSE)
ok("i18n_head_parcial() puts only the prefix's keys, merged into window.HD_I18N",
   grepl("window.HD_I18N=Object.assign(window.HD_I18N||{},", html, fixed = TRUE) &&
     identical(sort(names(dic)), carga(es)) && length(dic) < 100L)

# A text holding "</script>" must not end the page's <script> (integrador).
real <- .i18n_cache[["es"]]
.i18n_cache[["es"]] <- c(real, list(cuentas.carga.prueba = "fin</script><b>x</b>"))
html_mal <- as.character(i18n_head_parcial("cuentas.carga."))
.i18n_cache[["es"]] <- real
cuerpo <- sub("^<script>(.*)</script>$", "\\1", html_mal)
ok("a text with </script> is escaped (<\\/): the script tag ends only once",
   lengths(regmatches(html_mal, gregexpr("</script>", html_mal, fixed = TRUE))) == 1L &&
     grepl("fin<\\/script><b>x<\\/b>", cuerpo, fixed = TRUE) &&
     identical(jsonlite::fromJSON(sub("^.*Object\\.assign\\(window\\.HD_I18N\\|\\|\\{\\},(.*)\\);$", "\\1", cuerpo),
                                  simplifyVector = FALSE)$cuentas.carga.prueba, "fin</script><b>x</b>"))

app <- readLines("app.R", warn = FALSE, encoding = "UTF-8")
i <- grep('i18n_head_parcial("cuentas.carga.")', app, fixed = TRUE)
j <- grep('tags$script(src = "overlay_init.js?v=', app, fixed = TRUE)
ok("app.R gives the sign-in page the overlay's texts before the overlay's script",
   length(i) == 1L && length(j) == 1L && i < j)

ov <- readLines("www/overlay_init.js", warn = FALSE, encoding = "UTF-8")
ok("overlay_init.js has no hand-written Spanish left (no accented letters or ¡ ¿)",
   !any(grepl("[áéíóúñÁÉÍÓÚÑ¡¿]", ov)))
ok("overlay_init.js reads its texts with hdT() (a plain lookup when i18n.js is not on the page)",
   any(grepl("var hdT = window.hdT || function(k)", ov, fixed = TRUE)) &&
     any(grepl("hdT('cuentas.carga.lema')", ov, fixed = TRUE)) &&
     any(grepl("hdT('cuentas.carga.listo.titulo')", ov, fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
