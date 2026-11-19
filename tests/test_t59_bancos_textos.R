# =============================================================================
# tests/test_t59_bancos_textos.R
# T59 (1): the reasons and warnings of Bancos' TXT account detection
# (R/bancos_detect.R) come from the dictionary (tablas.bancos.detectar.*),
# read through .bnc_detect_txt() when needed -- never tr() at the file's top
# level, which loads before R/i18n.R (rule 29). In Spanish every text reads
# exactly as the hand-written list did (taken from git, before T59), with the
# same arguments; in English it is translated.
#
# Run from project root:  Rscript tests/test_t59_bancos_textos.R
# =============================================================================
cat("=== test_t59_bancos_textos ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
cargar <- function(archivo, nombres, env = globalenv()) for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres) eval(e, env)
cargar("R/bancos_detect.R", ".bnc_detect_txt")
ok(".bnc_detect_txt() exists", is.function(get0(".bnc_detect_txt")))

antes <- new.env()
src <- tryCatch(system2("git", c("show", "876b4d6:R/bancos_detect.R"), stdout = TRUE, stderr = FALSE), error = function(e) NULL)
for (e in parse(text = enc2utf8(paste(src, collapse = "\n")), keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(".BNC_DETECT_TXT"))) eval(e, antes)
VIEJO <- antes$.BNC_DETECT_TXT
ok("the hand-written list before T59 is read from git (13 texts)", is.list(VIEJO) && length(VIEJO) == 13L)

# Each text with the arguments its caller passes, by name for the new one.
casos <- list(
  motivo_numero    = list(v = list("012345678"),               n = list(cuenta = "012345678")),
  motivo_sub       = list(v = list("02"),                      n = list(sub = "02")),
  motivo_moneda    = list(v = list("USD"),                     n = list(moneda = "USD")),
  motivo_nombre    = list(v = list(),                          n = list()),
  aviso_sin_numero = list(v = list("0123456789012"),           n = list(cuenta = "0123456789012")),
  aviso_moneda_no  = list(v = list(),                          n = list()),
  aviso_moneda_dif = list(v = list("USD", "MXN"),              n = list(moneda_archivo = "USD", moneda_cuenta = "MXN")),
  aviso_sub_dif    = list(v = list("01"),                      n = list(sub = "01")),
  aviso_nombre_dif = list(v = list("NETWORKS CROSSDOCKING"),   n = list(empresa = "NETWORKS CROSSDOCKING")),
  aviso_empate     = list(v = list(3L),                        n = list(n = 3L)),
  nota_otra_cuenta = list(v = list(),                          n = list()),
  aviso_nada       = list(v = list(),                          n = list()),
  aviso_totales    = list(v = list("1,234.56 MXN", "1,200.00 MXN", "50.00 MXN", "50.00 MXN"),
                          n = list(cargos_archivo = "1,234.56 MXN", cargos_leidos = "1,200.00 MXN",
                                   abonos_archivo = "50.00 MXN", abonos_leidos = "50.00 MXN")))
ok("every text of the old list has its case here", setequal(names(casos), names(VIEJO)))
for (k in names(casos)) {
  esperado <- do.call(sprintf, c(list(VIEJO[[k]]), casos[[k]]$v))
  ok(sprintf("%s: in Spanish, exactly the old text", k),
     identical(do.call(.bnc_detect_txt, c(list(k), casos[[k]]$n)), esperado))
}
idioma_orig <- hd_idioma
ok("in English, translated (aviso_nada)",
   { assign("hd_idioma", function(...) "en", envir = globalenv()); r <- .bnc_detect_txt("aviso_nada")
     assign("hd_idioma", idioma_orig, envir = globalenv()); identical(r, "The account could not be identified; pick it by hand.") })
ok("an account number passed as a number is not split by thousands commas (tr() would)",
   grepl("número 123456789012;", .bnc_detect_txt("aviso_sin_numero", cuenta = 123456789012), fixed = TRUE))
es <- jsonlite::fromJSON("www/i18n/es.json"); enj <- jsonlite::fromJSON("www/i18n/en.json")
claves <- paste0("tablas.bancos.detectar.", names(casos))
ok("all 13 keys exist in es.json and en.json", all(claves %in% names(es)) && all(claves %in% names(enj)))
ok("no %s left in them ({placeholders} only)", !any(grepl("%[sd]", unlist(c(es[claves], enj[claves])))))

det <- paste(readLines("R/bancos_detect.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("R/bancos_detect.R: no hand-written list and no T_$ left", !grepl(".BNC_DETECT_TXT", det, fixed = TRUE) && !grepl("T_$", det, fixed = TRUE))
arriba <- Filter(function(e) !(is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) &&
                                identical(e[[3]][[1]], as.name("function"))),
                 as.list(parse("R/bancos_detect.R", keep.source = FALSE, encoding = "UTF-8")))
ok("R/bancos_detect.R: nothing at the top level calls tr() (it loads before R/i18n.R)",
   !any(vapply(arriba, function(e) "tr" %in% all.names(e), logical(1))))
bm <- paste(readLines("R/bancos_module.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("R/bancos_module.R reads the note through .bnc_detect_txt()",
   grepl('.bnc_detect_txt("nota_otra_cuenta")', bm, fixed = TRUE) && !grepl(".BNC_DETECT_TXT", bm, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
