# =============================================================================
# tests/test_avisos_global_r.R
# No module may bring a fixed-text notice back: every file in R/ (settings/
# included) and app.R goes through the common detector
# (tests/helpers/avisos_guardiana.R, showNotification() with or without
# shiny::). A new file is covered without listing it.
#
# PENDIENTES: the integrator's files still to pass, each with the count it
# has today. The count can only go down: a file listed here that gains a
# fixed notice fails, and one that reaches 0 must leave the list (so the
# list never hides anything once it is done).
#
# Run from project root:  Rscript tests/test_avisos_global_r.R
# AVISOS_RAIZ=<checkout> checks another checkout (the contraprueba).
# =============================================================================
cat("=== test_avisos_global_r ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("tests/helpers/avisos_guardiana.R")
raiz <- Sys.getenv("AVISOS_RAIZ", ".")

# The integrator's (avisos-sueltos, 2026-11-11). 10 and 75 then; one more each
# since the detector also sees if/else messages (avisos-integrador). app.R
# reached 0 and left (avisos-integrador 2/3).
PENDIENTES <- c("R/tiers_module.R" = 76L)

archivos <- c(list.files(file.path(raiz, "R"), "\\.R$", recursive = TRUE), "app.R")
archivos <- ifelse(archivos == "app.R", "app.R", file.path("R", archivos))
conteo <- vapply(archivos, function(f) length(avisos_fijos(file.path(raiz, f))), integer(1))

ok(sprintf("el detector recorre todo R/ y app.R (%d archivos)", length(archivos)),
   length(archivos) >= 90L &&"R/settings/settings_hub.R" %in% archivos && "app.R" %in% archivos)

fuera <- conteo[!names(conteo) %in% names(PENDIENTES) & conteo > 0L]
ok(sprintf("ningún archivo fuera de PENDIENTES tiene avisos con texto fijo (%d con alguno)", length(fuera)),
   { for (f in names(fuera)) message("  ", f, ": ", fuera[[f]], " -- ",
                                     paste(utils::head(avisos_fijos(file.path(raiz, f)), 3), collapse = " | "))
     !length(fuera) })

for (f in names(PENDIENTES)) {
  n <- conteo[[f]]
  ok(sprintf("PENDIENTES %s: %d aviso(s) fijo(s), no más que los %d anotados", f, n, PENDIENTES[[f]]),
     n <= PENDIENTES[[f]])
  ok(sprintf("PENDIENTES %s: todavía tiene alguno (con 0 sale de la lista)", f), n > 0L)
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
