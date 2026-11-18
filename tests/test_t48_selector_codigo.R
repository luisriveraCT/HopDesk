# =============================================================================
# tests/test_t48_selector_codigo.R
# T48: the narrow currency selectors show only the code ("USD"); the wide ones
# keep "USD — Dólar estadounidense". monedas_opciones(etiqueta = "codigo")
# (R/monedas.R, integrador's OK) gives the same values in the same order, only
# the labels change; the default is identical() to before.
# Narrow: the charges row of the Pasivos wizard (90 px), its residual (a
# column of 4) and the manual provision modal ("m" modal, half column).
#
# Run from project root:  Rscript tests/test_t48_selector_codigo.R
# =============================================================================
cat("=== test_t48_selector_codigo ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/monedas.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# monedas_opciones() as it was before T48 (R/monedas.R at 4554de3).
antes <- function(primero = character(), idioma = "es") {
  primero <- unique(primero[moneda_valida(primero)])
  cods <- c(primero, setdiff(.MONEDAS$codigo, primero))
  stats::setNames(cods, paste0(cods, " — ", moneda_nombre(cods, idioma)))
}
casos <- list(character(), MONEDA_BASE_OMISION, c("EUR", MONEDA_BASE_OMISION), c("ZZZ", "EUR"), c(NA, "", "EUR"))

ok("1: por omisión, identical() a antes: sin primero, con la base, con varias, con un código fuera del catálogo y con vacíos (es y en)",
   all(vapply(casos, function(p) identical(monedas_opciones(p), antes(p)) &&
                identical(monedas_opciones(p, "en"), antes(p, "en")) &&
                identical(monedas_opciones(p, "es", "nombre"), antes(p, "es")), logical(1))))
ok("2: con etiqueta = 'codigo', las etiquetas son el código: mismos valores, mismo orden",
   all(vapply(casos, function(p) {
     a <- monedas_opciones(p, etiqueta = "codigo"); b <- monedas_opciones(p)
     identical(unname(a), unname(b)) && identical(names(a), unname(a))
   }, logical(1))))
ok("2b: la base va primero también con solo el código", identical(unname(monedas_opciones("EUR", etiqueta = "codigo")[1]), "EUR"))
ok("3: una etiqueta que no existe truena (match.arg), nunca cae en silencio a otra",
   inherits(tryCatch(monedas_opciones(etiqueta = "simbolo"), error = function(e) e), "error"))

cat("\n-- selectores --\n")
# Every selectInput in the parsed code (not text) whose id is `id`, and
# whether its choices ask for etiqueta = "codigo". An empty argument is
# checked inside the list, never bound to a variable.
selectores <- function(archivo) {
  out <- list()
  buscar <- function(e) {
    if (is.call(e)) {
      f <- deparse(e[[1]])
      if (f %in% c("selectInput", "shiny::selectInput", "selectizeInput", "shiny::selectizeInput")) out[[length(out) + 1L]] <<- e
      l <- as.list(e)
      for (k in seq_along(l)[-1]) if (!(is.symbol(l[[k]]) && !nzchar(as.character(l[[k]])))) buscar(l[[k]])
    }
  }
  for (e in parse(archivo, keep.source = FALSE)) buscar(e)
  out
}
codigo <- function(archivo, id) {
  s <- Filter(function(x) grepl(id, paste(deparse(x[[2]]), collapse = ""), fixed = TRUE), selectores(archivo))
  if (length(s) != 1L) return(NA)
  txt <- paste(deparse(s[[1]]), collapse = " ")
  grepl("monedas_opciones(", txt, fixed = TRUE) && grepl('etiqueta = "codigo"', txt, fixed = TRUE)
}
ok("4: los angostos piden solo el código: fila de cargos, residual y modal de provisión manual",
   isTRUE(codigo("R/pasivos_wizard_steps.R", "pwiz_cargo_moneda_")) &&
     isTRUE(codigo("R/pasivos_wizard_steps.R", "pwiz_residual_moneda")) &&
     isTRUE(codigo("R/pasivos_table_module.R", "ppm_moneda")))
ok("5: los que tienen espacio conservan el nombre (cuentas de banco, moneda de pago, conversión, proveedores, moneda base)",
   identical(codigo("R/settings/settings_cuentas.R", "cta_moneda"), FALSE) &&
     identical(codigo("R/pasivos_wizard_steps.R", "pwiz_moneda_pago"), FALSE) &&
     identical(codigo("R/pasivos_module.R", "pcm_moneda"), FALSE) &&
     identical(codigo("R/proveedores_module.R", "prov_moneda"), FALSE) &&
     identical(codigo("R/empresas_module.R", "moneda_base_input"), FALSE))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
