# =============================================================================
# tests/test_t88_3c_closure.R
# T88-3c: prod's log had dozens of "is.na() applied to ... 'closure'" per
# sign-in. Traced in the real app (dev, 01554be4): all 20 came from app.R's
# crudo() -- attr(x, "crudo") %||% x, where the attribute IS the raw reactive
# (a function) on purpose (T69b), and %||% asked is.na() of it. Not a bug
# (crudo returned the right thing), but noise that hid real warnings.
#  1. %||% (global.R and the Excel worker's copy): NULL, empty, one NA -> y;
#     a function or a list comes back as it is, with no warning.
#  2. crudo(): the raw reactive when there is one, else x; no warning.
#  3. Both copies of %||% say the same thing.
# Run from project root:  Rscript tests/test_t88_3c_closure.R
# =============================================================================
cat("=== test_t88_3c_closure ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
sin_aviso <- function(expr) {
  avisos <- character()
  v <- withCallingHandlers(expr, warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") })
  list(v = v, avisos = avisos)
}
# The definitions, from the files themselves
tomar <- function(archivo, nombre) {
  for (e in parse(archivo, keep.source = FALSE)) {
    hallado <- NULL
    buscar <- function(x) {
      if (!is.null(hallado) || !is.call(x)) return()
      if (identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name(nombre))) { hallado <<- x; return() }
      a <- as.list(x)[-1]   # a[i][[1]]: an empty argument (x[, 1]) is never passed as a missing one
      for (i in seq_along(a)) if (is.call(a[i][[1]])) buscar(a[i][[1]])
    }
    buscar(e)
    if (!is.null(hallado)) { env <- new.env(); eval(hallado, env); return(get(nombre, env)) }
  }
  NULL
}
op_global <- tomar("R/global.R", "%||%")
op_worker <- tomar("R/cashflow_export_module.R", "%||%")
crudo     <- tomar("app.R", "crudo")
ok("0 found %||% in global.R and in the Excel worker, and crudo() in app.R",
   is.function(op_global) && is.function(op_worker) && is.function(crudo))

f <- function() 42
casos <- list(nulo = NULL, vacio = character(), na = NA, na_chr = NA_character_,
              valor = "x", vector = c("a", "b"), funcion = f, lista = list(1), lista_na = list(NA),
              entorno = globalenv())
esperado <- list(nulo = "y", vacio = "y", na = "y", na_chr = "y",
                 valor = "x", vector = c("a", "b"), funcion = f, lista = list(1),
                 lista_na = "y",   # as before T88-3c: is.na(list(NA)) is TRUE
                 entorno = globalenv())
for (op_nombre in c("global", "worker")) {
  op <- if (op_nombre == "global") op_global else op_worker
  for (k in names(casos)) {
    r <- sin_aviso(op(casos[[k]], "y"))
    ok(sprintf("1 %s: %s %%||%% 'y' gives %s, with no warning", op_nombre, k,
               if (identical(esperado[[k]], "y")) "y" else "x itself"),
       identical(r$v, esperado[[k]]) && !length(r$avisos))
  }
}

rv  <- function() "crudo"
envuelta <- function() "envuelta"; attr(envuelta, "crudo") <- rv
r1 <- sin_aviso(crudo(envuelta)); r2 <- sin_aviso(crudo(rv))
ok("2 crudo(): the raw reactive when there is one, no warning", identical(r1$v, rv) && !length(r1$avisos))
ok("2 crudo(): x itself when it has no raw one, no warning", identical(r2$v, rv) && !length(r2$avisos))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
