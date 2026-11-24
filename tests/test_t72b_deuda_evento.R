# =============================================================================
# tests/test_t72b_deuda_evento.R
# T72 fase B, commit 2: the write sites that still save WITHOUT evento= are a
# frozen list, T72_DEUDA_EVENTO (R/persistence.R): "archivo::funcion" = n
# calls to modificar_tabla / modificar_tabla_completa / .s3_write /
# .s3_write_with without `evento =`, in app.R and R/ (scripts/ are loose
# tools; the emails job does not log -- condition 7). The list can only go
# down, like A5's DEUDA_CAMINO:
#   1  no "archivo::funcion" outside the list writes without an event (new
#      code is born with one);
#   2  no count in the list goes up;
#   3  no count goes down without lowering the list (an entry at 0 leaves it);
#   4  the scan is not blind: it finds the calls it should (a planted one).
# The runtime alarm (.t72_registrar) reads the same list: a write whose call
# stack has none of its functions is outside the known debt.
# Run from project root:  Rscript tests/test_t72b_deuda_evento.R
# Contraprueba: a planted function writing without evento= fails 1.
# =============================================================================
cat("=== test_t72b_deuda_evento ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
# .t72_entrada (T72-G): auth_save_usuarios() writes with its own s3saveRDS and
# logs through it, so it and its wrappers are writers too.
ESCRITORAS <- c("modificar_tabla", "modificar_tabla_completa", ".s3_write", ".s3_write_with", ".t72_entrada")

# T72-C: a WRAPPER is a function with an `evento` formal that calls a writer
# (modificar_hd_admin, save_forecasting_metrics, ...): it is a writer itself.
# Its own inner calls are not debt (it passes the event on, or none); its
# callers that do not pass evento= are. Found to a fixed point (a wrapper of
# a wrapper), over every file at once.
envoltorios <- function(lista_exprs, base = ESCRITORAS) {
  defs <- list()
  juntar <- function(e) {
    if (!is.call(e)) return(invisible())
    if ((identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) && length(e) == 3L &&
        is.name(e[[2]]) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")) &&
        "evento" %in% names(e[[3]][[2]]))
      defs[[length(defs) + 1L]] <<- list(nombre = as.character(e[[2]]), cuerpo = e[[3]][[3]])
    for (k in seq_along(e)[-1]) { a <- e[[k]]; if (!missing(a) && is.call(a)) juntar(a) }
  }
  for (ex in lista_exprs) for (e in ex) juntar(e)
  llama <- function(e, nombres) {
    if (!is.call(e)) return(FALSE)
    if (is.name(e[[1]]) && as.character(e[[1]]) %in% nombres) return(TRUE)
    for (k in seq_along(e)[-1]) { a <- e[[k]]; if (!missing(a) && is.call(a) && llama(a, nombres)) return(TRUE) }
    FALSE
  }
  esc <- base
  repeat {
    nuevos <- unique(vapply(Filter(function(d) !d$nombre %in% esc && llama(d$cuerpo, esc), defs), `[[`, "", "nombre"))
    if (!length(nuevos)) break
    esc <- c(esc, nuevos)
  }
  esc
}

# Calls to a writer without evento=, by the nearest named function around it
# (x <- function ...); an anonymous function keeps the name it is inside.
escanear <- function(exprs, archivo, escritoras = ESCRITORAS) {
  out <- character()
  andar <- function(e, funcion) {
    if (!is.call(e)) return(invisible())
    f <- e[[1]]
    if ((identical(f, as.name("<-")) || identical(f, as.name("="))) && length(e) == 3L &&
        is.name(e[[2]]) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      andar(e[[3]][[3]], as.character(e[[2]])); return(invisible())
    }
    if (identical(f, as.name("function"))) { if (length(e) >= 3L) andar(e[[3]], funcion); return(invisible()) }
    if (is.name(f) && as.character(f) %in% escritoras && !"evento" %in% names(as.list(e)) &&
        !funcion %in% escritoras)   # a writer's (or wrapper's) own inner calls
      out <<- c(out, paste0(archivo, "::", funcion))
    for (k in seq_along(e)[-1]) { a <- e[[k]]; if (!missing(a) && is.call(a)) andar(a, funcion) }
  }
  for (e in exprs) andar(e, "<top>")
  out
}
archivos <- c("app.R", list.files("R", "\\.R$", recursive = TRUE, full.names = TRUE))
codigo <- lapply(archivos, function(a) parse(a, keep.source = FALSE, encoding = "UTF-8"))
ESCRITORAS_TODAS <- envoltorios(codigo)
sitios <- unlist(Map(function(ex, a) escanear(ex, a, ESCRITORAS_TODAS), codigo, archivos))
hoy <- table(sitios); hoy <- stats::setNames(as.integer(hoy), names(hoy))

# The list, read from R/persistence.R without loading the app.
deuda <- NULL
for (e in parse("R/persistence.R", keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("T72_DEUDA_EVENTO"))) deuda <- eval(e[[3]])
ok("0 T72_DEUDA_EVENTO is in R/persistence.R, a named integer vector",
   is.numeric(deuda) && length(deuda) > 0L && !is.null(names(deuda)) && !anyDuplicated(names(deuda)))
if (is.null(deuda)) deuda <- stats::setNames(integer(), character())

nuevos <- setdiff(names(hoy), names(deuda))
ok(sprintf("1 no write without evento= outside the list (%s)", if (length(nuevos)) paste(nuevos, collapse = ", ") else "none"),
   !length(nuevos))
comunes <- intersect(names(hoy), names(deuda))
suben <- comunes[hoy[comunes] > deuda[comunes]]
ok(sprintf("2 no count in the list goes up (%s)", if (length(suben)) paste(sprintf("%s %d>%d", suben, hoy[suben], deuda[suben]), collapse = ", ") else "none"),
   !length(suben))
bajan <- c(comunes[hoy[comunes] < deuda[comunes]], setdiff(names(deuda), names(hoy)))
ok(sprintf("3 the list is lowered with the code: no count went down unnoticed (%s)",
           if (length(bajan)) paste(bajan, collapse = ", ") else "none"),
   !length(bajan))

plantado <- parse(text = c(
  "f_nueva <- function(x) modificar_tabla('k.rds', 'acme', esquema, identity)",
  "f_bien  <- function(x) modificar_tabla('k.rds', 'acme', esquema, identity, evento = ev)",
  "f_obs   <- function(x) observeEvent(x, { .s3_write(1, 'k.rds', client_id = 'acme') })"), keep.source = FALSE)
p <- escanear(plantado, "R/plantado.R")
ok("4 the scan is not blind: it finds a planted write without an event (also inside an observer), never one with evento=",
   identical(sort(p), c("R/plantado.R::f_nueva", "R/plantado.R::f_obs")))
# T72-C: a new wrapper that passes evento on, whose caller does not pass it.
plantado2 <- parse(text = c(
  "w_nueva   <- function(x, evento = NULL) .s3_write(x, 'k.rds', client_id = 'acme', evento = evento)",
  "w_doble   <- function(x, evento = NULL) w_nueva(x, evento = evento)",
  "llamador  <- function() w_nueva(1)",
  "llamador2 <- function() w_doble(1)",
  "con_ev    <- function() w_doble(1, evento = ev)"), keep.source = FALSE)
esc2 <- envoltorios(list(plantado2))
p2 <- escanear(plantado2, "R/plantado.R", esc2)
ok("4b a wrapper (evento formal, calls a writer; also a wrapper of a wrapper) is a writer: its caller without evento= is found, the wrapper itself is not",
   all(c("w_nueva", "w_doble") %in% esc2) && identical(sort(p2), c("R/plantado.R::llamador", "R/plantado.R::llamador2")))
ok("4c the real wrappers are seen: modificar_hd_admin and the forecasting ones",
   all(c("modificar_hd_admin", "save_forecasting_metrics", "save_forecasting_subscriptions", "save_forecasting_sources",
         "save_forecasting_methods", "modificar_forecasting_metrics", "modificar_forecasting_subscriptions") %in% ESCRITORAS_TODAS))
cat(sprintf("   (%d calls without evento= in %d functions; %d writers counting the wrappers)\n",
            sum(hoy), length(hoy), length(ESCRITORAS_TODAS)))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
