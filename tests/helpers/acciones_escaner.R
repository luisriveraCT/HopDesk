# =============================================================================
# tests/helpers/acciones_escaner.R
# T87: every action a log row can carry, read from the code (R/ and app.R),
# for the catalog's scanner (tests/test_t87_catalogo.R):
#   - log_action(..., action = "x") and log_action(user, "module", "x", ...);
#   - the T72 write layer's events: accion = "x" (any event list) and
#     evento_sistema("origen", "x"); evento_parte_de("x") writes "x/parte";
#   - the layer's own "sin_evento".
# Only literal strings: an action built at run time (paste0) is the caller's
# to classify by its base (t87_clase_accion strips "_fallido").
# =============================================================================
acciones_en_codigo <- function(raiz = ".") {
  archivos <- c(file.path(raiz, "app.R"), list.files(file.path(raiz, "R"), "[.]R$", full.names = TRUE))
  out <- character()
  lit <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  visitar <- function(x) {
    if (!is.call(x)) return(invisible())
    f <- if (is.name(x[[1]])) as.character(x[[1]]) else ""
    args <- as.list(x)[-1]
    nm <- names(args) %||% rep("", length(args))
    if (f == "log_action") {
      if ("action" %in% nm && lit(args[[which(nm == "action")[1]]])) out <<- c(out, args[[which(nm == "action")[1]]])
      pos <- args[!nzchar(nm)]
      if (!"action" %in% nm && length(pos) >= 3L && lit(pos[[3]])) out <<- c(out, pos[[3]])
    }
    if (f == "evento_sistema") {
      a <- if ("accion" %in% nm) args[[which(nm == "accion")[1]]] else { pos <- args[!nzchar(nm)]; if (length(pos) >= 2L) pos[[2]] }
      if (lit(a)) out <<- c(out, a)
    }
    if (f == "evento_parte_de") {
      a <- if ("accion" %in% nm) args[[which(nm == "accion")[1]]] else { pos <- args[!nzchar(nm)]; if (length(pos) >= 1L) pos[[1]] }
      if (lit(a)) out <<- c(out, paste0(a, "/parte"))
    }
    if (f == "list" && "accion" %in% nm && lit(args[[which(nm == "accion")[1]]])) out <<- c(out, args[[which(nm == "accion")[1]]])
    for (a in as.list(x)[-1]) if (!missing(a) && is.call(a)) visitar(a)
    invisible()
  }
  for (f in archivos) {
    ex <- tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    for (e in ex) visitar(e)
  }
  sort(unique(c(out, "sin_evento")))
}
